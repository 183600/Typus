{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Dependencies (
  -- 类型检查器
  DependentTypeChecker,
  DependentTypeError(..),

  -- 语法树
  AST(..),
  Statement(..),
  TypeExpr(..),
  Constraint(..),

  -- 类型系统实体
  TypeVar(..),
  TypeConstraint(..),
  Substitution,

  -- Hindley-Milner 类型推断
  TypeScheme(..),
  TypeEnvironment(..),
  TypeInferenceState(..),
  TypeInferenceError(..),

  -- 构建与使用
  newDependentTypeChecker,
  newDependentTypeCheckerWithTypes,
  analyzeDependentTypes,
  analyzeAST,
  validateASTSemantics,

  -- 基础操作
  checkType,
  addType,
  addConstraint,
  checkTypeInstantiation,
  solveConstraints,
  getDependentTypeErrors,

  -- 类型推断操作
  inferType,
  inferStatement,
  inferProgram,
  generalize,
  instantiate,
  unifyTypes,
  applyTypeSubstitution,
  newTypeVariable,
  getFreshTypeVar,
  initialTypeEnvironment,

  -- 泛型处理
  instantiateScheme,
  generalizeInContext,
  checkPolyType,

  -- 约束求解
  solveTypeConstraints,
  simplifyConstraints,

  -- 作用域管理
  pushScope,
  popScope,
  inNewScope,

  -- 解析
  grammarDefinition,
  parseProgram,
  runParser
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Void (Void)
import           Data.Either (partitionEithers)
import           Data.List (nub)
import           Data.Maybe (catMaybes)
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad (when, unless)
import           Text.Megaparsec (Parsec, (<|>), choice, many, eof, errorBundlePretty, try, optional)
import qualified Text.Megaparsec as MP
import           Text.Megaparsec.Char (alphaNumChar, letterChar, char, string, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.IORef
import           Test.QuickCheck (Arbitrary(..), oneof, elements, listOf1, choose)
import           Debug.Trace (trace)

-- ============================================================================
-- 1. 文法
-- ============================================================================

grammarDefinition :: String
grammarDefinition = unlines
  [ "Typus Language BNF Grammar:",
    "",
    "<program>       ::= <statement>*",
    "<statement>     ::= <typeDef> | <varDecl> | <funcDecl> | <constraintDef>",
    "<typeDef>       ::= \"type\" <identifier> [<typeParams>] [<whereClause>]",
    "<typeParams>    ::= \"<\" <identifier> (\",\" <identifier>)* \">\"",
    "<whereClause>   ::= \"where\" <constraint> (\",\" <constraint>)*",
    "<varDecl>       ::= (\"var\" | \"const\") <identifier> \":\" <typeExpr>",
    "<funcDecl>      ::= \"func\" <identifier> \"(\" <paramList> \")\" [\":\" <typeExpr>]",
    "<paramList>     ::= <param> (\",\" <param>)* | ε",
    "<param>         ::= <identifier> \":\" <typeExpr>",
    "<constraintDef> ::= \"constraint\" <identifier> \"=\" <constraintExpr>",
    "<typeExpr>      ::= <simpleType> | <genericType> | <dependentType> | <functionType>",
    "<simpleType>    ::= <identifier>",
    "<genericType>   ::= <identifier> \"<\" <typeExpr> (\",\" <typeExpr>)* \">\"",
    "<dependentType> ::= <typeExpr> \"where\" <constraint> (\",\" <constraint>)*",
    "<functionType>  ::= \"func\" \"(\" <paramList> \")\" \":\" <typeExpr>",
    "<constraint>    ::= <sizeConstraint> | <rangeConstraint> | <predicateConstraint>",
    "<sizeConstraint> ::= <identifier> (\">\" | \">=\") <integer>",
    "<rangeConstraint> ::= <identifier> \"<\" <integer> \",\" <integer> \">\"",
    "<predicateConstraint> ::= <identifier> \"(\" <argList> \")\"",
    "<argList>       ::= <typeExpr> (\",\" <typeExpr>)* | ε",
    "<integer>       ::= [0-9]+",
    "<identifier>    ::= [A-Za-z_][A-Za-z0-9_]*"
  ]

-- ============================================================================
-- 2. AST
-- ============================================================================

data AST = Program [Statement]
  deriving (Show, Eq)

data Statement
  = STypeDef Text [Text] [Constraint]        -- type Name [<params>] [where ...]
  | STypeAlias Text TypeExpr [Constraint]    -- type Name = TypeRef [where ...]
  | SVarDecl Text TypeExpr
  | SFuncDecl Text [(Text, TypeExpr)] (Maybe TypeExpr)
  | SConstraintDef Text Constraint
  | SExistsDecl [Text] Statement             -- exists T. <statement>
  deriving (Show, Eq)

data TypeExpr
  = SimpleT Text
  | GenericT Text [TypeExpr]
  | FuncT [(Text, TypeExpr)] TypeExpr
  | RefineT TypeExpr [Constraint]
  deriving (Show, Eq)

data Constraint
  = SizeGT Text Int             -- x > N
  | SizeGE Text Int             -- x >= N
  | RangeC Text Int Int         -- x<min,max>
  | PredC Text [TypeExpr]       -- P(t1, t2, ...)
  deriving (Show, Eq)

-- ============================================================================
-- 3. 解析（Megaparsec 单阶段）
-- ============================================================================

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

rword :: Text -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedByIdentChar) >> pure ()
  where
    notFollowedByIdentChar :: Parser ()
    notFollowedByIdentChar = MP.notFollowedBy (alphaNumChar <|> char '_')

rws :: [Text]
rws = ["type","where","var","const","func","constraint","forall","exists","protocol","capability"]

identifier :: Parser Text
identifier = (lexeme . try) $ do
  x <- (letterChar <|> char '_' <|> char '\'')
  xs <- many (alphaNumChar <|> char '_' <|> char '\'')
  let s = T.pack (x:xs)
  if s `elem` rws then fail ("reserved word " <> T.unpack s) else pure s

integer :: Parser Int
integer = lexeme (fromInteger <$> L.decimal)

angles :: Parser a -> Parser a
angles = MP.between (symbol "<") (symbol ">")

parens :: Parser a -> Parser a
parens = MP.between (symbol "(") (symbol ")")

commaSep :: Parser a -> Parser [a]
commaSep p = p `MP.sepBy1` symbol ","

-- Program
parseProgram :: Parser AST
parseProgram = sc *> (Program <$> many parseStatement) <* eof

parseStatement :: Parser Statement
parseStatement = choice
  [ parseTypeDef
  , parseVarDecl
  , parseFuncDecl
  , parseConstraintDef
  , parseExistsDecl
  , parseProtocolDecl
  , parseCapabilityDecl
  ]

parseTypeDef :: Parser Statement
parseTypeDef = do
  rword "type"
  name <- identifier
  -- Parse optional type parameters first
  params <- optional (angles (commaSep parseTypeParam))
  -- Check if this is an alias (has = after the name and optional params)
  optionalType <- optional $ symbol "="
  case optionalType of
    Just _ -> do  -- This is a type alias: type Name [<params>] = TypeRef [where ...]
      target <- parseTypeExpr
      cs <- optional parseWhereClause
      pure $ STypeAlias name target (maybe [] id cs)
    Nothing -> do  -- This is a regular type definition: type Name [<params>] [where ...]
      cs <- optional parseWhereClause
      pure $ STypeDef name (maybe [] id params) (maybe [] id cs)
  where
    parseTypeParam :: Parser Text
    parseTypeParam = do
      pname <- identifier
      _ <- optional (symbol ":" *> parseTypeExpr)
      pure pname

parseWhereClause :: Parser [Constraint]
parseWhereClause = do
  rword "where"
  parseConstraintsSep
  where
    parseConstraintsSep = do
      c1 <- parseConstraint
      more <- many ((symbol "&&" <|> symbol ",") *> parseConstraint)
      pure (c1:more)

parseVarDecl :: Parser Statement
parseVarDecl = do
  _ <- (rword "var" <|> rword "const")
  name <- identifier
  _ <- symbol ":"
  t <- parseTypeExpr
  pure $ SVarDecl name t

parseFuncDecl :: Parser Statement
parseFuncDecl = do
  rword "func"
  name <- identifier
  _ <- optional (angles (commaSep identifier))
  params <- parens (parseParamList)
  rt <- optional (symbol ":" *> parseTypeExpr)
  pure $ SFuncDecl name params rt

parseParamList :: Parser [(Text, TypeExpr)]
parseParamList = (commaSep parseParam) <|> pure []
  where
    parseParam = MP.try namedParam MP.<|> typeOnlyParam

    namedParam = do
      n <- identifier
      _ <- symbol ":"
      t <- parseTypeExpr
      pure (n, t)

    typeOnlyParam :: Parser (Text, TypeExpr)
    typeOnlyParam = do
      t <- parseTypeExpr
      pure ("_", t)

parseConstraintDef :: Parser Statement
parseConstraintDef = do
  rword "constraint"
  name <- identifier
  _ <- symbol "="
  c <- parseConstraint
  pure $ SConstraintDef name c

parseProtocolDecl :: Parser Statement
parseProtocolDecl = do
  rword "protocol"
  _ <- identifier
  _ <- many (try protocolEntry)
  pure (SConstraintDef "protocol" (PredC "protocol" []))
  where
    protocolEntry = do
      _ <- identifier
      _ <- symbol ":"
      _ <- parseTypeExpr
      pure ()

parseCapabilityDecl :: Parser Statement
parseCapabilityDecl = do
  rword "capability"
  _ <- identifier
  pure (SConstraintDef "capability" (PredC "capability" []))

parseExistsDecl :: Parser Statement
parseExistsDecl = do
  rword "exists"
  vars <- commaSep identifier
  _ <- symbol "."
  stmt <- parseStatement
  pure $ SExistsDecl vars stmt

parseConstraint :: Parser Constraint
parseConstraint = do
  c <- choice [ try parseTypeClass, try parseEquality, try parseRelOpExtended, try parseRange, parsePredicateExt ]
  let _ = trace ("parsed constraint: " ++ show c) ()
  pure c
  where
    parseTypeClass = do
      n <- identifier
      _ <- symbol ":"
      t <- parseTypeExpr
      pure $ PredC ":" [SimpleT n, t]
    parseEquality = do
      lhs <- identifier
      _ <- symbol "=="
      rhsId <- optional identifier
      rhsVal <- case rhsId of
        Just r  -> pure (SimpleT r)
        Nothing -> do
          n <- integer
          pure (SimpleT (T.pack (show n)))
      pure $ PredC "==" [SimpleT lhs, rhsVal]
    parseRelOpExtended = do
      lhs <- parseTermLHS
      op <- (symbol ">=" *> pure ">=")
         <|> (symbol ">"  *> pure ">")
         <|> (symbol "<=" *> pure "<=")
         <|> (symbol "<"  *> pure "<")
      rhs <- parseTermRHS
      pure $ PredC (T.pack op) [lhs, rhs]
    parseTermLHS = MP.try (GenericT <$> identifier <*> parens ((commaSep parseTypeExpr) <|> pure []))
               <|> (SimpleT <$> identifier)
               <|> (SimpleT . T.pack . show <$> integer)
    parseTermRHS = MP.try (GenericT <$> identifier <*> parens ((commaSep parseTypeExpr) <|> pure []))
               <|> (SimpleT <$> identifier)
               <|> (SimpleT . T.pack . show <$> integer)
    parseRange = do
      n <- identifier
      _ <- symbol "<"
      a <- integer
      _ <- symbol ","
      b <- integer
      _ <- symbol ">"
      pure $ RangeC n a b
    parsePredicateExt = do
      p <- identifier
      args <- optional (parens ( (commaSep parseTypeExpr) <|> pure [] ))
      pure $ PredC p (maybe [] id args)

parseTypeExpr :: Parser TypeExpr
parseTypeExpr = MP.try parseConditionalType <|> MP.try parseForallType <|> MP.try parseRefType <|> parseFuncType <|> parseRefineOrApp <|> parseExistsType
  where
    parseForallType = do
      rword "forall"
      _ <- commaSep identifier
      _ <- symbol "."
      t <- parseTypeExpr
      pure t

    parseExistsType = do
      rword "exists"
      _ <- commaSep identifier
      _ <- symbol "."
      t <- parseTypeExpr
      pure t

    parseFuncType = do
      rword "func"
      ps <- parens parseParamList
      _ <- symbol ":"
      rt <- parseTypeExpr
      let ft = FuncT ps rt
      cs <- optional parseWhereClause
      case cs of
        Nothing -> pure ft
        Just cs' -> pure (RefineT ft cs')

    parseRefineOrApp = do
      base <- parseAppOrSimple
      cs <- optional parseWhereClause
      case cs of
        Nothing -> pure base
        Just cs' -> pure (RefineT base cs')

    parseAppOrSimple = do
      n <- identifier
      mags <- optional (angles (commaSep parseTypeArg))
      case mags of
        Nothing -> pure (SimpleT n)
        Just args -> pure (GenericT n args)

    parseTypeArg = MP.try parseBoolConstraintAsType <|> parseTypeExpr

    parseBoolConstraintAsType = do
      c <- parseRelOrPredExpr
      pure (RefineT (SimpleT "bool") [c])

    parseRelOrPredExpr = do
      try parseBinaryRel <|> parseBarePredicate

    parseBinaryRel = do
      lhs <- parseTerm
      op <- (symbol ">=" *> pure ">=")
         <|> (symbol ">"  *> pure ">")
         <|> (symbol "<=" *> pure "<=")
         <|> (symbol "<"  *> pure "<")
         <|> (symbol "==" *> pure "==")
      rhs <- parseTerm
      case lhs of
        SimpleT l -> pure $ PredC (T.pack op) [SimpleT l, rhs]
        _         -> pure $ PredC (T.pack op) [lhs, rhs]

    parseBarePredicate = do
      p <- identifier
      args <- optional (parens ((commaSep parseTypeExpr) <|> pure []))
      pure $ PredC p (maybe [] id args)

    parseTerm = MP.try (SimpleT . T.pack . show <$> integer)
           <|> MP.try (do f <- identifier; args <- parens ((commaSep parseTypeExpr) <|> pure []); pure (GenericT f args))
           <|> (SimpleT <$> identifier)

    parseConditionalType = do
      cond <- parseAppOrSimple
      _ <- symbol "?"
      tThen <- parseTypeExpr
      _ <- symbol ":"
      tElse <- parseTypeExpr
      let _ = trace ("parsed conditional type: " ++ show (GenericT "If" [cond, tThen, tElse])) ()
      pure (GenericT "If" [cond, tThen, tElse])

    parseRefType = do
      _ <- symbol "&"
      mlt <- optional (char '\'' *> identifier)
      mmut <- optional (rword "mut")
      t <- parseTypeExpr
      case (mmut, mlt) of
        (Just _, Just lt) -> pure (GenericT "RefMut" [SimpleT (T.cons '\'' lt), t])
        (Just _, Nothing) -> pure (GenericT "RefMut" [t])
        (Nothing, Just lt) -> pure (GenericT "Ref" [SimpleT (T.cons '\'' lt), t])
        _ -> pure (GenericT "Ref" [t])

-- ============================================================================
-- 4. Hindley-Milner 类型推断系统
-- ============================================================================

-- 类型方案（用于泛型）
data TypeScheme
  = Forall [String] TypeVar        -- Forall a b. t
  deriving (Show, Eq)

-- 类型环境
data TypeEnvironment = TypeEnvironment
  { teTypes         :: Map.Map String TypeDef
  , teSchemes       :: Map.Map String TypeScheme
  , teCurrentLevel  :: Int
  , teNextTypeVarId :: IORef Int
  }

-- 替换（ substitution ）
type Substitution = Map.Map String TypeVar

-- 类型推断状态
data TypeInferenceState = TypeInferenceState
  { typeEnv          :: TypeEnvironment
  , currentSubst     :: Substitution
  , inferenceErrors  :: [TypeInferenceError]
  }

-- 类型推断错误
data TypeInferenceError
  = UnificationFailure TypeVar TypeVar
  | InfiniteType String TypeVar
  | UnboundVariable String
  | TypeMismatchError TypeVar TypeVar
  | ConstraintNotSatisfied TypeConstraint
  | OccursCheckFailed String TypeVar
  | GenericEscape String TypeVar
  deriving (Show, Eq)

-- 类型推断 Monad
type TypeInference = StateT TypeInferenceState (ExceptT TypeInferenceError IO)

-- 类型变量生成器
nextTypeVarId :: TypeInference Int
nextTypeVarId = do
  env <- gets typeEnv
  liftIO $ atomicModifyIORef' (teNextTypeVarId env) (\i -> (i + 1, i))

-- 创建新的类型变量
newTypeVariable :: TypeInference TypeVar
newTypeVariable = do
  varId <- nextTypeVarId
  pure $ TVVar ("t" ++ show varId)

-- 获取新鲜的类型变量
getFreshTypeVar :: TypeInference TypeVar
getFreshTypeVar = newTypeVariable

-- 创建初始类型环境
initialTypeEnvironment :: IO TypeEnvironment
initialTypeEnvironment = do
  ref <- newIORef 0
  pure $ TypeEnvironment
    { teTypes = preludeTypeDefs
    , teSchemes = Map.empty
    , teCurrentLevel = 0
    , teNextTypeVarId = ref
    }



-- ============================================================================
-- 5. 类型系统与约束（原有）
-- ============================================================================

data TypeVar
  = TVCon String                  -- 具体类型构造子（含 0 元，如 int 等）
  | TVVar String                  -- 类型参数（形参）
  | TVApp String [TypeVar]        -- 类型应用（构造子名 + 实参）
  | TVFun [TypeVar] TypeVar       -- 函数类型
  | TVTuple [TypeVar]             -- 元组类型（如将来拓展）
  deriving (Show, Eq, Ord)

-- 约束（类型层）
data TypeConstraint
  = Equal TypeVar TypeVar
  | Subtype TypeVar TypeVar
  | Predicate String [TypeVar]
  | TypeSizeGE TypeVar Int
  | TypeSizeGT TypeVar Int
  | TypeRange TypeVar Int Int
  deriving (Show, Eq, Ord)

-- 错误
data DependentTypeError
  = DependentTypeMismatch TypeVar TypeVar
  | ConstraintViolation String TypeVar
  | TypeNotFound String
  | InvalidTypeArgument String
  | UnsolvableConstraint TypeConstraint
  | DependentInfiniteType String TypeVar
  | AmbiguousType String
  | ParseError String
  | SemanticError String
  deriving (Show, Eq)

-- 类型定义环境
data TypeDef = TypeDefDecl
  { tdParams      :: [String]
  , tdConstraints :: [TypeConstraint]   -- 以参数名（TVVar）表达的约束
  } deriving (Show, Eq)

data TypeEnv = TypeEnv
  { typeDefinitions   :: Map.Map String TypeDef
  , pendingConstraints :: [TypeConstraint]
  } deriving (Show, Eq)

data DependentTypeChecker = DependentTypeChecker
  { dtcTypeEnv  :: TypeEnv
  , tcErrors :: [DependentTypeError]
  } deriving (Show, Eq)

-- Prelude：内置一些常用类型（零参数）
preludeTypeDefs :: Map.Map String TypeDef
preludeTypeDefs = Map.fromList
  [ ("int",      TypeDefDecl [] [])
  , ("string",   TypeDefDecl [] [])
  , ("bool",     TypeDefDecl [] [])
  , ("float64",  TypeDefDecl [] [])
  ]

newDependentTypeChecker :: DependentTypeChecker
newDependentTypeChecker = DependentTypeChecker
  { dtcTypeEnv = TypeEnv preludeTypeDefs []
  , tcErrors = []
  }

newDependentTypeCheckerWithTypes :: [(String, [String], [TypeConstraint])] -> DependentTypeChecker
newDependentTypeCheckerWithTypes typeDefs =
  let defs = Map.fromList [ (n, TypeDefDecl ps cs) | (n, ps, cs) <- typeDefs ]
   in DependentTypeChecker
        { dtcTypeEnv = TypeEnv (preludeTypeDefs <> defs) []
        , tcErrors = []
        }

-- ============================================================================
-- 5. AST -> 语义检查
-- ============================================================================

analyzeDependentTypes :: String -> [DependentTypeError]
analyzeDependentTypes src =
  case runParser src of
    Left e   -> [ParseError e]
    Right ast ->
      let (errs, _) = runState (validateASTSemantics ast) newDependentTypeChecker
      in errs

analyzeAST :: AST -> [DependentTypeError]
analyzeAST ast =
  let (errs, _) = runState (validateASTSemantics ast) newDependentTypeChecker
  in errs

validateASTSemantics :: AST -> State DependentTypeChecker [DependentTypeError]
validateASTSemantics (Program ss) = do
  mapM_ validateStatement ss
  _ <- solveConstraints
  st <- get
  pure (reverse (tcErrors st))

validateStatement :: Statement -> State DependentTypeChecker ()
validateStatement stmt = case stmt of
  STypeDef name params cs -> do
    let cs' = map (convertConstraint (Set.fromList (map T.unpack params))) cs
    addType (T.unpack name) (map T.unpack params) cs'

  STypeAlias name target cs -> do
    -- For type aliases, we need to verify the target type exists
    let (tv, extraCs) = convertTypeExprAndRefinements Set.empty target
    checkType tv
    mapM_ addConstraint extraCs
    -- Add the type alias to the environment with constraints
    let cs' = map (convertConstraint Set.empty) cs  -- Convert constraints for alias
    addType (T.unpack name) [] cs'

  SVarDecl _name texpr -> do
    let (tv, extraCs) = convertTypeExprAndRefinements Set.empty texpr
    checkType tv
    mapM_ addConstraint extraCs

  SFuncDecl _name params rt -> do
    mapM_ (\(_n,t) -> do
              let (pt, pcs) = convertTypeExprAndRefinements Set.empty t
              checkType pt
              mapM_ addConstraint pcs) params
    case rt of
      Nothing -> pure ()
      Just t  -> do
        let (rv, rcs) = convertTypeExprAndRefinements Set.empty t
        checkType rv
        mapM_ addConstraint rcs

  SConstraintDef _ c -> do
    let c' = convertConstraint Set.empty c
    addConstraint c'

  SExistsDecl _vars innerStmt -> do
    -- For exists declarations, we validate the inner statement
    -- The variables are treated as existential type parameters
    validateStatement innerStmt

-- ============================================================================
-- 6. 转换与检查
-- ============================================================================

-- 将 TypeExpr 转换为 TypeVar，并提取附加在 refineT 的约束（转换为 TypeConstraint）
convertTypeExprAndRefinements :: Set.Set String -> TypeExpr -> (TypeVar, [TypeConstraint])
convertTypeExprAndRefinements params te = case te of
  SimpleT n ->
    ( nameToTypeVar params n
    , []
    )
  GenericT n args ->
    let argPairs = map (convertTypeExprAndRefinements params) args
        argTVs = map fst argPairs
        argCs  = concatMap snd argPairs
    in ( TVApp (T.unpack n) argTVs
       , argCs
       )
  FuncT ps rt ->
    let psPairs = [ convertTypeExprAndRefinements params t | (_,t) <- ps ]
        psTVs = map fst psPairs
        psCs  = concatMap snd psPairs
        (rtTV, rtCs) = convertTypeExprAndRefinements params rt
    in ( TVFun psTVs rtTV, psCs <> rtCs )
  RefineT base cs ->
    let (bTV, bCs) = convertTypeExprAndRefinements params base
        cs' = map (convertConstraint params) cs
    in (bTV, bCs <> cs')

nameToTypeVar :: Set.Set String -> Text -> TypeVar
nameToTypeVar params n =
  let s = T.unpack n
  in if s `Set.member` params then TVVar s else TVCon s

convertTypeExpr :: Set.Set String -> TypeExpr -> TypeVar
convertTypeExpr params t = fst (convertTypeExprAndRefinements params t)

convertConstraint :: Set.Set String -> Constraint -> TypeConstraint
convertConstraint params c = case c of
  SizeGE name k ->
    TypeSizeGE (nameToTypeVar params name) k
  SizeGT name k ->
    TypeSizeGT (nameToTypeVar params name) k
  RangeC name a b ->
    TypeRange (nameToTypeVar params name) a b
  PredC pname args ->
    let tvars = map (convertTypeExpr params) args
    in Predicate (T.unpack pname) tvars

-- 添加类型定义
addType :: String -> [String] -> [TypeConstraint] -> State DependentTypeChecker ()
addType name params cs = do
  st <- get
  let env = dtcTypeEnv st
      defs = typeDefinitions env
      def  = TypeDefDecl params cs
      defs' = Map.insert name def defs
  put st { dtcTypeEnv = env { typeDefinitions = defs' } }

-- 添加全局约束（将被 solveConstraints 统一求解/验证）
addConstraint :: TypeConstraint -> State DependentTypeChecker ()
addConstraint c = do
  st <- get
  let env = dtcTypeEnv st
  put st { dtcTypeEnv = env { pendingConstraints = c : pendingConstraints env } }

-- 收集错误
addTypeError :: DependentTypeError -> State DependentTypeChecker ()
addTypeError e = modify (\st -> st { tcErrors = e : tcErrors st })

-- 查找类型定义
lookupTypeDef :: String -> State DependentTypeChecker (Maybe TypeDef)
lookupTypeDef n = do
  st <- get
  pure $ Map.lookup n (typeDefinitions (dtcTypeEnv st))

-- 简化的类型存在性/应用性检查（纯构造子/参数个数）
checkType :: TypeVar -> State DependentTypeChecker ()
checkType tv = case tv of
  TVCon n -> do
    mdef <- lookupTypeDef n
    case mdef of
      Nothing -> addTypeError (TypeNotFound n)
      Just (TypeDefDecl ps _) ->
        when (not (null ps)) $
          addTypeError (InvalidTypeArgument n)

  TVVar _ -> pure () -- 形参仅在实例化时检验

  TVApp n args -> do
    mdef <- lookupTypeDef n
    case mdef of
      Nothing -> addTypeError (TypeNotFound n)
      Just (TypeDefDecl ps cs) -> do
        when (length ps /= length args) $
          addTypeError (InvalidTypeArgument n)
        mapM_ checkType args
        -- 将定义上的约束用 args 实例化后立即加入
        let subst = zip ps args
        mapM_ (checkTypeConstraint subst) cs

  TVFun ps rt -> mapM_ checkType ps >> checkType rt

  TVTuple xs -> mapM_ checkType xs

-- 明确的实例化检查（给定构造子名与实参）
checkTypeInstantiation :: String -> [TypeVar] -> State DependentTypeChecker ()
checkTypeInstantiation n args = do
  mdef <- lookupTypeDef n
  case mdef of
    Nothing -> addTypeError (TypeNotFound n)
    Just (TypeDefDecl ps cs) -> do
      when (length ps /= length args) $
        addTypeError (InvalidTypeArgument n)
      mapM_ checkType args
      let subst = zip ps args
      mapM_ (checkTypeConstraint subst) cs

-- ============================================================================
-- 7. 约束求解（先合一等式，再验证谓词）
-- ============================================================================

-- 统一替换：参数名 -> 类型
type Subst = [(String, TypeVar)]

applySubst :: Subst -> TypeVar -> TypeVar
applySubst s tv = case tv of
  TVVar x ->
    case lookup x s of
      Nothing -> TVVar x
      Just t  -> if t == TVVar x then TVVar x else applySubst s t
  TVCon _ -> tv
  TVApp f args -> TVApp f (map (applySubst s) args)
  TVFun ps rt  -> TVFun (map (applySubst s) ps) (applySubst s rt)
  TVTuple xs   -> TVTuple (map (applySubst s) xs)

applySubstC :: Subst -> TypeConstraint -> TypeConstraint
applySubstC s c = case c of
  Equal a b       -> Equal (applySubst s a) (applySubst s b)
  Subtype a b     -> Subtype (applySubst s a) (applySubst s b)
  Predicate p xs  -> Predicate p (map (applySubst s) xs)
  TypeSizeGE t n  -> TypeSizeGE (applySubst s t) n
  TypeSizeGT t n  -> TypeSizeGT (applySubst s t) n
  TypeRange t a b -> TypeRange (applySubst s t) a b

occurs :: String -> TypeVar -> Bool
occurs x tv = case tv of
  TVVar y       -> x == y
  TVCon _       -> False
  TVApp _ args  -> any (occurs x) args
  TVFun ps rt   -> any (occurs x) ps || occurs x rt
  TVTuple xs    -> any (occurs x) xs

-- 合一等式与构造等价
unify :: [(TypeVar, TypeVar)] -> Maybe Subst
unify = go []
  where
    go s [] = Just s
    go s ((a,b):rest)
      | a == b = go s rest
      | otherwise =
          case (a,b) of
            (TVVar x, t)
              | occurs x t -> Nothing
              | otherwise  -> go ((x,t):s) (applyPairs (x,t) rest)
            (t, TVVar x)    -> go s ((TVVar x,t):rest)
            (TVCon f, TVCon g)
              | f == g      -> go s rest
              | otherwise   -> Nothing
            (TVApp f xs, TVApp g ys)
              | f == g && length xs == length ys
                            -> go s (zip xs ys ++ rest)
              | otherwise   -> Nothing
            (TVFun ps1 r1, TVFun ps2 r2)
              | length ps1 == length ps2
                            -> go s (zip ps1 ps2 ++ [(r1,r2)] ++ rest)
              | otherwise   -> Nothing
            (TVTuple xs, TVTuple ys)
              | length xs == length ys
                            -> go s (zip xs ys ++ rest)
              | otherwise   -> Nothing
            _               -> Nothing

    applyPairs (x,t) = map (\(l,r) -> (applySubst [(x,t)] l, applySubst [(x,t)] r))

-- 子类型（此处简单处理为相等）
isSubtype :: TypeVar -> TypeVar -> Bool
isSubtype a b = a == b

-- 验证单一约束（在替换后）
validateConstraint :: TypeConstraint -> Either DependentTypeError ()
validateConstraint c = case c of
  Equal a b ->
    if a == b then Right () else Left (DependentTypeMismatch a b)

  Subtype a b ->
    if isSubtype a b then Right () else Left (DependentTypeMismatch a b)

  TypeSizeGE _ n ->
    if n >= 0 then Right () else Left (SemanticError "size >= n must have n >= 0")

  TypeSizeGT _ n ->
    if n >= 0 then Right () else Left (SemanticError "size > n must have n >= 0")

  TypeRange _ a b ->
    if a <= b then Right () else Left (SemanticError "invalid range: min > max")

  -- 对一般谓词约束，这里不做决策（可在后续扩展为用户定义求解）
  Predicate _ _ ->
    Right ()

-- 直接检查类型定义上的约束（已替换后）
checkTypeConstraint :: Subst -> TypeConstraint -> State DependentTypeChecker ()
checkTypeConstraint subst c =
  case validateConstraint (applySubstC subst c) of
    Right _ -> pure ()
    Left e  -> addTypeError e

-- 求解当前环境中的待定约束
solveConstraints :: State DependentTypeChecker Bool
solveConstraints = do
  st <- get
  let cs = pendingConstraints (dtcTypeEnv st)
      (eqs, others) = partitionEithers (map pickEq cs)
      pickEq cc = case cc of
        Equal a b   -> Left (a,b)
        Subtype a b -> Left (a,b) -- 简化处理：与等式一起合一
        _           -> Right cc
  case unify eqs of
    Nothing -> do
      addTypeError (SemanticError "failed to unify type equalities/subtypes")
      st' <- get
      put st' { dtcTypeEnv = (dtcTypeEnv st') { pendingConstraints = [] } }
      pure False
    Just subst -> do
      let cs' = map (applySubstC subst) others
          results = map validateConstraint cs'
          (errs, _) = partitionEithers (map toEither results)
          toEither (Left e)  = Left e
          toEither (Right x) = Right x
      mapM_ addTypeError errs
      put st { dtcTypeEnv = (dtcTypeEnv st) { pendingConstraints = [] } }
      pure (null errs)

getDependentTypeErrors :: DependentTypeChecker -> [DependentTypeError]
getDependentTypeErrors = reverse . tcErrors

-- ============================================================================
-- 8. Hindley-Milner 类型推断核心算法
-- ============================================================================

-- 推断表达式的类型
inferType :: TypeExpr -> TypeInference TypeVar
inferType expr = case expr of
  SimpleT name -> do
    -- 查找类型定义
    env <- gets typeEnv
    case Map.lookup (T.unpack name) (teTypes env) of
      Nothing -> pure $ TVVar "error"  -- 简化错误处理
      Just _ -> pure $ TVCon (T.unpack name)

  GenericT name args -> do
    -- 推断类型参数
    argTypes <- mapM inferType args
    pure $ TVApp (T.unpack name) argTypes

  FuncT params returnType -> do
    -- 推断函数类型
    paramTypes <- mapM (inferType . snd) params
    retType <- inferType returnType
    pure $ TVFun paramTypes retType

  RefineT baseType constraints -> do
    -- 推断基础类型，然后添加约束
    base <- inferType baseType
    let typeConstraints = map (convertConstraint Set.empty) constraints
    -- 添加约束到环境中
    mapM_ addTypeConstraint typeConstraints
    pure base

-- 推断语句的类型
inferStatement :: Statement -> TypeInference (Maybe TypeVar)
inferStatement stmt = case stmt of
  STypeDef name params constraints -> do
    let typeConstraints = map (convertConstraint (Set.fromList (map T.unpack params))) constraints
    env <- gets typeEnv
    let typeDef = TypeDefDecl (map T.unpack params) typeConstraints
    let updatedEnv = env { teTypes = Map.insert (T.unpack name) typeDef (teTypes env) }
    modify $ \s -> s { typeEnv = updatedEnv }
    pure Nothing

  SVarDecl name typeExpr -> do
    varType <- inferType typeExpr
    env <- gets typeEnv
    let scheme = Forall [] varType  -- 简单情况下没有类型变量
    let updatedEnv = env { teSchemes = Map.insert (T.unpack name) scheme (teSchemes env) }
    modify $ \s -> s { typeEnv = updatedEnv }
    pure (Just varType)

  SFuncDecl name params returnType -> do
    -- 推断参数类型
    paramTypes <- mapM (inferType . snd) params
    -- 推断返回类型
    retType <- case returnType of
      Nothing -> getFreshTypeVar
      Just rt -> inferType rt
    let funcType = TVFun paramTypes retType
    env <- gets typeEnv
    let scheme = Forall [] funcType
    let updatedEnv = env { teSchemes = Map.insert (T.unpack name) scheme (teSchemes env) }
    modify $ \s -> s { typeEnv = updatedEnv }
    pure (Just funcType)

  SConstraintDef _ constraint -> do
    let typeConstraint = convertConstraint Set.empty constraint
    addTypeConstraint typeConstraint
    pure Nothing

  STypeAlias _ _ _ ->
    -- 类型别名在类型推断中可能不需要特殊处理
    -- 因为它们在解析时应该已经被展开
    pure Nothing

  SExistsDecl _vars innerStmt -> do
    -- For exists declarations, infer the type of the inner statement
    _ <- inferStatement innerStmt
    pure Nothing

-- 推断整个程序的类型
inferProgram :: AST -> TypeInference [TypeVar]
inferProgram (Program statements) = do
  types <- mapM inferStatement statements
  pure $ catMaybes types

-- 泛化：将类型变量转换为泛型类型方案
generalize :: Int -> TypeVar -> TypeInference TypeScheme
generalize level tv = do
  let freeVars = extractFreeTypeVars tv
  let nonGenericVars = filter (not . isGenericLevel level) freeVars
  pure $ Forall nonGenericVars tv
  where
    isGenericLevel _ _ = True  -- 简化版本，实际需要检查作用域

-- 实例化：将泛型类型方案替换为具体类型
instantiate :: TypeScheme -> TypeInference TypeVar
instantiate (Forall vars tv) = do
  -- 为每个类型变量创建新的类型变量
  newVars <- mapM (\_ -> getFreshTypeVar) vars
  let substitution = Map.fromList (zip vars newVars)
  pure $ applyTypeSubstitution substitution tv

-- 类型合一
unifyTypes :: TypeVar -> TypeVar -> TypeInference ()
unifyTypes t1 t2 = do
  case unify [(t1, t2)] of
    Nothing -> pure ()  -- 简化错误处理
    Just substList -> do
      let subst = Map.fromList substList
      -- 应用替换到当前状态
      modify $ \s -> s { currentSubst = subst `Map.union` currentSubst s }

-- 应用类型替换
applyTypeSubstitution :: Substitution -> TypeVar -> TypeVar
applyTypeSubstitution subst tv = case tv of
  TVVar name -> case Map.lookup name subst of
    Nothing -> tv
    Just newTv -> newTv
  TVCon _ -> tv
  TVApp name args -> TVApp name (map (applyTypeSubstitution subst) args)
  TVFun params ret -> TVFun (map (applyTypeSubstitution subst) params) (applyTypeSubstitution subst ret)
  TVTuple args -> TVTuple (map (applyTypeSubstitution subst) args)

-- 添加类型约束
addTypeConstraint :: TypeConstraint -> TypeInference ()
addTypeConstraint constraint = do
  -- 简化版本：直接验证约束
  case validateConstraint constraint of
    Right () -> pure ()
    Left _ -> pure ()  -- 简化错误处理

-- 提取自由类型变量
extractFreeTypeVars :: TypeVar -> [String]
extractFreeTypeVars = go Set.empty
  where
    go seen tv = case tv of
      TVVar name -> if Set.member name seen then [] else [name]
      TVCon _ -> []
      TVApp _ args -> concatMap (go seen) args
      TVFun params ret -> concatMap (go seen) params ++ go seen ret
      TVTuple args -> concatMap (go seen) args

-- 实例化类型方案（对外的公共接口）
instantiateScheme :: TypeScheme -> TypeInference TypeVar
instantiateScheme = instantiate

-- 在上下文中泛化
generalizeInContext :: TypeVar -> TypeInference TypeScheme
generalizeInContext tv = do
  env <- gets typeEnv
  generalize (teCurrentLevel env) tv

-- 检查多态类型
checkPolyType :: TypeVar -> TypeInference ()
checkPolyType tv = do
  let freeVars = extractFreeTypeVars tv
  unless (null freeVars) $ pure ()  -- 简化版本

-- 解决类型约束
solveTypeConstraints :: [TypeConstraint] -> TypeInference ()
solveTypeConstraints constraints = do
  mapM_ solveSingleConstraint constraints
  where
    solveSingleConstraint constraint =
      case validateConstraint constraint of
        Right () -> pure ()
        Left _ -> pure ()  -- 简化处理，不抛出错误

-- 简化约束
simplifyConstraints :: [TypeConstraint] -> [TypeConstraint]
simplifyConstraints = nub  -- 简化版本：去重

-- ============================================================================
-- 9. 高级约束求解和类型变量管理
-- ============================================================================


-- 类型变量的作用域管理
pushScope :: TypeInference ()
pushScope = modify $ \s ->
  let env = typeEnv s
      newLevel = teCurrentLevel env + 1
      newEnv = env { teCurrentLevel = newLevel }
  in s { typeEnv = newEnv }

popScope :: TypeInference ()
popScope = modify $ \s ->
  let env = typeEnv s
      newLevel = max 0 (teCurrentLevel env - 1)
      newEnv = env { teCurrentLevel = newLevel }
  in s { typeEnv = newEnv }

-- 在新的作用域中运行类型推断
inNewScope :: TypeInference a -> TypeInference a
inNewScope action = do
  pushScope
  result <- action
  popScope
  pure result


-- ============================================================================
-- 10. 综合类型推断API
-- ============================================================================




-- ============================================================================
-- 11. 完整的泛型类型推断系统
-- ============================================================================





-- 类型推断的完整验证管道
-- validateGenericInference :: String -> IO (Either String ())
-- validateGenericInference _source = pure $ Right ()  -- 简化实现

-- ============================================================================
-- 12. 解析入口（对外 API）
-- 保持与旧接口相似的包装，返回字符串错误
runParser :: String -> Either String AST
runParser input =
  case MP.runParser parseProgram "(source)" (T.pack input) of
    Left e   -> Left (errorBundlePretty e)
    Right ast -> Right ast

instance Arbitrary TypeVar where
  arbitrary = elements [TVCon "int", TVCon "string", TVCon "bool", TVVar "T"]

instance Arbitrary TypeConstraint where
  arbitrary = oneof
    [ TypeSizeGE <$> arbitrary <*> choose (0, 100)
    , TypeSizeGT <$> arbitrary <*> choose (0, 100)
    , TypeRange <$> arbitrary <*> choose (0, 50) <*> choose (51, 100)
    , Equal <$> arbitrary <*> arbitrary
    , Predicate <$> elements ["Ord", "Eq", "Show"] <*> listOf1 arbitrary
    ]

instance Arbitrary Statement where
  arbitrary = oneof
    [ pure (STypeDef "MyType" ["T"] [])
    , pure (SVarDecl "x" (SimpleT "int"))
    , pure (SFuncDecl "id" [("x", SimpleT "T")] (Just (SimpleT "T")))
    ]

instance Arbitrary AST where
  arbitrary = do
    stmts <- listOf1 arbitrary
    pure (Program stmts)
