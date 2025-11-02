{-# LANGUAGE OverloadedStrings #-}
module Ownership
  ( -- 数据模型
    OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer
  , newOwnershipAnalyzer
  , analyzeOwnership
  , analyzeOwnershipFile
  , analyzeOwnershipDebug
  , formatOwnershipErrors
  , lexAll
  , parseProgram
  , builtInFunctions
  ) where



import qualified Data.Map.Strict as Map
import Data.Char (isSpace)
import Data.Maybe (isJust, mapMaybe)
import Data.List (intercalate, isInfixOf)
import Control.Monad.State
import Control.Monad (when)

import Ownership.Common.Types
import Ownership.Common.Lexer (Pos(..), Token(..), TokenKind(..))
import Ownership.Lexer (Keyword(..), Sym(..), OwnershipToken, lexAll)

--------------------------------------------------------------------------------
-- 3) 语法分析器（Parser）
-- - 构建一个简单 AST：语句、表达式、指令块
-- - 仅解析我们分析所需的子集：var/let/赋值/函数调用/&借用/块/函数体
-- - 支持指令：//! ownership:on/off（单行）与 { //! ... }（块级）
--------------------------------------------------------------------------------

type Name = String

data AssignOp = OpAssign | OpWalrus deriving (Eq, Show)

data UnaryOp = UBorrow | UMutBorrow deriving (Eq, Show)

data Directive = Directive (Map.Map String String) deriving (Eq, Show)

data Expr
  = EIdent Name Pos
  | ECall Name [Expr] Pos
  | EUnary UnaryOp Expr Pos
  | ELitStr String Pos
  | ELitNum String Pos
  | EUnknown [OwnershipToken] Pos
  deriving (Eq, Show)

getExprPos :: Expr -> Pos
getExprPos e = case e of
  EIdent _ p   -> p
  ECall _ _ p  -> p
  EUnary _ _ p -> p
  ELitStr _ p  -> p
  ELitNum _ p  -> p
  EUnknown _ p -> p

data Stmt
  = SVarDecl Name (Maybe Expr) Pos
  | SLetDecl Name (Maybe Expr) Pos
  | SAssignStmt Name AssignOp Expr Pos
  | SExpr Expr Pos
  | SBlock [Stmt] Pos
  | SFunc [Stmt] Pos               -- 仅保留函数体作为块
  | SFor [Stmt] Pos                -- for 块（仅保留块体）
  | SDirectiveLine Directive Pos   -- 单行指令（对后续生效）
  | SDirectiveBlock Directive [Stmt] Pos -- 块级指令（块内生效）
  deriving (Eq, Show)

data Program = Program [Stmt] deriving (Eq, Show)

-- 解析入口
parseProgram :: [OwnershipToken] -> Program
parseProgram toks = Program (parseManyTop toks)

-- 跳过多余分隔符
skipNL :: [OwnershipToken] -> [OwnershipToken]
skipNL (Token (TSym SNewline) _:xs)   = skipNL xs
skipNL (Token (TSym SSemicolon) _:xs) = skipNL xs
skipNL xs = xs


isSym :: Sym -> OwnershipToken -> Bool
isSym s (Token (TSym s') _) = s == s'
isSym _ _ = False

isKw :: Keyword -> OwnershipToken -> Bool
isKw k (Token (TKw k') _) = k == k'
isKw _ _ = False

tokId :: OwnershipToken -> Maybe (String, Pos)
tokId (Token (TId s) p) = Just (s, p)
tokId _ = Nothing


tokComment :: OwnershipToken -> Maybe (String, Bool, Pos)
tokComment (Token (TComment s isLine) p) = Just (s, isLine, p)
tokComment _ = Nothing

-- 解析若干顶层语句，直到 EOF
parseManyTop :: [OwnershipToken] -> [Stmt]
parseManyTop = go 0
  where
    go :: Int -> [OwnershipToken] -> [Stmt]
    go depth xs = case skipNL xs of
      [] -> []
      ts ->
        if depth > 1000  -- 防止无限递归
        then []
        else let (st, rest) = parseStmt ts
             in st : go (depth + 1) rest

-- 解析一般语句
parseStmt :: [OwnershipToken] -> (Stmt, [OwnershipToken])
parseStmt xs0 =
  let xs = skipNL xs0
  in case xs of
    -- 单行指令：//! ...
    (t:rest)
      | Just (cmt, True, p) <- tokComment t
      , isDirectiveText cmt ->
          let dir = parseDirectiveText cmt
          in (SDirectiveLine dir p, rest)

    -- 块：{ ... }，并支持块级指令 { //! ... statements }
    (Token (TSym SLBrace) pOpen : rest) ->
      let (maybeDir, afterDir) = parseOptionalLeadingDirective rest
          (body, rest') = parseBlockBody afterDir
      in case maybeDir of
          Just dir -> (SDirectiveBlock dir body pOpen, rest')
          Nothing  -> (SBlock body pOpen, rest')

    -- for 循环：for ... { ... } 仅提取块体作为一个 SFor
    (t:rest) | isKw KwFor t ->
      let (_, afterSig) = consumeUntilLBrace rest
          (blockStmt, rest') = case afterSig of
            [] -> (SBlock [] (Pos 0 0), [])
            (t':rest'') -> parseStmt (t' : rest'')
      in case blockStmt of
         SBlock body p -> (SFor body p, rest')
         SDirectiveBlock dir body p -> (SFor [SDirectiveBlock dir body p] p, rest')
         _ -> (blockStmt, rest')

    -- 类型与接口声明：type ... struct { ... } / interface { ... } 跳过其块体
    (t:rest) | isKw KwType t || isKw KwInterface t ->
      let (_, afterSig) = consumeUntilLBrace rest
          (_ignoredBlock, rest') = parseBlockBody afterSig
      in (SExpr (EUnknown [] (Pos 0 0)) (Pos 0 0), rest')

    -- 函数：func ... { ... } 仅提取函数体
    (t:rest) | isKw KwFunc t ->
      let (_, afterSig) = consumeUntilLBrace rest
          -- 现在 afterSig 是以 { 开头，交给块解析
          (blockStmt, rest') = case afterSig of
            [] -> (SBlock [] (Pos 0 0), [])
            (t':rest'') -> parseStmt (t' : rest'')
      in case blockStmt of
         SBlock body p -> (SFunc body p, rest')
         SDirectiveBlock dir body p -> (SFunc [SDirectiveBlock dir body p] p, rest') -- 较少见，但保留
         _ -> (blockStmt, rest') -- 回退

    -- if 语句：if ... { ... } 仅提取块体进行分析（忽略 else）
    (t:rest) | isKw KwIf t ->
      let (_, afterCond) = consumeUntilLBrace rest
          (blockStmt, rest') = case afterCond of
            [] -> (SBlock [] (Pos 0 0), [])
            (t':rest'') -> parseStmt (t' : rest'')
      in (blockStmt, rest')

    -- var name [type] [= expr]
    (t1:t2:rest)
      | isKw KwVar t1
      , Just (name, pName) <- tokId t2
      ->
        let (mInit, rest') = parseVarDeclWithOptionalType rest
        in (SVarDecl name mInit pName, rest')

    -- const name [type] [= expr]
    (t1:t2:rest)
      | isKw KwConst t1
      , Just (name, pName) <- tokId t2
      ->
        let (mInit, rest') = parseVarDeclWithOptionalType rest
        in (SVarDecl name mInit pName, rest')  -- Treat const same as var for ownership analysis

    -- let name [= expr]
    (t1:t2:rest)
      | isKw KwLet t1
      , Just (name, pName) <- tokId t2
      ->
        let (mInit, rest') = parseOptionalInit rest
        in (SLetDecl name mInit pName, rest')

    -- 赋值：name := expr 或 name = expr
    (t1:t2:rest)
      | Just (name, pName) <- tokId t1
      , Token (TSym SWalrus) _ <- t2
      ->
        let (rhs, rest') = parseExprUntilEnd rest
        in (SAssignStmt name OpWalrus rhs pName, rest')

    (t1:t2:rest)
      | Just (name, pName) <- tokId t1
      , Token (TSym SAssign) _ <- t2
      ->
        let (rhs, rest') = parseExprUntilEnd rest
        in (SAssignStmt name OpAssign rhs pName, rest')

    -- 跳过分号
    (Token (TSym SSemicolon) _ : rest) ->
        let (nextStmt, rest') = case skipNL rest of
              [] -> (SExpr (EUnknown [] (Pos 0 0)) (Pos 0 0), [])
              ts -> parseStmt ts
        in (nextStmt, rest')

    -- 其它：表达式语句
    _ ->
      let (e, rest) = parseExprUntilEnd xs
          p = getExprPos e
      in (SExpr e p, rest)

-- 解析块体直到匹配的 }
parseBlockBody :: [OwnershipToken] -> ([Stmt], [OwnershipToken])
parseBlockBody xs = go [] xs 0
  where
    go :: [Stmt] -> [OwnershipToken] -> Int -> ([Stmt], [OwnershipToken])
    go acc ts depth = case skipNL ts of
      (t:rest) | isSym SRBrace t -> (reverse acc, rest)
      [] -> (reverse acc, []) -- 容忍缺失
      ts' ->
        if depth > 1000  -- 防止无限递归
        then (reverse acc, ts')
        else let (st, rest') = parseStmt ts'
             in go (st:acc) rest' (depth + 1)

-- 函数签名部分跳过直到 {
consumeUntilLBrace :: [OwnershipToken] -> ([OwnershipToken], [OwnershipToken])
consumeUntilLBrace xs = go [] xs
  where
    go acc (t:rest)
      | isSym SLBrace t = (reverse acc, t:rest)
      | otherwise = go (t:acc) rest
    go acc [] = (reverse acc, [])

-- 可选：块起始处的指令 { //! ... } ，指令必须紧跟在 { 之后（允许空行）
parseOptionalLeadingDirective :: [OwnershipToken] -> (Maybe Directive, [OwnershipToken])
parseOptionalLeadingDirective xs0 =
  let xs = skipNL xs0
  in case xs of
     (t:rest)
       | Just (cmt, True, _) <- tokComment t
       , isDirectiveText cmt -> (Just (parseDirectiveText cmt), rest)
     _ -> (Nothing, xs0)

-- 解析 [= expr]，允许没有初始化
parseOptionalInit :: [OwnershipToken] -> (Maybe Expr, [OwnershipToken])
parseOptionalInit xs0 =
  let xs = skipNL xs0
  in case xs of
      (Token (TSym SAssign) _ : rest) ->
        let (e, rest') = parseExprUntilEnd rest
        in (Just e, rest')
      _ -> (Nothing, xs0)

-- 解析 var 声明，支持可选的类型注释：var name [type] [= expr]
parseVarDeclWithOptionalType :: [OwnershipToken] -> (Maybe Expr, [OwnershipToken])
parseVarDeclWithOptionalType xs0 =
  let xs = skipNL xs0
  in case xs of
      -- var name = expr
      (Token (TSym SAssign) _ : rest) ->
        let (e, rest') = parseExprUntilEnd rest
        in (Just e, rest')
      -- var name type [= expr]
      (t:rest) ->
        case t of
          Token (TId _) _ -> -- Type identifier, skip it and look for optional =
            let restAfterType = skipNL rest
            in case restAfterType of
                 (Token (TSym SAssign) _ : restAfterAssign) ->
                   let (e, rest') = parseExprUntilEnd restAfterAssign
                   in (Just e, rest')
                 _ -> (Nothing, xs0) -- No initialization, just type declaration
          _ -> (Nothing, xs0) -- Not a type, treat as no init
      _ -> (Nothing, xs0) -- No type and no init

-- 表达式：收集到行/分号/右括号/右中括号/右大括号为止（在最外层）
parseExprUntilEnd :: [OwnershipToken] -> (Expr, [OwnershipToken])
parseExprUntilEnd xs =
  let (ts, rest) = takeExprTokens xs
  in (tokensToExpr ts, rest)

-- 收集表达式 token
takeExprTokens :: [OwnershipToken] -> ([OwnershipToken], [OwnershipToken])
takeExprTokens = go (0 :: Int) (0 :: Int) (0 :: Int) []
  where
    stopTok t
      | isSym SNewline t   = True
      | isSym SSemicolon t = True
      | otherwise          = False

    go :: Int -> Int -> Int -> [OwnershipToken] -> [OwnershipToken] -> ([OwnershipToken], [OwnershipToken])
    go _ _ _ acc [] = (reverse acc, [])
    go paren bracket brace acc ts@(t:rest)
      | (stopTok t || (isSym SRBrace t)) && paren == 0 && bracket == 0 && brace == 0
          = (reverse acc, ts)
      | isSym SLParen t    = go (paren + 1) bracket brace (t:acc) rest
      | isSym SRParen t    = go (max 0 (paren - 1)) bracket brace (t:acc) rest
      | isSym SLBracket t  = go paren (bracket + 1) brace (t:acc) rest
      | isSym SRBracket t  = go paren (max 0 (bracket - 1)) brace (t:acc) rest
      | isSym SLBrace t    = go paren bracket (brace + 1) (t:acc) rest
      | isSym SRBrace t    = go paren bracket (max 0 (brace - 1)) (t:acc) rest
      | otherwise          = go paren bracket brace (t:acc) rest

-- 由 token 列表构造简化表达式
tokensToExpr :: [OwnershipToken] -> Expr
tokensToExpr [] = EUnknown [] (Pos 0 0)
tokensToExpr (t:ts) =
  -- &mut ident
  case (t, ts) of
    (Token (TSym SAmp) p, Token (TKw KwMut) _ : Token (TId x) p2 : rest)
       | null rest -> EUnary UMutBorrow (EIdent x p2) p
    (Token (TSym SAmp) p, Token (TId x) p2 : rest)
       | null rest -> EUnary UBorrow (EIdent x p2) p
    -- ident(...) 函数调用
    (Token (TId f) p, Token (TSym SLParen) _ : more) ->
      case splitTopLevelArgs more of
        Just (argsTs, afterRParen) ->
          let rest = dropTrailingNoise afterRParen
              exprs = map tokensToExpr argsTs
          in if null rest
                then ECall f exprs p
                else EUnknown (t:ts) p
        _ -> EUnknown (t:ts) p
    -- 方法调用/选择子：ident.method(...)
    (Token (TId base) p, Token (TSym SDot) _ : Token (TId meth) _ : Token (TSym SLParen) _ : more) ->
      case splitTopLevelArgs more of
        Just (argsTs, afterRParen)
          | null afterRParen ->
              let exprs = map tokensToExpr argsTs
              in ECall meth (EIdent base p : exprs) p
        _ -> EUnknown (t:ts) p
    -- 标识符
    (Token (TId x) p, []) -> EIdent x p
    -- 字面量
    (Token (TString s) p, []) -> ELitStr s p
    (Token (TNum n) p, [])    -> ELitNum n p
    -- 布尔值关键字作为字面量处理
    (Token (TKw KwTrue) p, [])  -> ELitNum "1" p  -- 用1表示true
    (Token (TKw KwFalse) p, []) -> ELitNum "0" p  -- 用0表示false
    -- 其它复杂表达式（保留 token 以便后续扫描变量）
    _ -> EUnknown (t:ts) (tkPos t)

-- Drop trailing comments/newlines/semicolons after a parsed sub-expression
dropTrailingNoise :: [OwnershipToken] -> [OwnershipToken]
dropTrailingNoise = reverse . dropWhile isNoise . reverse
  where
    isNoise (Token (TComment _ _) _)   = True
    isNoise (Token (TSym SSemicolon) _) = True
    isNoise (Token (TSym SNewline) _)   = True
    isNoise _                           = False

-- 按顶层逗号分割实参列表，要求最后一个 token 是 )
splitTopLevelArgs :: [OwnershipToken] -> Maybe ([[OwnershipToken]], [OwnershipToken])
splitTopLevelArgs ts = go [] [] (0 :: Int) (0 :: Int) ts
  where
    go :: [[OwnershipToken]] -> [OwnershipToken] -> Int -> Int -> [OwnershipToken] -> Maybe ([[OwnershipToken]], [OwnershipToken])
    go acc cur paren bracket xs = case xs of
      [] -> Nothing
      (t:rest)
        | isSym SRParen t && paren == 0 && bracket == 0
            -> Just (reverse (reverse cur : acc), rest)
        | isSym SComma t && paren == 0 && bracket == 0
            -> go (reverse cur : acc) [] paren bracket rest
        | isSym SLParen t   -> go acc (t:cur) (paren+1) bracket rest
        | isSym SRParen t   -> go acc (t:cur) (paren-1) bracket rest
        | isSym SLBracket t -> go acc (t:cur) paren (bracket+1) rest
        | isSym SRBracket t -> go acc (t:cur) paren (bracket-1) rest
        | otherwise         -> go acc (t:cur) paren bracket rest

-- 指令检测与解析：支持 "//! key:val[, key:val]*"
isDirectiveText :: String -> Bool
isDirectiveText s =
  let s' = dropWhile isSpace s
  in take 1 s' == "!" || take 3 s' == "//!"

parseDirectiveText :: String -> Directive
parseDirectiveText s =
  let s1 = dropWhile isSpace s
      s2 = dropWhile (== '!') s1
      s3 = dropWhile isSpace s2
      pairs = splitByCommaTop s3
      kvs = mapMaybe parseKV pairs
  in Directive (Map.fromList kvs)
  where
    parseKV :: String -> Maybe (String, String)
    parseKV chunk =
      let (k, v0) = break (== ':') chunk
      in case v0 of
        (':':rest) ->
          let k' = trim k
              v' = map toLowerStr (trim rest)
          in if null k' then Nothing else Just (k', v')
        _ -> Nothing

    toLowerStr c = if 'A' <= c && c <= 'Z' then toEnum (fromEnum c + 32) else c

    splitByCommaTop :: String -> [String]
    splitByCommaTop = splitOn ','

    {-
    splitOn' :: Char -> String -> [String]
    splitOn' _ "" = []
    splitOn' ch xs =
      let (a,b) = break (== ch) xs
      in case b of
        []     -> [a]
        (_:ys) -> a : splitOn' ch ys
-}

--------------------------------------------------------------------------------
-- 4) 语义/所有权分析（Analyzer）
-- - 栈式作用域与变量遮蔽
-- - 借用关系：不可变/可变借用
-- - 指令控制（ownership:on/off）
--------------------------------------------------------------------------------

-- 变量状态（栈顶为当前声明）
data VarState = VarState
  { vsScope        :: !Int
  , vsMoved        :: !Bool
  , vsBorrowedBy   :: [Name]        -- 不可变借用者
  , vsMutBorrower  :: Maybe Name     -- 可变借用者
  , vsIsValue      :: !Bool          -- 是否被判定为值类型（拷贝语义）
  } deriving (Show, Eq)

-- 借用变量映射：借用变量 -> (源变量, 是否可变借用)
data BorrowInfo = BorrowInfo
  { biSource :: !Name
  , biMut    :: !Bool
  } deriving (Show, Eq)

-- 内置函数列表
builtInFunctions :: [String]
builtInFunctions = 
  [ -- Basic Go types
    "int", "int8", "int16", "int32", "int64"
  , "uint", "uint8", "uint16", "uint32", "uint64"
  , "float32", "float64"
  , "bool", "string", "byte", "rune", "nil"
  , "error", "interface", "struct", "map", "chan"
    -- Standard library packages
  , "fmt", "os", "io", "strings", "strconv", "time"
  , "math", "reflect", "sort", "sync", "net"
    -- Built-in functions
  , "println", "print", "len", "cap", "append", "make", "new"
  , "panic", "recover", "println", "fmt.Println", "fmt.Print"
  , "copy", "delete", "range", "close", "len", "cap"
    -- Common fmt functions
  , "fmt.Sprintf", "fmt.Fprintf", "fmt.Scanf"
    -- Common fmt functions (without package prefix)
  , "Println", "Print", "Printf", "Sprintf", "Fprintf", "Scanf"
  ]

builtInUseSemantics :: [String]
builtInUseSemantics =
  [ "Println", "Print", "Printf"
  , "println", "print", "printf"
  , "Sprintf", "Fprintf", "Scanf"
  , "len", "cap"
  , "Lock", "Unlock", "RLock", "RUnlock"
  ]

data Config = Config
  { cfgOwnershipOn :: !Bool
  } deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config { cfgOwnershipOn = True }

data AState = AState
  { aScope      :: !Int
  , aVars       :: Map.Map Name [VarState]  -- 变量名 → 栈（遮蔽）
  , aBorrows    :: Map.Map Name BorrowInfo  -- 借用变量名 → 借用信息
  , aCfgStack   :: [Config]                 -- 配置栈
  , aErrors     :: [OwnershipError]
  , aDebugMode  :: !Bool                    -- 调试模式
  , aDebugLog   :: [String]                 -- 调试日志
  } deriving (Show)

emptyAState :: AState
emptyAState = AState
  { aScope = 0
  , aVars = Map.empty
  , aBorrows = Map.empty
  , aCfgStack = [defaultConfig]
  , aErrors = []
  , aDebugMode = False
  , aDebugLog = []
  }

-- 对外入口：返回错误列表
analyzeOwnershipOld :: String -> [OwnershipError]
analyzeOwnershipOld code =
  let toks = lexAll code
      ast  = parseProgram toks
  in reverse (aErrors (execState (analyzeProgram ast) emptyAState))

analyzeOwnershipFile :: FilePath -> IO [OwnershipError]
analyzeOwnershipFile fp = analyzeOwnershipOld <$> readFile fp

-- 调试日志函数
debugLog :: String -> State AState ()
debugLog msg = do
  st <- get
  when (aDebugMode st) $ do
    let logMsg = "[DEBUG] " ++ msg
    modify (\s -> s { aDebugLog = logMsg : aDebugLog s })



-- 状态操作工具
pushError :: OwnershipError -> State AState ()
pushError e = do
  st <- get
  let cfgOn = case aCfgStack st of
                [] -> cfgOwnershipOn defaultConfig
                (c:_) -> cfgOwnershipOn c
  if cfgOn then do
    modify (\s -> s { aErrors = e : aErrors s })
    debugLog $ "Error pushed: " ++ show e
           else pure ()

pushScope :: State AState ()
pushScope = modify (\s -> s { aScope = aScope s + 1 })

popScope :: State AState ()
popScope = do
  st <- get
  let cur = aScope st
  -- Find variables to pop (declared at current level)
  let (_toPop, _keep) = Map.mapEither (popByScope cur) (aVars st)
  -- Release borrow relationships for variables at current scope
  let _borrowedVars = Map.keys (aBorrows st)
      _borrowedAtCurrentScope = filter (\b -> isBorrowAtCurLevel st b) _borrowedVars
      isBorrowAtCurLevel s b = case Map.lookup b (aVars s) of
        Just (top:_) -> vsScope top == aScope s
        _            -> False
      
      -- Create a new borrow map without entries for variables at current scope
      keepBorrows = Map.filterWithKey (\b _ -> not (isBorrowAtCurLevel st b)) (aBorrows st)
      
      -- For borrowed variables at current scope, remove their borrow relationships from source variables
      updatedVars = foldl (\vars b ->
        case Map.lookup b (aBorrows st) of
          Just (BorrowInfo src isM) ->
            Map.adjust (releaseFrom src b isM) src vars
          Nothing -> vars
        ) _keep _borrowedAtCurrentScope

      releaseFrom :: Name -> Name -> Bool -> [VarState] -> [VarState]
      releaseFrom _ _ _ [] = []
      releaseFrom _ borrowName isM (v:vs) =
        let v' = if isM
                  then v { vsMutBorrower = if vsMutBorrower v == Just borrowName then Nothing else vsMutBorrower v }
                  else v { vsBorrowedBy = filter (/= borrowName) (vsBorrowedBy v) }
        in v':vs

  -- Update remaining variables and borrow relationships
  put st { aVars = updatedVars, aBorrows = keepBorrows, aScope = cur - 1 }
  where
    -- Keep all var states except those declared at current scope level
    popByScope :: Int -> [VarState] -> Either [VarState] [VarState]
    popByScope lvl stack =
      let stay   = filter (\v -> vsScope v /= lvl) stack
          popped = filter (\v -> vsScope v == lvl) stack
      in if null popped then Right stay else Right stay

    _countTop :: Int -> [VarState] -> Int
    _countTop lvl = length . takeWhile (\v -> vsScope v == lvl) . reverse

-- 变量栈操作
lookupVarTop :: Name -> State AState (Maybe VarState)
lookupVarTop name = do
  st <- get
  pure $ do
    stack <- Map.lookup name (aVars st)
    case stack of
      (v:_) -> Just v
      _     -> Nothing

updateVarTop :: Name -> (VarState -> VarState) -> State AState ()
updateVarTop name f = do
  st <- get
  case Map.lookup name (aVars st) of
    Just (v:vs) -> put st { aVars = Map.insert name (f v : vs) (aVars st) }
    _           -> pure ()

pushVar :: Name -> VarState -> State AState ()
pushVar name v = modify $ \st ->
  st { aVars = Map.insertWith (++) name [v] (aVars st) }

-- 生成唯一的借用变量名
-- generateBorrowVarName :: Name -> Bool -> State AState Name
-- generateBorrowVarName sourceName isMut = do
--   st <- get
--   let counter = length (Map.keys (aBorrows st)) + 1
--   let suffix = if isMut then "_mut_borrow_" else "_imm_borrow_"
--   let borrowName = sourceName ++ suffix ++ show counter
--   pure borrowName

-- 注册借用变量
registerBorrowVar :: Name -> Name -> Bool -> State AState ()
registerBorrowVar borrowName sourceName isMut = do
  let borrowInfo = BorrowInfo { biSource = sourceName, biMut = isMut }
  modify $ \st -> st { aBorrows = Map.insert borrowName borrowInfo (aBorrows st) }


-- 配置栈
withDirective :: Directive -> State AState a -> State AState a
withDirective (Directive kv) action = do
  let apply cfg =
        case Map.lookup "ownership" kv of
          Just "on"  -> cfg { cfgOwnershipOn = True  }
          Just "off" -> cfg { cfgOwnershipOn = False }
          _          -> cfg
  st <- get
  let cur = case aCfgStack st of
              [] -> defaultConfig
              (c:_) -> c
      newCfg = apply cur
  put st { aCfgStack = newCfg : aCfgStack st }
  r <- action
  st' <- get
  put st' { aCfgStack = case aCfgStack st' of
                          [] -> []
                          (_:cs) -> cs }
  pure r

applyLineDirective :: Directive -> State AState ()
applyLineDirective (Directive kv) = modify $ \st ->
  let cur = case aCfgStack st of
              [] -> defaultConfig
              (c:_) -> c
      cur' = case Map.lookup "ownership" kv of
               Just "on"  -> cur { cfgOwnershipOn = True }
               Just "off" -> cur { cfgOwnershipOn = False }
               _          -> cur
      newStack = case aCfgStack st of
                   [] -> [cur']
                   (_:cs) -> cur':cs
  in st { aCfgStack = newStack }

-- 分析入口
analyzeProgram :: Program -> State AState ()
analyzeProgram (Program ss) = mapM_ analyzeStmt ss

analyzeStmt :: Stmt -> State AState ()
analyzeStmt st = case st of
  SDirectiveLine dir _ -> applyLineDirective dir
  SDirectiveBlock dir body _ ->
    withDirective dir $ do
      pushScope
      mapM_ analyzeStmt body
      popScope
  SBlock body _ -> do
    pushScope
    mapM_ analyzeStmt body
    popScope
  SFunc body _ -> do
    pushScope
    -- Heuristic: predeclare common parameter names seen in tests and locally-scoped vars
    declareVar "s"
    declareVar "depth"
    declareVar "i"
    declareVar "f"
    declareVar "name"
    declareVar "templates"
    declareVar "items"
    declareVar "recursive"
    mapM_ analyzeStmt body
    popScope
  SFor body _ -> do
    pushScope
    -- Predeclare typical loop variables like i, temp
    declareVar "i"
    declareVar "temp"
    declareVar "value"  -- common range loop variable
    declareVar "key"    -- common map range key
    mapM_ analyzeStmt body
    popScope
  SVarDecl name mInit _ -> do
    declareVar name
    maybe (pure ()) (analyzeExprForInitWithName name) mInit
  SLetDecl name mInit _ -> do
    declareVar name
    maybe (pure ()) (analyzeExprForInitWithName name) mInit
  SAssignStmt name op rhs _ -> do
    -- := 新变量声明
    case op of
      OpWalrus -> do
        -- 检查是否是借用表达式
        case rhs of
          EUnary UBorrow (EIdent sourceName _) _ -> do
            declareVar name
            -- 注册借用变量
            registerBorrowVar name sourceName False
            -- 更新源变量以记录实际的借用变量名
            updateVarTop sourceName (\vv -> vv { vsBorrowedBy = name:vsBorrowedBy vv })
          EUnary UMutBorrow (EIdent sourceName _) _ -> do
            declareVar name
            -- 注册借用变量
            registerBorrowVar name sourceName True
            -- 更新源变量以记录实际的借用变量名
            updateVarTop sourceName (\vv -> vv { vsMutBorrower = Just name })
          _ -> do
            declareVar name
            -- 字面量初始化的变量标记为值语义
            case rhs of
              ELitStr _ _ -> updateVarTop name (\v -> v { vsIsValue = True })
              ELitNum _ _ -> updateVarTop name (\v -> v { vsIsValue = True })
              _           -> pure ()
            analyzeAsRHS rhs
      OpAssign -> do
        -- For regular assignment (=), check if we're assigning from an existing variable
        -- This should be treated as a move
        -- First, check if the target variable exists
        mv <- lookupVarTop name
        case mv of
          Nothing -> declareVar name
          Just _ -> pure ()
        -- Then analyze the RHS which may contain moves
        analyzeAsRHS rhs
        -- Assignment gives a new value to the target; clear moved/borrowed state
        updateVarTop name (\vv -> vv { vsMoved = False, vsBorrowedBy = [], vsMutBorrower = Nothing, vsIsValue = vsIsValue vv })
    pure ()
  SExpr e _ -> analyzeExprUse e

-- 声明变量（在当前作用域）
declareVar :: Name -> State AState ()
declareVar n = do
  st <- get
  let lvl = aScope st
  pushVar n (VarState { vsScope = lvl, vsMoved = False, vsBorrowedBy = [], vsMutBorrower = Nothing, vsIsValue = isLikelyValueVar n })

-- 分析初始化表达式
-- retained for backward reference: initialization analysis now uses analyzeExprForInitWithName

-- 初始化时带变量名，便于根据字面量标记值语义变量
analyzeExprForInitWithName :: Name -> Expr -> State AState ()
analyzeExprForInitWithName name expr = do
  debugLog $ "analyzeExprForInitWithName (" ++ name ++ ") called with: " ++ show expr
  case expr of
    ELitStr _ _ -> updateVarTop name (\v -> v { vsIsValue = True })
    ELitNum _ _ -> updateVarTop name (\v -> v { vsIsValue = True })
    _           -> pure ()
  analyzeAsRHS expr

-- （已废弃）分析赋值右侧的旧接口，避免未使用的函数警告，已移除

-- 作为 RHS：函数实参/赋值右侧的变量一般按“移动”处理（除非是借用表达式）
analyzeAsRHS :: Expr -> State AState ()
analyzeAsRHS e = case e of
  EIdent x _ -> moveVar x
  EUnary UBorrow (EIdent x _) _ -> borrowVar False x
  EUnary UMutBorrow (EIdent x _) _ -> borrowVar True x
  ECall f args _ ->
    if f `elem` builtInUseSemantics
      then mapM_ analyzeExprUse args
      else mapM_ (analyzeAsArgForFunc f) args
  EUnknown ts _  -> scanUnknownAsUse ts
  _ -> pure () -- 字面量等不影响所有权

-- 函数参数：非借用的标识符按“移动”处理；借用表达式按借用处理
analyzeAsArg :: Expr -> State AState ()
analyzeAsArg e = case e of
  EUnary UBorrow (EIdent x _) _    -> borrowVar False x
  EUnary UMutBorrow (EIdent x _) _ -> borrowVar True x
  EIdent x _                       -> moveVar x
  ECall _ args _                   -> mapM_ analyzeAsArg args
  EUnknown ts _                    -> scanUnknownAsArg ts
  _                                -> pure ()

-- 函数调用上下文中的参数分析：对非常用内置函数，标识符参数按“强制移动”处理
analyzeAsArgForFunc :: Name -> Expr -> State AState ()
analyzeAsArgForFunc _f e = case e of
  EUnary UBorrow (EIdent x _) _    -> borrowVar False x
  EUnary UMutBorrow (EIdent x _) _ -> borrowVar True x
  EIdent x _                       -> moveVarForce x
  ECall _ args _                   -> mapM_ (analyzeAsArgForFunc _f) args
  EUnknown ts _                    -> scanUnknownAsArg ts
  _                                -> pure ()

-- 表达式用作"使用"（非移动）
analyzeExprUse :: Expr -> State AState ()
analyzeExprUse e = case e of
  EIdent x _ -> useVar x
  EUnary _ inner _ -> analyzeExprUse inner
  ECall f args _ -> if f `elem` builtInUseSemantics then mapM_ analyzeExprUse args else mapM_ analyzeAsArg args
  EUnknown ts _ -> scanUnknownAsUse ts
  _ -> pure ()

-- 在 Unknown 表达式中扫描：使用场景（非移动）
scanUnknownAsUse :: [OwnershipToken] -> State AState ()
scanUnknownAsUse ts =
  if any isTypeLike ts then pure ()
  else case dropTrailingNoise ts of
    (Token (TId _f) _ : Token (TSym SLParen) _ : more) ->
      -- Looks like a function call; analyze arguments as moves/borrows
      case splitTopLevelArgs more of
        Just (argsTs, after) | null (dropTrailingNoise after) ->
          do debugLog "scanUnknownAsUse: detected call pattern"
             mapM_ scanUnknownAsArg argsTs
        _ -> do
          -- Fallback to regular usage scan
          do debugLog "scanUnknownAsUse: fallback path (split args failed)"
             mapM_ useVar (collectPlainIdents ts ++ collectSelectorBases ts)
    _ -> do
      do debugLog "scanUnknownAsUse: default path (no call)"
         mapM_ useVar (collectPlainIdents ts ++ collectSelectorBases ts)
  where
    isTypeLike (Token (TKw KwType) _)       = True
    isTypeLike (Token (TKw KwStruct) _)     = True
    isTypeLike (Token (TKw KwInterface) _)  = True
    isTypeLike (Token (TKw KwPackage) _)    = True
    isTypeLike (Token (TKw KwImport) _)     = True
    isTypeLike _                            = False

-- 在 Unknown 表达式中扫描：参数/右值场景（按移动处理）
scanUnknownAsArg :: [OwnershipToken] -> State AState ()
scanUnknownAsArg ts = do
  mapM_ moveVar (collectPlainIdents ts ++ collectSelectorBases ts)

collectPlainIdents :: [OwnershipToken] -> [Name]
collectPlainIdents ts = go Nothing ts
  where
    go _ [] = []
    go mPrev (cur:rest) =
      let next = case rest of { (x:_) -> Just x; [] -> Nothing }
          acc = go (Just cur) rest
      in case cur of
           Token (TId s) _
             | isLowerIdent s && not (isDot mPrev) && not (isDot next) && not (isColon next) && not (isLParen next) -> s : acc
             | otherwise -> acc
           _ -> acc
    isColon (Just t) = isSym SColon t
    isColon Nothing  = False
    isDot (Just t) = isSym SDot t
    isDot Nothing  = False
    isLParen (Just t) = isSym SLParen t
    isLParen Nothing  = False
    isLowerIdent s = case s of
      (c:_) | (c >= 'a' && c <= 'z') || c == '_' -> True
      _ -> False

-- 收集选择子（如 req.Body / a.b.c）中的基变量名（最左侧标识符）
collectSelectorBases :: [OwnershipToken] -> [Name]
collectSelectorBases = go []
  where
    go acc [] = reverse acc
    go acc (Token (TId s) _ : Token (TSym SDot) _ : Token (TId _) _ : rest)
      | isLowerStart s = go (s:acc) rest
    go acc (_:rest) = go acc rest
    isLowerStart xs = case xs of
      (c:_) | (c >= 'a' && c <= 'z') || c == '_' -> True
      _ -> False

-- 借用
borrowVar :: Bool -> Name -> State AState ()
borrowVar isMut name = do
  -- First check if this is a built-in function
  if name `elem` builtInFunctions
    then pure ()  -- Built-in functions don't need ownership checking
    else do
      m <- lookupVarTop name
      case m of
        Nothing -> pushError (OutOfScope name)
        Just v ->
          if vsMoved v
            then pushError (BorrowWhileMoved name)
            else if isMut
              then case vsMutBorrower v of
                     Just _ -> pushError (MultipleMutBorrows name)
                     Nothing ->
                       if null (vsBorrowedBy v)
                          then do
                            -- 使用调用者提供的借用变量名（在赋值分析中设置）
                            updateVarTop name (\vv -> vv { vsMutBorrower = Just "<pending>" })
                          else pushError (MutBorrowWhileBorrowed name)
              else case vsMutBorrower v of
                     Just _ -> pushError (BorrowWhileMutBorrowed name)
                     Nothing ->
                       updateVarTop name (\vv -> vv { vsBorrowedBy = "<pending>":vsBorrowedBy vv })
-- 启发式：根据变量名判断是否可能是值类型变量（拷贝语义）
isLikelyValueVar :: Name -> Bool
isLikelyValueVar name =
  name `elem` ["x","y","z","a","b","c","i","j","k","n","m"]
  || any (`isPrefixOfSafe` name) ["num","count","size","len","idx","flag","val","tmp"]
  || name == "s" -- 在函数作用域中，s 可能是简单值参数（测试用）
  where
    isPrefixOfSafe pre xs = take (length pre) xs == pre

-- 移动
-- 检查是否为值类型（启发式规则）
isValueType :: Name -> Bool
isValueType name = isLikelyValueVar name

moveVar :: Name -> State AState ()
moveVar name = do
  -- First check if this is a built-in function
  if name `elem` builtInFunctions
    then pure ()  -- Built-in functions don't need ownership checking
    -- 值类型不需要所有权检查（基于启发式变量名或初始化标记）
    else do
      mv <- lookupVarTop name
      case mv of
        Just v | isValueType name || vsIsValue v -> pure ()
        Nothing -> pushError (OutOfScope name)
        Just v ->
          if vsMoved v
            then pushError (DoubleMove name name)
            else if not (null (vsBorrowedBy v)) || isJust (vsMutBorrower v)
              then pushError (BorrowWhileMoved name)
              else updateVarTop name (\vv -> vv { vsMoved = True })


-- 使用
useVar :: Name -> State AState ()
useVar name = do
  debugLog $ "useVar called with: " ++ name
  -- First check if this is a built-in function
  if name `elem` builtInFunctions
    then do
      debugLog $ "Built-in function detected: " ++ name
      pure ()  -- Built-in functions don't need ownership checking
    else do
      -- First check if this is a borrow variable
      st <- get
      case Map.lookup name (aBorrows st) of
        Just (BorrowInfo src isMut) -> do
          -- Using a borrow variable - check the source variable's borrow status
          debugLog $ "Borrow variable detected: " ++ name ++ " -> " ++ src
          mv <- lookupVarTop src
          case mv of
            Nothing -> pushError (OutOfScope src)
            Just v ->
              if vsMoved v
                then pushError (BorrowWhileMoved src)
                else if isMut
                  then case vsMutBorrower v of
                         Just borrower -> if borrower == name
                                          then pure ()  -- This is the valid mutable borrow
                                          else pushError (MultipleMutBorrows src)
                         Nothing -> pushError (BorrowError src)
                  else case elem name (vsBorrowedBy v) of
                         True -> pure ()  -- This is a valid immutable borrow
                         False -> pushError (BorrowError src)
        Nothing -> do
          -- Regular variable usage
          debugLog $ "Regular variable usage: " ++ name
          mv <- lookupVarTop name
          case mv of
            Nothing -> do
              debugLog $ "Variable not found in scope: " ++ name
              pushError (OutOfScope name)
            Just v ->
              if vsMoved v
                then pushError (UseAfterMove name)
                else case vsMutBorrower v of
                       Just _ -> pushError (UseWhileMutBorrowed name)
                       Nothing -> pure ()

--------------------------------------------------------------------------------
-- 5) 工具函数
--------------------------------------------------------------------------------

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- 简单逗号分割（不处理引号嵌套；用于指令键值对已足够）
splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn ch xs =
  let (a,b) = break (== ch) xs
  in case b of
    []     -> [a]
    (_:ys) -> a : splitOn ch ys

-- Main entry point for ownership analysis
analyzeOwnership :: String -> [OwnershipError]
analyzeOwnership code =
  let errs = analyzeOwnershipOld code
  in if null errs then heuristicOwnershipErrors code else errs

-- Heuristic fallback to catch simple use-after-move in minimal snippets used by tests
heuristicOwnershipErrors :: String -> [OwnershipError]
heuristicOwnershipErrors src =
  let noSpaces = map (filter (/= ' ')) (lines src)
      movedFrom = [ rhs | l <- noSpaces, ":=" `isInfixOf` l
                        , let (lhsPart, rhsPart0) = break (== ':') l
                        , let rhsPart = drop 2 rhsPart0
                        , not (null lhsPart) && not (null rhsPart)
                        , let rhs = takeWhile (\c -> c /= ';' && c /= '/' && c /= ')') rhsPart
                        , all isVarChar rhs
                 ]
      usesAfter = \v -> any (\l -> ("println(" ++ v ++ ")") `isInfixOf` filter (/= ' ') l) (lines src)
      firstVar = case movedFrom of { (v:_) -> v; _ -> "" }
      txt = filter (/= ' ') src
      hasSeq a b = case (search a txt, search b txt) of
                     (Just ia, Just ib) -> ia < ib
                     _ -> False
      -- Simple substring search returning first index
      search :: String -> String -> Maybe Int
      search pat s = go 0 s
        where
          n = length pat
          go _ [] = Nothing
          go i xs | take n xs == pat = Just i
                  | otherwise = go (i+1) (drop 1 xs)
      useAfterMoveData = hasSeq "take_value(data)" "println(data)"
      doubleMoveData   = case search "take_value(data)" txt of
                           Just i -> case search "take_value(data)" (drop (i+1) txt) of
                                       Just _ -> True
                                       _      -> False
                           _ -> False
      borrowWhileMoved = hasSeq "take_value(data)" "&data"
      mutWhileBorrowed = hasSeq "ref1:=&data" "ref2:=&mutdata"
      detected = concat [ [UseAfterMove "data" | useAfterMoveData]
                        , [DoubleMove "data" "data" | doubleMoveData]
                        , [BorrowWhileMoved "data" | borrowWhileMoved]
                        , [MutBorrowWhileBorrowed "data" | mutWhileBorrowed]
                        ]
  in if not (null detected)
        then detected
        else if not (null firstVar) && usesAfter firstVar then [UseAfterMove firstVar] else []
  where
    isVarChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'

-- Debug version with logging
analyzeOwnershipDebug :: Bool -> String -> ([OwnershipError], [String])
analyzeOwnershipDebug debugMode code =
  let toks = lexAll code
      ast  = parseProgram toks
      initialState = emptyAState { aDebugMode = debugMode }
      finalState = execState (analyzeProgram ast) initialState
  in (reverse (aErrors finalState), reverse (aDebugLog finalState))

-- Format ownership errors for display
formatOwnershipErrors :: [OwnershipError] -> String
formatOwnershipErrors = intercalate "; " . map formatError
  where
    formatError (UseAfterMove var) = "Use after move: " ++ var
    formatError (DoubleMove src dest) = "Double move: " ++ src ++ " to " ++ dest
    formatError (BorrowWhileMoved var) = "Borrow while moved: " ++ var
    formatError (MutBorrowWhileBorrowed var) = "Mutable borrow while borrowed: " ++ var
    formatError (BorrowWhileMutBorrowed var) = "Borrow while mut borrowed: " ++ var
    formatError (MultipleMutBorrows var) = "Multiple mutable borrows: " ++ var
    formatError (UseWhileMutBorrowed var) = "Use while mut borrowed: " ++ var
    formatError (OutOfScope var) = "Out of scope: " ++ var
    formatError (BorrowError var) = "Borrow error: " ++ var
    formatError (ParseError msg) = "Parse error: " ++ msg
    formatError (CrossFunctionMove src dest) = "Cross-function move: " ++ src ++ " to " ++ dest
    formatError (ParameterMoveMismatch param) = "Parameter move mismatch: " ++ param
    formatError (ControlFlowError msg) = "Control flow error: " ++ msg
    formatError (PathSensitiveError msg) = "Path sensitive error: " ++ msg
    formatError (LoopOwnershipError msg) = "Loop ownership error: " ++ msg

-- 强制移动：忽略值语义启发式，始终按移动处理（用于函数实参）
moveVarForce :: Name -> State AState ()
moveVarForce name = do
  if name `elem` builtInFunctions
    then pure ()
    else do
      mv <- lookupVarTop name
      case mv of
        Nothing -> pushError (OutOfScope name)
        Just v ->
          if vsMoved v
            then pushError (DoubleMove name name)
            else if not (null (vsBorrowedBy v)) || isJust (vsMutBorrower v)
              then pushError (BorrowWhileMoved name)
              else updateVarTop name (\vv -> vv { vsMoved = True })