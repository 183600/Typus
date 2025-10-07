{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- 一个健壮的依赖类型（示例语言）解析器，使用 Megaparsec 实现
-- 特性：
-- - 词法/语法分离：由 Megaparsec 的 space/lexeme/symbol 主动处理空白与注释
-- - 支持 type/func/alias 顶层定义
-- - 支持嵌套泛型的类型引用 TypeRef
-- - 支持 struct { ... } 结构体体，字段 name: TypeRef
-- - 支持 where 子句以及常见约束（==、>、>=、<、<=、len、nonempty、谓词调用）
-- - 多错误收集与错误恢复：一个定义出错不会阻断后续定义的解析
-- - 状态中收集作用域（typeScope）并做重复定义检测
-- - 单文件实现（不依赖外部 Utils）

module DependentTypesParser (
  -- 状态与错误
  DependentTypesParser(..),
  DependentTypeError(..),

  -- 抽象语法树
  TypeRef(..),
  TypeBody(..),
  Field(..),
  TypeParameter(..),
  TypeConstraint(..),
  DependentType(..),

  -- 运行与入口
  DependentParseResult,
  runDependentTypesParser,       -- 解析整个输入，返回所有定义与最终状态
  parseDependentType,            -- 解析并返回第一个顶层定义（若存在）
  parseTypeDeclaration,          -- 单独解析一个 type 定义
  validateDependentTypeSyntax    -- 校验输入，返回收集到的错误（不抛异常）
) where

import Control.Monad (void)
import Data.Char (isAlphaNum)
import qualified Data.List as List

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec, (<?>)
  , MonadParsec(try, eof, lookAhead)
  , anySingle
  , manyTill
  , withRecovery
  , runParser
  , errorBundlePretty
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (char, letterChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L
-- import Debug.Trace (trace)

--------------------------------------------------------------------------------
-- 辅助与通用
--------------------------------------------------------------------------------

-- 内部工具：去空白
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (<= ' ')

-- Parser 类型（直接用 String 流，避免引入 text 依赖）
type Parser = Parsec Void String

-- 空白与注释（--、// 行注释；/* ... */ 块注释）
sc :: Parser ()
sc = L.space
      space1
      (L.skipLineComment "--" MP.<|> L.skipLineComment "//")
      (L.skipBlockComment "/*" "*/")

-- 带空白消费的词法构建工具
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- 一些括号小工具
parens :: Parser a -> Parser a
parens = MP.between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = MP.between (symbol "{") (symbol "}")

angles :: Parser a -> Parser a
angles = MP.between (symbol "<") (symbol ">")

comma :: Parser String
comma = symbol ","

colon :: Parser String
colon = symbol ":"

pipe :: Parser String
pipe = symbol "|"


arrow :: Parser String
arrow = symbol "->"

-- 关键字与标识符
reservedWords :: [String]
reservedWords =
  [ "type", "func", "where", "alias", "struct", "len", "nonempty"
  ]

-- 标识符：首字符字母或下划线，后续字母/数字/下划线；不允许关键字
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> identStart <*> MP.many identChar
    identStart :: Parser Char
    identStart = letterChar MP.<|> char '_'
    identChar :: Parser Char
    identChar  = MP.satisfy (\c -> isAlphaNum c || c == '_')
    check x = if x `elem` reservedWords
              then fail $ "关键字不能作为标识符: " ++ show x
              else return x

-- 解析整型字面量
pInt :: Parser Int
pInt = lexeme L.decimal

-- 在需要"至少一个空白"的地方使用（例如参数名 与 类型之间）

--------------------------------------------------------------------------------
-- AST 定义
--------------------------------------------------------------------------------

-- 依赖类型错误
data DependentTypeError
    = SyntaxError String Int String        -- 消息、行号（若无法获取则置 0）、片段（可空）
    | InvalidTypeSyntax String             -- 例如重复定义
    | MissingConstraint String
    | InvalidParameter String
    | ConstraintParseError String
    | TypeVariableError String
    deriving (Show, Eq)

-- 类型引用，支持嵌套泛型，如 Map<Key, Value<T>>
data TypeRef = TypeRef
  { refName :: String
  , refArgs :: [TypeRef]
  } deriving (Show, Eq)

-- 结构体字段
data Field = Field
  { fieldName :: String
  , fieldType :: TypeRef
  } deriving (Show, Eq)

-- 类型体，目前支持结构体
data TypeBody
  = StructBody [Field]
  deriving (Show, Eq)

-- 类型参数
data TypeParameter = TypeParameter
  { paramName        :: String
  , paramType        :: TypeRef               -- 参数"类型/种类"，默认 int
  , paramConstraints :: [TypeConstraint]
  } deriving (Show, Eq)

-- 类型约束
data TypeConstraint
    = EqualityConstraint String String
    | RangeConstraint String Int Int
    | SizeConstraint String Int
    | NonEmptyConstraint String
    | PredicateConstraint String [String]
    | TypeClassConstraint String TypeRef
    | CustomConstraint String String
    deriving (Show, Eq)

-- 顶层定义
data DependentType
    = TypeDecl String [TypeParameter] TypeBody [TypeConstraint]
    | DependentFunction String [(String, TypeRef)] TypeRef [TypeConstraint]
    | TypeAlias String TypeRef [TypeConstraint]
    deriving (Show, Eq)

-- 辅助函数用于访问记录字段











-- 解析器状态
data DependentTypesParser = DependentTypesParser
  { parserErrors :: [DependentTypeError]
  , typeScope    :: Map String DependentType
  , sourceName   :: String
  } deriving (Show, Eq)

-- 解析结果：返回所有成功的定义 与 最终状态（包含错误集合与作用域）
type DependentParseResult = Either String ([DependentType], DependentTypesParser)

--------------------------------------------------------------------------------
-- 类型引用解析（支持嵌套泛型）
--------------------------------------------------------------------------------

-- 通用类型引用：Simple | Generic<...>
parseTypeReference :: Parser TypeRef
parseTypeReference = do
  name <- identifier
  args <- MP.option [] (angles (parseTypeReference `MP.sepBy1` comma))
  pure $ TypeRef name args

-- 常用的内置类型便捷值
tInt :: TypeRef
tInt = TypeRef "int" []

tVoid :: TypeRef
tVoid = TypeRef "void" []

--------------------------------------------------------------------------------
-- 约束解析
--------------------------------------------------------------------------------

-- 操作符解析
opEq, opGT, opLT, opGE, opLE :: Parser String
opEq = symbol "=="
opGT = symbol ">"
opLT = symbol "<"
opGE = symbol ">="
opLE = symbol "<="

-- 简单值：标识符或数字，最终统一存入 String
simpleVal :: Parser String
simpleVal = (show <$> pInt) MP.<|> identifier

-- 谓词调用：pred(arg1, arg2, ...)
predicateCall :: Parser TypeConstraint
predicateCall = do
  name <- identifier
  args <- parens (simpleVal `MP.sepBy` comma)
  pure $ PredicateConstraint name args

-- len name (op) int
lenConstraint :: Parser TypeConstraint
lenConstraint = do
  _ <- MP.try (symbol "len")
  nm <- identifier
  c <- (   (opEq >> (SizeConstraint nm <$> pInt))
       MP.<|> (opGT >> ((\n -> SizeConstraint nm (n + 1)) <$> pInt))  -- len x > n 等价 len x == n+1（示例）
       ) <?> "len 约束操作（== 或 >）"
  pure c

-- nonempty name
nonemptyConstraint :: Parser TypeConstraint
nonemptyConstraint = do
  _ <- MP.try (symbol "nonempty")
  nm <- identifier
  pure $ NonEmptyConstraint nm

-- 一般比较：x >= 3 / x > 0 / x == y ...
compareConstraint :: Parser TypeConstraint
compareConstraint = do
  nm <- identifier
  let _range low hi = RangeConstraint nm low hi
  c <- (   (opGE >> (RangeConstraint nm <$> pInt <*> pure maxBound))
       MP.<|> (opLE >> (RangeConstraint nm <$> pure minBound <*> pInt))
       MP.<|> (opGT >> (RangeConstraint nm <$> ((+1) <$> pInt) <*> pure maxBound))
       MP.<|> (opLT >> (RangeConstraint nm <$> pure minBound <*> (subtract 1 <$> pInt)))
       MP.<|> (opEq >> (EqualityConstraint nm <$> simpleVal))
       ) <?> "比较约束（==, >, >=, <, <=）"
  pure c

typeClassConstraint :: Parser TypeConstraint
typeClassConstraint = do
  nm <- identifier
  _ <- colon
  cls <- parseTypeReference
  pure $ TypeClassConstraint nm cls

-- 单个约束
parseConstraint :: Parser TypeConstraint
parseConstraint =
      MP.try predicateCall
  MP.<|> MP.try lenConstraint
  MP.<|> MP.try nonemptyConstraint
  MP.<|> MP.try typeClassConstraint
  MP.<|> compareConstraint
  MP.<|> do
        raw <- lexeme $ MP.some (MP.satisfy (\c -> c /= '&' && c /= ',' && c /= '\n' && c /= '\r'))
        pure $ CustomConstraint (trim raw) ""

parseConstraints :: Parser [TypeConstraint]
parseConstraints = parseConstraint `MP.sepBy1` symbol "&"

parseWhereClause :: Parser [TypeConstraint]
parseWhereClause = do
  _ <- symbol "where"
  parseConstraints

--------------------------------------------------------------------------------
-- 类型参数解析
--------------------------------------------------------------------------------

-- 类型参数：name [: TypeRef] [| constraints]
-- 兼容：如果没有 ":"，则默认类型为 int
parseTypeParameter :: Parser TypeParameter
parseTypeParameter = do
  name <- identifier
  mTy <- MP.optional (MP.try (colon *> parseTypeReference))
  mCons <- MP.optional (pipe *> parseConstraints)
  pure $ TypeParameter
    { paramName        = name
    , paramType        = fromMaybe tInt mTy
    , paramConstraints = fromMaybe [] mCons
    }

parseTypeParameterList :: Parser [TypeParameter]
parseTypeParameterList =
  angles (parseTypeParameter `MP.sepBy` comma)

--------------------------------------------------------------------------------
-- 结构体解析
--------------------------------------------------------------------------------

-- 字段：name : TypeRef
parseField :: Parser Field
parseField = do
  nm <- identifier
  _  <- colon
  ty <- parseTypeReference
  pure $ Field nm ty

fieldSep :: Parser ()
fieldSep = void (symbol ",") MP.<|> void (symbol ";")

-- struct { f1: T, f2: U; ... }
parseStructBody :: Parser TypeBody
parseStructBody = do
  _ <- symbol "struct"
  fields <- braces (parseField `MP.sepEndBy` fieldSep)
  pure $ StructBody fields

--------------------------------------------------------------------------------
-- 顶层定义解析
--------------------------------------------------------------------------------

-- type Name [<params>] struct {...} [where ...]
parseTypeDecl :: Parser DependentType
parseTypeDecl = do
  _ <- symbol "type"
  name <- identifier
  params <- MP.option [] (MP.try parseTypeParameterList)
  body <- parseStructBody
  cons <- MP.option [] (MP.try parseWhereClause)
  pure $ TypeDecl name params body cons

-- alias Name = TypeRef [where ...]
parseAlias :: Parser DependentType
parseAlias = do
  _ <- symbol "alias"
  name <- identifier
  _ <- symbol "="
  target <- parseTypeReference
  cons <- MP.option [] (MP.try parseWhereClause)
  pure $ TypeAlias name target cons

-- 函数参数：name [: TypeRef]，若不写类型默认 int
parseFunctionParam :: Parser (String, TypeRef)
parseFunctionParam = do
  nm <- identifier
  mTy <- MP.optional (colon *> parseTypeReference)
  pure (nm, fromMaybe tInt mTy)

-- 跳过函数体（若存在）：平衡大括号
skipBalancedBraces :: Parser ()
skipBalancedBraces = void $ braces (MP.many chunk)
  where
    chunk =
          void (MP.some (MP.satisfy (\c -> c /= '{' && c /= '}' )))
      MP.<|> skipBalancedBraces

-- func Name(params) [-> TypeRef] [where ...] [{ ... }]
parseFuncDecl :: Parser DependentType
parseFuncDecl = do
  _ <- symbol "func"
  name <- identifier
  params <- parens (parseFunctionParam `MP.sepBy` comma)
  ret <- MP.option tVoid (MP.try (arrow *> parseTypeReference))
  cons <- MP.option [] (MP.try parseWhereClause)
  -- 可选函数体
  _ <- MP.optional (MP.try skipBalancedBraces)
  pure $ DependentFunction name params ret cons

-- 顶层定义（无恢复）
parseTopDecl :: Parser DependentType
parseTopDecl =
      MP.try parseTypeDecl
  MP.<|> MP.try parseAlias
  MP.<|> parseFuncDecl

-- 同步到下一个顶层定义的开始（type/func/alias）或 EOF
syncToNextDecl :: Parser ()
syncToNextDecl = do
  let starts = MP.try (lookAhead (symbol "type"))
           MP.<|> MP.try (lookAhead (symbol "func"))
           MP.<|> MP.try (lookAhead (symbol "alias"))
           MP.<|> MP.try (lookAhead (symbol ""))
  _ <- manyTill anySingle starts
  pure ()

-- 带恢复的顶层定义解析：出错时收集错误并跳到下一个定义
parseTopDeclWithRecovery :: Parser (Either DependentTypeError DependentType)
parseTopDeclWithRecovery =
  withRecovery handler (Right <$> parseTopDecl)
  where
    handler :: MP.ParseError String Void -> Parser (Either DependentTypeError DependentType)
    handler _ = do
      -- 记录错误消息；行号可选（此处置 0），片段为空
      syncToNextDecl
      pure $ Left (SyntaxError "Parse error" 0 "")

-- 整个程序：可包含多个定义
parseProgram :: Parser [Either DependentTypeError DependentType]
parseProgram = do
  sc
  decls <- MP.many (parseTopDeclWithRecovery <* sc)
  eof
  pure decls

--------------------------------------------------------------------------------
-- 运行与 API
--------------------------------------------------------------------------------

-- 把成功的定义累进到作用域，同时检测重复定义
buildScope :: [DependentType] -> ([DependentTypeError], Map String DependentType)
buildScope ds =
  List.foldl' go ([], Map.empty) ds
  where
    defName :: DependentType -> String
    defName = \case
      TypeDecl n _ _ _      -> n
      TypeAlias n _ _       -> n
      DependentFunction n _ _ _ -> n

    go (errs, mp) d =
      let n = defName d
      in if Map.member n mp
         then (errs ++ [InvalidTypeSyntax ("重复定义: " ++ n)], mp)
         else (errs, Map.insert n d mp)

-- 解析整个输入，返回所有定义与最终状态
runDependentTypesParser :: String -> DependentParseResult
runDependentTypesParser input =
  case runParser parseProgram "<input>" input of
    Left fatalBundle ->
      Left (errorBundlePretty fatalBundle)
    Right parts ->
      let (errs1, oks) = List.foldl'
             (\(es, ds) r -> case r of
                                Left e  -> (es ++ [e], ds)
                                Right d -> (es, ds ++ [d]))
             ([], [])
             parts
          (errs2, scope) = buildScope oks
          st = DependentTypesParser
                 { parserErrors = errs1 ++ errs2
                 , typeScope    = scope
                 , sourceName   = "<input>"
                 }
      in Right (oks, st)

-- 返回第一个成功的顶层定义（如果没有则报错），并返回最终状态
parseDependentType :: String -> Either String (DependentType, DependentTypesParser)
parseDependentType input = do
  (defs, st) <- runDependentTypesParser input
  case defs of
    (d:_) -> Right (d, st)
    []    -> Left "未找到任何可解析的顶层定义"

-- 仅解析一个 type 定义（不含其他定义）
parseTypeDeclaration :: String -> Either String DependentType
parseTypeDeclaration input =
  case runParser (sc *> parseTypeDecl <* sc <* eof) "<type decl>" input of
    Left e  -> Left (errorBundlePretty e)
    Right d -> Right d

-- 校验输入语法并返回错误集合（不抛异常）
validateDependentTypeSyntax :: String -> [DependentTypeError]
validateDependentTypeSyntax input =
  case runDependentTypesParser input of
    Left fatal -> [SyntaxError fatal 0 input]
    Right (_, st) -> parserErrors st
