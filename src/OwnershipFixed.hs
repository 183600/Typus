{-# LANGUAGE OverloadedStrings #-}
module OwnershipFixed
  ( -- 数据模型
    OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer
  , newOwnershipAnalyzer
  , analyzeOwnership
  , analyzeOwnershipFile
  , formatOwnershipErrors
  , lexAll
  , parseProgram
  ) where

import qualified Data.Map.Strict as Map
import Data.Char (isSpace, isDigit, isAlpha)
import Data.Maybe (isJust, mapMaybe)
import Data.List (intercalate)
import Control.Monad.State
import Control.Monad (zipWithM_)

--------------------------------------------------------------------------------
-- 1) 数据类型与错误类型
--------------------------------------------------------------------------------

-- 所有权类型（示意）
data OwnershipType
  = Owned String      -- 拥有所有权（变量名）
  | Borrowed String   -- 不可变借用（借自谁）
  | MutBorrowed String -- 可变借用（借自谁）
  deriving (Show, Eq)

-- 所有权错误类型
data OwnershipError
  = UseAfterMove String             -- 使用已移动的值
  | DoubleMove String String        -- 重复移动（源→目标）
  | BorrowWhileMoved String         -- 在被移动后借用
  | MutBorrowWhileBorrowed String   -- 已有不可变借用时进行可变借用
  | BorrowWhileMutBorrowed String   -- 已有可变借用时进行不可变借用
  | MultipleMutBorrows String       -- 多个可变借用
  | UseWhileMutBorrowed String      -- 可变借用期间使用原值
  | OutOfScope String               -- 变量越界使用/未声明
  | BorrowError String              -- 借用错误
  | ParseError String               -- 解析错误
  | CrossFunctionMove String String -- 跨函数移动错误
  | ParameterMoveMismatch String    -- 参数移动不匹配
  deriving (Show, Eq)

-- 分析器句柄（占位，保留外部接口）
newtype OwnershipAnalyzer = OwnershipAnalyzer () deriving (Show, Eq)

newOwnershipAnalyzer :: OwnershipAnalyzer
newOwnershipAnalyzer = OwnershipAnalyzer ()

--------------------------------------------------------------------------------
-- 2) 词法分析器（Lexer）
-- - 负责把源码切分为 Token
-- - 处理字符串/字符字面量、单行与多行注释
-- - 产生 Newline token，利于简单的“行终结”判断
-- - 忽略注释中的符号，不影响块/括号嵌套
--------------------------------------------------------------------------------

data Pos = Pos { pLine :: !Int, pCol :: !Int } deriving (Eq, Show)

data Keyword
  = KwVar | KwLet | KwFunc | KwReturn | KwIf | KwElse | KwFor
  | KwPackage | KwImport | KwType | KwStruct | KwInterface | KwConst
  | KwMut
  deriving (Eq, Show)

data Sym
  = SLBrace | SRBrace | SLParen | SRParen | SLBracket | SRBracket
  | SSemicolon | SComma | SColon | SAssign | SWalrus | SAmp | SDot
  | SNewline
  deriving (Eq, Show)

data TokenKind
  = TId String
  | TKw Keyword
  | TSym Sym
  | TString String
  | TNum String
  | TComment String Bool  -- 内容, 是否单行注释
  deriving (Eq, Show)

data Token = Token
  { tkKind :: !TokenKind
  , tkPos  :: !Pos
  } deriving (Eq, Show)

-- 关键字表
kwFromStr :: String -> Maybe Keyword
kwFromStr s = case s of
  "var"     -> Just KwVar
  "let"     -> Just KwLet
  "func"    -> Just KwFunc
  "return"  -> Just KwReturn
  "if"      -> Just KwIf
  "else"    -> Just KwElse
  "for"     -> Just KwFor
  "package" -> Just KwPackage
  "import"  -> Just KwImport
  "type"    -> Just KwType
  "struct"  -> Just KwStruct
  "interface"->Just KwInterface
  "const"   -> Just KwConst
  "mut"     -> Just KwMut
  _         -> Nothing

-- 词法分析入口
lexAll :: String -> [Token]
lexAll = go (Pos 1 1)
  where
    go :: Pos -> String -> [Token]
    go _ [] = []
    go pos s@(c:cs)
      -- 换行
      | c == '\n' =
          Token (TSym SNewline) pos : go (Pos (pLine pos + 1) 1) cs
      -- 空白
      | c == ' ' || c == '\t' || c == '\r' =
          go (bump pos 1) cs
      -- 注释：//...
      | startsWith "//" s =
          let (comment, rest, consumedNL, newPos) = readLineComment pos s
          in Token (TComment comment True) pos : case consumedNL of
                True  -> Token (TSym SNewline) newPos : go newPos rest
                False -> go newPos rest
      -- 注释：/* ... */
      | startsWith "/*" s =
          let (comment, rest, newPos) = readBlockComment pos s
          in Token (TComment comment False) pos : go newPos rest
      -- 字符串
      | c == '"' =
          let (str, rest, newPos) = readString pos cs
          in Token (TString str) pos : go newPos rest
      -- 字符字面量（简化处理）
      | c == '\'' =
          let (ch, rest, newPos) = readChar pos cs
          in Token (TString ch) pos : go newPos rest
      -- 两字符操作符 := 先于 =
      | startsWith ":=" s =
          Token (TSym SWalrus) pos : go (bump pos 2) (drop 2 s)
      -- 单字符符号
      | c == '=' = Token (TSym SAssign)  pos : go (bump pos 1) cs
      | c == '{' = Token (TSym SLBrace)  pos : go (bump pos 1) cs
      | c == '}' = Token (TSym SRBrace)  pos : go (bump pos 1) cs
      | c == '(' = Token (TSym SLParen)  pos : go (bump pos 1) cs
      | c == ')' = Token (TSym SRParen)  pos : go (bump pos 1) cs
      | c == '[' = Token (TSym SLBracket)pos : go (bump pos 1) cs
      | c == ']' = Token (TSym SRBracket)pos : go (bump pos 1) cs
      | c == ';' = Token (TSym SSemicolon)pos: go (bump pos 1) cs
      | c == ',' = Token (TSym SComma)    pos: go (bump pos 1) cs
      | c == ':' = Token (TSym SColon)    pos: go (bump pos 1) cs
      | c == '&' = Token (TSym SAmp)      pos: go (bump pos 1) cs
      | c == '.' = Token (TSym SDot)      pos: go (bump pos 1) cs
      -- 数字
      | isDigit c =
          let (num, rest) = span isNumChar s
              newPos = bump pos (length num)
          in Token (TNum num) pos : go newPos rest
      -- 标识符/关键字
      | isIdentStart c =
          let (ident, rest) = span isIdentChar s
              newPos = bump pos (length ident)
              tk = case kwFromStr ident of
                     Just kw -> TKw kw
                     Nothing -> TId ident
          in Token tk pos : go newPos rest
      -- 其它（未识别的字符，直接跳过）
      | otherwise = go (bump pos 1) cs

    bump (Pos l c) n = Pos l (c + n)
    startsWith pref xs = pref == take (length pref) xs

    isNumChar x = isDigit x || x == '.' || x == '_'
    isIdentStart x = x == '_' || isAlpha x
    isIdentChar x = isIdentStart x || isDigit x

    readString :: Pos -> String -> (String, String, Pos)
    readString p s = goStr [] p s
      where
        goStr acc posN [] = (reverse acc, [], posN)  -- 不完整也尽量收集
        goStr acc posN (x:xs)
          | x == '\\' = case xs of
              (y:ys) -> goStr (y:'\\':acc) (bump posN 2) ys
              []     -> (reverse ('\\':acc), [], bump posN 1)
          | x == '"'  = (reverse acc, xs, bump posN 1)
          | x == '\n' = -- 字符串中断行，仍推进行列
              goStr ('\n':acc) (Pos (pLine posN + 1) 1) xs
          | otherwise = goStr (x:acc) (bump posN 1) xs

    readChar :: Pos -> String -> (String, String, Pos)
    readChar p s = goChr [] p s
      where
        goChr acc posN [] = (reverse acc, [], posN)
        goChr acc posN (x:xs)
          | x == '\\' = case xs of
              (y:ys) -> goChr (y:'\\':acc) (bump posN 2) ys
              []     -> (reverse ('\\':acc), [], bump posN 1)
          | x == '\'' = (reverse acc, xs, bump posN 1)
          | x == '\n' = goChr ('\n':acc) (Pos (pLine posN + 1) 1) xs
          | otherwise = goChr (x:acc) (bump posN 1) xs

    readLineComment :: Pos -> String -> (String, String, Bool, Pos)
    readLineComment pos0 xs0 =
      let (_sl, rest0) = splitAt 2 xs0  -- //
          (content, rest) = break (== '\n') rest0
          newPos = bump pos0 (2 + length content)
      in (content, dropWhile (== '\n') rest, not (null rest), if null rest
            then newPos
            else Pos (pLine pos0 + 1) 1)

    readBlockComment :: Pos -> String -> (String, String, Pos)
    readBlockComment pos0 xs0 =
      let (_op, rest0) = splitAt 2 xs0   -- /*
      in goBC [] pos0 rest0
      where
        goBC acc posN [] = (reverse acc, [], posN)
        goBC acc posN (x:xs)
          | x == '*' && take 1 xs == "/" =
              (reverse acc, drop 2 xs, bump posN 2)
          | x == '\n' = goBC ('\n':acc) (Pos (pLine posN + 1) 1) xs
          | otherwise = goBC (x:acc) (bump posN 1) xs

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
  | EUnknown [Token] Pos
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
  | SDirectiveLine Directive Pos   -- 单行指令（对后续生效）
  | SDirectiveBlock Directive [Stmt] Pos -- 块级指令（块内生效）
  deriving (Eq, Show)

data Program = Program [Stmt] deriving (Eq, Show)

-- 解析入口
parseProgram :: [Token] -> Program
parseProgram toks = Program (parseManyTop toks)

-- 跳过多余分隔符
skipNL :: [Token] -> [Token]
skipNL (Token (TSym SNewline) _:xs)   = skipNL xs
skipNL (Token (TSym SSemicolon) _:xs) = skipNL xs
skipNL xs = xs


isSym :: Sym -> Token -> Bool
isSym s (Token (TSym s') _) = s == s'
isSym _ _ = False

isKw :: Keyword -> Token -> Bool
isKw k (Token (TKw k') _) = k == k'
isKw _ _ = False

tokId :: Token -> Maybe (String, Pos)
tokId (Token (TId s) p) = Just (s, p)
tokId _ = Nothing


tokComment :: Token -> Maybe (String, Bool, Pos)
tokComment (Token (TComment s isLine) p) = Just (s, isLine, p)
tokComment _ = Nothing

-- 解析若干顶层语句，直到 EOF
parseManyTop :: [Token] -> [Stmt]
parseManyTop = go 0
  where
    go :: Int -> [Token] -> [Stmt]
    go depth xs = case skipNL xs of
      [] -> []
      ts ->
        if depth > 1000  -- 防止无限递归
        then []
        else let (st, rest) = parseStmt ts
             in st : go (depth + 1) rest

-- 解析一般语句
parseStmt :: [Token] -> (Stmt, [Token])
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

    -- 其它：表达式语句
    _ ->
      let (e, rest) = parseExprUntilEnd xs
          p = getExprPos e
      in (SExpr e p, rest)

-- 解析块体直到匹配的 }
parseBlockBody :: [Token] -> ([Stmt], [Token])
parseBlockBody xs = go [] xs 0
  where
    go :: [Stmt] -> [Token] -> Int -> ([Stmt], [Token])
    go acc ts depth = case skipNL ts of
      (t:rest) | isSym SRBrace t -> (reverse acc, rest)
      [] -> (reverse acc, []) -- 容忍缺失
      ts' ->
        if depth > 1000  -- 防止无限递归
        then (reverse acc, ts')
        else let (st, rest') = parseStmt ts'
             in go (st:acc) rest' (depth + 1)

-- 函数签名部分跳过直到 {
consumeUntilLBrace :: [Token] -> ([Token], [Token])
consumeUntilLBrace xs = go [] xs
  where
    go acc (t:rest)
      | isSym SLBrace t = (reverse acc, t:rest)
      | otherwise = go (t:acc) rest
    go acc [] = (reverse acc, [])

-- 可选：块起始处的指令 { //! ... } ，指令必须紧跟在 { 之后（允许空行）
parseOptionalLeadingDirective :: [Token] -> (Maybe Directive, [Token])
parseOptionalLeadingDirective xs0 =
  let xs = skipNL xs0
  in case xs of
     (t:rest)
       | Just (cmt, True, _) <- tokComment t
       , isDirectiveText cmt -> (Just (parseDirectiveText cmt), rest)
     _ -> (Nothing, xs0)

-- 解析 [= expr]，允许没有初始化
parseOptionalInit :: [Token] -> (Maybe Expr, [Token])
parseOptionalInit xs0 =
  let xs = skipNL xs0
  in case xs of
      (Token (TSym SAssign) _ : rest) ->
        let (e, rest') = parseExprUntilEnd rest
        in (Just e, rest')
      _ -> (Nothing, xs0)

-- 解析 var 声明，支持可选的类型注释：var name [type] [= expr]
parseVarDeclWithOptionalType :: [Token] -> (Maybe Expr, [Token])
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
parseExprUntilEnd :: [Token] -> (Expr, [Token])
parseExprUntilEnd xs =
  let (ts, rest) = takeExprTokens xs
  in (tokensToExpr ts, rest)

-- 收集表达式 token
takeExprTokens :: [Token] -> ([Token], [Token])
takeExprTokens = go (0 :: Int) (0 :: Int) []
  where
    stopTok t
      | isSym SNewline t   = True
      | isSym SSemicolon t = True
      | isSym SRBrace t    = True
      | otherwise          = False

    go :: Int -> Int -> [Token] -> [Token] -> ([Token], [Token])
    go _ _ acc [] = (reverse acc, [])
    go paren bracket acc ts@(t:rest)
      | stopTok t && paren == 0 && bracket == 0
          = (reverse acc, ts)
      | isSym SLParen t    = go (paren + 1) bracket (t:acc) rest
      | isSym SRParen t    = go (max 0 (paren - 1)) bracket (t:acc) rest
      | isSym SLBracket t  = go paren (bracket + 1) (t:acc) rest
      | isSym SRBracket t  = go paren (max 0 (bracket - 1)) (t:acc) rest
      | otherwise          = go paren bracket (t:acc) rest

-- 由 token 列表构造简化表达式
tokensToExpr :: [Token] -> Expr
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
        Just (argsTs, afterRParen)
          | null afterRParen ->
              let exprs = map tokensToExpr argsTs
              in ECall f exprs p
        _ -> EUnknown (t:ts) p
    -- 标识符
    (Token (TId x) p, []) -> EIdent x p
    -- 字面量
    (Token (TString s) p, []) -> ELitStr s p
    (Token (TNum n) p, [])    -> ELitNum n p
    -- 其它复杂表达式（保留 token 以便后续扫描变量）
    _ -> EUnknown (t:ts) (tkPos t)

-- 按顶层逗号分割实参列表，要求最后一个 token 是 )
splitTopLevelArgs :: [Token] -> Maybe ([[Token]], [Token])
splitTopLevelArgs ts = go [] [] (0 :: Int) (0 :: Int) ts
  where
    go :: [[Token]] -> [Token] -> Int -> Int -> [Token] -> Maybe ([[Token]], [Token])
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

--------------------------------------------------------------------------------
-- 4) 语义/所有权分析（Analyzer）
-- - 栈式作用域与变量遮蔽
-- - 借用关系：不可变/可变借用
-- - 指令控制（ownership:on/off）
-- - 函数边界处理与跨函数所有权分析
--------------------------------------------------------------------------------

-- 变量状态（栈顶为当前声明）
data VarState = VarState
  { vsScope        :: !Int
  , vsMoved        :: !Bool
  , vsBorrowedBy   :: [Name]        -- 不可变借用者
  , vsMutBorrower  :: Maybe Name     -- 可变借用者
  } deriving (Show, Eq)

-- 借用变量映射：借用变量 -> (源变量, 是否可变借用)
data BorrowInfo = BorrowInfo
  { biSource :: !Name
  , biMut    :: !Bool
  } deriving (Show, Eq)

-- 函数签名信息
data FunctionSig = FunctionSig
  { fsName :: !Name
  , fsParams :: [(Name, ParamType)]  -- 参数名和类型
  , fsReturnType :: ReturnType
  } deriving (Show, Eq)

-- 参数类型：按值传递（移动）、不可变借用、可变借用
data ParamType
  = ParamMove       -- 按值传递（移动所有权）
  | ParamBorrow     -- 不可变借用
  | ParamMutBorrow  -- 可变借用
  deriving (Show, Eq)

-- 返回类型：按值返回、借用返回
data ReturnType
  = ReturnValue     -- 返回所有权
  | ReturnBorrow    -- 返回借用（需要更复杂的分析）
  deriving (Show, Eq)

-- 内置函数定义
builtInFunctions :: [FunctionSig]
builtInFunctions =
  [ FunctionSig "println" [("arg", ParamMove)] ReturnValue
  , FunctionSig "print" [("arg", ParamMove)] ReturnValue
  , FunctionSig "len" [("arg", ParamBorrow)] ReturnValue
  , FunctionSig "cap" [("arg", ParamBorrow)] ReturnValue
  , FunctionSig "append" [("slice", ParamMutBorrow), ("elements", ParamMove)] ReturnValue
  , FunctionSig "make" [("type", ParamMove), ("size", ParamMove)] ReturnValue
  , FunctionSig "new" [("type", ParamMove)] ReturnValue
  , FunctionSig "panic" [("msg", ParamMove)] ReturnValue
  , FunctionSig "recover" [] ReturnValue
  ]

-- 配置
data Config = Config
  { cfgOwnershipOn :: !Bool
  } deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config { cfgOwnershipOn = True }

-- 分析器状态
data AState = AState
  { aScope      :: !Int
  , aVars       :: Map.Map Name [VarState]  -- 变量名 → 栈（遮蔽）
  , aBorrows    :: Map.Map Name BorrowInfo  -- 借用变量名 → 借用信息
  , aCfgStack   :: [Config]                 -- 配置栈
  , aErrors     :: [OwnershipError]
  , aFunctions  :: Map.Map Name FunctionSig -- 函数签名映射
  , aCurrentFunc :: Maybe Name              -- 当前分析的函数名
  } deriving (Show)

emptyAState :: AState
emptyAState = AState
  { aScope = 0
  , aVars = Map.empty
  , aBorrows = Map.empty
  , aCfgStack = [defaultConfig]
  , aErrors = []
  , aFunctions = Map.fromList [(fsName f, f) | f <- builtInFunctions]
  , aCurrentFunc = Nothing
  }

-- 对外入口：返回错误列表
analyzeOwnership :: String -> [OwnershipError]
analyzeOwnership code =
  let toks = lexAll code
      ast  = parseProgram toks
      -- First pass: collect function signatures
      funcSigs = collectFunctionSignatures ast
      -- Second pass: analyze with function signatures
      initialState = emptyAState { aFunctions = Map.union (Map.fromList [(fsName f, f) | f <- funcSigs]) (aFunctions emptyAState) }
  in reverse (aErrors (execState (analyzeProgram ast) initialState))

-- 收集函数签名（第一遍扫描）
collectFunctionSignatures :: Program -> [FunctionSig]
collectFunctionSignatures (Program stmts) = concatMap extractFuncSigs stmts
  where
    extractFuncSigs :: Stmt -> [FunctionSig]
    extractFuncSigs stmt = case stmt of
      SFunc _ _ -> [extractFuncSigFromBody [] undefined]
      _ -> []
    
    extractFuncSigFromBody :: [Stmt] -> Pos -> FunctionSig
    extractFuncSigFromBody _ _ = 
      -- This is a simplified extraction - in a real implementation,
      -- we would parse the actual function signature
      -- For now, we'll create a generic signature based on function name patterns
      let funcName = "unknown_func" :: String  -- This would be extracted from actual function declaration
      in FunctionSig funcName [] ReturnValue

analyzeOwnershipFile :: FilePath -> IO [OwnershipError]
analyzeOwnershipFile fp = analyzeOwnership <$> readFile fp

-- 状态操作工具
pushError :: OwnershipError -> State AState ()
pushError e = do
  st <- get
  let cfgOn = case aCfgStack st of
                [] -> cfgOwnershipOn defaultConfig
                (c:_) -> cfgOwnershipOn c
  if cfgOn then modify (\s -> s { aErrors = e : aErrors s })
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
    popByScope :: Int -> [VarState] -> Either [VarState] [VarState]
    popByScope lvl stack =
      let (_stay, popped) = splitAt (length stack - countTop lvl stack) stack
      in if null popped then Right stack
         else Left popped

    countTop :: Int -> [VarState] -> Int
    countTop lvl = length . takeWhile (\v -> vsScope v == lvl) . reverse

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
--   let suffix :: String = if isMut then "_mut_borrow_" else "_imm_borrow_"
--   let borrowName = sourceName ++ suffix ++ show counter
--   pure borrowName

-- 注册借用变量
registerBorrowVar :: Name -> Name -> Bool -> State AState ()
registerBorrowVar borrowName sourceName isMut = do
  let borrowInfo = BorrowInfo { biSource = sourceName, biMut = isMut }
  modify $ \st -> st { aBorrows = Map.insert borrowName borrowInfo (aBorrows st) }

-- 查找函数签名
lookupFunction :: Name -> State AState (Maybe FunctionSig)
lookupFunction name = do
  st <- get
  pure $ Map.lookup name (aFunctions st)

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
    -- Set current function context
    modify (\s -> s { aCurrentFunc = Just "current_func" })
    mapM_ analyzeStmt body
    modify (\s -> s { aCurrentFunc = Nothing })
    popScope
  SVarDecl name mInit _ -> do
    declareVar name
    maybe (pure ()) analyzeExprForInit mInit
  SLetDecl name mInit _ -> do
    declareVar name
    maybe (pure ()) analyzeExprForInit mInit
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
    analyzeExprForAssignment name rhs
  SExpr e _ -> analyzeExprUse e

-- 声明变量（在当前作用域）
declareVar :: Name -> State AState ()
declareVar n = do
  st <- get
  let lvl = aScope st
  pushVar n (VarState { vsScope = lvl, vsMoved = False, vsBorrowedBy = [], vsMutBorrower = Nothing })

-- 分析初始化表达式
analyzeExprForInit :: Expr -> State AState ()
analyzeExprForInit = analyzeAsRHS

-- 分析赋值右侧
analyzeExprForAssignment :: Name -> Expr -> State AState ()
analyzeExprForAssignment _target = analyzeAsRHS

-- 作为 RHS：函数实参/赋值右侧的变量一般按“移动”处理（除非是借用表达式）
analyzeAsRHS :: Expr -> State AState ()
analyzeAsRHS e = case e of
  EIdent x _ -> moveVar x
  EUnary UBorrow (EIdent x _) _ -> borrowVar False x
  EUnary UMutBorrow (EIdent x _) _ -> borrowVar True x
  ECall fn args _ -> analyzeFunctionCall fn args
  EUnknown ts _  -> scanUnknownAsUse ts
  _ -> pure () -- 字面量等不影响所有权

-- 分析函数调用（关键改进：处理跨函数边界）
analyzeFunctionCall :: Name -> [Expr] -> State AState ()
analyzeFunctionCall fnName args = do
  mFunc <- lookupFunction fnName
  case mFunc of
    Nothing -> do
      -- Unknown function - treat all arguments as moves for safety
      mapM_ analyzeAsArg args
    Just funcSig -> do
      -- Analyze each argument according to function signature
      let params = fsParams funcSig
      if length args /= length params
        then pushError (ParseError $ "Argument count mismatch for " ++ fnName)
        else zipWithM_ analyzeArgWithParam args params
      
      -- For functions that move parameters, mark those variables as moved
      let movedParams = [name | (name, ParamMove) <- params]
      mapM_ moveVar movedParams

-- 根据参数类型分析函数参数
analyzeArgWithParam :: Expr -> (Name, ParamType) -> State AState ()
analyzeArgWithParam expr (_, paramType) = case (expr, paramType) of
  (EIdent argName _, ParamMove) -> do
    -- 按值传递：移动所有权
    moveVar argName
  (EUnary UBorrow (EIdent argName _) _, ParamBorrow) -> do
    -- 不可变借用
    borrowVar False argName
  (EUnary UMutBorrow (EIdent argName _) _, ParamMutBorrow) -> do
    -- 可变借用
    borrowVar True argName
  (EIdent argName _, ParamBorrow) -> do
    -- 隐式借用（未显式使用&）
    borrowVar False argName
  (EIdent argName _, ParamMutBorrow) -> do
    -- 隐式可变借用
    borrowVar True argName
  _ -> analyzeAsArg expr -- 回退到默认处理

-- 函数参数：非借用的标识符按“移动”处理；借用表达式按借用处理
analyzeAsArg :: Expr -> State AState ()
analyzeAsArg e = case e of
  EUnary UBorrow (EIdent x _) _    -> borrowVar False x
  EUnary UMutBorrow (EIdent x _) _ -> borrowVar True x
  EIdent x _                       -> moveVar x
  ECall _ args _                   -> mapM_ analyzeAsArg args
  EUnknown ts _                    -> scanUnknownAsUse ts
  _                                -> pure ()

-- 表达式用作“使用”（非移动）
analyzeExprUse :: Expr -> State AState ()
analyzeExprUse e = case e of
  EIdent x _ -> useVar x
  EUnary _ inner _ -> analyzeExprUse inner
  ECall fn args _ -> do
    analyzeFunctionCall fn args
    -- Also analyze any remaining expressions in arguments
    mapM_ analyzeExprUse args
  EUnknown ts _ -> scanUnknownAsUse ts
  _ -> pure ()

-- 在 Unknown 表达式中扫描可能的标识符使用（保守，不做移动）
scanUnknownAsUse :: [Token] -> State AState ()
scanUnknownAsUse ts = mapM_ (\x -> useVar x) (collectPlainIdents ts)

collectPlainIdents :: [Token] -> [Name]
collectPlainIdents = mapMaybe f
  where
    f (Token (TId s) _) = Just s
    f _                 = Nothing

-- 借用
borrowVar :: Bool -> Name -> State AState ()
borrowVar isMut name = do
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
                 Nothing -> do
                   -- 使用调用者提供的借用变量名（在赋值分析中设置）
                   updateVarTop name (\vv -> vv { vsBorrowedBy = "<pending>":vsBorrowedBy vv })

-- 移动
moveVar :: Name -> State AState ()
moveVar name = do
  mv <- lookupVarTop name
  case mv of
    Nothing -> pushError (OutOfScope name)
    Just v ->
      if vsMoved v
        then pushError (DoubleMove name name)
        else if not (null (vsBorrowedBy v)) || isJust (vsMutBorrower v)
          then pushError (BorrowWhileMoved name) -- 有借用时再移动，这里统一报 BorrowWhileMoved
          else updateVarTop name (\vv -> vv { vsMoved = True })


-- 使用
useVar :: Name -> State AState ()
useVar name = do
  -- First check if this is a borrow variable
  st <- get
  case Map.lookup name (aBorrows st) of
    Just (BorrowInfo src isMut) -> do
      -- Using a borrow variable - check the source variable's borrow status
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
      mv <- lookupVarTop name
      case mv of
        Nothing -> pushError (OutOfScope name)
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