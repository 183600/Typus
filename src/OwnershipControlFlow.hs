{-# LANGUAGE OverloadedStrings, TupleSections #-}
module OwnershipControlFlow
  ( -- 数据模型
    OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer
  , newOwnershipAnalyzer
  , analyzeOwnership
  , analyzeOwnershipFile
  , formatOwnershipErrors
  , ControlFlowGraph
  , ControlFlowNode(..)
  , ControlFlowEdge(..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (isSpace, isDigit, isAlpha)
import Data.Maybe (isJust, mapMaybe)
import Data.List (intercalate, isPrefixOf, nub)
import Control.Monad.State
import Control.Monad (zipWithM_, when)
import Data.Graph ()

--------------------------------------------------------------------------------
-- 1) 数据类型与错误类型
--------------------------------------------------------------------------------

-- 所有权类型
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
  | ControlFlowError String         -- 控制流分析错误
  | PathSensitiveError String       -- 路径敏感分析错误
  deriving (Show, Eq)

-- 分析器句柄
newtype OwnershipAnalyzer = OwnershipAnalyzer () deriving (Show, Eq)

newOwnershipAnalyzer :: OwnershipAnalyzer
newOwnershipAnalyzer = OwnershipAnalyzer ()

--------------------------------------------------------------------------------
-- 2) 控制流图定义
--------------------------------------------------------------------------------

data ControlFlowNode
  = CFEntry           -- 入口节点
  | CFExit            -- 出口节点
  | CFStmt Int Stmt   -- 语句节点（带ID）
  | CFCondition Int Expr [Stmt] [Stmt]  -- 条件节点（if/else）
  | CFLoop Int Expr [Stmt]              -- 循环节点
  | CFJoin Int        -- 汇合节点
  deriving (Show, Eq)

data ControlFlowEdge
  = CFEdge            -- 普通边
  | CFTrueEdge        -- 真分支
  | CFFalseEdge       -- 假分支
  | CFLoopEntryEdge   -- 循环入口
  | CFLoopBackEdge    -- 循环回边
  deriving (Show, Eq)

type ControlFlowGraph = (Map.Map Int ControlFlowNode, [(Int, Int, ControlFlowEdge)])

--------------------------------------------------------------------------------
-- 3) 词法分析器（增强版）
--------------------------------------------------------------------------------

data Pos = Pos { pLine :: !Int, pCol :: !Int } deriving (Eq, Show)

data Keyword
  = KwVar | KwLet | KwFunc | KwReturn | KwIf | KwElse | KwFor | KwWhile
  | KwPackage | KwImport | KwType | KwStruct | KwInterface | KwConst
  | KwMut | KwBreak | KwContinue
  deriving (Eq, Show)

data Sym
  = SLBrace | SRBrace | SLParen | SRParen | SLBracket | SRBracket
  | SSemicolon | SComma | SColon | SAssign | SWalrus | SAmp | SDot
  | SNewline | SLt | SGt | SLeq | SGeq | SEq | SNe | SAnd | SOr
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
  "while"   -> Just KwWhile
  "package" -> Just KwPackage
  "import"  -> Just KwImport
  "type"    -> Just KwType
  "struct"  -> Just KwStruct
  "interface"->Just KwInterface
  "const"   -> Just KwConst
  "mut"     -> Just KwMut
  "break"   -> Just KwBreak
  "continue"-> Just KwContinue
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
          let (comment, consumedNL, newPos) = readLineComment pos s
          in Token (TComment comment True) pos : case consumedNL of
                True  -> Token (TSym SNewline) newPos : go newPos cs
                False -> go newPos cs
      -- 注释：/* ... */
      | startsWith "/*" s =
          let (comment, newPos) = readBlockComment pos s
          in Token (TComment comment False) pos : go newPos cs
      -- 字符串
      | c == '"' =
          let (str, newPos) = readString pos cs
          in Token (TString str) pos : go newPos cs
      -- 字符字面量（简化处理）
      | c == '\'' =
          let (ch, newPos) = readChar pos cs
          in Token (TString ch) pos : go newPos cs
      -- 两字符操作符
      | startsWith ":=" s =
          Token (TSym SWalrus) pos : go (bump pos 2) (drop 2 s)
      | startsWith "<=" s =
          Token (TSym SLeq) pos : go (bump pos 2) (drop 2 s)
      | startsWith ">=" s =
          Token (TSym SGeq) pos : go (bump pos 2) (drop 2 s)
      | startsWith "==" s =
          Token (TSym SEq) pos : go (bump pos 2) (drop 2 s)
      | startsWith "!=" s =
          Token (TSym SNe) pos : go (bump pos 2) (drop 2 s)
      | startsWith "&&" s =
          Token (TSym SAnd) pos : go (bump pos 2) (drop 2 s)
      | startsWith "||" s =
          Token (TSym SOr) pos : go (bump pos 2) (drop 2 s)
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
      | c == '&' = Token (TSym SAmp)      pos : go (bump pos 1) cs
      | c == '.' = Token (TSym SDot)      pos : go (bump pos 1) cs
      | c == '<' = Token (TSym SLt)       pos : go (bump pos 1) cs
      | c == '>' = Token (TSym SGt)       pos : go (bump pos 1) cs
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

    readString :: Pos -> String -> (String, Pos)
    readString p s = goStr [] p s
      where
        goStr acc posN [] = (reverse acc, posN)
        goStr acc posN (x:xs)
          | x == '\\' = case xs of
              (y:ys) -> goStr (y:'\\':acc) (bump posN 2) ys
              []     -> (reverse ('\\':acc), bump posN 1)
          | x == '"'  = (reverse acc, bump posN 1)
          | x == '\n' = goStr ('\n':acc) (Pos (pLine posN + 1) 1) xs
          | otherwise = goStr (x:acc) (bump posN 1) xs

    readChar :: Pos -> String -> (String, Pos)
    readChar p s = goChr [] p s
      where
        goChr acc posN [] = (reverse acc, posN)
        goChr acc posN (x:xs)
          | x == '\\' = case xs of
              (y:ys) -> goChr (y:'\\':acc) (bump posN 2) ys
              []     -> (reverse ('\\':acc), bump posN 1)
          | x == '\'' = (reverse acc, bump posN 1)
          | x == '\n' = goChr ('\n':acc) (Pos (pLine posN + 1) 1) xs
          | otherwise = goChr (x:acc) (bump posN 1) xs

    readLineComment :: Pos -> String -> (String, Bool, Pos)
    readLineComment pos0 xs0 =
      let (_sl, rest0) = splitAt 2 xs0  -- //
          (content, rest) = break (== '\n') rest0
          newPos = bump pos0 (2 + length content)
          hasNewline = not (null rest)
      in (content, hasNewline, if hasNewline then Pos (pLine pos0 + 1) 1 else newPos)

    readBlockComment :: Pos -> String -> (String, Pos)
    readBlockComment pos0 xs0 =
      let (_op, rest0) = splitAt 2 xs0   -- /*
      in goBC [] pos0 rest0
      where
        goBC acc posN [] = (reverse acc, posN)
        goBC acc posN (x:xs)
          | x == '*' && take 1 xs == "/" =
              (reverse acc, bump posN 2)
          | x == '\n' = goBC ('\n':acc) (Pos (pLine posN + 1) 1) xs
          | otherwise = goBC (x:acc) (bump posN 1) xs

--------------------------------------------------------------------------------
-- 4) 语法分析器（支持控制流）
--------------------------------------------------------------------------------

type Name = String

data AssignOp = OpAssign | OpWalrus deriving (Eq, Show)

data UnaryOp = UBorrow | UMutBorrow deriving (Eq, Show)

data BinaryOp = BOAnd | BOOr | BOEq | BONe | BOLt | BOGt | BOLe | BOGe deriving (Eq, Show)

data Directive = Directive (Map.Map String String) deriving (Eq, Show)

data Expr
  = EIdent Name Pos
  | ECall Name [Expr] Pos
  | EUnary UnaryOp Expr Pos
  | EBinary BinaryOp Expr Expr Pos
  | ELitStr String Pos
  | ELitNum String Pos
  | EUnknown [Token] Pos
  deriving (Eq, Show)

getExprPos :: Expr -> Pos
getExprPos e = case e of
  EIdent _ p   -> p
  ECall _ _ p  -> p
  EUnary _ _ p -> p
  EBinary _ _ _ p -> p
  ELitStr _ p  -> p
  ELitNum _ p  -> p
  EUnknown _ p -> p

data Stmt
  = SVarDecl Name (Maybe Expr) Pos
  | SLetDecl Name (Maybe Expr) Pos
  | SAssignStmt Name AssignOp Expr Pos
  | SExpr Expr Pos
  | SBlock [Stmt] Pos
  | SIf Expr [Stmt] [Stmt] Pos  -- if condition thenStmt elseStmt
  | SFor (Maybe Name) Expr [Stmt] Pos  -- for item in collection { body }
  | SWhile Expr [Stmt] Pos      -- while condition { body }
  | SBreak Pos
  | SContinue Pos
  | SFunc Name [(Name, Maybe String)] (Maybe String) [Stmt] Pos
  | SDirectiveLine Directive Pos
  | SDirectiveBlock Directive [Stmt] Pos
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

    -- if 语句
    (t:rest) | isKw KwIf t ->
      let (condition, afterCond) = parseCondition rest
          (thenBody, afterThen) = parseBlockBody afterCond
          (elseBody, rest') = parseOptionalElse afterThen
      in (SIf condition thenBody elseBody (getTokenPos t), rest')

    -- for 循环
    (t:rest) | isKw KwFor t ->
      let (loopVar, afterVar) = parseOptionalLoopVar rest
          (collection, afterCollection) = parseExprUntilSemicolon afterVar
          (body, rest') = parseBlockBody afterCollection
      in (SFor loopVar collection body (getTokenPos t), rest')

    -- while 循环
    (t:rest) | isKw KwWhile t ->
      let (condition, afterCond) = parseCondition rest
          (body, rest') = parseBlockBody afterCond
      in (SWhile condition body (getTokenPos t), rest')

    -- break 语句
    (t:rest) | isKw KwBreak t ->
      (SBreak (getTokenPos t), rest)

    -- continue 语句
    (t:rest) | isKw KwContinue t ->
      (SContinue (getTokenPos t), rest)

    -- 函数：func name(params) returnType { body }
    (t:rest) | isKw KwFunc t ->
      let (funcName, afterName) = parseFuncName rest
          (params, afterParams) = parseFuncParams afterName
          (returnType, afterReturn) = parseFuncReturnType afterParams
          (body, rest') = parseFuncBody afterReturn
      in (SFunc funcName params returnType body (getTokenPos t), rest')

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
        in (SVarDecl name mInit pName, rest')

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

-- 解析条件表达式
parseCondition :: [Token] -> (Expr, [Token])
parseCondition toks = parseExprUntilRParen toks

-- 解析可选循环变量
parseOptionalLoopVar :: [Token] -> (Maybe Name, [Token])
parseOptionalLoopVar xs = case skipNL xs of
  (Token (TId name) _ : rest) -> (Just name, rest)
  _ -> (Nothing, xs)

-- 解析可选 else 分支
parseOptionalElse :: [Token] -> ([Stmt], [Token])
parseOptionalElse xs = case skipNL xs of
  (t:rest) | isKw KwElse t ->
    case skipNL rest of
      (Token (TSym SLBrace) _ : _) -> 
        let (body, rest') = parseBlockBody rest
        in (body, rest')
      (t2:rest2) | isKw KwIf t2 ->
        -- else if - 创建一个嵌套的if语句
        let (condition, afterCond) = parseCondition rest2
            (thenBody, afterThen) = parseBlockBody afterCond
            (elseBody, rest') = parseOptionalElse afterThen
            nestedIf = SIf condition thenBody elseBody (getTokenPos t2)
        in ([nestedIf], rest')
      _ -> ([], xs)
  _ -> ([], xs)

-- 解析函数名
parseFuncName :: [Token] -> (Name, [Token])
parseFuncName toks = case skipNL toks of
  (Token (TId name) _ : rest) -> (name, rest)
  _ -> ("unknown_func", toks)

-- 解析函数参数： (param1 type1, param2 type2, ...)
parseFuncParams :: [Token] -> ([(Name, Maybe String)], [Token])
parseFuncParams toks = case skipNL toks of
  (Token (TSym SLParen) _ : rest) ->
    let (params, afterParams) = collectParams rest []
    in case skipNL afterParams of
         (Token (TSym SRParen) _ : rest') -> (reverse params, rest')
         _ -> ([], toks)
  _ -> ([], toks)
  where
    collectParams :: [Token] -> [(Name, Maybe String)] -> ([(Name, Maybe String)], [Token])
    collectParams toks' acc = case skipNL toks' of
      (Token (TId paramName) _ : Token (TId paramType) _ : rest) ->
        collectParams rest ((paramName, Just paramType) : acc)
      (Token (TId paramName) _ : rest) ->
        collectParams rest ((paramName, Nothing) : acc)
      (Token (TSym SComma) _ : rest) ->
        collectParams rest acc
      rest -> (acc, rest)

-- 解析返回类型
parseFuncReturnType :: [Token] -> (Maybe String, [Token])
parseFuncReturnType toks = case skipNL toks of
  (Token (TId retType) _ : rest) -> (Just retType, rest)
  _ -> (Nothing, toks)

-- 解析函数体
parseFuncBody :: [Token] -> ([Stmt], [Token])
parseFuncBody toks = case skipNL toks of
  (Token (TSym SLBrace) _ : rest) -> parseBlockBody rest
  _ -> ([], toks)

getTokenPos :: Token -> Pos
getTokenPos = tkPos

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

-- 解析直到右括号
parseExprUntilRParen :: [Token] -> (Expr, [Token])
parseExprUntilRParen xs = 
  let (exprTokens, rest) = takeUntilRParen xs 0
      expr = tokensToExpr exprTokens
  in (expr, rest)
  where
    takeUntilRParen :: [Token] -> Int -> ([Token], [Token])
    takeUntilRParen [] _ = ([], [])
    takeUntilRParen (t:rest) depth
      | isSym SLParen t = 
          let (inner, remaining) = takeUntilRParen rest (depth + 1)
          in (t:inner, remaining)
      | isSym SRParen t =
          if depth == 0 
          then ([], rest) 
          else let (tokens, remaining) = takeUntilRParen rest (depth - 1)
               in (t:tokens, remaining)
      | otherwise = 
          let (tokens, remaining) = takeUntilRParen rest depth
          in (t:tokens, remaining)

-- 解析直到分号或块结束
parseExprUntilSemicolon :: [Token] -> (Expr, [Token])
parseExprUntilSemicolon xs =
  let (exprTokens, rest) = takeUntilSemicolon xs
      expr = tokensToExpr exprTokens
  in (expr, rest)
  where
    takeUntilSemicolon :: [Token] -> ([Token], [Token])
    takeUntilSemicolon [] = ([], [])
    takeUntilSemicolon (t:rest)
      | isSym SSemicolon t = ([], rest)
      | isSym SLBrace t = ([], t:rest)  -- 块开始，停止
      | otherwise = 
          let (tokens, remaining) = takeUntilSemicolon rest
          in (t:tokens, remaining)

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
      _ -> (Nothing, xs)

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
    go paren bracket acc ts@(t:rest)
      | stopTok t && paren == 0 && bracket == 0
          = (reverse acc, ts)
      | isSym SLParen t    = go (paren + 1) bracket (t:acc) rest
      | isSym SRParen t    = go (max 0 (paren - 1)) bracket (t:acc) rest
      | isSym SLBracket t  = go paren (bracket + 1) (t:acc) rest
      | isSym SRBracket t  = go paren (max 0 (bracket - 1)) (t:acc) rest
      | otherwise          = go paren bracket (t:acc) rest
    go _ _ acc [] = (reverse acc, [])

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
    -- 二元操作符（简化处理）
    _ -> case parseBinaryExpr (t:ts) of
           Just expr -> expr
           Nothing -> case t of
             Token (TId x) p -> if null ts then EIdent x p else EUnknown (t:ts) p
             Token (TString s) p -> if null ts then ELitStr s p else EUnknown (t:ts) p
             Token (TNum n) p -> if null ts then ELitNum n p else EUnknown (t:ts) p
             _ -> EUnknown (t:ts) (tkPos t)

-- 简化的二元表达式解析
parseBinaryExpr :: [Token] -> Maybe Expr
parseBinaryExpr [] = Nothing
parseBinaryExpr [t] = case t of
  Token (TId x) p -> Just $ EIdent x p
  Token (TString s) p -> Just $ ELitStr s p
  Token (TNum n) p -> Just $ ELitNum n p
  _ -> Nothing
parseBinaryExpr ts = 
  -- 查找二元操作符，简化处理
  case findBinaryOp ts of
    Just (left, op, right, pos) ->
      case (parseBinaryExpr left, parseBinaryExpr right) of
        (Just l, Just r) -> Just $ EBinary op l r pos
        _ -> Nothing
    Nothing -> Nothing

findBinaryOp :: [Token] -> Maybe ([Token], BinaryOp, [Token], Pos)
findBinaryOp ts = case ts of
  [] -> Nothing
  (t:rest) -> case t of
    Token (TSym SLt) p -> Just ([], BOLt, rest, p)
    Token (TSym SGt) p -> Just ([], BOGt, rest, p)
    Token (TSym SLeq) p -> Just ([], BOLe, rest, p)
    Token (TSym SGeq) p -> Just ([], BOGe, rest, p)
    Token (TSym SEq) p -> Just ([], BOEq, rest, p)
    Token (TSym SNe) p -> Just ([], BONe, rest, p)
    Token (TSym SAnd) p -> Just ([], BOAnd, rest, p)
    Token (TSym SOr) p -> Just ([], BOOr, rest, p)
    _ -> do
      (left, op, right, pos) <- findBinaryOp rest
      return (t:left, op, right, pos)

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
-- 5) 控制流图构建
--------------------------------------------------------------------------------

buildControlFlowGraph :: Program -> ControlFlowGraph
buildControlFlowGraph (Program stmts) =
  let (nodes, edges, _) = buildCFG stmts 1
      entryNode :: (Int, ControlFlowNode)
      entryNode = (0, CFEntry)
      exitNode :: (Int, ControlFlowNode)
      exitNode = (maxNodeId + 1, CFExit)
      allNodes = Map.fromList (entryNode : nodes ++ [exitNode])
      allEdges = (0, headNodeId, CFEdge) : edges ++ [(lastNodeId, maxNodeId + 1, CFEdge)]
      maxNodeId = maximum (map fst nodes)
      headNodeId = if null nodes then 1 else minimum (map fst nodes)
      lastNodeId = if null nodes then 1 else maximum (map fst nodes)
  in (allNodes, allEdges)
  where
    buildCFG :: [Stmt] -> Int -> ([(Int, ControlFlowNode)], [(Int, Int, ControlFlowEdge)], Int)
    buildCFG stmts' startId = foldl buildStmtCFG ([], [], startId) stmts'
    
    buildStmtCFG :: ([(Int, ControlFlowNode)], [(Int, Int, ControlFlowEdge)], Int) -> Stmt 
                  -> ([(Int, ControlFlowNode)], [(Int, Int, ControlFlowEdge)], Int)
    buildStmtCFG (_, _, nextId) stmt = case stmt of
      SIf cond thenBody elseBody _ ->
        let condNode = (nextId, CFCondition nextId cond thenBody elseBody)
            thenStart = nextId + 1
            (thenNodes, thenEdges, thenNext) = buildCFG thenBody thenStart
            elseStart = thenNext
            (elseNodes, elseEdges, elseNext) = buildCFG elseBody elseStart
            joinNodeId = elseNext
            joinNode = (joinNodeId, CFJoin joinNodeId)
            
            newNodes = condNode : thenNodes ++ elseNodes ++ [joinNode]
            newEdges = (nextId, thenStart, CFTrueEdge) : 
                      (nextId, elseStart, CFFalseEdge) :
                      map (\(from, to, edge) -> (from + nextId, to + nextId, edge)) thenEdges ++
                      map (\(from, to, edge) -> (from + thenNext, to + thenNext, edge)) elseEdges ++
                      [(thenNext - 1, joinNodeId, CFEdge), (elseNext - 1, joinNodeId, CFEdge)]
        in (newNodes, newEdges, joinNodeId + 1)
      
      SFor _ collection body _ ->
        let loopNode = (nextId, CFLoop nextId collection body)
            bodyStart = nextId + 1
            (bodyNodes, bodyEdges, bodyNext) = buildCFG body bodyStart
            joinNodeId = bodyNext
            joinNode = (joinNodeId, CFJoin joinNodeId)
            
            newNodes = loopNode : bodyNodes ++ [joinNode]
            newEdges = (nextId, bodyStart, CFLoopEntryEdge) :
                      (bodyNext - 1, nextId, CFLoopBackEdge) :
                      (nextId, joinNodeId, CFEdge) :  -- 退出循环的边
                      map (\(from, to, edge) -> (from + nextId, to + nextId, edge)) bodyEdges
        in (newNodes, newEdges, joinNodeId + 1)
      
      SWhile cond body _ ->
        let whileNode = (nextId, CFLoop nextId cond body)
            bodyStart = nextId + 1
            (bodyNodes, bodyEdges, bodyNext) = buildCFG body bodyStart
            joinNodeId = bodyNext
            joinNode = (joinNodeId, CFJoin joinNodeId)
            
            newNodes = whileNode : bodyNodes ++ [joinNode]
            newEdges = (nextId, bodyStart, CFLoopEntryEdge) :
                      (bodyNext - 1, nextId, CFLoopBackEdge) :
                      (nextId, joinNodeId, CFEdge) :  -- 退出循环的边
                      map (\(from, to, edge) -> (from + nextId, to + nextId, edge)) bodyEdges
        in (newNodes, newEdges, joinNodeId + 1)
      
      SBlock body _ ->
        let (bodyNodes, bodyEdges, bodyNext) = buildCFG body nextId
        in (bodyNodes, bodyEdges, bodyNext)
      
      _ ->
        let stmtNode = (nextId, CFStmt nextId stmt)
        in ([stmtNode], [], nextId + 1)

--------------------------------------------------------------------------------
-- 6) 路径敏感的所有权分析
--------------------------------------------------------------------------------

-- 变量状态
data VarState = VarState
  { vsScope        :: !Int
  , vsMoved        :: !Bool
  , vsBorrowedBy   :: [Name]        -- 不可变借用者
  , vsMutBorrower  :: Maybe Name     -- 可变借用者
  } deriving (Show, Eq)

-- 借用信息
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

-- 参数类型
data ParamType
  = ParamMove       -- 按值传递（移动所有权）
  | ParamBorrow     -- 不可变借用
  | ParamMutBorrow  -- 可变借用
  deriving (Show, Eq)

-- 返回类型
data ReturnType
  = ReturnValue     -- 返回所有权
  | ReturnBorrow    -- 返回借用
  deriving (Show, Eq)

-- 路径状态 - 跟踪在特定控制流路径上的所有权状态
data PathState = PathState
  { psVars :: Map.Map Name VarState      -- 变量状态
  , psBorrows :: Map.Map Name BorrowInfo -- 借用信息
  , psErrors :: [OwnershipError]         -- 路径上的错误
  , psVisited :: Set.Set Int             -- 访问过的节点
  } deriving (Show, Eq)

-- 分析器状态
data AnalyzerState = AnalyzerState
  { asScope      :: !Int
  , asFunctions  :: Map.Map Name FunctionSig -- 函数签名映射
  , asPaths      :: [PathState]            -- 所有路径状态
  , asCurrentPath :: PathState             -- 当前路径
  , asNodeId     :: !Int                   -- 当前节点ID
  } deriving (Show)

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

-- 对外入口：返回错误列表
analyzeOwnership :: String -> [OwnershipError]
analyzeOwnership code =
  let toks = lexAll code
      ast  = parseProgram toks
      cfg  = buildControlFlowGraph ast
      -- First pass: collect function signatures
      funcSigs = collectFunctionSignatures ast
      -- Second pass: analyze with control flow
      initialState = createInitialAnalyzerState funcSigs
      finalState = execState (analyzeWithControlFlow cfg) initialState
  in nub $ concatMap psErrors (asPaths finalState)

-- 创建初始分析器状态
createInitialAnalyzerState :: [FunctionSig] -> AnalyzerState
createInitialAnalyzerState funcSigs = AnalyzerState
  { asScope = 0
  , asFunctions = Map.union (Map.fromList [(fsName f, f) | f <- funcSigs]) 
                             (Map.fromList [(fsName f, f) | f <- builtInFunctions])
  , asPaths = []
  , asCurrentPath = emptyPathState
  , asNodeId = 0
  }

-- 空路径状态
emptyPathState :: PathState
emptyPathState = PathState
  { psVars = Map.empty
  , psBorrows = Map.empty
  , psErrors = []
  , psVisited = Set.empty
  }

-- 收集函数签名（第一遍扫描）
collectFunctionSignatures :: Program -> [FunctionSig]
collectFunctionSignatures (Program stmts) = concatMap extractFuncSigs stmts
  where
    extractFuncSigs :: Stmt -> [FunctionSig]
    extractFuncSigs stmt = case stmt of
      SFunc name params returnType _ _ -> [buildFunctionSig name params returnType]
      _ -> []
    
    buildFunctionSig :: Name -> [(Name, Maybe String)] -> Maybe String -> FunctionSig
    buildFunctionSig name params returnType =
      let paramTypes = map (\(paramName, mType) -> 
                            case mType of
                              Just t | "&mut" `isPrefixOf` t -> (paramName, ParamMutBorrow)
                              Just t | "&" `isPrefixOf` t -> (paramName, ParamBorrow)
                              _ -> (paramName, ParamMove)
                           ) params
          retType = case returnType of
                     Just t | "&" `isPrefixOf` t -> ReturnBorrow
                     _ -> ReturnValue
      in FunctionSig name paramTypes retType

analyzeOwnershipFile :: FilePath -> IO [OwnershipError]
analyzeOwnershipFile fp = analyzeOwnership <$> readFile fp

-- 基于控制流的分析
analyzeWithControlFlow :: ControlFlowGraph -> State AnalyzerState ()
analyzeWithControlFlow (nodes, edges) = do
  -- 从入口节点开始分析
  let entryNodeId :: Int
      entryNodeId = 0
  analyzeNode entryNodeId
  where
    analyzeNode :: Int -> State AnalyzerState ()
    analyzeNode nodeId = do
      st <- get
      -- 检查是否已经访问过这个节点（避免无限循环）
      let currentPath = asCurrentPath st
      when (not (Set.member nodeId (psVisited currentPath))) $ do
        -- 标记节点为已访问
        modify $ \s -> s 
          { asCurrentPath = (asCurrentPath s) 
              { psVisited = Set.insert nodeId (psVisited (asCurrentPath s))
              }
          , asNodeId = nodeId
          }
        
        -- 分析当前节点
        case Map.lookup nodeId nodes of
          Nothing -> return ()  -- 节点不存在
          Just node -> analyzeControlFlowNode node
          
        -- 分析后继节点
        let outgoingEdges = filter (\(from, _, _) -> from == nodeId) edges
        mapM_ (\(_, to, edgeType) -> analyzeEdge to edgeType) outgoingEdges
    
    analyzeEdge :: Int -> ControlFlowEdge -> State AnalyzerState ()
    analyzeEdge toNodeId edgeType = do
      -- 为不同的边类型创建不同的路径状态
      case edgeType of
        CFTrueEdge -> do
          -- 真分支：保存当前状态并分析
          saveCurrentPath
          analyzeNode toNodeId
          restorePathState
        CFFalseEdge -> do
          -- 假分支：保存当前状态并分析
          saveCurrentPath
          analyzeNode toNodeId
          restorePathState
        CFLoopEntryEdge -> do
          -- 循环入口：需要特殊处理循环变量
          saveCurrentPath
          analyzeNode toNodeId
          restorePathState
        CFLoopBackEdge -> do
          -- 循环回边：合并循环状态
          analyzeNode toNodeId
        _ -> analyzeNode toNodeId
    
    saveCurrentPath :: State AnalyzerState ()
    saveCurrentPath = modify $ \s -> s 
      { asPaths = asCurrentPath s : asPaths s
      }
    
    restorePathState :: State AnalyzerState ()
    restorePathState = modify $ \s -> s
      { asCurrentPath = case asPaths s of
          [] -> emptyPathState
          (x:_) -> x
      }

-- 分析控制流节点
analyzeControlFlowNode :: ControlFlowNode -> State AnalyzerState ()
analyzeControlFlowNode node = case node of
  CFEntry -> return ()  -- 入口节点无需分析
  CFExit -> return ()   -- 出口节点无需分析
  CFStmt _ stmt -> analyzeStmt stmt
  CFCondition _ cond _ _ -> do
    -- 分析条件表达式
    analyzeExpr cond
    -- 分别分析两个分支（在路径敏感分析中处理）
    return ()
  CFLoop _ cond _ -> do
    -- 分析循环条件
    analyzeExpr cond
    -- 分析循环体（在路径敏感分析中处理）
    return ()
  CFJoin _ -> return ()  -- 汇合节点无需分析

-- 分析语句
analyzeStmt :: Stmt -> State AnalyzerState ()
analyzeStmt stmt = case stmt of
  SBlock body _ -> do
    pushScope
    mapM_ analyzeStmt body
    popScope
  
  SIf cond _ _ _ -> do
    -- 条件已经在控制流节点中分析过
    analyzeExpr cond
    -- 分支分析由控制流图处理
  
  SFor mVar collection _ _ -> do
    -- 分析循环变量声明
    case mVar of
      Just varName -> declareVar varName
      Nothing -> return ()
    analyzeExpr collection
    -- 循环体分析由控制流图处理
  
  SWhile cond _ _ -> do
    analyzeExpr cond
    -- 循环体分析由控制流图处理
  
  SBreak _ -> return ()  -- break语句不影响所有权
  SContinue _ -> return ()  -- continue语句不影响所有权
  
  SFunc name params _ body _ -> do
    -- 分析函数体，在函数作用域中声明参数
    pushScope
    mapM_ (declareParam name) params
    mapM_ analyzeStmt body
    popScope
  
  SVarDecl name mInit _ -> do
    declareVar name
    maybe (pure ()) analyzeExpr mInit
  
  SLetDecl name mInit _ -> do
    declareVar name
    maybe (pure ()) analyzeExpr mInit
  
  SAssignStmt name op rhs _ -> do
    case op of
      OpWalrus -> do
        case rhs of
          EUnary UBorrow (EIdent sourceName _) _ -> do
            declareVar name
            registerBorrowVar name sourceName False
            updateVar sourceName (\vv -> vv { vsBorrowedBy = name:vsBorrowedBy vv })
          EUnary UMutBorrow (EIdent sourceName _) _ -> do
            declareVar name
            registerBorrowVar name sourceName True
            updateVar sourceName (\vv -> vv { vsMutBorrower = Just name })
          _ -> do
            declareVar name
            analyzeAsRHS rhs
      OpAssign -> do
        mv <- lookupVar name
        case mv of
          Nothing -> declareVar name
          Just _ -> pure ()
        analyzeAsRHS rhs
    analyzeExprForAssignment name rhs
  
  SExpr e _ -> analyzeExpr e

  SDirectiveLine _ _ -> return ()  -- 指令行不影响所有权分析
  SDirectiveBlock _ body _ -> do  -- 块级指令，分析其内容
    mapM_ analyzeStmt body

-- 作用域管理
pushScope :: State AnalyzerState ()
pushScope = modify $ \s -> s { asScope = asScope s + 1 }

popScope :: State AnalyzerState ()
popScope = do
  st <- get
  let cur = asScope st
  -- 清理当前作用域的变量和借用关系
  let currentPath = asCurrentPath st
      varsToKeep = Map.filter (\v -> vsScope v < cur) (psVars currentPath)
      borrowsToKeep = Map.filterWithKey (\_ info -> 
        Map.member (biSource info) varsToKeep
        ) (psBorrows currentPath)
  
  modify $ \s -> s 
    { asScope = cur - 1
    , asCurrentPath = (asCurrentPath s) 
        { psVars = varsToKeep
        , psBorrows = borrowsToKeep
        }
    }

-- 变量操作
lookupVar :: Name -> State AnalyzerState (Maybe VarState)
lookupVar name = do
  st <- get
  let currentPath = asCurrentPath st
  return $ Map.lookup name (psVars currentPath)

updateVar :: Name -> (VarState -> VarState) -> State AnalyzerState ()
updateVar name f = do
  st <- get
  let currentPath = asCurrentPath st
      updatedVars = Map.adjust f name (psVars currentPath)
  modify $ \s -> s 
    { asCurrentPath = (asCurrentPath s) { psVars = updatedVars }
    }

declareVar :: Name -> State AnalyzerState ()
declareVar name = do
  st <- get
  let lvl = asScope st
      currentPath = asCurrentPath st
      newVar = VarState lvl False [] Nothing
      updatedVars = Map.insert name newVar (psVars currentPath)
  modify $ \s -> s 
    { asCurrentPath = (asCurrentPath s) { psVars = updatedVars }
    }

declareParam :: Name -> (Name, Maybe String) -> State AnalyzerState ()
declareParam _ (paramName, mType) = do
  let paramType = case mType of
        Just t | "&mut" `isPrefixOf` t -> ParamMutBorrow
        Just t | "&" `isPrefixOf` t -> ParamBorrow
        _ -> ParamMove
  
  case paramType of
    ParamMove -> declareVar paramName
    ParamBorrow -> declareVar paramName
    ParamMutBorrow -> declareVar paramName

registerBorrowVar :: Name -> Name -> Bool -> State AnalyzerState ()
registerBorrowVar borrowName sourceName isMut = do
  st <- get
  let currentPath = asCurrentPath st
      borrowInfo = BorrowInfo { biSource = sourceName, biMut = isMut }
      updatedBorrows = Map.insert borrowName borrowInfo (psBorrows currentPath)
  modify $ \s -> s 
    { asCurrentPath = (asCurrentPath s) { psBorrows = updatedBorrows }
    }

pushError :: OwnershipError -> State AnalyzerState ()
pushError err = do
  st <- get
  let currentPath = asCurrentPath st
      updatedErrors = err : psErrors currentPath
  modify $ \s -> s 
    { asCurrentPath = (asCurrentPath s) { psErrors = updatedErrors }
    }

-- 表达式分析
analyzeExpr :: Expr -> State AnalyzerState ()
analyzeExpr expr = case expr of
  EIdent name _ -> useVar name
  ECall func args _ -> analyzeFunctionCall func args
  EUnary UBorrow (EIdent name _) _ -> borrowVar False name
  EUnary UMutBorrow (EIdent name _) _ -> borrowVar True name
  EUnary _ inner _ -> analyzeExpr inner
  EBinary _ left right _ -> do
    analyzeExpr left
    analyzeExpr right
  ELitStr _ _ -> return ()
  ELitNum _ _ -> return ()
  EUnknown _ _ -> return ()

analyzeAsRHS :: Expr -> State AnalyzerState ()
analyzeAsRHS expr = case expr of
  EIdent name _ -> moveVar name
  EUnary UBorrow (EIdent name _) _ -> borrowVar False name
  EUnary UMutBorrow (EIdent name _) _ -> borrowVar True name
  ECall func args _ -> analyzeFunctionCall func args
  EBinary _ left right _ -> do
    analyzeAsRHS left
    analyzeAsRHS right
  _ -> return ()

analyzeExprForAssignment :: Name -> Expr -> State AnalyzerState ()
analyzeExprForAssignment _target = analyzeAsRHS

-- 函数调用分析
analyzeFunctionCall :: Name -> [Expr] -> State AnalyzerState ()
analyzeFunctionCall funcName args = do
  mFunc <- lookupFunction funcName
  case mFunc of
    Nothing -> do
      -- 未知函数 - 为安全起见，将所有参数视为移动
      mapM_ analyzeAsArg args
    Just funcSig -> do
      -- 根据函数签名分析每个参数
      let params = fsParams funcSig
      if length args /= length params
        then pushError (ParseError $ "Argument count mismatch for " ++ funcName)
        else zipWithM_ analyzeArgWithParam args params

analyzeArgWithParam :: Expr -> (Name, ParamType) -> State AnalyzerState ()
analyzeArgWithParam expr (_, paramType) = case (expr, paramType) of
  (EIdent argName _, ParamMove) -> moveVar argName
  (EUnary UBorrow (EIdent argName _) _, ParamBorrow) -> borrowVar False argName
  (EUnary UMutBorrow (EIdent argName _) _, ParamMutBorrow) -> borrowVar True argName
  (EIdent argName _, ParamBorrow) -> borrowVar False argName
  (EIdent argName _, ParamMutBorrow) -> borrowVar True argName
  _ -> analyzeAsArg expr

analyzeAsArg :: Expr -> State AnalyzerState ()
analyzeAsArg e = case e of
  EUnary UBorrow (EIdent x _) _    -> borrowVar False x
  EUnary UMutBorrow (EIdent x _) _ -> borrowVar True x
  EIdent x _                       -> moveVar x
  ECall _ args _                   -> mapM_ analyzeAsArg args
  EBinary _ left right _           -> do
    analyzeAsArg left
    analyzeAsArg right
  _                                -> pure ()

-- 变量操作
borrowVar :: Bool -> Name -> State AnalyzerState ()
borrowVar isMut name = do
  m <- lookupVar name
  case m of
    Nothing -> pushError (OutOfScope name)
    Just v ->
      if vsMoved v
        then pushError (BorrowWhileMoved name)
        else if isMut
          then case vsMutBorrower v of
                 Just _ -> pushError (MultipleMutBorrows name)
                 Nothing -> updateVar name (\vv -> vv { vsMutBorrower = Just (name ++ "_mut_borrow") })
          else case vsMutBorrower v of
                 Just _ -> pushError (BorrowWhileMutBorrowed name)
                 Nothing -> updateVar name (\vv -> vv { vsBorrowedBy = (name ++ "_borrow"):vsBorrowedBy vv })

moveVar :: Name -> State AnalyzerState ()
moveVar name = do
  mv <- lookupVar name
  case mv of
    Nothing -> pushError (OutOfScope name)
    Just v ->
      if vsMoved v
        then pushError (DoubleMove name name)
        else if not (null (vsBorrowedBy v)) || isJust (vsMutBorrower v)
          then pushError (BorrowWhileMoved name)
          else updateVar name (\vv -> vv { vsMoved = True })

useVar :: Name -> State AnalyzerState ()
useVar name = do
  -- 首先检查这是否是借用变量
  st <- get
  let currentPath = asCurrentPath st
  case Map.lookup name (psBorrows currentPath) of
    Just (BorrowInfo src isMut) -> do
      -- 使用借用变量 - 检查源变量的借用状态
      mv <- lookupVar src
      case mv of
        Nothing -> pushError (OutOfScope src)
        Just v ->
          if vsMoved v
            then pushError (BorrowWhileMoved src)
            else if isMut
              then case vsMutBorrower v of
                     Just borrower -> if borrower == name
                                      then pure ()  -- 这是有效的可变借用
                                      else pushError (MultipleMutBorrows src)
                     Nothing -> pushError (BorrowError src)
              else case elem name (vsBorrowedBy v) of
                     True -> pure ()  -- 这是有效的不可变借用
                     False -> pushError (BorrowError src)
    Nothing -> do
      -- 常规变量使用
      mv <- lookupVar name
      case mv of
        Nothing -> pushError (OutOfScope name)
        Just v ->
          if vsMoved v
            then pushError (UseAfterMove name)
            else case vsMutBorrower v of
                   Just _ -> pushError (UseWhileMutBorrowed name)
                   Nothing -> pure ()

-- 查找函数签名
lookupFunction :: Name -> State AnalyzerState (Maybe FunctionSig)
lookupFunction name = do
  st <- get
  return $ Map.lookup name (asFunctions st)

--------------------------------------------------------------------------------
-- 7) 工具函数
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
    formatError (ControlFlowError msg) = "Control flow error: " ++ msg
    formatError (PathSensitiveError msg) = "Path sensitive error: " ++ msg