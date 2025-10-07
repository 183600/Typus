{-# LANGUAGE OverloadedStrings #-}
module OwnershipComprehensive
  ( -- 数据模型
    OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer
  , newOwnershipAnalyzer
  , analyzeOwnership
  , analyzeOwnershipFile
  , formatOwnershipErrors
  ) where

import qualified Data.Map.Strict as Map
import Data.Char (isSpace, isDigit, isAlpha, isAlphaNum)
import Data.Maybe (isJust)
import Data.List (intercalate)
import Control.Monad.State
import Control.Monad ()

-- This is a simplified but comprehensive ownership analyzer that focuses on
-- the core ownership semantics without getting bogged down in complex parsing

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
-- 2) 简化但有效的词法分析
--------------------------------------------------------------------------------

-- 简化的词法分析，专注于所有权相关的构造
data Token = TIdent String | TKeyword String | TSymbol Char | TString String | TNumber String
  deriving (Show, Eq)

-- 简化的词法分析器
simpleLex :: String -> [Token]
simpleLex [] = []
simpleLex ('"':rest) =
  let (str, remaining') = break (== '"') rest
  in case remaining' of
       ('"':remaining) -> TString str : simpleLex remaining
       _ -> TString str : simpleLex remaining'
simpleLex (c:cs) 
  | isSpace c = simpleLex cs
  | isAlpha c = 
      let (word, rest) = span (\x -> isAlphaNum x || x == '_') (c:cs)
      in if word `elem` ["func", "var", "let", "mut", "return", "if", "else", "for"]
         then TKeyword word : simpleLex rest
         else TIdent word : simpleLex rest
  | isDigit c =
      let (num, rest) = span (\x -> isDigit x || x == '.') (c:cs)
      in TNumber num : simpleLex rest
  | c == '&' = 
      let (op, rest) = span (\x -> x `elem` ("&mut" :: String)) (c:cs)
      in TKeyword op : simpleLex rest
  | c `elem` ("(){}[],;:=+-*/<>!" :: String) =
      TSymbol c : simpleLex cs
  | otherwise = simpleLex cs

--------------------------------------------------------------------------------
-- 3) 简化的语法分析 - 专注于所有权结构
--------------------------------------------------------------------------------

data OwnershipExpr
  = OEIdent String
  | OECall String [OwnershipExpr]
  | OEBorrow String Bool  -- variable name, is mutable
  | OELiteral String
  deriving (Show, Eq)

data OwnershipStmt
  = OSVarDecl String (Maybe OwnershipExpr)
  | OSAssign String OwnershipExpr
  | OSFuncCall String [OwnershipExpr]
  | OSBlock [OwnershipStmt]
  | OSFuncDef String [(String, ParamType)] [OwnershipStmt]
  deriving (Show, Eq)

-- 参数类型
data ParamType
  = ParamMove       -- 按值传递（移动所有权）
  | ParamBorrow     -- 不可变借用
  | ParamMutBorrow  -- 可变借用
  deriving (Show, Eq)

-- 简化的解析器，专注于提取所有权相关信息
parseOwnershipCode :: String -> [OwnershipStmt]
parseOwnershipCode code = 
  let tokens = simpleLex code
  in parseTokens tokens
  where
    parseTokens :: [Token] -> [OwnershipStmt]
    parseTokens [] = []
    parseTokens (TKeyword "func" : TIdent name : rest) =
      case parseFuncParams rest of
        (params, remaining) ->
          case parseFuncBody remaining of
            (body, rest') ->
              let funcStmt = OSFuncDef name params body
              in funcStmt : parseTokens rest'
    parseTokens (TIdent var : TSymbol ':' : TSymbol '=' : rest) =
      case parseExpr rest of
        (expr, remaining) ->
          let assignStmt = OSAssign var expr
          in assignStmt : parseTokens remaining
    parseTokens (TKeyword "var" : TIdent var : TSymbol '=' : rest) =
      case parseExpr rest of
        (expr, remaining) ->
          let varStmt = OSVarDecl var (Just expr)
          in varStmt : parseTokens remaining
    parseTokens (TIdent func : TSymbol '(' : rest) =
      case parseArgs rest of
        (args, TSymbol ')' : remaining) ->
          let callStmt = OSFuncCall func args
          in callStmt : parseTokens remaining
        _ -> parseTokens rest
    parseTokens (_ : rest) = parseTokens rest
    
    parseFuncParams :: [Token] -> ([(String, ParamType)], [Token])
    parseFuncParams (TSymbol '(' : TSymbol ')' : rest) = ([], rest)
    parseFuncParams (TSymbol '(' : rest) =
      let (params, remaining) = collectParams rest []
      in (reverse params, remaining)
    parseFuncParams rest = ([], rest)
    
    collectParams :: [Token] -> [(String, ParamType)] -> ([(String, ParamType)], [Token])
    collectParams [] acc = (reverse acc, [])
    collectParams (TSymbol ')' : rest) acc = (reverse acc, rest)
    collectParams (TSymbol ',' : rest) acc = collectParams rest acc
    collectParams (TKeyword "&mut" : TIdent param : rest) acc = 
      collectParams rest ((param, ParamMutBorrow) : acc)
    collectParams (TKeyword "&" : TIdent param : rest) acc = 
      collectParams rest ((param, ParamBorrow) : acc)
    collectParams (TIdent param : rest) acc = 
      collectParams rest ((param, ParamMove) : acc)
    collectParams (_ : rest) acc = collectParams rest acc
    
    parseFuncBody :: [Token] -> ([OwnershipStmt], [Token])
    parseFuncBody tokens =
      let (bodyTokens, remaining) = span (/= TSymbol '}') tokens
          body = parseTokens bodyTokens
      in (body, if null remaining then [] else drop 1 remaining)
    
    parseArgs :: [Token] -> ([OwnershipExpr], [Token])
    parseArgs [] = ([], [])
    parseArgs (TSymbol ')' : rest) = ([], rest)
    parseArgs tokens =
      let (arg, rest) = parseSingleArg tokens
          (moreArgs, remaining) = case rest of
                                   (TSymbol ',' : rest') -> parseArgs rest'
                                   _ -> parseArgs rest
      in (arg : moreArgs, remaining)
    
    parseSingleArg :: [Token] -> (OwnershipExpr, [Token])
    parseSingleArg (TKeyword "&mut" : TIdent var : rest) = (OEBorrow var True, rest)
    parseSingleArg (TKeyword "&" : TIdent var : rest) = (OEBorrow var False, rest)
    parseSingleArg (TIdent var : rest) = (OEIdent var, rest)
    parseSingleArg (TString str : rest) = (OELiteral str, rest)
    parseSingleArg (TNumber num : rest) = (OELiteral num, rest)
    parseSingleArg (_ : rest) = (OELiteral "unknown", rest)
    parseSingleArg [] = error "parseSingleArg: empty input"
    
    parseExpr :: [Token] -> (OwnershipExpr, [Token])
    parseExpr [] = (OELiteral "", [])
    parseExpr tokens = parseSingleArg tokens

--------------------------------------------------------------------------------
-- 4) 所有权分析器
--------------------------------------------------------------------------------

-- 变量状态
data VarState = VarState
  { vsScope        :: !Int
  , vsMoved        :: !Bool
  , vsBorrowedBy   :: [String]        -- 不可变借用者
  , vsMutBorrower  :: Maybe String     -- 可变借用者
  } deriving (Show, Eq)

-- 借用信息
data BorrowInfo = BorrowInfo
  { biSource :: !String
  , biMut    :: !Bool
  } deriving (Show, Eq)

-- 分析器状态
data AnalyzerState = AnalyzerState
  { asScope      :: !Int
  , asVars       :: Map.Map String [VarState]  -- 变量名 → 栈（遮蔽）
  , asBorrows    :: Map.Map String BorrowInfo  -- 借用变量名 → 借用信息
  , asErrors     :: [OwnershipError]
  } deriving (Show)

emptyAnalyzerState :: AnalyzerState
emptyAnalyzerState = AnalyzerState
  { asScope = 0
  , asVars = Map.empty
  , asBorrows = Map.empty
  , asErrors = []
  }

-- 对外入口
analyzeOwnership :: String -> [OwnershipError]
analyzeOwnership code = 
  let stmts = parseOwnershipCode code
  in reverse (asErrors (execState (analyzeStmts stmts) emptyAnalyzerState))

analyzeOwnershipFile :: FilePath -> IO [OwnershipError]
analyzeOwnershipFile fp = analyzeOwnership <$> readFile fp

-- 分析语句序列
analyzeStmts :: [OwnershipStmt] -> State AnalyzerState ()
analyzeStmts = mapM_ analyzeStmt

analyzeStmt :: OwnershipStmt -> State AnalyzerState ()
analyzeStmt stmt = case stmt of
  OSVarDecl name mInit -> do
    declareVar name
    maybe (pure ()) analyzeExpr mInit
  
  OSAssign name expr -> do
    mv <- lookupVar name
    case mv of
      Nothing -> declareVar name
      Just _ -> pure ()
    analyzeExpr expr
    analyzeAssignment name expr
  
  OSFuncCall func args -> do
    analyzeFunctionCall func args
  
  OSBlock stmts -> do
    pushScope
    analyzeStmts stmts
    popScope
  
  OSFuncDef name params body -> do
    -- 分析函数体，在函数作用域中声明参数
    pushScope
    mapM_ (declareParam name) params
    analyzeStmts body
    popScope

-- 声明变量
declareVar :: String -> State AnalyzerState ()
declareVar name = do
  st <- get
  let lvl = asScope st
  modify $ \s -> s { asVars = Map.insertWith (++) name [VarState lvl False [] Nothing] (asVars s) }

-- 声明函数参数
declareParam :: String -> (String, ParamType) -> State AnalyzerState ()
declareParam _ (paramName, paramType) = do
  case paramType of
    ParamMove -> declareVar paramName
    ParamBorrow -> do
      -- 对于借用参数，我们需要创建一个特殊的借用关系
      -- 这里简化处理，实际实现会更复杂
      declareVar paramName
    ParamMutBorrow -> do
      declareVar paramName

-- 分析表达式
analyzeExpr :: OwnershipExpr -> State AnalyzerState ()
analyzeExpr expr = case expr of
  OEIdent name -> useVar name
  OECall func args -> analyzeFunctionCall func args
  OEBorrow name isMut -> borrowVar isMut name
  OELiteral _ -> pure ()

-- 分析赋值
analyzeAssignment :: String -> OwnershipExpr -> State AnalyzerState ()
analyzeAssignment _ expr = case expr of
  OEIdent source -> moveVar source
  OEBorrow source isMut -> borrowVar isMut source
  _ -> pure ()

-- 分析函数调用
analyzeFunctionCall :: String -> [OwnershipExpr] -> State AnalyzerState ()
analyzeFunctionCall func args = do
  -- 内置函数处理
  case func of
    "println" -> mapM_ analyzeExpr args
    "print" -> mapM_ analyzeExpr args
    "len" -> mapM_ analyzeExpr args
    "cap" -> mapM_ analyzeExpr args
    _ -> do
      -- 用户定义函数：根据参数类型分析
      -- 这里我们简化处理，假设所有参数按值传递（移动）
      -- 在实际实现中，我们会查找函数签名
      mapM_ analyzeArgAsMove args
  where
    analyzeArgAsMove (OEIdent name) = moveVar name
    analyzeArgAsMove (OEBorrow name _) = borrowVar False name
    analyzeArgAsMove _ = pure ()

-- 变量操作
lookupVar :: String -> State AnalyzerState (Maybe VarState)
lookupVar name = do
  st <- get
  case Map.lookup name (asVars st) of
    Just (v:_) -> return (Just v)
    _ -> return Nothing

updateVar :: String -> (VarState -> VarState) -> State AnalyzerState ()
updateVar name f = do
  st <- get
  case Map.lookup name (asVars st) of
    Just (v:vs) -> modify $ \s -> s { asVars = Map.insert name (f v : vs) (asVars s) }
    Just [] -> pure ()
    Nothing -> pure ()

pushScope :: State AnalyzerState ()
pushScope = modify $ \s -> s { asScope = asScope s + 1 }

popScope :: State AnalyzerState ()
popScope = modify $ \s -> s { asScope = asScope s - 1 }

pushError :: OwnershipError -> State AnalyzerState ()
pushError err = modify $ \s -> s { asErrors = err : asErrors s }

-- 借用操作
borrowVar :: Bool -> String -> State AnalyzerState ()
borrowVar isMut name = do
  mv <- lookupVar name
  case mv of
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
                 Nothing -> updateVar name (\vv -> vv { vsBorrowedBy = (name ++ "_borrow") : vsBorrowedBy vv })

-- 移动操作
moveVar :: String -> State AnalyzerState ()
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

-- 使用操作
useVar :: String -> State AnalyzerState ()
useVar name = do
  mv <- lookupVar name
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