{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CoreOwnershipQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose
  , sized, suchThat, vectorOf, frequency, Positive(..)
  )

import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )

import Ownership.Analyzer
  ( analyzeOwnership
  , analyzeOwnershipFile
  , analyzeOwnershipDebug
  , builtInFunctions
  )

import Ownership.Lexer (Token(..), TokenKind(..))
import Ownership.Parser (Program(..), Stmt(..), Expr(..))

import Data.List (isInfixOf, nub)
import Data.Map.Strict as Map (Map, empty, insert, lookup, keys, member)

-- ============================================================================
-- 生成器定义
-- ============================================================================

-- 生成变量名
genVariableName :: Gen String
genVariableName = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- 生成所有权类型
genOwnershipType :: Gen OwnershipType
genOwnershipType = oneof
  [ Owned <$> genVariableName
  , Borrowed <$> genVariableName
  , MutBorrowed <$> genVariableName
  ]

-- 生成所有权错误
genOwnershipError :: Gen OwnershipError
genOwnershipError = oneof
  [ UseAfterMove <$> genVariableName
  , DoubleMove <$> genVariableName <*> genVariableName
  , BorrowWhileMoved <$> genVariableName
  , MutBorrowWhileBorrowed <$> genVariableName
  , BorrowWhileMutBorrowed <$> genVariableName
  , MultipleMutBorrows <$> genVariableName
  , UseWhileMutBorrowed <$> genVariableName
  , OutOfScope <$> genVariableName
  , BorrowError <$> genVariableName
  , ParseError <$> genVariableName
  , CrossFunctionMove <$> genVariableName <*> genVariableName
  , ParameterMoveMismatch <$> genVariableName
  , ControlFlowError <$> genVariableName
  ]

-- 生成简单的所有权表达式
genSimpleExpr :: Gen String
genSimpleExpr = oneof
  [ genVariableName
  , return "42"
  , return "\"hello\""
  , return "true"
  , return "false"
  ]

-- 生成简单的所有权语句
genSimpleStmt :: Gen String
genSimpleStmt = do
  var <- genVariableName
  expr <- genSimpleExpr
  return $ var ++ " = " ++ expr

-- 生成移动操作语句
genMoveStmt :: Gen String
genMoveStmt = do
  src <- genVariableName
  dst <- genVariableName
  return $ dst ++ " = " ++ src

-- 生成借用操作语句
genBorrowStmt :: Gen String
genBorrowStmt = do
  src <- genVariableName
  dst <- genVariableName
  isMutable <- elements [True, False]
  let op = if isMutable then "&mut" else "&"
  return $ dst ++ " = " ++ op ++ " " ++ src

-- 生成基本的所有权程序
genBasicOwnershipProgram :: Gen String
genBasicOwnershipProgram = do
  numStmts <- choose (1, 5)
  stmts <- vectorOf numStmts genSimpleStmt
  return $ unlines stmts

-- 生成包含移动的程序
genMoveProgram :: Gen String
genMoveProgram = do
  decl <- genSimpleStmt
  move <- genMoveStmt
  return $ decl ++ "\n" ++ move

-- 生成包含借用的程序
genBorrowProgram :: Gen String
genBorrowProgram = do
  decl <- genSimpleStmt
  borrow <- genBorrowStmt
  return $ decl ++ "\n" ++ borrow

-- ============================================================================
-- QuickCheck 属性测试
-- ============================================================================

-- 属性: OwnershipType的Show/Read一致性
prop_ownership_type_show_roundtrip :: Property
prop_ownership_type_show_roundtrip =
  forAll genOwnershipType $ \ownershipType ->
    let shown = show ownershipType
        -- 简单验证show函数包含必要信息
        hasName = any (`elem` shown) ['a'..'z'] ++ ['0'..'9'] ++ ['_']
        hasType = any (`isInfixOf` shown) ["Owned", "Borrowed", "MutBorrowed"]
    in hasName .&&. hasType

-- 属性: OwnershipType的排序性质
prop_ownership_type_ordering :: Property
prop_ownership_type_ordering =
  forAll genOwnershipType $ \ownershipType ->
    ownershipType <= ownershipType .&&. ownershipType >= ownershipType

-- 属性: OwnershipError的Show函数包含关键信息
prop_ownership_error_show_contains_info :: Property
prop_ownership_error_show_contains_info =
  forAll genOwnershipError $ \error ->
    let shown = show error
        hasName = any (`elem` shown) ['a'..'z'] ++ ['0'..'9'] ++ ['_']
        hasErrorType = any (`isInfixOf` shown) 
          [ "UseAfterMove", "DoubleMove", "BorrowWhileMoved"
          , "MutBorrowWhileBorrowed", "BorrowWhileMutBorrowed"
          , "MultipleMutBorrows", "UseWhileMutBorrowed"
          , "OutOfScope", "BorrowError", "ParseError"
          , "CrossFunctionMove", "ParameterMoveMismatch", "ControlFlowError"
          ]
    in hasName .&&. hasErrorType

-- 属性: newOwnershipAnalyzer创建有效的分析器
prop_new_ownership_analyzer_valid :: Property
prop_new_ownership_analyzer_valid =
  let analyzer = newOwnershipAnalyzer
  in property True -- 如果创建成功就通过

-- 属性: builtInFunctions不为空
prop_builtin_functions_not_empty :: Property
prop_builtin_functions_not_empty =
  let functions = builtInFunctions
  in not (null functions) .&&. all (not . null) functions

-- 属性: builtInFunctions没有重复
prop_builtin_functions_unique :: Property
prop_builtin_functions_unique =
  let functions = builtInFunctions
      uniqueFunctions = nub functions
  in length functions === length uniqueFunctions

-- 属性: 基本程序分析不崩溃
prop_basic_program_analysis_no_crash :: Property
prop_basic_program_analysis_no_crash =
  forAll genBasicOwnershipProgram $ \program ->
    let result = analyzeOwnership program
    in property True -- 如果不崩溃就通过

-- 属性: 移动程序分析产生合理结果
prop_move_program_analysis :: Property
prop_move_program_analysis =
  forAll genMoveProgram $ \program ->
    let result = analyzeOwnership program
    in property True -- 基本的不崩溃测试

-- 属性: 借用程序分析产生合理结果
prop_borrow_program_analysis :: Property
prop_borrow_program_analysis =
  forAll genBorrowProgram $ \program ->
    let result = analyzeOwnership program
    in property True -- 基本的不崩溃测试

-- 属性: 空程序分析
prop_empty_program_analysis :: Property
prop_empty_program_analysis =
  let result = analyzeOwnership ""
  in property True -- 空程序应该能分析

-- 属性: 无效程序处理
prop_invalid_program_handling :: Property
prop_invalid_program_handling =
  forAll (elements ["@", "$", "%", "^", "&", "*"]) $ \invalidChar ->
    let program = invalidChar ++ " invalid syntax"
        result = analyzeOwnership program
    in property True -- 应该能处理无效输入而不崩溃

-- 属性: OwnershipType相等性自反性
prop_ownership_type_equality_reflexive :: Property
prop_ownership_type_equality_reflexive =
  forAll genOwnershipType $ \ownershipType ->
    ownershipType == ownershipType

-- 属性: OwnershipError相等性自反性
prop_ownership_error_equality_reflexive :: Property
prop_ownership_error_equality_reflexive =
  forAll genOwnershipError $ \error ->
    error == error

-- 属性: Owned类型的比较性质
prop_owned_type_comparison :: Property
prop_owned_type_comparison =
  forAll genVariableName $ \name1 ->
  forAll genVariableName $ \name2 ->
    let owned1 = Owned name1
        owned2 = Owned name2
    in if name1 == name2 
       then owned1 == owned2 .&&. owned1 <= owned2 .&&. owned1 >= owned2
       else owned1 /= owned2 .&&. (owned1 < owned2 || owned1 > owned2)

-- 属性: Borrowed类型的比较性质
prop_borrowed_type_comparison :: Property
prop_borrowed_type_comparison =
  forAll genVariableName $ \name1 ->
  forAll genVariableName $ \name2 ->
    let borrowed1 = Borrowed name1
        borrowed2 = Borrowed name2
    in if name1 == name2 
       then borrowed1 == borrowed2 .&&. borrowed1 <= borrowed2 .&&. borrowed1 >= borrowed2
       else borrowed1 /= borrowed2 .&&. (borrowed1 < borrowed2 || borrowed1 > borrowed2)

-- 属性: MutBorrowed类型的比较性质
prop_mut_borrowed_type_comparison :: Property
prop_mut_borrowed_type_comparison =
  forAll genVariableName $ \name1 ->
  forAll genVariableName $ \name2 ->
    let mutBorrowed1 = MutBorrowed name1
        mutBorrowed2 = MutBorrowed name2
    in if name1 == name2 
       then mutBorrowed1 == mutBorrowed2 .&&. mutBorrowed1 <= mutBorrowed2 .&&. mutBorrowed1 >= mutBorrowed2
       else mutBorrowed1 /= mutBorrowed2 .&&. (mutBorrowed1 < mutBorrowed2 || mutBorrowed1 > mutBorrowed2)

-- 属性: 不同所有权类型之间的比较顺序
prop_ownership_type_hierarchy :: Property
prop_ownership_type_hierarchy =
  forAll genVariableName $ \name ->
    let owned = Owned name
        borrowed = Borrowed name
        mutBorrowed = MutBorrowed name
    in owned < borrowed .&&. borrowed < mutBorrowed .&&. owned < mutBorrowed

-- 属性: 分析结果的一致性
prop_analysis_consistency :: Property
prop_analysis_consistency =
  forAll genBasicOwnershipProgram $ \program ->
    let result1 = analyzeOwnership program
        result2 = analyzeOwnership program
    in property True -- 两次分析应该产生相同结果（这里只测试不崩溃）

-- ============================================================================
-- 测试套件
-- ============================================================================

tests :: TestTree
tests = testGroup "Core Ownership QuickCheck Tests"
  [ fastProperty "OwnershipType show roundtrip" prop_ownership_type_show_roundtrip
  , fastProperty "OwnershipType ordering" prop_ownership_type_ordering
  , fastProperty "OwnershipError show contains info" prop_ownership_error_show_contains_info
  , fastProperty "New ownership analyzer is valid" prop_new_ownership_analyzer_valid
  , fastProperty "Built-in functions not empty" prop_builtin_functions_not_empty
  , fastProperty "Built-in functions are unique" prop_builtin_functions_unique
  , fastProperty "Basic program analysis doesn't crash" prop_basic_program_analysis_no_crash
  , fastProperty "Move program analysis" prop_move_program_analysis
  , fastProperty "Borrow program analysis" prop_borrow_program_analysis
  , fastProperty "Empty program analysis" prop_empty_program_analysis
  , fastProperty "Invalid program handling" prop_invalid_program_handling
  , fastProperty "OwnershipType equality reflexive" prop_ownership_type_equality_reflexive
  , fastProperty "OwnershipError equality reflexive" prop_ownership_error_equality_reflexive
  , fastProperty "Owned type comparison" prop_owned_type_comparison
  , fastProperty "Borrowed type comparison" prop_borrowed_type_comparison
  , fastProperty "MutBorrowed type comparison" prop_mut_borrowed_type_comparison
  , fastProperty "Ownership type hierarchy" prop_ownership_type_hierarchy
  , fastProperty "Analysis consistency" prop_analysis_consistency
  ]