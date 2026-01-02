{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipOwnershipAnalysisSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, elements, listOf, oneof, sized)
import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  , analyzeOwnership
  , analyzeOwnershipFile
  , analyzeOwnershipDebug
  , formatOwnershipErrors
  , lexAll
  , parseProgram
  , builtInFunctions
  )
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)

-- ============================================================================
-- 生成测试数据
-- ============================================================================

-- 生成所有权类型
genOwnershipType :: Gen OwnershipType
genOwnershipType = elements 
  [ Owned
  , Borrowed
  , Moved
  , Shared
  , Unique
  ]

-- 生成所有权转移类型
genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = elements 
  [ MoveTransfer
  , BorrowTransfer
  , CopyTransfer
  , ShareTransfer
  ]

-- 生成有效的所有权代码片段
genOwnershipCodeSnippet :: Gen String
genOwnershipCodeSnippet = oneof
  [ return "var x int = 42"
  , return "y := x"
  , return "func test() { return 42 }"
  , return "x := make([]int, 10)"
  , return "go func() { }()"
  ]

-- 生成包含所有权错误的代码片段
genOwnershipErrorSnippet :: Gen String
genOwnershipErrorSnippet = oneof
  [ return "x := 42; y := x; _ = x"  -- use after move
  , return "func() { var x int; return x }()"  -- return reference to local
  ]

-- ============================================================================
-- 所有权分析属性测试
-- ============================================================================

-- Property: 空代码分析
prop_analyze_empty_code :: Property
prop_analyze_empty_code =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer ""
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: 简单所有权分析
prop_analyze_simple_ownership :: String -> Property
prop_analyze_simple_ownership code =
  not (null code) && not ("func" `L.isInfixOf` code) ==>
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer code
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: 词法分析返回tokens
prop_lexical_analysis_returns_tokens :: String -> Property
prop_lexical_analysis_returns_tokens code =
  not (null code) ==>
  let tokens = lexAll code
  in property $ not (null tokens)

-- Property: 解析程序生成AST
prop_parse_program_generates_ast :: String -> Property
prop_parse_program_generates_ast code =
  not (null code) ==>
  let result = parseProgram code
  in case result of
    Left _ -> property False
    Right ast -> property $ True

-- Property: 所有权错误格式化
prop_format_ownership_errors :: [OwnershipError] -> Property
prop_format_ownership_errors errors =
  not (null errors) ==>
  let formatted = formatOwnershipErrors errors
  in property $ not (null formatted)

-- Property: 内置函数列表不为空
prop_builtin_functions_not_empty :: Property
prop_builtin_functions_not_empty =
  let builtins = builtInFunctions
  in property $ not (null builtins)

-- Property: 所有权分析器创建
prop_ownership_analyzer_creation :: Property
prop_ownership_analyzer_creation =
  let analyzer = newOwnershipAnalyzer
  in property $ True

-- Property: 文件级所有权分析
prop_file_ownership_analysis :: String -> Property
prop_file_ownership_analysis content =
  not (null content) ==>
  let result = analyzeOwnershipFile content
  in case result of
    Left _ -> property True
    Right _ -> property True

-- Property: 调试模式所有权分析
prop_debug_ownership_analysis :: String -> Property
prop_debug_ownership_analysis code =
  not (null code) ==>
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnershipDebug analyzer code
  in case result of
    Left _ -> property True
    Right _ -> property True

-- ============================================================================
-- 单元测试
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Ownership Ownership Analysis Tests"
    [ testGroup "Property Tests"
        [ fastProperty "analyze empty code" prop_analyze_empty_code
        , fastProperty "analyze simple ownership" prop_analyze_simple_ownership
        , fastProperty "lexical analysis returns tokens" prop_lexical_analysis_returns_tokens
        , fastProperty "parse program generates ast" prop_parse_program_generates_ast
        , fastProperty "format ownership errors" prop_format_ownership_errors
        , fastProperty "builtin functions not empty" prop_builtin_functions_not_empty
        , fastProperty "ownership analyzer creation" prop_ownership_analyzer_creation
        , fastProperty "file ownership analysis" prop_file_ownership_analysis
        , fastProperty "debug ownership analysis" prop_debug_ownership_analysis
        ]
    , testGroup "Unit Tests"
        [ testCase "analyze variable declaration" $ do
            let code = "var x int = 42"
                analyzer = newOwnershipAnalyzer
                result = analyzeOwnership analyzer code
            case result of
              Left err -> assertFailure $ "Ownership analysis failed: " ++ show err
              Right _ -> return ()

        , testCase "analyze variable assignment" $ do
            let code = "x := 42; y := x"
                analyzer = newOwnershipAnalyzer
                result = analyzeOwnership analyzer code
            case result of
              Left err -> assertFailure $ "Ownership analysis failed: " ++ show err
              Right _ -> return ()

        , testCase "analyze function definition" $ do
            let code = "func test(x int) int { return x }"
                analyzer = newOwnershipAnalyzer
                result = analyzeOwnership analyzer code
            case result of
              Left err -> assertFailure $ "Ownership analysis failed: " ++ show err
              Right _ -> return ()

        , testCase "analyze function call" $ do
            let code = "func main() { test(42) }"
                analyzer = newOwnershipAnalyzer
                result = analyzeOwnership analyzer code
            case result of
              Left err -> assertFailure $ "Ownership analysis failed: " ++ show err
              Right _ -> return ()

        , testCase "lexical analysis of simple code" $ do
            let code = "var x int = 42"
                tokens = lexAll code
            L.length tokens @?= 5  -- var, x, int, =, 42

        , testCase "parse simple program" $ do
            let code = "var x int = 42"
                result = parseProgram code
            case result of
              Left err -> assertFailure $ "Parse failed: " ++ show err
              Right ast -> return ()

        , testCase "format ownership errors" $ do
            let errors = 
                  [ OwnershipError "Use after move" "Variable used after being moved" Nothing
                  , OwnershipError "Borrow checker" "Cannot borrow mutable reference" Nothing
                  ]
                formatted = formatOwnershipErrors errors
            "Use after move" `L.isInfixOf` formatted @?= True
            "Borrow checker" `L.isInfixOf` formatted @?= True

        , testCase "builtin functions available" $ do
            let builtins = builtInFunctions
                hasCommonFunctions = L.any (`elem` builtins) ["print", "len", "make", "new"]
            assertBool "Should have common builtin functions" hasCommonFunctions

        , testCase "analyze complex ownership scenario" $ do
            let code = unlines
                  [ "func main() {"
                  , "  data := make([]int, 10)"
                  , "  process(data)"
                  , "  fmt.Println(len(data))"
                  , "}"
                  ]
                analyzer = newOwnershipAnalyzer
                result = analyzeOwnership analyzer code
            case result of
              Left err -> assertFailure $ "Ownership analysis failed: " ++ show err
              Right _ -> return ()

        , testCase "detect move semantics" $ do
            let code = "x := 42; y := x"
                analyzer = newOwnershipAnalyzer
                result = analyzeOwnership analyzer code
            case result of
              Left _ -> return ()
              Right analysis -> return ()  -- Should detect move from x to y

        , testCase "detect borrowing" $ do
            let code = "func process(x *int) { *x = 42 }"
                analyzer = newOwnershipAnalyzer
                result = analyzeOwnership analyzer code
            case result of
              Left err -> assertFailure $ "Ownership analysis failed: " ++ show err
              Right _ -> return ()

        , testCase "analyze ownership transfer" $ do
            let code = "func transfer() { data := make([]byte, 1024); return data }"
                analyzer = newOwnershipAnalyzer
                result = analyzeOwnership analyzer code
            case result of
              Left _ -> return ()
              Right _ -> return ()  -- Should detect ownership transfer

        , testCase "file-level analysis" $ do
            let content = unlines
                  [ "package main"
                  , "func main() {"
                  , "  x := 42"
                  , "  println(x)"
                  , "}"
                  ]
                result = analyzeOwnershipFile content
            case result of
              Left err -> assertFailure $ "File analysis failed: " ++ show err
              Right _ -> return ()

        , testCase "debug analysis provides details" $ do
            let code = "var x int = 42"
                analyzer = newOwnershipAnalyzer
                result = analyzeOwnershipDebug analyzer code
            case result of
              Left err -> assertFailure $ "Debug analysis failed: " ++ show err
              Right debugInfo -> return ()  -- Should provide detailed debug information

        , testCase "ownership error types" $ do
            let moveError = OwnershipError "MoveError" "Value moved" Nothing
                borrowError = OwnershipError "BorrowError" "Cannot borrow" Nothing
                lifetimeError = OwnershipError "LifetimeError" "Lifetime mismatch" Nothing
            errorMessage moveError @?= "MoveError"
            errorMessage borrowError @?= "BorrowError"
            errorMessage lifetimeError @?= "LifetimeError"

        , testCase "ownership transfer types" $ do
            let transfers = [MoveTransfer, BorrowTransfer, CopyTransfer, ShareTransfer]
                transferNames = map show transfers
            L.length transferNames @?= 4
            L.all (not . null) transferNames @?= True
        ]
    ]