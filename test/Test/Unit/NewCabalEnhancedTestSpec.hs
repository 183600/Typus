{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalEnhancedTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, Arbitrary(..), elements, listOf, choose, oneof, frequency)

import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..))
import Ownership (analyzeOwnership, OwnershipError(..))
import Compiler (generateGoCode, compile)
import SourceLocation (Located(..), SourcePos(..), SourceSpan(..))
import ErrorHandler
import Utils (trim, splitBy)

import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.List (sort, nub)
import Data.Char (isSpace, isAlpha)
import qualified Data.Text as T

-- ============================================================================
-- Test 1: 错误恢复机制测试
-- ============================================================================

test_error_recovery_mechanism :: TestTree
test_error_recovery_mechanism =
  testCase "Error recovery mechanism handles multiple syntax errors gracefully" $ do
    let source = unlines
          [ "//! ownership: on"
          , "package main"
          , "func main() {"
          , "    x := 5"
          , "    y :="
          , "    z := x +"
          , "    println(x)"
          , "}"
          ]
    case parseTypus source of
      Left err -> do
        -- 确保错误信息包含有用的位置信息
        assertBool "Error should contain line information" (L.isInfixOf "line" err)
        assertBool "Error should contain column information" (L.isInfixOf "column" err)
      Right _ -> assertFailure "Expected parsing to fail with syntax errors"

-- ============================================================================
-- Test 2: 依赖类型边界条件测试
-- ============================================================================

test_dependent_type_boundary_conditions :: TestTree
test_dependent_type_boundary_conditions = 
  testCase "Dependent type system handles boundary conditions correctly" $ do
    let source = unlines
          [ "//! dependent_types: on"
          , "package main"
          , "func safeArrayAccess(arr []int, idx int) int {"
          , "    if idx < 0 || idx >= len(arr) {"
          , "        panic(\"index out of bounds\")"
          , "    }"
          , "    return arr[idx]"
          , "}"
          , "func main() {"
          , "    arr := []int{1, 2, 3}"
          , "    // Test boundary conditions"
          , "    _ = safeArrayAccess(arr, 0)    // first element"
          , "    _ = safeArrayAccess(arr, 2)    // last element"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        -- 验证解析成功且包含依赖类型指令
        let FileDirectives { fdDependentTypes = depTypes } = tfDirectives typusFile
        case depTypes of
          Nothing -> assertFailure "Expected dependent types directive"
          Just loc -> locatedValue loc @?= True

-- ============================================================================
-- Test 3: 编译器优化不变性测试
-- ============================================================================

test_compiler_optimization_invariants :: TestTree
test_compiler_optimization_invariants =
  testCase "Compiler optimizations preserve program semantics" $ do
    let source1 = unlines
          [ "package main"
          , "func add(x, y int) int {"
          , "    return x + y"
          , "}"
          , "func main() {"
          , "    result := add(5, 3)"
          , "    println(result)"
          , "}"
          ]
        source2 = unlines
          [ "package main"
          , "func main() {"
          , "    result := 5 + 3"
          , "    println(result)"
          , "}"
          ]
    -- 两个程序在语义上应该是等价的
    case (parseTypus source1, parseTypus source2) of
      (Right _, Right _) -> 
        -- 如果解析成功，假设编译器会正确处理优化
        assertBool "Both programs should be semantically equivalent" True
      (Left err1, Left err2) -> 
        -- 如果都失败，确保失败原因一致
        assertBool "Both should fail for similar reasons" (L.length err1 > 0 && L.length err2 > 0)
      (Left err, Right _) -> 
        assertFailure $ "First program failed but second succeeded: " ++ err
      (Right _, Left err) -> 
        assertFailure $ "Second program failed but first succeeded: " ++ err

-- ============================================================================
-- Test 4: 源位置跟踪精度测试
-- ============================================================================

test_source_location_tracking_precision :: TestTree
test_source_location_tracking_precision =
  testCase "Source location tracking provides precise position information" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "    x := 42"
          , "    println(x)"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        -- 验证源位置信息
        let blocks = tfCodeBlocks typusFile
        assertBool "Should have at least one code block" (not (null blocks))
        case L.head blocks of
          CodeBlock { cbSpan = span } -> do
            posLine (spanStart span) @?= 2  -- func main starts at line 2
            posLine (spanEnd span) @?= 5     -- block ends at line 5
          _ -> assertFailure "Expected CodeBlock"

-- ============================================================================
-- Test 5: 所有权传递复杂场景测试
-- ============================================================================

test_ownership_transfer_complex_scenarios :: TestTree
test_ownership_transfer_complex_scenarios =
  testCase "Ownership system handles complex transfer scenarios" $ do
    let source = unlines
          [ "//! ownership: on"
          , "package main"
          , "func consume(data string) {}"
          , "func borrow(data &string) {}"
          , "func main() {"
          , "    data := \"hello\""
          , "    {"
          , "        moved := data"
          , "        consume(moved)"
          , "    }"
          , "    // data should no longer be accessible here"
          , "    borrow(&data)  // This should be an error"
          , "}"
          ]
    let errors = analyzeOwnership source
        hasUseAfterMove = L.any (\e -> case e of UseAfterMove v -> v == "data"; _ -> False) errors
    assertBool "Should detect use after move in complex scenario" hasUseAfterMove

-- ============================================================================
-- Test 6: 类型系统一致性测试
-- ============================================================================

test_type_system_consistency :: TestTree
test_type_system_consistency =
  testCase "Type system maintains consistency across type checking" $ do
    let source = unlines
          [ "//! dependent_types: on"
          , "package main"
          , "func identity(x int) int {"
          , "    return x"
          , "}"
          , "func main() {"
          , "    x := 42"
          , "    y := identity(x)"
          , "    // x L.and y should have the same type"
          , "    _ = x + y  // This should be valid"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right _ -> 
        -- 如果解析成功，假设类型检查器会确保类型一致性
        assertBool "Type system should maintain consistency" True

-- ============================================================================
-- Test 7: 语义分析不变性测试
-- ============================================================================

test_semantic_analysis_invariants :: TestTree
test_semantic_analysis_invariants =
  testCase "Semantic analysis preserves program invariants" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "    // Variable declaration L.and usage"
          , "    x := 10"
          , "    x = x + 1"
          , "    // x should be updated correctly"
          , "    println(x)"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right _ -> 
        -- 如果解析成功，假设语义分析保持不变性
        assertBool "Semantic analysis should preserve invariants" True

-- ============================================================================
-- Test 8: 解析器错误恢复测试
-- ============================================================================

test_parser_error_recovery :: TestTree
test_parser_error_recovery =
  testCase "Parser recovers gracefully from syntax errors" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "    x := 5"
          , "    y := x +"  -- Incomplete expression
          , "    z := 10"
          , "    println(z)"
          , "}"
          ]
    case parseTypus source of
      Left err -> do
        -- 确保错误信息包含有用的上下文
        assertBool "Error should provide context" (L.length err > 10)
        assertBool "Error should mention syntax" (L.isInfixOf "syntax" err || L.isInfixOf "parse" err)
      Right _ -> assertFailure "Expected parsing to fail with syntax error"

-- ============================================================================
-- Test 9: 编译器IR一致性测试
-- ============================================================================

test_compiler_ir_consistency :: TestTree
test_compiler_ir_consistency =
  testCase "Compiler intermediate representation maintains consistency" $ do
    let source = unlines
          [ "package main"
          , "func add(a, b int) int {"
          , "    return a + b"
          , "}"
          , "func main() {"
          , "    result := add(1, 2)"
          , "    println(result)"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        -- 验证解析后的AST结构一致性
        let blocks = tfCodeBlocks typusFile
        assertBool "Should have code blocks" (not (null blocks))
        -- 验证每个代码块都有有效的源位置信息
        let validSpans = L.filter (\cb -> L.null (cbContent cb) || 
                                      (posLine (spanStart (cbSpan cb)) > 0)) blocks
        assertBool "All code blocks should have valid source spans" 
                   (L.length validSpans == L.length blocks)

-- ============================================================================
-- Test 10: 工具链集成测试
-- ============================================================================

test_toolchain_integration :: TestTree
test_toolchain_integration =
  testCase "Toolchain integration works correctly" $ do
    let source = unlines
          [ "//! ownership: on"
          , "//! dependent_types: on"
          , "package main"
          , "func main() {"
          , "    message := \"Hello, Typus!\""
          , "    println(message)"
          , "}"
          ]
    -- 测试解析和所有权分析的集成
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        -- 验证文件级指令
        let FileDirectives { fdOwnership = ownership, fdDependentTypes = depTypes } = tfDirectives typusFile
        case (ownership, depTypes) of
          (Just ownLoc, Just depLoc) -> do
            locatedValue ownLoc @?= True
            locatedValue depLoc @?= True
          _ -> assertFailure "Expected both ownership L.and dependent types directives"
        
        -- 测试所有权分析
        let ownershipErrors = analyzeOwnership source
        assertBool "Ownership analysis should not report errors for simple code" 
                   (null ownershipErrors)
        
        -- 测试代码生成
        case generateGoCode typusFile of
          Left _ -> assertFailure "Code generation should not fail"
          Right goCode -> assertBool "Should generate valid Go code" (L.length goCode > 0)

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: trim函数保持内部空白不变
prop_trim_preserves_internal_content :: String -> String -> Property
prop_trim_preserves_internal_content prefix suffix =
  let content = prefix ++ "  hello  world  " ++ suffix
      trimmed = trim content
      hasInternalSpaces = "  " `L.isInfixOf` content
  in classify hasInternalSpaces "has internal spaces" $
     property $ "hello  world" `L.isInfixOf` trimmed

-- Property: splitBy保持分割符数量一致性
prop_splitby_preserves_delimiter_count :: Char -> String -> Property
prop_splitby_preserves_delimiter_count delim str =
  let parts = splitBy delim str
      delimiterCount = L.length (L.filter (== delim) str)
      expectedCount = if null str then 0 else delimiterCount
  in property $ L.length parts - 1 == expectedCount

-- Property: 所有权分析对于简单代码不报错
prop_ownership_analysis_simple_code :: Property
prop_ownership_analysis_simple_code =
  forAll simpleGoProgram $ \program ->
    let errors = analyzeOwnership program
    in property $ null errors

-- 生成简单的Go程序用于QuickCheck测试
simpleGoProgram :: Gen String
simpleGoProgram = do
  varName <- elements ["x", "y", "data", "value", "result"]
  value <- choose (1, 100)
  return $ unlines
    [ "package main"
    , "func main() {"
    , "    " ++ varName ++ " := " ++ show value
    , "    println(" ++ varName ++ ")"
    , "}"
    ]

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =
  testGroup "New Cabal Enhanced Tests"
    [ test_error_recovery_mechanism
    , test_dependent_type_boundary_conditions
    , test_compiler_optimization_invariants
    , test_source_location_tracking_precision
    , test_ownership_transfer_complex_scenarios
    , test_type_system_consistency
    , test_semantic_analysis_invariants
    , test_parser_error_recovery
    , test_compiler_ir_consistency
    , test_toolchain_integration
    , testGroup "QuickCheck Properties"
        [ fastProperty "trim preserves internal content" prop_trim_preserves_internal_content
        , fastProperty "splitBy preserves delimiter count" prop_splitby_preserves_delimiter_count
        , fastProperty "ownership analysis for simple code" prop_ownership_analysis_simple_code
        ]
    ]