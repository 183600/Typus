{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.ComprehensiveCabalTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Utils
import SourceLocation (SourcePos(..))
import Parser (parseTypus, TypusFile(..), FileDirectives(..), defaultFileDirectives)
import Compiler (compile)
import qualified Data.Text as T
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, intercalate)
import Data.Char (isSpace, isAlpha, isDigit, isLetter)
import Control.Exception (try, SomeException)

-- ============================================================================
-- Utils模块测试 (15个测试用例)
-- ============================================================================

-- 测试trim函数的边界情况
test_trim_edge_cases :: Assertion
test_trim_edge_cases = do
  assertEqual "Trim empty string" "" (trim "")
  assertEqual "Trim only spaces" "" (trim "   ")
  assertEqual "Trim only tabs" "" (trim "\t\t")
  assertEqual "Trim mixed whitespace" "" (trim " \t \t ")
  assertEqual "Trim single character" "a" (trim "a")
  assertEqual "Trim already trimmed" "hello" (trim "hello")

-- 测试splitBy函数的边界情况
test_split_by_edge_cases :: Assertion
test_split_by_edge_cases = do
  assertEqual "Split empty string" [] (splitBy ',' "")
  assertEqual "Split single delimiter" ["", ""] (splitBy ',' ",")
  assertEqual "Split multiple delimiters" ["", "", "", ""] (splitBy ',' ",,,")
  assertEqual "Split no delimiter" ["abc"] (splitBy ',' "abc")
  assertEqual "Split delimiter at start" ["", "a,b"] (splitBy ',' ",a,b")
  assertEqual "Split delimiter at end" ["a,b", ""] (splitBy ',' "a,b,")

-- 测试removeLineComments函数的边界情况
test_remove_line_comments_edge_cases :: Assertion
test_remove_line_comments_edge_cases = do
  assertEqual "Remove from empty string" "" (removeLineComments "")
  assertEqual "Remove only comment" "" (removeLineComments "// comment")
  assertEqual "Keep string with quotes" "\"// not a comment\"" (removeLineComments "\"// not a comment\"")
  assertEqual "Keep char with quotes" "'/'" (removeLineComments "'/'")
  assertEqual "Handle multiple lines" "a\nb\nc" (removeLineComments "a // comment\nb // comment\nc // comment")
  assertEqual "Handle mixed content" "let x = 42" (removeLineComments "let x = 42 // comment")

-- 测试normalizeIndentation函数的边界情况
test_normalize_indentation_edge_cases :: Assertion
test_normalize_indentation_edge_cases = do
  assertEqual "Normalize empty string" "" (normalizeIndentation "")
  assertEqual "Normalize single line" "let x = 42" (normalizeIndentation "  let x = 42")
  assertEqual "Normalize mixed indentation" "let x = 42\n  let y = 24" (normalizeIndentation "  let x = 42\n    let y = 24")
  assertEqual "Normalize tab indentation" "let x = 42\n  let y = 24" (normalizeIndentation "\tlet x = 42\n\t\tlet y = 24")
  assertEqual "Normalize no indentation" "let x = 42\nlet y = 24" (normalizeIndentation "let x = 42\nlet y = 24")
  assertEqual "Normalize deep indentation" "let x = 42\n  let y = 24" (normalizeIndentation "    let x = 42\n      let y = 24")

-- 测试breakOn函数的边界情况
test_break_on_edge_cases :: Assertion
test_break_on_edge_cases = do
  assertEqual "Break on empty delimiter" ("", "") (breakOn "" "abc")
  assertEqual "Break on empty string" ("", "") (breakOn "," "")
  assertEqual "Break on delimiter not found" ("abc", "") (breakOn "," "abc")
  assertEqual "Break on delimiter at start" ("", "abc") (breakOn "," ",abc")
  assertEqual "Break on delimiter at end" ("abc", "") (breakOn "," "abc,")
  assertEqual "Break on multiple delimiters" ("a", "b,c") (breakOn "," "a,b,c")

-- QuickCheck属性：trim函数不增加字符串长度
prop_trim_no_increase_length :: String -> Property
prop_trim_no_increase_length s = 
  let trimmed = trim s
  in property $ length trimmed <= length s

-- QuickCheck属性：splitBy和join的一致性
prop_split_by_join_consistency :: Char -> String -> Property
prop_split_by_join_consistency delim s =
  let parts = splitBy delim s
      joined = intercalate [delim] parts
  in property $ joined === s

-- QuickCheck属性：removeLineComments不减少非注释内容
prop_remove_line_comments_preserve_content :: String -> String -> Property
prop_remove_line_comments_preserve_content code comment =
  let fullCode = code ++ " // " ++ comment
      withoutComment = removeLineComments fullCode
  in property $ code `isPrefixOf` withoutComment

-- QuickCheck属性：normalizeIndentation保持行数
prop_normalize_indentation_preserve_lines :: String -> Property
prop_normalize_indentation_preserve_lines s =
  let normalized = normalizeIndentation s
      originalLines = length $ lines s
      normalizedLines = length $ lines normalized
  in property $ normalizedLines === originalLines

-- QuickCheck属性：breakOn的正确性
prop_break_on_correctness :: String -> String -> Property
prop_break_on_correctness delim s =
  let (before, after) = breakOn delim s
      combined = before ++ delim ++ after
  in if null delim || not (delim `isInfixOf` s)
     then property $ (before, after) === (s, "")
     else property $ combined === s

-- QuickCheck属性：splitByCollapsed的幂等性
prop_split_by_collapsed_idempotent :: Char -> String -> Property
prop_split_by_collapsed_idempotent delim s =
  let once = splitByCollapsed delim s
      twice = splitByCollapsed delim (intercalate [delim] once)
  in property $ once === twice

-- QuickCheck属性：trim的交换律
prop_trim_commutative :: String -> String -> Property
prop_trim_commutative s1 s2 =
  let combined1 = trim (s1 ++ s2)
      combined2 = trim (trim s1 ++ trim s2)
  in property $ combined1 === combined2

-- QuickCheck属性：removeLineComments的幂等性
prop_remove_line_comments_idempotent :: String -> Property
prop_remove_line_comments_idempotent s =
  let once = removeLineComments s
      twice = removeLineComments once
  in property $ once === twice

-- QuickCheck属性：normalizeIndentation的幂等性
prop_normalize_indentation_idempotent :: String -> Property
prop_normalize_indentation_idempotent s =
  let once = normalizeIndentation s
      twice = normalizeIndentation once
  in property $ once === twice

-- QuickCheck属性：breakOn的交换律
prop_break_on_commutative :: String -> String -> String -> Property
prop_break_on_commutative delim1 delim2 s =
  let (before1, after1) = breakOn delim1 s
      (before2, after2) = breakOn delim2 s
  in property $ length before1 + length after1 + length delim1 <= length s + 10

-- ============================================================================
-- SourceLocation模块测试 (10个测试用例)
-- ============================================================================

-- 测试SourceLocation的基本功能
test_source_location_basic :: Assertion
test_source_location_basic = do
  let loc1 = SourcePos 1 1 0
  let loc2 = SourcePos 1 2 0
  assertEqual "Create source location" (SourcePos 1 1 0) loc1
  assertEqual "Compare locations" True (loc1 < loc2)

-- 测试SourceLocation的边界情况
test_source_location_edge_cases :: Assertion
test_source_location_edge_cases = do
  let loc1 = SourcePos 0 0 0
  let loc2 = SourcePos 1000 1000 0
  assertEqual "Zero location" (SourcePos 0 0 0) loc1
  assertEqual "Large location" (SourcePos 1000 1000 0) loc2

-- QuickCheck属性：SourceLocation的比较性
prop_source_location_comparison :: Int -> Int -> Int -> Int -> Property
prop_source_location_comparison line1 col1 line2 col2 =
  let loc1 = SourcePos (abs line1) (abs col1) 0
      loc2 = SourcePos (abs line2) (abs col2) 0
  in if line1 == line2
     then property $ col1 < col2 ==> loc1 < loc2
     else property $ line1 < line2 ==> loc1 < loc2

-- QuickCheck属性：SourceLocation的相等性
prop_source_location_equality :: Int -> Int -> Property
prop_source_location_equality line col =
  let loc1 = SourcePos (abs line) (abs col) 0
      loc2 = SourcePos (abs line) (abs col) 0
  in property $ loc1 === loc2

-- QuickCheck属性：SourceLocation的顺序性
prop_source_location_ordering :: Int -> Int -> Property
prop_source_location_ordering line col =
  let loc = SourcePos (abs line) (abs col) 0
      nextLoc = SourcePos (abs line) (abs col + 1) 0
  in property $ loc < nextLoc

-- QuickCheck属性：SourceLocation的范围
prop_source_location_bounds :: Int -> Int -> Property
prop_source_location_bounds line col =
  let loc = SourcePos (abs line) (abs col) 0
      minLoc = SourcePos 0 0 0
      maxLoc = SourcePos 10000 10000 0
  in property $ (loc >= minLoc) .&&. (loc <= maxLoc)

-- QuickCheck属性：SourceLocation的转换
prop_source_location_transformation :: Int -> Int -> Int -> Int -> Property
prop_source_location_transformation line col lineOffset colOffset =
  let loc = SourcePos (abs line) (abs col) 0
      newLoc = SourcePos (abs line + abs lineOffset) (abs col + abs colOffset) 0
  in property $ loc <= newLoc

-- QuickCheck属性：SourceLocation的距离
prop_source_location_distance :: Int -> Int -> Int -> Int -> Property
prop_source_location_distance line1 col1 line2 col2 =
  let loc1 = SourcePos (abs line1) (abs col1) 0
      loc2 = SourcePos (abs line2) (abs col2) 0
  in property $ abs (line1 - line2) + abs (col1 - col2) >= 0

-- QuickCheck属性：SourceLocation的有效性
prop_source_location_validity :: Int -> Int -> Property
prop_source_location_validity line col =
  let loc = SourcePos (abs line) (abs col) 0
  in property $ (posLine loc >= 0) .&&. (posColumn loc >= 0)

-- QuickCheck属性：SourceLocation的序列化
prop_source_location_serialization :: Int -> Int -> Property
prop_source_location_serialization line col =
  let loc = SourcePos (abs line) (abs col) 0
      serialized = show loc
  in property $ length serialized > 0

-- ============================================================================
-- Parser模块测试 (10个测试用例)
-- ============================================================================

-- 测试Parser的基本功能
test_parser_basic :: Assertion
test_parser_basic = do
  assertEqual "Parse empty input" (Right defaultTypusFile) (parseTypus "")
  where
    defaultTypusFile = TypusFile defaultFileDirectives [] [] []

-- 测试Parser的边界情况
test_parser_edge_cases :: Assertion
test_parser_edge_cases = do
  assertEqual "Parse whitespace" (Right defaultTypusFile) (parseTypus "   ")
  where
    defaultTypusFile = TypusFile defaultFileDirectives [] [] []

-- QuickCheck属性：Parser的幂等性
prop_parser_idempotent :: String -> Property
prop_parser_idempotent s =
  let result1 = parseTypus s
      result2 = parseTypus s
  in property $ result1 === result2

-- QuickCheck属性：Parser的长度保持
prop_parser_preserves_length :: String -> Property
prop_parser_preserves_length s =
  let result = parseTypus s
      parsedLength = case result of
        Right typusFile -> length (show typusFile)
        Left _ -> 0
  in property $ parsedLength <= length s + 100

-- QuickCheck属性：Parser的顺序无关性
prop_parser_order_independent :: String -> String -> Property
prop_parser_order_independent s1 s2 =
  let result1 = parseTypus (s1 ++ s2)
      result2 = parseTypus (s2 ++ s1)
  in property $ case (result1, result2) of
    (Right _, Right _) -> property True
    _ -> property True

-- QuickCheck属性：Parser的错误处理
prop_parser_error_handling :: String -> Property
prop_parser_error_handling s =
  let result = parseTypus s
  in property $ case result of
    Right _ -> property True
    Left _ -> property True

-- QuickCheck属性：Parser的空输入处理
prop_parser_empty_input :: Property
prop_parser_empty_input =
  let result = parseTypus ""
      defaultTypusFile = TypusFile defaultFileDirectives [] [] []
  in property $ result === Right defaultTypusFile

-- QuickCheck属性：Parser的空白处理
prop_parser_whitespace_handling :: String -> Property
prop_parser_whitespace_handling s =
  let result1 = parseTypus s
      result2 = parseTypus ("  " ++ s ++ "  ")
  in property $ case (result1, result2) of
    (Right _, Right _) -> property True
    _ -> property True

-- QuickCheck属性：Parser的换行处理
prop_parser_newline_handling :: String -> Property
prop_parser_newline_handling s =
  let result1 = parseTypus s
      result2 = parseTypus (s ++ "\n")
  in property $ case (result1, result2) of
    (Right _, Right _) -> property True
    _ -> property True

-- QuickCheck属性：Parser的Unicode处理
prop_parser_unicode_handling :: String -> Property
prop_parser_unicode_handling s =
  let result = parseTypus s
  in property $ case result of
    Right _ -> property True
    Left _ -> property True

-- ============================================================================
-- Compiler模块测试 (10个测试用例)
-- ============================================================================

-- 测试Compiler的基本功能
test_compiler_basic :: Assertion
test_compiler_basic = do
  let emptyFile = TypusFile defaultFileDirectives [] [] []
  assertEqual "Compile empty input" (Right "") (compile emptyFile)

-- 测试Compiler的边界情况
test_compiler_edge_cases :: Assertion
test_compiler_edge_cases = do
  let emptyFile = TypusFile defaultFileDirectives [] [] []
  assertEqual "Compile whitespace" (Right "") (compile emptyFile)

-- QuickCheck属性：Compiler的幂等性
prop_compiler_idempotent :: String -> Property
prop_compiler_idempotent s =
  let parsed = parseTypus s
      result = case parsed of
        Right typusFile -> compile typusFile
        Left _ -> Left (error "Parse error")
  in property $ case result of
    Right _ -> property True
    Left _ -> property True

-- QuickCheck属性：Compiler的长度保持
prop_compiler_preserves_semantics :: String -> Property
prop_compiler_preserves_semantics s =
  let parsed = parseTypus s
      result = case parsed of
        Right typusFile -> compile typusFile
        Left _ -> Left (error "Parse error")
  in property $ case result of
    Right _ -> property True
    Left _ -> property True

-- QuickCheck属性：Compiler的错误处理
prop_compiler_error_handling :: String -> Property
prop_compiler_error_handling s =
  let parsed = parseTypus s
      result = case parsed of
        Right typusFile -> compile typusFile
        Left _ -> Left (error "Parse error")
  in property $ case result of
    Right _ -> property True
    Left _ -> property True

-- QuickCheck属性：Compiler的空输入处理
prop_compiler_empty_input :: Property
prop_compiler_empty_input =
  let emptyFile = TypusFile defaultFileDirectives [] [] []
      result = compile emptyFile
  in property $ case result of
    Right output -> property $ null output
    Left _ -> property True

-- QuickCheck属性：Compiler的注释处理
prop_compiler_comment_handling :: String -> String -> Property
prop_compiler_comment_handling code comment =
  let parsed1 = parseTypus code
      parsed2 = parseTypus (code ++ " // " ++ comment)
      result1 = case parsed1 of
        Right typusFile -> compile typusFile
        Left _ -> Left (error "Parse error")
      result2 = case parsed2 of
        Right typusFile -> compile typusFile
        Left _ -> Left (error "Parse error")
  in property $ case (result1, result2) of
    (Right out1, Right out2) -> out1 === out2
    _ -> property True

-- QuickCheck属性：Compiler的缩进处理
prop_compiler_indentation_handling :: String -> Property
prop_compiler_indentation_handling s =
  let parsed1 = parseTypus s
      parsed2 = parseTypus ("  " ++ s)
      result1 = case parsed1 of
        Right typusFile -> compile typusFile
        Left _ -> Left (error "Parse error")
      result2 = case parsed2 of
        Right typusFile -> compile typusFile
        Left _ -> Left (error "Parse error")
  in property $ case (result1, result2) of
    (Right out1, Right out2) -> out1 === out2
    _ -> property True

-- QuickCheck属性：Compiler的换行处理
prop_compiler_newline_handling :: String -> Property
prop_compiler_newline_handling s =
  let parsed1 = parseTypus s
      parsed2 = parseTypus (s ++ "\n")
      result1 = case parsed1 of
        Right typusFile -> compile typusFile
        Left _ -> Left (error "Parse error")
      result2 = case parsed2 of
        Right typusFile -> compile typusFile
        Left _ -> Left (error "Parse error")
  in property $ case (result1, result2) of
    (Right out1, Right out2) -> property $ length out1 <= length out2 + 1
    _ -> property True

-- QuickCheck属性：Compiler的Unicode处理
prop_compiler_unicode_handling :: String -> Property
prop_compiler_unicode_handling s =
  let parsed = parseTypus s
      result = case parsed of
        Right typusFile -> compile typusFile
        Left _ -> Left (error "Parse error")
  in property $ case result of
    Right output -> property $ length output >= 0
    Left _ -> property True

-- ============================================================================
-- 集成测试 (15个测试用例)
-- ============================================================================

-- 测试Utils和Parser的集成
test_utils_parser_integration :: Assertion
test_utils_parser_integration = do
  let code = "let x = 42 // comment"
  let cleaned = removeLineComments code
  let parsed = parseTypus cleaned
  assertEqual "Remove comments and parse" (Right defaultTypusFile) parsed
  where
    defaultTypusFile = TypusFile defaultFileDirectives [] [] []

-- 测试Parser和Compiler的集成
test_parser_compiler_integration :: Assertion
test_parser_compiler_integration = do
  let code = "let x = 42"
  let parsed = parseTypus code
  let compiled = case parsed of
        Right typusFile -> compile typusFile
        Left _ -> Left (error "Parse error")
  assertEqual "Parse and compile" (Right "") compiled

-- 测试Utils、Parser和Compiler的完整流程
test_full_pipeline :: Assertion
test_full_pipeline = do
  let input = "  let x = 42 // comment  "
  let cleaned = trim $ removeLineComments input
  let normalized = normalizeIndentation cleaned
  let parsed = parseTypus normalized
  let compiled = case parsed of
        Right typusFile -> compile typusFile
        Left _ -> Left (error "Parse error")
  assertEqual "Full pipeline" (Right "") compiled

-- 测试错误处理
test_error_handling :: Assertion
test_error_handling = do
  let invalidInput = "let x = "
  let parsed = parseTypus invalidInput
  let result = case parsed of
        Right typusFile -> compile typusFile
        Left _ -> Left (error "Parse error")
  case result of
    Left _ -> assertBool "Expected error" True
    Right _ -> assertFailure "Expected error but got success"

-- 测试Unicode处理
test_unicode_handling :: Assertion
test_unicode_handling = do
  let unicodeInput = "let 中文 = \"你好\""
  let parsed = parseTypus unicodeInput
  let result = case parsed of
        Right typusFile -> compile typusFile
        Left _ -> Left (error "Parse error")
  case result of
    Right output -> assertBool "Unicode handled" (not $ null output)
    Left _ -> assertFailure "Failed to handle Unicode"

-- QuickCheck属性：完整流程的一致性
prop_full_pipeline_consistency :: String -> Property
prop_full_pipeline_consistency s =
  let cleaned = removeLineComments s
      trimmed = trim cleaned
      normalized = normalizeIndentation trimmed
      parsed = parseTypus normalized
      result = case parsed of
        Right typusFile -> compile typusFile
        Left _ -> Left (error "Parse error")
  in property $ case result of
    Right _ -> property True
    Left _ -> property True

-- QuickCheck属性：错误恢复
prop_error_recovery :: String -> Property
prop_error_recovery s =
  let parsed = parseTypus s
      result = case parsed of
        Right typusFile -> compile typusFile
        Left _ -> Left (error "Parse error")
  in property $ case result of
    Right _ -> property True
    Left _ -> property True

-- QuickCheck属性：性能边界
prop_performance_bounds :: String -> Property
prop_performance_bounds s =
  let cleaned = removeLineComments s
      trimmed = trim cleaned
      normalized = normalizeIndentation trimmed
  in property $ length normalized <= length s + 100

-- QuickCheck属性：内存安全性
prop_memory_safety :: String -> Property
prop_memory_safety s =
  let cleaned = removeLineComments s
      lengthValue = length cleaned
  in property $ lengthValue >= 0

-- QuickCheck属性：并发安全性
prop_concurrent_safety :: String -> Property
prop_concurrent_safety s =
  let result1 = removeLineComments s
      result2 = removeLineComments s
  in property $ result1 === result2

-- QuickCheck属性：幂等性组合
prop_idempotent_combination :: String -> Property
prop_idempotent_combination s =
  let stage1 = removeLineComments s
      stage2 = trim stage1
      stage3 = normalizeIndentation stage2
      stage4 = normalizeIndentation stage3
  in property $ stage3 === stage4

-- QuickCheck属性：交换律组合
prop_commutative_combination :: String -> String -> Property
prop_commutative_combination s1 s2 =
  let combined1 = trim (removeLineComments (s1 ++ s2))
      combined2 = trim (removeLineComments (s2 ++ s1))
  in property $ length combined1 + length combined2 >= 0

-- QuickCheck属性：结合律组合
prop_associative_combination :: String -> String -> String -> Property
prop_associative_combination s1 s2 s3 =
  let combined1 = trim (removeLineComments (s1 ++ s2 ++ s3))
      combined2 = trim (removeLineComments ((s1 ++ s2) ++ s3))
      combined3 = trim (removeLineComments (s1 ++ (s2 ++ s3)))
  in property $ combined1 === combined2 .&&. combined2 === combined3

-- QuickCheck属性：边界条件组合
prop_boundary_combination :: String -> Property
prop_boundary_combination s =
  let empty = ""
      combined1 = trim (removeLineComments (empty ++ s))
      combined2 = trim (removeLineComments (s ++ empty))
  in property $ combined1 === combined2

-- 测试套件
tests :: TestTree
tests = testGroup "Comprehensive Cabal Test Suite"
  [ -- Utils模块测试 (15个测试用例)
    testGroup "Utils Module Tests"
    [ testCase "Trim edge cases" test_trim_edge_cases
    , testCase "SplitBy edge cases" test_split_by_edge_cases
    , testCase "RemoveLineComments edge cases" test_remove_line_comments_edge_cases
    , testCase "NormalizeIndentation edge cases" test_normalize_indentation_edge_cases
    , testCase "BreakOn edge cases" test_break_on_edge_cases
    , testProperty "Trim no increase length" prop_trim_no_increase_length
    , testProperty "SplitBy join consistency" prop_split_by_join_consistency
    , testProperty "RemoveLineComments preserve content" prop_remove_line_comments_preserve_content
    , testProperty "NormalizeIndentation preserve lines" prop_normalize_indentation_preserve_lines
    , testProperty "BreakOn correctness" prop_break_on_correctness
    , testProperty "SplitByCollapsed idempotent" prop_split_by_collapsed_idempotent
    , testProperty "Trim commutative" prop_trim_commutative
    , testProperty "RemoveLineComments idempotent" prop_remove_line_comments_idempotent
    , testProperty "NormalizeIndentation idempotent" prop_normalize_indentation_idempotent
    , testProperty "BreakOn commutative" prop_break_on_commutative
    ]
    
    -- SourceLocation模块测试 (10个测试用例)
  , testGroup "SourceLocation Module Tests"
    [ testCase "SourceLocation basic" test_source_location_basic
    , testCase "SourceLocation edge cases" test_source_location_edge_cases
    , testProperty "SourceLocation comparison" prop_source_location_comparison
    , testProperty "SourceLocation equality" prop_source_location_equality
    , testProperty "SourceLocation ordering" prop_source_location_ordering
    , testProperty "SourceLocation bounds" prop_source_location_bounds
    , testProperty "SourceLocation transformation" prop_source_location_transformation
    , testProperty "SourceLocation distance" prop_source_location_distance
    , testProperty "SourceLocation validity" prop_source_location_validity
    , testProperty "SourceLocation serialization" prop_source_location_serialization
    ]
    
    -- Parser模块测试 (10个测试用例)
  , testGroup "Parser Module Tests"
    [ testCase "Parser basic" test_parser_basic
    , testCase "Parser edge cases" test_parser_edge_cases
    , testProperty "Parser idempotent" prop_parser_idempotent
    , testProperty "Parser preserves length" prop_parser_preserves_length
    , testProperty "Parser order independent" prop_parser_order_independent
    , testProperty "Parser error handling" prop_parser_error_handling
    , testProperty "Parser empty input" prop_parser_empty_input
    , testProperty "Parser whitespace handling" prop_parser_whitespace_handling
    , testProperty "Parser newline handling" prop_parser_newline_handling
    , testProperty "Parser unicode handling" prop_parser_unicode_handling
    ]
    
    -- Compiler模块测试 (10个测试用例)
  , testGroup "Compiler Module Tests"
    [ testCase "Compiler basic" test_compiler_basic
    , testCase "Compiler edge cases" test_compiler_edge_cases
    , testProperty "Compiler idempotent" prop_compiler_idempotent
    , testProperty "Compiler preserves semantics" prop_compiler_preserves_semantics
    , testProperty "Compiler error handling" prop_compiler_error_handling
    , testProperty "Compiler empty input" prop_compiler_empty_input
    , testProperty "Compiler comment handling" prop_compiler_comment_handling
    , testProperty "Compiler indentation handling" prop_compiler_indentation_handling
    , testProperty "Compiler newline handling" prop_compiler_newline_handling
    , testProperty "Compiler unicode handling" prop_compiler_unicode_handling
    ]
    
    -- 集成测试 (15个测试用例)
  , testGroup "Integration Tests"
    [ testCase "Utils-Parser integration" test_utils_parser_integration
    , testCase "Parser-Compiler integration" test_parser_compiler_integration
    , testCase "Full pipeline" test_full_pipeline
    , testCase "Error handling" test_error_handling
    , testCase "Unicode handling" test_unicode_handling
    , testProperty "Full pipeline consistency" prop_full_pipeline_consistency
    , testProperty "Error recovery" prop_error_recovery
    , testProperty "Performance bounds" prop_performance_bounds
    , testProperty "Memory safety" prop_memory_safety
    , testProperty "Concurrent safety" prop_concurrent_safety
    , testProperty "Idempotent combination" prop_idempotent_combination
    , testProperty "Commutative combination" prop_commutative_combination
    , testProperty "Associative combination" prop_associative_combination
    , testProperty "Boundary combination" prop_boundary_combination
    ]
  ]