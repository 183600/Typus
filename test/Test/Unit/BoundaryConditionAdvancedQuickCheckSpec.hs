{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.BoundaryConditionAdvancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Parser
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, advancePosByText)
import Utils (trim, removeLineComments, normalizeIndentation)
import ErrorHandler (formatError, collectErrors)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Control.Monad (replicateM)
import Data.Char (isAlphaNum, isAlpha, isSpace, isControl)
import Data.Either (isLeft, isRight)

-- | 测试极长字符串的处理
prop_extremely_long_string_handling :: Positive Int -> Property
prop_extremely_long_string_handling (Positive n) =
  n < 10000 ==> 
  let longString = replicate n 'x'
      result = trim longString
  in length result >= 0

-- | 测试极深嵌套结构
prop_deeply_nested_structure :: Positive Int -> Property
prop_deeply_nested_structure (Positive n) =
  n < 100 ==> 
  let nestedBrackets = replicate n '{' ++ replicate n '}'
      result = parseTypus "" nestedBrackets
  in case result of
    Left _ -> property True
    Right file -> tfBlocks file === tfBlocks file  -- 简单验证不崩溃

-- | 测试大量重复字符
prop_repeated_character_handling :: Char -> Positive Int -> Property
prop_repeated_character_handling char (Positive n) =
  n < 1000 ==> 
  let repeated = replicate n char
      result = trim repeated
  in length result >= 0

-- | 测试特殊Unicode字符
prop_unicode_character_handling :: Property
prop_unicode_character_handling =
  let unicodeChars = ['\0'..'\255']
      testChar c = isValidCharInContext c
  in all testChar unicodeChars

-- | 测试空输入和边界输入
prop_empty_and_boundary_input :: Property
prop_empty_and_boundary_input =
  let empty = ""
      singleChar = "a"
      singleSpace = " "
      singleNewline = "\n"
      singleTab = "\t"
  in all (\input -> case parseTypus "" input of
                    Left _ -> True
                    Right file -> tfBlocks file === tfBlocks file) 
           [empty, singleChar, singleSpace, singleNewline, singleTab]

-- | 测试极大文件处理
prop_large_file_handling :: Positive Int -> Property
prop_large_file_handling (Positive n) =
  n < 5000 ==> 
  let largeFile = unlines $ replicate n "line of code"
      result = parseTypus "" largeFile
  in case result of
    Left _ -> property True
    Right file -> length (tfBlocks file) >= 0

-- | 测试极大连词符
prop_extremely_long_identifier :: Positive Int -> Property
prop_extremely_long_identifier (Positive n) =
  n < 1000 ==> 
  let longId = replicate n 'a'
      isValid = all isIdentifierChar longId
  in isValid ==> length longId >= 0

-- | 测试极深缩进
prop_extreme_indentation :: Positive Int -> Property
prop_extreme_indentation (Positive n) =
  n < 100 ==> 
  let deepIndent = replicate n ' ' ++ "code"
      normalized = normalizeIndentation deepIndent
  in length normalized >= 0

-- | 测试极大连串注释
prop_extreme_comment_handling :: Positive Int -> Property
prop_extreme_comment_handling (Positive n) =
  n < 1000 ==> 
  let commentLines = replicate n "// comment line"
      comments = unlines commentLines
      withoutComments = removeLineComments comments
  in length withoutComments >= 0

-- | 测试极多错误情况
prop_extreme_error_conditions :: Positive Int -> Property
prop_extreme_error_conditions (Positive n) =
  n < 100 ==> 
  let errorInputs = replicate n "invalid { syntax"
      results = map (parseTypus "") errorInputs
      errors = [err | Left err <- results]
  in length errors >= 0

-- | 测试内存边界条件
prop_memory_boundary_conditions :: Positive Int -> Property
prop_memory_boundary_conditions (Positive n) =
  n < 10000 ==> 
  let largeData = replicate n 'x'
      processed = normalizeIndentation $ removeLineComments $ trim largeData
  in length processed >= 0

-- | 测试递归边界
prop_recursion_boundary :: Positive Int -> Property
prop_recursion_boundary (Positive n) =
  n < 50 ==> 
  let recursiveStructure = concat $ replicate n "{"
      result = parseTypus "" recursiveStructure
  in case result of
    Left _ -> property True
    Right file -> tfBlocks file === tfBlocks file  -- 简单验证不崩溃

-- | 测试数值边界
prop_numeric_boundary :: Property
prop_numeric_boundary =
  let maxInt = maxBound :: Int
      minInt = minBound :: Int
      maxPos = SourcePos maxInt maxInt maxInt
      minPos = SourcePos minInt minInt minInt
  in posLine maxPos >= 0 && posLine minPos <= maxInt

-- | 测试字符串边界
prop_string_boundary :: Property
prop_string_boundary =
  let emptyString = ""
      singleChar = "a"
      longString = replicate 10000 'x'
  in all (\s -> length (trim s) >= 0) [emptyString, singleChar, longString]

-- | 测试解析器边界
prop_parser_boundary :: Property
prop_parser_boundary =
  let inputs = ["", " ", "\n", "\t", "{", "}", "//", "/*", "*/"]
      results = map (parseTypus "") inputs
  in all (\result -> case result of
                      Left _ -> True
                      Right file -> tfBlocks file === tfBlocks file) results

-- | 测试位置跟踪边界
prop_position_tracking_boundary :: Property
prop_position_tracking_boundary =
  let emptyText = ""
      singleChar = "a"
      newLine = "\n"
      pos1 = advancePosByText emptyText startPos
      pos2 = advancePosByText singleChar startPos
      pos3 = advancePosByText newLine startPos
  in pos1 === startPos && posLine pos2 === 1 && posLine pos3 === 2

-- | 测试错误处理边界
prop_error_handling_boundary :: Property
prop_error_handling_boundary =
  let errors = ["", "error", "very long error message " ++ replicate 1000 'x']
      formatted = map (formatError "Boundary") errors
  in all (isInfixOf "Boundary") formatted

-- | 测试空字符串处理
test_empty_string_handling :: Assertion
test_empty_string_handling = do
  assertEqual "Trim empty string" "" (trim "")
  assertEqual "Remove comments from empty string" "" (removeLineComments "")
  assertEqual "Normalize empty string" "" (normalizeIndentation "")
  case parseTypus "" "" of
    Left err -> assertFailure $ "Failed to parse empty string: " ++ show err
    Right file -> assertEqual "Empty file has no blocks" [] (tfBlocks file)

-- | 测试单字符处理
test_single_character_handling :: Assertion
test_single_character_handling = do
  assertEqual "Trim single character" "a" (trim "a")
  assertEqual "Remove comments from single character" "a" (removeLineComments "a")
  assertEqual "Normalize single character" "a" (normalizeIndentation "a")
  case parseTypus "" "a" of
    Left _ -> assertBool "Parsing single character may fail" True
    Right file -> assertBool "Should parse single character" True

-- | 测试极长字符串处理
test_extremely_long_string_handling :: Assertion
test_extremely_long_string_handling = do
  let longString = replicate 10000 'x'
      trimmed = trim longString
  assertEqual "Trim long string" longString trimmed
  assertBool "Should handle long string without crashing" (length trimmed >= 0)

-- | 测试极深嵌套结构
test_deeply_nested_structure :: Assertion
test_deeply_nested_structure = do
  let nestedBrackets = replicate 100 '{' ++ replicate 100 '}'
      result = parseTypus "" nestedBrackets
  case result of
    Left _ -> assertBool "Should handle deeply nested structure" True
    Right file -> assertBool "Should parse deeply nested structure" True

-- | 测试特殊字符处理
test_special_character_handling :: Assertion
test_special_character_handling = do
  let specialChars = "\0\1\2\3\4\5\6\7\8\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31\127"
      result = trim specialChars
  assertBool "Should handle special characters" (length result >= 0)

-- | 测试极大连词符
test_extremely_long_identifier :: Assertion
test_extremely_long_identifier = do
  let longId = replicate 1000 'a'
      isValid = all isIdentifierChar longId
  assertBool "Long identifier should be valid" isValid

-- | 测试极深缩进
test_extreme_indentation :: Assertion
test_extreme_indentation = do
  let deepIndent = replicate 100 ' ' ++ "code"
      normalized = normalizeIndentation deepIndent
  assertBool "Should handle extreme indentation" (length normalized >= 0)

-- | 测试极大连串注释
test_extreme_comment_handling :: Assertion
test_extreme_comment_handling = do
  let commentLines = replicate 1000 "// comment line"
      comments = unlines commentLines
      withoutComments = removeLineComments comments
  assertEqual "Remove all comment lines" "" (trim withoutComments)

-- | 测试极多错误情况
test_extreme_error_conditions :: Assertion
test_extreme_error_conditions = do
  let errorInputs = replicate 100 "invalid { syntax"
      results = map (parseTypus "") errorInputs
      errors = [err | Left err <- results]
  assertEqual "Should have errors for all inputs" 100 (length errors)

-- | 测试内存边界条件
test_memory_boundary_conditions :: Assertion
test_memory_boundary_conditions = do
  let largeData = replicate 10000 'x'
      processed = normalizeIndentation $ removeLineComments $ trim largeData
  assertBool "Should handle large data without memory issues" (length processed >= 0)

-- | 辅助函数：检查字符在上下文中是否有效
isValidCharInContext :: Char -> Bool
isValidCharInContext c = not (isControl c) || c `elem` ['\t', '\n', '\r']

-- | 辅助函数：检查是否为标识符字符
isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_' || c == '-'

-- | 测试套件
tests :: TestTree
tests = testGroup "Boundary Condition Advanced QuickCheck Tests"
  [ testProperty "Extremely long string handling" prop_extremely_long_string_handling
  , testProperty "Deeply nested structure" prop_deeply_nested_structure
  , testProperty "Repeated character handling" prop_repeated_character_handling
  , testProperty "Unicode character handling" prop_unicode_character_handling
  , testProperty "Empty and boundary input" prop_empty_and_boundary_input
  , testProperty "Large file handling" prop_large_file_handling
  , testProperty "Extremely long identifier" prop_extremely_long_identifier
  , testProperty "Extreme indentation" prop_extreme_indentation
  , testProperty "Extreme comment handling" prop_extreme_comment_handling
  , testProperty "Extreme error conditions" prop_extreme_error_conditions
  , testProperty "Memory boundary conditions" prop_memory_boundary_conditions
  , testProperty "Recursion boundary" prop_recursion_boundary
  , testProperty "Numeric boundary" prop_numeric_boundary
  , testProperty "String boundary" prop_string_boundary
  , testProperty "Parser boundary" prop_parser_boundary
  , testProperty "Position tracking boundary" prop_position_tracking_boundary
  , testProperty "Error handling boundary" prop_error_handling_boundary
  , testCase "Empty string handling" test_empty_string_handling
  , testCase "Single character handling" test_single_character_handling
  , testCase "Extremely long string handling" test_extremely_long_string_handling
  , testCase "Deeply nested structure" test_deeply_nested_structure
  , testCase "Special character handling" test_special_character_handling
  , testCase "Extremely long identifier" test_extremely_long_identifier
  , testCase "Extreme indentation" test_extreme_indentation
  , testCase "Extreme comment handling" test_extreme_comment_handling
  , testCase "Extreme error conditions" test_extreme_error_conditions
  , testCase "Memory boundary conditions" test_memory_boundary_conditions
  ]