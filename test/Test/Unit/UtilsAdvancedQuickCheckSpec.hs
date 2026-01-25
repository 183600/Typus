{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Unit.UtilsAdvancedQuickCheckSpec where


import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck

import Utils
import Data.List (intercalate, isPrefixOf, isSuffixOf, isInfixOf)
import Data.Char (isSpace, isAlphaNum, isAlpha)
import qualified Data.Text as T
import Control.Monad (replicateM)
import Data.Either (isRight)

-- | 测试trim函数的幂等性
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

-- | 测试trim函数不改变字符串中间的空格
prop_trim_preserves_internal_spaces :: String -> String -> String -> Property
prop_trim_preserves_internal_spaces s1 s2 s3 =
  let s = s1 ++ "   " ++ s2 ++ "   " ++ s3
      trimmed = trim s
      expected = s1 ++ "   " ++ s2 ++ "   " ++ s3
  in not (all isSpace s1) && not (all isSpace s2) && not (all isSpace s3) ==>
     trimmed === expected

-- | 测试splitBy函数的逆操作
prop_splitBy_join :: Char -> String -> Property
prop_splitBy_join delim s =
  let parts = splitBy delim s
      rejoined = intercalate [delim] parts
  in rejoined === s

-- | 测试splitByComma函数与splitBy的一致性
prop_splitByComma_consistency :: String -> Property
prop_splitByComma_consistency s = splitByComma s === splitBy ',' s

-- | 测试splitByCollapsed的幂等性
prop_splitByCollapsed_idempotent :: Char -> String -> Property
prop_splitByCollapsed_idempotent delim s =
  let parts = splitByCollapsed delim s
      rejoined = intercalate [delim] parts
      parts2 = splitByCollapsed delim rejoined
  in parts2 === parts

-- | 测试removeLineComments不处理字符串中的注释
prop_removeLineComments_preserves_strings :: String -> Property
prop_removeLineComments_preserves_strings s =
  let quoted = "\"" ++ s ++ "\""
      result = removeLineComments quoted
  in result === quoted

-- | 测试removeLineComments正确移除注释
prop_removeLineComments_removes_comments :: String -> Property
prop_removeLineComments_removes_comments s =
  let code = s ++ "// comment\nmore code"
      result = removeLineComments code
      expected = s ++ "\nmore code"
  in not ("//" `isInfixOf` s) ==> result === expected

-- | 测试normalizeIndentation保持相对缩进
prop_normalizeIndentation_preserves_relative :: Positive Int -> Positive Int -> String -> Property
prop_normalizeIndentation_preserves_relative (Positive indent1) (Positive indent2) s =
  let line1 = replicate indent1 ' ' ++ s
      line2 = replicate indent2 ' ' ++ s
      text = line1 ++ "\n" ++ line2
      normalized = normalizeIndentation text
      lines' = lines normalized
  in length lines' >= 2 ==> 
     let indentDiff = abs (indent1 - indent2)
         normalizedLines = dropWhile isSpace <$> lines'
     in all (isPrefixOf s) normalizedLines

-- | 测试breakOn函数的正确性
prop_breakOn_correctness :: String -> String -> Property
prop_breakOn_correctness needle haystack =
  let (before, after) = breakOn needle haystack
  in needle `isInfixOf` haystack ==> 
     before ++ needle ++ after === haystack

-- | 测试safeProcessString的安全性
prop_safeProcessString_safe :: String -> Property
prop_safeProcessString_safe s =
  let processed = safeProcessString s
  in case processed of
    Left _ -> property True
    Right str -> property (all isValidChar str)

-- | 测试isValidChar的定义
prop_isValidChar_properties :: Char -> Property
prop_isValidChar_properties c =
  let valid = isValidChar c
  in valid ==> (isAlphaNum c || isAlpha c || isSpace c || c `elem` (".,;:!()[]{}\"'" :: String))

-- | 测试trim函数与边界条件
test_trim_edge_cases :: Assertion
test_trim_edge_cases = do
  assertEqual "Empty string" "" (trim "")
  assertEqual "All spaces" "" (trim "   ")
  assertEqual "No spaces" "test" (trim "test")
  assertEqual "Leading and trailing" "test" (trim "  test  ")
  assertEqual "Only one space" "" (trim " ")
  assertEqual "Mixed whitespace" "test" (trim "\t\n test \n\t")

-- | 测试splitBy函数的边界条件
test_splitBy_edge_cases :: Assertion
test_splitBy_edge_cases = do
  assertEqual "Empty string" [] (splitBy ',' "")
  assertEqual "Single delimiter" ["", ""] (splitBy ',' ",")
  assertEqual "No delimiters" ["test"] (splitBy ',' "test")
  assertEqual "Consecutive delimiters" ["", "", ""] (splitBy ',' ",,,")
  assertEqual "Leading and trailing" ["", "test", ""] (splitBy ',' ",test,")

-- | 测试removeLineComments的边界条件
test_removeLineComments_edge_cases :: Assertion
test_removeLineComments_edge_cases = do
  assertEqual "Empty string" "" (removeLineComments "")
  assertEqual "Only comment" "" (removeLineComments "// comment")
  assertEqual "No comment" "code" (removeLineComments "code")
  assertEqual "String with comment marker" "\"// not comment\"" (removeLineComments "\"// not comment\"")
  assertEqual "Multiple lines" "code1\ncode2" (removeLineComments "code1\n// comment\ncode2")

-- | 测试normalizeIndentation的边界条件
test_normalizeIndentation_edge_cases :: Assertion
test_normalizeIndentation_edge_cases = do
  assertEqual "Empty string" "" (normalizeIndentation "")
  assertEqual "Single line" "code" (normalizeIndentation "  code")
  assertEqual "No indentation" "code" (normalizeIndentation "code")
  assertEqual "Mixed indentation" "code\n  code2" (normalizeIndentation "  code\n    code2")

-- | 测试字符串处理的安全性
test_string_processing_safety :: Assertion
test_string_processing_safety = do
  let unsafe = "\1\2\3\4\5"
      safe = safeProcessString unsafe
  case safe of
    Left _ -> assertFailure "safeProcessString should not fail"
    Right str -> do
      assertBool "All characters should be valid" (all isValidChar str)
      assertBool "Result should be different from input" (str /= unsafe)

-- | 生成任意非空字符串用于测试
nonEmptyString :: Gen String
nonEmptyString = arbitrary `suchThat` (/= "")

-- | 测试套件
tests :: TestTree
tests = testGroup "Utils Advanced QuickCheck Tests"
  [ testProperty "Trim idempotent" prop_trim_idempotent
  , testProperty "Trim preserves internal spaces" prop_trim_preserves_internal_spaces
  , testProperty "SplitBy join" prop_splitBy_join
  , testProperty "SplitByComma consistency" prop_splitByComma_consistency
  , testProperty "SplitByCollapsed idempotent" prop_splitByCollapsed_idempotent
  , testProperty "RemoveLineComments preserves strings" prop_removeLineComments_preserves_strings
  , testProperty "RemoveLineComments removes comments" prop_removeLineComments_removes_comments
  , testProperty "NormalizeIndentation preserves relative" prop_normalizeIndentation_preserves_relative
  , testProperty "BreakOn correctness" prop_breakOn_correctness
  , testProperty "SafeProcessString safe" prop_safeProcessString_safe
  , testProperty "IsValidChar properties" prop_isValidChar_properties
  , testCase "Trim edge cases" test_trim_edge_cases
  , testCase "SplitBy edge cases" test_splitBy_edge_cases
  , testCase "RemoveLineComments edge cases" test_removeLineComments_edge_cases
  , testCase "NormalizeIndentation edge cases" test_normalizeIndentation_edge_cases
  , testCase "String processing safety" test_string_processing_safety
  ]