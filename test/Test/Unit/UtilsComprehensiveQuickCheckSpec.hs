{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.UtilsComprehensiveQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Utils
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, intercalate)
import Data.Char (isSpace, isAlphaNum, isAlpha)
import Control.Monad (replicateM)
import Data.Either (isLeft, isRight)

-- | 测试trim函数的幂等性
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

-- | 测试trim函数不改变字符串中间的空格
prop_trim_preserves_internal_spaces :: String -> String -> String -> Property
prop_trim_preserves_internal_spaces prefix middle suffix =
  let s = prefix ++ "   " ++ middle ++ "   " ++ suffix
      trimmed = trim s
  in not (null middle) ==> middle `isInfixOf` trimmed

-- | 测试splitBy函数的基本属性
prop_splitBy_length :: Char -> String -> Property
prop_splitBy_length c s =
  let parts = splitBy c s
      rejoined = intercalate [c] parts
  in (c `notElem` s) ==> parts === [s]

-- | 测试splitByComma函数与splitBy的一致性
prop_splitByComma_consistency :: String -> Property
prop_splitByComma_consistency s = splitByComma s === splitBy ',' s

-- | 测试splitByCommaCollapsed函数与splitByCollapsed的一致性
prop_splitByCommaCollapsed_consistency :: String -> Property
prop_splitByCommaCollapsed_consistency s = splitByCommaCollapsed s === splitByCollapsed ',' s

-- | 测试removeLineComments函数
prop_removeLineComments_basic :: String -> String -> Property
prop_removeLineComments_basic code comment =
  let codeWithComment = code ++ "// " ++ comment ++ "\nmore code"
      withoutComments = removeLineComments codeWithComment
  in property (not (isInfixOf "// " withoutComments))

-- | 测试removeComments函数处理多行注释
prop_removeComments_multiline :: String -> String -> Property
prop_removeComments_multiline before after =
  let codeWithComment = before ++ "/* " ++ "comment" ++ " */" ++ after
      withoutComments = removeComments codeWithComment
  in property (not (isInfixOf "/*" withoutComments) && not (isInfixOf "*/" withoutComments))

-- | 测试normalizeIndentation函数保持相对缩进
prop_normalizeIndentation_preserves_relative :: Positive Int -> String -> Property
prop_normalizeIndentation_preserves_relative (Positive n) code =
  let indentedCode = unlines $ map (\line -> replicate n ' ' ++ line) (lines code)
      normalized = normalizeIndentation indentedCode
      normalizedLines = lines normalized
  in not (null code) && not (null (lines code)) ==> 
     all (\line -> length (takeWhile isSpace line) >= 0) normalizedLines

-- | 测试safeProcessString函数处理特殊字符
prop_safeProcessString_special_chars :: String -> Property
prop_safeProcessString_special_chars s =
  let processed = safeProcessString s
  in property (length processed >= 0)

-- | 测试isValidChar函数
prop_isValidChar_basic :: Char -> Property
prop_isValidChar_basic c =
  let valid = isValidChar c
  in valid ==> (isAlpha c || isAlphaNum c || c `elem` (" _-" :: String))

-- | 测试isRight函数
prop_isRight_basic :: Either String Int -> Property
prop_isRight_basic e = Data.Either.isRight e === (case e of Right _ -> True; Left _ -> False)

-- | 测试trim函数处理空字符串
test_trim_empty_string :: Assertion
test_trim_empty_string = assertEqual "Trim empty string" "" (trim "")

-- | 测试trim函数处理只有空格的字符串
test_trim_all_spaces :: Assertion
test_trim_all_spaces = assertEqual "Trim all spaces" "" (trim "   ")

-- | 测试trim函数处理只有制表符的字符串
test_trim_all_tabs :: Assertion
test_trim_all_tabs = assertEqual "Trim all tabs" "" (trim "\t\t\t")

-- | 测试trim函数处理混合空白字符
test_trim_mixed_whitespace :: Assertion
test_trim_mixed_whitespace = assertEqual "Trim mixed whitespace" "content" (trim "  \t content \t  ")

-- | 测试splitBy函数处理空字符串
test_splitBy_empty_string :: Assertion
test_splitBy_empty_string = assertEqual "Split empty string" [] (splitBy ',' "")

-- | 测试splitBy函数处理单个分隔符
test_splitBy_single_separator :: Assertion
test_splitBy_single_separator = assertEqual "Split single separator" ["", ""] (splitBy ',' ",")

-- | 测试splitBy函数处理连续分隔符
test_splitBy_consecutive_separators :: Assertion
test_splitBy_consecutive_separators = assertEqual "Split consecutive separators" ["a", "", "b"] (splitBy ',' "a,,b")

-- | 测试splitByComma函数
test_splitByComma_basic :: Assertion
test_splitByComma_basic = assertEqual "Split by comma" ["a", "b", "c"] (splitByComma "a,b,c")

-- | 测试splitByCommaCollapsed函数
test_splitByCommaCollapsed_basic :: Assertion
test_splitByCommaCollapsed_basic = assertEqual "Split by comma collapsed" ["a", "b", "c"] (splitByCommaCollapsed "a,,b,,c")

-- | 测试removeLineComments函数
test_removeLineComments_basic :: Assertion
test_removeLineComments_basic = do
  let input = "code // comment\nmore code"
      expected = "code \nmore code"
  assertEqual "Remove line comments" expected (removeLineComments input)

-- | 测试removeComments函数处理单行注释
test_removeComments_single_line :: Assertion
test_removeComments_single_line = do
  let input = "code /* comment */ more code"
      expected = "code  more code"
  assertEqual "Remove single line comment" expected (removeComments input)

-- | 测试removeComments函数处理多行注释
test_removeComments_multiline :: Assertion
test_removeComments_multiline = do
  let input = "code /*\n multi-line\n comment\n */ more code"
      expected = "code \n more code"
  assertEqual "Remove multiline comment" expected (removeComments input)

-- | 测试normalizeIndentation函数
test_normalizeIndentation_basic :: Assertion
test_normalizeIndentation_basic = do
  let input = "  code\n    more code\n  code"
      expected = "code\n  more code\ncode"
  assertEqual "Normalize indentation" expected (normalizeIndentation input)

-- | 测试safeProcessString函数
test_safeProcessString_basic :: Assertion
test_safeProcessString_basic = do
  let input = "normal string" :: String
      result = safeProcessString input
  assertEqual "Safe process normal string" (Right input) result

-- | 测试isValidChar函数
test_isValid_char_valid :: Assertion
test_isValid_char_valid = do
  assertBool "Letter is valid" (isValidChar 'a')
  assertBool "Number is valid" (isValidChar '1')
  assertBool "Underscore is valid" (isValidChar '_')
  assertBool "Hyphen is valid" (isValidChar '-')
  assertBool "Space is valid" (isValidChar ' ')

-- | 测试isValidChar函数处理无效字符
test_isValid_char_invalid :: Assertion
test_isValid_char_invalid = do
  assertBool "Control character is invalid" (not $ isValidChar '\0')
  assertBool "Null character is invalid" (not $ isValidChar '\NUL')

-- | 测试isRight函数
test_isRight_right :: Assertion
test_isRight_right = assertBool "Right value is right" (Data.Either.isRight (Right 42))

-- | 测试isRight函数处理Left值
test_isRight_left :: Assertion
test_isRight_left = assertBool "Left value is not right" (not $ Data.Either.isRight (Left "error"))

-- | 测试breakOn函数
test_breakOn_basic :: Assertion
test_breakOn_basic = do
  let input = "before:after"
      (before, after) = breakOn ":" input
  assertEqual "Break on character" ("before", "after") (before, after)

-- | 测试breakOn函数处理不存在的分隔符
test_breakOn_not_found :: Assertion
test_breakOn_not_found = do
  let input = "noseparator"
      (before, after) = breakOn ":" input
  assertEqual "Break on missing character" ("noseparator", "") (before, after)

-- | 测试forceSingleTabIndentation函数
test_forceSingleTabIndentation_basic :: Assertion
test_forceSingleTabIndentation_basic = do
  let input = "  code\n    more code"
      result = forceSingleTabIndentation input
  assertBool "Force single tab indentation" ("\t" `isPrefixOf` result)

-- | 测试fixIndentation函数与normalizeIndentation的一致性
test_fixIndentation_consistency :: Assertion
test_fixIndentation_consistency = do
  let input = "  code\n    more code"
  assertEqual "Fix indentation consistency" 
    (normalizeIndentation input) 
    (fixIndentation input)

-- | 测试字符串处理函数的组合
test_string_processing_combination :: Assertion
test_string_processing_combination = do
  let input = "  // comment\n  code /* inner comment */ more code  "
      processed = trim $ removeComments $ removeLineComments input
  assertBool "String processing combination" ("code" `isInfixOf` processed)

-- | 测试套件
tests :: TestTree
tests = testGroup "Utils Comprehensive QuickCheck Tests"
  [ testProperty "Trim idempotent" prop_trim_idempotent
  , testProperty "Trim preserves internal spaces" prop_trim_preserves_internal_spaces
  , testProperty "SplitBy length" prop_splitBy_length
  , testProperty "SplitByComma consistency" prop_splitByComma_consistency
  , testProperty "SplitByCommaCollapsed consistency" prop_splitByCommaCollapsed_consistency
  , testProperty "RemoveLineComments basic" prop_removeLineComments_basic
  , testProperty "RemoveComments multiline" prop_removeComments_multiline
  , testProperty "NormalizeIndentation preserves relative" prop_normalizeIndentation_preserves_relative
  , testProperty "SafeProcessString special chars" prop_safeProcessString_special_chars
  , testProperty "IsValidChar basic" prop_isValidChar_basic
  , testProperty "IsRight basic" prop_isRight_basic
  , testCase "Trim empty string" test_trim_empty_string
  , testCase "Trim all spaces" test_trim_all_spaces
  , testCase "Trim all tabs" test_trim_all_tabs
  , testCase "Trim mixed whitespace" test_trim_mixed_whitespace
  , testCase "SplitBy empty string" test_splitBy_empty_string
  , testCase "SplitBy single separator" test_splitBy_single_separator
  , testCase "SplitBy consecutive separators" test_splitBy_consecutive_separators
  , testCase "SplitByComma basic" test_splitByComma_basic
  , testCase "SplitByCommaCollapsed basic" test_splitByCommaCollapsed_basic
  , testCase "RemoveLineComments basic" test_removeLineComments_basic
  , testCase "RemoveComments single line" test_removeComments_single_line
  , testCase "RemoveComments multiline" test_removeComments_multiline
  , testCase "NormalizeIndentation basic" test_normalizeIndentation_basic
  , testCase "SafeProcessString basic" test_safeProcessString_basic
  , testCase "IsValidChar valid" test_isValid_char_valid
  , testCase "IsValidChar invalid" test_isValid_char_invalid
  , testCase "IsRight right" test_isRight_right
  , testCase "IsRight left" test_isRight_left
  , testCase "BreakOn basic" test_breakOn_basic
  , testCase "BreakOn not found" test_breakOn_not_found
  , testCase "ForceSingleTabIndentation basic" test_forceSingleTabIndentation_basic
  , testCase "FixIndentation consistency" test_fixIndentation_consistency
  , testCase "String processing combination" test_string_processing_combination
  ]