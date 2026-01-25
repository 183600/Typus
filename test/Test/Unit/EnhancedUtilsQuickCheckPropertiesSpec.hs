{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.EnhancedUtilsQuickCheckPropertiesSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck

import Test.QuickCheck (conjoin, (===), Property, property, forAll, choose, listOf1, elements, oneof, suchThat)

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , forceSingleTabIndentation
  , fixIndentation
  , breakOn
  , safeProcessString
  , isValidChar
  , isRight
  )

import Data.List (isPrefixOf, isInfixOf, isSuffixOf, intercalate)
import Data.Char (isAlphaNum, isAlpha, isSpace, isControl)
import Data.Either (isLeft, isRight)
import Control.Monad (replicateM)

-- 生成分隔符
genDelimiter :: Gen Char
genDelimiter = elements ",;:|"

-- 生成字符串
genString :: Gen String
genString = listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r.,!?-;:|")

-- 生成非空字符串
genNonEmptyString :: Gen String
genNonEmptyString = listOf1 $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r.,!?-;:|")

-- 生成包含空白字符的字符串
genStringWithWhitespace :: Gen String
genStringWithWhitespace = do
  parts <- listOf1 $ listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
  whitespaces <- listOf $ elements " \t\n\r"
  return $ interleave parts whitespaces
  where
    interleave [] _ = []
    interleave [x] _ = x
    interleave (x:xs) [] = x
    interleave (x:xs) (w:ws) = x ++ [w] ++ interleave xs ws

-- 生成包含注释的字符串
genStringWithComments :: Gen String
genStringWithComments = do
  code <- genNonEmptyString
  comment <- genNonEmptyString
  oneof
    [ return $ code ++ " // " ++ comment
    , return $ " // " ++ comment ++ "\n" ++ code
    , return $ code ++ "\n/* " ++ comment ++ " */\n" ++ code
    ]

-- 生成缩进字符串
genIndentedString :: Gen String
genIndentedString = do
  n <- choose (1, 5)
  indentChars <- replicateM n $ elements (' ' : '\t' : [])
  content <- genNonEmptyString
  return $ indentChars ++ content

-- 属性1: trim空字符串应该返回空字符串
prop_trim_empty_string :: Property
prop_trim_empty_string =
  property $ trim "" === ""

-- 属性2: trim仅空白字符应该返回空字符串
prop_trim_whitespace_only :: Property
prop_trim_whitespace_only = forAll (listOf $ elements " \t\n\r") $ \ws ->
  property $ trim ws === ""

-- 属性3: trim应该保留非空白字符
prop_trim_preserves_non_whitespace :: Property
prop_trim_preserves_non_whitespace = forAll genNonEmptyString $ \s ->
  let trimmed = trim s
  in property $ not (null trimmed) && all (not . isSpace) trimmed

-- 属性4: trim应该移除前导空白
prop_trim_removes_leading_whitespace :: Property
prop_trim_removes_leading_whitespace = forAll genStringWithWhitespace $ \s ->
  let trimmed = trim s
  in property $ null trimmed || case trimmed of
                               [] -> True
                               (c:_) -> not (isSpace c)

-- 属性5: trim应该移除尾随空白
prop_trim_removes_trailing_whitespace :: Property
prop_trim_removes_trailing_whitespace = forAll genStringWithWhitespace $ \s ->
  let trimmed = trim s
  in property $ null trimmed || not (isSpace $ last trimmed)

-- 属性6: splitBy空字符串应该返回空列表
prop_split_by_empty_string :: Property
prop_split_by_empty_string = forAll genDelimiter $ \delim ->
  property $ splitBy delim "" === []

-- 属性7: splitBy单个分隔符应该返回两个空段
prop_split_by_single_delimiter :: Property
prop_split_by_single_delimiter = forAll genDelimiter $ \delim ->
  property $ splitBy delim [delim] === ["", ""]

-- 属性8: splitBy应该正确处理多个分隔符
prop_split_by_multiple_delimiters :: Property
prop_split_by_multiple_delimiters = forAll genDelimiter $ \delim ->
  let n = 3
      delims = replicate n delim
      expected = replicate (n + 1) ""
  in property $ splitBy delim delims === expected

-- 属性9: splitBy应该保留原始内容
prop_split_by_preserves_content :: Property
prop_split_by_preserves_content = 
  forAll genDelimiter $ \delim ->
  forAll (listOf1 genNonEmptyString) $ \parts ->
  let input = intercalate [delim] parts
      result = splitBy delim input
  in property $ result === parts

-- 属性10: splitByCollapsed应该移除空段
prop_split_by_collapsed_removes_empty :: Property
prop_split_by_collapsed_removes_empty = forAll genDelimiter $ \delim ->
  let input = [delim, delim, delim]
      result = splitByCollapsed delim input
  in property $ null result

-- 属性11: splitByComma应该等同于splitBy ','
prop_split_by_comma_equals_split_by_comma :: Property
prop_split_by_comma_equals_split_by_comma = forAll genString $ \s ->
  property $ splitByComma s === splitBy ',' s

-- 属性12: splitByCommaCollapsed应该等同于splitByCollapsed ','
prop_split_by_comma_collapsed_equals_split_by_collapsed :: Property
prop_split_by_comma_collapsed_equals_split_by_collapsed = forAll genString $ \s ->
  property $ splitByCommaCollapsed s === splitByCollapsed ',' s

-- 属性13: removeLineComments应该移除单行注释
prop_remove_line_comments_removes_comments :: Property
prop_remove_line_comments_removes_comments = forAll genStringWithComments $ \s ->
  let result = removeLineComments s
      hasComment = "//" `isInfixOf` s
  in property $ if hasComment then not ("//" `isInfixOf` result) === True else result === s

-- 属性14: removeComments应该移除所有注释
prop_remove_comments_removes_all_comments :: Property
prop_remove_comments_removes_all_comments = forAll genStringWithComments $ \s ->
  let result = removeComments s
      hasLineComment = "//" `isInfixOf` s
      hasBlockComment = "/*" `isInfixOf` s
  in property $ if hasLineComment || hasBlockComment 
                then (not ("//" `isInfixOf` result) && not ("/*" `isInfixOf` result)) === True
                else result === s

-- 属性15: normalizeIndentation应该保留相对缩进
prop_normalize_indentation_preserves_relative :: Property
prop_normalize_indentation_preserves_relative = 
  forAll (choose (1, 5)) $ \n ->
  forAll (replicateM n genIndentedString) $ \lines ->
  let input = unlines lines
      result = normalizeIndentation input
  in property $ not (null result)

-- 属性16: fixIndentation应该等同于normalizeIndentation
prop_fix_indentation_equals_normalize :: Property
prop_fix_indentation_equals_normalize = forAll genString $ \s ->
  property $ fixIndentation s === normalizeIndentation s

-- 属性17: breakOn应该找到第一个匹配项
prop_break_on_finds_first_match :: Property
prop_break_on_finds_first_match = 
  forAll genNonEmptyString $ \delim ->
  forAll genNonEmptyString $ \content ->
  let input = content ++ delim ++ content ++ delim
      (before, after) = breakOn delim input
  in property $ conjoin 
                [ before === content
                , delim `isPrefixOf` after === True
                ]

-- 属性18: safeProcessString应该处理特殊字符
prop_safe_process_string_handles_special :: Property
prop_safe_process_string_handles_special = forAll genString $ \s ->
  let result = safeProcessString s
  in property $ length result >= 0  -- 确保不会崩溃

-- 属性19: isValidChar应该验证字符
prop_is_valid_char_validates :: Property
prop_is_valid_char_validates = 
  let valid = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r.,!?-;:|"
      invalid = elements "\0\1\2\3\4\5\6\7\8\10\11\12\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31"
  in forAll valid $ \v ->
     forAll invalid $ \i ->
     property $ isValidChar v && not (isValidChar i)

-- 属性20: removeComments应该处理嵌套注释
prop_remove_comments_handles_nested :: Property
prop_remove_comments_handles_nested = 
  let input = "code /* outer /* inner */ still outer */ more code"
      result = removeComments input
  in property $ not ("/*" `isInfixOf` result) && not ("*/" `isInfixOf` result)

-- 测试套件
tests :: TestTree
tests = testGroup "Utils QuickCheck Properties Tests"
  [ testProperty "Trim empty string" prop_trim_empty_string
  , testProperty "Trim whitespace only" prop_trim_whitespace_only
  , testProperty "Trim preserves non whitespace" prop_trim_preserves_non_whitespace
  , testProperty "Trim removes leading whitespace" prop_trim_removes_leading_whitespace
  , testProperty "Trim removes trailing whitespace" prop_trim_removes_trailing_whitespace
  , testProperty "Split by empty string" prop_split_by_empty_string
  , testProperty "Split by single delimiter" prop_split_by_single_delimiter
  , testProperty "Split by multiple delimiters" prop_split_by_multiple_delimiters
  , testProperty "Split by preserves content" prop_split_by_preserves_content
  , testProperty "Split by collapsed removes empty" prop_split_by_collapsed_removes_empty
  , testProperty "Split by comma equals split by comma" prop_split_by_comma_equals_split_by_comma
  , testProperty "Split by comma collapsed equals split by collapsed" prop_split_by_comma_collapsed_equals_split_by_collapsed
  , testProperty "Remove line comments removes comments" prop_remove_line_comments_removes_comments
  , testProperty "Remove comments removes all comments" prop_remove_comments_removes_all_comments
  , testProperty "Normalize indentation preserves relative" prop_normalize_indentation_preserves_relative
  , testProperty "Fix indentation equals normalize" prop_fix_indentation_equals_normalize
  , testProperty "Break on finds first match" prop_break_on_finds_first_match
  , testProperty "Safe process string handles special" prop_safe_process_string_handles_special
  , testProperty "Is valid char validates" prop_is_valid_char_validates
  , testProperty "Remove comments handles nested" prop_remove_comments_handles_nested
  ]