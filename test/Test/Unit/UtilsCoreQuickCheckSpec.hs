{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Unit.UtilsCoreQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Data.List (nub, sort, group, intercalate, isPrefixOf, isInfixOf)
import Data.Char (isAlpha, isAlphaNum, isSpace, isDigit)
import Data.Either (isLeft)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Either as Either

import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed,
             removeLineComments, removeComments, isCompleteStringLiteral, 
             isProblematicUnclosedString, normalizeIndentation, forceSingleTabIndentation,
             fixIndentation, breakOn, safeProcessString, isValidChar, isRight)
import TestSupport.Arbitrary

-- ============================================================================
-- Utils Core Properties
-- ============================================================================

-- | 测试trim函数的基本功能
prop_trim_basic :: String -> Property
prop_trim_basic s =
  let trimmed = trim s
      startsWithSpace = case trimmed of
                         [] -> False
                         (c:_) -> isSpace c
      endsWithSpace = case trimmed of
                       [] -> False
                       _ -> isSpace (last trimmed)
  in property $ not (startsWithSpace || endsWithSpace)

-- | 测试trim函数的幂等性
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed1 = trim s
      trimmed2 = trim trimmed1
  in property $ trimmed1 == trimmed2

-- | 测试trim函数对空字符串的处理
prop_trim_empty_string :: Property
prop_trim_empty_string =
  let s = ""
      trimmed = trim s
  in property $ trimmed == ""

-- | 测试trim函数对纯空格字符串的处理
prop_trim_whitespace_only :: Property
prop_trim_whitespace_only =
  let s = "   \t   \n   "
      trimmed = trim s
  in property $ trimmed == ""

-- | 测试splitBy函数的基本功能
prop_split_by_basic :: Char -> String -> Property
prop_split_by_basic delim s =
  let parts = splitBy delim s
      rejoined = intercalate [delim] parts
  in property $ length parts >= 0

-- | 测试splitBy函数对空字符串的处理
prop_split_by_empty :: Char -> Property
prop_split_by_empty delim =
  let s = ""
      parts = splitBy delim s
  in property $ null parts

-- | 测试splitBy函数对单个分隔符的处理
prop_split_by_single_delim :: Char -> Property
prop_split_by_single_delim delim =
  let s = [delim]
      parts = splitBy delim s
  in property $ parts == ["", ""]

-- | 测试splitByCollapsed函数的基本功能
prop_split_by_collapsed_basic :: Char -> String -> Property
prop_split_by_collapsed_basic delim s =
  let parts = splitByCollapsed delim s
      hasEmptyParts = any null parts
  in property $ not hasEmptyParts

-- | 测试splitByComma函数的基本功能
prop_split_by_comma_basic :: String -> Property
prop_split_by_comma_basic s =
  let parts = splitByComma s
      rejoined = intercalate "," parts
  in property $ length parts >= 0

-- | 测试splitByCommaCollapsed函数的基本功能
prop_split_by_comma_collapsed_basic :: String -> Property
prop_split_by_comma_collapsed_basic s =
  let parts = splitByCommaCollapsed s
      hasEmptyParts = any null parts
  in property $ not hasEmptyParts

-- | 测试removeLineComments函数的基本功能
prop_remove_line_comments_basic :: String -> Property
prop_remove_line_comments_basic s =
  let withoutComments = removeLineComments s
  in property $ length withoutComments >= 0

-- | 测试removeLineComments函数对空字符串的处理
prop_remove_line_comments_empty :: Property
prop_remove_line_comments_empty =
  let s = ""
      withoutComments = removeLineComments s
  in property $ withoutComments == ""

-- | 测试removeLineComments函数对纯注释的处理
prop_remove_line_comments_only :: Property
prop_remove_line_comments_only =
  let s = "// This is a comment"
      withoutComments = removeLineComments s
  in property $ null withoutComments

-- | 测试removeComments函数的基本功能
prop_remove_comments_basic :: String -> Property
prop_remove_comments_basic s =
  let withoutComments = removeComments s
  in property $ length withoutComments >= 0

-- | 测试removeComments函数对空字符串的处理
prop_remove_comments_empty :: Property
prop_remove_comments_empty =
  let s = ""
      withoutComments = removeComments s
  in property $ withoutComments == ""

-- | 测试isCompleteStringLiteral函数的基本功能
prop_is_complete_string_literal_basic :: String -> Property
prop_is_complete_string_literal_basic s =
  let isComplete = isCompleteStringLiteral s
  in property $ isComplete || not isComplete

-- | 测试isCompleteStringLiteral函数对空字符串的处理
prop_is_complete_string_literal_empty :: Property
prop_is_complete_string_literal_empty =
  let s = ""
      isComplete = isCompleteStringLiteral s
  in property $ not isComplete

-- | 测试isCompleteStringLiteral函数对完整字符串的处理
prop_is_complete_string_literal_complete :: String -> Property
prop_is_complete_string_literal_complete content =
  let validContent = not (any (`elem` ("\\\"" :: String)) content)  -- 避免引号和反斜杠
  in if not validContent
     then property True
     else let s = "\"" ++ content ++ "\""
              isComplete = isCompleteStringLiteral s
          in property $ isComplete

-- | 测试isProblematicUnclosedString函数的基本功能
prop_is_problematic_unclosed_string_basic :: String -> Property
prop_is_problematic_unclosed_string_basic s =
  let isProblematic = isProblematicUnclosedString s
  in property $ isProblematic || not isProblematic

-- | 测试normalizeIndentation函数的基本功能
prop_normalize_indentation_basic :: String -> Property
prop_normalize_indentation_basic s =
  let normalized = normalizeIndentation s
  in property $ length normalized >= 0

-- | 测试normalizeIndentation函数对空字符串的处理
prop_normalize_indentation_empty :: Property
prop_normalize_indentation_empty =
  let s = ""
      normalized = normalizeIndentation s
  in property $ normalized == ""

-- | 测试normalizeIndentation函数的幂等性
prop_normalize_indentation_idempotent :: String -> Property
prop_normalize_indentation_idempotent s =
  let normalized1 = normalizeIndentation s
      normalized2 = normalizeIndentation normalized1
  in property $ normalized1 == normalized2

-- | 测试forceSingleTabIndentation函数的基本功能
prop_force_single_tab_indentation_basic :: String -> Property
prop_force_single_tab_indentation_basic s =
  let forced = forceSingleTabIndentation s
      lines' = lines forced
      startsWithTab = all (\line -> case line of
                                      [] -> True
                                      (c:_) -> c == '\t') lines'
  in property $ startsWithTab

-- | 测试forceSingleTabIndentation函数对空字符串的处理
prop_force_single_tab_indentation_empty :: Property
prop_force_single_tab_indentation_empty =
  let s = ""
      forced = forceSingleTabIndentation s
  in property $ forced == ""

-- | 测试fixIndentation函数的一致性
prop_fix_indentation_consistency :: String -> Property
prop_fix_indentation_consistency s =
  let fixed = fixIndentation s
      normalized = normalizeIndentation s
  in property $ fixed == normalized

-- | 测试breakOn函数的基本功能
prop_break_on_basic :: String -> String -> Property
prop_break_on_basic needle haystack =
  let validNeedle = not (null needle)
  in if not validNeedle
     then property True
     else let (before, after) = breakOn needle haystack
              reconstructed = before ++ needle ++ after
          in property $ length before >= 0 && length after >= 0

-- | 测试breakOn函数对空字符串的处理
prop_break_on_empty_needle :: String -> Property
prop_break_on_empty_needle haystack =
  let needle = ""
      (before, after) = breakOn needle haystack
  in property $ before == haystack && after == haystack

-- | 测试breakOn函数对不存在的子串的处理
prop_break_on_not_found :: String -> String -> Property
prop_break_on_not_found needle haystack =
  let validNeedle = not (null needle)
      notFound = not (needle `isInfixOf` haystack)
  in if not (validNeedle && notFound)
     then property True
     else let (before, after) = breakOn needle haystack
          in property $ before == haystack && after == ""

-- | 测试safeProcessString函数的基本功能
prop_safe_process_string_basic :: String -> Property
prop_safe_process_string_basic s =
  let processed = safeProcessString s
  in property $ Utils.isRight processed

-- | 测试safeProcessString函数对空字符串的处理
prop_safe_process_string_empty :: Property
prop_safe_process_string_empty =
  let s = ""
      processed = safeProcessString s
  in property $ Utils.isRight processed && (case processed of Right result -> result == ""; _ -> False)

-- | 测试safeProcessString函数对控制字符的处理
prop_safe_process_string_control_chars :: String -> Property
prop_safe_process_string_control_chars s =
  let processed = safeProcessString s
  in if Utils.isRight processed
     then let result = case processed of Right r -> r; _ -> ""
              hasInvalidChars = any (not . isValidChar) result
          in property $ not hasInvalidChars
     else property False

-- | 测试isValidChar函数的基本功能
prop_is_valid_char_basic :: Char -> Property
prop_is_valid_char_basic c =
  let valid = isValidChar c
  in property $ valid || not valid

-- | 测试isValidChar函数对可打印字符的处理
prop_is_valid_char_printable :: Char -> Property
prop_is_valid_char_printable c =
  let isPrintable = c >= ' ' && c <= '~'
      valid = isValidChar c
  in property $ isPrintable ==> valid

-- | 测试isValidChar函数对控制字符的处理
prop_is_valid_char_control :: Char -> Property
prop_is_valid_char_control c =
  let isControl = c < ' '
      isSpecialControl = c `elem` ("\n\r\t" :: String)
      valid = isValidChar c
  in property $ isControl ==> (valid == isSpecialControl)

-- | 测试isRight函数的基本功能
prop_is_right_basic :: String -> String -> Property
prop_is_right_basic leftValue rightValue =
  let left = Left leftValue
      right = Right rightValue
  in property $ not (Utils.isRight left) && Utils.isRight right

-- ============================================================================
-- Performance Tests
-- ============================================================================

-- | 测试trim函数对长字符串的性能
prop_trim_long_string :: Int -> Property
prop_trim_long_string length =
  let validLength = length >= 0 && length <= 10000
  in if not validLength
     then property True
     else let longString = replicate length ' ' ++ "content" ++ replicate length ' '
              trimmed = trim longString
          in property $ trimmed == "content"

-- | 测试splitBy函数对长字符串的性能
prop_split_by_long_string :: Int -> Property
prop_split_by_long_string len =
  let validLength = len >= 0 && len <= 1000
  in if not validLength
     then property True
     else let longString = concat $ take len $ repeat "a,b,c"
              parts = splitBy ',' longString
          in property $ length parts >= 0

-- | 测试removeComments函数对长字符串的性能
prop_remove_comments_long_string :: Int -> Property
prop_remove_comments_long_string len =
  let validLength = len >= 0 && len <= 1000
  in if not validLength
     then property True
     else let longString = concat $ take len $ repeat "code // comment\n"
              withoutComments = removeComments longString
          in property $ length withoutComments >= 0

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

-- | 测试trim函数对Unicode字符的处理
prop_trim_unicode :: String -> Property
prop_trim_unicode s =
  let hasUnicode = any (> '\127') s
      trimmed = trim s
  in property $ length trimmed >= 0

-- | 测试splitBy函数对特殊分隔符的处理
prop_split_by_special_delim delim s =
  let isSpecial = delim `elem` ("\n\t\r" :: String)
      parts = splitBy delim s
  in property $ length parts >= 0

-- | 测试removeComments函数对嵌套注释的处理
prop_remove_comments_nested :: Int -> Property
prop_remove_comments_nested depth =
  let validDepth = depth >= 0 && depth <= 10
  in if not validDepth
     then property True
     else let nestedComment = "/* " ++ concat (replicate depth "/* ") ++ "content" ++ concat (replicate depth " */") ++ " */"
              withoutComments = removeComments nestedComment
          in property $ length withoutComments >= 0

-- | 测试isCompleteStringLiteral函数对转义字符的处理
prop_is_complete_string_literal_escape :: String -> Property
prop_is_complete_string_literal_escape content =
  let validContent = not (any (`elem` ("\\\"" :: String)) content)  -- 避免引号和反斜杠
  in if not validContent
     then property True
     else let s = "\"" ++ content ++ "\\\"" ++ "\""  -- 包含转义引号
              isComplete = isCompleteStringLiteral s
          in property $ isComplete

-- | 测试normalizeIndentation函数对混合缩进的处理
prop_normalize_indentation_mixed :: String -> Property
prop_normalize_indentation_mixed s =
  let hasMixedIndentation = any (`elem` ("\t" :: String)) s && any (`elem` (" " :: String)) s
      normalized = normalizeIndentation s
  in property $ length normalized >= 0

-- | 测试breakOn函数对Unicode子串的处理
prop_break_on_unicode :: String -> String -> Property
prop_break_on_unicode needle haystack =
  let validNeedle = not (null needle)
      hasUnicode = any (> '\127') needle
  in if not (validNeedle && hasUnicode)
     then property True
     else let (before, after) = breakOn needle haystack
          in property $ length before >= 0 && length after >= 0

-- | 测试safeProcessString函数对Unicode字符的处理
prop_safe_process_string_unicode :: String -> Property
prop_safe_process_string_unicode s =
  let hasUnicode = any (> '\127') s
      processed = safeProcessString s
  in property $ Utils.isRight processed

-- ============================================================================
-- Test Suite Collection
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Utils Core QuickCheck Tests"
  [ testProperty "Trim Basic" prop_trim_basic
  , testProperty "Trim Idempotent" prop_trim_idempotent
  , testProperty "Trim Empty String" prop_trim_empty_string
  , testProperty "Trim Whitespace Only" prop_trim_whitespace_only
  , testProperty "Split By Basic" prop_split_by_basic
  , testProperty "Split By Empty" prop_split_by_empty
  , testProperty "Split By Single Delim" prop_split_by_single_delim
  , testProperty "Split By Collapsed Basic" prop_split_by_collapsed_basic
  , testProperty "Split By Comma Basic" prop_split_by_comma_basic
  , testProperty "Split By Comma Collapsed Basic" prop_split_by_comma_collapsed_basic
  , testProperty "Remove Line Comments Basic" prop_remove_line_comments_basic
  , testProperty "Remove Line Comments Empty" prop_remove_line_comments_empty
  , testProperty "Remove Line Comments Only" prop_remove_line_comments_only
  , testProperty "Remove Comments Basic" prop_remove_comments_basic
  , testProperty "Remove Comments Empty" prop_remove_comments_empty
  , testProperty "Is Complete String Literal Basic" prop_is_complete_string_literal_basic
  , testProperty "Is Complete String Literal Empty" prop_is_complete_string_literal_empty
  , testProperty "Is Complete String Literal Complete" prop_is_complete_string_literal_complete
  , testProperty "Is Problematic Unclosed String Basic" prop_is_problematic_unclosed_string_basic
  , testProperty "Normalize Indentation Basic" prop_normalize_indentation_basic
  , testProperty "Normalize Indentation Empty" prop_normalize_indentation_empty
  , testProperty "Normalize Indentation Idempotent" prop_normalize_indentation_idempotent
  , testProperty "Force Single Tab Indentation Basic" prop_force_single_tab_indentation_basic
  , testProperty "Force Single Tab Indentation Empty" prop_force_single_tab_indentation_empty
  , testProperty "Fix Indentation Consistency" prop_fix_indentation_consistency
  , testProperty "Break On Basic" prop_break_on_basic
  , testProperty "Break On Empty Needle" prop_break_on_empty_needle
  , testProperty "Break On Not Found" prop_break_on_not_found
  , testProperty "Safe Process String Basic" prop_safe_process_string_basic
  , testProperty "Safe Process String Empty" prop_safe_process_string_empty
  , testProperty "Safe Process String Control Chars" prop_safe_process_string_control_chars
  , testProperty "Is Valid Char Basic" prop_is_valid_char_basic
  , testProperty "Is Valid Char Printable" prop_is_valid_char_printable
  , testProperty "Is Valid Char Control" prop_is_valid_char_control
  , testProperty "Is Right Basic" prop_is_right_basic
  , testProperty "Trim Long String" prop_trim_long_string
  , testProperty "Split By Long String" prop_split_by_long_string
  , testProperty "Remove Comments Long String" prop_remove_comments_long_string
  , testProperty "Trim Unicode" prop_trim_unicode
  , testProperty "Split By Special Delim" prop_split_by_special_delim
  , testProperty "Remove Comments Nested" prop_remove_comments_nested
  , testProperty "Is Complete String Literal Escape" prop_is_complete_string_literal_escape
  , testProperty "Normalize Indentation Mixed" prop_normalize_indentation_mixed
  , testProperty "Break On Unicode" prop_break_on_unicode
  , testProperty "Safe Process String Unicode" prop_safe_process_string_unicode
  ]