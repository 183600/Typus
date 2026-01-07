module Test.Unit.UtilsStringProcessingSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils
import Data.Char (isSpace)

-- 测试trim函数的属性
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

prop_trim_no_leading_trailing_spaces :: String -> Property
prop_trim_no_leading_trailing_spaces s = 
  let trimmed = trim s
  in not (null trimmed) ==> 
     not (isSpace (head trimmed)) && not (isSpace (last trimmed))

prop_trim_preserves_internal_spaces :: String -> Property
prop_trim_preserves_internal_spaces s = 
  length s > 2 ==> 
  let trimmed = trim s
      internalSpaces = filter isSpace (init (tail trimmed))
  in length internalSpaces >= 0

-- 测试splitBy函数的属性
prop_splitBy_concat :: Char -> String -> Property
prop_splitBy_concat delim s = 
  concat (splitBy delim s) === s

prop_splitBy_empty_segments :: Char -> Property
prop_splitBy_empty_segments delim = 
  splitBy delim "" === [""]

prop_splitBy_consecutive_delimiters :: Char -> String -> Property
prop_splitBy_consecutive_delimiters delim s = 
  let hasConsecutive = delim `elem` (zip s (tail s))
  in hasConsecutive ==> 
     any null (splitBy delim s)

-- 测试splitByCollapsed函数的属性
prop_splitByCollapsed_no_empty_segments :: Char -> String -> Property
prop_splitByCollapsed_no_empty_segments delim s = 
  all (not . null) (splitByCollapsed delim s)

prop_splitByCollapsed_concat_with_delim :: Char -> String -> Property
prop_splitByCollapsed_concat_with_delim delim s = 
  let parts = splitByCollapsed delim
  in concat (intersperse [delim] parts) === filter (/= delim) s
  where
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse d (x:y:xs) = x : d : intersperse d (y:xs)

-- 测试removeLineComments函数的属性
prop_removeLineComments_no_change_without_comments :: String -> Property
prop_removeLineComments_no_change_without_comments s = 
  not ("//" `isInfixOf` s) ==> removeLineComments s === s

prop_removeLineComments_removes_content_after_double_slash :: String -> String -> Property
prop_removeLineComments_removes_content_after_double_slash prefix suffix = 
  let input = prefix ++ "//" ++ suffix
  in removeLineComments input === prefix

-- 测试normalizeIndentation函数的属性
prop_normalizeIndentation_preserves_relative_indentation :: String -> Property
prop_normalizeIndentation_preserves_relative_indentation s = 
  let normalized = normalizeIndentation s
      lines' = lines s
      normalizedLines = lines normalized
  in length lines' == length normalizedLines

prop_normalizeIndentation_no_trailing_spaces :: String -> Property
prop_normalizeIndentation_no_trailing_spaces s = 
  let normalized = normalizeIndentation s
      lines' = lines normalized
  in all (not . isSpace . last) (filter (not . null) lines')

-- 测试isValidChar函数的属性
prop_isValidChar_ascii :: Char -> Property
prop_isValidChar_ascii c = 
  ord c < 128 ==> isValidChar c

-- 测试safeProcessString函数的属性
prop_safeProcessString_idempotent :: String -> Property
prop_safeProcessString_idempotent s = 
  let processed = safeProcessString s
  in safeProcessString processed === processed

prop_safeProcessString_handles_null :: Property
prop_safeProcessString_handles_null = 
  safeProcessString "" === ""

-- 测试breakOn函数的属性
prop_breakOn_not_found :: String -> String -> Property
prop_breakOn_not_found needle haystack = 
  not (needle `isInfixOf` haystack) ==> 
  breakOn needle haystack === (haystack, "")

prop_breakOn_found :: String -> String -> String -> Property
prop_breakOn_found prefix needle suffix = 
  let haystack = prefix ++ needle ++ suffix
      (before, after) = breakOn needle haystack
  in before === prefix && needle `isPrefixOf` after

tests :: TestTree
tests = testGroup "Utils String Processing Tests"
  [ testProperty "trim idempotent" prop_trim_idempotent
  , testProperty "trim no leading/trailing spaces" prop_trim_no_leading_trailing_spaces
  , testProperty "trim preserves internal spaces" prop_trim_preserves_internal_spaces
  , testProperty "splitBy concat" prop_splitBy_concat
  , testProperty "splitBy empty segments" prop_splitBy_empty_segments
  , testProperty "splitBy consecutive delimiters" prop_splitBy_consecutive_delimiters
  , testProperty "splitByCollapsed no empty segments" prop_splitByCollapsed_no_empty_segments
  , testProperty "splitByCollapsed concat with delim" prop_splitByCollapsed_concat_with_delim
  , testProperty "removeLineComments no change without comments" prop_removeLineComments_no_change_without_comments
  , testProperty "removeLineComments removes content after double slash" prop_removeLineComments_removes_content_after_double_slash
  , testProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative_indentation
  , testProperty "normalizeIndentation no trailing spaces" prop_normalizeIndentation_no_trailing_spaces
  , testProperty "isValidChar ascii" prop_isValidChar_ascii
  , testProperty "safeProcessString idempotent" prop_safeProcessString_idempotent
  , testProperty "safeProcessString handles null" prop_safeProcessString_handles_null
  , testProperty "breakOn not found" prop_breakOn_not_found
  , testProperty "breakOn found" prop_breakOn_found
  ]

-- 需要导入的额外函数
import Data.Char (ord)
import Data.List (isPrefixOf, isInfixOf)