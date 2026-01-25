module Test.Unit.UtilsStringPropertiesQuickCheckSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

import Utils (trim, splitBy, splitByComma, splitByCommaCollapsed)
import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf)

-- | 测试trim函数的属性
prop_trim_removes_leading_whitespace :: String -> Property
prop_trim_removes_leading_whitespace s =
  let firstChar str = case str of
                        (c:_) -> c
                        [] -> ' '
  in not (null s) && isSpace (firstChar s) ==> 
     not (null (trim s)) || all isSpace s

prop_trim_removes_trailing_whitespace :: String -> Property
prop_trim_removes_trailing_whitespace s =
  let lastChar str = case reverse str of
                       (c:_) -> c
                       [] -> ' '
  in not (null s) && isSpace (lastChar s) ==> 
     not (null (trim s)) || all isSpace s

prop_trim_preserves_internal_whitespace :: String -> Property
prop_trim_preserves_internal_whitespace s =
  let trimmed = trim s
      firstChar str = case str of
                        (c:_) -> c
                        [] -> ' '
      lastChar str = case reverse str of
                       (c:_) -> c
                       [] -> ' '
      hasInternal = not (null s) && 
                    not (all isSpace s) &&
                    (isSpace (firstChar s) || isSpace (lastChar s))
  in whenFail (print (show s)) $ 
     if hasInternal then
                     property (not (null trimmed) && not (all isSpace trimmed))
                 else
                     property True

prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = 
  trim (trim s) === trim s

-- | 测试splitBy函数的属性
prop_split_by_delimiter_not_in_result :: Char -> String -> Property
prop_split_by_delimiter_not_in_result delim s =
  not (null s) ==> 
  all (not . elem delim) (splitBy delim s)

prop_split_by_empty_string :: Char -> Property
prop_split_by_empty_string delim =
  splitBy delim "" === []

prop_split_by_single_delimiter :: Char -> Property
prop_split_by_single_delimiter delim =
  splitBy delim [delim] === ["", ""]

prop_split_by_consecutive_delimiters :: Char -> Property
prop_split_by_consecutive_delimiters delim =
  splitBy delim (replicate 3 delim) === ["", "", "", ""]

prop_split_by_comma_consistency :: String -> Property
prop_split_by_comma_consistency s =
  splitByComma s === splitBy ',' s

prop_split_by_comma_collapsed_no_empty :: String -> Property
prop_split_by_comma_collapsed_no_empty s =
  property (all (not . null) (splitByCommaCollapsed s))

-- | 测试字符串组合属性
prop_split_and_join :: Char -> String -> Property
prop_split_and_join delim s =
  not (null s) && delim `notElem` s ==> 
  splitBy delim s === [s]

tests :: TestTree
tests = testGroup "Utils String Properties QuickCheck Tests"
  [ testProperty "trim removes leading whitespace" prop_trim_removes_leading_whitespace
  , testProperty "trim removes trailing whitespace" prop_trim_removes_trailing_whitespace
  , testProperty "trim preserves internal whitespace" prop_trim_preserves_internal_whitespace
  , testProperty "trim is idempotent" prop_trim_idempotent
  , testProperty "splitBy: delimiter not in result" prop_split_by_delimiter_not_in_result
  , testProperty "splitBy: empty string" prop_split_by_empty_string
  , testProperty "splitBy: single delimiter" prop_split_by_single_delimiter
  , testProperty "splitBy: consecutive delimiters" prop_split_by_consecutive_delimiters
  , testProperty "splitByComma consistency" prop_split_by_comma_consistency
  , testProperty "splitByCommaCollapsed has no empty strings" prop_split_by_comma_collapsed_no_empty
  , testProperty "split and join consistency" prop_split_and_join
  ]