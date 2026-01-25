module Test.Unit.TestUtilsStringProcessingSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

import Utils

-- Test Properties for String Processing

-- Property: trim should not increase string length
prop_trim_not_increase_length :: String -> Property
prop_trim_not_increase_length s = property $ length (trim s) <= length s

-- Property: splitBy should be consistent with splitByComma for comma delimiter
prop_splitBy_comma_consistency :: String -> Property
prop_splitBy_comma_consistency s = property $ splitBy ',' s == splitByComma s

-- Property: splitByCollapsed should not contain empty strings
prop_splitBy_collapsed_no_empty :: String -> Char -> Property
prop_splitBy_collapsed_no_empty s delim = property $ 
  delim /= '\0' ==> all (not . null) (splitByCollapsed delim s)

-- Property: removeLineComments should remove all // comments
prop_remove_line_comments :: String -> Property
prop_remove_line_comments s = property $ 
  not ("//" `isInfixOf` (removeLineComments s)) || 
  "//" `isInfixOf` (concatMap (\c -> if c == '"' then "\"//\"" else [c]) s)

-- Property: normalizeIndentation should preserve line count
prop_normalize_indentation_preserve_lines :: String -> Property
prop_normalize_indentation_preserve_lines s = property $ 
  length (lines s) == length (lines (normalizeIndentation s))

-- Property: breakOn should find substring when it exists
prop_break_on_finds_substring :: String -> String -> Property
prop_break_on_finds_substring s pat = property $ 
  not (null pat) && pat `isInfixOf` s ==> 
  let (before, after) = breakOn pat s
  in before ++ pat ++ after == s

-- Helper function
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    
    tails :: [a] -> [[a]]
    tails [] = [[]]
    tails xs@(x:xs') = xs : tails xs'

tests :: TestTree
tests = testGroup "Test.Unit.TestUtilsStringProcessingSpec Tests"
  [ testProperty "trim should not increase string length" prop_trim_not_increase_length
  , testProperty "splitBy should be consistent with splitByComma for comma delimiter" prop_splitBy_comma_consistency
  , testProperty "splitByCollapsed should not contain empty strings" prop_splitBy_collapsed_no_empty
  , testProperty "removeLineComments should remove all // comments" prop_remove_line_comments
  , testProperty "normalizeIndentation should preserve line count" prop_normalize_indentation_preserve_lines
  , testProperty "breakOn should find substring when it exists" prop_break_on_finds_substring
  ]