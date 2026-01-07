module Test.Unit.UtilsStringProcessingSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils

-- Test trim function
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

prop_trim_no_leading_trailing_whitespace :: String -> Property
prop_trim_no_leading_trailing_whitespace s = 
  let trimmed = trim s
  in property $ not (null trimmed) ==> 
    head trimmed /= ' ' && last trimmed /= ' '

-- Test splitBy function
prop_splitBy_concatenation :: Char -> String -> Property
prop_splitBy_concatenation c s = 
  let parts = splitBy c s
  in property $ not (null parts) ==> 
    concat (intersperse [c] parts) `elem` [s, s ++ [c], [c] ++ s]

-- Test splitByCollapsed function
prop_splitByCollapsed_no_empty_strings :: Char -> String -> Property
prop_splitByCollapsed_no_empty_strings c s = 
  let parts = splitByCollapsed c s
  in property $ all (not . null) parts

prop_splitByCollapsed_concatenation :: Char -> String -> Property
prop_splitByCollapsed_concatenation c s = 
  let parts = splitByCollapsed c s
  in property $ not (null parts) ==> 
    concat (intersperse [c] parts) `elem` [s, filter (/= c) s]

-- Helper function
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs

tests :: TestTree
tests = testGroup "Utils String Processing Tests"
  [ testProperty "trim is idempotent" prop_trim_idempotent
  , testProperty "trim removes leading and trailing whitespace" prop_trim_no_leading_trailing_whitespace
  , testProperty "splitBy concatenation property" prop_splitBy_concatenation
  , testProperty "splitByCollapsed has no empty strings" prop_splitByCollapsed_no_empty_strings
  , testProperty "splitByCollapsed concatenation property" prop_splitByCollapsed_concatenation
  ]