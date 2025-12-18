{-# LANGUAGE CPP #-}

module Test.Unit.StringUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Utils (trim, splitBy, splitByCollapsed, removeLineComments)
import Data.Maybe (listToMaybe)

prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
  in trim trimmed === trimmed

prop_trim_removes_whitespace :: String -> Property
prop_trim_removes_whitespace s =
  let result = trim s
      firstChar = listToMaybe result
      lastChar = listToMaybe (reverse result)
  in not (null result) ==> maybe True (/= ' ') firstChar && maybe True (/= ' ') lastChar

prop_splitBy_preserves_content :: Char -> NonEmptyList Char -> Property
prop_splitBy_preserves_content delim (NonEmpty s) =
  delim `notElem` s ==>
  let parts = splitBy delim s
  in concat parts === s

prop_splitBy_count :: Char -> String -> Property
prop_splitBy_count delim s =
  let parts = splitBy delim s
      delimCount = length (filter (== delim) s)
  in length parts === delimCount + 1

prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s =
  let parts = splitByCollapsed delim s
  in property $ all (not . null) parts

prop_removeLineComments_preserves_non_comment :: Property
prop_removeLineComments_preserves_non_comment =
  forAll (listOf $ elements ['a'..'z']) $ \s ->
    (not $ "//" `isInfixOf` s) ==>
    trim (removeLineComments s) === trim s
  where
    isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'

tests :: TestTree
tests = testGroup "StringUtils QuickCheck"
  [ fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "trim removes whitespace" prop_trim_removes_whitespace
  , fastProperty "splitBy preserves content" prop_splitBy_preserves_content
  , fastProperty "splitBy count matches delimiters" prop_splitBy_count
  , fastProperty "splitByCollapsed has no empty strings" prop_splitByCollapsed_no_empty
  , fastProperty "removeLineComments preserves non-comments" prop_removeLineComments_preserves_non_comment
  ]
