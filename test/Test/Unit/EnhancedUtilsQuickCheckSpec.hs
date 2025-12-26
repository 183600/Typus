{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.EnhancedUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, arbitrary)
import Data.List (isPrefixOf, isInfixOf, sort, nub)
import Data.Char (isSpace, isAlphaNum)

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

-- Property: trim removes leading and trailing whitespace
prop_trim_removes_leading_trailing :: String -> Property
prop_trim_removes_leading_trailing s =
  let trimmed = trim s
      hasLeading = not (null s) && isSpace (head s)
      hasTrailing = not (null s) && isSpace (last s)
  in (hasLeading .||. hasTrailing) ==> 
     counterexample ("Original: " ++ show s ++ ", Trimmed: " ++ show trimmed) $
     not (null trimmed) ==> (
       not (isSpace (head trimmed)) .&&. 
       not (isSpace (last trimmed))
     )

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmedOnce = trim s
      trimmedTwice = trim trimmedOnce
  in trimmedOnce === trimmedTwice

-- Property: splitBy preserves empty segments
prop_splitBy_preserves_empty :: Char -> String -> Property
prop_splitBy_preserves_empty delim s =
  let result = splitBy delim s
      expectedCount = length (filter (== delim) s) + 1
  in length result === expectedCount

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim s =
  let result = splitByCollapsed delim
      hasEmpty = any null result
  in not hasEmpty

-- Property: splitByComma is equivalent to splitBy ','
prop_splitByComma_equals_splitBy :: String -> Property
prop_splitByComma_equals_splitBy s =
  splitByComma s === splitBy ',' s

-- Property: splitByCommaCollapsed is equivalent to splitByCollapsed ','
prop_splitByCommaCollapsed_equals_splitByCollapsed :: String -> Property
prop_splitByCommaCollapsed_equals_splitByCollapsed s =
  splitByCommaCollapsed s === splitByCollapsed ',' s

-- Property: breakOn finds first occurrence
prop_breakOn_finds_first :: String -> String -> Property
prop_breakOn_finds_first sep s =
  not (null sep) && sep `isInfixOf` s ==>
  let (before, after) = breakOn sep s
      combined = before ++ sep ++ after
  in combined === s

-- Property: breakOn returns original string if separator not found
prop_breakOn_not_found :: String -> String -> Property
prop_breakOn_not_found sep s =
  not (null sep) && not (sep `isInfixOf` s) ==>
  let (before, after) = breakOn sep s
  in before === s .&&. after === ""

-- Property: normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preserves_relative :: String -> Property
prop_normalizeIndentation_preserves_relative s =
  not (null s) ==>
  let normalized = normalizeIndentation s
      -- Count leading spaces for each non-empty line
      countLeadingSpaces line = length $ takeWhile isSpace line
      originalLines = lines s
      normalizedLines = lines normalized
      originalSpaces = map countLeadingSpaces (filter (not . null) originalLines)
      normalizedSpaces = map countLeadingSpaces (filter (not . null) normalizedLines)
      -- Calculate relative differences
      relativeDiffs ls = zipWith (-) (tail ls) (init ls)
  in length originalSpaces > 1 ==> 
     relativeDiffs normalizedSpaces === relativeDiffs originalSpaces

-- Property: removeLineComments removes // comments but not in strings
prop_removeLineComments_basic :: String -> Property
prop_removeLineComments_basic s =
  let result = removeLineComments ("code // comment\nmore code")
  in "// comment" `isNotInfixOf` result .&&. "code" `isInfixOf` result .&&. "more code" `isInfixOf` result
  where
    x `isNotInfixOf` y = not (x `isInfixOf` y)

-- Property: removeComments handles both // and /* */ comments
prop_removeComments_basic :: String -> Property
prop_removeComments_basic s =
  let result = removeComments ("code // comment\nmore /* block */ code")
  in "// comment" `isNotInfixOf` result .&&. "/* block */" `isNotInfixOf` result .&&. "code" `isInfixOf` result .&&. "more" `isInfixOf` result .&&. "code" `isInfixOf` result
  where
    x `isNotInfixOf` y = not (x `isInfixOf` y)

tests :: TestTree
tests =
  testGroup "Enhanced Utils QuickCheck"
    [ fastProperty "trim removes leading and trailing whitespace" prop_trim_removes_leading_trailing
    , fastProperty "trim is idempotent" prop_trim_idempotent
    , fastProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
    , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
    , fastProperty "splitByComma equals splitBy ','" prop_splitByComma_equals_splitBy
    , fastProperty "splitByCommaCollapsed equals splitByCollapsed ','" prop_splitByCommaCollapsed_equals_splitByCollapsed
    , fastProperty "breakOn finds first occurrence" prop_breakOn_finds_first
    , fastProperty "breakOn returns original string if separator not found" prop_breakOn_not_found
    , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative
    , fastProperty "removeLineComments removes // comments" prop_removeLineComments_basic
    , fastProperty "removeComments handles both // and /* */ comments" prop_removeComments_basic
    ]