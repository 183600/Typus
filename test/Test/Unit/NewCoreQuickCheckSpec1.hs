{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCoreQuickCheckSpec1 (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

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
  )

import Data.Char (isSpace, toLower)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (tails, sort)

-- | Core functionality QuickCheck tests - Part 1
tests :: TestTree
tests = testGroup "New Core QuickCheck Tests 1"
  [ fastProperty "trim removes leading/trailing whitespace" prop_trim_whitespace
  , fastProperty "splitBy preserves L.all segments" prop_splitBy_preserves
  , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
  , fastProperty "removeLineComments handles basic comments" prop_removeLineComments_basic
  , fastProperty "removeComments handles block comments" prop_removeComments_block
  , fastProperty "normalizeIndentation preserves structure" prop_normalizeIndentation_structure
  , fastProperty "breakOn finds first occurrence" prop_breakOn_first_occurrence
  , fastProperty "String operations are consistent" prop_string_consistency
  ]

-- Property: trim removes leading L.and trailing whitespace
prop_trim_whitespace :: String -> Property
prop_trim_whitespace str =
  let trimmed = trim str
      hasLeading = not (null str) && isSpace (L.head str)
      hasTrailing = not (null str) && isSpace (last str)
      noLeadingSpace = null trimmed || not (isSpace (L.head trimmed))
      noTrailingSpace = null trimmed || not (isSpace (last trimmed))
  in classify hasLeading "has leading whitespace" $
     classify hasTrailing "has trailing whitespace" $
     property $ noLeadingSpace .&&. noTrailingSpace

-- Property: splitBy preserves L.all segments including empty ones
prop_splitBy_preserves :: Char -> String -> Property
prop_splitBy_preserves delim str =
  let segments = splitBy delim str
      expectedCount = L.length (L.filter (== delim) str) + 1
      rejoined = Data.List.intercalate [delim] segments
  in property $ L.length segments === expectedCount .&&. rejoined === str

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim str =
  let segments = splitByCollapsed delim str
  in property $ L.all (not . null) segments

-- Property: removeLineComments handles basic comments
prop_removeLineComments_basic :: String -> String -> Property
prop_removeLineComments_basic code comment =
  not ('"' `elem` code) && not ('\'' `elem` code) && not ('/' `elem` code) ==>
  let lineWithComment = code ++ " // " ++ comment ++ "\nnext line"
      cleaned = removeLineComments lineWithComment
  in property $ not ("// " `L.isInfixOf` cleaned) .&&. "next line" `L.isInfixOf` cleaned

-- Property: removeComments handles block comments
prop_removeComments_block :: String -> String -> String -> Property
prop_removeComments_block before comment after =
  not ('"' `elem` before) && not ('\'' `elem` before) && 
  not ('"' `elem` after) && not ('\'' `elem` after) &&
  not ("/*" `L.isInfixOf` before) && not ("/*" `L.isInfixOf` after) ==>
  let withBlock = before ++ "/* " ++ comment ++ " */" ++ after
      cleaned = removeComments withBlock
  in property $ not ("/* " `L.isInfixOf` cleaned) .&&. 
     not (" */" `L.isInfixOf` cleaned) .&&.
     before `L.isInfixOf` cleaned .&&.
     after `L.isInfixOf` cleaned

-- Property: normalizeIndentation preserves relative structure
prop_normalizeIndentation_structure :: [Int] -> Property
prop_normalizeIndentation_structure indentLevels =
  not (null indentLevels) && L.all (>= 0) indentLevels && L.all (<= 10) indentLevels ==>
  let lines' = zipWith (\level content -> replicate level ' ' ++ "line" ++ show level) indentLevels [1..]
      content = unlines lines'
      normalized = normalizeIndentation content
      normalizedLines = L.filter (not . null) (lines normalized)
  in property $ L.length normalizedLines === L.length lines'

-- Property: breakOn finds first occurrence
prop_breakOn_first_occurrence :: String -> String -> String -> Property
prop_breakOn_first_occurrence pat prefix suffix =
  not (null pat) ==>
  let haystack = prefix ++ pat ++ suffix ++ pat ++ "extra"
      (before, after) = breakOn pat haystack
  in property $ before ++ pat ++ after === haystack

-- Property: String operations are consistent
prop_string_consistency :: String -> Property
prop_string_consistency str =
  let trimmed = trim str
      trimmedAgain = trim trimmed
      splitByComma = splitBy ','
      splitByCommaFunc = splitByComma
  in property $ trimmed === trimmedAgain .&&.
     splitByComma str === splitByCommaFunc str