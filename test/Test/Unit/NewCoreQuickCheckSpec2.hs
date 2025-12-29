{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCoreQuickCheckSpec2 (tests) where

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
import Data.List (isPrefixOf, tails, isInfixOf, sort)

-- | Core functionality QuickCheck tests - Part 2
tests :: TestTree
tests = testGroup "New Core QuickCheck Tests 2"
  [ fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "splitBy and join roundtrip" prop_splitBy_join_roundtrip
  , fastProperty "removeComments preserves non-comment content" prop_removeComments_preserve_content
  , fastProperty "normalizeIndentation is idempotent" prop_normalizeIndentation_idempotent
  , fastProperty "forceSingleTabIndentation enforces tabs" prop_forceSingleTabIndentation_enforces
  , fastProperty "fixIndentation equals normalizeIndentation" prop_fixIndentation_equals_normalize
  , fastProperty "breakOn handles edge cases" prop_breakOn_edge_cases
  , fastProperty "Complex string processing pipeline" prop_complex_pipeline
  ]

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmedOnce = trim str
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce === trimmedTwice

-- Property: splitBy and join roundtrip
prop_splitBy_join_roundtrip :: Char -> String -> Property
prop_splitBy_join_roundtrip delim str =
  let parts = splitBy delim str
      rejoined = Data.List.intercalate [delim] parts
  in property $ rejoined === str

-- Property: removeComments preserves non-comment content
prop_removeComments_preserve_content :: String -> String -> Property
prop_removeComments_preserve_content before after =
  not ('"' `elem` before) && not ('\'' `elem` before) && 
  not ('"' `elem` after) && not ('\'' `elem` after) &&
  not ("/*" `isInfixOf` before) && not ("/*" `isInfixOf` after) &&
  not ("//" `isInfixOf` before) && not ("//" `isInfixOf` after) ==>
  let content = before ++ "code" ++ after
      withComments = before ++ "code /* comment */ // line comment\n" ++ after
      cleaned = removeComments withComments
  in property $ "code" `isInfixOf` cleaned .&&.
     before `isInfixOf` cleaned .&&.
     after `isInfixOf` cleaned

-- Property: normalizeIndentation is idempotent
prop_normalizeIndentation_idempotent :: String -> Property
prop_normalizeIndentation_idempotent str =
  let normalizedOnce = normalizeIndentation str
      normalizedTwice = normalizeIndentation normalizedOnce
  in property $ normalizedOnce === normalizedTwice

-- Property: forceSingleTabIndentation enforces tabs
prop_forceSingleTabIndentation_enforces :: String -> Property
prop_forceSingleTabIndentation_enforces str =
  let result = forceSingleTabIndentation str
      resultLines = lines result
      nonEmptyLines = filter (not . null . trim) resultLines
  in property $ all (\line -> case line of ('\t':_) -> True; _ -> False) nonEmptyLines

-- Property: fixIndentation equals normalizeIndentation
prop_fixIndentation_equals_normalize :: String -> Property
prop_fixIndentation_equals_normalize str =
  fixIndentation str === normalizeIndentation str

-- Property: breakOn handles edge cases
prop_breakOn_edge_cases :: String -> String -> Property
prop_breakOn_edge_cases pat haystack =
  let (before, after) = breakOn pat haystack
      haystackReconstructed = before ++ pat ++ after
  in if null pat
     then property $ before === "" .&&. after === haystack
     else if pat `isInfixOf` haystack
          then property $ haystackReconstructed === haystack
          else property $ before === haystack .&&. after === ""

-- Property: Complex string processing pipeline
prop_complex_pipeline :: String -> String -> String -> Property
prop_complex_pipeline prefix middle suffix =
  not ('"' `elem` prefix) && not ('\'' `elem` prefix) &&
  not ('"' `elem` middle) && not ('\'' `elem` middle) &&
  not ('"' `elem` suffix) && not ('\'' `elem` suffix) &&
  not ("/*" `isInfixOf` prefix) && not ("/*" `isInfixOf` middle) && not ("/*" `isInfixOf` suffix) ==>
  let input = prefix ++ "  /* comment */  " ++ middle ++ "  // line comment  " ++ suffix
      processed = removeComments $ trim $ normalizeIndentation input
      hasNoComments = not ("/*" `isInfixOf` processed) .&&. not ("//" `isInfixOf` processed)
      hasNoLeadingTrailingSpace = null processed || not (isSpace (head processed)) .&&. not (isSpace (last processed))
  in property $ hasNoComments .&&. hasNoLeadingTrailingSpace