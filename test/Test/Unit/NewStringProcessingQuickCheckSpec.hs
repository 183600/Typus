{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewStringProcessingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

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

-- Property: trim removes L.all leading L.and trailing whitespace
prop_trim_comprehensive :: String -> String -> String -> Property
prop_trim_comprehensive prefix middle suffix =
  let content = prefix ++ middle ++ suffix
      trimmed = trim content
      hasLeading = L.any isSpace prefix
      hasTrailing = L.any isSpace suffix
      noLeadingSpace = null trimmed || not (isSpace (L.head trimmed))
      noTrailingSpace = null trimmed || not (isSpace (last trimmed))
  in classify hasLeading "has leading whitespace" $
     classify hasTrailing "has trailing whitespace" $
     property $ noLeadingSpace .&&. noTrailingSpace

-- Property: splitBy L.and splitByCollapsed relationship
prop_splitBy_vs_splitByCollapsed :: Char -> String -> Property
prop_splitBy_vs_splitByCollapsed delim input =
  let regular = splitBy delim input
      collapsed = splitByCollapsed delim input
      regularLength = L.length regular
      collapsedLength = L.length collapsed
  in property $ collapsedLength <= regularLength .&&.
     (if not (delim `elem` input) then regular === collapsed else property True)

-- Property: splitByComma consistency
prop_splitByComma_consistency :: String -> Property
prop_splitByComma_consistency input =
  splitByComma input === splitBy ',' input

-- Property: splitByCommaCollapsed consistency  
prop_splitByCommaCollapsed_consistency :: String -> Property
prop_splitByCommaCollapsed_consistency input =
  splitByCommaCollapsed input === splitByCollapsed ',' input

-- Property: removeLineComments preserves non-comment content
prop_removeLineComments_preserves_content :: String -> String -> Property
prop_removeLineComments_preserves_content before after =
  not ('"' `elem` before) && not ('\'' `elem` before) &&
  not ('"' `elem` after) && not ('\'' `elem` after) &&
  not ("/" `L.isInfixOf` before) && not ("/" `L.isInfixOf` after) ==>
  let content = before ++ "\n" ++ after ++ "\n// comment\n" ++ before
      cleaned = removeLineComments content
  in property $ before `L.isInfixOf` cleaned .&&. after `L.isInfixOf` cleaned

-- Property: removeComments handles both comment types
prop_removeComments_comprehensive :: String -> String -> String -> Property
prop_removeComments_comprehensive before middle after =
  not ('"' `elem` before) && not ('\'' `elem` before) &&
  not ('"' `elem` middle) && not ('\'' `elem` middle) &&
  not ('"' `elem` after) && not ('\'' `elem` after) &&
  not ("/*" `L.isInfixOf` before) && not ("/*" `L.isInfixOf` middle) && not ("/*" `L.isInfixOf` after) ==>
  let content = before ++ " // line comment\n" ++ middle ++ " /* block comment */ " ++ after
      cleaned = removeComments content
  in property $ not ("// line comment" `L.isInfixOf` cleaned) .&&.
     not ("/* block comment */" `L.isInfixOf` cleaned) .&&.
     before `L.isInfixOf` cleaned .&&. middle `L.isInfixOf` cleaned .&&. after `L.isInfixOf` cleaned

-- Property: normalizeIndentation preserves line count
prop_normalizeIndentation_preserves_lines :: [String] -> Property
prop_normalizeIndentation_preserves_lines lineList =
  not (null lineList) ==>
  let content = unlines lineList
      normalized = normalizeIndentation content
      originalLines = lines content
      normalizedLines = lines normalized
  in property $ L.length originalLines === L.length normalizedLines

-- Property: forceSingleTabIndentation converts spaces to tabs
prop_forceSingleTabIndentation_conversion :: String -> Property
prop_forceSingleTabIndentation_conversion content =
  not (L.null (trim content)) ==>
  let spaced = "    " ++ content
      tabbed = forceSingleTabIndentation spaced
      resultLines = lines tabbed
      nonEmptyLines = L.filter (not . null . trim) resultLines
  in property $ L.all (\line -> case line of ('\t':_) -> True; _ -> False) nonEmptyLines

-- Property: breakOn correctness
prop_breakOn_correctness :: String -> String -> String -> Property
prop_breakOn_correctness prefix delimiter suffix =
  not (null delimiter) ==>
  let full = prefix ++ delimiter ++ suffix
      (before, after) = breakOn delimiter full
  in property $ before ++ delimiter ++ after === full

-- Property: String processing pipeline consistency
prop_string_pipeline_consistency :: String -> Property
prop_string_pipeline_consistency input =
  let pipeline1 = input |> trim |> removeComments |> normalizeIndentation
      pipeline2 = input |> removeComments |> trim |> normalizeIndentation
  in property $ pipeline1 === pipeline2

-- Property: splitBy roundtrip with join
prop_splitBy_roundtrip :: Char -> String -> Property
prop_splitBy_roundtrip delim input =
  let parts = splitBy delim input
      rejoined = Data.List.intercalate [delim] parts
  in rejoined === input

-- Property: trim idempotency
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmed1 = trim str
      trimmed2 = trim trimmed1
  in trimmed1 === trimmed2

-- Property: removeComments idempotency
prop_removeComments_idempotent :: String -> Property
prop_removeComments_idempotent input =
  let removedOnce = removeComments input
      removedTwice = removeComments removedOnce
  in removedOnce === removedTwice

-- Property: normalizeIndentation idempotency
prop_normalizeIndentation_idempotent :: String -> Property
prop_normalizeIndentation_idempotent input =
  let normalizedOnce = normalizeIndentation input
      normalizedTwice = normalizeIndentation normalizedOnce
  in normalizedOnce === normalizedTwice

-- Property: Complex comment scenarios
prop_complex_comment_scenarios :: String -> String -> String -> Property
prop_complex_comment_scenarios code1 code2 comment =
  not ('"' `elem` code1) && not ('\'' `elem` code1) &&
  not ('"' `elem` code2) && not ('\'' `elem` code2) &&
  not ("/*" `L.isInfixOf` code1) && not ("/*" `L.isInfixOf` code2) ==>
  let complex = code1 ++ " // comment1\n/* " ++ comment ++ " */\n" ++ code2 ++ " // comment2"
      processed = removeComments complex
  in property $ not ("// comment1" `L.isInfixOf` processed) .&&.
     not ("// comment2" `L.isInfixOf` processed) .&&.
     not ("/* " `L.isInfixOf` processed) .&&.
     not ("*/" `L.isInfixOf` processed) .&&.
     code1 `L.isInfixOf` processed .&&.
     code2 `L.isInfixOf` processed

-- Helper function for pipeline operator
(|>) :: a -> (a -> b) -> b
x |> f = f

tests :: TestTree
tests = testGroup "New String Processing QuickCheck Tests"
  [ fastProperty "trim removes L.all leading L.and trailing whitespace" prop_trim_comprehensive
  , fastProperty "splitBy vs splitByCollapsed relationship" prop_splitBy_vs_splitByCollapsed
  , fastProperty "splitByComma consistency" prop_splitByComma_consistency
  , fastProperty "splitByCommaCollapsed consistency" prop_splitByCommaCollapsed_consistency
  , fastProperty "removeLineComments preserves non-comment content" prop_removeLineComments_preserves_content
  , fastProperty "removeComments handles both comment types" prop_removeComments_comprehensive
  , fastProperty "normalizeIndentation preserves line count" prop_normalizeIndentation_preserves_lines
  , fastProperty "forceSingleTabIndentation converts spaces to tabs" prop_forceSingleTabIndentation_conversion
  , fastProperty "breakOn correctness" prop_breakOn_correctness
  , fastProperty "string pipeline consistency" prop_string_pipeline_consistency
  ]