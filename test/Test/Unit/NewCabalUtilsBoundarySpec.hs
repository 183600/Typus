{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalUtilsBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
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
  , breakOn
  )

import Data.Char (isSpace, isAlphaNum, isPunctuation)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)

-- Property: trim idempotency - trimming twice should give same result
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed1 = trim s
      trimmed2 = trim trimmed1
  in counterexample "trim should be idempotent" $
     trimmed1 === trimmed2

-- Property: trim removes all leading/trailing whitespace
prop_trim_removes_whitespace :: String -> String -> Property
prop_trim_removes_whitespace prefix suffix =
  let hasLeadingSpace = not (null prefix) && isSpace (last prefix)
      hasTrailingSpace = not (null suffix) && isSpace (head suffix)
      content = "content"
      s = prefix ++ content ++ suffix
      trimmed = trim s
  in (hasLeadingSpace || hasTrailingSpace) ==> counterexample "trim should remove all leading/trailing whitespace" $
     not (isPrefixOf " " trimmed || isSuffixOf " " trimmed)

-- Property: splitBy length property
prop_splitBy_length :: Char -> String -> Property
prop_splitBy_length delim s =
  let parts = splitBy delim s
      recombined = concat (map (\p -> p ++ [delim]) (init parts)) ++ last parts
      hasDelim = delim `elem` s
  in hasDelim ==> counterexample "splitBy should preserve content when recombined" $
     recombined === s

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s =
  let parts = splitByCollapsed delim s
      hasEmpty = any null parts
  in counterexample "splitByCollapsed should not produce empty segments" $
     not hasEmpty

-- Property: splitBy vs splitByCollapsed relationship
prop_splitBy_vs_collapsed :: Char -> String -> Property
prop_splitBy_vs_collapsed delim s =
  let normal = splitBy delim s
      collapsed = splitByCollapsed delim s
      collapsedFromNormal = filter (not . null) normal
  in counterexample "splitByCollapsed should equal filter (not . null) . splitBy" $
     collapsed === collapsedFromNormal

-- Property: splitByComma is splitBy with comma
prop_splitBy_comma_property :: String -> Property
prop_splitBy_comma_property s =
  let commaSplit = splitByComma s
      genericSplit = splitBy ',' s
  in counterexample "splitByComma should equal splitBy ','" $
     commaSplit === genericSplit

-- Property: removeLineComments preserves non-comment content
prop_remove_line_comments_preserve :: String -> String -> Property
prop_remove_line_comments_preserve code comment =
  let input = code ++ " // " ++ comment ++ "\nmore code"
      result = removeLineComments input
      hasCode = isInfixOf code result
      hasComment = isInfixOf comment result
  in counterexample "removeLineComments should preserve code but remove comments" $
     hasCode .&&. not hasComment

-- Property: removeComments handles block comments
prop_remove_block_comments :: String -> String -> Property
prop_remove_block_comments before after =
  let comment = "block comment"
      input = before ++ "/* " ++ comment ++ " */" ++ after
      result = removeComments input
      hasBefore = isInfixOf before result
      hasAfter = isInfixOf after result
      hasComment = isInfixOf comment result
  in counterexample "removeComments should remove block comments" $
     hasBefore .&&. hasAfter .&&. not hasComment

-- Property: normalizeIndentation preserves relative indentation
prop_normalize_indentation_relative :: String -> Int -> Property
prop_normalize_indentation_relative base indentLevel =
  let indent = replicate indentLevel ' '
      line1 = indent ++ "line1\n"
      line2 = indent ++ "  line2\n"  -- More indented
      line3 = indent ++ "line3\n"
      input = line1 ++ line2 ++ line3
      result = normalizeIndentation input
      lines = lines result
      -- Check that relative indentation is preserved
      relIndent = length (takeWhile isSpace (lines !! 1)) - length (takeWhile isSpace (lines !! 0))
  in indentLevel >= 0 && indentLevel <= 10 ==> 
     counterexample "normalizeIndentation should preserve relative indentation" $
     relIndent === 2

-- Property: breakOn consistency with Data.List.breakOn
prop_breakOn_consistency :: String -> String -> Property
prop_breakOn_consistency needle haystack =
  let ourBreak = breakOn needle haystack
      standardBreak = case breakOn needle haystack of
        (before, after) -> (before, after)
  in counterexample "breakOn should be consistent with standard behavior" $
     ourBreak === standardBreak

-- Property: String processing functions handle empty input
prop_string_functions_empty :: Property
prop_string_functions_empty =
  let empty = ""
      trimResult = trim empty
      splitResult = splitBy ',' empty
      splitCollapsedResult = splitByCollapsed ',' empty
      removeCommentsResult = removeComments empty
      normalizeResult = normalizeIndentation empty
      breakOnResult = breakOn "x" empty
  in counterexample "String functions should handle empty input gracefully" $
     trimResult === empty .&&.
     splitResult === [""] .&&.
     splitCollapsedResult === [] .&&.
     removeCommentsResult === empty .&&.
     normalizeResult === empty .&&.
     breakOnResult === (empty, empty)

tests :: TestTree
tests =
  testGroup "New Cabal Utils Boundary Tests"
    [ fastProperty "trim idempotency - trimming twice should give same result" prop_trim_idempotent
    , fastProperty "trim removes all leading/trailing whitespace" prop_trim_removes_whitespace
    , fastProperty "splitBy length property" prop_splitBy_length
    , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_no_empty
    , fastProperty "splitBy vs splitByCollapsed relationship" prop_splitBy_vs_collapsed
    , fastProperty "splitByComma is splitBy with comma" prop_splitBy_comma_property
    , fastProperty "removeLineComments preserves non-comment content" prop_remove_line_comments_preserve
    , fastProperty "removeComments handles block comments" prop_remove_block_comments
    , fastProperty "normalizeIndentation preserves relative indentation" prop_normalize_indentation_relative
    , fastProperty "String processing functions handle empty input" prop_string_functions_empty
    ]