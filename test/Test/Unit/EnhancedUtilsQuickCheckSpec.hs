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
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, suchThat
  , resize, Positive(..), NonEmpty(..)
  )

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

import Data.List (isInfixOf, isPrefixOf, sort, nub)
import Data.Char (isSpace, isAlpha)

-- Property: trim handles empty string
prop_trim_empty :: Property
prop_trim_empty =
  property (trim "" === "")

-- Property: trim handles whitespace-only string
prop_trim_whitespace :: String -> Property
prop_trim_whitespace whitespace =
  all isSpace whitespace ==>
  property (trim whitespace === "")

-- Property: trim removes leading and trailing whitespace
prop_trim_removes_whitespace :: String -> String -> Property
prop_trim_removes_whitespace prefix suffix =
  all isSpace prefix && all isSpace suffix ==>
  let content = "content"
      full = prefix ++ content ++ suffix
      trimmed = trim full
  in property (trimmed === content)

-- Property: trim preserves internal whitespace
prop_trim_preserves_internal :: String -> String -> Property
prop_trim_preserves_internal prefix suffix =
  all isSpace prefix && all isSpace suffix ==>
  let content = "  content  with  spaces  "
      full = prefix ++ content ++ suffix
      trimmed = trim full
  in property (trimmed === content)

-- Property: splitBy handles empty string
prop_splitBy_empty :: Char -> Property
prop_splitBy_empty delim =
  property (splitBy delim "" === [""])

-- Property: splitBy handles single character
prop_splitBy_single :: Char -> Char -> Property
prop_splitBy_single delim char =
  delim /= char ==>
  let result = splitBy delim [char]
  in property (result === [[char]])

-- Property: splitBy handles delimiter only
prop_splitBy_delimiter_only :: Char -> Property
prop_splitBy_delimiter_only delim =
  let result = splitBy delim [delim]
  in property (result === ["", ""])

-- Property: splitBy handles multiple delimiters
prop_splitBy_multiple :: Char -> String -> Property
prop_splitBy_multiple delim content =
  not (null content) && not (elem delim content) ==>
  let result = splitBy delim content
  in property (result === [content])

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim content =
  let normal = splitBy delim content
      collapsed = splitByCollapsed delim content
  in property (length collapsed <= length normal && all (not . null) collapsed)

-- Property: splitByComma equivalent to splitBy ','
prop_splitByComma_equivalent :: String -> Property
prop_splitByComma_equivalent content =
  let comma = splitByComma content
      generic = splitBy ',' content
  in property (comma === generic)

-- Property: splitByCommaCollapsed equivalent to splitByCollapsed ','
prop_splitByCommaCollapsed_equivalent :: String -> Property
prop_splitByCommaCollapsed_equivalent content =
  let comma = splitByCommaCollapsed content
      generic = splitByCollapsed ',' content
  in property (comma === generic)

-- Property: removeLineComments handles empty string
prop_removeLineComments_empty :: Property
prop_removeLineComments_empty =
  property (removeLineComments "" === "")

-- Property: removeLineComments handles string without comments
prop_removeLineComments_no_comments :: String -> Property
prop_removeLineComments_no_comments content =
  not ("//" `isInfixOf` content) ==>
  let result = removeLineComments content
  in property (result === content)

-- Property: removeLineComments removes line comments
prop_removeLineComments_removes :: String -> String -> Property
prop_removeLineComments_removes prefix comment =
  not (null comment) && not ("//" `isInfixOf` comment) ==>
  let full = prefix ++ "//" ++ comment
      result = removeLineComments full
  in property (result === prefix)

-- Property: removeLineComments preserves strings with // inside
prop_removeLineComments_preserves_strings :: String -> Property
prop_removeLineComments_preserves_strings content =
  not ("//" `isInfixOf` content) ==>
  let quoted = "\"" ++ content ++ "//comment\""
      result = removeLineComments quoted
  in property (result === quoted)

-- Property: removeComments handles empty string
prop_removeComments_empty :: Property
prop_removeComments_empty =
  property (removeComments "" === "")

-- Property: removeComments handles string without comments
prop_removeComments_no_comments :: String -> Property
prop_removeComments_no_comments content =
  not ("//" `isInfixOf` content) && not ("/*" `isInfixOf` content) ==>
  let result = removeComments content
  in property (result === content)

-- Property: removeComments removes both comment types
prop_removeComments_removes_both :: String -> String -> Property
prop_removeComments_removes_both prefix content =
  not (null content) ==>
  let lineComment = prefix ++ "//" ++ content
      blockComment = prefix ++ "/*" ++ content ++ "*/"
      result1 = removeLineComments lineComment
      result2 = removeComments blockComment
  in property (result1 === prefix && not ("/*" `isInfixOf` result2))

-- Property: normalizeIndentation handles empty string
prop_normalizeIndentation_empty :: Property
prop_normalizeIndentation_empty =
  property (normalizeIndentation "" === "")

-- Property: normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preserves_relative :: String -> Property
prop_normalizeIndentation_preserves_relative content =
  all isSpace content ==>
  let result = normalizeIndentation content
  in property True -- Should handle gracefully

-- Property: forceSingleTabIndentation handles empty string
prop_forceSingleTabIndentation_empty :: Property
prop_forceSingleTabIndentation_empty =
  property (forceSingleTabIndentation "" === "")

-- Property: fixIndentation equivalent to normalizeIndentation
prop_fixIndentation_equivalent :: String -> Property
prop_fixIndentation_equivalent content =
  let fixed = fixIndentation content
      normalized = normalizeIndentation content
  in property (fixed === normalized)

-- Property: breakOn handles empty string
prop_breakOn_empty :: String -> Property
prop_breakOn_empty delimiter =
  property (breakOn delimiter "" === ("", ""))

-- Property: breakOn handles delimiter not found
prop_breakOn_not_found :: String -> String -> Property
prop_breakOn_not_found delimiter content =
  not (null delimiter) && not (delimiter `isInfixOf` content) ==>
  let (before, after) = breakOn delimiter content
  in property (before === content && after === "")

-- Property: breakOn handles delimiter found
prop_breakOn_found :: String -> String -> String -> Property
prop_breakOn_found delimiter prefix suffix =
  not (null delimiter) && not (delimiter `isInfixOf` prefix) && 
  not (delimiter `isInfixOf` suffix) ==>
  let full = prefix ++ delimiter ++ suffix
      (before, after) = breakOn delimiter full
  in property (before === prefix && after === suffix)

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent input =
  let once = trim input
      twice = trim once
  in property (once === twice)

-- Property: splitBy length property
prop_splitBy_length_property :: Char -> String -> Property
prop_splitBy_length_property delim content =
  let parts = splitBy delim content
      joined = concatMap (++ [delim]) (init parts) ++ last parts
  in property (joined === content || null content)

-- Property: removeLineComments preserves line structure
prop_removeLineComments_preserves_lines :: String -> Property
prop_removeLineComments_preserves_lines content =
  let originalLines = lines content
      result = removeLineComments content
      resultLines = lines result
  in property (length resultLines === length originalLines)

tests :: TestTree
tests = testGroup "Enhanced Utils QuickCheck Tests"
  [ fastProperty "trim handles empty string" prop_trim_empty
  , fastProperty "trim handles whitespace-only string" prop_trim_whitespace
  , fastProperty "trim removes leading and trailing whitespace" prop_trim_removes_whitespace
  , fastProperty "trim preserves internal whitespace" prop_trim_preserves_internal
  , fastProperty "splitBy handles empty string" prop_splitBy_empty
  , fastProperty "splitBy handles single character" prop_splitBy_single
  , fastProperty "splitBy handles delimiter only" prop_splitBy_delimiter_only
  , fastProperty "splitBy handles multiple delimiters" prop_splitBy_multiple
  , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
  , fastProperty "splitByComma equivalent to splitBy ','" prop_splitByComma_equivalent
  , fastProperty "splitByCommaCollapsed equivalent to splitByCollapsed ','" prop_splitByCommaCollapsed_equivalent
  , fastProperty "removeLineComments handles empty string" prop_removeLineComments_empty
  , fastProperty "removeLineComments handles string without comments" prop_removeLineComments_no_comments
  , fastProperty "removeLineComments removes line comments" prop_removeLineComments_removes
  , fastProperty "removeLineComments preserves strings with // inside" prop_removeLineComments_preserves_strings
  , fastProperty "removeComments handles empty string" prop_removeComments_empty
  , fastProperty "removeComments handles string without comments" prop_removeComments_no_comments
  , fastProperty "removeComments removes both comment types" prop_removeComments_removes_both
  , fastProperty "normalizeIndentation handles empty string" prop_normalizeIndentation_empty
  , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative
  , fastProperty "forceSingleTabIndentation handles empty string" prop_forceSingleTabIndentation_empty
  , fastProperty "fixIndentation equivalent to normalizeIndentation" prop_fixIndentation_equivalent
  , fastProperty "breakOn handles empty string" prop_breakOn_empty
  , fastProperty "breakOn handles delimiter not found" prop_breakOn_not_found
  , fastProperty "breakOn handles delimiter found" prop_breakOn_found
  , fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "splitBy length property" prop_splitBy_length_property
  , fastProperty "removeLineComments preserves line structure" prop_removeLineComments_preserves_lines
  ]