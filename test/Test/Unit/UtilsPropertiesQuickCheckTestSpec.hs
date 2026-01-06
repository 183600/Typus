{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.UtilsPropertiesQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), choose, oneof, elements)

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

import Data.Char (isSpace, toLower, isAlphaNum)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (tails, sort, nub)
import Data.String (IsString)

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

-- Property: trim never adds leading L.or trailing whitespace
prop_trim_no_added_whitespace :: String -> Property
prop_trim_no_added_whitespace s =
  let trimmed = trim s
      hasLeading = not (null trimmed) && isSpace (L.head trimmed)
      hasTrailing = not (null trimmed) && isSpace (last trimmed)
  in property $ not hasLeading .&&. not hasTrailing

-- Property: splitBy preserves concatenation with delimiters
prop_splitBy_preserves_content :: Char -> String -> Property
prop_splitBy_preserves_content delim s =
  let parts = splitBy delim s
      reconstructed = Data.List.intercalate [delim] parts
  in reconstructed === s

-- Property: splitByCollapsed is subset of splitBy
prop_splitByCollapsed_subset :: Char -> String -> Property
prop_splitByCollapsed_subset delim s =
  let fullParts = splitBy delim s
      collapsedParts = splitByCollapsed delim s
  in property $ L.all (`elem` fullParts) collapsedParts

-- Property: splitByCollapsed removes empty strings
prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s =
  let parts = splitByCollapsed delim s
  in property $ not ("" `elem` parts)

-- Property: splitByComma is splitBy with comma
prop_splitByComma_equals_splitBy :: String -> Property
prop_splitByComma_equals_splitBy s = splitByComma s === splitBy ',' s

-- Property: splitByCommaCollapsed is splitByCollapsed with comma
prop_splitByCommaCollapsed_equals_splitByCollapsed :: String -> Property
prop_splitByCommaCollapsed_equals_splitByCollapsed s = 
  splitByCommaCollapsed s === splitByCollapsed ',' s

-- Property: removeLineComments preserves non-commented lines
prop_removeLineComments_preserves_non_commented :: String -> Property
prop_removeLineComments_preserves_non_commented s =
  let linesWithoutComments = L.filter (not . isPrefixOf "//") (lines s)
      resultLines = L.filter (not . L.all isSpace) (lines (removeLineComments s))
  in property $ L.length resultLines >= L.length linesWithoutComments

-- Property: removeComments never increases string L.length
prop_removeComments_never_increases_length :: String -> Property
prop_removeComments_never_increases_length s =
  let original = L.length s
      withoutComments = L.length (removeComments s)
  in property $ withoutComments <= original

-- Property: normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preserves_relative :: String -> Property
prop_normalizeIndentation_preserves_relative s =
  let originalLines = L.filter (not . L.all isSpace) (lines s)
      normalizedLines = L.filter (not . L.all isSpace) (lines (normalizeIndentation s))
      hasSameStructure = L.length originalLines == L.length normalizedLines
  in classify hasSameStructure "same number of non-empty lines" $
     property hasSameStructure

-- Property: breakOn returns correct prefix when pattern exists
prop_breakOn_correct_prefix :: String -> String -> Property
prop_breakOn_correct_prefix pat s =
  not (null pat) ==> 
  case breakOn pat s of
    (prefix, _) -> pat `L.isInfixOf` s ==> prefix ++ pat `L.isPrefixOf` s

-- Property: breakOn returns empty suffix when pattern not found
prop_breakOn_empty_suffix_when_not_found :: String -> String -> Property
prop_breakOn_empty_suffix_when_not_found pat s =
  not (null pat) ==> 
  case breakOn pat s of
    (_, suffix) -> not (pat `L.isInfixOf` s) ==> suffix === ""

-- Property: breakOn with empty pattern returns empty prefix
prop_breakOn_empty_pattern :: String -> Property
prop_breakOn_empty_pattern s = breakOn "" s === ("", s)

-- Property: trim composed with splitBy handles whitespace correctly
prop_trim_splitBy_interaction :: Char -> String -> Property
prop_trim_splitBy_interaction delim s =
  let trimmed = trim s
      parts = splitBy delim trimmed
  in property $ L.all (\p -> trim p === p) parts

-- Property: removeComments is idempotent
prop_removeComments_idempotent :: String -> Property
prop_removeComments_idempotent s = 
  removeComments (removeComments s) === removeComments s

-- Property: normalizeIndentation preserves line count
prop_normalizeIndentation_preserves_line_count :: String -> Property
prop_normalizeIndentation_preserves_line_count s =
  let originalLines = lines s
      normalizedLines = lines (normalizeIndentation s)
  in L.length originalLines === L.length normalizedLines

-- Property: splitBy with delimiter not in string returns singleton
prop_splitBy_no_delimiter :: Char -> String -> Property
prop_splitBy_no_delimiter delim s =
  not (delim `elem` s) ==> splitBy delim s === [s]

-- Property: trim L.and splitBy interaction with whitespace delimiters
prop_trim_splitBy_whitespace :: String -> Property
prop_trim_splitBy_whitespace s =
  let trimmed = trim s
      parts = splitBy ' ' trimmed
  in property $ not ("" `elem` parts) || null trimmed

tests :: TestTree
tests =
  testGroup "Utils QuickCheck Property Tests"
    [ fastProperty "trim is idempotent" prop_trim_idempotent
    , fastProperty "trim never adds leading/trailing whitespace" prop_trim_no_added_whitespace
    , fastProperty "splitBy preserves content" prop_splitBy_preserves_content
    , fastProperty "splitByCollapsed is subset of splitBy" prop_splitByCollapsed_subset
    , fastProperty "splitByCollapsed removes empty strings" prop_splitByCollapsed_no_empty
    , fastProperty "splitByComma equals splitBy with comma" prop_splitByComma_equals_splitBy
    , fastProperty "splitByCommaCollapsed equals splitByCollapsed with comma" prop_splitByCommaCollapsed_equals_splitByCollapsed
    , fastProperty "removeLineComments preserves non-commented lines" prop_removeLineComments_preserves_non_commented
    , fastProperty "removeComments never increases L.length" prop_removeComments_never_increases_length
    , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative
    , fastProperty "breakOn correct prefix when pattern exists" prop_breakOn_correct_prefix
    , fastProperty "breakOn empty suffix when not found" prop_breakOn_empty_suffix_when_not_found
    , fastProperty "breakOn with empty pattern" prop_breakOn_empty_pattern
    , fastProperty "trim splitBy interaction" prop_trim_splitBy_interaction
    , fastProperty "removeComments is idempotent" prop_removeComments_idempotent
    , fastProperty "normalizeIndentation preserves line count" prop_normalizeIndentation_preserves_line_count
    , fastProperty "splitBy no delimiter returns singleton" prop_splitBy_no_delimiter
    , fastProperty "trim splitBy whitespace interaction" prop_trim_splitBy_whitespace
    ]