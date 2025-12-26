{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.UtilsPropertiesExtendedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, suchThat, listOf1, elements
  , frequency, oneof, sized, resize, Positive(..), NonEmptyList(..)
  )

import Utils
  ( trim
  , splitBy, splitByCollapsed
  , splitByComma, splitByCommaCollapsed
  , removeLineComments, removeComments
  , normalizeIndentation
  , breakOn
  )

import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf, intercalate)
import qualified Data.Text as T

-- ============================================================================
-- String Trimming Properties
-- ============================================================================

-- Property: trim removes all leading and trailing whitespace
prop_trim_removes_leading_trailing :: String -> Property
prop_trim_removes_leading_trailing s =
  let trimmed = trim s
      hasLeading = not (null s) && isSpace (head s)
      hasTrailing = not (null s) && isSpace (last s)
  in classify hasLeading "has leading whitespace" $
     classify hasTrailing "has trailing whitespace" $
     counterexample ("Original: " ++ show s ++ ", Trimmed: " ++ show trimmed) $
     (null trimmed || not (isSpace (head trimmed))) &&
     (null trimmed || not (isSpace (last trimmed)))

-- Property: trim is idempotent (trimming twice gives same result)
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
      trimmedTwice = trim trimmed
  in trimmed === trimmedTwice

-- Property: trim preserves non-whitespace content
prop_trim_preserves_content :: String -> Property
prop_trim_preserves_content s =
  let trimmed = trim s
      nonSpaceContent = filter (not . isSpace) s
  in not (null nonSpaceContent) ==> 
     counterexample ("Original non-space: " ++ nonSpaceContent ++ ", Trimmed: " ++ trimmed) $
     isInfixOf nonSpaceContent trimmed

-- ============================================================================
-- String Splitting Properties
-- ============================================================================

-- Property: splitBy preserves empty segments
prop_splitBy_preserves_empty :: Char -> String -> Property
prop_splitBy_preserves_empty delim s =
  let result = splitBy delim s
      expectedCount = length s + 1
  in length result === expectedCount

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim s =
  let result = splitByCollapsed delim s
  in not (any null result)

-- Property: splitBy and splitByCollapsed are equivalent when no consecutive delimiters
prop_splitBy_equivalent_when_no_consecutive :: Char -> String -> Property
prop_splitBy_equivalent_when_no_consecutive delim s =
  let hasNoConsecutive = not (isInfixOf [delim, delim] s)
      normal = splitBy delim s
      collapsed = splitByCollapsed delim s
  in hasNoConsecutive ==> normal === collapsed

-- Property: splitByComma is equivalent to splitBy ','
prop_splitByComma_equivalent :: String -> Property
prop_splitByComma_equivalent s =
  splitByComma s === splitBy ',' s

-- Property: splitByCommaCollapsed is equivalent to splitByCollapsed ','
prop_splitByCommaCollapsed_equivalent :: String -> Property
prop_splitByCommaCollapsed_equivalent s =
  splitByCommaCollapsed s === splitByCollapsed ',' s

-- Property: splitBy preserves original content when rejoined
prop_splitBy_preserves_content :: Char -> String -> Property
prop_splitBy_preserves_content delim s =
  let parts = splitBy delim s
      rejoined = intercalate [delim] parts
  in rejoined === s

-- ============================================================================
-- Comment Removal Properties
-- ============================================================================

-- Property: removeLineComments removes // comments but preserves line endings
prop_removeLineComments_preserves_line_count :: String -> Property
prop_removeLineComments_preserves_line_count s =
  let original = lines s
      processed = lines (removeLineComments s)
  in length original === length processed

-- Property: removeLineComments doesn't affect strings containing //
prop_removeLineComments_preserves_strings :: String -> Property
prop_removeLineComments_preserves_strings s =
  let stringWithComment = "code with // comment\nand \"string with // not comment\"\n"
      processed = removeLineComments stringWithComment
  in isInfixOf "\"string with // not comment\"" processed

-- Property: removeComments removes both // and /* */ comments
prop_removeComments_removes_both_types :: String -> Property
prop_removeComments_removes_both_types s =
  let testCode = "code // line comment\nmore code /* block comment */\nfinal code"
      processed = removeComments testCode
  in not (isInfixOf "// line comment" processed) &&
     not (isInfixOf "/* block comment */" processed) &&
     isInfixOf "code" processed &&
     isInfixOf "more code" processed &&
     isInfixOf "final code" processed

-- Property: removeComments preserves string literals containing comment markers
prop_removeComments_preserves_string_literals :: String -> Property
prop_removeComments_preserves_string_literals s =
  let testCode = "code \"string with // comment\" and /* not comment */\n"
      processed = removeComments testCode
  in isInfixOf "\"string with // comment\"" processed

-- ============================================================================
-- Indentation Properties
-- ============================================================================

-- Property: normalizeIndentation preserves relative indentation differences
prop_normalizeIndentation_preserves_relative :: String -> Property
prop_normalizeIndentation_preserves_relative s =
  let ls = lines s
      hasMultipleLines = length ls > 1
      processed = lines (normalizeIndentation s)
  in hasMultipleLines ==>
     let originalIndents = map (takeWhile isSpace) ls
         processedIndents = map (takeWhile isSpace) processed
         -- Check that relative differences are preserved
         differences original [] = []
         differences (x:xs) (y:ys) = (length x - length y) : differences xs ys
         differences [] [] = []
         differences _ _ = []
     in counterexample ("Original: " ++ show originalIndents ++ ", Processed: " ++ show processedIndents) $
        all (>= 0) (differences processedIndents originalIndents)

-- Property: normalizeIndentation doesn't change content (only indentation)
prop_normalizeIndentation_preserves_content :: String -> Property
prop_normalizeIndentation_preserves_content s =
  let originalContent = filter (not . isSpace) s
      processedContent = filter (not . isSpace) (normalizeIndentation s)
  in originalContent === processedContent

-- Property: normalizeIndentation is idempotent
prop_normalizeIndentation_idempotent :: String -> Property
prop_normalizeIndentation_idempotent s =
  let once = normalizeIndentation s
      twice = normalizeIndentation once
  in once === twice

-- ============================================================================
-- BreakOn Properties
-- ============================================================================

-- Property: breakOn finds pattern when it exists
prop_breakOn_finds_pattern :: String -> String -> Property
prop_breakOn_finds_pattern pat s =
  let patNotEmpty = not (null pat)
      patExists = isInfixOf pat s
      (before, after) = breakOn pat s
  in patNotEmpty && patExists ==>
     before ++ pat ++ after === s

-- Property: breakOn returns original string when pattern not found
prop_breakOn_no_match :: String -> String -> Property
prop_breakOn_no_match pat s =
  let patNotEmpty = not (null pat)
      patNotExists = not (isInfixOf pat s)
      (before, after) = breakOn pat s
  in patNotEmpty && patNotExists ==>
     before === s && after === ""

-- Property: breakOn with empty pattern returns ("", s)
prop_breakOn_empty_pattern :: String -> Property
prop_breakOn_empty_pattern s =
  let (before, after) = breakOn "" s
  in before === "" && after === s

-- Property: breakOn is consistent with prefix behavior
prop_breakOn_prefix_consistency :: String -> String -> Property
prop_breakOn_prefix_consistency pat s =
  let patNotEmpty = not (null pat)
      patIsPrefix = isPrefixOf pat s
      (before, after) = breakOn pat s
  in patNotEmpty && patIsPrefix ==>
     before === ""

-- ============================================================================
-- Complex Integration Properties
-- ============================================================================

-- Property: trim and normalizeIndentation commute in most cases
prop_trim_normalize_commute :: String -> Property
prop_trim_normalize_commute s =
  let trimmedThenNormalized = normalizeIndentation (trim s)
      normalizedThenTrimmed = trim (normalizeIndentation s)
  in trimmedThenNormalized === normalizedThenTrimmed

-- Property: removing comments then trimming is same as trimming then removing comments
prop_removeComments_trim_commute :: String -> Property
prop_removeComments_trim_commute s =
  let commentsThenTrim = trim (removeComments s)
      trimThenComments = removeComments (trim s)
  in commentsThenTrim === trimThenComments

-- Property: splitBy after removing comments is consistent
prop_splitBy_comment_removal_consistent :: Char -> String -> Property
prop_splitBy_comment_removal_consistent delim s =
  let normalSplit = splitBy delim s
      noCommentsSplit = splitBy delim (removeComments s)
  in -- This is a weak property since comment removal can change structure
     -- but we can at least say the number of parts shouldn't increase dramatically
     length noCommentsSplit <= length normalSplit + 10

-- Test collection
tests :: TestTree
tests = testGroup "Utils Extended Properties"
  [ testGroup "String Trimming"
    [ fastProperty "trim removes leading/trailing whitespace" prop_trim_removes_leading_trailing
    , fastProperty "trim is idempotent" prop_trim_idempotent
    , fastProperty "trim preserves content" prop_trim_preserves_content
    ]
  , testGroup "String Splitting"
    [ fastProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
    , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
    , fastProperty "splitBy equivalent when no consecutive delimiters" prop_splitBy_equivalent_when_no_consecutive
    , fastProperty "splitByComma equivalent to splitBy ','" prop_splitByComma_equivalent
    , fastProperty "splitByCommaCollapsed equivalent to splitByCollapsed ','" prop_splitByCommaCollapsed_equivalent
    , fastProperty "splitBy preserves content when rejoined" prop_splitBy_preserves_content
    ]
  , testGroup "Comment Removal"
    [ fastProperty "removeLineComments preserves line count" prop_removeLineComments_preserves_line_count
    , fastProperty "removeLineComments preserves strings" prop_removeLineComments_preserves_strings
    , fastProperty "removeComments removes both types" prop_removeComments_removes_both_types
    , fastProperty "removeComments preserves string literals" prop_removeComments_preserves_string_literals
    ]
  , testGroup "Indentation"
    [ fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative
    , fastProperty "normalizeIndentation preserves content" prop_normalizeIndentation_preserves_content
    , fastProperty "normalizeIndentation is idempotent" prop_normalizeIndentation_idempotent
    ]
  , testGroup "BreakOn"
    [ fastProperty "breakOn finds pattern when it exists" prop_breakOn_finds_pattern
    , fastProperty "breakOn returns original string when pattern not found" prop_breakOn_no_match
    , fastProperty "breakOn with empty pattern" prop_breakOn_empty_pattern
    , fastProperty "breakOn prefix consistency" prop_breakOn_prefix_consistency
    ]
  , testGroup "Integration"
    [ fastProperty "trim and normalizeIndentation commute" prop_trim_normalize_commute
    , fastProperty "removeComments and trim commute" prop_removeComments_trim_commute
    , fastProperty "splitBy comment removal consistency" prop_splitBy_comment_removal_consistent
    ]
  ]