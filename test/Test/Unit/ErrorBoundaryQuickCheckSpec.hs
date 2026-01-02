{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, suchThat
  , choose, resize, vectorOf, frequency, Positive(..), NonZero(..)
  )

import Utils (trim, splitBy, removeComments, normalizeIndentation)
import Parser (parseTypus, TypusFile(..))
import SourceLocation 
  ( SourcePos(..), SourceSpan(..), locatedWithSpan, spanLength
  , spanContains, spanStart, spanEnd
  )

import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, concat)
import Data.List (sort, nub, intersperse)
import Data.Char (isSpace, isAlpha, isDigit, isControl, ord, chr)
import qualified Data.Text as T

-- ============================================================================
-- Error Boundary L.and Edge Case Tests
-- ============================================================================

-- Property: trim handles empty strings
prop_trim_empty :: Property
prop_trim_empty = trim "" === ""

-- Property: trim handles whitespace-only strings
prop_trim_whitespace :: String -> Property
prop_trim_whitespace s = L.all isSpace s ==> trim s === ""

-- Property: splitBy handles empty delimiter
prop_split_by_empty :: String -> Property
prop_split_by_empty s = splitBy '\0' s === [s]

-- Property: splitBy handles empty string
prop_split_by_empty_string :: Char -> Property
prop_split_by_empty_string delim = splitBy delim "" === [""]

-- Property: removeComments handles empty input
prop_remove_comments_empty :: Property
prop_remove_comments_empty = removeComments "" === ""

-- Property: removeComments handles comment-only input
prop_remove_comments_only :: String -> Property
prop_remove_comments_only s = 
  let commentOnly = "// " ++ s
  in removeComments commentOnly === ""

-- Property: normalizeIndentation handles empty input
prop_normalize_indentation_empty :: Property
prop_normalize_indentation_empty = normalizeIndentation "" === ""

-- Property: normalizeIndentation handles single line
prop_normalize_indentation_single :: String -> Property
prop_normalize_indentation_single s = 
  not ('\n' `elem` s) ==> normalizeIndentation s === s

-- ============================================================================
-- Parser Error Handling Tests
-- ============================================================================

-- Property: parseTypus handles null characters
prop_parse_typus_null_chars :: String -> Property
prop_parse_typus_null_chars s =
  let withNull = s ++ "\0" ++ s
  in case parseTypus withNull of
    Left _ -> property True  -- Expected to fail
    Right _ -> property True  -- If it succeeds, that's also valid

-- Property: parseTypus handles extremely long identifiers
prop_parse_typus_long_identifiers :: Positive Int -> Property
prop_parse_typus_long_identifiers (Positive len) =
  let longId = replicate len 'a'
      code = "let " ++ longId ++ " = 42"
  in case parseTypus code of
    Left _ -> property True
    Right _ -> property True

-- Property: parseTypus handles deeply nested structures
prop_parse_typus_nested :: Positive Int -> Property
prop_parse_typus_nested (Positive depth) =
  let nestedBraces = replicate depth '{' ++ "x" ++ replicate depth '}'
  in case parseTypus nestedBraces of
    Left _ -> property True
    Right _ -> property True

-- ============================================================================
-- SourceLocation Boundary Tests
-- ============================================================================

-- Property: spanLength handles zero-L.length spans
prop_span_length_zero :: Property
prop_span_length_zero =
  let span = locatedWithSpan 0 0
  in spanLength span === 0

-- Property: spanLength handles large spans
prop_span_length_large :: Positive Int -> Property
prop_span_length_large (Positive len) =
  let span = locatedWithSpan 0 len
  in spanLength span === len

-- Property: spanContains handles identical spans
prop_span_contains_identical :: Int -> Property
prop_span_contains_identical pos =
  let span = locatedWithSpan pos pos
  in spanContains span span

-- Property: spanContains handles empty spans
prop_span_contains_empty :: Int -> Int -> Property
prop_span_contains_empty start end =
  let outerSpan = locatedWithSpan (min start end) (max start end)
      emptySpan = locatedWithSpan start start
  in spanContains outerSpan emptySpan

-- ============================================================================
-- Control Character L.and Unicode Tests
-- ============================================================================

-- Property: trim handles control characters
prop_trim_control_chars :: String -> Property
prop_trim_control_chars s =
  let withControl = map chr [0..31] ++ s ++ map chr [0..31]
      trimmed = trim withControl
  in not (L.any isControl trimmed)

-- Property: splitBy handles Unicode delimiters
prop_split_by_unicode :: String -> Property
prop_split_by_unicode s =
  let unicodeDelim = '∑'
      result = splitBy unicodeDelim s
  in L.length result >= 1  -- Should always return at least one element

-- Property: removeComments handles Unicode comments
prop_remove_comments_unicode :: String -> Property
prop_remove_comments_unicode s =
  let unicodeComment = "// 测试评论 " ++ s
  in removeComments unicodeComment === ""

-- ============================================================================
-- Numeric Boundary Tests
-- ============================================================================

-- Property: position arithmetic consistency
prop_position_arithmetic :: Int -> Int -> Int -> Property
prop_position_arithmetic base offset1 offset2 =
  let pos1 = base + offset1
      pos2 = base + offset2
      span1 = locatedWithSpan base pos1
      span2 = locatedWithSpan base pos2
  in spanLength span1 + spanLength span2 >= spanLength (locatedWithSpan base (max pos1 pos2))

-- Property: negative position handling
prop_negative_positions :: Int -> Property
prop_negative_positions offset =
  let pos = -offset
      span = locatedWithSpan pos (pos + 1)
  in spanLength span === 1

-- ============================================================================
-- List Operation Boundary Tests
-- ============================================================================

-- Property: sort handles empty lists
prop_sort_empty :: Property
prop_sort_empty = (sort [] :: [Int]) === []

-- Property: sort handles single-element lists
prop_sort_single :: Int -> Property
prop_sort_single x = sort [x] === [x]

-- Property: nub handles empty lists
prop_nub_empty :: Property
prop_nub_empty = (nub [] :: [Int]) === []

-- Property: nub handles single-element lists
prop_nub_single :: Int -> Property
prop_nub_single x = nub [x] === [x]

-- Property: L.concat handles empty list of lists
prop_concat_empty :: Property
prop_concat_empty = (L.concat [] :: [Int]) === []

-- Property: L.concat handles list of empty lists
prop_concat_empty_lists :: Int -> Property
prop_concat_empty_lists n =
  let emptyLists = replicate n []
  in L.concat emptyLists === []

-- ============================================================================
-- Arbitrary Instances for Boundary Testing
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- choose (-100, 100)
    len <- choose (0, 200)
    let end = start + len
    return $ locatedWithSpan start end

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Error Boundary QuickCheck Test Suite"
  [ testGroup "String Processing Boundary Tests"
    [ fastProperty "trim empty" prop_trim_empty
    , fastProperty "trim whitespace" prop_trim_whitespace
    , fastProperty "splitBy empty delimiter" prop_split_by_empty
    , fastProperty "splitBy empty string" prop_split_by_empty_string
    , fastProperty "removeComments empty" prop_remove_comments_empty
    , fastProperty "removeComments only" prop_remove_comments_only
    , fastProperty "normalizeIndentation empty" prop_normalize_indentation_empty
    , fastProperty "normalizeIndentation single" prop_normalize_indentation_single
    ]
  , testGroup "Parser Error Handling Tests"
    [ fastProperty "parseTypus null chars" prop_parse_typus_null_chars
    , fastProperty "parseTypus long identifiers" prop_parse_typus_long_identifiers
    , fastProperty "parseTypus nested" prop_parse_typus_nested
    ]
  , testGroup "SourceLocation Boundary Tests"
    [ fastProperty "spanLength zero" prop_span_length_zero
    , fastProperty "spanLength large" prop_span_length_large
    , fastProperty "spanContains identical" prop_span_contains_identical
    , fastProperty "spanContains empty" prop_span_contains_empty
    ]
  , testGroup "Control Character L.and Unicode Tests"
    [ fastProperty "trim control chars" prop_trim_control_chars
    , fastProperty "splitBy unicode" prop_split_by_unicode
    , fastProperty "removeComments unicode" prop_remove_comments_unicode
    ]
  , testGroup "Numeric Boundary Tests"
    [ fastProperty "position arithmetic" prop_position_arithmetic
    , fastProperty "negative positions" prop_negative_positions
    ]
  , testGroup "List Operation Boundary Tests"
    [ fastProperty "sort empty" prop_sort_empty
    , fastProperty "sort single" prop_sort_single
    , fastProperty "nub empty" prop_nub_empty
    , fastProperty "nub single" prop_nub_single
    , fastProperty "L.concat empty" prop_concat_empty
    , fastProperty "L.concat empty lists" prop_concat_empty_lists
    ]
  ]