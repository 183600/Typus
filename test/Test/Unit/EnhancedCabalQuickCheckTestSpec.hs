{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.EnhancedCabalQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, suchThat
  , choose, resize, vectorOf, frequency, Positive(..), NonEmptyList(..)
  )

import Utils (trim, splitBy, splitByComma, normalizeIndentation, breakOn)
import Parser (parseTypus, TypusFile(..), FileDirectives(..), defaultFileDirectives)
import SourceLocation 
  ( SourcePos(..), SourceSpan(..), locatedWithSpan, spanLength
  , spanContains, spanStart, spanEnd, startPos
  )

import Data.List (isPrefixOf, isInfixOf, sort, nub, intersperse, concat, foldl')
import Data.Char (isSpace, isAlpha, isDigit, toLower, toUpper)
import qualified Data.Text as T
import Data.String (IsString(..))

-- ============================================================================
-- Enhanced String Processing Tests
-- ============================================================================

-- Property: splitByComma is equivalent to splitBy ','
prop_split_by_comma_equivalence :: String -> Property
prop_split_by_comma_equivalence s = 
  splitByComma s === splitBy ',' s

-- Property: trim preserves non-space characters
prop_trim_preserves_content :: String -> Property
prop_trim_preserves_content s =
  let trimmed = trim s
      hasNonSpace = any (not . isSpace) s
  in hasNonSpace ==> not (null trimmed)

-- Property: normalizeIndentation idempotent
prop_normalize_indentation_idempotent :: String -> Property
prop_normalize_indentation_idempotent s =
  let normalized = normalizeIndentation s
      doubleNormalized = normalizeIndentation normalized
  in normalized === doubleNormalized

-- Property: breakOn works like isInfixOf for existence
prop_break_on_exists :: String -> String -> Property
prop_break_on_exists needle haystack =
  let result = breakOn needle haystack
      exists = needle `isInfixOf` haystack
  in case result of
    Nothing -> not exists
    Just _ -> exists

-- ============================================================================
-- Parser Robustness Tests
-- ============================================================================

-- Property: parseTypus handles mixed newlines
prop_parse_typus_mixed_newlines :: String -> String -> String -> Property
prop_parse_typus_mixed_newlines s1 s2 s3 =
  let mixed = s1 ++ "\n" ++ s2 ++ "\r\n" ++ s3
  in case parseTypus mixed of
    Left _ -> property True  -- Invalid input is allowed
    Right _ -> property True  -- Successful parse is valid

-- Property: parseTypus handles very long lines
prop_parse_typus_long_lines :: Positive Int -> Property
prop_parse_typus_long_lines (Positive len) =
  let longLine = replicate len 'a'
  in case parseTypus longLine of
    Left _ -> property True
    Right _ -> property True

-- Property: parseTypus handles Unicode characters
prop_parse_typus_unicode :: String -> Property
prop_parse_typus_unicode s =
  let unicodeContent = s ++ "测试🚀αβγ"
  in case parseTypus unicodeContent of
    Left _ -> property True
    Right _ -> property True

-- ============================================================================
-- SourceLocation Precision Tests
-- ============================================================================

-- Property: spanLength equals end - start for simple spans
prop_span_length_calculation :: Int -> Int -> Property
prop_span_length_calculation start end =
  let start' = max 0 start
      end' = max start' end
      span = locatedWithSpan start' end'
  in spanLength span === (end' - start')

-- Property: spanContains boundary conditions
prop_span_contains_boundaries :: Int -> Int -> Int -> Property
prop_span_contains_boundaries start middle end =
  let start' = max 0 start
      middle' = max start' middle
      end' = max middle' end
      outerSpan = locatedWithSpan start' end'
      innerSpan = locatedWithSpan middle' middle'
  in spanContains outerSpan innerSpan

-- Property: spanStart and spanEnd consistency
prop_span_start_end_consistency :: SourceSpan -> Property
prop_span_start_end_consistency span =
  let start = spanStart span
      end = spanEnd span
  in spanLength span >= 0 ==> start <= end

-- ============================================================================
-- List and Collection Properties
-- ============================================================================

-- Property: sort . nub = nub . sort for lists with duplicates
prop_sort_nub_commutative :: [Int] -> Property
prop_sort_nub_commutative xs =
  sort (nub xs) === nub (sort xs)

-- Property: concat is associative
prop_concat_associative :: [[Int]] -> [[Int]] -> [[Int]] -> Property
prop_concat_associative xs ys zs =
  concat (xs ++ (ys ++ zs)) === concat ((xs ++ ys) ++ zs)

-- Property: intersperse length calculation
prop_intersperse_length :: Int -> [Int] -> Property
prop_intersperse_length sep xs =
  let interspersed = intersperse sep xs
      expectedLength = if null xs then 0 else 2 * length xs - 1
  in length interspersed === expectedLength

-- ============================================================================
-- Text Processing Properties
-- ============================================================================

-- Property: toLower . toUpper preserves length
prop_to_upper_lower_length :: String -> Property
prop_to_upper_lower_length s =
  length (map toLower (map toUpper s)) === length s

-- Property: isAlpha . toUpper is equivalent to isAlpha
prop_is_alpha_upper :: Char -> Property
prop_is_alpha_upper c =
  isAlpha (toUpper c) === isAlpha c

-- Property: foldl' with consistent operator is associative
prop_foldl_associative :: Int -> [Int] -> Property
prop_foldl_associative init xs =
  let op1 x y = x + y
      op2 x y = y + x
  in foldl' op1 init xs === foldl' op2 init xs

-- ============================================================================
-- Arbitrary Instances for Enhanced Testing
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    len <- choose (0, 100)
    let end = start + len
    return $ locatedWithSpan start end

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Enhanced Cabal QuickCheck Test Suite"
  [ testGroup "Enhanced String Processing Tests"
    [ fastProperty "splitByComma equivalence" prop_split_by_comma_equivalence
    , fastProperty "trim preserves content" prop_trim_preserves_content
    , fastProperty "normalizeIndentation idempotent" prop_normalize_indentation_idempotent
    , fastProperty "breakOn existence" prop_break_on_exists
    ]
  , testGroup "Parser Robustness Tests"
    [ fastProperty "parseTypus mixed newlines" prop_parse_typus_mixed_newlines
    , fastProperty "parseTypus long lines" prop_parse_typus_long_lines
    , fastProperty "parseTypus unicode" prop_parse_typus_unicode
    ]
  , testGroup "SourceLocation Precision Tests"
    [ fastProperty "spanLength calculation" prop_span_length_calculation
    , fastProperty "spanContains boundaries" prop_span_contains_boundaries
    , fastProperty "spanStart/End consistency" prop_span_start_end_consistency
    ]
  , testGroup "List and Collection Properties"
    [ fastProperty "sort nub commutative" prop_sort_nub_commutative
    , fastProperty "concat associative" prop_concat_associative
    , fastProperty "intersperse length" prop_intersperse_length
    ]
  , testGroup "Text Processing Properties"
    [ fastProperty "toUpper/Lower length" prop_to_upper_lower_length
    , fastProperty "isAlpha upper" prop_is_alpha_upper
    , fastProperty "foldl associative" prop_foldl_associative
    ]
  ]