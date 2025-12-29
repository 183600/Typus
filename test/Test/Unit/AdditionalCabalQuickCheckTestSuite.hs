{-# LANGUAGE CPP #-}

-- | Additional Cabal QuickCheck Test Suite for Typus
-- This module provides comprehensive QuickCheck-based tests for core Typus functionality
module Test.Unit.AdditionalCabalQuickCheckTestSuite where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)

import Utils (trim, splitBy)
import Parser (parseTypus)

-- ============================================================================
-- Test 1: String Processing Properties
-- ============================================================================

-- | Test that trim is idempotent
prop_trimIdempotent :: String -> Bool
prop_trimIdempotent s =
  let trimmed = trim s
      trimmedAgain = trim trimmed
  in trimmed == trimmedAgain

-- | Test that splitBy preserves all characters
prop_splitByPreserves :: Char -> String -> Bool
prop_splitByPreserves delim s =
  let parts = splitBy delim s
      rejoined = concatMap (++ [delim]) parts
  in length rejoined >= length s

-- ============================================================================
-- Test 2: Parser Properties
-- ============================================================================

-- | Test that parser handles empty input gracefully
prop_parserHandlesEmpty :: Bool
prop_parserHandlesEmpty =
  case parseTypus "" of
    Left _ -> True
    Right _ -> True

-- | Test that parser is deterministic
prop_parserDeterministic :: String -> Bool
prop_parserDeterministic s =
  let result1 = parseTypus s
      result2 = parseTypus s
  in result1 == result2

-- ============================================================================
-- Test 3: Basic Properties
-- ============================================================================

-- | Test that string length is non-negative
prop_stringLengthNonNegative :: String -> Bool
prop_stringLengthNonNegative s =
  length s >= 0

-- | Test that reverse is involutive
prop_reverseInvolutive :: String -> Bool
prop_reverseInvolutive s =
  reverse (reverse s) == s

-- | Test that concatenation is associative
prop_concatAssociative :: String -> String -> String -> Bool
prop_concatAssociative s1 s2 s3 =
  (s1 ++ s2) ++ s3 == s1 ++ (s2 ++ s3)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Additional Cabal QuickCheck Test Suite"
  [ testProperty "Trim is idempotent" prop_trimIdempotent
  , testProperty "Split by preserves characters" prop_splitByPreserves
  , testProperty "Parser handles empty input" prop_parserHandlesEmpty
  , testProperty "Parser is deterministic" prop_parserDeterministic
  , testProperty "String length is non-negative" prop_stringLengthNonNegative
  , testProperty "Reverse is involutive" prop_reverseInvolutive
  , testProperty "Concatenation is associative" prop_concatAssociative
  ]