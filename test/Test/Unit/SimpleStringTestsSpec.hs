{-# LANGUAGE CPP #-}

module Test.Unit.SimpleStringTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), property)
import Utils (trim, splitBy, splitByCollapsed)
import Data.Char (isSpace)

-- ============================================================================
-- Simple String Tests
-- ============================================================================

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmedOnce = trim str
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce === trimmedTwice

-- Property: splitBy with delimiter not in string
prop_splitBy_no_delimiter :: String -> Property
prop_splitBy_no_delimiter input =
  '\0' `notElem` input ==>
  let result = splitBy '\0' input
      expected = if null input then [""] else [input]
  in property $ result === expected

-- Property: splitByCollapsed empty string is empty
prop_splitByCollapsed_empty :: Char -> Property
prop_splitByCollapsed_empty delim =
  property $ splitByCollapsed delim === []

-- Property: trim of empty string is empty
prop_trim_empty :: Property
prop_trim_empty =
  property $ trim "" === ""

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Simple String Tests"
  [ testGroup "Unit Tests"
    [ testCase "trim removes leading and trailing whitespace" $ do
        trim "\t  hello  world \n" @?= "hello  world"

    , testCase "splitBy preserves empty segments" $ do
        splitBy ':' "a::b:" @?= ["a", "", "b", ""]

    , testCase "splitByCollapsed removes empty segments" $ do
        splitByCollapsed ':' "::alpha::beta::" @?= ["alpha", "beta"]

    , testCase "trim of empty string is empty" $ do
        trim "" @?= ""
    ]
  , testGroup "Property Tests"
    [ fastProperty "trim is idempotent" prop_trim_idempotent
    , fastProperty "splitBy with delimiter not in string" prop_splitBy_no_delimiter
    , fastProperty "splitByCollapsed empty string is empty" prop_splitByCollapsed_empty
    , fastProperty "trim of empty string is empty" prop_trim_empty
    ]
  ]