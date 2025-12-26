{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.SimpleCabalTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===))
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import Utils (trim, splitBy)

-- | Simple test suite for basic cabal tests
tests :: TestTree
tests = testGroup "Simple Cabal Tests"
  [ testProperty "trim is idempotent" propTrimIdempotent
  , testProperty "splitBy preserves segments" propSplitByPreservesSegments
  , testCase "trim basic functionality" testTrimBasic
  , testCase "splitBy basic functionality" testSplitByBasic
  ]

-- | Property: trim is idempotent
propTrimIdempotent :: String -> Property
propTrimIdempotent s = trim (trim s) === trim s

-- | Property: splitBy preserves segments
propSplitByPreservesSegments :: Char -> String -> Property
propSplitByPreservesSegments delim s = 
  let segments = splitBy delim s
      rejoined = L.intercalate [delim] segments
  in property $ length rejoined >= length s

-- | Unit test for trim basic functionality
testTrimBasic :: IO ()
testTrimBasic = do
  assertEqual "trim empty string" "" (trim "")
  assertEqual "trim whitespace" "" (trim "   ")
  assertEqual "trim preserves content" "abc" (trim "  abc  ")

-- | Unit test for splitBy basic functionality
testSplitByBasic :: IO ()
testSplitByBasic = do
  assertEqual "splitBy single char" ["a", "b"] (splitBy ',' "a,b")
  assertEqual "splitBy with empty segments" ["a", "", "b"] (splitBy ',' "a,,b")

-- Helper imports
import qualified Data.List as L

-- Helper function for property testing
property :: Bool -> Property
property = property' where
  property' :: Bool -> Property
  property' = id