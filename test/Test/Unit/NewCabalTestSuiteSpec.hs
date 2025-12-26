{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalTestSuiteSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===))
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import qualified Data.Text as T
import qualified Data.List as L

import Utils (trim, splitBy, splitByCollapsed, removeLineComments, removeComments, normalizeIndentation, breakOn)

-- | Test suite for new cabal tests focusing on core utility functions
tests :: TestTree
tests = testGroup "New Cabal Test Suite"
  [ testProperty "trim idempotent" propTrimIdempotent
  , testProperty "splitBy consistency" propSplitByConsistency
  , testProperty "splitByCollapsed preserves non-empty segments" propSplitByCollapsedPreservesNonEmpty
  , testProperty "removeLineComments preserves non-comment content" propRemoveLineCommentsPreservesContent
  , testProperty "removeComments handles nested patterns" propRemoveCommentsHandlesNested
  , testProperty "normalizeIndentation preserves relative structure" propNormalizeIndentationPreservesRelative
  , testProperty "breakOn basic correctness" propBreakOnCorrectness
  , testCase "trim edge cases" testTrimEdgeCases
  , testCase "splitBy edge cases" testSplitByEdgeCases
  , testCase "removeComments complex cases" testRemoveCommentsComplex
  ]

-- | Property: trim is idempotent (applying it twice gives same result)
propTrimIdempotent :: String -> Property
propTrimIdempotent s = trim (trim s) === trim s
  where
    (===) :: Eq a => a -> a -> Property
    x === y = property $ x == y

-- | Property: splitBy and splitByCollapsed relationship
propSplitByConsistency :: Char -> String -> Property
propSplitByConsistency delim s = 
  let normal = splitBy delim s
      collapsed = splitByCollapsed delim s
  in property $ all (not . null) collapsed && 
                L.sort collapsed === L.sort (filter (not . null) normal)

-- | Property: splitByCollapsed preserves all non-empty segments
propSplitByCollapsedPreservesNonEmpty :: Char -> String -> Property
propSplitByCollapsedPreservesNonEmpty delim s =
  let segments = splitBy delim s
      nonEmptySegments = filter (not . null) segments
      collapsed = splitByCollapsed delim s
  in property $ L.sort collapsed === L.sort nonEmptySegments

-- | Property: removeLineComments preserves non-comment content
propRemoveLineCommentsPreservesContent :: String -> Property
propRemoveLineCommentsPreservesContent s =
  let withoutComments = removeLineComments s
      linesWithoutComments = lines withoutComments
      linesOriginal = lines s
  in property $ length linesWithoutComments <= length linesOriginal &&
                not (any ("//" `L.isPrefixOf`) linesWithoutComments)

-- | Property: removeComments handles nested patterns correctly
propRemoveCommentsHandlesNested :: String -> Property
propRemoveCommentsHandlesNested s =
  let withoutComments = removeComments s
  in property $ not ("/*" `L.isInfixOf` withoutComments) &&
                not ("*/" `L.isInfixOf` withoutComments)

-- | Property: normalizeIndentation preserves relative structure
propNormalizeIndentationPreservesRelative :: String -> Property
propNormalizeIndentationPreservesRelative s =
  let normalized = normalizeIndentation s
      originalLines = lines s
      normalizedLines = lines normalized
  in property $ length originalLines == length normalizedLines

-- | Property: breakOn basic correctness
propBreakOnCorrectness :: String -> String -> Property
propBreakOnCorrectness pat s =
  let (before, after) = breakOn pat s
  in if null pat
     then property $ before == "" && after == s
     else if pat `L.isInfixOf` s
          then property $ before ++ pat ++ after == s
          else property $ before == s && after == ""

-- | Unit tests for trim edge cases
testTrimEdgeCases :: IO ()
testTrimEdgeCases = do
  assertEqual "trim empty string" "" (trim "")
  assertEqual "trim only whitespace" "" (trim "   \t\n  ")
  assertEqual "trim no whitespace" "abc" (trim "abc")
  assertEqual "trim mixed whitespace" "abc" (trim "  \t abc \n  ")

-- | Unit tests for splitBy edge cases  
testSplitByEdgeCases :: IO ()
testSplitByEdgeCases = do
  assertEqual "splitBy empty string" [""] (splitBy ',' "")
  assertEqual "splitBy single char" ["a"] (splitBy ',' "a")
  assertEqual "splitBy with delimiter" ["a", "b"] (splitBy ',' "a,b")
  assertEqual "splitBy with empty segments" ["a", "", "b"] (splitBy ',' "a,,b")
  assertEqual "splitBy with leading/trailing delimiters" ["", "a", ""] (splitBy ',', ",a,")

-- | Unit tests for removeComments complex cases
testRemoveCommentsComplex :: IO ()
testRemoveCommentsComplex = do
  let input1 = "code // comment\nmore code"
      expected1 = "code \nmore code"
  assertEqual "remove line comments" expected1 (removeComments input1)
  
  let input2 = "code /* block comment */ more code"
      expected2 = "code  more code"
  assertEqual "remove block comments" expected2 (removeComments input2)
  
  let input3 = "code \"// not a comment\" more // real comment"
      expected3 = "code \"// not a comment\" more "
  assertEqual "ignore comments in strings" expected3 (removeComments input3)

-- Helper function for property testing
property :: Bool -> Property
property = property' where
  property' :: Bool -> Property
  property' = id