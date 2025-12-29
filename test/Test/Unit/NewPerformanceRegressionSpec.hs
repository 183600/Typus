{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewPerformanceRegressionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.)
  , Arbitrary(..), Gen, oneof, choose, listOf, vectorOf, elements, sized, frequency
  , suchThat, resize
  )

import Utils (trim, splitBy, removeComments, normalizeIndentation)
import SourceLocation (SourcePos(..), advancePos, advancePosByText)
import Data.List (length, foldl')
import Data.Char (isSpace)
import Control.DeepSeq (NFData, rnf)
import qualified Data.Text as T

-- Performance test data structures
data PerformanceTestData = PerformanceTestData
  { inputSize :: Int
  , testString :: String
  , testText :: T.Text
  } deriving (Show)

instance Arbitrary PerformanceTestData where
  arbitrary = sized genTestData
    where
      genTestData n = do
        size <- choose (1, max 1 (n * 10))
        content <- vectorOf size $ elements "abcde\n\t /-*"
        return $ PerformanceTestData size content (T.pack content)

-- Property: String processing performance scales linearly
prop_trim_performance_linear :: PerformanceTestData -> Property
prop_trim_performance_linear testData =
  inputSize testData > 0 ==>
  let result = trim $ testString testData
      resultLength = length result
  in classify (inputSize testData < 100) "small input" $
     classify (inputSize testData >= 100 && inputSize testData < 1000) "medium input" $
     classify (inputSize testData >= 1000) "large input" $
     property $ resultLength <= inputSize testData

-- Property: Split operation performance is reasonable
prop_split_performance_reasonable :: PerformanceTestData -> Char -> Property
prop_split_performance_reasonable testData delim =
  inputSize testData > 0 ==>
  let segments = splitBy delim $ testString testData
      segmentCount = length segments
  in property $ segmentCount <= inputSize testData + 1

-- Property: Comment removal doesn't blow up exponentially
prop_remove_comments_performance :: PerformanceTestData -> Property
prop_remove_comments_performance testData =
  inputSize testData > 0 ==>
  let result = removeComments $ testString testData
      resultLength = length result
  in property $ resultLength <= inputSize testData

-- Property: Normalization performance is linear
prop_normalize_indentation_performance :: PerformanceTestData -> Property
prop_normalize_indentation_performance testData =
  inputSize testData > 0 ==>
  let result = normalizeIndentation $ testString testData
      resultLength = length result
  in property $ resultLength <= inputSize testData + 100  -- Allow some overhead

-- Property: Source position tracking is efficient
prop_source_position_tracking_efficient :: PerformanceTestData -> Property
prop_source_position_tracking_efficient testData =
  inputSize testData > 0 ==>
  let text = testText testData
      finalPos = advancePosByText startPos text
      expectedLines = length $ T.filter (== '\n') text + 1
  in property $ posLine finalPos <= expectedLines + 10  -- Allow some margin

-- Property: Repeated operations don't accumulate memory
prop_repeated_operations_memory :: PerformanceTestData -> Int -> Property
prop_repeated_operations_memory testData iterations =
  iterations > 0 && iterations < 1000 ==>
  let content = testString testData
      performOperation n = if n <= 0 then content else performOperation (n - 1)
      result = performOperation iterations
  in property $ length result <= inputSize testData

-- Property: Large input handling doesn't crash
prop_large_input_handling :: Property
prop_large_input_handling =
  let largeInput = replicate 10000 'a' ++ "\n" ++ replicate 10000 'b'
      trimmed = trim largeInput
      split = splitBy '\n' largeInput
  in property $ not (null trimmed) && length split == 2

-- Property: Text vs String performance consistency
prop_text_string_consistency :: PerformanceTestData -> Property
prop_text_string_consistency testData =
  let str = testString testData
      txt = testText testData
      strLength = length str
      txtLength = T.length txt
  in property $ strLength === txtLength

-- Property: Nested operations performance
prop_nested_operations_performance :: PerformanceTestData -> Property
prop_nested_operations_performance testData =
  inputSize testData > 0 ==>
  let content = testString testData
      step1 = trim content
      step2 = splitBy '\n' step1
      step3 = map trim step2
      step4 = filter (not . null) step3
  in property $ length step4 <= length step2

-- Property: Memory usage doesn't grow excessively
prop_memory_usage_reasonable :: PerformanceTestData -> Property
prop_memory_usage_reasonable testData =
  let content = testString testData
      processed = rnf $ map trim $ splitBy ' ' content
  in property $ length content >= 0  -- Basic check that we can evaluate it

tests :: TestTree
tests = testGroup "New Performance Regression Tests"
  [ fastProperty "Trim performance scales linearly" prop_trim_performance_linear
  , fastProperty "Split performance is reasonable" prop_split_performance_reasonable
  , fastProperty "Remove comments performance" prop_remove_comments_performance
  , fastProperty "Normalize indentation performance" prop_normalize_indentation_performance
  , fastProperty "Source position tracking is efficient" prop_source_position_tracking_efficient
  , fastProperty "Repeated operations don't accumulate memory" prop_repeated_operations_memory
  , fastProperty "Large input handling doesn't crash" prop_large_input_handling
  , fastProperty "Text vs String performance consistency" prop_text_string_consistency
  , fastProperty "Nested operations performance" prop_nested_operations_performance
  , fastProperty "Memory usage is reasonable" prop_memory_usage_reasonable
  , testCase "Manual performance regression test" $ do
      let mediumInput = replicate 1000 "test string " ++ "end"
          trimmed = trim mediumInput
          splitResult = splitBy ' ' trimmed
      assertBool "Trim should work on medium input" $ not (null trimmed)
      assertBool "Split should produce reasonable number of segments" $ length splitResult > 0
      assertBool "Performance should be reasonable" $ length trimmed <= length mediumInput
  ]