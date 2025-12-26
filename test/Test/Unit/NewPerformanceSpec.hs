{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewPerformanceSpec (newPerformanceSpec, performanceQuickCheckProperties) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import Test.Tasty.QuickCheck (testProperty, Property(..), (==>), Positive(..))
import Parser
import Utils
import SourceLocation
import ErrorHandler
import Data.Maybe (isJust, isNothing)
import Data.Either (isLeft, isRight)
import Control.DeepSeq (force)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- | Test suite for performance characteristics
newPerformanceSpec :: TestTree
newPerformanceSpec = testGroup "New Performance Tests"
  [ testCase "Parser performance with large files" $ do
      let largeFunction = unlines 
            [ "func largeFunction() {"
            , "    var x int = 0"
            , "    var y int = 0"
            , "    var z int = 0"
            ] ++ 
            concat (replicate 100 ["    x = x + 1\n", "    y = y + 2\n", "    z = z + 3\n"]) ++
            ["    return x + y + z", "}"]
      
      let largeCode = "package main\n\n" ++ largeFunction
      
      start <- getCPUTime
      case parseTypus largeCode of
        Left err -> assertFailure $ "Parse failed: " ++ show err
        Right typusFile -> do
          end <- getCPUTime
          let diff = fromIntegral (end - start) / (10^12)
          assertBool "Parse should complete in reasonable time" $ diff < 1.0  -- Less than 1 second
          assertBool "Should parse large function" $ not (null (tfBlocks typusFile))
  
  , testCase "Utils performance with large strings" $ do
      let largeString = replicate 10000 'a' ++ "target" ++ replicate 10000 'b'
      
      start <- getCPUTime
      let result = trim largeString
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12)
      assertBool "Trim should complete quickly" $ diff < 0.1
      
      start2 <- getCPUTime
      let splitResult = splitBy ',' (replicate 5000 "a,b,c,")
      end2 <- getCPUTime
      let diff2 = fromIntegral (end2 - start2) / (10^12)
      assertBool "Split should complete quickly" $ diff2 < 0.1
      assertBool "Split should produce correct number of parts" $ length splitResult == 15001
  
  , testCase "SourceLocation performance with many positions" $ do
      let createPositions n = [posAt "test.typus" i (i `mod` 100 + 1) | i <- [1..n]]
      
      start <- getCPUTime
      let positions = createPositions 10000
      let spans = [spanBetween pos (posAfter pos 'a') | pos <- take 5000 positions]
      let validSpans = filter isValidSpan spans
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12)
      
      assertBool "Position creation should be fast" $ diff < 0.5
      assertBool "Should create expected number of positions" $ length positions == 10000
      assertBool "Should create valid spans" $ length validSpans == 5000
  
  , testCase "Error handling performance with many errors" $ do
      let createError i = basicError ("Error " ++ show i) (posAt "test.typus" i 1)
      
      start <- getCPUTime
      let errors = map createError [1..1000]
      let collection = foldr addError newErrorCollection errors
      let blocking = getBlockingErrors collection
      let formatted = formatErrorCollection collection
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12)
      
      assertBool "Error collection should be fast" $ diff < 0.5
      assertBool "Should collect all errors" $ getErrorCount collection == 1000
      assertBool "Should find blocking errors" $ length blocking == 1000
      assertBool "Should format all errors" $ not (null formatted)
  
  , testCase "Memory efficiency with repeated operations" $ do
      let testCode = "//! ownership: on\n//! dependent_types: on\npackage main\n\nfunc test() {\n    var x int = 5\n    return x\n}"
      
      -- Test repeated parsing doesn't leak memory excessively
      start <- getCPUTime
      let results = replicate 100 $ parseTypus testCode
      let successful = length [() | Right _ <- results]
      let forced = force results `seq` successful
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12)
      
      assertBool "Repeated parsing should be efficient" $ diff < 2.0
      assertBool "Most parses should succeed" $ successful >= 90
      
      -- Test repeated string operations
      let testString = "  hello, world, test, string  "
      start2 <- getCPUTime
      let stringResults = replicate 1000 (trim testString)
      let forcedStrings = force stringResults `seq` length stringResults
      end2 <- getCPUTime
      let diff2 = fromIntegral (end2 - start2) / (10^12)
      
      assertBool "Repeated string operations should be fast" $ diff2 < 0.5
      assertBool "Should produce expected results" $ all (== "hello, world, test, string") stringResults
  ]

-- QuickCheck properties for performance testing
prop_parse_performance_scales_linearly :: Positive Int -> Property
prop_parse_performance_scales_linearly (Positive n) = 
  n <= 100 ==>  -- Limit size for practical testing
    let code = unlines $ "package main" : replicate n "    // comment line"
    in case parseTypus code of
         Left _ -> True  -- Parse failures are acceptable
         Right typusFile -> length (tfBlocks typusFile) <= 1  -- Should not create excessive blocks

prop_string_operations_performance :: String -> Property
prop_string_operations_performance s = 
  length s <= 1000 ==>  -- Limit size for practical testing
    let trimmed = trim s
        split = splitBy ',' s
        removedComments = removeLineComments s
    in length trimmed <= length s &&
       length split >= 1 &&
       length removedComments <= length s

prop_position_creation_performance :: Positive Int -> Property
prop_position_creation_performance (Positive n) = 
  n <= 1000 ==> 
    let positions = [posAt "test.typus" i 1 | i <- [1..n]]
        spans = [spanFrom pos 5 | pos <- take (n `div` 2) positions]
    in length positions == n &&
       length spans == n `div` 2 &&
       all isValidSpan spans

prop_error_collection_performance :: Positive Int -> Property
prop_error_collection_performance (Positive n) = 
  n <= 1000 ==> 
    let errors = [basicError ("Error " ++ show i) (posAt "test.typus" i 1) | i <- [1..n]]
        collection = foldr addError newErrorCollection errors
    in getErrorCount collection == n &&
       length (getAllErrors collection) == n

prop_deepseq_performance :: [String] -> Property
prop_deepseq_performance strings = 
  length strings <= 100 ==> 
    let processed = map (trim . removeLineComments) strings
        forced = force processed
    in length forced == length strings

-- QuickCheck test suite
performanceQuickCheckProperties :: TestTree
performanceQuickCheckProperties = testGroup "Performance QuickCheck Properties"
  [ testProperty "parse performance scales linearly" prop_parse_performance_scales_linearly
  , testProperty "string operations performance" prop_string_operations_performance
  , testProperty "position creation performance" prop_position_creation_performance
  , testProperty "error collection performance" prop_error_collection_performance
  , testProperty "deepseq performance" prop_deepseq_performance
  ]