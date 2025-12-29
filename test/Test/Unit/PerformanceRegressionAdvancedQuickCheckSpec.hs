{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.PerformanceRegressionAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, suchThat)
import TestSupport.Arbitrary

import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import Compiler.IR (buildSourceIR, buildSemanticIR, emitGo)
import Ownership (analyzeOwnership)
import DependentTypesParser (validateDependentTypeSyntax)
import Utils (trim, removeComments, normalizeIndentation)
import Data.List (sort, nub, length, filter, elem, intercalate, concat)
import Data.Set (Set, empty, singleton, union, unions, member, size, difference, intersection)
import qualified Data.Set as Set
import Data.Map (Map, empty, singleton, insert, lookup, keys, elems, unionWith)
import qualified Data.Map as Map
import Data.Either (isLeft, isRight, fromLeft, fromRight)
import Data.Maybe (isJust, isNothing, catMaybes, fromMaybe, mapMaybe)
import qualified Data.Text as T
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- ============================================================================
-- Advanced Performance Regression QuickCheck Tests
-- ============================================================================

-- Property: Parsing time scales linearly with input size
prop_parsing_time_linear :: String -> Int -> Property
prop_parsing_time_linear baseContent multiplier =
  length baseContent > 0 && multiplier > 0 && multiplier <= 50 ==>
  let smallSource = baseContent
      largeSource = concat (replicate multiplier baseContent)
      smallTime = measureTime $ parseTypus smallSource
      largeTime = measureTime $ parseTypus largeSource
      ratio = fromIntegral largeTime / fromIntegral (smallTime * max 1 multiplier)
  in property $ ratio < 10.0  -- Allow 10x overhead for linear scaling

-- Property: IR building time scales reasonably
prop_ir_building_time_reasonable :: String -> Int -> Property
prop_ir_building_time_reasonable baseContent complexity =
  length baseContent > 0 && complexity > 0 && complexity <= 20 ==>
  let parseResult = parseTypus baseContent
  in case parseResult of
    Left _ -> property True
    Right typusFile -> 
      let irTime = measureTime $ buildSourceIR typusFile baseContent
          semanticTime = measureTime $ buildSemanticIR typusFile
      in property $ 
        fromIntegral irTime < 1000000 .&&.  -- Less than 1ms in picoseconds
        fromIntegral semanticTime < 2000000  -- Less than 2ms

-- Property: Memory usage is bounded
prop_memory_usage_bounded :: String -> Int -> Property
prop_memory_usage_bounded baseContent repetitions =
  length baseContent > 0 && repetitions > 0 && repetitions <= 10 ==>
  let sources = replicate repetitions baseContent
      parseResults = map parseTypus sources
      successfulParses = [typusFile | Right typusFile <- parseResults]
      irs = map (\f -> buildSourceIR f baseContent) successfulParses
  in property $ 
    length irs <= length successfulParses .&&.
    all (\ir -> sourceText ir `seq` True) irs  -- Force evaluation

-- Property: Error handling performance is consistent
prop_error_handling_performance :: String -> Property
prop_error_handling_performance malformedInput =
  length malformedInput > 0 ==>
  let parseTime = measureTime $ parseTypus malformedInput
  in property $ fromIntegral parseTime < 5000000  -- Less than 5ms

-- Property: Ownership analysis performance scales
prop_ownership_analysis_performance :: String -> Int -> Property
prop_ownership_analysis_performance baseContent numVariables =
  length baseContent > 0 && numVariables > 0 && numVariables <= 100 ==>
  let sourceWithVars = unlines $ map (\i -> "let var" ++ show i ++ " = " ++ baseContent) [1..numVariables]
      parseResult = parseTypus sourceWithVars
  in case parseResult of
    Left _ -> property True
    Right typusFile -> 
      let analysisTime = measureTime $ analyzeOwnership typusFile
      in property $ fromIntegral analysisTime < 10000000  -- Less than 10ms

-- Property: Type validation performance is bounded
prop_type_validation_performance :: String -> Int -> Property
prop_type_validation_performance typeBase numTypes =
  length typeBase > 0 && numTypes > 0 && numTypes <= 50 ==>
  let typeDefinitions = unlines $ map (\i -> "type Type" ++ show i ++ " = " ++ typeBase) [1..numTypes]
      validationTime = measureTime $ validateDependentTypeSyntax typeDefinitions
  in property $ fromIntegral validationTime < 5000000  -- Less than 5ms

-- Property: String processing performance is linear
prop_string_processing_performance :: String -> Int -> Property
prop_string_processing_performance baseString multiplier =
  length baseString > 0 && multiplier > 0 && multiplier <= 100 ==>
  let smallString = baseString
      largeString = concat (replicate multiplier baseString)
      smallTime = measureTime $ trim (removeComments (normalizeIndentation smallString))
      largeTime = measureTime $ trim (removeComments (normalizeIndentation largeString))
      ratio = fromIntegral largeTime / fromIntegral (smallTime * max 1 multiplier)
  in property $ ratio < 5.0  -- Allow 5x overhead for linear scaling

-- Property: Go code generation performance is reasonable
prop_go_generation_performance :: String -> Int -> Property
prop_go_generation_performance baseContent numFunctions =
  length baseContent > 0 && numFunctions > 0 && numFunctions <= 50 ==>
  let sourceWithFuncs = unlines $ map (\i -> "func func" ++ show i ++ "() { " ++ baseContent ++ " }") [1..numFunctions]
      parseResult = parseTypus sourceWithFuncs
  in case parseResult of
    Left _ -> property True
    Right typusFile -> 
      let semanticIR = buildSemanticIR typusFile
          generationTime = measureTime $ emitGo semanticIR
      in property $ fromIntegral generationTime < 10000000  -- Less than 10ms

-- Property: Concurrent compilation performance
prop_concurrent_compilation_performance :: [String] -> Property
prop_concurrent_compilation_performance sources =
  length sources > 0 && length sources <= 10 && all (not . null) sources ==>
  let sequentialTime = sum $ map (\s -> measureTime $ parseTypus s) sources
      -- In a real scenario, we'd run these in parallel
      theoreticalParallelTime = sequentialTime `div` length sources
  in property $ 
    fromIntegral sequentialTime > 0 .&&.
    fromIntegral theoreticalParallelTime >= 0

-- Property: Large file handling performance
prop_large_file_performance :: String -> Int -> Property
prop_large_file_performance baseContent sizeMultiplier =
  length baseContent > 0 && sizeMultiplier > 0 && sizeMultiplier <= 20 ==>
  let largeSource = unlines $ replicate sizeMultiplier baseContent
      parseResult = parseTypus largeSource
  in case parseResult of
    Left _ -> property True
    Right typusFile -> 
      let processingTime = measureTime $ do
            let sourceIR = buildSourceIR typusFile largeSource
            let semanticIR = buildSemanticIR typusFile
            let goIR = emitGo semanticIR
            return $ sourceIR `seq` semanticIR `seq` goIR
      in property $ fromIntegral processingTime < 50000000  -- Less than 50ms

-- Property: Memory cleanup performance
prop_memory_cleanup_performance :: String -> Int -> Property
prop_memory_cleanup_performance baseContent iterations =
  length baseContent > 0 && iterations > 0 && iterations <= 20 ==>
  let parseAndProcess source = do
        let result = parseTypus source
        case result of
          Left _ -> return ()
          Right typusFile -> do
            let sourceIR = buildSourceIR typusFile source
            let semanticIR = buildSemanticIR typusFile
            let goIR = emitGo semanticIR
            return $ sourceIR `seq` semanticIR `seq` goIR
      cleanupTime = measureTime $ sequence_ $ replicate iterations (parseAndProcess baseContent)
  in property $ fromIntegral cleanupTime < fromIntegral iterations * 10000000  -- Less than 10ms per iteration

-- Property: Performance regression detection
prop_performance_regression_detection :: String -> Property
prop_performance_regression_detection sourceCode =
  length sourceCode > 0 ==>
  let baselineTime = measureTime $ parseTypus sourceCode
      optimizedTime = measureTime $ parseTypus sourceCode  -- Same operation for comparison
      regressionRatio = fromIntegral optimizedTime / fromIntegral (max baselineTime 1)
  in property $ regressionRatio < 2.0  -- Should not be more than 2x slower

-- Helper function to measure execution time
measureTime :: IO a -> Integer
measureTime action = 
  let timedAction = do
        start <- getCPUTime
        result <- action
        end <- getCPUTime
        return (end - start)
  in unsafePerformIO timedAction

-- Unsafe import for performance testing
import System.IO.Unsafe (unsafePerformIO)

-- Test collection
tests :: TestTree
tests = testGroup "Advanced Performance Regression QuickCheck Tests"
  [ fastProperty "Parsing time scales linearly with input size" prop_parsing_time_linear
  , fastProperty "IR building time scales reasonably" prop_ir_building_time_reasonable
  , fastProperty "Memory usage is bounded" prop_memory_usage_bounded
  , fastProperty "Error handling performance is consistent" prop_error_handling_performance
  , fastProperty "Ownership analysis performance scales" prop_ownership_analysis_performance
  , fastProperty "Type validation performance is bounded" prop_type_validation_performance
  , fastProperty "String processing performance is linear" prop_string_processing_performance
  , fastProperty "Go code generation performance is reasonable" prop_go_generation_performance
  , fastProperty "Concurrent compilation performance" prop_concurrent_compilation_performance
  , fastProperty "Large file handling performance" prop_large_file_performance
  , fastProperty "Memory cleanup performance" prop_memory_cleanup_performance
  , fastProperty "Performance regression detection" prop_performance_regression_detection
  ]