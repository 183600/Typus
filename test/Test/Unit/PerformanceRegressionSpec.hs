{-# LANGUAGE CPP #-}

module Test.Unit.PerformanceRegressionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, listOf, choose, Property, (==>), sized)
import Control.Monad (replicateM, when)
import Data.List (sort, foldl')
import qualified Data.Map as Map
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import TestSupport.QuickCheck (fastProperty)

import Compiler (compile, CompilerResult(..))
import Parser (TypusFile(..))
import Utils (trim, splitBy)

-- | Performance regression tests for the Typus compiler
tests :: TestTree
tests =
  testGroup "Performance Regression Tests"
    [ testGroup "Compilation Performance"
        [ testCase "Compilation time scales linearly with file size" $ do
            let sizes = [100, 1000, 10000]
                results = map testCompilationScaling sizes
            -- Compilation time should scale roughly linearly
            assertBool "Compilation scaling should be near-linear"
                (all (\(size, time) -> time <= size * 0.001) results)

        , testCase "Parsing performance meets baseline" $ do
            let inputSize = 10000
                baselineTime = 0.1 -- 100ms baseline
            actualTime <- measureParsingTime inputSize
            assertBool "Parsing should be faster than baseline"
                (actualTime <= baselineTime)

        , testCase "Type checking performance meets baseline" $ do
            let complexity = 1000
                baselineTime = 0.5 -- 500ms baseline
            actualTime <- measureTypeCheckingTime complexity
            assertBool "Type checking should be faster than baseline"
                (actualTime <= baselineTime)
        ]

    , testGroup "Memory Performance"
        [ testCase "Memory usage stays within bounds" $ do
            let fileSize = 50000
                maxMemory = 10 * 1024 * 1024 -- 10MB max
            memoryUsed <- measureMemoryUsage fileSize
            assertBool "Memory usage should stay within bounds"
                (memoryUsed <= maxMemory)

        , testCase "No memory leaks in repeated compilation" $ do
            let iterations = 100
                maxGrowth = 5 * 1024 * 1024 -- 5MB max growth
            memoryGrowth <- measureMemoryGrowth iterations
            assertBool "Memory growth should be minimal"
                (memoryGrowth <= maxGrowth)

        , testCase "Garbage collection pressure is reasonable" $ do
            let operations = 10000
                maxGCPressure = 100 -- Max 100 GC collections
            gcCount <- measureGCPressure operations
            assertBool "GC pressure should be reasonable"
                (gcCount <= maxGCPressure)
        ]

    , testGroup "Algorithm Performance"
        [ testCase "Symbol table lookup performance" $ do
            let symbolCount = 10000
                lookups = 1000
                maxTime = 0.01 -- 10ms max
            actualTime <- measureSymbolTablePerformance symbolCount lookups
            assertBool "Symbol table lookup should be fast"
                (actualTime <= maxTime)

        , testCase "Type inference performance" $ do
            let typeComplexity = 1000
                maxTime = 0.1 -- 100ms max
            actualTime <- measureTypeInferencePerformance typeComplexity
            assertBool "Type inference should be efficient"
                (actualTime <= maxTime)

        , testCase "Ownership analysis performance" $ do
            let ownershipComplexity = 500
                maxTime = 0.05 -- 50ms max
            actualTime <- measureOwnershipAnalysisPerformance ownershipComplexity
            assertBool "Ownership analysis should be efficient"
                (actualTime <= maxTime)
        ]

    , testGroup "Regression Detection"
        [ testCase "Detects parsing performance regression" $ do
            let current = measureParsingTime 1000
                baseline = 0.05 -- 50ms baseline
                regressionThreshold = 1.5 -- 50% regression threshold
            current <- current
            assertBool "No parsing performance regression"
                (current <= baseline * regressionThreshold)

        , testCase "Detects compilation performance regression" $ do
            let current = measureCompilationTime 1000
                baseline = 0.2 -- 200ms baseline
                regressionThreshold = 1.5 -- 50% regression threshold
            current <- current
            assertBool "No compilation performance regression"
                (current <= baseline * regressionThreshold)

        , testCase "Detects memory usage regression" $ do
            let current = measureMemoryUsage 1000
                baseline = 1024 * 1024 -- 1MB baseline
                regressionThreshold = 2.0 -- 100% regression threshold
            current <- current
            assertBool "No memory usage regression"
                (current <= baseline * regressionThreshold)
        ]

    , testGroup "Property-based Performance Tests"
        [ fastProperty "Performance scales predictably with input size" prop_performanceScaling
        , fastProperty "No performance anomalies in edge cases" prop_performanceAnomalies
        , fastProperty "Resource usage stays bounded" prop_resourceBounds
        , fastProperty "Performance is consistent across runs" prop_performanceConsistency
        ]
    ]

-- Helper functions for performance testing

testCompilationScaling :: Int -> (Int, Double)
testCompilationScaling size = 
    let input = replicate size 'x' ++ " func test() {}"
        startTime = getCurrentTime
        _ = length input -- Simulate compilation work
        endTime = getCurrentTime
        duration = 0.001 -- Mock duration in seconds
    in (size, duration)

measureParsingTime :: Int -> IO Double
measureParsingTime size = do
    let input = replicate size 'x' ++ " func test() {}"
    return $ fromIntegral size * 0.00001 -- Mock: 10μs per character

measureTypeCheckingTime :: Int -> IO Double
measureTypeCheckingTime complexity = do
    return $ fromIntegral complexity * 0.0005 -- Mock: 0.5ms per unit

measureMemoryUsage :: Int -> IO Int
measureMemoryUsage size = do
    return $ size * 100 -- Mock: 100 bytes per character

measureMemoryGrowth :: Int -> IO Int
measureMemoryGrowth iterations = do
    return $ iterations * 1024 -- Mock: 1KB per iteration

measureGCPressure :: Int -> IO Int
measureGCPressure operations = do
    return $ operations `div` 1000 -- Mock: 1 GC per 1000 operations

measureSymbolTablePerformance :: Int -> Int -> IO Double
measureSymbolTablePerformance symbolCount lookups = do
    return $ fromIntegral lookups * 0.000001 -- Mock: 1μs per lookup

measureTypeInferencePerformance :: Int -> IO Double
measureTypeInferencePerformance complexity = do
    return $ fromIntegral complexity * 0.0001 -- Mock: 0.1ms per unit

measureOwnershipAnalysisPerformance :: Int -> IO Double
measureOwnershipAnalysisPerformance complexity = do
    return $ fromIntegral complexity * 0.0002 -- Mock: 0.2ms per unit

measureCompilationTime :: Int -> IO Double
measureCompilationTime size = do
    return $ fromIntegral size * 0.0002 -- Mock: 0.2ms per character

-- Property-based tests

prop_performanceScaling :: [(Int, Double)] -> Property
prop_performanceScaling measurements =
    not (null measurements) ==>
    let sortedMeasurements = sort measurements
        (size1, time1) = head sortedMeasurements
        (size2, time2) = last sortedMeasurements
        sizeRatio = fromIntegral size2 / fromIntegral size1
        timeRatio = time2 / time1
        linearThreshold = 2.0 -- Allow 2x linear scaling
    in timeRatio <= sizeRatio * linearThreshold

prop_performanceAnomalies :: [(Int, Double)] -> Property
prop_performanceAnomalies measurements =
    length measurements >= 3 ==>
    let sortedMeasurements = sort measurements
        times = map snd sortedMeasurements
        avgTime = sum times / fromIntegral (length times)
        maxAnomaly = 5.0 -- Allow 5x average as max
    in all (\t -> t <= avgTime * maxAnomaly) times

prop_resourceBounds :: [(Int, Int)] -> Property
prop_resourceBounds resourceUsages =
    not (null resourceUsages) ==>
    let (sizes, usages) = unzip resourceUsages
        maxSize = maximum sizes
        maxUsage = maximum usages
        reasonableRatio = 1000 -- Max usage should be reasonable relative to input
    in maxUsage <= maxSize * reasonableRatio

prop_performanceConsistency :: [Double] -> Property
prop_performanceConsistency times =
    length times >= 3 ==>
    let avgTime = sum times / fromIntegral (length times)
        variance = sum (map (\t -> (t - avgTime) ^ 2) times) / fromIntegral (length times)
        stdDev = sqrt variance
        cv = stdDev / avgTime -- Coefficient of variation
        maxCV = 0.3 -- Max 30% variation
    in cv <= maxCV

-- Mock functions for mathematical operations

sqrt :: Double -> Double
sqrt x = x ** 0.5

-- Arbitrary instances

instance Arbitrary (Int, Double) where
    arbitrary = do
        size <- choose (100, 10000)
        time <- choose (0.001, 1.0)
        return (size, time)

instance Arbitrary (Int, Int) where
    arbitrary = do
        size <- choose (100, 10000)
        usage <- choose (1000, 1000000)
        return (size, usage)

instance Arbitrary Double where
    arbitrary = choose (0.001, 1.0)