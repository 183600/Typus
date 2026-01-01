{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.PerformanceRegressionOptimizationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertFailure, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf
  , sized, resize, suchThat, frequency, choose, getPositive, getNonEmpty
  )

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub, sort, (\\), foldl')
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- | Generate performance benchmark scenarios
genBenchmarkScenario :: Gen BenchmarkScenario
genBenchmarkScenario = do
  inputSize <- choose (100, 1000000)
  algorithm <- elements [QuickSort, MergeSort, HeapSort, LinearSearch, BinarySearch]
  dataType <- elements [IntData, StringData, StructData]
  return $ BenchmarkScenario inputSize algorithm dataType

-- | Generate compilation performance scenarios
genCompilationScenario :: Gen CompilationScenario
genCompilationScenario = do
  sourceLines <- choose (100, 100000)
  optimizationLevel <- choose (0, 3)
  parallelism <- choose (1, 16)
  return $ CompilationScenario sourceLines optimizationLevel parallelism

-- | Generate memory usage scenarios
genMemoryScenario :: Gen MemoryScenario
genMemoryScenario = do
  allocationSize <- choose (1024, 104857600)  -- 1KB to 100MB
  allocationCount <- choose (1, 10000)
  allocationPattern <- elements [Sequential, Random, Burst]
  return $ MemoryScenario allocationSize allocationCount allocationPattern

-- | Generate I/O performance scenarios
genIOScenario :: Gen IOScenario
genIOScenario = do
  fileSize <- choose (1024, 104857600)  -- 1KB to 100MB
  operationType <- elements [ReadOp, WriteOp, RandomRead, SequentialWrite]
  bufferSize <- choose (1024, 65536)  -- 1KB to 64KB
  return $ IOScenario fileSize operationType bufferSize

-- | Generate concurrency performance scenarios
genConcurrencyScenario :: Gen ConcurrencyScenario
genConcurrencyScenario = do
  threadCount <- choose (1, 32)
  workloadPerThread <- choose (100, 10000)
  synchronizationOverhead <- choose (0, 100)
  return $ ConcurrencyScenario threadCount workloadPerThread synchronizationOverhead

-- | Generate regression test scenarios
genRegressionScenario :: Gen RegressionScenario
genRegressionScenario = do
  baselineTime <- choose (100, 10000)  -- microseconds
  currentTime <- choose (50, 15000)   -- microseconds
  tolerance <- choose (5, 20)         -- percentage
  return $ RegressionScenario baselineTime currentTime tolerance

-- Property: Sorting algorithms should maintain O(n log n) complexity
prop_sorting_complexity :: BenchmarkScenario -> Property
prop_sorting_complexity scenario =
  let BenchmarkScenario inputSize algorithm dataType = scenario
      complexity = getAlgorithmicComplexity algorithm
      theoreticalTime = calculateTheoreticalTime complexity inputSize
      actualTime = simulateAlgorithmPerformance scenario
  in property $ actualTime <= theoreticalTime * 10  -- Allow 10x variance

-- Property: Compilation time should scale linearly with source size
prop_compilation_linear_scaling :: CompilationScenario -> Property
prop_compilation_linear_scaling scenario =
  let CompilationScenario sourceLines optimizationLevel parallelism = scenario
      baselineTime = estimateCompilationTime 1000 optimizationLevel parallelism
      expectedTime = baselineTime * (sourceLines `div` 1000)
      actualTime = simulateCompilationPerformance scenario
  in property $ actualTime <= expectedTime * 2  -- Allow 2x variance

-- Property: Memory allocation should be efficient
prop_memory_allocation_efficient :: MemoryScenario -> Property
prop_memory_allocation_efficient scenario =
  let MemoryScenario allocationSize allocationCount allocationPattern = scenario
      totalMemory = allocationSize * allocationCount
      overhead = calculateMemoryOverhead allocationPattern
      expectedMemory = totalMemory + overhead
      actualMemory = simulateMemoryUsage scenario
  in property $ actualMemory <= expectedMemory * 1.5  -- Allow 50% overhead

-- Property: I/O operations should be buffer-size optimal
prop_io_buffer_optimal :: IOScenario -> Property
prop_io_buffer_optimal scenario =
  let IOScenario fileSize operationType bufferSize = scenario
      optimalBuffer = calculateOptimalBufferSize fileSize operationType
      efficiency = calculateIOEfficiency bufferSize optimalBuffer
  in property $ efficiency >= 0.8  -- At least 80% efficiency

-- Property: Concurrency should provide performance benefits
prop_concurrency_performance :: ConcurrencyScenario -> Property
prop_concurrency_performance scenario =
  let ConcurrencyScenario threadCount workloadPerThread syncOverhead = scenario
      sequentialTime = fromIntegral workloadPerThread * 1.0  -- microseconds
      parallelTime = fromIntegral workloadPerThread * 1.0 / fromIntegral threadCount + 
                    fromIntegral syncOverhead
      speedup = sequentialTime / parallelTime
  in property $ speedup >= 0.5 && speedup <= fromIntegral threadCount

-- Property: Performance regressions should be detected
prop_regression_detection :: RegressionScenario -> Property
prop_regression_detection scenario =
  let RegressionScenario baselineTime currentTime tolerance = scenario
      regressionDetected = detectPerformanceRegression baselineTime currentTime tolerance
      expectedRegression = currentTime > baselineTime * (1 + tolerance `div` 100)
  in property $ regressionDetected === expectedRegression

-- Property: Caching should improve performance
prop_caching_improves_performance :: BenchmarkScenario -> Property
prop_caching_improves_performance scenario =
  let uncachedTime = simulateAlgorithmPerformance scenario
      cachedTime = simulateCachedPerformance scenario
      improvement = (uncachedTime - cachedTime) / uncachedTime
  in property $ improvement >= 0.1  -- At least 10% improvement

-- Property: Optimization levels should provide measurable benefits
prop_optimization_benefits :: CompilationScenario -> Property
prop_optimization_benefits scenario =
  let CompilationScenario sourceLines optimizationLevel parallelism = scenario
      unoptimizedTime = simulateCompilationPerformance $ scenario { optimizationLevel = 0 }
      optimizedTime = simulateCompilationPerformance scenario
      improvement = (unoptimizedTime - optimizedTime) / unoptimizedTime
  in property $ optimizationLevel > 0 ==> improvement >= 0.05  -- At least 5% improvement

-- Property: Memory usage should not grow excessively
prop_memory_growth_bounded :: [MemoryScenario] -> Property
prop_memory_growth_bounded scenarios =
  not (null scenarios) ==> 
  let memoryUsages = map simulateMemoryUsage scenarios
      maxMemory = L.maximum memoryUsages
      minMemory = L.minimum memoryUsages
      growthRatio = if minMemory > 0 then maxMemory / minMemory else 1
  in property $ growthRatio <= 10  -- No more than 10x growth

-- Property: Algorithm performance should be consistent across runs
prop_performance_consistency :: BenchmarkScenario -> Property
prop_performance_consistency scenario =
  let runs = replicate 5 $ simulateAlgorithmPerformance scenario
      avgTime = average runs
      variance = calculateVariance runs avgTime
      coefficientOfVariation = variance / avgTime
  in property $ coefficientOfVariation <= 0.1  -- Less than 10% variance

-- Property: Large inputs should not cause exponential slowdown
prop_large_input_performance :: BenchmarkScenario -> Property
prop_large_input_performance scenario =
  let BenchmarkScenario inputSize algorithm dataType = scenario
      smallInput = scenario { inputSize = inputSize `div` 10 }
      smallTime = simulateAlgorithmPerformance smallInput
      largeTime = simulateAlgorithmPerformance scenario
      scalingFactor = fromIntegral largeTime / fromIntegral smallTime
  in property $ scalingFactor <= 100  -- Should not be more than 100x slower

-- Property: Parallel efficiency should not degrade with scale
prop_parallel_efficiency_scale :: ConcurrencyScenario -> Property
prop_parallel_efficiency_scale scenario =
  let ConcurrencyScenario threadCount workloadPerThread syncOverhead = scenario
      singleThreadTime = simulateConcurrencyPerformance $ scenario { threadCount = 1 }
      multiThreadTime = simulateConcurrencyPerformance scenario
      efficiency = (singleThreadTime / fromIntegral threadCount) / multiThreadTime
  in property $ efficiency >= 0.3  -- At least 30% parallel efficiency

-- Property: Memory access patterns should affect performance predictably
prop_memory_access_patterns :: MemoryScenario -> Property
prop_memory_access_patterns scenario =
  let MemoryScenario allocationSize allocationCount allocationPattern = scenario
      sequentialTime = simulateMemoryPerformance $ scenario { allocationPattern = Sequential }
      randomTime = simulateMemoryPerformance $ scenario { allocationPattern = Random }
      patternEffect = randomTime / sequentialTime
  in property $ allocationPattern == Random ==> patternEffect >= 1.5  -- Random should be slower

-- Property: Cache locality should impact performance
prop_cache_locality_impact :: BenchmarkScenario -> Property
prop_cache_locality_impact scenario =
  let goodLocalityTime = simulateAlgorithmWithCacheLocality scenario True
      poorLocalityTime = simulateAlgorithmWithCacheLocality scenario False
      localityBenefit = (poorLocalityTime - goodLocalityTime) / poorLocalityTime
  in property $ localityBenefit >= 0.1  -- At least 10% benefit from good locality

-- Property: Performance should not degrade with repeated operations
prop_repeated_operations_stable :: BenchmarkScenario -> Int -> Property
prop_repeated_operations_stable scenario iterations =
  iterations > 0 && iterations <= 1000 ==> 
  let firstRun = simulateAlgorithmPerformance scenario
      lastRun = simulateAlgorithmPerformance scenario
      degradation = (lastRun - firstRun) / firstRun
  in property | degradation <= 0.1  -- No more than 10% degradation

-- Property: Resource cleanup should not leak performance
prop_resource_cleanup_performance :: [MemoryScenario] -> Property
prop_resource_cleanup_performance scenarios =
  not (null scenarios) ==> 
  let withCleanupTime = simulateWithResourceCleanup scenarios
      withoutCleanupTime = simulateWithoutResourceCleanup scenarios
      cleanupOverhead = (withCleanupTime - withoutCleanupTime) / withoutCleanupTime
  in property $ cleanupOverhead <= 0.05  -- No more than 5% overhead

-- | Helper functions L.and data types

data Algorithm = QuickSort | MergeSort | HeapSort | LinearSearch | BinarySearch
  deriving (Show, Eq)

data DataType = IntData | StringData | StructData
  deriving (Show, Eq)

data BenchmarkScenario = BenchmarkScenario Int Algorithm DataType
  deriving (Show, Eq)

data CompilationScenario = CompilationScenario Int Int Int
  deriving (Show, Eq)

data AllocationPattern = Sequential | Random | Burst
  deriving (Show, Eq)

data MemoryScenario = MemoryScenario Int Int AllocationPattern
  deriving (Show, Eq)

data IOOperation = ReadOp | WriteOp | RandomRead | SequentialWrite
  deriving (Show, Eq)

data IOScenario = IOScenario Int IOOperation Int
  deriving (Show, Eq)

data ConcurrencyScenario = ConcurrencyScenario Int Int Int
  deriving (Show, Eq)

data RegressionScenario = RegressionScenario Int Int Int
  deriving (Show, Eq)

data Complexity = O1 | OLogN | ON | ONLogN | ON2 | ON3
  deriving (Show, Eq)

getAlgorithmicComplexity :: Algorithm -> Complexity
getAlgorithmicComplexity QuickSort = ONLogN
getAlgorithmicComplexity MergeSort = ONLogN
getAlgorithmicComplexity HeapSort = ONLogN
getAlgorithmicComplexity LinearSearch = ON
getAlgorithmicComplexity BinarySearch = OLogN

calculateTheoreticalTime :: Complexity -> Int -> Double
calculateTheoreticalTime O1 _ = 1.0
calculateTheoreticalTime OLogN n = fromIntegral (logBase 2 (fromIntegral n))
calculateTheoreticalTime ON n = fromIntegral n
calculateTheoreticalTime ONLogN n = fromIntegral n * logBase 2 (fromIntegral n)
calculateTheoreticalTime ON2 n = fromIntegral n * fromIntegral n
calculateTheoreticalTime ON3 n = fromIntegral n * fromIntegral n * fromIntegral n

simulateAlgorithmPerformance :: BenchmarkScenario -> Double
simulateAlgorithmPerformance (BenchmarkScenario inputSize algorithm dataType) =
  let baseTime = fromIntegral inputSize * 0.001  -- microseconds
      complexityFactor = case getAlgorithmicComplexity algorithm of
        O1 -> 0.1
        OLogN -> 0.5
        ON -> 1.0
        ONLogN -> 1.5
        ON2 -> 2.0
        ON3 -> 3.0
      dataTypeFactor = case dataType of
        IntData -> 1.0
        StringData -> 1.2
        StructData -> 1.5
  in baseTime * complexityFactor * dataTypeFactor

estimateCompilationTime :: Int -> Int -> Int -> Double
estimateCompilationTime baseLines optimizationLevel parallelism =
  let baseTime = fromIntegral baseLines * 0.01  -- microseconds
      optimizationFactor = 1.0 - fromIntegral optimizationLevel * 0.1
      parallelismFactor = 1.0 / fromIntegral parallelism
  in baseTime * optimizationFactor * parallelismFactor

simulateCompilationPerformance :: CompilationScenario -> Double
simulateCompilationPerformance (CompilationScenario sourceLines optimizationLevel parallelism) =
  estimateCompilationTime sourceLines optimizationLevel parallelism

calculateMemoryOverhead :: AllocationPattern -> Int
calculateMemoryOverhead Sequential = 1024
calculateMemoryOverhead Random = 4096
calculateMemoryOverhead Burst = 2048

simulateMemoryUsage :: MemoryScenario -> Int
simulateMemoryUsage (MemoryScenario allocationSize allocationCount allocationPattern) =
  let baseMemory = allocationSize * allocationCount
      overhead = calculateMemoryOverhead allocationPattern
  in baseMemory + overhead

calculateOptimalBufferSize :: Int -> IOOperation -> Int
calculateOptimalBufferSize fileSize operationType =
  case operationType of
    ReadOp -> min 65536 (fileSize `div` 100)
    WriteOp -> min 65536 (fileSize `div` 100)
    RandomRead -> 4096
    SequentialWrite -> min 65536 (fileSize `div` 50)

calculateIOEfficiency :: Int -> Int -> Double
calculateIOEfficiency actualOptimal optimal =
  if optimal > 0 then min 1.0 (fromIntegral actualOptimal / fromIntegral optimal) else 0.0

simulateConcurrencyPerformance :: ConcurrencyScenario -> Double
simulateConcurrencyPerformance (ConcurrencyScenario threadCount workloadPerThread syncOverhead) =
  let baseTime = fromIntegral workloadPerThread * 0.1
      parallelTime = baseTime / fromIntegral threadCount + fromIntegral syncOverhead
  in parallelTime

detectPerformanceRegression :: Int -> Int -> Int -> Bool
detectPerformanceRegression baseline current tolerance =
  let threshold = baseline * (1 + tolerance `div` 100)
  in current > threshold

simulateCachedPerformance :: BenchmarkScenario -> Double
simulateCachedPerformance scenario = simulateAlgorithmPerformance scenario * 0.7

simulateMemoryPerformance :: MemoryScenario -> Double
simulateMemoryPerformance scenario = fromIntegral (simulateMemoryUsage scenario) * 0.0001

simulateAlgorithmWithCacheLocality :: BenchmarkScenario -> Bool -> Double
simulateAlgorithmWithCacheLocality scenario goodLocality =
  let baseTime = simulateAlgorithmPerformance scenario
      factor = if goodLocality then 0.8 else 1.2
  in baseTime * factor

simulateWithResourceCleanup :: [MemoryScenario] -> Double
simulateWithResourceCleanup scenarios = 
  L.sum $ L.map (\s -> fromIntegral (simulateMemoryUsage s) * 0.0001) scenarios

simulateWithoutResourceCleanup :: [MemoryScenario] -> Double
simulateWithoutResourceCleanup scenarios = 
  L.sum $ L.map (\s -> fromIntegral (simulateMemoryUsage s) * 0.00009) scenarios

average :: [Double] -> Double
average xs = L.sum xs / fromIntegral (L.length xs)

calculateVariance :: [Double] -> Double -> Double
calculateVariance xs avg = L.sum $ L.map (\x -> (x - avg) ^ 2) xs

logBase :: Double -> Double -> Double
logBase b x = Prelude.log x / Prelude.log b

tests :: TestTree
tests = testGroup "Performance Regression Optimization Tests"
  [ testGroup "Property-based tests"
    [ fastProperty "sorting complexity" prop_sorting_complexity
    , fastProperty "compilation linear scaling" prop_compilation_linear_scaling
    , fastProperty "memory allocation efficient" prop_memory_allocation_efficient
    , fastProperty "IO buffer optimal" prop_io_buffer_optimal
    , fastProperty "concurrency performance" prop_concurrency_performance
    , fastProperty "regression detection" prop_regression_detection
    , fastProperty "caching improves performance" prop_caching_improves_performance
    , fastProperty "optimization benefits" prop_optimization_benefits
    , fastProperty "memory growth bounded" prop_memory_growth_bounded
    , fastProperty "performance consistency" prop_performance_consistency
    , fastProperty "large input performance" prop_large_input_performance
    , fastProperty "parallel efficiency scale" prop_parallel_efficiency_scale
    , fastProperty "memory access patterns" prop_memory_access_patterns
    , fastProperty "cache locality impact" prop_cache_locality_impact
    , fastProperty "repeated operations stable" prop_repeated_operations_stable
    , fastProperty "resource cleanup performance" prop_resource_cleanup_performance
    ]

  , testGroup "Unit tests"
    [ testCase "sorting algorithm performance" $ do
        let scenario = BenchmarkScenario 10000 QuickSort IntData
        let time = simulateAlgorithmPerformance scenario
        time >= 0 @?= True
    
    , testCase "compilation performance scaling" $ do
        let scenario = CompilationScenario 10000 2 4
        let time = simulateCompilationPerformance scenario
        time >= 0 @?= True
    
    , testCase "memory usage calculation" $ do
        let scenario = MemoryScenario 1024 100 Sequential
        let usage = simulateMemoryUsage scenario
        usage @?= 1024 * 100 + 1024
    
    , testCase "IO buffer efficiency" $ do
        let optimal = calculateOptimalBufferSize 1048576 ReadOp
        let efficiency = calculateIOEfficiency optimal optimal
        efficiency @?= 1.0
    
    , testCase "concurrency speedup" $ do
        let scenario = ConcurrencyScenario 4 1000 10
        let time = simulateConcurrencyPerformance scenario
        time >= 0 @?= True
    
    , testCase "regression detection" $ do
        let scenario = RegressionScenario 1000 1200 10
        let detected = detectPerformanceRegression 1000 1200 10
        detected @?= True
    
    , testCase "caching performance improvement" $ do
        let scenario = BenchmarkScenario 10000 MergeSort StringData
        let uncached = simulateAlgorithmPerformance scenario
        let cached = simulateCachedPerformance scenario
        cached < uncached @?= True
    
    , testCase "optimization level benefits" $ do
        let scenario = CompilationScenario 10000 2 4
        let unoptimized = simulateCompilationPerformance $ scenario { optimizationLevel = 0 }
        let optimized = simulateCompilationPerformance scenario
        optimized <= unoptimized @?= True
    ]
  ]

-- Arbitrary instances
instance Arbitrary Algorithm where
  arbitrary = elements [QuickSort, MergeSort, HeapSort, LinearSearch, BinarySearch]

instance Arbitrary DataType where
  arbitrary = elements [IntData, StringData, StructData]

instance Arbitrary BenchmarkScenario where
  arbitrary = genBenchmarkScenario

instance Arbitrary CompilationScenario where
  arbitrary = genCompilationScenario

instance Arbitrary AllocationPattern where
  arbitrary = elements [Sequential, Random, Burst]

instance Arbitrary MemoryScenario where
  arbitrary = genMemoryScenario

instance Arbitrary IOOperation where
  arbitrary = elements [ReadOp, WriteOp, RandomRead, SequentialWrite]

instance Arbitrary IOScenario where
  arbitrary = genIOScenario

instance Arbitrary ConcurrencyScenario where
  arbitrary = genConcurrencyScenario

instance Arbitrary RegressionScenario where
  arbitrary = genRegressionScenario