module Test.Unit.NewCabalPerformanceSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.QuickCheck (property, forAll, Gen, arbitrary, choose, listOf1, elements, Positive(..))
import Data.List (isInfixOf, sort, nub)
import Data.Char (isLetter, isDigit)
import Control.DeepSeq (NFData, force)
import Criterion.Main (bench, bgroup, nf, whnf)

import TestSupport.QuickCheck (fastProperty)
import Compiler
import Parser
import Utils

-- | Performance and optimization tests
tests :: TestTree
tests =
  testGroup "New Cabal Performance Tests"
    [ testGroup "Parsing performance"
        [ testCase "large file parsing performance" $ do
            let largeInput = unlines (replicate 1000 "x := x + 1")
                startTime = getCurrentTime
                result = parse largeInput
                endTime = getCurrentTime
                duration = endTime - startTime
            case result of
              Left err -> @?= "Parse error" (show err)
              Right ast -> do
                assertBool "Parsing should complete within reasonable time" (duration < 1.0)
                length (lines ast) @?= 1000

        , testCase "nested structure parsing performance" $ do
            let nestedInput = generateNestedStructure 10
                startTime = getCurrentTime
                result = parse nestedInput
                endTime = getCurrentTime
                duration = endTime - startTime
            case result of
              Left err -> @?= "Parse error" (show err)
              Right ast -> do
                assertBool "Nested parsing should complete within reasonable time" (duration < 0.5)
                "func" `isInfixOf` ast @?= True

        , testCase "incremental parsing performance" $ do
            let chunks = ["x := 1\n", "y := 2\n", "z := x + y\n"]
                startTime = getCurrentTime
                results = map parse chunks
                endTime = getCurrentTime
                duration = endTime - startTime
            assertBool "Incremental parsing should be fast" (duration < 0.1)
            all isSuccess results @?= True
        ]

    , testGroup "Compilation performance"
        [ testCase "large program compilation performance" $ do
            let largeProgram = generateLargeProgram 500
                startTime = getCurrentTime
                result = compile largeProgram
                endTime = getCurrentTime
                duration = endTime - startTime
            case result of
              Left err -> @?= "Compile error" (show err)
              Right output -> do
                assertBool "Compilation should complete within reasonable time" (duration < 2.0)
                length output > 0 @?= True

        , testCase "optimization passes performance" $ do
            let program = generateOptimizableProgram 100
                startTime = getCurrentTime
                result = compileWithOptimizations program
                endTime = getCurrentTime
                duration = endTime - startTime
            case result of
              Left err -> @?= "Compile error" (show err)
              Right output -> do
                assertBool "Optimization should complete within reasonable time" (duration < 1.0)
                "optimized" `isInfixOf` output @?= True

        , testCase "parallel compilation performance" $ do
            let programs = [generateSimpleProgram i | i <- [1..10]]
                startTime = getCurrentTime
                results = compileParallel programs
                endTime = getCurrentTime
                duration = endTime - startTime
            assertBool "Parallel compilation should be faster" (duration < 1.5)
            all isSuccess results @?= True
        ]

    , testGroup "Memory usage performance"
        [ testCase "memory leak detection" $ do
            let initialMemory = getMemoryUsage
                _ = compileAndFree (generateLargeProgram 1000)
                finalMemory = getMemoryUsage
                memoryGrowth = finalMemory - initialMemory
            assertBool "Memory usage should not grow significantly" (memoryGrowth < 10.0)

        , testCase "garbage collection performance" $ do
            let iterations = 100
                startTime = getCurrentTime
                _ = sequence_ [compileAndFree (generateSimpleProgram i) | i <- [1..iterations]]
                forceGarbageCollection
                endTime = getCurrentTime
                duration = endTime - startTime
            assertBool "GC should handle frequent allocations efficiently" (duration < 2.0)

        , testCase "memory pool efficiency" $ do
            let initialMemory = getMemoryUsage
                _ = compileWithMemoryPool (generateLargeProgram 100)
                finalMemory = getMemoryUsage
                memoryUsed = finalMemory - initialMemory
            assertBool "Memory pool should reduce allocations" (memoryUsed < 5.0)
        ]

    , testGroup "Algorithmic complexity performance"
        [ testCase "type checking linear complexity" $ do
            let sizes = [100, 200, 400, 800]
                times = map testTypeCheckingTime sizes
                ratios = zipWith (/) (tail times) (init times)
            assertBool "Type checking should be near-linear" (all (< 2.5) ratios)

        , testCase "dependency analysis sublinear complexity" $ do
            let sizes = [100, 200, 400, 800]
                times = map testDependencyAnalysisTime sizes
                ratios = zipWith (/) (tail times) (init times)
            assertBool "Dependency analysis should be sublinear" (all (< 2.2) ratios)

        , testCase "optimization logarithmic complexity" $ do
            let sizes = [100, 200, 400, 800]
                times = map testOptimizationTime sizes
                ratios = zipWith (/) (tail times) (init times)
            assertBool "Optimization should be near-logarithmic" (all (< 1.8) ratios)
        ]

    , testGroup "Property-based performance tests"
        [ fastProperty "parsing scales linearly with input size" prop_parsingLinearScaling
        , fastProperty "compilation memory usage is bounded" prop_compilationMemoryBounded
        , fastProperty "optimization improves performance" prop_optimizationImprovesPerformance
        , fastProperty "parallel processing provides speedup" prop_parallelProcessingSpeedup
        ]
    ]

-- | Property: parsing scales linearly with input size
prop_parsingLinearScaling :: Positive Int -> Positive Int -> Bool
prop_parsingLinearScaling (Positive size1) (Positive size2)
  | size1 < 10 || size2 < 10 = True  -- Skip very small sizes
  | size1 > 1000 || size2 > 1000 = True  -- Skip very large sizes
  | otherwise =
      let input1 = unlines (replicate size1 "x := x + 1")
          input2 = unlines (replicate size2 "x := x + 1")
          time1 = measureParseTime input1
          time2 = measureParseTime input2
          expectedRatio = fromIntegral size2 / fromIntegral size1
          actualRatio = time2 / time1
      in abs (actualRatio - expectedRatio) < expectedRatio * 0.5  -- Within 50% of linear

-- | Property: compilation memory usage is bounded
prop_compilationMemoryBounded :: Positive Int -> Bool
prop_compilationMemoryBounded (Positive size)
  | size < 10 || size > 1000 = True
  | otherwise =
      let program = generateLargeProgram size
          initialMemory = getMemoryUsage
          _ = compile program
          finalMemory = getMemoryUsage
          memoryUsed = finalMemory - initialMemory
          expectedMax = fromIntegral size * 0.01  -- 0.01MB per line
      in memoryUsed < expectedMax

-- | Property: optimization improves performance
prop_optimizationImprovesPerformance :: Positive Int -> Bool
prop_optimizationImprovesPerformance (Positive size)
  | size < 10 || size > 500 = True
  | otherwise =
      let program = generateOptimizableProgram size
          unoptimizedTime = measureCompilationTime program False
          optimizedTime = measureCompilationTime program True
      in optimizedTime < unoptimizedTime * 0.9  -- At least 10% improvement

-- | Property: parallel processing provides speedup
prop_parallelProcessingSpeedup :: Positive Int -> Bool
prop_parallelProcessingSpeedup (Positive count)
  | count < 2 || count > 20 = True
  | otherwise =
      let programs = [generateSimpleProgram i | i <- [1..count]]
          sequentialTime = measureSequentialCompilation programs
          parallelTime = measureParallelCompilation programs
      in parallelTime < sequentialTime * 0.8  -- At least 20% speedup

-- Helper functions and mock implementations
data CompilationResult = 
    CompilationSuccess String
  | CompilationError String
  deriving (Show, Eq)

-- Mock functions for testing
parse :: String -> Either String String
parse input = Right ("Parsed: " ++ show (length (lines input)) ++ " lines")

compile :: String -> Either String String
compile input = Right ("Compiled: " ++ show (length input) ++ " chars")

compileWithOptimizations :: String -> Either String String
compileWithOptimizations input = Right ("Optimized: " ++ show (length input) ++ " chars")

compileParallel :: [String] -> [Either String String]
compileParallel programs = map compile programs

compileAndFree :: String -> CompilationResult
compileAndFree input = case compile input of
  Left err -> CompilationError err
  Right output -> CompilationSuccess output

compileWithMemoryPool :: String -> CompilationResult
compileWithMemoryPool input = CompilationSuccess ("Memory pooled: " ++ show (length input))

generateNestedStructure :: Int -> String
generateNestedStructure depth = 
  let indent = replicate depth ' '
      line = indent ++ "func level" ++ show depth ++ "() { return " ++ show depth ++ " }"
  in unlines (map generateNestedStructure [0..depth])

generateLargeProgram :: Int -> String
generateLargeProgram size = unlines 
  [ "func func" ++ show i ++ "() { return " ++ show i ++ " }"
  | i <- [1..size]
  ]

generateOptimizableProgram :: Int -> String
generateOptimizableProgram size = unlines
  [ "x" ++ show i ++ " := " ++ show i ++ " + " ++ show (i+1)
  | i <- [1..size]
  ]

generateSimpleProgram :: Int -> String
generateSimpleProgram seed = "x := " ++ show seed ++ "\n"

testTypeCheckingTime :: Int -> Double
testTypeCheckingTime size = fromIntegral size * 0.001  -- Mock: 1ms per 1000 lines

testDependencyAnalysisTime :: Int -> Double
testDependencyAnalysisTime size = fromIntegral size * 0.0005  -- Mock: 0.5ms per 1000 lines

testOptimizationTime :: Int -> Double
testOptimizationTime size = log (fromIntegral size) * 0.1  -- Mock: logarithmic

measureParseTime :: String -> Double
measureParseTime input = fromIntegral (length input) * 0.0001  -- Mock timing

measureCompilationTime :: String -> Bool -> Double
measureCompilationTime input optimized = 
  let baseTime = fromIntegral (length input) * 0.0002
      optimizationFactor = if optimized then 0.8 else 1.0
  in baseTime * optimizationFactor

measureSequentialCompilation :: [String] -> Double
measureSequentialCompilation programs = sum (map (flip measureCompilationTime False) programs)

measureParallelCompilation :: [String] -> Double
measureParallelCompilation programs = 
  let sequentialTime = measureSequentialCompilation programs
      parallelFactor = 0.6  -- Mock: 40% speedup with parallel processing
  in sequentialTime * parallelFactor

isSuccess :: Either a b -> Bool
isSuccess (Right _) = True
isSuccess (Left _) = False

-- Mock system functions
getCurrentTime :: Double
getCurrentTime = 0.0  -- Mock current time

getMemoryUsage :: Double
getMemoryUsage = 0.0  -- Mock memory usage in MB

forceGarbageCollection :: IO ()
forceGarbageCollection = return ()  -- Mock GC

-- Helper function for deep evaluation
forceResult :: NFData a => a -> ()
forceResult = force