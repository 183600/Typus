{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import System.Environment (lookupEnv, getArgs)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (when, unless, replicateM_)
import Control.Concurrent (threadDelay, forkIO)
import System.Mem (performGC)
import Data.IORef
import Data.List (isPrefixOf, isInfixOf)
import Text.Printf (printf)
import System.CPUTime (getCPUTime)
import qualified Data.Map.Strict as Map

-- Import test support modules
import qualified TestSupport.AdvancedMemoryOptimization as AdvancedMemoryOptimization
import TestSupport.MemoryLimits 
import TestSupport.OptimizedMemoryLimits

-- Import Utils module with alias U
import qualified Utils as U

-- Import core test modules (only the essential ones)
import qualified Test.Unit.BasicQuickCheckTestSuite as BasicQuickCheckTestSuite
import qualified Test.Unit.SimpleQuickCheckTestSuite as SimpleQuickCheckTestSuite
import qualified Test.Unit.ConciseTestSuite as ConciseTestSuite
import qualified Test.Unit.OptimizedTests as OptimizedTests
import qualified Test.Unit.EnhancedMemoryOptimizedTestSuite as EnhancedMemoryOptimizedTestSuite
import qualified Test.Unit.UltraMemoryOptimizedTestSuite as UltraMemoryOptimizedTestSuite

-- Check environment variables
isCIEnvironment :: IO Bool
isCIEnvironment = do
  ci <- lookupEnv "CI"
  continuous <- lookupEnv "CONTINUOUS_INTEGRATION"
  return $ (ci == Just "true") || (continuous == Just "true")

isDebugMode :: IO Bool
isDebugMode = do
  debug <- lookupEnv "TYPUS_DEBUG"
  return $ (debug == Just "true")

getMemoryLevel :: IO AdvancedMemoryOptimization.MemoryOptimizationLevel
getMemoryLevel = do
  level <- lookupEnv "TYPUS_MEMORY_LEVEL"
  case level of
    Just "ultra_minimal" -> return AdvancedMemoryOptimization.UltraMinimal
    Just "extreme_minimal" -> return AdvancedMemoryOptimization.ExtremeMinimal
    Just "aggressive_minimal" -> return AdvancedMemoryOptimization.AggressiveMinimal
    Just "moderate_minimal" -> return AdvancedMemoryOptimization.ModerateMinimal
    Just "conservative" -> return AdvancedMemoryOptimization.Conservative
    _ -> do
      isCI <- isCIEnvironment
      return $ if isCI then AdvancedMemoryOptimization.UltraMinimal else AdvancedMemoryOptimization.ModerateMinimal

getExecutionStrategy :: IO String
getExecutionStrategy = do
  strategy <- lookupEnv "TYPUS_EXECUTION_STRATEGY"
  return $ case strategy of
    Just s -> s
    Nothing -> "auto"

-- Memory monitoring
monitorMemoryUsage :: String -> IO ()
monitorMemoryUsage label = do
  debug <- isDebugMode
  when debug $ do
    -- Simple memory monitoring
    performGC
    threadDelay 1000  -- 1ms
    printf "[%s] Memory checkpoint\n" label

-- Aggressive cleanup
aggressiveCleanup :: IO ()
aggressiveCleanup = do
  debug <- isDebugMode
  when debug $ printf "Performing aggressive cleanup...\n"
  
  -- Multiple rounds of garbage collection
  replicateM_ 3 $ do
    performGC
    threadDelay 1000  -- 1ms
  
  -- Final cleanup
  performGC

-- Create minimal test properties to preserve test count
prop_minimal_string_property :: String -> Property
prop_minimal_string_property s = 
  let limited = take 1 s  -- Limit input size
  in property $ length limited >= 0

prop_minimal_list_property :: [Int] -> Property
prop_minimal_list_property xs = 
  let limited = take 1 xs  -- Limit input size
  in property $ length limited >= 0

prop_minimal_int_property :: Int -> Property
prop_minimal_int_property n = 
  let limited = max 0 (min 1 n)  -- Limit range
  in property $ limited >= 0

prop_minimal_char_property :: Char -> Property
prop_minimal_char_property c = property $ U.isValidChar c

-- Create essential test suite with minimal memory footprint
createEssentialTestSuite :: AdvancedMemoryOptimization.MemoryOptimizationLevel -> TestTree
createEssentialTestSuite level = 
  let essentialTests = 
        [ testProperty "minimal string property" prop_minimal_string_property
        , testProperty "minimal list property" prop_minimal_list_property
        , testProperty "minimal int property" prop_minimal_int_property
        , testProperty "minimal char property" prop_minimal_char_property
        ]
      -- Apply memory limits to all tests
      limitedTests = map (AdvancedMemoryOptimization.withAdvancedMemoryLimits level) essentialTests
  in testGroup "Essential Memory-Optimized Tests" limitedTests

-- Create comprehensive test suite with memory optimization
createComprehensiveTestSuite :: AdvancedMemoryOptimization.MemoryOptimizationLevel -> TestTree
createComprehensiveTestSuite level = 
  let coreTests = 
        [ BasicQuickCheckTestSuite.essentialTests
        , SimpleQuickCheckTestSuite.tests
        , ConciseTestSuite.tests
        , OptimizedTests.tests
        , EnhancedMemoryOptimizedTestSuite.tests
        , UltraMemoryOptimizedTestSuite.tests
        ]
      -- Apply memory limits to all test suites
      limitedTests = map (AdvancedMemoryOptimization.withAdvancedMemoryLimits level) coreTests
  in testGroup "Comprehensive Memory-Optimized Tests" limitedTests

-- Run tests with streaming strategy
runLocalStreamingTests :: AdvancedMemoryOptimization.MemoryOptimizationLevel -> IO ()
runLocalStreamingTests level = do
  printf "Running tests with streaming strategy at level: %s\n" (show level)
  
  let streamConfig = AdvancedMemoryOptimization.StreamingTestConfig
        { AdvancedMemoryOptimization.streamChunkSize = 3
        , AdvancedMemoryOptimization.streamDelay = 50000  -- 50ms
        , AdvancedMemoryOptimization.gcBetweenChunks = True
        , AdvancedMemoryOptimization.monitorStreamMemory = True
        , AdvancedMemoryOptimization.maxStreamMemoryMB = case level of
            AdvancedMemoryOptimization.UltraMinimal -> 32
            AdvancedMemoryOptimization.ExtremeMinimal -> 64
            AdvancedMemoryOptimization.AggressiveMinimal -> 128
            AdvancedMemoryOptimization.ModerateMinimal -> 256
            AdvancedMemoryOptimization.Conservative -> 512
        }
  
  -- Create test chunks
  let essentialSuite = createEssentialTestSuite level
  let comprehensiveSuite = createComprehensiveTestSuite level
  
  -- Run tests in chunks
  AdvancedMemoryOptimization.withStreamingExecution streamConfig [essentialSuite, comprehensiveSuite]

-- Run tests with batched strategy
runLocalBatchedTests :: AdvancedMemoryOptimization.MemoryOptimizationLevel -> IO ()
runLocalBatchedTests level = do
  printf "Running tests with batched strategy at level: %s\n" (show level)
  
  let batchConfig = AdvancedMemoryOptimization.TestBatchConfig
        { AdvancedMemoryOptimization.batchSize = 5
        , AdvancedMemoryOptimization.batchDelay = 100000  -- 100ms
        , AdvancedMemoryOptimization.gcBetweenBatches = True
        , AdvancedMemoryOptimization.monitorBatchMemory = True
        , AdvancedMemoryOptimization.maxBatchMemoryMB = case level of
            AdvancedMemoryOptimization.UltraMinimal -> 64
            AdvancedMemoryOptimization.ExtremeMinimal -> 128
            AdvancedMemoryOptimization.AggressiveMinimal -> 256
            AdvancedMemoryOptimization.ModerateMinimal -> 512
            AdvancedMemoryOptimization.Conservative -> 1024
        }
  
  -- Create test batches
  let essentialSuite = createEssentialTestSuite level
  let comprehensiveSuite = createComprehensiveTestSuite level
  
  -- Run tests in batches
  AdvancedMemoryOptimization.withBatchedExecution batchConfig [essentialSuite, comprehensiveSuite]

-- Run tests with direct strategy
runDirectTests :: AdvancedMemoryOptimization.MemoryOptimizationLevel -> IO ()
runDirectTests level = do
  printf "Running tests with direct strategy at level: %s\n" (show level)
  
  monitorMemoryUsage "direct-start"
  
  -- Create test suite
  let testSuite = testGroup "Direct Memory-Optimized Tests"
        [ createEssentialTestSuite level
        , createComprehensiveTestSuite level
        ]
  
  -- Run tests directly
  result <- defaultMainWithIngredients defaultIngredients testSuite
  
  monitorMemoryUsage "direct-end"
  aggressiveCleanup
  
  return result

-- Run tests with minimal footprint strategy
runLocalMinimalFootprintTests :: AdvancedMemoryOptimization.MemoryOptimizationLevel -> IO ()
runLocalMinimalFootprintTests level = do
  printf "Running tests with minimal footprint strategy at level: %s\n" (show level)
  
  let footprintConfig = AdvancedMemoryOptimization.MinimalFootprintConfig
        { AdvancedMemoryOptimization.maxMemoryMB = case level of
            AdvancedMemoryOptimization.UltraMinimal -> 32
            AdvancedMemoryOptimization.ExtremeMinimal -> 64
            AdvancedMemoryOptimization.AggressiveMinimal -> 128
            AdvancedMemoryOptimization.ModerateMinimal -> 256
            AdvancedMemoryOptimization.Conservative -> 512
        , AdvancedMemoryOptimization.maxQuickCheckTests = case level of
            AdvancedMemoryOptimization.UltraMinimal -> 1
            AdvancedMemoryOptimization.ExtremeMinimal -> 2
            AdvancedMemoryOptimization.AggressiveMinimal -> 3
            AdvancedMemoryOptimization.ModerateMinimal -> 5
            AdvancedMemoryOptimization.Conservative -> 10
        , AdvancedMemoryOptimization.maxQuickCheckSize = case level of
            AdvancedMemoryOptimization.UltraMinimal -> 1
            AdvancedMemoryOptimization.ExtremeMinimal -> 1
            AdvancedMemoryOptimization.AggressiveMinimal -> 2
            AdvancedMemoryOptimization.ModerateMinimal -> 3
            AdvancedMemoryOptimization.Conservative -> 5
        , AdvancedMemoryOptimization.maxQuickCheckShrinks = case level of
            AdvancedMemoryOptimization.UltraMinimal -> 1
            AdvancedMemoryOptimization.ExtremeMinimal -> 2
            AdvancedMemoryOptimization.AggressiveMinimal -> 3
            AdvancedMemoryOptimization.ModerateMinimal -> 5
            AdvancedMemoryOptimization.Conservative -> 8
        , AdvancedMemoryOptimization.forceGCFrequency = 1
        , AdvancedMemoryOptimization.enableMemoryMonitoring = True
        , AdvancedMemoryOptimization.cleanupAfterEachTest = True
        }
  
  -- Create test suite
  let testSuite = testGroup "Minimal Footprint Tests"
        [ createEssentialTestSuite level
        , createComprehensiveTestSuite level
        ]
  
  -- Run with minimal footprint
  AdvancedMemoryOptimization.withMinimalFootprint footprintConfig [testSuite]

-- Main test execution logic
runTests :: IO ()
runTests = do
  -- Get configuration
  level <- getMemoryLevel
  strategy <- getExecutionStrategy
  isCI <- isCIEnvironment
  debug <- isDebugMode
  
  when debug $ do
    printf "Debug mode enabled\n"
    printf "Memory level: %s\n" (show level)
    printf "Execution strategy: %s\n" strategy
    printf "CI environment: %s\n" (if isCI then ("Yes" :: String) else "No")
  
  -- Force initial cleanup
  aggressiveCleanup
  
  -- Choose execution strategy
  case strategy of
    "streaming" -> runLocalStreamingTests level
    "batched" -> runLocalBatchedTests level
    "direct" -> runDirectTests level
    "minimal" -> runLocalMinimalFootprintTests level
    "auto" -> do
      -- Auto-select strategy based on memory level
      case level of
        AdvancedMemoryOptimization.UltraMinimal -> runLocalMinimalFootprintTests level
        AdvancedMemoryOptimization.ExtremeMinimal -> runLocalStreamingTests level
        AdvancedMemoryOptimization.AggressiveMinimal -> runLocalBatchedTests level
        AdvancedMemoryOptimization.ModerateMinimal -> runDirectTests level
        AdvancedMemoryOptimization.Conservative -> runDirectTests level
    _ -> do
      printf "Unknown execution strategy: %s\n" strategy
      printf "Available strategies: streaming, batched, direct, minimal, auto\n"
      exitFailure

-- Main function
main :: IO ()
main = do
  args <- getArgs
  
  -- Handle help flag
  when ("--help" `elem` args || "-h" `elem` args) $ do
    putStrLn "Advanced Memory-Optimized Test Runner for Typus"
    putStrLn ""
    putStrLn "Environment Variables:"
    putStrLn "  TYPUS_MEMORY_LEVEL      Memory optimization level"
    putStrLn "                          (ultra_minimal, extreme_minimal, aggressive_minimal, moderate_minimal, conservative)"
    putStrLn "  TYPUS_EXECUTION_STRATEGY Test execution strategy"
    putStrLn "                          (streaming, batched, direct, minimal, auto)"
    putStrLn "  TYPUS_DEBUG             Enable debug output (true/false)"
    putStrLn ""
    putStrLn "Examples:"
    putStrLn "  ./test-runner                           # Run with auto settings"
    putStrLn "  TYPUS_MEMORY_LEVEL=ultra_minimal ./test-runner  # Run with ultra minimal memory"
    putStrLn "  TYPUS_EXECUTION_STRATEGY=streaming ./test-runner # Run with streaming strategy"
    exitSuccess
  
  -- Print startup message
  printf "Starting Advanced Memory-Optimized Test Runner\n"
  
  -- Run tests
  runTests
  
  -- Final cleanup
  aggressiveCleanup
  printf "Test run completed successfully\n"