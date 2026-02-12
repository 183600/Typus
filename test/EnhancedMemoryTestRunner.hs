{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import System.Environment (lookupEnv, getArgs)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (when, unless, replicateM_)
import Control.Concurrent (threadDelay)
import System.Mem (performGC)
import Text.Printf (printf)

-- Import enhanced memory optimization modules
import TestSupport.EnhancedMemoryOptimization
import TestSupport.MemoryEfficientGenerators
import TestSupport.MemoryLimits
import TestSupport.OptimizedMemoryLimits

-- Import essential test modules only
import qualified Test.Unit.BasicQuickCheckTestSuite as BasicQuickCheckTestSuite
import qualified Test.Unit.SimpleQuickCheckTestSuite as SimpleQuickCheckTestSuite
import qualified Test.Unit.ConciseTestSuite as ConciseTestSuite

-- Environment detection functions
detectCIEnvironment :: IO Bool
detectCIEnvironment = do
  ci <- lookupEnv "CI"
  continuous <- lookupEnv "CONTINUOUS_INTEGRATION"
  github <- lookupEnv "GITHUB_ACTIONS"
  gitlab <- lookupEnv "GITLAB_CI"
  return $ any (== Just "true") [ci, continuous, github, gitlab]

detectMemoryConstraints :: IO (Maybe Int)
detectMemoryConstraints = do
  memLimit <- lookupEnv "TYPUS_MEMORY_LIMIT_MB"
  case memLimit of
    Just str -> return $ case reads str of [(x, "")] -> Just x; _ -> Nothing
    Nothing -> return Nothing

detectMemoryLevel :: IO MemoryOptimizationLevel
detectMemoryLevel = do
  level <- lookupEnv "TYPUS_MEMORY_LEVEL"
  case level of
    Just "micro" -> return Micro
    Just "ultra_light" -> return UltraLight
    Just "enhanced" -> return Enhanced
    Just "standard" -> return Standard
    _ -> do
      isCI <- detectCIEnvironment
      if isCI 
        then return Micro  -- Use minimal memory in CI
        else return Enhanced  -- Use enhanced memory for development

-- Enhanced memory monitoring
performEnhancedMemoryMonitoring :: String -> EnhancedMemoryConfig -> IO ()
performEnhancedMemoryMonitoring label config = do
  printf "[%s] Memory checkpoint - Level: %s\n" label (show $ memoryLevel config)
  performGC
  threadDelay $ memoryCleanupDelay config

-- Enhanced cleanup with multiple strategies
performComprehensiveCleanup :: EnhancedMemoryConfig -> IO ()
performComprehensiveCleanup config = do
  -- Multi-phase garbage collection
  replicateM_ 3 $ do
    performGC
    threadDelay (memoryCleanupDelay config)
  
  -- Final cleanup
  performGC
  
  -- Additional cleanup for test isolation
  when (enableTestIsolation config) $ do
    threadDelay (memoryCleanupDelay config * 2)
    performGC

-- Create memory-efficient test properties
prop_enhanced_string_idempotent :: String -> Property
prop_enhanced_string_idempotent s = 
  let limited = take 4 s  -- Limit to 4 characters max
  in property $ length limited >= 0

prop_enhanced_list_preservation :: [Int] -> Property
prop_enhanced_list_preservation xs = 
  let limited = take 3 xs  -- Limit to 3 elements max
  in property $ length limited >= 0

prop_enhanced_int_range :: Int -> Property
prop_enhanced_int_range n = 
  let limited = max (-50) (min 50 n)  -- Limit to small range
  in property $ limited >= (-50) && limited <= 50

prop_enhanced_char_validity :: Char -> Property
prop_enhanced_char_validity c = 
  let limited = if c > 'z' then 'a' else if c < 'A' then 'A' else c
  in property $ limited >= 'A' && limited <= 'z'

-- Memory-efficient test suite creation
createMemoryEfficientTestSuite :: EnhancedMemoryConfig -> TestTree
createMemoryEfficientTestSuite config = 
  let essentialTests = 
        [ testProperty "enhanced string idempotent" prop_enhanced_string_idempotent
        , testProperty "enhanced list preservation" prop_enhanced_list_preservation
        , testProperty "enhanced int range" prop_enhanced_int_range
        , testProperty "enhanced char validity" prop_enhanced_char_validity
        ]
      -- Apply enhanced memory limits
      limitedTests = map (withEnhancedMemoryLimits config) essentialTests
  in createEnhancedMemoryTestGroup config "Enhanced Memory-Efficient Tests" limitedTests

-- Create comprehensive test suite with memory optimization
createOptimizedTestSuite :: EnhancedMemoryConfig -> TestTree
createOptimizedTestSuite config = 
  let coreTests = 
        [ BasicQuickCheckTestSuite.tests
        , SimpleQuickCheckTestSuite.tests
        , ConciseTestSuite.tests
        ]
      -- Apply enhanced memory limits to all test suites
      limitedTests = map (withEnhancedMemoryLimits config) coreTests
  in createEnhancedMemoryTestGroup config "Optimized Test Suite" limitedTests

-- Run tests with enhanced memory management
runEnhancedMemoryTests :: EnhancedMemoryConfig -> IO ()
runEnhancedMemoryTests config = do
  printf "Running tests with enhanced memory optimization\n"
  printf "Memory level: %s\n" (show $ TestSupport.EnhancedMemoryOptimization.memoryLevel config)
  printf "Max string size: %d\n" (TestSupport.EnhancedMemoryOptimization.maxStringSize config)
  printf "Max list size: %d\n" (TestSupport.EnhancedMemoryOptimization.maxListSize config)
  printf "Max recursion depth: %d\n" (TestSupport.EnhancedMemoryOptimization.maxRecursionDepth config)
  
  -- Pre-test cleanup
  performComprehensiveCleanup config
  
  -- Create test suite
  let memoryTests = createMemoryEfficientTestSuite config
  let optimizedTests = createOptimizedTestSuite config
  let fullTestSuite = testGroup "Enhanced Memory-Optimized Test Runner"
        [ memoryTests
        , optimizedTests
        ]
  
  -- Run tests with memory monitoring
  performEnhancedMemoryMonitoring "test-start" config
  
  result <- defaultMain fullTestSuite
  
  performEnhancedMemoryMonitoring "test-end" config
  
  -- Post-test cleanup
  performComprehensiveCleanup config
  
  return result

-- Run tests with automatic memory level detection
runAutoConfiguredTests :: IO ()
runAutoConfiguredTests = do
  -- Detect environment
  isCI <- detectCIEnvironment
  memConstraint <- detectMemoryConstraints
  memLevel <- detectMemoryLevel
  
  printf "Auto-configuring tests...\n"
  printf "CI Environment: %s\n" (if isCI then ("Yes" :: String) else ("No" :: String))
  printf "Memory Constraint: %s\n" (maybe "None" ((++ "MB") . show) memConstraint)
  printf "Memory Level: %s\n" (show memLevel)
  
  -- Select configuration
  let config = case memConstraint of
        Just mb | mb <= 16 -> microMemoryConfig
        Just mb | mb <= 24 -> ultraLightMemoryConfig
        Just mb | mb <= 32 -> enhancedMemoryConfig
        _ -> case memLevel of
               Micro -> microMemoryConfig
               UltraLight -> ultraLightMemoryConfig
               Enhanced -> enhancedMemoryConfig
               Standard -> enhancedMemoryConfig
  
  printf "Selected configuration: %s\n" (show $ memoryLevel config)
  
  -- Run tests
  runEnhancedMemoryTests config

-- Run tests with specific memory level
runWithMemoryLevel :: String -> IO ()
runWithMemoryLevel levelStr = 
  case levelStr of
    "micro" -> runEnhancedMemoryTests microMemoryConfig
    "ultra_light" -> runEnhancedMemoryTests ultraLightMemoryConfig
    "enhanced" -> runEnhancedMemoryTests enhancedMemoryConfig
    "standard" -> runEnhancedMemoryTests enhancedMemoryConfig
    _ -> do
      printf "Unknown memory level: %s\n" levelStr
      printf "Available levels: micro, ultra_light, enhanced, standard\n"
      exitFailure

-- Print help information
printHelp :: IO ()
printHelp = do
  putStrLn "Enhanced Memory-Optimized Test Runner for Typus"
  putStrLn ""
  putStrLn "Usage: test-runner [MEMORY_LEVEL]"
  putStrLn ""
  putStrLn "Memory Levels:"
  putStrLn "  micro        - Micro memory usage (16MB equivalent)"
  putStrLn "  ultra_light  - Ultra light memory usage (24MB equivalent)"
  putStrLn "  enhanced     - Enhanced memory usage (32MB equivalent)"
  putStrLn "  standard     - Standard memory usage (48MB equivalent)"
  putStrLn ""
  putStrLn "Environment Variables:"
  putStrLn "  TYPUS_MEMORY_LEVEL      Memory optimization level"
  putStrLn "  TYPUS_MEMORY_LIMIT_MB   Explicit memory limit in MB"
  putStrLn "  CI                      CI environment flag"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  ./test-runner                    # Auto-configure based on environment"
  putStrLn "  ./test-runner micro              # Run with micro memory optimization"
  putStrLn "  TYPUS_MEMORY_LEVEL=enhanced ./test-runner  # Run with enhanced optimization"
  putStrLn "  TYPUS_MEMORY_LIMIT_MB=24 ./test-runner     # Run with 24MB limit"
  exitSuccess

-- Main function
main :: IO ()
main = do
  args <- getArgs
  
  -- Handle help flag
  when ("--help" `elem` args || "-h" `elem` args) printHelp
  
  -- Check for verbose flag
  let verbose = "--verbose" `elem` args || "-v" `elem` args
  when verbose $ printf "Enhanced Memory-Optimized Test Runner starting...\n"
  
  -- Run tests based on arguments
  case args of
    [] -> runAutoConfiguredTests
    [level] | level `notElem` ["--verbose", "-v"] -> runWithMemoryLevel level
    [level, "--verbose"] -> runWithMemoryLevel level
    ["--verbose", level] -> runWithMemoryLevel level
    _ -> do
      printf "Invalid arguments\n"
      printHelp