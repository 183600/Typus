module Main where

import Test.Tasty
import Test.Unit.OptimizedTests (ciOptimizedTests, tests)
import TestSupport.OptimizedMemoryLimits (optimizedMemoryConfig, withOptimizedMemory, memoryLimitMB)
import System.Environment (lookupEnv)

-- Check if we're in a CI/CD environment
isCIEnvironment :: IO Bool
isCIEnvironment = do
  ci <- lookupEnv "CI"
  continuous <- lookupEnv "CONTINUOUS_INTEGRATION"
  return $ (ci == Just "true") || (continuous == Just "true")

-- Main function with memory optimization
main :: IO ()
main = do
  -- Check if we're in a CI/CD environment
  isCI <- isCIEnvironment
  
  -- Use optimized memory configuration
  withOptimizedMemory $ \config -> do
    putStrLn $ "Running tests with " ++ show (memoryLimitMB config) ++ "MB memory limit"
    
    -- Choose test suite based on environment
    let testSuite = if isCI 
                    then ciOptimizedTests  -- Use minimal memory for CI/CD
                    else tests              -- Use full optimized suite for development
    
    defaultMain testSuite