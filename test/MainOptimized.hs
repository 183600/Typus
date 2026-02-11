{-# LANGUAGE OverloadedStrings #-}

-- | 内存优化的主测试入口
-- 这个模块提供了一个统一的内存优化测试入口，确保所有测试都在内存限制下运行
module Main where

import Test.Tasty
import System.Environment (lookupEnv)
import Control.Monad (when)

-- 导入优化的测试套件
import Test.Unit.ExtendedQuickCheckTestSuiteOptimized (tests, ultraOptimizedTests)
import Test.Unit.UltraMemoryOptimizedTestSuite (tests as ultraMemoryTests)

-- 导入内存优化支持
import TestSupport.UnifiedMemoryOptimization 
  ( UnifiedMemoryConfig(..)
  , extremeMemoryConfig
  , minimalMemoryConfig
  , standardMemoryConfig
  , ciMemoryConfig
  , createOptimizedTestRunner
  , optimizeTestFileStructure
  )

-- 导入其他内存优化支持
import TestSupport.MemoryLimits (withMinimalMemoryLimits, gcBetweenTests)
import TestSupport.OptimizedMemoryLimits (forceOptimizedCleanup)

-- | 检测运行环境
data TestEnvironment = CI | Development | Production | Minimal deriving (Show, Eq)

-- | 检测当前测试环境
detectEnvironment :: IO TestEnvironment
detectEnvironment = do
  ci <- lookupEnv "CI"
  continuous <- lookupEnv "CONTINUOUS_INTEGRATION"
  production <- lookupEnv "PRODUCTION"
  minimal <- lookupEnv "MINIMAL_TESTS"
  
  case (ci, continuous, production, minimal) of
    (Just "true", _, _, _) -> return CI
    (_, Just "true", _, _) -> return CI
    (_, _, Just "true", _) -> return Production
    (_, _, _, Just "true") -> return Minimal
    _ -> return Development

-- | 根据环境选择内存配置
selectMemoryConfig :: TestEnvironment -> UnifiedMemoryConfig
selectMemoryConfig env = case env of
  CI -> ciMemoryConfig
  Development -> standardMemoryConfig
  Production -> minimalMemoryConfig
  Minimal -> extremeMemoryConfig

-- | 根据环境选择测试套件
selectTestSuite :: TestEnvironment -> TestTree
selectTestSuite env = case env of
  Minimal -> ultraOptimizedTests
  CI -> ultraOptimizedTests
  Development -> tests
  Production -> ultraOptimizedTests

-- | 内存优化测试运行器
runOptimizedTests :: TestEnvironment -> IO ()
runOptimizedTests env = do
  let config = selectMemoryConfig env
  let testSuite = selectTestSuite env
  
  putStrLn $ "Running tests in " ++ show env ++ " environment"
  putStrLn $ "Memory limit: " ++ show (memoryLimitMB config) ++ "MB"
  putStrLn $ "Test selection ratio: " ++ show (testSelectionRatio config * 100) ++ "%"
  
  -- 使用优化的测试运行器
  createOptimizedTestRunner config $ do
    -- 强制垃圾回收
    gcBetweenTests
    
    -- 运行测试
    defaultMain testSuite
    
    -- 强制清理
    forceOptimizedCleanup

-- | 主函数
main :: IO ()
main = do
  -- 检测环境
  env <- detectEnvironment
  
  -- 打印配置信息
  putStrLn "=== Typus Memory-Optimized Test Runner ==="
  putStrLn $ "Environment: " ++ show env
  putStrLn $ "Memory Configuration: " ++ show (memoryLimitMB $ selectMemoryConfig env) ++ "MB"
  
  -- 如果是开发环境，显示优化建议
  when (env == Development) $ do
    putStrLn ""
    optimizeTestFileStructure
    putStrLn ""
  
  -- 运行优化测试
  runOptimizedTests env
  
  putStrLn "=== Test run completed ==="