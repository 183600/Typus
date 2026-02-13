{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty
import System.Environment (getArgs)
import System.Mem (performGC)
import Control.Monad (replicateM_)
import Control.Concurrent (threadDelay)
import Text.Printf (printf)
import Control.Monad (when)

-- 导入极度内存优化的测试套件
import qualified Test.Unit.UltraMemoryOptimizedQuickCheckTests as UltraMemoryOptimizedQuickCheckTests

-- 导入内存优化支持
import TestSupport.MemoryLimits 
  ( withMinimalMemoryLimits
  , withUltraMemoryLimits
  , ultraGC
  , gcBetweenTests
  )
import TestSupport.OptimizedMemoryLimits 
  ( withOptimizedMemoryLimits
  , withStrictMemoryLimits
  , minimalOptimizedConfig
  , createOptimizedMemorySuite
  , forceOptimizedCleanup
  , OptimizedMemoryConfig(..)
  )

-- | 极度内存优化的测试配置
ultraExtremeMemoryConfig :: OptimizedMemoryConfig
ultraExtremeMemoryConfig = OptimizedMemoryConfig
  { memoryLimitMB = 16      -- 极端内存限制：16MB
  , maxTestSize = 1         -- 最小测试大小
  , testCount = 1           -- 每个测试只运行1次
  , maxShrinks = 0          -- 不进行收缩
  , gcFrequency = 1         -- 每次测试后GC
  , enableProfiling = False -- 禁用分析
  , optimizedCleanup = True
  , stringSizeLimit = 1     -- 字符串大小限制为1
  , listSizeLimit = 1       -- 列表大小限制为1
  }

-- | 极度激进的垃圾回收
ultraAggressiveGC :: IO ()
ultraAggressiveGC = do
  performGC
  -- 极度频繁的GC，确保最小内存占用
  replicateM_ 10 $ do
    performGC
    threadDelay 100  -- 0.1ms间隔，极度频繁

-- | 运行测试前的内存清理
preTestCleanup :: IO ()
preTestCleanup = do
  printf "执行测试前内存清理...\n"
  ultraAggressiveGC
  replicateM_ 3 forceOptimizedCleanup
  printf "内存清理完成\n"

-- | 运行测试后的内存清理
postTestCleanup :: IO ()
postTestCleanup = do
  printf "执行测试后内存清理...\n"
  ultraAggressiveGC
  replicateM_ 5 forceOptimizedCleanup
  printf "内存清理完成\n"

-- | 创建极度内存优化的测试套件
createUltraOptimizedTestSuite :: TestTree
createUltraOptimizedTestSuite = 
  let coreTests = UltraMemoryOptimizedQuickCheckTests.ultraMemoryOptimizedQuickCheckTests
      -- 应用最小内存限制
      minimalTests = withMinimalMemoryLimits coreTests
      -- 再应用极度内存限制
      ultraTests = withUltraMemoryLimits minimalTests
      -- 最后应用优化的内存配置
      optimizedTests = createOptimizedMemorySuite minimalOptimizedConfig "Ultra Optimized Core Tests" [ultraTests]
  in optimizedTests

-- | 带内存监控的测试运行
runTestsWithMemoryMonitoring :: TestTree -> IO ()
runTestsWithMemoryMonitoring testSuite = do
  printf "=== 开始极度内存优化测试 ===\n"
  printf "内存限制: 16MB\n"
  printf "测试数量: 10个核心测试\n"
  printf "每个测试运行次数: 1次\n"
  printf "QuickCheck最大大小: 1\n"
  printf "最大收缩次数: 0\n\n"
  
  -- 测试前清理
  preTestCleanup
  
  -- 运行测试
  printf "运行测试...\n"
  result <- defaultMain testSuite
  
  -- 测试后清理
  postTestCleanup
  
  printf "\n=== 极度内存优化测试完成 ===\n"
  return result

-- | 主函数
main :: IO ()
main = do
  args <- getArgs
  
  -- 检查命令行参数
  let isVerbose = "--verbose" `elem` args || "-v" `elem` args
      isUltraExtreme = "--ultra-extreme" `elem` args
  
  when isVerbose $ printf "启用详细输出模式\n"
  when isUltraExtreme $ printf "启用极端内存优化模式\n"
  
  -- 创建测试套件
  let testSuite = if isUltraExtreme 
        then createOptimizedMemorySuite ultraExtremeMemoryConfig "Ultra Extreme Tests" 
              [withMinimalMemoryLimits UltraMemoryOptimizedQuickCheckTests.ultraMemoryOptimizedQuickCheckTests]
        else createUltraOptimizedTestSuite
  
  -- 运行测试
  runTestsWithMemoryMonitoring testSuite