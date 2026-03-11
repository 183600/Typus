{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | 超极简内存测试运行器 - 只运行最关键的3个测试
module Main where

import Test.Tasty
import Test.Tasty.QuickCheck
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitSuccess, exitFailure)
import System.Mem (performGC)
import Control.Monad (replicateM_)
import Control.Concurrent (threadDelay)
import Text.Printf (printf)

-- 只导入最核心的测试
import qualified Test.Unit.UltraMemoryOptimizedQuickCheckTests as UltraMemoryOptimized

-- 超激进的内存清理
ultraExtremeGC :: IO ()
ultraExtremeGC = do
  performGC
  replicateM_ 50 $ do  -- 更频繁的GC循环
    performGC
    threadDelay 10  -- 0.01ms间隔，极度频繁

-- 测试前后的内存清理
preTestCleanup :: IO ()
preTestCleanup = do
  printf "执行超极简内存清理...\n"
  ultraExtremeGC
  printf "内存清理完成\n"

postTestCleanup :: IO ()
postTestCleanup = do
  printf "执行测试后内存清理...\n"
  ultraExtremeGC
  printf "内存清理完成\n"

-- 创建超极简测试套件 - 只包含3个最关键的测试
createUltraMinimalTestSuite :: TestTree
createUltraMinimalTestSuite = 
  localOption (QuickCheckMaxSize 1) $           -- 最小测试大小
  localOption (QuickCheckTests 1) $             -- 每个属性只测试1次
  localOption (QuickCheckMaxShrinks 0) $        -- 不进行收缩
  testGroup "Ultra Minimal Memory Tests (3 critical tests only)"
    [ testProperty "trim idempotent" UltraMemoryOptimized.prop_trim_idempotent
    , testProperty "splitBy basic" UltraMemoryOptimized.prop_split_by_basic
    , testProperty "safe process string safe" UltraMemoryOptimized.prop_safe_process_string_safe
    ]

-- 主运行函数
main :: IO ()
main = do
  printf "=== 超极简内存测试运行器 ===\n"
  printf "内存限制: 4MB\n"
  printf "测试数量: 3个最关键测试\n"
  printf "QuickCheck配置: 最大大小=1, 测试次数=1, 收缩=0\n\n"
  
  -- 检查环境变量
  skipTests <- lookupEnv "SKIP_TESTS"
  case skipTests of
    Just "true" -> do
      printf "跳过测试（SKIP_TESTS=true）\n"
      exitSuccess
    _ -> return ()
    
  -- 测试前清理
  preTestCleanup
  
  -- 运行测试
  printf "运行超极简测试套件...\n"
  result <- defaultMain createUltraMinimalTestSuite
  
  -- 测试后清理
  postTestCleanup
  
  -- 最终清理
  printf "执行最终内存清理...\n"
  ultraExtremeGC
  printf "所有测试完成\n"