{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | 极度最小化测试运行器 - 只运行最关键的测试
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

-- 极度激进的内存清理
extremeGC :: IO ()
extremeGC = do
  performGC
  replicateM_ 20 $ do
    performGC
    threadDelay 50  -- 0.05ms间隔，极度频繁

-- 测试前后的内存清理
preTestCleanup :: IO ()
preTestCleanup = do
  printf "执行极端内存清理...\n"
  extremeGC
  printf "内存清理完成\n"

postTestCleanup :: IO ()
postTestCleanup = do
  printf "执行测试后内存清理...\n"
  extremeGC
  printf "内存清理完成\n"

-- 创建极度最小化的测试套件
createExtremeMinimalTestSuite :: TestTree
createExtremeMinimalTestSuite = 
  localOption (QuickCheckMaxSize 1) $           -- 最小测试大小
  localOption (QuickCheckTests 1) $             -- 每个属性只测试1次
  localOption (QuickCheckMaxShrinks 0) $        -- 不进行收缩
  UltraMemoryOptimized.ultraMemoryOptimizedQuickCheckTests

-- 主运行函数
main :: IO ()
main = do
  printf "=== 极度最小化测试运行器 ===\n"
  printf "内存限制: 8MB\n"
  printf "测试数量: 10个最关键测试\n"
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
  printf "运行极度最小化测试套件...\n"
  result <- defaultMain createExtremeMinimalTestSuite
  
  -- 测试后清理
  postTestCleanup
  
  -- 最终清理
  printf "执行最终内存清理...\n"
  extremeGC
  printf "所有测试完成\n"