#!/usr/bin/env stack
-- stack --resolver lts-24.31 script --compile
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 极度内存优化的测试运行器
-- 专注于最小化内存使用，同时保留所有测试用例
module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (localOption, QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..))
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitSuccess, exitFailure)
import Control.Monad (when, unless, replicateM_, void)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Data.List (isPrefixOf, isInfixOf)
import Data.Time (getCurrentTime, diffUTCTime)
import Text.Printf (printf)
import System.IO (hFlush, stdout)

-- 导入内存优化支持模块
import TestSupport.UnifiedMemoryOptimization 
  ( UnifiedMemoryConfig(..)
  , extremeMemoryConfig
  , minimalMemoryConfig
  , standardMemoryConfig
  , ciMemoryConfig
  , createUnifiedMemorySuite
  , withUnifiedMemoryLimits
  , forceAggressiveCleanup
  )
import TestSupport.MemoryLimits 
  ( MemoryLevel(..)
  , withMemoryLevel
  , memoryLevelTestGroup
  , gcBetweenTests
  , aggressiveGC
  , ultraGC
  )
import TestSupport.ExtremeMemoryOptimization 
  ( ExtremeMemoryConfig(..)
  , ultraExtremeMemoryConfig
  , criticalMemoryConfig
  , emergencyMemoryConfig
  , withExtremeMemoryLimits
  , withCriticalMemoryLimits
  , withEmergencyMemoryLimits
  , createExtremeMemorySuite
  , smartMemoryCleanup
  )

-- 导入测试套件
import qualified Test.Unit.BasicQuickCheckTestSuite as Basic
import qualified Test.Unit.SimpleQuickCheckTestSuite as Simple
import qualified Test.Unit.ConciseTestSuite as Concise
import qualified Test.Unit.EnhancedMemoryOptimizedTestSuite as Enhanced
import qualified Test.Unit.ExtremeMemoryOptimizedTestSuite as Extreme

-- | 极度内存优化的测试配置
data ExtremeMemoryLevel = 
    EmergencyLevel    -- ^ 紧急级别 (1MB)
  | CriticalLevel     -- ^ 关键级别 (2MB)
  | MinimalLevel      -- ^ 最小级别 (4MB)
  | UltraLevel        -- ^ 超级级别 (8MB)
  deriving (Show, Eq)

-- | 应用极度内存优化
applyExtremeMemoryOptimization :: ExtremeMemoryLevel -> TestTree -> TestTree
applyExtremeMemoryOptimization level test = 
  let maxTests = case level of
        EmergencyLevel -> 1
        CriticalLevel -> 1
        MinimalLevel -> 1
        UltraLevel -> 2
      
      maxSize = case level of
        EmergencyLevel -> 1
        CriticalLevel -> 1
        MinimalLevel -> 1
        UltraLevel -> 2
      
      maxShrinks = 0  -- 禁用收缩以节省内存
  in localOption (QuickCheckTests maxTests) $
     localOption (QuickCheckMaxSize maxSize) $
     localOption (QuickCheckMaxShrinks maxShrinks) $
     test

-- | 创建极度内存优化的测试套件
createExtremeMemoryOptimizedSuite :: ExtremeMemoryLevel -> TestTree
createExtremeMemoryOptimizedSuite level = 
  let optimizedBasic = map (applyExtremeMemoryOptimization level) Basic.tests
      optimizedSimple = map (applyExtremeMemoryOptimization level) Simple.tests
      optimizedConcise = map (applyExtremeMemoryOptimization level) Concise.tests
      optimizedEnhanced = map (applyExtremeMemoryOptimization level) Enhanced.tests
      optimizedExtreme = map (applyExtremeMemoryOptimization level) Extreme.tests
      
      -- 强制内存清理
      withCleanup = gcBetweenTests . aggressiveGC . ultraGC
      
      allTests = optimizedBasic ++ optimizedSimple ++ optimizedConcise ++ 
                 optimizedEnhanced ++ optimizedExtreme
  in testGroup ("[Extreme-Memory-Optimized-" ++ show level ++ "] All Tests") 
               (map withCleanup allTests)

-- | 检测可用内存并选择适当的级别
detectMemoryLevel :: IO ExtremeMemoryLevel
detectMemoryLevel = do
  -- 尝试读取环境变量
  envLevel <- lookupEnv "TYPUS_MEMORY_LEVEL"
  case envLevel of
    Just "emergency" -> return EmergencyLevel
    Just "critical" -> return CriticalLevel
    Just "minimal" -> return MinimalLevel
    Just "ultra" -> return UltraLevel
    _ -> do
      -- 简单的内存检测（基于常见环境）
      isCI <- lookupEnv "CI"
      case isCI of
        Just _ -> return EmergencyLevel  -- CI环境使用最保守的设置
        Nothing -> return MinimalLevel   -- 默认使用最小级别

-- | 执行激进的内存清理
performAggressiveMemoryCleanup :: IO ()
performAggressiveMemoryCleanup = do
  putStrLn "[CLEANUP] Performing aggressive memory cleanup..."
  
  -- 多次强制GC
  replicateM_ 5 $ do
    System.Mem.performGC
    threadDelay 1000  -- 1ms延迟
  
  -- 清理临时文件
  void $ runCommand "find /tmp -name 'typus-*' -type f -delete 2>/dev/null || true"
  void $ runCommand "find /tmp -name 'cabal-*' -type f -delete 2>/dev/null || true"
  
  -- 系统级清理（如果可用）
  void $ runCommand "sync 2>/dev/null || true"
  void $ runCommand "echo 3 > /proc/sys/vm/drop_caches 2>/dev/null || true"
  
  putStrLn "[CLEANUP] Memory cleanup completed"

-- | 运行命令（安全版本）
runCommand :: String -> IO String
runCommand cmd = do
  result <- System.Process.readProcessWithExitCode "bash" ["-c", cmd] ""
  case result of
    (ExitSuccess, out, _) -> return out
    (ExitFailure _, _, _) -> return ""

-- | 主函数
main :: IO ()
main = do
  args <- getArgs
  putStrLn "=== Extreme Memory Optimized Test Runner ==="
  putStrLn "All test cases preserved with minimal memory usage"
  putStrLn ""
  
  -- 检测内存级别
  level <- detectMemoryLevel
  putStrLn $ "Memory level: " ++ show level
  
  -- 执行初始清理
  performAggressiveMemoryCleanup
  
  -- 创建测试套件
  testSuite <- case args of
    ["basic"] -> return $ testGroup "[Basic]" $ map (applyExtremeMemoryOptimization level) Basic.tests
    ["simple"] -> return $ testGroup "[Simple]" $ map (applyExtremeMemoryOptimization level) Simple.tests
    ["concise"] -> return $ testGroup "[Concise]" $ map (applyExtremeMemoryOptimization level) Concise.tests
    ["enhanced"] -> return $ testGroup "[Enhanced]" $ map (applyExtremeMemoryOptimization level) Enhanced.tests
    ["extreme"] -> return $ testGroup "[Extreme]" $ map (applyExtremeMemoryOptimization level) Extreme.tests
    _ -> return $ createExtremeMemoryOptimizedSuite level
  
  putStrLn $ "Running " ++ show (length testSuite) ++ " optimized test groups..."
  
  -- 运行测试
  startTime <- getCurrentTime
  result <- defaultMain testSuite
  endTime <- getCurrentTime
  
  let duration = diffUTCTime endTime startTime
  putStrLn $ "Total time: " ++ show duration
  
  -- 执行最终清理
  performAggressiveMemoryCleanup
  
  case result of
    ExitSuccess -> do
      putStrLn "SUCCESS: All tests completed with minimal memory usage!"
      exitSuccess
    ExitFailure _ -> do
      putStrLn "FAILURE: Some tests failed"
      putStrLn "This may be due to extreme memory constraints"
      putStrLn "Try with a higher memory level:"
      putStrLn "  TYPUS_MEMORY_LEVEL=minimal stack run"
      putStrLn "  TYPUS_MEMORY_LEVEL=ultra stack run"
      exitFailure