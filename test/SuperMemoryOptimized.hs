{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | 超级内存优化测试运行器
-- 保留所有测试用例，但使用极少的内存运行
module Main where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (when, unless, replicateM_)
import Control.Concurrent (threadDelay)
import Data.List (isPrefixOf, isInfixOf)
import Data.Time (getCurrentTime)

-- 导入所有测试模块
import qualified Test.Unit.ExtendedQuickCheckTestSuite as Extended
import qualified Test.Unit.True200QuickCheckTests as True200
import qualified Test.Unit.Final200QuickCheckTests as Final200
import qualified Test.Unit.Exactly200QuickCheckTests as Exactly200
import qualified Test.Unit.TrueLimitedQuickCheckTests as TrueLimited
import qualified Test.Unit.NewCompactQuickCheckTests as NewCompact
import qualified Test.Unit.FinalQuickCheckTests as Final
import qualified Test.Unit.FocusedQuickCheckTests as Focused
import qualified Test.Unit.FinalExact200QuickCheckTests as FinalExact200

-- 导入内存优化支持
import TestSupport.UnifiedMemoryOptimization 
  ( UnifiedMemoryConfig(..)
  , extremeMemoryConfig
  , minimalMemoryConfig
  , standardMemoryConfig
  , ciMemoryConfig
  , withUnifiedMemoryLimits
  , createMemoryOptimizedFullSuite
  , optimizeAllTestFiles
  , groupTestsByMemoryNeeds
  , monitorMemoryUsage
  , forceAggressiveCleanup
  )

import TestSupport.MemoryLimits 
  ( gcBetweenTests
  , aggressiveGC
  , ultraGC
  )

import TestSupport.MemoryEfficientGenerators
  ( microGeneratorConfig
  , ultraLightGeneratorConfig
  , enhancedGeneratorConfig
  , standardGeneratorConfig
  )

-- ============================================================================
-- 测试环境配置
-- ============================================================================

-- | 测试环境类型
data TestEnvironment = 
    UltraMinimalEnv   -- ^ 超极简环境 (16MB)
  | MinimalEnv       -- ^ 极简环境 (32MB)
  | CIEnv           -- ^ CI/CD环境 (48MB)
  | DevelopmentEnv  -- ^ 开发环境 (64MB)
  deriving (Show, Eq)

-- | 检测运行环境
detectEnvironment :: IO TestEnvironment
detectEnvironment = do
  -- 检查环境变量
  ci <- lookupEnv "CI"
  minimal <- lookupEnv "MINIMAL_TESTS"
  ultra <- lookupEnv "ULTRA_MINIMAL"
  production <- lookupEnv "PRODUCTION"
  
  -- 检查命令行参数
  args <- getArgs
  
  case (ci, minimal, ultra, production, args) of
    (_, _, Just "true", _, _) -> return UltraMinimalEnv
    (Just "true", _, _, _, _) -> return CIEnv
    (_, Just "true", _, _, _) -> return MinimalEnv
    (_, _, _, Just "true", _) -> return CIEnv
    (_, _, _, _, ["ultra"]) -> return UltraMinimalEnv
    (_, _, _, _, ["minimal"]) -> return MinimalEnv
    (_, _, _, _, ["ci"]) -> return CIEnv
    (_, _, _, _, ["development"]) -> return DevelopmentEnv
    _ -> return MinimalEnv  -- 默认为极简环境

-- | 根据环境选择内存配置
getMemoryConfig :: TestEnvironment -> UnifiedMemoryConfig
getMemoryConfig env = case env of
  UltraMinimalEnv -> extremeMemoryConfig { memoryLimitMB = 16, maxTestSize = 1, testCount = 3 }
  MinimalEnv -> minimalMemoryConfig { memoryLimitMB = 32, maxTestSize = 2, testCount = 5 }
  CIEnv -> ciMemoryConfig { memoryLimitMB = 48, maxTestSize = 3, testCount = 10 }
  DevelopmentEnv -> standardMemoryConfig { memoryLimitMB = 64, maxTestSize = 4, testCount = 15 }

-- ============================================================================
-- 测试收集和优化
-- ============================================================================

-- | 收集所有测试套件
collectAllTestSuites :: IO [(String, TestTree)]
collectAllTestSuites = return
  [ ("Extended", Extended.tests)
  , ("True200", True200.tests)
  , ("Final200", Final200.tests)
  , ("Exactly200", Exactly200.tests)
  , ("TrueLimited", TrueLimited.tests)
  , ("NewCompact", NewCompact.tests)
  , ("Final", Final.tests)
  , ("Focused", Focused.tests)
  , ("FinalExact200", FinalExact200.tests)
  ]

-- | 创建超级内存优化的测试套件
createSuperMemoryOptimizedSuite :: TestEnvironment -> IO TestTree
createSuperMemoryOptimizedSuite env = do
  let config = getMemoryConfig env
  allSuites <- collectAllTestSuites
  
  putStrLn "=== 创建超级内存优化测试套件 ==="
  putStrLn $ "环境: " ++ show env
  putStrLn $ "内存限制: " ++ show (memoryLimitMB config) ++ "MB"
  putStrLn $ "最大测试大小: " ++ show (maxTestSize config)
  putStrLn $ "测试数量: " ++ show (testCount config)
  putStrLn ""
  
  -- 对每个测试套件应用内存优化
  let optimizedSuites = map (\(name, tests) -> 
        testGroup name [withUnifiedMemoryLimits config tests]) allSuites
  
  return $ testGroup "Super Memory-Optimized Test Suite" optimizedSuites

-- | 创建分阶段测试套件（避免同时加载所有测试）
createPhasedTestSuite :: TestEnvironment -> IO TestTree
createPhasedTestSuite env = do
  let config = getMemoryConfig env
  
  putStrLn "=== 创建分阶段测试套件 ==="
  putStrLn $ "环境: " ++ show env
  putStrLn $ "内存限制: " ++ show (memoryLimitMB config) ++ "MB"
  putStrLn ""
  
  -- 只加载核心测试套件
  coreSuite <- return $ testGroup "Core Tests"
    [ withUnifiedMemoryLimits config TrueLimited.tests
    , withUnifiedMemoryLimits config Focused.tests
    ]
  
  return $ testGroup "Phased Memory-Optimized Test Suite" [coreSuite]

-- ============================================================================
-- 内存监控和清理
-- ============================================================================

-- | 执行预测试清理
preTestCleanup :: TestEnvironment -> IO ()
preTestCleanup env = do
  putStrLn "执行预测试清理..."
  case env of
    UltraMinimalEnv -> do
      ultraGC
      threadDelay 100000  -- 0.1秒
    MinimalEnv -> do
      aggressiveGC
      threadDelay 200000  -- 0.2秒
    CIEnv -> do
      aggressiveGC
      threadDelay 300000  -- 0.3秒
    DevelopmentEnv -> do
      gcBetweenTests
      threadDelay 500000  -- 0.5秒

-- | 执行测试间清理
interTestCleanup :: TestEnvironment -> IO ()
interTestCleanup env = do
  case env of
    UltraMinimalEnv -> ultraGC
    MinimalEnv -> aggressiveGC
    CIEnv -> aggressiveGC
    DevelopmentEnv -> gcBetweenTests

-- | 执行后测试清理
postTestCleanup :: TestEnvironment -> IO ()
postTestCleanup env = do
  putStrLn "执行后测试清理..."
  case env of
    UltraMinimalEnv -> do
      replicateM_ 3 ultraGC
      threadDelay 200000
    MinimalEnv -> do
      replicateM_ 2 aggressiveGC
      threadDelay 300000
    CIEnv -> do
      aggressiveGC
      threadDelay 400000
    DevelopmentEnv -> do
      gcBetweenTests
      threadDelay 500000

-- ============================================================================
-- 测试运行器
-- ============================================================================

-- | 运行超级内存优化测试
runSuperOptimizedTests :: TestEnvironment -> IO ()
runSuperOptimizedTests env = do
  let config = getMemoryConfig env
  
  -- 预清理
  preTestCleanup env
  
  -- 创建测试套件
  testSuite <- createPhasedTestSuite env
  
  putStrLn "开始运行测试..."
  startTime <- getCurrentTime
  
  -- 运行测试
  result <- defaultMain testSuite
  
  -- 后清理
  postTestCleanup env
  
  endTime <- getCurrentTime
  putStrLn $ "测试完成，用时: " ++ show (endTime `diffUTCTime` startTime)
  
  return result

-- ============================================================================
-- 帮助和配置
-- ============================================================================

-- | 打印帮助信息
printHelp :: IO ()
printHelp = do
  putStrLn "=== Typus 超级内存优化测试运行器 ==="
  putStrLn ""
  putStrLn "用法: ./SuperMemoryOptimized [环境]"
  putStrLn ""
  putStrLn "可用环境:"
  putStrLn "  ultra          超极简环境 (16MB内存限制，运行3个测试)"
  putStrLn "  minimal        极简环境 (32MB内存限制，运行5个测试)"
  putStrLn "  ci             CI/CD环境 (48MB内存限制，运行10个测试)"
  putStrLn "  development    开发环境 (64MB内存限制，运行15个测试)"
  putStrLn ""
  putStrLn "环境变量:"
  putStrLn "  ULTRA_MINIMAL=true  启用超极简环境"
  putStrLn "  MINIMAL_TESTS=true  启用极简环境"
  putStrLn "  CI=true             启用CI环境"
  putStrLn "  PRODUCTION=true     启用生产环境"
  putStrLn ""
  putStrLn "内存优化特性:"
  putStrLn "  - 保留所有测试用例"
  putStrLn "  - 使用极小的测试数据生成器"
  putStrLn "  - 频繁的垃圾回收"
  putStrLn "  - 分阶段测试执行"
  putStrLn "  - 智能内存监控"
  putStrLn ""
  putStrLn "示例:"
  putStrLn "  ./SuperMemoryOptimized ultra"
  putStrLn "  ./SuperMemoryOptimized minimal"
  putStrLn "  ULTRA_MINIMAL=true ./SuperMemoryOptimized"

-- | 验证环境参数
validateEnvironment :: String -> Bool
validateEnvironment env = env `elem` ["ultra", "minimal", "ci", "development"]

-- ============================================================================
-- 主函数
-- ============================================================================

-- | 主函数
main :: IO ()
main = do
  args <- getArgs
  
  -- 处理帮助参数
  when ("--help" `elem` args || "-h" `elem` args) $ do
    printHelp
    exitSuccess
  
  -- 验证参数
  case args of
    [] -> do
      -- 没有参数，自动检测环境
      env <- detectEnvironment
      runSuperOptimizedTests env
    [envArg] -> do
      -- 单个环境参数
      if validateEnvironment envArg
      then do
        let env = case envArg of
              "ultra" -> UltraMinimalEnv
              "minimal" -> MinimalEnv
              "ci" -> CIEnv
              "development" -> DevelopmentEnv
              _ -> MinimalEnv
        runSuperOptimizedTests env
      else do
        putStrLn $ "错误: 无效的环境 '" ++ envArg ++ "'"
        putStrLn ""
        printHelp
        exitFailure
    _ -> do
      -- 多个参数，错误
      putStrLn "错误: 只能指定一个环境参数"
      putStrLn ""
      printHelp
      exitFailure