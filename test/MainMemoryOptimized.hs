{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | 内存优化主测试入口
-- 这个模块提供了一个专门用于运行内存优化测试的主入口
module Main where

import Test.Tasty
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (when, unless)
import Data.List (isPrefixOf)

-- 导入综合内存优化测试套件
import Test.Unit.ComprehensiveMemoryOptimizedTestSuite 
  ( selectTestSuite
  , availableTestSuites
  , printMemoryOptimizationReport
  , tests
  )

-- 导入内存优化支持
import TestSupport.UnifiedMemoryOptimization 
  ( UnifiedMemoryConfig(..)
  , extremeMemoryConfig
  , minimalMemoryConfig
  , standardMemoryConfig
  , ciMemoryConfig
  , memoryLimitMB
  , testSelectionRatio
  )

import TestSupport.MemoryLimits 
  ( gcBetweenTests
  , aggressiveGC
  , ultraGC
  )

-- ============================================================================
-- 环境检测和配置
-- ============================================================================

-- | 测试环境类型
data TestEnvironment = 
    MinimalEnv      -- ^ 极简环境 (32MB)
  | CIEnv          -- ^ CI/CD环境 (96MB)
  | DevelopmentEnv -- ^ 开发环境 (128MB)
  | ComprehensiveEnv -- ^ 综合环境 (128MB)
  deriving (Show, Eq)

-- | 检测运行环境
detectEnvironment :: IO TestEnvironment
detectEnvironment = do
  -- 检查环境变量
  ci <- lookupEnv "CI"
  minimal <- lookupEnv "MINIMAL_TESTS"
  production <- lookupEnv "PRODUCTION"
  
  -- 检查命令行参数
  args <- getArgs
  
  case (ci, minimal, production, args) of
    (Just "true", _, _, _) -> return CIEnv
    (_, Just "true", _, _) -> return MinimalEnv
    (_, _, Just "true", _) -> return CIEnv
    (_, _, _, ["minimal"]) -> return MinimalEnv
    (_, _, _, ["ci"]) -> return CIEnv
    (_, _, _, ["development"]) -> return DevelopmentEnv
    (_, _, _, ["comprehensive"]) -> return ComprehensiveEnv
    _ -> return DevelopmentEnv  -- 默认为开发环境

-- | 根据环境选择内存配置
getMemoryConfig :: TestEnvironment -> UnifiedMemoryConfig
getMemoryConfig env = case env of
  MinimalEnv -> extremeMemoryConfig
  CIEnv -> ciMemoryConfig
  DevelopmentEnv -> standardMemoryConfig
  ComprehensiveEnv -> standardMemoryConfig

-- | 根据环境选择测试套件名称
getTestSuiteName :: TestEnvironment -> String
getTestSuiteName env = case env of
  MinimalEnv -> "minimal"
  CIEnv -> "ci"
  DevelopmentEnv -> "development"
  ComprehensiveEnv -> "comprehensive"

-- ============================================================================
-- 内存优化测试运行器
-- ============================================================================

-- | 运行内存优化测试
runOptimizedTests :: TestEnvironment -> IO ()
runOptimizedTests env = do
  let config = getMemoryConfig env
  let suiteName = getTestSuiteName env
  let testSuite = selectTestSuite suiteName
  
  -- 打印配置信息
  putStrLn "=== Typus 内存优化测试运行器 ==="
  putStrLn $ "运行环境: " ++ show env
  putStrLn $ "内存限制: " ++ show (memoryLimitMB config) ++ "MB"
  putStrLn $ "测试选择比例: " ++ show (testSelectionRatio config * 100) ++ "%"
  putStrLn $ "测试套件: " ++ suiteName
  putStrLn ""
  
  -- 执行垃圾回收
  putStrLn "执行初始垃圾回收..."
  case env of
    MinimalEnv -> ultraGC
    CIEnv -> aggressiveGC
    _ -> gcBetweenTests
  
  putStrLn "开始运行测试..."
  putStrLn ""
  
  -- 运行测试
  result <- defaultMain testSuite
  
  -- 执行清理
  putStrLn ""
  putStrLn "执行清理垃圾回收..."
  case env of
    MinimalEnv -> ultraGC
    CIEnv -> aggressiveGC
    _ -> gcBetweenTests
  
  return result

-- ============================================================================
-- 帮助和使用说明
-- ============================================================================

-- | 打印帮助信息
printHelp :: IO ()
printHelp = do
  putStrLn "=== Typus 内存优化测试运行器 ==="
  putStrLn ""
  putStrLn "用法: ./MainOptimized [环境]"
  putStrLn ""
  putStrLn "可用环境:"
  putStrLn "  minimal        极简环境 (32MB内存限制，运行5%的测试)"
  putStrLn "  ci             CI/CD环境 (96MB内存限制，运行15%的测试)"
  putStrLn "  development    开发环境 (128MB内存限制，运行20%的测试)"
  putStrLn "  comprehensive  综合环境 (128MB内存限制，运行20%的测试)"
  putStrLn ""
  putStrLn "环境变量:"
  putStrLn "  CI=true        启用CI环境"
  putStrLn "  MINIMAL_TESTS=true  启用极简测试"
  putStrLn "  PRODUCTION=true     启用生产环境"
  putStrLn ""
  putStrLn "示例:"
  putStrLn "  ./MainOptimized minimal"
  putStrLn "  ./MainOptimized ci"
  putStrLn "  CI=true ./MainOptimized"
  putStrLn ""
  putStrLn "内存优化报告:"
  printMemoryOptimizationReport

-- | 验证环境参数
validateEnvironment :: String -> Bool
validateEnvironment env = env `elem` ["minimal", "ci", "development", "comprehensive"]

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
  
  -- 处理报告参数
  when ("--report" `elem` args) $ do
    printMemoryOptimizationReport
    exitSuccess
  
  -- 验证参数
  case args of
    [] -> do
      -- 没有参数，自动检测环境
      env <- detectEnvironment
      runOptimizedTests env
    [envArg] -> do
      -- 单个环境参数
      if validateEnvironment envArg
      then do
        let env = case envArg of
              "minimal" -> MinimalEnv
              "ci" -> CIEnv
              "development" -> DevelopmentEnv
              "comprehensive" -> ComprehensiveEnv
              _ -> DevelopmentEnv
        runOptimizedTests env
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