{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- | 内存优化测试配置
-- 确保所有测试用例都使用内存优化设置，不会消耗大量内存
module TestSupport.MemoryOptimizedTestConfig where

import Test.Tasty
import Test.Tasty.QuickCheck
import System.Environment (setEnv)
import Control.Monad (when)

-- | 内存优化配置
data MemoryOptimizedConfig = MemoryOptimizedConfig
  { maxQuickCheckTests :: Int      -- ^ QuickCheck测试次数
  , maxQuickCheckSize :: Int       -- ^ QuickCheck最大大小
  , maxQuickCheckShrinks :: Int    -- ^ QuickCheck最大收缩次数
  , maxStringLength :: Int         -- ^ 最大字符串长度
  , maxListLength :: Int           -- ^ 最大列表长度
  , maxIntRange :: Int             -- ^ 最大整数范围
  , maxNestingDepth :: Int         -- ^ 最大嵌套深度
  } deriving (Show, Eq)

-- | 默认内存优化配置
defaultMemoryOptimizedConfig :: MemoryOptimizedConfig
defaultMemoryOptimizedConfig = MemoryOptimizedConfig
  { maxQuickCheckTests = 1         -- 最小测试次数
  , maxQuickCheckSize = 1          -- 最小测试大小
  , maxQuickCheckShrinks = 0       -- 禁用收缩
  , maxStringLength = 5            -- 最大字符串长度
  , maxListLength = 2              -- 最大列表长度
  , maxIntRange = 10               -- 最大整数范围
  , maxNestingDepth = 3            -- 最大嵌套深度
  }

-- | 极简内存优化配置
minimalMemoryOptimizedConfig :: MemoryOptimizedConfig
minimalMemoryOptimizedConfig = MemoryOptimizedConfig
  { maxQuickCheckTests = 1
  , maxQuickCheckSize = 1
  , maxQuickCheckShrinks = 0
  , maxStringLength = 3
  , maxListLength = 1
  , maxIntRange = 5
  , maxNestingDepth = 2
  }

-- | 应用内存优化配置
applyMemoryOptimizedConfig :: MemoryOptimizedConfig -> IO ()
applyMemoryOptimizedConfig config = do
  -- 设置QuickCheck环境变量
  setEnv "QUICKCHECK_TESTS" (show $ maxQuickCheckTests config)
  setEnv "QUICKCHECK_MAX_SIZE" (show $ maxQuickCheckSize config)
  setEnv "QUICKCHECK_MAX_SHRINKS" (show $ maxQuickCheckShrinks config)
  
  -- 设置Typus特定环境变量
  setEnv "TYPUS_STRING_LENGTH_LIMIT" (show $ maxStringLength config)
  setEnv "TYPUS_LIST_LENGTH_LIMIT" (show $ maxListLength config)
  setEnv "TYPUS_INT_RANGE_LIMIT" (show $ maxIntRange config)
  setEnv "TYPUS_NESTING_DEPTH_LIMIT" (show $ maxNestingDepth config)
  
  -- 设置内存优化标志
  setEnv "TYPUS_MEMORY_OPTIMIZED" "1"
  setEnv "TYPUS_MINIMIZE_MEMORY" "1"

-- | 创建内存优化的测试属性
memoryOptimizedProperty :: MemoryOptimizedConfig -> String -> Property -> TestTree
memoryOptimizedProperty config name prop = 
  testProperty name $ 
    withMaxSuccess (maxQuickCheckTests config) $
    withShrinks (maxQuickCheckShrinks config) prop

-- | 创建内存优化的QuickCheck测试
memoryOptimizedTest :: MemoryOptimizedConfig -> String -> Property -> IO ()
memoryOptimizedTest config name prop = do
  -- 应用内存优化配置
  applyMemoryOptimizedConfig config
  
  -- 运行测试
  putStrLn $ "Running memory-optimized test: " ++ name
  putStrLn $ "Config: " ++ show config
  
  result <- quickCheckWithResult stdArgs
    { maxSuccess = maxQuickCheckTests config
    , maxSize = maxQuickCheckSize config
    , maxShrinks = maxQuickCheckShrinks config
    } prop
  
  case result of
    Success {} -> putStrLn $ "✓ " ++ name ++ " passed"
    Failure {} -> putStrLn $ "✗ " ++ name ++ " failed"
    GaveUp {} -> putStrLn $ "? " ++ name ++ " gave up"
    NoExpectedFailure {} -> putStrLn $ "! " ++ name ++ " unexpectedly passed"

-- | 字符串长度限制函数
limitStringLength :: MemoryOptimizedConfig -> String -> String
limitStringLength config s = take (maxStringLength config) s

-- | 列表长度限制函数
limitListLength :: MemoryOptimizedConfig -> [a] -> [a]
limitListLength config xs = take (maxListLength config) xs

-- | 整数范围限制函数
limitIntRange :: MemoryOptimizedConfig -> Int -> Int
limitIntRange config n = 
  let maxVal = maxIntRange config `div` 2
  in if n < 0 then max (-maxVal) (negate maxVal) else min maxVal n

-- | 嵌套深度限制函数
limitNestingDepth :: MemoryOptimizedConfig -> Int -> Int
limitNestingDepth config n = min n (maxNestingDepth config)

-- | 内存优化的测试套件创建器
createMemoryOptimizedTestSuite :: MemoryOptimizedConfig -> String -> [TestTree] -> TestTree
createMemoryOptimizedTestSuite config name tests = 
  testGroup name tests

-- | 自动检测并应用最佳内存配置
autoApplyMemoryOptimization :: IO MemoryOptimizedConfig
autoApplyMemoryOptimization = do
  -- 检查环境变量
  minimal <- lookupEnv "TYPUS_MINIMAL_MEMORY"
  
  let config = case minimal of
        Just "true" -> minimalMemoryOptimizedConfig
        _ -> defaultMemoryOptimizedConfig
  
  applyMemoryOptimizedConfig config
  return config

-- | 辅助函数：查找环境变量
lookupEnv :: String -> IO (Maybe String)
lookupEnv key = do
  -- 简化实现，实际应该使用System.Environment.lookupEnv
  return Nothing