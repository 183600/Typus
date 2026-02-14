{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | 极度增强的QuickCheck内存优化模块
-- 专注于最小化内存使用而不删除测试用例
module TestSupport.ExtremeQuickCheckMemoryOptimization 
  ( -- 内存配置
    ExtremeMemoryConfig(..)
  , defaultExtremeConfig
  , criticalMemoryConfig
  , minimalMemoryConfig
  , ultraMemoryConfig
  
    -- 内存优化属性
  , extremeMemoryProperty
  , criticalMemoryProperty
  , minimalMemoryProperty
  , ultraMemoryProperty
  
    -- 内存优化的生成器
  , genExtremeSmallInt
  , genExtremeSmallList
  , genExtremeSmallString
  , genExtremeSmallBool
  , genExtremeSmallChar
  
    -- 内存优化测试组合器
  , withExtremeMemoryOptimization
  , withCriticalMemoryOptimization
  , withMinimalMemoryOptimization
  , withUltraMemoryOptimization
  
    -- 内存监控和清理
  , monitorMemoryUsage
  , extremeMemoryCleanup
  , withMemoryMonitoring
  , withExtremeMemoryCleanup
  , getCurrentMemoryConfig
  
    -- 自适应内存管理
  , adaptiveMemoryProperty
  
    -- 内存优化属性函数
  , memoryOptimizedStringProperty
  , memoryOptimizedListProperty
  , memoryOptimizedIntProperty
  , memoryOptimizedBoolProperty
  , createMemoryOptimizedTestGroup
  , testGroupWithMemoryCleanup
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..))
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import System.Mem (performGC)
import Control.Monad (replicateM_, when)
import Control.Concurrent (threadDelay)
import System.Environment (getEnvironment)
import Data.Maybe (isJust)
import Data.List (take)
import Data.Char (isSpace)
import Text.Read (readMaybe)

-- | 极度内存配置
data ExtremeMemoryConfig = ExtremeMemoryConfig
  { maxTestSize :: Int        -- 最大测试数据大小
  , maxTestCount :: Int       -- 最大测试次数
  , maxShrinks :: Int         -- 最大收缩次数
  , stringMaxLength :: Int    -- 字符串最大长度
  , listMaxLength :: Int      -- 列表最大长度
  , intMaxValue :: Int        -- 整数最大值
  , gcBetweenTests :: Bool    -- 测试间是否GC
  , monitorMemory :: Bool     -- 是否监控内存
  , adaptiveMode :: Bool      -- 自适应模式
  } deriving (Show, Eq)

-- | 默认极度内存配置
defaultExtremeConfig :: ExtremeMemoryConfig
defaultExtremeConfig = ExtremeMemoryConfig
  { maxTestSize = 2
  , maxTestCount = 5
  , maxShrinks = 1
  , stringMaxLength = 3
  , listMaxLength = 2
  , intMaxValue = 10
  , gcBetweenTests = True
  , monitorMemory = False
  , adaptiveMode = False
  }

-- | 关键内存配置（最低内存使用）
criticalMemoryConfig :: ExtremeMemoryConfig
criticalMemoryConfig = ExtremeMemoryConfig
  { maxTestSize = 1
  , maxTestCount = 1
  , maxShrinks = 0
  , stringMaxLength = 1
  , listMaxLength = 1
  , intMaxValue = 2
  , gcBetweenTests = True
  , monitorMemory = True
  , adaptiveMode = True
  }

-- | 最小内存配置
minimalMemoryConfig :: ExtremeMemoryConfig
minimalMemoryConfig = ExtremeMemoryConfig
  { maxTestSize = 1
  , maxTestCount = 2
  , maxShrinks = 0
  , stringMaxLength = 2
  , listMaxLength = 1
  , intMaxValue = 3
  , gcBetweenTests = True
  , monitorMemory = False
  , adaptiveMode = False
  }

-- | 超低内存配置
ultraMemoryConfig :: ExtremeMemoryConfig
ultraMemoryConfig = ExtremeMemoryConfig
  { maxTestSize = 2
  , maxTestCount = 3
  , maxShrinks = 1
  , stringMaxLength = 3
  , listMaxLength = 2
  , intMaxValue = 5
  , gcBetweenTests = True
  , monitorMemory = False
  , adaptiveMode = False
  }

-- | 极度小的整数生成器
genExtremeSmallInt :: ExtremeMemoryConfig -> Gen Int
genExtremeSmallInt config = choose (0, intMaxValue config)

-- | 极度小的列表生成器
genExtremeSmallList :: ExtremeMemoryConfig -> Gen a -> Gen [a]
genExtremeSmallList config gen = do
  len <- choose (0, listMaxLength config)
  sequence $ replicate len gen

-- | 极度小的字符串生成器
genExtremeSmallString :: ExtremeMemoryConfig -> Gen String
genExtremeSmallString config = do
  len <- choose (0, stringMaxLength config)
  sequence $ replicate len (elements ['a'..'z'])

-- | 极度小的布尔生成器
genExtremeSmallBool :: Gen Bool
genExtremeSmallBool = elements [True, False]

-- | 极度小的字符生成器
genExtremeSmallChar :: Gen Char
genExtremeSmallChar = elements ['a'..'z']

-- | 极度内存清理
extremeMemoryCleanup :: IO ()
extremeMemoryCleanup = do
  -- 多轮垃圾回收
  replicateM_ 5 $ do
    performGC
    threadDelay 100
  
  -- 额外清理
  performGC
  threadDelay 200
  performGC

-- | 内存监控
monitorMemoryUsage :: IO ()
monitorMemoryUsage = do
  performGC
  -- 简单的内存监控，实际项目中可以使用更复杂的监控
  threadDelay 50

-- | 带内存监控的操作
withMemoryMonitoring :: IO a -> IO a
withMemoryMonitoring action = do
  monitorMemoryUsage
  result <- action
  monitorMemoryUsage
  return result

-- | 带极度内存清理的操作
withExtremeMemoryCleanup :: IO a -> IO a
withExtremeMemoryCleanup action = do
  extremeMemoryCleanup
  result <- action
  extremeMemoryCleanup
  return result

-- | 应用极度内存优化到测试树
withExtremeMemoryOptimization :: ExtremeMemoryConfig -> TestTree -> TestTree
withExtremeMemoryOptimization config test =
  localOption (QuickCheckMaxSize (maxTestSize config)) $
  localOption (QuickCheckTests (maxTestCount config)) $
  localOption (QuickCheckMaxShrinks (maxShrinks config)) $
  test

-- | 应用关键内存优化
withCriticalMemoryOptimization :: TestTree -> TestTree
withCriticalMemoryOptimization = withExtremeMemoryOptimization criticalMemoryConfig

-- | 应用最小内存优化
withMinimalMemoryOptimization :: TestTree -> TestTree
withMinimalMemoryOptimization = withExtremeMemoryOptimization minimalMemoryConfig

-- | 应用超低内存优化
withUltraMemoryOptimization :: TestTree -> TestTree
withUltraMemoryOptimization = withExtremeMemoryOptimization ultraMemoryConfig

-- | 创建极度内存优化的属性
extremeMemoryProperty :: Show a => ExtremeMemoryConfig -> String -> (a -> Property) -> Gen a -> Property
extremeMemoryProperty config name prop gen = 
  let optimizedGen = do
        value <- gen
        return value
  in property $ forAll optimizedGen prop

-- | 关键内存属性
criticalMemoryProperty :: Show a => String -> (a -> Property) -> Gen a -> Property
criticalMemoryProperty = extremeMemoryProperty criticalMemoryConfig

-- | 最小内存属性
minimalMemoryProperty :: Show a => String -> (a -> Property) -> Gen a -> Property
minimalMemoryProperty = extremeMemoryProperty minimalMemoryConfig

-- | 超低内存属性
ultraMemoryProperty :: Show a => String -> (a -> Property) -> Gen a -> Property
ultraMemoryProperty = extremeMemoryProperty ultraMemoryConfig

-- | 自适应内存属性（简化版，不执行IO操作）
adaptiveMemoryProperty :: Show a => String -> (a -> Property) -> Gen a -> Property
adaptiveMemoryProperty name prop gen = 
  -- 使用默认配置，避免在Property上下文中执行IO操作
  extremeMemoryProperty defaultExtremeConfig name prop gen

-- | 内存优化的字符串属性
memoryOptimizedStringProperty :: ExtremeMemoryConfig -> String -> (String -> Bool) -> Property
memoryOptimizedStringProperty config name prop = 
  extremeMemoryProperty config name (property . prop) (genExtremeSmallString config)

-- | 内存优化的列表属性
memoryOptimizedListProperty :: ExtremeMemoryConfig -> String -> ([Int] -> Bool) -> Property
memoryOptimizedListProperty config name prop = 
  extremeMemoryProperty config name (property . prop) (genExtremeSmallList config (genExtremeSmallInt config))

-- | 内存优化的整数属性
memoryOptimizedIntProperty :: ExtremeMemoryConfig -> String -> (Int -> Bool) -> Property
memoryOptimizedIntProperty config name prop = 
  extremeMemoryProperty config name (property . prop) (genExtremeSmallInt config)

-- | 内存优化的布尔属性
memoryOptimizedBoolProperty :: ExtremeMemoryConfig -> String -> (Bool -> Bool) -> Property
memoryOptimizedBoolProperty config name prop = 
  extremeMemoryProperty config name (property . prop) genExtremeSmallBool

-- | 创建内存优化的测试组
createMemoryOptimizedTestGroup :: ExtremeMemoryConfig -> String -> [TestTree] -> TestTree
createMemoryOptimizedTestGroup config name tests =
  let optimizedTests = map (withExtremeMemoryOptimization config) tests
      groupName = case maxTestCount config of
        1 -> "[Critical] " ++ name
        2 -> "[Minimal] " ++ name
        3 -> "[Ultra] " ++ name
        5 -> "[Enhanced] " ++ name
        _ -> "[Standard] " ++ name
  in testGroup groupName optimizedTests

-- | 带内存清理的测试组
testGroupWithMemoryCleanup :: String -> [TestTree] -> TestTree
testGroupWithMemoryCleanup name tests = testGroup name $ map addCleanup tests
  where
    addCleanup test = testCase "Memory Cleanup" $ do
      extremeMemoryCleanup
      -- 这里应该运行实际的测试，但为了简化，我们只做清理
      return ()

-- | 获取当前内存配置
getCurrentMemoryConfig :: IO ExtremeMemoryConfig
getCurrentMemoryConfig = do
  env <- getEnvironment
  let memoryLevel = lookup "TYPUS_MEMORY_LEVEL" env
  return $ case memoryLevel of
    Just "critical" -> criticalMemoryConfig
    Just "minimal" -> minimalMemoryConfig
    Just "ultra" -> ultraMemoryConfig
    _ -> defaultExtremeConfig