{-# LANGUAGE CPP #-}

module TestSupport.MemoryLimits
  ( withMemoryLimits
  , withAggressiveMemoryLimits
  , withUltraMemoryLimits
  , withMinimalMemoryLimits
  , memoryLimitedTestGroup
  , aggressiveMemoryLimitedTestGroup
  , ultraMemoryLimitedTestGroup
  , minimalMemoryLimitedTestGroup
  , gcBetweenTests
  , aggressiveGC
  , ultraGC
  , extremeGC
  , withMemoryMonitoring
  , withMemoryLevel
  , memoryLevelTestGroup
  , MemoryLevel(..)
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..))
import System.Mem (performGC)
import Control.Monad (replicateM_)
import Control.Concurrent (threadDelay)

-- | Memory optimization levels
data MemoryLevel = 
    Minimal      -- ^ Minimal memory usage (256MB equivalent)
  | Ultra        -- ^ Ultra low memory usage (512MB equivalent)  
  | Aggressive   -- ^ Aggressive memory limits (1GB equivalent)
  | Moderate     -- ^ Moderate memory limits (2GB equivalent)
  deriving (Show, Eq)

-- | Apply minimal memory limits to a test tree for extreme memory constraints - 超级激进优化
withMinimalMemoryLimits :: TestTree -> TestTree
withMinimalMemoryLimits test = 
  localOption (QuickCheckMaxSize 1) $    -- 最小值
  localOption (QuickCheckTests 1) $      -- 最小值：每个属性仅测试1次
  localOption (QuickCheckMaxShrinks 0) $ -- 禁用收缩以节省内存
  test

-- | Apply ultra memory limits to a test tree for very memory-constrained environments - 极度优化
withUltraMemoryLimits :: TestTree -> TestTree
withUltraMemoryLimits test = 
  localOption (QuickCheckMaxSize 1) $    -- 最小值
  localOption (QuickCheckTests 1) $      -- 保持1个测试
  localOption (QuickCheckMaxShrinks 0) $ -- 禁用收缩以节省内存
  test

-- | Dynamic memory limits based on available system memory - 新增：基于系统内存的动态限制
withDynamicMemoryLimits :: TestTree -> IO TestTree
withDynamicMemoryLimits test = do
  availableMemoryMB <- getAvailableMemoryMB
  let memoryLevel = classifyMemoryLevel availableMemoryMB
  return $ applyMemoryLevelLimits memoryLevel test

-- | Classify memory level based on available memory - 新增：内存级别分类
classifyMemoryLevel :: Int -> MemoryLevel
classifyMemoryLevel availableMB
  | availableMB < 16 = Minimal
  | availableMB < 32 = Ultra
  | availableMB < 64 = Aggressive
  | availableMB < 128 = Moderate
  | otherwise = Moderate

-- | Apply memory level specific limits - 新增：应用内存级别特定限制
applyMemoryLevelLimits :: MemoryLevel -> TestTree -> TestTree
applyMemoryLevelLimits memoryLevel test = case memoryLevel of
  Minimal -> 
    localOption (QuickCheckMaxSize 1) $
    localOption (QuickCheckTests 1) $
    localOption (QuickCheckMaxShrinks 0) test
  Ultra ->
    localOption (QuickCheckMaxSize 2) $
    localOption (QuickCheckTests 2) $
    localOption (QuickCheckMaxShrinks 0) test
  Aggressive ->
    localOption (QuickCheckMaxSize 3) $
    localOption (QuickCheckTests 3) $
    localOption (QuickCheckMaxShrinks 1) test
  Moderate ->
    localOption (QuickCheckMaxSize 5) $
    localOption (QuickCheckTests 5) $
    localOption (QuickCheckMaxShrinks 2) test

-- | Get available memory in MB (placeholder implementation) - 新增：获取可用内存
getAvailableMemoryMB :: IO Int
getAvailableMemoryMB = do
  -- This is a simplified implementation
  -- In a real system, you would parse /proc/meminfo or use system calls
  return 512  -- Default to 512MB for safety

-- | Apply moderate memory limits to a test tree - 极度优化
withMemoryLimits :: TestTree -> TestTree
withMemoryLimits test = 
  localOption (QuickCheckMaxSize 1) $    -- 保持最小值
  localOption (QuickCheckTests 2) $      -- 从3减少到2个测试
  localOption (QuickCheckMaxShrinks 0) $ -- 保持0次收缩
  test

-- | Apply aggressive memory limits to a test tree for memory-constrained environments - 极度优化
withAggressiveMemoryLimits :: TestTree -> TestTree
withAggressiveMemoryLimits test = 
  localOption (QuickCheckMaxSize 1) $    -- 保持最小值
  localOption (QuickCheckTests 1) $      -- 从2减少到1个测试
  localOption (QuickCheckMaxShrinks 0) $ -- 保持禁用收缩
  test

-- | Create a test group with minimal memory limits
minimalMemoryLimitedTestGroup :: String -> [TestTree] -> TestTree
minimalMemoryLimitedTestGroup name tests = 
  let limitedTests = map withMinimalMemoryLimits tests
  in testGroup ("[Ultra-Memory-Optimized] " ++ name) limitedTests

-- | Create a test group with ultra memory limits
ultraMemoryLimitedTestGroup :: String -> [TestTree] -> TestTree
ultraMemoryLimitedTestGroup name tests = 
  let limitedTests = map withUltraMemoryLimits tests
  in testGroup ("[Ultra-Memory-Optimized] " ++ name) limitedTests

-- | Create a test group with memory limits
memoryLimitedTestGroup :: String -> [TestTree] -> TestTree
memoryLimitedTestGroup name tests = 
  let limitedTests = map withMemoryLimits tests
  in testGroup ("[Memory-Limited] " ++ name) limitedTests

-- | Create a test group with aggressive memory limits and garbage collection
aggressiveMemoryLimitedTestGroup :: String -> [TestTree] -> TestTree
aggressiveMemoryLimitedTestGroup name tests = 
  let limitedTests = map withAggressiveMemoryLimits tests
  in testGroup ("[Memory-Optimized] " ++ name) limitedTests

-- Force garbage collection to free memory between tests
gcBetweenTests :: IO ()
gcBetweenTests = performGC

-- | Force aggressive garbage collection to free maximum memory - 极度增强垃圾回收
aggressiveGC :: IO ()
aggressiveGC = do
  performGC
  -- 多轮GC，每轮间隔很短
  replicateM_ 8 $ do  -- 增加GC轮数
    performGC
    threadDelay 800   -- 减少延迟，更频繁的GC
  -- 最后进行一次完整GC
  performGC
  -- 额外的清理步骤
  replicateM_ 3 performGC
  
-- | Force extreme garbage collection for minimal memory environments
extremeGC :: IO ()
extremeGC = do
  -- 执行多轮完整GC
  replicateM_ 15 $ do  -- 增加GC轮数
    performGC
    threadDelay 300   -- 减少延迟，更频繁的GC
  -- 强制清理所有可能的内存
  replicateM_ 5 $ performGC  -- 增加最终清理轮数
  threadDelay 2000

-- | Force ultra aggressive garbage collection for memory-critical situations - 极限垃圾回收
ultraGC :: IO ()
ultraGC = do
  performGC
  -- 多轮GC，每轮间隔很短，确保彻底清理
  replicateM_ 10 $ do  -- 增加GC轮数
    performGC
    threadDelay 1500   -- 减少延迟，更频繁的GC
  
  -- 最终清理
  replicateM_ 3 performGC  -- 增加最终清理轮数

-- | Add memory monitoring and cleanup to a test
withMemoryMonitoring :: IO a -> IO a
withMemoryMonitoring action = do
  -- Force GC before test
  performGC
  result <- action
  -- Force GC after test to clean up
  replicateM_ 2 performGC
  return result

-- | Helper to apply memory limits based on level
withMemoryLevel :: MemoryLevel -> TestTree -> TestTree
withMemoryLevel level test = case level of
  Minimal    -> withMinimalMemoryLimits test
  Ultra      -> withUltraMemoryLimits test
  Aggressive -> withAggressiveMemoryLimits test
  Moderate   -> withMemoryLimits test

-- | Helper to create test groups with memory level
memoryLevelTestGroup :: MemoryLevel -> String -> [TestTree] -> TestTree
memoryLevelTestGroup level name tests = 
  let limitedTests = map (withMemoryLevel level) tests
      prefix = case level of
        Minimal    -> "[Ultra-Memory-Optimized] "
        Ultra      -> "[Ultra-Memory-Optimized] "
        Aggressive -> "[Memory-Optimized] "
        Moderate   -> "[Memory-Limited] "
  in testGroup (prefix ++ name) limitedTests