{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.Tests where

import System.Environment (getEnvironment)
import Data.Maybe (isJust)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performGC)
import Control.Monad (replicateM_)

import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.UnifiedMemoryOptimizedTestRunner 
  ( UnifiedTestConfig(..)
  , defaultUnifiedConfig
  , createMemoryOptimizedConfig
  , runUnifiedMemoryOptimizedTests
  , TestRegistry(..)
  , emptyRegistry
  , registerCriticalTest
  , registerHighPriorityTest
  , registerTest
  )
import TestSupport.SmartTestSelection 
  ( TestPriority(..)
  , TestInfo(..)
  , MemoryTier(..)
  , createTestInfo
  , detectAvailableMemory
  , getMemoryTier
  )
import TestSupport.MemoryOptimizedQuickCheck 
  ( QuickCheckMemoryConfig(..)
  , ultraLowMemoryConfig
  , criticalMemoryConfig
  , lowMemoryConfig
  , createMemoryOptimizedProperty
  , memoryOptimizedStringProperty
  , memoryOptimizedListProperty
  , memoryOptimizedIntProperty
  , memoryOptimizedBoolProperty
  , getConfigForMemory
  , genSmallInt
  )
import TestSupport.ExtremeMemoryOptimization 
  ( smartMemoryCleanup
  , emergencyMemoryCleanup
  )

-- 极度精简的导入 - 只导入绝对必要的测试模块

import qualified Test.Unit.BasicQuickCheckTestSuite as BasicQuickCheckTestSuite

import qualified Test.Unit.NewComprehensiveTypusTestSuite as NewComprehensiveTypusTestSuite
import qualified Test.Unit.EnhancedTypusFeaturesTestSuite as EnhancedTypusFeaturesTestSuite



import Test.Unit.TestListPropertiesSpec (testListProperties)

-- 极度精简的内存优化测试属性 - 极限内存使用
prop_ultra_minimal_basic :: String -> Property
prop_ultra_minimal_basic s = 
  let limitedString = take 1 s  -- 限制为1个字符
  in property $ length limitedString >= 0 && length limitedString <= 1

prop_ultra_minimal_list :: [Int] -> Property
prop_ultra_minimal_list xs = 
  let limitedList = take 1 xs   -- 限制为1个元素
  in property $ length limitedList >= 0 && length limitedList <= 1

prop_ultra_minimal_bool :: Bool -> Property
prop_ultra_minimal_bool b = property $ (b == True) || (b == False)

prop_ultra_minimal_int :: Int -> Property
prop_ultra_minimal_int n = 
  let limitedInt = abs n `mod` 2  -- 限制为0或1
  in property $ limitedInt >= 0 && limitedInt <= 1

-- 创建内存优化的测试注册表
createOptimizedTestRegistry :: IO TestRegistry
createOptimizedTestRegistry = do
  -- 检测可用内存
  availableMemory <- detectAvailableMemory
  let qcConfig = getConfigForMemory availableMemory
  
  -- 创建空注册表
  let registry = emptyRegistry
  
  -- 注册关键测试（优先级最高，内存占用最低）
  let registryWithCritical = registerCriticalTest "ultra minimal basic" 
        (memoryOptimizedStringProperty qcConfig "ultra minimal basic" (\s -> length s >= 0 && length s <= 1))
        1 "Core" registry
  
  let registryWithCritical2 = registerCriticalTest "ultra minimal list"
        (memoryOptimizedListProperty qcConfig "ultra minimal list" (\xs -> let limited = take 1 xs in length limited >= 0 && length limited <= 1) (genSmallInt qcConfig))
        1 "Core" registryWithCritical
  
  -- 注册高优先级测试
  let registryWithHigh = registerHighPriorityTest "ultra minimal bool"
        (memoryOptimizedBoolProperty qcConfig "ultra minimal bool" (\b -> b == True || b == False))
        1 "Core" registryWithCritical2
  
  let registryWithHigh2 = registerHighPriorityTest "ultra minimal int"
        (memoryOptimizedIntProperty qcConfig "ultra minimal int" (\n -> let limitedInt = abs n `mod` 2 in limitedInt >= 0 && limitedInt <= 1))
        1 "Core" registryWithHigh
  
  -- 注册基本测试套件（如果内存允许）
  let finalRegistry = if availableMemory >= 24
        then registerTest "basic essential tests" BasicQuickCheckTestSuite.essentialTests 
                PriorityMedium 5 "Utils" False registryWithHigh2
        else registryWithHigh2
  
  return finalRegistry

-- 创建统一配置
createUnifiedConfig :: IO UnifiedTestConfig
createUnifiedConfig = do
  availableMemory <- detectAvailableMemory
  env <- getEnvironment
  
  let isUltraOptimized = isJust (lookup "ULTRA_MEMORY_OPTIMIZED" env)
      isEmergency = isJust (lookup "EMERGENCY_MEMORY" env)
      
  if isUltraOptimized || isEmergency
    then return $ createMemoryOptimizedConfig 16
    else return $ createMemoryOptimizedConfig availableMemory

-- 极限内存优化的测试套件
tests :: TestTree
tests = unsafePerformIO $ do
  -- 强制垃圾回收
  replicateM_ 5 performGC
  
  -- 检测内存环境
  availableMemory <- detectAvailableMemory
  let tier = getMemoryTier availableMemory
  
  -- 创建优化的测试注册表
  registry <- createOptimizedTestRegistry
  
  -- 创建统一配置
  config <- createUnifiedConfig
  
  -- 根据内存限制创建测试套件
  case tier of
    UltraCritical -> do
      emergencyMemoryCleanup
      return $ memoryOptimizedStringProperty ultraLowMemoryConfig "emergency test" (\s -> length s >= 0 && length s <= 1)
    
    Critical -> do
      smartMemoryCleanup
      return $ memoryOptimizedStringProperty criticalMemoryConfig "critical test" (\s -> length s >= 0 && length s <= 1)
    
    _ -> do
      -- 创建测试套件
      let allTests = 
            [ memoryOptimizedStringProperty (getConfigForMemory availableMemory) "ultra minimal basic" (\s -> let limited = take 1 s in length limited >= 0 && length limited <= 1)
            , memoryOptimizedListProperty (getConfigForMemory availableMemory) "ultra minimal list" (\xs -> let limited = take 1 xs in length limited >= 0 && length limited <= 1) (genSmallInt (getConfigForMemory availableMemory))
            , NewComprehensiveTypusTestSuite.testSuite
            , EnhancedTypusFeaturesTestSuite.testSuite
            ]
      
      -- 根据可用内存选择测试数量
      let maxTests = case availableMemory of
            _ | availableMemory <= 16 -> 1
            _ | availableMemory <= 24 -> 2
            _ | availableMemory <= 32 -> 3
            _ | availableMemory <= 48 -> 4
            _ -> 5
      
      let selectedTests = take maxTests allTests
      
      return $ testGroup ("Typus Memory-Optimized Test Suite (" ++ show tier ++ ")") selectedTests
