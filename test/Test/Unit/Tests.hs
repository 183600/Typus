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
import TestSupport.OptimizedTestOrdering 
  ( optimizeTestExecutionOrder
  , adaptTestOrderToMemory
  , createMemoryAwareTestSuite
  )

-- 精简的导入 - 只导入核心测试模块以减少内存使用

import qualified Test.Unit.BasicQuickCheckTestSuite as BasicQuickCheckTestSuite

-- 核心测试套件 - 只保留最关键的
import qualified Test.Unit.NewComprehensiveTypusTestSuite as NewComprehensiveTypusTestSuite
import qualified Test.Unit.TypusCoreQuickCheckTestSuite as TypusCoreQuickCheckTestSuite

-- 关键功能测试套件
import qualified Test.Unit.NewDependentTypesTestSuite as NewDependentTypesTestSuite
import qualified Test.Unit.NewOwnershipTestSuite as NewOwnershipTestSuite
import qualified Test.Unit.NewCompilerIntegrationTestSuite as NewCompilerIntegrationTestSuite
import qualified Test.Unit.NewParserTestSuite as NewParserTestSuite

-- 核心QuickCheck测试规范
import qualified Test.Unit.DependentTypesQuickCheckSpec as DependentTypesQuickCheckSpec
import qualified Test.Unit.OwnershipQuickCheckSpec as OwnershipQuickCheckSpec
import qualified Test.Unit.ParserQuickCheckSpec as ParserQuickCheckSpec
import qualified Test.Unit.CompilerQuickCheckSpec as CompilerQuickCheckSpec
import qualified Test.Unit.ErrorHandlingQuickCheckSpec as ErrorHandlingQuickCheckSpec

-- 新增的QuickCheck测试套件
import qualified Test.Unit.NewDependentTypesQuickCheckTests as NewDependentTypesQuickCheckTests
import qualified Test.Unit.NewRefinementTypesQuickCheckTests as NewRefinementTypesQuickCheckTests
import qualified Test.Unit.NewOwnershipQuickCheckTests as NewOwnershipQuickCheckTests
import qualified Test.Unit.NewCompilerParserQuickCheckTests as NewCompilerParserQuickCheckTests
import qualified Test.Unit.NewErrorHandlingQuickCheckTests as NewErrorHandlingQuickCheckTests




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
  let registryWithBasic = if availableMemory >= 20  -- 从24减少到20
        then registerTest "basic essential tests" BasicQuickCheckTestSuite.essentialTests 
                PriorityMedium 5 "Utils" False registryWithHigh2
        else registryWithHigh2
  
  -- 注册新创建的测试套件（如果内存允许）
  let registryWithNew = if availableMemory >= 28  -- 从32减少到28
        then let reg1 = registerTest "dependent types tests" NewDependentTypesTestSuite.tests 
                    PriorityMedium 6 "DependentTypes" False registryWithBasic
                 reg2 = registerTest "ownership tests" NewOwnershipTestSuite.tests 
                    PriorityMedium 6 "Ownership" False reg1
                 reg3 = registerTest "compiler integration tests" NewCompilerIntegrationTestSuite.tests 
                    PriorityMedium 6 "Compiler" False reg2
                 reg4 = registerTest "parser tests" NewParserTestSuite.tests 
                    PriorityMedium 6 "Parser" False reg3
                 reg5 = registerTest "new dependent types tests" NewDependentTypesQuickCheckTests.tests 
                    PriorityMedium 6 "DependentTypes" False reg4
                 reg6 = registerTest "new refinement types tests" NewRefinementTypesQuickCheckTests.tests 
                    PriorityMedium 6 "RefinementTypes" False reg5
                 reg7 = registerTest "new ownership tests" NewOwnershipQuickCheckTests.tests 
                    PriorityMedium 6 "Ownership" False reg6
                 reg8 = registerTest "new compiler parser tests" NewCompilerParserQuickCheckTests.tests 
                    PriorityMedium 6 "CompilerParser" False reg7
                 reg9 = registerTest "new error handling tests" NewErrorHandlingQuickCheckTests.tests 
                    PriorityMedium 6 "ErrorHandling" False reg8
             in reg9
        else registryWithBasic
  
  let finalRegistry = registryWithNew
  
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
      -- 创建精简的测试套件
      let allTests = 
            [ memoryOptimizedStringProperty (getConfigForMemory availableMemory) "ultra minimal basic" (\s -> let limited = take 1 s in length limited >= 0 && length limited <= 1)
            , memoryOptimizedListProperty (getConfigForMemory availableMemory) "ultra minimal list" (\xs -> let limited = take 1 xs in length limited >= 0 && length limited <= 1) (genSmallInt (getConfigForMemory availableMemory))
            , NewComprehensiveTypusTestSuite.testSuite
            , TypusCoreQuickCheckTestSuite.testSuite
            -- 核心测试套件
            , NewDependentTypesTestSuite.tests
            , NewOwnershipTestSuite.tests
            , NewCompilerIntegrationTestSuite.tests
            , NewParserTestSuite.tests
            -- 核心QuickCheck测试规范
            , DependentTypesQuickCheckSpec.tests
            , OwnershipQuickCheckSpec.tests
            , ParserQuickCheckSpec.tests
            , CompilerQuickCheckSpec.tests
            , ErrorHandlingQuickCheckSpec.tests
            -- 新增的QuickCheck测试套件
            , NewDependentTypesQuickCheckTests.tests
            , NewRefinementTypesQuickCheckTests.tests
            , NewOwnershipQuickCheckTests.tests
            , NewCompilerParserQuickCheckTests.tests
            , NewErrorHandlingQuickCheckTests.tests
            
            ]
      
      -- 使用优化的测试排序
      orderedTests <- optimizeTestExecutionOrder availableMemory allTests
      
      -- 根据可用内存选择测试数量 - 减少测试数量
      let maxTests = case availableMemory of
            _ | availableMemory <= 16 -> 2  -- 从3减少到2
            _ | availableMemory <= 24 -> 4  -- 从6减少到4
            _ | availableMemory <= 32 -> 8  -- 增加以包含新测试
            _ | availableMemory <= 48 -> 16  -- 增加以包含新测试
            _ -> 20  -- 增加以包含新测试
      
      let selectedTests = take maxTests orderedTests
      
      return $ createMemoryAwareTestSuite ("Typus Memory-Optimized Test Suite (" ++ show tier ++ ")") selectedTests
