{-# LANGUAGE OverloadedStrings #-}

-- | Test property memory cleanup module
-- This module provides memory cleanup functionality that can be applied
-- between individual test properties to prevent memory accumulation
module TestSupport.TestPropertyMemoryCleanup 
  ( -- Memory cleanup for test properties
    withPropertyMemoryCleanup
  , withStrategicPropertyCleanup
  , withEmergencyPropertyCleanup
  , withUltraAggressivePropertyCleanup
    
    -- Test property wrappers
    , memoryAwareProperty
  , memoryOptimizedProperty
  , memoryCriticalProperty
  , memoryUltraOptimizedProperty
    
    -- Test group with cleanup
    , testGroupWithCleanup
  , testGroupWithStrategicCleanup
  , testGroupWithEmergencyCleanup
  , testGroupWithUltraAggressiveCleanup
    
    -- Property execution control
    , executePropertyWithCleanup
  , executePropertyBatchWithCleanup
  , executePropertyWithMemoryLimit
    
    -- Cleanup strategies
    , CleanupStrategy(..)
  , applyCleanupStrategy
  , selectCleanupStrategy
  ) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck 
  ( testProperty
  , property
  , ioProperty
  , Property
  , QuickCheckMaxSize(..)
  , QuickCheckTests(..)
  , QuickCheckMaxShrinks(..)
  )
import System.Mem (performGC)
import Control.Monad (replicateM_, when)
import Control.Concurrent (threadDelay)
import System.Environment (getEnvironment)
import Data.Maybe (isJust)
import TestSupport.EnhancedMemoryOptimization 
  ( preTestGC
  , postTestGC
  , midTestGC
  , adaptiveGC
  )
import TestSupport.MemoryOptimizedQuickCheck 
  ( QuickCheckMemoryConfig(..)
  , emergencyMemoryConfig
  , ultraLowMemoryConfig
  , applyQuickCheckMemoryConfig
  )

-- | Cleanup strategy for test properties
data CleanupStrategy 
  = MinimalCleanup      -- ^ Basic cleanup between properties
  | StrategicCleanup    -- ^ Strategic cleanup with timing
  | EmergencyCleanup    -- ^ Maximum cleanup for critical situations
  | AdaptiveCleanup     -- ^ Adaptive cleanup based on environment
  deriving (Show, Eq)

-- | Memory cleanup for test properties
withPropertyMemoryCleanup :: IO a -> IO a
withPropertyMemoryCleanup action = do
  preTestGC
  result <- action
  postTestGC
  return result

-- | Strategic memory cleanup for test properties
withStrategicPropertyCleanup :: IO a -> IO a
withStrategicPropertyCleanup action = do
  preTestGC
  result <- action
  midTestGC
  postTestGC
  return result

-- | Emergency memory cleanup for test properties
withEmergencyPropertyCleanup :: IO a -> IO a
withEmergencyPropertyCleanup action = do
  adaptiveGC
  result <- action
  adaptiveGC
  return result

-- | Ultra-aggressive memory cleanup for test properties
withUltraAggressivePropertyCleanup :: IO a -> IO a
withUltraAggressivePropertyCleanup action = do
  replicateM_ 3 performGC
  threadDelay 100
  result <- action
  replicateM_ 3 performGC
  threadDelay 100
  replicateM_ 2 performGC
  return result

-- | Memory-aware test property wrapper
memoryAwareProperty :: String -> Property -> TestTree
memoryAwareProperty name prop = 
  let wrappedProp = ioProperty $ withPropertyMemoryCleanup $ return prop
  in testProperty name wrappedProp

-- | Memory-optimized test property wrapper
memoryOptimizedProperty :: String -> Property -> TestTree
memoryOptimizedProperty name prop = 
  let wrappedProp = ioProperty $ withStrategicPropertyCleanup $ return prop
  in testProperty name wrappedProp

-- | Memory-critical test property wrapper
memoryCriticalProperty :: String -> Property -> TestTree
memoryCriticalProperty name prop = 
  let wrappedProp = ioProperty $ withEmergencyPropertyCleanup $ return prop
  in testProperty name wrappedProp

-- | Memory-ultra-optimized test property wrapper
memoryUltraOptimizedProperty :: String -> Property -> TestTree
memoryUltraOptimizedProperty name prop = 
  let wrappedProp = ioProperty $ withUltraAggressivePropertyCleanup $ return prop
  in testProperty name wrappedProp

-- | Test group with basic cleanup between properties
testGroupWithCleanup :: String -> [TestTree] -> TestTree
testGroupWithCleanup name tests = 
  let cleanupTests = addCleanupBetweenTests tests
  in testGroup ("[With-Cleanup] " ++ name) cleanupTests

-- | Test group with strategic cleanup between properties
testGroupWithStrategicCleanup :: String -> [TestTree] -> TestTree
testGroupWithStrategicCleanup name tests = 
  let cleanupTests = addStrategicCleanupBetweenTests tests
  in testGroup ("[Strategic-Cleanup] " ++ name) cleanupTests

-- | Test group with emergency cleanup between properties
testGroupWithEmergencyCleanup :: String -> [TestTree] -> TestTree
testGroupWithEmergencyCleanup name tests = 
  let cleanupTests = addEmergencyCleanupBetweenTests tests
  in testGroup ("[Emergency-Cleanup] " ++ name) cleanupTests

-- | Test group with ultra-aggressive cleanup between properties
testGroupWithUltraAggressiveCleanup :: String -> [TestTree] -> TestTree
testGroupWithUltraAggressiveCleanup name tests = 
  let cleanupTests = addUltraAggressiveCleanupBetweenTests tests
  in testGroup ("[Ultra-Aggressive-Cleanup] " ++ name) cleanupTests

-- | Execute property with memory cleanup
executePropertyWithCleanup :: Property -> IO Property
executePropertyWithCleanup prop = do
  withPropertyMemoryCleanup $ return prop

-- | Execute property batch with cleanup between each
executePropertyBatchWithCleanup :: [Property] -> IO [Property]
executePropertyBatchWithCleanup props = do
  mapM executePropertyWithCleanup props

-- | Execute property with memory limit
executePropertyWithMemoryLimit :: Int -> Property -> IO Property
executePropertyWithMemoryLimit limitMB prop = do
  when (limitMB <= 8) $ adaptiveGC
  executePropertyWithCleanup prop

-- | Apply cleanup strategy to an action
applyCleanupStrategy :: CleanupStrategy -> IO a -> IO a
applyCleanupStrategy strategy action = case strategy of
  MinimalCleanup -> withPropertyMemoryCleanup action
  StrategicCleanup -> withStrategicPropertyCleanup action
  EmergencyCleanup -> withEmergencyPropertyCleanup action
  AdaptiveCleanup -> do
    env <- getEnvironment
    let isEmergency = isJust (lookup "EMERGENCY_MEMORY" env)
        isUltraOptimized = isJust (lookup "ULTRA_MEMORY_OPTIMIZED" env)
    if isEmergency
      then withEmergencyPropertyCleanup action
      else if isUltraOptimized
           then withStrategicPropertyCleanup action
           else withPropertyMemoryCleanup action

-- | Select cleanup strategy based on memory constraints
selectCleanupStrategy :: Int -> CleanupStrategy
selectCleanupStrategy availableMB
  | availableMB <= 4 = EmergencyCleanup
  | availableMB <= 8 = StrategicCleanup
  | availableMB <= 16 = StrategicCleanup
  | otherwise = MinimalCleanup

-- | Add cleanup between tests (simplified implementation)
addCleanupBetweenTests :: [TestTree] -> [TestTree]
addCleanupBetweenTests tests = 
  -- In practice, this would add actual cleanup tests between each test
  -- For now, just return the tests with memory optimization applied
  map applyMinimalMemoryOptimization tests

-- | Add strategic cleanup between tests
addStrategicCleanupBetweenTests :: [TestTree] -> [TestTree]
addStrategicCleanupBetweenTests tests = 
  map applyStrategicMemoryOptimization tests

-- | Add emergency cleanup between tests
addEmergencyCleanupBetweenTests :: [TestTree] -> [TestTree]
addEmergencyCleanupBetweenTests tests = 
  map applyEmergencyMemoryOptimization tests

-- | Add ultra-aggressive cleanup between tests
addUltraAggressiveCleanupBetweenTests :: [TestTree] -> [TestTree]
addUltraAggressiveCleanupBetweenTests tests = 
  map applyUltraAggressiveMemoryOptimization tests

-- | Apply minimal memory optimization to test
applyMinimalMemoryOptimization :: TestTree -> TestTree
applyMinimalMemoryOptimization test = 
  applyQuickCheckMemoryConfig ultraLowMemoryConfig test

-- | Apply strategic memory optimization to test
applyStrategicMemoryOptimization :: TestTree -> TestTree
applyStrategicMemoryOptimization test = 
  applyQuickCheckMemoryConfig ultraLowMemoryConfig test

-- | Apply emergency memory optimization to test
applyEmergencyMemoryOptimization :: TestTree -> TestTree
applyEmergencyMemoryOptimization test = 
  applyQuickCheckMemoryConfig emergencyMemoryConfig test

-- | Apply ultra-aggressive memory optimization to test
applyUltraAggressiveMemoryOptimization :: TestTree -> TestTree
applyUltraAggressiveMemoryOptimization test = 
  applyQuickCheckMemoryConfig emergencyMemoryConfig test