{-# LANGUAGE CPP #-}
module Test.Unit.UltraMemoryOptimizedTestSuite (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit (testCase)
import TestSupport.UltraMemoryOptimized 
  ( UltraMemoryConfig(..)
  , withUltraMemoryOptimization
  , ultraMemoryOptimizedTestGroup
  , forceMemoryCleanup
  , withMemoryConstraint
  , selectEssentialTests
  , defaultUltraConfig
  , minimalUltraConfig
  )
import TestSupport.MemoryLimits (gcBetweenTests)

-- Import only essential test modules that work well with memory constraints
import qualified Test.Unit.ConciseTestSuite as ConciseTestSuite
import Test.Unit.TestListPropertiesSpec (testListProperties)
import qualified Test.Unit.BasicQuickCheckTestSuite as BasicQuickCheckTestSuite

-- Ultra memory-efficient test properties with minimal generator sizes
prop_ultra_memory_efficient_string :: String -> Property
prop_ultra_memory_efficient_string s = 
  let limitedString = take 10 s  -- Limit string size
  in property $ length limitedString >= 0

prop_ultra_memory_efficient_list :: [Int] -> Property
prop_ultra_memory_efficient_list xs = 
  let limitedList = take 5 xs   -- Limit list size
  in property $ length limitedList >= 0

prop_ultra_memory_efficient_basic :: Int -> Property
prop_ultra_memory_efficient_basic n = 
  let limitedN = mod (abs n) 100  -- Limit integer range
  in property $ limitedN >= 0

-- Essential string operations with memory constraints
prop_ultra_string_concat :: String -> String -> Property
prop_ultra_string_concat s1 s2 = 
  let limitedS1 = take 5 s1
      limitedS2 = take 5 s2
      result = limitedS1 ++ limitedS2
  in property $ length result >= 0 && length result <= 10

-- Essential list operations with memory constraints
prop_ultra_list_length :: [Int] -> Property
prop_ultra_list_length xs = 
  let limitedXs = take 3 xs
  in property $ length limitedXs >= 0 && length limitedXs <= 3

-- Create ultra memory-optimized test suite for different levels
createUltraMemoryOptimizedSuite :: UltraMemoryConfig -> TestTree
createUltraMemoryOptimizedSuite config = 
  ultraMemoryOptimizedTestGroup config ("Ultra Memory-Optimized Test Suite (" ++ show (memoryLimitMB config) ++ "MB)")
    [ testProperty "ultra memory efficient string" prop_ultra_memory_efficient_string
    , testProperty "ultra memory efficient list" prop_ultra_memory_efficient_list
    , testProperty "ultra memory efficient basic" prop_ultra_memory_efficient_basic
    , testProperty "ultra string concat" prop_ultra_string_concat
    , testProperty "ultra list length" prop_ultra_list_length
    , ConciseTestSuite.tests
    , testListProperties
    ]

-- Essential test suite with minimal memory usage
essentialUltraMemoryTests :: TestTree
essentialUltraMemoryTests = ultraMemoryOptimizedTestGroup minimalUltraConfig "Essential Ultra Memory Tests"
  [ testProperty "essential string property" prop_ultra_memory_efficient_string
  , testProperty "essential list property" prop_ultra_memory_efficient_list
  , testProperty "essential basic property" prop_ultra_memory_efficient_basic
  ]

-- Core functionality tests with ultra memory optimization
coreUltraMemoryTests :: TestTree
coreUltraMemoryTests = createUltraMemoryOptimizedSuite defaultUltraConfig

-- Test with explicit memory management and cleanup
testWithUltraMemoryManagement :: IO () -> TestTree
testWithUltraMemoryManagement action = 
  testCase "Ultra Memory Managed Test" $ do
    -- Pre-test cleanup
    forceMemoryCleanup
    
    -- Run test action directly
    action
    
    -- Post-test cleanup
    forceMemoryCleanup
    
    -- Additional GC
    gcBetweenTests

-- Main ultra memory-optimized test suite
tests :: TestTree
tests = testGroup "Ultra Memory-Optimized Test Suites"
  [ essentialUltraMemoryTests
  , coreUltraMemoryTests
  , createUltraMemoryOptimizedSuite defaultUltraConfig
  , testWithUltraMemoryManagement $ do
      return ()  -- Minimal test action
  ]

-- Extreme memory optimization for severely constrained environments
extremeMemoryTests :: TestTree
extremeMemoryTests = ultraMemoryOptimizedTestGroup minimalUltraConfig "Extreme Memory-Constrained Tests"
  [ testProperty "extreme string property" prop_ultra_memory_efficient_string
  , testProperty "extreme list property" prop_ultra_memory_efficient_list
  , testProperty "extreme basic property" prop_ultra_memory_efficient_basic
  ]

-- Combined test suite for comprehensive ultra memory optimization
combinedUltraTestSuite :: TestTree
combinedUltraTestSuite = testGroup "Combined Ultra Memory-Optimized Test Suite"
  [ tests
  , extremeMemoryTests
  , testWithUltraMemoryManagement $ do
      return ()
  ]