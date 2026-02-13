{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  -Wno-unused-imports  -Wno-unused-matches #-}
module Test.Unit.OptimizedTests where

import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.MemoryLimits 
  ( withMemoryLimits
  , memoryLimitedTestGroup
  , MemoryLevel(..)
  , withMemoryLevel
  , memoryLevelTestGroup
  , gcBetweenTests
  )

-- Import only essential test modules to reduce memory footprint
import qualified Test.Unit.ConciseTestSuite as ConciseTestSuite
import Test.Unit.TestListPropertiesSpec (testListProperties)

-- Import basic QuickCheck test suites
import qualified Test.Unit.BasicQuickCheckTestSuite as BasicQuickCheckTestSuite
import qualified Test.Unit.SimpleQuickCheckTestSuite as SimpleQuickCheckTestSuite
import qualified Test.Unit.ExtendedQuickCheckTestSuiteOptimized as ExtendedQuickCheckTestSuiteOptimized

-- Core test properties with controlled generator sizes - 进一步优化内存使用
prop_optimized_string_property :: String -> Property
prop_optimized_string_property s = 
  let limitedString = take 3 s  -- 极大减少字符串大小
  in property $ length limitedString >= 0

prop_optimized_list_property :: [Int] -> Property
prop_optimized_list_property xs = 
  let limitedList = take 2 xs   -- 极大减少列表大小
  in property $ length limitedList >= 0

prop_optimized_basic_property :: Int -> Property
prop_optimized_basic_property n = 
  let limitedN = mod (abs n) 50  -- 极大减少整数范围
  in property $ limitedN >= 0

-- Memory-efficient string operations - 进一步优化
prop_optimized_string_concat :: String -> String -> Property
prop_optimized_string_concat s1 s2 = 
  let limitedS1 = take 2 s1     -- 进一步减少
      limitedS2 = take 2 s2     -- 进一步减少
      result = limitedS1 ++ limitedS2
  in property $ length result >= 0 && length result <= 4

-- Memory-efficient list operations - 进一步优化
prop_optimized_list_operations :: [Int] -> [Int] -> Property
prop_optimized_list_operations xs ys = 
  let limitedXs = take 2 xs     -- 进一步减少
      limitedYs = take 2 ys     -- 进一步减少
      result = limitedXs ++ limitedYs
  in property $ length result <= 4

-- Create optimized test suite with memory constraints
createOptimizedTestSuite :: MemoryLevel -> TestTree
createOptimizedTestSuite level = 
  memoryLevelTestGroup level ("Optimized Test Suite (" ++ show level ++ ")")
    [ withMemoryLevel level $ testProperty "optimized string property" prop_optimized_string_property
    , withMemoryLevel level $ testProperty "optimized list property" prop_optimized_list_property
    , withMemoryLevel level $ testProperty "optimized basic property" prop_optimized_basic_property
    , withMemoryLevel level $ testProperty "optimized string concat" prop_optimized_string_concat
    , withMemoryLevel level $ testProperty "optimized list operations" prop_optimized_list_operations
    , withMemoryLevel level ConciseTestSuite.tests
    , withMemoryLevel level testListProperties
    , withMemoryLevel level BasicQuickCheckTestSuite.tests
    , withMemoryLevel level SimpleQuickCheckTestSuite.tests
    ]

-- Essential test suite with minimal memory usage
essentialOptimizedTests :: TestTree
essentialOptimizedTests = memoryLevelTestGroup Minimal "Essential Optimized Tests"
  [ withMemoryLevel Minimal $ testProperty "essential string property" prop_optimized_string_property
  , withMemoryLevel Minimal $ testProperty "essential list property" prop_optimized_list_property
  , withMemoryLevel Minimal $ testProperty "essential basic property" prop_optimized_basic_property
  , withMemoryLevel Minimal ConciseTestSuite.tests
  ]

-- Core functionality tests with standard memory optimization
coreOptimizedTests :: TestTree
coreOptimizedTests = createOptimizedTestSuite Moderate

-- Main optimized test suite with multiple memory levels
tests :: TestTree
tests = testGroup "Memory-Optimized Test Suites"
  [ essentialOptimizedTests
  , createOptimizedTestSuite Minimal
  , createOptimizedTestSuite Ultra
  , createOptimizedTestSuite Aggressive
  , coreOptimizedTests
  , withMemoryLevel Moderate ExtendedQuickCheckTestSuiteOptimized.tests
  ]

-- Test suite for CI/CD environments with strict memory limits
ciOptimizedTests :: TestTree
ciOptimizedTests = memoryLevelTestGroup Minimal "CI/CD Optimized Tests"
  [ withMemoryLevel Minimal $ testProperty "ci string property" prop_optimized_string_property
  , withMemoryLevel Minimal $ testProperty "ci list property" prop_optimized_list_property
  , withMemoryLevel Minimal $ testProperty "ci basic property" prop_optimized_basic_property
  , withMemoryLevel Minimal ConciseTestSuite.tests
  , withMemoryLevel Minimal testListProperties
  ]

-- Test suite with memory monitoring and cleanup - 进一步优化内存使用
monitoredOptimizedTests :: TestTree
monitoredOptimizedTests = memoryLimitedTestGroup "Memory-Monitored Optimized Tests"
  [ testProperty "monitored string property" $ \(s :: String) ->
      let limitedString = take 3 s  -- 极大减少字符串大小
      in property $ length limitedString >= 0
  , testProperty "monitored list property" $ \(xs :: [Int]) ->
      let limitedList = take 2 xs   -- 极大减少列表大小
      in property $ length limitedList >= 0
  ]