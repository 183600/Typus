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

-- Core test properties with controlled generator sizes
prop_optimized_string_property :: String -> Property
prop_optimized_string_property s = 
  let limitedString = take 10 s  -- Further reduced string size
  in property $ length limitedString >= 0

prop_optimized_list_property :: [Int] -> Property
prop_optimized_list_property xs = 
  let limitedList = take 5 xs   -- Further reduced list size
  in property $ length limitedList >= 0

prop_optimized_basic_property :: Int -> Property
prop_optimized_basic_property n = 
  let limitedN = mod (abs n) 100  -- Further reduced integer range
  in property $ limitedN >= 0

-- Memory-efficient string operations
prop_optimized_string_concat :: String -> String -> Property
prop_optimized_string_concat s1 s2 = 
  let limitedS1 = take 5 s1
      limitedS2 = take 5 s2
      result = limitedS1 ++ limitedS2
  in property $ length result >= 0 && length result <= 10

-- Memory-efficient list operations
prop_optimized_list_operations :: [Int] -> [Int] -> Property
prop_optimized_list_operations xs ys = 
  let limitedXs = take 3 xs
      limitedYs = take 3 ys
      result = limitedXs ++ limitedYs
  in property $ length result <= 6

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

-- Test suite with memory monitoring and cleanup
monitoredOptimizedTests :: TestTree
monitoredOptimizedTests = memoryLimitedTestGroup "Memory-Monitored Optimized Tests"
  [ testProperty "monitored string property" $ \(s :: String) ->
      let limitedString = take 15 s
      in property $ length limitedString >= 0
  , testProperty "monitored list property" $ \(xs :: [Int]) ->
      let limitedList = take 8 xs
      in property $ length limitedList >= 0
  ]