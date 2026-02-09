module Test.Unit.MemoryOptimizedTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.MemoryLimits 
  ( MemoryLevel(..)
  , withMemoryLevel
  , memoryLevelTestGroup
  )

-- Import essential test modules that work well with memory constraints
import qualified Test.Unit.ConciseTestSuite as ConciseTestSuite
import Test.Unit.TestListPropertiesSpec (testListProperties)
import qualified Test.Unit.BasicQuickCheckTestSuite as BasicQuickCheckTestSuite
import qualified Test.Unit.UtilsComprehensiveSpec as UtilsComprehensiveSpec

-- Basic memory-efficient test properties
prop_memory_efficient_property :: String -> Property
prop_memory_efficient_property s = 
  let limitedString = take 5 s
  in property $ length limitedString >= 0

prop_string_operations :: String -> String -> Property
prop_string_operations s1 s2 = 
  let limitedS1 = take 3 s1
      limitedS2 = take 3 s2
      result = limitedS1 ++ limitedS2
  in property $ length result >= length limitedS1 && length result >= length limitedS2 && length result <= 6

-- Create memory-optimized test suites for different levels
createMemoryOptimizedSuite :: MemoryLevel -> TestTree
createMemoryOptimizedSuite level = 
  memoryLevelTestGroup level ("Memory-Optimized Test Suite (" ++ show level ++ ")")
    [ withMemoryLevel level $ testProperty "basic memory efficient property" prop_memory_efficient_property
    , withMemoryLevel level $ testProperty "string operations property" prop_string_operations
    , withMemoryLevel level ConciseTestSuite.tests
    , withMemoryLevel level testListProperties
    , withMemoryLevel level BasicQuickCheckTestSuite.tests
    , withMemoryLevel level UtilsComprehensiveSpec.utilsQuickCheckTests
    ]

-- Main test suite with adaptive memory levels
tests :: TestTree
tests = testGroup "Memory-Optimized Test Suites"
  [ createMemoryOptimizedSuite Minimal
  , createMemoryOptimizedSuite Ultra  
  , createMemoryOptimizedSuite Aggressive
  , createMemoryOptimizedSuite Moderate
  ]

-- Test suite for ultra memory-constrained environments
ultraMemoryTests :: TestTree
ultraMemoryTests = memoryLevelTestGroup Ultra "Ultra Memory-Constrained Tests"
  [ withMemoryLevel Ultra $ testProperty "minimal property" prop_memory_efficient_property
  , withMemoryLevel Ultra ConciseTestSuite.tests
  , withMemoryLevel Ultra testListProperties
  ]

-- Test suite for moderate memory environments
moderateMemoryTests :: TestTree
moderateMemoryTests = memoryLevelTestGroup Moderate "Moderate Memory Tests"
  [ withMemoryLevel Moderate $ testProperty "moderate property" prop_memory_efficient_property
  , withMemoryLevel Moderate $ testProperty "string operations" prop_string_operations
  , withMemoryLevel Moderate ConciseTestSuite.tests
  , withMemoryLevel Moderate BasicQuickCheckTestSuite.tests
  , withMemoryLevel Moderate UtilsComprehensiveSpec.utilsQuickCheckTests
  ]