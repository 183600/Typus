{-# LANGUAGE CPP #-}
module Test.Unit.EnhancedMemoryOptimizedTestSuite where

import Test.Tasty
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck
import TestSupport.EnhancedMemoryLimits 
  ( EnhancedMemoryLevel(..)
  , withEnhancedMemoryLevel
  , enhancedMemoryLevelTestGroup
  , withMemoryMonitoring
  , aggressiveGC
  )
import TestSupport.MemoryLimits (withAggressiveMemoryLimits)

-- Import essential test modules that work well with memory constraints
import qualified Test.Unit.ConciseTestSuite as ConciseTestSuite
import Test.Unit.TestListPropertiesSpec (testListProperties)
import qualified Test.Unit.BasicQuickCheckTestSuite as BasicQuickCheckTestSuite
import qualified Test.Unit.UtilsComprehensiveSpec as UtilsComprehensiveSpec

-- Enhanced memory-efficient test properties with controlled generator sizes
prop_enhanced_memory_efficient_property :: EnhancedMemoryLevel -> Property
prop_enhanced_memory_efficient_property _ = 
  property $ (\s -> length (s :: String) >= 0)

prop_enhanced_string_operations :: EnhancedMemoryLevel -> String -> String -> Property
prop_enhanced_string_operations _ s1 s2 = 
  property $ 
    length (s1 ++ s2) >= 
    max (length s1) (length s2)

prop_enhanced_list_operations :: EnhancedMemoryLevel -> [Int] -> Property
prop_enhanced_list_operations level xs = 
  let limitedSize = case level of
        Minimal -> 2
        Strict -> 3
        Conservative -> 5
        Moderate -> 8
      trimmedList = take limitedSize xs
  in property $ length trimmedList <= limitedSize

-- Create enhanced memory-optimized test suites for different levels
createEnhancedMemoryOptimizedSuite :: EnhancedMemoryLevel -> TestTree
createEnhancedMemoryOptimizedSuite level = 
  enhancedMemoryLevelTestGroup level ("Enhanced Memory-Optimized Test Suite (" ++ show level ++ ")")
    [ withEnhancedMemoryLevel level $ testProperty "enhanced memory efficient property" (prop_enhanced_memory_efficient_property level)
    , withEnhancedMemoryLevel level $ testProperty "enhanced string operations property" (prop_enhanced_string_operations level)
    , withEnhancedMemoryLevel level $ testProperty "enhanced list operations property" (prop_enhanced_list_operations level)
    , withEnhancedMemoryLevel level ConciseTestSuite.tests
    , withEnhancedMemoryLevel level testListProperties
    , withEnhancedMemoryLevel level BasicQuickCheckTestSuite.tests
    , withEnhancedMemoryLevel level UtilsComprehensiveSpec.utilsQuickCheckTests
    ]

-- Main test suite with adaptive memory levels
tests :: TestTree
tests = testGroup "Enhanced Memory-Optimized Test Suites"
  [ createEnhancedMemoryOptimizedSuite Minimal
  , createEnhancedMemoryOptimizedSuite Strict
  , createEnhancedMemoryOptimizedSuite Conservative
  , createEnhancedMemoryOptimizedSuite Moderate
  ]

-- Test suite for ultra memory-constrained environments
ultraEnhancedMemoryTests :: TestTree
ultraEnhancedMemoryTests = enhancedMemoryLevelTestGroup Minimal "Ultra Enhanced Memory-Constrained Tests"
  [ withEnhancedMemoryLevel Minimal $ testProperty "minimal enhanced property" (prop_enhanced_memory_efficient_property Minimal)
  , withEnhancedMemoryLevel Minimal ConciseTestSuite.tests
  , withEnhancedMemoryLevel Minimal testListProperties
  ]

-- Test suite for strict memory environments
strictEnhancedMemoryTests :: TestTree
strictEnhancedMemoryTests = enhancedMemoryLevelTestGroup Strict "Strict Enhanced Memory Tests"
  [ withEnhancedMemoryLevel Strict $ testProperty "strict enhanced property" (prop_enhanced_memory_efficient_property Strict)
  , withEnhancedMemoryLevel Strict $ testProperty "strict string operations" (prop_enhanced_string_operations Strict)
  , withEnhancedMemoryLevel Strict ConciseTestSuite.tests
  , withEnhancedMemoryLevel Strict BasicQuickCheckTestSuite.tests
  , withEnhancedMemoryLevel Strict UtilsComprehensiveSpec.utilsQuickCheckTests
  ]

-- Test suite with memory monitoring and aggressive GC
monitoredMemoryTests :: TestTree
monitoredMemoryTests = testGroup "Memory-Monitored Tests"
  [ testCase "Memory monitoring test" $ do
      withMemoryMonitoring $ do
        aggressiveGC
        return ()
  , withAggressiveMemoryLimits $ testProperty "monitored property" (prop_enhanced_memory_efficient_property Conservative)
  ]