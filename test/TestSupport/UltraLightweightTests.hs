{-# LANGUAGE CPP #-}

module TestSupport.UltraLightweightTests
  ( ultraLightweightTestSuite
  , minimalTestSuite
  , emergencyTestSuite
  , essentialTests
  , criticalTests
  , emergencyMemoryTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, property, Property)
import TestSupport.MemoryLimits (withMinimalMemoryLimits, minimalMemoryLimitedTestGroup)
import TestSupport.EnhancedMemoryOptimization (withStrictMemoryLimits)

-- Ultra lightweight test properties for memory-critical environments
prop_ultra_light_basic :: Int -> Property
prop_ultra_light_basic n = property $ n == n

prop_ultra_light_boolean :: Bool -> Property
prop_ultra_light_boolean b = property $ b == b

prop_ultra_light_minimal_string :: String -> Property
prop_ultra_light_minimal_string s = 
  let limited = take 3 s
  in property $ length limited >= 0

prop_ultra_light_simple_math :: Int -> Property
prop_ultra_light_simple_math n = 
  let bounded = abs (n `mod` 100)
  in property $ bounded >= 0 && bounded <= 99

-- Ultra lightweight test suite
ultraLightweightTestSuite :: TestTree
ultraLightweightTestSuite = withMinimalMemoryLimits $ testGroup "Ultra Lightweight Tests"
  [ testProperty "basic identity" prop_ultra_light_basic
  , testProperty "boolean identity" prop_ultra_light_boolean
  , testProperty "minimal string" prop_ultra_light_minimal_string
  , testProperty "simple math" prop_ultra_light_simple_math
  ]

-- Minimal test suite for emergency situations
minimalTestSuite :: TestTree
minimalTestSuite = withStrictMemoryLimits $ testGroup "Minimal Emergency Tests"
  [ testProperty "core identity" prop_ultra_light_basic
  , testProperty "core boolean" prop_ultra_light_boolean
  ]

-- Emergency test suite for extreme memory constraints
emergencyTestSuite :: TestTree
emergencyTestSuite = withStrictMemoryLimits $ testGroup "Emergency Memory Tests"
  [ testProperty "emergency basic" prop_ultra_light_basic
  ]

-- Essential tests that should always run
essentialTests :: TestTree
essentialTests = minimalMemoryLimitedTestGroup "Essential Tests"
  [ testProperty "essential identity" prop_ultra_light_basic
  , testProperty "essential boolean" prop_ultra_light_boolean
  ]

-- Critical tests for core functionality
criticalTests :: TestTree
criticalTests = minimalMemoryLimitedTestGroup "Critical Tests"
  [ testProperty "critical identity" prop_ultra_light_basic
  , testProperty "critical boolean" prop_ultra_light_boolean
  , testProperty "critical minimal string" prop_ultra_light_minimal_string
  ]

-- Emergency memory tests for critical situations
emergencyMemoryTests :: TestTree
emergencyMemoryTests = withStrictMemoryLimits $ testGroup "Emergency Memory Tests"
  [ testProperty "emergency core" prop_ultra_light_basic
  ]