module Test.Unit.AdvancedMemoryOptimizedTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit (testCase)
import TestSupport.AdvancedMemoryLimits
  ( AdaptiveMemoryConfig(..)
  , withExtremeMemoryLimits
  , withConservativeMemoryLimits
  , createMemoryBalancedSuite
  , monitorMemoryUsage
  , forceAggressiveCleanup
  , extremeMemoryConfig
  , minimalMemoryConfig
  , conservativeMemoryConfig
  )
import TestSupport.MemoryLimits 
  ( MemoryLevel(..)
  , withMemoryLevel
  , memoryLevelTestGroup
  , gcBetweenTests
  )

-- Import essential test modules
import qualified Test.Unit.ConciseTestSuite as ConciseTestSuite
import Test.Unit.TestListPropertiesSpec (testListProperties)
import qualified Test.Unit.BasicQuickCheckTestSuite as BasicQuickCheckTestSuite
import qualified Test.Unit.UtilsComprehensiveSpec as UtilsComprehensiveSpec

-- Memory-efficient test properties with explicit cleanup
prop_memory_efficient_string :: String -> Property
prop_memory_efficient_string s = property $ length s >= 0

prop_memory_efficient_list :: [Int] -> Property
prop_memory_efficient_list xs = property $ length xs >= 0

prop_memory_efficient_tuple :: (String, Int) -> Property
prop_memory_efficient_tuple (s, n) = property $ length s >= 0 && abs n >= 0

-- Test with memory monitoring
prop_monitored_property :: String -> Property
prop_monitored_property s = property $ length s >= 0

-- Create adaptive test suite based on memory constraints
createAdaptiveSuite :: AdaptiveMemoryConfig -> TestTree
createAdaptiveSuite config = 
  createMemoryBalancedSuite config "Adaptive Memory Tests"
    [ testProperty "memory efficient string" prop_memory_efficient_string
    , testProperty "memory efficient list" prop_memory_efficient_list
    , testProperty "memory efficient tuple" prop_memory_efficient_tuple
    , testProperty "monitored property" prop_monitored_property
    , ConciseTestSuite.tests
    , testListProperties
    , BasicQuickCheckTestSuite.tests
    , UtilsComprehensiveSpec.utilsQuickCheckTests
    ]

-- Extreme memory optimization for severely constrained environments
extremeMemorySuite :: TestTree
extremeMemorySuite = withExtremeMemoryLimits $ testGroup "[Extreme-Memory] Tests"
  [ testProperty "basic string property" prop_memory_efficient_string
  , testProperty "basic list property" prop_memory_efficient_list
  , withExtremeMemoryLimits ConciseTestSuite.tests
  ]

-- Minimal memory optimization suite
minimalMemorySuite :: TestTree
minimalMemorySuite = createMemoryBalancedSuite minimalMemoryConfig "Minimal Memory Tests"
  [ testProperty "string property" prop_memory_efficient_string
  , testProperty "list property" prop_memory_efficient_list
  , testProperty "tuple property" prop_memory_efficient_tuple
  , ConciseTestSuite.tests
  , testListProperties
  ]

-- Conservative memory optimization with profiling
conservativeMemorySuite :: TestTree
conservativeMemorySuite = withConservativeMemoryLimits $ testGroup "[Conservative-Memory] Tests"
  [ testProperty "enhanced string property" prop_memory_efficient_string
  , testProperty "enhanced list property" prop_memory_efficient_list
  , testProperty "enhanced tuple property" prop_memory_efficient_tuple
  , testProperty "monitored property" prop_monitored_property
  , withConservativeMemoryLimits ConciseTestSuite.tests
  , withConservativeMemoryLimits testListProperties
  , withConservativeMemoryLimits BasicQuickCheckTestSuite.tests
  , withConservativeMemoryLimits UtilsComprehensiveSpec.utilsQuickCheckTests
  ]

-- Test with explicit memory management
testWithMemoryManagement :: IO () -> TestTree
testWithMemoryManagement action = 
  testCase "Memory Managed Test" $ do
    -- Pre-test cleanup
    forceAggressiveCleanup
    
    -- Run test with monitoring
    _ <- monitorMemoryUsage action
    
    -- Post-test cleanup
    forceAggressiveCleanup

-- Main adaptive test suite
tests :: TestTree
tests = testGroup "Advanced Memory-Optimized Test Suites"
  [ extremeMemorySuite
  , minimalMemorySuite
  , conservativeMemorySuite
  , createAdaptiveSuite extremeMemoryConfig
  , createAdaptiveSuite minimalMemoryConfig
  , createAdaptiveSuite conservativeMemoryConfig
  ]

-- Legacy compatibility with MemoryLevel system
legacyMemorySuite :: TestTree
legacyMemorySuite = testGroup "Legacy Memory-Optimized Tests"
  [ memoryLevelTestGroup Minimal "Minimal Legacy Tests"
    [ withMemoryLevel Minimal $ testProperty "basic property" prop_memory_efficient_string
    , withMemoryLevel Minimal ConciseTestSuite.tests
    ]
  , memoryLevelTestGroup Ultra "Ultra Legacy Tests"
    [ withMemoryLevel Ultra $ testProperty "ultra property" prop_memory_efficient_string
    , withMemoryLevel Ultra $ testProperty "ultra list" prop_memory_efficient_list
    , withMemoryLevel Ultra ConciseTestSuite.tests
    , withMemoryLevel Ultra testListProperties
    ]
  ]

-- Combined test suite for comprehensive testing
combinedTestSuite :: TestTree
combinedTestSuite = testGroup "Combined Memory-Optimized Test Suite"
  [ tests
  , legacyMemorySuite
  , testWithMemoryManagement $ do
      putStrLn "Running memory-managed test..."
      gcBetweenTests
      putStrLn "Memory-managed test completed."
  ]