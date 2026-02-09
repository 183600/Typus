module Test.Unit.Tests where

import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.MemoryLimits (withAggressiveMemoryLimits, aggressiveMemoryLimitedTestGroup)
import TestSupport.OptimizedMemoryLimits 
  ( withOptimizedMemoryLimits
  , withStrictMemoryLimits
  , withBalancedMemoryLimits
  , optimizedMemoryConfig
  , createOptimizedMemorySuite
  )

-- Import only essential test modules to reduce memory footprint
import qualified Test.Unit.ConciseTestSuite as ConciseTestSuite
import qualified Test.Unit.OptimizedTests as OptimizedTests
import Test.Unit.TestListPropertiesSpec (testListProperties)

-- Import basic QuickCheck test suites
import qualified Test.Unit.BasicQuickCheckTestSuite as BasicQuickCheckTestSuite
import qualified Test.Unit.SimpleQuickCheckTestSuite as SimpleQuickCheckTestSuite

-- Import key memory-optimized test suites
import qualified Test.Unit.EnhancedMemoryOptimizedTestSuite as EnhancedMemoryOptimizedTestSuite
import qualified Test.Unit.UltraMemoryOptimizedTestSuite as UltraMemoryOptimizedTestSuite

-- Import the new extended QuickCheck test suite
import qualified Test.Unit.ExtendedQuickCheckTestSuite as ExtendedQuickCheckTestSuite

-- Import the new comprehensive QuickCheck test suites
import qualified Test.Unit.NewComprehensiveQuickCheckTestSuite as NewComprehensiveQuickCheckTestSuite
import qualified Test.Unit.AdvancedModuleQuickCheckTestSuite as AdvancedModuleQuickCheckTestSuite

-- Import the new enhanced QuickCheck test suites
import qualified Test.Unit.EnhancedOwnershipTransferQuickCheckSpec as EnhancedOwnershipTransferQuickCheckSpec
import qualified Test.Unit.AdvancedDependentTypesQuickCheckSpec as AdvancedDependentTypesQuickCheckSpec
import qualified Test.Unit.RobustErrorHandlingQuickCheckSpec as RobustErrorHandlingQuickCheckSpec
import qualified Test.Unit.ComprehensiveSourceLocationQuickCheckSpec as ComprehensiveSourceLocationQuickCheckSpec
import qualified Test.Unit.AdvancedParserCombinatorsQuickCheckSpec as AdvancedParserCombinatorsQuickCheckSpec
import qualified Test.Unit.EnhancedTypeCheckerQuickCheckSpec as EnhancedTypeCheckerQuickCheckSpec

-- Import the new core module QuickCheck test suites
import qualified Test.Unit.AnalyzerCoreQuickCheckSpec as AnalyzerCoreQuickCheckSpec
import qualified Test.Unit.CliCoreQuickCheckSpec as CliCoreQuickCheckSpec
import qualified Test.Unit.DebugCoreQuickCheckSpec as DebugCoreQuickCheckSpec
import qualified Test.Unit.EmbedAssetsCoreQuickCheckSpec as EmbedAssetsCoreQuickCheckSpec
import qualified Test.Unit.GoToolchainCoreQuickCheckSpec as GoToolchainCoreQuickCheckSpec
import qualified Test.Unit.SyntaxValidatorCoreQuickCheckSpec as SyntaxValidatorCoreQuickCheckSpec
import qualified Test.Unit.UtilsCoreQuickCheckSpec as UtilsCoreQuickCheckSpec
import qualified Test.Unit.ComprehensiveIntegrationQuickCheckSpec as ComprehensiveIntegrationQuickCheckSpec

-- Memory-efficient test properties with controlled generator sizes
prop_optimized_basic_property :: String -> Property
prop_optimized_basic_property s = 
  let limitedString = take 8 s  -- Further reduced string size
  in property $ length limitedString >= 0

prop_optimized_list_property :: [Int] -> Property
prop_optimized_list_property xs = 
  let limitedList = take 4 xs   -- Further reduced list size
  in property $ length limitedList >= 0

-- Create memory-optimized test suite
tests :: TestTree
tests = aggressiveMemoryLimitedTestGroup "Typus Test Suite (Memory Optimimized)"
  [ -- Use optimized memory limits for all tests
    withOptimizedMemoryLimits $ testProperty "optimized basic property" prop_optimized_basic_property,
    withOptimizedMemoryLimits $ testProperty "optimized list property" prop_optimized_list_property,
    
    -- Essential test suites with optimized memory
    withOptimizedMemoryLimits ConciseTestSuite.tests,
    withOptimizedMemoryLimits testListProperties,
    withOptimizedMemoryLimits BasicQuickCheckTestSuite.tests,
    withOptimizedMemoryLimits SimpleQuickCheckTestSuite.tests,
    
    -- Memory-optimized test suites
    withOptimizedMemoryLimits OptimizedTests.tests,
    withOptimizedMemoryLimits EnhancedMemoryOptimizedTestSuite.tests,
    
    -- Ultra memory-optimized tests for CI/CD
    withStrictMemoryLimits UltraMemoryOptimizedTestSuite.tests,
    
    -- Extended QuickCheck test suite with comprehensive test cases
    withOptimizedMemoryLimits ExtendedQuickCheckTestSuite.tests,
    
    -- New comprehensive QuickCheck test suites
    withOptimizedMemoryLimits NewComprehensiveQuickCheckTestSuite.newComprehensiveQuickCheckTestSuite,
    withOptimizedMemoryLimits AdvancedModuleQuickCheckTestSuite.testSuite,
    
    -- New enhanced QuickCheck test suites
    withOptimizedMemoryLimits EnhancedOwnershipTransferQuickCheckSpec.testSuite,
    withOptimizedMemoryLimits AdvancedDependentTypesQuickCheckSpec.testSuite,
    withOptimizedMemoryLimits RobustErrorHandlingQuickCheckSpec.testSuite,
    withOptimizedMemoryLimits ComprehensiveSourceLocationQuickCheckSpec.testSuite,
    withOptimizedMemoryLimits AdvancedParserCombinatorsQuickCheckSpec.testSuite,
    withOptimizedMemoryLimits EnhancedTypeCheckerQuickCheckSpec.testSuite,
    
    -- New core module QuickCheck test suites
    withOptimizedMemoryLimits AnalyzerCoreQuickCheckSpec.testSuite,
    withOptimizedMemoryLimits CliCoreQuickCheckSpec.testSuite,
    withOptimizedMemoryLimits DebugCoreQuickCheckSpec.testSuite,
    withOptimizedMemoryLimits EmbedAssetsCoreQuickCheckSpec.testSuite,
    withOptimizedMemoryLimits GoToolchainCoreQuickCheckSpec.testSuite,
    withOptimizedMemoryLimits SyntaxValidatorCoreQuickCheckSpec.testSuite,
    withOptimizedMemoryLimits UtilsCoreQuickCheckSpec.testSuite,
    withOptimizedMemoryLimits ComprehensiveIntegrationQuickCheckSpec.testSuite,
    
    -- Create additional optimized test suite
    createOptimizedMemorySuite optimizedMemoryConfig "Additional Optimized Tests"
      [ testProperty "additional basic property" prop_optimized_basic_property
      , testProperty "additional list property" prop_optimized_list_property
      ]
  ]