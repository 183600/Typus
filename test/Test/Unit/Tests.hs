module Test.Unit.Tests where



import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.MemoryLimits (withAggressiveMemoryLimits, aggressiveMemoryLimitedTestGroup)

-- Only import essential existing test modules
import qualified Test.Unit.ConciseTestSuite as ConciseTestSuite
import Test.Unit.TestListPropertiesSpec (testListProperties)

-- Import modified test modules
import qualified Test.Unit.BasicQuickCheckTestSuite as BasicQuickCheckTestSuite
import qualified Test.Unit.BasicQuickCheckTestsSpec as BasicQuickCheckTestsSpec
import qualified Test.Unit.BoundaryConditionAdvancedQuickCheckSpec as BoundaryConditionAdvancedQuickCheckSpec
import qualified Test.Unit.BoundaryConditionComprehensiveSpec as BoundaryConditionComprehensiveSpec
import qualified Test.Unit.CodeGenerationQuickCheckSpec as CodeGenerationQuickCheckSpec
import Test.Unit.TestParserErrorRecoverySpec (testParserErrorRecovery)
import Test.Unit.TestParserDirectivesSpec (testParserDirectives)

-- Import AdvancedTextProcessingSpec
import qualified Test.Unit.AdvancedTextProcessingSpec as AdvancedTextProcessingSpec

-- Import NewAdditionalParserQuickCheckTestSpec
import qualified Test.Unit.NewAdditionalParserQuickCheckTestSpec as NewAdditionalParserQuickCheckTestSpec

-- Import EnhancedMemoryOptimizedTestSuite
import qualified Test.Unit.EnhancedMemoryOptimizedTestSuite as EnhancedMemoryOptimizedTestSuite

-- Import AdvancedMemoryOptimizedTestSuite
import qualified Test.Unit.AdvancedMemoryOptimizedTestSuite as AdvancedMemoryOptimizedTestSuite

-- Import modified test modules
import qualified Test.Unit.FinalQuickCheckTestSuite as FinalQuickCheckTestSuite
import qualified Test.Unit.SimpleQuickCheckTestSuite as SimpleQuickCheckTestSuite

-- Basic test properties
prop_basic_property :: String -> Property
prop_basic_property s = property $ length s >= 0

tests :: TestTree
tests = aggressiveMemoryLimitedTestGroup "Test.Unit.Tests Tests (Memory Optimized)"
  [ withAggressiveMemoryLimits $ testProperty "basic property" prop_basic_property,
    withAggressiveMemoryLimits ConciseTestSuite.tests,
    withAggressiveMemoryLimits testListProperties,
    withAggressiveMemoryLimits BasicQuickCheckTestSuite.tests,
    withAggressiveMemoryLimits BasicQuickCheckTestsSpec.tests,
    withAggressiveMemoryLimits BoundaryConditionAdvancedQuickCheckSpec.tests,
    withAggressiveMemoryLimits BoundaryConditionComprehensiveSpec.boundaryConditionComprehensiveTests,
    withAggressiveMemoryLimits CodeGenerationQuickCheckSpec.tests,
    withAggressiveMemoryLimits testParserErrorRecovery,
    withAggressiveMemoryLimits testParserDirectives,
    withAggressiveMemoryLimits AdvancedTextProcessingSpec.tests,  -- AdvancedTextProcessingSpec tests
    withAggressiveMemoryLimits NewAdditionalParserQuickCheckTestSpec.newAdditionalParserQuickCheckTestSpec,  -- New Additional Parser QuickCheck Tests
    withAggressiveMemoryLimits EnhancedMemoryOptimizedTestSuite.tests,  -- Enhanced Memory Optimized Test Suite
    withAggressiveMemoryLimits AdvancedMemoryOptimizedTestSuite.tests,  -- Advanced Memory Optimized Test Suite
    withAggressiveMemoryLimits FinalQuickCheckTestSuite.tests,  -- Final QuickCheck Test Suite
    withAggressiveMemoryLimits SimpleQuickCheckTestSuite.tests  -- Simple QuickCheck Test Suite
  ]