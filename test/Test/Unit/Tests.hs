module Test.Unit.Tests where



import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.MemoryLimits (withMemoryLimits, memoryLimitedTestGroup)

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

-- Import modified test modules
import qualified Test.Unit.FinalQuickCheckTestSuite as FinalQuickCheckTestSuite
import qualified Test.Unit.SimpleQuickCheckTestSuite as SimpleQuickCheckTestSuite

-- Basic test properties
prop_basic_property :: String -> Property
prop_basic_property s = property $ length s >= 0

tests :: TestTree
tests = memoryLimitedTestGroup "Test.Unit.Tests Tests"
  [ withMemoryLimits $ testProperty "basic property" prop_basic_property,
    withMemoryLimits ConciseTestSuite.tests,
    withMemoryLimits testListProperties,
    withMemoryLimits BasicQuickCheckTestSuite.tests,
    withMemoryLimits BasicQuickCheckTestsSpec.tests,
    withMemoryLimits BoundaryConditionAdvancedQuickCheckSpec.tests,
    withMemoryLimits BoundaryConditionComprehensiveSpec.boundaryConditionComprehensiveTests,
    withMemoryLimits CodeGenerationQuickCheckSpec.tests,
    withMemoryLimits testParserErrorRecovery,
    withMemoryLimits testParserDirectives,
    withMemoryLimits AdvancedTextProcessingSpec.tests,  -- AdvancedTextProcessingSpec tests
    withMemoryLimits NewAdditionalParserQuickCheckTestSpec.newAdditionalParserQuickCheckTestSpec,  -- New Additional Parser QuickCheck Tests
    withMemoryLimits FinalQuickCheckTestSuite.tests,  -- Final QuickCheck Test Suite
    withMemoryLimits SimpleQuickCheckTestSuite.tests  -- Simple QuickCheck Test Suite
  ]