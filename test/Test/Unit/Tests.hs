module Test.Unit.Tests where



import Test.Tasty
import Test.Tasty.QuickCheck

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

-- Basic test properties
prop_basic_property :: String -> Property
prop_basic_property s = property $ length s >= 0

tests :: TestTree
tests = testGroup "Test.Unit.Tests Tests"
  [ testProperty "basic property" prop_basic_property,
    ConciseTestSuite.tests,
    testListProperties,
    BasicQuickCheckTestSuite.tests,
    BasicQuickCheckTestsSpec.tests,
    BoundaryConditionAdvancedQuickCheckSpec.tests,
    BoundaryConditionComprehensiveSpec.boundaryConditionComprehensiveTests,
    CodeGenerationQuickCheckSpec.tests,
    testParserErrorRecovery,
    testParserDirectives,
    AdvancedTextProcessingSpec.tests,  -- AdvancedTextProcessingSpec tests
    NewAdditionalParserQuickCheckTestSpec.newAdditionalParserQuickCheckTestSpec  -- New Additional Parser QuickCheck Tests
  ]