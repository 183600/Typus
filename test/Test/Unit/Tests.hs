module Test.Unit.Tests where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Test.Unit.NewCorePropertiesQuickCheckSpec as NewCoreProperties
import qualified Test.Unit.ComprehensiveCoreModulesQuickCheckSpec as ComprehensiveCoreModules
import qualified Test.Unit.NewQuickCheckTestSuiteSpec as NewQuickCheckTestSuite
import qualified Test.Unit.AdditionalQuickCheckTestSuiteSpec as AdditionalQuickCheckTestSuite
import qualified Test.Unit.NewComprehensiveQuickCheckSpec as NewComprehensiveQuickCheck

-- Import new test modules
import qualified Test.Unit.NewSourceLocationTestSpec as NewSourceLocationTest
import qualified Test.Unit.NewParserTestSpec as NewParserTest
import qualified Test.Unit.NewCompilerTestSpec as NewCompilerTest
import qualified Test.Unit.NewErrorHandlerTestSpec as NewErrorHandlerTest
import qualified Test.Unit.NewUtilsTestSpec as NewUtilsTest
import qualified Test.Unit.NewOwnershipTestSpec as NewOwnershipTest
import qualified Test.Unit.NewDependenciesTestSpec as NewDependenciesTest

-- Import core functionality test modules
import qualified Test.Unit.ParserCoreFunctionalitySpec as ParserCoreFunctionality
import qualified Test.Unit.CompilerCoreFunctionalitySpec as CompilerCoreFunctionality
import qualified Test.Unit.UtilsCoreFunctionalitySpec as UtilsCoreFunctionality
import qualified Test.Unit.IntegrationQuickCheckSpec as IntegrationQuickCheck

-- Import new comprehensive test modules
import qualified Test.Unit.ComprehensiveCabalTestSuite as ComprehensiveCabalTestSuite
import qualified Test.Unit.AdditionalCabalQuickCheckTests as AdditionalCabalQuickCheckTests

-- Basic test properties
prop_basic_property :: String -> Property
prop_basic_property s = property $ length s >= 0

tests :: TestTree
tests = testGroup "Test.Unit.Tests Tests"
  [ testProperty "basic property" prop_basic_property,
    NewCoreProperties.tests,
    ComprehensiveCoreModules.tests,
    NewQuickCheckTestSuite.tests,
    AdditionalQuickCheckTestSuite.tests,
    NewComprehensiveQuickCheck.tests,
    
    -- New comprehensive test modules
    NewSourceLocationTest.tests,
    NewParserTest.tests,
    NewCompilerTest.tests,
    NewErrorHandlerTest.tests,
    NewUtilsTest.tests,
    NewOwnershipTest.tests,
    NewDependenciesTest.tests,
    
    -- Core functionality test modules
    ParserCoreFunctionality.parserCoreFunctionalityTests,
    CompilerCoreFunctionality.compilerCoreFunctionalityTests,
    UtilsCoreFunctionality.utilsCoreFunctionalityTests,
    IntegrationQuickCheck.integrationQuickCheckTests,
    
    -- New comprehensive test modules
    ComprehensiveCabalTestSuite.tests,
    AdditionalCabalQuickCheckTests.tests
  ]
