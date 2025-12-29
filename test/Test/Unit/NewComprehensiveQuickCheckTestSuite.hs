{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewComprehensiveQuickCheckTestSuite where

import Test.Tasty
import Test.Unit.NewUtilsQuickCheckPropertyTestsSpec (testSuite as utilsTests)
import Test.Unit.NewSourceLocationMathPropertiesSpec (testSuite as sourceLocationTests)
import Test.Unit.NewParserCombinatorPropertiesSpec (testSuite as parserTests)
import Test.Unit.NewStringProcessingBoundarySpec (testSuite as stringProcessingTests)
import Test.Unit.NewErrorHandlerConsistencySpec (testSuite as errorHandlerTests)

-- ============================================================================
-- Comprehensive QuickCheck Test Suite
-- ============================================================================

-- | This module combines multiple QuickCheck test suites into a comprehensive
-- test collection covering core functionality of the Typus compiler.
--
-- The test suite includes:
-- 1. Utils module tests - string processing utilities
-- 2. SourceLocation tests - position and span calculations
-- 3. Parser tests - directive and data structure properties
-- 4. String processing tests - boundary conditions and edge cases
-- 5. Error handler tests - consistency and state management
--
-- Each test suite focuses on property-based testing using QuickCheck to
-- verify invariants and mathematical properties of the core modules.

testSuite :: TestTree
testSuite = testGroup "New Comprehensive QuickCheck Test Suite"
  [ utilsTests
  , sourceLocationTests
  , parserTests
  , stringProcessingTests
  , errorHandlerTests
  ]