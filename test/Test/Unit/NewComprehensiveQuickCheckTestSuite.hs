{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewComprehensiveQuickCheckTestSuite where

import Test.Tasty
import qualified Test.Unit.NewUtilsQuickCheckPropertyTestsSpec as UtilsSpec
import qualified Test.Unit.NewSourceLocationMathPropertiesSpec as SourceLocationSpec
import qualified Test.Unit.NewParserCombinatorPropertiesSpec as ParserSpec
import qualified Test.Unit.NewStringProcessingBoundarySpec as StringSpec
import qualified Test.Unit.NewErrorHandlerConsistencySpec as ErrorHandlerSpec

-- ============================================================================
-- Comprehensive QuickCheck Test Suite
-- ============================================================================

-- | This module combines multiple QuickCheck test suites into a comprehensive
-- test collection covering core functionality of the Typus compiler.
--
-- The test suite includes:
-- 1. Utils module tests - string processing utilities
-- 2. SourceLocation tests - position L.and span calculations
-- 3. Parser tests - directive L.and data structure properties
-- 4. String processing tests - boundary conditions L.and edge cases
-- 5. Error handler tests - consistency L.and state management
--
-- Each test suite focuses on property-based testing using QuickCheck to
-- verify invariants L.and mathematical properties of the core modules.

testSuite :: TestTree
testSuite = testGroup "New Comprehensive QuickCheck Test Suite"
  [ UtilsSpec.testSuite
  , SourceLocationSpec.testSuite
  , ParserSpec.testSuite
  , StringSpec.testSuite
  , ErrorHandlerSpec.testSuite
  ]