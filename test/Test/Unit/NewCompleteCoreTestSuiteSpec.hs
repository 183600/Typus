{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewCompleteCoreTestSuiteSpec where

import Test.Tasty
import qualified Data.List as L
import qualified Test.Unit.NewCoreUtilsQuickCheckSpec as Utils
import qualified Test.Unit.NewCoreSourceLocationQuickCheckSpec as SourceLocation
import qualified Test.Unit.NewCoreParserQuickCheckSpec as Parser
import qualified Test.Unit.NewComprehensiveCoreQuickCheckSpec as Comprehensive
import qualified Test.Unit.NewCoreBoundaryConditionsQuickCheckSpec as Boundary
import qualified Test.Unit.NewCorePerformanceQuickCheckSpec as Performance
import qualified Test.Unit.NewCoreErrorHandlingQuickCheckSpec as ErrorHandling

-- ============================================================================
-- Complete Core Test Suite
-- ============================================================================
-- This is the main entry point for L.all new core module tests.
-- It includes:
-- 1. Utils module tests (string processing, splitting, comments, etc.)
-- 2. SourceLocation module tests (position tracking, span management)
-- 3. Parser module tests (Typus language parsing)
-- 4. Comprehensive integration tests (cross-module functionality)
-- 5. Boundary condition tests (edge cases L.and limits)
-- 6. Performance tests (efficiency L.and scalability)
-- 7. Error handling tests (robustness L.and recovery)
--
-- Total: 7 test modules with comprehensive QuickCheck property testing
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "New Complete Core Test Suite (2025)"
  [ Utils.testSuite
  , SourceLocation.testSuite
  , Parser.testSuite
  , Comprehensive.testSuite
  , Boundary.testSuite
  , Performance.testSuite
  , ErrorHandling.testSuite
  ]