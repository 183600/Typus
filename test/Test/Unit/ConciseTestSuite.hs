{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.ConciseTestSuite where

import Test.Tasty (TestTree, testGroup)
import Test.Unit.ConciseUtilsQuickCheckSpec (tests)
import Test.Unit.ConciseSourceLocationQuickCheckSpec (tests)
import Test.Unit.ConciseParserQuickCheckSpec (tests)
import Test.Unit.ConciseCompilerQuickCheckSpec (tests)
import Test.Unit.ConciseErrorHandlerQuickCheckSpec (tests)
import Test.Unit.ConciseOwnershipQuickCheckSpec (tests)
import Test.Unit.ConciseDependenciesQuickCheckSpec (tests)
import Test.Unit.ConciseIntegrationQuickCheckSpec (tests)

conciseTestSuite :: TestTree
conciseTestSuite = testGroup "Concise Typus Test Suite (48 tests)"
  [ testGroup "Core Modules"
    [ testGroup "Utils Module" tests
    , testGroup "SourceLocation Module" tests
    , testGroup "Parser Module" tests
    , testGroup "Compiler Module" tests
    , testGroup "ErrorHandler Module" tests
    , testGroup "Ownership Module" tests
    , testGroup "Dependencies Module" tests
    ]
  , testGroup "Integration Tests"
    [ testGroup "End-to-End Tests" tests
    ]
  ]