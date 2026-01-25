{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.ConciseTestSuite where



import Test.Tasty (TestTree, testGroup)
import qualified Test.Unit.ConciseUtilsQuickCheckSpec as Utils
import qualified Test.Unit.ConciseSourceLocationQuickCheckSpec as SourceLocation
import qualified Test.Unit.ConciseParserQuickCheckSpec as Parser
import qualified Test.Unit.ConciseCompilerQuickCheckSpec as Compiler
import qualified Test.Unit.ConciseErrorHandlerQuickCheckSpec as ErrorHandler
import qualified Test.Unit.ConciseOwnershipQuickCheckSpec as Ownership
import qualified Test.Unit.ConciseDependenciesQuickCheckSpec as Dependencies
import qualified Test.Unit.ConciseIntegrationQuickCheckSpec as Integration
import qualified Test.Unit.CodeGenerationQuickCheckSpec as CodeGeneration

conciseTestSuite :: TestTree
conciseTestSuite = testGroup "Concise Typus Test Suite (48 tests)"
  [ testGroup "Core Modules"
    [ Utils.tests
    , SourceLocation.tests
    , Parser.tests
    , Compiler.tests
    , ErrorHandler.tests
    , Ownership.tests
    , Dependencies.tests
    , CodeGeneration.tests
    ]
  , testGroup "Integration Tests"
    [ Integration.tests
    ]
  ]

tests :: TestTree
tests = conciseTestSuite