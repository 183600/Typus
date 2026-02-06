{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.ConciseTestSuite where



import Test.Tasty (TestTree, testGroup)
import TestSupport.MemoryLimits (withMemoryLimits, memoryLimitedTestGroup)
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
conciseTestSuite = memoryLimitedTestGroup "Concise Typus Test Suite (48 tests)"
  [ memoryLimitedTestGroup "Core Modules"
    [ withMemoryLimits Utils.tests
    , withMemoryLimits SourceLocation.tests
    , withMemoryLimits Parser.tests
    , withMemoryLimits Compiler.tests
    , withMemoryLimits ErrorHandler.tests
    , withMemoryLimits Ownership.tests
    , withMemoryLimits Dependencies.tests
    , withMemoryLimits CodeGeneration.tests
    ]
  , memoryLimitedTestGroup "Integration Tests"
    [ withMemoryLimits Integration.tests
    ]
  ]

tests :: TestTree
tests = conciseTestSuite