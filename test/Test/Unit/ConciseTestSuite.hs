{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.ConciseTestSuite where



import Test.Tasty (TestTree)
import TestSupport.MemoryLimits (withAggressiveMemoryLimits, aggressiveMemoryLimitedTestGroup)
import qualified Test.Unit.SimpleQuickCheckTestSuite as Utils
import qualified Test.Unit.ConciseSourceLocationQuickCheckSpec as SourceLocation
import qualified Test.Unit.ConciseParserQuickCheckSpec as Parser
import qualified Test.Unit.ConciseCompilerQuickCheckSpec as Compiler
import qualified Test.Unit.ConciseErrorHandlerQuickCheckSpec as ErrorHandler
import qualified Test.Unit.ConciseOwnershipQuickCheckSpec as Ownership
import qualified Test.Unit.ConciseDependenciesQuickCheckSpec as Dependencies
import qualified Test.Unit.ConciseIntegrationQuickCheckSpec as Integration
import qualified Test.Unit.CodeGenerationQuickCheckSpec as CodeGeneration

conciseTestSuite :: TestTree
conciseTestSuite = aggressiveMemoryLimitedTestGroup "Concise Typus Test Suite (48 tests) - Memory Optimized"
  [ aggressiveMemoryLimitedTestGroup "Core Modules"
    [ withAggressiveMemoryLimits Utils.tests
    , withAggressiveMemoryLimits SourceLocation.tests
    , withAggressiveMemoryLimits Parser.tests
    , withAggressiveMemoryLimits Compiler.tests
    , withAggressiveMemoryLimits ErrorHandler.tests
    , withAggressiveMemoryLimits Ownership.tests
    , withAggressiveMemoryLimits Dependencies.tests
    , withAggressiveMemoryLimits CodeGeneration.tests
    ]
  , aggressiveMemoryLimitedTestGroup "Integration Tests"
    [ withAggressiveMemoryLimits Integration.tests
    ]
  ]

tests :: TestTree
tests = conciseTestSuite