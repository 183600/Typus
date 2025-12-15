{-# LANGUAGE CPP #-}

module Test.Unit.DebugIntegrationPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import System.IO.Silently (capture_)

import DebugIntegration
import CommandLineDebug (defaultCLIDebugConfig)

prop_withDebugging_executes_action :: Property
prop_withDebugging_executes_action =
  ioProperty $ do
    config <- defaultCLIDebugConfig
    result <- withDebugging config "test" (return 42)
    return (result === 42)

prop_debugParseStep_executes_action :: Property
prop_debugParseStep_executes_action =
  ioProperty $ do
    config <- defaultCLIDebugConfig
    result <- debugParseStep config "test.typus" (return True)
    return (result === True)

prop_debugCompileStep_executes_action :: Property
prop_debugCompileStep_executes_action =
  ioProperty $ do
    config <- defaultCLIDebugConfig
    result <- debugCompileStep config "test.typus" (return "compiled")
    return (result === "compiled")

prop_debugOwnershipStep_executes_action :: Property
prop_debugOwnershipStep_executes_action =
  ioProperty $ do
    config <- defaultCLIDebugConfig
    result <- debugOwnershipStep config "test.typus" (return ([] :: [String]))
    return (result === ([] :: [String]))

prop_createDebugBreakpoints_returns_list :: Property
prop_createDebugBreakpoints_returns_list =
  ioProperty $ do
    config <- defaultCLIDebugConfig
    createDebugBreakpoints config
    return (property True)

prop_setupCompilerDebugging_executes :: Property
prop_setupCompilerDebugging_executes =
  ioProperty $ do
    _ <- setupCompilerDebugging
    return (property True)

prop_debugCompilerStart_executes :: Property
prop_debugCompilerStart_executes =
  ioProperty $ do
    config <- defaultCLIDebugConfig
    debugCompilerStart config "test.typus"
    return (property True)

prop_debugCompilerEnd_executes :: Property
prop_debugCompilerEnd_executes =
  ioProperty $ do
    config <- defaultCLIDebugConfig
    debugCompilerEnd config "compilation complete"
    return (property True)

tests :: TestTree
tests = testGroup "DebugIntegration Properties QuickCheck Tests"
  [ fastProperty "withDebugging executes action" prop_withDebugging_executes_action
  , fastProperty "debugParseStep executes action" prop_debugParseStep_executes_action
  , fastProperty "debugCompileStep executes action" prop_debugCompileStep_executes_action
  , fastProperty "debugOwnershipStep executes action" prop_debugOwnershipStep_executes_action
  , fastProperty "createDebugBreakpoints returns list" prop_createDebugBreakpoints_returns_list
  , fastProperty "setupCompilerDebugging executes" prop_setupCompilerDebugging_executes
  , fastProperty "debugCompilerStart executes" prop_debugCompilerStart_executes
  , fastProperty "debugCompilerEnd executes" prop_debugCompilerEnd_executes
  ]
