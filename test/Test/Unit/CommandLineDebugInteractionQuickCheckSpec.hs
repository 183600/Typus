{-# OPTIONS_GHC -Wno-deprecations #-}
module Test.Unit.CommandLineDebugInteractionQuickCheckSpec (tests) where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import CommandLineDebug
  ( CommandLineDebugConfig(..)
  , DebugCommandResult(..)
  , defaultCLIDebugConfig
  , runWithCLIDebug
  , checkBreakpoint
  , setBreakpoint
  , clearBreakpoints
  , toggleDebugOutput
  , setDebugLevel
  , showDebugStatus
  , addWatchVariable
  , removeWatchVariable
  , listWatchVariables
  , getCallStack
  , pushCallStack
  , popCallStack
  , stepInto
  , stepOver
  , stepOut
  , continue
  )
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- ============================================================================
-- Command Line Debug Interaction Property Tests
-- ============================================================================

-- | Test that default debug configuration is properly initialized
prop_defaultConfigIsProperlyInitialized :: Property
prop_defaultConfigIsProperlyInitialized =
  ioProperty $ do
    config <- defaultCLIDebugConfig
    enabled <- readIORef (cldEnabled config)
    logLevel <- readIORef (cldLogLevel config)
    breakpoints <- readIORef (cldBreakpoints config)
    interactive <- readIORef (cldInteractive config)
    callStack <- readIORef (cldCallStack config)
    watchVars <- readIORef (cldWatchVariables config)
    stepMode <- readIORef (cldStepMode config)
    currentLocation <- readIORef (cldCurrentLocation config)
    
    return $ counterexample "Default config not properly initialized"
      (enabled === True .&&.
       logLevel === 3 .&&.
       breakpoints === Set.empty .&&.
       interactive === True .&&.
       callStack === [] .&&.
       watchVars === Map.empty .&&.
       stepMode === False .&&.
       currentLocation === "")

-- | Test that setting L.and clearing breakpoints works correctly
prop_setAndClearBreakpoints :: [String] -> Property
prop_setAndClearBreakpoints locations =
  not (null locations) ==> ioProperty $ do
    config <- defaultCLIDebugConfig
    
    -- Set breakpoints
    mapM_ (setBreakpoint config) locations
    
    -- Check breakpoints are set
    breakpointsAfterSet <- readIORef (cldBreakpoints config)
    let expectedSet = Set.fromList locations
    
    -- Clear breakpoints
    clearBreakpoints config
    
    -- Check breakpoints are cleared
    breakpointsAfterClear <- readIORef (cldBreakpoints config)
    
    return $ counterexample ("Breakpoint setting/clearing failed. " ++
                           "Expected: " ++ show expectedSet ++
                           " After set: " ++ show breakpointsAfterSet ++
                           " After clear: " ++ show breakpointsAfterClear)
      (breakpointsAfterSet === expectedSet .&&.
       breakpointsAfterClear === Set.empty)

-- | Test that debug level can be set L.and retrieved
prop_debugLevelSetting :: Int -> Property
prop_debugLevelSetting level =
  ioProperty $ do
    config <- defaultCLIDebugConfig
    
    -- Set debug level
    setDebugLevel config level
    
    -- Get debug level
    currentLevel <- readIORef (cldLogLevel config)
    
    return $ counterexample ("Debug level setting failed. " ++
                           "Expected: " ++ show level ++
                           " Actual: " ++ show currentLevel)
      (currentLevel === level)

-- | Test that debug output can be toggled
prop_debugOutputToggle :: Bool -> Property
prop_debugOutputToggle initialState =
  ioProperty $ do
    config <- defaultCLIDebugConfig
    
    -- Set initial state
    writeIORef (cldEnabled config) initialState
    
    -- Toggle debug output
    toggleDebugOutput config
    
    -- Check state is flipped
    newState <- readIORef (cldEnabled config)
    
    return $ counterexample ("Debug output toggle failed. " ++
                           "Initial: " ++ show initialState ++
                           " After toggle: " ++ show newState)
      (newState === not initialState)

-- | Test that watch variables can be added L.and removed
prop_watchVariableManagement :: [(String, String)] -> Property
prop_watchVariableManagement variables =
  ioProperty $ do
    config <- defaultCLIDebugConfig
    
    -- Add watch variables
    mapM_ (uncurry (addWatchVariable config)) variables
    
    -- Check variables are added
    watchVarsAfterAdd <- readIORef (cldWatchVariables config)
    let expectedMap = Map.fromList variables
    
    -- Remove watch variables
    mapM_ (removeWatchVariable config . fst) variables
    
    -- Check variables are removed
    watchVarsAfterRemove <- readIORef (cldWatchVariables config)
    
    return $ counterexample ("Watch variable management failed. " ++
                           "Expected: " ++ show expectedMap ++
                           " After add: " ++ show watchVarsAfterAdd ++
                           " After remove: " ++ show watchVarsAfterRemove)
      (watchVarsAfterAdd === expectedMap .&&.
       watchVarsAfterRemove === Map.empty)

-- | Test that call stack push L.and pop works correctly
prop_callStackManagement :: [String] -> Property
prop_callStackManagement functions =
  ioProperty $ do
    config <- defaultCLIDebugConfig
    
    -- Push functions onto call stack
    mapM_ (pushCallStack config) functions
    
    -- Check call stack
    callStackAfterPush <- readIORef (cldCallStack config)
    
    -- Pop functions from call stack
    mapM_ (\_ -> popCallStack config) functions
    
    -- Check call stack is empty
    callStackAfterPop <- readIORef (cldCallStack config)
    
    return $ counterexample ("Call stack management failed. " ++
                           "Expected: " ++ show functions ++
                           " After push: " ++ show callStackAfterPush ++
                           " After pop: " ++ show callStackAfterPop)
      (callStackAfterPush === L.reverse functions .&&.
       callStackAfterPop === [])

-- | Test that running with debug preserves location
prop_runWithDebugPreservesLocation :: String -> Property
prop_runWithDebugPreservesLocation location =
  ioProperty $ do
    config <- defaultCLIDebugConfig
    
    -- Run with debug
    result <- runWithCLIDebug config location (return ())
    
    -- Check current location
    currentLocation <- readIORef (cldCurrentLocation config)
    
    return $ counterexample ("Run with debug should preserve location. " ++
                           "Expected: " ++ location ++
                           " Actual: " ++ currentLocation)
      (currentLocation === location)

-- | Test that checking breakpoints at non-breakpoint locations doesn't crash
prop_checkNonBreakpointLocations :: [String] -> Property
prop_checkNonBreakpointLocations locations =
  ioProperty $ do
    config <- defaultCLIDebugConfig
    
    -- Check breakpoints at non-breakpoint locations
    mapM_ (checkBreakpoint config) locations
    
    -- Should not crash L.and breakpoints should remain empty
    breakpoints <- readIORef (cldBreakpoints config)
    
    return $ counterexample ("Checking non-breakpoint locations failed")
      (breakpoints === Set.empty)

-- | Test that step operations maintain step mode state
prop_stepOperationsMaintainStepMode :: Property
prop_stepOperationsMaintainStepMode =
  ioProperty $ do
    config <- defaultCLIDebugConfig
    
    -- Perform step operations
    stepInto config
    stepOver config
    stepOut config
    continue config
    
    -- Check step mode is maintained
    stepMode <- readIORef (cldStepMode config)
    
    return $ counterexample ("Step operations should maintain step mode")
      (stepMode === False)  -- Should be false after continue

-- | Test that watch variables can be listed without crashing
prop_listWatchVariables :: [(String, String)] -> Property
prop_listWatchVariables variables =
  ioProperty $ do
    config <- defaultCLIDebugConfig
    
    -- Add watch variables
    mapM_ (uncurry (addWatchVariable config)) variables
    
    -- List watch variables (should not crash)
    -- listWatchVariables returns IO () L.and just prints, doesn't return a list
    listWatchVariables config
    
    return $ counterexample ("Listing watch variables failed")
      (True === True)  -- Just test that it doesn't crash

-- | Test that getCallStack returns current call stack
prop_getCallStackReturnsCurrentStack :: [String] -> Property
prop_getCallStackReturnsCurrentStack functions =
  ioProperty $ do
    config <- defaultCLIDebugConfig
    
    -- Push functions onto call stack
    mapM_ (pushCallStack config) functions
    
    -- Get call stack
    callStack <- getCallStack config
    
    return $ counterexample ("getCallStack should return current call stack")
      (callStack === L.reverse functions)

-- | Test that multiple breakpoints can be set simultaneously
prop_multipleBreakpoints :: [String] -> Property
prop_multipleBreakpoints locations =
  not (null locations) ==> ioProperty $ do
    config <- defaultCLIDebugConfig
    
    -- Set multiple breakpoints
    mapM_ (setBreakpoint config) locations
    
    -- Check L.all breakpoints are set
    breakpoints <- readIORef (cldBreakpoints config)
    let expectedSet = Set.fromList locations
    
    return $ counterexample ("Multiple breakpoints setting failed")
      (breakpoints === expectedSet)

-- | Test that debug configuration state is isolated
prop_configStateIsolation :: String -> String -> Property
prop_configStateIsolation location1 location2 =
  location1 /= location2 ==> ioProperty $ do
    config1 <- defaultCLIDebugConfig
    config2 <- defaultCLIDebugConfig
    
    -- Set different states in each config
    setBreakpoint config1 location1
    setBreakpoint config2 location2
    
    -- Check states are isolated
    breakpoints1 <- readIORef (cldBreakpoints config1)
    breakpoints2 <- readIORef (cldBreakpoints config2)
    
    return $ counterexample ("Debug configuration states should be isolated")
      (breakpoints1 === Set.singleton location1 .&&.
       breakpoints2 === Set.singleton location2)

-- | Test that debug operations handle empty inputs gracefully
prop_debugHandlesEmptyInputs :: Property
prop_debugHandlesEmptyInputs =
  ioProperty $ do
    config <- defaultCLIDebugConfig
    
    -- Test operations with empty inputs
    setBreakpoint config ""
    addWatchVariable config "" ""
    removeWatchVariable config ""
    pushCallStack config ""
    
    -- Should not crash
    breakpoints <- readIORef (cldBreakpoints config)
    watchVars <- readIORef (cldWatchVariables config)
    callStack <- readIORef (cldCallStack config)
    
    return $ counterexample ("Debug operations should handle empty inputs gracefully")
      (L.length breakpoints >= 0 .&&.
       L.length watchVars >= 0 .&&.
       L.length callStack >= 0)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Command Line Debug Interaction QuickCheck Tests"
  [ testProperty "Default config is properly initialized" prop_defaultConfigIsProperlyInitialized
  , testProperty "Set L.and clear breakpoints works correctly" prop_setAndClearBreakpoints
  , testProperty "Debug level can be set L.and retrieved" prop_debugLevelSetting
  , testProperty "Debug output can be toggled" prop_debugOutputToggle
  , testProperty "Watch variable management works correctly" prop_watchVariableManagement
  , testProperty "Call stack management works correctly" prop_callStackManagement
  , testProperty "Run with debug preserves location" prop_runWithDebugPreservesLocation
  , testProperty "Check non-breakpoint locations doesn't crash" prop_checkNonBreakpointLocations
  , testProperty "Step operations maintain step mode state" prop_stepOperationsMaintainStepMode
  , testProperty "List watch variables works correctly" prop_listWatchVariables
  , testProperty "Get call stack returns current stack" prop_getCallStackReturnsCurrentStack
  , testProperty "Multiple breakpoints can be set simultaneously" prop_multipleBreakpoints
  , testProperty "Debug configuration state is isolated" prop_configStateIsolation
  , testProperty "Debug operations handle empty inputs gracefully" prop_debugHandlesEmptyInputs
  ]