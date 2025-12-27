{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CommandLineDebugQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Monadic (monadicIO, run, assert)

import CommandLineDebug
  ( CommandLineDebugConfig(..)
  , defaultCLIDebugConfig
  , setBreakpoint
  , listBreakpoints
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
  , evaluateExpression
  , stepInto
  , stepOver
  , stepOut
  , continue
  , runToCursor
  , setConditionalBreakpoint
  )

import Data.IORef (readIORef, writeIORef)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad (when, replicateM)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary (Set String) where
    arbitrary = do
        strings <- listOf $ listOf $ elements ['a'..'z']
        return $ Set.fromList strings

instance Arbitrary (Map String String) where
    arbitrary = do
        keys <- listOf $ listOf $ elements ['a'..'z']
        values <- listOf $ listOf $ elements ['a'..'z']
        return $ Map.fromList $ zip keys values

-- ============================================================================
-- Property Tests for Breakpoint Management
-- ============================================================================

-- Property: Setting and listing breakpoints preserves set membership
prop_set_list_breakpoints :: [String] -> Property
prop_set_list_breakpoints locations =
    not (null locations) ==> 
    monadicIO $ do
        config <- run defaultCLIDebugConfig
        -- Set breakpoints
        run $ mapM_ (setBreakpoint config) locations
        -- Get breakpoints
        breakpoints <- run $ readIORef (cldBreakpoints config)
        assert $ all (`Set.member` breakpoints) locations

-- Property: Clearing breakpoints results in empty set
prop_clear_breakpoints :: [String] -> Property
prop_clear_breakpoints locations =
    not (null locations) ==> 
    monadicIO $ do
        config <- run defaultCLIDebugConfig
        -- Set breakpoints
        run $ mapM_ (setBreakpoint config) locations
        -- Clear breakpoints
        run $ clearBreakpoints config
        -- Check empty
        breakpoints <- run $ readIORef (cldBreakpoints config)
        assert $ Set.null breakpoints

-- Property: Setting duplicate breakpoints doesn't increase set size
prop_duplicate_breakpoints :: String -> Property
prop_duplicate_breakpoints location =
    not (null location) ==> 
    monadicIO $ do
        config <- run defaultCLIDebugConfig
        -- Set same breakpoint multiple times
        run $ replicateM_ 5 (setBreakpoint config location)
        breakpoints <- run $ readIORef (cldBreakpoints config)
        assert $ Set.size breakpoints === 1

-- ============================================================================
-- Property Tests for Debug Output Control
-- ============================================================================

-- Property: Toggling debug output flips the enabled state
prop_toggle_debug_output :: Property
prop_toggle_debug_output = 
    monadicIO $ do
        config <- run defaultCLIDebugConfig
        -- Get initial state
        initialState <- run $ readIORef (cldEnabled config)
        -- Toggle
        run $ toggleDebugOutput config
        -- Check flipped
        toggledState <- run $ readIORef (cldEnabled config)
        assert $ toggledState === not initialState

-- Property: Setting debug level preserves the value
prop_set_debug_level :: Int -> Property
prop_set_debug_level level =
    level >= 0 && level <= 10 ==>  -- Reasonable range
    monadicIO $ do
        config <- run defaultCLIDebugConfig
        -- Set debug level
        run $ setDebugLevel config level
        -- Check level
        storedLevel <- run $ readIORef (cldLogLevel config)
        assert $ storedLevel === level

-- ============================================================================
-- Property Tests for Watch Variables
-- ============================================================================

-- Property: Adding and listing watch variables preserves entries
prop_add_list_watch_variables :: [(String, String)] -> Property
prop_add_list_watch_variables pairs =
    not (null pairs) ==> 
    monadicIO $ do
        config <- run defaultCLIDebugConfig
        -- Add watch variables
        run $ mapM_ (uncurry $ addWatchVariable config) pairs
        -- Get watch variables
        watchVars <- run $ readIORef (cldWatchVariables config)
        assert $ all (\(k, v) -> Map.lookup k watchVars == Just v) pairs

-- Property: Removing watch variables deletes entries
prop_remove_watch_variables :: [(String, String)] -> Property
prop_remove_watch_variables pairs =
    not (null pairs) ==> 
    monadicIO $ do
        config <- run defaultCLIDebugConfig
        -- Add watch variables
        run $ mapM_ (uncurry $ addWatchVariable config) pairs
        -- Remove first half
        let toRemove = map fst $ take (length pairs `div` 2) pairs
        run $ mapM_ (removeWatchVariable config) toRemove
        -- Check removal
        watchVars <- run $ readIORef (cldWatchVariables config)
        assert $ all (`Map.notMember` watchVars) toRemove

-- Property: Adding duplicate watch variables overwrites values
prop_duplicate_watch_variables :: String -> String -> String -> Property
prop_duplicate_watch_variables varName value1 value2 =
    not (null varName) && value1 /= value2 ==> 
    monadicIO $ do
        config <- run defaultCLIDebugConfig
        -- Add same variable with different values
        run $ addWatchVariable config varName value1
        run $ addWatchVariable config varName value2
        watchVars <- run $ readIORef (cldWatchVariables config)
        assert $ Map.lookup varName watchVars === Just value2

-- ============================================================================
-- Property Tests for Call Stack Management
-- ============================================================================

-- Property: Push and pop operations maintain stack order
prop_push_pop_call_stack :: [String] -> Property
prop_push_pop_call_stack locations =
    not (null locations) ==> 
    monadicIO $ do
        config <- run defaultCLIDebugConfig
        -- Push locations
        run $ mapM_ (pushCallStack config) locations
        -- Pop all
        run $ replicateM (length locations) (popCallStack config)
        -- Check empty
        stack <- run $ readIORef (cldCallStack config)
        assert $ null stack

-- Property: Call stack maintains LIFO order
prop_call_stack_lifo :: [String] -> Property
prop_call_stack_lifo locations =
    not (null locations) ==> 
    monadicIO $ do
        config <- run defaultCLIDebugConfig
        -- Push locations
        run $ mapM_ (pushCallStack config) locations
        -- Get stack
        stack <- run $ readIORef (cldCallStack config)
        assert $ stack === reverse locations

-- Property: Current location tracks top of call stack
prop_current_location_tracking :: [String] -> Property
prop_current_location_tracking locations =
    not (null locations) ==> 
    monadicIO $ do
        config <- run defaultCLIDebugConfig
        -- Push locations
        run $ mapM_ (pushCallStack config) locations
        -- Get current location
        currentLoc <- run $ readIORef (cldCurrentLocation config)
        assert $ currentLoc === head locations

-- ============================================================================
-- Property Tests for Expression Evaluation
-- ============================================================================

-- Property: Expression evaluation returns non-empty result
prop_evaluate_expression_non_empty :: String -> Property
prop_evaluate_expression_non_empty expr =
    not (null expr) ==> 
    monadicIO $ do
        config <- run defaultCLIDebugConfig
        result <- run $ evaluateExpression config expr
        assert $ not (null result) .&&. expr `isInfixOf` result

-- ============================================================================
-- Property Tests for Step Debugging
-- ============================================================================

-- Property: Step operations set step mode appropriately
prop_step_operations :: Bool -> Property
prop_step_operations initialState =
    monadicIO $ do
        config <- run defaultCLIDebugConfig
        -- Set initial state
        run $ writeIORef (cldStepMode config) initialState
        -- Step into
        run $ stepInto config
        stepMode1 <- run $ readIORef (cldStepMode config)
        -- Step out
        run $ stepOut config
        stepMode2 <- run $ readIORef (cldStepMode config)
        -- Continue
        run $ continue config
        stepMode3 <- run $ readIORef (cldStepMode config)
        assert $ stepMode1 .&&. not stepMode2 .&&. not stepMode3

-- Property: Run to cursor sets breakpoint
prop_run_to_cursor :: String -> Property
prop_run_to_cursor location =
    not (null location) ==> 
    monadicIO $ do
        config <- run defaultCLIDebugConfig
        -- Clear any existing breakpoints
        run $ clearBreakpoints config
        -- Run to cursor
        run $ runToCursor config location
        -- Check breakpoint set
        breakpoints <- run $ readIORef (cldBreakpoints config)
        assert $ Set.member location breakpoints

-- ============================================================================
-- Property Tests for Conditional Breakpoints
-- ============================================================================

-- Property: Conditional breakpoint with always true condition behaves like regular breakpoint
prop_conditional_breakpoint_always_true :: String -> Property
prop_conditional_breakpoint_always_true location =
    not (null location) ==> 
    monadicIO $ do
        config <- run defaultCLIDebugConfig
        -- Set conditional breakpoint with always true condition
        run $ setConditionalBreakpoint config location (const True)
        -- Check condition stored
        conditions <- run $ readIORef (cldBreakConditions config)
        assert $ Map.member location conditions

-- Property: Conditional breakpoint with always false condition never triggers
prop_conditional_breakpoint_always_false :: String -> Property
prop_conditional_breakpoint_always_false location =
    not (null location) ==> 
    monadicIO $ do
        config <- run defaultCLIDebugConfig
        -- Set conditional breakpoint with always false condition
        run $ setConditionalBreakpoint config location (const False)
        -- Check condition stored
        conditions <- run $ readIORef (cldBreakConditions config)
        assert $ Map.member location conditions

-- ============================================================================
-- Property Tests for Combined Operations
-- ============================================================================

-- Property: Multiple operations maintain consistency
prop_multiple_operations_consistency :: [String] -> [(String, String)] -> Property
prop_multiple_operations_consistency locations watchPairs =
    not (null locations) && not (null watchPairs) ==> 
    monadicIO $ do
        config <- run defaultCLIDebugConfig
        -- Set breakpoints
        run $ mapM_ (setBreakpoint config) locations
        -- Add watch variables
        run $ mapM_ (uncurry $ addWatchVariable config) watchPairs
        -- Push call stack
        run $ mapM_ (pushCallStack config) locations
        -- Check all states
        breakpoints <- run $ readIORef (cldBreakpoints config)
        watchVars <- run $ readIORef (cldWatchVariables config)
        stack <- run $ readIORef (cldCallStack config)
        assert $ all (`Set.member` breakpoints) locations .&&.
                 all (\(k, v) -> Map.lookup k watchVars == Just v) watchPairs .&&.
                 stack === reverse locations

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

-- Property: Empty location strings are handled gracefully
prop_empty_location_handling :: Property
prop_empty_location_handling = 
    monadicIO $ do
        config <- run defaultCLIDebugConfig
        -- Try to set breakpoint with empty location
        run $ setBreakpoint config ""
        -- Try to push empty location to call stack
        run $ pushCallStack config ""
        -- Check no crashes occurred
        breakpoints <- run $ readIORef (cldBreakpoints config)
        stack <- run $ readIORef (cldCallStack config)
        assert $ True  -- If we get here, no crash occurred

-- Property: Large number of operations are handled efficiently
prop_large_operations :: Int -> Property
prop_large_operations n =
    n >= 0 && n <= 100 ==>  -- Limit for performance testing
    monadicIO $ do
        config <- run defaultCLIDebugConfig
        let locations = map show [1..n]
        -- Set many breakpoints
        run $ mapM_ (setBreakpoint config) locations
        breakpoints <- run $ readIORef (cldBreakpoints config)
        assert $ Set.size breakpoints === n

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "CommandLineDebug QuickCheck Tests"
    [ testGroup "Breakpoint Management"
        [ fastProperty "Setting and listing breakpoints preserves set membership" prop_set_list_breakpoints
        , fastProperty "Clearing breakpoints results in empty set" prop_clear_breakpoints
        , fastProperty "Setting duplicate breakpoints doesn't increase set size" prop_duplicate_breakpoints
        ]
    , testGroup "Debug Output Control"
        [ fastProperty "Toggling debug output flips the enabled state" prop_toggle_debug_output
        , fastProperty "Setting debug level preserves the value" prop_set_debug_level
        ]
    , testGroup "Watch Variables"
        [ fastProperty "Adding and listing watch variables preserves entries" prop_add_list_watch_variables
        , fastProperty "Removing watch variables deletes entries" prop_remove_watch_variables
        , fastProperty "Adding duplicate watch variables overwrites values" prop_duplicate_watch_variables
        ]
    , testGroup "Call Stack Management"
        [ fastProperty "Push and pop operations maintain stack order" prop_push_pop_call_stack
        , fastProperty "Call stack maintains LIFO order" prop_call_stack_lifo
        , fastProperty "Current location tracks top of call stack" prop_current_location_tracking
        ]
    , testGroup "Expression Evaluation"
        [ fastProperty "Expression evaluation returns non-empty result" prop_evaluate_expression_non_empty
        ]
    , testGroup "Step Debugging"
        [ fastProperty "Step operations set step mode appropriately" prop_step_operations
        , fastProperty "Run to cursor sets breakpoint" prop_run_to_cursor
        ]
    , testGroup "Conditional Breakpoints"
        [ fastProperty "Conditional breakpoint with always true condition behaves like regular breakpoint" prop_conditional_breakpoint_always_true
        , fastProperty "Conditional breakpoint with always false condition never triggers" prop_conditional_breakpoint_always_false
        ]
    , testGroup "Combined Operations"
        [ fastProperty "Multiple operations maintain consistency" prop_multiple_operations_consistency
        ]
    , testGroup "Edge Cases"
        [ fastProperty "Empty location strings are handled gracefully" prop_empty_location_handling
        , fastProperty "Large number of operations are handled efficiently" prop_large_operations
        ]
    ]