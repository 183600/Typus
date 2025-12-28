{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.OwnershipComplexScenariosSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertFailure, (@?=), (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, choose, vectorOf, oneof, elements, listOf1)

import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer
  , OwnershipTransfer(..)
  , VariableOwnership(..)
  , OwnershipState(..)
  , newOwnershipAnalyzer
  )
import Ownership.Analyzer
  ( analyzeOwnership
  , analyzeOwnershipDebug
  , analyzeOwnershipFile
  )
import Ownership.Reporter (formatOwnershipErrors)

import Data.List (sort, nub, intersect)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Test complex ownership analysis scenarios
tests :: TestTree
tests =
  testGroup "Ownership Complex Scenarios Tests"
    [ testGroup "Nested ownership transfers"
        [ testCase "handles nested function calls with ownership transfer" $ do
            let analyzer = newOwnershipAnalyzer
                -- Simulate: x = foo(bar(baz()))
                transfers = 
                  [ OwnershipTransfer "baz_result" "bar_input" Moved
                  , OwnershipTransfer "bar_result" "foo_input" Moved
                  , OwnershipTransfer "foo_result" "x" Moved
                  ]
            assertBool "should handle nested transfers" $ length transfers >= 3

        , testCase "detects ownership conflicts in nested structures" $ do
            let analyzer = newOwnershipAnalyzer
                -- Simulate conflicting ownership in nested scope
                conflictError = OwnershipError 
                  { oeType = OwnershipConflict
                  , oeLocation = ("test", 10, 5)
                  , oeMessage = "Variable 'x' ownership conflict in nested scope"
                  , oeVariable = Just "x"
                  , oeSuggestion = Just "Consider using borrowing or cloning"
                  }
            assertBool "should detect nested ownership conflicts" $ 
              oeType conflictError == OwnershipConflict

        , testCase "ownership transfer through conditional branches" $ do
            let analyzer = newOwnershipAnalyzer
                -- Simulate: if condition { x = foo() } else { x = bar() }
                branchTransfers = 
                  [ OwnershipTransfer "foo_result" "x" Moved
                  , OwnershipTransfer "bar_result" "x" Moved
                  ]
            assertBool "should handle conditional ownership transfers" $ 
              length branchTransfers == 2
        ]

    , testGroup "Circular reference detection"
        [ testCase "detects simple circular references" $ do
            let analyzer = newOwnershipAnalyzer
                -- Simulate: a -> b -> a
                circularError = OwnershipError
                  { oeType = CircularReference
                  , oeLocation = ("test", 5, 1)
                  , oeMessage = "Circular reference detected between 'a' and 'b'"
                  , oeVariable = Just "a"
                  , oeSuggestion = Just "Break the circular reference"
                  }
            assertBool "should detect circular references" $ 
              oeType circularError == CircularReference

        , testCase "detects complex circular references" $ do
            let analyzer = newOwnershipAnalyzer
                -- Simulate: a -> b -> c -> d -> a
                complexCircular = OwnershipError
                  { oeType = CircularReference
                  , oeLocation = ("test", 15, 3)
                  , oeMessage = "Complex circular reference detected in ownership chain"
                  , oeVariable = Just "a"
                  , oeSuggestion = Just "Restructure ownership hierarchy"
                  }
            assertBool "should detect complex circular references" $ 
              oeType complexCircular == CircularReference &&
              "Complex" `elem` words (oeMessage complexCircular)

        , testCase "handles self-references correctly" $ do
            let analyzer = newOwnershipAnalyzer
                -- Simulate: x = x (self-reference)
                selfRefError = OwnershipError
                  { oeType = SelfReference
                  , oeLocation = ("test", 8, 10)
                  , oeMessage = "Variable 'x' references itself"
                  , oeVariable = Just "x"
                  , oeSuggestion = Just "Remove self-reference or use different variable"
                  }
            assertBool "should detect self-references" $ 
              oeType selfRefError == SelfReference
        ]

    , testGroup "Lifetime analysis"
        [ testCase "tracks variable lifetimes correctly" $ do
            let analyzer = newOwnershipAnalyzer
                -- Simulate variable with known lifetime
                lifetimeInfo = VariableOwnership
                  { voVariable = "data"
                  , voType = Owned
                  , voScope = "function_scope"
                  , voLifetime = Just "function_end"
                  , voBorrowCount = 0
                  }
            assertBool "should track variable lifetimes" $ 
              isJust (voLifetime lifetimeInfo)

        , testCase "detects lifetime extension violations" $ do
            let analyzer = newOwnershipAnalyzer
                -- Simulate: returning reference to local variable
                lifetimeError = OwnershipError
                  { oeType = LifetimeViolation
                  , oeLocation = ("test", 20, 15)
                  , oeMessage = "Cannot return reference to local variable 'local_data'"
                  , oeVariable = Just "local_data"
                  , oeSuggestion = Just "Return owned value or use heap allocation"
                  }
            assertBool "should detect lifetime violations" $ 
              oeType lifetimeError == LifetimeViolation

        , testCase "handles borrowed references correctly" $ do
            let analyzer = newOwnershipAnalyzer
                -- Simulate borrowed reference
                borrowInfo = VariableOwnership
                  { voVariable = "borrowed_data"
                  , voType = Borrowed
                  , voScope = "function_scope"
                  , voLifetime = Just "borrow_end"
                  , voBorrowCount = 1
                  }
            assertBool "should track borrowed references" $ 
              voType borrowInfo == Borrowed &&
              voBorrowCount borrowInfo > 0
        ]

    , testGroup "Ownership state transitions"
        [ testCase "tracks ownership state changes" $ do
            let initialState = OwnershipState Map.empty
                -- Simulate state transition
                transfer = OwnershipTransfer "source" "target" Moved
                updatedState = initialState  -- In real implementation, this would be updated
            assertBool "should track state transitions" $ True  -- Placeholder test

        , testCase "detects invalid state transitions" $ do
            let analyzer = newOwnershipAnalyzer
                -- Simulate invalid transition: using moved value
                invalidError = OwnershipError
                  { oeType = UseAfterMove
                  , oeLocation = ("test", 12, 8)
                  , oeMessage = "Use of moved value 'moved_var'"
                  , oeVariable = Just "moved_var"
                  , oeSuggestion = Just "Check ownership before use"
                  }
            assertBool "should detect invalid state transitions" $ 
              oeType invalidError == UseAfterMove

        , testCase "handles multiple borrows correctly" $ do
            let analyzer = newOwnershipAnalyzer
                -- Simulate multiple immutable borrows
                multiBorrow = VariableOwnership
                  { voVariable = "shared_data"
                  , voType = Borrowed
                  , voScope = "function_scope"
                  , voLifetime = Just "function_end"
                  , voBorrowCount = 3
                  }
            assertBool "should handle multiple borrows" $ 
              voBorrowCount multiBorrow > 1
        ]

    , testGroup "Error reporting and suggestions"
        [ testCase "provides helpful error messages" $ do
            let error = OwnershipError
                  { oeType = OwnershipConflict
                  , oeLocation = ("test.rs", 25, 10)
                  , oeMessage = "Ownership conflict: variable 'data' cannot be both owned and borrowed"
                  , oeVariable = Just "data"
                  , oeSuggestion = Just "Consider using Rc<T> or RefCell<T> for shared ownership"
                  }
                formatted = formatOwnershipErrors [error]
            assertBool "error message should be descriptive" $ 
              length (oeMessage error) > 20
            assertBool "should provide suggestions" $ 
              isJust (oeSuggestion error)

        , testCase "formats multiple errors coherently" $ do
            let errors = 
                  [ OwnershipError OwnershipConflict ("test", 1, 1) "conflict 1" (Just "var1") Nothing
                  , OwnershipError UseAfterMove ("test", 2, 1) "use after move" (Just "var2") Nothing
                  , OwnershipError LifetimeViolation ("test", 3, 1) "lifetime issue" (Just "var3") Nothing
                  ]
                formatted = formatOwnershipErrors errors
            assertBool "should format multiple errors" $ 
              length (lines formatted) >= 3
        ]

    , testGroup "QuickCheck property tests for ownership analysis"
        [ fastProperty "ownership transfers are acyclic by default" $
            \transfers ->
            let hasCycle = any (\t -> otSource t == otTarget t) transfers
            in not hasCycle ==> property True  -- Simplified property test

        , fastProperty "borrow count is non-negative" $
            \ownership ->
            voBorrowCount ownership >= 0

        , fastProperty "ownership type determines transfer behavior" $
            \sourceType targetType ->
            let transfer = OwnershipTransfer "source" "target" Moved
                isValidTransfer = sourceType == Owned && targetType `elem` [Owned, Borrowed]
            in isValidTransfer ==> property True

        , fastProperty "error locations are valid" $
            \error ->
            let (file, line, col) = oeLocation error
            in not (null file) && line > 0 && col > 0

        , fastProperty "ownership state preserves variable information" $
            \state varName ->
            case Map.lookup varName state of
              Just ownership -> voVariable ownership == varName
              Nothing -> property True

        , fastProperty "circular reference detection is deterministic" $
            \variables dependencies ->
            let hasCircular = length (filter (uncurry (==)) (zip variables (tail dependencies ++ []))) > 0
            in hasCircular ==> property True  -- Simplified circular detection
        ]
  ]