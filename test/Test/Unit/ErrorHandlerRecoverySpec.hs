{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.ErrorHandlerRecoverySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertFailure, (@?=), (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, choose, vectorOf, oneof, elements, listOf1)

import Compiler.Errors.Core
  ( Error(..)
  , ErrorSeverity(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorMessage(..)
  , ErrorBundle(..)
  , addError
  , hasErrors
  , getErrors
  , getWarnings
  , getErrorsBySeverity
  , formatError
  , formatErrorBundle
  , mergeErrorBundles
  , recoverFromErrors
  , canRecoverFrom
  )

import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- | Test error recovery mechanisms
tests :: TestTree
tests =
  testGroup "Error Handler Recovery Tests"
    [ testGroup "Error recovery basic tests"
        [ testCase "canRecoverFrom allows recovery from warnings" $ do
            let warning = Error Warning (ErrorLocation "test" 1 1) "test warning" [] []
            assertBool "should recover from warnings" $ canRecoverFrom warning

        , testCase "canRecoverFrom may not allow recovery from critical errors" $ do
            let critical = Error Critical (ErrorLocation "test" 1 1) "critical error" [] []
            assertBool "should not recover from critical errors" $ not (canRecoverFrom critical)

        , testCase "recoverFromErrors filters non-recoverable errors" $ do
            let recoverable = Error Warning (ErrorLocation "test" 1 1) "warning" [] []
                nonRecoverable = Error Critical (ErrorLocation "test" 2 1) "critical" [] []
                bundle = ErrorBundle [recoverable, nonRecoverable]
                recovered = recoverFromErrors bundle
                remainingErrors = getErrors recovered
            length remainingErrors @?= 1
            errorSeverity (head remainingErrors) @?= Warning

        , testCase "mergeErrorBundles preserves recovery information" $ do
            let bundle1 = ErrorBundle [Error Warning (ErrorLocation "test1" 1 1) "warning1" [] []]
                bundle2 = ErrorBundle [Error Critical (ErrorLocation "test2" 1 1) "critical1" [] []]
                merged = mergeErrorBundles bundle1 bundle2
                allErrors = getErrors merged
            length allErrors @?= 2
        ]

    , testGroup "Error context and recovery"
        [ testCase "error context affects recovery possibility" $ do
            let contextError = Error Warning (ErrorLocation "test" 1 1) "error in context" 
                                [ContextItem "function", ContextItem "module"] []
                contextErrorCritical = Error Critical (ErrorLocation "test" 1 1) "critical in context"
                                      [ContextItem "function", ContextItem "module"] []
            assertBool "contextual warning should be recoverable" $ canRecoverFrom contextError
            assertBool "contextual critical should not be recoverable" $ not (canRecoverFrom contextErrorCritical)

        , testCase "recovery preserves error context" $ do
            let contextualError = Error Warning (ErrorLocation "test" 1 1) "contextual error"
                                               [ContextItem "parse", ContextItem "expression"] []
                bundle = ErrorBundle [contextualError]
                recovered = recoverFromErrors bundle
                remainingErrors = getErrors recovered
            if not (null remainingErrors)
                then errorContext (head remainingErrors) @?= [ContextItem "parse", ContextItem "expression"]
                else assertFailure "expected at least one recoverable error"

        , testCase "nested error contexts are handled properly" $ do
            let nestedError = Error Warning (ErrorLocation "test" 1 1) "nested error"
                              [ContextItem "outer", ContextItem "inner", ContextItem "deep"] []
                bundle = ErrorBundle [nestedError]
            assertBool "should handle nested contexts" $ hasErrors bundle
        ]

    , testGroup "Error formatting and recovery"
        [ testCase "formatError works for recoverable errors" $ do
            let recoverable = Error Warning (ErrorLocation "test" 1 1) "recoverable warning" [] []
                formatted = formatError recoverable
            assertBool "formatted error should not be empty" $ not (null formatted)
            assertBool "formatted error should contain location" $ "test" `elem` words formatted

        , testCase "formatErrorBundle handles recovered errors" $ do
            let bundle = ErrorBundle 
                  [ Error Warning (ErrorLocation "test1" 1 1) "warning1" [] []
                  , Error Critical (ErrorLocation "test2" 1 1) "critical1" [] []
                  ]
                recovered = recoverFromErrors bundle
                formatted = formatErrorBundle recovered
            assertBool "formatted bundle should not be empty" $ not (null formatted)

        , testCase "error formatting preserves severity information" $ do
            let warning = Error Warning (ErrorLocation "test" 1 1) "warning message" [] []
                critical = Error Critical (ErrorLocation "test" 1 1) "critical message" [] []
                warningFormatted = formatError warning
                criticalFormatted = formatError critical
            assertBool "warning format should indicate warning" $ 
              any (\word -> word `elem` ["warning", "Warning"]) (words warningFormatted)
            assertBool "critical format should indicate critical" $ 
              any (\word -> word `elem` ["critical", "Critical", "error", "Error"]) (words criticalFormatted)
        ]

    , testGroup "Error bundle operations and recovery"
        [ testCase "addError respects recovery rules" $ do
            let initialBundle = ErrorBundle []
                recoverable = Error Warning (ErrorLocation "test" 1 1) "new warning" [] []
                updated = addError recoverable initialBundle
            assertBool "adding recoverable error should result in errors" $ hasErrors updated

        , testCase "getErrorsBySeverity works with recovered bundles" $ do
            let bundle = ErrorBundle
                  [ Error Warning (ErrorLocation "test" 1 1) "warning1" [] []
                  , Error Critical (ErrorLocation "test" 2 1) "critical1" [] []
                  , Error Warning (ErrorLocation "test" 3 1) "warning2" [] []
                  ]
                recovered = recoverFromErrors bundle
                warnings = getErrorsBySeverity Warning recovered
                criticals = getErrorsBySeverity Critical recovered
            length warnings @?= 2
            length criticals @?= 0  -- critical errors should be filtered out

        , testCase "mergeErrorBundles with recovery" $ do
            let bundle1 = ErrorBundle [Error Warning (ErrorLocation "test1" 1 1) "warning1" [] []]
                bundle2 = ErrorBundle [Error Critical (ErrorLocation "test2" 1 1) "critical1" [] []]
                merged = mergeErrorBundles bundle1 bundle2
                recovered = recoverFromErrors merged
                finalErrors = getErrors recovered
            length finalErrors @?= 1  -- only warning should remain
        ]

    , testGroup "QuickCheck property tests for error recovery"
        [ fastProperty "recoverFromErrors preserves ordering of recoverable errors" $
            \errors ->
            let bundle = ErrorBundle errors
                recovered = recoverFromErrors bundle
                remainingErrors = getErrors recovered
                originalRecoverable = filter canRecoverFrom errors
            in remainingErrors === originalRecoverable

        , fastProperty "canRecoverFrom is consistent with severity" $
            \severity location message ->
            let error = Error severity location message [] []
                expected = severity `elem` [Warning, Info]
            in canRecoverFrom error === expected

        , fastProperty "mergeErrorBundles is associative" $
            \bundle1 bundle2 bundle3 ->
            let merged1 = mergeErrorBundles (mergeErrorBundles bundle1 bundle2) bundle3
                merged2 = mergeErrorBundles bundle1 (mergeErrorBundles bundle2 bundle3)
                recovered1 = recoverFromErrors merged1
                recovered2 = recoverFromErrors merged2
            in getErrors recovered1 === getErrors recovered2

        , fastProperty "recoverFromErrors is idempotent" $
            \bundle ->
            let recovered1 = recoverFromErrors bundle
                recovered2 = recoverFromErrors recovered1
            in getErrors recovered1 === getErrors recovered2

        , fastProperty "getWarnings only returns recoverable warning errors" $
            \errors ->
            let bundle = ErrorBundle errors
                recovered = recoverFromErrors bundle
                warnings = getWarnings recovered
                allWarnings = filter (\e -> errorSeverity e == Warning) errors
            in length warnings === length allWarnings

        , fastProperty "formatError produces non-empty strings for valid errors" $
            \severity location message ->
            not (null message) ==>
            let error = Error severity location message [] []
                formatted = formatError error
            in not (null formatted)

        , fastProperty "error recovery preserves error locations" $
            \errors ->
            let recoverableErrors = filter canRecoverFrom errors
                bundle = ErrorBundle recoverableErrors
                recovered = recoverFromErrors bundle
                remainingErrors = getErrors recovered
                originalLocations = map errorLocation recoverableErrors
                recoveredLocations = map errorLocation remainingErrors
            in sort originalLocations === sort recoveredLocations
        ]
  ]