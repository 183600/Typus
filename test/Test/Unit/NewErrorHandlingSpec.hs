{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.Unit.NewErrorHandlingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , emptyContext
  , _atRange
  )

tests :: TestTree
tests =
  testGroup "New Error Handling"
    [ testCase "creates basic error with correct properties" $ do
        let loc = _atLocation 10 5
            err = errorAt "test-id" = 5") (Just "main") (Just "x") (Just "Int") []
            err = (errorAt "test-id" error" loc) `withContext` ctx
        
        context err @?= ctx
        contextFunction (context err) @?= Just "main"
        contextVariable (context err) @?= Just "x"
        contextType (context err) @?= Just "Int"

    , testCase "wraps error with additional message" $ do
        let loc = _atLocation 10 10
            innerErr = errorAt "test-id" `L.isInfixOf` formatted)
        assertBool "format contains category" ("[Parsing]" `L.isInfixOf` formatted)
        assertBool "format contains message" ("Unexpected token" `L.isInfixOf` formatted)

    , testCase "formats error with location" $ do
        let loc = _atFileLocation "test.typus" 10 5
            err = errorAt "test-id" `L.isInfixOf` formatted)
        assertBool "format contains message" ("Location test" `L.isInfixOf` formatted)

    , testCase "formats multiple errors sorted by severity" $ do
        let loc = _atLocation 1 1
            info = infoAt "test-id" L.head lines')
        assertBool "info comes last" ("[INFO]" `L.isInfixOf` last lines')

    , testCase "filters errors by category" $ do
        let loc = _atLocation 1 1
            typeError = errorWithCategory "TYPE001" TypeChecking "Type error" loc
            ownershipError = errorWithCategory "OWN001" Ownership "Ownership error" loc
            parseError = errorWithCategory "PARSE001" Parsing "Parse error" loc
            errors = [typeError, ownershipError, parseError]
            typeErrors = filterByCategory TypeChecking errors
        
        L.length typeErrors @?= 1
        errorId (L.head typeErrors) @?= "TYPE001"

    , testCase "filters errors by severity" $ do
        let loc = _atLocation 1 1
            info = infoAt "test-id" (L.head warningsOnly) @?= Warning

    , testCase "generates error statistics" $ do
        let loc = _atLocation 1 1
            info = infoAt "test-id" `L.isInfixOf` report)
        assertBool "report contains statistics" ("Statistics:" `L.isInfixOf` report)
        assertBool "report contains error details" ("Detailed Errors:" `L.isInfixOf` report)
        assertBool "report contains suggestions" ("Suggestions:" `L.isInfixOf` report)

    , testCase "handles error location with range" $ do
        let loc = _atRange 5 1 5 10
            err = errorAt "test-id" `L.isInfixOf` formatted)

    , testCase "creates error with timestamp" $ do
        let loc = _atLocation 1 1
            err = errorAt "test-id" `L.isInfixOf` formatted)

    , testCase "creates custom recovery strategy" $ do
        let loc = _atLocation 1 1
            customRec = customRecovery True True (Just "Retry") (Just "Check connection") 30 0.8
            err = (errorAt "test-id" recovery" loc) { recovery = customRec }
        
        canRecoverFrom err @?= True
        shouldContinueAfter err @?= True
        recoveryAction (recovery err) @?= Just "Retry"
        recoveryHint (recovery err) @?= Just "Check connection"
        recoveryCost (recovery err) @?= 30
        recoveryConfidence (recovery err) @?= 0.8

    , testCase "error recovery strategies behave correctly" $ do
        let loc = _atLocation 1 1
            fatalErr = fatalError "FATAL001" "Fatal error" loc
            regularErr = errorAt "test-id" formatError wrapped2)
        assertBool "contains both wrapper messages" ("Second wrapper" `L.isInfixOf` formatError wrapped2)
    ]