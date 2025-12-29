{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalErrorHandlerSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Data.Text (Text)
import qualified Data.Text as T

import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , emptyContext
  , ErrorCollector
  , newErrorCollector
  , addError
  , addWarning
  , addInfo
  , getErrors
  , getWarnings
  , hasErrors
  , hasWarnings
  , formatError
  , errorAt
  , warningAt
  , infoAt
  , errorWithCategory
  , warningWithCategory
  , infoWithCategory
  , withLocation
  , withContext
  , filterBySeverity
  , filterByCategory
  )

-- | Unit tests for ErrorHandler module
tests :: TestTree
tests =
  testGroup "New Cabal ErrorHandler Tests"
    [ testGroup "Unit Tests"
        [ testCase "newErrorCollector: starts with no errors" $
            let collector = newErrorCollector
            in do
              hasErrors collector @?= False
              hasWarnings collector @?= False
              
        , testCase "addError: adds error to collector" $
            let collector = newErrorCollector
                error = errorAt (ErrorLocation 1 1 0) "Test error"
                collector' = addError error collector
            in hasErrors collector' @?= True
            
        , testCase "addWarning: adds warning to collector" $
            let collector = newErrorCollector
                warning = warningAt (ErrorLocation 1 1 0) "Test warning"
                collector' = addWarning warning collector
            in hasWarnings collector' @?= True
            
        , testCase "formatError: includes error message" $
            let error = errorAt (ErrorLocation 1 1 0) "Test error"
                formatted = formatError error
            in "Test error" `T.isInfixOf` formatted @?= True
            
        , testCase "errorWithCategory: sets category correctly" $
            let error = errorWithCategory TypeErrorCategory "Type error"
            in errorCategory error @?= TypeErrorCategory
            
        , testCase "emptyContext: has no information" $
            let ctx = emptyContext
            in null ctx @?= True
        ]
    
    , testGroup "QuickCheck Properties"
        [ fastProperty "newErrorCollector: always starts empty" $
            \_ -> let collector = newErrorCollector
                  in not (hasErrors collector) && not (hasWarnings collector)
                  
        , fastProperty "addError: makes hasErrors true" $
            \msg line col ->
              let error = errorAt (ErrorLocation line col 0) msg
                  collector = addError error newErrorCollector
              in hasErrors collector
              
        , fastProperty "addWarning: makes hasWarnings true" $
            \msg line col ->
              let warning = warningAt (ErrorLocation line col 0) msg
                  collector = addWarning warning newErrorCollector
              in hasWarnings collector
              
        , fastProperty "addInfo: doesn't affect hasErrors or hasWarnings" $
            \msg line col ->
              let info = infoAt (ErrorLocation line col 0) msg
                  collector = addInfo info newErrorCollector
              in not (hasErrors collector) && not (hasWarnings collector)
              
        , fastProperty "filterBySeverity: preserves error severity" $
            \errors severity ->
              let filtered = filterBySeverity severity errors
              in all (\e -> errorSeverity e == severity) filtered
              
        , fastProperty "filterByCategory: preserves error category" $
            \errors category ->
              let filtered = filterByCategory category errors
              in all (\e -> errorCategory e == category) filtered
              
        , fastProperty "withLocation: updates error location" $
            \msg line col newLine newCol ->
              let error = errorAt (ErrorLocation line col 0) msg
                  newLoc = ErrorLocation newLine newCol 0
                  updated = withLocation newLoc error
              in errorLocation updated === newLoc
        ]
    ]