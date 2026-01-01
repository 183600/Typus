{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | QuickCheck tests for ErrorHandling module
module Test.Unit.NewFreshErrorHandlingQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler.Errors.Core 
  ( TypeError(..), CombinedError(..), ErrorSeverity(..), ErrorCategory(..)
  , ErrorLocation(..), ErrorContext(..), emptyContext
  , ErrorCollector, newErrorCollector, addError, addWarning, addInfo
  , getErrors, getWarnings, getInfo, getAllMessages, hasErrors, hasWarnings
  , formatError, formatErrors, errorAt, warningAt, infoAt "test-id" shouldContinueAfter, errorAtWithTimestamp
  )
import SourceLocation (SourcePos(..), startPos)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing)
import Data.Either (isLeft, isRight)

-- ============================================================================
-- Test Suite Definition
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "New ErrorHandling QuickCheck Tests"
  [ errorTypeProperties
  , errorCollectorProperties
  , errorFormattingProperties
  , errorRecoveryProperties
  , timestampProperties
  ]

-- ============================================================================
-- Error Type Properties
-- ============================================================================

errorTypeProperties :: TestTree
errorTypeProperties = testGroup "Error Type Properties"
  [ testProperty "TypeError: essential information is preserved in formatting" $
      \severity category msg ->
        let err = TypeError severity category msg emptyContext
            formatted = formatError err
        in msg `L.isInfixOf` formatted
        
  , testProperty "TypeError: severity affects formatting" $
      \msg severity1 severity2 ->
        let err1 = TypeError severity1 ErrorType msg emptyContext
            err2 = TypeError severity2 ErrorType msg emptyContext
            fmt1 = formatError err1
            fmt2 = formatError err2
        in severity1 /= severity2 ==> fmt1 /= fmt2
        
  , testProperty "TypeError: category affects formatting" $
      \msg cat1 cat2 ->
        let err1 = TypeError ErrorInfo cat1 msg emptyContext
            err2 = TypeError ErrorInfo cat2 msg emptyContext
            fmt1 = formatError err1
            fmt2 = formatError err2
        in cat1 /= cat2 ==> fmt1 /= fmt2
        
  , testProperty "TypeError: context is included in formatting" $
      \msg contextInfo ->
        let context = emptyContext { ecAdditionalInfo = Just contextInfo }
            err = TypeError ErrorError ErrorType msg context
            formatted = formatError err
        in not (null contextInfo) ==> contextInfo `L.isInfixOf` formatted
        
  , testProperty "CombinedError: contains multiple errors" $
      \msg1 msg2 ->
        let err1 = TypeError ErrorError ErrorType msg1 emptyContext
            err2 = TypeError ErrorWarning ErrorType msg2 emptyContext
            combined = CombinedError [err1, err2]
        in L.length (ceErrors combined) === 2
  ]

-- ============================================================================
-- Error Collector Properties
-- ============================================================================

errorCollectorProperties :: TestTree
errorCollectorProperties = testGroup "Error Collector Properties"
  [ testProperty "ErrorCollector: new collector is empty" $
      \() ->
        let collector = newErrorCollector
        in not (hasErrors collector) && not (hasWarnings collector)
        
  , testProperty "ErrorCollector: adding error increases error count" $
      \msg ->
        let collector = newErrorCollector
            collector' = addError startPos msg collector
        in hasErrors collector' && not (hasErrors collector)
        
  , testProperty "ErrorCollector: adding warning increases warning count" $
      \msg ->
        let collector = newErrorCollector
            collector' = addWarning startPos msg collector
        in hasWarnings collector' && not (hasWarnings collector)
        
  , testProperty "ErrorCollector: adding info doesn't affect error/warning flags" $
      \msg ->
        let collector = newErrorCollector
            collector' = addInfo startPos msg collector
        in not (hasErrors collector') && not (hasWarnings collector')
        
  , testProperty "ErrorCollector: can add multiple errors" $
      \msgs ->
        let collector = L.foldl (\c msg -> addError startPos msg c) newErrorCollector msgs
            errors = getErrors collector
        in L.length errors === L.length msgs
        
  , testProperty "ErrorCollector: errors are retrievable in order" $
      \msgs ->
        let collector = L.foldl (\c msg -> addError startPos msg c) newErrorCollector msgs
            errors = getErrors collector
            errorMessages = map teMessage errors
        in errorMessages === msgs
        
  , testProperty "ErrorCollector: warnings are separate from errors" $
      \errorMsgs warningMsgs ->
        let collector = L.foldl (\c msg -> addError startPos msg c) newErrorCollector errorMsgs
            collector' = L.foldl (\c msg -> addWarning startPos msg c) collector warningMsgs
            errors = getErrors collector'
            warnings = getWarnings collector'
        in L.length errors === L.length errorMsgs && L.length warnings === L.length warningMsgs
  ]

-- ============================================================================
-- Error Formatting Properties
-- ============================================================================

errorFormattingProperties :: TestTree
errorFormattingProperties = testGroup "Error Formatting Properties"
  [ testProperty "formatError: includes position information" $
      \line col msg ->
        let pos = SourcePos line col
            err = errorAt "test-id" startPos msg) msgs
            formatted = formatErrors errors
            -- Check that L.all messages appear in order
        in L.all (`L.isInfixOf` formatted) msgs
        
  , testProperty "formatError: long messages are handled gracefully" $
      \shortMsg ->
        let longMsg = shortMsg ++ replicate 100 'x'
            err = errorAt "test-id" 1 1) (secondsToDiffTime 0)
          err = errorAtWithTimestamp time startPos "test message"
          formatted = formatError err
      assertBool "timestamp should be included" $ "2025-01-01" `L.isInfixOf` formatted
      
  , testProperty "different timestamps produce different output" $
      \msg ->
        let time1 = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)
            time2 = UTCTime (fromGregorian 2025 1 2) (secondsToDiffTime 0)
            err1 = errorAtWithTimestamp time1 startPos msg
            err2 = errorAtWithTimestamp time2 startPos msg
            fmt1 = formatError err1
            fmt2 = formatError err2
        in fmt1 /= fmt2
        
  , testCase "timestamp format is consistent" $
    do
      let time = UTCTime (fromGregorian 2025 12 29) (secondsToDiffTime 43200)
          err = errorAtWithTimestamp time startPos "test message"
          formatted = formatError err
      assertBool "date format should be ISO-like" $ "2025-12-29" `L.isInfixOf` formatted
  ]

-- ============================================================================
-- Helper Functions
-- ============================================================================

isInfixOf :: String -> String -> Bool
L.isInfixOf = Data.List.L.isInfixOf

isPrefixOf :: String -> String -> Bool
L.isPrefixOf = Data.List.L.isPrefixOf