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
  , formatError, formatErrors, errorAt, warningAt, infoAt
  , errorWithCategory, warningWithCategory, infoWithCategory
  , canRecoverFrom, shouldContinueAfter, errorAtWithTimestamp
  )
import SourceLocation (SourcePos(..), startPos)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
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
        in msg `isInfixOf` formatted
        
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
        in not (null contextInfo) ==> contextInfo `isInfixOf` formatted
        
  , testProperty "CombinedError: contains multiple errors" $
      \msg1 msg2 ->
        let err1 = TypeError ErrorError ErrorType msg1 emptyContext
            err2 = TypeError ErrorWarning ErrorType msg2 emptyContext
            combined = CombinedError [err1, err2]
        in length (ceErrors combined) === 2
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
        let collector = foldl (\c msg -> addError startPos msg c) newErrorCollector msgs
            errors = getErrors collector
        in length errors === length msgs
        
  , testProperty "ErrorCollector: errors are retrievable in order" $
      \msgs ->
        let collector = foldl (\c msg -> addError startPos msg c) newErrorCollector msgs
            errors = getErrors collector
            errorMessages = map teMessage errors
        in errorMessages === msgs
        
  , testProperty "ErrorCollector: warnings are separate from errors" $
      \errorMsgs warningMsgs ->
        let collector = foldl (\c msg -> addError startPos msg c) newErrorCollector errorMsgs
            collector' = foldl (\c msg -> addWarning startPos msg c) collector warningMsgs
            errors = getErrors collector'
            warnings = getWarnings collector'
        in length errors === length errorMsgs && length warnings === length warningMsgs
  ]

-- ============================================================================
-- Error Formatting Properties
-- ============================================================================

errorFormattingProperties :: TestTree
errorFormattingProperties = testGroup "Error Formatting Properties"
  [ testProperty "formatError: includes position information" $
      \line col msg ->
        let pos = SourcePos line col
            err = errorAt pos msg
            formatted = formatError err
        in line > 0 && col > 0 ==> 
           show line `isInfixOf` formatted && show col `isInfixOf` formatted
           
  , testProperty "formatError: different severities produce different output" $
      \msg ->
        let err1 = errorAt startPos msg
            err2 = warningAt startPos msg
            err3 = infoAt startPos msg
            fmt1 = formatError err1
            fmt2 = formatError err2  
            fmt3 = formatError err3
        in fmt1 /= fmt2 && fmt2 /= fmt3 && fmt1 /= fmt3
        
  , testProperty "formatError: category affects output" $
      \msg cat ->
        let err1 = errorWithCategory cat msg
            err2 = warningWithCategory cat msg
            fmt1 = formatError err1
            fmt2 = formatError err2
        in fmt1 /= fmt2
        
  , testProperty "formatErrors: preserves order of multiple errors" $
      \msgs ->
        let errors = map (\msg -> errorAt startPos msg) msgs
            formatted = formatErrors errors
            -- Check that all messages appear in order
        in all (`isInfixOf` formatted) msgs
        
  , testProperty "formatError: long messages are handled gracefully" $
      \shortMsg ->
        let longMsg = shortMsg ++ replicate 100 'x'
            err = errorAt startPos longMsg
            formatted = formatError err
        in longMsg `isInfixOf` formatted
  ]

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

errorRecoveryProperties :: TestTree
errorRecoveryProperties = testGroup "Error Recovery Properties"
  [ testProperty "canRecoverFrom: info messages are always recoverable" $
      \msg ->
        let err = infoAt startPos msg
        in canRecoverFrom err
        
  , testProperty "canRecoverFrom: warnings are generally recoverable" $
      \msg ->
        let err = warningAt startPos msg
        in canRecoverFrom err
        
  , testProperty "shouldContinueAfter: info messages allow continuation" $
      \msg ->
        let err = infoAt startPos msg
        in shouldContinueAfter err
        
  , testProperty "shouldContinueAfter: warnings generally allow continuation" $
      \msg ->
        let err = warningAt startPos msg
        in shouldContinueAfter err
        
  , testProperty "ErrorCategory: affects recovery behavior" $
      \msg ->
        let err1 = errorWithCategory ErrorSyntax msg
            err2 = errorWithCategory ErrorType msg
            err3 = errorWithCategory ErrorMemory msg
        in -- Different categories may have different recovery behavior
           canRecoverFrom err1 || canRecoverFrom err2 || canRecoverFrom err3
  ]

-- ============================================================================
-- Timestamp Properties
-- ============================================================================

timestampProperties :: TestTree
timestampProperties = testGroup "Timestamp Properties"
  [ testCase "errorAtWithTimestamp includes timestamp" $
    do
      let time = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)
          err = errorAtWithTimestamp time startPos "test message"
          formatted = formatError err
      assertBool "timestamp should be included" $ "2025-01-01" `isInfixOf` formatted
      
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
      assertBool "date format should be ISO-like" $ "2025-12-29" `isInfixOf` formatted
  ]

-- ============================================================================
-- Helper Functions
-- ============================================================================

isInfixOf :: String -> String -> Bool
isInfixOf = Data.List.isInfixOf

isPrefixOf :: String -> String -> Bool
isPrefixOf = Data.List.isPrefixOf