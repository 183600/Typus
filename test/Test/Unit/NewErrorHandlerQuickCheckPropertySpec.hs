{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | QuickCheck property tests for ErrorHandler module
module Test.Unit.NewErrorHandlerQuickCheckPropertySpec where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Compiler.Errors.Core
import SourceLocation (SourcePos(..), SourceSpan(..))
import Data.List (sort, sortBy)
import Data.Ord (comparing)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T (pack, unpack)

-- | Test group for ErrorHandler module QuickCheck properties
testErrorHandlerQuickCheckProperties :: TestTree
testErrorHandlerQuickCheckProperties = testGroup "ErrorHandler Module QuickCheck Property Tests"
  [ errorSeverityProperties
  , errorLocationProperties
  , errorContextProperties
  , errorRecoveryProperties
  , typeErrorProperties
  , combinedErrorProperties
  , errorCollectorProperties
  , errorFormattingProperties
  ]

-- | Properties for ErrorSeverity
errorSeverityProperties :: TestTree
errorSeverityProperties = testGroup "ErrorSeverity properties"
  [ testProperty "severityPriority ordering is correct" $
    \_ -> 
      let priorities = map severityPriority [Fatal, Error, Warning, Info]
      in priorities === [100, 80, 30, 10]
  
  , testProperty "compareSeverity respects priority ordering" $
    \sev1 sev2 -> 
      let result = compareSeverity sev1 sev2
          expected = compare (severityPriority sev1) (severityPriority sev2)
      in result === expected
  
  , testProperty "isAtLeast is reflexive" $
    \sev -> isAtLeast sev sev
  
  , testProperty "isAtLeast is transitive" $
    \sev1 sev2 sev3 -> 
      isAtLeast sev1 sev2 && isAtLeast sev2 sev3 ==> isAtLeast sev1 sev3
  
  , testProperty "Fatal is the highest severity" $
    \sev -> isAtLeast Fatal sev || sev === Fatal
  
  , testProperty "Info is the lowest severity" $
    \sev -> isAtLeast sev Info || sev === Info
  
  , testProperty "_isRecoverable is false only for Fatal" $
    \sev -> _isRecoverable sev === (sev /= Fatal)
  ]

-- | Properties for ErrorLocation
errorLocationProperties :: TestTree
errorLocationProperties = testGroup "ErrorLocation properties"
  [ testProperty "getErrorLine returns line field" $
    \line col -> 
      let loc = _atLocation line col
      in getErrorLine loc === line
  
  , testProperty "getErrorColumn returns column field" $
    \line col -> 
      let loc = _atLocation line col
      in getErrorColumn loc === col
  
  , testProperty "_atLocation creates location with no file" $
    \line col -> 
      let loc = _atLocation line col
      in filePath loc === Nothing &&
          line loc === line &&
          column loc === col &&
          endLine loc === Nothing &&
          endColumn loc === Nothing
  
  , testProperty "_atFileLocation creates location with file" $
    \file line col -> 
      let loc = _atFileLocation file line col
      in filePath loc === Just file &&
          line loc === line &&
          column loc === col
  
  , testProperty "_atRange creates location with range" $
    \startLine startCol endLine endCol -> 
      let loc = _atRange startLine startCol endLine endCol
      in line loc === startLine &&
          column loc === startCol &&
          endLine loc === Just endLine &&
          endColumn loc === Just endCol
  ]

-- | Properties for ErrorContext
errorContextProperties :: TestTree
errorContextProperties = testGroup "ErrorContext properties"
  [ testProperty "emptyContext has L.all Nothing values" $
    \_ -> contextCode emptyContext === Nothing &&
          contextFunction emptyContext === Nothing &&
          contextVariable emptyContext === Nothing &&
          contextType emptyContext === Nothing &&
          contextAdditional emptyContext === []
  
  , testProperty "ErrorContext preserves provided values" $
    \code func var typeVal additional -> 
      let ctx = ErrorContext code func var typeVal additional
      in contextCode ctx === code &&
          contextFunction ctx === func &&
          contextVariable ctx === var &&
          contextType ctx === typeVal &&
          contextAdditional ctx === additional
  ]

-- | Properties for ErrorRecovery
errorRecoveryProperties :: TestTree
errorRecoveryProperties = testGroup "ErrorRecovery properties"
  [ testProperty "fatalRecovery cannot recover" $
    \_ -> not (canRecover fatalRecovery) && not (shouldContinue fatalRecovery)
  
  , testProperty "errorRecovery can recover L.and continue" $
    \_ -> canRecover errorRecovery && shouldContinue errorRecovery
  
  , testProperty "warningRecovery can recover L.and continue" $
    \_ -> canRecover warningRecovery && shouldContinue warningRecovery
  
  , testProperty "infoRecovery can recover L.and continue" $
    \_ -> canRecover infoRecovery && shouldContinue infoRecovery
  
  , testProperty "customRecovery preserves provided values" $
    \canRec shouldCont action hint cost confidence -> 
      let recovery = customRecovery canRec shouldCont action hint cost confidence
      in canRecover recovery === canRec &&
         shouldContinue recovery === shouldCont &&
         recoveryAction recovery === action &&
         recoveryHint recovery === hint &&
         recoveryCost recovery === cost &&
         recoveryConfidence recovery === confidence
  
  , testProperty "_sequenceRecovery combines strategies correctly" $
    \r1 r2 -> 
      let combined = _sequenceRecovery r1 r2
      in canRecover combined === (canRecover r1 && canRecover r2) &&
         shouldContinue combined === (shouldContinue r1 && shouldContinue r2)
  
  , testProperty "_chooseBestRecovery picks strategy with highest confidence" $
    \r1 r2 -> 
      let best = _chooseBestRecovery [r1, r2]
          conf1 = recoveryConfidence r1
          conf2 = recoveryConfidence r2
          expected = if conf1 > conf2 then r1 else r2
      in (canRecover r1 && canRecover r2) ==> recoveryConfidence best === max conf1 conf2
  ]

-- | Properties for TypeError
typeErrorProperties :: TestTree
typeErrorProperties = testGroup "TypeError properties"
  [ testProperty "errorAt "test-id" (recovery err)
  
  , testProperty "shouldContinueAfter uses recovery strategy" $
    \errId msg line col -> 
      let loc = _atLocation line col
          err = errorAt "test-id" (recovery err)
  ]

-- | Properties for CombinedError
combinedErrorProperties :: TestTree
combinedErrorProperties = testGroup "CombinedError properties"
  [ testProperty "combinedErrorSeverity extracts severity correctly" $
    \sev -> 
      let ownErr = OwnershipErrorCombined sev undefined
          depErr = DependentTypeErrorCombined sev undefined
          intErr = IntegrationError "" sev
          crossErr = CrossAnalyzerError "" sev []
      in combinedErrorSeverity ownErr === sev &&
         combinedErrorSeverity depErr === sev &&
         combinedErrorSeverity intErr === sev &&
         combinedErrorSeverity crossErr === sev
  
  , testProperty "filterCombinedErrorsBySeverity filters correctly" $
    \minSeverity errors -> 
      let filtered = filterCombinedErrorsBySeverity minSeverity errors
          shouldBeIncluded = L.filter (\err -> isAtLeast minSeverity (combinedErrorSeverity err)) errors
      in L.length filtered === L.length shouldBeIncluded
  ]

-- | Properties for ErrorCollector
errorCollectorProperties :: TestTree
errorCollectorProperties = testGroup "ErrorCollector properties"
  [ testProperty "getErrors filters by Error L.and Fatal severity" $
    \errors -> 
      let filtered = getErrors errors
          expected = L.filter (\e -> severity e == Error || severity e == Fatal) errors
      in L.length filtered === L.length expected
  
  , testProperty "getWarnings filters by Warning severity" $
    \errors -> 
      let filtered = getWarnings errors
          expected = L.filter (\e -> severity e == Warning) errors
      in L.length filtered === L.length expected
  
  , testProperty "getInfo filters by Info severity" $
    \errors -> 
      let filtered = getInfo errors
          expected = L.filter (\e -> severity e == Info) errors
      in L.length filtered === L.length expected
  
  , testProperty "getAllMessages returns L.all errors" $
    \errors -> getAllMessages errors === errors
  
  , testProperty "hasErrors is true if there are Error L.or Fatal severity messages" $
    \errors -> 
      let hasErr = L.any (\e -> severity e == Error || severity e == Fatal) errors
      in hasErrors errors === hasErr
  
  , testProperty "hasWarnings is true if there are Warning severity messages" $
    \errors -> 
      let hasWarn = L.any (\e -> severity e == Warning) errors
      in hasWarnings errors === hasWarn
  ]

-- | Properties for error formatting
errorFormattingProperties :: TestTree
errorFormattingProperties = testGroup "Error formatting properties"
  [ testProperty "formatError includes severity string" $
    \errId msg line col -> 
      let loc = _atLocation line col
          err = errorAt "test-id" ++ severityStr ++ "]") `L.isInfixOf` formatted
  
  , testProperty "formatError includes category" $
    \errId msg line col -> 
      let loc = _atLocation line col
          err = errorAt "test-id" (category err) ++ "]"
      in categoryStr `L.isInfixOf` formatted
  
  , testProperty "formatError includes message" $
    \errId msg line col -> 
      let loc = _atLocation line col
          err = errorAt "test-id" (null formatted)
  
  , testProperty "error with unknown location formats correctly" $
    \errId msg -> 
      let err = errorAt "test-id" (null formatted)
  
  , testProperty "combineErrors preserves L.all error information" $
    \err1 err2 -> 
      let combined = combineErrors err1 err2
      in errorChain combined === [err1, err2]
  
  , testProperty "wrapError creates error chain" $
    \errId msg line col wrappedErr -> 
      let loc = _atLocation line col
          err = errorAt "test-id" errorChain wrapped === [wrappedErr]
  ]