{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | QuickCheck property tests for ErrorHandler module
module Test.Unit.NewErrorHandlerQuickCheckPropertySpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Compiler.Errors.Core
import SourceLocation (SourcePos(..), SourceSpan(..))
import Data.List (sort, sortBy)
import Data.Ord (comparing)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

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
  [ testProperty "emptyContext has all Nothing values" $
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
  
  , testProperty "errorRecovery can recover and continue" $
    \_ -> canRecover errorRecovery && shouldContinue errorRecovery
  
  , testProperty "warningRecovery can recover and continue" $
    \_ -> canRecover warningRecovery && shouldContinue warningRecovery
  
  , testProperty "infoRecovery can recover and continue" $
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
  [ testProperty "errorAt creates error with provided values" $
    \errId msg line col -> 
      let loc = _atLocation line col
          err = errorAt errId msg loc
      in errorId err === errId &&
         message err === msg &&
         location err === loc &&
         severity err === Error &&
         category err === Unknown
  
  , testProperty "errorWithCategory creates error with provided category" $
    \errId errCategory msg line col -> 
      let loc = _atLocation line col
          err = errorWithCategory errId errCategory msg loc
      in errorId err === errId &&
         message err === msg &&
         location err === loc &&
         severity err === Error &&
         category err === errCategory
  
  , testProperty "warningAt creates warning" $
    \errId msg line col -> 
      let loc = _atLocation line col
          err = warningAt errId msg loc
      in severity err === Warning
  
  , testProperty "infoAt creates info" $
    \errId msg line col -> 
      let loc = _atLocation line col
          err = infoAt errId msg loc
      in severity err === Info
  
  , testProperty "fatalError creates fatal error" $
    \errId msg line col -> 
      let loc = _atLocation line col
          err = fatalError errId msg loc
      in severity err === Fatal
  
  , testProperty "withLocation updates error location" $
    \errId msg line1 col1 line2 col2 -> 
      let loc1 = _atLocation line1 col1
          loc2 = _atLocation line2 col2
          err = errorAt errId msg loc1
          updatedErr = withLocation loc2 err
      in location updatedErr === loc2
  
  , testProperty "withContext updates error context" $
    \errId msg line col ctx -> 
      let loc = _atLocation line col
          err = errorAt errId msg loc
          updatedErr = withContext ctx err
      in context updatedErr === ctx
  
  , testProperty "withSuggestions updates error suggestions" $
    \errId msg line col suggestions -> 
      let loc = _atLocation line col
          err = errorAt errId msg loc
          updatedErr = withSuggestions suggestions err
      in suggestions updatedErr === suggestions
  
  , testProperty "withTimestamp adds timestamp" $
    \errId msg line col timestamp -> 
      let loc = _atLocation line col
          err = errorAt errId msg loc
          updatedErr = withTimestamp timestamp err
      in timestamp updatedErr === Just timestamp
  
  , testProperty "canRecoverFrom uses recovery strategy" $
    \errId msg line col -> 
      let loc = _atLocation line col
          err = errorAt errId msg loc
      in canRecoverFrom err === canRecover (recovery err)
  
  , testProperty "shouldContinueAfter uses recovery strategy" $
    \errId msg line col -> 
      let loc = _atLocation line col
          err = errorAt errId msg loc
      in shouldContinueAfter err === shouldContinue (recovery err)
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
          shouldBeIncluded = filter (\err -> isAtLeast minSeverity (combinedErrorSeverity err)) errors
      in length filtered === length shouldBeIncluded
  ]

-- | Properties for ErrorCollector
errorCollectorProperties :: TestTree
errorCollectorProperties = testGroup "ErrorCollector properties"
  [ testProperty "getErrors filters by Error and Fatal severity" $
    \errors -> 
      let filtered = getErrors errors
          expected = filter (\e -> severity e == Error || severity e == Fatal) errors
      in length filtered === length expected
  
  , testProperty "getWarnings filters by Warning severity" $
    \errors -> 
      let filtered = getWarnings errors
          expected = filter (\e -> severity e == Warning) errors
      in length filtered === length expected
  
  , testProperty "getInfo filters by Info severity" $
    \errors -> 
      let filtered = getInfo errors
          expected = filter (\e -> severity e == Info) errors
      in length filtered === length expected
  
  , testProperty "getAllMessages returns all errors" $
    \errors -> getAllMessages errors === errors
  
  , testProperty "hasErrors is true if there are Error or Fatal severity messages" $
    \errors -> 
      let hasErr = any (\e -> severity e == Error || severity e == Fatal) errors
      in hasErrors errors === hasErr
  
  , testProperty "hasWarnings is true if there are Warning severity messages" $
    \errors -> 
      let hasWarn = any (\e -> severity e == Warning) errors
      in hasWarnings errors === hasWarn
  ]

-- | Properties for error formatting
errorFormattingProperties :: TestTree
errorFormattingProperties = testGroup "Error formatting properties"
  [ testProperty "formatError includes severity string" $
    \errId msg line col -> 
      let loc = _atLocation line col
          err = errorAt errId msg loc
          formatted = formatError err
          severityStr = case severity err of
            Fatal -> "FATAL"
            Error -> "ERROR"
            Warning -> "WARNING"
            Info -> "INFO"
      in ("[" ++ severityStr ++ "]") `isInfixOf` formatted
  
  , testProperty "formatError includes category" $
    \errId msg line col -> 
      let loc = _atLocation line col
          err = errorAt errId msg loc
          formatted = formatError err
          categoryStr = "[" ++ show (category err) ++ "]"
      in categoryStr `isInfixOf` formatted
  
  , testProperty "formatError includes message" $
    \errId msg line col -> 
      let loc = _atLocation line col
          err = errorAt errId msg loc
          formatted = formatError err
      in T.unpack msg `isInfixOf` formatted
  
  , testProperty "formatErrorWithLocation includes location information" $
    \errId msg line col -> 
      let loc = _atLocation line col
          err = errorAt errId msg loc
          formatted = formatErrorWithLocation err
          locationStr = show line ++ ":" ++ show col
      in locationStr `isInfixOf` formatted
  
  , testProperty "formatErrors sorts by severity" $
    \errors -> 
      let formatted = formatErrors errors
          sorted = sortBySeverity errors
          sortedFormatted = formatErrors sorted
      in formatted === sortedFormatted
  
  , testProperty "formatErrorsWithLocation sorts by severity" $
    \errors -> 
      let formatted = formatErrorsWithLocation errors
          sorted = sortBySeverity errors
          sortedFormatted = formatErrorsWithLocation sorted
      in formatted === sortedFormatted
  ]

-- | Additional edge case properties
edgeCaseProperties :: TestTree
edgeCaseProperties = testGroup "ErrorHandler edge case properties"
  [ testProperty "empty error list formatting returns empty string" $
    \_ -> formatErrors [] === "" && formatErrorsWithLocation [] === ""
  
  , testProperty "error with empty message formats correctly" $
    \errId line col -> 
      let loc = _atLocation line col
          err = errorAt errId "" loc
          formatted = formatError err
      in not (null formatted)
  
  , testProperty "error with unknown location formats correctly" $
    \errId msg -> 
      let err = errorAt errId msg _unknownLocation
          formatted = formatErrorWithLocation err
      in not (null formatted)
  
  , testProperty "combineErrors preserves all error information" $
    \err1 err2 -> 
      let combined = combineErrors err1 err2
      in errorChain combined === [err1, err2]
  
  , testProperty "wrapError creates error chain" $
    \errId msg line col wrappedErr -> 
      let loc = _atLocation line col
          err = errorAt errId msg loc
          wrapped = wrapError err wrappedErr
      in errorChain wrapped === [wrappedErr]
  ]