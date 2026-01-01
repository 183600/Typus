{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.ErrorHandlerEnhancedQuickCheckSpec where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler.Errors.Core (TypeError(..), CombinedError(..), ErrorSeverity(..), 
                            ErrorCategory(..), ErrorLocation(..), ErrorContext(..),
                            ErrorRecovery(..), emptyContext, 
                            fatalRecovery, errorRecovery, warningRecovery, infoRecovery,
                            customRecovery, formatError, formatErrors, 
                            errorAt, warningAt, infoAt, fatalError,
                            filterBySeverity, filterByCategory, hasErrors, hasWarnings,
                            severityPriority, isAtLeast, combinedErrorSeverity,
                            filterCombinedErrorsBySeverity)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.List (sort)
import Data.Maybe (isJust, isNothing)

tests :: TestTree
tests = testGroup "ErrorHandler Enhanced QuickCheck Tests"
  [ errorSeverityProperties
  , errorLocationProperties
  , errorContextProperties
  , errorRecoveryProperties
  , errorCollectorProperties
  , errorFormattingProperties
  , combinedErrorProperties
  ]

-- | Error severity properties
errorSeverityProperties :: TestTree
errorSeverityProperties = testGroup "Error Severity Properties"
  [ testProperty "severity priority ordering" $
      \sev1 sev2 -> 
        let p1 = severityPriority sev1
            p2 = severityPriority sev2
        in (sev1 >= sev2) === (p1 >= p2)
  
  , testProperty "isAtLeast reflexivity" $
      \sev -> isAtLeast sev sev === True
  
  , testProperty "isAtLeast transitivity" $
      \sev1 sev2 sev3 -> 
        isAtLeast sev1 sev2 && isAtLeast sev2 sev3 ==> isAtLeast sev1 sev3
  
  , testProperty "Fatal is highest severity" $
      \sev -> isAtLeast sev Fatal === (sev == Fatal)
  
  , testProperty "Info is lowest severity" $
      \sev -> isAtLeast Info sev === True
  
  , testProperty "severityPriority positive" $
      \sev -> severityPriority sev > 0
  
  , testProperty "filterBySeverity preserves order" $
      \sev errors -> 
        let filtered = filterBySeverity sev errors
            sortedFiltered = sort filtered
        in filtered === sortedFiltered
  ]

-- | Error location properties
errorLocationProperties :: TestTree
errorLocationProperties = testGroup "Error Location Properties"
  [ testProperty "location with line L.and column" $
      \line col -> 
        line > 0 && col > 0 ==> 
        let location = ErrorLocation Nothing line col Nothing Nothing
        in line location === line .&&. column location === col
  
  , testProperty "location with file path" $
      \file line col -> 
        line > 0 && col > 0 ==> 
        let location = ErrorLocation (Just file) line col Nothing Nothing
        in filePath location === Just file .&&. 
           line location === line .&&. 
           column location === col
  
  , testProperty "location with range" $
      \startLine startCol endLine endCol -> 
        startLine > 0 && startCol > 0 && endLine >= startLine ==> 
        let location = ErrorLocation Nothing startLine startCol (Just endLine) (Just endCol)
        in line location === startLine .&&. 
           column location === startCol .&&. 
           endLine location === Just endLine .&&. 
           endColumn location === Just endCol
  ]

-- | Error context properties
errorContextProperties :: TestTree
errorContextProperties = testGroup "Error Context Properties"
  [ testProperty "emptyContext has no information" $
      \() -> 
        let ctx = emptyContext
        in contextCode ctx === Nothing .&&. 
           contextFunction ctx === Nothing .&&. 
           contextVariable ctx === Nothing .&&. 
           contextType ctx === Nothing .&&. 
           L.null (contextAdditional ctx)
  
  , testProperty "context preserves code information" $
      \code -> 
        let ctx = emptyContext { contextCode = Just code }
        in contextCode ctx === Just code
  
  , testProperty "context preserves function information" $
      \function -> 
        let ctx = emptyContext { contextFunction = Just function }
        in contextFunction ctx === Just function
  
  , testProperty "context preserves additional information" $
      \key value -> 
        let ctx = emptyContext { contextAdditional = [(key, value)] }
        in contextAdditional ctx === [(key, value)]
  ]

-- | Error recovery properties
errorRecoveryProperties :: TestTree
errorRecoveryProperties = testGroup "Error Recovery Properties"
  [ testProperty "fatalRecovery cannot recover" $
      \() -> 
        let recovery = fatalRecovery
        in canRecover recovery === False .&&. 
           shouldContinue recovery === False
  
  , testProperty "errorRecovery can recover L.and continue" $
      \() -> 
        let recovery = errorRecovery
        in canRecover recovery === True .&&. 
           shouldContinue recovery === True
  
  , testProperty "warningRecovery can recover L.and continue" $
      \() -> 
        let recovery = warningRecovery
        in canRecover recovery === True .&&. 
           shouldContinue recovery === True
  
  , testProperty "infoRecovery can recover L.and continue" $
      \() -> 
        let recovery = infoRecovery
        in canRecover recovery === True .&&. 
           shouldContinue recovery === True
  
  , testProperty "customRecovery preserves parameters" $
      \canRec shouldCont action hint cost confidence -> 
        cost >= 0 && cost <= 100 && confidence >= 0.0 && confidence <= 1.0 ==> 
        let recovery = customRecovery canRec shouldCont action hint cost confidence
        in canRecover recovery === canRec .&&. 
           shouldContinue recovery === shouldCont .&&. 
           recoveryAction recovery === action .&&. 
           recoveryHint recovery === hint .&&. 
           recoveryCost recovery === cost .&&. 
           recoveryConfidence recovery === confidence
  
  , testProperty "recovery cost bounds" $
      \recovery -> 
        recoveryCost recovery >= 0 && recoveryCost recovery <= 100
  
  , testProperty "recovery confidence bounds" $
      \recovery -> 
        recoveryConfidence recovery >= 0.0 && recoveryConfidence recovery <= 1.0
  ]

-- | Error collector properties
errorCollectorProperties :: TestTree
errorCollectorProperties = testGroup "Error Collector Properties"
  [ testProperty "hasErrors detects fatal errors" $
      \message -> 
        let error = fatalError message
            errors = [error]
        in hasErrors errors === True
  
  , testProperty "hasErrors detects regular errors" $
      \message -> 
        let error = errorAt "test-id" == category) filtered
  
  , testProperty "filterBySeverity preserves matching errors" $
      \severity errors -> 
        let filtered = filterBySeverity severity errors
        in L.all (\e -> severity e == severity) filtered
  ]

-- | Error formatting properties
errorFormattingProperties :: TestTree
errorFormattingProperties = testGroup "Error Formatting Properties"
  [ testProperty "formatError includes severity" $
      \severity message -> 
        let error = errorAt "test-id" (combinedErrorSeverity e) minSeverity) filtered
  ]

-- Helper functions for testing
errorAt "test-id" sev msg) { category = cat }

fatalError :: Text -> TypeError
fatalError = errorAt "test-id" = Data.List.L.isInfixOf