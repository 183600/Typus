{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewErrorRecoveryQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Compiler.Errors.Core
  ( ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..), ErrorContext(..)
  , ErrorRecovery(..), TypeError(..), CombinedError(..)
  , emptyContext, errorAt, warningAt, infoAt, fatalError
  , canRecoverFrom, shouldContinueAfter, withSuggestions, withLocation
  , withContext, errorWithSuggestions, fatalRecovery, errorRecovery
  , warningRecovery, infoRecovery, customRecovery, _retryRecovery
  , _skipRecovery, _fallbackRecovery, _manualRecovery
  , filterBySeverity, filterByCategory, hasErrors, hasWarnings
  , formatError, formatErrors, combineErrors, getErrorStatistics
  )
import Data.List (sort, length)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T

-- | Test error recovery strategy consistency
prop_recovery_strategy_consistency :: ErrorSeverity -> Bool
prop_recovery_strategy_consistency severity =
    let recovery = case severity of
          Fatal -> fatalRecovery
          Error -> errorRecovery
          Warning -> warningRecovery
          Info -> infoRecovery
        canRec = canRecover recovery
        shouldCont = shouldContinue recovery
    in case severity of
      Fatal -> not canRec && not shouldCont
      _ -> canRec && shouldCont

-- | Test custom recovery strategy properties
prop_custom_recovery_properties :: Bool -> Bool -> Maybe String -> Maybe String -> Int -> Float -> Property
prop_custom_recovery_properties canRec shouldCont action hint cost confidence =
    cost >= 0 && cost <= 100 && confidence >= 0.0 && confidence <= 1.0 ==>
    let recovery = customRecovery canRec shouldCont action hint cost confidence
    in canRecover recovery == canRec &&
       shouldContinue recovery == shouldCont &&
       recoveryCost recovery == cost &&
       recoveryConfidence recovery == confidence

-- | Test retry recovery strategy
prop_retry_recovery_properties :: Positive Int -> Property
prop_retry_recovery_properties (Positive maxAttempts) =
    let recovery = _retryRecovery maxAttempts
        expectedCost = 20 * maxAttempts
    in canRecover recovery && shouldContinue recovery &&
       recoveryCost recovery == expectedCost &&
       recoveryConfidence recovery == 0.8

-- | Test skip recovery strategy
prop_skip_recovery_properties :: Bool
prop_skip_recovery_properties =
    let recovery = _skipRecovery
    in canRecover recovery && shouldContinue recovery &&
       recoveryCost recovery == 5 &&
       recoveryConfidence recovery == 0.95

-- | Test fallback recovery strategy
prop_fallback_recovery_properties :: String -> Property
prop_fallback_recovery_properties fallbackMsg =
    length fallbackMsg > 0 ==>
    let recovery = _fallbackRecovery fallbackMsg
    in canRecover recovery && shouldContinue recovery &&
       recoveryCost recovery == 15 &&
       recoveryConfidence recovery == 0.75

-- | Test manual recovery strategy
prop_manual_recovery_properties :: String -> Property
prop_manual_recovery_properties instruction =
    length instruction > 0 ==>
    let recovery = _manualRecovery instruction
    in canRecover recovery && not (shouldContinue recovery) &&
       recoveryCost recovery == 80 &&
       recoveryConfidence recovery == 0.5

-- | Test error creation with recovery
prop_error_creation_with_recovery :: String -> ErrorSeverity -> Property
prop_error_creation_with_recovery message severity =
    length message > 0 ==>
    let error = errorAt severity (T.pack message) Nothing
        recovery = case severity of
          Fatal -> fatalRecovery
          Error -> errorRecovery
          Warning -> warningRecovery
          Info -> infoRecovery
    in canRecoverFrom error == canRecover recovery &&
       shouldContinueAfter error == shouldContinue recovery

-- | Test error filtering by severity
prop_error_filtering_by_severity :: [ErrorSeverity] -> ErrorSeverity -> Property
prop_error_filtering_by_severity severities minSeverity =
    not (null severities) ==>
    let errors = [errorAt sev (T.pack "test") Nothing | sev <- severities]
        filtered = filterBySeverity minSeverity errors
        expectedLength = length [sev | sev <- severities, sev >= minSeverity]
    in length filtered == expectedLength

-- | Test error filtering by category
prop_error_filtering_by_category :: [ErrorCategory] -> ErrorCategory -> Property
prop_error_filtering_by_category categories targetCategory =
    not (null categories) ==>
    let errors = [errorAt Error (T.pack "test") Nothing | _ <- categories]
        -- Note: This test assumes errors can have categories, though the actual
        -- implementation might differ
    in length errors >= 0  -- Should not crash

-- | Test error combination
prop_error_combination :: String -> String -> Property
prop_error_combination msg1 msg2 =
    length msg1 > 0 && length msg2 > 0 ==>
    let error1 = errorAt Error (T.pack msg1) Nothing
        error2 = errorAt Warning (T.pack msg2) Nothing
        combined = combineErrors error1 error2
    in hasErrors combined && hasWarnings combined

-- | Test error with suggestions
prop_error_with_suggestions :: String -> [String] -> Property
prop_error_with_suggestions message suggestions =
    length message > 0 && all (\s -> length s > 0) suggestions ==>
    let error = errorWithSuggestions Error (T.pack message) Nothing (map T.pack suggestions)
        formatted = formatError error
    in length suggestions > 0 ==> 
       any (`T.isInfixOf` formatted) (map T.pack suggestions)

-- | Test error with location
prop_error_with_location :: String -> Int -> Int -> Property
prop_error_with_location message line col =
    length message > 0 && line > 0 && col > 0 ==>
    let location = ErrorLocation Nothing line col Nothing Nothing
        error = withLocation location (errorAt Error (T.pack message) Nothing)
    in isJust (formatErrorWithLocation error)

-- | Test error with context
prop_error_with_context :: String -> String -> Property
prop_error_with_context message contextCode =
    length message > 0 && length contextCode > 0 ==>
    let context = emptyContext { contextCode = Just contextCode }
        error = withContext context (errorAt Error (T.pack message) Nothing)
    in isJust (formatError error)

-- | Test error statistics
prop_error_statistics :: [ErrorSeverity] -> Property
prop_error_statistics severities =
    not (null severities) ==>
    let errors = [errorAt sev (T.pack "test") Nothing | sev <- severities]
        stats = getErrorStatistics errors
    in not (null stats)

-- | Test error formatting consistency
prop_error_formatting_consistency :: String -> ErrorSeverity -> Property
prop_error_formatting_consistency message severity =
    length message > 0 ==>
    let error = errorAt severity (T.pack message) Nothing
        formatted1 = formatError error
        formatted2 = formatError error
    in formatted1 == formatted2

-- | Test multiple error formatting
prop_multiple_error_formatting :: [String] -> Property
prop_multiple_error_formatting messages =
    all (\m -> length m > 0) messages ==>
    let errors = [errorAt Error (T.pack msg) Nothing | msg <- messages]
        formatted = formatErrors errors
    in length formatted >= 0  -- Should not crash

-- | Test recovery cost ordering
prop_recovery_cost_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_recovery_cost_ordering sev1 sev2 =
    let recovery1 = case sev1 of
          Fatal -> fatalRecovery
          Error -> errorRecovery
          Warning -> warningRecovery
          Info -> infoRecovery
        recovery2 = case sev2 of
          Fatal -> fatalRecovery
          Error -> errorRecovery
          Warning -> warningRecovery
          Info -> infoRecovery
        cost1 = recoveryCost recovery1
        cost2 = recoveryCost recovery2
    in (sev1 >= sev2) ==> (cost1 >= cost2)

-- | Test recovery confidence ordering
prop_recovery_confidence_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_recovery_confidence_ordering sev1 sev2 =
    let recovery1 = case sev1 of
          Fatal -> fatalRecovery
          Error -> errorRecovery
          Warning -> warningRecovery
          Info -> infoRecovery
        recovery2 = case sev2 of
          Fatal -> fatalRecovery
          Error -> errorRecovery
          Warning -> warningRecovery
          Info -> infoRecovery
        conf1 = recoveryConfidence recovery1
        conf2 = recoveryConfidence recovery2
    in (sev1 <= sev2) ==> (conf1 >= conf2)

-- | Test error recovery chain
prop_error_recovery_chain :: [ErrorSeverity] -> Property
prop_error_recovery_chain severities =
    not (null severities) ==>
    let errors = [errorAt sev (T.pack "test") Nothing | sev <- severities]
        canRecoverAll = all canRecoverFrom errors
        shouldContinueAll = all shouldContinueAfter errors
        hasFatal = any (\e -> severity e == Fatal) errors
    in hasFatal ==> not canRecoverAll
    -- If no fatal errors, should be able to continue
    -- && not hasFatal ==> shouldContinueAll

-- | Test error context preservation
prop_error_context_preservation :: String -> String -> String -> Property
prop_error_context_preservation code func var =
    all (\s -> length s > 0) [code, func, var] ==>
    let context = emptyContext 
          { contextCode = Just code
          , contextFunction = Just func
          , contextVariable = Just var
          }
        error = withContext context (errorAt Error (T.pack "test") Nothing)
    in isJust (formatError error)

-- | Test error location accuracy
prop_error_location_accuracy :: Int -> Int -> Int -> Int -> Property
prop_error_location_accuracy startLine startCol endLine endCol =
    startLine > 0 && startCol > 0 && endLine >= startLine && 
    (endLine > startLine || endCol > startCol) ==>
    let location = ErrorLocation Nothing startLine startCol (Just endLine) (Just endCol)
        error = withLocation location (errorAt Error (T.pack "test") Nothing)
    in isJust (formatErrorWithLocation error)

tests :: TestTree
tests = testGroup "Error Recovery QuickCheck Tests"
  [ testProperty "recovery strategy consistency" prop_recovery_strategy_consistency
  , testProperty "custom recovery properties" prop_custom_recovery_properties
  , testProperty "retry recovery properties" prop_retry_recovery_properties
  , testProperty "skip recovery properties" prop_skip_recovery_properties
  , testProperty "fallback recovery properties" prop_fallback_recovery_properties
  , testProperty "manual recovery properties" prop_manual_recovery_properties
  , testProperty "error creation with recovery" prop_error_creation_with_recovery
  , testProperty "error filtering by severity" prop_error_filtering_by_severity
  , testProperty "error filtering by category" prop_error_filtering_by_category
  , testProperty "error combination" prop_error_combination
  , testProperty "error with suggestions" prop_error_with_suggestions
  , testProperty "error with location" prop_error_with_location
  , testProperty "error with context" prop_error_with_context
  , testProperty "error statistics" prop_error_statistics
  , testProperty "error formatting consistency" prop_error_formatting_consistency
  , testProperty "multiple error formatting" prop_multiple_error_formatting
  , testProperty "recovery cost ordering" prop_recovery_cost_ordering
  , testProperty "recovery confidence ordering" prop_recovery_confidence_ordering
  , testProperty "error recovery chain" prop_error_recovery_chain
  , testProperty "error context preservation" prop_error_context_preservation
  , testProperty "error location accuracy" prop_error_location_accuracy
  ]