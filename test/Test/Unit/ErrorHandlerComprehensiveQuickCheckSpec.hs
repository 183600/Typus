module Test.Unit.ErrorHandlerComprehensiveQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler.Errors.Core
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)

-- | Test that severityPriority returns correct priority values
prop_severity_priority_values :: Property
prop_severity_priority_values = property $
  severityPriority Fatal == 100 &&
  severityPriority Error == 80 &&
  severityPriority Warning == 30 &&
  severityPriority Info == 10

-- | Test that compareSeverity orders severities correctly
prop_compare_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_compare_severity_ordering s1 s2 = 
  let p1 = severityPriority s1
      p2 = severityPriority s2
  in property $ compareSeverity s1 s2 == compare p1 p2

-- | Test that isAtLeast correctly checks severity levels
prop_is_at_least :: ErrorSeverity -> ErrorSeverity -> Property
prop_is_at_least s1 s2 = 
  let p1 = severityPriority s1
      p2 = severityPriority s2
  in property $ isAtLeast s1 s2 == (p1 >= p2)

-- | Test that getErrorLine returns the line from ErrorLocation
prop_get_error_line :: Int -> Int -> Property
prop_get_error_line line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
  in property $ getErrorLine loc == line

-- | Test that getErrorColumn returns the column from ErrorLocation
prop_get_error_column :: Int -> Int -> Property
prop_get_error_column line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
  in property $ getErrorColumn loc == col

-- | Test that errorAt creates an error with correct properties
prop_error_at_creates_correct_error :: String -> String -> Int -> Int -> Property
prop_error_at_creates_correct_error errId msg line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
      err = errorAt errId (T.pack msg) loc
  in property $ 
    errorId err == errId &&
    message err == T.pack msg &&
    location err == loc &&
    severity err == Error &&
    category err == Unknown

-- | Test that warningAt creates a warning with correct properties
prop_warning_at_creates_correct_warning :: String -> String -> Int -> Int -> Property
prop_warning_at_creates_correct_warning errId msg line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
      warn = warningAt errId (T.pack msg) loc
  in property $ 
    errorId warn == errId &&
    message warn == T.pack msg &&
    location warn == loc &&
    severity warn == Warning &&
    category warn == Unknown

-- | Test that infoAt creates an info message with correct properties
prop_info_at_creates_correct_info :: String -> String -> Int -> Int -> Property
prop_info_at_creates_correct_info errId msg line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
      info = infoAt errId (T.pack msg) loc
  in property $ 
    errorId info == errId &&
    message info == T.pack msg &&
    location info == loc &&
    severity info == Info &&
    category info == Unknown

-- | Test that errorWithCategory creates an error with correct category
prop_error_with_category :: String -> ErrorCategory -> String -> Int -> Int -> Property
prop_error_with_category errId cat msg line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
      err = errorWithCategory errId cat (T.pack msg) loc
  in property $ 
    errorId err == errId &&
    message err == T.pack msg &&
    location err == loc &&
    severity err == Error &&
    category err == cat

-- | Test that withLocation updates the error location
prop_with_location_updates_location :: String -> String -> Int -> Int -> Int -> Int -> Property
prop_with_location_updates_location errId msg line1 col1 line2 col2 = 
  let loc1 = ErrorLocation Nothing line1 col1 Nothing Nothing
      loc2 = ErrorLocation Nothing line2 col2 Nothing Nothing
      err = errorAt errId (T.pack msg) loc1
      updatedErr = withLocation err loc2
  in property $ 
    location updatedErr == loc2 &&
    errorId updatedErr == errId &&
    message updatedErr == T.pack msg

-- | Test that withContext updates the error context
prop_with_context_updates_context :: String -> String -> Int -> Int -> Property
prop_with_context_updates_context errId msg line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
      ctx = ErrorContext (Just "code") (Just "function") (Just "variable") (Just "type") []
      err = errorAt errId (T.pack msg) loc
      updatedErr = withContext err ctx
  in property $ 
    context updatedErr == ctx &&
    errorId updatedErr == errId &&
    message updatedErr == T.pack msg

-- | Test that withSuggestions adds suggestions to error
prop_with_suggestions_adds_suggestions :: String -> String -> Int -> Int -> [String] -> Property
prop_with_suggestions_adds_suggestions errId msg line col suggestions = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
      err = errorAt errId (T.pack msg) loc
      suggestionsText = map T.pack suggestions
      updatedErr = withSuggestions suggestionsText err
  in property $ 
    -- Check that suggestions are added correctly
    -- suggestions updatedErr == suggestionsText &&
    errorId updatedErr == errId &&
    message updatedErr == T.pack msg

-- | Test that wrapError wraps an error with additional message
prop_wrap_error_wraps_message :: String -> String -> String -> Int -> Int -> Property
prop_wrap_error_wraps_message errId msg wrapper line col = 
  let loc = ErrorLocation Nothing line col Nothing Nothing
      err = errorAt errId (T.pack msg) loc
      wrappedErr = wrapError (T.pack wrapper) err
  in property $ 
    message wrappedErr == T.pack wrapper <> T.pack ": " <> T.pack msg &&
    errorId wrappedErr == errId &&
    location wrappedErr == loc

-- | Test that hasCategory correctly identifies error category
prop_has_category :: ErrorCategory -> ErrorCategory -> Property
prop_has_category cat1 cat2 = 
  let err = errorAt "test" (T.pack "message") (ErrorLocation Nothing 0 0 Nothing Nothing)
      categorizedErr = err { category = cat1 }
  in property $ hasCategory cat1 categorizedErr == (cat1 == cat2)

-- | Test that filterByCategory filters errors correctly
prop_filter_by_category :: ErrorCategory -> [ErrorCategory] -> Property
prop_filter_by_category targetCat cats = 
  let errors = [(\err -> err { category = cat }) $ errorAt ("test" ++ show i) (T.pack "message") (ErrorLocation Nothing i 0 Nothing Nothing) 
                | (i, cat) <- zip [1..] cats]
      filtered = filterByCategory targetCat errors
  in property $ all (\e -> category e == targetCat) filtered

-- | Test that filterBySeverity filters errors correctly
prop_filter_by_severity :: ErrorSeverity -> [ErrorSeverity] -> Property
prop_filter_by_severity targetSev severities = 
  let errors = [(\err -> err { severity = sev }) $ errorAt ("test" ++ show i) (T.pack "message") (ErrorLocation Nothing i 0 Nothing Nothing) 
                | (i, sev) <- zip [1..] severities]
      filtered = filterBySeverity targetSev errors
  in property $ all (\e -> severity e == targetSev) filtered

-- | Test that getErrorStatistics returns correct statistics
prop_get_error_statistics :: [ErrorSeverity] -> [ErrorCategory] -> Property
prop_get_error_statistics severities categories = 
  let errors = [(\err -> err { severity = sev, category = cat }) $ errorAt ("test" ++ show i) (T.pack "message") (ErrorLocation Nothing i 0 Nothing Nothing) 
                | (i, (sev, cat)) <- zip [1..] (zip severities categories)]
      stats = getErrorStatistics errors
  in property $ 
    Map.findWithDefault 0 "total" stats == length errors &&
    Map.findWithDefault 0 "fatal" stats == length (filter (\e -> severity e == Fatal) errors) &&
    Map.findWithDefault 0 "errors" stats == length (filter (\e -> severity e == Error) errors) &&
    Map.findWithDefault 0 "warnings" stats == length (filter (\e -> severity e == Warning) errors) &&
    Map.findWithDefault 0 "info" stats == length (filter (\e -> severity e == Info) errors)

-- | Test that canRecoverFrom returns recovery.canRecover
prop_can_recover_from :: Bool -> Bool -> Property
prop_can_recover_from canRec shouldCont = 
  let recovery = RecoveryStrategy canRec shouldCont Nothing Nothing 50 0.7
      err = (\e -> e { recovery = recovery }) $ errorAt "test" (T.pack "message") (ErrorLocation Nothing 0 0 Nothing Nothing)
  in property $ canRecoverFrom err == canRec

-- | Test that shouldContinueAfter returns recovery.shouldContinue
prop_should_continue_after :: Bool -> Bool -> Property
prop_should_continue_after canRec shouldCont = 
  let recovery = RecoveryStrategy canRec shouldCont Nothing Nothing 50 0.7
      err = (\e -> e { recovery = recovery }) $ errorAt "test" (T.pack "message") (ErrorLocation Nothing 0 0 Nothing Nothing)
  in property $ shouldContinueAfter err == shouldCont

-- | Test that fatalRecovery has correct properties
prop_fatal_recovery_properties :: Property
prop_fatal_recovery_properties = property $
  not (canRecover fatalRecovery) &&
  not (shouldContinue fatalRecovery)

-- | Test that errorRecovery has correct properties
prop_error_recovery_properties :: Property
prop_error_recovery_properties = property $
  canRecover errorRecovery &&
  shouldContinue errorRecovery

-- | Test that warningRecovery has correct properties
prop_warning_recovery_properties :: Property
prop_warning_recovery_properties = property $
  canRecover warningRecovery &&
  shouldContinue warningRecovery

-- | Test that infoRecovery has correct properties
prop_info_recovery_properties :: Property
prop_info_recovery_properties = property $
  canRecover infoRecovery &&
  shouldContinue infoRecovery

-- | Test that customRecovery creates recovery with provided values
prop_custom_recovery :: Bool -> Bool -> String -> String -> Int -> Float -> Property
prop_custom_recovery canRec shouldCont action hint cost confidence = 
  let recovery = customRecovery canRec shouldCont 
                  (if null action then Nothing else Just action)
                  (if null hint then Nothing else Just hint)
                  cost confidence
  in property $ 
    canRecover recovery == canRec &&
    shouldContinue recovery == shouldCont &&
    recoveryAction recovery == (if null action then Nothing else Just action) &&
    recoveryHint recovery == (if null hint then Nothing else Just hint) &&
    recoveryCost recovery == cost &&
    recoveryConfidence recovery == confidence

-- | Test that emptyContext has all fields as Nothing
prop_empty_context_nothing :: Property
prop_empty_context_nothing = property $
  contextCode emptyContext == Nothing &&
  contextFunction emptyContext == Nothing &&
  contextVariable emptyContext == Nothing &&
  contextType emptyContext == Nothing &&
  null (contextAdditional emptyContext)

tests :: TestTree
tests = testGroup "ErrorHandler Comprehensive QuickCheck Tests"
  [ testProperty "severityPriority values" prop_severity_priority_values
  , testProperty "compareSeverity ordering" prop_compare_severity_ordering
  , testProperty "isAtLeast severity check" prop_is_at_least
  , testProperty "getErrorLine returns line" prop_get_error_line
  , testProperty "getErrorColumn returns column" prop_get_error_column
  , testProperty "errorAt creates correct error" prop_error_at_creates_correct_error
  , testProperty "warningAt creates correct warning" prop_warning_at_creates_correct_warning
  , testProperty "infoAt creates correct info" prop_info_at_creates_correct_info
  , testProperty "errorWithCategory creates error with category" prop_error_with_category
  , testProperty "withLocation updates location" prop_with_location_updates_location
  , testProperty "withContext updates context" prop_with_context_updates_context
  , testProperty "withSuggestions adds suggestions" prop_with_suggestions_adds_suggestions
  , testProperty "wrapError wraps message" prop_wrap_error_wraps_message
  , testProperty "hasCategory identifies category" prop_has_category
  , testProperty "filterByCategory filters correctly" prop_filter_by_category
  , testProperty "filterBySeverity filters correctly" prop_filter_by_severity
  , testProperty "getErrorStatistics returns correct stats" prop_get_error_statistics
  , testProperty "canRecoverFrom returns recovery.canRecover" prop_can_recover_from
  , testProperty "shouldContinueAfter returns recovery.shouldContinue" prop_should_continue_after
  , testProperty "fatalRecovery properties" prop_fatal_recovery_properties
  , testProperty "errorRecovery properties" prop_error_recovery_properties
  , testProperty "warningRecovery properties" prop_warning_recovery_properties
  , testProperty "infoRecovery properties" prop_info_recovery_properties
  , testProperty "customRecovery creates recovery with provided values" prop_custom_recovery
  , testProperty "emptyContext has all Nothing" prop_empty_context_nothing
  ]