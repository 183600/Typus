{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Error handling QuickCheck tests for the Typus compiler
-- This module contains property-based tests for error handling utilities
module Test.Unit.NewErrorHandlingQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck ((==>), conjoin, counterexample)
import Utils
  ( trim
  , splitBy
  , splitByComma
  , removeLineComments
  , removeComments
  , safeProcessString
  , isValidChar
  , breakOn
  )
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Control.Monad (foldM)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Compiler.Errors.Core as Error
import Compiler.Errors.Core (ErrorSeverity(..), ErrorCategory(..), TypeError(..), ErrorLocation(..), ErrorContext(..),
                            errorAt, errorWithCategory, warningAt, infoAt, 
                            fatalError, withLocation, withContext, combineErrors,
                            combinedErrorSeverity, filterByCategory, filterBySeverity,
                            hasCategory, isAtLeast, severityPriority, location, line, column, 
                            fatalRecovery, emptyContext, contextCode)
import SourceLocation (toErrorLocation)
import Data.Time (UTCTime, getCurrentTime)
import Data.List (sort, nub)
import Data.Ord (comparing)

-- ============================================================================
-- Helper Types and Functions
-- ============================================================================

-- | Custom error type for testing
data TestError = TestError
  { errorMessage :: String
  , errorSeverity :: ErrorSeverity
  , errorCategory :: ErrorCategory'
  , errorLocation :: ErrorLocation
  } deriving (Show, Eq)
dummyLoc :: ErrorLocation
dummyLoc = ErrorLocation (Just "test") 1 1 Nothing Nothing

-- | Custom error category for testing
data ErrorCategory' = SyntaxError' | TypeError' | RuntimeError' | WarningCategory'
  deriving (Show, Eq, Enum)

-- | Arbitrary instance for ErrorCategory'
instance Arbitrary ErrorCategory' where
  arbitrary = elements [SyntaxError', TypeError', RuntimeError', WarningCategory']

-- | Arbitrary instance for ErrorSeverity
instance Arbitrary ErrorSeverity where
  arbitrary = elements [Info, Warning, Error, Fatal]

-- | Arbitrary instance for TestError
instance Arbitrary TestError where
  arbitrary = do
    msg <- arbitrary
    severity <- arbitrary
    category <- arbitrary
    return $ TestError msg severity category dummyLoc

-- | Create a test error
mkTestError :: String -> ErrorSeverity -> ErrorCategory' -> TestError
mkTestError msg severity category = TestError msg severity category dummyLoc

-- | Convert custom category to core category
toErrorCategory :: ErrorCategory' -> ErrorCategory
toErrorCategory SyntaxError' = Error.Parsing
toErrorCategory TypeError' = Error.TypeChecking
toErrorCategory RuntimeError' = Error.Runtime
toErrorCategory WarningCategory' = Error.Semantic

-- | Check if error is critical
isCriticalError :: TestError -> Bool
isCriticalError err = errorSeverity err == Fatal || errorSeverity err == Error

-- | Check if error is recoverable
isRecoverableError :: TestError -> Bool
isRecoverableError err = errorSeverity err `elem` [Warning, Info]

-- | Check if error has context
hasErrorContext :: TestError -> Bool
hasErrorContext err = not (null (errorMessage err))

-- | Combine two errors
combineErrors' :: TestError -> TestError -> TestError
combineErrors' e1 e2 = TestError
  { errorMessage = errorMessage e1 ++ "; " ++ errorMessage e2
  , errorSeverity = max (errorSeverity e1) (errorSeverity e2)
  , errorCategory = errorCategory e1 -- Keep first category
  , errorLocation = errorLocation e1 -- Keep first location
  }

-- | Filter errors by severity
filterBySeverity' :: ErrorSeverity -> [TestError] -> [TestError]
filterBySeverity' severity = filter ((>= severity) . errorSeverity)

-- | Filter errors by category
filterByCategory' :: ErrorCategory' -> [TestError] -> [TestError]
filterByCategory' category = filter ((== category) . errorCategory)

-- | Check if error has specific category
hasCategory' :: ErrorCategory' -> TestError -> Bool
hasCategory' category err = errorCategory err == category

-- | Check if error is at least as severe as given severity
isAtLeast' :: ErrorSeverity -> TestError -> Bool
isAtLeast' severity err = errorSeverity err >= severity

-- | Get severity priority
severityPriority' :: ErrorSeverity -> Int
severityPriority' Info = 1
severityPriority' Warning = 2
severityPriority' Error = 3
severityPriority' Fatal = 4

-- ============================================================================
-- Error Creation Tests
-- ============================================================================

-- | Test error creation: basic properties
prop_error_creation_basic :: String -> ErrorSeverity -> ErrorCategory' -> Bool
prop_error_creation_basic msg severity category = 
  let err = mkTestError msg severity category
  in errorMessage err == msg &&
     errorSeverity err == severity &&
     errorCategory err == category

-- | Test error creation: empty message
prop_error_creation_empty :: ErrorSeverity -> ErrorCategory' -> Bool
prop_error_creation_empty severity category = 
  let err = mkTestError "" severity category
  in null (errorMessage err) &&
     errorSeverity err == severity &&
     errorCategory err == category

-- | Test error creation: long message
prop_error_creation_long :: String -> ErrorSeverity -> ErrorCategory' -> Bool
prop_error_creation_long s severity category = 
  let longMsg = concat (replicate 1000 s)
      err = mkTestError longMsg severity category
  in length (errorMessage err) >= 1000 &&
     errorSeverity err == severity &&
     errorCategory err == category

-- | Test error creation: special characters
prop_error_creation_special :: String -> ErrorSeverity -> ErrorCategory' -> Bool
prop_error_creation_special s severity category = 
  let specialMsg = s ++ "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      err = mkTestError specialMsg severity category
  in errorMessage err == specialMsg &&
     errorSeverity err == severity &&
     errorCategory err == category

-- | Test error creation: unicode characters
prop_error_creation_unicode :: String -> ErrorSeverity -> ErrorCategory' -> Bool
prop_error_creation_unicode s severity category = 
  let unicodeMsg = s ++ "ñáéíóú你好世界"
      err = mkTestError unicodeMsg severity category
  in errorMessage err == unicodeMsg &&
     errorSeverity err == severity &&
     errorCategory err == category

-- ============================================================================
-- Error Classification Tests
-- ============================================================================

-- | Test error classification: critical errors
prop_error_classification_critical :: String -> ErrorSeverity -> ErrorCategory' -> Bool
prop_error_classification_critical msg severity category = 
  let err = mkTestError msg severity category
  in isCriticalError err == (severity `elem` [Fatal, Error])

-- | Test error classification: recoverable errors
prop_error_classification_recoverable :: String -> ErrorSeverity -> ErrorCategory' -> Bool
prop_error_classification_recoverable msg severity category = 
  let err = mkTestError msg severity category
  in isRecoverableError err == (severity `elem` [Warning, Info])

-- | Test error classification: has context
prop_error_classification_hasContext :: String -> ErrorSeverity -> ErrorCategory' -> Bool
prop_error_classification_hasContext msg severity category = 
  let err = mkTestError msg severity category
  in hasErrorContext err == not (null msg)

-- | Test error classification: by category
prop_error_classification_category :: String -> ErrorSeverity -> ErrorCategory' -> ErrorCategory' -> Bool
prop_error_classification_category msg severity cat1 cat2 = 
  let err = mkTestError msg severity cat1
  in hasCategory' cat2 err == (cat1 == cat2)

-- | Test error classification: by severity
prop_error_classification_severity :: String -> ErrorSeverity -> ErrorCategory' -> ErrorSeverity -> Bool
prop_error_classification_severity msg severity1 category severity2 = 
  let err = mkTestError msg severity1 category
  in isAtLeast' severity2 err == (severity1 >= severity2)

-- ============================================================================
-- Error Combination Tests
-- ============================================================================

-- | Test error combination: basic properties
prop_error_combination_basic :: String -> ErrorSeverity -> ErrorCategory' -> String -> ErrorSeverity -> ErrorCategory' -> Bool
prop_error_combination_basic msg1 sev1 cat1 msg2 sev2 cat2 = 
  let e1 = mkTestError msg1 sev1 cat1
      e2 = mkTestError msg2 sev2 cat2
      combined = combineErrors' e1 e2
  in errorMessage combined == msg1 ++ "; " ++ msg2 &&
     errorSeverity combined == max sev1 sev2 &&
     errorCategory combined == cat1

-- | Test error combination: commutativity of severity
prop_error_combination_severity_commutative :: String -> ErrorSeverity -> ErrorCategory' -> String -> ErrorSeverity -> ErrorCategory' -> Bool
prop_error_combination_severity_commutative msg1 sev1 cat1 msg2 sev2 cat2 = 
  let e1 = mkTestError msg1 sev1 cat1
      e2 = mkTestError msg2 sev2 cat2
      combined1 = combineErrors' e1 e2
      combined2 = combineErrors' e2 e1
  in errorSeverity combined1 == errorSeverity combined2

-- | Test error combination: associativity
prop_error_combination_associative :: String -> ErrorSeverity -> ErrorCategory' -> String -> ErrorSeverity -> ErrorCategory' -> String -> ErrorSeverity -> ErrorCategory' -> Bool
prop_error_combination_associative msg1 sev1 cat1 msg2 sev2 cat2 msg3 sev3 cat3 = 
  let e1 = mkTestError msg1 sev1 cat1
      e2 = mkTestError msg2 sev2 cat2
      e3 = mkTestError msg3 sev3 cat3
      combined1 = combineErrors' (combineErrors' e1 e2) e3
      combined2 = combineErrors' e1 (combineErrors' e2 e3)
  in errorSeverity combined1 == errorSeverity combined2

-- | Test error combination: identity element
prop_error_combination_identity :: String -> ErrorSeverity -> ErrorCategory' -> Bool
prop_error_combination_identity msg severity category = 
  let err = mkTestError msg severity category
      empty = mkTestError "" Info WarningCategory'
      combined = combineErrors' err empty
  in errorSeverity combined == severity &&
     errorCategory combined == category

-- | Test error combination: idempotence for same severity
prop_error_combination_idempotent :: String -> ErrorSeverity -> ErrorCategory' -> String -> ErrorSeverity -> Bool
prop_error_combination_idempotent msg1 severity category msg2 severity2 = 
  if severity `elem` [Info, Warning] && severity2 `elem` [Info, Warning]
  then let e1 = mkTestError msg1 severity category
           e2 = mkTestError msg2 severity2 category
           combined1 = combineErrors' e1 e2
           combined2 = combineErrors' e2 e1
       in errorSeverity combined1 == errorSeverity combined2 &&
          errorCategory combined1 == category
  else True
-- ============================================================================
-- Error Filtering Tests
-- ============================================================================

-- | Test error filtering: by severity
prop_error_filtering_severity :: String -> ErrorSeverity -> ErrorCategory' -> [TestError] -> Bool
prop_error_filtering_severity msg severity category errors = 
  let err = mkTestError msg severity category
      allErrors = err : errors
      filtered = filterBySeverity' severity allErrors
  in all (\e -> errorCategory e == category) filtered &&
      all (\e -> isAtLeast' severity e) filtered

-- | Test error filtering: by category
prop_error_filtering_category :: String -> ErrorSeverity -> ErrorCategory' -> [TestError] -> Bool
prop_error_filtering_category msg severity category errors = 
  let err = mkTestError msg severity category
      allErrors = err : errors
      filtered = filterByCategory' category allErrors
  in all (\e -> hasCategory' category e) filtered &&
     err `elem` filtered

-- | Test error filtering: empty list
prop_error_filtering_empty :: ErrorSeverity -> ErrorCategory' -> Bool
prop_error_filtering_empty severity category = 
  let filteredBySev = filterBySeverity' severity []
      filteredByCat = filterByCategory' category []
  in null filteredBySev && null filteredByCat

-- | Test error filtering: no matches
prop_error_filtering_no_matches :: String -> ErrorSeverity -> ErrorCategory' -> [TestError] -> Bool
prop_error_filtering_no_matches msg severity category errors = 
  let allDifferentSeverities = filter ((/= severity) . errorSeverity) errors
      allDifferentCategories = filter ((/= category) . errorCategory) errors
      filteredBySev = filterBySeverity' severity allDifferentSeverities
      filteredByCat = filterByCategory' category allDifferentCategories
  in null filteredBySev && null filteredByCat

-- | Test error filtering: all matches
prop_error_filtering_all_matches :: String -> ErrorSeverity -> ErrorCategory' -> Int -> Bool
prop_error_filtering_all_matches msg severity category n = 
  if n > 0 
  then let errors = replicate n (mkTestError msg severity category)
           filteredBySev = filterBySeverity' severity errors
           filteredByCat = filterByCategory' category errors
       in length filteredBySev == n && length filteredByCat == n
  else True

-- ============================================================================
-- Error Priority Tests
-- ============================================================================

-- | Test error priority: severity ordering
prop_error_priority_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Bool
prop_error_priority_severity_ordering sev1 sev2 = 
  let priority1 = severityPriority' sev1
      priority2 = severityPriority' sev2
  in (sev1 >= sev2) == (priority1 >= priority2)

-- | Test error priority: maximum severity
prop_error_priority_max_severity :: [TestError] -> Bool
prop_error_priority_max_severity errors = 
  case errors of
    [] -> True
    (e:es) -> 
      let maxSev = maximum (map errorSeverity errors)
          maxPrio = maximum (map (severityPriority' . errorSeverity) errors)
          expectedPrio = severityPriority' maxSev
      in maxPrio == expectedPrio

-- | Test error priority: consistency
prop_error_priority_consistency :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Bool
prop_error_priority_consistency sev1 sev2 sev3 = 
  let p1 = severityPriority' sev1
      p2 = severityPriority' sev2
      p3 = severityPriority' sev3
  in if p1 >= p2 && p2 >= p3 then p1 >= p3 else True

-- | Test error priority: uniqueness
prop_error_priority_uniqueness :: [ErrorSeverity] -> Bool
prop_error_priority_uniqueness severities = 
  let priorities = map severityPriority' severities
      uniquePriorities = nub priorities
  in length priorities == length uniquePriorities

-- | Test error priority: range
prop_error_priority_range :: ErrorSeverity -> Bool
prop_error_priority_range severity = 
  let priority = severityPriority' severity
  in priority >= 1 && priority <= 4

-- ============================================================================
-- Error Context Tests
-- ============================================================================

-- | Test error context: message preservation
prop_error_context_message :: String -> ErrorSeverity -> ErrorCategory' -> Bool
prop_error_context_message msg severity category = 
  let err = mkTestError msg severity category
  in errorMessage err == msg

-- | Test error context: context preservation
prop_error_context_preservation :: String -> ErrorSeverity -> ErrorCategory' -> String -> ErrorSeverity -> ErrorCategory' -> Bool
prop_error_context_preservation msg1 sev1 cat1 msg2 sev2 cat2 = 
  let e1 = mkTestError msg1 sev1 cat1
      e2 = mkTestError msg2 sev2 cat2
      combined = combineErrors' e1 e2
  in msg1 `isInfixOf` errorMessage combined &&
     msg2 `isInfixOf` errorMessage combined

-- | Test error context: context ordering
prop_error_context_ordering :: String -> ErrorSeverity -> ErrorCategory' -> String -> ErrorSeverity -> ErrorCategory' -> Bool
prop_error_context_ordering msg1 sev1 cat1 msg2 sev2 cat2 = 
  let e1 = mkTestError msg1 sev1 cat1
      e2 = mkTestError msg2 sev2 cat2
      combined = combineErrors' e1 e2
  in errorMessage combined == msg1 ++ "; " ++ msg2

-- | Test error context: empty context
prop_error_context_empty :: ErrorSeverity -> ErrorCategory' -> Bool
prop_error_context_empty severity category = 
  let err = mkTestError "" severity category
  in null (errorMessage err) && not (hasErrorContext err)

-- | Test error context: nested context
prop_error_context_nested :: String -> ErrorSeverity -> ErrorCategory' -> String -> ErrorSeverity -> ErrorCategory' -> String -> ErrorSeverity -> ErrorCategory' -> Bool
prop_error_context_nested msg1 sev1 cat1 msg2 sev2 cat2 msg3 sev3 cat3 = 
  let e1 = mkTestError msg1 sev1 cat1
      e2 = mkTestError msg2 sev2 cat2
      e3 = mkTestError msg3 sev3 cat3
      combined = combineErrors' (combineErrors' e1 e2) e3
  in msg1 `isInfixOf` errorMessage combined &&
     msg2 `isInfixOf` errorMessage combined &&
     msg3 `isInfixOf` errorMessage combined

-- ============================================================================
-- Error Recovery Tests
-- ============================================================================

-- | Test error recovery: recoverable errors
prop_error_recovery_recoverable :: String -> ErrorSeverity -> ErrorCategory' -> Bool
prop_error_recovery_recoverable msg severity category = 
  let err = mkTestError msg severity category
  in isRecoverableError err == (severity `elem` [Warning, Info])

-- | Test error recovery: critical errors
prop_error_recovery_critical :: String -> ErrorSeverity -> ErrorCategory' -> Bool
prop_error_recovery_critical msg severity category = 
  let err = mkTestError msg severity category
  in not (isRecoverableError err) == (severity `elem` [Error, Fatal])

-- | Test error recovery: recovery strategy
prop_error_recovery_strategy :: String -> ErrorSeverity -> ErrorCategory' -> Bool
prop_error_recovery_strategy msg severity category = 
  let err = mkTestError msg severity category
  in case severity of
        Info -> True -- Always recoverable
        Warning -> True -- Usually recoverable
        Error -> length msg < 100 -- Recoverable if short message
        Fatal -> False -- Never recoverable

-- | Test error recovery: recovery chain
prop_error_recovery_chain :: [TestError] -> Bool
prop_error_recovery_chain errors = 
  let recoverable = filter isRecoverableError errors
      critical = filter (not . isRecoverableError) errors
  in null critical || length recoverable >= length critical `div` 2

-- | Test error recovery: recovery priority
prop_error_recovery_priority :: TestError -> TestError -> Bool
prop_error_recovery_priority e1 e2 = 
  let recoverable1 = isRecoverableError e1
      recoverable2 = isRecoverableError e2
      severity1 = errorSeverity e1
      severity2 = errorSeverity e2
  in if recoverable1 && not recoverable2
     then severityPriority' severity1 <= severityPriority' severity2
     else True

-- ============================================================================
-- Error Aggregation Tests
-- ============================================================================

-- | Test error aggregation: count preservation
prop_error_aggregation_count :: [TestError] -> Bool
prop_error_aggregation_count errors = 
  let combined = foldl combineErrors' (mkTestError "" Info WarningCategory') errors
      expectedCount = length errors + 1
  in length (words (errorMessage combined)) >= expectedCount

-- | Test error aggregation: severity preservation
prop_error_aggregation_severity :: [TestError] -> Bool
prop_error_aggregation_severity errors = 
  case errors of
    [] -> True
    (e:es) -> 
      let combined = foldl combineErrors' e es
          maxSeverity = maximum (map errorSeverity errors)
      in errorSeverity combined == maxSeverity

-- | Test error aggregation: category preservation
prop_error_aggregation_category :: [TestError] -> Bool
prop_error_aggregation_category errors = 
  case errors of
    [] -> True
    (e:es) -> 
      let combined = foldl combineErrors' e es
          firstCategory = errorCategory e
      in errorCategory combined == firstCategory

-- | Test error aggregation: empty list
prop_error_aggregation_empty :: Bool
prop_error_aggregation_empty = 
  let combined = foldl combineErrors' (mkTestError "" Info WarningCategory') []
  in errorSeverity combined == Info && errorCategory combined == WarningCategory'

-- | Test error aggregation: single error
prop_error_aggregation_single :: String -> ErrorSeverity -> ErrorCategory' -> Bool
prop_error_aggregation_single msg severity category = 
  let err = mkTestError msg severity category
      combined = foldl combineErrors' err []
  in combined == err

-- ============================================================================
-- Test Group
-- ============================================================================

tests :: TestTree
tests = testGroup "Error Handling QuickCheck Tests"
  [ -- Error Creation Tests
    testProperty "error creation basic" prop_error_creation_basic
  , testProperty "error creation empty" prop_error_creation_empty
  , testProperty "error creation long" prop_error_creation_long
  , testProperty "error creation special" prop_error_creation_special
  , testProperty "error creation unicode" prop_error_creation_unicode
  
  -- Error Classification Tests
  , testProperty "error classification critical" prop_error_classification_critical
  , testProperty "error classification recoverable" prop_error_classification_recoverable
  , testProperty "error classification hasContext" prop_error_classification_hasContext
  , testProperty "error classification category" prop_error_classification_category
  , testProperty "error classification severity" prop_error_classification_severity
  
  -- Error Combination Tests
  , testProperty "error combination basic" prop_error_combination_basic
  , testProperty "error combination severity commutative" prop_error_combination_severity_commutative
  , testProperty "error combination associative" prop_error_combination_associative
  , testProperty "error combination identity" prop_error_combination_identity
  , testProperty "error combination idempotent" prop_error_combination_idempotent
  
  -- Error Filtering Tests
  , testProperty "error filtering severity" prop_error_filtering_severity
  , testProperty "error filtering category" prop_error_filtering_category
  , testProperty "error filtering empty" prop_error_filtering_empty
  , testProperty "error filtering no matches" prop_error_filtering_no_matches
  , testProperty "error filtering all matches" prop_error_filtering_all_matches
  
  -- Error Priority Tests
  , testProperty "error priority severity ordering" prop_error_priority_severity_ordering
  , testProperty "error priority max severity" prop_error_priority_max_severity
  , testProperty "error priority consistency" prop_error_priority_consistency
  , testProperty "error priority uniqueness" prop_error_priority_uniqueness
  , testProperty "error priority range" prop_error_priority_range
  
  -- Error Context Tests
  , testProperty "error context message" prop_error_context_message
  , testProperty "error context preservation" prop_error_context_preservation
  , testProperty "error context ordering" prop_error_context_ordering
  , testProperty "error context empty" prop_error_context_empty
  , testProperty "error context nested" prop_error_context_nested
  
  -- Error Recovery Tests
  , testProperty "error recovery recoverable" prop_error_recovery_recoverable
  , testProperty "error recovery critical" prop_error_recovery_critical
  , testProperty "error recovery strategy" prop_error_recovery_strategy
  , testProperty "error recovery chain" prop_error_recovery_chain
  , testProperty "error recovery priority" prop_error_recovery_priority
  
  -- Error Aggregation Tests
  , testProperty "error aggregation count" prop_error_aggregation_count
  , testProperty "error aggregation severity" prop_error_aggregation_severity
  , testProperty "error aggregation category" prop_error_aggregation_category
  , testProperty "error aggregation empty" prop_error_aggregation_empty
  , testProperty "error aggregation single" prop_error_aggregation_single
  ]