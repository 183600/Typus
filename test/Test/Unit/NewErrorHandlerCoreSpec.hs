module Test.Unit.NewErrorHandlerCoreSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Property, (===), Arbitrary(..), Gen, choose, listOf, elements, forAll, oneof, suchThat)

import Compiler.Errors.Core 
  ( TypeError(..), ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..), ErrorContext(..)
  , ErrorRecovery(..), CombinedError(..)
  , ErrorCollector, newErrorCollector, addError, addWarning, addInfo
  , getErrors, getWarnings, getInfo, getAllMessages, hasErrors, hasWarnings
  , formatError, formatErrors, errorAt, warningAt, infoAt
  , errorWithCategory, warningWithCategory, infoWithCategory
  , hasCategory, filterByCategory, filterBySeverity, canRecoverFrom
  , shouldContinueAfter, combineErrors, combinedErrorSeverity
  , emptyContext, severityPriority, compareSeverity
  )
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Data.Text as T
import qualified Ownership.Common.Types as Own
import qualified Dependencies.TypeSystem as Dep
import Control.Monad.State (evalState)

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate error severities
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Fatal, Error, Warning, Info]

-- Generate error categories
genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

-- Generate error locations
genErrorLocation :: Gen ErrorLocation
genErrorLocation = oneof
  [ SourceLocation <$> (SourcePos <$> choose (1, 100) <*> choose (1, 100) <*> choose (0, 1000))
  , pure UnknownLocation
  ]

-- Generate error contexts
genErrorContext :: Gen ErrorContext
genErrorContext = do
  depth <- choose (0, 5)
  context <- listOf $ elements ["function", "module", "block", "expression"]
  pure $ ErrorContext context depth

-- Generate error recovery strategies
genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = elements [NoRecovery, SkipToken, InsertToken, Retry, Abort]

-- Generate error messages
genErrorMessage :: Gen T.Text
genErrorMessage = T.pack <$> listOf1 (elements ['a'..'z'] ++ " ")

-- Generate error IDs
genErrorId :: Gen String
genErrorId = do
  num <- choose (1000, 9999)
  pure $ "ERR" ++ show num

-- Generate type errors
genTypeError :: Gen TypeError
genTypeError = TypeError
  <$> genErrorId
  <*> genErrorSeverity
  <*> genErrorCategory
  <*> genErrorMessage
  <*> genErrorLocation
  <*> genErrorContext
  <*> genErrorRecovery
  <*> pure []  -- suggestions
  <*> pure []  -- relatedErrors
  <*> pure []  -- errorChain
  <*> pure Nothing  -- timestamp

-- Generate ownership errors (simplified)
genOwnershipError :: Gen Own.OwnershipError
genOwnershipError = elements [Own.BorrowError, Own.MoveError, Own.LifetimeError]

-- Generate dependent type errors (simplified)
genDependentTypeError :: Gen Dep.DependentTypeError
genDependentTypeError = elements [Dep.ConstraintError, Dep.InferenceError]

-- Generate combined errors
genCombinedError :: Gen CombinedError
genCombinedError = oneof
  [ OwnershipErrorCombined <$> genErrorSeverity <*> genOwnershipError
  , DependentTypeErrorCombined <$> genErrorSeverity <*> genDependentTypeError
  , IntegrationError <$> genErrorId <*> genErrorSeverity
  , CrossAnalyzerError <$> genErrorId <*> genErrorSeverity <*> listOf genCombinedError
  ]

-- ============================================================================
-- Property Tests for ErrorSeverity
-- ============================================================================

-- Property: severity priority should be consistent with severity ordering
prop_severity_priority_ordering :: Property
prop_severity_priority_ordering = 
  forAll genErrorSeverity $ \sev1 ->
    forAll genErrorSeverity $ \sev2 ->
      let p1 = severityPriority sev1
          p2 = severityPriority sev2
      in (sev1 > sev2) === (p1 > p2)

-- Property: compareSeverity should be consistent with severityPriority
prop_compare_severity_consistency :: Property
prop_compare_severity_consistency = 
  forAll genErrorSeverity $ \sev1 ->
    forAll genErrorSeverity $ \sev2 ->
      let ordering = compareSeverity sev1 sev2
          p1 = severityPriority sev1
          p2 = severityPriority sev2
      in (ordering == GT) === (p1 > p2)

-- Property: Fatal should have highest priority
prop_fatal_highest_priority :: Property
prop_fatal_highest_priority = 
  forAll genErrorSeverity $ \sev ->
    let fatalPriority = severityPriority Fatal
        sevPriority = severityPriority sev
    in fatalPriority >= sevPriority

-- ============================================================================
-- Property Tests for ErrorCollector
-- ============================================================================

-- Property: Adding errors should increase error count
prop_add_error_increases_count :: Property
prop_add_error_increases_count = 
  forAll genTypeError $ \err ->
    forAll (listOf genTypeError) $ \existingErrors ->
      let newErrors = err : existingErrors
          oldCount = length (getErrors existingErrors)
          newCount = length (getErrors newErrors)
      in if severity err == Error || severity err == Fatal
         then newCount === oldCount + 1
         else newCount === oldCount

-- Property: Adding warnings should increase warning count
prop_add_warning_increases_count :: Property
prop_add_warning_increases_count = 
  forAll genTypeError $ \err ->
    forAll (listOf genTypeError) $ \existingErrors ->
      let newErrors = err { severity = Warning } : existingErrors
          oldCount = length (getWarnings existingErrors)
          newCount = length (getWarnings newErrors)
      in newCount === oldCount + 1

-- Property: hasErrors should be true if there are error-severity messages
prop_has_errors_detection :: Property
prop_has_errors_detection = 
  forAll (listOf genTypeError) $ \errors ->
    let hasErr = hasErrors errors
        hasErrorSeverity = any (\e -> severity e == Error || severity e == Fatal) errors
    in hasErr === hasErrorSeverity

-- Property: hasWarnings should be true if there are warning-severity messages
prop_has_warnings_detection :: Property
prop_has_warnings_detection = 
  forAll (listOf genTypeError) $ \errors ->
    let hasWarn = hasWarnings errors
        hasWarningSeverity = any (\e -> severity e == Warning) errors
    in hasWarn === hasWarningSeverity

-- ============================================================================
-- Property Tests for Error Filtering
-- ============================================================================

-- Property: filterBySeverity should only return errors with at least the specified severity
prop_filter_by_severity_correctness :: Property
prop_filter_by_severity_correctness = 
  forAll genErrorSeverity $ \minSeverity ->
    forAll (listOf genTypeError) $ \errors ->
      let filtered = filterBySeverity minSeverity errors
      in all (\e -> severity e >= minSeverity) filtered

-- Property: filterByCategory should only return errors with the specified category
prop_filter_by_category_correctness :: Property
prop_filter_by_category_correctness = 
  forAll genErrorCategory $ \category ->
    forAll (listOf genTypeError) $ \errors ->
      let filtered = filterByCategory category errors
      in all (\e -> category e == category) filtered

-- Property: hasCategory should be true if any error has the specified category
prop_has_category_detection :: Property
prop_has_category_detection = 
  forAll genErrorCategory $ \category ->
    forAll (listOf genTypeError) $ \errors ->
      let hasCat = hasCategory category errors
          hasCategoryInList = any (\e -> category e == category) errors
      in hasCat === hasCategoryInList

-- ============================================================================
-- Property Tests for CombinedError
-- ============================================================================

-- Property: combinedErrorSeverity should return the correct severity
prop_combined_error_severity_correctness :: Property
prop_combined_error_severity_correctness = 
  forAll genCombinedError $ \combinedErr ->
    let expectedSeverity = case combinedErr of
          OwnershipErrorCombined sev _ -> sev
          DependentTypeErrorCombined sev _ -> sev
          IntegrationError _ sev -> sev
          CrossAnalyzerError _ sev _ -> sev
        actualSeverity = combinedErrorSeverity combinedErr
    in actualSeverity === expectedSeverity

-- ============================================================================
-- Property Tests for Error Recovery
-- ============================================================================

-- Property: canRecoverFrom should be false for Fatal errors
prop_cannot_recover_from_fatal :: Property
prop_cannot_recover_from_fatal = 
  forAll genTypeError $ \err ->
    let fatalErr = err { severity = Fatal }
    in not (canRecoverFrom fatalErr)

-- Property: shouldContinueAfter should be false for Fatal errors
prop_should_not_continue_after_fatal :: Property
prop_should_not_continue_after_fatal = 
  forAll genTypeError $ \err ->
    let fatalErr = err { severity = Fatal }
    in not (shouldContinueAfter fatalErr)

-- ============================================================================
-- Property Tests for Error Formatting
-- ============================================================================

-- Property: formatError should include the error message
prop_format_error_includes_message :: Property
prop_format_error_includes_message = 
  forAll genTypeError $ \err ->
    let formatted = formatError err
        msgStr = T.unpack (message err)
    in msgStr `elem` words formatted

-- Property: formatError should include the severity string
prop_format_error_includes_severity :: Property
prop_format_error_includes_severity = 
  forAll genTypeError $ \err ->
    let formatted = formatError err
        severityStr = case severity err of
          Fatal -> "FATAL"
          Error -> "ERROR"
          Warning -> "WARNING"
          Info -> "INFO"
    in severityStr `elem` words formatted

-- ============================================================================
-- Unit Tests
-- ============================================================================

test_error_creation_helpers :: IO ()
test_error_creation_helpers = do
  let pos = SourcePos 1 1 0
      span = SourceSpan pos pos
      location = SourceLocation span
      context = emptyContext
      recovery = NoRecovery
      
      err = errorAt location "Test error" TypeChecking recovery context
      warn = warningAt location "Test warning" Parsing recovery context
      info = infoAt location "Test info" Semantic recovery context
  
  severity err @?= Error
  severity warn @?= Warning
  severity info @?= Info
  category err @?= TypeChecking
  category warn @?= Parsing
  category info @?= Semantic

test_error_with_category :: IO ()
test_error_with_category = do
  let pos = SourcePos 1 1 0
      span = SourceSpan pos pos
      location = SourceLocation span
      context = emptyContext
      recovery = NoRecovery
      
      err = errorWithCategory location "Test error" Ownership recovery context
      warn = warningWithCategory location "Test warning" Constraint recovery context
  
  category err @?= Ownership
  category warn @?= Constraint
  severity err @?= Error
  severity warn @?= Warning

test_error_collection :: IO ()
test_error_collection = do
  let pos = SourcePos 1 1 0
      span = SourceSpan pos pos
      location = SourceLocation span
      context = emptyContext
      recovery = NoRecovery
      
      err1 = errorAt location "Error 1" TypeChecking recovery context
      err2 = warningAt location "Warning 1" Parsing recovery context
      err3 = infoAt location "Info 1" Semantic recovery context
      
      -- Simulate error collection
      initialErrors = []
      withErr1 = evalState (addError err1) initialErrors
      withErr2 = evalState (addError err2) withErr1
      withErr3 = evalState (addInfo err3) withErr2
      
      finalErrors = withErr3
  
  length finalErrors @?= 3
  hasErrors finalErrors @?= True
  hasWarnings finalErrors @?= True
  length (getErrors finalErrors) @?= 1
  length (getWarnings finalErrors) @?= 1
  length (getInfo finalErrors) @?= 1

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Error Handler Core Tests"
  [ -- ErrorSeverity properties
    testProperty "Severity priority ordering" prop_severity_priority_ordering
  , testProperty "Compare severity consistency" prop_compare_severity_consistency
  , testProperty "Fatal has highest priority" prop_fatal_highest_priority
  
  -- ErrorCollector properties
  , testProperty "Add error increases count" prop_add_error_increases_count
  , testProperty "Add warning increases count" prop_add_warning_increases_count
  , testProperty "Has errors detection" prop_has_errors_detection
  , testProperty "Has warnings detection" prop_has_warnings_detection
  
  -- Error filtering properties
  , testProperty "Filter by severity correctness" prop_filter_by_severity_correctness
  , testProperty "Filter by category correctness" prop_filter_by_category_correctness
  , testProperty "Has category detection" prop_has_category_detection
  
  -- CombinedError properties
  , testProperty "Combined error severity correctness" prop_combined_error_severity_correctness
  
  -- Error recovery properties
  , testProperty "Cannot recover from fatal" prop_cannot_recover_from_fatal
  , testProperty "Should not continue after fatal" prop_should_not_continue_after_fatal
  
  -- Error formatting properties
  , testProperty "Format error includes message" prop_format_error_includes_message
  , testProperty "Format error includes severity" prop_format_error_includes_severity
  
  -- Unit tests
  , testCase "Error creation helpers" test_error_creation_helpers
  , testCase "Error with category" test_error_with_category
  , testCase "Error collection" test_error_collection
  ]