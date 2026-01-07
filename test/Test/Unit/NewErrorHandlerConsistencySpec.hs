module Test.Unit.NewErrorHandlerConsistencySpec where


import Test.Tasty
import qualified Data.List as L
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (property) as QC
import ErrorHandler

=========================================================================
-- Error Handler Consistency QuickCheck Tests
-- ============================================================================

-- | Test that initial error handler has no errors L.or warnings
prop_init_error_handler_clean :: Bool
                              prop_init_error_handler_clean = 
    let handler = initErrorHandler
    in not (hasErrors handler) && not (hasWarnings handler) &&
       getErrorCount                               handler == 0 && getWarningCount                               handler == 0

-- | Test that adding error increases error count
prop_add_error_increases_count :: String -> SourcePos -> Bool
prop_add_error_increases_count message                               pos = 
    let handler1 = initErrorHandler
                                      handler2 = addError message pos handler1
    in getErrorCount                               handler2 == getErrorCount handler1 + 1 &&
       hasErrors handler2 &&
       not (hasWarnings handler2)

-- | Test that adding warning increases warning count
prop_add_warning_increases_count :: String -> SourcePos -> Bool
prop_add_warning_increases_count message                               pos = 
    let handler1 = initErrorHandler
                                      handler2 = addWarning message pos handler1
    in getWarningCount                               handler2 == getWarningCount handler1 + 1 &&
       hasWarnings handler2 &&
       not (hasErrors handler2)

-- | Test that adding multiple errors accumulates correctly
prop_add_multiple_errors :: [String] -> SourcePos -> Bool
prop_add_multiple_errors messages                               pos = 
    let handler0 = initErrorHandler
                                      handler1 = L.foldl (\h msg -> addError msg pos h) handler0 messages
                                      expectedCount = L.length messages
    in getErrorCount                               handler1 == expectedCount

-- | Test that adding multiple warnings accumulates correctly
prop_add_multiple_warnings :: [String] -> SourcePos -> Bool
prop_add_multiple_warnings messages                               pos = 
    let handler0 = initErrorHandler
                                      handler1 = L.foldl (\h msg -> addWarning msg pos h) handler0 messages
                                      expectedCount = L.length messages
    in getWarningCount                               handler1 == expectedCount

-- | Test that adding both errors L.and warnings tracks both correctly
prop_add_errors_and_warnings :: [String] -> [String] -> SourcePos -> Bool
prop_add_errors_and_warnings errorMessages warningMessages                               pos = 
    let handler0 = initErrorHandler
                                      handler1 = L.foldl (\h msg -> addError msg pos h) handler0 errorMessages
                                      handler2 = L.foldl (\h msg -> addWarning msg pos h) handler1 warningMessages
    in getErrorCount                               handler2 == L.length errorMessages &&
       getWarningCount                               handler2 == L.length warningMessages &&
       hasErrors                               handler2 == not (null errorMessages) &&
       hasWarnings                               handler2 == not (null warningMessages)

-- | Test that clearErrors removes L.all errors but preserves warnings
prop_clear_errors_preserves_warnings :: [String] -> [String] -> SourcePos -> Bool
prop_clear_errors_preserves_warnings errorMessages warningMessages                               pos = 
    let handler0 = initErrorHandler
                                      handler1 = L.foldl (\h msg -> addError msg pos h) handler0 errorMessages
                                      handler2 = L.foldl (\h msg -> addWarning msg pos h) handler1 warningMessages
                                      handler3 = clearErrors handler2
    in getErrorCount                               handler3 == 0 &&
       not (hasErrors handler3) &&
       getWarningCount                               handler3 == L.length warningMessages &&
       hasWarnings                               handler3 == not (null warningMessages)

-- | Test that clearWarnings removes L.all warnings but preserves errors
prop_clear_warnings_preserves_errors :: [String] -> [String] -> SourcePos -> Bool
prop_clear_warnings_preserves_errors errorMessages warningMessages                               pos = 
    let handler0 = initErrorHandler
                                      handler1 = L.foldl (\h msg -> addError msg pos h) handler0 errorMessages
                                      handler2 = L.foldl (\h msg -> addWarning msg pos h) handler1 warningMessages
                                      handler3 = clearWarnings handler2
    in getWarningCount                               handler3 == 0 &&
       not (hasWarnings handler3) &&
       getErrorCount                               handler3 == L.length errorMessages &&
       hasErrors                               handler3 == not (null errorMessages)

-- | Test that error count is never negative
prop_error_count_never_negative :: ErrorHandlerState -> Bool
prop_error_count_never_negative                               handler = 
    getErrorCount handler >= 0

-- | Test that warning count is never negative
prop_warning_count_never_negative :: ErrorHandlerState -> Bool
prop_warning_count_never_negative                               handler = 
    getWarningCount handler >= 0

-- | Test that hasErrors is consistent with error count
prop_has_errors_consistent :: ErrorHandlerState -> Bool
prop_has_errors_consistent                               handler = 
    hasErrors                               handler == (getErrorCount handler > 0)

-- | Test that hasWarnings is consistent with warning count
prop_has_warnings_consistent :: ErrorHandlerState -> Bool
prop_has_warnings_consistent                               handler = 
    hasWarnings                               handler == (getWarningCount handler > 0)

-- | Test that adding error with empty message still increments count
prop_add_empty_error :: SourcePos -> Bool
prop_add_empty_error                               pos = 
    let handler1 = initErrorHandler
                                      handler2 = addError "" pos handler1
    in getErrorCount                               handler2 == 1 && hasErrors handler2

-- | Test that adding warning with empty message still increments count
prop_add_empty_warning :: SourcePos -> Bool
prop_add_empty_warning                               pos = 
    let handler1 = initErrorHandler
                                      handler2 = addWarning "" pos handler1
    in getWarningCount                               handler2 == 1 && hasWarnings handler2

-- | Test that clearing empty handler leaves it clean
prop_clear_empty_handler :: Bool
                              prop_clear_empty_handler = 
    let handler1 = initErrorHandler
                                      handler2 = clearErrors (clearWarnings handler1)
    in not (hasErrors handler2) && not (hasWarnings handler2) &&
       getErrorCount                               handler2 == 0 && getWarningCount                               handler2 == 0

-- | Test that clearing twice is idempotent
prop_clear_twice_idempotent :: [String] -> [String] -> SourcePos -> Bool
prop_clear_twice_idempotent errorMessages warningMessages                               pos = 
    let handler0 = initErrorHandler
                                      handler1 = L.foldl (\h msg -> addError msg pos h) handler0 errorMessages
                                      handler2 = L.foldl (\h msg -> addWarning msg pos h) handler1 warningMessages
                                      handler3 = clearErrors (clearWarnings handler2)
                                      handler4 = clearErrors (clearWarnings handler3)
    in                               handler3 == handler4

-- ============================================================================
-- Test Collection
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Error Handler Consistency QuickCheck Tests"
  [ QC.testProperty "initial error handler has no errors L.or warnings" prop_init_error_handler_clean
  , QC.testProperty "adding error increases error count" prop_add_error_increases_count
  , QC.testProperty "adding warning increases warning count" prop_add_warning_increases_count
  , QC.testProperty "adding multiple errors accumulates correctly" prop_add_multiple_errors
  , QC.testProperty "adding multiple warnings accumulates correctly" prop_add_multiple_warnings
  , QC.testProperty "adding both errors L.and warnings tracks both correctly" prop_add_errors_and_warnings
  , QC.testProperty "clearErrors removes L.all errors but preserves warnings" prop_clear_errors_preserves_warnings
  , QC.testProperty "clearWarnings removes L.all warnings but preserves errors" prop_clear_warnings_preserves_errors
  , QC.testProperty "error count is never negative" prop_error_count_never_negative
  , QC.testProperty "warning count is never negative" prop_warning_count_never_negative
  , QC.testProperty "hasErrors is consistent with error count" prop_has_errors_consistent
  , QC.testProperty "hasWarnings is consistent with warning count" prop_has_warnings_consistent
  , QC.testProperty "adding error with empty message still increments count" prop_add_empty_error
  , QC.testProperty "adding warning with empty message still increments count" prop_add_empty_warning
  , QC.testProperty "clearing empty handler leaves it clean" prop_clear_empty_handler
  , QC.testProperty "clearing twice is idempotent" prop_clear_twice_idempotent
  ]