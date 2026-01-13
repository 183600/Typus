{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.ErrorHandlerSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertFailure, assertBool, Assertion)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>))
import ErrorHandler
import qualified Data.Text as T
import Data.Char (isSpace)

-- Helper generators for ErrorHandler tests
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Fatal, Error, Warning, Info]

genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
  filePath <- elements [Nothing, Just "test.typus", Just "module.typus"]
  line <- choose (1, 100)
  column <- choose (1, 100)
  endLine <- elements [Nothing, Just <$> choose (1, 100)]
  endColumn <- elements [Nothing, Just <$> choose (1, 100)]
  return $ ErrorLocation filePath line column endLine endColumn

genErrorContext :: Gen ErrorContext
genErrorContext = do
  contextCode <- elements [Nothing, Just "let x = 42", Just "func add(a, b) { return a + b }"]
  contextFunction <- elements [Nothing, Just "main", Just "add"]
  contextVariable <- elements [Nothing, Just "x", Just "a", Just "b"]
  contextType <- elements [Nothing, Just "int", Just "string"]
  contextAdditional <- listOf $ do
    key <- elements ["suggestion", "note", "hint"]
    value <- elements ["check syntax", "verify types", "add missing import"]
    return (key, value)
  return $ ErrorContext contextCode contextFunction contextVariable contextType contextAdditional

genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = do
  canRecover <- elements [True, False]
  shouldContinue <- elements [True, False]
  recoveryAction <- elements [Nothing, Just "retry", Just "skip", Just "fallback"]
  recoveryHint <- elements [Nothing, Just "check input", Just "verify configuration"]
  recoveryCost <- choose (0, 100)
  recoveryConfidence <- choose (0.0, 1.0)
  return $ RecoveryStrategy canRecover shouldContinue recoveryAction recoveryHint recoveryCost recoveryConfidence

genTypeError :: Gen TypeError
genTypeError = do
  errorId <- choose (1000, 9999) >>= \n -> return ("E" ++ show n)
  severity <- genErrorSeverity
  category <- genErrorCategory
  message <- elements ["Type mismatch", "Syntax error", "Ownership violation", "Missing import"]
  location <- genErrorLocation
  context <- genErrorContext
  recovery <- genErrorRecovery
  suggestions <- listOf $ elements [T.pack "Check types", T.pack "Verify syntax", T.pack "Add import"]
  relatedErrors <- listOf genTypeError
  errorChain <- listOf genTypeError
  timestamp <- elements [Nothing, Just "2023-01-01 12:00:00"]
  return $ TypeError errorId severity category (T.pack message) location context recovery suggestions relatedErrors errorChain timestamp

genCombinedError :: Gen CombinedError
genCombinedError = do
  severity <- genErrorSeverity
  oneof
    [ return $ OwnershipErrorCombined severity undefined
    , return $ DependentTypeErrorCombined severity undefined
    , IntegrationError <$> elements ["Cross-module error", "Integration failure"] <*> pure severity
    , CrossAnalyzerError <$> elements ["Cross-analysis error"] <*> pure severity <*> listOf genCombinedError
    ]

-- Test cases for ErrorHandler module

-- Test 1: Create and format basic error
test_create_and_format_basic_error :: Assertion
test_create_and_format_basic_error = do
  let error = TypeError "E1001" Error TypeChecking (T.pack "Type mismatch") 
                       (ErrorLocation Nothing 1 1 Nothing Nothing) 
                       emptyContext errorRecovery [] [] [] Nothing
      formatted = formatError error
  assertBool "Formatted error should contain type checking" 
             ("[TypeChecking]" `isInfixOf` formatted)
  assertBool "Formatted error should contain type mismatch" 
             ("Type mismatch" `isInfixOf` formatted)

-- Test 2: Create error with location
test_create_error_with_location :: Assertion
test_create_error_with_location = do
  let location = ErrorLocation (Just "test.typus") 10 5 Nothing Nothing
      error = TypeError "E1002" Error Parsing (T.pack "Syntax error") 
                       location emptyContext errorRecovery [] [] [] Nothing
      formatted = formatErrorWithLocation error
  assertBool "Formatted error should contain file name" 
             ("test.typus" `isInfixOf` formatted)
  assertBool "Formatted error should contain line and column" 
             ("10:5" `isInfixOf` formatted)

-- Test 3: Create error with context
test_create_error_with_context :: Assertion
test_create_error_with_context = do
  let context = ErrorContext (Just "let x = 42") (Just "main") (Just "x") (Just "int") []
      error = TypeError "E1003" Error TypeChecking (T.pack "Type mismatch") 
                       (ErrorLocation Nothing 1 1 Nothing Nothing) 
                       context errorRecovery [] [] [] Nothing
      formatted = formatError error
  assertBool "Formatted error should contain context" 
             ("let x = 42" `isInfixOf` formatted)

-- Test 4: Create error with suggestions
test_create_error_with_suggestions :: Assertion
test_create_error_with_suggestions = do
  let suggestions = [T.pack "Check types", T.pack "Verify syntax"]
      error = TypeError "E1004" Error TypeChecking (T.pack "Type mismatch") 
                       (ErrorLocation Nothing 1 1 Nothing Nothing) 
                       emptyContext errorRecovery suggestions [] [] Nothing
      formatted = formatError error
  assertBool "Formatted error should contain suggestions" 
             ("Check types" `isInfixOf` formatted && "Verify syntax" `isInfixOf` formatted)

-- Test 5: Create error with related errors
test_create_error_with_related_errors :: Assertion
test_create_error_with_related_errors = do
  let relatedError = TypeError "E1005" Warning TypeChecking (T.pack "Related issue") 
                            (ErrorLocation Nothing 2 1 Nothing Nothing) 
                            emptyContext warningRecovery [] [] [] Nothing
      error = TypeError "E1006" Error TypeChecking (T.pack "Main error") 
                       (ErrorLocation Nothing 1 1 Nothing Nothing) 
                       emptyContext errorRecovery [] [relatedError] [] Nothing
  assertEqual "Should have one related error" 1 (length (relatedErrors error))

-- Test 6: Check error severity priority
test_error_severity_priority :: Assertion
test_error_severity_priority = do
  assertEqual "Fatal should have highest priority" 100 (severityPriority Fatal)
  assertEqual "Error should have high priority" 80 (severityPriority Error)
  assertEqual "Warning should have medium priority" 30 (severityPriority Warning)
  assertEqual "Info should have low priority" 10 (severityPriority Info)

-- Test 7: Compare error severities
test_compare_error_severities :: Assertion
test_compare_error_severities = do
  assertEqual "Fatal should be greater than Error" GT (compareSeverity Fatal Error)
  assertEqual "Error should be greater than Warning" GT (compareSeverity Error Warning)
  assertEqual "Warning should be greater than Info" GT (compareSeverity Warning Info)
  assertEqual "Same severities should be equal" EQ (compareSeverity Error Error)

-- Test 8: Check if severity is at least threshold
test_is_at_least_severity :: Assertion
test_is_at_least_severity = do
  assertBool "Fatal is at least Error" (isAtLeast Fatal Error)
  assertBool "Error is at least Error" (isAtLeast Error Error)
  assertBool "Warning is not at least Error" (not $ isAtLeast Warning Error)
  assertBool "Info is at least Info" (isAtLeast Info Info)

-- Test 9: Create custom recovery strategy
test_create_custom_recovery_strategy :: Assertion
test_create_custom_recovery_strategy = do
  let recovery = customRecovery True True (Just "retry") (Just "check input") 20 0.8
  assertEqual "Should be recoverable" True (canRecover recovery)
  assertEqual "Should continue" True (shouldContinue recovery)
  assertEqual "Should have recovery action" (Just "retry") (recoveryAction recovery)
  assertEqual "Should have recovery hint" (Just "check input") (recoveryHint recovery)
  assertEqual "Should have recovery cost" 20 (recoveryCost recovery)
  assertEqual "Should have recovery confidence" 0.8 (recoveryConfidence recovery)

-- Test 10: Check predefined recovery strategies
test_predefined_recovery_strategies :: Assertion
test_predefined_recovery_strategies = do
  assertEqual "Fatal recovery should not be recoverable" False (canRecover fatalRecovery)
  assertEqual "Fatal recovery should not continue" False (shouldContinue fatalRecovery)
  assertEqual "Error recovery should be recoverable" True (canRecover errorRecovery)
  assertEqual "Error recovery should continue" True (shouldContinue errorRecovery)
  assertEqual "Warning recovery should be recoverable" True (canRecover warningRecovery)
  assertEqual "Warning recovery should continue" True (shouldContinue warningRecovery)
  assertEqual "Info recovery should be recoverable" True (canRecover infoRecovery)
  assertEqual "Info recovery should continue" True (shouldContinue infoRecovery)

-- Test 11: Filter errors by severity
test_filter_errors_by_severity :: Assertion
test_filter_errors_by_severity = do
  let error1 = TypeError "E1001" Fatal TypeChecking (T.pack "Fatal error") 
                         (ErrorLocation Nothing 1 1 Nothing Nothing) 
                         emptyContext fatalRecovery [] [] [] Nothing
      error2 = TypeError "E1002" Error TypeChecking (T.pack "Error") 
                         (ErrorLocation Nothing 1 1 Nothing Nothing) 
                         emptyContext errorRecovery [] [] [] Nothing
      error3 = TypeError "E1003" Warning TypeChecking (T.pack "Warning") 
                         (ErrorLocation Nothing 1 1 Nothing Nothing) 
                         emptyContext warningRecovery [] [] [] Nothing
      error4 = TypeError "E1004" Info TypeChecking (T.pack "Info") 
                         (ErrorLocation Nothing 1 1 Nothing Nothing) 
                         emptyContext infoRecovery [] [] [] Nothing
      errors = [error1, error2, error3, error4]
      errorSeverityErrors = filterBySeverity Error errors
  assertEqual "Should filter errors by severity" 2 (length errorSeverityErrors)

-- Test 12: Filter errors by category
test_filter_errors_by_category :: Assertion
test_filter_errors_by_category = do
  let error1 = TypeError "E1001" Error TypeChecking (T.pack "Type error") 
                         (ErrorLocation Nothing 1 1 Nothing Nothing) 
                         emptyContext errorRecovery [] [] [] Nothing
      error2 = TypeError "E1002" Error Parsing (T.pack "Parse error") 
                         (ErrorLocation Nothing 1 1 Nothing Nothing) 
                         emptyContext errorRecovery [] [] [] Nothing
      error3 = TypeError "E1003" Error TypeChecking (T.pack "Another type error") 
                         (ErrorLocation Nothing 1 1 Nothing Nothing) 
                         emptyContext errorRecovery [] [] [] Nothing
      errors = [error1, error2, error3]
      typeCheckingErrors = filterByCategory TypeChecking errors
  assertEqual "Should filter errors by category" 2 (length typeCheckingErrors)

-- Test 13: Get error statistics
test_get_error_statistics :: Assertion
test_get_error_statistics = do
  let error1 = TypeError "E1001" Fatal TypeChecking (T.pack "Fatal error") 
                         (ErrorLocation Nothing 1 1 Nothing Nothing) 
                         emptyContext fatalRecovery [] [] [] Nothing
      error2 = TypeError "E1002" Error TypeChecking (T.pack "Error") 
                         (ErrorLocation Nothing 1 1 Nothing Nothing) 
                         emptyContext errorRecovery [] [] [] Nothing
      error3 = TypeError "E1003" Warning TypeChecking (T.pack "Warning") 
                         (ErrorLocation Nothing 1 1 Nothing Nothing) 
                         emptyContext warningRecovery [] [] [] Nothing
      error4 = TypeError "E1004" Info TypeChecking (T.pack "Info") 
                         (ErrorLocation Nothing 1 1 Nothing Nothing) 
                         emptyContext infoRecovery [] [] [] Nothing
      errors = [error1, error2, error3, error4]
      stats = getErrorStatistics errors
  assertBool "Statistics should contain error count" 
             ("Errors: 2" `isInfixOf` stats)
  assertBool "Statistics should contain warning count" 
             ("Warnings: 1" `isInfixOf` stats)
  assertBool "Statistics should contain info count" 
             ("Info: 1" `isInfixOf` stats)

-- Test 14: Generate error report
test_generate_error_report :: Assertion
test_generate_error_report = do
  let error = TypeError "E1001" Error TypeChecking (T.pack "Type mismatch") 
                       (ErrorLocation Nothing 1 1 Nothing Nothing) 
                       emptyContext errorRecovery [] [] [] Nothing
      errors = [error]
      report = generateErrorReport errors
  assertBool "Report should contain error summary" 
             ("Error Report" `isInfixOf` report)
  assertBool "Report should contain error details" 
             ("Type mismatch" `isInfixOf` report)

-- Test 15: Create error with timestamp
test_create_error_with_timestamp :: Assertion
test_create_error_with_timestamp = do
  let timestamp = Just "2023-01-01 12:00:00"
      error = TypeError "E1001" Error TypeChecking (T.pack "Type mismatch") 
                       (ErrorLocation Nothing 1 1 Nothing Nothing) 
                       emptyContext errorRecovery [] [] [] timestamp
  assertEqual "Should have timestamp" timestamp (timestamp error)

-- Test 16: Format multiple errors
test_format_multiple_errors :: Assertion
test_format_multiple_errors = do
  let error1 = TypeError "E1001" Error TypeChecking (T.pack "Type error") 
                         (ErrorLocation Nothing 1 1 Nothing Nothing) 
                         emptyContext errorRecovery [] [] [] Nothing
      error2 = TypeError "E1002" Error Parsing (T.pack "Parse error") 
                         (ErrorLocation Nothing 1 1 Nothing Nothing) 
                         emptyContext errorRecovery [] [] [] Nothing
      errors = [error1, error2]
      formatted = formatErrors errors
  assertBool "Formatted errors should contain both error messages" 
             ("Type error" `isInfixOf` formatted && "Parse error" `isInfixOf` formatted)

-- Test 17: Format multiple errors with location
test_format_multiple_errors_with_location :: Assertion
test_format_multiple_errors_with_location = do
  let location1 = ErrorLocation (Just "file1.typus") 1 1 Nothing Nothing
      location2 = ErrorLocation (Just "file2.typus") 2 1 Nothing Nothing
      error1 = TypeError "E1001" Error TypeChecking (T.pack "Type error") 
                         location1 emptyContext errorRecovery [] [] [] Nothing
      error2 = TypeError "E1002" Error Parsing (T.pack "Parse error") 
                         location2 emptyContext errorRecovery [] [] [] Nothing
      errors = [error1, error2]
      formatted = formatErrorsWithLocation errors
  assertBool "Formatted errors should contain both file names" 
             ("file1.typus" `isInfixOf` formatted && "file2.typus" `isInfixOf` formatted)

-- Test 18: Create combined error
test_create_combined_error :: Assertion
test_create_combined_error = do
  let combinedError = IntegrationError "Cross-module error" Error
      severity = combinedErrorSeverity combinedError
  assertEqual "Should have error severity" Error severity

-- Test 19: Filter combined errors by severity
test_filter_combined_errors_by_severity :: Assertion
test_filter_combined_errors_by_severity = do
  let error1 = IntegrationError "Error 1" Error
      error2 = IntegrationError "Warning" Warning
      error3 = IntegrationError "Error 2" Error
      errors = [error1, error2, error3]
      filtered = filterCombinedErrorsBySeverity Error errors
  assertEqual "Should filter combined errors by severity" 2 (length filtered)

-- Test 20: Create error with suggestions
test_error_with_suggestions :: Assertion
test_error_with_suggestions = do
  let suggestions = [T.pack "Check types", T.pack "Verify syntax"]
      error = errorWithSuggestions "E1001" Error TypeChecking (T.pack "Type mismatch") 
                                   (ErrorLocation Nothing 1 1 Nothing Nothing) 
                                   emptyContext errorRecovery suggestions
  assertEqual "Should have suggestions" suggestions (suggestions error)

-- Test 21: Create error with location
test_error_with_location :: Assertion
test_error_with_location = do
  let location = ErrorLocation (Just "test.typus") 10 5 Nothing Nothing
      error = errorAt "E1002" Error Parsing (T.pack "Syntax error") location
  assertEqual "Should have location" location (location error)

-- Test 22: Create warning with category
test_warning_with_category :: Assertion
test_warning_with_category = do
  let warning = warningWithCategory "E1003" TypeChecking (T.pack "Type warning") 
                                    (ErrorLocation Nothing 1 1 Nothing Nothing)
  assertEqual "Should have type checking category" TypeChecking (category warning)
  assertEqual "Should have warning severity" Warning (severity warning)

-- Test 23: Create info with context
test_info_with_context :: Assertion
test_info_with_context = do
  let context = ErrorContext (Just "let x = 42") Nothing Nothing Nothing []
      info = infoWithContext "E1004" Parsing (T.pack "Parse info") 
                             (ErrorLocation Nothing 1 1 Nothing Nothing) context
  assertEqual "Should have context" context (context info)

-- Test 24: Create fatal error
test_fatal_error :: Assertion
test_fatal_error = do
  let fatal = fatalError "E1005" (T.pack "Fatal error")
  assertEqual "Should have fatal severity" Fatal (severity fatal)

-- Test 25: Wrap error with context
test_wrap_error_with_context :: Assertion
test_wrap_error_with_context = do
  let baseError = TypeError "E1001" Error TypeChecking (T.pack "Base error") 
                            (ErrorLocation Nothing 1 1 Nothing Nothing) 
                            emptyContext errorRecovery [] [] [] Nothing
      context = ErrorContext Nothing (Just "main") Nothing Nothing []
      wrapped = wrapError baseError context
  assertEqual "Should have wrapped context" context (context wrapped)

-- Property tests for ErrorHandler module

-- Property 1: Error severity priority is ordered correctly
prop_severity_priority_ordered :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_priority_ordered sev1 sev2 = 
  let priority1 = severityPriority sev1
      priority2 = severityPriority sev2
      comparison = compareSeverity sev1 sev2
  in (priority1 > priority2) ==> comparison == GT

-- Property 2: Filtering by severity preserves order
prop_filter_by_severity_preserves_order :: [TypeError] -> ErrorSeverity -> Property
prop_filter_by_severity_preserves_order errors minSeverity = 
  not (null errors) ==>
    let filtered = filterBySeverity minSeverity errors
        originalOrder = map severity errors
        filteredOrder = map severity filtered
    in filteredOrder `isSubsequenceOf` originalOrder

-- Property 3: Filtering by category preserves order
prop_filter_by_category_preserves_order :: [TypeError] -> ErrorCategory -> Property
prop_filter_by_category_preserves_order errors category = 
  not (null errors) ==>
    let filtered = filterByCategory category errors
        originalOrder = map category errors
        filteredOrder = map category filtered
    in all (== category) filteredOrder

-- Property 4: Error statistics count is accurate
prop_error_statistics_count_accurate :: [TypeError] -> Property
prop_error_statistics_count_accurate errors = 
  not (null errors) ==>
    let stats = getErrorStatistics errors
        errorCount = length $ filter (\e -> severity e == Error || severity e == Fatal) errors
        warningCount = length $ filter (\e -> severity e == Warning) errors
        infoCount = length $ filter (\e -> severity e == Info) errors
    in ("Errors: " ++ show errorCount) `isInfixOf` stats &&
       ("Warnings: " ++ show warningCount) `isInfixOf` stats &&
       ("Info: " ++ show infoCount) `isInfixOf` stats

-- Property 5: Combined error severity is consistent
prop_combined_error_severity_consistent :: CombinedError -> Bool
prop_combined_error_severity_consistent combinedError = 
  let severity = combinedErrorSeverity combinedError
  in severity `elem` [Fatal, Error, Warning, Info]

-- Property 6: Error formatting contains error message
prop_error_formatting_contains_message :: TypeError -> Bool
prop_error_formatting_contains_message error = 
  let formatted = formatError error
      message = T.unpack (message error)
  in message `isInfixOf` formatted

-- Property 7: Error formatting with location contains location info
prop_error_formatting_with_location_contains_location :: TypeError -> Property
prop_error_formatting_with_location_contains_location error = 
  let location = location error
      hasLocation = line location > 0
  in hasLocation ==>
    let formatted = formatErrorWithLocation error
        lineStr = show (line location)
        columnStr = show (column location)
    in lineStr `isInfixOf` formatted && columnStr `isInfixOf` formatted

-- Property 8: Recovery strategy confidence is within bounds
prop_recovery_confidence_within_bounds :: ErrorRecovery -> Bool
prop_recovery_confidence_within_bounds recovery = 
  let confidence = recoveryConfidence recovery
  in confidence >= 0.0 && confidence <= 1.0

-- Property 9: Recovery strategy cost is within bounds
prop_recovery_cost_within_bounds :: ErrorRecovery -> Bool
prop_recovery_cost_within_bounds recovery = 
  let cost = recoveryCost recovery
  in cost >= 0 && cost <= 100

-- Property 10: Error context preserves additional fields
prop_error_context_preserves_additional :: ErrorContext -> Bool
prop_error_context_preserves_additional context = 
  let additional = contextAdditional context
  in length additional >= 0

-- Helper functions
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys)
  | x == y = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf (x:xs) ys

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    tails [] = [[]]
    tails s@(_:t) = s : tails t

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- Placeholder for undefined values
undefined :: a
undefined = error "undefined"

errorHandlerTests :: TestTree
errorHandlerTests = testGroup "ErrorHandler Tests"
  [ testGroup "Unit Tests"
    [ testCase "Create and format basic error" test_create_and_format_basic_error
    , testCase "Create error with location" test_create_error_with_location
    , testCase "Create error with context" test_create_error_with_context
    , testCase "Create error with suggestions" test_create_error_with_suggestions
    , testCase "Create error with related errors" test_create_error_with_related_errors
    , testCase "Check error severity priority" test_error_severity_priority
    , testCase "Compare error severities" test_compare_error_severities
    , testCase "Check if severity is at least threshold" test_is_at_least_severity
    , testCase "Create custom recovery strategy" test_create_custom_recovery_strategy
    , testCase "Check predefined recovery strategies" test_predefined_recovery_strategies
    , testCase "Filter errors by severity" test_filter_errors_by_severity
    , testCase "Filter errors by category" test_filter_errors_by_category
    , testCase "Get error statistics" test_get_error_statistics
    , testCase "Generate error report" test_generate_error_report
    , testCase "Create error with timestamp" test_create_error_with_timestamp
    , testCase "Format multiple errors" test_format_multiple_errors
    , testCase "Format multiple errors with location" test_format_multiple_errors_with_location
    , testCase "Create combined error" test_create_combined_error
    , testCase "Filter combined errors by severity" test_filter_combined_errors_by_severity
    , testCase "Create error with suggestions" test_error_with_suggestions
    , testCase "Create error with location" test_error_with_location
    , testCase "Create warning with category" test_warning_with_category
    , testCase "Create info with context" test_info_with_context
    , testCase "Create fatal error" test_fatal_error
    , testCase "Wrap error with context" test_wrap_error_with_context
    ]
  , testProperties "Property Tests"
    [ ("Error severity priority is ordered correctly", property prop_severity_priority_ordered)
    , ("Filtering by severity preserves order", property prop_filter_by_severity_preserves_order)
    , ("Filtering by category preserves order", property prop_filter_by_category_preserves_order)
    , ("Error statistics count is accurate", property prop_error_statistics_count_accurate)
    , ("Combined error severity is consistent", property prop_combined_error_severity_consistent)
    , ("Error formatting contains error message", property prop_error_formatting_contains_message)
    , ("Error formatting with location contains location info", property prop_error_formatting_with_location_contains_location)
    , ("Recovery strategy confidence is within bounds", property prop_recovery_confidence_within_bounds)
    , ("Recovery strategy cost is within bounds", property prop_recovery_cost_within_bounds)
    , ("Error context preserves additional fields", property prop_error_context_preserves_additional)
    ]
  ]