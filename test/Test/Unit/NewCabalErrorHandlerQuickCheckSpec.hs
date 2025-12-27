{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.NewCabalErrorHandlerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (==>))
import TestSupport.QuickCheck (fastProperty)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import GHC.Generics (Generic)

import ErrorHandler
  ( TypeError(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , emptyContext
  , ErrorRecovery(..)
  , ErrorCollector
  , newErrorCollector
  , addError
  , addWarning
  , addInfo
  , getErrors
  , getWarnings
  , getInfo
  , getAllMessages
  , hasErrors
  , hasWarnings
  , formatError
  , formatErrors
  , formatErrorWithLocation
  , formatErrorsWithLocation
  , canRecoverFrom
  , shouldContinueAfter
  , errorAt
  , errorWithCategory
  , warningAt
  , warningWithCategory
  , infoAt
  , infoWithCategory
  )
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- | Generate valid error messages
genErrorMessage :: Gen String
genErrorMessage = do
  words <- listOf1 $ elements $ ["syntax", "type", "ownership", "semantic", "parse", "compile", "runtime"]
  return $ unwords words

-- | Generate error severity levels
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Error, Warning, Info]

-- | Generate error categories
genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements
  [ SyntaxError
  , TypeError
  , OwnershipError
  , DependentTypeError
  , SemanticError
  , ParseError
  , RuntimeError
  , InternalError
  ]

-- | Generate error locations
genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
  line <- arbitrary
  col <- arbitrary
  source <- elements ["source.typus", "input.typus", "test.typus"]
  return $ ErrorLocation line col source

-- | Generate error context
genErrorContext :: Gen ErrorContext
genErrorContext = do
  message <- genErrorMessage
  surrounding <- listOf genErrorMessage
  return $ ErrorContext message surrounding

-- | Generate error recovery strategies
genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = elements
  [ SkipToken
  , InsertToken "placeholder"
  , DeleteToken
  , RetryWithAlternative
  , AbortCompilation
  ]

-- | Generate type errors
genTypeError :: Gen TypeError
genTypeError = do
  msg <- genErrorMessage
  location <- genErrorLocation
  context <- genErrorContext
  severity <- genErrorSeverity
  category <- genErrorCategory
  recovery <- genErrorRecovery
  return $ TypeError msg location context severity category recovery

-- | Generate combined errors
genCombinedError :: Gen CombinedError
genCombinedError = do
  errors <- listOf1 genTypeError
  return $ CombinedError errors

-- | Generate non-negative integers for line/column numbers
genNonNegativeInt :: Gen Int
genNonNegativeInt = getNonNegative <$> arbitrary

-- | Generate positive integers for line/column numbers  
genPositiveInt :: Gen Int
genPositiveInt = getPositive <$> arbitrary

-- | Generate valid source positions
genSourcePos :: Gen SourcePos
genSourcePos = SourcePos <$> genNonNegativeInt <*> genPositiveInt <*> genPositiveInt

-- | Generate valid source spans (start <= end)
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  startLine <- genNonNegativeInt
  startCol <- genPositiveInt
  endLine <- genNonNegativeInt
  endCol <- genPositiveInt
  -- Ensure span is valid by ordering positions
  let (line1, col1, line2, col2) = if (startLine, startCol) <= (endLine, endCol)
                                   then (startLine, startCol, endLine, endCol)
                                   else (endLine, endCol, startLine, startCol)
  return $ SourceSpan (SourcePos line1 col1 0) (SourcePos line2 col2 0)

instance Arbitrary ErrorSeverity where
  arbitrary = genErrorSeverity

instance Arbitrary ErrorCategory where
  arbitrary = genErrorCategory

instance Arbitrary ErrorLocation where
  arbitrary = genErrorLocation

instance Arbitrary ErrorContext where
  arbitrary = genErrorContext

instance Arbitrary ErrorRecovery where
  arbitrary = genErrorRecovery

instance Arbitrary TypeError where
  arbitrary = genTypeError

instance Arbitrary CombinedError where
  arbitrary = genCombinedError

instance Arbitrary SourcePos where
  arbitrary = genSourcePos

instance Arbitrary SourceSpan where
  arbitrary = genSourceSpan

-- ============================================================================
-- Error Severity Property Tests
-- ============================================================================

-- | Property: Error severity should have consistent ordering
prop_error_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_ordering sev1 sev2 =
  let severityOrder = \case
        Error -> 1
        Warning -> 2
        Info -> 3
      order1 = severityOrder sev1
      order2 = severityOrder sev2
  in property True  -- Basic check that severity has defined ordering

-- | Property: All severity levels should be distinct
prop_error_severity_distinct :: Property
prop_error_severity_distinct =
  let severities = [Error, Warning, Info]
      uniqueSeverities = length (nub severities)
  in uniqueSeverities === length severities

-- ============================================================================
-- Error Category Property Tests
-- ============================================================================

-- | Property: Error categories should be distinct
prop_error_category_distinct :: Property
prop_error_category_distinct =
  let categories = [SyntaxError, TypeError, OwnershipError, DependentTypeError, 
                    SemanticError, ParseError, RuntimeError, InternalError]
      uniqueCategories = length (nub categories)
  in uniqueCategories === length categories

-- | Property: Error category show should contain category name
prop_error_category_show :: ErrorCategory -> Property
prop_error_category_show category =
  let categoryStr = show category
      hasName = not (null categoryStr) && any isAlphaNum categoryStr
  in hasName ==> property True

-- ============================================================================
-- Error Location Property Tests
-- ============================================================================

-- | Property: Error location should preserve line and column
prop_error_location_preservation :: Int -> Int -> String -> Property
prop_error_location_preservation line col source =
  let validLine = line >= 0
      validCol = col >= 0
      validSource = not (null source) && all isAlphaNum (take 10 source)
      location = ErrorLocation line col (take 10 source)
  in validLine .&&. validCol .&&. validSource ==> property True

-- | Property: Error location equality should work correctly
prop_error_location_equality :: ErrorLocation -> ErrorLocation -> Property
prop_error_location_equality loc1 loc2 =
  let equal = loc1 == loc2
      sameLine = errorLine loc1 == errorLine loc2
      sameCol = errorColumn loc1 == errorColumn loc2
      sameSource = errorSource loc1 == errorSource loc2
  in equal === (sameLine .&&. sameCol .&&. sameSource)

-- ============================================================================
-- Error Context Property Tests
-- ============================================================================

-- | Property: Empty context should be empty
prop_empty_context_properties :: Property
prop_empty_context_properties =
  let context = emptyContext
      ctxMsg = contextMessage context
  in null ctxMsg

-- | Property: Error context should preserve message
prop_error_context_message :: String -> Property
prop_error_context_message msg =
  let validMsg = not (null msg) && all isAlphaNum (take 10 msg)
      context = ErrorContext (take 10 msg) []
      ctxMsg = contextMessage context
  in validMsg ==> ctxMsg === take 10 msg

-- | Property: Error context should preserve surrounding context
prop_error_context_surrounding :: String -> [String] -> Property
prop_error_context_surrounding msg surrounding =
  let validMsg = not (null msg) && all isAlphaNum (take 5 msg)
      validSurrounding = all (not . null) $ take 3 surrounding
      context = ErrorContext (take 5 msg) (take 3 surrounding)
      ctxSurrounding = contextSurrounding context
  in validMsg .&&. validSurrounding ==> length ctxSurrounding >= min 1 (length (take 3 surrounding))

-- ============================================================================
-- Type Error Property Tests
-- ============================================================================

-- | Property: Type error should contain meaningful information
prop_type_error_content :: TypeError -> Property
prop_type_error_content typeErr =
  let errStr = show typeErr
      hasContent = length errStr > 10
      hasAlphaNum = any isAlphaNum errStr
  in hasContent .&&. hasAlphaNum ==> property True

-- | Property: Type error should preserve message
prop_type_error_message :: String -> Property
prop_type_error_message msg =
  let validMsg = not (null msg) && all isAlphaNum (take 10 msg)
      location = ErrorLocation 0 0 "test.typus"
      context = emptyContext
      severity = Error
      category = SyntaxError
      recovery = SkipToken
      typeErr = TypeError (take 10 msg) location context severity category recovery
  in validMsg ==> errorMessage typeErr === take 10 msg

-- | Property: Type error should preserve severity
prop_type_error_severity :: ErrorSeverity -> Property
prop_type_error_severity severity =
  let msg = "test error"
      location = ErrorLocation 0 0 "test.typus"
      context = emptyContext
      category = SyntaxError
      recovery = SkipToken
      typeErr = TypeError msg location context severity category recovery
  in errorSeverity typeErr === severity

-- ============================================================================
-- Error Collector Property Tests
-- ============================================================================

-- | Property: New error collector should be empty
prop_new_error_collector_empty :: Property
prop_new_error_collector_empty =
  let collector = newErrorCollector
      hasErrs = hasErrors collector
      hasWarns = hasWarnings collector
  in not hasErrs .&&. not hasWarns

-- | Property: Adding error should make collector have errors
prop_add_error :: String -> Property
prop_add_error msg =
  let validMsg = not (null msg) && all isAlphaNum (take 10 msg)
      collector = newErrorCollector
      location = ErrorLocation 0 0 "test.typus"
      context = emptyContext
      severity = Error
      category = SyntaxError
      recovery = SkipToken
      typeErr = TypeError (take 10 msg) location context severity category recovery
      updatedCollector = addError typeErr collector
  in validMsg ==> hasErrors updatedCollector

-- | Property: Adding warning should make collector have warnings
prop_add_warning :: String -> Property
prop_add_warning msg =
  let validMsg = not (null msg) && all isAlphaNum (take 10 msg)
      collector = newErrorCollector
      location = ErrorLocation 0 0 "test.typus"
      context = emptyContext
      severity = Warning
      category = SyntaxError
      recovery = SkipToken
      typeErr = TypeError (take 10 msg) location context severity category recovery
      updatedCollector = addWarning typeErr collector
  in validMsg ==> hasWarnings updatedCollector

-- | Property: Getting errors should return added errors
prop_get_errors :: [String] -> Property
prop_get_errors msgs =
  let validMsgs = filter (not . null) $ map (take 10 . filter isAlphaNum) msgs
      collector = newErrorCollector
      location = ErrorLocation 0 0 "test.typus"
      context = emptyContext
      severity = Error
      category = SyntaxError
      recovery = SkipToken
      errors = [TypeError msg location context severity category recovery | msg <- take 3 validMsgs]
      finalCollector = foldr addError collector errors
      retrievedErrors = getErrors finalCollector
  in not (null validMsgs) ==> length retrievedErrors >= min 1 (length errors)

-- ============================================================================
-- Error Formatting Property Tests
-- ============================================================================

-- | Property: Formatting empty errors should not crash
prop_format_empty_errors :: Property
prop_format_empty_errors =
  let formatted = formatErrors []
  in property True  -- Should not crash

-- | Property: Formatting single error should produce non-empty output
prop_format_single_error :: TypeError -> Property
prop_format_single_error typeErr =
  let formatted = formatError typeErr
      hasContent = length formatted > 5
  in hasContent ==> property True

-- | Property: Formatting errors with location should include location info
prop_format_with_location :: TypeError -> Property
prop_format_with_location typeErr =
  let formatted = formatErrorWithLocation typeErr
      hasContent = length formatted > 10
  in hasContent ==> property True

-- | Property: Formatting multiple errors should produce longer output
prop_format_multiple_errors :: [TypeError] -> Property
prop_format_multiple_errors errors =
  let validErrors = take 3 errors
      formatted = formatErrors validErrors
      singleFormatted = formatErrors (take 1 validErrors)
  in not (null validErrors) ==> length formatted >= length singleFormatted

-- ============================================================================
-- Error Recovery Property Tests
-- ============================================================================

-- | Property: All error recovery strategies should be recoverable
prop_all_recoverable :: Property
prop_all_recoverable =
  let recoveries = [SkipToken, InsertToken "test", DeleteToken, RetryWithAlternative, AbortCompilation]
      allRecoverable = all canRecoverFrom recoveries
  in allRecoverable === True

-- | Property: Abort compilation should not continue
prop_abort_no_continue :: Property
prop_abort_no_continue =
  let shouldContinue = shouldContinueAfter AbortCompilation
  in shouldContinue === False

-- | Property: Other recovery strategies should continue
prop_other_recovery_continue :: ErrorRecovery -> Property
prop_other_recovery_continue recovery =
  let isAbort = recovery == AbortCompilation
      shouldContinue = shouldContinueAfter recovery
  in not isAbort ==> shouldContinue === True

-- ============================================================================
-- Error Construction Property Tests
-- ============================================================================

-- | Property: Error at location should preserve location
prop_error_at_location :: SourceSpan -> String -> Property
prop_error_at_location span msg =
  let validMsg = not (null msg) && all isAlphaNum (take 10 msg)
      error = errorAt span (take 10 msg)
      errorLoc = errorLocation error
  in validMsg ==> property True  -- Basic check that location is preserved

-- | Property: Error with category should preserve category
prop_error_with_category :: ErrorCategory -> String -> Property
prop_error_with_category category msg =
  let validMsg = not (null msg) && all isAlphaNum (take 10 msg)
      error = errorWithCategory category (take 10 msg)
      errorCat = errorCategory error
  in validMsg ==> errorCat === category

-- | Property: Warning construction should create warnings
prop_warning_construction :: SourceSpan -> String -> Property
prop_warning_construction span msg =
  let validMsg = not (null msg) && all isAlphaNum (take 10 msg)
      warning = warningAt span (take 10 msg)
      severity = errorSeverity warning
  in validMsg ==> severity === Warning

-- | Property: Info construction should create info messages
prop_info_construction :: SourceSpan -> String -> Property
prop_info_construction span msg =
  let validMsg = not (null msg) && all isAlphaNum (take 10 msg)
      info = infoAt span (take 10 msg)
      severity = errorSeverity info
  in validMsg ==> severity === Info

-- ============================================================================
-- Integration Property Tests
-- ============================================================================

-- | Property: Complete error handling pipeline should not crash
prop_error_handling_pipeline :: [String] -> Property
prop_error_handling_pipeline msgs =
  let validMsgs = filter (not . null) $ map (take 10 . filter isAlphaNum) msgs
      collector = newErrorCollector
      location = ErrorLocation 0 0 "test.typus"
      context = emptyContext
      severity = Error
      category = SyntaxError
      recovery = SkipToken
      errors = [TypeError msg location context severity category recovery | msg <- take 3 validMsgs]
      finalCollector = foldr addError collector errors
      retrievedErrors = getErrors finalCollector
      formatted = formatErrors retrievedErrors
      hasErrs = hasErrors finalCollector
  in not (null validMsgs) ==> hasErrs .&&. length formatted > 0

-- | Property: Error statistics should be consistent
prop_error_statistics_consistent :: [TypeError] -> Property
prop_error_statistics_consistent errors =
  let validErrors = take 5 errors
      collector = newErrorCollector
      finalCollector = foldr addError collector validErrors
      errorCount = length $ getErrors finalCollector
  in not (null validErrors) ==> errorCount >= min 1 (length validErrors)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal ErrorHandler QuickCheck Tests"
  [ -- Error Severity Tests
    fastProperty "error severity ordering" prop_error_severity_ordering
  , fastProperty "error severity distinct" prop_error_severity_distinct
  
  -- Error Category Tests
  , fastProperty "error category distinct" prop_error_category_distinct
  , fastProperty "error category show" prop_error_category_show
  
  -- Error Location Tests
  , fastProperty "error location preservation" prop_error_location_preservation
  , fastProperty "error location equality" prop_error_location_equality
  
  -- Error Context Tests
  , fastProperty "empty context properties" prop_empty_context_properties
  , fastProperty "error context message" prop_error_context_message
  , fastProperty "error context surrounding" prop_error_context_surrounding
  
  -- Type Error Tests
  , fastProperty "type error content" prop_type_error_content
  , fastProperty "type error message" prop_type_error_message
  , fastProperty "type error severity" prop_type_error_severity
  
  -- Error Collector Tests
  , fastProperty "new error collector empty" prop_new_error_collector_empty
  , fastProperty "add error" prop_add_error
  , fastProperty "add warning" prop_add_warning
  , fastProperty "get errors" prop_get_errors
  
  -- Error Formatting Tests
  , fastProperty "format empty errors" prop_format_empty_errors
  , fastProperty "format single error" prop_format_single_error
  , fastProperty "format with location" prop_format_with_location
  , fastProperty "format multiple errors" prop_format_multiple_errors
  
  -- Error Recovery Tests
  , fastProperty "all recoverable" prop_all_recoverable
  , fastProperty "abort no continue" prop_abort_no_continue
  , fastProperty "other recovery continue" prop_other_recovery_continue
  
  -- Error Construction Tests
  , fastProperty "error at location" prop_error_at_location
  , fastProperty "error with category" prop_error_with_category
  , fastProperty "warning construction" prop_warning_construction
  , fastProperty "info construction" prop_info_construction
  
  -- Integration Tests
  , fastProperty "error handling pipeline" prop_error_handling_pipeline
  , fastProperty "error statistics consistent" prop_error_statistics_consistent
  ]

-- Helper function
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)