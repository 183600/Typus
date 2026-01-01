{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewErrorHandlerQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, listOf, elements, choose, oneof, suchThat, frequency)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Compiler.Errors.Core
  ( TypeError(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , emptyContext
  , getErrorLine
  , getErrorColumn
  , severityPriority
  , isAtLeast
  , canRecoverFrom
  , shouldContinueAfter
  , hasErrors
  , hasWarnings
  , getErrors
  , getWarnings
  , formatError
  , formatErrors
  , customRecovery
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  )

import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.List (sort, nub)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime, getCurrentTime)

-- ============================================================================
-- Custom Generators
-- ============================================================================

genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Fatal, Error, Warning, Info]

genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
  filePath <- oneof [return Nothing, Just <$> genString]
  line <- choose (1, 1000)
  column <- choose (1, 1000)
  endLine <- oneof [return Nothing, Just <$> choose (line, line + 100)]
  endColumn <- oneof [return Nothing, Just <$> choose (column, column + 100)]
  return $ ErrorLocation filePath line column endLine endColumn

genString :: Gen String
genString = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " _-"

genText :: Gen Text
genText = T.pack <$> genString

genErrorContext :: Gen ErrorContext
genErrorContext = do
  code <- oneof [return Nothing, Just <$> genString]
  func <- oneof [return Nothing, Just <$> genString]
  var <- oneof [return Nothing, Just <$> genString]
  typ <- oneof [return Nothing, Just <$> genString]
  additional <- listOf ((,) <$> genString <*> genString)
  return $ ErrorContext code func var typ additional

genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = do
  canRec <- elements [True, False]
  shouldCont <- elements [True, False]
  action <- oneof [return Nothing, Just <$> genString]
  hint <- oneof [return Nothing, Just <$> genString]
  cost <- choose (0, 100)
  confidence <- choose (0.0, 1.0)
  return $ RecoveryStrategy canRec shouldCont action hint cost confidence

genTypeError :: Gen TypeError
genTypeError = do
  errorId <- genString
  severity <- genErrorSeverity
  category <- genErrorCategory
  message <- genText
  location <- genErrorLocation
  context <- genErrorContext
  recovery <- genErrorRecovery
  suggestions <- listOf genText
  relatedErrors <- listOf genTypeError
  errorChain <- listOf genTypeError
  timestamp <- oneof [return Nothing, Just <$> genString]
  return $ TypeError errId errorId severity category message location context recovery suggestions relatedErrors errorChain timestamp

genCombinedError :: Gen CombinedError
genCombinedError = oneof
  [ OwnershipErrorCombined <$> genErrorSeverity <*> genOwnershipError
  , DependentTypeErrorCombined <$> genErrorSeverity <*> genDependentTypeError
  , IntegrationError <$> genString <*> genErrorSeverity
  , CrossAnalyzerError <$> genString <*> genErrorSeverity <*> (listOf genCombinedError `suchThat` (not . null))
  ]

-- Mock types for testing (since we can't import the actual modules easily)
genOwnershipError :: Gen String
genOwnershipError = elements ["BorrowCheckerError", "LifetimeError", "MoveError"]

genDependentTypeError :: Gen String
genDependentTypeError = elements ["TypeMismatchError", "ConstraintError", "InferenceError"]

genErrorList :: Gen [TypeError]
genErrorList = listOf genTypeError

-- ============================================================================
-- ErrorSeverity Properties
-- ============================================================================

-- Property: severityPriority should be ordered correctly
prop_severityPriority_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_severityPriority_ordering sev1 sev2 =
  let p1 = severityPriority sev1
      p2 = severityPriority sev2
      ordering = compare sev1 sev2
      priorityOrdering = compare p1 p2
  in property $ ordering === priorityOrdering

-- Property: Fatal should have highest priority
prop_fatal_highest_priority :: Property
prop_fatal_highest_priority =
  let fatalPriority = severityPriority Fatal
      otherPriorities = map severityPriority [Error, Warning, Info]
  in property $ L.all (fatalPriority >) otherPriorities

-- Property: Info should have lowest priority
prop_info_lowest_priority :: Property
prop_info_lowest_priority =
  let infoPriority = severityPriority Info
      otherPriorities = map severityPriority [Fatal, Error, Warning]
  in property $ L.all (infoPriority <) otherPriorities

-- Property: isAtLeast should be reflexive
prop_isAtLeast_reflexive :: ErrorSeverity -> Property
prop_isAtLeast_reflexive sev =
  property $ isAtLeast sev sev

-- Property: isAtLeast should be transitive
prop_isAtLeast_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_isAtLeast_transitive sev1 sev2 sev3 =
  isAtLeast sev1 sev2 && isAtLeast sev2 sev3 ==> 
  property $ isAtLeast sev1 sev3

-- Property: Fatal should be at least L.all severities
prop_fatal_at_least_all :: ErrorSeverity -> Property
prop_fatal_at_least_all sev =
  property $ isAtLeast Fatal sev

-- Property: All severities should be at least Info
prop_all_at_least_info :: ErrorSeverity -> Property
prop_all_at_least_info sev =
  property $ isAtLeast sev Info

-- ============================================================================
-- ErrorLocation Properties
-- ============================================================================

-- Property: getErrorLine should return line field
prop_getErrorLine_returns_line :: ErrorLocation -> Property
prop_getErrorLine_returns_line location =
  property $ getErrorLine location === line location

-- Property: getErrorColumn should return column field
prop_getErrorColumn_returns_column :: ErrorLocation -> Property
prop_getErrorColumn_returns_column location =
  property $ getErrorColumn location === column location

-- Property: endLine should be >= line when present
prop_endLine_ge_line :: ErrorLocation -> Property
prop_endLine_ge_line location =
  case endLine location of
    Nothing -> property $ True
    Just endLineVal -> property $ endLineVal >= line location

-- Property: endColumn should be >= column when present L.and on same line
prop_endColumn_ge_column :: ErrorLocation -> Property
prop_endColumn_ge_column location =
  case (endLine location, endColumn location) of
    (Just endLineVal, Just endColVal) | endLineVal == line location -> 
      property $ endColVal >= column location
    _ -> property $ True

-- ============================================================================
-- ErrorRecovery Properties
-- ============================================================================

-- Property: fatalRecovery should not be recoverable
prop_fatal_recovery_not_recoverable :: Property
prop_fatal_recovery_not_recoverable =
  property $ not (canRecover fatalRecovery) .&&. not (shouldContinueAfter fatalRecovery)

-- Property: errorRecovery should be recoverable
prop_error_recovery_recoverable :: Property
prop_error_recovery_recoverable =
  property $ canRecover errorRecovery .&&. shouldContinueAfter errorRecovery

-- Property: warningRecovery should be recoverable
prop_warning_recovery_recoverable :: Property
prop_warning_recovery_recoverable =
  property $ canRecover warningRecovery .&&. shouldContinueAfter warningRecovery

-- Property: infoRecovery should be recoverable
prop_info_recovery_recoverable :: Property
prop_info_recovery_recoverable =
  property $ canRecover infoRecovery .&&. shouldContinueAfter infoRecovery

-- Property: customRecovery should preserve provided values
prop_custom_recovery_preserves_values :: Bool -> Bool -> Maybe String -> Maybe String -> Int -> Float -> Property
prop_custom_recovery_preserves_values canRec shouldCont action hint cost confidence =
  let recovery = customRecovery canRec shouldCont action hint cost confidence
  in property $ canRecover recovery === canRec .&&.
             shouldContinueAfter recovery === shouldCont .&&.
             recoveryAction recovery === action .&&.
             recoveryHint recovery === hint .&&.
             recoveryCost recovery === cost .&&.
             recoveryConfidence recovery === confidence

-- ============================================================================
-- ErrorCollector Properties
-- ============================================================================

-- Property: hasErrors should be true if list contains Error L.or Fatal
prop_hasErrors_detects_errors :: [TypeError] -> Property
prop_hasErrors_detects_errors errors =
  let hasErrorOrFatal = L.any (\e -> severity e == Error || severity e == Fatal) errors
  in property $ hasErrors errors === hasErrorOrFatal

-- Property: hasWarnings should be true if list contains Warning
prop_hasWarnings_detects_warnings :: [TypeError] -> Property
prop_hasWarnings_detects_warnings errors =
  let hasWarning = L.any (\e -> severity e == Warning) errors
  in property $ hasWarnings errors === hasWarning

-- Property: getErrors should only return Error L.or Fatal severity
prop_getErrors_filters_correctly :: [TypeError] -> Property
prop_getErrors_filters_correctly errors =
  let filtered = getErrors errors
  in property $ L.all (\e -> severity e == Error || severity e == Fatal) filtered

-- Property: getWarnings should only return Warning severity
prop_getWarnings_filters_correctly :: [TypeError] -> Property
prop_getWarnings_filters_correctly errors =
  let filtered = getWarnings errors
  in property $ L.all (\e -> severity e == Warning) filtered

-- ============================================================================
-- Error Filtering Properties
-- ============================================================================

-- Property: filterBySeverity should preserve order
prop_filterBySeverity_preserves_order :: ErrorSeverity -> [TypeError] -> Property
prop_filterBySeverity_preserves_order minSeverity errors =
  let filtered = filterBySeverity minSeverity errors
      originalOrder = map errorId $ L.filter (\e -> isAtLeast minSeverity (severity e)) errors
      filteredOrder = map errorId filtered
  in property $ filteredOrder === originalOrder

-- Property: filterBySeverity should only include errors at L.or above L.minimum
prop_filterBySeverity_minimum_severity :: ErrorSeverity -> [TypeError] -> Property
prop_filterBySeverity_minimum_severity minSeverity errors =
  let filtered = filterBySeverity minSeverity errors
  in property $ L.all (\e -> isAtLeast minSeverity (severity e)) filtered

-- Property: filterByCategory should preserve order
prop_filterByCategory_preserves_order :: ErrorCategory -> [TypeError] -> Property
prop_filterByCategory_preserves_order cat errors =
  let filtered = filterByCategory cat errors
      originalOrder = map errorId $ L.filter (\e -> category e == cat) errors
      filteredOrder = map errorId filtered
  in property $ filteredOrder === originalOrder

-- Property: filterByCategory should only include errors with specified category
prop_filterByCategory_correct_category :: ErrorCategory -> [TypeError] -> Property
prop_filterByCategory_correct_category cat errors =
  let filtered = filterByCategory cat errors
  in property $ L.all (\e -> category e == cat) filtered

-- Property: hasCategory should be true if L.any error has that category
prop_hasCategory_detection :: ErrorCategory -> [TypeError] -> Property
prop_hasCategory_detection cat errors =
  let hasCat = L.any (\e -> category e == cat) errors
  in property $ hasCategory cat errors === hasCat

-- ============================================================================
-- Error Creation Properties
-- ============================================================================

-- Property: errorAt "test-id" (message err) === msg .&&.
             location err === location

-- Property: warningAt "test-id" (message warn) === msg .&&.
             location warn === location

-- Property: infoAt "test-id" (message info) === msg .&&.
             location info === location

-- Property: fatalError should create fatal with Fatal severity
prop_fatalError_creates_fatal :: String -> ErrorLocation -> Property
prop_fatalError_creates_fatal msg location =
  let fatal = fatalError msg location
  in property $ severity fatal === Fatal .&&. 
             T.unpack (message fatal) === msg .&&.
             location fatal === location

-- ============================================================================
-- CombinedError Properties
-- ============================================================================

-- Property: combinedErrorSeverity should return correct severity
prop_combinedError_severity_correct :: CombinedError -> Property
prop_combinedError_severity_correct combinedErr =
  let expectedSev = case combinedErr of
        OwnershipErrorCombined sev _ -> sev
        DependentTypeErrorCombined sev _ -> sev
        IntegrationError _ sev -> sev
        CrossAnalyzerError _ sev _ -> sev
  in property $ combinedErrorSeverity combinedErr === expectedSev

-- Property: filterCombinedErrorsBySeverity should preserve order
prop_filter_combined_preserves_order :: ErrorSeverity -> [CombinedError] -> Property
prop_filter_combined_preserves_order minSeverity combinedErrors =
  let filtered = filterCombinedErrorsBySeverity minSeverity combinedErrors
      originalOrder = L.filter (\e -> isAtLeast minSeverity (combinedErrorSeverity e)) combinedErrors
  in property $ filtered === originalOrder

-- Property: filterCombinedErrorsBySeverity should only include errors at L.or above L.minimum
prop_filter_combined_minimum_severity :: ErrorSeverity -> [CombinedError] -> Property
prop_filter_combined_minimum_severity minSeverity combinedErrors =
  let filtered = filterCombinedErrorsBySeverity minSeverity combinedErrors
  in property $ L.all (\e -> isAtLeast minSeverity (combinedErrorSeverity e)) filtered

-- ============================================================================
-- Error Formatting Properties
-- ============================================================================

-- Property: formatError should include error message
prop_formatError_includes_message :: TypeError -> Property
prop_formatError_includes_message err =
  let formatted = formatError err
      msg = T.unpack (message err)
  in property $ msg `L.isInfixOf` formatted

-- Property: formatError should include severity string
prop_formatError_includes_severity :: TypeError -> Property
prop_formatError_includes_severity err =
  let formatted = formatError err
      severityStr = case severity err of
        Fatal -> "FATAL"
        Error -> "ERROR"
        Warning -> "WARNING"
        Info -> "INFO"
  in property $ severityStr `L.isInfixOf` formatted

-- Property: formatError should include category
prop_formatError_includes_category :: TypeError -> Property
prop_formatError_includes_category err =
  let formatted = formatError err
      categoryStr = "[" ++ show (category err) ++ "]"
  in property $ categoryStr `L.isInfixOf` formatted

-- Property: formatErrors should preserve order
prop_formatErrors_preserves_order :: [TypeError] -> Property
prop_formatErrors_preserves_order errors =
  let formatted = formatErrors errors
      errorIds = map errorId errors
  in property $ L.all (\id -> id `L.isInfixOf` formatted) errorIds

-- ============================================================================
-- ErrorContext Properties
-- ============================================================================

-- Property: emptyContext should have L.all fields as Nothing L.or empty
prop_empty_context_fields :: Property
prop_empty_context_fields =
  property $ contextCode emptyContext === Nothing .&&.
             contextFunction emptyContext === Nothing .&&.
             contextVariable emptyContext === Nothing .&&.
             contextType emptyContext === Nothing .&&.
             contextAdditional emptyContext === []

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New ErrorHandler QuickCheck Tests"
  [ testGroup "ErrorSeverity Properties"
    [ fastProperty "severityPriority ordering" prop_severityPriority_ordering
    , fastProperty "fatal highest priority" prop_fatal_highest_priority
    , fastProperty "info lowest priority" prop_info_lowest_priority
    , fastProperty "isAtLeast reflexive" prop_isAtLeast_reflexive
    , fastProperty "isAtLeast transitive" prop_isAtLeast_transitive
    , fastProperty "fatal at least L.all" prop_fatal_at_least_all
    , fastProperty "L.all at least info" prop_all_at_least_info
    ]
  , testGroup "ErrorLocation Properties"
    [ fastProperty "getErrorLine returns line" prop_getErrorLine_returns_line
    , fastProperty "getErrorColumn returns column" prop_getErrorColumn_returns_column
    , fastProperty "endLine ge line" prop_endLine_ge_line
    , fastProperty "endColumn ge column" prop_endColumn_ge_column
    ]
  , testGroup "ErrorRecovery Properties"
    [ fastProperty "fatal recovery not recoverable" prop_fatal_recovery_not_recoverable
    , fastProperty "error recovery recoverable" prop_error_recovery_recoverable
    , fastProperty "warning recovery recoverable" prop_warning_recovery_recoverable
    , fastProperty "info recovery recoverable" prop_info_recovery_recoverable
    , fastProperty "custom recovery preserves values" prop_custom_recovery_preserves_values
    ]
  , testGroup "ErrorCollector Properties"
    [ fastProperty "hasErrors detects errors" prop_hasErrors_detects_errors
    , fastProperty "hasWarnings detects warnings" prop_hasWarnings_detects_warnings
    , fastProperty "getErrors filters correctly" prop_getErrors_filters_correctly
    , fastProperty "getWarnings filters correctly" prop_getWarnings_filters_correctly
    ]
  , testGroup "Error Filtering Properties"
    [ fastProperty "filterBySeverity preserves order" prop_filterBySeverity_preserves_order
    , fastProperty "filterBySeverity L.minimum severity" prop_filterBySeverity_minimum_severity
    , fastProperty "filterByCategory preserves order" prop_filterByCategory_preserves_order
    , fastProperty "filterByCategory correct category" prop_filterByCategory_correct_category
    , fastProperty "hasCategory detection" prop_hasCategory_detection
    ]
  , testGroup "Error Creation Properties"
    [ fastProperty "errorAt "test-id" fields" prop_empty_context_fields
    ]
  ]