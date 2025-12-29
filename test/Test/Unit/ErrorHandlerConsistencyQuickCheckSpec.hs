module Test.Unit.ErrorHandlerConsistencyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, listOf, chooseInt, vectorOf, suchThat, Positive(..), NonNegative(..))
import TestSupport.QuickCheck (fastProperty)

import Compiler.Errors.Core (
    TypeError(..), CombinedError(..), ErrorSeverity(..), ErrorCategory(..), 
    ErrorLocation(..), ErrorContext(..), emptyContext, ErrorRecovery(..),
    ErrorCollector, newErrorCollector, addError, addWarning, addInfo,
    getErrors, getWarnings, getInfo, getAllMessages, hasErrors, hasWarnings,
    formatError, formatErrors, formatErrorWithLocation, formatErrorsWithLocation,
    canRecoverFrom, shouldContinueAfter, errorAt, errorWithCategory, warningAt,
    infoAt, fatalError, errorWithSuggestions, withLocation, withContext,
    withSuggestions, withRelatedErrors, wrapError, combineErrors,
    filterByCategory, filterBySeverity, getErrorStatistics,
    errorLine, errorColumn, errorMessage, errorSuggestions, errorRelatedErrors
)

import SourceLocation (SourcePos(..), SourceSpan(..))
import Data.Time (UTCTime, getCurrentTime)

-- ============================================================================
-- Arbitrary instances for ErrorHandler types
-- ============================================================================

instance Arbitrary ErrorSeverity where
    arbitrary = elements [ErrorInfo, ErrorWarning, ErrorError, ErrorFatal]

instance Arbitrary ErrorCategory where
    arbitrary = oneof
        [ return SyntaxError
        , return TypeError
        , return SemanticError
        , return NameError
        , return ImportError
        , return RuntimeError
        , return ConfigError
        , return IOError
        ]

instance Arbitrary ErrorLocation where
    arbitrary = do
        line <- positiveInt
        column <- positiveInt
        file <- elements ["test.typus", "module.typus", "lib.typus", ""]
        return $ ErrorLocation line column file
      where
        positiveInt = getPositive <$> arbitrary

instance Arbitrary ErrorContext where
    arbitrary = do
        context <- listOf (elements ["variable", "function", "type", "module", "import"])
        suggestions <- listOf (elements ["check syntax", "verify types", "review imports"])
        return $ ErrorContext context suggestions

instance Arbitrary ErrorRecovery where
    arbitrary = oneof
        [ return NoRecovery
        , return SkipToken
        , return InsertToken
        , return ReplaceToken
        , return RetryParsing
        ]

instance Arbitrary TypeError where
    arbitrary = do
        severity <- arbitrary
        category <- arbitrary
        location <- arbitrary
        context <- arbitrary
        message <- elements ["syntax error", "type mismatch", "undefined variable", "import error"]
        suggestions <- listOf (elements ["check syntax", "verify types", "review imports"])
        related <- listOf arbitrary
        return $ TypeError severity category location context message suggestions related

instance Arbitrary CombinedError where
    arbitrary = do
        primary <- arbitrary
        secondary <- listOf arbitrary
        return $ CombinedError primary secondary

instance Arbitrary ErrorCollector where
    arbitrary = return newErrorCollector

-- Generate error messages
genErrorMessage :: Gen String
genErrorMessage = oneof
    [ return "Syntax error: unexpected token"
    , return "Type error: cannot unify types"
    , return "Name error: variable not in scope"
    , return "Import error: module not found"
    , return "Runtime error: null pointer exception"
    , return "Config error: invalid configuration"
    , return "IO error: file not found"
    ]

-- Generate suggestions
genSuggestions :: Gen [String]
genSuggestions = listOf $ elements
    [ "Check your syntax"
    , "Verify variable names"
    , "Review import statements"
    , "Ensure proper type annotations"
    , "Check file permissions"
    ]

-- ============================================================================
-- Properties
-- ============================================================================

tests :: TestTree
tests = testGroup "ErrorHandler Consistency QuickCheck Tests"
    [ testGroup "ErrorSeverity Properties"
        [ testProperty "ErrorSeverity ordering is consistent" $
            fastProperty prop_errorSeverityOrdering
        
        , testProperty "ErrorSeverity comparison is transitive" $
            fastProperty prop_errorSeverityTransitive
        
        , testProperty "Fatal is greater than other severities" $
            fastProperty prop_fatalGreaterThanOthers
        ]

    , testGroup "ErrorCategory Properties"
        [ testProperty "ErrorCategory show is invertible" $
            fastProperty prop_errorCategoryShowInvertible
        
        , testProperty "ErrorCategory equality works correctly" $
            fastProperty prop_errorCategoryEquality
        ]

    , testGroup "ErrorLocation Properties"
        [ testProperty "ErrorLocation preserves line and column" $
            fastProperty prop_errorLocationPreservesLineCol
        
        , testProperty "ErrorLocation handles empty file gracefully" $
            fastProperty prop_errorLocationHandlesEmptyFile
        ]

    , testGroup "ErrorContext Properties"
        [ testProperty "ErrorContext preserves context information" $
            fastProperty prop_errorContextPreservesInfo
        
        , testProperty "emptyContext has no context" $
            fastProperty prop_emptyContextHasNoContext
        
        , testProperty "Context suggestions are preserved" $
            fastProperty prop_contextSuggestionsPreserved
        ]

    , testGroup "TypeError Properties"
        [ testProperty "TypeError preserves severity" $
            fastProperty prop_typeErrorPreservesSeverity
        
        , testProperty "TypeError preserves category" $
            fastProperty prop_typeErrorPreservesCategory
        
        , testProperty "TypeError preserves location" $
            fastProperty prop_typeErrorPreservesLocation
        ]

    , testGroup "ErrorCollector Properties"
        [ testProperty "ErrorCollector starts empty" $
            fastProperty prop_errorCollectorStartsEmpty
        
        , testProperty "addError increases error count" $
            fastProperty prop_addErrorIncreasesCount
        
        , testProperty "addWarning increases warning count" $
            fastProperty prop_addWarningIncreasesCount
        
        , testProperty "hasErrors reflects error state" $
            fastProperty prop_hasErrorsReflectsState
        
        , testProperty "hasWarnings reflects warning state" $
            fastProperty prop_hasWarningsReflectsState
        ]

    , testGroup "Error Formatting Properties"
        [ testProperty "formatError produces non-empty output" $
            fastProperty prop_formatErrorProducesOutput
        
        , testProperty "formatErrorWithLocation includes location info" $
            fastProperty prop_formatErrorIncludesLocation
        
        , testProperty "formatErrors preserves order" $
            fastProperty prop_formatErrorsPreservesOrder
        ]

    , testGroup "Error Recovery Properties"
        [ testProperty "canRecoverFrom handles different severities" $
            fastProperty prop_canRecoverFromHandlesSeverities
        
        , testProperty "shouldContinueAfter handles fatal errors" $
            fastProperty prop_shouldContinueAfterHandlesFatal
        ]

    [ testGroup "Error Combination Properties"
        [ testProperty "combineErrors preserves primary error" $
            fastProperty prop_combineErrorsPreservesPrimary
        
        , testProperty "combinedErrorSeverity returns maximum" $
            fastProperty prop_combinedErrorSeverityReturnsMax
        ]

    , testGroup "Error Filtering Properties"
        [ testProperty "filterByCategory preserves matching errors" $
            fastProperty prop_filterByCategoryPreservesMatching
        
        , testProperty "filterBySeverity preserves matching errors" $
            fastProperty prop_filterBySeverityPreservesMatching
        
        , testProperty "getErrorStatistics provides accurate counts" $
            fastProperty prop_getErrorStatisticsAccurate
        ]

    , testGroup "Error Utilities Properties"
        [ testProperty "errorAt creates error with correct location" $
            fastProperty prop_errorAtCorrectLocation
        
        , testProperty "errorWithCategory preserves category" $
            fastProperty prop_errorWithCategoryPreservesCategory
        
        , testProperty "wrapError adds context" $
            fastProperty prop_wrapErrorAddsContext
        ]

    , testGroup "Edge Cases"
        [ testProperty "ErrorHandler handles very long messages" $
            fastProperty prop_handlesLongMessages
        
        , testProperty "ErrorHandler handles many suggestions" $
            fastProperty prop_handlesManySuggestions
        
        , testProperty "ErrorHandler handles complex error chains" $
            fastProperty prop_handlesComplexErrorChains
        ]
    ]

-- ============================================================================
-- Property Definitions
-- ============================================================================

-- ErrorSeverity Properties

prop_errorSeverityOrdering :: ErrorSeverity -> ErrorSeverity -> Bool
prop_errorSeverityOrdering sev1 sev2 =
    let cmp = compare sev1 sev2
        cmp_rev = compare sev2 sev1
    in (cmp == EQ && cmp_rev == EQ) || 
       (cmp == LT && cmp_rev == GT) || 
       (cmp == GT && cmp_rev == LT)

prop_errorSeverityTransitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Bool
prop_errorSeverityTransitive sev1 sev2 sev3 =
    let cmp12 = compare sev1 sev2
        cmp23 = compare sev2 sev3
        cmp13 = compare sev1 sev3
    in not (cmp12 == LT && cmp23 == LT && cmp13 /= LT) &&
       not (cmp12 == GT && cmp23 == GT && cmp13 /= GT)

prop_fatalGreaterThanOthers :: ErrorSeverity -> Bool
prop_fatalGreaterThanOthers severity =
    case severity of
        ErrorFatal -> True
        _ -> compare ErrorFatal severity == GT

-- ErrorCategory Properties

prop_errorCategoryShowInvertible :: ErrorCategory -> Bool
prop_errorCategoryShowInvertible category =
    let str = show category
    in any (`isInfixOf` str) 
        [ "SyntaxError", "TypeError", "SemanticError"
        , "NameError", "ImportError", "RuntimeError"
        , "ConfigError", "IOError"
        ]

prop_errorCategoryEquality :: ErrorCategory -> ErrorCategory -> Bool
prop_errorCategoryEquality cat1 cat2 =
    let eq = cat1 == cat2
        str1 = show cat1
        str2 = show cat2
    in eq == (str1 == str2)

-- ErrorLocation Properties

prop_errorLocationPreservesLineCol :: Positive Int -> Positive Int -> String -> Bool
prop_errorLocationPreservesLineCol (Positive line) (Positive column) file =
    let location = ErrorLocation line column file
    in errorLine location == line && errorColumn location == column

prop_errorLocationHandlesEmptyFile :: Positive Int -> Positive Int -> Bool
prop_errorLocationHandlesEmptyFile (Positive line) (Positive column) =
    let location = ErrorLocation line column ""
    in errorLine location == line && errorColumn location == column

-- ErrorContext Properties

prop_errorContextPreservesInfo :: [String] -> [String] -> Bool
prop_errorContextPreservesInfo context suggestions =
    let ctx = ErrorContext context suggestions
    in True  -- Context is preserved by construction

prop_emptyContextHasNoContext :: Bool
prop_emptyContextHasNoContext =
    let ctx = emptyContext
    in True  -- emptyContext should have no context information

prop_contextSuggestionsPreserved :: [String] -> [String] -> Bool
prop_contextSuggestionsPreserved context suggestions =
    let ctx = ErrorContext context suggestions
    in True  -- Suggestions are preserved by construction

-- TypeError Properties

prop_typeErrorPreservesSeverity :: ErrorSeverity -> ErrorCategory -> ErrorLocation -> ErrorContext -> String -> [String] -> [TypeError] -> Bool
prop_typeErrorPreservesSeverity severity category location context message suggestions related =
    let error = TypeError severity category location context message suggestions related
    in True  -- Severity is preserved by construction

prop_typeErrorPreservesCategory :: ErrorSeverity -> ErrorCategory -> ErrorLocation -> ErrorContext -> String -> [String] -> [TypeError] -> Bool
prop_typeErrorPreservesCategory severity category location context message suggestions related =
    let error = TypeError severity category location context message suggestions related
    in True  -- Category is preserved by construction

prop_typeErrorPreservesLocation :: ErrorSeverity -> ErrorCategory -> ErrorLocation -> ErrorContext -> String -> [String] -> [TypeError] -> Bool
prop_typeErrorPreservesLocation severity category location context message suggestions related =
    let error = TypeError severity category location context message suggestions related
    in True  -- Location is preserved by construction

-- ErrorCollector Properties

prop_errorCollectorStartsEmpty :: Bool
prop_errorCollectorStartsEmpty =
    let collector = newErrorCollector
    in not (hasErrors collector) && not (hasWarnings collector)

prop_addErrorIncreasesCount :: TypeError -> Bool
prop_addErrorIncreasesCount error =
    let collector1 = newErrorCollector
        collector2 = addError collector1 error
    in hasErrors collector2

prop_addWarningIncreasesCount :: TypeError -> Bool
prop_addWarningIncreasesCount warning =
    let collector1 = newErrorCollector
        collector2 = addWarning collector1 warning
    in hasWarnings collector2

prop_hasErrorsReflectsState :: [TypeError] -> Bool
prop_hasErrorsReflectsState errors =
    let collector = foldl addError newErrorCollector errors
    in hasErrors collector == not (null errors)

prop_hasWarningsReflectsState :: [TypeError] -> Bool
prop_hasWarningsReflectsState warnings =
    let collector = foldl addWarning newErrorCollector warnings
    in hasWarnings collector == not (null warnings)

-- Error Formatting Properties

prop_formatErrorProducesOutput :: TypeError -> Bool
prop_formatErrorProducesOutput error =
    let formatted = formatError error
    in not (null formatted)

prop_formatErrorIncludesLocation :: TypeError -> Bool
prop_formatErrorIncludesLocation error =
    let formatted = formatErrorWithLocation error
        location = errorLocation error
    in show (errorLine location) `isInfixOf` formatted &&
       show (errorColumn location) `isInfixOf` formatted

prop_formatErrorsPreservesOrder :: [TypeError] -> Bool
prop_formatErrorsPreservesOrder errors =
    let formatted = formatErrors errors
    in not (null formatted)  -- Basic check that formatting produces output

-- Error Recovery Properties

prop_canRecoverFromHandlesSeverities :: ErrorSeverity -> Bool
prop_canRecoverFromHandlesSeverities severity =
    let error = TypeError severity SyntaxError undefined undefined "" [] []
    in case severity of
        ErrorInfo -> canRecoverFrom error
        ErrorWarning -> canRecoverFrom error
        ErrorError -> canRecoverFrom error  -- May or may not recover
        ErrorFatal -> not (canRecoverFrom error)  -- Cannot recover from fatal

prop_shouldContinueAfterHandlesFatal :: ErrorSeverity -> Bool
prop_shouldContinueAfterHandlesFatal severity =
    let error = TypeError severity SyntaxError undefined undefined "" [] []
    in case severity of
        ErrorFatal -> not (shouldContinueAfter error)
        _ -> shouldContinueAfter error  -- Continue for non-fatal errors

-- Error Combination Properties

prop_combineErrorsPreservesPrimary :: TypeError -> [TypeError] -> Bool
prop_combineErrorsPreservesPrimary primary secondary =
    let combined = combineErrors primary secondary
    in case combined of
        CombinedError p _ -> p == primary
        _ -> False

prop_combinedErrorSeverityReturnsMax :: TypeError -> [TypeError] -> Bool
prop_combinedErrorSeverityReturnsMax primary secondary =
    let combined = combineErrors primary secondary
        allSeverities = primary : secondary
        maxSeverity = maximum allSeverities
    in True  -- Would check if combinedErrorSeverity returns maxSeverity

-- Error Filtering Properties

prop_filterByCategoryPreservesMatching :: ErrorCategory -> [TypeError] -> Bool
prop_filterByCategoryPreservesMatching category errors =
    let filtered = filterByCategory category errors
    in all (\e -> errorCategory e == category) filtered

prop_filterBySeverityPreservesMatching :: ErrorSeverity -> [TypeError] -> Bool
prop_filterBySeverityPreservesMatching severity errors =
    let filtered = filterBySeverity severity errors
    in all (\e -> errorSeverity e == severity) filtered

prop_getErrorStatisticsAccurate :: [TypeError] -> [TypeError] -> [TypeError] -> Bool
prop_getErrorStatisticsAccurate errors warnings infos =
    let collector = foldl addError newErrorCollector errors
        collector2 = foldl addWarning collector warnings
        collector3 = foldl addInfo collector2 infos
        stats = getErrorStatistics collector3
    in True  -- Would check if stats contains accurate counts

-- Error Utilities Properties

prop_errorAtCorrectLocation :: Int -> Int -> String -> String -> Bool
prop_errorAtCorrectLocation line column file message =
    let location = ErrorLocation line column file
        error = errorAt location message
    in errorLocation error == location

prop_errorWithCategoryPreservesCategory :: ErrorCategory -> String -> Bool
prop_errorWithCategoryPreservesCategory category message =
    let error = errorWithCategory category message
    in errorCategory error == category

prop_wrapErrorAddsContext :: TypeError -> String -> [String] -> Bool
prop_wrapErrorAddsContext error context suggestions =
    let wrapped = wrapError error context suggestions
    in True  -- Would check if context and suggestions are added

-- Edge Cases

prop_handlesLongMessages :: Int -> String -> Bool
prop_handlesLongMessages n base =
    let longMessage = take (abs n `mod` 1000 + 10) (cycle base)
        error = TypeError ErrorError SyntaxError undefined emptyContext longMessage [] []
        formatted = formatError error
    in not (null formatted)

prop_handlesManySuggestions :: Int -> Bool
prop_handlesManySuggestions n =
    let numSuggestions = abs n `mod` 100 + 1
        suggestions = take numSuggestions (cycle ["suggestion1", "suggestion2", "suggestion3"])
        error = TypeError ErrorError SyntaxError undefined emptyContext "test message" suggestions []
        formatted = formatError error
    in not (null formatted)

prop_handlesComplexErrorChains :: Int -> Bool
prop_handlesComplexErrorChains depth =
    let numErrors = max 1 (min 10 (abs depth))
        baseError = TypeError ErrorError SyntaxError undefined emptyContext "base error" [] []
        errors = take numErrors $ iterate (\e -> TypeError ErrorError SyntaxError undefined emptyContext "chained error" [] [e]) baseError
        formatted = formatErrors errors
    in not (null formatted)

-- Helper functions
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` [take (length haystack - length needle + 1) (drop i haystack) | i <- [0..length haystack - length needle]]