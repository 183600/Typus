module Test.Unit.ErrorHandlerAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, elements, choose, listOf, suchThat, vectorOf)
import TestSupport.QuickCheck (fastProperty)

import Compiler.Errors.Core (
    TypeError(..), ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..), 
    ErrorContext(..), ErrorRecovery(..), emptyContext,
    errorAt, errorWithCategory, warningAt, infoAt, fatalError,
    withLocation, withContext, withSuggestions, withTimestamp,
    errorWithSuggestions, wrapError, combineErrors,
    hasCategory, filterByCategory, filterBySeverity,
    getErrorStatistics, formatError, formatErrorWithLocation,
    canRecoverFrom, shouldContinueAfter,
    errorRecovery, warningRecovery, fatalRecovery, infoRecovery,
    customRecovery, toErrorLocation, _atLocation, _atRange
    )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Map.Strict as Map

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary ErrorSeverity where
    arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary ErrorCategory where
    arbitrary = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, 
                         Constraint, Inference, Integration, Unknown]

instance Arbitrary ErrorLocation where
    arbitrary = do
        line <- choose (0, 1000)
        column <- choose (0, 1000)
        endLine <- oneof [pure Nothing, Just <$> choose (line, 1000)]
        endColumn <- oneof [pure Nothing, Just <$> choose (0, 1000)]
        filePath <- oneof [pure Nothing, Just <$> arbitrary]
        return $ ErrorLocation filePath line column endLine endColumn

instance Arbitrary ErrorContext where
    arbitrary = do
        code <- oneof [pure Nothing, Just <$> arbitrary]
        func <- oneof [pure Nothing, Just <$> arbitrary]
        var <- oneof [pure Nothing, Just <$> arbitrary]
        typ <- oneof [pure Nothing, Just <$> arbitrary]
        additional <- listOf ((,) <$> arbitrary <*> arbitrary)
        return $ ErrorContext code func var typ additional

instance Arbitrary ErrorRecovery where
    arbitrary = do
        canRec <- arbitrary
        shouldCont <- arbitrary
        action <- oneof [pure Nothing, Just <$> arbitrary]
        hint <- oneof [pure Nothing, Just <$> arbitrary]
        cost <- choose (0, 100)
        confidence <- choose (0.0, 1.0)
        return $ ErrorRecovery canRec shouldCont action hint cost confidence

instance Arbitrary TypeError where
    arbitrary = do
        errorId <- arbitrary
        severity <- arbitrary
        category <- arbitrary
        message <- T.pack <$> arbitrary
        location <- arbitrary
        context <- arbitrary
        recovery <- arbitrary
        suggestions <- listOf (T.pack <$> arbitrary)
        relatedErrors <- listOf arbitrary
        errorChain <- listOf arbitrary
        timestamp <- oneof [pure Nothing, Just <$> arbitrary]
        return $ TypeError errorId severity category message location context recovery 
                            suggestions relatedErrors errorChain timestamp

-- ============================================================================
-- Error Severity Properties
-- ============================================================================

prop_severityOrderingIsCorrect :: ErrorSeverity -> ErrorSeverity -> Bool
prop_severityOrderingIsCorrect sev1 sev2 =
    let severityOrdering sev = case sev of
          Fatal -> 4
          Error -> 3
          Warning -> 2
          Info -> 1
    in if sev1 >= sev2
       then severityOrdering sev1 >= severityOrdering sev2
       else severityOrdering sev1 <= severityOrdering sev2

prop_fatalErrorsCannotRecover :: ErrorLocation -> Bool
prop_fatalErrorsCannotRecover loc =
    let err = fatalError "TEST" "Test fatal error" loc
    in not (canRecoverFrom err) && not (shouldContinueAfter err)

prop_warningErrorsCanRecover :: ErrorLocation -> Bool
prop_warningErrorsCanRecover loc =
    let err = warningAt "TEST" "Test warning" loc
    in canRecoverFrom err && shouldContinueAfter err

prop_infoErrorsCanRecover :: ErrorLocation -> Bool
prop_infoErrorsCanRecover loc =
    let err = infoAt "TEST" "Test info" loc
    in canRecoverFrom err && shouldContinueAfter err

-- ============================================================================
-- Error Category Properties
-- ============================================================================

prop_hasCategoryWorksCorrectly :: ErrorCategory -> ErrorLocation -> Bool
prop_hasCategoryWorksCorrectly cat loc =
    let err = errorWithCategory "TEST" cat "Test message" loc
    in hasCategory cat err && not (hasCategory (nextCategory cat) err)
  where
    nextCategory TypeChecking = Ownership
    nextCategory Ownership = Parsing
    nextCategory Parsing = Semantic
    nextCategory Semantic = Runtime
    nextCategory Runtime = Constraint
    nextCategory Constraint = Inference
    nextCategory Inference = Integration
    nextCategory Integration = Unknown
    nextCategory Unknown = TypeChecking

prop_filterByCategoryPreservesOthers :: ErrorCategory -> [TypeError] -> Bool
prop_filterByCategoryPreservesOthers cat errors =
    let filtered = filterByCategory cat errors
    in all hasCategory filtered && length filtered <= length errors

prop_filterBySeverityPreservesOthers :: ErrorSeverity -> [TypeError] -> Bool
prop_filterBySeverityPreservesOthers sev errors =
    let filtered = filterBySeverity sev errors
    in all (\e -> severity e == sev) filtered && length filtered <= length errors

-- ============================================================================
-- Error Construction Properties
-- ============================================================================

prop_errorAtPreservesLocation :: String -> Text -> ErrorLocation -> Bool
prop_errorAtPreservesLocation errId msg loc =
    let err = errorAt errId msg loc
    in errorId err == errId &&
       message err == msg &&
       location err == loc &&
       severity err == Error &&
       category err == Unknown

prop_errorWithCategorySetsCategory :: String -> ErrorCategory -> Text -> ErrorLocation -> Bool
prop_errorWithCategorySetsCategory errId cat msg loc =
    let err = errorWithCategory errId cat msg loc
    in errorId err == errId &&
       message err == msg &&
       location err == loc &&
       severity err == Error &&
       category err == cat

prop_wrapErrorPreservesInnerError :: Text -> TypeError -> Bool
prop_wrapErrorPreservesInnerError wrapperMsg innerErr =
    let wrapped = wrapError wrapperMsg innerErr
    in message wrapped == wrapperMsg <> ": " <> message innerErr &&
       innerErr `elem` errorChain wrapped &&
       severity wrapped == severity innerErr &&
       location wrapped == location innerErr

prop_withSuggestionsAddsSuggestions :: [Text] -> TypeError -> Bool
prop_withSuggestionsAddsSuggestions newSuggestions err =
    let modified = withSuggestions newSuggestions err
        allSuggestions = newSuggestions ++ suggestions err
    in suggestions modified == allSuggestions

-- ============================================================================
-- Error Collection Properties
-- ============================================================================

prop_combineErrorsPreservesAll :: [TypeError] -> Bool
prop_combineErrorsPreservesAll errors =
    let combined = combineErrors errors
        -- Count all errors including related ones
        totalCount = length errors + sum (length . relatedErrors <$> errors)
        combinedCount = length combined
    in combinedCount >= length errors && combinedCount <= totalCount

prop_getErrorStatisticsCountsCorrectly :: [TypeError] -> Bool
prop_getErrorStatisticsCountsCorrectly errors =
    let stats = getErrorStatistics errors
        total = Map.findWithDefault 0 "total" stats
        fatal = Map.findWithDefault 0 "fatal" stats
        errorCount = Map.findWithDefault 0 "errors" stats
        warnings = Map.findWithDefault 0 "warnings" stats
        info = Map.findWithDefault 0 "info" stats
    in total == length errors &&
       fatal == length (filterBySeverity Fatal errors) &&
       errorCount == length (filterBySeverity Error errors) &&
       warnings == length (filterBySeverity Warning errors) &&
       info == length (filterBySeverity Info errors)

-- ============================================================================
-- Error Formatting Properties
-- ============================================================================

prop_formatErrorContainsSeverity :: TypeError -> Bool
prop_formatErrorContainsSeverity err =
    let formatted = formatError err
        severityStr = case severity err of
          Fatal -> "FATAL"
          Error -> "ERROR"
          Warning -> "WARNING"
          Info -> "INFO"
    in severityStr `isInfixOf` formatted

prop_formatErrorContainsMessage :: TypeError -> Bool
prop_formatErrorContainsMessage err =
    let formatted = formatError err
        msgStr = T.unpack (message err)
    in msgStr `isInfixOf` formatted

prop_formatErrorWithLocationContainsLocation :: TypeError -> Bool
prop_formatErrorWithLocationContainsLocation err =
    let formatted = formatErrorWithLocation err
        loc = location err
        lineStr = if line loc > 0 then show (line loc) else "?"
        colStr = if column loc > 0 then show (column loc) else "?"
    in lineStr `isInfixOf` formatted && colStr `isInfixOf` formatted

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

prop_customRecoveryWorksCorrectly :: Bool -> Bool -> Maybe String -> Maybe String -> Int -> Float -> Bool
prop_customRecoveryWorksCorrectly canRec shouldCont action hint cost confidence =
    let recovery = customRecovery canRec shouldCont action hint cost confidence
    in canRecover recovery == canRec &&
       shouldContinue recovery == shouldCont &&
       recoveryAction recovery == action &&
       recoveryHint recovery == hint &&
       recoveryCost recovery == cost &&
       recoveryConfidence recovery == confidence

prop_fatalRecoveryCannotRecover :: Bool
prop_fatalRecoveryCannotRecover =
    let recovery = fatalRecovery
    in not (canRecover recovery) && not (shouldContinue recovery)

prop_errorRecoveryCanContinue :: Bool
prop_errorRecoveryCanContinue =
    let recovery = errorRecovery
    in canRecover recovery && shouldContinue recovery

-- ============================================================================
-- Advanced Error Properties
-- ============================================================================

prop_withLocationOverridesLocation :: ErrorLocation -> TypeError -> Bool
prop_withLocationOverridesLocation newLoc err =
    let modified = withLocation err newLoc
    in location modified == newLoc &&
       message modified == message err &&
       severity modified == severity err

prop_withContextOverridesContext :: ErrorContext -> TypeError -> Bool
prop_withContextOverridesContext newCtx err =
    let modified = withContext err newCtx
    in context modified == newCtx &&
       message modified == message err &&
       severity modified == severity err

prop_timestampDoesNotAffectOtherFields :: String -> TypeError -> Bool
prop_timestampDoesNotAffectOtherFields ts err =
    let modified = withTimestamp ts err
    in timestamp modified == Just ts &&
       message modified == message err &&
       severity modified == severity err &&
       location modified == location err

prop_errorIdPreservedThroughModifications :: String -> TypeError -> Bool
prop_errorIdPreservedThroughModifications errId err =
    let baseErr = err { errorId = errId }
        withLoc = withLocation baseErr (_atLocation 1 1)
        withCtx = withContext baseErr emptyContext
        withSugg = withSuggestions [] baseErr
        wrapped = wrapError "wrapper" baseErr
    in errorId withLoc == errId &&
       errorId withCtx == errId &&
       errorId withSugg == errId &&
       errorId wrapped == errId

-- Helper function for string infix check
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` [take (length needle) (drop x haystack) | x <- [0..length haystack - length needle]]

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =
  testGroup "ErrorHandler Advanced QuickCheck Tests"
    [ testGroup "Error Severity Properties"
        [ fastProperty "severity ordering is correct" prop_severityOrderingIsCorrect
        , fastProperty "fatal errors cannot recover" prop_fatalErrorsCannotRecover
        , fastProperty "warning errors can recover" prop_warningErrorsCanRecover
        , fastProperty "info errors can recover" prop_infoErrorsCanRecover
        ]

    , testGroup "Error Category Properties"
        [ fastProperty "hasCategory works correctly" prop_hasCategoryWorksCorrectly
        , fastProperty "filterByCategory preserves others" prop_filterByCategoryPreservesOthers
        , fastProperty "filterBySeverity preserves others" prop_filterBySeverityPreservesOthers
        ]

    , testGroup "Error Construction Properties"
        [ fastProperty "errorAt preserves location" prop_errorAtPreservesLocation
        , fastProperty "errorWithCategory sets category" prop_errorWithCategorySetsCategory
        , fastProperty "wrapError preserves inner error" prop_wrapErrorPreservesInnerError
        , fastProperty "withSuggestions adds suggestions" prop_withSuggestionsAddsSuggestions
        ]

    , testGroup "Error Collection Properties"
        [ fastProperty "combineErrors preserves all" prop_combineErrorsPreservesAll
        , fastProperty "getErrorStatistics counts correctly" prop_getErrorStatisticsCountsCorrectly
        ]

    , testGroup "Error Formatting Properties"
        [ fastProperty "formatError contains severity" prop_formatErrorContainsSeverity
        , fastProperty "formatError contains message" prop_formatErrorContainsMessage
        , fastProperty "formatErrorWithLocation contains location" prop_formatErrorWithLocationContainsLocation
        ]

    , testGroup "Error Recovery Properties"
        [ fastProperty "customRecovery works correctly" prop_customRecoveryWorksCorrectly
        , fastProperty "fatalRecovery cannot recover" prop_fatalRecoveryCannotRecover
        , fastProperty "errorRecovery can continue" prop_errorRecoveryCanContinue
        ]

    , testGroup "Advanced Error Properties"
        [ fastProperty "withLocation overrides location" prop_withLocationOverridesLocation
        , fastProperty "withContext overrides context" prop_withContextOverridesContext
        , fastProperty "timestamp does not affect other fields" prop_timestampDoesNotAffectOtherFields
        , fastProperty "errorId preserved through modifications" prop_errorIdPreservedThroughModifications
        ]
    ]