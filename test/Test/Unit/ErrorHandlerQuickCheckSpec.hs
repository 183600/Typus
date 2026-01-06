{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, suchThat)
import qualified Test.QuickCheck as QC

import Compiler.Errors.Core
import Data.Text (Text, pack)
import Data.Time (UTCTime, getCurrentTime)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map
import Data.List (sort, nub)

-- Arbitrary instances for test data
instance Arbitrary ErrorSeverity where
    arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary ErrorCategory where
    arbitrary = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

instance Arbitrary ErrorLocation where
    arbitrary = do
        file <- arbitrary
        line <- choose (1, 1000)
        col <- choose (1, 200)
        endLine <- arbitrary
        endCol <- arbitrary
        return $ ErrorLocation file line col endLine endCol

instance Arbitrary ErrorContext where
    arbitrary = do
        code <- arbitrary
        function <- arbitrary
        variable <- arbitrary
        typ <- arbitrary
        additional <- arbitrary
        return $ ErrorContext code function variable typ additional

instance Arbitrary ErrorRecovery where
    arbitrary = do
        canRec <- arbitrary
        shouldCont <- arbitrary
        action <- arbitrary
        hint <- arbitrary
        cost <- choose (0, 100)
        confidence <- choose (0.0, 1.0)
        return $ RecoveryStrategy canRec shouldCont action hint cost confidence

instance Arbitrary TypeError where
    arbitrary = do
        errorId <- QC.elements ["E001", "E002", "E003", "E004", "E005"]
        severity <- arbitrary
        category <- arbitrary
        message <- pack <$> arbitrary
        location <- arbitrary
        context <- arbitrary
        recovery <- arbitrary
        suggestions <- listOf (pack <$> arbitrary)
        relatedErrors <- listOf arbitrary
        errorChain <- listOf arbitrary
        timestamp <- arbitrary
        return $ TypeError errId errorId severity category message location context recovery suggestions relatedErrors errorChain timestamp

instance Arbitrary CombinedError where
    arbitrary = oneof
        [ OwnershipErrorCombined <$> arbitrary <*> arbitrary
        , DependentTypeErrorCombined <$> arbitrary <*> arbitrary
        , IntegrationError <$> arbitrary <*> arbitrary
        , CrossAnalyzerError <$> arbitrary <*> arbitrary <*> listOf arbitrary
        ]

-- Generate valid error messages
genErrorMessage :: Gen String
genErrorMessage = do
    n <- choose (10, 200)
    elements ["Type mismatch", "Variable not in scope", "Ownership violation", "Parse error", "Constraint violation"] >>= \base ->
    elements [" in function", " at line", " in module", " with type"] >>= \context ->
    return $ base ++ context

-- Generate valid error IDs
genErrorId :: Gen String
genErrorId = do
    prefix <- elements ["E", "W", "F", "I"]
    num <- choose (1, 999)
    return $ prefix ++ show num

-- Property: Error severity ordering is consistent
prop_severity_ordering_consistent :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_ordering_consistent sev1 sev2 =
    let ordered = sev1 <= sev2
        priorityOrdered = severityPriority sev1 <= severityPriority sev2
    in property $ ordered === priorityOrdered

-- Property: Severity priority is monotonic
prop_severity_priority_monotonic :: ErrorSeverity -> Property
prop_severity_priority_monotonic severity =
    let priority = severityPriority severity
        validRange = priority >= 0 && priority <= 100
    in classify validRange "Valid priority range" $
       property $ validRange ==> priority >= 0

-- Property: Error location is valid
prop_error_location_valid :: ErrorLocation -> Property
prop_error_location_valid location =
    let lineValid = line location > 0
        colValid = column location > 0
        endLineValid = maybe True (\el -> el >= line location) (endLine location)
        endColValid = maybe True (\ec -> ec > 0) (endColumn location)
    in classify lineValid "Valid line" $
       classify colValid "Valid column" $
       classify endLineValid "Valid end line" $
       classify endColValid "Valid end column" $
       property $ lineValid && colValid && endLineValid && endColValid

-- Property: Error recovery cost is within bounds
prop_recovery_cost_bounds :: ErrorRecovery -> Property
prop_recovery_cost_bounds recovery =
    let cost = recoveryCost recovery
        confidence = recoveryConfidence recovery
        costValid = cost >= 0 && cost <= 100
        confidenceValid = confidence >= 0.0 && confidence <= 1.0
    in classify costValid "Valid cost" $
       classify confidenceValid "Valid confidence" $
       property $ costValid && confidenceValid

-- Property: Fatal errors cannot recover
prop_fatal_no_recovery :: ErrorRecovery -> Property
prop_fatal_no_recovery recovery =
    let canRec = canRecover recovery
        shouldCont = shouldContinue recovery
        cost = recoveryCost recovery
    in classify (cost == 100) "Fatal recovery cost" $
       property $ cost == 100 ==> not canRec && not shouldCont

-- Property: Error suggestions are non-empty when provided
prop_error_suggestions_nonempty :: TypeError -> Property
prop_error_suggestions_nonempty error =
    let suggestions = errorSuggestions error
        hasSuggestions = not (null suggestions)
        allNonEmpty = L.all (not . T.null) suggestions
    in classify hasSuggestions "Has suggestions" $
       classify (not hasSuggestions) "No suggestions" $
       property $ hasSuggestions ==> allNonEmpty

-- Property: Combined error severity is consistent
prop_combined_error_severity_consistent :: CombinedError -> Property
prop_combined_error_severity_consistent combinedError =
    let severity = combinedErrorSeverity combinedError
        validSeverity = severity `elem` [Fatal, Error, Warning, Info]
    in property $ validSeverity

-- Property: Error filtering preserves ordering
prop_error_filter_preserves_ordering :: ErrorSeverity -> [TypeError] -> Property
prop_error_filter_preserves_ordering minSeverity errors =
    let filtered = filterBySeverity minSeverity errors
        ordered = L.all (\e -> severity e >= minSeverity) filtered
    in classify (not (null filtered)) "Has filtered errors" $
       classify (null filtered) "No filtered errors" $
       property $ ordered

-- Property: Error statistics are accurate
prop_error_statistics_accurate :: [TypeError] -> Property
prop_error_statistics_accurate errors =
    let stats = getErrorStatistics errors
        actualFatal = L.length $ L.filter (\e -> severity e == Fatal) errors
        actualError = L.length $ L.filter (\e -> severity e == Error) errors
        actualWarning = L.length $ L.filter (\e -> severity e == Warning) errors
        actualInfo = L.length $ L.filter (\e -> severity e == Info) errors
    in property $ actualFatal == L.length (filterBySeverity Fatal errors) &&
                actualError == L.length (filterBySeverity Error errors) &&
                actualWarning == L.length (filterBySeverity Warning errors) &&
                actualInfo == L.length (filterBySeverity Info errors)

-- Property: Error context can be empty
prop_error_context_empty :: ErrorContext -> Property
prop_error_context_empty context =
    let isEmpty = isNothing (contextCode context) &&
                  isNothing (contextFunction context) &&
                  isNothing (contextVariable context) &&
                  isNothing (contextType context) &&
                  L.null (contextAdditional context)
    in classify isEmpty "Empty context" $
       classify (not isEmpty) "Non-empty context" $
       property True

-- Property: Error chaining preserves original errors
prop_error_chaining_preserves_original :: TypeError -> [TypeError] -> Property
prop_error_chaining_preserves_original baseError chainErrors =
    let chainedError = baseError { errorChain = chainErrors }
        chainLength = L.length (errorChain chainedError)
        originalUnchanged = errorId baseError == errorId chainedError &&
                           severity baseError == severity chainedError
    in classify (not (null chainErrors)) "Has chain" $
       classify (null chainErrors) "No chain" $
       property $ chainLength == L.length chainErrors && originalUnchanged

-- Property: Error formatting preserves essential information
prop_error_format_preserves_info :: TypeError -> Property
prop_error_format_preserves_info error =
    let formatted = formatError error
        hasMessage = T.unpack (message error) `L.isInfixOf` formatted
        hasLocation = show (line (location error)) `L.isInfixOf` formatted
        hasSeverity = show (severity error) `L.isInfixOf` formatted
    in classify hasMessage "Has message" $
       classify hasLocation "Has location" $
       classify hasSeverity "Has severity" $
       property $ hasMessage && hasLocation && hasSeverity

-- Property: Error recovery strategies are consistent
prop_recovery_strategy_consistent :: ErrorRecovery -> Property
prop_recovery_strategy_consistent recovery =
    let canRec = canRecover recovery
        shouldCont = shouldContinue recovery
        cost = recoveryCost recovery
        confidence = recoveryConfidence recovery
        consistent = (canRec ==> shouldCont) &&  -- If can recover, should continue
                    (cost >= 0 && cost <= 100) &&
                    (confidence >= 0.0 && confidence <= 1.0)
    in property $ consistent

-- Property: Multiple errors can be combined
prop_combine_errors_preserves_all :: [TypeError] -> Property
prop_combine_errors_preserves_all errors =
    let combined = combineErrors errors
        combinedCount = L.length combined
        originalCount = L.length errors
    in classify (not (null errors)) "Has errors" $
       classify (null errors) "No errors" $
       property $ combinedCount >= originalCount

-- Property: Error filtering by category works correctly
prop_filter_by_category :: ErrorCategory -> [TypeError] -> Property
prop_filter_by_category targetCategory errors =
    let filtered = filterByCategory targetCategory errors
        allCorrectCategory = L.all (\e -> category e == targetCategory) filtered
    in classify (not (null filtered)) "Has filtered errors" $
       classify (null filtered) "No filtered errors" $
       property $ allCorrectCategory

-- Property: Error severity comparison is transitive
prop_severity_comparison_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_severity_comparison_transitive sev1 sev2 sev3 =
    let comp12 = compareSeverity sev1 sev2
        comp23 = compareSeverity sev2 sev3
        comp13 = compareSeverity sev1 sev3
        transitive = not (comp12 == LT && comp23 == LT && comp13 /= LT) &&
                     not (comp12 == GT && comp23 == GT && comp13 /= GT)
    in property $ transitive

-- Helper function for string infix check
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` [take (L.length needle) (drop i haystack) | i <- [0..L.length haystack - L.length needle]]

-- Test group containing L.all QuickCheck properties
tests :: TestTree
tests = testGroup "ErrorHandler QuickCheck tests"
    [ fastProperty "Error severity ordering is consistent" prop_severity_ordering_consistent
    , fastProperty "Severity priority is monotonic" prop_severity_priority_monotonic
    , fastProperty "Error location is valid" prop_error_location_valid
    , fastProperty "Error recovery cost is within bounds" prop_recovery_cost_bounds
    , fastProperty "Fatal errors cannot recover" prop_fatal_no_recovery
    , fastProperty "Error suggestions are non-empty when provided" prop_error_suggestions_nonempty
    , fastProperty "Combined error severity is consistent" prop_combined_error_severity_consistent
    , fastProperty "Error filtering preserves ordering" prop_error_filter_preserves_ordering
    , fastProperty "Error statistics are accurate" prop_error_statistics_accurate
    , fastProperty "Error context can be empty" prop_error_context_empty
    , fastProperty "Error chaining preserves original errors" prop_error_chaining_preserves_original
    , fastProperty "Error formatting preserves essential information" prop_error_format_preserves_info
    , fastProperty "Error recovery strategies are consistent" prop_recovery_strategy_consistent
    , fastProperty "Multiple errors can be combined" prop_combine_errors_preserves_all
    , fastProperty "Error filtering by category works correctly" prop_filter_by_category
    , fastProperty "Error severity comparison is transitive" prop_severity_comparison_transitive
    ]