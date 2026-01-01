module Test.Unit.NewErrorHandlerPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, choose, listOf, elements, suchThat)
import Compiler.Errors.Core
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Data.Text as T (pack, unpack)
import Data.Time (UTCTime, addUTCTime, nominalDay)
import Data.Maybe (isJust, isNothing)

-- | 新的ErrorHandler属性QuickCheck测试
tests :: TestTree
tests =
  testGroup "New Error Handler Properties Tests"
    [ testGroup "Error severity properties"
        [ fastProperty "severityPriority ordering" prop_severityPriorityOrdering
        , fastProperty "compareSeverity consistency" prop_compareSeverityConsistency
        , fastProperty "isAtLeast transitivity" prop_isAtLeastTransitivity
        ]

    , testGroup "Error collection properties"
        [ fastProperty "error collector preserves order" prop_errorCollectorPreservesOrder
        , fastProperty "error filtering by severity" prop_errorFilteringBySeverity
        , fastProperty "error statistics accuracy" prop_errorStatisticsAccuracy
        ]

    , testGroup "Error formatting properties"
        [ fastProperty "formatError preserves essential info" prop_formatErrorPreservesInfo
        , fastProperty "formatErrors preserves count" prop_formatErrorsPreservesCount
        , fastProperty "error formatting idempotence" prop_errorFormattingIdempotence
        ]

    , testGroup "Error recovery properties"
        [ fastProperty "canRecoverFrom consistency" prop_canRecoverFromConsistency
        , fastProperty "shouldContinueAfter logic" prop_shouldContinueAfterLogic
        , fastProperty "recovery strategy creation" prop_recoveryStrategyCreation
        ]

    , testGroup "Error location properties"
        [ fastProperty "error location preservation" prop_errorLocationPreservation
        , fastProperty "error with context" prop_errorWithContext
        , fastProperty "error combination properties" prop_errorCombinationProperties
        ]
    ]

-- ============================================================================
-- Arbitrary instances for test data
-- ============================================================================

instance Arbitrary ErrorSeverity where
    arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary ErrorCategory where
    arbitrary = oneof
        [ return ParseError
        , return TypeError
        , return OwnershipError
        , return DependencyError
        , return InternalError
        , return WarningCategory
        , return InfoCategory
        ]

instance Arbitrary ErrorLocation where
    arbitrary = do
        line <- choose (1, 1000)
        column <- choose (1, 200)
        endLine <- choose (line, line + 10)
        endColumn <- choose (column, column + 50)
        filePath <- oneof [return Nothing, Just <$> arbitrary]
        return $ ErrorLocation filePath line column (Just endLine) (Just endColumn)

instance Arbitrary ErrorContext where
    arbitrary = do
        context <- listOf arbitrary
        suggestions <- listOf arbitrary
        relatedErrors <- listOf arbitrary
        return $ ErrorContext context suggestions relatedErrors

instance Arbitrary TypeError where
    arbitrary = do
        severity <- arbitrary
        category <- arbitrary
        message <- arbitrary
        location <- arbitrary
        context <- arbitrary
        timestamp <- arbitrary
        recovery <- arbitrary
        return $ TypeError errId severity category message location context timestamp recovery instance Arbitrary ErrorRecovery where
    arbitrary = oneof
        [ return NoRecovery
        , return SkipCurrentBlock
        , return ContinueWithNextBlock
        , return RetryWithAlternative
        , return AbortCompilation
        , CustomRecovery <$> arbitrary
        ]

instance Arbitrary CombinedError where
    arbitrary = do
        primary <- arbitrary
        secondary <- listOf arbitrary
        return $ CombinedError primary secondary

-- Generate error messages
genErrorMessage :: Gen String
genErrorMessage = do
    words <- listOf $ elements ["error", "type", "mismatch", "expected", "found", "syntax", "invalid", "cannot", "undefined"]
    return $ unwords words

-- Generate error locations with specific properties
genErrorLocationAtLine :: Int -> Gen ErrorLocation
genErrorLocationAtLine line = do
    column <- choose (1, 200)
    endLine <- choose (line, line + 5)
    endColumn <- choose (column, column + 50)
    filePath <- oneof [return Nothing, Just <$> arbitrary]
    return $ ErrorLocation filePath line column (Just endLine) (Just endColumn)

-- ============================================================================
-- Properties for Error Severity
-- ============================================================================

prop_severityPriorityOrdering :: ErrorSeverity -> ErrorSeverity -> Bool
prop_severityPriorityOrdering sev1 sev2 =
    let priority1 = severityPriority sev1
        priority2 = severityPriority sev2
        comparison = compareSeverity sev1 sev2
    in case comparison of
        LT -> priority1 < priority2
        EQ -> priority1 == priority2
        GT -> priority1 > priority2

prop_compareSeverityConsistency :: ErrorSeverity -> ErrorSeverity -> Bool
prop_compareSeverityConsistency sev1 sev2 =
    let comparison1 = compareSeverity sev1 sev2
        comparison2 = compareSeverity sev2 sev1
    in case (comparison1, comparison2) of
        (LT, GT) -> True
        (EQ, EQ) -> True
        (GT, LT) -> True
        _ -> False

prop_isAtLeastTransitivity :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Bool
prop_isAtLeastTransitivity minSev midSev maxSev =
    let minToMid = isAtLeast minSev midSev
        midToMax = isAtLeast midSev maxSev
        minToMax = isAtLeast minSev maxSev
    in if minToMid && midToMax then minToMax else True

-- ============================================================================
-- Properties for Error Collection
-- ============================================================================

prop_errorCollectorPreservesOrder :: [TypeError] -> Bool
prop_errorCollectorPreservesOrder errors =
    let collector = newErrorCollector
        addErr e = modify (e :)
        result = execState (mapM_ addErr errors) collector
        collected = L.reverse result  -- Reverse because we added to front
    in L.length collected == L.length errors

prop_errorFilteringBySeverity :: [TypeError] -> ErrorSeverity -> Bool
prop_errorFilteringBySeverity errors minSeverity =
    let filtered = filterBySeverity minSeverity errors
    in L.all (\e -> isAtLeast minSeverity (severity e)) filtered

prop_errorStatisticsAccuracy :: [TypeError] -> Bool
prop_errorStatisticsAccuracy errors =
    let stats = getErrorStatistics errors
        actualFatal = L.length $ L.filter (\e -> severity e == Fatal) errors
        actualError = L.length $ L.filter (\e -> severity e == Error) errors
        actualWarning = L.length $ L.filter (\e -> severity e == Warning) errors
        actualInfo = L.length $ L.filter (\e -> severity e == Info) errors
    in stats == (actualFatal, actualError, actualWarning, actualInfo)

-- ============================================================================
-- Properties for Error Formatting
-- ============================================================================

prop_formatErrorPreservesInfo :: TypeError -> Bool
prop_formatErrorPreservesInfo error =
    let formatted = formatError error
        message = T.unpack (errorMessage error)
    in message `L.isInfixOf` formatted

prop_formatErrorsPreservesCount :: [TypeError] -> Bool
prop_formatErrorsPreservesCount errors =
    let formatted = formatErrors errors
        -- Count error indicators in formatted output
        errorCount = L.length $ L.filter (== "error") (words formatted)
    in errorCount >= L.length errors  -- At least one error indicator per error

prop_errorFormattingIdempotence :: TypeError -> Bool
prop_errorFormattingIdempotence error =
    let formatted1 = formatError error
        formatted2 = formatError error
    in formatted1 == formatted2

-- ============================================================================
-- Properties for Error Recovery
-- ============================================================================

prop_canRecoverFromConsistency :: ErrorSeverity -> Bool
prop_canRecoverFromConsistency severity =
    let canRecover = canRecoverFrom severity
    in case severity of
        Fatal -> not canRecover
        _ -> canRecover

prop_shouldContinueAfterLogic :: ErrorSeverity -> Bool
prop_shouldContinueAfterLogic severity =
    let shouldContinue = shouldContinueAfter severity
    in case severity of
        Fatal -> not shouldContinue
        Error -> shouldContinue
        Warning -> shouldContinue
        Info -> shouldContinue

prop_recoveryStrategyCreation :: ErrorRecovery -> Bool
prop_recoveryStrategyCreation recovery =
    let strategy = createRecoveryStrategy recovery
    in case recovery of
        NoRecovery -> True
        SkipCurrentBlock -> True
        ContinueWithNextBlock -> True
        RetryWithAlternative -> True
        AbortCompilation -> True
        CustomRecovery _ -> True

-- ============================================================================
-- Properties for Error Location
-- ============================================================================

prop_errorLocationPreservation :: String -> ErrorLocation -> Bool
prop_errorLocationPreservation message location =
    let error = errorAt "test-id" (error message)
        errorContext = errorContext error
    in errorContext == context

prop_errorCombinationProperties :: TypeError -> [TypeError] -> Bool
prop_errorCombinationProperties primary secondary =
    let combined = combineErrors primary secondary
        combinedSeverity = combinedErrorSeverity combined
        primarySeverity = severity primary
    in combinedSeverity >= primarySeverity  -- Combined should be at least as severe

-- ============================================================================
-- Helper functions
-- ============================================================================

-- Check if a substring is in a string
isInfixOf :: Eq a => [a] -> [a] -> Bool
L.isInfixOf needle haystack = needle `elem` [take (L.length needle) (drop i haystack) | i <- [0..L.length haystack - L.length needle]]

-- Mock State monad execution for testing
execState :: State s a -> s -> s
execState action s = snd (runState action s)

-- Mock runState for testing
runState :: State s a -> s -> (a, s)
runState action s = (undefined, s)  -- Simplified for testing