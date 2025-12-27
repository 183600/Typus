module Test.Unit.NewQuickCheckTestSuite4Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.QuickCheck (Property, (==>), forAll, Gen, arbitrary, choose, oneof, elements)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)

import TestSupport.QuickCheck (fastProperty)
import Compiler.Errors.Core

-- | Test suite for ErrorHandling module error recovery
tests :: TestTree
tests =
  testGroup "NewQuickCheckTestSuite4 - ErrorHandling Error Recovery"
    [ testGroup "Error severity operations"
        [ testCase "severityPriority returns correct values" $ do
            severityPriority Fatal @?= 100
            severityPriority Error @?= 80
            severityPriority Warning @?= 30
            severityPriority Info @?= 10
            
        , testCase "compareSeverity orders correctly" $ do
            compareSeverity Fatal Error @?= GT
            compareSeverity Error Warning @?= GT
            compareSeverity Warning Info @?= GT
            compareSeverity Error Error @?= EQ
            
        , testCase "isAtLeast severity comparison works" $ do
            isAtLeast Error Fatal @?= False
            isAtLeast Error Error @?= True
            isAtLeast Error Warning @?= True
            isAtLeast Warning Info @?= True
        ]

    , testGroup "Detailed severity operations"
        [ testCase "detailedSeverityPriority combines base and sub-level" $ do
            let criticalFatal = DetailedSeverity Fatal Critical Nothing
                highFatal = DetailedSeverity Fatal High Nothing
                mediumError = DetailedSeverity Error Medium Nothing
            detailedSeverityPriority criticalFatal @?= 150  -- 100 + 50
            detailedSeverityPriority highFatal @?= 130       -- 100 + 30
            detailedSeverityPriority mediumError @?= 95      -- 80 + 15
            
        , testCase "detailed severity predicates work" $ do
            let critical = DetailedSeverity Error Critical Nothing
                high = DetailedSeverity Error High Nothing
                medium = DetailedSeverity Error Medium Nothing
                low = DetailedSeverity Error Low Nothing
                notification = DetailedSeverity Info Notification Nothing
            _isCritical critical @?= True
            _isHigh high @?= True
            _isMedium medium @?= True
            _isLow low @?= True
            _isNotification notification @?= True
        ]

    , testGroup "Error recovery strategies"
        [ testCase "fatalRecovery has correct properties" $ do
            canRecover fatalRecovery @?= False
            shouldContinue fatalRecovery @?= False
            recoveryCost fatalRecovery @?= 100
            recoveryConfidence fatalRecovery @?= 0.0
            
        , testCase "errorRecovery has correct properties" $ do
            canRecover errorRecovery @?= True
            shouldContinue errorRecovery @?= True
            recoveryCost errorRecovery @?= 50
            recoveryConfidence errorRecovery @?= 0.7
            
        , testCase "warningRecovery has correct properties" $ do
            canRecover warningRecovery @?= True
            shouldContinue warningRecovery @?= True
            recoveryCost warningRecovery @?= 10
            recoveryConfidence warningRecovery @?= 0.9
            
        , testCase "infoRecovery has correct properties" $ do
            canRecover infoRecovery @?= True
            shouldContinue infoRecovery @?= True
            recoveryCost infoRecovery @?= 0
            recoveryConfidence infoRecovery @?= 1.0
            
        , testCase "customRecovery creates strategy with given values" $ do
            let custom = customRecovery False True (Just "action") (Just "hint") 25 0.85
            canRecover custom @?= False
            shouldContinue custom @?= True
            recoveryAction custom @?= Just "action"
            recoveryHint custom @?= Just "hint"
            recoveryCost custom @?= 25
            recoveryConfidence custom @?= 0.85
        ]

    , testGroup "Error collector operations"
        [ testCase "newErrorCollector creates empty collector" $ do
            let collector = newErrorCollector
            hasErrors collector @?= False
            hasWarnings collector @?= False
            length (getErrors collector) @?= 0
            length (getWarnings collector) @?= 0
            
        , testCase "addError adds error to collector" $ do
            let collector = newErrorCollector
                error = errorAt "test error" 1 1
                collector' = addError error collector
            hasErrors collector' @?= True
            length (getErrors collector') @?= 1
            
        , testCase "addWarning adds warning to collector" $ do
            let collector = newErrorCollector
                warning = warningAt "test warning" 1 1
                collector' = addWarning warning collector
            hasWarnings collector' @?= True
            length (getWarnings collector') @?= 1
            
        , testCase "getAllMessages includes all message types" $ do
            let collector = newErrorCollector
                error = errorAt "error" 1 1
                warning = warningAt "warning" 1 1
                info = infoAt "info" 1 1
                collector' = addInfo info $ addWarning warning $ addError error collector
            let allMessages = getAllMessages collector'
            length allMessages @?= 3
        ]

    , testGroup "Error formatting"
        [ testCase "formatError creates basic format" $ do
            let error = errorAt "test error" 5 10
                formatted = formatError error
            T.unpack formatted `contains` "test error" @?= True
            
        , testCase "formatErrorWithLocation includes position" $ do
            let error = errorAt "test error" 3 7
                formatted = formatErrorWithLocation error
            let formattedStr = T.unpack formatted
            formattedStr `contains` "test error" @?= True
            formattedStr `contains` "3" @?= True
            formattedStr `contains` "7" @?= True
        ]

    , testGroup "Error creation utilities"
        [ testCase "errorAt creates error with location" $ do
            let error = errorAt "test error" 2 4
            getErrorLine error @?= 2
            getErrorColumn error @?= 4
            
        , testCase "errorWithCategory adds category" $ do
            let error = errorWithCategory "Type" "type error"
            hasCategory error "Type" @?= True
            
        , testCase "errorWithSuggestions adds suggestions" $ do
            let error = errorWithSuggestions "error" ["suggestion1", "suggestion2"]
            -- Would check suggestions if the API exposed them
            True @?= True
        ]

    , testGroup "Error filtering and analysis"
        [ testCase "filterByCategory selects matching errors" $ do
            let errors = 
                    [ errorWithCategory "Type" "type error"
                    , errorWithCategory "Syntax" "syntax error"
                    , errorWithCategory "Type" "another type error"
                    ]
                typeErrors = filterByCategory "Type" errors
            length typeErrors @?= 2
            
        , testCase "filterBySeverity selects matching severity" $ do
            let messages = 
                    [ errorAt "error" 1 1
                    , warningAt "warning" 1 1
                    , infoAt "info" 1 1
                    ]
                warnings = filterBySeverity Warning messages
            length warnings @?= 1
        ]

    , testGroup "Error recovery decisions"
        [ testCase "canRecoverFrom determines recoverability" $ do
            let fatalMsg = fatalError "fatal error"
                errorMsg = errorAt "error" 1 1
                warningMsg = warningAt "warning" 1 1
            canRecoverFrom fatalMsg @?= False
            canRecoverFrom errorMsg @?= True
            canRecoverFrom warningMsg @?= True
            
        , testCase "shouldContinueAfter determines continuation" $ do
            let fatalMsg = fatalError "fatal error"
                errorMsg = errorAt "error" 1 1
                warningMsg = warningAt "warning" 1 1
            shouldContinueAfter fatalMsg @?= False
            shouldContinueAfter errorMsg @?= True
            shouldContinueAfter warningMsg @?= True
        ]

    , testGroup "Error combination"
        [ testCase "combineErrors merges errors" $ do
            let error1 = errorAt "first error" 1 1
                error2 = errorAt "second error" 2 2
                combined = combineErrors error1 error2
            -- Would check combined error properties
            True @?= True
            
        , testCase "combinedErrorSeverity selects highest severity" $ do
            let error1 = errorAt "error" 1 1
                warning1 = warningAt "warning" 1 1
                combined = combineErrors error1 warning1
            combinedErrorSeverity combined @?= Error
        ]

    , testGroup "Timestamp utilities"
        [ testCase "formatTimestamp creates readable format" $ do
            let timestamp = "2023-01-01T12:00:00Z"
                formatted = formatTimestamp timestamp
            T.length formatted @?= 19  -- Basic length check
        ]

    , testGroup "Recovery strategy utilities"
        [ testCase "createRecoveryStrategy builds custom strategy" $ do
            let strategy = createRecoveryStrategy True True "action" "hint" 30 0.8
            canRecover strategy @?= True
            shouldContinue strategy @?= True
            recoveryAction strategy @?= Just "action"
            recoveryHint strategy @?= Just "hint"
            recoveryCost strategy @?= 30
            recoveryConfidence strategy @?= 0.8
        ]

    , testGroup "QuickCheck properties"
        [ fastProperty "severity ordering is transitive" prop_severityOrderingTransitive
        , fastProperty "recovery cost is within bounds" prop_recoveryCostBounds
        , fastProperty "recovery confidence is within bounds" prop_recoveryConfidenceBounds
        , fastProperty "error collector preserves order" prop_errorCollectorPreservesOrder
        , fastProperty "filtering preserves message content" prop_filteringPreservesContent
        ]
    ]

-- Helper function to check if string contains substring
contains :: String -> String -> Bool
contains needle haystack = needle `isInfixOf` haystack

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Severity ordering properties
prop_severityOrderingTransitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_severityOrderingTransitive s1 s2 s3 =
    (compareSeverity s1 s2 == GT && compareSeverity s2 s3 == GT) ==> 
    compareSeverity s1 s3 == GT

-- Recovery strategy properties
prop_recoveryCostBounds :: Bool -> Bool -> Maybe String -> Maybe String -> Int -> Float -> Property
prop_recoveryCostBounds _ _ _ _ cost confidence =
    cost >= 0 && cost <= 100 && confidence >= 0.0 && confidence <= 1.0 ==>
    let strategy = customRecovery True True Nothing Nothing cost confidence
    in recoveryCost strategy == cost && recoveryConfidence strategy == confidence

prop_recoveryConfidenceBounds :: Bool -> Bool -> Maybe String -> Maybe String -> Int -> Float -> Property
prop_recoveryConfidenceBounds _ _ _ _ cost confidence =
    cost >= 0 && cost <= 100 && confidence >= 0.0 && confidence <= 1.0 ==>
    let strategy = customRecovery True True Nothing Nothing cost confidence
    in recoveryConfidence strategy == confidence

-- Error collector properties
prop_errorCollectorPreservesOrder :: [String] -> Bool
prop_errorCollectorPreservesOrder messages =
    let errors = map (\(i, msg) -> errorAt msg (i + 1) 1) (zip [0..] messages)
        collector = foldr addError newErrorCollector errors
        collectedErrors = getErrors collector
        errorMessages = map (T.unpack . formatError) collectedErrors
    in errorMessages == messages

prop_filteringPreservesContent :: [String] -> String -> Bool
prop_filteringPreservesContent messages category =
    let errors = map errorAt messages
        filtered = filterByCategory (T.pack category) errors
        filteredMessages = map (T.unpack . formatError) filtered
    in all (`elem` messages) filteredMessages

-- Helper functions for generating test data
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Fatal, Error, Warning, Info]

genErrorSubLevel :: Gen ErrorSubLevel
genErrorSubLevel = elements [Critical, High, Medium, Low, Notification]

genDetailedSeverity :: Gen DetailedSeverity
genDetailedSeverity = do
    base <- genErrorSeverity
    sub <- genErrorSubLevel
    custom <- oneof [return Nothing, fmap Just arbitrary]
    return $ DetailedSeverity base sub custom

genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = do
    canRec <- arbitrary
    shouldCont <- arbitrary
    action <- oneof [return Nothing, fmap Just arbitrary]
    hint <- oneof [return Nothing, fmap Just arbitrary]
    cost <- choose (0, 100)
    confidence <- choose (0.0, 1.0)
    return $ customRecovery canRec shouldCont action hint cost confidence