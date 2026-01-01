module Test.Unit.NewQuickCheckTestSuite4Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.QuickCheck (Property, (==>), forAll, Gen, arbitrary, choose, oneof, elements)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
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
        [ testCase "detailedSeverityPriority combines base L.and sub-level" $ do
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
            L.length (getErrors collector) @?= 0
            L.length (getWarnings collector) @?= 0
            
        , testCase "addError adds error to collector" $ do
            let collector = newErrorCollector
                error = errorAt "test-id" (getErrors collector') @?= 1
            
        , testCase "addWarning adds warning to collector" $ do
            let collector = newErrorCollector
                warning = warningAt "test-id" (getWarnings collector') @?= 1
            
        , testCase "getAllMessages includes L.all message types" $ do
            let collector = newErrorCollector
                error = errorAt "test-id" == GT) ==> 
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
    let errors = L.map (\(i, msg) -> errorAt "test-id" + 1) 1) (zip [0..] messages)
        collector = foldr addError newErrorCollector errors
        collectedErrors = getErrors collector
        errorMessages = L.map (T.unpack . formatError) collectedErrors
    in errorMessages == messages

prop_filteringPreservesContent :: [String] -> String -> Bool
prop_filteringPreservesContent messages category =
    let errors = map errorAt "test-id" (T.pack category) errors
        filteredMessages = L.map (T.unpack . formatError) filtered
    in L.all (`elem` messages) filteredMessages

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