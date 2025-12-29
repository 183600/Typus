module Test.Unit.NewCabalErrorHandlerQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, oneof, elements, listOf, suchThat)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing)
import qualified Data.Map.Strict as Map
import Control.Monad.State (evalState)

import Compiler.Errors.Core
import TestSupport.QuickCheck (fastProperty)

-- | QuickCheck tests for ErrorHandler module error handling functions
tests :: TestTree
tests =
  testGroup "New Cabal ErrorHandler QuickCheck Tests"
    [ testProperty "severityPriority respects ordering Fatal > Error > Warning > Info" prop_severityPriorityOrdering
    , testProperty "compareSeverity respects priority ordering" prop_compareSeverityCorrectness
    , testProperty "isAtLeast correctly checks severity minimum" prop_isAtLeastCorrectness
    , testProperty "detailedSeverityPriority combines base and sub-level priorities" prop_detailedSeverityPriorityCorrectness
    , testProperty "ErrorLocation construction works correctly" prop_errorLocationConstruction
    , testProperty "ErrorContext emptyContext has all Nothing fields" prop_emptyContextCorrectness
    , testProperty "ErrorRecovery strategies have consistent properties" prop_errorRecoveryConsistency
    , testProperty "customRecovery creates valid recovery strategies" prop_customRecoveryValidity
    , testProperty "TypeError construction preserves all fields" prop_typeErrorConstruction
    , testProperty "CombinedError severity extraction works" prop_combinedErrorSeverityExtraction
    , testProperty "filterCombinedErrorsBySeverity filters correctly" prop_filterCombinedErrorsCorrectness
    , testProperty "ErrorCollector addError increases error count" prop_errorCollectorAddError
    , testProperty "ErrorCollector addWarning increases warning count" prop_errorCollectorAddWarning
    , testProperty "hasErrors correctly detects error presence" prop_hasErrorsCorrectness
    , testProperty "hasWarnings correctly detects warning presence" prop_hasWarningsCorrectness
    , testProperty "filterBySeverity filters errors by severity" prop_filterBySeverityCorrectness
    , testProperty "filterByCategory filters errors by category" prop_filterByCategoryCorrectness
    , testGroup "Edge cases"
        [ testCase "severityPriority returns highest for Fatal" $
            severityPriority Fatal @?= 100
        , testCase "severityPriority returns lowest for Info" $
            severityPriority Info @?= 10
        , testCase "isAtLeast with same severity returns True" $
            isAtLeast Error Error @?= True
        , testCase "isAtLeast with higher minimum returns False" $
            isAtLeast Fatal Info @?= False
        , testCase "emptyContext has all Nothing fields" $ do
            emptyContext @?= ErrorContext Nothing Nothing Nothing Nothing []
        , testCase "fatalRecovery cannot recover" $ do
            canRecover fatalRecovery @?= False
            shouldContinue fatalRecovery @?= False
        , testCase "errorRecovery can recover" $ do
            canRecover errorRecovery @?= True
            shouldContinue errorRecovery @?= True
        , testCase "warningRecovery can recover" $ do
            canRecover warningRecovery @?= True
            shouldContinue warningRecovery @?= True
        , testCase "infoRecovery can recover" $ do
            canRecover infoRecovery @?= True
            shouldContinue infoRecovery @?= True
        ]
    ]

-- | Property: severityPriority respects ordering Fatal > Error > Warning > Info
prop_severityPriorityOrdering :: Property
prop_severityPriorityOrdering = 
  severityPriority Fatal > severityPriority Error .&&.
  severityPriority Error > severityPriority Warning .&&.
  severityPriority Warning > severityPriority Info

-- | Property: compareSeverity respects priority ordering
prop_compareSeverityCorrectness :: ErrorSeverity -> ErrorSeverity -> Property
prop_compareSeverityCorrectness sev1 sev2 = 
  compareSeverity sev1 sev2 === compare (severityPriority sev1) (severityPriority sev2)

-- | Property: isAtLeast correctly checks severity minimum
prop_isAtLeastCorrectness :: ErrorSeverity -> ErrorSeverity -> Property
prop_isAtLeastCorrectness minSeverity actualSeverity = 
  isAtLeast minSeverity actualSeverity === (severityPriority actualSeverity >= severityPriority minSeverity)

-- | Property: detailedSeverityPriority combines base and sub-level priorities
prop_detailedSeverityPriorityCorrectness :: ErrorSeverity -> ErrorSubLevel -> Property
prop_detailedSeverityPriorityCorrectness baseSev subLevel = 
  let detailed = DetailedSeverity baseSev subLevel Nothing
      expectedPriority = severityPriority baseSev + subLevelPriority subLevel
      subLevelPriority Critical = 50
      subLevelPriority High = 30
      subLevelPriority Medium = 15
      subLevelPriority Low = 5
      subLevelPriority Notification = 0
  in detailedSeverityPriority detailed === expectedPriority

-- | Property: ErrorLocation construction works correctly
prop_errorLocationConstruction :: Maybe String -> Int -> Int -> Maybe Int -> Maybe Int -> Property
prop_errorLocationConstruction filePath line column endLine endColumn = 
  line >= 0 && column >= 0 && 
  all (>=0) (maybeToList endLine) && 
  all (>=0) (maybeToList endColumn) ==>
  let location = ErrorLocation filePath line column endLine endColumn
  in ErrorLocation.filePath location === filePath .&&.
     ErrorLocation.line location === line .&&.
     ErrorLocation.column location === column .&&.
     ErrorLocation.endLine location === endLine .&&.
     ErrorLocation.endColumn location === endColumn

-- | Property: ErrorContext emptyContext has all Nothing fields
prop_emptyContextCorrectness :: Property
prop_emptyContextCorrectness = 
  let ctx = emptyContext
  in contextCode ctx === Nothing .&&.
     contextFunction ctx === Nothing .&&.
     contextVariable ctx === Nothing .&&.
     contextType ctx === Nothing .&&.
     contextAdditional ctx === []

-- | Property: ErrorRecovery strategies have consistent properties
prop_errorRecoveryConsistency :: ErrorRecovery -> Property
prop_errorRecoveryConsistency recovery = 
  let canRec = canRecover recovery
      shouldCont = shouldContinue recovery
      cost = recoveryCost recovery
      confidence = recoveryConfidence recovery
  in cost >= 0 && cost <= 100 .&&.
     confidence >= 0.0 && confidence <= 1.0 .&&.
     (if not canRec then not shouldCont else True)

-- | Property: customRecovery creates valid recovery strategies
prop_customRecoveryValidity :: Bool -> Bool -> Maybe String -> Maybe String -> Int -> Float -> Property
prop_customRecoveryValidity canRec shouldCont action hint cost confidence = 
  cost >= 0 && cost <= 100 && confidence >= 0.0 && confidence <= 1.0 ==>
  let recovery = customRecovery canRec shouldCont action hint cost confidence
  in canRecover recovery === canRec .&&.
     shouldContinue recovery === shouldCont .&&.
     recoveryCost recovery === cost .&&.
     recoveryConfidence recovery === confidence .&&.
     recoveryAction recovery === action .&&.
     recoveryHint recovery === hint

-- | Property: TypeError construction preserves all fields
prop_typeErrorConstruction :: String -> ErrorSeverity -> ErrorCategory -> String -> ErrorLocation -> ErrorContext -> ErrorRecovery -> [String] -> Property
prop_typeErrorConstruction errorId sev category message location context recovery suggestions = 
  let error = TypeError
        { errorId = errorId
        , severity = sev
        , category = category
        , message = T.pack message
        , location = location
        , context = context
        , recovery = recovery
        , suggestions = map T.pack suggestions
        , relatedErrors = []
        , errorChain = []
        , timestamp = Nothing
        }
  in TypeError.errorId error === errorId .&&.
     TypeError.severity error === sev .&&.
     TypeError.category error === category .&&.
     TypeError.message error === T.pack message .&&.
     TypeError.location error === location .&&.
     TypeError.context error === context .&&.
     TypeError.recovery error === recovery .&&.
     TypeError.suggestions error === map T.pack suggestions .&&.
     null (TypeError.relatedErrors error) .&&.
     null (TypeError.errorChain error) .&&.
     isNothing (TypeError.timestamp error)

-- | Property: CombinedError severity extraction works
prop_combinedErrorSeverityExtraction :: ErrorSeverity -> String -> Property
prop_combinedErrorSeverityExtraction sev message = 
  let integrationError = IntegrationError message sev
  in combinedErrorSeverity integrationError === sev

-- | Property: filterCombinedErrorsBySeverity filters correctly
prop_filterCombinedErrorsCorrectness :: ErrorSeverity -> [ErrorSeverity] -> Property
prop_filterCombinedErrorsCorrectness minSeverity severities = 
  let errors = map (\sev -> IntegrationError ("test" ++ show sev) sev) severities
      filtered = filterCombinedErrorsBySeverity minSeverity errors
      expected = filter (\sev -> isAtLeast minSeverity sev) severities
  in length filtered === length expected

-- | Property: ErrorCollector addError increases error count
prop_errorCollectorAddError :: TypeError -> Property
prop_errorCollectorAddError error = 
  let collector = addError error
      result = evalState collector []
  in length result === 1 .&&. head result === error

-- | Property: ErrorCollector addWarning increases warning count
prop_errorCollectorAddWarning :: TypeError -> Property
prop_errorCollectorAddWarning warning = 
  let collector = addWarning warning
      result = evalState collector []
  in length result === 1 .&&. head result === warning

-- | Property: hasErrors correctly detects error presence
prop_hasErrorsCorrectness :: [TypeError] -> Property
prop_hasErrorsCorrectness errors = 
  let hasErrs = hasErrors errors
      hasErrorSeverity = any (\e -> severity e == Error || severity e == Fatal) errors
  in hasErrs === hasErrorSeverity

-- | Property: hasWarnings correctly detects warning presence
prop_hasWarningsCorrectness :: [TypeError] -> Property
prop_hasWarningsCorrectness warnings = 
  let hasWarns = hasWarnings warnings
      hasWarningSeverity = any (\e -> severity e == Warning) warnings
  in hasWarns === hasWarningSeverity

-- | Property: filterBySeverity filters errors by severity
prop_filterBySeverityCorrectness :: ErrorSeverity -> [TypeError] -> Property
prop_filterBySeverityCorrectness targetSeverity errors = 
  let filtered = filterBySeverity targetSeverity errors
      expected = filter (\e -> severity e == targetSeverity) errors
  in length filtered === length expected

-- | Property: filterByCategory filters errors by category
prop_filterByCategoryCorrectness :: ErrorCategory -> [TypeError] -> Property
prop_filterByCategoryCorrectness targetCategory errors = 
  let filtered = filterByCategory targetCategory errors
      expected = filter (\e -> category e == targetCategory) errors
  in length filtered === length expected

-- Helper function to convert Maybe to list
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- Helper operator for composing properties
(.&&.) :: Property -> Property -> Property
(.&&.) = (&&)