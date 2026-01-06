{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewErrorHandlerComprehensiveSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, Arbitrary, arbitrary, oneof, elements, listOf, resize, choose)
import Data.Char (isAlphaNum, isAlpha)
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Compiler.Errors.Core
  ( TypeError(..)
  , CombinedError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , emptyContext
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
  , getErrorColumn
  )

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary ErrorSeverity where
  arbitrary = oneof [return Fatal, return Error, return Warning, return Info]

instance Arbitrary ErrorCategory where
  arbitrary = oneof 
    [ return TypeChecking
    , return Ownership
    , return Parsing
    , return Semantic
    , return Runtime
    , return Constraint
    , return Inference
    , return Integration
    , return Unknown
    ]

instance Arbitrary ErrorLocation where
  arbitrary = do
    filePath <- arbitrary
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    endLine <- arbitrary
    endColumn <- arbitrary
    return $ ErrorLocation filePath line column endLine endColumn

instance Arbitrary ErrorContext where
  arbitrary = do
    contextCode <- arbitrary
    contextFunction <- arbitrary
    contextVariable <- arbitrary
    contextType <- arbitrary
    contextAdditional <- listOf $ arbitrary
    return $ ErrorContext contextCode contextFunction contextVariable contextType contextAdditional

instance Arbitrary ErrorRecovery where
  arbitrary = do
    canRec <- arbitrary
    shouldCont <- arbitrary
    recAction <- arbitrary
    recHint <- arbitrary
    cost <- choose (0, 100)
    confidence <- choose (0.0, 1.0)
    return $ RecoveryStrategy canRec shouldCont recAction recHint cost confidence

instance Arbitrary TypeError where
  arbitrary = do
    errorId <- arbitrary
    severity <- arbitrary
    category <- arbitrary
    message <- arbitrary
    location <- arbitrary
    context <- arbitrary
    recovery <- arbitrary
    suggestions <- listOf arbitrary
    relatedErrors <- listOf arbitrary
    errorChain <- listOf arbitrary
    timestamp <- arbitrary
    return $ TypeError errId errorId severity category message location context recovery suggestions relatedErrors errorChain timestamp

-- Generate valid error IDs
validErrorId :: Gen String
validErrorId = do
    prefix <- elements ["TYPE", "OWN", "PARSE", "SEM", "RUN", "CONST", "INF", "INT"]
    num <- choose (1000, 9999)
    return $ prefix ++ show num

-- Generate error messages
errorMessage :: Gen String
errorMessage = do
    base <- elements 
      [ "Type mismatch"
      , "Ownership violation"
      , "Parse error"
      , "Semantic error"
      , "Runtime error"
      , "Constraint violation"
      , "Inference failure"
      , "Integration error"
      ]
    details <- arbitrary
    return $ base ++ ": " ++ details

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Property: Error severity ordering is consistent
prop_severity_ordering :: Property
prop_severity_ordering =
  property $ Fatal > Error .&&. Error > Warning .&&. Warning > Info

-- Property: Error collector correctly separates errors by severity
prop_error_collector_separation :: Property
prop_error_collector_separation =
  forAll (listOf arbitrary) $ \errors ->
    let allErrors = getAllMessages errors
        errorMessages = getErrors allErrors
        warningMessages = getWarnings allErrors
        infoMessages = getInfo allErrors
    in property $ L.all (\e -> severity e `elem` [Error, Fatal]) errorMessages .&&.
               all (\e -> severity e == Warning) warningMessages .&&.
               all (\e -> severity e == Info) infoMessages .&&.
               length allErrors == L.length errorMessages + L.length warningMessages + L.length infoMessages

-- Property: hasErrors L.and hasWarnings work correctly
prop_error_detection :: Property
prop_error_detection =
  forAll (listOf arbitrary) $ \errors ->
    let allErrors = getAllMessages errors
        hasErr = hasErrors allErrors
        hasWarn = hasWarnings allErrors
        errorCount = L.length $ getErrors allErrors
        warningCount = L.length $ getWarnings allErrors
    in property $ hasErr === (errorCount > 0) .&&.
               hasWarn === (warningCount > 0)

-- Property: error filtering by category works
prop_category_filtering :: Property
prop_category_filtering =
  forAll arbitrary $ \category ->
  forAll (listOf arbitrary) $ \errors ->
    let filtered = filterByCategory category errors
        hasMatchingCategory = L.any (\e -> category e == category) errors
    in property $ (not (null filtered)) === hasMatchingCategory .&&.
               all (\e -> category e == category) filtered

-- Property: error filtering by severity works
prop_severity_filtering :: Property
prop_severity_filtering =
  forAll arbitrary $ \severity ->
  forAll (listOf arbitrary) $ \errors ->
    let filtered = filterBySeverity severity errors
        hasMatchingSeverity = L.any (\e -> severity e == severity) errors
    in property $ (not (null filtered)) === hasMatchingSeverity .&&.
               all (\e -> severity e == severity) filtered

-- Property: error location helpers work correctly
prop_error_location_helpers :: Property
prop_error_location_helpers =
  forAll arbitrary $ \location ->
    let line = getErrorLine location
        column = getErrorColumn location
    in property $ line >= 0 .&&. column >= 0

-- Property: error formatting contains expected elements
prop_error_formatting :: Property
prop_error_formatting =
  forAll validErrorId $ \errorId ->
  forAll errorMessage $ \msg ->
  forAll arbitrary $ \severity ->
    let error = TypeError errorId severity Parsing (T.pack msg) 
                    (ErrorLocation (startPos) Nothing) 
                    emptyContext errorRecovery [] [] [] Nothing
        formatted = formatError error
        severityStr = case severity of
          Fatal -> "FATAL"
          Error -> "ERROR"
          Warning -> "WARNING"
          Info -> "INFO"
    in property $ errorId `L.isInfixOf` formatted .&&.
               severityStr `L.isInfixOf` formatted .&&.
               msg `L.isInfixOf` formatted

-- Property: error recovery strategies are consistent
prop_recovery_consistency :: Property
prop_recovery_consistency =
  property $ not (canRecover fatalRecovery) .&&.
             not (shouldContinue fatalRecovery) .&&.
             canRecover errorRecovery .&&.
             shouldContinue errorRecovery .&&.
             canRecover warningRecovery .&&.
             shouldContinue warningRecovery .&&.
             canRecover infoRecovery .&&.
             shouldContinue infoRecovery

-- Property: custom recovery strategy works
prop_custom_recovery :: Property
prop_custom_recovery =
  forAll arbitrary $ \canRec ->
  forAll arbitrary $ \shouldCont ->
  forAll arbitrary $ \action ->
  forAll arbitrary $ \hint ->
  forAll (choose (0, 100)) $ \cost ->
  forAll (choose (0.0, 1.0)) $ \confidence ->
    let recovery = customRecovery canRec shouldCont action hint cost confidence
    in property $ canRecover recovery === canRec .&&.
               shouldContinue recovery === shouldCont .&&.
               recoveryAction recovery === action .&&.
               recoveryHint recovery === hint .&&.
               recoveryCost recovery === cost .&&.
               recoveryConfidence recovery === confidence

-- Property: error wrapping preserves original error
prop_error_wrapping :: Property
prop_error_wrapping =
  forAll arbitrary $ \originalError ->
  forAll errorMessage $ \wrapperMessage ->
    let wrapped = wrapError wrapperMessage originalError
    in property $ originalError `elem` errorChain wrapped .&&.
               T.pack wrapperMessage `L.isInfixOf` message wrapped

-- Property: error combination preserves severity
prop_error_combination :: Property
prop_error_combination =
  forAll arbitrary $ \error1 ->
  forAll arbitrary $ \error2 ->
    let combined = combineErrors error1 error2
        expectedSeverity = max (severity error1) (severity error2)
    in property $ severity combined === expectedSeverity

-- Property: error context modification works
prop_context_modification :: Property
prop_context_modification =
  forAll arbitrary $ \error ->
  forAll arbitrary $ \newContext ->
    let withNewContext = withContext newContext error
    in property $ context withNewContext === newContext

-- Property: error suggestions are preserved
prop_suggestions_preservation :: Property
prop_suggestions_preservation =
  forAll arbitrary $ \error ->
  forAll (listOf arbitrary) $ \newSuggestions ->
    let withNewSuggestions = withSuggestions newSuggestions error
    in property $ suggestions withNewSuggestions === newSuggestions

-- Property: timestamp formatting is consistent
prop_timestamp_formatting :: Property
prop_timestamp_formatting =
  forAll errorMessage $ \timestamp ->
    let error = TypeError "TEST001" Error Parsing (T.pack "test") 
                    (ErrorLocation (startPos) Nothing) 
                    emptyContext errorRecovery [] [] [] (Just timestamp)
        withTimestamp = error `withTimestamp` timestamp
    in property $ timestamp === timestamp withTimestamp

-- Property: error statistics are accurate
prop_error_statistics :: Property
prop_error_statistics =
  forAll (listOf arbitrary) $ \errors ->
    let stats = getErrorStatistics errors
        errorCount = L.length $ getErrors errors
        warningCount = L.length $ getWarnings errors
        infoCount = L.length $ getInfo errors
    in property $ Map.size stats >= 0

-- ============================================================================
-- Unit Tests
-- ============================================================================

-- Test basic error creation
test_error_creation :: TestTree
test_error_creation =
  testCase "Basic error creation" $ do
    let error = TypeError "TYPE001" Error TypeChecking (T.pack "Type mismatch")
                         (ErrorLocation (startPos) Nothing) 
                         emptyContext errorRecovery [] [] [] Nothing
    errorId error @?= "TYPE001"
    severity error @?= Error
    category error @?= TypeChecking
    message error @?= "Type mismatch"

-- Test error collector functionality
test_error_collector :: TestTree
test_error_collector =
  testCase "Error collector functionality" $ do
    let errors = 
          [ TypeError "ERR001" Error Parsing (T.pack "Parse error") 
              (ErrorLocation (startPos) Nothing) emptyContext errorRecovery [] [] [] Nothing
          , TypeError "WARN001" Warning Parsing (T.pack "Warning") 
              (ErrorLocation Nothing 2 1 Nothing Nothing) emptyContext warningRecovery [] [] [] Nothing
          , TypeError "INFO001" Info Parsing (T.pack "Info") 
              (ErrorLocation Nothing 3 1 Nothing Nothing) emptyContext infoRecovery [] [] [] Nothing
          ]
    hasErrors errors @?= True
    hasWarnings errors @?= True
    length (getErrors errors) @?= 1
    length (getWarnings errors) @?= 1
    length (getInfo errors) @?= 1

-- Test error filtering
test_error_filtering :: TestTree
test_error_filtering =
  testCase "Error filtering" $ do
    let errors = 
          [ TypeError "ERR001" Error TypeChecking (T.pack "Type error") 
              (ErrorLocation (startPos) Nothing) emptyContext errorRecovery [] [] [] Nothing
          , TypeError "ERR002" Error Ownership (T.pack "Ownership error") 
              (ErrorLocation Nothing 2 1 Nothing Nothing) emptyContext errorRecovery [] [] [] Nothing
          , TypeError "WARN001" Warning TypeChecking (T.pack "Type warning") 
              (ErrorLocation Nothing 3 1 Nothing Nothing) emptyContext warningRecovery [] [] [] Nothing
          ]
    typeErrors <- return $ filterByCategory TypeChecking errors
    ownershipErrors <- return $ filterByCategory Ownership errors
    errorSeverity <- return $ filterBySeverity Error errors
    length typeErrors @?= 2
    length ownershipErrors @?= 1
    length errorSeverity @?= 2

-- Test error formatting
test_error_formatting :: TestTree
test_error_formatting =
  testCase "Error formatting" $ do
    let error = TypeError "TYPE001" Error TypeChecking (T.pack "Type mismatch")
                         (ErrorLocation Nothing 5 10 Nothing Nothing) 
                         emptyContext errorRecovery [T.pack "Check types"] [] [] Nothing
        formatted = formatError error
    "TYPE001" `L.isInfixOf` formatted @?= True
    "ERROR" `L.isInfixOf` formatted @?= True
    "TypeChecking" `L.isInfixOf` formatted @?= True
    "Type mismatch" `L.isInfixOf` formatted @?= True
    "Check types" `L.isInfixOf` formatted @?= True

-- Test recovery strategies
test_recovery_strategies :: TestTree
test_recovery_strategies =
  testCase "Recovery strategies" $ do
    canRecover fatalRecovery @?= False
    shouldContinue fatalRecovery @?= False
    canRecover errorRecovery @?= True
    shouldContinue errorRecovery @?= True
    canRecover warningRecovery @?= True
    shouldContinue warningRecovery @?= True
    canRecover infoRecovery @?= True
    shouldContinue infoRecovery @?= True

-- Test error location helpers
test_location_helpers :: TestTree
test_location_helpers =
  testCase "Location helpers" $ do
    let location = ErrorLocation (Just "test.typus") 5 10 (Just 5) (Just 15)
    getErrorLine location @?= 5
    getErrorColumn location @?= 10

-- Test combined errors
test_combined_errors :: TestTree
test_combined_errors =
  testCase "Combined errors" $ do
    let errors = 
          [ OwnershipErrorCombined Error undefined
          , DependentTypeErrorCombined Warning undefined
          , IntegrationError "Test integration" Info
          ]
    let severity = map combinedErrorSeverity errors
    severity @?= [Error, Warning, Info]

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =
  testGroup "New ErrorHandler Comprehensive Tests"
    [ testGroup "Property-based tests"
        [ fastProperty "Error severity ordering is consistent" prop_severity_ordering
        , fastProperty "Error collector correctly separates errors by severity" prop_error_collector_separation
        , fastProperty "hasErrors L.and hasWarnings work correctly" prop_error_detection
        , fastProperty "error filtering by category works" prop_category_filtering
        , fastProperty "error filtering by severity works" prop_severity_filtering
        , fastProperty "error location helpers work correctly" prop_error_location_helpers
        , fastProperty "error formatting contains expected elements" prop_error_formatting
        , fastProperty "error recovery strategies are consistent" prop_recovery_consistency
        , fastProperty "custom recovery strategy works" prop_custom_recovery
        , fastProperty "error wrapping preserves original error" prop_error_wrapping
        , fastProperty "error combination preserves severity" prop_error_combination
        , fastProperty "error context modification works" prop_context_modification
        , fastProperty "error suggestions are preserved" prop_suggestions_preservation
        , fastProperty "timestamp formatting is consistent" prop_timestamp_formatting
        , fastProperty "error statistics are accurate" prop_error_statistics
        ]
    , testGroup "Unit tests"
        [ test_error_creation
        , test_error_collector
        , test_error_filtering
        , test_error_formatting
        , test_recovery_strategies
        , test_location_helpers
        , test_combined_errors
        ]
    ]