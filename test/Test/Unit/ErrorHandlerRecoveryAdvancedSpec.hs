{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.ErrorHandlerRecoveryAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, oneof, listOf)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)

import Compiler.Errors.Core
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
  , canRecoverFrom
  , shouldContinueAfter
  , formatErrors
  )

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import Data.Time (UTCTime)
import Data.List (sort)

-- ============================================================================
-- Test Generators
-- ============================================================================

instance Arbitrary ErrorSeverity where
  arbitrary = oneof [return Error, return Warning, return Info]

instance Arbitrary ErrorCategory where
  arbitrary = oneof 
    [ return SyntaxError
    , return TypeError
    , return NameError
    , return OwnershipError
    , return DependentTypeError
    , return InternalError
    ]

instance Arbitrary ErrorLocation where
  arbitrary = oneof
    [ NoLocation <$ return ()
    , SourceLocation <$> arbitrary
    , SpanLocation <$> arbitrary
    ]

instance Arbitrary ErrorContext where
  arbitrary = do
    function <- listOf $ choose ('a', 'z')
    module' <- listOf $ choose ('a', 'z')
    description <- listOf $ choose ('a', 'z')
    return $ ErrorContext function module' description

instance Arbitrary ErrorRecovery where
  arbitrary = oneof
    [ return CannotRecover
    , return SkipCurrentStatement
    , return SkipCurrentBlock
    , return ContinueWithWarning
    , return AttemptCorrection
    ]

instance Arbitrary TypeError where
  arbitrary = do
    severity <- arbitrary
    category <- arbitrary
    location <- arbitrary
    context <- arbitrary
    message <- listOf $ choose ('a', 'z')
    suggestion <- oneof [return Nothing, Just <$> listOf (choose ('a', 'z'))]
    recovery <- arbitrary
    return $ TypeError errId severity category location context message suggestion recovery instance Arbitrary CombinedError where
  arbitrary = do
    primary <- arbitrary
    related <- listOf arbitrary
    return $ CombinedError primary related

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

-- Property: Error recovery should be deterministic for same error type
propErrorRecoveryDeterministic :: TypeError -> TypeError -> Bool
propErrorRecoveryDeterministic err1 err2 =
  let recovery1 = canRecoverFrom err1
      recovery2 = canRecoverFrom err2
  in errCategory err1 == errCategory err2 ==> recovery1 == recovery2

-- Property: Syntax errors should generally be recoverable
propSyntaxErrorsRecoverable :: ErrorLocation -> ErrorContext -> String -> Bool
propSyntaxErrorsRecoverable location context message =
  let syntaxError = TypeError Error SyntaxError location context message Nothing CannotRecover
  in canRecoverFrom syntaxError

-- Property: Internal errors should generally not be recoverable
propInternalErrorsNotRecoverable :: ErrorLocation -> ErrorContext -> String -> Bool
propInternalErrorsNotRecoverable location context message =
  let internalError = TypeError Error InternalError location context message Nothing CannotRecover
  in not (canRecoverFrom internalError)

-- Property: Should continue after warnings but not after errors
propContinueAfterWarningsNotErrors :: ErrorSeverity -> ErrorLocation -> ErrorContext -> String -> Bool
propContinueAfterWarningsNotErrors severity location context message =
  let err = TypeError severity SyntaxError location context message Nothing CannotRecover
  in shouldContinueAfter err == (severity == Warning || severity == Info)

-- Property: Error collector should maintain order of added errors
propErrorCollectorMaintainsOrder :: [TypeError] -> Bool
propErrorCollectorMaintainsOrder errors =
  let collector = newErrorCollector
      collector' = L.foldl (\acc err -> addError err acc) collector errors
      retrievedErrors = getErrors collector'
  in map errMessage retrievedErrors == map errMessage errors

-- Property: Error collector should separate errors L.and warnings correctly
propErrorCollectorSeparatesBySeverity :: [TypeError] -> Bool
propErrorCollectorSeparatesBySeverity errors =
  let collector = newErrorCollector
      collector' = L.foldl (\acc err -> 
            case errSeverity err of
              Error -> addError err acc
              Warning -> addWarning err acc
              Info -> addInfo err acc
          ) collector errors
      retrievedErrors = getErrors collector'
      retrievedWarnings = getWarnings collector'
      retrievedInfo = getInfo collector'
      
      errorMessages = map errMessage retrievedErrors
      warningMessages = map errMessage retrievedWarnings
      infoMessages = map errMessage retrievedInfo
      
      originalErrors = L.filter (\e -> errSeverity e == Error) errors
      originalWarnings = L.filter (\e -> errSeverity e == Warning) errors
      originalInfo = L.filter (\e -> errSeverity e == Info) errors
  in L.length errorMessages == L.length originalErrors &&
     L.length warningMessages == L.length originalWarnings &&
     L.length infoMessages == L.length originalInfo

-- Property: Combined error should preserve primary error severity
propCombinedErrorPreservesPrimary :: TypeError -> [TypeError] -> Bool
propCombinedErrorPreservesPrimary primary related =
  let combined = CombinedError primary related
  in errSeverity (cePrimary combined) == errSeverity primary

-- Property: Error formatting should never crash
propErrorFormattingNeverCrashes :: TypeError -> Bool
propErrorFormattingNeverCrashes err =
  let formatted = formatError err
  in L.length formatted >= 0  -- Should never crash

-- Property: Multiple errors formatting should never crash
propMultipleErrorsFormattingNeverCrashes :: [TypeError] -> Bool
propMultipleErrorsFormattingNeverCrashes errors =
  let formatted = formatErrors errors
  in L.length formatted >= 0  -- Should never crash

-- ============================================================================
-- Unit Tests
-- ============================================================================

-- Test error recovery strategies
testErrorRecoveryStrategies :: TestTree
testErrorRecoveryStrategies = testCase "Error recovery strategies" $ do
  let syntaxErr = TypeError Error SyntaxError NoLocation emptyContext "syntax error" Nothing SkipCurrentStatement
  let typeErr = TypeError Error TypeError NoLocation emptyContext "type error" Nothing CannotRecover
  let warning = TypeError Warning SyntaxError NoLocation emptyContext "warning" Nothing ContinueWithWarning
  
  assertBool "Should recover from syntax errors" (canRecoverFrom syntaxErr)
  assertBool "Should not recover from type errors" (not (canRecoverFrom typeErr))
  assertBool "Should recover from warnings" (canRecoverFrom warning)
  
  assertBool "Should continue after warnings" (shouldContinueAfter warning)
  assertBool "Should attempt recovery after syntax errors" (shouldContinueAfter syntaxErr)

-- Test error collector functionality
testErrorCollectorFunctionality :: TestTree
testErrorCollectorFunctionality = testCase "Error collector functionality" $ do
  let collector = newErrorCollector
  let err1 = TypeError Error SyntaxError NoLocation emptyContext "error1" Nothing CannotRecover
  let err2 = TypeError Warning SyntaxError NoLocation emptyContext "warning1" Nothing ContinueWithWarning
  let err3 = TypeError Info SyntaxError NoLocation emptyContext "info1" Nothing ContinueWithWarning
  
  let collector1 = addError err1 collector
  let collector2 = addWarning err2 collector1
  let collector3 = addInfo err3 collector2
  
  assertBool "Should have errors" (hasErrors collector3)
  assertBool "Should have warnings" (hasWarnings collector3)
  
  let errors = getErrors collector3
  let warnings = getWarnings collector3
  let infos = getInfo collector3
  
  assertEqual "Should have 1 error" 1 (L.length errors)
  assertEqual "Should have 1 warning" 1 (L.length warnings)
  assertEqual "Should have 1 info" 1 (L.length infos)

-- Test error context functionality
testErrorContextFunctionality :: TestTree
testErrorContextFunctionality = testCase "Error context functionality" $ do
  let context = ErrorContext "main" "TestModule" "Test description"
  assertEqual "Context function should be preserved" "main" (ecFunction context)
  assertEqual "Context module should be preserved" "TestModule" (ecModule context)
  assertEqual "Context description should be preserved" "Test description" (ecDescription context)

-- Test error at position
testErrorAtPosition :: TestTree
testErrorAtPosition = testCase "Error at position" $ do
  let pos = SourcePos 10 5
  let err = errorAt "test-id" (errSeverity err)
  assertEqual "Error should have correct category" SyntaxError (errCategory err)
  assertEqual "Error should have correct message" "test error" (errMessage err)
  
  case errLocation err of
    SourceLocation loc -> assertEqual "Error should be at correct position" pos loc
    _ -> assertBool "Error should have source location" False

-- Test warning at position
testWarningAtPosition :: TestTree
testWarningAtPosition = testCase "Warning at position" $ do
  let pos = SourcePos 15 3
  let warning = warningAt "test-id" (errSeverity warning)
  assertEqual "Warning should have correct category" SyntaxError (errCategory warning)
  assertEqual "Warning should have correct message" "test warning" (errMessage warning)

-- Test info at position
testInfoAtPosition :: TestTree
testInfoAtPosition = testCase "Info at position" $ do
  let pos = SourcePos 20 8
  let info = infoAt "test-id" (errSeverity info)
  assertEqual "Info should have correct category" SyntaxError (errCategory info)
  assertEqual "Info should have correct message" "test info" (errMessage info)

-- Test combined error handling
testCombinedErrorHandling :: TestTree
testCombinedErrorHandling = testCase "Combined error handling" $ do
  let primary = TypeError Error SyntaxError NoLocation emptyContext "primary" Nothing CannotRecover
  let related1 = TypeError Warning SyntaxError NoLocation emptyContext "related1" Nothing ContinueWithWarning
  let related2 = TypeError Info SyntaxError NoLocation emptyContext "related2" Nothing ContinueWithWarning
  
  let combined = CombinedError primary [related1, related2]
  
  assertEqual "Combined error should preserve primary" primary (cePrimary combined)
  assertEqual "Combined error should preserve related count" 2 (L.length (ceRelated combined))

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Error Handler Recovery Advanced Tests"
  [ -- QuickCheck properties
    testProperty "Error recovery is deterministic" propErrorRecoveryDeterministic
  , testProperty "Syntax errors are recoverable" propSyntaxErrorsRecoverable
  , testProperty "Internal errors are not recoverable" propInternalErrorsNotRecoverable
  , testProperty "Continue after warnings not errors" propContinueAfterWarningsNotErrors
  , testProperty "Error collector maintains order" propErrorCollectorMaintainsOrder
  , testProperty "Error collector separates by severity" propErrorCollectorSeparatesBySeverity
  , testProperty "Combined error preserves primary" propCombinedErrorPreservesPrimary
  , testProperty "Error formatting never crashes" propErrorFormattingNeverCrashes
  , testProperty "Multiple errors formatting never crashes" propMultipleErrorsFormattingNeverCrashes
  
    -- Unit tests
  , testErrorRecoveryStrategies
  , testErrorCollectorFunctionality
  , testErrorContextFunctionality
  , testErrorAtPosition
  , testWarningAtPosition
  , testInfoAtPosition
  , testCombinedErrorHandling
  ]