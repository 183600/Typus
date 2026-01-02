{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CompilerErrorHandlingQuickCheckSpec (tests) where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Control.Monad.State (execState)

import Compiler.Errors.Core
import Compiler (CompilationPhase(..))
import SourceLocation (SourcePos(..), SourceSpan(..))

import qualified Data.Text as T
import Data.Maybe (isJust, isNothing)

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

-- Arbitrary instances for error types
instance Arbitrary ErrorSeverity where
  arbitrary = elements [Info, Warning, Error, Fatal]

instance Arbitrary ErrorCategory where
  arbitrary = elements 
    [ TypeChecking
    , Ownership
    , Parsing
    , Semantic
    , Runtime
    , Constraint
    , Inference
    , Integration
    , Unknown
    ]

instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SourceSpan where
  arbitrary = SourceSpan <$> arbitrary <*> arbitrary

instance Arbitrary ErrorLocation where
  arbitrary = ErrorLocation <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ErrorContext where
  arbitrary = do
    contextCode <- arbitrary
    contextFunction <- arbitrary
    contextVariable <- arbitrary
    contextType <- arbitrary
    contextAdditional <- arbitrary
    return $ ErrorContext contextCode contextFunction contextVariable contextType contextAdditional

instance Arbitrary TypeError where
  arbitrary = do
    errorId <- arbitrary
    severity <- arbitrary
    category <- arbitrary
    message <- arbitrary
    location <- arbitrary
    context <- arbitrary
    let recovery = errorRecovery
    suggestions <- arbitrary
    relatedErrors <- arbitrary
    errorChain <- arbitrary
    timestamp <- arbitrary
    return $ TypeError errorId severity category message location context recovery suggestions relatedErrors errorChain timestamp

instance Arbitrary CompilationPhase where
  arbitrary = elements
    [ ParsingPhase
    , TypeCheckingPhase
    , OwnershipAnalysisPhase
    , CodeGenerationPhase
    ]

-- Test properties
tests :: TestTree
tests = testGroup "Compiler Error Handling QuickCheck Tests"
  [ testProperty "Error severity ordering works correctly" testSeverityOrdering
  , testProperty "Error context can be created L.and retrieved" testErrorContext
  , testProperty "Error location formatting is consistent" testErrorLocationFormatting
  , testProperty "Error collector maintains error counts" testErrorCollector
  , testProperty "Error recovery decisions are consistent" testErrorRecovery
  , testProperty "Combined errors aggregate correctly" testCombinedErrors
  , testProperty "Error timestamps are valid" testErrorTimestamps
  ]

testSeverityOrdering :: ErrorSeverity -> ErrorSeverity -> Property
testSeverityOrdering severity1 severity2 =
  let severityOrder s = case s of
        Info -> 1
        Warning -> 2
        Error -> 3
        Fatal -> 4
  in (severityOrder severity1 <= severityOrder severity2) === True

testErrorContext :: ErrorContext -> Property
testErrorContext context =
  let code = contextCode context
      func = contextFunction context
  in (isJust code || isJust func) === True

testErrorLocationFormatting :: ErrorLocation -> Property
testErrorLocationFormatting location =
  let error = errorAt "test" (T.pack "Test message") location
      formatted = formatErrorWithLocation error
  in (T.length (T.pack formatted) > 0) === True

testErrorCollector :: [TypeError] -> Property
testErrorCollector errors =
  let collectorErrors = execState (mapM_ addError errors) []
      finalErrors = getErrors collectorErrors
  in L.length finalErrors === L.length errors

testErrorRecovery :: TypeError -> Property
testErrorRecovery typeError =
  let canRecover = canRecoverFrom typeError
      shouldContinue = shouldContinueAfter typeError
      errorSeverity = severity typeError
  in case errorSeverity of
    Info -> canRecover === True .&&. shouldContinue === True
    Warning -> canRecover === True .&&. shouldContinue === True
    Error -> canRecover === True .&&. shouldContinue === True
    Fatal -> canRecover === False .&&. shouldContinue === False

testCombinedErrors :: TypeError -> TypeError -> Property
testCombinedErrors error1 error2 =
  let combinedErrors = combineErrors [error1, error2]
      maxSeverity = maximum [severity error1, severity error2]
  in all (\e -> severity e <= maxSeverity) combinedErrors === True

testErrorTimestamps :: TypeError -> Property
testErrorTimestamps typeError =
  let ts = timestamp typeError
  in case ts of
    Nothing -> property True
    Just timestamp -> property $ not (null timestamp)  -- Timestamp should not be empty if present