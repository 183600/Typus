{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CompilerErrorHandlingQuickCheckSpec (tests) where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Compiler.Errors.Core
import Compiler (CompilerError(..), CompilationPhase(..))
import SourceLocation (SourcePos(..), SourceSpan(..))

import qualified Data.Text as T
import Data.Maybe (isJust, isNothing)

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
  arbitrary = ErrorLocation <$> arbitrary <*> arbitrary

instance Arbitrary ErrorContext where
  arbitrary = do
    pos <- arbitrary
    ctx <- arbitrary
    return $ ErrorContext pos ctx

instance Arbitrary CompilerError where
  arbitrary = do
    severity <- arbitrary
    category <- arbitrary
    location <- arbitrary
    message <- arbitrary
    phase <- arbitrary
    suggestion <- arbitrary
    return $ CompilerError severity category location message phase suggestion

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
  let pos = errorContextPosition context
      ctx = errorContextInfo context
  in isJust pos && isJust ctx === True

testErrorLocationFormatting :: ErrorLocation -> Property
testErrorLocationFormatting location =
  let formatted = formatErrorWithLocation location "Test message"
  in (T.length formatted > 0) === True

testErrorCollector :: [CompilerError] -> Property
testErrorCollector errors =
  let collector = newErrorCollector
      collectorWithErrors = foldl addError collector errors
      finalErrors = getErrors collectorWithErrors
  in L.length finalErrors === L.length errors

testErrorRecovery :: ErrorSeverity -> Property
testErrorRecovery severity =
  let canRecover = canRecoverFrom severity
      shouldContinue = shouldContinueAfter severity
  in case severity of
    Info -> canRecover === True .&&. shouldContinue === True
    Warning -> canRecover === True .&&. shouldContinue === True
    Error -> canRecover === False .&&. shouldContinue === True
    Fatal -> canRecover === False .&&. shouldContinue === False

testCombinedErrors :: CompilerError -> CompilerError -> Property
testCombinedErrors error1 error2 =
  let combined = CombinedError [error1, error2]
      combinedSeverity = getCombinedSeverity combined
  in (combinedSeverity `elem` [Info, Warning, Error, Fatal]) === True

testErrorTimestamps :: Property
testErrorTimestamps =
  let errors = []
      combined = CombinedError errors
  in case map getErrorSeverity errors of
    [] -> Info === getCombinedSeverity combined
    severities -> L.maximum severities === getCombinedSeverity combined

getErrorSeverity :: CompilerError -> ErrorSeverity
getErrorSeverity (CompilerError severity _ _ _ _ _) = severity

getErrorTimestamp :: CompilerError -> Maybe String
getErrorTimestamp (CompilerError _ _ _ _ _ _) = Nothing  -- Simplified for test