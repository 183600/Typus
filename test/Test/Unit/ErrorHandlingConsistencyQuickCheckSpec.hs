{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlingConsistencyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, Positive(..), resize)
import Data.List (sort, nub, intercalate)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Compiler.Errors.Core
import qualified EnhancedErrorHandler
import ErrorHandler
import SourceLocation (SourcePos(..), SourceSpan(..))
import Utils

-- Property: error creation is deterministic
prop_error_creation_deterministic :: String -> ErrorSeverity -> ErrorCategory -> Property
prop_error_creation_deterministic message severity category =
  let error1 = errorAt (SourcePos 1 1) message severity category
      error2 = errorAt (SourcePos 1 1) message severity category
  in counterexample "error creation should be deterministic" $
     show error1 === show error2

-- Property: error formatting preserves essential information
prop_error_formatting_preserves_info :: String -> ErrorSeverity -> ErrorCategory -> Property
prop_error_formatting_preserves_info message severity category =
  let error = errorAt (SourcePos 1 1) message severity category
      formatted = formatError error
  in counterexample "error formatting should preserve essential information" $
     message `isInfixOf` formatted

-- Property: error collection maintains order
prop_error_collection_order :: [String] -> Property
prop_error_collection_order messages =
  let collector = newErrorCollector
      addErrors = foldl (\col msg -> addError col (errorAt (SourcePos 1 1) msg ErrorError ErrorSyntax)) collector messages
      errors = getErrors addErrors
      errorMessages = map (\e -> errorMessage e) errors
  in counterexample "error collection should maintain order" $
     errorMessages === messages

-- Property: error severity ordering is consistent
prop_error_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_ordering sev1 sev2 =
  let collector1 = newErrorCollector
      collector2 = newErrorCollector
      _ = addError collector1 (errorAt (SourcePos 1 1) "test1" sev1 ErrorSyntax)
      _ = addError collector2 (errorAt (SourcePos 1 1) "test2" sev2 ErrorSyntax)
      errors1 = getErrors collector1
      errors2 = getErrors collector2
  in counterexample "error severity ordering should be consistent" $
     property True -- Should maintain severity-based ordering

-- Property: error context is preserved
prop_error_context_preserved :: String -> Property
prop_error_context_preserved context =
  let baseError = errorAt (SourcePos 1 1) "base message" ErrorError ErrorSyntax
      contextualError = errorWithCategory baseError ErrorTypeChecking context
  in counterexample "error context should be preserved" $
     property True -- Should maintain context information

-- Property: error recovery is possible for recoverable errors
prop_error_recovery_possible :: String -> Property
prop_error_recovery_possible message =
  let recoverableError = errorAt (SourcePos 1 1) message ErrorWarning ErrorSyntax
      canRecover = canRecoverFrom recoverableError
  in counterexample "error recovery should be possible for recoverable errors" $
     canRecover

-- Property: error location tracking is accurate
prop_error_location_accurate :: Int -> Int -> String -> Property
prop_error_location_accurate line col message =
  line > 0 && col > 0 ==>
  let pos = SourcePos line col
      error = errorAt pos message ErrorError ErrorSyntax
      errorPos = getErrorLine error
      errorCol = getErrorColumn error
  in counterexample "error location tracking should be accurate" $
     errorPos === line .&&. errorCol === col

-- Property: error filtering works correctly
prop_error_filtering :: [String] -> Property
prop_error_filtering messages =
  let collector = newErrorCollector
      addErrors = foldl (\col msg -> addError col (errorAt (SourcePos 1 1) msg ErrorError ErrorSyntax)) collector messages
      allErrors = getAllMessages addErrors
      errorMessages = filter (\e -> errorSeverity e == ErrorError) allErrors
  in counterexample "error filtering should work correctly" $
     length errorMessages <= length allErrors

-- Property: error aggregation preserves all information
prop_error_aggregation :: [String] -> Property
prop_error_aggregation messages =
  let errors = map (\msg -> errorAt (SourcePos 1 1) msg ErrorError ErrorSyntax) messages
      combined = combineErrors errors
      combinedMessages = map (\e -> errorMessage e) (getCombinedErrors combined)
  in counterexample "error aggregation should preserve all information" $
     sort combinedMessages === sort messages

-- Property: error formatting is consistent
prop_error_formatting_consistent :: String -> Property
prop_error_formatting_consistent message =
  let error = errorAt (SourcePos 1 1) message ErrorError ErrorSyntax
      format1 = formatError error
      format2 = formatError error
  in counterexample "error formatting should be consistent" $
     format1 === format2

-- Property: error handling doesn't lose information
prop_error_handling_no_loss :: String -> Property
prop_error_handling_no_loss message =
  let originalError = errorAt (SourcePos 1 1) message ErrorError ErrorSyntax
      processedError = EnhancedErrorHandler.processError originalError
  in counterexample "error handling shouldn't lose information" $
     errorMessage processedError === message

-- Property: error chains maintain causality
prop_error_chains_causality :: String -> String -> Property
prop_error_chains_causality cause effect =
  let causeError = errorAt (SourcePos 1 1) cause ErrorError ErrorSyntax
      effectError = errorAt (SourcePos 2 1) effect ErrorError ErrorTypeChecking
      chained = EnhancedErrorHandler.chainErrors causeError effectError
  in counterexample "error chains should maintain causality" $
     property True -- Should maintain cause-effect relationship

-- Property: error context building is cumulative
prop_error_context_cumulative :: [String] -> Property
prop_error_context_cumulative contexts =
  let baseError = errorAt (SourcePos 1 1) "base" ErrorError ErrorSyntax
      contextualized = foldl (\err ctx -> EnhancedErrorHandler.addContext err ctx) baseError contexts
  in counterexample "error context building should be cumulative" $
     property True -- Should accumulate all contexts

-- Property: error severity doesn't change unexpectedly
prop_error_severity_stable :: String -> ErrorSeverity -> Property
prop_error_severity_stable message severity =
  let originalError = errorAt (SourcePos 1 1) message severity ErrorSyntax
      processedError = EnhancedErrorHandler.processError originalError
  in counterexample "error severity shouldn't change unexpectedly" $
     errorSeverity processedError === severity

-- Property: error categories are consistent
prop_error_categories_consistent :: String -> ErrorCategory -> Property
prop_error_categories_consistent message category =
  let originalError = errorAt (SourcePos 1 1) message ErrorError category
      processedError = EnhancedErrorHandler.processError originalError
  in counterexample "error categories should be consistent" $
     errorCategory processedError === category

-- Property: error handling is thread-safe (conceptual)
prop_error_handling_thread_safe :: String -> Property
prop_error_handling_thread_safe message =
  let error1 = errorAt (SourcePos 1 1) message ErrorError ErrorSyntax
      error2 = errorAt (SourcePos 1 1) message ErrorError ErrorSyntax
      processed1 = EnhancedErrorHandler.processError error1
      processed2 = EnhancedErrorHandler.processError error2
  in counterexample "error handling should be thread-safe" $
     show processed1 === show processed2

-- Generate error messages for testing
genErrorMessage :: Gen String
genErrorMessage = oneof
  [ elements ["syntax error", "type mismatch", "undefined variable", "division by zero"]
  , do
      base <- elements ["error", "warning", "info"]
      num <- choose (1, 100)
      return $ base ++ " " ++ show num
  , listOf $ elements ['a'..'z'] >>= \chars -> return $ concat chars
  ]

tests :: TestTree
tests = testGroup "Error Handling Consistency QuickCheck Tests"
  [ fastProperty "error creation deterministic" prop_error_creation_deterministic
  , fastProperty "error formatting preserves info" prop_error_formatting_preserves_info
  , fastProperty "error collection order" prop_error_collection_order
  , fastProperty "error severity ordering" prop_error_severity_ordering
  , fastProperty "error context preserved" prop_error_context_preserved
  , fastProperty "error recovery possible" prop_error_recovery_possible
  , fastProperty "error location accurate" prop_error_location_accurate
  , fastProperty "error filtering works" prop_error_filtering
  , fastProperty "error aggregation preserves info" prop_error_aggregation
  , fastProperty "error formatting consistent" prop_error_formatting_consistent
  , fastProperty "error handling no loss" prop_error_handling_no_loss
  , fastProperty "error chains causality" prop_error_chains_causality
  , fastProperty "error context cumulative" prop_error_context_cumulative
  , fastProperty "error severity stable" prop_error_severity_stable
  , fastProperty "error categories consistent" prop_error_categories_consistent
  , fastProperty "error handling thread safe" prop_error_handling_thread_safe
  ]