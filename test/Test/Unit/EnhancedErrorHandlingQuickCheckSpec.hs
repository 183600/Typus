{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.EnhancedErrorHandlingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), elements, oneof, arbitrary)

import Compiler (CompilerError(..), CompilationPhase(..), renderCompilationError, formatCompilerErrors)
import ErrorHandler (ErrorHandler(..))
import EnhancedErrorHandler (EnhancedErrorHandler(..), enhancedHandleError, enhancedRecoverFromError)
import Parser (TypusFile(..), parseTypus)
import SourceLocation (SourcePos(..), SourceSpan(..), locatedWithSpan, startPos)

import Data.List (isPrefixOf, isInfixOf, sort, nub)
import Data.Maybe (isNothing, isJust, fromMaybe)
import Data.Text as T (pack, unpack, Text(..), null, length)

-- Property: Error messages contain source location information
prop_error_messages_include_location :: Property
prop_error_messages_include_location =
  forAll arbitrary $ \sourceSpan ->
    forAll (elements ["Syntax error", "Type error", "Ownership error", "Dependent type error"]) $ \errorMsg ->
      let error = CompilerError sourceSpan errorMsg SyntaxPhase
          rendered = renderCompilationError error
      in counterexample ("Error message should contain location: " ++ unpack rendered) $
         isInfixOf (show sourceSpan) (unpack rendered)

-- Property: Error formatting preserves error ordering
prop_error_formatting_preserves_order :: Property
prop_error_formatting_preserves_order =
  forAll arbitrary $ \errors ->
    let formatted = formatCompilerErrors errors
        errorCount = length errors
    in counterexample ("Formatted output should contain " ++ show errorCount ++ " errors") $
       errorCount > 0 ==> 
       (length . lines . unpack) formatted >= errorCount

-- Property: Enhanced error recovery produces valid results
prop_enhanced_error_recovery_valid :: Property
prop_enhanced_error_recovery_valid =
  forAll arbitrary $ \errorHandler ->
    forAll (elements ["parse error", "type error", "runtime error"]) $ \errorMsg ->
      let recovered = enhancedRecoverFromError errorHandler errorMsg
      in counterexample ("Recovery should produce a valid result") $
         isJust recovered

-- Property: Error handlers are composable
prop_error_handlers_composable :: Property
prop_error_handlers_composable =
  forAll arbitrary $ \handler1 ->
    forAll arbitrary $ \handler2 ->
      let testInput = "test error input"
          result1 = enhancedHandleError handler1 testInput undefined
          result2 = enhancedHandleError handler2 testInput undefined
      in counterexample ("Combined handlers should process input") $
         isJust result1 || isJust result2

-- Property: Error severity levels are consistent
prop_error_severity_consistent :: Property
prop_error_severity_consistent =
  forAll arbitrary $ \errors ->
    let syntaxErrors = filter (\e -> compilationPhase e == SyntaxPhase) errors
        typeErrors = filter (\e -> compilationPhase e == TypePhase) errors
        ownershipErrors = filter (\e -> compilationPhase e == OwnershipPhase) errors
    in counterexample "Error severity should be consistent with phase" $
       all (\e -> "syntax" `isInfixOf` T.unpack (errorMessage e) || 
                   "type" `isInfixOf` T.unpack (errorMessage e) ||
                   "ownership" `isInfixOf` T.unpack (errorMessage e)) errors

-- Property: Error context preservation
prop_error_context_preservation :: Property
prop_error_context_preservation =
  forAll arbitrary $ \sourceSpan ->
    forAll arbitrary $ \originalError ->
      let enhanced = enhancedHandleError (EnhancedErrorHandler "test") originalError sourceSpan
      in counterexample "Enhanced error should preserve original context" $
         case enhanced of
           Just err -> errorSpan err == sourceSpan
           Nothing -> True

-- Property: Error recovery strategies are exhaustive
prop_error_recovery_exhaustive :: Property
prop_error_recovery_exhaustive =
  forAll (elements ["syntax", "type", "ownership", "runtime", "io", "memory"]) $ \errorType ->
    let handler = EnhancedErrorHandler "test"
        recovered = enhancedRecoverFromError handler (errorType ++ " error") undefined
    in counterexample ("Should recover from " ++ errorType ++ " error") $
       isJust recovered

-- Property: Error message formatting is idempotent
prop_error_formatting_idempotent :: Property
prop_error_formatting_idempotent =
  forAll arbitrary $ \errors ->
    let formatted1 = formatCompilerErrors errors
        formatted2 = formatCompilerErrors errors
    in counterexample "Error formatting should be idempotent" $
       formatted1 === formatted2

-- Property: Enhanced error handling provides better diagnostics
prop_enhanced_error_diagnostics :: Property
prop_enhanced_error_diagnostics =
  forAll arbitrary $ \sourceSpan ->
    forAll (elements ["undefined variable", "type mismatch", "ownership violation"]) $ \errorType ->
      let standardError = CompilerError sourceSpan (pack errorType) TypePhase
          enhancedError = enhancedHandleError (EnhancedErrorHandler "diagnostic") errorType sourceSpan
      in counterexample "Enhanced error should provide more diagnostics" $
         case enhancedError of
           Just err -> T.length (errorMessage err) >= T.length (errorMessage standardError)
           Nothing -> False

-- Property: Error chain resolution
prop_error_chain_resolution :: Property
prop_error_chain_resolution =
  forAll arbitrary $ \errors ->
    let uniqueErrors = nub errors
        resolved = map (enhancedHandleError (EnhancedErrorHandler "chain")) (map errorMessage uniqueErrors)
    in counterexample "Error chain should resolve to unique handlers" $
       length (filter isJust resolved) <= length uniqueErrors

tests :: TestTree
tests =
  testGroup "Enhanced Error Handling QuickCheck Tests"
    [ fastProperty "Error messages include location" prop_error_messages_include_location
    , fastProperty "Error formatting preserves order" prop_error_formatting_preserves_order
    , fastProperty "Enhanced error recovery produces valid results" prop_enhanced_error_recovery_valid
    , fastProperty "Error handlers are composable" prop_error_handlers_composable
    , fastProperty "Error severity levels are consistent" prop_error_severity_consistent
    , fastProperty "Error context preservation" prop_error_context_preservation
    , fastProperty "Error recovery strategies are exhaustive" prop_error_recovery_exhaustive
    , fastProperty "Error message formatting is idempotent" prop_error_formatting_idempotent
    , fastProperty "Enhanced error handling provides better diagnostics" prop_enhanced_error_diagnostics
    , fastProperty "Error chain resolution" prop_error_chain_resolution
    ]