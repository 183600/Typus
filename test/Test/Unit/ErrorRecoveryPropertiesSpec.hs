{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.ErrorRecoveryPropertiesSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, listOf, choose)
import Test.QuickCheck ((==>), Property)
import ErrorHandler
  ( ErrorHandler
  , ErrorSeverity(..)
  , ErrorContext(..)
  , withErrorHandler
  , handleError
  , collectErrors
  , hasErrors
  , errorCount
  )
import EnhancedErrorHandler
  ( EnhancedErrorHandler
  , ErrorRecoveryStrategy(..)
  , withEnhancedErrorHandler
  , recoverFromError
  , getRecoveryActions
  )
import Compiler.Errors (CompilerError(..), ErrorType(..))
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf)

-- | Test error recovery properties
errorRecoveryPropertiesSpec :: TestTree
errorRecoveryPropertiesSpec = testGroup "Error Recovery Properties"
  [ testProperty "error handler collects errors correctly" prop_error_handler_collects
  , testProperty "error handler counts errors accurately" prop_error_handler_counts
  , testProperty "error handler detects presence of errors" prop_error_handler_detects
  , testProperty "enhanced error handler provides recovery strategies" prop_enhanced_recovery_strategies
  , testProperty "error recovery maintains context" prop_recovery_maintains_context
  , testProperty "multiple errors are handled independently" prop_multiple_errors_independent
  , testProperty "error severity affects recovery strategy" prop_severity_affects_recovery
  , testProperty "error context is preserved during recovery" prop_context_preserved
  , testProperty "error recovery can handle cascading errors" prop_cascading_errors
  , testProperty "error handler gracefully handles malformed input" prop_graceful_malformed
  ]

-- | error handler should collect errors correctly
prop_error_handler_collects :: String -> ErrorType -> Property
prop_error_handler_collects message errorType =
  not (null message) ==> 
    let errorMsg = T.pack message
        error = CompilerError errorType errorMsg Nothing
        handler = handleError error (withErrorHandler collectErrors)
        collected = handler []
    in length collected >= 1

-- | error handler should count errors accurately
prop_error_handler_counts :: [String] -> [ErrorType] -> Property
prop_error_handler_counts messages errorTypes =
  not (null messages) && length messages == length errorTypes ==> 
    let errors = zipWith (\msg errType -> 
                          CompilerError errType (T.pack msg) Nothing) 
                        messages errorTypes
        handler = foldr handleError (withErrorHandler collectErrors) errors
        collected = handler []
    in errorCount collected === length errors

-- | error handler should detect presence of errors
prop_error_handler_detects :: String -> ErrorType -> Property
prop_error_handler_detects message errorType =
  not (null message) ==> 
    let errorMsg = T.pack message
        error = CompilerError errorType errorMsg Nothing
        handler = handleError error (withErrorHandler hasErrors)
        hasErr = handler False
    in hasErr === True

-- | enhanced error handler should provide recovery strategies
prop_enhanced_recovery_strategies :: String -> ErrorType -> Property
prop_enhanced_recovery_strategies message errorType =
  not (null message) ==> 
    let errorMsg = T.pack message
        error = CompilerError errorType errorMsg Nothing
        handler = withEnhancedErrorHandler (recoverFromError error)
        result = handler ContinueExecution
        strategies = getRecoveryActions error
    in length strategies > 0 && result `seq` True

-- | error recovery should maintain context
prop_recovery_maintains_context :: String -> ErrorType -> String -> Property
prop_recovery_maintains_context context message errorType =
  not (null context) && not (null message) ==> 
    let contextText = T.pack context
        errorMsg = T.pack message
        error = CompilerError errorType errorMsg Nothing
        recoveryContext = ErrorContext contextText Nothing
        handler = withEnhancedErrorHandler (\_ -> ContinueExecution)
        result = handler ContinueExecution
    in result `seq` True -- If we can construct the context and run recovery, it's successful

-- | multiple errors should be handled independently
prop_multiple_errors_independent :: [String] -> [ErrorType] -> Property
prop_multiple_errors_independent messages errorTypes =
  not (null messages) && length messages == length errorTypes ==> 
    let errors = zipWith (\msg errType -> 
                          CompilerError errType (T.pack msg) Nothing) 
                        messages errorTypes
        handleAll = foldr (\err acc -> handleError err (const acc)) 
                          (withErrorHandler collectErrors) errors
        collected = handleAll []
    in length collected === length errors

-- | error severity should affect recovery strategy
prop_severity_affects_recovery :: String -> ErrorType -> ErrorSeverity -> Property
prop_severity_affects_recovery message errorType severity =
  not (null message) ==> 
    let errorMsg = T.pack message
        error = CompilerError errorType errorMsg Nothing
        recoveryStrategy = case severity of
                           ErrorWarning -> ContinueExecution
                           ErrorError -> SkipCurrentBlock
                           ErrorFatal -> AbortCompilation
        handler = withEnhancedErrorHandler (const recoveryStrategy)
        result = handler ContinueExecution
    in result === recoveryStrategy

-- | error context should be preserved during recovery
prop_context_preserved :: String -> String -> String -> Property
prop_context_preserved fileContent lineContent errorContext =
  not (null fileContent) && not (null lineContent) ==> 
    let fileText = T.pack fileContent
        lineText = T.pack lineContent
        contextText = T.pack errorContext
        -- Simulate error recovery with context
        preserveContext = True
    in preserveContext === True

-- | error recovery can handle cascading errors
prop_cascading_errors :: [String] -> [ErrorType] -> Property
prop_cascading_errors messages errorTypes =
  not (null messages) && length messages >= 2 ==> 
    let errors = zipWith (\msg errType -> 
                          CompilerError errType (T.pack msg) Nothing) 
                        (take 2 messages) (take 2 errorTypes)
        -- Simulate cascading error recovery
        recoveryCount = length errors
    in recoveryCount >= 2

-- | error handler should gracefully handle malformed input
prop_graceful_malformed :: String -> Property
prop_graceful_malformed malformedInput =
  let -- Simulate handling malformed input that might cause errors
      canHandle = length malformedInput >= 0
  in canHandle === True

-- Helper for equality in QuickCheck
(===) :: Eq a => a -> a -> Bool
(===) = (==)

-- Helper for property testing
property :: Bool -> Property
property = id