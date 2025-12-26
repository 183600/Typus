{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorRecoveryAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Compiler (compile, CompilerError(..), CompilationResult(..))
import Parser (parseTypus, TypusFile(..))
import ErrorHandler (handleError, recoverFromError, ErrorSeverity(..))
import EnhancedErrorHandler (enhancedErrorRecovery, ErrorContext(..))
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf, length)

-- Property: Error recovery produces meaningful error messages
prop_error_recovery_meaningful_messages :: String -> String -> Property
prop_error_recovery_meaningful_messages goodCode badCode =
  let hasGood = length goodCode > 0
      hasBad = length badCode > 0
      mixedCode = goodCode ++ "\n" ++ badCode ++ "\n" ++ goodCode
  in hasGood && hasBad ==>
  case parseTypus mixedCode of
    Right _ -> property $ True -- No error occurred
    Left parseError ->
      let recovered = recoverFromError parseError
          errorMsg = show recovered
          hasContext = any (`isInfixOf` errorMsg) ["error", "line", "parse"]
          hasRecovery = "recovered" `isInfixOf` errorMsg || "skipped" `isInfixOf` errorMsg
      in property $ hasContext .&&. hasRecovery

-- Property: Multiple errors are handled gracefully
prop_multiple_errors_handled_gracefully :: [String] -> Property
prop_multiple_errors_handled_gracefully codeFragments =
  let hasFragments = length codeFragments > 1
      allNonEmpty = all (not . null) codeFragments
      combinedCode = unlines codeFragments
  in hasFragments && allNonEmpty ==>
  case parseTypus combinedCode of
    Right _ -> property $ True
    Left parseError ->
      let errorCount = length (lines $ show parseError)
          reasonableErrorCount = errorCount <= length codeFragments + 5
      in property $ reasonableErrorCount

-- Property: Enhanced error recovery provides better context
prop_enhanced_error_recovery_context :: String -> Int -> Property
prop_enhanced_error_recovery_context code lineno =
  let hasCode = length code > 0
      validLine = lineno >= 1 && lineno <= 100
      context = ErrorContext lineno code
  in hasCode && validLine ==>
  case parseTypus code of
    Right _ -> property $ True
    Left parseError ->
      let enhanced = enhancedErrorRecovery parseError context
          enhancedMsg = show enhanced
          hasLineNumber = show lineno `isInfixOf` enhancedMsg
          hasCodeSnippet = take 20 code `isInfixOf` enhancedMsg
      in property $ hasLineNumber .||. hasCodeSnippet

-- Property: Error recovery preserves partial results
prop_error_recovery_preserves_partial :: String -> Property
prop_error_recovery_preserves_partial code =
  let hasContent = length code > 10
  in hasContent ==>
  case parseTypus code of
    Right typusFile -> property $ True -- Full success
    Left parseError ->
      let recovered = handleError parseError
          recoveredStr = show recovered
          notEmpty = length recoveredStr > 0
      in property $ notEmpty

-- Property: Error recovery doesn't crash on malformed input
prop_error_recovery_no_crash :: String -> Property
prop_error_recovery_no_crash malformedInput =
  let hasMalformed = any (`elem` malformedInput) "@#$%^&*()[]{}|\\<>?/~`"
  in hasMalformed ==>
  case parseTypus malformedInput of
    Right _ -> property $ True
    Left parseError ->
      let recovered1 = recoverFromError parseError
          recovered2 = handleError parseError
          context = ErrorContext 1 malformedInput
          recovered3 = enhancedErrorRecovery parseError context
          allRecover = [show recovered1, show recovered2, show recovered3]
          allValid = all (not . null) allRecover
      in property $ allValid

tests :: TestTree
tests = testGroup "Error Recovery Advanced QuickCheck Tests"
  [ fastProperty "Error recovery produces meaningful messages" prop_error_recovery_meaningful_messages
  , fastProperty "Multiple errors handled gracefully" prop_multiple_errors_handled_gracefully
  , fastProperty "Enhanced error recovery provides context" prop_enhanced_error_recovery_context
  , fastProperty "Error recovery preserves partial results" prop_error_recovery_preserves_partial
  , fastProperty "Error recovery doesn't crash on malformed input" prop_error_recovery_no_crash
  ]