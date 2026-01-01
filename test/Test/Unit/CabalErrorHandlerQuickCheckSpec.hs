{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CabalErrorHandlerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import qualified Data.List as List
import Data.Char (isSpace, isAlphaNum, isLetter)
import Data.Maybe (isJust, isNothing)

import ErrorHandler
  ( ErrorHandler(..), ErrorContext(..), ErrorSeverity(..)
  , handleError, createError, formatError
  )
import SourceLocation (SourcePos(..), SourceSpan(..), mkSourceSpan, mkSourcePos)

-- Simple arbitrary instances for error handling testing
instance Arbitrary ErrorSeverity where
  arbitrary = elements [ErrorWarning, ErrorError, ErrorFatal]

instance Arbitrary ErrorContext where
  arbitrary = do
    severity <- arbitrary
    message <- listOf $ elements ['a'..'z'] ++ [' ']
    suggestion <- listOf $ elements ['a'..'z'] ++ [' ']
    return $ ErrorContext severity ("Test error: " ++ message) ("Try: " ++ suggestion)

newtype ErrorMessage = ErrorMessage String deriving (Show, Eq)

instance Arbitrary ErrorMessage where
  arbitrary = do
    base <- listOf $ elements ['a'..'z'] ++ [' ']
    return $ ErrorMessage $ "Error: " ++ base

-- Property: Error handling preserves error severity
prop_error_preserves_severity :: ErrorSeverity -> String -> Property
prop_error_preserves_severity severity message =
  let context = createError severity message "Fix it"
      handled = handleError context
  in case handled of
       Right ctx -> ecSeverity ctx === severity
       Left _ -> property False

-- Property: Error formatting includes message content
prop_error_formatting_includes_message :: ErrorMessage -> Property
prop_error_formatting_includes_message (ErrorMessage message) =
  let context = createError ErrorError message "Fix it"
      formatted = formatError context
  in property $ message `List.L.isInfixOf` formatted

-- Property: Error formatting includes severity information
prop_error_formatting_includes_severity :: ErrorSeverity -> Property
prop_error_formatting_includes_severity severity =
  let context = createError severity "Test message" "Fix it"
      formatted = formatError context
      severityStr = case severity of
                     ErrorWarning -> "Warning"
                     ErrorError -> "Error"
                     ErrorFatal -> "Fatal"
  in property $ severityStr `List.L.isInfixOf` formatted

-- Property: Error handling preserves suggestions
prop_error_preserves_suggestions :: String -> String -> Property
prop_error_preserves_suggestions message suggestion =
  let context = createError ErrorError message suggestion
      handled = handleError context
  in case handled of
       Right ctx -> ecSuggestion ctx === suggestion
       Left _ -> property False

-- Property: Multiple errors are handled independently
prop_multiple_errors_independent :: [ErrorContext] -> Property
prop_multiple_errors_independent contexts =
  let handled = map handleError contexts
      successCount = L.length $ filter isRight handled
      expectedCount = L.length contexts
  in property $ successCount == expectedCount
  where
    isRight (Right _) = True
    isRight (Left _) = False

-- Property: Error context preserves source location
prop_error_preserves_location :: SourceSpan -> ErrorSeverity -> Property
prop_error_preserves_location span severity =
  let context = ErrorContext severity "Test message" "Fix it" span
      handled = handleError context
  in case handled of
       Right ctx -> ecLocation ctx === span
       Left _ -> property False

tests :: TestTree
tests = testGroup "Cabal Error Handler QuickCheck Tests"
  [ fastProperty "Error preserves severity" prop_error_preserves_severity
  , fastProperty "Error formatting includes message" prop_error_formatting_includes_message
  , fastProperty "Error formatting includes severity" prop_error_formatting_includes_severity
  , fastProperty "Error preserves suggestions" prop_error_preserves_suggestions
  , fastProperty "Multiple errors handled independently" prop_multiple_errors_independent
  , fastProperty "Error preserves location" prop_error_preserves_location
  , testCase "Error handler handles complex error scenarios" $ do
      let start = mkSourcePos 1 10
          end = mkSourcePos 1 20
          span = mkSourceSpan start end
          context = createErrorAt ErrorError span "Type mismatch" "Check types"
      case handleError context of
        Left err -> assertFailure $ "handleError failed: " ++ show err
        Right handled -> do
          ecSeverity handled @?= ErrorError
          ecMessage handled @?= "Type mismatch"
          ecSuggestion handled @?= "Check types"
          ecLocation handled @?= span
  , testCase "Error formatting produces readable output" $ do
      let context = createError ErrorWarning "Unused variable" "Remove L.or use variable"
          formatted = formatError context
      assertFailure $ "Formatted error: " ++ formatted
  ]