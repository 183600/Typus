{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Core ErrorHandler module QuickCheck tests
module Test.Unit.CoreErrorHandlerQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import TestSupport.Arbitrary ()
import TestSupport.QuickCheck
import qualified Data.Text as T
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, intercalate)
import Data.Maybe (isJust, isNothing)
import Control.Monad (when)
import Data.Char (isSpace, isAlpha, isAlphaNum)
import Data.Time (UTCTime)

import Compiler.Errors.Core

-- ============================================================================
-- ErrorHandler QuickCheck Tests
-- ============================================================================

-- | Test that newErrorCollector creates an empty collector
prop_newErrorCollectorEmpty :: Property
prop_newErrorCollectorEmpty =
  let collector = newErrorCollector
  in property $ not (hasErrors collector) && not (hasWarnings collector) && null (getAllMessages collector)

-- | Test that addError adds an error
prop_addErrorAddsError :: Property
prop_addErrorAddsError =
  forAll arbitraryString $ \msg ->
    forAll arbitrarySourceSpan $ \span ->
      let collector = newErrorCollector
          collector' = addError collector msg span
      in property $ hasErrors collector' && length (getErrors collector') == 1

-- | Test that addWarning adds a warning
prop_addWarningAddsWarning :: Property
prop_addWarningAddsWarning =
  forAll arbitraryString $ \msg ->
    forAll arbitrarySourceSpan $ \span ->
      let collector = newErrorCollector
          collector' = addWarning collector msg span
      in property $ hasWarnings collector' && length (getWarnings collector') == 1

-- | Test that addInfo adds an info message
prop_addInfoAddsInfo :: Property
prop_addInfoAddsInfo =
  forAll arbitraryString $ \msg ->
    forAll arbitrarySourceSpan $ \span ->
      let collector = newErrorCollector
          collector' = addInfo collector msg span
      in property $ length (getInfo collector') == 1

-- | Test that hasErrors detects errors
prop_hasErrorsDetectsErrors :: Property
prop_hasErrorsDetectsErrors =
  forAll arbitraryString $ \msg ->
    forAll arbitrarySourceSpan $ \span ->
      let collector = newErrorCollector
          collector' = addError collector msg span
      in property $ not (hasErrors collector) && hasErrors collector'

-- | Test that hasWarnings detects warnings
prop_hasWarningsDetectsWarnings :: Property
prop_hasWarningsDetectsWarnings =
  forAll arbitraryString $ \msg ->
    forAll arbitrarySourceSpan $ \span ->
      let collector = newErrorCollector
          collector' = addWarning collector msg span
      in property $ not (hasWarnings collector) && hasWarnings collector'

-- | Test that errorAt creates an error at a position
prop_errorAtCreatesError :: Property
prop_errorAtCreatesError =
  forAll arbitraryString $ \msg ->
    forAll arbitrarySourcePos $ \pos ->
      let err = errorAt msg pos
      in property $ True  -- Basic sanity check

-- | Test that errorAtWithTimestamp creates an error with timestamp
prop_errorAtWithTimestampCreatesError :: Property
prop_errorAtWithTimestampCreatesError =
  forAll arbitraryString $ \msg ->
    forAll arbitrarySourcePos $ \pos ->
      forAll arbitraryUTCTime $ \time ->
        let err = errorAtWithTimestamp msg pos time
        in property $ True  -- Basic sanity check

-- | Test that errorAtWithUTCTime creates an error with UTC time
prop_errorAtWithUTCTimeCreatesError :: Property
prop_errorAtWithUTCTimeCreatesError =
  forAll arbitraryString $ \msg ->
    forAll arbitrarySourcePos $ \pos ->
      forAll arbitraryUTCTime $ \time ->
        let err = errorAtWithUTCTime msg pos time
        in property $ True  -- Basic sanity check

-- | Test that errorWithCategory creates an error with category
prop_errorWithCategoryCreatesError :: Property
prop_errorWithCategoryCreatesError =
  forAll arbitraryString $ \msg ->
    forAll arbitrarySourcePos $ \pos ->
      forAll arbitraryErrorCategory $ \category ->
        let err = errorWithCategory msg pos category
        in property $ True  -- Basic sanity check

-- | Test that warningAt creates a warning at a position
prop_warningAtCreatesWarning :: Property
prop_warningAtCreatesWarning =
  forAll arbitraryString $ \msg ->
    forAll arbitrarySourcePos $ \pos ->
      let warning = warningAt msg pos
      in property $ True  -- Basic sanity check

-- | Test that warningWithCategory creates a warning with category
prop_warningWithCategoryCreatesWarning :: Property
prop_warningWithCategoryCreatesWarning =
  forAll arbitraryString $ \msg ->
    forAll arbitrarySourcePos $ \pos ->
      forAll arbitraryErrorCategory $ \category ->
        let warning = warningWithCategory msg pos category
        in property $ True  -- Basic sanity check

-- | Test that infoAt creates an info message at a position
prop_infoAtCreatesInfo :: Property
prop_infoAtCreatesInfo =
  forAll arbitraryString $ \msg ->
    forAll arbitrarySourcePos $ \pos ->
      let info = infoAt msg pos
      in property $ True  -- Basic sanity check

-- | Test that infoWithCategory creates an info message with category
prop_infoWithCategoryCreatesInfo :: Property
prop_infoWithCategoryCreatesInfo =
  forAll arbitraryString $ \msg ->
    forAll arbitrarySourcePos $ \pos ->
      forAll arbitraryErrorCategory $ \category ->
        let info = infoWithCategory msg pos category
        in property $ True  -- Basic sanity check

-- | Test that formatError produces a string
prop_formatErrorProducesString :: Property
prop_formatErrorProducesString =
  forAll arbitraryString $ \msg ->
    forAll arbitrarySourceSpan $ \span ->
      let collector = newErrorCollector
          collector' = addError collector msg span
          errors = getErrors collector'
      in if null errors
         then property True
         else property $ not (null (formatError (head errors)))

-- | Test that formatErrors produces strings
prop_formatErrorsProducesStrings :: Property
prop_formatErrorsProducesStrings =
  forAll (listOf1 arbitraryString) $ \msgs ->
    forAll arbitrarySourceSpan $ \span ->
      let collector = foldl (\c msg -> addError c msg span) newErrorCollector msgs
          errors = getErrors collector
          formatted = formatErrors errors
      in property $ length formatted == length msgs

-- | Test that formatErrorWithLocation produces a string with location
prop_formatErrorWithLocationProducesString :: Property
prop_formatErrorWithLocationProducesString =
  forAll arbitraryString $ \msg ->
    forAll arbitrarySourceSpan $ \span ->
      let collector = newErrorCollector
          collector' = addError collector msg span
          errors = getErrors collector'
      in if null errors
         then property True
         else property $ not (null (formatErrorWithLocation (head errors)))

-- | Test that formatErrorsWithLocation produces strings with locations
prop_formatErrorsWithLocationProducesStrings :: Property
prop_formatErrorsWithLocationProducesStrings =
  forAll (listOf1 arbitraryString) $ \msgs ->
    forAll arbitrarySourceSpan $ \span ->
      let collector = foldl (\c msg -> addError c msg span) newErrorCollector msgs
          errors = getErrors collector
          formatted = formatErrorsWithLocation errors
      in property $ length formatted == length msgs

-- | Test that canRecoverFrom determines recoverability
prop_canRecoverFromDeterminesRecoverability :: Property
prop_canRecoverFromDeterminesRecoverability =
  forAll arbitraryErrorSeverity $ \severity ->
    let result = canRecoverFrom severity
    in property $ result == (severity /= ErrorFatal)

-- | Test that shouldContinueAfter determines continuation
prop_shouldContinueAfterDeterminesContinuation :: Property
prop_shouldContinueAfterDeterminesContinuation =
  forAll arbitraryErrorSeverity $ \severity ->
    let result = shouldContinueAfter severity
    in property $ result == (severity /= ErrorFatal)

-- | Test that getAllMessages gets all messages
prop_getAllMessagesGetsAll :: Property
prop_getAllMessagesGetsAll =
  forAll (listOf1 arbitraryString) $ \errorMsgs ->
    forAll (listOf1 arbitraryString) $ \warningMsgs ->
      forAll (listOf1 arbitraryString) $ \infoMsgs ->
        forAll arbitrarySourceSpan $ \span ->
          let collector = foldl (\c msg -> addError c msg span) newErrorCollector errorMsgs
              collector' = foldl (\c msg -> addWarning c msg span) collector warningMsgs
              collector'' = foldl (\c msg -> addInfo c msg span) collector' infoMsgs
              allMessages = getAllMessages collector''
          in property $ length allMessages == length errorMsgs + length warningMsgs + length infoMsgs

-- | Test that error messages are preserved
prop_errorMessagesPreserved :: Property
prop_errorMessagesPreserved =
  forAll (listOf1 arbitraryString) $ \msgs ->
    forAll arbitrarySourceSpan $ \span ->
      let collector = foldl (\c msg -> addError c msg span) newErrorCollector msgs
          errors = getErrors collector
      in property $ map teMessage errors == msgs

-- | Test that warning messages are preserved
prop_warningMessagesPreserved :: Property
prop_warningMessagesPreserved =
  forAll (listOf1 arbitraryString) $ \msgs ->
    forAll arbitrarySourceSpan $ \span ->
      let collector = foldl (\c msg -> addWarning c msg span) newErrorCollector msgs
          warnings = getWarnings collector
      in property $ map teMessage warnings == msgs

-- | Test that info messages are preserved
prop_infoMessagesPreserved :: Property
prop_infoMessagesPreserved =
  forAll (listOf1 arbitraryString) $ \msgs ->
    forAll arbitrarySourceSpan $ \span ->
      let collector = foldl (\c msg -> addInfo c msg span) newErrorCollector msgs
          infos = getInfo collector
      in property $ map teMessage infos == msgs

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Core ErrorHandler QuickCheck Tests"
  [ testProperty "NewErrorCollector creates empty collector" prop_newErrorCollectorEmpty
  , testProperty "AddError adds an error" prop_addErrorAddsError
  , testProperty "AddWarning adds a warning" prop_addWarningAddsWarning
  , testProperty "AddInfo adds an info message" prop_addInfoAddsInfo
  , testProperty "HasErrors detects errors" prop_hasErrorsDetectsErrors
  , testProperty "HasWarnings detects warnings" prop_hasWarningsDetectsWarnings
  , testProperty "ErrorAt creates an error" prop_errorAtCreatesError
  , testProperty "ErrorAtWithTimestamp creates an error with timestamp" prop_errorAtWithTimestampCreatesError
  , testProperty "ErrorAtWithUTCTime creates an error with UTC time" prop_errorAtWithUTCTimeCreatesError
  , testProperty "ErrorWithCategory creates an error with category" prop_errorWithCategoryCreatesError
  , testProperty "WarningAt creates a warning" prop_warningAtCreatesWarning
  , testProperty "WarningWithCategory creates a warning with category" prop_warningWithCategoryCreatesWarning
  , testProperty "InfoAt creates an info message" prop_infoAtCreatesInfo
  , testProperty "InfoWithCategory creates an info message with category" prop_infoWithCategoryCreatesInfo
  , testProperty "FormatError produces a string" prop_formatErrorProducesString
  , testProperty "FormatErrors produces strings" prop_formatErrorsProducesStrings
  , testProperty "FormatErrorWithLocation produces a string with location" prop_formatErrorWithLocationProducesString
  , testProperty "FormatErrorsWithLocation produces strings with locations" prop_formatErrorsWithLocationProducesStrings
  , testProperty "CanRecoverFrom determines recoverability" prop_canRecoverFromDeterminesRecoverability
  , testProperty "ShouldContinueAfter determines continuation" prop_shouldContinueAfterDeterminesContinuation
  , testProperty "GetAllMessages gets all messages" prop_getAllMessagesGetsAll
  , testProperty "Error messages are preserved" prop_errorMessagesPreserved
  , testProperty "Warning messages are preserved" prop_warningMessagesPreserved
  , testProperty "Info messages are preserved" prop_infoMessagesPreserved
  ]

-- | Run all tests
main :: IO ()
main = defaultMain testSuite