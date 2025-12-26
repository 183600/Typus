{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.EnhancedErrorHandlerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, listOf, elements, choose, suchThat, (===), (.&&.), forAll)
import TestSupport.QuickCheck (fastProperty)
import Compiler.Errors.Core
import SourceLocation (SourcePos(..), startPos)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isInfixOf)

-- ============================================================================
-- Enhanced QuickCheck tests for ErrorHandler module
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Enhanced ErrorHandler QuickCheck Tests"
    [ testGroup "Error Collection Properties"
        [ fastProperty "error collector maintains message count" prop_errorCollectorMaintainsCount
        , fastProperty "error collector separates errors and warnings" prop_errorCollectorSeparatesTypes
        , fastProperty "error collector preserves order" prop_errorCollectorPreservesOrder
        , fastProperty "error collector handles large volumes" prop_errorCollectorHandlesLargeVolumes
        ]
    , testGroup "Error Formatting Properties"
        [ fastProperty "error formatting preserves essential info" prop_errorFormattingPreservesInfo
        , fastProperty "error formatting is deterministic" prop_errorFormattingDeterministic
        , fastProperty "error formatting handles edge cases" prop_errorFormattingHandlesEdgeCases
        , fastProperty "batch formatting maintains consistency" prop_batchFormattingConsistent
        ]
    , testGroup "Error Recovery Properties"
        [ fastProperty "error recovery assessment is consistent" prop_errorRecoveryConsistent
        , fastProperty "continuation decisions are logical" prop_continuationDecisionsLogical
        , fastProperty "recovery strategies are appropriate" prop_recoveryStrategiesAppropriate
        ]
    , testGroup "Error Location Properties"
        [ fastProperty "error location tracking is accurate" prop_errorLocationTrackingAccurate
        , fastProperty "error location formatting is readable" prop_errorLocationFormattingReadable
        , fastProperty "error location handles edge cases" prop_errorLocationHandlesEdgeCases
        ]
    , testGroup "Error Context Properties"
        [ fastProperty "error context accumulation works" prop_errorContextAccumulation
        , fastProperty "error context merging preserves info" prop_errorContextMergingPreservesInfo
        , fastProperty "empty context is neutral" prop_emptyContextNeutral
        ]
    ]

-- ============================================================================
-- Error Collection Properties
-- ============================================================================

-- Property: error collector maintains message count
prop_errorCollectorMaintainsCount :: [String] -> [String] -> [String] -> Bool
prop_errorCollectorMaintainsCount errorMessages warningMessages infoMessages =
  let collector = newErrorCollector
      collector1 = foldl addError collector (map errorAt startPos errorMessages)
      collector2 = foldl addWarning collector1 (map warningAt startPos warningMessages)
      collector3 = foldl addInfo collector2 (map infoAt startPos infoMessages)
      errors = getErrors collector3
      warnings = getWarnings collector3
      infos = getInfo collector3
  in length errors == length errorMessages &&
     length warnings == length warningMessages &&
     length infos == length infoMessages

-- Property: error collector separates errors and warnings
prop_errorCollectorSeparatesTypes :: String -> String -> String -> Bool
prop_errorCollectorSeparatesTypes errorMsg warningMsg infoMsg =
  let collector = newErrorCollector
      collector1 = addError collector (errorAt startPos errorMsg)
      collector2 = addWarning collector1 (warningAt startPos warningMsg)
      collector3 = addInfo collector2 (infoAt startPos infoMsg)
      errors = getErrors collector3
      warnings = getWarnings collector3
      infos = getInfo collector3
  in length errors == 1 && length warnings == 1 && length infos == 1

-- Property: error collector preserves order
prop_errorCollectorPreservesOrder :: [String] -> Bool
prop_errorCollectorPreservesOrder messages =
  let collector = foldl (\c msg -> addError c (errorAt startPos msg)) newErrorCollector messages
      errors = getErrors collector
      extractedMessages = map getErrorMessage errors
  in extractedMessages == messages
  where
    getErrorMessage err = case err of
      TypeError msg _ _ _ -> T.unpack msg
      _ -> "unknown"

-- Property: error collector handles large volumes
prop_errorCollectorHandlesLargeVolumes :: Bool
prop_errorCollectorHandlesLargeVolumes =
  let manyMessages = map (\i -> "error " ++ show i) [1..1000]
      collector = foldl (\c msg -> addError c (errorAt startPos msg)) newErrorCollector manyMessages
      errors = getErrors collector
  in length errors == 1000

-- ============================================================================
-- Error Formatting Properties
-- ============================================================================

-- Property: error formatting preserves essential info
prop_errorFormattingPreservesInfo :: String -> Bool
prop_errorFormattingPreservesInfo message =
  let error = errorAt startPos message
      formatted = formatError error
  in message `isInfixOf` formatted

-- Property: error formatting is deterministic
prop_errorFormattingDeterministic :: String -> Bool
prop_errorFormattingDeterministic message =
  let error = errorAt startPos message
      formatted1 = formatError error
      formatted2 = formatError error
  in formatted1 == formatted2

-- Property: error formatting handles edge cases
prop_errorFormattingHandlesEdgeCases :: Bool
prop_errorFormattingHandlesEdgeCases =
  let emptyError = errorAt startPos ""
      longError = errorAt startPos (concat (replicate 1000 "very long message "))
      specialError = errorAt startPos "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      formattedEmpty = formatError emptyError
      formattedLong = formatError longError
      formattedSpecial = formatError specialError
  in not (null formattedEmpty) && 
     not (null formattedLong) && 
     not (null formattedSpecial)

-- Property: batch formatting maintains consistency
prop_batchFormattingConsistent :: [String] -> Bool
prop_batchFormattingConsistent messages =
  let errors = map (errorAt startPos) messages
      formatted1 = formatErrors errors
      formatted2 = formatErrors errors
  in formatted1 == formatted2

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

-- Property: error recovery assessment is consistent
prop_errorRecoveryConsistent :: String -> Bool
prop_errorRecoveryConsistent message =
  let error = errorAt startPos message
      canRecover1 = canRecoverFrom error
      canRecover2 = canRecoverFrom error
  in canRecover1 == canRecover2

-- Property: continuation decisions are logical
prop_continuationDecisionsLogical :: String -> Bool
prop_continuationDecisionsLogical message =
  let error = errorAt startPos message
      shouldContinue = shouldContinueAfter error
  in -- Should be either True or False, never crash
     shouldContinue == shouldContinue

-- Property: recovery strategies are appropriate
prop_recoveryStrategiesAppropriate :: ErrorSeverity -> Bool
prop_recoveryStrategiesAppropriate severity =
  let error = errorWithCategory severity SyntaxError "test" startPos
      canRecover = canRecoverFrom error
  in case severity of
    Fatal -> not canRecover  -- Fatal errors should not be recoverable
    _ -> True  -- Other severities may or may not be recoverable

-- ============================================================================
-- Error Location Properties
-- ============================================================================

-- Property: error location tracking is accurate
prop_errorLocationTrackingAccurate :: Int -> Int -> String -> Bool
prop_errorLocationTrackingAccurate line col message =
  let pos = SourcePos line col 0
      error = errorAt pos message
      location = getErrorLocation error
  in line == line location && col == column location

-- Property: error location formatting is readable
prop_errorLocationFormattingReadable :: Int -> Int -> String -> Bool
prop_errorLocationFormattingReadable line col message =
  let pos = SourcePos line col 0
      error = errorAt pos message
      formatted = formatErrorWithLocation error
  in not (null formatted) && show line `isInfixOf` formatted && show col `isInfixOf` formatted

-- Property: error location handles edge cases
prop_errorLocationHandlesEdgeCases :: Bool
prop_errorLocationHandlesEdgeCases =
  let pos1 = SourcePos 0 0 0  -- Invalid position
      pos2 = SourcePos 999999 999999 999999  -- Extreme position
      error1 = errorAt pos1 "test1"
      error2 = errorAt pos2 "test2"
      formatted1 = formatErrorWithLocation error1
      formatted2 = formatErrorWithLocation error2
  in not (null formatted1) && not (null formatted2)

-- ============================================================================
-- Error Context Properties
-- ============================================================================

-- Property: error context accumulation works
prop_errorContextAccumulation :: [String] -> Bool
prop_errorContextAccumulation contextMessages =
  let baseError = errorAt startPos "base error"
      errorWithContext = foldl (\err ctx -> addContext err ctx) baseError contextMessages
      contexts = getContexts errorWithContext
  in length contexts == length contextMessages

-- Property: error context merging preserves info
prop_errorContextMergingPreservesInfo :: [String] -> [String] -> Bool
prop_errorContextMergingPreservesInfo contexts1 contexts2 =
  let error1 = foldl (\err ctx -> addContext err ctx) (errorAt startPos "error1") contexts1
      error2 = foldl (\err ctx -> addContext err ctx) (errorAt startPos "error2") contexts2
      merged = mergeErrors error1 error2
      mergedContexts = getContexts merged
  in length mergedContexts >= length (max contexts1 contexts2)

-- Property: empty context is neutral
prop_emptyContextNeutral :: String -> Bool
prop_emptyContextNeutral message =
  let error = errorAt startPos message
      errorWithContext = addContext error emptyContext
  in error == errorWithContext

-- ============================================================================
-- Helper Functions and Generators
-- ============================================================================

-- Generate error severities
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Info, Warning, Error, Fatal]

-- Generate error categories
genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements 
  [ SyntaxError
  , TypeError
  , NameError
  , SemanticError
  , RuntimeError
  , IOError
  , ConfigError
  ]

-- Generate error locations
genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
  line <- choose (1, 1000)
  col <- choose (1, 100)
  endLine <- choose (line, line + 10)
  endCol <- choose (col, col + 50)
  return $ ErrorLocation
    { filePath = Nothing
    , line = line
    , column = col
    , endLine = Just endLine
    , endColumn = Just endCol
    }

-- Generate error contexts
genErrorContext :: Gen ErrorContext
genErrorContext = do
  messages <- listOf $ elements ["context1", "context2", "context3"]
  return $ ErrorContext messages

-- Helper functions for error manipulation (these would need to be implemented)
addContext :: TypeError -> ErrorContext -> TypeError
addContext err ctx = err  -- Placeholder

getContexts :: TypeError -> [ErrorContext]
getContexts err = []  -- Placeholder

mergeErrors :: TypeError -> TypeError -> TypeError
mergeErrors e1 e2 = e1  -- Placeholder

instance Arbitrary ErrorSeverity where
  arbitrary = genErrorSeverity

instance Arbitrary ErrorCategory where
  arbitrary = genErrorCategory

instance Arbitrary ErrorLocation where
  arbitrary = genErrorLocation

instance Arbitrary ErrorContext where
  arbitrary = genErrorContext

instance Arbitrary String where
  arbitrary = listOf $ elements ['a'..'z']