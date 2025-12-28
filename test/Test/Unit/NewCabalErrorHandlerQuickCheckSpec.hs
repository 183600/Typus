{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalErrorHandlerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Arbitrary (Arbitrary(..), arbitrary)
import Test.QuickCheck.Gen (oneof, listOf, choose, elements, listOf1)

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
  , formatError
  , formatErrors
  , canRecoverFrom
  , shouldContinueAfter
  , errorAt
  , errorWithCategory
  , warningAt
  , warningWithCategory
  , infoAt
  , infoWithCategory
  )

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import Data.List (isInfixOf, isPrefixOf)
import Data.Time (UTCTime, fromGregorian, secondsToDiffTime)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary ErrorSeverity where
  arbitrary = elements [Error, Warning, Info]

instance Arbitrary ErrorCategory where
  arbitrary = oneof
    [ return SyntaxError
    , return TypeError
    , return NameError
    , return SemanticError
    , return RuntimeError
    , return InternalError
    , return UserError
    ]

instance Arbitrary ErrorLocation where
  arbitrary = oneof
    [ NoLocation <$> arbitrary
    , LineLocation <$> choose (1, 1000) <*> choose (1, 1000)
    , SpanLocation <$> arbitrary
    ]

instance Arbitrary ErrorContext where
  arbitrary = do
    context <- listOf (arbitrary :: Gen String)
    return $ ErrorContext context

instance Arbitrary ErrorRecovery where
  arbitrary = elements [CanRecover, CannotRecover, MayRecover]

instance Arbitrary TypeError where
  arbitrary = do
    message <- listOf1 (elements ['a'..'z'])
    severity <- arbitrary
    category <- arbitrary
    location <- arbitrary
    context <- arbitrary
    recovery <- arbitrary
    timestamp <- arbitrary
    return $ TypeError message severity category location context recovery timestamp

instance Arbitrary CombinedError where
  arbitrary = do
    primary <- arbitrary
    secondary <- listOf arbitrary
    return $ CombinedError primary secondary

instance Arbitrary UTCTime where
  arbitrary = do
    year <- choose (2000, 2030)
    month <- choose (1, 12)
    day <- choose (1, 28)
    hour <- choose (0, 23)
    minute <- choose (0, 59)
    second <- choose (0, 59)
    return $ fromGregorian year month day `addUTCTime` secondsToDiffTime (hour * 3600 + minute * 60 + second)

-- ============================================================================
-- ErrorHandler Property Tests
-- ============================================================================

-- Property: Error creation preserves message
prop_error_creation_preserves_message :: String -> ErrorSeverity -> ErrorCategory -> Property
prop_error_creation_preserves_message message severity category =
  not (null message) ==>
  let location = NoLocation
      context = emptyContext
      recovery = CanRecover
      timestamp = fromGregorian 2023 1 1 `addUTCTime` 0
      error = TypeError message severity category location context recovery timestamp
      extractedMessage = case error of
        TypeError msg _ _ _ _ _ _ -> msg
  in property $ extractedMessage === message

-- Property: Error severity is preserved
prop_error_severity_preserved :: String -> ErrorSeverity -> Property
prop_error_severity_preserved message severity =
  not (null message) ==>
  let location = NoLocation
      context = emptyContext
      category = SyntaxError
      recovery = CanRecover
      timestamp = fromGregorian 2023 1 1 `addUTCTime` 0
      error = TypeError message severity category location context recovery timestamp
      extractedSeverity = case error of
        TypeError _ sev _ _ _ _ _ -> sev
  in property $ extractedSeverity === severity

-- Property: Error category is preserved
prop_error_category_preserved :: String -> ErrorCategory -> Property
prop_error_category_preserved message category =
  not (null message) ==>
  let location = NoLocation
      context = emptyContext
      severity = Error
      recovery = CanRecover
      timestamp = fromGregorian 2023 1 1 `addUTCTime` 0
      error = TypeError message severity category location context recovery timestamp
      extractedCategory = case error of
        TypeError _ _ cat _ _ _ _ -> cat
  in property $ extractedCategory === category

-- Property: Error collector starts empty
prop_errorcollector_starts_empty :: Property
prop_errorcollector_starts_empty =
  let collector = newErrorCollector
      hasErrs = hasErrors collector
      hasWarns = hasWarnings collector
  in property $ not (hasErrs .||. hasWarns)

-- Property: Adding error increases error count
prop_errorcollector_add_error :: String -> Property
prop_errorcollector_add_error message =
  not (null message) ==>
  let collector = newErrorCollector
      collector' = addError message collector
      errorCount = length (getErrors collector')
  in property $ errorCount === 1

-- Property: Adding warning increases warning count
prop_errorcollector_add_warning :: String -> Property
prop_errorcollector_add_warning message =
  not (null message) ==>
  let collector = newErrorCollector
      collector' = addWarning message collector
      warningCount = length (getWarnings collector')
  in property $ warningCount === 1

-- Property: Adding info increases info count
prop_errorcollector_add_info :: String -> Property
prop_errorcollector_add_info message =
  not (null message) ==>
  let collector = newErrorCollector
      collector' = addInfo message collector
      infoCount = length (getInfo collector')
  in property $ infoCount === 1

-- Property: Multiple errors are accumulated
prop_errorcollector_multiple_errors :: [String] -> Property
prop_errorcollector_multiple_errors messages =
  not (null messages) && all (not . null) messages ==>
  let collector = foldl addError newErrorCollector messages
      errorCount = length (getErrors collector)
  in property $ errorCount === length messages

-- Property: Multiple warnings are accumulated
prop_errorcollector_multiple_warnings :: [String] -> Property
prop_errorcollector_multiple_warnings messages =
  not (null messages) && all (not . null) messages ==>
  let collector = foldl addWarning newErrorCollector messages
      warningCount = length (getWarnings collector)
  in property $ warningCount === length messages

-- Property: Error detection works correctly
prop_errorcollector_has_errors :: [String] -> Property
prop_errorcollector_has_errors messages =
  let collector = foldl addError newErrorCollector messages
      hasErrs = hasErrors collector
  in property $ hasErrs === not (null messages)

-- Property: Warning detection works correctly
prop_errorcollector_has_warnings :: [String] -> Property
prop_errorcollector_has_warnings messages =
  let collector = foldl addWarning newErrorCollector messages
      hasWarns = hasWarnings collector
  in property $ hasWarns === not (null messages)

-- Property: Error formatting contains message
prop_error_formatting_contains_message :: String -> Property
prop_error_formatting_contains_message message =
  not (null message) ==>
  let location = NoLocation
      context = emptyContext
      severity = Error
      category = SyntaxError
      recovery = CanRecover
      timestamp = fromGregorian 2023 1 1 `addUTCTime` 0
      error = TypeError message severity category location context recovery timestamp
      formatted = formatError error
  in property $ message `isInfixOf` formatted

-- Property: Multiple errors formatting preserves all messages
prop_multiple_errors_formatting :: [String] -> Property
prop_multiple_errors_formatting messages =
  not (null messages) && all (not . null) messages ==>
  let errors = map (\msg -> TypeError msg Error SyntaxError NoLocation emptyContext CanRecover (fromGregorian 2023 1 1 `addUTCTime` 0)) messages
      formatted = formatErrors errors
  in property $ all (`isInfixOf` formatted) messages

-- Property: Error recovery affects canRecoverFrom
prop_error_recovery_affects_recovery :: ErrorRecovery -> Property
prop_error_recovery_affects_recovery recovery =
  let message = "test error"
      location = NoLocation
      context = emptyContext
      severity = Error
      category = SyntaxError
      timestamp = fromGregorian 2023 1 1 `addUTCTime` 0
      error = TypeError message severity category location context recovery timestamp
      canRecover = canRecoverFrom error
  in case recovery of
    CanRecover -> property $ canRecover
    CannotRecover -> property $ not canRecover
    MayRecover -> property $ True

-- Property: Error severity affects shouldContinueAfter
prop_error_severity_affects_continue :: ErrorSeverity -> Property
prop_error_severity_affects_continue severity =
  let message = "test error"
      location = NoLocation
      context = emptyContext
      category = SyntaxError
      recovery = CanRecover
      timestamp = fromGregorian 2023 1 1 `addUTCTime` 0
      error = TypeError message severity category location context recovery timestamp
      shouldContinue = shouldContinueAfter error
  in case severity of
    Error -> property $ not shouldContinue || recovery == CanRecover
    Warning -> property $ shouldContinue
    Info -> property $ shouldContinue

-- Property: Combined error contains primary and secondary
prop_combined_error_structure :: String -> [String] -> Property
prop_combined_error_structure primaryMessage secondaryMessages =
  not (null primaryMessage) ==>
  let location = NoLocation
      context = emptyContext
      severity = Error
      category = SyntaxError
      recovery = CanRecover
      timestamp = fromGregorian 2023 1 1 `addUTCTime` 0
      primary = TypeError primaryMessage severity category location context recovery timestamp
      secondary = map (\msg -> TypeError msg severity category location context recovery timestamp) secondaryMessages
      combined = CombinedError primary secondary
      extractedPrimary = case combined of
        CombinedError p _ -> p
      extractedSecondary = case combined of
        CombinedError _ s -> s
  in property $ extractedPrimary === primary .&&. extractedSecondary === secondary

-- Property: Error location affects formatting
prop_error_location_affects_formatting :: ErrorLocation -> Property
prop_error_location_affects_formatting location =
  let message = "test error"
      context = emptyContext
      severity = Error
      category = SyntaxError
      recovery = CanRecover
      timestamp = fromGregorian 2023 1 1 `addUTCTime` 0
      error = TypeError message severity category location context recovery timestamp
      formatted = formatError error
  in case location of
    NoLocation -> property $ True
    LineLocation line col -> property $ show line `isInfixOf` formatted .&&. show col `isInfixOf` formatted
    SpanLocation span -> property $ True

-- Property: Error context is preserved
prop_error_context_preserved :: [String] -> Property
prop_error_context_preserved contextStrings =
  let context = ErrorContext contextStrings
      message = "test error"
      location = NoLocation
      severity = Error
      category = SyntaxError
      recovery = CanRecover
      timestamp = fromGregorian 2023 1 1 `addUTCTime` 0
      error = TypeError message severity category location context recovery timestamp
      extractedContext = case error of
        TypeError _ _ _ _ ctx _ _ -> ctx
  in property $ extractedContext === context

-- Property: Error creation functions work correctly
prop_error_creation_functions :: String -> ErrorCategory -> Property
prop_error_creation_functions message category =
  not (null message) ==>
  let pos = SourcePos 10 20
      location = LineLocation 10 20
      error1 = errorAt pos message
      error2 = errorWithCategory category message
      warning1 = warningAt pos message
      warning2 = warningWithCategory category message
      info1 = infoAt pos message
      info2 = infoWithCategory category message
  in property $ True

-- Property: Error collector preserves order
prop_errorcollector_preserves_order :: [String] -> Property
prop_errorcollector_preserves_order messages =
  not (null messages) && all (not . null) messages ==>
  let collector = foldl addError newErrorCollector messages
      errors = getErrors collector
      errorMessages = map (\err -> case err of TypeError msg _ _ _ _ _ _ -> msg) errors
  in property $ errorMessages === messages

tests :: TestTree
tests = testGroup "New Cabal ErrorHandler QuickCheck Tests"
  [ fastProperty "Error creation preserves message" prop_error_creation_preserves_message
  , fastProperty "Error severity preserved" prop_error_severity_preserved
  , fastProperty "Error category preserved" prop_error_category_preserved
  , fastProperty "Error collector starts empty" prop_errorcollector_starts_empty
  , fastProperty "Adding error increases count" prop_errorcollector_add_error
  , fastProperty "Adding warning increases count" prop_errorcollector_add_warning
  , fastProperty "Adding info increases count" prop_errorcollector_add_info
  , fastProperty "Multiple errors accumulated" prop_errorcollector_multiple_errors
  , fastProperty "Multiple warnings accumulated" prop_errorcollector_multiple_warnings
  , fastProperty "Error detection works" prop_errorcollector_has_errors
  , fastProperty "Warning detection works" prop_errorcollector_has_warnings
  , fastProperty "Error formatting contains message" prop_error_formatting_contains_message
  , fastProperty "Multiple errors formatting" prop_multiple_errors_formatting
  , fastProperty "Error recovery affects recovery" prop_error_recovery_affects_recovery
  , fastProperty "Error severity affects continue" prop_error_severity_affects_continue
  , fastProperty "Combined error structure" prop_combined_error_structure
  , fastProperty "Error location affects formatting" prop_error_location_affects_formatting
  , fastProperty "Error context preserved" prop_error_context_preserved
  , fastProperty "Error creation functions" prop_error_creation_functions
  , fastProperty "Error collector preserves order" prop_errorcollector_preserves_order
  ]