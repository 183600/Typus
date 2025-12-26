{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewErrorHandlerCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import ErrorHandler
  ( ErrorHandler
  , ErrorSeverity(..)
  , ErrorContext(..)
  , ErrorMessage(..)
  , ErrorInfo(..)
  , createErrorHandler
  , addError
  , addWarning
  , hasErrors
  , hasWarnings
  , getErrors
  , getWarnings
  , clearErrors
  , clearWarnings
  , errorCount
  , warningCount
  , formatError
  , formatWarning
  )

import SourceLocation (SourcePos(..), startPos, posAt)
import Data.List (sort, nub)

-- Property: Create error handler consistency
prop_create_error_handler_consistency :: Property
prop_create_error_handler_consistency =
  let handler = createErrorHandler
  in property $ not (hasErrors handler) .&&. not (hasWarnings handler) .&&.
     errorCount handler === 0 .&&. warningCount handler === 0

-- Property: Add and detect errors
prop_add_detect_errors :: String -> String -> Property
prop_add_detect_errors message context =
  length message <= 100 && length context <= 100 ==>
  let handler1 = createErrorHandler
      handler2 = addError message context handler1
  in property $ not (hasErrors handler1) .&&. hasErrors handler2 .&&.
     errorCount handler1 === 0 .&&. errorCount handler2 === 1

-- Property: Add and detect warnings
prop_add_detect_warnings :: String -> String -> Property
prop_add_detect_warnings message context =
  length message <= 100 && length context <= 100 ==>
  let handler1 = createErrorHandler
      handler2 = addWarning message context handler1
  in property $ not (hasWarnings handler1) .&&. hasWarnings handler2 .&&.
     warningCount handler1 === 0 .&&. warningCount handler2 === 1

-- Property: Multiple errors accumulate correctly
prop_multiple_errors_accumulate :: [String] -> [String] -> Property
prop_multiple_errors_accumulate messages contexts =
  not (null messages) && length messages <= 5 && length contexts <= 5 &&
  all (\m -> length m <= 50) messages && all (\c -> length c <= 50) contexts ==>
  let handler1 = createErrorHandler
      handler2 = foldr (\(msg, ctx) h -> addError msg ctx h) handler1 (zip messages contexts)
      expectedCount = length messages
  in property $ hasErrors handler2 .&&. errorCount handler2 === expectedCount

-- Property: Multiple warnings accumulate correctly
prop_multiple_warnings_accumulate :: [String] -> [String] -> Property
prop_multiple_warnings_accumulate messages contexts =
  not (null messages) && length messages <= 5 && length contexts <= 5 &&
  all (\m -> length m <= 50) messages && all (\c -> length c <= 50) contexts ==>
  let handler1 = createErrorHandler
      handler2 = foldr (\(msg, ctx) h -> addWarning msg ctx h) handler1 (zip messages contexts)
      expectedCount = length messages
  in property $ hasWarnings handler2 .&&. warningCount handler2 === expectedCount

-- Property: Mixed errors and warnings
prop_mixed_errors_warnings :: [String] -> [String] -> Property
prop_mixed_errors_warnings errorMessages warningMessages =
  length errorMessages <= 3 && length warningMessages <= 3 &&
  all (\m -> length m <= 30) (errorMessages ++ warningMessages) ==>
  let handler1 = createErrorHandler
      handler2 = foldr (\msg h -> addError msg "error context" h) handler1 errorMessages
      handler3 = foldr (\msg h -> addWarning msg "warning context" h) handler2 warningMessages
  in property $ hasErrors handler3 .&&. hasWarnings handler3 .&&.
     errorCount handler3 === length errorMessages .&&.
     warningCount handler3 === length warningMessages

-- Property: Clear errors functionality
prop_clear_errors :: [String] -> Property
prop_clear_errors messages =
  length messages <= 5 && all (\m -> length m <= 50) messages ==>
  let handler1 = createErrorHandler
      handler2 = foldr (\msg h -> addError msg "context" h) handler1 messages
      handler3 = clearErrors handler2
  in property $ hasErrors handler2 .&&. not (hasErrors handler3) .&&.
     errorCount handler2 === length messages .&&. errorCount handler3 === 0

-- Property: Clear warnings functionality
prop_clear_warnings :: [String] -> Property
prop_clear_warnings messages =
  length messages <= 5 && all (\m -> length m <= 50) messages ==>
  let handler1 = createErrorHandler
      handler2 = foldr (\msg h -> addWarning msg "context" h) handler1 messages
      handler3 = clearWarnings handler2
  in property $ hasWarnings handler2 .&&. not (hasWarnings handler3) .&&.
     warningCount handler2 === length messages .&&. warningCount handler3 === 0

-- Property: Get errors returns correct content
prop_get_errors_content :: [String] -> [String] -> Property
prop_get_errors_content messages contexts =
  not (null messages) && length messages <= 3 && length contexts <= 3 &&
  all (\m -> length m <= 30) messages && all (\c -> length c <= 30) contexts ==>
  let handler1 = createErrorHandler
      handler2 = foldr (\(msg, ctx) h -> addError msg ctx h) handler1 (zip messages contexts)
      errors = getErrors handler2
      errorMessages = map errorMessage errors
      expectedMessages = take (length contexts) messages
  in property $ length errors === length expectedMessages .&&.
     sort errorMessages === sort expectedMessages

-- Property: Get warnings returns correct content
prop_get_warnings_content :: [String] -> [String] -> Property
prop_get_warnings_content messages contexts =
  not (null messages) && length messages <= 3 && length contexts <= 3 &&
  all (\m -> length m <= 30) messages && all (\c -> length c <= 30) contexts ==>
  let handler1 = createErrorHandler
      handler2 = foldr (\(msg, ctx) h -> addWarning msg ctx h) handler1 (zip messages contexts)
      warnings = getWarnings handler2
      warningMessages = map errorMessage warnings
      expectedMessages = take (length contexts) messages
  in property $ length warnings === length expectedMessages .&&.
     sort warningMessages === sort expectedMessages

-- Property: Error formatting consistency
prop_error_formatting_consistency :: String -> String -> Property
prop_error_formatting_consistency message context =
  length message <= 80 && length context <= 80 ==>
  let handler = addError message context createErrorHandler
      errors = getErrors handler
      formatted = case errors of
                    [] -> ""
                    (e:_) -> formatError e
  in property $ message `isInfixOf` formatted .&&. context `isInfixOf` formatted

-- Property: Warning formatting consistency
prop_warning_formatting_consistency :: String -> String -> Property
prop_warning_formatting_consistency message context =
  length message <= 80 && length context <= 80 ==>
  let handler = addWarning message context createErrorHandler
      warnings = getWarnings handler
      formatted = case warnings of
                    [] -> ""
                    (w:_) -> formatWarning w
  in property $ message `isInfixOf` formatted .&&. context `isInfixOf` formatted

-- Property: Error handler isolation
prop_error_handler_isolation :: String -> String -> Property
prop_error_handler_isolation message1 message2 =
  length message1 <= 50 && length message2 <= 50 && message1 /= message2 ==>
  let handler1 = addError message1 "context1" createErrorHandler
      handler2 = addError message2 "context2" createErrorHandler
      errors1 = getErrors handler1
      errors2 = getErrors handler2
  in property $ errorCount handler1 === 1 .&&. errorCount handler2 === 1 .&&.
     length errors1 === 1 .&&. length errors2 === 1 .&&.
     errorMessage (head errors1) === message1 .&&.
     errorMessage (head errors2) === message2

-- Property: Large number of errors handling
prop_large_errors_handling :: Int -> String -> Property
prop_large_errors_handling count baseMessage =
  count > 0 && count <= 100 && length baseMessage <= 20 ==>
  let messages = map (\i -> baseMessage ++ show i) [1..count]
      handler = foldr (\msg h -> addError msg "context" h) createErrorHandler messages
  in property $ errorCount handler === count .&&. 
     length (getErrors handler) === count

-- Property: Error and warning separation
prop_error_warning_separation :: String -> String -> Property
prop_error_warning_separation errorMsg warningMsg =
  length errorMsg <= 50 && length warningMsg <= 50 && errorMsg /= warningMsg ==>
  let handler = createErrorHandler
      handler2 = addError errorMsg "error context" handler
      handler3 = addWarning warningMsg "warning context" handler2
      errors = getErrors handler3
      warnings = getWarnings handler3
  in property $ length errors === 1 .&&. length warnings === 1 .&&.
     errorMessage (head errors) === errorMsg .&&.
     errorMessage (head warnings) === warningMsg

-- Property: Clear errors doesn't affect warnings
prop_clear_errors_preserves_warnings :: [String] -> [String] -> Property
prop_clear_errors_preserves_warnings errorMessages warningMessages =
  length errorMessages <= 3 && length warningMessages <= 3 &&
  all (\m -> length m <= 30) (errorMessages ++ warningMessages) ==>
  let handler1 = createErrorHandler
      handler2 = foldr (\msg h -> addError msg "error context" h) handler1 errorMessages
      handler3 = foldr (\msg h -> addWarning msg "warning context" h) handler2 warningMessages
      handler4 = clearErrors handler3
  in property $ not (hasErrors handler4) .&&. hasWarnings handler4 .&&.
     errorCount handler4 === 0 .&&. warningCount handler4 === length warningMessages

-- Property: Clear warnings doesn't affect errors
prop_clear_warnings_preserves_errors :: [String] -> [String] -> Property
prop_clear_warnings_preserves_errors errorMessages warningMessages =
  length errorMessages <= 3 && length warningMessages <= 3 &&
  all (\m -> length m <= 30) (errorMessages ++ warningMessages) ==>
  let handler1 = createErrorHandler
      handler2 = foldr (\msg h -> addError msg "error context" h) handler1 errorMessages
      handler3 = foldr (\msg h -> addWarning msg "warning context" h) handler2 warningMessages
      handler4 = clearWarnings handler3
  in property $ hasErrors handler4 .&&. not (hasWarnings handler4) .&&.
     errorCount handler4 === length errorMessages .&&. warningCount handler4 === 0

-- Property: Empty message handling
prop_empty_message_handling :: Property
prop_empty_message_handling =
  let handler1 = addError "" "context" createErrorHandler
      handler2 = addWarning "" "context" handler1
      errors = getErrors handler2
      warnings = getWarnings handler2
  in property $ length errors === 1 .&&. length warnings === 1 .&&.
     errorMessage (head errors) === "" .&&.
     errorMessage (head warnings) === ""

tests :: TestTree
tests = testGroup "New Error Handler Core QuickCheck Tests"
  [ fastProperty "create error handler consistency" prop_create_error_handler_consistency
  , fastProperty "add and detect errors" prop_add_detect_errors
  , fastProperty "add and detect warnings" prop_add_detect_warnings
  , fastProperty "multiple errors accumulate correctly" prop_multiple_errors_accumulate
  , fastProperty "multiple warnings accumulate correctly" prop_multiple_warnings_accumulate
  , fastProperty "mixed errors and warnings" prop_mixed_errors_warnings
  , fastProperty "clear errors functionality" prop_clear_errors
  , fastProperty "clear warnings functionality" prop_clear_warnings
  , fastProperty "get errors returns correct content" prop_get_errors_content
  , fastProperty "get warnings returns correct content" prop_get_warnings_content
  ]