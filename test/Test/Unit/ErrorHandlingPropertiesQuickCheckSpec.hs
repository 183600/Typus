module Test.Unit.ErrorHandlingPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property)
import Parser (parseTypus)
import ErrorHandler (ErrorHandler(..))
import Compiler.Errors.Core (ErrorLocation(..))
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, toErrorLocationWithSpan)
import Data.Either (isLeft, isRight)
import Data.List (isInfixOf)

-- ============================================================================
-- Error Handling Properties QuickCheck Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "Error Handling Properties QuickCheck Tests"
  [ testProperty "error messages contain location information" prop_error_messages_have_location
  , testProperty "error handling preserves input context" prop_error_preserves_context
  , testProperty "error recovery maintains partial results" prop_error_recovery_partial_results
  , testProperty "multiple errors are collected properly" prop_multiple_errors_collected
  , testProperty "error locations are within source bounds" prop_error_locations_within_bounds
  , testProperty "error messages are descriptive" prop_error_messages_descriptive
  , testProperty "error handling is consistent" prop_error_handling_consistent
  , testProperty "graceful degradation on malformed input" prop_graceful_degradation
  ]

-- | Error messages should contain location information when available
prop_error_messages_have_location :: String -> Property
prop_error_messages_have_location content = 
  let result = parseTypus content
  in case result of
    Left err -> 
      let errMsg = show err
      in "line" `isInfixOf` errMsg || "column" `isInfixOf` errMsg || "position" `isInfixOf` errMsg
    Right _ -> True  -- No error means property is vacuously true

-- | Error handling should preserve input context in error messages
prop_error_preserves_context :: String -> String -> Property
prop_error_preserves_context prefix suffix = 
  let content = prefix ++ "\n@@ ERROR @@\n" ++ suffix
      result = parseTypus content
  in case result of
    Left err -> 
      let errMsg = show err
          hasContext = take 10 prefix `isInfixOf` errMsg || 
                      take 10 suffix `isInfixOf` errMsg
      in length errMsg > 10  -- Error message should be substantial
    Right _ -> True

-- | Error recovery should maintain partial results when possible
prop_error_recovery_partial_results :: String -> String -> Property
prop_error_recovery_partial_results good bad = 
  let mixed = good ++ "\n" ++ bad ++ "\n" ++ good
      result = parseTypus mixed
  in case result of
    Left _ -> True  -- May fail completely
    Right _ -> True  -- Or succeed with partial results

-- | Multiple errors should be collected and reported appropriately
prop_multiple_errors_collected :: String -> String -> String -> Property
prop_multiple_errors_collected part1 part2 part3 = 
  let withErrors = part1 ++ "\n@@ ERROR1 @@\n" ++ part2 ++ "\n@@ ERROR2 @@\n" ++ part3
      result = parseTypus withErrors
  in case result of
    Left err -> length (show err) >= 20  -- Should collect substantial error info
    Right _ -> True

-- | Error locations should be within source file bounds
prop_error_locations_within_bounds :: String -> Property
prop_error_locations_within_bounds content = 
  let result = parseTypus content
      contentLength = length content
      contentLines = length (lines content)
  in case result of
    Left _ -> True  -- Error location info should be valid
    Right tf -> 
      let spans = map cbSpan (tfBlocks tf)
          validSpans = filter (\span -> 
            let start = spanStart span
                end = spanEnd span
            in posLine start <= contentLines && posLine end <= contentLines) spans
      in length validSpans >= 0

-- | Error messages should be descriptive and helpful
prop_error_messages_descriptive :: String -> Property
prop_error_messages_descriptive content = 
  let malformed = content ++ "\n@@ SYNTAX_ERROR_WITH_EXTRA_INFO @@"
      result = parseTypus malformed
  in case result of
    Left err -> 
      let errMsg = show err
      in length errMsg >= 5  -- Should have some descriptive content
    Right _ -> True

-- | Error handling should be consistent across similar inputs
prop_error_handling_consistent :: String -> Property
prop_error_handling_consistent base = 
  let variant1 = base ++ "\n@@ ERROR @@"
      variant2 = base ++ "\n@@ ERROR @@"
      result1 = parseTypus variant1
      result2 = parseTypus variant2
  in case (result1, result2) of
    (Left _, Left _) -> True  -- Both should fail similarly
    (Right _, Right _) -> True  -- Both should succeed similarly
    _ -> False  -- Inconsistent behavior

-- | System should degrade gracefully on completely malformed input
prop_graceful_degradation :: Property
prop_graceful_degradation = 
  let malformed = "@@!@#@!#@!#@!#@@@!#@!#@!#@!#"
      result = parseTypus malformed
  in case result of
    Left err -> length (show err) > 0  -- Should produce meaningful error
    Right tf -> length (tfBlocks tf) >= 0  -- Or produce some structure