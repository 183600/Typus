{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.CoreIntegrationSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils (trim, splitBy, removeLineComments)
import SourceLocation (SourcePos(..), SourceSpan(..), spanTo, startPos, posAfter, mergeSpans, locatedAt)
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..))
import ErrorHandler (Error(..), ErrorSeverity(..), ErrorLocation(..), defaultErrorHandler, handleError, getErrors)
import Ownership (Ownership(..), defaultOwnershipPolicy, checkOwnership, transferOwnership, hasOwnership)
import Data.Either (isLeft, isRight)
import Data.Map (Map, empty, insert)
import Data.List (isInfixOf)

-- Integration tests for core modules

-- Test properties for module integration

-- | Parser should handle directives correctly and ErrorHandler should track errors
prop_parser_errorhandler_integration :: String -> Property
prop_parser_errorhandler_integration s = 
  let input = "// @ownership: true\nfunction " ++ s ++ "() {\n  return 42;\n}"
      parseResult = parseTypus input "test.typus"
      handler = case parseResult of
        Left err -> handleError defaultErrorHandler (Error Error err NoLocation)
        Right _ -> defaultErrorHandler
  in property $ case parseResult of
    Left _ -> hasErrors handler
    Right _ -> not (hasErrors handler)

-- | SourceLocation should work with Parser error positions
prop_parser_sourcelocation_integration :: Positive Int -> String -> Property
prop_parser_sourcelocation_integration (Positive n) s = 
  let inputWithLines = unlines $ replicate n "valid line" ++ ["invalid line {"]
      parseResult = parseTypus inputWithLines "test.typus"
  in property $ case parseResult of
    Left err -> show n `isInfixOf` show err
    Right _ -> property False  -- Should have failed

-- | Utils functions should work with Parser output
prop_parser_utils_integration :: String -> Property
prop_parser_utils_integration s = 
  let input = "function " ++ s ++ "() {\n  return 42;\n}"
      parseResult = parseTypus input "test.typus"
      trimmedInput = trim input
  in property $ case parseResult of
    Right _ -> not (null trimmedInput)
    Left _ -> property True  -- Can fail, we just test that trim works

-- | Ownership should work with parsed code
prop_parser_ownership_integration :: String -> Property
prop_parser_ownership_integration s = 
  let input = "function " ++ s ++ "() {\n  let x = new Resource();\n  return x;\n}"
      parseResult = parseTypus input "test.typus"
      ownershipMap = empty :: Map String Ownership
      analysis = case parseResult of
        Right _ -> checkOwnership defaultOwnershipPolicy "x" ownershipMap
        Left _ -> undefined
  in property $ case parseResult of
    Right _ -> analysis /= undefined
    Left _ -> property True  -- Can fail, we just test integration

-- | ErrorHandler should track errors from multiple modules
prop_errorhandler_multimodule_integration :: String -> String -> Property
prop_errorhandler_multimodule_integration s1 s2 = 
  let input1 = "function " ++ s1 ++ "() {\n  return 42;\n}"
      input2 = "function " ++ s2 ++ "() {\n  return 42;\n}"
      parseResult1 = parseTypus input1 "test1.typus"
      parseResult2 = parseTypus input2 "test2.typus"
      handler = case parseResult1 of
        Left err1 -> handleError defaultErrorHandler (Error Error err1 NoLocation)
        Right _ -> defaultErrorHandler
      handler' = case parseResult2 of
        Left err2 -> handleError handler (Error Error err2 NoLocation)
        Right _ -> handler
  in property $ case (parseResult1, parseResult2) of
    (Left _, Left _) -> errorCount handler' == 2
    (Left _, Right _) -> errorCount handler' == 1
    (Right _, Left _) -> errorCount handler' == 1
    (Right _, Right _) -> errorCount handler' == 0

-- | Utils and SourceLocation should work together
prop_utils_sourcelocation_integration :: String -> Property
prop_utils_sourcelocation_integration s = 
  let linesOfCode = splitBy '\n' s
      positions = map (\i -> startPos { sourceLine = i }) [1..length linesOfCode]
      locatedValues = zipWith locatedAt positions linesOfCode
  in property $ length locatedValues == length linesOfCode

-- | Ownership and ErrorHandler should work together
prop_ownership_errorhandler_integration :: String -> Property
prop_ownership_errorhandler_integration varName = 
  let ownershipMap = empty :: Map String Ownership
      analysis = checkOwnership defaultOwnershipPolicy varName ownershipMap
      handler = case analysis of
        Left err -> handleError defaultErrorHandler (Error Warning err NoLocation)
        Right _ -> defaultErrorHandler
  in property $ errorCount handler <= 1  -- At most one error

-- Unit tests
test_parser_utils_integration :: Assertion
test_parser_utils_integration = do
  let input = "  function test() {\n    return 42;\n  }  "
  let trimmedInput = trim input
  let parseResult = parseTypus trimmedInput "test.typus"
  assertBool "parser should handle trimmed input" (isRight parseResult)

test_parser_errorhandler_integration :: Assertion
test_parser_errorhandler_integration = do
  let validInput = "function test() {\n  return 42;\n}"
  let invalidInput = "function test() {\n  return 42\n}"  -- Missing semicolon
  let validResult = parseTypus validInput "test.typus"
  let invalidResult = parseTypus invalidInput "test.typus"
  assertBool "valid input should parse" (isRight validResult)
  assertBool "invalid input should fail" (isLeft invalidResult)

test_parser_sourcelocation_integration :: Assertion
test_parser_sourcelocation_integration = do
  let input = unlines 
    [ "function test() {"
    , "  return 42;"
    , "}"
    , ""
    , "invalid line {"
    ]
  let parseResult = parseTypus input "test.typus"
  case parseResult of
    Left err -> assertBool "error should contain line number" ("4" `isInfixOf` show err)
    Right _ -> assertFailure "parsing should have failed"

test_ownership_utils_integration :: Assertion
test_ownership_utils_integration = do
  let varNames = ["x", "y", "z"]
  let ownershipMap = foldl (\m v -> insert v Owned m) empty varNames
  let hasOwnershipList = map (`hasOwnership` ownershipMap) varNames
  assertBool "all variables should have ownership" (all id hasOwnershipList)

test_errorhandler_sourcelocation_integration :: Assertion
test_errorhandler_sourcelocation_integration = do
  let span = spanTo (SourcePos 1 1) (SourcePos 1 10)
  let location = SourceLocation span
  let handler = handleError defaultErrorHandler (Error Error "Test error" location)
  let errors = getErrors handler
  assertBool "handler should track error" (hasErrors handler)
  case errors of
    [Error _ _ loc] -> assertEqual "error should have location" location loc
    _ -> assertFailure "should have exactly one error"

test_full_integration_workflow :: Assertion
test_full_integration_workflow = do
  -- 1. Parse code with Utils preprocessing
  let rawInput = "  function test() {\n    // Return value\n    return 42;\n  }  "
  let trimmedInput = trim rawInput
  let processedInput = removeLineComments trimmedInput
  
  -- 2. Parse with Parser
  let parseResult = parseTypus processedInput "test.typus"
  assertBool "parsing should succeed" (isRight parseResult)
  
  -- 3. Check ownership
  let ownershipMap = insert "result" Owned empty
  let analysis = checkOwnership defaultOwnershipPolicy "result" ownershipMap
  assertBool "ownership analysis should succeed" (analysis /= undefined)
  
  -- 4. Track any errors
  let handler = case parseResult of
    Left err -> handleError defaultErrorHandler (Error Error err NoLocation)
    Right _ -> defaultErrorHandler
  assertBool "should have no errors" (not $ hasErrors handler)

test_multimodule_error_tracking :: Assertion
test_multimodule_error_tracking = do
  let inputs = 
    [ "function test1() { return 42; }"  -- Valid
    , "function test2() { return 42"      -- Invalid (missing semicolon)
    , "function test3() { return 42; }"   -- Valid
    ]
  
  let parseResults = map (`parseTypus` "test.typus") inputs
  let handler = foldl (\h r -> case r of
        Left err -> handleError h (Error Error err NoLocation)
        Right _ -> h) defaultErrorHandler parseResults
  
  assertEqual "should have exactly one error" 1 (errorCount handler)
  assertBool "should have errors" (hasErrors handler)

test_ownership_transfer_integration :: Assertion
test_ownership_transfer_integration = do
  -- Simulate ownership transfer in parsed code
  let initialMap = insert "resource" Owned empty
  let afterTransfer = transferOwnership "resource" "owner" initialMap
  
  assertBool "resource should not have ownership after transfer" (not $ hasOwnership "resource" afterTransfer)
  assertBool "owner should have ownership after transfer" (hasOwnership "owner" afterTransfer)
  
  -- Check that this would be tracked in error handler if something went wrong
  let handler = handleError defaultErrorHandler (Error Info "Ownership transferred" NoLocation)
  assertEqual "should track info message" 1 (errorCount handler)

test_parser_directives_integration :: Assertion
test_parser_directives_integration = do
  let inputWithDirectives = unlines
    [ "// @ownership: true"
    , "// @dependent-types: false"
    , "function test() {"
    , "  return 42;"
    , "}"
    ]
  let parseResult = parseTypus inputWithDirectives "test.typus"
  assertBool "parsing with directives should succeed" (isRight parseResult)

-- Test suite
tests :: TestTree
tests = testGroup "Core Integration Tests"
  [ testProperties "QuickCheck Properties"
    [ prop_parser_errorhandler_integration
    , prop_parser_sourcelocation_integration
    , prop_ownership_errorhandler_integration
    ]
  , testCase "parser utils integration" test_parser_utils_integration
  , testCase "parser errorhandler integration" test_parser_errorhandler_integration
  , testCase "full integration workflow" test_full_integration_workflow
  ]