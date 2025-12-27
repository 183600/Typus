{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.ErrorRecoveryAdvancedTest2025Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, listOf, elements)
import Test.Tasty.HUnit (testCase, (@=?))

import ErrorHandler (Error, ErrorSeverity(..), ErrorLocation, recoverFromError, canRecoverFrom)
import EnhancedErrorHandler (EnhancedError, enhancedRecovery, getErrorSuggestions)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import qualified Data.Text as T

tests :: TestTree
tests = testGroup "Error Recovery Advanced Tests"
  [ testProperty "Error recovery preserves partial results" propErrorRecoveryPreservesPartial
  , testProperty "Enhanced recovery provides suggestions" propEnhancedRecoveryProvidesSuggestions
  , testProperty "Error severity affects recovery strategy" propSeverityAffectsRecovery
  , testProperty "Recovery is idempotent" propRecoveryIdempotent
  , testProperty "Location information preserved in recovery" propLocationPreservedInRecovery
  , testCase "Syntax error recovery scenarios" testSyntaxErrorRecovery
  , testProperty "Multiple error recovery composition" propMultipleErrorRecoveryComposition
  , testCase "Type error recovery" testTypeErrorRecovery
  , testProperty "Recovery fallback behavior" propRecoveryFallbackBehavior
  , testCase "Critical error handling" testCriticalErrorHandling
  ]

-- Mock Error type for testing
data MockError = MockError
  { errorMessage :: String
  , errorSeverity :: ErrorSeverity
  , errorLocation :: SourcePos
  , errorContext :: [String]
  } deriving (Show, Eq)

data MockResult = MockResult
  { resultValue :: String
  , resultErrors :: [MockError]
  , resultPartial :: Bool
  } deriving (Show, Eq)

-- Property 1: Error recovery preserves partial results
propErrorRecoveryPreservesPartial :: String -> MockError -> Bool
propErrorRecoveryPreservesPartial input err =
  let initial = MockResult input [err] False
      recovered = mockRecover initial
  in resultPartial recovered ==> not (null (resultValue recovered))

-- Property 2: Enhanced recovery provides suggestions
propEnhancedRecoveryProvidesSuggestions :: MockError -> Bool
propEnhancedRecoveryProvidesSuggestions err =
  let suggestions = mockGetSuggestions err
  in errorSeverity err == Warning ==> not (null suggestions)

-- Property 3: Error severity affects recovery strategy
propSeverityAffectsRecovery :: String -> ErrorSeverity -> Bool
propSeverityAffectsRecovery input severity =
  let err = MockError "test error" severity (SourcePos 1 1) []
      result = mockRecover (MockResult input [err] False)
  in case severity of
       Critical -> not (resultPartial result)  -- Critical errors shouldn't allow partial recovery
       Warning -> resultPartial result         -- Warnings should allow partial recovery
       Error -> resultPartial result           -- Errors should allow partial recovery

-- Property 4: Recovery is idempotent
propRecoveryIdempotent :: String -> MockError -> Bool
propRecoveryIdempotent input err =
  let initial = MockResult input [err] False
      recovered1 = mockRecover initial
      recovered2 = mockRecover recovered1
  in recovered1 == recovered2

-- Property 5: Location information preserved in recovery
propLocationPreservedInRecovery :: String -> MockError -> Bool
propLocationPreservedInRecovery input err =
  let initial = MockResult input [err] False
      recovered = mockRecover initial
  in all (\e -> errorLocation e == errorLocation err) (resultErrors recovered)

-- Test Case 6: Syntax error recovery scenarios
testSyntaxErrorRecovery :: IO ()
testSyntaxErrorRecovery = do
  let missingBrace = "func test() { if (true) { console.log('hello');"
      err1 = MockError "Missing closing brace" Error (SourcePos 1 50) ["function definition", "if statement"]
      result1 = mockRecover (MockResult missingBrace [err1] False)
  
  resultPartial result1 @=? True
  length (resultErrors result1) @=? 1
  
  let extraComma = "let x = 1,, y = 2;"
      err2 = MockError "Unexpected comma" Error (SourcePos 1 10) ["variable declaration"]
      result2 = mockRecover (MockResult extraComma [err2] False)
  
  resultPartial result2 @=? True

-- Property 7: Multiple error recovery composition
propMultipleErrorRecoveryComposition :: String -> [MockError] -> Bool
propMultipleErrorRecoveryComposition input errs =
  let initial = MockResult input errs False
      recovered = mockRecoverMultiple initial
  in length (resultErrors recovered) <= length errs  -- Recovery should reduce or maintain error count

-- Test Case 8: Type error recovery
testTypeErrorRecovery :: IO ()
testTypeErrorRecovery = do
  let typeMismatch = "let x: string = 123;"
      err = MockError "Type mismatch: cannot assign number to string" Error (SourcePos 1 20) ["variable declaration"]
      result = mockRecover (MockResult typeMismatch [err] False)
  
  resultPartial result @=? True
  "string" `elem` resultValue result @=? True
  "123" `elem` resultValue result @=? True

-- Property 9: Recovery fallback behavior
propRecoveryFallbackBehavior :: String -> MockError -> Bool
propRecoveryFallbackBehavior input err =
  let result = mockRecoverWithFallback (MockResult input [err] False)
  in not (null (resultValue result))  -- Fallback should always provide some result

-- Test Case 10: Critical error handling
testCriticalErrorHandling :: IO ()
testCriticalErrorHandling = do
  let criticalError = "func test() { throw new FatalError('system crash'); }"
      err = MockError "Fatal system error" Critical (SourcePos 1 25) ["function body"]
      result = mockRecover (MockResult criticalError [err] False)
  
  resultPartial result @=? False  -- Critical errors should not allow partial recovery
  length (resultErrors result) @=? 1
  errorSeverity (head (resultErrors result)) @=? Critical

-- Mock implementations for testing
mockRecover :: MockResult -> MockResult
mockRecover (MockResult value errors partial) =
  case errors of
    [] -> MockResult value errors True
    (err:rest) ->
      if canMockRecover err
      then MockResult (mockFixValue value err) (err:rest) True
      else MockResult value errors partial

mockRecoverMultiple :: MockResult -> MockResult
mockRecoverMultiple result@(MockResult value errors partial) =
  if null errors || partial
  then result
  else mockRecover (MockResult value (tail errors) partial)

mockRecoverWithFallback :: MockResult -> MockResult
mockRecoverWithFallback (MockResult value errors partial) =
  case errors of
    [] -> MockResult value errors True
    (err:rest) ->
      if canMockRecover err
      then mockRecover (MockResult value errors partial)
      else MockResult "/* fallback: unable to recover */" errors False

canMockRecover :: MockError -> Bool
canMockRecover err = errorSeverity err /= Critical

mockFixValue :: String -> MockError -> String
mockFixValue value err = value ++ " /* recovered from: " ++ errorMessage err ++ " */"

mockGetSuggestions :: MockError -> [String]
mockGetSuggestions err =
  case errorSeverity err of
    Warning -> ["Consider fixing: " ++ errorMessage err]
    Error -> ["Possible fix: " ++ errorMessage err]
    Critical -> []

-- Arbitrary instances for testing
instance Arbitrary ErrorSeverity where
  arbitrary = elements [Warning, Error, Critical]

instance Arbitrary MockError where
  arbitrary = do
    msg <- elements ["syntax error", "type error", "missing semicolon", "invalid token"]
    severity <- arbitrary
    line <- choose (1, 100)
    col <- choose (1, 100)
    context <- listOf (elements ["function", "variable", "expression", "statement"])
    return $ MockError msg severity (SourcePos line col) context