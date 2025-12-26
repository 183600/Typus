{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CompilerErrorHandlingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===))
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import Compiler.Errors.Core (Error(..), ErrorType(..), ErrorSeverity(..), ErrorLocation(..))
import Compiler.Error (compileError, formatError, errorContext)
import Compiler (compile, compileModule)

-- | Test suite for Compiler Error Handling
tests :: TestTree
tests = testGroup "Compiler Error Handling"
  [ testProperty "error creation preserves message" propErrorCreationPreservesMessage
  , testProperty "error severity ordering" propErrorSeverityOrdering
  , testProperty "error location contains position" propErrorLocationContainsPosition
  , testProperty "error context adds information" propErrorContextAddsInformation
  , testProperty "error formatting includes all parts" propErrorFormattingIncludesAllParts
  , testCase "syntax error creation" testSyntaxErrorCreation
  , testCase "type error creation" testTypeErrorCreation
  , testCase "semantic error creation" testSemanticErrorCreation
  , testCase "error chaining" testErrorChaining
  , testCase "error recovery" testErrorRecovery
  ]

-- | Property: error creation preserves message
propErrorCreationPreservesMessage :: String -> ErrorType -> ErrorSeverity -> Property
propErrorCreationPreservesMessage msg errorType severity =
  let error = Error
        { errorMessage = msg
        , errorType = errorType
        , errorSeverity = severity
        , errorLocation = Nothing
        , errorContext = []
        }
  in property $ errorMessage error == msg

-- | Property: error severity ordering
propErrorSeverityOrdering :: ErrorSeverity -> ErrorSeverity -> Property
propErrorSeverityOrdering sev1 sev2 =
  let ordering = compare sev1 sev2
  in property $ (ordering == EQ) == (sev1 == sev2) &&
                (ordering == LT) == (sev1 == ErrorWarning && sev2 == ErrorInfo) ||
                (sev1 == ErrorInfo && sev2 == ErrorError)

-- | Property: error location contains position
propErrorLocationContainsPosition :: Int -> Int -> Property
propErrorLocationContainsPosition line column =
  let location = ErrorLocation
        { filePath = Nothing
        , line = line
        , column = column
        , endLine = Nothing
        , endColumn = Nothing
        }
  in property $ line > 0 && column > 0

-- | Property: error context adds information
propErrorContextAddsInformation :: String -> String -> Property
propErrorContextAddsInformation baseMsg contextMsg =
  let baseError = Error
        { errorMessage = baseMsg
        , errorType = TypeError
        , errorSeverity = ErrorError
        , errorLocation = Nothing
        , errorContext = []
        }
      contextError = errorContext baseError contextMsg
  in property $ length (errorContext contextError) > length (errorContext baseError)

-- | Property: error formatting includes all parts
propErrorFormattingIncludesAllParts :: String -> String -> ErrorType -> ErrorSeverity -> Property
propErrorFormattingIncludesAllParts msg file errorType severity =
  let location = ErrorLocation
        { filePath = Just file
        , line = 10
        , column = 5
        , endLine = Nothing
        , endColumn = Nothing
        }
      error = Error
        { errorMessage = msg
        , errorType = errorType
        , errorSeverity = severity
        , errorLocation = Just location
        , errorContext = ["context1", "context2"]
        }
      formatted = formatError error
  in property $ msg `L.isInfixOf` formatted &&
                file `L.isInfixOf` formatted &&
                show errorType `L.isInfixOf` formatted &&
                show severity `L.isInfixOf` formatted

-- | Unit tests for syntax error creation
testSyntaxErrorCreation :: IO ()
testSyntaxErrorCreation = do
  let error = compileError SyntaxError "Unexpected token" Nothing
  assertEqual "error type" SyntaxError $ errorType error
  assertEqual "error message" "Unexpected token" $ errorMessage error
  assertEqual "error severity" ErrorError $ errorSeverity error

-- | Unit tests for type error creation
testTypeErrorCreation :: IO ()
testTypeErrorCreation = do
  let location = ErrorLocation Nothing 5 10 Nothing Nothing
      error = compileError TypeError "Type mismatch" (Just location)
  assertEqual "error type" TypeError $ errorType error
  assertEqual "error message" "Type mismatch" $ errorMessage error
  assertEqual "error location" (Just location) $ errorLocation error

-- | Unit tests for semantic error creation
testSemanticErrorCreation :: IO ()
testSemanticErrorCreation = do
  let error = compileError SemanticError "Undefined variable" Nothing
  assertEqual "error type" SemanticError $ errorType error
  assertEqual "error message" "Undefined variable" $ errorMessage error

-- | Unit tests for error chaining
testErrorChaining :: IO ()
testErrorChaining = do
  let baseError = compileError TypeError "Base error" Nothing
      chainedError = errorContext baseError "Additional context"
  assertEqual "chained error has more context" 1 $ length (errorContext chainedError)
  assertEqual "base message preserved" "Base error" $ errorMessage chainedError

-- | Unit tests for error recovery
testErrorRecovery :: IO ()
testErrorRecovery = do
  let input = "let x = 42 + ; y = 10"
      result = compile input
  assertBool "compilation recovers from error" $ either (const False) (const True) result

-- Helper functions and imports
import qualified Data.List as L

-- Mock compileError function
compileError :: ErrorType -> String -> Maybe ErrorLocation -> Error
compileError errType msg loc = Error
  { errorMessage = msg
  , errorType = errType
  , errorSeverity = ErrorError
  , errorLocation = loc
  , errorContext = []
  }

-- Mock errorContext function
errorContext :: Error -> String -> Error
errorContext error contextMsg = error
  { errorContext = errorContext error ++ [contextMsg]
  }

-- Mock formatError function
formatError :: Error -> String
formatError error =
  let locationStr = case errorLocation error of
        Nothing -> ""
        Just loc -> case filePath loc of
          Nothing -> show (line loc) ++ ":" ++ show (column loc)
          Just file -> file ++ ":" ++ show (line loc) ++ ":" ++ show (column loc)
  in locationStr ++ ": " ++ errorMessage error ++ " (" ++ show (errorType error) ++ ")"

-- Mock compile function
compile :: String -> Either Error String
compile input = if ";" `L.isInfixOf` input
                then Left $ compileError SyntaxError "Syntax error" Nothing
                else Right "compiled"

-- Helper function for property testing
property :: Bool -> Property
property = property' where
  property' :: Bool -> Property
  property' = id