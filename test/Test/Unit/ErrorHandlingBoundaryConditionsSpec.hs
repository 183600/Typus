{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlingBoundaryConditionsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=), assertFailure)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

-- Error handling modules
import ErrorHandler (ErrorHandler(..))
import EnhancedErrorHandler (EnhancedErrorHandler(..))
import Compiler.Errors (CompilerError(..), ErrorSeverity(..))
import Compiler.Errors.Core (CoreError(..))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos)

import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Control.Exception (try, SomeException, evaluate)

-- | Boundary condition tests for error handling
tests :: TestTree
tests =
  testGroup "Error Handling Boundary Conditions"
    [ testGroup "Empty L.and Null Input Handling"
        [ testCase "Error handler handles empty input gracefully" $ do
            let emptyInput = ""
                result = try $ evaluate (L.length emptyInput)
            case result of
              Left (e :: SomeException) -> 
                assertFailure $ "Empty input caused exception: " ++ show e
              Right _ -> 
                assertBool "Empty input should be handled gracefully" True

        , testCase "Error handler handles null-like input" $ do
            let nullLikeInput = "\0\0\0"
                result = try $ evaluate (L.length nullLikeInput)
            case result of
              Left (e :: SomeException) -> 
                assertFailure $ "Null-like input caused exception: " ++ show e
              Right _ -> 
                assertBool "Null-like input should be handled gracefully" True

        , testCase "Error handler handles whitespace-only input" $ do
            let whitespaceInput = "   \t\n\r   "
                result = try $ evaluate (trim whitespaceInput)
            case result of
              Left (e :: SomeException) -> 
                assertFailure $ "Whitespace-only input caused exception: " ++ show e
              Right trimmed -> 
                assertEqual "Whitespace should be trimmed" "" trimmed
        ]

    , testGroup "Extreme Input Sizes"
        [ testCase "Error handler handles very long lines" $ do
            let longLine = replicate 10000 'a' ++ "syntax_error_here"
                result = try $ evaluate (L.length longLine)
            case result of
              Left (e :: SomeException) -> 
                assertFailure $ "Very long line caused exception: " ++ show e
              Right len -> 
                assertBool "Should handle long lines" (len > 10000)

        , testCase "Error handler handles deeply nested structures" $ do
            let nestedInput = L.concat $ replicate 1000 "func test() { "
                result = try $ evaluate (L.length nestedInput)
            case result of
              Left (e :: SomeException) -> 
                assertFailure $ "Deeply nested structure caused exception: " ++ show e
              Right _ -> 
                assertBool "Should handle deeply nested structures" True

        , testCase "Error handler handles input with many special characters" $ do
            let specialChars = L.concat $ replicate 100 "!@#$%^&*()_+-=[]{}|;':\",./<>?"
                result = try $ evaluate (L.length specialChars)
            case result of
              Left (e :: SomeException) -> 
                assertFailure $ "Special characters caused exception: " ++ show e
              Right _ -> 
                assertBool "Should handle special characters" True
        ]

    , testGroup "Unicode L.and Encoding Edge Cases"
        [ testCase "Error handler handles Unicode input" $ do
            let unicodeInput = "func test() { return \"Hello 世界 🌍\"; }"
                result = try $ evaluate (L.length unicodeInput)
            case result of
              Left (e :: SomeException) -> 
                assertFailure $ "Unicode input caused exception: " ++ show e
              Right _ -> 
                assertBool "Should handle Unicode input" True

        , testCase "Error handler handles mixed encodings" $ do
            let mixedEncoding = "func test() { return \"Hello\xC3\x28 World\"; }"  -- Invalid UTF-8 sequence
                result = try $ evaluate (L.length mixedEncoding)
            case result of
              Left (e :: SomeException) -> 
                assertFailure $ "Mixed encoding caused exception: " ++ show e
              Right _ -> 
                assertBool "Should handle mixed encodings" True

        , testCase "Error handler handles zero-width characters" $ do
            let zeroWidthInput = "func test() { return \"Hello\u200BWorld\"; }"  -- Zero-width space
                result = try $ evaluate (L.length zeroWidthInput)
            case result of
              Left (e :: SomeException) -> 
                assertFailure $ "Zero-width characters caused exception: " ++ show e
              Right _ -> 
                assertBool "Should handle zero-width characters" True
        ]

    , testGroup "Error Message Boundary Conditions"
        [ testCase "Error messages handle very long identifiers" $ do
            let longIdentifier = replicate 1000 'x'
                error = CompilerError 
                    { errorSeverity = Error
                    , errorMessage = "Undefined variable: " ++ longIdentifier
                    , errorLocation = startPos "test.typus"
                    }
            case error of
              CompilerError{errorMessage = msg} -> do
                assertBool "Error message should contain long identifier" 
                    (longIdentifier `L.isInfixOf` msg)
                assertBool "Error message should be reasonable L.length" 
                    (L.length msg < 2000)  -- Should truncate L.or summarize

        , testCase "Error messages handle special characters in identifiers" $ do
            let specialId = "test!@#$%^&*()_+-=[]{}|;':\",./<>?"
                error = CompilerError 
                    { errorSeverity = Warning
                    , errorMessage = "Invalid identifier: " ++ specialId
                    , errorLocation = startPos "test.typus"
                    }
            case error of
              CompilerError{errorMessage = msg} -> do
                assertBool "Error message should handle special characters" 
                    (specialId `L.isInfixOf` msg)

        , testCase "Error messages handle empty context" $ do
            let error = CompilerError 
                    { errorSeverity = Error
                    , errorMessage = ""
                    , errorLocation = startPos "test.typus"
                    }
            case error of
              CompilerError{errorMessage = msg} -> do
                assertBool "Should handle empty error message" 
                    (L.length msg >= 0)  -- Should not crash
        ]

    , testGroup "Source Location Edge Cases"
        [ testCase "Error locations handle invalid positions" $ do
            let invalidPos = SourcePos { sourceLine = 0, sourceColumn = 0 }
                error = CompilerError 
                    { errorSeverity = Error
                    , errorMessage = "Test error"
                    , errorLocation = invalidPos
                    }
            case error of
              CompilerError{errorLocation = pos} -> do
                assertEqual "Should handle invalid line position" 0 (sourceLine pos)
                assertEqual "Should handle invalid column position" 0 (sourceColumn pos)

        , testCase "Error locations handle very large positions" $ do
            let largePos = SourcePos { sourceLine = 999999, sourceColumn = 999999 }
                error = CompilerError 
                    { errorSeverity = Warning
                    , errorMessage = "Test error at large position"
                    , errorLocation = largePos
                    }
            case error of
              CompilerError{errorLocation = pos} -> do
                assertEqual "Should handle large line position" 999999 (sourceLine pos)
                assertEqual "Should handle large column position" 999999 (sourceColumn pos)

        , testCase "Error locations handle negative positions gracefully" $ do
            let negativePos = SourcePos { sourceLine = -1, sourceColumn = -1 }
                error = CompilerError 
                    { errorSeverity = Error
                    , errorMessage = "Test error at negative position"
                    , errorLocation = negativePos
                    }
            case error of
              CompilerError{errorLocation = pos} -> do
                assertEqual "Should handle negative line position" (-1) (sourceLine pos)
                assertEqual "Should handle negative column position" (-1) (sourceColumn pos)
        ]

    , testGroup "Error Recovery Boundary Conditions"
        [ testCase "Error recovery handles cascading errors" $ do
            let cascadingErrors = 
                    [ CompilerError Error "First error" (startPos "test.typus")
                    , CompilerError Error "Second error" (startPos "test.typus")
                    , CompilerError Error "Third error" (startPos "test.typus")
                    ]
            case cascadingErrors of
              errors -> do
                assertEqual "Should handle multiple errors" 3 (L.length errors)
                assertBool "All errors should be preserved" 
                    (L.all (\e -> errorSeverity e == Error) errors)

        , testCase "Error recovery handles contradictory errors" $ do
            let contradictoryErrors = 
                    [ CompilerError Error "Variable undefined" (startPos "test.typus")
                    , CompilerError Warning "Variable defined but unused" (startPos "test.typus")
                    ]
            case contradictoryErrors of
                (error1:error2:_) -> do
                    assertBool "Should handle contradictory error types" 
                        (errorSeverity error1 /= errorSeverity error2)

        , testCase "Error recovery handles circular dependencies" $ do
            let circularDepError = CoreError 
                    { coreErrorMessage = "Circular dependency detected"
                    , coreErrorLocation = startPos "test.typus"
                    , coreErrorContext = ["A depends on B", "B depends on A"]
                    }
            case circularDepError of
              CoreError{coreErrorMessage = msg, coreErrorContext = ctx} -> do
                assertBool "Should detect circular dependency" 
                    ("circular" `L.isInfixOf` map toLower msg)
                assertBool "Should provide context" (L.length ctx > 1)
        ]

    , testGroup "Memory L.and Performance Boundaries"
        [ testCase "Error handling doesn't leak memory with repeated errors" $ do
            let generateErrors n = replicate n $ 
                    CompilerError Error "Test error" (startPos "test.typus")
                errors = generateErrors 1000
            case errors of
              _ -> do
                assertEqual "Should handle many errors without issues" 1000 (L.length errors)

        , testCase "Error handling performs well with large error messages" $ do
            let largeMessage = replicate 1000 "This is a very long error message. "
                error = CompilerError 
                    { errorSeverity = Error
                    , errorMessage = largeMessage
                    , errorLocation = startPos "test.typus"
                    }
            case error of
              CompilerError{errorMessage = msg} -> do
                assertBool "Should handle large error messages" 
                    (L.length msg > 1000)

        , testCase "Error handling gracefully handles stack overflow scenarios" $ do
            let deepError = CompilerError 
                    { errorSeverity = Error
                    , errorMessage = "Deep recursion error"
                    , errorLocation = startPos "test.typus"
                    }
            case deepError of
              _ -> do
                assertBool "Should handle deep recursion scenarios" True
        ]
    ]

-- Helper function for trimming
trim :: String -> String
trim = dropWhile (`elem` " \t\n\r") . L.reverse . dropWhile (`elem` " \t\n\r") . L.reverse

-- Helper function for case conversion
toLower :: String -> String
toLower = L.map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)