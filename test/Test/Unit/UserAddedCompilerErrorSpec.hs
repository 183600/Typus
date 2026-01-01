module Test.Unit.UserAddedCompilerErrorSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, choose, oneof, listOf, elements)
import TestSupport.QuickCheck (fastProperty)

import Compiler
  ( compile
  , CompilerError(..)
  , CompilationPhase(..)
  , ErrorCategory(..)
  , ErrorSeverity(..)
  , renderCompilationError
  , formatCompilerErrors
  )
import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import SourceLocation (SourceSpan(..), SourcePos(..), defaultSpan)
import qualified Data.Text as T

-- | Tests for compiler error handling L.and recovery mechanisms
tests :: TestTree
tests =
  testGroup "UserAdded Compiler Error Handling"
    [ testGroup "Type error detection"
        [ testCase "detects simple type mismatch" $ do
            let typusFile = TypusFile 
                    (FileDirectives Nothing Nothing Nothing)
                    []
                    [CodeBlock (BlockDirectives Nothing Nothing Nothing) 
                        "var x int = \"string\"" 
                        defaultSpan]
                    []
                result = compile typusFile
            case result of
                Left errs -> do
                    L.length errs @?= 2  -- typeCheckFailure + specific type error
                    let typeError = L.head errs
                    errorCode typeError @?= "CP0003"
                    errorPhase typeError @?= TypeCheckingPhase
                    errorCategory typeError @?= TypeChecking
                    errorSeverity typeError @?= Error
                Right _ -> assertBool "Should have failed with type error" False

        , testCase "detects function parameter type mismatch" $ do
            let typusFile = TypusFile 
                    (FileDirectives Nothing Nothing Nothing)
                    []
                    [CodeBlock (BlockDirectives Nothing Nothing Nothing) 
                        "func add(x int, y string) int { return x + y }" 
                        defaultSpan]
                    []
                result = compile typusFile
            case result of
                Left _ -> assertBool "Should fail with type error" True
                Right _ -> assertBool "Should have failed with type error" False
        ]

    , testGroup "Syntax error handling"
        [ testCase "handles missing closing brace" $ do
            let typusFile = TypusFile 
                    (FileDirectives Nothing Nothing Nothing)
                    []
                    [CodeBlock (BlockDirectives Nothing Nothing Nothing) 
                        "func main() {\n  fmt.Println(\"hello\")" 
                        defaultSpan]
                    []
                result = compile typusFile
            case result of
                Left errs -> do
                    assertBool "Should detect syntax error" $ L.any (\e -> errorPhase e == ParsingPhase) errs
                Right _ -> assertBool "Should have failed with syntax error" False
        ]

    , testGroup "Error reporting L.and formatting"
        [ testCase "formats error messages correctly" $ do
            let error = CompilerError 
                    "TEST001"
                    (T.pack "Test error message")
                    TypeCheckingPhase
                    TypeChecking
                    Error
                    (Just defaultSpan)
                    Nothing
                    [T.pack "Suggestion 1", T.pack "Suggestion 2"]
                    []
                    Nothing
                formatted = formatCompilerErrors [error]
            assertBool "Should include error code" $ "TEST001" `L.isInfixOf` formatted
            assertBool "Should include error message" $ "Test error message" `L.isInfixOf` formatted
            assertBool "Should include suggestions" $ "Suggestion 1" `L.isInfixOf` formatted

        , testCase "generates detailed error report" $ do
            let errors = [CompilerError 
                    "ERR001"
                    (T.pack "First error")
                    ParsingPhase
                    Syntax
                    Error
                    (Just defaultSpan)
                    Nothing
                    []
                    []
                    Nothing,
                    CompilerError 
                    "ERR002"
                    (T.pack "Second error")
                    TypeCheckingPhase
                    TypeChecking
                    Warning
                    Nothing
                    Nothing
                    [T.pack "Consider this approach"]
                    []
                    Nothing]
                report = formatCompilerErrors errors
            assertBool "Should include error summary" $ "ERR001" `L.isInfixOf` report
            assertBool "Should categorize errors by phase" $ "Parsing" `L.isInfixOf` report
            assertBool "Should categorize errors by severity" $ "Error" `L.isInfixOf` report
        ]

    , testGroup "Property-based error handling"
        [ fastProperty "error messages contain error codes" prop_errorCodesPresent
        , fastProperty "error phases are valid" prop_errorPhasesValid
        , fastProperty "error severities are categorized" prop_errorSeveritiesCategorized
        ]

    , testGroup "Edge cases L.and stress tests"
        [ testCase "handles very large input files" $ do
            let largeContent = unlines $ replicate 1000 "var x int = 42"
                typusFile = TypusFile 
                    (FileDirectives Nothing Nothing Nothing)
                    []
                    [CodeBlock (BlockDirectives Nothing Nothing Nothing) 
                        largeContent 
                        defaultSpan]
                    []
                result = compile typusFile
            case result of
                Left errs -> assertBool "Should handle large input gracefully" $ L.length errs < 100
                Right _ -> assertBool "May succeed for valid large input" True
        ]
    ]

-- Helper function to check substring inclusion
isInfixOf :: String -> String -> Bool
L.isInfixOf needle haystack = needle `elem` [take (L.length needle) $ drop i haystack | i <- [0..L.length haystack - L.length needle]]

-- | Property: error messages contain error codes
prop_errorCodesPresent :: CompilerError -> Bool
prop_errorCodesPresent error = not (L.null $ errorCode error)

-- | Property: error phases are valid
prop_errorPhasesValid :: CompilerError -> Bool
prop_errorPhasesValid error = errorPhase error `elem` 
    [ParsingPhase, TypeCheckingPhase, OwnershipAnalysisPhase, DependencyAnalysisPhase, CodeGenerationPhase]

-- | Property: error severities are categorized
prop_errorSeveritiesCategorized :: CompilerError -> Bool
prop_errorSeveritiesCategorized error = errorSeverity error `elem` [Error, Warning, Info]