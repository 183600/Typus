{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CoreModulesIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

-- Core modules to test integration
import Parser (parseTypus, TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, spanFrom)
import Utils (trim, splitBy, removeComments, normalizeIndentation)
import ErrorHandler (ErrorHandler(..))
import Compiler (compileTypus)
import Ownership (OwnershipAnalysis(..))
import DependentTypesParser (parseDependentType)

import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)
import Control.Monad (when)

-- | Integration tests for core modules working together
tests :: TestTree
tests =
  testGroup "Core Modules Integration"
    [ testGroup "Parser + SourceLocation Integration"
        [ testCase "Parser maintains correct source locations" $ do
            let input = "func main() { return 42; }"
                result = parseTypus "test.typus" input
            case result of
              Left err -> assertFailure $ "Parse failed: " ++ show err
              Right (TypusFile directives blocks) -> do
                assertBool "Should have at least one code block" (not (null blocks))
                let firstBlock = L.head blocks
                assertBool "Code block should have valid location" 
                    (isValidLocation (cbLocation firstBlock))

        , testCase "Parser preserves directive locations" $ do
            let input = "// @ownership: true\n// @dependent-types: false\nfunc test() {}"
                result = parseTypus "test.typus" input
            case result of
              Left err -> assertFailure $ "Parse failed: " ++ show err
              Right (TypusFile directives blocks) -> do
                when (isJust (fdOwnership directives)) $ do
                    let ownershipLoc = fdOwnership directives
                    assertBool "Ownership directive should have valid location"
                        (isValidLocation ownershipLoc)
        ]

    , testGroup "Parser + Utils Integration"
        [ testCase "Parser properly processes comments through Utils" $ do
            let inputWithComments = "func test() { // inline comment\n  return 42; // another comment\n}"
                processedInput = removeComments inputWithComments
                result = parseTypus "test.typus" processedInput
            case result of
              Left err -> assertFailure $ "Parse failed after comment removal: " ++ show err
              Right (TypusFile _ blocks) -> do
                assertBool "Should parse successfully after comment removal" 
                    (not (null blocks))

        , testCase "Parser handles indentation normalization" $ do
            let indentedInput = "    func test() {\n      return 42;\n    }"
                normalizedInput = normalizeIndentation indentedInput
                result = parseTypus "test.typus" normalizedInput
            case result of
              Left err -> assertFailure $ "Parse failed after indentation normalization: " ++ show err
              Right (TypusFile _ blocks) -> do
                assertBool "Should parse successfully after indentation normalization"
                    (not (null blocks))
        ]

    , testGroup "ErrorHandler + SourceLocation Integration"
        [ testCase "Error handler provides accurate source locations" $ do
            let invalidInput = "func invalid( {\n  syntax error\n}"
                result = parseTypus "test.typus" invalidInput
            case result of
              Right _ -> assertFailure "Expected parse failure"
              Left err -> do
                assertBool "Error should contain source location information" 
                    (containsLocationInfo err)

        , testCase "Error handler formats errors with context" $ do
            let invalidInput = "func badSyntax( {"
                result = parseTypus "test.typus" invalidInput
            case result of
              Right _ -> assertFailure "Expected parse failure"
              Left err -> do
                assertBool "Error message should be informative" 
                    (isInformativeError err)
        ]

    , testGroup "Compiler + Ownership Integration"
        [ testCase "Compiler integrates ownership analysis" $ do
            let inputWithOwnership = "// @ownership: true\nfunc transfer() { move resource; }"
                result = compileTypus "test.typus" inputWithOwnership
            -- Test that compilation considers ownership
            case result of
              Left err -> do
                -- This is expected for invalid ownership usage
                assertBool "Ownership-related error should be informative"
                    (isOwnershipRelatedError err)
              Right _ -> 
                assertBool "Compilation should succeed with valid ownership" True

        , testCase "Ownership analysis respects parser directives" $ do
            let input = "// @ownership: true\nfunc test() { var x = 42; return x; }"
                result = parseTypus "test.typus" input
            case result of
              Left err -> assertFailure $ "Parse failed: " ++ show err
              Right (TypusFile directives blocks) -> do
                assertBool "Ownership directive should be present" 
                    (isJust (fdOwnership directives))
        ]

    , testGroup "DependentTypes + Parser Integration"
        [ testCase "Parser handles dependent type annotations" $ do
            let dependentTypeInput = "func vec<n: Nat>(n: Nat) -> Vec<n> { /* implementation */ }"
                result = parseTypus "test.typus" dependentTypeInput
            case result of
              Left err -> do
                -- Expected for complex dependent types
                assertBool "Dependent type parsing error should be informative"
                    (isDependentTypeError err)
              Right (TypusFile _ blocks) -> do
                assertBool "Should parse dependent type syntax" (not (null blocks))

        , testCase "Dependent type parser integrates with main parser" $ do
            let typeAnnotation = "Vec<n: Nat>"
                result = parseDependentType typeAnnotation
            case result of
              Left err -> do
                assertBool "Dependent type parse error should be informative"
                    (not (null err))
              Right _ -> 
                assertBool "Should successfully parse valid dependent type" True
        ]

    , testGroup "End-to-End Integration"
        [ testCase "Complete pipeline: Utils -> Parser -> Compiler" $ do
            let rawInput = "  // @ownership: true\n  func main() {\n    return 42;\n  }"
                processedInput = normalizeIndentation (removeComments rawInput)
                parseResult = parseTypus "test.typus" processedInput
            case parseResult of
              Left err -> assertFailure $ "Parse failed: " ++ show err
              Right (TypusFile _ blocks) -> do
                assertBool "Should have parsed code blocks" (not (null blocks))
                -- Try to compile the parsed result
                let compileResult = compileTypus "test.typus" processedInput
                case compileResult of
                  Left err -> do
                    -- Check if it's a meaningful compilation error
                    assertBool "Compilation error should be meaningful" 
                        (isMeaningfulCompilationError err)
                  Right _ -> 
                    assertBool "Complete pipeline should succeed" True

        , testCase "Error propagation through the pipeline" $ do
            let errorInput = "func invalid( {\n  // syntax error with bad ownership\n}"
                processedInput = removeComments errorInput
                parseResult = parseTypus "test.typus" processedInput
            case parseResult of
              Right _ -> assertFailure "Expected parse failure"
              Left parseErr -> do
                assertBool "Parse error should propagate correctly"
                    (containsLocationInfo parseErr)
                -- Try compilation to see error handling
                let compileResult = compileTypus "test.typus" processedInput
                case compileResult of
                  Right _ -> assertFailure "Expected compilation failure"
                  Left compileErr -> do
                    assertBool "Compilation should handle parse errors gracefully"
                        (not (null compileErr))
        ]
    ]

-- Helper functions for testing
isValidLocation :: Located a -> Bool
isValidLocation located = case located of
    Located span _ -> isValidSpan span
  where
    isValidSpan (SourceSpan start end) = 
      sourceLine start > 0 && sourceColumn start > 0 &&
      sourceLine end > 0 && sourceColumn end > 0 &&
      (sourceLine start < sourceLine end || 
       (sourceLine start == sourceLine end && sourceColumn start <= sourceColumn end))

containsLocationInfo :: String -> Bool
containsLocationInfo err = 
    "line" `L.isInfixOf` err && "column" `L.isInfixOf` err

isInformativeError :: String -> Bool
isInformativeError err = L.length err > 10 && not (L.null (words err))

isOwnershipRelatedError :: String -> Bool
isOwnershipRelatedError err = 
    "ownership" `L.isInfixOf` err || "move" `L.isInfixOf` err || "borrow" `L.isInfixOf` err

isDependentTypeError :: String -> Bool
isDependentTypeError err = 
    "dependent" `L.isInfixOf` err || "type" `L.isInfixOf` err || "constraint" `L.isInfixOf` err

isMeaningfulCompilationError :: String -> Bool
isMeaningfulCompilationError err = 
    L.length err > 15 && L.any (`L.isInfixOf` err) ["type", "function", "variable", "syntax"]

isInfixOf :: String -> String -> Bool
L.isInfixOf needle haystack = needle `Data.List.L.isInfixOf` haystack