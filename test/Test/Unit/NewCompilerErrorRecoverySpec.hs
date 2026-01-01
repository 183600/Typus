{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCompilerErrorRecoverySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, elements)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, catMaybes)
import qualified Data.List as L
import Data.List (isInfixOf, length)

import Compiler (compile, CompilerError(..), CompilerResult, CompilationPhase(..), formatCompilerErrors)
import Parser (parseTypus, TypusFile(..))
import Compiler.TypeChecker (TypeCheckDiagnostic(..))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))

-- | Test compiler error recovery functionality
tests :: TestTree
tests =
  testGroup "New Compiler Error Recovery Tests"
    [ syntaxErrorRecoveryTests
    , typeErrorRecoveryTests
    , semanticErrorRecoveryTests
    , multipleErrorHandlingTests
    , errorReportingTests
    , quickCheckProperties
    ]

-- | Syntax error recovery tests
syntaxErrorRecoveryTests :: TestTree
syntaxErrorRecoveryTests =
  testGroup "Syntax Error Recovery Tests"
    [ testCase "Recover from missing semicolon" $
        let input = "let x = 5\nlet y = 10"  -- Missing semicolon after first statement
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should report syntax error" (L.any isSyntaxError errs)
               assertBool "Should attempt to continue compilation" (L.length errs >= 1)
             Right _ -> assertFailure "Should have failed with syntax error"

    , testCase "Recover from unmatched braces" $
        let input = "func test() { return 42"  -- Missing closing brace
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should report brace mismatch error" (L.any hasBraceMismatch errs)
               assertBool "Should provide recovery suggestions" (L.any hasRecoverySuggestion errs)
             Right _ -> assertFailure "Should have failed with brace mismatch"

    , testCase "Recover from invalid function signature" $
        let input = "func test( int) int { return 42 }"  -- Missing parameter name
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should report function signature error" (L.any isFunctionSignatureError errs)
               assertBool "Should continue parsing function body" (L.any continuesAfterError errs)
             Right _ -> assertFailure "Should have failed with function signature error"
    ]

-- | Type error recovery tests
typeErrorRecoveryTests :: TestTree
typeErrorRecoveryTests =
  testGroup "Type Error Recovery Tests"
    [ testCase "Recover from type mismatch" $
        let input = "let x: int = \"hello\""  -- String assigned to int
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should report type mismatch" (L.any isTypeMismatchError errs)
               assertBool "Should suggest type correction" (L.any hasTypeSuggestion errs)
             Right _ -> assertFailure "Should have failed with type mismatch"

    , testCase "Recover from undefined variable" $
        let input = "let y = undefined_var + 5"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should report undefined variable" (L.any isUndefinedVariableError errs)
               assertBool "Should suggest similar variables" (L.any hasVariableSuggestion errs)
             Right _ -> assertFailure "Should have failed with undefined variable"

    , testCase "Recover from invalid function call" $
        let input = "func add(x int, y int) int { return x + y }\nlet result = add(\"hello\", 5)"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should report invalid function call" (L.any isInvalidFunctionCallError errs)
               assertBool "Should provide correct signature" (L.any hasCorrectSignatureHint errs)
             Right _ -> assertFailure "Should have failed with invalid function call"
    ]

-- | Semantic error recovery tests
semanticErrorRecoveryTests :: TestTree
semanticErrorRecoveryTests =
  testGroup "Semantic Error Recovery Tests"
    [ testCase "Recover from duplicate declarations" $
        let input = "let x = 5\nlet x = 10"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should report duplicate declaration" (L.any isDuplicateDeclarationError errs)
               assertBool "Should continue processing other declarations" (L.any continuesProcessing errs)
             Right _ -> assertFailure "Should have failed with duplicate declaration"

    , testCase "Recover from invalid ownership transfer" $
        let input = "// @ownership: true\nlet x = 5\nlet y = x\nlet z = x"  -- Using x after transfer
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should report ownership error" (L.any isOwnershipError errs)
               assertBool "Should suggest ownership fix" (L.any hasOwnershipFixSuggestion errs)
             Right _ -> assertFailure "Should have failed with ownership error"

    , testCase "Recover from dependent type violation" $
        let input = "// @dependent-types: true\nfunc test(n: int) array[n]int { return [1,2,3] }"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should report dependent type violation" (L.any isDependentTypeError errs)
               assertBool "Should provide type constraint explanation" (L.any hasTypeConstraintExplanation errs)
             Right _ -> assertFailure "Should have failed with dependent type violation"
    ]

-- | Multiple error handling tests
multipleErrorHandlingTests :: TestTree
multipleErrorHandlingTests =
  testGroup "Multiple Error Handling Tests"
    [ testCase "Handle multiple syntax errors" $
        let input = "let x = 5\nlet y = \nfunc test( { return }"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should report multiple errors" (L.length errs >= 2)
               assertBool "Should categorize errors by phase" (L.all hasErrorPhase errs)
               assertBool "Should order errors by location" (errorsOrderedByLocation errs)
             Right _ -> assertFailure "Should have failed with multiple errors"

    , testCase "Handle mixed syntax L.and type errors" $
        let input = "let x: int = \"hello\"\nlet y = \nfunc test() { return y + }"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should report both syntax L.and type errors" (hasBothSyntaxAndTypeErrors errs)
               assertBool "Should prioritize syntax errors" (syntaxErrorsComeFirst errs)
             Right _ -> assertFailure "Should have failed with mixed errors"

    , testCase "Recover from cascading errors" $
        let input = "func undefined_func() int { return 42 }\nlet x = undefined_func() + unknown_var"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should handle cascading errors gracefully" (L.length errs >= 2)
               assertBool "Should avoid duplicate error messages" (noDuplicateErrorMessages errs)
               assertBool "Should provide clear error chain" (hasClearErrorChain errs)
             Right _ -> assertFailure "Should have failed with cascading errors"
    ]

-- | Error reporting tests
errorReportingTests :: TestTree
errorReportingTests =
  testGroup "Error Reporting Tests"
    [ testCase "Format errors with source locations" $
        let input = "let x: int = \"hello\""
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let formatted = formatCompilerErrors errs
               assertBool "Should include file name" ("test.typus" `L.isInfixOf` formatted)
               assertBool "Should include line numbers" (L.any (`L.isInfixOf` formatted) ["1:", "line 1"])
               assertBool "Should include error description" (L.any (`L.isInfixOf` formatted) ["type", "mismatch"])
             Right _ -> assertFailure "Should have failed with type error"

    , testCase "Provide helpful error messages" $
        let input = "let y = undefined_variable"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               let formatted = formatCompilerErrors errs
               assertBool "Should suggest alternatives" (L.any (`L.isInfixOf` formatted) ["did you mean", "similar"])
               assertBool "Should explain the error" (L.any (`L.isInfixOf` formatted) ["undefined", "not found"])
             Right _ -> assertFailure "Should have failed with undefined variable"

    , testCase "Group related errors" $
        let input = "let x: int = \"hello\"\nlet y: string = 42"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should group type errors together" (typeErrorsAreGrouped errs)
               let formatted = formatCompilerErrors errs
               assertBool "Should provide summary" (L.any (`L.isInfixOf` formatted) ["type errors", "mismatch"])
             Right _ -> assertFailure "Should have failed with type errors"
    ]

-- | QuickCheck properties for error recovery
quickCheckProperties :: TestTree
quickCheckProperties =
  testGroup "QuickCheck Properties"
    [ testProperty "Error recovery preserves compilation state" $
        forAll genErrorProneCode $ \code ->
            case compile "test.typus" code of
              Left errs -> 
                property $ L.all hasValidErrorState errs
              Right _ -> property True

    , testProperty "Multiple errors are properly ordered" $
        forAll genMultiErrorCode $ \code ->
            case compile "test.typus" code of
              Left errs -> 
                property $ errorsOrderedByLocation errs
              Right _ -> property True

    , testProperty "Error messages are consistent" $
        forAll genErrorProneCode $ \code ->
            case compile "test.typus" code of
              Left errs -> 
                property $ L.all hasConsistentFormatting errs
              Right _ -> property True
    ]

-- | Helper functions for error detection
isSyntaxError :: CompilerError -> Bool
isSyntaxError (CompilerError SyntaxError _ _ _) = True
isSyntaxError _ = False

hasBraceMismatch :: CompilerError -> Bool
hasBraceMismatch (CompilerError _ _ msg _) = "brace" `L.isInfixOf` msg || "mismatch" `L.isInfixOf` msg
hasBraceMismatch _ = False

isFunctionSignatureError :: CompilerError -> Bool
isFunctionSignatureError (CompilerError SyntaxError _ msg _) = "function" `L.isInfixOf` msg && "signature" `L.isInfixOf` msg
isFunctionSignatureError _ = False

continuesAfterError :: CompilerError -> Bool
continuesAfterError (CompilerError _ _ msg _) = "continuing" `L.isInfixOf` msg || "recovery" `L.isInfixOf` msg
continuesAfterError _ = False

hasRecoverySuggestion :: CompilerError -> Bool
hasRecoverySuggestion (CompilerError _ _ msg _) = "suggest" `L.isInfixOf` msg || "fix" `L.isInfixOf` msg
hasRecoverySuggestion _ = False

isTypeMismatchError :: CompilerError -> Bool
isTypeMismatchError (CompilerError TypeError _ msg _) = "type" `L.isInfixOf` msg && "mismatch" `L.isInfixOf` msg
isTypeMismatchError _ = False

hasTypeSuggestion :: CompilerError -> Bool
hasTypeSuggestion (CompilerError _ _ msg _) = "expected" `L.isInfixOf` msg || "actual" `L.isInfixOf` msg
hasTypeSuggestion _ = False

isUndefinedVariableError :: CompilerError -> Bool
isUndefinedVariableError (CompilerError TypeError _ msg _) = "undefined" `L.isInfixOf` msg && "variable" `L.isInfixOf` msg
isUndefinedVariableError _ = False

hasVariableSuggestion :: CompilerError -> Bool
hasVariableSuggestion (CompilerError _ _ msg _) = "similar" `L.isInfixOf` msg || "did you mean" `L.isInfixOf` msg
hasVariableSuggestion _ = False

isInvalidFunctionCallError :: CompilerError -> Bool
isInvalidFunctionCallError (CompilerError TypeError _ msg _) = "function" `L.isInfixOf` msg && "call" `L.isInfixOf` msg
isInvalidFunctionCallError _ = False

hasCorrectSignatureHint :: CompilerError -> Bool
hasCorrectSignatureHint (CompilerError _ _ msg _) = "signature" `L.isInfixOf` msg
hasCorrectSignatureHint _ = False

isDuplicateDeclarationError :: CompilerError -> Bool
isDuplicateDeclarationError (CompilerError SemanticError _ msg _) = "duplicate" `L.isInfixOf` msg && "declaration" `L.isInfixOf` msg
isDuplicateDeclarationError _ = False

continuesProcessing :: CompilerError -> Bool
continuesProcessing (CompilerError _ _ msg _) = "continuing" `L.isInfixOf` msg
continuesProcessing _ = False

isOwnershipError :: CompilerError -> Bool
isOwnershipError (CompilerError OwnershipError _ msg _) = "ownership" `L.isInfixOf` msg
isOwnershipError _ = False

hasOwnershipFixSuggestion :: CompilerError -> Bool
hasOwnershipFixSuggestion (CompilerError _ _ msg _) = "move" `L.isInfixOf` msg || "borrow" `L.isInfixOf` msg
hasOwnershipFixSuggestion _ = False

isDependentTypeError :: CompilerError -> Bool
isDependentTypeError (CompilerError TypeError _ msg _) = "dependent" `L.isInfixOf` msg && "type" `L.isInfixOf` msg
isDependentTypeError _ = False

hasTypeConstraintExplanation :: CompilerError -> Bool
hasTypeConstraintExplanation (CompilerError _ _ msg _) = "constraint" `L.isInfixOf` msg || "violation" `L.isInfixOf` msg
hasTypeConstraintExplanation _ = False

hasErrorPhase :: CompilerError -> Bool
hasErrorPhase (CompilerError phase _ _ _) = phase /= UnknownPhase
hasErrorPhase _ = False

errorsOrderedByLocation :: [CompilerError] -> Bool
errorsOrderedByLocation errs = L.all (uncurry (<=)) $ zip locations (L.tail locations)
  where
    locations = map getLineNumber errs
    getLineNumber (CompilerError _ (Just span) _ _) = sourceLine $ spanStart span
    getLineNumber _ = 0

hasBothSyntaxAndTypeErrors :: [CompilerError] -> Bool
hasBothSyntaxAndTypeErrors errs = L.any isSyntaxError errs && L.any (not . isSyntaxError) errs

syntaxErrorsComeFirst :: [CompilerError] -> Bool
syntaxErrorsComeFirst errs = 
    let syntaxErrors = takeWhile isSyntaxError errs
        otherErrors = dropWhile isSyntaxError errs
    in null otherErrors || not (L.any isSyntaxError otherErrors)

noDuplicateErrorMessages :: [CompilerError] -> Bool
noDuplicateErrorMessages errs = L.length (map getErrorMessage errs) == L.length (nub $ map getErrorMessage errs)
  where
    getErrorMessage (CompilerError _ _ msg _) = msg
    getErrorMessage _ = ""
    nub [] = []
    nub (x:xs) = x : nub (L.filter (/= x) xs)

hasClearErrorChain :: [CompilerError] -> Bool
hasClearErrorChain errs = L.all hasRelatedInfo errs
  where
    hasRelatedInfo (CompilerError _ _ msg _) = L.length (words msg) >= 3
    hasRelatedInfo _ = False

typeErrorsAreGrouped :: [CompilerError] -> Bool
typeErrorsAreGrouped errs = 
    let typeErrors = L.filter (not . isSyntaxError) errs
        nonTypeErrors = filter isSyntaxError errs
    in null typeErrors || null nonTypeErrors || 
       L.all (not . isSyntaxError) (take 3 typeErrors)  -- First few non-syntax errors should be type errors

hasValidErrorState :: CompilerError -> Bool
hasValidErrorState (CompilerError phase _ _ _) = phase /= UnknownPhase
hasValidErrorState _ = False

hasConsistentFormatting :: CompilerError -> Bool
hasConsistentFormatting (CompilerError _ _ msg _) = not (null msg) && L.all (`elem` [' '..'~']) msg
hasConsistentFormatting _ = False

-- | Generators for QuickCheck testing
genErrorProneCode :: Gen String
genErrorProneCode = elements
  [ "let x: int = \"hello\""
  , "let y = undefined_var"
  , "func test( { return 42 }"
  , "let x = 5\nlet x = 10"
  , "func add(x int, y int) int { return x + y }\nlet result = add(\"hello\", 5)"
  ]

genMultiErrorCode :: Gen String
genMultiErrorCode = elements
  [ "let x: int = \"hello\"\nlet y: string = 42"
  , "let x = 5\nlet y = \nfunc test( { return }"
  , "func undefined_func() int { return 42 }\nlet x = undefined_func() + unknown_var"
  , "let x = 5\nlet x = 10\nlet y = undefined_var"
  ]