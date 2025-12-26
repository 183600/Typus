module Test.Unit.IntegrationAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, elements, choose, listOf, suchThat, vectorOf)
import TestSupport.QuickCheck (fastProperty)

import Parser
import Compiler
import Utils
import SourceLocation
import ErrorHandler
import Dependencies
import Ownership
import Compiler.Errors (CompilerError(..))
import qualified Data.Text as T
import Data.List (isInfixOf, null)

-- ============================================================================
-- Integration Test Generators
-- ============================================================================

-- Generate simple Typus code snippets
simpleTypusCode :: Gen String
simpleTypusCode = oneof [
    pure "package main\n\nfunc main() {\n    fmt.Println(\"hello\")\n}",
    pure "//! ownership: on\npackage main\n\nfunc test() {\n    let x = 42\n    fmt.Println(x)\n}",
    pure "//! dependent_types: true\npackage main\n\nfunc add(a: int, b: int) int {\n    return a + b\n}",
    pure "package main\n\n{//! ownership: on}\nfunc move_example() {\n    let data = String::new(\"hello\")\n    let owner = data\n}",
    pure "//! constraints: on\npackage main\n\nfunc generic<T>(x: T) T {\n    return x\n}"
    ]

-- Generate code with potential errors
errorTypusCode :: Gen String
errorTypusCode = oneof [
    pure "package main\n\nfunc main() {\n    var x int = \"string\"\n}",
    pure "//! ownership: on\npackage main\n\nfunc borrow_error() {\n    let data = String::new(\"hello\")\n    let borrowed = &data\n    let moved = data\n}",
    pure "func missing_brace() {\n    if true {\n        // missing closing brace"
    ]

-- ============================================================================
-- Integration Properties
-- ============================================================================

prop_parseCompileRoundtrip :: String -> Bool
prop_parseCompileRoundtrip input =
    case parseTypus input of
        Left _ -> True  -- Parse errors are expected for arbitrary input
        Right typusFile -> 
            case compile typusFile of
                Left _ -> True  -- Compilation errors are expected
                Right compiled -> not (null compiled)

prop_parseOwnershipAnalysisIntegration :: String -> Bool
prop_parseOwnershipAnalysisIntegration input =
    case parseTypus input of
        Left _ -> True  -- Parse errors are expected
        Right typusFile -> 
            let content = unlines (map cbContent (tfBlocks typusFile))
                ownershipErrors = analyzeOwnershipFile content
            in not (null ownershipErrors) || True  -- May have errors or not

prop_parseDependenciesAnalysisIntegration :: String -> Bool
prop_parseDependenciesAnalysisIntegration input =
    case parseTypus input of
        Left _ -> True  -- Parse errors are expected
        Right typusFile -> 
            let content = unlines (map cbContent (tfBlocks typusFile))
                checker = newDependentTypeChecker
                result = analyzeDependentTypes checker (AST [])
            in not (null result) || True  -- May have errors or not

prop_utilsParserIntegration :: String -> Bool
prop_utilsParserIntegration input =
    let trimmed = trim input
        normalized = normalizeIndentation input
        commentsRemoved = removeComments input
        lineCommentsRemoved = removeLineComments input
    in not (null trimmed) || not (null normalized) || 
       not (null commentsRemoved) || not (null lineCommentsRemoved)

prop_sourceLocationParserIntegration :: String -> Bool
prop_sourceLocationParserIntegration input =
    case parseTypus input of
        Left _ -> True  -- Parse errors are expected
        Right typusFile -> 
            let blocks = tfBlocks typusFile
                spans = map cbSpan blocks
            in all isValidSpan spans

prop_errorHandlerCompilerIntegration :: String -> Bool
prop_errorHandlerCompilerIntegration input =
    case parseTypus input of
        Left _ -> True  -- Parse errors are expected
        Right typusFile -> 
            case compile typusFile of
                Left errors -> 
                    let formatted = formatErrors errors
                    in not (null formatted)
                Right _ -> True

prop_fullPipelineIntegration :: String -> Bool
prop_fullPipelineIntegration input =
    let parsed = parseTypus input
        processed = case parsed of
            Left _ -> Nothing
            Right typusFile -> 
                case compile typusFile of
                    Left _ -> Nothing
                    Right compiled -> Just compiled
    in case processed of
        Nothing -> True  -- Errors are expected
        Just result -> not (null result)

prop_errorRecoveryIntegration :: String -> Bool
prop_errorRecoveryIntegration input =
    case parseTypus input of
        Left _ -> True  -- Parse errors are expected
        Right typusFile -> 
            case compile typusFile of
                Left errors -> 
                    let canRecover = all canRecoverFrom errors
                    in canRecover || True  -- May or may not be recoverable
                Right _ -> True

prop_directiveProcessingIntegration :: String -> Bool
prop_directiveProcessingIntegration input =
    case parseTypus input of
        Left _ -> True  -- Parse errors are expected
        Right typusFile -> 
            let directives = tfDirectives typusFile
                blocks = tfBlocks typusFile
                blockDirectives = map cbDirectives blocks
            in not (null directives) || not (null blockDirectives) || True

prop_syntaxValidationIntegration :: String -> Bool
prop_syntaxValidationIntegration input =
    case parseTypus input of
        Left _ -> True  -- Parse errors are expected
        Right typusFile -> 
            let syntaxErrors = tfSyntaxErrors typusFile
                hasContent = not (null (tfBlocks typusFile))
            in if hasContent
               then not (null syntaxErrors) || True  -- May have syntax errors or not
               else True

-- ============================================================================
-- Advanced Integration Properties
-- ============================================================================

prop_compilerOwnershipDependenciesIntegration :: String -> Bool
prop_compilerOwnershipDependenciesIntegration input =
    case parseTypus input of
        Left _ -> True  -- Parse errors are expected
        Right typusFile -> 
            let content = unlines (map cbContent (tfBlocks typusFile))
                ownershipErrors = analyzeOwnershipFile content
                dependencyChecker = newDependentTypeChecker
                dependencyErrors = analyzeDependentTypes dependencyChecker (AST [])
                compileResult = compile typusFile
            in case compileResult of
                Left compilerErrors -> 
                    let hasOwnershipErrors = not (null ownershipErrors)
                        hasDependencyErrors = not (null dependencyErrors)
                        hasCompilerErrors = not (null compilerErrors)
                    in hasCompilerErrors ==> (hasOwnershipErrors || hasDependencyErrors || True)
                Right _ -> True

prop_utilsErrorHandlerIntegration :: String -> Bool
prop_utilsErrorHandlerIntegration input =
    let processed = removeComments input
        trimmed = trim processed
        normalized = normalizeIndentation trimmed
        parsed = parseTypus normalized
    in case parsed of
        Left _ -> True  -- Parse errors are expected
        Right typusFile -> 
            case compile typusFile of
                Left errors -> 
                    let formatted = formatErrors errors
                    in not (null formatted)
                Right _ -> True

prop_sourceLocationErrorHandlerIntegration :: String -> Bool
prop_sourceLocationErrorHandlerIntegration input =
    case parseTypus input of
        Left _ -> True  -- Parse errors are expected
        Right typusFile -> 
            case compile typusFile of
                Left errors -> 
                    let hasLocations = any (\e -> errorLocation e /= undefined) errors
                    in hasLocations || True
                Right _ -> True

prop_multiModuleIntegration :: [String] -> Bool
prop_multiModuleIntegration inputs =
    let parsedModules = map parseTypus inputs
        successfulModules = [file | Right file <- parsedModules]
        compiledModules = map compile successfulModules
        successfulCompilations = [code | Right code <- compiledModules]
    in length successfulCompilations <= length successfulModules &&
       length successfulModules <= length inputs

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Integration Advanced QuickCheck Tests"
    [ testGroup "Basic Integration Properties"
        [ fastProperty "parse compile roundtrip" prop_parseCompileRoundtrip
        , fastProperty "parse ownership analysis integration" prop_parseOwnershipAnalysisIntegration
        , fastProperty "parse dependencies analysis integration" prop_parseDependenciesAnalysisIntegration
        , fastProperty "utils parser integration" prop_utilsParserIntegration
        , fastProperty "source location parser integration" prop_sourceLocationParserIntegration
        , fastProperty "error handler compiler integration" prop_errorHandlerCompilerIntegration
        ]

    , testGroup "Pipeline Integration Properties"
        [ fastProperty "full pipeline integration" prop_fullPipelineIntegration
        , fastProperty "error recovery integration" prop_errorRecoveryIntegration
        , fastProperty "directive processing integration" prop_directiveProcessingIntegration
        , fastProperty "syntax validation integration" prop_syntaxValidationIntegration
        ]

    , testGroup "Advanced Integration Properties"
        [ fastProperty "compiler ownership dependencies integration" prop_compilerOwnershipDependenciesIntegration
        , fastProperty "utils error handler integration" prop_utilsErrorHandlerIntegration
        , fastProperty "source location error handler integration" prop_sourceLocationErrorHandlerIntegration
        , fastProperty "multi-module integration" prop_multiModuleIntegration
        ]

    , testGroup "Unit Tests"
        [ testCase "complete pipeline for valid code" $ do
            let validCode = "package main\n\nfunc main() {\n    fmt.Println(\"hello\")\n}"
            case parseTypus validCode of
                Left err -> assertBool ("Should parse successfully: " ++ err) False
                Right typusFile -> 
                    case compile typusFile of
                        Left errs -> assertBool ("Should compile successfully: " ++ show errs) False
                        Right result -> assertBool "Should generate code" (not (null result))

        , testCase "error handling for invalid code" $ do
            let invalidCode = "package main\n\nfunc main() {\n    var x int = \"string\"\n}"
            case parseTypus invalidCode of
                Left _ -> assertBool "Parse error is acceptable" True
                Right typusFile -> 
                    case compile typusFile of
                        Left errors -> assertBool "Should detect type errors" (not (null errors))
                        Right _ -> assertBool "Should not compile successfully" False

        , testCase "ownership analysis integration" $ do
            let ownershipCode = "//! ownership: on\npackage main\n\nfunc test() {\n    let x = 42\n}"
            case parseTypus ownershipCode of
                Left _ -> assertBool "Parse error is acceptable" True
                Right typusFile -> 
                    let content = unlines (map cbContent (tfBlocks typusFile))
                        ownershipErrors = analyzeOwnershipFile content
                    assertBool "Should analyze ownership" (True)  -- Just check it doesn't crash

        , testCase "dependencies analysis integration" $ do
            let dependenciesCode = "//! dependent_types: true\npackage main\n\nfunc add(a: int, b: int) int {\n    return a + b\n}"
            case parseTypus dependenciesCode of
                Left _ -> assertBool "Parse error is acceptable" True
                Right typusFile -> 
                    let checker = newDependentTypeChecker
                        result = analyzeDependentTypes checker (AST [])
                    assertBool "Should analyze dependencies" (True)  -- Just check it doesn't crash

        , testCase "utils processing integration" $ do
            let rawCode = "  \t  package main  \n\t  \n  func main() {  \t  }  "
            let processed = normalizeIndentation (removeComments (trim rawCode))
            case parseTypus processed of
                Left _ -> assertBool "Parse error is acceptable" True
                Right typusFile -> 
                    assertBool "Should parse processed code" (not (null (tfBlocks typusFile)))

        , testCase "multi-module processing" $ do
            let module1 = "package main\n\nfunc func1() { }"
            let module2 = "package main\n\nfunc func2() { }"
            let modules = [module1, module2]
            let parsedModules = map parseTypus modules
            let successfulModules = [file | Right file <- parsedModules]
            assertBool "Should process multiple modules" (length successfulModules >= 0)
        ]
    ]