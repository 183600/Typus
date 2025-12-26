module Test.Unit.CompilerAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, elements, choose, listOf, suchThat, vectorOf)
import TestSupport.QuickCheck (fastProperty)

import Compiler
import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import Compiler.Errors (CompilerError(..), CompilationPhase(..), ErrorCategory(..), ErrorSeverity(..))
import Compiler.TypeChecker (TypeCheckDiagnostic(..))
import qualified Data.Text as T
import Data.List (isInfixOf, null)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary CompilationPhase where
    arbitrary = elements [ParsingPhase, TypeCheckingPhase, OwnershipPhase, CodeGenPhase]

instance Arbitrary ErrorCategory where
    arbitrary = elements [Parsing, TypeChecking, Ownership, Semantic, Runtime, Constraint, Inference, Integration]

instance Arbitrary ErrorSeverity where
    arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary CompilerError where
    arbitrary = do
        errorId <- arbitrary
        message <- T.pack <$> arbitrary
        phase <- arbitrary
        category <- arbitrary
        severity <- arbitrary
        span' <- oneof [pure Nothing, Just <$> arbitrary]
        context <- oneof [pure Nothing, Just <$> arbitrary]
        suggestions <- listOf (T.pack <$> arbitrary)
        stackTrace <- listOf arbitrary
        timestamp <- oneof [pure Nothing, Just <$> arbitrary]
        return $ CompilerError errorId message phase category severity span' context suggestions stackTrace timestamp

instance Arbitrary TypeCheckDiagnostic where
    arbitrary = do
        context <- oneof [pure Nothing, Just <$> arbitrary]
        detail <- arbitrary
        return $ TypeCheckDiagnostic context detail

instance Arbitrary TypusFile where
    arbitrary = do
        directives <- arbitrary
        buildTags <- listOf arbitrary
        blocks <- listOf arbitrary
        syntaxErrors <- listOf arbitrary
        return $ TypusFile directives buildTags blocks syntaxErrors

-- ============================================================================
-- Compiler Properties
-- ============================================================================

prop_compilePreservesValidStructure :: TypusFile -> Bool
prop_compilePreservesValidStructure typusFile =
    case compile typusFile of
        Left _ -> True  -- Compilation errors are expected for arbitrary files
        Right result -> not (null result)

prop_compileDetectsTypeErrors :: TypusFile -> Bool
prop_compileDetectsTypeErrors typusFile =
    let hasTypeErrorContent = "var x int = \"string\"" `isInfixOf` unlines (map cbContent (tfBlocks typusFile))
    in if hasTypeErrorContent
       then case compile typusFile of
           Left errors -> any (\e -> errorSeverity e == Error) errors
           Right _ -> False
       else True  -- Other cases are handled by other properties

prop_generateGoCodeAlwaysReturnsString :: TypusFile -> Bool
prop_generateGoCodeAlwaysReturnsString typusFile =
    let result = generateGoCode typusFile
    in not (null result)

prop_generateGoCodePreservesContent :: TypusFile -> Bool
prop_generateGoCodePreservesContent typusFile =
    let originalContent = unlines (map cbContent (tfBlocks typusFile))
        generatedCode = generateGoCode typusFile
        hasSomeContent = not (null originalContent)
    in if hasSomeContent
       then any (`isInfixOf` generatedCode) (words originalContent) || 
            any (`isInfixOf` generatedCode) (lines originalContent)
       else True

prop_renderCompilationErrorHandlesEmptyList :: Bool
prop_renderCompilationErrorHandlesEmptyList =
    let result = renderCompilationError []
    in null result || result == ""

prop_renderCompilationErrorHandlesNonEmptyList :: [CompilerError] -> Bool
prop_renderCompilationErrorHandlesNonEmptyList errors =
    let result = renderCompilationError errors
    in if null errors
       then null result || result == ""
       else not (null result)

prop_formatCompilerErrorsConsistent :: [CompilerError] -> Bool
prop_formatCompilerErrorsConsistent errors =
    let formatted1 = renderCompilationError errors
        formatted2 = renderCompilationError errors
    in formatted1 == formatted2

prop_analyzeErrorsHandlesEmptyList :: Bool
prop_analyzeErrorsHandlesEmptyList =
    let result = analyzeErrors []
    in result == []

prop_analyzeErrorsReturnsSameLength :: [CompilerError] -> Bool
prop_analyzeErrorsReturnsSameLength errors =
    let result = analyzeErrors errors
    in length result == length errors

prop_hasTypeErrorsDetectsTypeErrors :: [TypeCheckDiagnostic] -> Bool
prop_hasTypeErrorsDetectsTypeErrors diagnostics =
    let hasErrors = not (null diagnostics)
    in hasTypeErrors diagnostics == hasErrors

prop_diagnoseTypeErrorsHandlesEmptyFile :: Bool
prop_diagnoseTypeErrorsHandlesEmptyFile =
    let emptyFile = TypusFile defaultFileDirectives [] [] []
    in case diagnoseTypeErrors emptyFile of
        Left _ -> True
        Right diagnostics -> null diagnostics

prop_extractDeclarationsHandlesEmptyFile :: Bool
prop_extractDeclarationsHandlesEmptyFile =
    let emptyFile = TypusFile defaultFileDirectives [] [] []
        declarations = extractDeclarations emptyFile
    in null declarations

prop_extractFunctionCallsHandlesEmptyFile :: Bool
prop_extractFunctionCallsHandlesEmptyFile =
    let emptyFile = TypusFile defaultFileDirectives [] [] []
        functionCalls = extractFunctionCalls emptyFile
    in null functionCalls

prop_buildTypeEnvHandlesEmptyPairs :: Bool
prop_buildTypeEnvHandlesEmptyPairs =
    let typeEnv = buildTypeEnv []
    in null typeEnv

prop_buildTypeEnvFromPairsHandlesEmptyPairs :: Bool
prop_buildTypeEnvFromPairsHandlesEmptyPairs =
    let typeEnv = buildTypeEnvFromPairs []
    in null typeEnv

prop_createTypusFileFromErrorsHandlesEmptyErrors :: Bool
prop_createTypusFileFromErrorsHandlesEmptyErrors =
    let typusFile = createTypusFileFromErrors []
    in null (tfBlocks typusFile)

prop_isMethodDeclarationDetectsMethods :: String -> Bool
prop_isMethodDeclarationDetectsMethods input =
    let result = isMethodDeclaration input
        hasMethodPattern = "func (" `isInfixOf` input
    in if hasMethodPattern
       then result
       else True  -- May or may not be method, no specific requirement

prop_checkTypeErrorHandlesValidInput :: TypeCheckDiagnostic -> Bool
prop_checkTypeErrorHandlesValidInput diagnostic =
    checkTypeError diagnostic  -- Should not crash

prop_hasMalformedSyntaxHandlesEmptyFile :: Bool
prop_hasMalformedSyntaxHandlesEmptyFile =
    let emptyFile = TypusFile defaultFileDirectives [] [] []
    in not (hasMalformedSyntax emptyFile)  -- Empty file should not have malformed syntax

prop_checkDependentTypesHandlesEmptyFile :: Bool
prop_checkDependentTypesHandlesEmptyFile =
    let emptyFile = TypusFile defaultFileDirectives [] [] []
    in checkDependentTypes emptyFile  -- Should not crash

prop_checkOwnershipHandlesEmptyFile :: Bool
prop_checkOwnershipHandlesEmptyFile =
    let emptyFile = TypusFile defaultFileDirectives [] [] []
    in checkOwnership emptyFile  -- Should not crash

prop_ensureSourceIRHandlesEmptyFile :: Bool
prop_ensureSourceIRHandlesEmptyFile =
    let emptyFile = TypusFile defaultFileDirectives [] [] []
    in case ensureSourceIR emptyFile of
        Left _ -> True
        Right _ -> True

prop_typeDiagnosticToCompilerErrorPreservesContent :: TypeCheckDiagnostic -> Bool
prop_typeDiagnosticToCompilerErrorPreservesContent diagnostic =
    let error = typeDiagnosticToCompilerError diagnostic
        expectedDetail = case diagnostic of
            TypeCheckDiagnostic _ detail -> detail
    in expectedDetail `isInfixOf` T.unpack (errorMessage error)

-- ============================================================================
-- Advanced Properties
-- ============================================================================

prop_compileAndGenerateGoCodeConsistency :: TypusFile -> Bool
prop_compileAndGenerateGoCodeConsistency typusFile =
    case compile typusFile of
        Left _ -> True  -- Compilation errors are expected
        Right compiledResult -> 
            let generatedCode = generateGoCode typusFile
            in not (null compiledResult) && not (null generatedCode)

prop_errorSeverityOrdering :: CompilerError -> CompilerError -> Bool
prop_errorSeverityOrdering err1 err2 =
    let sev1 = errorSeverity err1
        sev2 = errorSeverity err2
        severityOrder sev = case sev of
            Fatal -> 4
            Error -> 3
            Warning -> 2
            Info -> 1
    in if sev1 >= sev2
       then severityOrder sev1 >= severityOrder sev2
       else severityOrder sev1 <= severityOrder sev2

prop_errorPhaseOrdering :: CompilerError -> CompilerError -> Bool
prop_errorPhaseOrdering err1 err2 =
    let phase1 = errorPhase err1
        phase2 = errorPhase err2
        phaseOrder phase = case phase of
            ParsingPhase -> 1
            TypeCheckingPhase -> 2
            OwnershipPhase -> 3
            CodeGenPhase -> 4
    in if phase1 >= phase2
       then phaseOrder phase1 >= phaseOrder phase2
       else phaseOrder phase1 <= phaseOrder phase2

prop_compilerErrorIdPreserved :: String -> TypusFile -> Bool
prop_compilerErrorIdPreserved errorId typusFile =
    let hasErrorIdContent = errorId `isInfixOf` unlines (map cbContent (tfBlocks typusFile))
    in if hasErrorIdContent
       then case compile typusFile of
           Left errors -> any (\e -> errorId `isInfixOf` errorId e) errors
           Right _ -> False
       else True

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Compiler Advanced QuickCheck Tests"
    [ testGroup "Basic Compilation Properties"
        [ fastProperty "compile preserves valid structure" prop_compilePreservesValidStructure
        , fastProperty "compile detects type errors" prop_compileDetectsTypeErrors
        , fastProperty "generateGoCode always returns string" prop_generateGoCodeAlwaysReturnsString
        , fastProperty "generateGoCode preserves content" prop_generateGoCodePreservesContent
        ]

    , testGroup "Error Handling Properties"
        [ fastProperty "renderCompilationError handles empty list" prop_renderCompilationErrorHandlesEmptyList
        , fastProperty "renderCompilationError handles non-empty list" prop_renderCompilationErrorHandlesNonEmptyList
        , fastProperty "formatCompilerErrors is consistent" prop_formatCompilerErrorsConsistent
        , fastProperty "analyzeErrors handles empty list" prop_analyzeErrorsHandlesEmptyList
        , fastProperty "analyzeErrors returns same length" prop_analyzeErrorsReturnsSameLength
        ]

    , testGroup "Type Checking Properties"
        [ fastProperty "hasTypeErrors detects type errors" prop_hasTypeErrorsDetectsTypeErrors
        , fastProperty "diagnoseTypeErrors handles empty file" prop_diagnoseTypeErrorsHandlesEmptyFile
        , fastProperty "extractDeclarations handles empty file" prop_extractDeclarationsHandlesEmptyFile
        , fastProperty "extractFunctionCalls handles empty file" prop_extractFunctionCallsHandlesEmptyFile
        , fastProperty "buildTypeEnv handles empty pairs" prop_buildTypeEnvHandlesEmptyPairs
        , fastProperty "buildTypeEnvFromPairs handles empty pairs" prop_buildTypeEnvFromPairsHandlesEmptyPairs
        ]

    , testGroup "Compiler Utilities Properties"
        [ fastProperty "createTypusFileFromErrors handles empty errors" prop_createTypusFileFromErrorsHandlesEmptyErrors
        , fastProperty "isMethodDeclaration detects methods" prop_isMethodDeclarationDetectsMethods
        , fastProperty "checkTypeError handles valid input" prop_checkTypeErrorHandlesValidInput
        , fastProperty "hasMalformedSyntax handles empty file" prop_hasMalformedSyntaxHandlesEmptyFile
        , fastProperty "checkDependentTypes handles empty file" prop_checkDependentTypesHandlesEmptyFile
        , fastProperty "checkOwnership handles empty file" prop_checkOwnershipHandlesEmptyFile
        , fastProperty "ensureSourceIR handles empty file" prop_ensureSourceIRHandlesEmptyFile
        , fastProperty "typeDiagnosticToCompilerError preserves content" prop_typeDiagnosticToCompilerErrorPreservesContent
        ]

    , testGroup "Advanced Properties"
        [ fastProperty "compile and generateGoCode consistency" prop_compileAndGenerateGoCodeConsistency
        , fastProperty "error severity ordering" prop_errorSeverityOrdering
        , fastProperty "error phase ordering" prop_errorPhaseOrdering
        , fastProperty "compiler error id preserved" prop_compilerErrorIdPreserved
        ]

    , testGroup "Unit Tests"
        [ testCase "compile simple valid file" $ do
            let simpleFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives "package main\nfunc main() {}\n" undefined] []
            case compile simpleFile of
                Left err -> assertBool ("Should compile successfully: " ++ show err) False
                Right result -> assertBool "Should generate Go code" (not (null result))

        , testCase "compile file with type error" $ do
            let errorFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives "var x int = \"string\"" undefined] []
            case compile errorFile of
                Left errors -> assertBool "Should detect type error" (any (\e -> errorSeverity e == Error) errors)
                Right _ -> assertBool "Should not compile successfully" False

        , testCase "generateGoCode for simple file" $ do
            let simpleFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives "func test() {}\n" undefined] []
            let result = generateGoCode simpleFile
            assertBool "Should generate some Go code" (not (null result))

        , testCase "renderCompilationError for empty list" $ do
            let result = renderCompilationError []
            assertBool "Should handle empty list gracefully" (null result || result == "")

        , testCase "renderCompilationError for non-empty list" $ do
            let errors = [CompilerError "TEST001" (T.pack "test error") ParsingPhase Parsing Error Nothing Nothing [] [] Nothing]
            let result = renderCompilationError errors
            assertBool "Should format errors" (not (null result))
        ]
    ]