module Test.Unit.CoreCompilerSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, oneof, elements, listOf)
import qualified Data.Text as T

import Parser (TypusFile(..), CodeBlock(..), defaultFileDirectives, defaultBlockDirectives)
import Compiler
import Compiler.Errors (CompilationPhase(..), ErrorCategory(..), ErrorSeverity(..), CompilerError(..))
import qualified Compiler.Errors.Compiler as CE
import SourceLocation (SourcePos(..), SourceSpan(..))

-- | Core functionality tests for Compiler module
tests :: TestTree
tests =
  testGroup "Core Compiler Tests"
    [ testGroup "Basic compilation"
        [ testCase "compile handles empty file" $ do
            let emptyFile = TypusFile defaultFileDirectives [] [] []
                result = compile emptyFile
            case result of
                Left errs -> assertBool "should not fail on empty file" $ null errs
                Right goCode -> assertBool "should generate Go code" $ not (null goCode)

        , testCase "compile handles simple valid code" $ do
            let simpleBlock = CodeBlock defaultBlockDirectives "func main() {\n  fmt.Println(\"hello\")\n}" 
                               (SourceSpan (SourcePos 1 1 0) (SourcePos 3 2 50))
                simpleFile = TypusFile defaultFileDirectives [] [simpleBlock] []
                result = compile simpleFile
            case result of
                Left errs -> assertBool ("should compile simple code: " ++ show errs) $ null errs
                Right goCode -> do
                    assertBool "should contain function" $ "func main()" `isInfixOf` goCode
                    assertBool "should contain print statement" $ "fmt.Println" `isInfixOf` goCode

        , testCase "compile detects type errors" $ do
            let blockWithError = CodeBlock defaultBlockDirectives "var x int = \"string\"" 
                                   (SourceSpan (SourcePos 1 1 0) (SourcePos 1 25 24))
                fileWithError = TypusFile defaultFileDirectives [] [blockWithError] []
                result = compile fileWithError
            case result of
                Left errs -> do
                    assertBool "should have type error" $ length errs >= 1
                    let typeErr = head errs
                        innerErr = ceError typeErr
                    assertBool "should be type checking error" $ 
                        phase typeErr == TypeCheckingPhase
                    assertBool "should be type checking category" $ 
                        category innerErr == TypeChecking
                    assertBool "should mention string/int conflict" $ 
                        "string" `T.isInfixOf` message innerErr && 
                        "int" `T.isInfixOf` message innerErr
                Right _ -> assertBool "should not succeed with type error" False
        ]

    , testGroup "Error handling and reporting"
        [ testCase "renderCompilationError formats errors" $ do
            let errors = [typeCheckFailure]
                formatted = renderCompilationError errors
            assertBool "should contain error message" $ "Type errors detected" `isInfixOf` formatted
            assertBool "should contain error code" $ "CP0002" `isInfixOf` formatted

        , testCase "formatCompilerErrors handles multiple errors" $ do
            let errors = [CE.malformedSyntaxError, typeCheckFailure]
                formatted = formatCompilerErrors errors
            assertBool "should contain malformed syntax error" $ "Malformed syntax" `isInfixOf` formatted
            assertBool "should contain type checking error" $ "Type errors detected" `isInfixOf` formatted

let errors = [CE.malformedSyntaxError, typeCheckFailure]
                report = generateDetailedReport errors
            assertBool "should contain error summary" $ "Error Summary" `isInfixOf` report
            assertBool "should contain total count" $ "Total errors: 2" `isInfixOf` report
            assertBool "should contain phase breakdown" $ "by Phase" `isInfixOf` report
        ]

    , testGroup "Source IR validation"
        [ testCase "ensureSourceIR accepts valid files" $ do
            let validBlock = CodeBlock defaultBlockDirectives "func test() {}" 
                               (SourceSpan (SourcePos 1 1 0) (SourcePos 1 16 15))
                validFile = TypusFile defaultFileDirectives [] [validBlock] []
                result = ensureSourceIR validFile
            case result of
                Left _ -> assertBool "should accept valid file" False
                Right _ -> assertBool "should create source IR" True

        , testCase "ensureSourceIR rejects malformed syntax" $ do
            let malformedBlock = CodeBlock defaultBlockDirectives "func incomplete {" 
                                   (SourceSpan (SourcePos 1 1 0) (SourcePos 1 16 15))
                malformedFile = TypusFile defaultFileDirectives [] [malformedBlock] []
                result = ensureSourceIR malformedFile
            case result of
                Left errs -> do
                    assertBool "should have errors" $ not (null errs)
                    let err = head errs
                        innerErr = ceError err
                    assertBool "should be parsing error" $ phase err == ParsingPhase
                    assertBool "should mention malformed syntax" $ 
                        "Malformed syntax" `T.isInfixOf` message innerErr
                Right _ -> assertBool "should not accept malformed syntax" False
        ]

    , testCase "generateGoCode handles valid files" $ do
            let block = CodeBlock defaultBlockDirectives "func main() {\n  fmt.Println(\"test\")\n}" 
                           (SourceSpan (SourcePos 1 1 0) (SourcePos 3 2 50))
                file = TypusFile defaultFileDirectives [] [block] []
                goCode = generateGoCode file
            assertBool "should generate Go code" $ not (null goCode)
            assertBool "should contain function" $ "func main()" `isInfixOf` goCode

        , testCase "generateGoCode falls back on errors" $ do
            let problematicBlock = CodeBlock defaultBlockDirectives "func incomplete {" 
                                     (SourceSpan (SourcePos 1 1 0) (SourcePos 1 16 15))
                problematicFile = TypusFile defaultFileDirectives [] [problematicBlock] []
                goCode = generateGoCode problematicFile
            assertBool "should still generate output" $ not (null goCode)
            assertBool "should contain original content" $ "func incomplete {" `isInfixOf` goCode

    , testGroup "Type checking diagnostics"
        [ testCase "typeDiagnosticToCompilerError converts correctly" $ do
            let diagnostic = TypeCheckDiagnostic (Just "main") "undefined variable"
                error = typeDiagnosticToCompilerError diagnostic
                innerErr = ceError error
            assertBool "should include context" $ "main" `T.isInfixOf` message innerErr
            assertBool "should include detail" $ "undefined variable" `T.isInfixOf` message innerErr
            assertBool "should be type checking error" $ phase error == TypeCheckingPhase

        , testCase "diagnoseTypeErrors handles valid code" $ do
            let validBlock = CodeBlock defaultBlockDirectives "func test() { x := 5 }" 
                               (SourceSpan (SourcePos 1 1 0) (SourcePos 1 22 21))
                validFile = TypusFile defaultFileDirectives [] [validBlock] []
                result = diagnoseTypeErrors validFile
            case result of
                Left _ -> assertBool "should not fail on valid code" False
                Right diagnostics -> assertBool "should have diagnostics" $ True

        , testCase "hasMalformedSyntax detects issues" $ do
            let malformedBlock = CodeBlock defaultBlockDirectives "func { broken }" 
                                   (SourceSpan (SourcePos 1 1 0) (SourcePos 1 17 16))
                malformedFile = TypusFile defaultFileDirectives [] [malformedBlock] []
            assertBool "should detect malformed syntax" $ hasMalformedSyntax malformedFile

        , testCase "hasMalformedSyntax accepts valid code" $ do
            let validBlock = CodeBlock defaultBlockDirectives "func test() { return }" 
                               (SourceSpan (SourcePos 1 1 0) (SourcePos 1 26 25))
                validFile = TypusFile defaultFileDirectives [] [validBlock] []
            assertBool "should accept valid syntax" $ not (hasMalformedSyntax validFile)
        ]

    , testGroup "Error analysis"
        [ testCase "analyzeErrors categorizes correctly" $ do
            let errors = [CE.malformedSyntaxError, typeCheckFailure]
                analysis = analyzeErrors errors
            assertBool "should detect parsing errors" $ hasParsingErrors analysis
            assertBool "should detect type checking errors" $ hasTypeCheckingErrors analysis
            assertBool "should have error count" $ totalErrorCount analysis == 2

        , testCase "checkTypeError identifies type issues" $ do
            let typeErr = typeCheckFailure
            assertBool "should recognize type error" $ checkTypeError (ceError typeErr)
            assertBool "should not recognize non-type error" $ not (checkTypeError (ceError CE.malformedSyntaxError))
        ]

    , testGroup "Property-based tests"
        [ testProperty "compile is deterministic" $
            \file -> case compile file of
                Right code1 -> case compile file of
                    Right code2 -> code1 == code2
                    Left _ -> False
                Left _ -> True

        , testProperty "generateGoCode always returns output" $
            \file -> not (null (generateGoCode file))

        , testProperty "renderCompilationError is deterministic" $
            \errors -> renderCompilationError errors == renderCompilationError errors

        , testProperty "analyzeErrors counts match input" $
            \errors -> totalErrorCount (analyzeErrors errors) == length errors

        , testProperty "typeDiagnosticToCompilerError preserves error type" $
            \diagnostic -> 
                let error = typeDiagnosticToCompilerError diagnostic
                    innerErr = ceError error
                in phase error == TypeCheckingPhase && category innerErr == TypeChecking
        ]
    ]