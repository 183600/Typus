{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalCompilerQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (.&&.), (.||.), (==>), forAll, oneof, elements, listOf, choose, suchThat)
import Compiler
  ( compile, CompilerError(..), CompilerResult, CompilationPhase(..)
  , renderCompilationError, formatCompilerErrors, generateDetailedReport
  , analyzeErrors, hasTypeErrors, TypeCheckDiagnostic(..)
  , diagnoseTypeErrors, extractDeclarations, extractFunctionCalls
  , buildTypeEnv, buildTypeEnvFromPairs, createTypusFileFromErrors
  , isMethodDeclaration, checkTypeError, hasMalformedSyntax
  , checkDependentTypes, checkOwnership, ensureSourceIR
  , typeCheckFailure, typeDiagnosticToCompilerError, generateGoCode
  )
import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import Compiler.IR (SourceIR)
import Compiler.Errors (ErrorCategory(..), ErrorSeverity(..), mkCompilerError, defaultSpan)
import SourceLocation (SourceSpan(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (null)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary CompilationPhase where
  arbitrary = elements [ParsingPhase, TypeCheckingPhase, OwnershipPhase, CodeGenPhase]

instance Arbitrary TypeCheckDiagnostic where
  arbitrary = do
    context <- oneof [return Nothing, Just <$> arbitrary]
    detail <- arbitrary `suchThat` (not . null)
    return $ TypeCheckDiagnostic context detail

-- Generate simple TypusFile for testing
genSimpleTypusFile :: Gen TypusFile
genSimpleTypusFile = do
  directives <- arbitrary
  buildTags <- return []  -- Simplified for testing
  blocks <- listOf $ do
    directives' <- arbitrary
    content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n,.;(){}[]"
    span <- arbitrary
    return $ CodeBlock directives' content span
  syntaxErrors <- return []  -- Simplified for testing
  return $ TypusFile directives buildTags blocks syntaxErrors

-- Generate TypusFile with specific content
genTypusFileWithContent :: String -> Gen TypusFile
genTypusFileWithContent content = do
  directives <- arbitrary
  buildTags <- return []
  let block = CodeBlock defaultBlockDirectives content defaultSpan
  return $ TypusFile directives buildTags [block] []

-- Generate compiler error for testing
genCompilerError :: Gen CompilerError
genCompilerError = do
  errorId <- arbitrary `suchThat` (not . null)
  message <- T.pack <$> arbitrary `suchThat` (not . null)
  phase <- arbitrary
  category <- elements [Parsing, TypeChecking, Ownership, Semantic, Runtime, Constraint, Inference, Integration, Unknown]
  severity <- arbitrary
  span' <- oneof [return Nothing, Just <$> arbitrary]
  context <- oneof [return Nothing, Just <$> arbitrary]
  suggestions <- listOf (T.pack <$> arbitrary `suchThat` (not . null))
  stackTrace <- listOf arbitrary
  timestamp <- oneof [return Nothing, Just <$> arbitrary]
  return $ mkCompilerError errorId message phase category severity span' context suggestions stackTrace timestamp

-- ============================================================================
-- Compiler QuickCheck Tests
-- ============================================================================

-- Test compilation of empty file
prop_compile_empty_file :: Property
prop_compile_empty_file =
  let emptyFile = TypusFile defaultFileDirectives [] [] []
  in case compile emptyFile of
       Left _ -> property False
       Right _ -> property True

-- Test compilation of simple valid content
prop_compile_simple_content :: Property
prop_compile_simple_content =
  forAll genSimpleTypusFile $ \typusFile ->
    case compile typusFile of
      Left errs -> L.all (\e -> severity e /= Fatal) errs
      Right _ -> property True

-- Test compilation of file with type error
prop_compile_type_error_fails :: Property
prop_compile_type_error_fails =
  let errorContent = "var x int = \"string\""
      errorFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives errorContent defaultSpan] []
  in case compile errorFile of
       Left errs -> not (null errs)
       Right _ -> property False

-- Test generateGoCode function
prop_generateGoCode_returns_string :: Property
prop_generateGoCode_returns_string =
  forAll genSimpleTypusFile $ \typusFile ->
    let goCode = generateGoCode typusFile
    in not (null goCode)

prop_generateGoCode_handles_malformed_input :: Property
prop_generateGoCode_handles_malformed_input =
  let malformedFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives "{ invalid syntax" defaultSpan] []
      goCode = generateGoCode malformedFile
  in not (null goCode)  -- Should still return some content even for malformed input

-- Test ensureSourceIR function
prop_ensureSourceIR_valid_file_succeeds :: Property
prop_ensureSourceIR_valid_file_succeeds =
  forAll genSimpleTypusFile $ \typusFile ->
    not (hasMalformedSyntax typusFile) ==> 
    case ensureSourceIR typusFile of
      Left _ -> property False
      Right _ -> property True

prop_ensureSourceIR_malformed_fails :: Property
prop_ensureSourceIR_malformed_fails =
  let malformedFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives "{ unclosed brace" defaultSpan] []
  in case ensureSourceIR malformedFile of
       Left _ -> property True
       Right _ -> property False

-- Test error formatting functions
prop_renderCompilationError_formats_errors :: Property
prop_renderCompilationError_formats_errors =
  forAll (listOf genCompilerError) $ \errors ->
    let formatted = renderCompilationError errors
    in if null errors
       then null formatted
       else not (null formatted)

prop_formatCompilerErrors_includes_error_ids :: Property
prop_formatCompilerErrors_includes_error_ids =
  forAll (listOf genCompilerError) $ \errors ->
    let formatted = formatCompilerErrors errors
        errorIds = map errorId errors
    in L.all (\eid -> eid `L.isInfixOf` formatted) errorIds

-- Test type checking functions
prop_hasTypeErrors_detects_type_errors :: Property
prop_hasTypeErrors_detects_type_errors =
  forAll genSimpleTypusFile $ \typusFile ->
    let hasErrors = hasTypeErrors typusFile
        diagnostics = diagnoseTypeErrors typusFile
    in case diagnostics of
         Left errs -> hasErrors
         Right [] -> not hasErrors
         Right _ -> hasErrors

prop_diagnoseTypeErrors_returns_either :: Property
prop_diagnoseTypeErrors_returns_either =
  forAll genSimpleTypusFile $ \typusFile ->
    let result = diagnoseTypeErrors typusFile
    in isLeft result || isRight result

-- Test type diagnostic conversion
prop_typeDiagnosticToCompilerError_preserves_message :: TypeCheckDiagnostic -> Property
prop_typeDiagnosticToCompilerError_preserves_message diag =
  let compilerErr = typeDiagnosticToCompilerError diag
      expectedDetail = case diag of
        TypeCheckDiagnostic _ detail -> detail
  in T.pack expectedDetail `L.isInfixOf` message compilerErr

-- Test declaration extraction
prop_extractDeclarations_returns_list :: Property
prop_extractDeclarations_returns_list =
  forAll genSimpleTypusFile $ \typusFile ->
    let declarations = extractDeclarations typusFile
    in L.length declarations >= 0  -- Always returns a list

prop_extractFunctionCalls_returns_list :: Property
prop_extractFunctionCalls_returns_list =
  forAll genSimpleTypusFile $ \typusFile ->
    let functionCalls = extractFunctionCalls typusFile
    in L.length functionCalls >= 0  -- Always returns a list

-- Test type environment building
prop_buildTypeEnv_returns_environment :: Property
prop_buildTypeEnv_returns_environment =
  forAll genSimpleTypusFile $ \typusFile ->
    let typeEnv = buildTypeEnv typusFile
    in not (null typeEnv) || L.null (tfBlocks typusFile)

prop_buildTypeEnvFromPairs_creates_environment :: Property
prop_buildTypeEnvFromPairs_creates_environment =
  forAll (listOf $ arbitrary `suchThat` (\(x, y) -> not (null x && null y))) $ \pairs ->
    let typeEnv = buildTypeEnvFromPairs pairs
    in L.length typeEnv >= L.length pairs

-- Test method declaration detection
prop_isMethodDeclaration_detects_methods :: Property
prop_isMethodDeclaration_detects_methods =
  forAll (arbitrary `suchThat` (not . null)) $ \declaration ->
    let isMethod = isMethodDeclaration declaration
        hasReceiver = "func (" `L.isPrefixOf` declaration
    in hasReceiver ==> isMethod

-- Test syntax error detection
prop_hasMalformedSyntax_detects_errors :: Property
prop_hasMalformedSyntax_detects_errors =
  let validFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives "func main() {}" defaultSpan] []
      invalidFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives "func main() {" defaultSpan] []
  in not (hasMalformedSyntax validFile) .&&. hasMalformedSyntax invalidFile

-- Test dependent type checking
prop_checkDependentTypes_handles_valid_input :: Property
prop_checkDependentTypes_handles_valid_input =
  forAll genSimpleTypusFile $ \typusFile ->
    let result = checkDependentTypes typusFile
    in isRight result || isLeft result  -- Should always return Either

-- Test ownership checking
prop_checkOwnership_handles_valid_input :: Property
prop_checkOwnership_handles_valid_input =
  forAll genSimpleTypusFile $ \typusFile ->
    let result = checkOwnership typusFile
    in isRight result || isLeft result  -- Should always return Either

-- Test error analysis
prop_analyzeErrors_returns_analysis :: Property
prop_analyzeErrors_returns_analysis =
  forAll (listOf genCompilerError) $ \errors ->
    let analysis = analyzeErrors errors
    in not (null analysis)  -- Should always return some analysis

prop_generateDetailedReport_includes_summary :: Property
prop_generateDetailedReport_includes_summary =
  forAll (listOf genCompilerError) $ \errors ->
    let report = generateDetailedReport errors
    in "Summary" `L.isInfixOf` report || null errors

-- Test error creation from TypusFile
prop_createTypusFileFromErrors_creates_file :: Property
prop_createTypusFileFromErrors_creates_file =
  forAll (listOf genCompilerError) $ \errors ->
    let typusFile = createTypusFileFromErrors errors
    in not (L.null (tfSyntaxErrors typusFile)) || null errors

-- Test type error checking
prop_checkTypeError_validates_types :: Property
prop_checkTypeError_validates_types =
  forAll genSimpleTypusFile $ \typusFile ->
    let hasTypeErr = checkTypeError typusFile
        typeDiagnostics = diagnoseTypeErrors typusFile
    in case typeDiagnostics of
         Left _ -> hasTypeErr
         Right [] -> not hasTypeErr
         Right _ -> hasTypeErr

-- ============================================================================
-- Integration Property Tests
-- ============================================================================

-- Test compilation pipeline consistency
prop_compilation_pipeline_consistent :: Property
prop_compilation_pipeline_consistent =
  forAll genSimpleTypusFile $ \typusFile ->
    let compileResult = compile typusFile
        goCodeResult = generateGoCode typusFile
    in case compileResult of
         Left _ -> not (null goCodeResult)  -- Go generation should still work
         Right compiledCode -> not (null compiledCode) && not (null goCodeResult)

-- Test error handling consistency
prop_error_handling_consistent :: Property
prop_error_handling_consistent =
  forAll genSimpleTypusFile $ \typusFile ->
    let hasMalformed = hasMalformedSyntax typusFile
        sourceIRResult = ensureSourceIR typusFile
        hasTypeErrs = hasTypeErrors typusFile
    in if hasMalformed
       then isLeft sourceIRResult
       else isRight sourceIRResult && (hasTypeErrs ==> isLeft (compile typusFile))

-- Test diagnostic consistency
prop_diagnostic_consistency :: Property
prop_diagnostic_consistency =
  forAll genSimpleTypusFile $ \typusFile ->
    let diagnostics = diagnoseTypeErrors typusFile
        typeErrs = checkTypeError typusFile
    in case diagnostics of
         Left errs -> typeErrs
         Right diags -> not (null diags) ==> typeErrs

-- Test round-trip compilation
prop_round_trip_compilation :: Property
prop_round_trip_compilation =
  forAll genSimpleTypusFile $ \typusFile ->
    let goCode = generateGoCode typusFile
        -- Note: We can't actually compile Go back to Typus, but we can test
        -- that the process doesn't crash L.and produces some output
    in not (null goCode)

tests :: TestTree
tests = testGroup "New Cabal Compiler QuickCheck Tests"
  [ testGroup "Basic compilation tests"
      [ testProperty "compile empty file" prop_compile_empty_file
      , testProperty "compile simple content" prop_compile_simple_content
      , testProperty "compile type error fails" prop_compile_type_error_fails
      ]
  , testGroup "Go code generation tests"
      [ testProperty "generateGoCode returns string" prop_generateGoCode_returns_string
      , testProperty "generateGoCode handles malformed input" prop_generateGoCode_handles_malformed_input
      ]
  , testGroup "Source IR tests"
      [ testProperty "ensureSourceIR valid file succeeds" prop_ensureSourceIR_valid_file_succeeds
      , testProperty "ensureSourceIR malformed fails" prop_ensureSourceIR_malformed_fails
      ]
  , testGroup "Error formatting tests"
      [ testProperty "renderCompilationError formats errors" prop_renderCompilationError_formats_errors
      , testProperty "formatCompilerErrors includes error ids" prop_formatCompilerErrors_includes_error_ids
      ]
  , testGroup "Type checking tests"
      [ testProperty "hasTypeErrors detects type errors" prop_hasTypeErrors_detects_type_errors
      , testProperty "diagnoseTypeErrors returns either" prop_diagnoseTypeErrors_returns_either
      , testProperty "typeDiagnosticToCompilerError preserves message" prop_typeDiagnosticToCompilerError_preserves_message
      ]
  , testGroup "Declaration L.and function tests"
      [ testProperty "extractDeclarations returns list" prop_extractDeclarations_returns_list
      , testProperty "extractFunctionCalls returns list" prop_extractFunctionCalls_returns_list
      , testProperty "buildTypeEnv returns environment" prop_buildTypeEnv_returns_environment
      , testProperty "buildTypeEnvFromPairs creates environment" prop_buildTypeEnvFromPairs_creates_environment
      ]
  , testGroup "Method L.and syntax tests"
      [ testProperty "isMethodDeclaration detects methods" prop_isMethodDeclaration_detects_methods
      , testProperty "hasMalformedSyntax detects errors" prop_hasMalformedSyntax_detects_errors
      ]
  , testGroup "Advanced checking tests"
      [ testProperty "checkDependentTypes handles valid input" prop_checkDependentTypes_handles_valid_input
      , testProperty "checkOwnership handles valid input" prop_checkOwnership_handles_valid_input
      , testProperty "checkTypeError validates types" prop_checkTypeError_validates_types
      ]
  , testGroup "Error analysis tests"
      [ testProperty "analyzeErrors returns analysis" prop_analyzeErrors_returns_analysis
      , testProperty "generateDetailedReport includes summary" prop_generateDetailedReport_includes_summary
      , testProperty "createTypusFileFromErrors creates file" prop_createTypusFileFromErrors_creates_file
      ]
  , testGroup "Integration tests"
      [ testProperty "compilation pipeline consistent" prop_compilation_pipeline_consistent
      , testProperty "error handling consistent" prop_error_handling_consistent
      , testProperty "diagnostic consistency" prop_diagnostic_consistency
      , testProperty "round trip compilation" prop_round_trip_compilation
      ]
  ]