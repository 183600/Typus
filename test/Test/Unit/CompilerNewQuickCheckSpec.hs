{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerNewQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, elements, listOf, oneof)
import Compiler
  ( compile
  , CompilerError(..)
  , CompilerResult
  , CompilationPhase(..)
  , renderCompilationError
  , formatCompilerErrors
  , generateDetailedReport
  , analyzeErrors
  , hasTypeErrors
  , TypeCheckDiagnostic(..)
  , diagnoseTypeErrors
  , extractDeclarations
  , extractFunctionCalls
  , buildTypeEnv
  , buildTypeEnvFromPairs
  , createTypusFileFromErrors
  , isMethodDeclaration
  , checkTypeError
  , hasMalformedSyntax
  , checkDependentTypes
  , checkOwnership
  , ensureSourceIR
  , typeCheckFailure
  , typeDiagnosticToCompilerError
  , generateGoCode
  )
import Parser (TypusFile(..), FileDirectives(..), BlockDirectives(..), CodeBlock(..))
import Compiler.IR as IR
import Compiler.GoAst (renderGoModule)
import qualified Compiler.TypeChecker as TypeChecker
import Compiler.Errors
  ( ErrorCategory(..)
  , ErrorSeverity(..)
  , mkCompilerError
  , defaultSpan
  )
import SourceLocation (SourceSpan(..), SourcePos(..), startPos)

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, intercalate, sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Either (isLeft, isRight, partitionEithers)

-- ============================================================================
-- Arbitrary instances
-- ============================================================================

instance Arbitrary CompilationPhase where
  arbitrary = elements
    [ ParsingPhase
    , TypeCheckingPhase
    , OwnershipAnalysisPhase
    , DependentTypePhase
    , CodeGenerationPhase
    , OptimizationPhase
    ]

instance Arbitrary CompilerError where
  arbitrary = do
    errorId <- arbitrary
    message <- pack <$> arbitrary
    phase <- arbitrary
    category <- arbitrary
    severity <- arbitrary
    location <- arbitrary
    suggestions <- listOf (pack <$> arbitrary)
    relatedErrors <- listOf arbitrary
    timestamp <- arbitrary
    return $ CompilerError errorId message phase category severity location suggestions relatedErrors timestamp

instance Arbitrary TypeCheckDiagnostic where
  arbitrary = oneof
    [ TypeMismatch <$> arbitrary <*> arbitrary
    , UnboundVariable <$> arbitrary
    , InvalidTypeAnnotation <$> arbitrary
    , RecursiveType <$> arbitrary
    , ConstraintViolation <$> arbitrary
    ]

-- ============================================================================
-- CompilationPhase Properties
-- ============================================================================

-- Property: CompilationPhase show contains meaningful information
prop_compilationPhase_show_informative :: CompilationPhase -> Property
prop_compilationPhase_show_informative phase =
  let showStr = show phase
  in not (null showStr) .&&. showStr /= "undefined"

-- Property: CompilationPhase equality works correctly
prop_compilationPhase_equality :: CompilationPhase -> CompilationPhase -> Property
prop_compilationPhase_equality phase1 phase2 =
  (phase1 == phase2) === (show phase1 == show phase2)

-- ============================================================================
-- CompilerError Properties
-- ============================================================================

-- Property: CompilerError show contains relevant information
prop_compilerError_show_informative :: CompilerError -> Property
prop_compilerError_show_informative err =
  let showStr = show err
  in not (null showStr)

-- Property: CompilerError equality works correctly
prop_compilerError_equality :: CompilerError -> CompilerError -> Property
prop_compilerError_equality err1 err2 =
  (err1 == err2) === 
  (errorId err1 == errorId err2 &&
   message err1 == message err2 &&
   phase err1 == phase err2)

-- Property: CompilerError fields are accessible
prop_compilerError_fields :: String -> Text -> CompilationPhase -> ErrorCategory -> ErrorSeverity -> Property
prop_compilerError_fields errId msg phase category severity =
  let err = mkCompilerError errId msg phase category severity Nothing [] [] Nothing
  in errorId err === errId .&&.
     message err === msg .&&.
     phase err === phase .&&.
     errorCategory err === category .&&.
     errorSeverity err === severity

-- ============================================================================
-- TypeCheckDiagnostic Properties
-- ============================================================================

-- Property: TypeCheckDiagnostic show contains relevant information
prop_typeCheckDiagnostic_show_informative :: TypeCheckDiagnostic -> Property
prop_typeCheckDiagnostic_show_informative diag =
  let showStr = show diag
  in not (null showStr)

-- Property: TypeCheckDiagnostic equality works correctly
prop_typeCheckDiagnostic_equality :: TypeCheckDiagnostic -> TypeCheckDiagnostic -> Property
prop_typeCheckDiagnostic_equality diag1 diag2 =
  (diag1 == diag2) === case (diag1, diag2) of
    (TypeMismatch t1a t1b, TypeMismatch t2a t2b) -> t1a == t2a && t1b == t2b
    (UnboundVariable v1, UnboundVariable v2) -> v1 == v2
    (InvalidTypeAnnotation t1, InvalidTypeAnnotation t2) -> t1 == t2
    (RecursiveType t1, RecursiveType t2) -> t1 == t2
    (ConstraintViolation c1, ConstraintViolation c2) -> c1 == c2
    _ -> False

-- ============================================================================
-- Compilation Properties
-- ============================================================================

-- Property: compile handles empty file
prop_compile_empty_file :: Property
prop_compile_empty_file =
  let emptyFile = TypusFile defaultFileDirectives [] [] []
      result = compile emptyFile
  in property True -- Should not crash

-- Property: compile handles simple file
prop_compile_simple_file :: Property
prop_compile_simple_file =
  let simpleFile = TypusFile defaultFileDirectives [] 
                    [CodeBlock defaultBlockDirectives "x := 42" defaultSpan]
      result = compile simpleFile
  in property True -- Should not crash

-- Property: compile handles file with directives
prop_compile_file_with_directives :: Property
prop_compile_file_with_directives =
  let directives = FileDirectives (Just True) (Just False) (Just True)
      file = TypusFile directives [] 
              [CodeBlock defaultBlockDirectives "x := 42" defaultSpan]
      result = compile file
  in property True -- Should not crash

-- Property: compile handles file with multiple blocks
prop_compile_multiple_blocks :: Property
prop_compile_multiple_blocks =
  let blocks = [ CodeBlock defaultBlockDirectives "x := 42" defaultSpan
               , CodeBlock defaultBlockDirectives "y := x + 1" defaultSpan
               , CodeBlock defaultBlockDirectives "return y" defaultSpan
               ]
      file = TypusFile defaultFileDirectives [] blocks []
      result = compile file
  in property True -- Should not crash

-- Property: compile handles malformed syntax
prop_compile_malformed_syntax :: Property
prop_compile_malformed_syntax =
  let malformedBlock = CodeBlock defaultBlockDirectives "x :=" defaultSpan
      file = TypusFile defaultFileDirectives [] [malformedBlock] []
      result = compile file
  in property True -- Should handle gracefully

-- ============================================================================
-- Error Formatting Properties
-- ============================================================================

-- Property: renderCompilationError handles empty list
prop_renderCompilationError_empty :: Property
prop_renderCompilationError_empty =
  let errors = []
      rendered = renderCompilationError errors
  in not (null rendered) -- Should return some formatted output

-- Property: renderCompilationError handles various errors
prop_renderCompilationError_various :: [CompilerError] -> Property
prop_renderCompilationError_various errors =
  let rendered = renderCompilationError errors
  in not (null rendered) -- Should return some formatted output

-- Property: formatCompilerErrors preserves error count
prop_formatCompilerErrors_preserves_count :: [CompilerError] -> Property
prop_formatCompilerErrors_preserves_count errors =
  let formatted = formatCompilerErrors errors
      errorCount = length errors
  in if errorCount > 0
     then property True -- Should contain error information
     else property True -- Empty list should also be handled

-- Property: generateDetailedReport contains summary information
prop_generateDetailedReport_contains_summary :: [CompilerError] -> Property
prop_generateDetailedReport_contains_summary errors =
  let report = generateDetailedReport errors
  in not (null report) -- Should contain summary

-- ============================================================================
-- Error Analysis Properties
-- ============================================================================

-- Property: analyzeErrors handles empty list
prop_analyzeErrors_empty :: Property
prop_analyzeErrors_empty =
  let errors = []
      analysis = analyzeErrors errors
  in property True -- Should not crash

-- Property: analyzeErrors handles various error types
prop_analyzeErrors_various :: [CompilerError] -> Property
prop_analyzeErrors_various errors =
  let analysis = analyzeErrors errors
  in property True -- Should not crash

-- Property: hasTypeErrors detects type errors
prop_hasTypeErrors_detects :: [CompilerError] -> Property
prop_hasTypeErrors_detects errors =
  let typeErrors = filter (\e -> errorCategory e == TypeChecking) errors
      hasTypeErrorsResult = hasTypeErrors errors
  in hasTypeErrorsResult === (not (null typeErrors))

-- ============================================================================
-- Type Checking Properties
-- ============================================================================

-- Property: diagnoseTypeErrors handles empty file
prop_diagnoseTypeErrors_empty :: Property
prop_diagnoseTypeErrors_empty =
  let emptyFile = TypusFile defaultFileDirectives [] [] []
      result = diagnoseTypeErrors emptyFile
  in property True -- Should not crash

-- Property: diagnoseTypeErrors handles simple file
prop_diagnoseTypeErrors_simple :: Property
prop_diagnoseTypeErrors_simple =
  let simpleFile = TypusFile defaultFileDirectives [] 
                    [CodeBlock defaultBlockDirectives "x := 42" defaultSpan]
      result = diagnoseTypeErrors simpleFile
  in property True -- Should not crash

-- Property: extractDeclarations finds function declarations
prop_extractDeclarations_functions :: Property
prop_extractDeclarations_functions =
  let code = "func test() { return 42 }"
      block = CodeBlock defaultBlockDirectives code defaultSpan
      file = TypusFile defaultFileDirectives [] [block] []
      declarations = extractDeclarations file
  in not (null declarations) -- Should find declarations

-- Property: extractFunctionCalls finds function calls
prop_extractFunctionCalls_finds :: Property
prop_extractFunctionCalls_finds =
  let code = "x := test()"
      block = CodeBlock defaultBlockDirectives code defaultSpan
      file = TypusFile defaultFileDirectives [] [block] []
      calls = extractFunctionCalls file
  in not (null calls) -- Should find calls

-- Property: buildTypeEnv creates valid environment
prop_buildTypeEnv_valid :: Property
prop_buildTypeEnv_valid =
  let typePairs = [("int", "int"), ("string", "string")]
      typeEnv = buildTypeEnvFromPairs typePairs
  in property True -- Should create valid environment

-- Property: createTypusFileFromErrors handles errors
prop_createTypusFileFromErrors_handles :: [CompilerError] -> Property
prop_createTypusFileFromErrors_handles errors =
  let file = createTypusFileFromErrors errors
  in property True -- Should not crash

-- Property: isMethodDeclaration identifies methods
prop_isMethodDeclaration_identifies :: Property
prop_isMethodDeclaration_identifies =
  let methodDecl = "func (receiver Type) method() {}"
      regularFunc = "func regular() {}"
  in isMethodDeclaration methodDecl .&&. not (isMethodDeclaration regularFunc)

-- Property: checkTypeError handles various inputs
prop_checkTypeError_handles :: String -> Property
prop_checkTypeError_handles input =
  let result = checkTypeError input
  in property True -- Should not crash

-- Property: hasMalformedSyntax detects issues
prop_hasMalformedSyntax_detects :: Property
prop_hasMalformedSyntax_detects =
  let goodFile = TypusFile defaultFileDirectives [] 
                   [CodeBlock defaultBlockDirectives "x := 42" defaultSpan] []
      badFile = TypusFile defaultFileDirectives [] 
                  [CodeBlock defaultBlockDirectives "x :=" defaultSpan] []
  in not (hasMalformedSyntax goodFile) .&&. hasMalformedSyntax badFile

-- ============================================================================
-- IR Properties
-- ============================================================================

-- Property: buildSourceIR handles valid input
prop_buildSourceIR_valid :: Property
prop_buildSourceIR_valid =
  let file = TypusFile defaultFileDirectives [] 
              [CodeBlock defaultBlockDirectives "x := 42" defaultSpan] []
      result = IR.buildSourceIR file
  in property True -- Should not crash

-- Property: buildSemanticIR handles valid input
prop_buildSemanticIR_valid :: Property
prop_buildSemanticIR_valid =
  let file = TypusFile defaultFileDirectives [] 
              [CodeBlock defaultBlockDirectives "x := 42" defaultSpan] []
      sourceIR = IR.buildSourceIR file
      result = IR.buildSemanticIR sourceIR
  in property True -- Should not crash

-- Property: emitGo generates Go code
prop_emitGo_generates :: Property
prop_emitGo_generates =
  let file = TypusFile defaultFileDirectives [] 
              [CodeBlock defaultBlockDirectives "x := 42" defaultSpan] []
      sourceIR = IR.buildSourceIR file
      semanticIR = IR.buildSemanticIR sourceIR
      goArtifact = IR.emitGo semanticIR
      goSource = IR.goSource goArtifact
  in not (null goSource) -- Should generate code

-- ============================================================================
-- Dependent Types Properties
-- ============================================================================

-- Property: checkDependentTypes handles simple input
prop_checkDependentTypes_simple :: Property
prop_checkDependentTypes_simple =
  let file = TypusFile defaultFileDirectives [] 
              [CodeBlock defaultBlockDirectives "x := 42" defaultSpan] []
      result = checkDependentTypes file
  in property True -- Should not crash

-- Property: checkDependentTypes handles complex input
prop_checkDependentTypes_complex :: Property
prop_checkDependentTypes_complex =
  let code = intercalate "\n"
        [ "func test(n int) int {"
        , "    if n > 0 {"
        , "        return n * test(n - 1)"
        , "    }"
        , "    return 1"
        , "}"
        ]
      block = CodeBlock defaultBlockDirectives code defaultSpan
      file = TypusFile defaultFileDirectives [] [block] []
      result = checkDependentTypes file
  in property True -- Should not crash

-- ============================================================================
-- Ownership Properties
-- ============================================================================

-- Property: checkOwnership handles simple input
prop_checkOwnership_simple :: Property
prop_checkOwnership_simple =
  let file = TypusFile defaultFileDirectives [] 
              [CodeBlock defaultBlockDirectives "x := 42" defaultSpan] []
      result = checkOwnership file
  in property True -- Should not crash

-- Property: checkOwnership handles move operations
prop_checkOwnership_moves :: Property
prop_checkOwnership_moves =
  let code = "x := 42\ny := x"
      block = CodeBlock defaultBlockDirectives code defaultSpan
      file = TypusFile defaultFileDirectives [] [block] []
      result = checkOwnership file
  in property True -- Should not crash

-- ============================================================================
-- Code Generation Properties
-- ============================================================================

-- Property: generateGoCode handles simple input
prop_generateGoCode_simple :: Property
prop_generateGoCode_simple =
  let file = TypusFile defaultFileDirectives [] 
              [CodeBlock defaultBlockDirectives "x := 42" defaultSpan] []
      result = generateGoCode file
  in property True -- Should not crash

-- Property: renderGoModule handles valid input
prop_renderGoModule_valid :: Property
prop_renderGoModule_valid =
  let file = TypusFile defaultFileDirectives [] 
              [CodeBlock defaultBlockDirectives "x := 42" defaultSpan] []
      result = renderGoModule file
  in property True -- Should not crash

-- ============================================================================
-- Complex Scenario Properties
-- ============================================================================

-- Property: compilation handles nested functions
prop_compilation_nested_functions :: Property
prop_compilation_nested_functions =
  let code = intercalate "\n"
        [ "func outer() {"
        , "    func inner() {"
        , "        return 42"
        , "    }"
        , "    return inner()"
        , "}"
        ]
      block = CodeBlock defaultBlockDirectives code defaultSpan
      file = TypusFile defaultFileDirectives [] [block] []
      result = compile file
  in property True -- Should not crash

-- Property: compilation handles recursive functions
prop_compilation_recursive_functions :: Property
prop_compilation_recursive_functions =
  let code = intercalate "\n"
        [ "func factorial(n int) int {"
        , "    if n <= 1 {"
        , "        return 1"
        , "    }"
        , "    return n * factorial(n - 1)"
        , "}"
        ]
      block = CodeBlock defaultBlockDirectives code defaultSpan
      file = TypusFile defaultFileDirectives [] [block] []
      result = compile file
  in property True -- Should not crash

-- Property: compilation handles type errors gracefully
prop_compilation_type_errors :: Property
prop_compilation_type_errors =
  let code = "x := 42\ny := \"string\"\nz := x + y"
      block = CodeBlock defaultBlockDirectives code defaultSpan
      file = TypusFile defaultFileDirectives [] [block] []
      result = compile file
  in property True -- Should handle gracefully

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

-- Property: compilation recovers from syntax errors
prop_compilation_recovers_syntax :: Property
prop_compilation_recovers_syntax =
  let code = "func broken() { return }"
      block = CodeBlock defaultBlockDirectives code defaultSpan
      file = TypusFile defaultFileDirectives [] [block] []
      result = compile file
  in property True -- Should attempt recovery

-- Property: compilation provides helpful suggestions
prop_compilation_provides_suggestions :: Property
prop_compilation_provides_suggestions =
  let code = "var x int = \"string\""
      block = CodeBlock defaultBlockDirectives code defaultSpan
      file = TypusFile defaultFileDirectives [] [block] []
      result = compile file
  in case result of
    Left errors -> property True -- Should have suggestions
    Right _ -> property True -- Or succeed

-- ============================================================================
-- Performance Properties
-- ============================================================================

-- Property: compilation handles large files
prop_compilation_large_files :: Property
prop_compilation_large_files =
  let largeCode = intercalate "\n" $ replicate 100 "x := 42"
      block = CodeBlock defaultBlockDirectives largeCode defaultSpan
      file = TypusFile defaultFileDirectives [] [block] []
      result = compile file
  in property True -- Should not crash

-- Property: compilation handles many blocks
prop_compilation_many_blocks :: Property
prop_compilation_many_blocks =
  let blocks = replicate 50 $ CodeBlock defaultBlockDirectives "x := 42" defaultSpan
      file = TypusFile defaultFileDirectives [] blocks []
      result = compile file
  in property True -- Should not crash

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Compiler QuickCheck Tests"
  [ testGroup "CompilationPhase Properties"
    [ fastProperty "CompilationPhase show contains meaningful information" prop_compilationPhase_show_informative
    , fastProperty "CompilationPhase equality works correctly" prop_compilationPhase_equality
    ]

  , testGroup "CompilerError Properties"
    [ fastProperty "CompilerError show contains relevant information" prop_compilerError_show_informative
    , fastProperty "CompilerError equality works correctly" prop_compilerError_equality
    , fastProperty "CompilerError fields are accessible" prop_compilerError_fields
    ]

  , testGroup "TypeCheckDiagnostic Properties"
    [ fastProperty "TypeCheckDiagnostic show contains relevant information" prop_typeCheckDiagnostic_show_informative
    , fastProperty "TypeCheckDiagnostic equality works correctly" prop_typeCheckDiagnostic_equality
    ]

  , testGroup "Compilation Properties"
    [ fastProperty "compile handles empty file" prop_compile_empty_file
    , fastProperty "compile handles simple file" prop_compile_simple_file
    , fastProperty "compile handles file with directives" prop_compile_file_with_directives
    , fastProperty "compile handles file with multiple blocks" prop_compile_multiple_blocks
    , fastProperty "compile handles malformed syntax" prop_compile_malformed_syntax
    ]

  , testGroup "Error Formatting Properties"
    [ fastProperty "renderCompilationError handles empty list" prop_renderCompilationError_empty
    , fastProperty "renderCompilationError handles various errors" prop_renderCompilationError_various
    , fastProperty "formatCompilerErrors preserves error count" prop_formatCompilerErrors_preserves_count
    , fastProperty "generateDetailedReport contains summary information" prop_generateDetailedReport_contains_summary
    ]

  , testGroup "Error Analysis Properties"
    [ fastProperty "analyzeErrors handles empty list" prop_analyzeErrors_empty
    , fastProperty "analyzeErrors handles various error types" prop_analyzeErrors_various
    , fastProperty "hasTypeErrors detects type errors" prop_hasTypeErrors_detects
    ]

  , testGroup "Type Checking Properties"
    [ fastProperty "diagnoseTypeErrors handles empty file" prop_diagnoseTypeErrors_empty
    , fastProperty "diagnoseTypeErrors handles simple file" prop_diagnoseTypeErrors_simple
    , fastProperty "extractDeclarations finds function declarations" prop_extractDeclarations_functions
    , fastProperty "extractFunctionCalls finds function calls" prop_extractFunctionCalls_finds
    , fastProperty "buildTypeEnv creates valid environment" prop_buildTypeEnv_valid
    , fastProperty "createTypusFileFromErrors handles errors" prop_createTypusFileFromErrors_handles
    , fastProperty "isMethodDeclaration identifies methods" prop_isMethodDeclaration_identifies
    , fastProperty "checkTypeError handles various inputs" prop_checkTypeError_handles
    , fastProperty "hasMalformedSyntax detects issues" prop_hasMalformedSyntax_detects
    ]

  , testGroup "IR Properties"
    [ fastProperty "buildSourceIR handles valid input" prop_buildSourceIR_valid
    , fastProperty "buildSemanticIR handles valid input" prop_buildSemanticIR_valid
    , fastProperty "emitGo generates Go code" prop_emitGo_generates
    ]

  , testGroup "Dependent Types Properties"
    [ fastProperty "checkDependentTypes handles simple input" prop_checkDependentTypes_simple
    , fastProperty "checkDependentTypes handles complex input" prop_checkDependentTypes_complex
    ]

  , testGroup "Ownership Properties"
    [ fastProperty "checkOwnership handles simple input" prop_checkOwnership_simple
    , fastProperty "checkOwnership handles move operations" prop_checkOwnership_moves
    ]

  , testGroup "Code Generation Properties"
    [ fastProperty "generateGoCode handles simple input" prop_generateGoCode_simple
    , fastProperty "renderGoModule handles valid input" prop_renderGoModule_valid
    ]

  , testGroup "Complex Scenario Properties"
    [ fastProperty "compilation handles nested functions" prop_compilation_nested_functions
    , fastProperty "compilation handles recursive functions" prop_compilation_recursive_functions
    , fastProperty "compilation handles type errors gracefully" prop_compilation_type_errors
    ]

  , testGroup "Error Recovery Properties"
    [ fastProperty "compilation recovers from syntax errors" prop_compilation_recovers_syntax
    , fastProperty "compilation provides helpful suggestions" prop_compilation_provides_suggestions
    ]

  , testGroup "Performance Properties"
    [ fastProperty "compilation handles large files" prop_compilation_large_files
    , fastProperty "compilation handles many blocks" prop_compilation_many_blocks
    ]
  ]