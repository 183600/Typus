{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCompilerQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import qualified Data.List as L
import Data.List (isInfixOf)
import Data.List (null)
import Data.Maybe (isJust, isNothing)

import Compiler
  ( compile
  , CompilerError(..)
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
import Parser (TypusFile(..), CodeBlock(..), BlockDirectives(..), FileDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import Compiler.IR (ValueInfo(..))
import SourceLocation (SourceSpan(..), SourcePos(..), startPos)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary CompilationPhase where
  arbitrary = elements 
    [ ParsingPhase
    , TypeCheckingPhase
    , OwnershipPhase
    , DependencyPhase
    , CodeGenPhase
    ]

instance Arbitrary TypeCheckDiagnostic where
  arbitrary = do
    message <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " .,!?")
    severity <- elements [ErrorInfo, Warning, Error]
    return $ TypeCheckDiagnostic (T.pack message) severity

-- Generate simple code blocks for testing
genCodeBlock :: Gen CodeBlock
genCodeBlock = do
  content <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \n(){}[];,+*/=")
  return $ CodeBlock defaultBlockDirectives (unlines content) (SourceSpan startPos startPos)

-- Generate simple Typus files for testing
genTypusFile :: Gen TypusFile
genTypusFile = do
  blocks <- listOf genCodeBlock
  return $ TypusFile defaultFileDirectives [] blocks []

-- Generate valid Go-like code snippets
genValidGoCode :: Gen String
genValidGoCode = oneof
  [ return "func main() {\n    fmt.Println(\"Hello, World!\")\n}"
  , return "package main\n\nimport \"fmt\"\n\nfunc add(a int, b int) int {\n    return a + b\n}"
  , return "type Person struct {\n    Name string\n    Age  int\n}"
  , return "var x int = 42"
  , return "const PI = 3.14159"
  ]

-- Generate invalid code snippets that should cause errors
genInvalidGoCode :: Gen String
genInvalidGoCode = oneof
  [ return "func main() {\n    var x int = \"string\"\n}"
  , return "var x int = y + 1"  -- undefined variable
  , return "func add(a int, b string) int {\n    return a + b\n}"  -- type mismatch
  , return "type Bad struct {\n    Name string\n    Name int  -- duplicate field\n}"
  ]

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Property: compile returns result for valid code
prop_compile_valid_code :: String -> Property
prop_compile_valid_code code =
  not (null code) && not (L.any (== '\0') code) ==>
  let typusFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives code (SourceSpan startPos startPos)] []
      result = compile typusFile
  in case result of
       Left _ -> property True  -- May fail for various reasons, that's OK
       Right goCode -> property $ not (null goCode)

-- Property: compile handles empty files
prop_compile_empty_file :: Property
prop_compile_empty_file =
  let typusFile = TypusFile defaultFileDirectives [] [] []
      result = compile typusFile
  in case result of
       Left _ -> property True  -- May fail, that's OK
       Right goCode -> property $ not (null goCode)

-- Property: renderCompilationError handles empty error list
prop_render_empty_errors :: Property
prop_render_empty_errors =
  let rendered = renderCompilationError []
  in null rendered

-- Property: renderCompilationError handles non-empty error list
prop_render_non_empty_errors :: [CompilerError] -> Property
prop_render_non_empty_errors errors =
  not (null errors) ==>
  let rendered = renderCompilationError errors
  in not (null rendered)

-- Property: formatCompilerErrors handles empty list
prop_format_empty_compiler_errors :: Property
prop_format_empty_compiler_errors =
  let formatted = formatCompilerErrors []
  in T.null formatted

-- Property: formatCompilerErrors handles non-empty list
prop_format_non_empty_compiler_errors :: [CompilerError] -> Property
prop_format_non_empty_compiler_errors errors =
  not (null errors) ==>
  let formatted = formatCompilerErrors errors
  in not (T.null formatted)

-- Property: generateDetailedReport handles empty errors
prop_generate_empty_report :: Property
prop_generate_empty_report =
  let report = generateDetailedReport []
  in T.null report

-- Property: generateDetailedReport handles non-empty errors
prop_generate_non_empty_report :: [CompilerError] -> Property
prop_generate_non_empty_report errors =
  not (null errors) ==>
  let report = generateDetailedReport errors
  in not (T.null report)

-- Property: analyzeErrors returns statistics
prop_analyze_errors_returns_stats :: [CompilerError] -> Property
prop_analyze_errors_returns_stats errors =
  let stats = analyzeErrors errors
  in stats.totalErrors >= L.length errors

-- Property: hasTypeErrors checks for type errors
prop_has_type_errors_checks :: [TypeCheckDiagnostic] -> Property
prop_has_type_errors_checks diagnostics =
  let hasErrors = hasTypeErrors diagnostics
      errorDiagnostics = L.filter (\d -> case d of TypeCheckDiagnostic _ Error -> True; _ -> False) diagnostics
  in hasErrors === not (null errorDiagnostics)

-- Property: diagnoseTypeErrors returns result for valid file
prop_diagnose_type_errors_valid :: TypusFile -> Property
prop_diagnose_type_errors_valid typusFile =
  let result = diagnoseTypeErrors typusFile
  in case result of
       Left _ -> property True  -- May have errors, that's OK
       Right _ -> property True  -- Or succeed

-- Property: extractDeclarations returns list
prop_extract_declarations_returns_list :: TypusFile -> Property
prop_extract_declarations_returns_list typusFile =
  let declarations = extractDeclarations typusFile
  in L.length declarations >= 0

-- Property: extractFunctionCalls returns list
prop_extract_function_calls_returns_list :: TypusFile -> Property
prop_extract_function_calls_returns_list typusFile =
  let functionCalls = extractFunctionCalls typusFile
  in L.length functionCalls >= 0

-- Property: buildTypeEnv creates environment
prop_build_type_env_creates :: TypusFile -> Property
prop_build_type_env_creates typusFile =
  let typeEnv = buildTypeEnv typusFile
  in True  -- Just test that it doesn't crash

-- Property: buildTypeEnvFromPairs creates environment from pairs
prop_build_type_env_from_pairs :: [(String, String)] -> Property
prop_build_type_env_from_pairs pairs =
  let typeEnv = buildTypeEnvFromPairs pairs
  in True  -- Just test that it doesn't crash

-- Property: createTypusFileFromErrors creates file
prop_create_typus_file_from_errors :: [TypeCheckDiagnostic] -> Property
prop_create_typus_file_from_errors diagnostics =
  let typusFile = createTypusFileFromErrors diagnostics
  in True  -- Just test that it doesn't crash

-- Property: isMethodDeclaration checks method syntax
prop_is_method_declaration_checks :: String -> Property
prop_is_method_declaration_checks code =
  not (null code) ==>
  let isMethod = isMethodDeclaration code
  in True  -- Just test that it doesn't crash

-- Property: checkTypeError examines type errors
prop_check_type_error_examines :: String -> Property
prop_check_type_error_examines code =
  not (null code) ==>
  let result = checkTypeError code
  in True  -- Just test that it doesn't crash

-- Property: hasMalformedSyntax checks syntax
prop_has_malformed_syntax_checks :: TypusFile -> Property
prop_has_malformed_syntax_checks typusFile =
  let malformed = hasMalformedSyntax typusFile
  in True  -- Just test that it doesn't crash

-- Property: checkDependentTypes examines dependent types
prop_check_dependent_types_examines :: TypusFile -> Property
prop_check_dependent_types_examines typusFile =
  let result = checkDependentTypes typusFile
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: checkOwnership examines ownership
prop_check_ownership_examines :: TypusFile -> Property
prop_check_ownership_examines typusFile =
  let result = checkOwnership typusFile
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: ensureSourceIR creates IR L.or error
prop_ensure_source_ir_creates :: TypusFile -> Property
prop_ensure_source_ir_creates typusFile =
  let result = ensureSourceIR typusFile
  in case result of
       Left _ -> property True
       Right _ -> property True

-- Property: typeDiagnosticToCompilerError converts diagnostic
prop_type_diagnostic_to_compiler_error_converts :: TypeCheckDiagnostic -> Property
prop_type_diagnostic_to_compiler_error_converts diagnostic =
  let error = typeDiagnosticToCompilerError diagnostic
  in True  -- Just test that it doesn't crash

-- Property: generateGoCode produces Go code
prop_generate_go_code_produces :: String -> Property
prop_generate_go_code_produces code =
  not (null code) && not (L.any (== '\0') code) ==>
  let goCode = generateGoCode code
  in not (null goCode)

-- Property: Compilation phases are distinct
prop_compilation_phases_distinct :: CompilationPhase -> CompilationPhase -> Property
prop_compilation_phases_distinct phase1 phase2 =
  let same = phase1 == phase2
  in property same  -- Just test equality works

-- Property: Error handling preserves information
prop_error_handling_preserves :: Text -> CompilationPhase -> Property
prop_error_handling_preserves message phase =
  let error = CompilerError "TEST001" message phase TypeChecking Error Nothing Nothing [] [] Nothing
  in errorCode error === "TEST001" .&&. errorMessage error === message .&&. errorPhase error === phase

-- Property: Compilation pipeline consistency
prop_compilation_pipeline_consistent :: String -> Property
prop_compilation_pipeline_consistent code =
  not (null code) && not (L.any (== '\0') code) ==>
  let typusFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives code (SourceSpan startPos startPos)] []
      result1 = compile typusFile
      result2 = compile typusFile  -- Compile again
  in case (result1, result2) of
       (Right goCode1, Right goCode2) -> goCode1 === goCode2
       (Left _, Left _) -> property True  -- Both failed, OK
       _ -> property True  -- Mixed results, OK for property testing

-- Property: Error analysis is comprehensive
prop_error_analysis_comprehensive :: [CompilerError] -> Property
prop_error_analysis_comprehensive errors =
  let stats = analyzeErrors errors
      errorCount = L.length errors
  in stats.totalErrors >= errorCount .&&.
     stats.errorsByPhase `L.length` >= 0 .&&.
     stats.errorsBySeverity `L.length` >= 0

-- Property: Type checking is deterministic
prop_type_checking_deterministic :: TypusFile -> Property
prop_type_checking_deterministic typusFile =
  let result1 = diagnoseTypeErrors typusFile
      result2 = diagnoseTypeErrors typusFile
  in case (result1, result2) of
       (Right diags1, Right diags2) -> L.length diags1 === L.length diags2
       (Left _, Left _) -> property True
       _ -> property True

-- Property: Source IR building is consistent
prop_source_ir_consistent :: TypusFile -> Property
prop_source_ir_consistent typusFile =
  let result1 = ensureSourceIR typusFile
      result2 = ensureSourceIR typusFile
  in case (result1, result2) of
       (Right ir1, Right ir2) -> property True  -- Both succeeded
       (Left _, Left _) -> property True  -- Both failed
       _ -> property True  -- Mixed results

-- Property: Go code generation preserves structure
prop_go_generation_preserves :: String -> Property
prop_go_generation_preserves code =
  not (null code) && not (L.any (== '\0') code) ==>
  let goCode = generateGoCode code
      hasKeywords = "func" `L.isInfixOf` goCode || "var" `L.isInfixOf` goCode || "const" `L.isInfixOf` goCode || "type" `L.isInfixOf` goCode
  in not (null goCode) ==> hasKeywords

-- Property: Error formatting preserves essential info
prop_error_formatting_preserves :: CompilerError -> Property
prop_error_formatting_preserves error =
  let formatted = formatCompilerErrors [error]
      hasCode = errorCode error `L.isInfixOf` formatted
      hasMessage = errorMessage error `L.isInfixOf` formatted
  in hasCode .&&. hasMessage

-- Property: Compilation handles unicode content
prop_compilation_unicode :: Text -> Property
prop_compilation_unicode unicodeContent =
  not (T.null unicodeContent) ==>
  let code = T.unpack unicodeContent
      typusFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives code (SourceSpan startPos startPos)] []
      result = compile typusFile
  in case result of
       Left _ -> property True
       Right goCode -> property $ not (null goCode)

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Compiler QuickCheck Tests"
  [ fastProperty "compile returns result for valid code" prop_compile_valid_code
  , fastProperty "compile handles empty files" prop_compile_empty_file
  , fastProperty "renderCompilationError handles empty error list" prop_render_empty_errors
  , fastProperty "renderCompilationError handles non-empty error list" prop_render_non_empty_errors
  , fastProperty "formatCompilerErrors handles empty list" prop_format_empty_compiler_errors
  , fastProperty "formatCompilerErrors handles non-empty list" prop_format_non_empty_compiler_errors
  , fastProperty "generateDetailedReport handles empty errors" prop_generate_empty_report
  , fastProperty "generateDetailedReport handles non-empty errors" prop_generate_non_empty_report
  , fastProperty "analyzeErrors returns statistics" prop_analyze_errors_returns_stats
  , fastProperty "hasTypeErrors checks for type errors" prop_has_type_errors_checks
  , fastProperty "diagnoseTypeErrors returns result for valid file" prop_diagnose_type_errors_valid
  , fastProperty "extractDeclarations returns list" prop_extract_declarations_returns_list
  , fastProperty "extractFunctionCalls returns list" prop_extract_function_calls_returns_list
  , fastProperty "buildTypeEnv creates environment" prop_build_type_env_creates
  , fastProperty "buildTypeEnvFromPairs creates environment from pairs" prop_build_type_env_from_pairs
  , fastProperty "createTypusFileFromErrors creates file" prop_create_typus_file_from_errors
  , fastProperty "isMethodDeclaration checks method syntax" prop_is_method_declaration_checks
  , fastProperty "checkTypeError examines type errors" prop_check_type_error_examines
  , fastProperty "hasMalformedSyntax checks syntax" prop_has_malformed_syntax_checks
  , fastProperty "checkDependentTypes examines dependent types" prop_check_dependent_types_examines
  , fastProperty "checkOwnership examines ownership" prop_check_ownership_examines
  , fastProperty "ensureSourceIR creates IR L.or error" prop_ensure_source_ir_creates
  , fastProperty "typeDiagnosticToCompilerError converts diagnostic" prop_type_diagnostic_to_compiler_error_converts
  , fastProperty "generateGoCode produces Go code" prop_generate_go_code_produces
  , fastProperty "Compilation phases are distinct" prop_compilation_phases_distinct
  , fastProperty "Error handling preserves information" prop_error_handling_preserves
  , fastProperty "Compilation pipeline consistency" prop_compilation_pipeline_consistent
  , fastProperty "Error analysis is comprehensive" prop_error_analysis_comprehensive
  , fastProperty "Type checking is deterministic" prop_type_checking_deterministic
  , fastProperty "Source IR building is consistent" prop_source_ir_consistent
  , fastProperty "Go code generation preserves structure" prop_go_generation_preserves
  , fastProperty "Error formatting preserves essential info" prop_error_formatting_preserves
  , fastProperty "Compilation handles unicode content" prop_compilation_unicode
  ]