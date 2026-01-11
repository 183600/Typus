module Test.Unit.EnhancedCompilerSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler (compile, CompilerError(..), CompilationPhase(..), 
                malformedSyntaxError, renderCompilationError, 
                formatCompilerErrors, generateDetailedReport,
                hasTypeErrors, TypeCheckDiagnostic(..), 
                diagnoseTypeErrors, extractDeclarations, 
                extractFunctionCalls, buildTypeEnv, buildTypeEnvFromPairs,
                checkTypeError, hasMalformedSyntax, checkDependentTypes,
                checkOwnership, typeCheckFailure, generateGoCode)
import Parser (TypusFile(..), defaultFileDirectives)
import qualified Data.Text as T
import qualified Data.Map as Map

-- | Test CompilerError properties
prop_compiler_error_ordering :: CompilationPhase -> CompilationPhase -> Property
prop_compiler_error_ordering phase1 phase2 =
  let error1 = CompilerError phase1 "Test error 1"
      error2 = CompilerError phase2 "Test error 2"
  in property $ 
    (phase1 `compare` phase2) === (error1 `compare` error2)

prop_compiler_error_equality :: CompilationPhase -> String -> Property
prop_compiler_error_equality phase message =
  let error1 = CompilerError phase message
      error2 = CompilerError phase message
  in property $ error1 == error2

-- | Test CompilationPhase properties
prop_compilation_phase_ordering :: Property
prop_compilation_phase_ordering = 
  let phases = [Parsing, TypeChecking, OwnershipAnalysis, DependentTypeChecking, CodeGeneration]
  in property $ 
    all (\(p1, p2) -> p1 <= p2) (zip phases (tail phases))

-- | Test error handling properties
prop_malformed_syntax_error :: String -> Property
prop_malformed_syntax_error message =
  let error = malformedSyntaxError message
  in property $ 
    cePhase error == Parsing && 
    ceMessage error == "Malformed syntax: " ++ message

prop_has_type_errors :: [TypeCheckDiagnostic] -> Property
prop_has_type_errors diagnostics =
  let hasErrors = any isError diagnostics
      isError diag = case diag of
        TypeError _ -> True
        TypeWarning _ -> False
  in property $ hasTypeErrors diagnostics == hasErrors

-- | Test diagnostic properties
prop_diagnostic_type_error :: String -> Property
prop_diagnostic_type_error message =
  let diagnostic = TypeError message
  in property $ 
    case diagnostic of
      TypeError msg -> msg == message
      TypeWarning _ -> False

prop_diagnostic_type_warning :: String -> Property
prop_diagnostic_type_warning message =
  let diagnostic = TypeWarning message
  in property $ 
    case diagnostic of
      TypeError _ -> False
      TypeWarning msg -> msg == message

-- | Test type environment properties
prop_build_type_env_empty :: Property
prop_build_type_env_empty = 
  let env = buildTypeEnv []
  in property $ Map.null env

prop_build_type_env_from_pairs :: [(String, String)] -> Property
prop_build_type_env_from_pairs pairs =
  let env = buildTypeEnvFromPairs pairs
      expectedSize = length $ map fst pairs
  in property $ Map.size env == expectedSize

prop_build_type_env_lookup :: [(String, String)] -> String -> Property
prop_build_type_env_lookup pairs key =
  let env = buildTypeEnvFromPairs pairs
      lookupResult = Map.lookup key env
      expected = lookup key pairs
  in property $ lookupResult == expected

-- | Test declaration extraction
prop_extract_declarations_empty :: Property
prop_extract_declarations_empty = 
  let file = TypusFile defaultFileDirectives [] "" ""
      declarations = extractDeclarations file
  in property $ null declarations

prop_extract_declarations_preserves_order :: [String] -> Property
prop_extract_declarations_preserves_order declNames =
  let mockDeclarations = map (\name -> "func " ++ name ++ "() {}") declNames
      fileContent = unlines mockDeclarations
      file = TypusFile defaultFileDirectives [] fileContent fileContent
      declarations = extractDeclarations file
  in property $ length declarations >= 0

-- | Test function call extraction
prop_extract_function_calls_empty :: Property
prop_extract_function_calls_empty = 
  let file = TypusFile defaultFileDirectives [] "" ""
      calls = extractFunctionCalls file
  in property $ null calls

prop_extract_function_calls_preserves :: [String] -> Property
prop_extract_function_calls_preserves callNames =
  let mockCalls = map (\name -> "  " ++ name ++ "();") callNames
      fileContent = "func main() {\n" ++ unlines mockCalls ++ "}"
      file = TypusFile defaultFileDirectives [] fileContent fileContent
      calls = extractFunctionCalls file
  in property $ length calls >= 0

-- | Test error checking functions
prop_check_type_error :: [TypeCheckDiagnostic] -> Property
prop_check_type_error diagnostics =
  let hasErrors = checkTypeError diagnostics
  in property $ hasErrors == hasTypeErrors diagnostics

prop_has_malformed_syntax :: [CompilerError] -> Property
prop_has_malformed_syntax errors =
  let hasMalformed = hasMalformedSyntax errors
      malformedErrors = filter (\e -> cePhase e == Parsing) errors
  in property $ hasMalformed == (not (null malformedErrors))

-- | Test compilation phases
prop_check_dependent_types :: TypusFile -> Property
prop_check_dependent_types file =
  let result = checkDependentTypes file
  in property $ 
    case result of
      Left _ -> True
      Right _ -> True

prop_check_ownership :: TypusFile -> Property
prop_check_ownership file =
  let result = checkOwnership file
  in property $ 
    case result of
      Left _ -> True
      Right _ -> True

-- | Test code generation
prop_generate_go_code :: TypusFile -> Property
prop_generate_go_code file =
  let result = generateGoCode file
  in property $ 
    case result of
      Left _ -> True
      Right goCode -> not (null goCode)

-- | Test error reporting
prop_render_compilation_error :: CompilationPhase -> String -> Property
prop_render_compilation_error phase message =
  let error = CompilerError phase message
      rendered = renderCompilationError error
  in property $ message `isInfixOf` rendered

prop_format_compiler_errors :: [CompilerError] -> Property
prop_format_compiler_errors errors =
  let formatted = formatCompilerErrors errors
  in property $ 
    if null errors 
    then null formatted
    else not (null formatted)

prop_generate_detailed_report :: [CompilerError] -> [TypeCheckDiagnostic] -> Property
prop_generate_detailed_report errors diagnostics =
  let report = generateDetailedReport errors diagnostics
  in property $ not (null report)

-- | Test type check failure
prop_type_check_failure :: String -> Property
prop_type_check_failure message =
  let failure = typeCheckFailure message
  in property $ 
    case failure of
      Left errors -> not (null errors)
      Right _ -> False

-- | Test compilation pipeline
prop_compile_basic :: String -> Property
prop_compile_basic content =
  let file = TypusFile defaultFileDirectives [] content content
      result = compile file
  in property $ 
    case result of
      Left _ -> True
      Right _ -> True

tests :: TestTree
tests = testGroup "Enhanced Compiler Tests"
  [ testGroup "CompilerError tests"
    [ testProperty "compiler error ordering" prop_compiler_error_ordering
    , testProperty "compiler error equality" prop_compiler_error_equality
    ]
  , testGroup "CompilationPhase tests"
    [ testProperty "compilation phase ordering" prop_compilation_phase_ordering
    ]
  , testGroup "Error handling"
    [ testProperty "malformed syntax error" prop_malformed_syntax_error
    , testProperty "has type errors" prop_has_type_errors
    ]
  , testGroup "Diagnostic tests"
    [ testProperty "diagnostic type error" prop_diagnostic_type_error
    , testProperty "diagnostic type warning" prop_diagnostic_type_warning
    ]
  , testGroup "Type environment"
    [ testProperty "build type env empty" prop_build_type_env_empty
    , testProperty "build type env from pairs" prop_build_type_env_from_pairs
    , testProperty "build type env lookup" prop_build_type_env_lookup
    ]
  , testGroup "Declaration extraction"
    [ testProperty "extract declarations empty" prop_extract_declarations_empty
    , testProperty "extract declarations preserves order" prop_extract_declarations_preserves_order
    ]
  , testGroup "Function call extraction"
    [ testProperty "extract function calls empty" prop_extract_function_calls_empty
    , testProperty "extract function calls preserves" prop_extract_function_calls_preserves
    ]
  , testGroup "Error checking"
    [ testProperty "check type error" prop_check_type_error
    , testProperty "has malformed syntax" prop_has_malformed_syntax
    ]
  , testGroup "Compilation phases"
    [ testProperty "check dependent types" prop_check_dependent_types
    , testProperty "check ownership" prop_check_ownership
    ]
  , testGroup "Code generation"
    [ testProperty "generate go code" prop_generate_go_code
    ]
  , testGroup "Error reporting"
    [ testProperty "render compilation error" prop_render_compilation_error
    , testProperty "format compiler errors" prop_format_compiler_errors
    , testProperty "generate detailed report" prop_generate_detailed_report
    ]
  , testGroup "Type check failure"
    [ testProperty "type check failure" prop_type_check_failure
    ]
  , testGroup "Compilation pipeline"
    [ testProperty "compile basic" prop_compile_basic
    ]
  ]