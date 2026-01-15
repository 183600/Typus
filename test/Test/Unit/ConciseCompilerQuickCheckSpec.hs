{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.ConciseCompilerQuickCheckSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>))
import qualified Data.Text as T
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Data.Char (isSpace, isAlpha, isAlphaNum, toLower, toUpper, isDigit, isLetter)
import Compiler (compile, CompilerError(..), CompilerResult, CompilationPhase(..), 
                SyntaxError(..), TypeError(..), malformedSyntaxError, 
                renderCompilationError, formatCompilerErrors, generateDetailedReport,
                analyzeErrors, hasTypeErrors, TypeCheckDiagnostic(..), diagnoseTypeErrors,
                extractDeclarations, extractFunctionCalls, buildTypeEnv, buildTypeEnvFromPairs,
                createTypusFileFromErrors, isMethodDeclaration, checkTypeError, hasMalformedSyntax,
                checkDependentTypes, checkOwnership, ensureSourceIR, typeCheckFailure,
                typeDiagnosticToCompilerError, generateGoCode)
import Parser (TypusFile(..), Declaration(..))

-- Helper generators for Compiler tests
genCompilationPhase :: Gen CompilationPhase
genCompilationPhase = elements [Parsing, TypeChecking, OwnershipChecking, CodeGeneration]

genSyntaxError :: Gen SyntaxError
genSyntaxError = do
  msg <- elements ["Unexpected token", "Missing semicolon", "Invalid syntax", "Unclosed bracket"]
  line <- choose (1, 100)
  col <- choose (1, 100)
  return $ SyntaxError msg line col

genTypeError :: Gen TypeError
genTypeError = do
  msg <- elements ["Type mismatch", "Undefined variable", "Invalid operation", "Wrong arity"]
  line <- choose (1, 100)
  col <- choose (1, 100)
  return $ TypeError msg line col

genCompilerError :: Gen CompilerError
genCompilerError = oneof
  [ SyntaxErr <$> genSyntaxError
  , TypeErr <$> genTypeError
  , OwnershipErr <$> elements ["Ownership violation", "Borrow checker error"] <*> choose (1, 100) <*> choose (1, 100)
  , DependencyErr <$> elements ["Circular dependency", "Missing import"] <*> choose (1, 100) <*> choose (1, 100)
  ]

genTypeCheckDiagnostic :: Gen TypeCheckDiagnostic
genTypeCheckDiagnostic = do
  msg <- elements ["Type inference failed", "Unification error", "Constraint violation"]
  severity <- elements ["Error", "Warning", "Info"]
  line <- choose (1, 100)
  col <- choose (1, 100)
  return $ TypeCheckDiagnostic msg severity line col

genSimpleTypusFile :: Gen TypusFile
genSimpleTypusFile = do
  numDecls <- choose (0, 3)
  decls <- vectorOf numDecls genSimpleDeclaration
  return $ TypusFile decls

genSimpleDeclaration :: Gen Declaration
genSimpleDeclaration = oneof
  [ genVarDecl
  , genFuncDecl
  ]

genVarDecl :: Gen Declaration
genVarDecl = do
  name <- elements ["x", "y", "z", "value", "result"]
  value <- elements ["0", "1", "true", "false", "\"hello\""]
  return $ VarDecl name value

genFuncDecl :: Gen Declaration
genFuncDecl = do
  name <- elements ["func1", "func2", "method", "calculate"]
  params <- listOf $ elements ["param1", "param2", "x", "y"]
  body <- elements ["return 0;", "return true;", "return x;"]
  return $ FuncDecl name params body

-- Test properties for Compiler module

-- Error handling tests
prop_render_compilation_error_no_crash :: CompilerError -> Property
prop_render_compilation_error_no_crash err = 
  let rendered = renderCompilationError err
  in property $ length rendered > 0

prop_format_compiler_errors_no_crash :: [CompilerError] -> Property
prop_format_compiler_errors_no_crash errs = 
  let formatted = formatCompilerErrors errs
  in property $ length formatted >= 0

prop_generate_detailed_report_no_crash :: [CompilerError] -> Property
prop_generate_detailed_report_no_crash errs = 
  let report = generateDetailedReport errs
  in property $ length report >= 0

prop_analyze_errors_no_crash :: [CompilerError] -> Property
prop_analyze_errors_no_crash errs = 
  let analysis = analyzeErrors errs
  in property $ length analysis >= 0

prop_has_type_errors_detection :: [CompilerError] -> Property
prop_has_type_errors_detection errs = 
  let hasTypeErrs = hasTypeErrors errs
      hasTypeErrs' = any isTypeError errs
      isTypeError (TypeErr _) = True
      isTypeError _ = False
  in hasTypeErrs === hasTypeErrs'

-- Type checking tests
prop_diagnose_type_errors_no_crash :: [TypeCheckDiagnostic] -> Property
prop_diagnose_type_errors_no_crash diags = 
  let diagnosed = diagnoseTypeErrors diags
  in property $ length diagnosed >= 0

prop_type_diagnostic_to_compiler_error :: TypeCheckDiagnostic -> Property
prop_type_diagnostic_to_compiler_error diag = 
  let err = typeDiagnosticToCompilerError diag
  in case err of
       TypeErr _ -> property True
       _ -> property False

prop_check_type_error_detection :: [CompilerError] -> Property
prop_check_type_error_detection errs = 
  let hasTypeErr = checkTypeError errs
      hasTypeErr' = hasTypeErrors errs
  in hasTypeErr === hasTypeErr'

-- Declaration extraction tests
prop_extract_declarations_no_crash :: TypusFile -> Property
prop_extract_declarations_no_crash file = 
  let extracted = extractDeclarations file
  in property $ length extracted >= 0

prop_extract_function_calls_no_crash :: TypusFile -> Property
prop_extract_function_calls_no_crash file = 
  let calls = extractFunctionCalls file
  in property $ length calls >= 0

prop_build_type_env_no_crash :: [(String, String)] -> Property
prop_build_type_env_no_crash pairs = 
  let env = buildTypeEnv pairs
  in property $ length env >= 0

prop_build_type_env_from_pairs_no_crash :: [(String, String)] -> Property
prop_build_type_env_from_pairs_no_crash pairs = 
  let env = buildTypeEnvFromPairs pairs
  in property $ length env >= 0

-- Compilation tests
prop_compile_no_crash :: String -> Property
prop_compile_no_crash code = 
  let result = compile code
  in case result of
       Left _ -> property True
       Right _ -> property True

prop_check_malformed_syntax :: [CompilerError] -> Property
prop_check_malformed_syntax errs = 
  let hasMalformed = hasMalformedSyntax errs
      hasMalformed' = any isSyntaxError errs
      isSyntaxError (SyntaxErr _) = True
      isSyntaxError _ = False
  in hasMalformed === hasMalformed'

prop_check_dependent_types_no_crash :: TypusFile -> Property
prop_check_dependent_types_no_crash file = 
  let result = checkDependentTypes file
  in case result of
       Left _ -> property True
       Right _ -> property True

prop_check_ownership_no_crash :: TypusFile -> Property
prop_check_ownership_no_crash file = 
  let result = checkOwnership file
  in case result of
       Left _ -> property True
       Right _ -> property True

prop_generate_go_code_no_crash :: TypusFile -> Property
prop_generate_go_code_no_crash file = 
  let result = generateGoCode file
  in case result of
       Left _ -> property True
       Right code -> property $ length code > 0

tests :: TestTree
tests = testGroup "Concise Compiler QuickCheck Tests"
  [ testProperties "Error Handling Tests"
    [ ("render compilation error no crash", prop_render_compilation_error_no_crash)
    , ("format compiler errors no crash", prop_format_compiler_errors_no_crash)
    , ("generate detailed report no crash", prop_generate_detailed_report_no_crash)
    , ("analyze errors no crash", prop_analyze_errors_no_crash)
    , ("has type errors detection", prop_has_type_errors_detection)
    ]
  , testProperties "Type Checking Tests"
    [ ("diagnose type errors no crash", prop_diagnose_type_errors_no_crash)
    , ("type diagnostic to compiler error", prop_type_diagnostic_to_compiler_error)
    , ("check type error detection", prop_check_type_error_detection)
    ]
  , testProperties "Declaration Extraction Tests"
    [ ("extract declarations no crash", prop_extract_declarations_no_crash)
    , ("extract function calls no crash", prop_extract_function_calls_no_crash)
    , ("build type env no crash", prop_build_type_env_no_crash)
    , ("build type env from pairs no crash", prop_build_type_env_from_pairs_no_crash)
    ]
  , testProperties "Compilation Tests"
    [ ("compile no crash", prop_compile_no_crash)
    , ("check malformed syntax", prop_check_malformed_syntax)
    , ("check dependent types no crash", prop_check_dependent_types_no_crash)
    , ("check ownership no crash", prop_check_ownership_no_crash)
    , ("generate go code no crash", prop_generate_go_code_no_crash)
    ]
  ]