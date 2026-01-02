{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerNewQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

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

import Parser (TypusFile(..))
import Compiler.GoAst (renderGoModule)
import qualified Compiler.IR as IR
import Compiler.TypeChecker
import Compiler.Errors

import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (length, isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (sort, null, nub)
import Data.Maybe (isJust, isNothing, fromMaybe, mapMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- ============================================================================
-- CompilationPhase Properties
-- ============================================================================

-- Property: Compilation phases are ordered correctly
prop_compilation_phases_ordered :: Property
prop_compilation_phases_ordered =
  let phases = [Parsing, TypeChecking, OwnershipAnalysis, DependentTypeAnalysis, CodeGeneration]
  in property $ phases === sort phases

-- Property: All compilation phases are unique
prop_compilation_phases_unique :: Property
prop_compilation_phases_unique =
  let phases = [Parsing, TypeChecking, OwnershipAnalysis, DependentTypeAnalysis, CodeGeneration]
  in property $ L.length phases === L.length (nub phases)

-- ============================================================================
-- CompilerError Properties
-- ============================================================================

-- Property: Compiler errors contain phase information
prop_compiler_error_contains_phase :: CompilationPhase -> String -> Property
prop_compiler_error_contains_phase phase errorMsg =
  not (null errorMsg) ==>
  let error = CompilerError phase errorMsg startPos
  in property $ errorPhase error === phase

-- Property: Compiler errors contain location information
prop_compiler_error_contains_location :: CompilationPhase -> String -> SourcePos -> Property
prop_compiler_error_contains_location phase errorMsg pos =
  not (null errorMsg) ==>
  let error = CompilerError phase errorMsg pos
  in property $ errorLocation error === pos

-- Property: Compiler errors preserve error messages
prop_compiler_error_preserves_message :: CompilationPhase -> String -> Property
prop_compiler_error_preserves_message phase errorMsg =
  not (null errorMsg) ==>
  let error = CompilerError phase errorMsg startPos
  in property $ errorMessage error === errorMsg

-- ============================================================================
-- TypeCheckDiagnostic Properties
-- ============================================================================

-- Property: Type diagnostics preserve error messages
prop_type_diagnostic_preserves_message :: String -> Property
prop_type_diagnostic_preserves_message errorMsg =
  not (null errorMsg) ==>
  let diagnostic = TypeCheckDiagnostic errorMsg startPos
  in property $ diagnosticMessage diagnostic === errorMsg

-- Property: Type diagnostics preserve location
prop_type_diagnostic_preserves_location :: String -> SourcePos -> Property
prop_type_diagnostic_preserves_location errorMsg pos =
  not (null errorMsg) ==>
  let diagnostic = TypeCheckDiagnostic errorMsg pos
  in property $ diagnosticLocation diagnostic === pos

-- ============================================================================
-- Declaration Extraction Properties
-- ============================================================================

-- Property: Extracting declarations from empty code returns empty list
prop_extract_declarations_empty :: Property
prop_extract_declarations_empty =
  let declarations = extractDeclarations ""
  in property $ null declarations

-- Property: Extracting declarations preserves function names
prop_extract_declarations_preserves_functions :: String -> Property
prop_extract_declarations_preserves_functions funcName =
  not (null funcName) && L.all (`elem` ['a'..'z' ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"]) funcName ==>
  let code = "func " ++ funcName ++ "() {}\n"
      declarations = extractDeclarations code
  in property $ L.any (funcName `L.isInfixOf`) declarations

-- Property: Extracting declarations handles multiple functions
prop_extract_declarations_multiple :: [String] -> Property
prop_extract_declarations_multiple funcNames =
  not (null funcNames) && L.all (not . null) funcNames && L.all (L.all (`elem` ['a'..'z' ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"])) funcNames ==>
  let funcDecls = L.map (\name -> "func " ++ name ++ "() {}") funcNames
      code = unlines funcDecls
      declarations = extractDeclarations code
  in property $ L.all (\name -> L.any (name `L.isInfixOf`) declarations) funcNames

-- ============================================================================
-- Function Call Extraction Properties
-- ============================================================================

-- Property: Extracting function calls from empty code returns empty list
prop_extract_calls_empty :: Property
prop_extract_calls_empty =
  let calls = extractFunctionCalls ""
  in property $ null calls

-- Property: Extracting function calls preserves call names
prop_extract_calls_preserves_calls :: String -> Property
prop_extract_calls_preserves_calls callName =
  not (null callName) && L.all (`elem` ['a'..'z' ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"]) callName ==>
  let code = callName ++ "();\n"
      calls = extractFunctionCalls code
  in property $ L.any (callName `L.isInfixOf`) calls

-- Property: Extracting function calls handles multiple calls
prop_extract_calls_multiple :: [String] -> Property
prop_extract_calls_multiple callNames =
  not (null callNames) && L.all (not . null) callNames && L.all (L.all (`elem` ['a'..'z' ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")) callNames ==>
  let callStatements = L.map (\name -> name ++ "();") callNames
      code = unlines callStatements
      calls = extractFunctionCalls code
  in property $ L.all (\name -> L.any (name `L.isInfixOf`) calls) callNames

-- ============================================================================
-- Type Environment Properties
-- ============================================================================

-- Property: Building type environment from empty pairs returns empty env
prop_build_type_env_empty :: Property
prop_build_type_env_empty =
  let env = buildTypeEnvFromPairs []
  in property $ null env

-- Property: Building type environment preserves type mappings
prop_build_type_env_preserves_mappings :: [(String, String)] -> Property
prop_build_type_env_preserves_mappings pairs =
  not (null pairs) && L.all (not . null . fst) pairs && L.all (not . null . snd) pairs ==>
  let env = buildTypeEnvFromPairs pairs
  in property $ L.all (\(name, typ) -> Map.member (T.pack name) env && 
                                     env Map.! (T.pack name) === T.pack typ) pairs

-- Property: Type environment lookup works correctly
prop_type_env_lookup :: [(String, String)] -> String -> Property
prop_type_env_lookup pairs lookupKey =
  not (null pairs) && not (null lookupKey) ==>
  let env = buildTypeEnvFromPairs pairs
      result = Map.lookup (T.pack lookupKey) env
  in property $ case lookup lookupKey pairs of
                   Nothing -> isNothing result
                   Just (_, typ) -> result === Just (T.pack typ)

-- ============================================================================
-- Method Detection Properties
-- ============================================================================

-- Property: Method declarations are detected correctly
prop_is_method_declaration_true :: String -> Property
prop_is_method_declaration_true methodName =
  not (null methodName) && L.all (`elem` ['a'..'z' ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"]) methodName ==>
  let methodDecl = "func (r *Receiver) " ++ methodName ++ "() {}"
  in property $ isMethodDeclaration methodDecl

-- Property: Non-method declarations are not detected as methods
prop_is_method_declaration_false :: String -> Property
prop_is_method_declaration_false funcName =
  not (null funcName) && L.all (`elem` ['a'..'z' ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")) funcName ==>
  let funcDecl = "func " ++ funcName ++ "() {}"
  in property $ not (isMethodDeclaration funcDecl)

-- ============================================================================
-- Error Analysis Properties
-- ============================================================================

-- Property: Analyzing empty errors returns no type errors
prop_analyze_empty_errors :: Property
prop_analyze_empty_errors =
  let errors = []
      hasTypeErrs = hasTypeErrors errors
  in property $ not hasTypeErrs

-- Property: Analyzing type errors detects type errors
prop_analyze_type_errors :: [String] -> Property
prop_analyze_type_errors errorMessages =
  not (null errorMessages) && L.all (not . null) errorMessages ==>
  let errors = L.map (\msg -> CompilerError TypeChecking msg startPos) errorMessages
      hasTypeErrs = hasTypeErrors errors
  in property $ hasTypeErrs

-- Property: Diagnosing type errors preserves messages
prop_diagnose_type_errors_preserves :: [String] -> Property
prop_diagnose_type_errors_preserves errorMessages =
  not (null errorMessages) && L.all (not . null) errorMessages ==>
  let diagnostics = L.map (\msg -> TypeCheckDiagnostic msg startPos) errorMessages
      compilerErrors = map typeDiagnosticToCompilerError diagnostics
  in property $ L.all (\(orig, err) -> errorMessage err === orig) (zip errorMessages compilerErrors)

-- ============================================================================
-- Compilation Properties
-- ============================================================================

-- Property: Compiling empty code produces result
prop_compile_empty_code :: Property
prop_compile_empty_code =
  let result = compile ""
  in property $ isJust result

-- Property: Compiling simple Go code produces result
prop_compile_simple_go_code :: String -> Property
prop_compile_simple_go_code funcName =
  not (null funcName) && L.all (`elem` ['a'..'z' ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"]) funcName ==>
  let code = "package main\n\nfunc " ++ funcName ++ "() {}\n"
      result = compile code
  in property $ isJust result

-- Property: Compilation preserves function names
prop_compilation_preserves_functions :: String -> Property
prop_compilation_preserves_functions funcName =
  not (null funcName) && L.all (`elem` ['a'..'z' ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"]) funcName ==>
  let code = "package main\n\nfunc " ++ funcName ++ "() {}\n"
      result = compile code
  in case result of
       Nothing -> property False
       Just compiled -> property $ funcName `L.isInfixOf` compiled

-- ============================================================================
-- Code Generation Properties
-- ============================================================================

-- Property: Generating Go code from empty IR works
prop_generate_go_code_empty :: Property
prop_generate_go_code_empty =
  let ir = IR.emptyIR
      goCode = generateGoCode ir
  in property $ not (null goCode)

-- Property: Generating Go code preserves structure
prop_generate_go_code_preserves_structure :: String -> Property
prop_generate_go_code_preserves_structure funcName =
  not (null funcName) && L.all (`elem` ['a'..'z' ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"]) funcName ==>
  let ir = IR.fromFunction (T.pack funcName) IR.emptyIR
      goCode = generateGoCode ir
  in property $ funcName `L.isInfixOf` goCode

-- ============================================================================
-- Error Formatting Properties
-- ============================================================================

-- Property: Formatting empty errors returns empty string
prop_format_empty_compiler_errors :: Property
prop_format_empty_compiler_errors =
  let errors = []
      formatted = formatCompilerErrors errors
  in property $ null formatted

-- Property: Formatting single error includes phase L.and message
prop_format_single_compiler_error :: CompilationPhase -> String -> Property
prop_format_single_compiler_error phase errorMsg =
  not (null errorMsg) ==>
  let error = CompilerError phase errorMsg startPos
      errors = [error]
      formatted = formatCompilerErrors errors
  in property $ show phase `L.isInfixOf` formatted .&&.
             errorMsg `L.isInfixOf` formatted

-- Property: Formatting multiple errors includes L.all messages
prop_format_multiple_compiler_errors :: [String] -> Property
prop_format_multiple_compiler_errors errorMessages =
  not (null errorMessages) && L.all (not . null) errorMessages ==>
  let errors = L.map (\msg -> CompilerError TypeChecking msg startPos) errorMessages
      formatted = formatCompilerErrors errors
  in property $ L.all (`L.isInfixOf` formatted) errorMessages

-- ============================================================================
-- Report Generation Properties
-- ============================================================================

-- Property: Generating detailed report includes statistics
prop_generate_detailed_report_includes_stats :: [String] -> Property
prop_generate_detailed_report_includes_stats errorMessages =
  not (null errorMessages) && L.all (not . null) errorMessages ==>
  let errors = L.map (\msg -> CompilerError TypeChecking msg startPos) errorMessages
      report = generateDetailedReport errors
  in property $ "Compilation Report" `L.isInfixOf` report .&&.
             "Total errors:" `L.isInfixOf` report

-- Property: Analyzing errors categorizes by phase
prop_analyze_errors_categorizes :: [CompilationPhase] -> [String] -> Property
prop_analyze_errors_categorizes phases errorMessages =
  not (null phases) && not (null errorMessages) && L.length phases === L.length errorMessages ==>
  let errors = zipWith (\phase msg -> CompilerError phase msg startPos) phases errorMessages
      analysis = analyzeErrors errors
  in property $ not (null analysis)

-- ============================================================================
-- Edge Case Properties
-- ============================================================================

-- Property: Compiling code with Unicode characters works
prop_compile_unicode :: String -> Property
prop_compile_unicode unicodeText =
  not (null unicodeText) ==>
  let code = "package main\n\nfunc main() {\n    // " ++ unicodeText + "\n}\n"
      result = compile code
  in property $ isJust result

-- Property: Compiling very long code works
prop_compile_long_code :: Int -> Property
prop_compile_long_code L.length =
  length > 0 && L.length <= 1000 ==>
  let longCode = "package main\n\nfunc main() {\n" ++ L.concat (replicate L.length "    x := 42;\n") ++ "}\n"
      result = compile longCode
  in property $ isJust result

-- Property: Compiling code with comments works
prop_compile_with_comments :: String -> String -> Property
prop_compile_with_comments funcName comment =
  not (null funcName) && not (null comment) && not ("//" `L.isInfixOf` comment) ==>
  let code = "package main\n\n// " ++ comment ++ "\nfunc " ++ funcName ++ "() {} // " ++ comment
      result = compile code
  in property $ isJust result

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Compiler New QuickCheck Tests"
  [ testGroup "CompilationPhase"
    [ fastProperty "phases ordered" prop_compilation_phases_ordered
    , fastProperty "phases unique" prop_compilation_phases_unique
    ]
  , testGroup "CompilerError"
    [ fastProperty "contains phase" prop_compiler_error_contains_phase
    , fastProperty "contains location" prop_compiler_error_contains_location
    , fastProperty "preserves message" prop_compiler_error_preserves_message
    ]
  , testGroup "TypeCheckDiagnostic"
    [ fastProperty "preserves message" prop_type_diagnostic_preserves_message
    , fastProperty "preserves location" prop_type_diagnostic_preserves_location
    ]
  , testGroup "DeclarationExtraction"
    [ fastProperty "empty code" prop_extract_declarations_empty
    , fastProperty "preserves functions" prop_extract_declarations_preserves_functions
    , fastProperty "multiple functions" prop_extract_declarations_multiple
    ]
  , testGroup "FunctionCallExtraction"
    [ fastProperty "empty code" prop_extract_calls_empty
    , fastProperty "preserves calls" prop_extract_calls_preserves_calls
    , fastProperty "multiple calls" prop_extract_calls_multiple
    ]
  , testGroup "TypeEnvironment"
    [ fastProperty "empty pairs" prop_build_type_env_empty
    , fastProperty "preserves mappings" prop_build_type_env_preserves_mappings
    , fastProperty "lookup works" prop_type_env_lookup
    ]
  , testGroup "MethodDetection"
    [ fastProperty "detects methods" prop_is_method_declaration_true
    , fastProperty "rejects non-methods" prop_is_method_declaration_false
    ]
  , testGroup "ErrorAnalysis"
    [ fastProperty "empty errors" prop_analyze_empty_errors
    , fastProperty "detects type errors" prop_analyze_type_errors
    , fastProperty "preserves messages" prop_diagnose_type_errors_preserves
    ]
  , testGroup "Compilation"
    [ fastProperty "empty code" prop_compile_empty_code
    , fastProperty "simple Go code" prop_compile_simple_go_code
    , fastProperty "preserves functions" prop_compilation_preserves_functions
    ]
  , testGroup "CodeGeneration"
    [ fastProperty "empty IR" prop_generate_go_code_empty
    , fastProperty "preserves structure" prop_generate_go_code_preserves_structure
    ]
  , testGroup "ErrorFormatting"
    [ fastProperty "empty errors" prop_format_empty_compiler_errors
    , fastProperty "single error" prop_format_single_compiler_error
    , fastProperty "multiple errors" prop_format_multiple_compiler_errors
    ]
  , testGroup "ReportGeneration"
    [ fastProperty "includes statistics" prop_generate_detailed_report_includes_stats
    , fastProperty "categorizes by phase" prop_analyze_errors_categorizes
    ]
  , testGroup "EdgeCases"
    [ fastProperty "unicode" prop_compile_unicode
    , fastProperty "long code" prop_compile_long_code
    , fastProperty "with comments" prop_compile_with_comments
    ]
  ]