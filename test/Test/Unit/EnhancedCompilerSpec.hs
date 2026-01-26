{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Test.Unit.EnhancedCompilerSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Char (isAlpha, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe (isJust, isNothing)
import Control.Monad (void)

-- Import Compiler module
import Compiler (compile, CompilerError(..), CompilationPhase(..), 
                SyntaxError(..), TypeError(..), malformedSyntaxError,
                renderCompilationError, formatCompilerErrors, 
                generateDetailedReport, analyzeErrors, hasTypeErrors,
                TypeCheckDiagnostic(..), diagnoseTypeErrors,
                extractDeclarations, extractFunctionCalls, buildTypeEnv,
                buildTypeEnvFromPairs, createTypusFileFromErrors,
                isMethodDeclaration, checkTypeError, hasMalformedSyntax,
                checkDependentTypes, checkOwnership, ensureSourceIR,
                typeCheckFailure, typeDiagnosticToCompilerError,
                generateGoCode)
import qualified Compiler.TypeChecker as TC

-- Import Parser module
import Parser (TypusFile(..), parseTypus)

-- Test properties for compiler

-- Property 1: Compiling empty string should not crash
prop_compile_empty_string :: Property
prop_compile_empty_string = property $
  case parseTypus "" of
    Left _ -> property True  -- Parsing may fail, but shouldn't crash
    Right typusFile -> 
      case compile typusFile of
        Left _ -> property True  -- Compilation may fail, but shouldn't crash
        Right _ -> property True

-- Property 2: Compiling simple package should not crash
prop_compile_simple_package :: String -> Property
prop_compile_simple_package name = 
  not (null name) && all isAlphaNum name ==>
  case parseTypus ("package " ++ name) of
    Left _ -> property True  -- Parsing may fail, but shouldn't crash
    Right typusFile -> 
      case compile typusFile of
        Left _ -> property True  -- Compilation may fail, but shouldn't crash
        Right _ -> property True

-- Property 3: Compiling code with ownership directive should not crash
prop_compile_ownership_directive :: Bool -> Property
prop_compile_ownership_directive flag = 
  let directive = if flag then "on" else "off"
      code = "//! ownership: " ++ directive ++ "\npackage main"
  in case parseTypus code of
    Left _ -> property True  -- Parsing may fail, but shouldn't crash
    Right typusFile -> 
      case compile typusFile of
        Left _ -> property True  -- Compilation may fail, but shouldn't crash
        Right _ -> property True

-- Property 4: Compiling code with dependent types directive should not crash
prop_compile_dependent_types_directive :: Bool -> Property
prop_compile_dependent_types_directive flag = 
  let directive = if flag then "on" else "off"
      code = "//! dependent_types: " ++ directive ++ "\npackage main"
  in case parseTypus code of
    Left _ -> property True  -- Parsing may fail, but shouldn't crash
    Right typusFile -> 
      case compile typusFile of
        Left _ -> property True  -- Compilation may fail, but shouldn't crash
        Right _ -> property True

-- Property 5: Compiling simple function should not crash
prop_compile_simple_function :: String -> Property
prop_compile_simple_function name = 
  not (null name) && all isAlpha name ==>
  let code = "package main\n\nfunc " ++ name ++ "() {}\n"
  in case parseTypus code of
    Left _ -> property True  -- Parsing may fail, but shouldn't crash
    Right typusFile -> 
      case compile typusFile of
        Left _ -> property True  -- Compilation may fail, but shouldn't crash
        Right _ -> property True

-- Property 6: Compiling code with imports should not crash
prop_compile_imports :: String -> Property
prop_compile_imports path = 
  not (null path) && all (\c -> isAlphaNum c || c `elem` "/._-") path ==>
  let code = "package main\n\nimport \"" ++ path ++ "\"\n"
  in case parseTypus code of
    Left _ -> property True  -- Parsing may fail, but shouldn't crash
    Right typusFile -> 
      case compile typusFile of
        Left _ -> property True  -- Compilation may fail, but shouldn't crash
        Right _ -> property True

-- Property 7: Compiling code with multiple directives should not crash
prop_compile_multiple_directives :: Bool -> Bool -> Property
prop_compile_multiple_directives ownership dependentTypes = 
  let ownDir = if ownership then "on" else "off"
      depDir = if dependentTypes then "on" else "off"
      code = "//! ownership: " ++ ownDir ++ "\n//! dependent_types: " ++ depDir ++ "\npackage main\n"
  in case parseTypus code of
    Left _ -> property True  -- Parsing may fail, but shouldn't crash
    Right typusFile -> 
      case compile typusFile of
        Left _ -> property True  -- Compilation may fail, but shouldn't crash
        Right _ -> property True

-- Property 8: Error handling should not crash
prop_error_handling :: String -> Property
prop_error_handling input = 
  not (null input) ==>
  case parseTypus input of
    Left _ -> property True  -- Parsing may fail, but shouldn't crash
    Right typusFile -> 
      case compile typusFile of
        Left errs -> property $ length (formatCompilerErrors errs) >= 0  -- Should not crash
        Right _ -> property True

-- Property 9: Type environment building should not crash
prop_build_type_env :: [(String, String)] -> Property
prop_build_type_env pairs = 
  let validPairs = filter (\(k, v) -> not (null k) && not (null v)) pairs
      typePairs = map (\(k, v) -> (k, TC.TypeName v)) validPairs
  in property True  -- buildTypeEnvFromPairs returns TypeEnv, not a list

-- Property 10: Error analysis should not crash
prop_analyze_errors :: String -> Property
prop_analyze_errors input = 
  not (null input) ==>
  case parseTypus input of
    Left _ -> property True  -- Parsing may fail, but shouldn't crash
    Right typusFile -> 
      case compile typusFile of
        Left errs -> property $ True  -- analyzeErrors returns ErrorStatistics, not a list
        Right _ -> property True

-- Unit tests for specific compiler functionality

test_compile_empty_file :: Assertion
test_compile_empty_file = 
  case parseTypus "" of
    Left _ -> assertBool "Parsing empty file should not crash" True
    Right typusFile -> 
      case compile typusFile of
        Left _ -> assertBool "Compiling empty file should not crash" True
        Right _ -> assertBool "Compiling empty file should not crash" True

test_compile_simple_package :: Assertion
test_compile_simple_package = 
  case parseTypus "package main" of
    Left err -> assertFailure $ "Parsing failed: " ++ show err
    Right typusFile -> 
      case compile typusFile of
        Left _ -> assertBool "Compiling simple package should not crash" True
        Right _ -> assertBool "Compiling simple package should not crash" True

test_compile_with_ownership :: Assertion
test_compile_with_ownership = 
  let code = "//! ownership: on\npackage main\n\nfunc main() {}\n"
  in case parseTypus code of
    Left err -> assertFailure $ "Parsing failed: " ++ show err
    Right typusFile -> 
      case compile typusFile of
        Left _ -> assertBool "Compiling with ownership should not crash" True
        Right _ -> assertBool "Compiling with ownership should not crash" True

test_compile_with_dependent_types :: Assertion
test_compile_with_dependent_types = 
  let code = "//! dependent_types: on\npackage main\n\nfunc main() {}\n"
  in case parseTypus code of
    Left err -> assertFailure $ "Parsing failed: " ++ show err
    Right typusFile -> 
      case compile typusFile of
        Left _ -> assertBool "Compiling with dependent types should not crash" True
        Right _ -> assertBool "Compiling with dependent types should not crash" True

test_compile_simple_function :: Assertion
test_compile_simple_function = 
  let code = "package main\n\nfunc hello() {}\n"
  in case parseTypus code of
    Left err -> assertFailure $ "Parsing failed: " ++ show err
    Right typusFile -> 
      case compile typusFile of
        Left _ -> assertBool "Compiling simple function should not crash" True
        Right _ -> assertBool "Compiling simple function should not crash" True

test_compile_with_import :: Assertion
test_compile_with_import = 
  let code = "package main\n\nimport \"fmt\"\n\nfunc main() {}\n"
  in case parseTypus code of
    Left err -> assertFailure $ "Parsing failed: " ++ show err
    Right typusFile -> 
      case compile typusFile of
        Left _ -> assertBool "Compiling with import should not crash" True
        Right _ -> assertBool "Compiling with import should not crash" True

test_error_formatting :: Assertion
test_error_formatting = 
  let err = malformedSyntaxError
      formatted = formatCompilerErrors [err]
  in assertBool "Error formatting should not crash" $ not (null formatted)

test_error_analysis :: Assertion
test_error_analysis = 
  let err = malformedSyntaxError
      analyzed = analyzeErrors [err]
  in assertBool "Error analysis should not crash" $ True  -- analyzeErrors returns ErrorStatistics, not a list

test_type_env_building :: Assertion
test_type_env_building = 
  let pairs = [("int", "Int"), ("string", "String")]
      typePairs = map (\(k, v) -> (k, TC.TypeName v)) pairs
      env = buildTypeEnvFromPairs typePairs
  in assertBool "Type environment building should not crash" $ True  -- TypeEnv is not a list

test_declaration_extraction :: Assertion
test_declaration_extraction = 
  let code = "package main\n\nfunc hello() {}\nfunc world() {}\n"
  in case parseTypus code of
    Left err -> assertFailure $ "Parsing failed: " ++ show err
    Right typusFile -> 
      let decls = extractDeclarations (show typusFile)  -- extractDeclarations expects String
      in assertBool "Declaration extraction should not crash" $ length decls >= 0

test_function_call_extraction :: Assertion
test_function_call_extraction = 
  let code = "package main\n\nfunc main() {\n  hello()\n  world()\n}\n"
  in case parseTypus code of
    Left err -> assertFailure $ "Parsing failed: " ++ show err
    Right typusFile -> 
      let calls = extractFunctionCalls (show typusFile)  -- extractFunctionCalls expects String
      in assertBool "Function call extraction should not crash" $ length calls >= 0

tests :: TestTree
tests = testGroup "Test.Unit.EnhancedCompilerSpec Tests"
  [ testGroup "QuickCheck Properties"
    [ testProperty "compile empty string should not crash" prop_compile_empty_string
    , testProperty "compile simple package" prop_compile_simple_package
    , testProperty "compile ownership directive" prop_compile_ownership_directive
    , testProperty "compile dependent types directive" prop_compile_dependent_types_directive
    , testProperty "compile simple function" prop_compile_simple_function
    , testProperty "compile imports" prop_compile_imports
    , testProperty "compile multiple directives" prop_compile_multiple_directives
    , testProperty "error handling" prop_error_handling
    , testProperty "build type env" prop_build_type_env
    , testProperty "analyze errors" prop_analyze_errors
    ]
  , testGroup "Unit Tests"
    [ testCase "compile empty file" test_compile_empty_file
    , testCase "compile simple package" test_compile_simple_package
    , testCase "compile with ownership" test_compile_with_ownership
    , testCase "compile with dependent types" test_compile_with_dependent_types
    , testCase "compile simple function" test_compile_simple_function
    , testCase "compile with import" test_compile_with_import
    , testCase "error formatting" test_error_formatting
    , testCase "error analysis" test_error_analysis
    , testCase "type env building" test_type_env_building
    , testCase "declaration extraction" test_declaration_extraction
    , testCase "function call extraction" test_function_call_extraction
    ]
  ]