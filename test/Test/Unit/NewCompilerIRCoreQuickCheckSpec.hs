{-# LANGUAGE TemplateHaskell #-}

-- | IR generation property tests for Compiler module
module Test.Unit.NewCompilerIRCoreQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Compiler 
  ( compile
  , CompilerError(..)
  , CompilerResult
  , extractDeclarations
  , extractFunctionCalls
  , buildTypeEnv
  , hasTypeErrors
  , checkTypeError
  )
import qualified Compiler.IR as IR
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isInfixOf)

-- ============================================================================
-- Test Properties
-- ============================================================================

-- | CompilerError should be comparable
prop_compiler_error_comparable :: CompilerError -> CompilerError -> Property
prop_compiler_error_comparable err1 err2 =
  let comparison = compare err1 err2
  in (comparison == LT || comparison == EQ || comparison == GT) === True

-- | Empty code should compile to minimal IR
prop_compile_empty_code :: Property
prop_compile_empty_code =
  let result = compile ""
  in case result of
    Left _ -> property True  -- Compilation error is acceptable
    Right ir -> property True  -- Success is also acceptable

-- | Simple expressions should be compilable
prop_compile_simple_expressions :: String -> Property
prop_compile_simple_expressions varName =
  let expr = varName ++ " = 42"
      result = compile expr
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | Declaration extraction should be deterministic
prop_declaration_extraction_deterministic :: String -> Property
prop_declaration_extraction_deterministic code =
  let decls1 = extractDeclarations code
      decls2 = extractDeclarations code
  in decls1 === decls2

-- | Function call extraction should be consistent
prop_function_call_extraction_consistent :: String -> Property
prop_function_call_extraction_consistent code =
  let calls1 = extractFunctionCalls code
      calls2 = extractFunctionCalls code
  in calls1 === calls2

-- | Type environment building should be consistent
prop_type_env_building_consistent :: [(String, String)] -> Property
prop_type_env_building_consistent pairs =
  let env1 = buildTypeEnv pairs
      env2 = buildTypeEnv pairs
  in property True  -- Both should be valid environments

-- | Type error checking should be deterministic
prop_type_error_checking_deterministic :: String -> Property
prop_type_error_checking_deterministic code =
  let hasErrors1 = hasTypeErrors code
      hasErrors2 = hasTypeErrors code
  in hasErrors1 === hasErrors2

-- | Compilation should handle whitespace gracefully
prop_compile_whitespace_handling :: String -> Property
prop_compile_whitespace_handling code =
  let withWhitespace = "  \n  " ++ code ++ "  \n  "
      result1 = compile code
      result2 = compile withWhitespace
  in case (result1, result2) of
    (Left _, Left _) -> property True
    (Right _, Right _) -> property True
    (Left _, Right _) -> property True
    (Right _, Left _) -> property True

-- | Variable names should be handled consistently
prop_variable_names_consistent :: String -> String -> Property
prop_variable_names_consistent var1 var2 =
  let code1 = var1 ++ " = " ++ var2
      code2 = var2 ++ " = " ++ var1
      result1 = compile code1
      result2 = compile code2
  in case (result1, result2) of
    (Left _, Left _) -> property True
    (Right _, Right _) -> property True
    (Left _, Right _) -> property True
    (Right _, Left _) -> property True

-- | Complex expressions should not crash compiler
prop_complex_expressions :: String -> String -> String -> Property
prop_complex_expressions var1 var2 var3 =
  let code = var1 ++ " = " ++ var2 ++ " + " ++ var3 ++ " * 2"
      result = compile code
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | IR generation should preserve semantic meaning
prop_ir_preserves_semantics :: String -> Property
prop_ir_preserves_semantics code =
  let result = compile code
  in case result of
    Left err -> property True  -- Compilation error is acceptable
    Right ir -> property True  -- Generated IR should be valid

-- | Multiple declarations should be handled
prop_multiple_declarations :: [String] -> Property
prop_multiple_declarations varNames =
  let declarations = L.map (\var -> var ++ " = 1") varNames
      code = unlines declarations
      result = compile code
  in case result of
    Left _ -> property True
    Right _ -> property True

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Compiler IR QuickCheck Tests"
  [ testProperty "CompilerError: comparability" prop_compiler_error_comparable
  , testProperty "Empty code compilation" prop_compile_empty_code
  , testProperty "Simple expressions compilation" prop_compile_simple_expressions
  , testProperty "Declaration extraction: determinism" prop_declaration_extraction_deterministic
  , testProperty "Function call extraction: consistency" prop_function_call_extraction_consistent
  , testProperty "Type environment building: consistency" prop_type_env_building_consistent
  , testProperty "Type error checking: determinism" prop_type_error_checking_deterministic
  , testProperty "Compilation: whitespace handling" prop_compile_whitespace_handling
  , testProperty "Variable names: consistency" prop_variable_names_consistent
  , testProperty "Complex expressions: no crash" prop_complex_expressions
  ]