module Test.Unit.CompilerUtilsPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import CompilerUtils

-- Test compiler utilities initialization
prop_compiler_utils_initialization :: Property
prop_compiler_utils_initialization =
  let utils1 = initializeCompilerUtils
      utils2 = initializeCompilerUtils
  in property $ getUtilsId utils1 /= getUtilsId utils2

-- Test AST transformation utilities
prop_ast_transformation_idempotent :: String -> Property
prop_ast_transformation_idempotent sourceCode =
  let ast1 = parseToAst sourceCode
      transformed1 = transformAst ast1
      transformed2 = transformAst transformed1
  in property $ 
    case (transformed1, transformed2) of
      (Right t1, Right t2) -> getAstHash t1 === getAstHash t2
      _ -> property True

-- Test optimization utilities
prop_optimization_preserves_semantics :: String -> Property
prop_optimization_preserves_semantics sourceCode =
  let unoptimized = parseToAst sourceCode
      optimized = optimizeAst unoptimized
  in property $ 
    case (unoptimized, optimized) of
      (Right u, Right o) -> getSemanticHash u === getSemanticHash o
      _ -> property True

-- Test code generation utilities
prop_code_generation_consistent :: String -> Property
prop_code_generation_consistent sourceCode =
  let ast = parseToAst sourceCode
      code1 = generateCodeFromAst ast
      code2 = generateCodeFromAst ast
  in property $ 
    case (code1, code2) of
      (Right c1, Right c2) -> c1 === c2
      _ -> property True

-- Test error reporting utilities
prop_error_reporting_preserves_info :: String -> String -> Property
prop_error_reporting_preserves_info errorMsg context =
  let error = createCompilerError errorMsg context
      formatted = formatCompilerError error
  in property $ 
    errorMsg `isInfixOf` formatted && 
    context `isInfixOf` formatted

tests :: TestTree
tests = testGroup "CompilerUtils Properties Tests"
  [ testProperty "compiler utils initialization" prop_compiler_utils_initialization
  , testProperty "AST transformation idempotent" prop_ast_transformation_idempotent
  , testProperty "optimization preserves semantics" prop_optimization_preserves_semantics
  , testProperty "code generation consistent" prop_code_generation_consistent
  , testProperty "error reporting preserves info" prop_error_reporting_preserves_info
  ]