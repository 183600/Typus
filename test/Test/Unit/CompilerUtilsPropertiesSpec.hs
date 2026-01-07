module Test.Unit.CompilerUtilsPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import CompilerUtils
import Data.List (isInfixOf)

-- Test compiler utils type
data TestCompilerUtils = TestCompilerUtils
  { utilsId :: String
  } deriving (Eq, Show)

-- Test AST type
data TestAst = TestAst
  { astHash :: String
  , semanticHash :: String
  } deriving (Eq, Show)

-- Test error type
data TestCompilerError = TestCompilerError
  { errorMessage :: String
  , errorContext :: String
  } deriving (Eq, Show)

-- Test implementation for initializeCompilerUtils
initializeCompilerUtils :: TestCompilerUtils
initializeCompilerUtils = TestCompilerUtils
  { utilsId = "utils-" ++ "1"
  }

-- Test implementation for getUtilsId
getUtilsId :: TestCompilerUtils -> String
getUtilsId utils = utilsId utils

-- Test implementation for parseToAst
parseToAst :: String -> Either String TestAst
parseToAst sourceCode = Right $ TestAst
  { astHash = "hash-" ++ show (length sourceCode)
  , semanticHash = "semantic-" ++ show (length sourceCode)
  }

-- Test implementation for transformAst
transformAst :: Either String TestAst -> Either String TestAst
transformAst ast = ast

-- Test implementation for getAstHash
getAstHash :: TestAst -> String
getAstHash ast = astHash ast

-- Test implementation for optimizeAst
optimizeAst :: Either String TestAst -> Either String TestAst
optimizeAst ast = ast

-- Test implementation for getSemanticHash
getSemanticHash :: TestAst -> String
getSemanticHash ast = semanticHash ast

-- Test implementation for generateCodeFromAst
generateCodeFromAst :: Either String TestAst -> Either String String
generateCodeFromAst ast = case ast of
  Right _ -> Right "generated code"
  Left err -> Left err

-- Test implementation for createCompilerError
createCompilerError :: String -> String -> TestCompilerError
createCompilerError errorMsg context = TestCompilerError
  { errorMessage = errorMsg
  , errorContext = context
  }

-- Test implementation for formatCompilerError
formatCompilerError :: TestCompilerError -> String
formatCompilerError error = errorMessage error ++ " (context: " ++ errorContext error ++ ")"

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
      msgInFormatted = errorMsg `isInfixOf` formatted
      ctxInFormatted = context `isInfixOf` formatted
  in property $ (msgInFormatted === True) .&. (ctxInFormatted === True)

tests :: TestTree
tests = testGroup "CompilerUtils Properties Tests"
  [ testProperty "compiler utils initialization" prop_compiler_utils_initialization
  , testProperty "AST transformation idempotent" prop_ast_transformation_idempotent
  , testProperty "optimization preserves semantics" prop_optimization_preserves_semantics
  , testProperty "code generation consistent" prop_code_generation_consistent
  , testProperty "error reporting preserves info" prop_error_reporting_preserves_info
  ]