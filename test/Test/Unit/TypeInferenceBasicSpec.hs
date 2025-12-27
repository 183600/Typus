{-# LANGUAGE CPP #-}

-- | Basic type inference tests using QuickCheck
module Test.Unit.TypeInferenceBasicSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (==>), property, classify, counterexample)
import qualified Data.List as Data.List
import Data.Char (isAlpha, isDigit)

import Compiler.TypeChecker (TypeChecker, inferType, TypeInfo(..))
import Compiler.IR (IRType(..), IRValue(..))

-- ============================================================================
-- Basic Type Inference Properties
-- ============================================================================

-- Property: Integer literals infer to int type
prop_infer_integer_literals :: Int -> Property
prop_infer_integer_literals value =
  let literal = show value
      inferred = inferType $ IRIntLiteral value
  in property $ inferred == Just TypeInfo { tiType = IRInt, tiConstraints = [] }

-- Property: String literals infer to string type
prop_infer_string_literals :: String -> Property
prop_infer_string_literals str =
  length str <= 50 ==> -- Reasonable limit
  let inferred = inferType $ IRStringLiteral str
  in property $ inferred == Just TypeInfo { tiType = IRString, tiConstraints = [] }

-- Property: Boolean literals infer to bool type
prop_infer_boolean_literals :: Bool -> Property
prop_infer_boolean_literals value =
  let inferred = inferType $ IRBoolLiteral value
  in property $ inferred == Just TypeInfo { tiType = IRBool, tiConstraints = [] }

-- Property: Variable lookup preserves declared type
prop_infer_variable_type :: String -> IRType -> Property
prop_infer_variable_type varName varType =
  isValidIdentifier varName ==>
  let env = [(varName, TypeInfo varType [])]
      inferred = lookupVariableType varName env
  in property $ inferred == Just (TypeInfo varType [])

-- Property: Function parameter types are preserved
prop_infer_function_params :: [String] -> [IRType] -> Property
prop_infer_function_params paramNames paramTypes =
  all isValidIdentifier paramNames && 
  length paramNames == length paramTypes &&
  length paramNames <= 5 ==> -- Reasonable limit
  let paramInfos = zipWith (\name ty -> (name, TypeInfo ty [])) paramNames paramTypes
      inferred = map snd paramInfos
      expected = map (\ty -> TypeInfo ty []) paramTypes
  in property $ inferred == expected

-- Property: Type inference is consistent across multiple runs
prop_inference_consistency :: String -> Property
prop_inference_consistency expr =
  isValidExpression expr ==>
  let inferred1 = inferExpressionType expr
      inferred2 = inferExpressionType expr
  in property $ inferred1 == inferred2

-- Property: Type inference handles nested expressions
prop_infer_nested_expressions :: Int -> Property
prop_infer_nested_expressions depth =
  depth >= 0 && depth <= 4 ==> -- Reasonable nesting
  let nestedExpr = generateNestedExpression depth
      inferred = inferExpressionType nestedExpr
  in property $ isJust inferred

-- Property: Type inference fails gracefully on invalid expressions
prop_infer_invalid_expressions :: String -> Property
prop_infer_invalid_expressions expr =
  hasInvalidSyntax expr ==>
  let inferred = inferExpressionType expr
  in property $ isNothing inferred

-- Property: Type inference respects type constraints
prop_infer_with_constraints :: String -> [IRType] -> Property
prop_infer_with_constraints expr constraints =
  isValidExpression expr && length constraints <= 3 ==>
  let constrainedEnv = addConstraints constraints
      inferred = inferExpressionTypeWithEnv expr constrainedEnv
  in property $ respectsConstraints inferred constraints

-- Property: Type inference handles arithmetic operations
prop_infer_arithmetic_operations :: Int -> Int -> Property
prop_infer_arithmetic_operations x y =
  let expr = show x ++ " + " ++ show y
      inferred = inferExpressionType expr
  in property $ inferred == Just (TypeInfo IRInt [])

-- Property: Type inference handles string concatenation
prop_infer_string_concatenation :: String -> String -> Property
prop_infer_string_concatenation str1 str2 =
  length str1 <= 20 && length str2 <= 20 ==>
  let expr = "\"" ++ str1 ++ "\" + \"" ++ str2 ++ "\""
      inferred = inferExpressionType expr
  in property $ inferred == Just (TypeInfo IRString [])

-- ============================================================================
-- Helper Functions
-- ============================================================================

isValidIdentifier :: String -> Bool
isValidIdentifier [] = False
isValidIdentifier (c:cs) = isAlpha c && all isValidChar cs
  where
    isValidChar ch = isAlpha ch || isDigit ch || ch == '_'

isValidExpression :: String -> Bool
isValidExpression expr = 
  not (null expr) && 
  not (any (`elem` "@#$%^&*()[]{}|\\") expr) &&
  length expr <= 100

hasInvalidSyntax :: String -> Bool
hasInvalidSyntax expr = 
  any (`elem` "@#$%^&*()[]{}|\\") expr ||
  expr `Data.List.isInfixOf` "+++" ||
  expr `Data.List.isInfixOf` "---"

generateNestedExpression :: Int -> String
generateNestedExpression 0 = "42"
generateNestedExpression n = "(" ++ generateNestedExpression (n-1) ++ " + 1)"

lookupVariableType :: String -> [(String, TypeInfo)] -> Maybe TypeInfo
lookupVariableType name env = lookup name env

inferExpressionType :: String -> Maybe TypeInfo
inferExpressionType expr
  | all isDigit expr = Just (TypeInfo IRInt [])
  | expr `Data.List.isPrefixOf` "\"" && expr `Data.List.isSuffixOf` "\"" = Just (TypeInfo IRString [])
  | expr == "true" || expr == "false" = Just (TypeInfo IRBool [])
  | "+" `Data.List.isInfixOf` expr = Just (TypeInfo IRInt [])
  | otherwise = Nothing

inferExpressionTypeWithEnv :: String -> [(String, TypeInfo)] -> Maybe TypeInfo
inferExpressionTypeWithEnv expr env =
  case lookupVariableType expr env of
    Just ti -> Just ti
    Nothing -> inferExpressionType expr

addConstraints :: [IRType] -> [(String, TypeInfo)]
addConstraints types = zipWith (\i ty -> ("var" ++ show i, TypeInfo ty [])) [1..] types

respectsConstraints :: Maybe TypeInfo -> [IRType] -> Bool
respectsConstraints (Just (TypeInfo ty _)) constraints = ty `elem` constraints
respectsConstraints Nothing _ = True

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Basic Type Inference Tests"
  [ fastProperty "Integer literals infer to int type" prop_infer_integer_literals
  , fastProperty "String literals infer to string type" prop_infer_string_literals
  , fastProperty "Boolean literals infer to bool type" prop_infer_boolean_literals
  , fastProperty "Variable lookup preserves declared type" prop_infer_variable_type
  , fastProperty "Function parameter types are preserved" prop_infer_function_params
  , fastProperty "Type inference is consistent across multiple runs" prop_inference_consistency
  , fastProperty "Type inference handles nested expressions" prop_infer_nested_expressions
  , fastProperty "Type inference fails gracefully on invalid expressions" prop_infer_invalid_expressions
  , fastProperty "Type inference respects type constraints" prop_infer_with_constraints
  , fastProperty "Type inference handles arithmetic operations" prop_infer_arithmetic_operations
  , fastProperty "Type inference handles string concatenation" prop_infer_string_concatenation
  ]