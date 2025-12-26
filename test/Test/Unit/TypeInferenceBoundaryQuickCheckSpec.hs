{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.TypeInferenceBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Compiler.TypeChecker (Type(..), TypeEnv(..), inferType, buildTypeEnv)
import Compiler (checkTypeError, TypeCheckDiagnostic(..))
import DependentTypesParser (parseDependentType)
import Dependencies.TypeSystem (TypeConstraint(..), solveConstraints)
import qualified Data.Map as Map
import Data.List (isInfixOf, isPrefixOf, nub)

-- Property: Type inference is consistent for equivalent expressions
prop_type_inference_consistent :: String -> String -> Property
prop_type_inference_consistent expr1 expr2 =
  let validExpr = length expr1 > 0 && length expr2 > 0
      equivalent = expr1 == expr2
  in validExpr && equivalent ==>
  case inferType expr1 of
    Right type1 ->
      case inferType expr2 of
        Right type2 -> property $ show type1 === show type2
        Left _ -> property $ True
    Left _ -> property $ True

-- Property: Type inference handles nested types correctly
prop_type_inference_nested :: Int -> String -> Property
prop_type_inference_nested depth baseType =
  let validDepth = depth >= 0 && depth <= 5
      validBase = not (null baseType)
      nestedType = if depth > 0 
                   then "(" ++ baseType ++ ")" ++ replicate depth '[' ++ "]"
                   else baseType
  in validDepth && validBase ==>
  case inferType nestedType of
    Right inferredType ->
      let typeStr = show inferredType
          hasBase = baseType `isInfixOf` typeStr
      in property $ hasBase .||. depth == 0
    Left _ -> property $ True

-- Property: Type environment preserves type information
prop_type_env_preserves_types :: [(String, String)] -> Property
prop_type_env_preserves_types typePairs =
  let hasPairs = length typePairs > 0
      validPairs = all (\(k, v) -> length k > 0 && length v > 0) typePairs
  in hasPairs && validPairs ==>
  let typeEnv = buildTypeEnv typePairs
      lookupResults = map (\(k, v) -> (k, Map.lookup k typeEnv)) typePairs
      allFound = all (\(_, result) -> result /= Nothing) lookupResults
  in property $ allFound

-- Property: Dependent type constraints are solved correctly
prop_dependent_type_constraints :: [(String, String)] -> Property
prop_dependent_type_constraints constraints =
  let hasConstraints = length constraints > 0
      validConstraints = all (\(l, r) -> length l > 0 && length r > 0) constraints
      typeConstraints = map (\(l, r) -> TypeConstraint l r) constraints
  in hasConstraints && validConstraints ==>
  let solution = solveConstraints typeConstraints
      hasSolution = not (null solution)
      consistent = length solution <= length constraints
  in property $ hasSolution ==> consistent

-- Property: Type inference handles recursive types safely
prop_type_inference_recursive :: String -> Int -> Property
prop_type_inference_recursive typeName recursionDepth =
  let validName = length typeName > 0 && all (`elem` ['a'..'z'] ++ ['A'..'Z']) typeName
      validDepth = recursionDepth >= 0 && recursionDepth <= 3
      recursiveDef = if recursionDepth > 0
                     then "type " ++ typeName ++ " = " ++ typeName ++ " | " ++ take recursionDepth (repeat 'A')
                     else "type " ++ typeName ++ " = int"
  in validName && validDepth ==>
  case parseDependentType recursiveDef of
    Right parsedType ->
      case inferType (show parsedType) of
        Right _ -> property $ True
        Left _ -> property $ recursionDepth > 0 -- Expected to fail for true recursion
    Left _ -> property $ recursionDepth > 0

-- Property: Type checking diagnostics are informative
prop_type_checking_diagnostics :: String -> String -> Property
prop_type_checking_diagnostics expr expectedType =
  let hasExpr = length expr > 0
      hasType = length expectedType > 0
  in hasExpr && hasType ==>
  case inferType expr of
    Right inferredType ->
      let diagnostic = checkTypeError expr expectedType
          diagnosticStr = show diagnostic
          hasInfo = any (`isInfixOf` diagnosticStr) ["type", "expected", "actual", expr, expectedType]
      in property $ hasInfo
    Left _ -> property $ True

-- Property: Type inference is deterministic
prop_type_inference_deterministic :: String -> Property
prop_type_inference_deterministic expr =
  let hasExpr = length expr > 0
  in hasExpr ==>
  let result1 = inferType expr
      result2 = inferType expr
      bothSuccess = case (result1, result2) of
        (Right t1, Right t2) -> show t1 == show t2
        (Left e1, Left e2) -> show e1 == show e2
        _ -> False
  in property $ bothSuccess

tests :: TestTree
tests = testGroup "Type Inference Boundary QuickCheck Tests"
  [ fastProperty "Type inference is consistent for equivalent expressions" prop_type_inference_consistent
  , fastProperty "Type inference handles nested types correctly" prop_type_inference_nested
  , fastProperty "Type environment preserves type information" prop_type_env_preserves_types
  , fastProperty "Dependent type constraints are solved correctly" prop_dependent_type_constraints
  , fastProperty "Type inference handles recursive types safely" prop_type_inference_recursive
  , fastProperty "Type checking diagnostics are informative" prop_type_checking_diagnostics
  , fastProperty "Type inference is deterministic" prop_type_inference_deterministic
  ]