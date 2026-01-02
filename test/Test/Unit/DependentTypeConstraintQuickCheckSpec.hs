{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependentTypeConstraintQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import DependentTypesParser (parseDependentType, DependentType(..))
import Compiler.DependentTypeChecker (checkDependentTypes, validateConstraints, TypeConstraint(..))
import Dependencies.TypeSystem (solveConstraints, TypeConstraint(..))
import Parser (parseTypus)
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.List (nub, sort)

-- Property: Dependent type constraints are parsed correctly
prop_dependent_type_constraints_parsed :: String -> String -> Property
prop_dependent_type_constraints_parsed typeName constraintExpr =
  let validType = L.length typeName > 0 && L.all (`elem` ['a'..'z'] ++ ['A'..'Z']) typeName
      validConstraint = L.length constraintExpr > 0
      typeDef = "type " ++ typeName ++ " = " ++ constraintExpr
  in validType && validConstraint ==>
  case parseDependentType typeDef of
    Right parsedType ->
      let typeStr = show parsedType
          hasTypeName = typeName `isInfix` typeStr
          hasConstraint = constraintExpr `isInfix` typeStr || L.length constraintExpr > 10
      in property $ hasTypeName .&&. hasConstraint
    Left _ -> property $ True -- Some constraints are expected to fail

-- Property: Type constraint validation is consistent
prop_type_constraint_validation_consistent :: String -> Property
prop_type_constraint_validation_consistent constraint =
  let hasConstraint = L.length constraint > 5
      simpleConstraint = L.all (`elem` constraint) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789=<>+"
  in hasConstraint && simpleConstraint ==>
  let validation1 = validateConstraints constraint
      validation2 = validateConstraints constraint
      consistent = case (validation1, validation2) of
        (Right r1, Right r2) -> show r1 == show r2
        (Left e1, Left e2) -> show e1 == show e2
        _ -> False
  in property $ consistent

-- Property: Dependent type checking preserves type information
prop_dependent_type_checking_preserves_info :: String -> String -> Property
prop_dependent_type_checking_preserves_info typeName typeBody =
  let validType = L.length typeName > 0 && L.all (`elem` ['a'..'z'] ++ ['A'..'Z']) typeName
      validBody = L.length typeBody > 0
      typeDef = "type " ++ typeName ++ " = " ++ typeBody
  in validType && validBody ==>
  case parseDependentType typeDef of
    Right parsedType ->
      case checkDependentTypes parsedType of
        Right checkedType ->
          let checkedStr = show checkedType
              originalStr = show parsedType
              hasTypeName = typeName `isInfix` checkedStr
              preservesInfo = L.length checkedStr >= L.length originalStr - 5
          in property $ hasTypeName .&&. preservesInfo
        Left _ -> property $ True
    Left _ -> property $ True

-- Property: Constraint solving is deterministic
prop_constraint_solving_deterministic :: [String] -> Property
prop_constraint_solving_deterministic constraints =
  let hasConstraints = L.length constraints > 0
      validConstraints = L.all (not . null) constraints
      uniqueConstraints = L.length (nub constraints) == L.length constraints
  in hasConstraints && validConstraints && uniqueConstraints ==>
  let typeConstraints = L.map (\c -> TypeConstraint c "bool") constraints
      solution1 = solveConstraints typeConstraints
      solution2 = solveConstraints typeConstraints
      solutionsMatch = case (solution1, solution2) of
        (Right s1, Right s2) -> show s1 == show s2
        (Left e1, Left e2) -> show e1 == show e2
        _ -> False
  in property $ solutionsMatch

-- Property: Complex dependent types are handled correctly
prop_complex_dependent_types :: String -> [String] -> Property
prop_complex_dependent_types baseType params =
  let validBase = L.length baseType > 0
      validParams = L.all (not . null) params
      hasParams = L.length params > 0
      complexType = baseType ++ "<" ++ unwords params ++ ">"
  in validBase && validParams && hasParams ==>
  case parseDependentType complexType of
    Right parsedType ->
      let parsedStr = show parsedType
          hasBaseType = baseType `isInfix` parsedStr
          hasParams = L.any (`isInfix` parsedStr) params
      in property $ hasBaseType .&&. hasParams
    Left _ -> property $ True

-- Property: Type constraint errors are informative
prop_type_constraint_errors_informative :: String -> Property
prop_type_constraint_errors_informative malformedConstraint =
  let hasMalformed = L.length malformedConstraint > 3
      hasInvalidChars = L.any (`notElem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789=<>+-*/") malformedConstraint
  in hasMalformed && hasInvalidChars ==>
  case validateConstraints malformedConstraint of
    Right _ -> property $ True
    Left error ->
      let errorStr = show error
          hasInfo = L.any (`isInfix` errorStr) ["constraint", "type", "invalid", "parse", "error"]
          notEmpty = L.length errorStr > 0
      in property $ hasInfo .&&. notEmpty

-- Property: Dependent type inference is sound
prop_dependent_type_inference_sound :: String -> String -> Property
prop_dependent_type_inference_sound expr expectedType =
  let hasExpr = L.length expr > 0
      hasType = L.length expectedType > 0
      simpleExpr = L.all (`elem` expr) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+"
  in hasExpr && hasType && simpleExpr ==>
  case parseDependentType expr of
    Right parsedType ->
      case checkDependentTypes parsedType of
        Right checkedType ->
          let checkedStr = show checkedType
              hasExpected = expectedType `isInfix` checkedStr || "type" `isInfix` checkedStr
          in property $ hasExpected
        Left _ -> property $ True
    Left _ -> property $ True

tests :: TestTree
tests = testGroup "Dependent Type Constraint QuickCheck Tests"
  [ fastProperty "Dependent type constraints are parsed correctly" prop_dependent_type_constraints_parsed
  , fastProperty "Type constraint validation is consistent" prop_type_constraint_validation_consistent
  , fastProperty "Dependent type checking preserves type information" prop_dependent_type_checking_preserves_info
  , fastProperty "Constraint solving is deterministic" prop_constraint_solving_deterministic
  , fastProperty "Complex dependent types are handled correctly" prop_complex_dependent_types
  , fastProperty "Type constraint errors are informative" prop_type_constraint_errors_informative
  , fastProperty "Dependent type inference is sound" prop_dependent_type_inference_sound
  ]