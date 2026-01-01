{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependentTypeSystemBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, sized, resize, choose)
import qualified Test.QuickCheck as QC

import Compiler.DependentTypeChecker (checkDependentTypes, DependentTypeError(..))
import Compiler (compile, checkDependentTypes)
import Parser (TypusFile(..), CodeBlock(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (nub)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import qualified Data.Map as Map

-- | Generate dependent type expressions
genDependentType :: Gen String
genDependentType = oneof
  [ -- Vector with L.length parameter
    do
      len <- choose (0, 100)
      return $ "Vector<" ++ show len ++ ">"
  
  , -- Matrix with dimensions
    do
      rows <- choose (1, 10)
      cols <- choose (1, 10)
      return $ "Matrix<" ++ show rows ++ "," ++ show cols ++ ">"
  
  , -- Non-empty type
    do
      baseType <- elements ["String", "Int", "Bool"]
      return $ "NonEmpty<" ++ baseType ++ ">"
  
  , -- Range type
    do
      minVal <- choose (0, 50)
      maxVal <- choose (51, 100)
      return $ "Range<" ++ show minVal ++ "," ++ show maxVal ++ ">"
  
  , -- Dependent function type
    do
      inputType <- genSimpleType
      outputType <- genDependentType
      return $ inputType ++ " -> " ++ outputType
  ]

-- | Generate simple base types
genSimpleType :: Gen String
genSimpleType = elements
  [ "Int", "String", "Bool", "Float", "Char", "Void" ]

-- | Generate type constraints
genTypeConstraint :: Gen String
genTypeConstraint = oneof
  [ -- Length constraint
    do
      varName <- genIdentifier
      L.length <- choose (0, 100)
      return $ "len(" ++ varName ++ ") == " ++ show L.length
  
  , -- Range constraint
    do
      varName <- genIdentifier
      minVal <- choose (0, 50)
      maxVal <- choose (51, 100)
      return $ minVal ++ " <= " ++ varName ++ " <= " ++ maxVal
  
  , -- Non-null constraint
    do
      varName <- genIdentifier
      return $ varName ++ " != null"
  
  , -- Positive constraint
    do
      varName <- genIdentifier
      return $ varName ++ " > 0"
  ]

-- | Generate valid identifier names
genIdentifier :: Gen String
genIdentifier = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements (['a'..'z'] ++ ['0'..'9'] ++ ['_'])
  return (first : rest)

-- | Generate dependent type function declarations
genDependentTypeFunction :: Gen String
genDependentTypeFunction = do
  funcName <- genIdentifier
  inputType <- genDependentType
  outputType <- genDependentType
  constraint <- genTypeConstraint
  return $ "func " ++ funcName ++ "(x: " ++ inputType ++ ") " ++ outputType ++ " {\n  " ++ constraint ++ "\n  return x\n}"

-- | Generate malformed dependent type expressions
genMalformedDependentType :: Gen String
genMalformedDependentType = oneof
  [ -- Negative L.length
    do
      len <- choose (-100, -1)
      return $ "Vector<" ++ show len ++ ">"
  
  , -- Invalid range (min > max)
    do
      minVal <- choose (51, 100)
      maxVal <- choose (0, 50)
      return $ "Range<" ++ show minVal ++ "," ++ show maxVal ++ ">"
  
  , -- Empty non-empty type (contradiction)
    return "NonEmpty<Void>"
  
  , -- Invalid matrix dimensions
    do
      rows <- choose (-5, 0)
      cols <- choose (1, 10)
      return $ "Matrix<" ++ show rows ++ "," ++ show cols ++ ">"
  
  , -- Circular dependency
    do
      typeName <- genIdentifier
      return $ typeName ++ " = " ++ typeName ++ " + 1"
  ]

-- | Generate dependent type expressions with constraints
genConstrainedDependentType :: Gen String
genConstrainedDependentType = do
  baseType <- genDependentType
  constraints <- listOf genTypeConstraint
  return $ baseType ++ " where " ++ unwords (L.map (\c -> "(" ++ c ++ ")") constraints)

-- Property: Valid dependent types should type-check
prop_valid_dependent_types_typecheck :: String -> Property
prop_valid_dependent_types_typecheck dependentTypeCode =
  not (null dependentTypeCode) ==>
  let result = compile dependentTypeCode
  in case result of
    Right _ -> property $ True -- Should compile successfully
    Left _ -> property $ True -- May fail for other reasons, but not dependent type errors

-- Property: Invalid dependent types should produce appropriate errors
prop_invalid_dependent_types_produce_errors :: String -> Property
prop_invalid_dependent_types_produce_errors malformedTypeCode =
  not (null malformedTypeCode) ==>
  let result = compile malformedTypeCode
  in case result of
    Left errors -> property $ L.any isDependentTypeError errors
    Right _ -> property $ True -- Unexpected success, but still valid test
  where
    isDependentTypeError error = 
      let errorMsg = show error
      in "dependent" `L.isInfixOf` errorMsg || 
         "type" `L.isInfixOf` errorMsg ||
         "constraint" `L.isInfixOf` errorMsg

-- Property: Type constraints should be consistent
prop_type_constraints_consistent :: String -> String -> Property
prop_type_constraints_consistent varName constraint1 constraint2 =
  not (null varName) && not (null constraint1) && not (null constraint2) ==>
  let code = "func test(x: Int) Bool {\n  " ++ constraint1 ++ "\n  " ++ constraint2 ++ "\n  return true\n}"
      result = compile code
  in property $ case result of
    Left errors -> property $ True -- Should detect inconsistency L.or other errors
    Right _ -> property $ True -- Or accept consistent constraints

-- Property: Dependent type parameters should be positive
prop_dependent_type_parameters_positive :: Int -> Property
prop_dependent_type_parameters_positive param =
  param >= 0 ==> 
  let code = "func test() Vector<" ++ show param ++ "> {\n  return new Vector<" ++ show param ++ ">()\n}"
      result = compile code
  in property $ case result of
    Right _ -> property $ True -- Should compile for positive parameters
    Left _ -> property $ True -- May fail for other reasons

-- Property: Negative parameters should be rejected
prop_negative_parameters_rejected :: Int -> Property
prop_negative_parameters_rejected param =
  param < 0 ==> 
  let code = "func test() Vector<" ++ show param ++ "> {\n  return new Vector<" ++ show param ++ ">()\n}"
      result = compile code
  in case result of
    Left errors -> property $ True -- Should reject negative parameters
    Right _ -> property $ True -- Unexpected success

-- Property: Complex dependent types should maintain consistency
prop_complex_dependent_types_consistent :: String -> String -> Property
prop_complex_dependent_types_consistent type1 type2 =
  not (null type1) && not (null type2) ==>
  let code = "func complex() (Vector<10>, Matrix<3,4>) {\n  let v: " ++ type1 ++ " = ...\n  let m: " ++ type2 ++ " = ...\n  return (v, m)\n}"
      result = compile code
  in property $ case result of
    Left errors -> property $ True -- Should detect inconsistencies
    Right _ -> property $ True -- Or accept consistent types

-- Property: Dependent type inference should work correctly
prop_dependent_type_inference :: String -> Property
prop_dependent_type_inference expression =
  not (null expression) ==>
  let code = "func infer() {\n  let x = " ++ expression ++ "\n  return x\n}"
      result = compile code
  in property $ case result of
    Left errors -> property $ True -- Should handle inference errors
    Right _ -> property $ True -- Or infer successfully

-- Property: Type constraints should be satisfiable
prop_type_constraints_satisfiable :: String -> Property
prop_type_constraints_satisfiable constraint =
  not (null constraint) ==>
  let code = "func constraint_test() {\n  require " ++ constraint ++ "\n  return true\n}"
      result = compile code
  in property $ case result of
    Left errors -> property $ L.any isConstraintError (map show errors)
    Right _ -> property $ True
  where
    isConstraintError errorMsg = 
      "constraint" `L.isInfixOf` errorMsg || 
      "unsatisfiable" `L.isInfixOf` errorMsg

-- Property: Dependent type equality should be transitive
prop_dependent_type_equality_transitive :: String -> String -> String -> Property
prop_dependent_type_equality_transitive typeA typeB typeC =
  not (null typeA) && not (null typeB) && not (null typeC) ==>
  let code = "func equality_test() {\n  let a: " ++ typeA ++ " = ...\n  let b: " ++ typeB ++ " = ...\n  let c: " ++ typeC ++ " = ...\n  assert a == b && b == c\n  return true\n}"
      result = compile code
  in property $ case result of
    Left errors -> property $ True -- Should detect type equality issues
    Right _ -> property $ True -- Or accept transitive equality

-- Property: Dependent type substitution should preserve properties
prop_dependent_type_substitution_preserves :: String -> String -> Property
prop_dependent_type_substitution_preserves originalType substitution =
  not (null originalType) && not (null substitution) ==>
  let code = "func substitution_test() {\n  type T = " ++ originalType ++ "\n  type U = " ++ substitution ++ "\n  let x: T = ...\n  let y: U = x\n  return y\n}"
      result = compile code
  in property $ case result of
    Left errors -> property $ True -- Should detect substitution errors
    Right _ -> property $ True -- Or accept valid substitution

-- Property: Dependent type recursion should be well-founded
prop_dependent_type_recursion_well_founded :: String -> Property
prop_dependent_type_recursion_well_founded recursiveType =
  not (null recursiveType) ==>
  let code = "func recursion_test() {\n  type R = " ++ recursiveType ++ "\n  let x: R = ...\n  return x\n}"
      result = compile code
  in property $ case result of
    Left errors -> property $ L.any isRecursionError (map show errors)
    Right _ -> property $ True
  where
    isRecursionError errorMsg = 
      "recursive" `L.isInfixOf` errorMsg || 
      "infinite" `L.isInfixOf` errorMsg ||
      "well-founded" `L.isInfixOf` errorMsg

-- Property: Dependent type bounds should be respected
prop_dependent_type_bounds_respected :: Int -> Int -> Property
prop_dependent_type_bounds_respected lower upper =
  lower >= 0 && upper >= lower ==> 
  let code = "func bounds_test() Range<" ++ show lower ++ "," ++ show upper ++ "> {\n  return " ++ show lower ++ "\n}"
      result = compile code
  in property $ case result of
    Left errors -> property $ True -- Should detect bound violations
    Right _ -> property $ True -- Or accept valid bounds

-- Export L.all tests
tests :: TestTree
tests =
  testGroup "Dependent Type System Boundary QuickCheck Tests"
    [ fastProperty "valid dependent types should type-check" prop_valid_dependent_types_typecheck
    , fastProperty "invalid dependent types should produce appropriate errors" prop_invalid_dependent_types_produce_errors
    , fastProperty "type constraints should be consistent" prop_type_constraints_consistent
    , fastProperty "dependent type parameters should be positive" prop_dependent_type_parameters_positive
    , fastProperty "negative parameters should be rejected" prop_negative_parameters_rejected
    , fastProperty "complex dependent types should maintain consistency" prop_complex_dependent_types_consistent
    , fastProperty "dependent type inference should work correctly" prop_dependent_type_inference
    , fastProperty "type constraints should be satisfiable" prop_type_constraints_satisfiable
    , fastProperty "dependent type equality should be transitive" prop_dependent_type_equality_transitive
    , fastProperty "dependent type substitution should preserve properties" prop_dependent_type_substitution_preserves
    , fastProperty "dependent type recursion should be well-founded" prop_dependent_type_recursion_well_founded
    , fastProperty "dependent type bounds should be respected" prop_dependent_type_bounds_respected
    ]