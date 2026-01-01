{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependentTypeValidationTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import DependentTypesParser
import Compiler.DependentTypeChecker
import Compiler.TypeChecker
import Compiler.IR
import SourceLocation
import Utils

import Data.Char (isSpace, isLetter, isDigit, toLower)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (tails, sort, intercalate)
import Data.String (IsString)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Property: Vector L.length type checking
prop_vector_length_typechecking :: Int -> Int -> Property
prop_vector_length_typechecking vecLen index =
  vecLen >= 0 && vecLen <= 100 && index >= 0 && index <= 100 ==>
  let vectorCode = "let v : Vector<" ++ show vecLen ++ ", int> = make_vector();"
      accessCode = "let x = v[" ++ show index ++ "];"
      fullCode = vectorCode ++ " " ++ accessCode
      result = checkDependentTypes fullCode
  in property $ if index < vecLen 
                then not (hasTypeError result)
                else hasTypeError result

-- Property: Matrix dimensions validation
prop_matrix_dimensions_validation :: Int -> Int -> Int -> Int -> Property
prop_matrix_dimensions_validation rows cols rowIdx colIdx =
  rows >= 0 && rows <= 10 && cols >= 0 && cols <= 10 &&
  rowIdx >= 0 && rowIdx <= 10 && colIdx >= 0 && colIdx <= 10 ==>
  let matrixCode = "let m : Matrix<" ++ show rows ++ ", " ++ show cols ++ ", int> = make_matrix();"
      accessCode = "let x = m[" ++ show rowIdx ++ "][" ++ show colIdx ++ "];"
      fullCode = matrixCode ++ " " ++ accessCode
      result = checkDependentTypes fullCode
  in property $ if rowIdx < rows && colIdx < cols
                then not (hasTypeError result)
                else hasTypeError result

-- Property: Refinement type checking
prop_refinement_typechecking :: Int -> Property
prop_refinement_typechecking value =
  value >= 0 && value <= 100 ==>
  let refinementCode = "let x : {v:int | v >= 0 && v <= 50} = " ++ show value ++ ";"
      result = checkDependentTypes refinementCode
  in property $ if value <= 50
                then not (hasTypeError result)
                else hasTypeError result

-- Property: Dependent function types
prop_dependent_function_types :: Int -> Property
prop_dependent_function_types n =
  n >= 0 && n <= 10 ==>
  let funcCode = "let f : (n:Nat) -> Vector<n, int> = \\n:Nat. make_vector_n(n);"
      callCode = "let v = f(" ++ show n ++ ");"
      fullCode = funcCode ++ " " ++ callCode
      result = checkDependentTypes fullCode
  in property $ not (hasTypeError result)

-- Property: Type-level computation
prop_type_level_computation :: Int -> Int -> Property
prop_type_level_computation m n =
  m >= 0 && m <= 10 && n >= 0 && n <= 10 ==>
  let typeCompCode = "type Add<m,n> = m + n;"
      vectorCode = "let v : Vector<Add<" ++ show m ++ ", " ++ show n ++ ">, int> = make_vector();"
      fullCode = typeCompCode ++ " " ++ vectorCode
      result = checkDependentTypes fullCode
  in property $ not (hasTypeError result)

-- Property: Singleton types
prop_singleton_types :: Int -> Property
prop_singleton_types value =
  value >= 0 && value <= 100 ==>
  let singletonCode = "let x : " ++ show value ++ " = " ++ show value ++ ";"
      result = checkDependentTypes singletonCode
  in property $ not (hasTypeError result)

-- Property: Proof terms validation
prop_proof_terms_validation :: Int -> Int -> Property
prop_proof_terms_validation m n =
  m >= 0 && m <= 50 && n >= 0 && n <= 50 ==>
  let proofCode = "let proof : m <= n = if " ++ show m ++ " <= " ++ show n ++ " then Refl else impossible;"
      result = checkDependentTypes proofCode
  in property $ if m <= n
                then not (hasTypeError result)
                else hasTypeError result

-- Property: Dependent pattern matching
prop_dependent_pattern_matching :: Int -> Property
prop_dependent_pattern_matching n =
  n >= 0 && n <= 10 ==>
  let patternCode = "let v : Vector<" ++ show n ++ ", int> = make_vector(); match v with | [] -> 0 | x::xs -> 1 end;"
      result = checkDependentTypes patternCode
  in property $ not (hasTypeError result)

-- Property: Type families
prop_type_families :: Int -> Property
prop_type_families n =
  n >= 0 && n <= 10 ==>
  let typeFamilyCode = "type family Elem n where Elem 0 = Void; Elem (n+1) = Int;"
      usageCode = "let x : Elem " ++ show n ++ " = 42;"
      fullCode = typeFamilyCode ++ " " ++ usageCode
      result = checkDependentTypes fullCode
  in property $ not (hasTypeError result)

-- Property: GADT validation
prop_gadt_validation :: String -> Property
prop_gadt_validation constructor =
  L.length constructor <= 10 && L.all isLetter constructor ==>
  let gadtCode = "data Expr a where Lit :: Int -> Expr Int; " ++ constructor ++ " :: Expr Bool -> Expr Bool;"
      result = checkDependentTypes gadtCode
  in property $ not (hasTypeError result) || not (constructor `L.isInfixOf` gadtCode)

-- Property: Dependent records
prop_dependent_records :: Int -> Property
prop_dependent_records size =
  size >= 0 && size <= 100 ==>
  let recordCode = "record Vec { len : Nat; data : Array len int; } let v : Vec = { len = " ++ show size ++ "; data = make_array(); };"
      result = checkDependentTypes recordCode
  in property $ not (hasTypeError result)

-- Property: Type-level natural numbers
prop_type_level_naturals :: Int -> Property
prop_type_level_naturals n =
  n >= 0 && n <= 20 ==>
  let natCode = "let x : Nat" ++ replicate n '+' ++ " = " ++ show n ++ ";"
      result = checkDependentTypes natCode
  in property $ not (hasTypeError result)

-- Property: Dependent type equality
prop_dependent_type_equality :: Int -> Int -> Property
prop_dependent_type_equality m n =
  m >= 0 && m <= 10 && n >= 0 && n <= 10 ==>
  let equalityCode = "let eq : Vector<" ++ show m ++ ", int> = Vector<" ++ show n ++ ", int> = if " ++ show m ++ " == " ++ show n ++ " then Refl else impossible;"
      result = checkDependentTypes equalityCode
  in property $ if m == n
                then not (hasTypeError result)
                else hasTypeError result

-- Property: Higher-kinded types
prop_higher_kinded_types :: String -> Property
prop_higher_kinded_types typeName =
  L.length typeName <= 10 && L.all isLetter typeName ==>
  let hktCode = "type " ++ typeName ++ " f = forall a. f a -> a;"
      result = checkDependentTypes hktCode
  in property $ not (hasTypeError result)

-- Property: Quantified types
prop_quantified_types :: String -> Property
prop_quantified_types varName =
  L.length varName <= 8 && L.all isLetter varName ==>
  let quantifiedCode = "let id : forall " ++ varName ++ ". " ++ varName ++ " -> " ++ varName ++ " = \\x. x;"
      result = checkDependentTypes quantifiedCode
  in property $ not (hasTypeError result)

-- Property: Type class constraints
prop_typeclass_constraints :: String -> Property
prop_typeclass_constraints className =
  L.length className <= 10 && L.all isLetter className ==>
  let classCode = "class " ++ className ++ " a where " ++ toLower (L.head className) : L.tail className ++ " :: a -> Int;"
      result = checkDependentTypes classCode
  in property $ not (hasTypeError result)

-- Property: Dependent type inference
prop_dependent_type_inference :: String -> Property
prop_dependent_type_inference expr =
  L.length expr <= 30 ==> -- Limit for performance
  let inferenceCode = "let x = " ++ expr ++ "; // type should be inferred"
      result = checkDependentTypes inferenceCode
  in property |]

-- Property: Type-level functions
prop_type_level_functions :: Int -> Property
prop_type_level_functions n =
  n >= 0 && n <= 10 ==>
  let typeFuncCode = "type Double n = n + n; let v : Vector<Double " ++ show n ++ ", int> = make_vector();"
      result = checkDependentTypes typeFuncCode
  in property $ not (hasTypeError result)

-- Property: Dependent type reduction
prop_dependent_type_reduction :: Int -> Property
prop_dependent_type_reduction n =
  n >= 0 && n <= 10 ==>
  let reductionCode = "type Fact n where Fact 0 = 1; Fact (n+1) = (n+1) * Fact n; let x : Fact " ++ show n ++ " = " ++ show (L.product [1..n]) ++ ";"
      result = checkDependentTypes reductionCode
  in property $ not (hasTypeError result)

-- Property: Type-level conditionals
prop_type_level_conditionals :: Bool -> Int -> Int -> Property
prop_type_level_conditionals cond m n =
  m >= 0 && m <= 10 && n >= 0 && n <= 10 ==>
  let conditionalCode = "type If b m n = if b then m else n; let x : If " ++ show cond ++ " " ++ show m ++ " " ++ show n ++ " = " ++ show (if cond then m else n) ++ ";"
      result = checkDependentTypes conditionalCode
  in property $ not (hasTypeError result)

-- Advanced dependent type tests

-- Property: Complex type expressions
prop_complex_type_expressions :: [Int] -> Property
prop_complex_type_expressions values =
  not (null values) && L.all (>=0) values && L.all (<=10) values && L.length values <= 5 ==>
  let complexCode = "let v : NestedVector<" ++ intercalate "," (map show values) ++ ", int> = make_nested();"
      result = checkDependentTypes complexCode
  in property |]

-- Property: Type-level recursion
prop_type_level_recursion :: Int -> Property
prop_type_level_recursion n =
  n >= 0 && n <= 5 ==> -- Limit for performance
  let recursiveCode = "type List n where List 0 = Nil; List (n+1) = Cons Int (List n); let l : List " ++ show n ++ " = make_list();"
      result = checkDependentTypes recursiveCode
  in property $ not (hasTypeError result)

-- Property: Dependent type unification
prop_dependent_type_unification :: Int -> Int -> Property
prop_dependent_type_unification m n =
  m >= 0 && m <= 10 && n >= 0 && n <= 10 ==>
  let unificationCode = "let f : forall n. Vector n int -> Vector n int = \\v. v; let result = f (make_vector_" ++ show m ++ "());"
      result = checkDependentTypes unificationCode
  in property $ not (hasTypeError result)

-- Helper function to check for type errors
hasTypeError :: TypeCheckResult -> Bool
hasTypeError result = case result of
  TypeError _ -> True
  _ -> False

tests :: TestTree
tests = testGroup "Dependent Type Validation Tests"
  [ fastProperty "Vector L.length type checking" prop_vector_length_typechecking
  , fastProperty "Matrix dimensions validation" prop_matrix_dimensions_validation
  , fastProperty "Refinement type checking" prop_refinement_typechecking
  , fastProperty "Dependent function types" prop_dependent_function_types
  , fastProperty "Type-level computation" prop_type_level_computation
  , fastProperty "Singleton types" prop_singleton_types
  , fastProperty "Proof terms validation" prop_proof_terms_validation
  , fastProperty "Dependent pattern matching" prop_dependent_pattern_matching
  , fastProperty "Type families" prop_type_families
  , fastProperty "GADT validation" prop_gadt_validation
  , fastProperty "Dependent records" prop_dependent_records
  , fastProperty "Type-level natural numbers" prop_type_level_naturals
  , fastProperty "Dependent type equality" prop_dependent_type_equality
  , fastProperty "Higher-kinded types" prop_higher_kinded_types
  , fastProperty "Quantified types" prop_quantified_types
  , fastProperty "Type class constraints" prop_typeclass_constraints
  , fastProperty "Dependent type inference" prop_dependent_type_inference
  , fastProperty "Type-level functions" prop_type_level_functions
  , fastProperty "Dependent type reduction" prop_dependent_type_reduction
  , fastProperty "Type-level conditionals" prop_type_level_conditionals
  , fastProperty "Complex type expressions" prop_complex_type_expressions
  , fastProperty "Type-level recursion" prop_type_level_recursion
  , fastProperty "Dependent type unification" prop_dependent_type_unification
  ]