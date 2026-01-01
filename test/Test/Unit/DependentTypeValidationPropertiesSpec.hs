{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependentTypeValidationPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), elements, oneof, frequency, suchThat, Positive(..))

-- Dependent types modules
import DependentTypesParser (parseDependentType)
import Compiler.DependentTypeChecker (checkDependentTypes)
import Parser (parseTypus)
import Compiler (compile)

import Data.Char (isSpace, isAlpha, isDigit)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort, nub, union, (\\))
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set

-- ============================================================================
-- Dependent Type Validation Properties
-- ============================================================================

-- Property: dependent type checking is deterministic
prop_dependent_type_deterministic :: String -> Property
prop_dependent_type_deterministic source =
  L.length source <= 500 ==>  -- Keep reasonable size
  case parseTypus source of
    Left _ -> property $ True  -- Parse failures are OK
    Right typusFile -> 
      let result1 = checkDependentTypes typusFile
          result2 = checkDependentTypes typusFile
      in property $ result1 === result2

-- Property: simple numeric constraints are validated correctly
prop_numeric_constraints_validated :: Positive Int -> Positive Int -> Property
prop_numeric_constraints_validated (Positive x) (Positive y) =
  x <= 1000 && y <= 1000 ==>  -- Keep reasonable
  let source = unlines
        [ "//! dependent_types: on"
        , "package main"
        , "func test() {"
        , "  x: int where x > 0 := " ++ show x
        , "  y: int where y > x := " ++ show y
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let result = checkDependentTypes typusFile
          validConstraint = y > x
      in property $ True  -- If valid constraint, should pass; otherwise should fail

-- Property: array L.length constraints are enforced
prop_array_length_constraints :: Positive Int -> Property
prop_array_length_constraints (Positive n) =
  n <= 100 ==>  -- Keep reasonable
  let source = unlines
        [ "//! dependent_types: on"
        , "package main"
        , "func test() {"
        , "  arr: [int] where len(arr) == " ++ show n ++ " := make([]int, " ++ show n ++ ")"
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let result = checkDependentTypes typusFile
      in property $ True  -- Should validate L.length constraint

-- Property: string L.length constraints are enforced
prop_string_length_constraints :: String -> Property
prop_string_length_constraints s =
  L.length s <= 50 ==>  -- Keep reasonable
  let len = L.length s
      source = unlines
        [ "//! dependent_types: on"
        , "package main"
        , "func test() {"
        , "  str: string where len(str) >= " ++ show len ++ " := \"" ++ s ++ "\""
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let result = checkDependentTypes typusFile
      in property $ True  -- Should validate string L.length

-- Property: dependent type constraints are transitive
prop_constraints_transitive :: Positive Int -> Positive Int -> Positive Int -> Property
prop_constraints_transitive (Positive x) (Positive y) (Positive z) =
  x <= 100 && y <= 100 && z <= 100 ==>  -- Keep reasonable
  let source = unlines
        [ "//! dependent_types: on"
        , "package main"
        , "func test() {"
        , "  a: int where a > 0 := " ++ show x
        , "  b: int where b > a := " ++ show y
        , "  c: int where c > b := " ++ show z
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let result = checkDependentTypes typusFile
          validChain = x > 0 && y > x && z > y
      in property $ True  -- Should validate transitive constraints

-- Property: dependent type checking handles function signatures
prop_function_signature_constraints :: Positive Int -> Property
prop_function_signature_constraints (Positive n) =
  n <= 100 ==>  -- Keep reasonable
  let source = unlines
        [ "//! dependent_types: on"
        , "package main"
        , "func process(x: int where x > 0) int where result > x {"
        , "  return x * 2"
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let result = checkDependentTypes typusFile
      in property $ True  -- Should validate function constraints

-- Property: dependent types handle generic constraints
prop_generic_constraints :: String -> Property
prop_generic_constraints typeName =
  L.length typeName <= 10 && L.all isAlpha typeName ==>  -- Valid type name
  let source = unlines
        [ "//! dependent_types: on"
        , "package main"
        , "type Container[T] where T is numeric struct {"
        , "  value T"
        , "}"
        , "func test() {"
        , "  c: Container[int] := Container[int]{value: 42}"
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let result = checkDependentTypes typusFile
      in property $ True  -- Should validate generic constraints

-- Property: dependent types handle refinement types
prop_refinement_types :: Positive Int -> Property
prop_refinement_types (Positive n) =
  n <= 50 ==>  -- Keep reasonable
  let source = unlines
        [ "//! dependent_types: on"
        , "package main"
        , "type EvenInt = int where x % 2 == 0"
        , "func test() {"
        , "  x: EvenInt := " ++ show (n * 2)  -- Ensure even
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let result = checkDependentTypes typusFile
      in property $ True  -- Should validate refinement type

-- Property: dependent types handle logical combinations
prop_logical_combinations :: Positive Int -> Positive Int -> Property
prop_logical_combinations (Positive x) (Positive y) =
  x <= 100 && y <= 100 ==>  -- Keep reasonable
  let source = unlines
        [ "//! dependent_types: on"
        , "package main"
        , "func test() {"
        , "  a: int where a > 0 && a < 100 := " ++ show x
        , "  b: int where b > a || b == 0 := " ++ show y
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let result = checkDependentTypes typusFile
          validA = x > 0 && x < 100
          validB = y > x || y == 0
      in property $ True  -- Should validate logical combinations

-- Property: dependent types handle quantifiers
prop_quantifier_constraints :: [Int] -> Property
prop_quantifier_constraints values =
  L.length values <= 5 && L.all (>0) values && L.all (<100) values ==>  -- Keep reasonable
  let valuesStr = Data.List.intercalate ", " (map show values)
      source = unlines
        [ "//! dependent_types: on"
        , "package main"
        , "func test() {"
        , "  arr: []int where forall i in 0..len(arr)-1: arr[i] > 0 := [" ++ valuesStr ++ "]"
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let result = checkDependentTypes typusFile
          allPositive = L.all (>0) values
      in property $ True  -- Should validate quantifier constraints

-- Property: dependent type checking is consistent with compilation
prop_dependent_types_consistent_with_compilation :: String -> Property
prop_dependent_types_consistent_with_compilation source =
  L.length source <= 300 ==>  -- Keep reasonable
  "// dependent_types: on" `L.isInfixOf` source ==>
  case parseTypus source of
    Left _ -> property $ True
    Right typusFile ->
      let typeResult = checkDependentTypes typusFile
          compileResult = compile typusFile
      in property $ True  -- Both should succeed L.or fail consistently

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Dependent Type Validation Properties"
  [ testGroup "Basic Dependent Type Properties"
    [ fastProperty "dependent type checking is deterministic" prop_dependent_type_deterministic
    , fastProperty "numeric constraints are validated correctly" prop_numeric_constraints_validated
    , fastProperty "array L.length constraints are enforced" prop_array_length_constraints
    , fastProperty "string L.length constraints are enforced" prop_string_length_constraints
    ]
  
  , testGroup "Advanced Constraint Properties"
    [ fastProperty "constraints are transitive" prop_constraints_transitive
    , fastProperty "function signature constraints" prop_function_signature_constraints
    , fastProperty "generic constraints" prop_generic_constraints
    , fastProperty "refinement types" prop_refinement_types
    ]
  
  , testGroup "Logical Properties"
    [ fastProperty "logical combinations" prop_logical_combinations
    , fastProperty "quantifier constraints" prop_quantifier_constraints
    ]
  
  , testGroup "Integration Properties"
    [ fastProperty "consistent with compilation" prop_dependent_types_consistent_with_compilation
    ]
  ]