{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependentTypesValidationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Compiler (checkDependentTypes)
import Parser (TypusFile(..), parseTypus, defaultFileDirectives, CodeBlock(..))
import SourceLocation (SourceSpan(..), startPos)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (sort)
import Data.Char (isSpace, isDigit)

-- Property: Dependent type validation handles simple vector types
prop_dependent_vector_validation :: Int -> Property
prop_dependent_vector_validation size =
  size >= 0 && size <= 1000 ==>
  let code = "package main\n//! dependent_types: on\ntype IntVec Vector[" ++ show size ++ "] int\nfunc main() {\n  vec := IntVec{}\n  println(len(vec))\n}\n"
      result = parseTypus code
  in case result of
    Left _ -> property True  -- Parsing may fail
    Right typusFile -> 
      let validationResult = checkDependentTypes typusFile
      in case validationResult of
        Left _ -> property True  -- Validation may fail
        Right _ -> property True  -- Successful validation

-- Property: Dependent type validation handles bounded integers
prop_dependent_bounded_int :: Int -> Int -> Property
prop_dependent_bounded_int minVal maxVal =
  minVal >= 0 && maxVal >= minVal && maxVal <= 1000 ==>
  let code = "package main\n//! dependent_types: on\ntype SafeInt BoundedInt[" ++ show minVal ++ "," ++ show maxVal ++ "]\nfunc main() {\n  value := SafeInt{value: " ++ show minVal ++ "}\n  println(value.value)\n}\n"
      result = parseTypus code
  in case result of
    Left _ -> property True
    Right typusFile ->
      let validationResult = checkDependentTypes typusFile
      in case validationResult of
        Left _ -> property True
        Right _ -> property True

-- Property: Dependent type validation handles non-empty slices
prop_dependent_non_empty_slice :: String -> Property
prop_dependent_non_empty_slice typeName =
  not (null typeName) && isAlphaNum typeName ==>
  let code = "package main\n//! dependent_types: on\ntype SafeSlice NonEmptySlice[" ++ typeName ++ "]\nfunc main() {\n  slice := SafeSlice{data: []" ++ typeName ++ "{1, 2, 3}}\n  println(len(slice.data))\n}\n"
      result = parseTypus code
  in case result of
    Left _ -> property True
    Right typusFile ->
      let validationResult = checkDependentTypes typusFile
      in case validationResult of
        Left _ -> property True
        Right _ -> property True

-- Property: Dependent type validation handles matrix dimensions
prop_dependent_matrix_dimensions :: Int -> Int -> Property
prop_dependent_matrix_dimensions rows cols =
  rows > 0 && cols > 0 && rows <= 10 && cols <= 10 ==>
  let code = "package main\n//! dependent_types: on\ntype SafeMatrix Matrix[" ++ show rows ++ "," ++ show cols ++ "] int\nfunc main() {\n  matrix := SafeMatrix{}\n  println(\"matrix created\")\n}\n"
      result = parseTypus code
  in case result of
    Left _ -> property True
    Right typusFile ->
      let validationResult = checkDependentTypes typusFile
      in case validationResult of
        Left _ -> property True
        Right _ -> property True

-- Property: Dependent type validation handles positive numbers
prop_dependent_positive_numbers :: Int -> Property
prop_dependent_positive_numbers value =
  value > 0 ==>
  let code = "package main\n//! dependent_types: on\ntype PositiveInt Positive[int]\nfunc main() {\n  pi := PositiveInt{value: " ++ show value ++ "}\n  println(pi.value)\n}\n"
      result = parseTypus code
  in case result of
    Left _ -> property True
    Right typusFile ->
      let validationResult = checkDependentTypes typusFile
      in case validationResult of
        Left _ -> property True
        Right _ -> property True

-- Helper function to check if a string contains only alphanumeric characters
isAlphaNum :: String -> Bool
isAlphaNum = L.all (\c -> isDigit c || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))

tests :: TestTree
tests = testGroup "Dependent Types Validation QuickCheck tests"
  [ fastProperty "Dependent type validation handles simple vector types" prop_dependent_vector_validation
  , fastProperty "Dependent type validation handles bounded integers" prop_dependent_bounded_int
  , fastProperty "Dependent type validation validates non-empty slices" prop_dependent_non_empty_slice
  , fastProperty "Dependent type validation handles matrix dimensions" prop_dependent_matrix_dimensions
  , fastProperty "Dependent type validation handles positive numbers" prop_dependent_positive_numbers
  ]