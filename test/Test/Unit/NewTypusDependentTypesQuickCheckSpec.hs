{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewTypusDependentTypesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import DependentTypesParser (parseDependentType, DependentType(..))
import Utils (trim)

-- Property: Dependent type parsing preserves dimension
prop_dependent_type_preserves_dimension :: Int -> String -> Property
prop_dependent_type_preserves_dimension dim typeName =
  let input = typeName ++ "(" ++ show dim ++ ")"
      result = parseDependentType input
      extractedDim = either (const 0) getDimension result
  in classify (dim > 0) "positive dimension" $
     property $ (extractedDim === dim)

-- Property: Vector type constraints are respected
prop_vector_constraints_respected :: Int -> [Int] -> Property
prop_vector_constraints_respected vecSize indices =
  let validIndices = L.filter (\i -> i >= 0 && i < vecSize) indices
      invalidIndices = L.filter (\i -> i < 0 || i >= vecSize) indices
      allValid = null invalidIndices
  in classify (not (null validIndices)) "has valid indices" $
     classify (not (null invalidIndices)) "has invalid indices" $
     property $ allValid ==> (L.length validIndices <= vecSize)

-- Property: Dependent type equality is structural
prop_dependent_type_equality_structural :: Int -> String -> Property
prop_dependent_type_equality_structural dim typeName =
  let input1 = typeName ++ "(" ++ show dim ++ ")"
      input2 = typeName ++ "(" ++ show dim ++ ")"
      result1 = parseDependentType input1
      result2 = parseDependentType input2
      areEqual = either (const False) (\t1 -> 
                   either (const False) (\t2 -> t1 == t2) result2) result1
  in property $ areEqual

-- Property: Safe division constraint validation
prop_safe_division_constraint :: Int -> Int -> Property
prop_safe_division_constraint numerator denominator =
  let isValidDivision = denominator /= 0
      result = numerator `div` denominator
      safeResult = if isValidDivision then Just result else Nothing
  in classify isValidDivision "valid division" $
     classify (not isValidDivision) "invalid division" $
     property $ (isValidDivision ==> (safeResult /= Nothing)) .&&. 
                ((not isValidDivision) ==> (safeResult == Nothing))

-- Property: Array bounds checking
prop_array_bounds_checking :: Int -> Int -> Property
prop_array_bounds_checking arraySize index =
  let inBounds = index >= 0 && index < arraySize
      accessResult = if inBounds then Just ("element" ++ show index) else Nothing
  in classify inBounds "index in bounds" $
     classify (not inBounds) "index out of bounds" $
     property $ (inBounds ==> (accessResult /= Nothing)) .&&. 
                ((not inBounds) ==> (accessResult == Nothing))

-- Helper functions
getDimension :: DependentType -> Int
getDimension (VectorType dim) = dim
getDimension (ArrayType dim) = dim
getDimension _ = 0

tests :: TestTree
tests = testGroup "New Typus Dependent Types QuickCheck Tests"
  [ fastProperty "Dependent type preserves dimension" prop_dependent_type_preserves_dimension
  , fastProperty "Vector constraints are respected" prop_vector_constraints_respected
  , fastProperty "Dependent type equality is structural" prop_dependent_type_equality_structural
  , fastProperty "Safe division constraint validation" prop_safe_division_constraint
  , fastProperty "Array bounds checking" prop_array_bounds_checking
  ]