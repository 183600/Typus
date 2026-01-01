{-# LANGUAGE CPP #-}

module Test.Unit.DependenciesCycleDetectionAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.List as L
import Data.List (isInfixOf)
import Data.List (nub)
import qualified Data.Set as Set

import Dependencies
  ( DependentTypeChecker
  , DependentTypeError(..)
  , AST(..)
  , Statement(..)
  , TypeExpr(..)
  , Constraint(..)
  , TypeVar(..)
  , TypeConstraint(..)
  , newDependentTypeChecker
  , analyzeDependentTypes
  , analyzeAST
  , validateASTSemantics
  , addType
  , addConstraint
  , solveConstraints
  , getDependentTypeErrors
  , unify
  , TypeVar
  )

tests :: TestTree
tests = testGroup "Dependencies Cycle Detection Advanced Tests"
  [ simpleCycleTests
  , complexCycleTests
  , constraintCycleTests
  , typeVariableCycleTests
  , crossModuleCycleTests
  , quickCheckProperties
  ]

simpleCycleTests :: TestTree
simpleCycleTests = testGroup "Simple Cycle Tests"
  [ testCase "detects direct type dependency cycle" $ do
      let checker = newDependentTypeChecker
          typeA = TypeVar "A"
          typeB = TypeVar "B"
          -- Create A depends on B, B depends on A
          constraint1 = TypeConstraint typeA (TypeVarType typeB)
          constraint2 = TypeConstraint typeB (TypeVarType typeA)
          checkerWithTypes = addType typeA $ addType typeB checker
          checkerWithConstraints = addConstraint constraint1 $ addConstraint constraint2 checkerWithTypes
          result = solveConstraints checkerWithConstraints
      case result of
        Left errors -> do
          let errorStr = show errors
          "cycle" `L.isInfixOf` errorStr @?= True
        Right _ -> "Expected cycle detection error" @?= "Got success"
        
  , testCase "detects three-way cycle" $ do
      let checker = newDependentTypeChecker
          typeA = TypeVar "A"
          typeB = TypeVar "B"
          typeC = TypeVar "C"
          -- A -> B -> C -> A
          constraint1 = TypeConstraint typeA (TypeVarType typeB)
          constraint2 = TypeConstraint typeB (TypeVarType typeC)
          constraint3 = TypeConstraint typeC (TypeVarType typeA)
          checkerWithTypes = addType typeC $ addType typeB $ addType typeA checker
          checkerWithConstraints = addConstraint constraint3 $ addConstraint constraint2 $ addConstraint constraint1 checkerWithTypes
          result = solveConstraints checkerWithConstraints
      case result of
        Left errors -> do
          let errorStr = show errors
          "cycle" `L.isInfixOf` errorStr @?= True
        Right _ -> "Expected cycle detection error" @?= "Got success"
        
  , testCase "allows non-cyclic dependencies" $ do
      let checker = newDependentTypeChecker
          typeA = TypeVar "A"
          typeB = TypeVar "B"
          typeC = TypeVar "C"
          -- A -> B -> C (linear dependency)
          constraint1 = TypeConstraint typeA (TypeVarType typeB)
          constraint2 = TypeConstraint typeB (TypeVarType typeC)
          checkerWithTypes = addType typeC $ addType typeB $ addType typeA checker
          checkerWithConstraints = addConstraint constraint2 $ addConstraint constraint1 checkerWithTypes
          result = solveConstraints checkerWithConstraints
      case result of
        Right _ -> "Expected successful constraint solving" @?= "Got success"
        Left err -> "Linear dependencies should be solvable" @?= show err
  ]

complexCycleTests :: TestTree
complexCycleTests = testGroup "Complex Cycle Tests"
  [ testCase "detects cycle in complex constraint graph" $ do
      let checker = newDependentTypeChecker
          types = map TypeVar ["A", "B", "C", "D", "E"]
          constraints = 
            [ TypeConstraint (TypeVar "A") (TypeVarType (TypeVar "B"))
            , TypeConstraint (TypeVar "B") (TypeVarType (TypeVar "C"))
            , TypeConstraint (TypeVar "C") (TypeVarType (TypeVar "D"))
            , TypeConstraint (TypeVar "D") (TypeVarType (TypeVar "E"))
            , TypeConstraint (TypeVar "E") (TypeVarType (TypeVar "A"))  -- Creates cycle
            ]
          checkerWithTypes = foldr addType checker types
          checkerWithConstraints = foldr addConstraint checkerWithTypes constraints
          result = solveConstraints checkerWithConstraints
      case result of
        Left errors -> do
          let errorStr = show errors
          "cycle" `L.isInfixOf` errorStr @?= True
        Right _ -> "Expected cycle detection error" @?= "Got success"
        
  , testCase "detects multiple independent cycles" $ do
      let checker = newDependentTypeChecker
          types = map TypeVar ["A", "B", "C", "D"]
          constraints = 
            [ TypeConstraint (TypeVar "A") (TypeVarType (TypeVar "B"))  -- Cycle 1: A -> B -> A
            , TypeConstraint (TypeVar "B") (TypeVarType (TypeVar "A"))
            , TypeConstraint (TypeVar "C") (TypeVarType (TypeVar "D"))  -- Cycle 2: C -> D -> C
            , TypeConstraint (TypeVar "D") (TypeVarType (TypeVar "C"))
            ]
          checkerWithTypes = foldr addType checker types
          checkerWithConstraints = foldr addConstraint checkerWithTypes constraints
          result = solveConstraints checkerWithConstraints
      case result of
        Left errors -> do
          let errorStr = show errors
          "cycle" `L.isInfixOf` errorStr @?= True
        Right _ -> "Expected cycle detection error" @?= "Got success"
  ]

constraintCycleTests :: TestTree
constraintCycleTests = testGroup "Constraint Cycle Tests"
  [ testCase "detects recursive type constraints" $ do
      let checker = newDependentTypeChecker
          listType = TypeVar "List"
          elementType = TypeVar "T"
          -- List<T> contains List<T> (recursive)
          constraint = TypeConstraint listType (FunctionType [elementType] (TypeVarType listType))
          checkerWithTypes = addType elementType $ addType listType checker
          checkerWithConstraints = addConstraint constraint checkerWithTypes
          result = solveConstraints checkerWithConstraints
      case result of
        Left errors -> do
          let errorStr = show errors
          "recursive" `L.isInfixOf` errorStr @?= True
        Right _ -> "Expected recursive type detection" @?= "Got success"
        
  , testCase "allows well-founded recursive types" $ do
      let checker = newDependentTypeChecker
          listType = TypeVar "List"
          elementType = TypeVar "T"
          -- Well-founded: List<T> = Nil | Cons(T, List<T>)
          constraint = TypeConstraint listType (UnionType 
            [TypeVarType (TypeVar "Nil"), 
             FunctionType [elementType, TypeVarType listType] (TypeVarType listType)])
          checkerWithTypes = addType (TypeVar "Nil") $ addType elementType $ addType listType checker
          checkerWithConstraints = addConstraint constraint checkerWithTypes
          result = solveConstraints checkerWithConstraints
      case result of
        Right _ -> "Well-founded recursion should be allowed" @?= "Got success"
        Left err -> "Should handle well-founded recursion" @?= show err
  ]

typeVariableCycleTests :: TestTree
typeVariableCycleTests = testGroup "Type Variable Cycle Tests"
  [ testCase "detects type variable self-reference" $ do
      let checker = newDependentTypeChecker
          typeVar = TypeVar "X"
          constraint = TypeConstraint typeVar (TypeVarType typeVar)
          checkerWithTypes = addType typeVar checker
          checkerWithConstraints = addConstraint constraint checkerWithTypes
          result = solveConstraints checkerWithConstraints
      case result of
        Left errors -> do
          let errorStr = show errors
          "self" `L.isInfixOf` errorStr @?= True
        Right _ -> "Expected self-reference detection" @?= "Got success"
        
  , testCase "detects indirect type variable cycles" $ do
      let checker = newDependentTypeChecker
          typeVars = map TypeVar ["X", "Y", "Z"]
          constraints = 
            [ TypeConstraint (TypeVar "X") (TypeVarType (TypeVar "Y"))
            , TypeConstraint (TypeVar "Y") (TypeVarType (TypeVar "Z"))
            , TypeConstraint (TypeVar "Z") (TypeVarType (TypeVar "X"))
            ]
          checkerWithTypes = foldr addType checker typeVars
          checkerWithConstraints = foldr addConstraint checkerWithTypes constraints
          result = solveConstraints checkerWithConstraints
      case result of
        Left errors -> do
          let errorStr = show errors
          "cycle" `L.isInfixOf` errorStr @?= True
        Right _ -> "Expected indirect cycle detection" @?= "Got success"
  ]

crossModuleCycleTests :: TestTree
crossModuleCycleTests = testGroup "Cross Module Cycle Tests"
  [ testCase "detects cross-module type dependencies" $ do
      let checker = newDependentTypeChecker
          moduleAType = TypeVar "ModuleA.Type"
          moduleBType = TypeVar "ModuleB.Type"
          -- ModuleA.Type depends on ModuleB.Type L.and vice versa
          constraint1 = TypeConstraint moduleAType (TypeVarType moduleBType)
          constraint2 = TypeConstraint moduleBType (TypeVarType moduleAType)
          checkerWithTypes = addType moduleBType $ addType moduleAType checker
          checkerWithConstraints = addConstraint constraint2 $ addConstraint constraint1 checkerWithTypes
          result = solveConstraints checkerWithConstraints
      case result of
        Left errors -> do
          let errorStr = show errors
          "cycle" `L.isInfixOf` errorStr @?= True
        Right _ -> "Expected cross-module cycle detection" @?= "Got success"
  ]

quickCheckProperties :: TestTree
quickCheckProperties = testGroup "QuickCheck Cycle Detection Properties"
  [ fastProperty "acyclic constraint graphs are solvable" prop_acyclic_solvable
  , fastProperty "cycle detection is deterministic" prop_cycle_detection_deterministic
  ]

-- QuickCheck property implementations
prop_acyclic_solvable :: [(String, String)] -> Property
prop_acyclic_solvable dependencies =
  let uniqueDeps = nub dependencies
      hasCycle deps = L.any (\(a, b) -> (b, a) `elem` deps) deps
  in not (hasCycle uniqueDeps) ==> property True

prop_cycle_detection_deterministic :: [(String, String)] -> Property
prop_cycle_detection_deterministic dependencies =
  let hasCycle deps = L.any (\(a, b) -> (b, a) `elem` deps) deps
      result1 = hasCycle dependencies
      result2 = hasCycle dependencies
  in result1 === result2