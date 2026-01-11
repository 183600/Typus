{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestDependencyTypeConstraintsSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Dependencies
import Dependencies.AST
import Dependencies.TypeSystem
import Dependencies.Inference (TypeScheme(..))
import qualified Dependencies.TypeSystem as DT (TypeVar(..))
import SourceLocation (SourcePos(..))
import qualified Data.Text as T
import TestSupport.Arbitrary ()

-- | Test suite for Dependencies type constraints
testDependencyTypeConstraints :: TestTree
testDependencyTypeConstraints = testGroup "Dependencies Type Constraints Tests"
  [ testCase "newDependentTypeChecker: creates checker with empty environment" $
      let checker = newDependentTypeChecker ()
          env = initialTypeEnvironment
      in typeEnvTypes env @?= []
      
  , testCase "newDependentTypeCheckerWithTypes: creates checker with predefined types" $
      let types = [("int", DT.TypeVar "Int"), ("string", DT.TypeVar "String")]
          checker = newDependentTypeCheckerWithTypes types
          env = initialTypeEnvironment
      in length (typeEnvTypes env) >= 2  -- At least our types
      
  , testCase "addType: adds type to environment" $
      let checker = newDependentTypeChecker ()
          newType = DT.TypeVar "NewType"
          checker' = addType "NewType" newType checker
      in case lookupType "NewType" checker' of
           Just t -> t @?= newType
           Nothing -> assertFailure "Type not found in environment"
           
  , testCase "addConstraint: adds constraint to checker" $
      let checker = newDependentTypeChecker ()
          type1 = DT.TypeVar "Type1"
          type2 = DT.TypeVar "Type2"
          constraint = EqualityConstraint type1 type2
          checker' = addConstraint constraint checker
      in length (getConstraints checker') > length (getConstraints checker)
      
  , testCase "checkType: validates type in environment" $
      let checker = newDependentTypeChecker ()
          type1 = DT.TypeVar "Int"
          checker' = addType "Int" type1 checker
      in case checkType "Int" checker' of
           Right _ -> return ()
           Left err -> assertFailure $ "Type check failed: " ++ show err
           
  , testCase "checkType: fails for unknown type" $
      let checker = newDependentTypeChecker ()
      in case checkType "UnknownType" checker of
           Right _ -> assertFailure "Type check should have failed"
           Left _ -> return ()
           
  , testCase "checkTypeInstantiation: validates type instantiation" $
      let checker = newDependentTypeChecker ()
          baseType = DT.TypeVar "List"
          paramType = DT.TypeVar "Int"
          instantiated = TypeConstructor "List" [paramType]
          checker' = addType "List" baseType checker
      in case checkTypeInstantiation instantiated checker' of
           Right _ -> return ()
           Left err -> assertFailure $ "Type instantiation check failed: " ++ show err
           
  , testCase "solveConstraints: solves simple equality constraints" $
      let checker = newDependentTypeChecker ()
          type1 = DT.TypeVar "a"
          type2 = DT.TypeVar "Int"
          constraint = EqualityConstraint type1 type2
          checker' = addConstraint constraint checker
      in case solveConstraints checker' of
           Right solved -> length (getSubstitution solved) > 0
           Left err -> assertFailure $ "Constraint solving failed: " ++ show err
           
  , testCase "solveConstraints: handles multiple constraints" $
      let checker = newDependentTypeChecker ()
          type1 = DT.TypeVar "a"
          type2 = DT.TypeVar "b"
          type3 = DT.TypeVar "Int"
          constraint1 = EqualityConstraint type1 type2
          constraint2 = EqualityConstraint type2 type3
          checker' = addConstraint constraint1 $ addConstraint constraint2 checker
      in case solveConstraints checker' of
           Right solved -> length (getSubstitution solved) >= 2
           Left err -> assertFailure $ "Multiple constraint solving failed: " ++ show err
           
  , testCase "inferType: infers type for simple expression" $
      let checker = newDependentTypeChecker ()
          expr = VarExpr "x"
          typeAssumption = DT.TypeVar "Int"
          checker' = addType "x" typeAssumption checker
      in case inferType expr checker' of
           Right inferred -> inferred @?= typeAssumption
           Left err -> assertFailure $ "Type inference failed: " ++ show err
           
  , testCase "inferType: infers type for function application" $
      let checker = newDependentTypeChecker ()
          funcType = TypeArrow (DT.TypeVar "Int") (DT.TypeVar "Bool")
          argType = DT.TypeVar "Int"
          expr = ApplyExpr (VarExpr "func") (VarExpr "arg")
          checker' = addType "func" funcType $ addType "arg" argType checker
      in case inferType expr checker' of
           Right inferred -> inferred @?= DT.TypeVar "Bool"
           Left err -> assertFailure $ "Function application type inference failed: " ++ show err
           
  , testCase "inferStatement: infers type for variable declaration" $
      let checker = newDependentTypeChecker ()
          stmt = VarDeclStmt "x" (Just (DT.TypeVar "Int")) (LiteralExpr (IntLiteral 42))
      in case inferStatement stmt checker of
           Right (checker', inferred) -> inferred @?= DT.TypeVar "Int"
           Left err -> assertFailure $ "Variable declaration type inference failed: " ++ show err
           
  , testCase "inferProgram: infers types for sequence of statements" $
      let checker = newDependentTypeChecker ()
          stmt1 = VarDeclStmt "x" (Just (DT.TypeVar "Int")) (LiteralExpr (IntLiteral 42))
          stmt2 = VarDeclStmt "y" (Just (DT.TypeVar "Int")) (VarExpr "x")
          program = [stmt1, stmt2]
      in case inferProgram program checker of
           Right (checker', types) -> length types @?= 2
           Left err -> assertFailure $ "Program type inference failed: " ++ show err
           
  , testCase "generalize: creates polymorphic type scheme" $
      let checker = newDependentTypeChecker ()
          typeVar = DT.TypeVar "a"
          scheme = generalize typeVar checker
      in case scheme of
           TypeScheme vars _ -> length vars >= 1
           _ -> assertFailure "Generalization should create TypeScheme"
           
  , testCase "instantiate: creates fresh instance of type scheme" $
      let checker = newDependentTypeChecker ()
          typeVar = DT.TypeVar "a"
          scheme = generalize typeVar checker
      in case instantiate scheme checker of
           Right instanceType -> case instanceType of
              DT.TypeVar _ -> return ()
              _ -> assertFailure "Instantiation should create fresh type variable"
           Left err -> assertFailure $ "Type instantiation failed: " ++ show err
           
  , testCase "unifyTypes: unifies compatible types" $
      let checker = newDependentTypeChecker ()
          type1 = DT.TypeVar "a"
          type2 = DT.TypeVar "Int"
      in case unifyTypes type1 type2 checker of
           Right (checker', substitution) -> length substitution > 0
           Left err -> assertFailure $ "Type unification failed: " ++ show err
           
  , testCase "unifyTypes: fails for incompatible types" $
      let checker = newDependentTypeChecker ()
          type1 = DT.TypeVar "Int"
          type2 = DT.TypeVar "String"
      in case unifyTypes type1 type2 checker of
           Right _ -> assertFailure "Type unification should have failed"
           Left _ -> return ()
           
  , testCase "applyTypeSubstitution: applies substitution to type" $
      let checker = newDependentTypeChecker ()
          typeVar = DT.TypeVar "a"
          replacement = DT.TypeVar "Int"
          substitution = [("a", replacement)]
      in applyTypeSubstitution substitution typeVar @?= replacement
           
  , testCase "pushScope: creates new scope" $
      let checker = newDependentTypeChecker ()
          checker' = pushScope checker
      in depth (typeEnv checker') > depth (typeEnv checker)
      
  , testCase "popScope: restores previous scope" $
      let checker = newDependentTypeChecker ()
          checker' = pushScope checker
          checker'' = popScope checker'
      in depth (typeEnv checker'') == depth (typeEnv checker)
      
  , testCase "inNewScope: executes action in temporary scope" $
      let checker = newDependentTypeChecker ()
          action = \c -> addType "temp" (DT.TypeVar "Temp") c
      in depth (typeEnv (inNewScope action checker)) == depth (typeEnv checker)
  ]

-- Helper functions
lookupType :: String -> DependentTypeChecker -> Maybe TypeExpr
lookupType name checker = 
  case typeEnvTypes (typeEnv checker) of
    types -> lookup name types

getConstraints :: DependentTypeChecker -> [TypeConstraint]
getConstraints checker = typeConstraints (typeEnv checker)

getSubstitution :: DependentTypeChecker -> [(String, TypeExpr)]
getSubstitution checker = typeSubstitution (typeEnv checker)

depth :: TypeEnvironment -> Int
depth env = length (typeEnvScopes env)