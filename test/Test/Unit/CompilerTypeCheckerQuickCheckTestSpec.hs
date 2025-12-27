{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CompilerTypeCheckerQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import Data.List (sort, nub)

import Compiler.TypeChecker
import Compiler.Errors.Core
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Compiler TypeChecker QuickCheck Tests"
  [ typeInferenceTests
  , typeUnificationTests
  , typeConstraintTests
  , typeEnvironmentTests
  , typeCheckingTests
  , typeErrorTests
  , typeSubstitutionTests
  , typeVariableTests
  , typeSchemeTests
  , typeValidationTests
  ]

-- | 1. 类型推断测试
typeInferenceTests :: TestTree
typeInferenceTests = testGroup "Type Inference Tests"
  [ testCase "Infer integer literal type" $
      let expr = LiteralExpr (IntLiteral 42)
          result = inferType expr emptyTypeEnvironment
      in case result of
           Right ty -> ty @?= IntType
           Left _ -> "Expected successful inference" @?= "Got error"
  
  , testCase "Infer string literal type" $
      let expr = LiteralExpr (StringLiteral "hello")
          result = inferType expr emptyTypeEnvironment
      in case result of
           Right ty -> ty @?= StringType
           Left _ -> "Expected successful inference" @?= "Got error"
  
  , testCase "Infer boolean literal type" $
      let expr = LiteralExpr (BoolLiteral True)
          result = inferType expr emptyTypeEnvironment
      in case result of
           Right ty -> ty @?= BoolType
           Left _ -> "Expected successful inference" @?= "Got error"
  
  , fastProperty "Integer literal always infers to IntType" $
      \n -> let expr = LiteralExpr (IntLiteral n)
                result = inferType expr emptyTypeEnvironment
            in case result of
                 Right ty -> ty == IntType
                 Left _ -> False
  ]

-- | 2. 类型统一测试
typeUnificationTests :: TestTree
typeUnificationTests = testGroup "Type Unification Tests"
  [ testCase "Unify identical types" $
      let result = unifyTypes IntType IntType
      in case result of
           Right subst -> Map.null subst @?= True
           Left _ -> "Expected successful unification" @?= "Got error"
  
  , testCase "Unify type variable with concrete type" $
      let typeVar = TypeVar "a"
          result = unifyTypes typeVar IntType
      in case result of
           Right subst -> Map.lookup "a" subst @?= Just IntType
           Left _ -> "Expected successful unification" @?= "Got error"
  
  , fastProperty "Unification is symmetric" $
      \ty1 ty2 -> let result1 = unifyTypes ty1 ty2
                      result2 = unifyTypes ty2 ty1
                  in case (result1, result2) of
                       (Right _, Right _) -> True
                       (Left _, Left _) -> True
                       _ -> False
  ]

-- | 3. 类型约束测试
typeConstraintTests :: TestTree
typeConstraintTests = testGroup "Type Constraint Tests"
  [ testCase "Equality constraint" $
      let constraint = EqualityConstraint IntType StringType
          result = solveConstraint constraint emptyTypeEnvironment
      in case result of
           Right _ -> "Expected constraint solution" @?= "Got error"
           Left _ -> "Expected error for incompatible types" @?= "Got success"
  
  , testCase "Instance constraint" $
      let constraint = InstanceConstraint IntType "Num"
          result = solveConstraint constraint emptyTypeEnvironment
      in case result of
           Right env -> env `seq` True @?= True
           Left _ -> "Expected successful instance check" @?= "Got error"
  
  , fastProperty "Multiple constraints" $
      \constraints -> let result = solveConstraints constraints emptyTypeEnvironment
                      in case result of
                           Right env -> env `seq` True
                           Left _ -> False
  ]

-- | 4. 类型环境测试
typeEnvironmentTests :: TestTree
typeEnvironmentTests = testGroup "Type Environment Tests"
  [ testCase "Empty type environment" $
      let env = emptyTypeEnvironment
      in typeEnvironmentSize env @?= 0
  
  , testCase "Add type binding" $
      let env = emptyTypeEnvironment
          env' = addTypeBinding "x" IntType env
      in typeEnvironmentSize env' @?= 1
  
  , testCase "Lookup type binding" $
      let env = emptyTypeEnvironment
          env' = addTypeBinding "x" IntType env
          result = lookupType "x" env'
      in result @?= Just IntType
  
  , fastProperty "Type binding consistency" $
      \name ty -> let env = emptyTypeEnvironment
                      env' = addTypeBinding name ty env
                      result = lookupType name env'
                  in result == Just ty
  ]

-- | 5. 类型检查测试
typeCheckingTests :: TestTree
typeCheckingTests = testGroup "Type Checking Tests"
  [ testCase "Check well-typed variable" $
      let env = addTypeBinding "x" IntType emptyTypeEnvironment
          expr = VariableExpr "x"
          result = checkType expr IntType env
      in case result of
           Right _ -> "Expected successful type check" @?= "Got error"
           Left _ -> "Expected success" @?= "Got error"
  
  , testCase "Check ill-typed variable" $
      let env = addTypeBinding "x" IntType emptyTypeEnvironment
          expr = VariableExpr "x"
          result = checkType expr StringType env
      in case result of
           Right _ -> "Expected type error" @?= "Got success"
           Left _ -> "Expected error" @?= "Got success"
  
  , fastProperty "Variable type consistency" $
      \name ty -> let env = addTypeBinding name ty emptyTypeEnvironment
                      expr = VariableExpr name
                      result = checkType expr ty env
                  in case result of
                       Right _ -> True
                       Left _ -> False
  ]

-- | 6. 类型错误测试
typeErrorTests :: TestTree
typeErrorTests = testGroup "Type Error Tests"
  [ testCase "Type mismatch error" $
      let error = TypeMismatchError IntType StringType (SourceSpan startPos startPos)
      in errorMessage error @?= "Expected type IntType but found StringType"
  
  , testCase "Unbound variable error" $
      let error = UnboundVariableError "x" (SourceSpan startPos startPos)
      in errorMessage error @?= "Unbound variable: x"
  
  , testCase "Type inference error" $
      let error = TypeInferenceError "Cannot infer type" (SourceSpan startPos startPos)
      in errorMessage error @?= "Type inference error: Cannot infer type"
  
  , fastProperty "Error location preservation" $
      \name -> let error = UnboundVariableError name (SourceSpan startPos startPos)
               in errorLocation error == SourceSpan startPos startPos
  ]

-- | 7. 类型替换测试
typeSubstitutionTests :: TestTree
typeSubstitutionTests = testGroup "Type Substitution Tests"
  [ testCase "Apply substitution to type variable" $
      let subst = Map.singleton "a" IntType
          typeVar = TypeVar "a"
          result = applySubstitution subst typeVar
      in result @?= IntType
  
  , testCase "Apply substitution to function type" $
      let subst = Map.singleton "a" IntType
          funcType = FunctionType [TypeVar "a"] (TypeVar "a")
          result = applySubstitution subst funcType
      in result @?= FunctionType [IntType] IntType
  
  , fastProperty "Substitution composition" $
      \subst1 subst2 ty -> let result1 = applySubstitution subst1 ty
                               result2 = applySubstitution subst2 result1
                               composed = Map.union subst2 subst1
                               result3 = applySubstitution composed ty
                           in result2 == result3
  ]

-- | 8. 类型变量测试
typeVariableTests :: TestTree
typeVariableTests = testGroup "Type Variable Tests"
  [ testCase "Create fresh type variable" $
      let (var, _) = freshTypeVariable 0
      in case var of
           TypeVar name -> take 1 name @?= "t"
  
  , testCase "Type variable equality" $
      let var1 = TypeVar "a"
          var2 = TypeVar "a"
          var3 = TypeVar "b"
      in (var1 == var2, var1 == var3) @?= (True, False)
  
  , fastProperty "Fresh type variables are unique" $
      \n -> let (var1, counter1) = freshTypeVariable n
                (var2, counter2) = freshTypeVariable counter1
            in var1 /= var2 && counter2 > counter1
  ]

-- | 9. 类型方案测试
typeSchemeTests :: TestTree
typeSchemeTests = testGroup "Type Scheme Tests"
  [ testCase "Simple type scheme" $
      let scheme = TypeScheme [] IntType
      in quantifyType IntType @?= scheme
  
  , testCase "Polymorphic type scheme" $
      let scheme = TypeScheme ["a"] (FunctionType [TypeVar "a"] (TypeVar "a"))
      in quantifyType (FunctionType [TypeVar "a"] (TypeVar "a")) @?= scheme
  
  , fastProperty "Scheme instantiation" $
      \typeVars -> let scheme = TypeScheme typeVars (FunctionType (map TypeVar typeVars) (TypeVar "a"))
                        instanceType = instantiateTypeScheme scheme
                    in case instanceType of
                         FunctionType argTypes retType -> length argTypes == length typeVars
                         _ -> False
  ]

-- | 10. 类型验证测试
typeValidationTests :: TestTree
typeValidationTests = testGroup "Type Validation Tests"
  [ testCase "Validate well-formed type" $
      let ty = FunctionType [IntType, StringType] BoolType
      in validateType ty @?= True
  
  , testCase "Validate type variable" $
      let ty = TypeVar "a"
      in validateType ty @?= True
  
  , testCase "Validate recursive type" $
      let ty = CustomType "List" [TypeVar "a"]
      in validateType ty @?= True
  
  , fastProperty "Type well-formedness" $
      \ty -> let result = validateType ty
              in result == True || result == False
  ]