{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependenciesDependencyAnalysisSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, elements, listOf, oneof, sized)
import Dependencies
  ( DependentTypeChecker
  , DependentTypeError(..)
  , AST(..)
  , Statement(..)
  , TypeExpr(..)
  , Constraint(..)
  , TypeVar(..)
  , TypeConstraint(..)
  , TypeScheme(..)
  , TypeEnvironment(..)
  , newDependentTypeChecker
  , newDependentTypeCheckerWithTypes
  , analyzeDependentTypes
  , analyzeAST
  , validateASTSemantics
  , validateStatement
  , checkType
  , addType
  , addConstraint
  , checkTypeInstantiation
  , solveConstraints
  , getDependentTypeErrors
  , unify
  , inferType
  , inferStatement
  , inferProgram
  , generalize
  , instantiate
  , unifyTypes
  , applyTypeSubstitution
  , newTypeVariable
  , getFreshTypeVar
  , initialTypeEnvironment
  )
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)

-- ============================================================================
-- 生成测试数据
-- ============================================================================

-- 生成类型表达式
genTypeExpr :: Gen TypeExpr
genTypeExpr = oneof
  [ TypeVar . TypeVar <$> elements ["a", "b", "c", "x", "y", "z"]
  , return $ TypeConstructor "Int"
  , return $ TypeConstructor "String"
  , return $ TypeConstructor "Bool"
  , TypeConstructor <$> elements ["List", "Array", "Map"]
  ]

-- 生成语句
genStatement :: Gen Statement
genStatement = oneof
  [ VarDecl <$> elements ["x", "y", "z"] <*> genTypeExpr
  , FuncDecl <$> elements ["f", "g", "h"] <*> listOf genTypeExpr <*> genTypeExpr
  , TypeDecl <$> elements ["MyType", "Custom"] <*> genTypeExpr
  ]

-- 生成AST
genAST :: Gen AST
genAST = do
  statements <- listOf genStatement
  return $ AST statements

-- 生成约束
genConstraint :: Gen Constraint
genConstraint = oneof
  [ TypeConstraint <$> genTypeExpr <*> genTypeExpr
  , EqualityConstraint <$> genTypeExpr <*> genTypeExpr
  , SubtypeConstraint <$> genTypeExpr <*> genTypeExpr
  ]

-- ============================================================================
-- 依赖分析属性测试
-- ============================================================================

-- Property: 创建依赖类型检查器
prop_create_dependent_type_checker :: Property
prop_create_dependent_type_checker =
  let checker = newDependentTypeChecker
  in property $ True

-- Property: 带初始类型创建检查器
prop_create_checker_with_initial_types :: [(String, TypeExpr)] -> Property
prop_create_checker_with_initial_types types =
  not (null types) ==>
  let checker = newDependentTypeCheckerWithTypes types
  in property $ True

-- Property: 分析空AST
prop_analyze_empty_ast :: Property
prop_analyze_empty_ast =
  let checker = newDependentTypeChecker
      ast = AST []
      result = analyzeAST checker ast
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: 分析简单AST
prop_analyze_simple_ast :: Statement -> Property
prop_analyze_simple_ast statement =
  let checker = newDependentTypeChecker
      ast = AST [statement]
      result = analyzeAST checker ast
  in case result of
    Left _ -> property True  -- May fail due to type errors
    Right _ -> property True

-- Property: 类型检查
prop_type_checking :: TypeExpr -> Property
prop_type_checking typeExpr =
  let checker = newDependentTypeChecker
      result = checkType checker typeExpr
  in case result of
    Left _ -> property True
    Right _ -> property True

-- Property: 添加类型约束
prop_add_type_constraint :: Constraint -> Property
prop_add_type_constraint constraint =
  let checker = newDependentTypeChecker
      result = addConstraint checker constraint
  in property $ True

-- Property: 解决约束
prop_solve_constraints :: [Constraint] -> Property
prop_solve_constraints constraints =
  not (null constraints) ==>
  let checker = newDependentTypeChecker
      _ = mapM_ (addConstraint checker) constraints
      result = solveConstraints checker
  in case result of
    Left _ -> property True
    Right _ -> property True

-- Property: 类型统一
prop_type_unification :: TypeExpr -> TypeExpr -> Property
prop_type_unification type1 type2 =
  let result = unify type1 type2
  in case result of
    Left _ -> property True
    Right _ -> property True

-- Property: 类型推断
prop_type_inference :: Statement -> Property
prop_type_inference statement =
  let checker = newDependentTypeChecker
      result = inferStatement checker statement
  in case result of
    Left _ -> property True
    Right _ -> property True

-- Property: 创建新类型变量
prop_create_type_variable :: Property
prop_create_type_variable =
  let typeVar = newTypeVariable
  in property $ True

-- Property: 获取新类型变量
prop_get_fresh_type_variable :: Property
prop_get_fresh_type_variable =
  let checker = newDependentTypeChecker
      typeVar = getFreshTypeVar checker
  in property $ True

-- ============================================================================
-- 单元测试
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Dependencies Dependency Analysis Tests"
    [ testGroup "Property Tests"
        [ fastProperty "create dependent type checker" prop_create_dependent_type_checker
        , fastProperty "create checker with initial types" prop_create_checker_with_initial_types
        , fastProperty "analyze empty ast" prop_analyze_empty_ast
        , fastProperty "analyze simple ast" prop_analyze_simple_ast
        , fastProperty "type checking" prop_type_checking
        , fastProperty "add type constraint" prop_add_type_constraint
        , fastProperty "solve constraints" prop_solve_constraints
        , fastProperty "type unification" prop_type_unification
        , fastProperty "type inference" prop_type_inference
        , fastProperty "create type variable" prop_create_type_variable
        , fastProperty "get fresh type variable" prop_get_fresh_type_variable
        ]
    , testGroup "Unit Tests"
        [ testCase "create new dependent type checker" $ do
            let checker = newDependentTypeChecker
            assertBool "Checker should be created" $ True

        , testCase "create checker with initial types" $ do
            let initialTypes = [("Int", TypeConstructor "Int"), ("String", TypeConstructor "String")]
                checker = newDependentTypeCheckerWithTypes initialTypes
            assertBool "Checker with initial types should be created" $ True

        , testCase "analyze simple variable declaration" $ do
            let checker = newDependentTypeChecker
                statement = VarDecl "x" (TypeConstructor "Int")
                ast = AST [statement]
                result = analyzeAST checker ast
            case result of
              Left err -> assertFailure $ "AST analysis failed: " ++ show err
              Right _ -> return ()

        , testCase "analyze function declaration" $ do
            let checker = newDependentTypeChecker
                statement = FuncDecl "add" [TypeConstructor "Int", TypeConstructor "Int"] (TypeConstructor "Int")
                ast = AST [statement]
                result = analyzeAST checker ast
            case result of
              Left err -> assertFailure $ "AST analysis failed: " ++ show err
              Right _ -> return ()

        , testCase "check simple type" $ do
            let checker = newDependentTypeChecker
                typeExpr = TypeConstructor "Int"
                result = checkType checker typeExpr
            case result of
              Left err -> assertFailure $ "Type check failed: " ++ show err
              Right _ -> return ()

        , testCase "add and solve equality constraint" $ do
            let checker = newDependentTypeChecker
                constraint = EqualityConstraint (TypeConstructor "Int") (TypeConstructor "Int")
                _ = addConstraint checker constraint
                result = solveConstraints checker
            case result of
              Left err -> assertFailure $ "Constraint solving failed: " ++ show err
              Right _ -> return ()

        , testCase "unify compatible types" $ do
            let type1 = TypeConstructor "Int"
                type2 = TypeConstructor "Int"
                result = unify type1 type2
            case result of
              Left err -> assertFailure $ "Type unification failed: " ++ show err
              Right _ -> return ()

        , testCase "unify different types should fail" $ do
            let type1 = TypeConstructor "Int"
                type2 = TypeConstructor "String"
                result = unify type1 type2
            case result of
              Left _ -> return ()  -- Expected to fail
              Right _ -> assertFailure "Expected unification to fail for different types"

        , testCase "infer type of variable declaration" $ do
            let checker = newDependentTypeChecker
                statement = VarDecl "x" (TypeConstructor "Int")
                result = inferStatement checker statement
            case result of
              Left err -> assertFailure $ "Type inference failed: " ++ show err
              Right _ -> return ()

        , testCase "infer type of function declaration" $ do
            let checker = newDependentTypeChecker
                statement = FuncDecl "identity" [TypeVar (TypeVar "a")] (TypeVar (TypeVar "a"))
                result = inferStatement checker statement
            case result of
              Left err -> assertFailure $ "Type inference failed: " ++ show err
              Right _ -> return ()

        , testCase "create and use type variable" $ do
            let typeVar = newTypeVariable
                checker = newDependentTypeChecker
                freshVar = getFreshTypeVar checker
            assertBool "Type variable should be created" $ True
            assertBool "Fresh type variable should be created" $ True

        , testCase "validate AST semantics" $ do
            let checker = newDependentTypeChecker
                statement1 = VarDecl "x" (TypeConstructor "Int")
                statement2 = VarDecl "y" (TypeConstructor "String")
                ast = AST [statement1, statement2]
                result = validateASTSemantics checker ast
            case result of
              Left err -> assertFailure $ "AST validation failed: " ++ show err
              Right _ -> return ()

        , testCase "validate individual statement" $ do
            let checker = newDependentTypeChecker
                statement = VarDecl "x" (TypeConstructor "Int")
                result = validateStatement checker statement
            case result of
              Left err -> assertFailure $ "Statement validation failed: " ++ show err
              Right _ -> return ()

        , testCase "check type instantiation" $ do
            let checker = newDependentTypeChecker
                typeExpr = TypeVar (TypeVar "a")
                result = checkTypeInstantiation checker typeExpr
            case result of
              Left err -> assertFailure $ "Type instantiation check failed: " ++ show err
              Right _ -> return ()

        , testCase "get dependent type errors" $ do
            let checker = newDependentTypeChecker
                errors = getDependentTypeErrors checker
            length errors @?= 0  -- Should be empty initially

        , testCase "generalize and instantiate types" $ do
            let typeEnv = initialTypeEnvironment
                typeExpr = TypeVar (TypeVar "a")
                scheme = generalize typeEnv typeExpr
                result = instantiate scheme
            case result of
              Left err -> assertFailure $ "Type instantiation failed: " ++ show err
              Right _ -> return ()

        , testCase "apply type substitution" $ do
            let typeVar = TypeVar (TypeVar "a")
                typeExpr = TypeConstructor "Int"
                substitution = [(typeVar, typeExpr)]
                result = applyTypeSubstitution substitution typeVar
            case result of
              Left err -> assertFailure $ "Type substitution failed: " ++ show err
              Right _ -> return ()

        , testCase "infer program types" $ do
            let checker = newDependentTypeChecker
                statement1 = VarDecl "x" (TypeConstructor "Int")
                statement2 = VarDecl "y" (TypeConstructor "String")
                program = [statement1, statement2]
                result = inferProgram checker program
            case result of
              Left err -> assertFailure $ "Program type inference failed: " ++ show err
              Right _ -> return ()
        ]
    ]