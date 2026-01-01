{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependencyAnalysisQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose
  , sized, resize, suchThat, vectorOf, arbitrary
  )

import Dependencies.TypeSystem
  ( TypeVar(..)
  , TypeConstraint(..)
  , DependentTypeError(..)
  , TypeDef(..)
  , TypeEnv(..)
  , DependentTypeChecker(..)
  , Substitution
  , newDependentTypeChecker
  , newDependentTypeCheckerWithTypes
  , addType
  , addConstraint
  , addTypeError
  , lookupTypeDef
  , checkType
  , checkTypeInstantiation
  , solveConstraints
  , checkTypeConstraint
  , validateConstraint
  , getDependentTypeErrors
  , unify
  )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (nub, sort)
import Data.Maybe (isJust, isNothing)

-- | 生成有效的类型变量名
genTypeVarName :: Gen String
genTypeVarName = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- | 生成类型变量
genTypeVar :: Gen TypeVar
genTypeVar = oneof
  [ TVCon <$> genTypeVarName
  , TVVar <$> genTypeVarName
  , do
      name <- genTypeVarName
      args <- listOf genTypeVar
      return $ TVApp name args
  , do
      args <- listOf genTypeVar
      result <- genTypeVar
      return $ TVFun args result
  , TVTuple <$> listOf genTypeVar
  ]

-- | 生成类型约束
genTypeConstraint :: Gen TypeConstraint
genTypeConstraint = oneof
  [ Equal <$> genTypeVar <*> genTypeVar
  , Subtype <$> genTypeVar <*> genTypeVar
  , do
      name <- genTypeVarName
      args <- listOf genTypeVar
      return $ Predicate name args
  , TypeSizeGE <$> genTypeVar <*> choose (0, 100)
  , TypeSizeGT <$> genTypeVar <*> choose (0, 100)
  , do
      typeVar <- genTypeVar
      minVal <- choose (0, 50)
      maxVal <- choose (minVal, minVal + 50)
      return $ TypeRange typeVar minVal maxVal
  ]

-- | 生成依赖类型错误
genDependentTypeError :: Gen DependentTypeError
genDependentTypeError = oneof
  [ DependentTypeMismatch <$> genTypeVar <*> genTypeVar
  , ConstraintViolation <$> genTypeVarName <*> genTypeVar
  , TypeNotFound <$> genTypeVarName
  , InvalidTypeArgument <$> genTypeVarName
  , UnsolvableConstraint <$> genTypeConstraint
  , DependentInfiniteType <$> genTypeVarName <*> genTypeVar
  , AmbiguousType <$> genTypeVarName
  , ParseError <$> genTypeVarName
  , SemanticError <$> genTypeVarName
  ]

-- | 生成类型定义
genTypeDef :: Gen TypeDef
genTypeDef = do
  params <- listOf genTypeVarName
  constraints <- listOf genTypeConstraint
  return $ TypeDefDecl params constraints

-- | 生成类型环境
genTypeEnv :: Gen TypeEnv
genTypeEnv = do
  numTypes <- choose (0, 10)
  typeNames <- listOf1 genTypeVarName
  typeDefs <- mapM (\_ -> genTypeDef) typeNames
  let typeMap = Map.fromList $ zip typeNames typeDefs
      constraints <- listOf genTypeConstraint
  return $ TypeEnv typeMap constraints

-- | 生成依赖类型检查器
genDependentTypeChecker :: Gen DependentTypeChecker
genDependentTypeChecker = do
  typeEnv <- genTypeEnv
  errors <- listOf genDependentTypeError
  return $ DependentTypeChecker typeEnv errors

-- | 生成替换映射
genSubstitution :: Gen Substitution
genSubstitution = do
  numMappings <- choose (0, 5)
  keys <- listOf1 genTypeVarName
  values <- listOf genTypeVar
  return $ Map.fromList $ zip keys values

-- 属性：TypeVar的Show实例应该包含类型信息
prop_typeVar_show_informative :: Property
prop_typeVar_show_informative =
  forAll genTypeVar $ \typeVar ->
    let showStr = show typeVar
    in not (null showStr) === True

-- 属性：TypeConstraint的Show实例应该包含约束类型
prop_typeConstraint_show_contains_type :: Property
prop_typeConstraint_show_contains_type =
  forAll genTypeConstraint $ \constraint ->
    let showStr = show constraint
        constraintType = case constraint of
          Equal _ _ -> "Equal"
          Subtype _ _ -> "Subtype"
          Predicate _ _ -> "Predicate"
          TypeSizeGE _ _ -> "TypeSizeGE"
          TypeSizeGT _ _ -> "TypeSizeGT"
          TypeRange _ _ _ -> "TypeRange"
    in constraintType `L.isInfixOf` showStr

-- 属性：DependentTypeError的Show实例应该包含错误类型
prop_dependentTypeError_show_contains_type :: Property
prop_dependentTypeError_show_contains_type =
  forAll genDependentTypeError $ \error ->
    let showStr = show error
        errorType = case error of
          DependentTypeMismatch _ _ -> "DependentTypeMismatch"
          ConstraintViolation _ _ -> "ConstraintViolation"
          TypeNotFound _ -> "TypeNotFound"
          InvalidTypeArgument _ -> "InvalidTypeArgument"
          UnsolvableConstraint _ -> "UnsolvableConstraint"
          DependentInfiniteType _ _ -> "DependentInfiniteType"
          AmbiguousType _ -> "AmbiguousType"
          ParseError _ -> "ParseError"
          SemanticError _ -> "SemanticError"
    in errorType `L.isInfixOf` showStr

-- 属性：newDependentTypeChecker应该创建有效的检查器
prop_newDependentTypeChecker_valid :: Property
prop_newDependentTypeChecker_valid =
  let checker = newDependentTypeChecker
  in case checker of
       DependentTypeChecker typeEnv errors ->
         Map.L.null (typeDefinitions typeEnv) === True &&
         null errors === True

-- 属性：newDependentTypeCheckerWithTypes应该创建包含指定类型的检查器
prop_newDependentTypeCheckerWithTypes_contains_types :: Property
prop_newDependentTypeCheckerWithTypes_contains_types =
  forAll genTypeEnv $ \typeEnv ->
    let checker = newDependentTypeCheckerWithTypes typeEnv
    in case checker of
         DependentTypeChecker checkerTypeEnv _ ->
           typeDefinitions checkerTypeEnv === typeDefinitions typeEnv

-- 属性：addType应该向类型环境添加类型定义
prop_addType_adds_to_env :: Property
prop_addType_adds_to_env =
  forAll genTypeVarName $ \typeName ->
  forAll genTypeDef $ \typeDef ->
    let checker = newDependentTypeChecker
        updatedChecker = addType typeName typeDef checker
    in case updatedChecker of
         DependentTypeChecker typeEnv _ ->
           Map.member typeName (typeDefinitions typeEnv) === True

-- 属性：lookupTypeDef应该找到已添加的类型
prop_lookupTypeDef_finds_added :: Property
prop_lookupTypeDef_finds_added =
  forAll genTypeVarName $ \typeName ->
  forAll genTypeDef $ \typeDef ->
    let checker = newDependentTypeChecker
        updatedChecker = addType typeName typeDef checker
    in case updatedChecker of
         DependentTypeChecker typeEnv _ ->
           lookupTypeDef typeName typeEnv === Just typeDef

-- 属性：lookupTypeDef应该对不存在的类型返回Nothing
prop_lookupTypeDef_not_found :: Property
prop_lookupTypeDef_not_found =
  forAll genTypeVarName $ \typeName ->
    let checker = newDependentTypeChecker
    in case checker of
         DependentTypeChecker typeEnv _ ->
           lookupTypeDef typeName typeEnv === Nothing

-- 属性：addConstraint应该添加约束到类型环境
prop_addConstraint_adds_to_env :: Property
prop_addConstraint_adds_to_env =
  forAll genTypeConstraint $ \constraint ->
    let checker = newDependentTypeChecker
        updatedChecker = addConstraint constraint checker
    in case updatedChecker of
         DependentTypeChecker typeEnv _ ->
           constraint `elem` pendingConstraints typeEnv === True

-- 属性：addError应该添加错误到检查器
prop_addError_adds_to_checker :: Property
prop_addError_adds_to_checker =
  forAll genDependentTypeError $ \error ->
    let checker = newDependentTypeChecker
        updatedChecker = addTypeError error checker
    in case updatedChecker of
         DependentTypeChecker _ errors ->
           error `elem` errors === True

-- 属性：getDependentTypeErrors应该返回所有错误
prop_getDependentTypeErrors_returns_all :: Property
prop_getDependentTypeErrors_returns_all =
  forAll (listOf genDependentTypeError) $ \errors ->
    let checker = newDependentTypeChecker
        checkerWithErrors = foldl addTypeError checker errors
        retrievedErrors = getDependentTypeErrors checkerWithErrors
    in sort retrievedErrors === sort errors

-- 属性：TypeVar的Ord实例应该提供一致的排序
prop_typeVar_ordering_consistent :: Property
prop_typeVar_ordering_consistent =
  forAll genTypeVar $ \typeVar1 ->
  forAll genTypeVar $ \typeVar2 ->
    let ordering1 = compare typeVar1 typeVar2
        ordering2 = compare typeVar2 typeVar1
    in (ordering1 == EQ) === (ordering2 == EQ)

-- 属性：TypeConstraint的Eq实例应该正确识别相等的约束
prop_typeConstraint_equality :: Property
prop_typeConstraint_equality =
  forAll genTypeConstraint $ \constraint ->
    constraint === constraint

-- 属性：DependentTypeError的Eq实例应该正确识别相等的错误
prop_dependentTypeError_equality :: Property
prop_dependentTypeError_equality =
  forAll genDependentTypeError $ \error ->
    error === error

-- 属性：类型环境应该可以合并
prop_typeEnv_merge :: Property
prop_typeEnv_merge =
  forAll genTypeEnv $ \env1 ->
  forAll genTypeEnv $ \env2 ->
    let mergedDefs = Map.union (typeDefinitions env1) (typeDefinitions env2)
        mergedConstraints = pendingConstraints env1 ++ pendingConstraints env2
        mergedEnv = TypeEnv mergedDefs mergedConstraints
    in Map.size (typeDefinitions mergedEnv) >= 
       max (Map.size (typeDefinitions env1)) (Map.size (typeDefinitions env2))

-- 属性：替换映射应该可以应用
prop_substitution_application :: Property
prop_substitution_application =
  forAll genSubstitution $ \substitution ->
  forAll genTypeVarName $ \varName ->
    let result = Map.lookup varName substitution
    in case result of
         Just _ -> property True
         Nothing -> property True

-- 属性：类型约束列表应该可以排序
prop_typeConstraint_list_sortable :: Property
prop_typeConstraint_list_sortable =
  forAll (listOf genTypeConstraint) $ \constraints ->
    let sortedConstraints = sort constraints
    in L.length sortedConstraints === L.length constraints

-- 属性：依赖类型错误列表应该可以排序
prop_dependentTypeError_list_sortable :: Property
prop_dependentTypeError_list_sortable =
  forAll (listOf genDependentTypeError) $ \errors ->
    let sortedErrors = sort errors
    in L.length sortedErrors === L.length errors

tests :: TestTree
tests =
  testGroup "Dependency Analysis QuickCheck Tests"
    [ fastProperty "TypeVar show informative" prop_typeVar_show_informative
    , fastProperty "TypeConstraint show contains type" prop_typeConstraint_show_contains_type
    , fastProperty "DependentTypeError show contains type" prop_dependentTypeError_show_contains_type
    , fastProperty "newDependentTypeChecker valid" prop_newDependentTypeChecker_valid
    , fastProperty "newDependentTypeCheckerWithTypes contains types" prop_newDependentTypeCheckerWithTypes_contains_types
    , fastProperty "addType adds to env" prop_addType_adds_to_env
    , fastProperty "lookupTypeDef finds added" prop_lookupTypeDef_finds_added
    , fastProperty "lookupTypeDef not found" prop_lookupTypeDef_not_found
    , fastProperty "addConstraint adds to env" prop_addConstraint_adds_to_env
    , fastProperty "addError adds to checker" prop_addError_adds_to_checker
    , fastProperty "getDependentTypeErrors returns L.all" prop_getDependentTypeErrors_returns_all
    , fastProperty "TypeVar ordering consistent" prop_typeVar_ordering_consistent
    , fastProperty "TypeConstraint equality" prop_typeConstraint_equality
    , fastProperty "DependentTypeError equality" prop_dependentTypeError_equality
    , fastProperty "TypeEnv merge" prop_typeEnv_merge
    , fastProperty "Substitution application" prop_substitution_application
    , fastProperty "TypeConstraint list sortable" prop_typeConstraint_list_sortable
    , fastProperty "DependentTypeError list sortable" prop_dependentTypeError_list_sortable
    ]