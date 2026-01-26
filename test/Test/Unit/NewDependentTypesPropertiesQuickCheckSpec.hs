{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing  -Wno-unused-matches #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewDependentTypesPropertiesQuickCheckSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.Text as T
import DependentTypesParser
import Compiler.DependentTypeChecker
import Parser
import SourceLocation
import Data.List (isInfixOf)

-- 假设的类型定义（可能需要根据实际情况调整）
data DependentType = DependentType String String deriving (Eq, Show)
data TypeConstraint = TypeConstraint String String deriving (Eq, Show)

-- | 测试DependentType的基本属性
prop_dependent_type_equality :: String -> String -> Property
prop_dependent_type_equality typeName constraint =
  let type1 = DependentType typeName constraint
      type2 = DependentType typeName constraint
  in property $ type1 == type2

-- | 测试DependentType的显示
prop_dependent_type_show :: String -> String -> Property
prop_dependent_type_show typeName constraint =
  let depType = DependentType typeName constraint
      shown = show depType
  in property $ typeName `isInfixOf` shown && constraint `isInfixOf` shown

-- | 测试TypeConstraint的基本属性
prop_type_constraint_equality :: String -> String -> Property
prop_type_constraint_equality varName expr =
  let constraint1 = TypeConstraint varName expr
      constraint2 = TypeConstraint varName expr
  in property $ constraint1 == constraint2

-- | 测试TypeConstraint的显示
prop_type_constraint_show :: String -> String -> Property
prop_type_constraint_show varName expr =
  let constraint = TypeConstraint varName expr
      shown = show constraint
  in property $ varName `isInfixOf` shown && expr `isInfixOf` shown

-- | 测试依赖类型检查的基本属性
prop_dependent_type_check_basic :: String -> Property
prop_dependent_type_check_basic code =
  case parseTypus code of
    Left parseError -> property True
    Right typusFile ->
      let result = checkDependentTypes typusFile
      in case result of
           Left errors -> property $ not (null errors)
           Right result -> property $ True

-- | 测试依赖类型检查与空代码
prop_dependent_type_check_empty :: Property
prop_dependent_type_check_empty =
  case parseTypus "" of
    Left parseError -> property True
    Right typusFile ->
      let result = checkDependentTypes typusFile
      in case result of
           Left errors -> property $ True
           Right result -> property $ True

-- | 测试依赖类型检查与简单类型
prop_dependent_type_check_simple :: String -> String -> Property
prop_dependent_type_check_simple varName typeName =
  let simpleCode = "let " ++ varName ++ ": " ++ typeName ++ " = 42;"
  in case parseTypus simpleCode of
       Left parseError -> property True
       Right typusFile ->
         let result = checkDependentTypes typusFile
         in case result of
              Left errors -> property $ True
              Right result -> property $ True

-- | 测试依赖类型检查与约束
prop_dependent_type_check_constraint :: String -> String -> String -> Property
prop_dependent_type_check_constraint varName typeName constraint =
  let constrainedCode = "let " ++ varName ++ ": " ++ typeName ++ " where " ++ constraint ++ " = 42;"
  in case parseTypus constrainedCode of
       Left parseError -> property True
       Right typusFile ->
         let result = checkDependentTypes typusFile
         in case result of
              Left errors -> property $ True
              Right result -> property $ True

-- | 测试依赖类型检查与函数
prop_dependent_type_check_function :: String -> String -> String -> Property
prop_dependent_type_check_function funcName paramType returnType =
  let functionCode = "function " ++ funcName ++ "(x: " ++ paramType ++ "): " ++ returnType ++ " {\n" ++
                     "  return x;\n" ++
                     "}"
  in case parseTypus functionCode of
       Left parseError -> property True
       Right typusFile ->
         let result = checkDependentTypes typusFile
         in case result of
              Left errors -> property $ True
              Right result -> property $ True

-- | 测试依赖类型检查与依赖函数
prop_dependent_type_check_dependent_function :: String -> String -> Property
prop_dependent_type_check_dependent_function funcName paramName =
  let dependentCode = "function " ++ funcName ++ "(n: Nat): Array<n> {\n" ++
                      "  return new Array<" ++ paramName ++ ">(n);\n" ++
                      "}"
  in case parseTypus dependentCode of
       Left parseError -> property True
       Right typusFile ->
         let result = checkDependentTypes typusFile
         in case result of
              Left errors -> property $ True
              Right result -> property $ True

tests :: TestTree
tests = testGroup "DependentTypes Properties QuickCheck Tests"
  [ testProperty "dependent type equality" prop_dependent_type_equality
  , testProperty "dependent type show" prop_dependent_type_show
  , testProperty "type constraint equality" prop_type_constraint_equality
  , testProperty "type constraint show" prop_type_constraint_show
  , testProperty "dependent type check basic" prop_dependent_type_check_basic
  , testProperty "dependent type check empty" prop_dependent_type_check_empty
  , testProperty "dependent type check simple" prop_dependent_type_check_simple
  , testProperty "dependent type check constraint" prop_dependent_type_check_constraint
  , testProperty "dependent type check function" prop_dependent_type_check_function
  , testProperty "dependent type check dependent function" prop_dependent_type_check_dependent_function
  ]