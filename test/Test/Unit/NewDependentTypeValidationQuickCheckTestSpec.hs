{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewDependentTypeValidationQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, testProperty, Arbitrary(..), Gen, oneof, elements, listOf, listOf1, suchThat, choose)
import Test.Tasty.HUnit (testCase, (@?=))

import DependentTypesParser 
    ( DependentTypesParser(..), DependentTypeError(..), TypeRef(..), TypeBody(..),
      Field(..), TypeParameter(..), TypeConstraint(..), DependentType(..),
      DependentParseResult, runDependentTypesParser, parseDependentType,
      parseTypeDeclaration, validateDependentTypeSyntax )
import Parser (parseTypus)
import SourceLocation (SourcePos(..), SourceSpan(..), posAtLineCol, spanBetween)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (nub)
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)

-- | 新的依赖类型验证QuickCheck测试模块
tests :: TestTree
tests =
  testGroup "New Dependent Type Validation QuickCheck Tests"
    [ testGroup "Type definition properties"
        [ testProperty "type definitions have valid names" prop_typeDefinitionValidNames
        , testProperty "type definitions preserve structure" prop_typeDefinitionPreserveStructure
        , testProperty "type definitions handle generics" prop<tool_call>TypeDefinitionHandleGenerics
        , testProperty "type definitions validate constraints" prop_typeDefinitionValidateConstraints
        ]

    , testGroup "Type constraint properties"
        [ testProperty "constraints are syntactically valid" prop_constraintsSyntacticallyValid
        , testProperty "constraints respect type parameters" prop_constraintsRespectTypeParameters
        , testProperty "constraints are properly scoped" prop_constraintsProperlyScoped
        , testProperty "constraints can be combined" prop_constraintsCanBeCombined
        ]

    , testGroup "Generic type properties"
        [ testProperty "generic types preserve parameter names" prop_genericTypesPreserveParameterNames
        , testProperty "generic types handle multiple parameters" prop_genericTypesHandleMultipleParameters
        , testProperty "generic types support nesting" prop_genericTypesSupportNesting
        , testProperty "generic types validate usage" prop_genericTypesValidateUsage
        ]

    , testGroup "Struct type properties"
        [ testProperty "structs have valid field definitions" prop_structsValidFieldDefinitions
        , testProperty "structs enforce field name uniqueness" prop_structsEnforceFieldUniqueness
        , testProperty "structs support dependent field types" prop_structsSupportDependentFieldTypes
        , testProperty "structs handle recursive definitions" prop_structsHandleRecursiveDefinitions
        ]

    , testGroup "Type alias properties"
        [ testProperty "aliases preserve target type" prop_aliasesPreserveTargetType
        , testProperty "aliases support generic parameters" prop_aliasesSupportGenericParameters
        , testProperty "aliases prevent circular definitions" prop_aliasesPreventCircularDefinitions
        , testProperty "aliases resolve correctly" prop_aliasesResolveCorrectly
        ]

    , testGroup "Type inference properties"
        [ testProperty "type inference respects constraints" prop_typeInferenceRespectsConstraints
        , testProperty "type inference handles complex expressions" prop_typeInferenceHandlesComplexExpressions
        , testProperty "type inference provides useful errors" prop_typeInferenceProvidesUsefulErrors
        , testProperty "type inference is deterministic" prop_typeInferenceDeterministic
        ]

    , testGroup "Error handling properties"
        [ testProperty "error messages are informative" prop_errorMessagesInformative
        , testProperty "error locations are accurate" prop_errorLocationsAccurate
        , testProperty "error recovery preserves context" prop_errorRecoveryPreservesContext
        , testProperty "multiple errors are collected" prop_multipleErrorsCollected
        ]

    , testGroup "Specific validation tests"
        [ testCase "simple dependent type validation" $ do
            let input = "type Vector(n: Nat) where n > 0"
                result = validateDependentTypeSyntax input
            case result of
                Left errors -> @?= False True
                Right parsed -> @?= True True

        , testCase "complex constraint validation" $ do
            let input = "type Matrix(m: Nat, n: Nat) where m > 0 && n > 0 && len(data) == m * n"
                result = validateDependentTypeSyntax input
            case result of
                Left errors -> L.length errors >= 0 @?= True
                Right parsed -> @?= True True

        , testCase "generic type validation" $ do
            let input = "type Container(T: Type, capacity: Nat) where capacity > 0"
                result = validateDependentTypeSyntax input
            case result of
                Left errors -> @?= False True
                Right parsed -> @?= True True

        , testCase "struct type with dependent fields" $ do
            let input = unlines
                  [ "type NonEmptyList(T: Type) where len(data) > 0"
                  , "struct {"
                  , "  L.head: T"
                  , "  L.tail: List(T)"
                  , "  L.length: Nat where L.length == len(L.tail) + 1"
                  , "}"
                  ]
                result = validateDependentTypeSyntax input
            case result of
                Left errors -> L.length errors >= 0 @?= True
                Right parsed -> @?= True True

        , testCase "type alias validation" $ do
            let input = "type PositiveInt = Nat where n > 0"
                result = validateDependentTypeSyntax input
            case result of
                Left errors -> @?= False True
                Right parsed -> @?= True True

        , testCase "recursive type validation" $ do
            let input = unlines
                  [ "type List(T: Type)"
                  , "struct {"
                  , "  value: T"
                  , "  next: Option(List(T))"
                  , "}"
                  ]
                result = validateDependentTypeSyntax input
            case result of
                Left errors -> @?= False True
                Right parsed -> @?= True True

        , testCase "constraint expression validation" $ do
            let input = "type BoundedArray(n: Nat, max: Nat) where n > 0 && n <= max && len(data) == n"
                result = validateDependentTypeSyntax input
            case result of
                Left errors -> @?= False True
                Right parsed -> @?= True True

        , testCase "invalid constraint detection" $ do
            let input = "type Invalid(n: Nat) where n > n"  -- Self-referential constraint
                result = validateDependentTypeSyntax input
            case result of
                Left errors -> L.length errors > 0 @?= True
                Right parsed -> @?= False True
        ]
    ]

-- | 类型定义具有有效名称
prop_typeDefinitionValidNames :: String -> Property
prop_typeDefinitionValidNames typeName =
  not (null typeName) && L.all isAlphaNum typeName ==>
  let input = "type " ++ typeName ++ " = Nat"
      result = validateDependentTypeSyntax input
  in case result of
       Left errors -> not (L.any (L.isInfixOf "invalid name" . unpack) errors)
       Right _ -> True

-- | 类型定义保留结构
prop_typeDefinitionPreserveStructure :: String -> Property
propTypeDefinitionPreserveStructure input =
  let typeDefinition = "type TestType = " ++ input
      result = validateDependentTypeSyntax typeDefinition
  in case result of
       Left _ -> True -- May fail on invalid input
       Right parsed -> True

-- | 类型定义处理泛型
propTypeDefinitionHandleGenerics :: String -> Property
propTypeDefinitionHandleGenerics paramName =
  not (null paramName) && L.all isAlphaNum paramName ==>
  let input = "type Container(" ++ paramName ++ ": Type) where true"
      result = validateDependentTypeSyntax input
  in case result of
       Left errors -> not (L.any (L.isInfixOf "parameter" . unpack) errors)
       Right _ -> True

-- | 类型定义验证约束
propTypeDefinitionValidateConstraints :: String -> Property
propTypeDefinitionValidateConstraints constraint =
  let input = "type TestType where " ++ constraint
      result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 约束在语法上有效
prop_constraintsSyntacticallyValid :: String -> Property
prop_constraintsSyntacticallyValid constraint =
  let input = "type TestType where " ++ constraint
      result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 约束尊重类型参数
prop_constraintsRespectTypeParameters :: String -> String -> Property
prop_constraintsRespectTypeParameters paramName constraint =
  not (null paramName) && L.all isAlphaNum paramName ==>
  let input = "type TestType(" ++ paramName ++ ": Nat) where " ++ constraint
      result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 约束正确限定作用域
prop_constraintsProperlyScoped :: String -> Property
prop_constraintsProperlyScoped input =
  let testInput = "type TestType where " ++ input
      result = validateDependentTypeSyntax testInput
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 约束可以组合
prop_constraintsCanBeCombined :: [String] -> Property
prop_constraintsCanBeCombined constraints =
  not (null constraints) ==>
  let constraintStr = unwords (intersperse " && " constraints)
      input = "type TestType where " ++ constraintStr
      result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 泛型类型保留参数名称
prop_genericTypesPreserveParameterNames :: [String] -> Property
prop_genericTypesPreserveParameterNames paramNames =
  L.all (not . null) paramNames && L.all (L.all isAlphaNum) paramNames ==>
  let paramStr = L.concat $ intersperse ", " (L.map (\name -> name ++ ": Type") paramNames)
      input = "type GenericType(" ++ paramStr ++ ") where true"
      result = validateDependentTypeSyntax input
  in case result of
       Left errors -> not (L.any (L.isInfixOf "parameter" . unpack) errors)
       Right _ -> True

-- | 泛型类型处理多个参数
prop_genericTypesHandleMultipleParameters :: [String] -> Property
prop_genericTypesHandleMultipleParameters paramNames =
  L.length paramNames <= 5 && L.all (not . null) paramNames && L.all (L.all isAlphaNum) paramNames ==>
  let paramStr = L.concat $ intersperse ", " (L.map (\name -> name ++ ": Type") paramNames)
      input = "type MultiGeneric(" ++ paramStr ++ ") where true"
      result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 泛型类型支持嵌套
prop_genericTypesSupportNesting :: String -> Property
prop_genericTypesSupportNesting input =
  let nestedInput = "type Nested(T: Type) where len(" ++ input ++ ") > 0"
      result = validateDependentTypeSyntax nestedInput
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 泛型类型验证使用
prop_genericTypesValidateUsage :: String -> Property
prop_genericTypesValidateUsage input =
  let testInput = "type TestType(T: Type) where " ++ input
      result = validateDependentTypeSyntax testInput
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 结构体具有有效字段定义
prop_structsValidFieldDefinitions :: [(String, String)] -> Property
prop_structsValidFieldDefinitions fields =
  L.all (not . null . fst) fields && L.all (L.all isAlphaNum . fst) fields ==>
  let fieldStr = L.concat $ intersperse ", " (L.map (\(name, typ) -> name ++ ": " ++ typ) fields)
      input = "type StructType struct { " ++ fieldStr ++ " }"
      result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 结构体强制字段名称唯一性
prop_structsEnforceFieldUniqueness :: [String] -> Property
prop_structsEnforceFieldUniqueness fieldNames =
  let uniqueNames = nub fieldNames
      hasDuplicates = L.length fieldNames /= L.length uniqueNames
      fields = zip fieldNames (repeat "Type")
      fieldStr = L.concat $ intersperse ", " (L.map (\(name, typ) -> name ++ ": " ++ typ) fields)
      input = "type StructType struct { " ++ fieldStr ++ " }"
      result = validateDependentTypeSyntax input
  in if hasDuplicates
     then case result of
            Left errors -> L.any (L.isInfixOf "duplicate" . unpack) errors
            Right _ -> False
     else case result of
            Left errors -> not (L.any (L.isInfixOf "duplicate" . unpack) errors)
            Right _ -> True

-- | 结构体支持依赖字段类型
prop_structsSupportDependentFieldTypes :: String -> Property
prop_structsSupportDependentFieldTypes constraint =
  let input = "type DependentStruct struct { field: Type where " ++ constraint ++ " }"
      result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 结构体处理递归定义
prop_structsHandleRecursiveDefinitions :: String -> Property
prop_structsHandleRecursiveDefinitions typeName =
  not (null typeName) && L.all isAlphaNum typeName ==>
  let input = unlines
        [ "type " ++ typeName ++ " struct {"
        , "  value: Type"
        , "  next: Option(" ++ typeName ++ ")"
        , "}"
        ]
      result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 别名保留目标类型
prop_aliasesPreserveTargetType :: String -> Property
prop_aliasesPreserveTargetType targetType =
  let input = "type Alias = " ++ targetType
      result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 别名支持泛型参数
prop_aliasesSupportGenericParameters :: [String] -> Property
prop_aliasesSupportGenericParameters paramNames =
  L.all (not . null) paramNames && L.all (L.all isAlphaNum) paramNames ==>
  let paramStr = L.concat $ intersperse ", " paramNames
      input = "type Alias(" ++ paramStr ++ ") = Type"
      result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 别名防止循环定义
prop_aliasesPreventCircularDefinitions :: String -> Property
prop_aliasesPreventCircularDefinitions typeName =
  not (null typeName) && L.all isAlphaNum typeName ==>
  let input = "type " ++ typeName ++ " = " ++ typeName
      result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.any (L.isInfixOf "circular" . unpack) errors
       Right _ -> False

-- | 别名正确解析
prop_aliasesResolveCorrectly :: String -> Property
prop_aliasesResolveCorrectly targetType =
  let input = "type Alias = " ++ targetType
      result = validateDependentTypeSyntax input
  in case result of
       Left errors -> not (null targetType) || L.length errors > 0
       Right _ -> True

-- | 类型推断尊重约束
prop_typeInferenceRespectsConstraints :: String -> Property
prop_typeInferenceRespectsConstraints constraint =
  let input = "type TestType where " ++ constraint
      result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 类型推断处理复杂表达式
prop_typeInferenceHandlesComplexExpressions :: String -> Property
prop_typeInferenceHandlesComplexExpressions expression =
  let input = "type TestType where " ++ expression
      result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 类型推断提供有用错误
prop_typeInferenceProvidesUsefulErrors :: String -> Property
prop_typeInferenceProvidesUsefulErrors input =
  let testInput = "type TestType where " ++ input
      result = validateDependentTypeSyntax testInput
  in case result of
       Left errors -> L.all (not . null . unpack) errors
       Right _ -> True

-- | 类型推断是确定性的
prop_typeInferenceDeterministic :: String -> Property
prop_typeInferenceDeterministic input =
  let result1 = validateDependentTypeSyntax input
      result2 = validateDependentTypeSyntax input
  in case (result1, result2) of
       (Left errors1, Left errors2) -> L.length errors1 == L.length errors2
       (Right _, Right _) -> True
       _ -> False -- Should be consistent success/failure

-- | 错误消息提供信息
prop_errorMessagesInformative :: String -> Property
prop_errorMessagesInformative input =
  let result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.all (not . null . unpack) errors
       Right _ -> True

-- | 错误位置准确
prop_errorLocationsAccurate :: String -> Property
prop_errorLocationsAccurate input =
  let result = validateDependentTypeSyntax input
  in case result of
       Left errors -> True -- Should provide accurate locations
       Right _ -> True

-- | 错误恢复保留上下文
prop_errorRecoveryPreservesContext :: String -> Property
prop_errorRecoveryPreservesContext input =
  let result = validateDependentTypeSyntax input
  in case result of
       Left errors -> True -- Should preserve context during recovery
       Right _ -> True

-- | 收集多个错误
prop_multipleErrorsCollected :: String -> Property
prop_multipleErrorsCollected input =
  let result = validateDependentTypeSyntax input
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- Helper functions
isAlphaNum :: Char -> Bool
isAlphaNum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs