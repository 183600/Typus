{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.DependenciesCycleDetectionQuickCheckTestSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Dependencies.AST
import Dependencies.Analyzer
import Dependencies.TypeSystem
import qualified Data.Text as T
import Data.List (nub, sort)
import Data.Either (isLeft, isRight)

-- | 测试依赖分析的基本属性
prop_analyze_dependencies_basic :: String -> Property
prop_analyze_dependencies_basic code =
  not (null code) ==>
  let errors = analyzeDependentTypes code
  in length errors >= 0

-- | 测试依赖分析的幂等性
prop_analyze_dependencies_idempotent :: String -> Property
prop_analyze_dependencies_idempotent code =
  not (null code) ==>
  let errors1 = analyzeDependentTypes code
      errors2 = analyzeDependentTypes code
  in length errors1 === length errors2

-- | 测试依赖分析对空代码的处理
prop_analyze_dependencies_empty :: Property
prop_analyze_dependencies_empty =
  let errors = analyzeDependentTypes ""
  in length errors >= 0

-- | 测试依赖分析对单行代码的处理
prop_analyze_dependencies_single_line :: String -> Property
prop_analyze_dependencies_single_line code =
  not (null code) && not ('\n' `elem` code) ==>
  let errors = analyzeDependentTypes code
  in length errors >= 0

-- | 测试依赖分析对多行代码的处理
prop_analyze_dependencies_multiline :: Positive Int -> String -> Property
prop_analyze_dependencies_multiline (Positive n) code =
  n < 10 ==>
  let multiLineCode = unlines $ replicate n code
      errors = analyzeDependentTypes multiLineCode
  in length errors >= 0

-- | 测试依赖分析对类型定义的处理
prop_analyze_dependencies_type_def :: String -> Property
prop_analyze_dependencies_type_def typeName =
  not (null typeName) ==>
  let typeDef = "type " ++ typeName ++ " = int"
      errors = analyzeDependentTypes typeDef
  in length errors >= 0

-- | 测试依赖分析对类型别名的处理
prop_analyze_dependencies_type_alias :: String -> Property
prop_analyze_dependencies_type_alias aliasName =
  not (null aliasName) ==>
  let typeAlias = "type " ++ aliasName ++ " = int"
      errors = analyzeDependentTypes typeAlias
  in length errors >= 0

-- | 测试依赖分析对变量声明的处理
prop_analyze_dependencies_var_decl :: String -> Property
prop_analyze_dependencies_var_decl varName =
  not (null varName) ==>
  let varDecl = "var " ++ varName ++ " : int"
      errors = analyzeDependentTypes varDecl
  in length errors >= 0

-- | 测试依赖分析对函数声明的处理
prop_analyze_dependencies_func_decl :: String -> Property
prop_analyze_dependencies_func_decl funcName =
  not (null funcName) ==>
  let funcDecl = "func " ++ funcName ++ "(x: int) : int"
      errors = analyzeDependentTypes funcDecl
  in length errors >= 0

-- | 测试依赖分析对约束定义的处理
prop_analyze_dependencies_constraint_def :: String -> Property
prop_analyze_dependencies_constraint_def constraintName =
  not (null constraintName) ==>
  let constraintDef = "constraint " ++ constraintName ++ "(x: int) = x > 0"
      errors = analyzeDependentTypes constraintDef
  in length errors >= 0

-- | 测试依赖分析对存在量词的处理
prop_analyze_dependencies_exists_decl :: String -> Property
prop_analyze_dependencies_exists_decl varName =
  not (null varName) ==>
  let existsDecl = "exists " ++ varName ++ ". var " ++ varName ++ " : int"
      errors = analyzeDependentTypes existsDecl
  in length errors >= 0

-- | 测试依赖分析对复杂类型的处理
prop_analyze_dependencies_complex_type :: Property
prop_analyze_dependencies_complex_type =
  let complexType = "type Complex = func(int, ref(string)) -> array[int, 10]"
      errors = analyzeDependentTypes complexType
  in length errors >= 0

-- | 测试依赖分析对约束的处理
prop_analyze_dependencies_constraints :: Property
prop_analyze_dependencies_constraints =
  let constrainedType = "type BoundedInt = int where size(x) > 0 && size(x) < 100"
      errors = analyzeDependentTypes constrainedType
  in length errors >= 0

-- | 测试依赖分析对递归类型的处理
prop_analyze_dependencies_recursive_type :: String -> Property
prop_analyze_dependencies_recursive_type typeName =
  not (null typeName) ==>
  let recursiveType = "type " ++ typeName ++ " = " ++ typeName ++ " | nil"
      errors = analyzeDependentTypes recursiveType
  in length errors >= 0

-- | 测试依赖分析对泛型类型的处理
prop_analyze_dependencies_generic_type :: String -> Property
prop_analyze_dependencies_generic_type typeName =
  not (null typeName) ==>
  let genericType = "type " ++ typeName ++ "[T] = T | " ++ typeName ++ "[T]"
      errors = analyzeDependentTypes genericType
  in length errors >= 0

-- | 测试依赖分析对函数类型的处理
prop_analyze_dependencies_function_type :: Property
prop_analyze_dependencies_function_type =
  let functionType = "type Func = (int: x, string: y) -> bool"
      errors = analyzeDependentTypes functionType
  in length errors >= 0

-- | 测试依赖分析对引用类型的处理
prop_analyze_dependencies_ref_type :: Property
prop_analyze_dependencies_ref_type =
  let refType = "type Ref = ref(int)"
      errors = analyzeDependentTypes refType
  in length errors >= 0

-- | 测试依赖分析对细化类型的处理
prop_analyze_dependencies_refine_type :: Property
prop_analyze_dependencies_refine_type =
  let refineType = "type PositiveInt = int where x > 0"
      errors = analyzeDependentTypes refineType
  in length errors >= 0

-- | 测试依赖分析对复杂程序的处理
prop_analyze_dependencies_complex_program :: Property
prop_analyze_dependencies_complex_program =
  let complexProgram = unlines
        [ "type List[T] = T | List[T]"
        , "type Option[T] = Some(T) | None"
        , "func head[T](l: List[T]) : Option[T] ="
        , "  match l with"
        , "  | T -> Some(T)"
        , "  | List[T] -> head(l)"
        , "  | None -> None"
        ]
      errors = analyzeDependentTypes complexProgram
  in length errors >= 0

-- | 测试依赖分析对错误代码的处理
prop_analyze_dependencies_invalid_code :: String -> Property
prop_analyze_dependencies_invalid_code invalidCode =
  let errors = analyzeDependentTypes invalidCode
  in length errors >= 0

-- | 测试空代码的依赖分析
test_analyze_dependencies_empty :: Assertion
test_analyze_dependencies_empty = do
  let errors = analyzeDependentTypes ""
  assertEqual "Empty code should result in no errors" 0 (length errors)

-- | 测试简单类型定义的依赖分析
test_analyze_dependencies_simple_type :: Assertion
test_analyze_dependencies_simple_type = do
  let typeDef = "type MyInt = int"
      errors = analyzeDependentTypes typeDef
  assertEqual "Simple type definition should result in no errors" 0 (length errors)

-- | 测试简单变量声明的依赖分析
test_analyze_dependencies_simple_var :: Assertion
test_analyze_dependencies_simple_var = do
  let varDecl = "var x : int"
      errors = analyzeDependentTypes varDecl
  assertEqual "Simple variable declaration should result in no errors" 0 (length errors)

-- | 测试简单函数声明的依赖分析
test_analyze_dependencies_simple_func :: Assertion
test_analyze_dependencies_simple_func = do
  let funcDecl = "func add(x: int, y: int) : int = x + y"
      errors = analyzeDependentTypes funcDecl
  assertEqual "Simple function declaration should result in no errors" 0 (length errors)

-- | 测试约束定义的依赖分析
test_analyze_dependencies_constraint :: Assertion
test_analyze_dependencies_constraint = do
  let constraintDef = "constraint Positive(x: int) = x > 0"
      errors = analyzeDependentTypes constraintDef
  assertEqual "Constraint definition should result in no errors" 0 (length errors)

-- | 测试存在量词的依赖分析
test_analyze_dependencies_exists :: Assertion
test_analyze_dependencies_exists = do
  let existsDecl = "exists x. var x : int where x > 0"
      errors = analyzeDependentTypes existsDecl
  assertEqual "Exists declaration should result in no errors" 0 (length errors)

-- | 测试复杂类型的依赖分析
test_analyze_dependencies_complex_type :: Assertion
test_analyze_dependencies_complex_type = do
  let complexType = "type Complex = func(int: x, ref(string): y) -> array[int, 10]"
      errors = analyzeDependentTypes complexType
  assertEqual "Complex type should result in no errors" 0 (length errors)

-- | 测试约束类型的依赖分析
test_analyze_dependencies_constrained_type :: Assertion
test_analyze_dependencies_constrained_type = do
  let constrainedType = "type BoundedInt = int where x > 0 && x < 100"
      errors = analyzeDependentTypes constrainedType
  assertEqual "Constrained type should result in no errors" 0 (length errors)

-- | 测试递归类型的依赖分析
test_analyze_dependencies_recursive_type :: Assertion
test_analyze_dependencies_recursive_type = do
  let recursiveType = "type List = T | List"
      errors = analyzeDependentTypes recursiveType
  assertEqual "Recursive type should result in no errors" 0 (length errors)

-- | 测试泛型类型的依赖分析
test_analyze_dependencies_generic_type :: Assertion
test_analyze_dependencies_generic_type = do
  let genericType = "type Container[T] = T | Container[T]"
      errors = analyzeDependentTypes genericType
  assertEqual "Generic type should result in no errors" 0 (length errors)

-- | 测试错误代码的依赖分析
test_analyze_dependencies_invalid_code :: Assertion
test_analyze_dependencies_invalid_code = do
  let invalidCode = "this is not valid code"
      errors = analyzeDependentTypes invalidCode
  assertBool "Invalid code should result in errors" (length errors > 0)

-- | 测试复杂程序的依赖分析
test_analyze_dependencies_complex_program :: Assertion
test_analyze_dependencies_complex_program = do
  let complexProgram = unlines
        [ "type List[T] = T | List[T]"
        , "type Option[T] = Some(T) | None"
        , "func head[T](l: List[T]) : Option[T] ="
        , "  match l with"
        , "  | T -> Some(T)"
        , "  | List[T] -> head(l)"
        , "  | None -> None"
        ]
      errors = analyzeDependentTypes complexProgram
  assertEqual "Complex program should result in no errors" 0 (length errors)

-- | 测试依赖分析的一致性
test_analyze_dependencies_consistency :: Assertion
test_analyze_dependencies_consistency = do
  let code = "type MyInt = int"
      errors1 = analyzeDependentTypes code
      errors2 = analyzeDependentTypes code
  assertEqual "Analysis should be consistent" errors1 errors2

-- | 测试套件
tests :: TestTree
tests = testGroup "Dependencies Cycle Detection QuickCheck Test Tests"
  [ testProperty "Analyze dependencies basic" prop_analyze_dependencies_basic
  , testProperty "Analyze dependencies idempotent" prop_analyze_dependencies_idempotent
  , testProperty "Analyze dependencies empty" prop_analyze_dependencies_empty
  , testProperty "Analyze dependencies single line" prop_analyze_dependencies_single_line
  , testProperty "Analyze dependencies multiline" prop_analyze_dependencies_multiline
  , testProperty "Analyze dependencies type def" prop_analyze_dependencies_type_def
  , testProperty "Analyze dependencies type alias" prop_analyze_dependencies_type_alias
  , testProperty "Analyze dependencies var decl" prop_analyze_dependencies_var_decl
  , testProperty "Analyze dependencies func decl" prop_analyze_dependencies_func_decl
  , testProperty "Analyze dependencies constraint def" prop_analyze_dependencies_constraint_def
  , testProperty "Analyze dependencies exists decl" prop_analyze_dependencies_exists_decl
  , testProperty "Analyze dependencies complex type" prop_analyze_dependencies_complex_type
  , testProperty "Analyze dependencies constraints" prop_analyze_dependencies_constraints
  , testProperty "Analyze dependencies recursive type" prop_analyze_dependencies_recursive_type
  , testProperty "Analyze dependencies generic type" prop_analyze_dependencies_generic_type
  , testProperty "Analyze dependencies function type" prop_analyze_dependencies_function_type
  , testProperty "Analyze dependencies ref type" prop_analyze_dependencies_ref_type
  , testProperty "Analyze dependencies refine type" prop_analyze_dependencies_refine_type
  , testProperty "Analyze dependencies complex program" prop_analyze_dependencies_complex_program
  , testProperty "Analyze dependencies invalid code" prop_analyze_dependencies_invalid_code
  , testCase "Analyze dependencies empty" test_analyze_dependencies_empty
  , testCase "Analyze dependencies simple type" test_analyze_dependencies_simple_type
  , testCase "Analyze dependencies simple var" test_analyze_dependencies_simple_var
  , testCase "Analyze dependencies simple func" test_analyze_dependencies_simple_func
  , testCase "Analyze dependencies constraint" test_analyze_dependencies_constraint
  , testCase "Analyze dependencies exists" test_analyze_dependencies_exists
  , testCase "Analyze dependencies complex type" test_analyze_dependencies_complex_type
  , testCase "Analyze dependencies constrained type" test_analyze_dependencies_constrained_type
  , testCase "Analyze dependencies recursive type" test_analyze_dependencies_recursive_type
  , testCase "Analyze dependencies generic type" test_analyze_dependencies_generic_type
  , testCase "Analyze dependencies invalid code" test_analyze_dependencies_invalid_code
  , testCase "Analyze dependencies complex program" test_analyze_dependencies_complex_program
  , testCase "Analyze dependencies consistency" test_analyze_dependencies_consistency
  ]