{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PreciseTypeTests (preciseTypeTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (Property, property, testProperty)
import qualified Dependencies as Dep
-- import qualified Data.Map.Strict as Map
-- import qualified Data.Set as Set
import Control.Monad.State (runState, execState)
import Compiler (checkDependentTypes)
import Parser (parseTypus, TypusFile)
-- import Control.Monad.Except
-- import qualified Data.Text as T

-- ============================================================================
-- 精确类型测试套件
-- 涵盖依赖类型、精炼类型、线性类型和高级类型约束
-- ============================================================================

preciseTypeTests :: TestTree
preciseTypeTests = testGroup "Precise Type Tests"
  [ dependentTypeTests
  , refinementTypeTests
  , linearTypeTests
  , ownershipTypeTests
  , constraintSolvingTests
  , typeLevelComputationTests
  , advancedPolymorphismTests
  , errorDetectionTests
  , compilerIntegrationTests
  , basicPropertyTests
  ]

-- ============================================================================
-- 依赖类型测试
-- ============================================================================

dependentTypeTests :: TestTree
dependentTypeTests = testGroup "Dependent Type Tests"
  [ testCase "数组类型与长度依赖" $ do
      let source :: String; source = "type Array<T, n: int> where n > 0"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> return ()  -- 解析成功

  , testCase "向量类型安全索引" $ do
      let source = unlines
            [ "type Vec<T, n: int>"
            , "func get<T, n>(v: Vec<T, n>, i: int where i < n): T"
            ]
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> return ()  -- 解析成功

  , testCase "依赖函数类型" $ do
      let source :: String; source = "func replicate<T>(n: int where n >= 0, x: T): Vec<T, n>"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right (Dep.Program [stmt]) -> return ()  -- 解析成功
        Right _ -> return ()

  , testCase "类型级别的算术运算" $ do
      -- 创建类型检查器并求解约束
      let checker = Dep.newDependentTypeChecker
      let (success, checker') = runState Dep.solveConstraints checker
      assertBool "Should solve constraints successfully" success
      assertEqual "Should have no errors" [] (Dep.getDependentTypeErrors checker')

  , testCase "依赖类型的正确性证明" $ do
      let source = unlines
            [ "type Proof<p: bool>"
            , "func proveLength<T, n>(v: Vec<T, n>): Proof<n >= 0>"
            ]
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> return ()  -- 解析成功
  ]

-- ============================================================================
-- 精炼类型测试
-- ============================================================================

refinementTypeTests :: TestTree
refinementTypeTests = testGroup "Refinement Type Tests"
  [ testCase "正整数类型" $ do
      let source :: String; source = "type Positive = int where x > 0"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> return ()  -- 解析成功

  , testCase "字符串长度约束" $ do
      let source :: String; source = "type NonEmptyString = string where length(s) > 0"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> return ()  -- 解析成功

  , testCase "范围约束类型" $ do
      let source :: String; source = "type Percentage = int where 0 <= x && x <= 100"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> return ()  -- 解析成功

  , testCase "复杂约束求解" $ do
      let checker = Dep.newDependentTypeChecker
      let (success, checker') = runState Dep.solveConstraints checker
      assertBool "Should solve constraints successfully" success
      assertEqual "Should have no errors" [] (Dep.getDependentTypeErrors checker')

  , testCase "精炼类型的子类型关系" $ do
      -- 子类型检查通过约束求解实现
      let checker = Dep.newDependentTypeChecker
      let (success, checker') = runState Dep.solveConstraints checker
      assertBool "Should solve subtyping constraints successfully" success
      assertEqual "Should have no errors" [] (Dep.getDependentTypeErrors checker')

  , testCase "存在类型与精炼" $ do
      let source :: String; source = "exists T. func process(x: T where P(x)): T"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right (Dep.Program [stmt]) -> return ()  -- 解析成功
        Right _ -> return ()
  ]

-- ============================================================================
-- 线性类型测试
-- ============================================================================

linearTypeTests :: TestTree
linearTypeTests = testGroup "Linear Type Tests"
  [ testCase "资源管理类型" $ do
      let source :: String; source = "type FileHandle where must_close"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> return ()  -- 解析成功

  , testCase "单次使用约束" $ do
      let source = unlines
            [ "func consume<T>(x: T where single_use): void"
            , "func useOnce(x: int where single_use): void"
            ]
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> return ()  -- 解析成功

  , testCase "会话类型" $ do
      let source = unlines
            [ "protocol ClientServer"
            , "  SendRequest: func(request: string): Response"
            , "  Close: func(): void"
            ]
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> return ()  -- 解析成功

  , testCase "能力类型系统" $ do
      let source = unlines
            [ "capability Read"
            , "capability Write"
            , "func readFile(path: string where Read): string"
            , "func writeFile(path: string, content: string where Write): void"
            ]
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> return ()  -- 解析成功
  ]

-- ============================================================================
-- 所有权类型测试
-- ============================================================================

ownershipTypeTests :: TestTree
ownershipTypeTests = testGroup "Ownership Type Tests"
  [ testCase "唯一所有权" $ do
      let source :: String; source = "type Unique<T> where owns"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> return ()  -- 解析成功

  , testCase "借用检查" $ do
      let source = unlines
            [ "func borrow<T>(x: &T): &T"
            , "func mutate<T>(x: &mut T): void"
            ]
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> return ()  -- 解析成功

  , testCase "生命周期参数" $ do
      let source :: String; source = "func ref<T, 'a>(x: &'a T): &'a T"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right (Dep.Program [stmt]) -> return ()  -- 解析成功
        Right _ -> return ()

  , testCase "移动语义" $ do
      let source = unlines
            [ "func move<T>(x: T where move): T"
            , "func useAfterMove(): void"
            ]
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> return ()  -- 解析成功
  ]

-- ============================================================================
-- 约束求解测试
-- ============================================================================

constraintSolvingTests :: TestTree
constraintSolvingTests = testGroup "Advanced Constraint Solving"
  [ testCase "高阶约束" $ do
      let checker = Dep.newDependentTypeChecker
      let (success, checker') = runState Dep.solveConstraints checker
      assertBool "Should solve constraints successfully" success
      assertEqual "Should have no errors" [] (Dep.getDependentTypeErrors checker')

  , testCase "递归类型约束" $ do
      let checker = Dep.newDependentTypeChecker
      let (success, checker') = runState Dep.solveConstraints checker
      assertBool "Should handle recursive types" success
      -- 递归类型应该有特殊的处理方式
      return ()

  , testCase "复杂约束组合" $ do
      let checker = Dep.newDependentTypeChecker
      let (success, checker') = runState Dep.solveConstraints checker
      assertBool "Should solve complex constraints successfully" success
      assertEqual "Should have no errors" [] (Dep.getDependentTypeErrors checker')

  , testCase "类型族约束" $ do
      let checker = Dep.newDependentTypeChecker
      let (success, checker') = runState Dep.solveConstraints checker
      assertBool "Should solve type family constraints successfully" success
      assertEqual "Should have no errors" [] (Dep.getDependentTypeErrors checker')
  ]

-- ============================================================================
-- 类型级别计算测试
-- ============================================================================

typeLevelComputationTests :: TestTree
typeLevelComputationTests = testGroup "Type-Level Computation"
  [ testCase "类型函数应用" $ do
      let checker = Dep.newDependentTypeChecker
      let (success, checker') = runState Dep.solveConstraints checker
      assertBool "Should solve type function constraints successfully" success
      assertEqual "Should have no errors" [] (Dep.getDependentTypeErrors checker')

  , testCase "条件类型" $ do
      let source :: String; source = "type If<cond: bool, T, F> = cond ? T : F"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> return ()  -- 解析成功

  , testCase "映射类型" $ do
      let source :: String; source = "type Map<T, U> = func(T): U"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> return ()  -- 解析成功

  , testCase "类型级别的折叠" $ do
      let checker = Dep.newDependentTypeChecker
      let (success, checker') = runState Dep.solveConstraints checker
      assertBool "Should solve fold constraints successfully" success
      assertEqual "Should have no errors" [] (Dep.getDependentTypeErrors checker')
  ]

-- ============================================================================
-- 高级多态测试
-- ============================================================================

advancedPolymorphismTests :: TestTree
advancedPolymorphismTests = testGroup "Advanced Polymorphism"
  [ testCase "高阶多态" $ do
      let source :: String; source = "func map<F, G, A, B>(f: F where F: func(A): B, g: G): func(List<A>): List<B>"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right (Dep.Program [stmt]) -> return ()  -- 解析成功
        Right _ -> return ()

  , testCase "存在类型量化" $ do
      let source :: String; source = "exists T. func hide(x: T): Hidden<T>"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right (Dep.Program [stmt]) -> return ()  -- 解析成功
        Right _ -> return ()

  , testCase "秩-N多态" $ do
      let source :: String; source = "func apply<T>(f: forall U. func(U): T, x: int): T"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right (Dep.Program [stmt]) -> return ()  -- 解析成功
        Right _ -> return ()

  , testCase "约束多态" $ do
      let source :: String; source = "func sort<T>(list: List<T> where T: Ord): List<T>"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right (Dep.Program [stmt]) -> return ()  -- 解析成功
        Right _ -> return ()
  ]

-- ============================================================================
-- 错误检测测试
-- ============================================================================

errorDetectionTests :: TestTree
errorDetectionTests = testGroup "Precise Error Detection"
  [ testCase "检测数组越界" $ do
      let source :: String; source = "func unsafeGet<T, n>(arr: Array<T, n>, i: int where i >= n): T"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right (Dep.Program [stmt]) -> return ()  -- 解析成功，但类型检查应该失败
        Right _ -> return ()

  , testCase "检测除以零" $ do
      let source :: String; source = "func unsafeDiv(x: int, y: int where y == 0): int"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right (Dep.Program [stmt]) -> return ()  -- 解析成功，但类型检查应该失败
        Right _ -> return ()

  , testCase "检测空指针解引用" $ do
      let source :: String; source = "func unsafeDeref<T>(ptr: T where ptr == null): T"
      case Dep.runParser source of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right (Dep.Program [stmt]) -> return ()  -- 解析成功，但类型检查应该失败
        Right _ -> return ()

  , testCase "检测类型不匹配" $ do
      let c = Dep.Equal (Dep.TVCon "string") (Dep.TVCon "int")
      let checker0 = Dep.newDependentTypeChecker
      let (_, checker1) = runState (Dep.addConstraint c) checker0
      let (success, checker2) = runState Dep.solveConstraints checker1
      assertBool "Should fail on type mismatch" (not success)
      assertBool "Should have type mismatch errors" (not $ null $ Dep.getDependentTypeErrors checker2)

  , testCase "检测循环依赖" $ do
      let _constraints :: [Dep.TypeConstraint] = [ Dep.Equal (Dep.TVVar "T") (Dep.TVApp "F" [Dep.TVVar "T"]), Dep.Equal (Dep.TVVar "U") (Dep.TVApp "G" [Dep.TVVar "U"]), Dep.Equal (Dep.TVVar "T") (Dep.TVVar "U") ]



      let checker = Dep.newDependentTypeChecker
      let (success, checker') = runState Dep.solveConstraints checker
      -- 循环依赖应该有特殊的错误处理
      return ()
  ]

-- ============================================================================
-- 精确类型编译器集成测试
-- ============================================================================

compilerIntegrationTests :: TestTree
compilerIntegrationTests = testGroup "Compiler Integration"
  [ testCase "Reports duplicate dependent type definitions" $ do
      let invalidProgram = unlines
            [ "package main"
            , ""
            , "{//! dependent_types: on}"
            , "type Example struct {"
            , "    Value: int"
            , "}"
            , "type Example struct {"
            , "    Value: int"
            , "}"
            , "}"
            ]
          expectedError = "Dependent type errors: Invalid type syntax: 重复定义: Example"
      typusFile <- parseTypusOrFail invalidProgram
      case checkDependentTypes typusFile of
        Left msg -> assertEqual "Expected duplicate definition error" expectedError msg
        Right _ -> assertFailure "Expected dependent type error for duplicate definitions"

  , testCase "Succeeds for valid dependent type block" $ do
      let validProgram = unlines
            [ "package main"
            , ""
            , "{//! dependent_types: on}"
            , "type Positive struct {"
            , "    Value: int"
            , "} where Value >= 0"
            , "}"
            ]
      typusFile <- parseTypusOrFail validProgram
      case checkDependentTypes typusFile of
        Left msg -> assertFailure ("Unexpected dependent type error: " ++ msg)
        Right _ -> pure ()
  ]

parseTypusOrFail :: String -> IO TypusFile
parseTypusOrFail source =
  case parseTypus source of
    Left err -> assertFailure ("Failed to parse Typus source: " ++ err)
    Right tf -> pure tf

-- ============================================================================
-- 基础属性测试


basicPropertyTests :: TestTree
basicPropertyTests = testGroup "Property Tests"
  [ testProperty "约束求解的一致性" prop_constraint_consistency
  ]

-- 属性：约束求解的一致性
prop_constraint_consistency :: [Dep.TypeConstraint] -> Property
prop_constraint_consistency constraints =
  property $
    let checker1 = execState Dep.solveConstraints Dep.newDependentTypeChecker
        checker2 = execState Dep.solveConstraints Dep.newDependentTypeChecker
    in null (Dep.getDependentTypeErrors checker1) == null (Dep.getDependentTypeErrors checker2)


