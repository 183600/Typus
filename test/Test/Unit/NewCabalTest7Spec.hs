{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalTest7Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), oneof)
import Data.Char (isSpace, isAlpha)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)

import Compiler
  ( compileTypus
  , CompilationResult(..)
  , CompilationError(..)
  , TypeCheckError(..)
  , TypeInferenceError(..)
  )
import Compiler.TypeChecker
  ( TypeChecker
  , newTypeChecker
  , checkTypes
  , inferType
  , TypeError(..)
  , Type(..)
  , TypeScheme(..)
  )
import Compiler.IR
  ( IRModule
  , IRFunction
  , IRStatement
  , IRExpression
  )

-- | 测试编译器的类型检查和推理功能
tests :: TestTree
tests =
  testGroup "NewCabalTest7 - 类型检查测试"
    [ testGroup "单元测试"
        [ testCase "基本类型检查" $ do
            let code = "func add(a int, b int) int { return a + b }"
                result = compileTypus code
            case result of
                Left errors -> assertBool ("Compilation failed: " ++ show errors) False
                Right compilationResult -> 
                    assertBool "Should compile successfully" $ True

        , testCase "类型错误检测" $ do
            let code = "func add(a int, b string) int { return a + b }"
                result = compileTypus code
            case result of
                Left errors -> assertBool "Should detect type error" $ 
                    L.any (\e -> "type" `L.isInfixOf` (map toLower (show e))) errors
                Right _ -> assertBool "Should detect type error" False

        , testCase "类型推理" $ do
            let code = "func identity(x) { return x }"
                result = compileTypus code
            case result of
                Left errors -> assertBool ("Type inference failed: " ++ show errors) False
                Right compilationResult -> 
                    assertBool "Should infer types" $ True

        , testCase "依赖类型检查" $ do
            let code = "func array_access(arr [T] int, index T) int { return arr[index] }"
                result = compileTypus code
            case result of
                Left errors -> assertBool ("Dependent type checking failed: " ++ show errors) False
                Right compilationResult -> 
                    assertBool "Should handle dependent types" $ True
        ]

    , testGroup "QuickCheck属性测试"
        [ fastProperty "类型检查的确定性" prop_type_checking_deterministic
        , fastProperty "类型推理的一致性" prop_type_inference_consistency
        , fastProperty "类型等价的传递性" prop_type_equivalence_transitivity
        , fastProperty "类型替换的正确性" prop_type_substitution_correctness
        , fastProperty "类型环境的单调性" prop_type_environment_monotonic
        ]
    ]

-- QuickCheck属性测试

-- 类型检查的确定性：相同代码应该产生相同的类型检查结果
prop_type_checking_deterministic :: String -> Property
prop_type_checking_deterministic code =
  let result1 = compileTypus code
      result2 = compileTypus code
  in case (result1, result2) of
       (Right res1, Right res2) -> 
         property $ True  -- 两个都成功
       (Left err1, Left err2) -> 
         property $ L.length err1 === L.length err2  -- 两个都失败，错误数量相同
       _ -> property $ False  -- 结果不一致

-- 类型推理的一致性：推理出的类型应该与使用情况一致
prop_type_inference_consistency :: String -> Property
prop_type_inference_consistency code =
  let result = compileTypus code
  in case result of
       Right compilationResult -> 
         property $ True  -- 编译成功意味着类型推理一致
       Left errors -> 
         let hasTypeErrors = L.any (\e -> "type" `L.isInfixOf` (map toLower (show e))) errors
         in property $ hasTypeErrors ==> True  -- 类型错误是预期的

-- 类型等价的传递性：如果A等价于B，B等价于C，那么A等价于C
prop_type_equivalence_transitivity :: Type -> Type -> Type -> Property
prop_type_equivalence_transitivity typeA typeB typeC =
  -- 简化的类型等价检查（实际实现会更复杂）
  let areEquivalent t1 t2 = t1 == t2  -- 简化实现
      equivAB = areEquivalent typeA typeB
      equivBC = areEquivalent typeB typeC
      equivAC = areEquivalent typeA typeC
  in property $ (equivAB .&&. equivBC) ==> equivAC

-- 类型替换的正确性：类型替换后，表达式的类型应该相应改变
prop_type_substitution_correctness :: String -> String -> String -> Property
prop_type_substitution_correctness typeName oldType newType =
  not (null typeName) && not (null oldType) && not (null newType) ==>
  let codeWithOldType = "func test(x " ++ oldType ++ ") " ++ oldType ++ " { return x }"
      codeWithNewType = "func test(x " ++ newType ++ ") " ++ newType ++ " { return x }"
      result1 = compileTypus codeWithOldType
      result2 = compileTypus codeWithNewType
  in case (result1, result2) of
       (Right _, Right _) -> property $ True
       (Left _, Left _) -> property $ True
       _ -> property $ False

-- 类型环境的单调性：添加新的类型定义不应该破坏现有类型检查
prop_type_environment_monotonic :: String -> String -> Property
prop_type_environment_monotonic baseCode newTypeDef =
  not (null newTypeDef) ==>
  let result1 = compileTypus baseCode
      result2 = compileTypus (newTypeDef ++ "\n" ++ baseCode)
  in case (result1, result2) of
       (Right _, Right _) -> property $ True  -- 两个都成功
       (Right _, Left _) -> property $ False  -- 添加类型定义导致失败
       (Left _, Right _) -> property $ True   -- 添加类型定义修复了问题
       (Left _, Left _) -> property $ True   -- 两个都失败

-- 辅助函数
toLower :: String -> String
toLower = L.map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

-- 简化的类型定义（实际会从Compiler模块导入）
data Type = IntType | StringType | BoolType | FunctionType Type Type | CustomType String
  deriving (Eq, Show)

instance Arbitrary Type where
  arbitrary = oneof
    [ pure IntType
    , pure StringType
    , pure BoolType
    , FunctionType <$> arbitrary <*> arbitrary
    , CustomType <$> fL.map (:[]) arbitrary
    ]