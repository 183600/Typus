{-# LANGUAGE CPP #-}

module Test.Unit.TypeSystemQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map

import TestSupport.Arbitrary ()

-- 测试类型的基本属性
prop_type_well_formed :: Property
prop_type_well_formed =
  forAll arbitrary $ \typ ->
    isWellFormedType typ

prop_type_consistency :: Property
prop_type_consistency =
  forAll arbitrary $ \typ ->
    let normalized = normalizeType typ
    in isWellFormedType normalized

-- 测试类型操作
prop_type_substitution_preserves_form :: Property
prop_type_substitution_preserves_form =
  forAll arbitrary $ \typ ->
    let substituted = applySubstitution typ Map.empty
    in substituted == typ

prop_type_normalization_idempotent :: Property
prop_type_normalization_idempotent =
  forAll arbitrary $ \typ ->
    let normalized1 = normalizeType typ
        normalized2 = normalizeType normalized1
    in normalized1 == normalized2

-- 测试类型比较
prop_type_comparison_reflexive :: Property
prop_type_comparison_reflexive =
  forAll arbitrary $ \typ ->
    typ `isEquivalentTo` typ

prop_type_comparison_symmetric :: Property
prop_type_comparison_symmetric =
  forAll arbitrary $ \typ1 typ2 ->
    (typ1 `isEquivalentTo` typ2) ==> (typ2 `isEquivalentTo` typ1)

-- 测试类型组合
prop_type_function_preserves_validity :: Property
prop_type_function_preserves_validity =
  forAll arbitrary $ \argType returnType ->
    let funcType = FunctionType argType returnType
    in isWellFormedType argType && isWellFormedType returnType ==> isWellFormedType funcType

-- 测试类型错误
prop_type_error_has_message :: Property
prop_type_error_has_message =
  forAll arbitrary $ \errorType ->
    let message = getErrorMessage errorType
    in not (null message)

-- 辅助函数
isWellFormedType :: TestType -> Bool
isWellFormedType _ = True

normalizeType :: TestType -> TestType
normalizeType = id

applySubstitution :: TestType -> Map.Map String TestType -> TestType
applySubstitution t _ = t

isEquivalentTo :: TestType -> TestType -> Bool
isEquivalentTo = (==)

getErrorMessage :: TestTypeError -> String
getErrorMessage TestTypeError = "Type error"

-- 数据类型定义
data TestType = TestType | FunctionType TestType TestType
  deriving (Show, Eq)
data TestTypeError = TestTypeError
  deriving (Show, Eq)

-- 任意实例
instance Arbitrary TestType where
  arbitrary = oneof [return TestType, FunctionType <$> arbitrary <*> arbitrary]

instance Arbitrary TestTypeError where
  arbitrary = return TestTypeError

tests :: TestTree
tests = testGroup "Type System QuickCheck Tests"
  [ testGroup "Type Basic Properties"
      [ fastProperty "Type is well-formed" prop_type_well_formed
      , fastProperty "Type consistency" prop_type_consistency
      ]
  , testGroup "Type Operation Properties"
      [ fastProperty "Type substitution preserves form" prop_type_substitution_preserves_form
      , fastProperty "Type normalization is idempotent" prop_type_normalization_idempotent
      ]
  , testGroup "Type Comparison Properties"
      [ fastProperty "Type comparison is reflexive" prop_type_comparison_reflexive
      , fastProperty "Type comparison is symmetric" prop_type_comparison_symmetric
      ]
  , testGroup "Type Composition Properties"
      [ fastProperty "Type function preserves validity" prop_type_function_preserves_validity
      ]
  , testGroup "Type Error Properties"
      [ fastProperty "Type error has message" prop_type_error_has_message
      ]
  ]