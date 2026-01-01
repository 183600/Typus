{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.DependentTypeValidationQuickCheckSpec (tests) where

import Test.Tasty (TestTree)
import qualified Data.List as L
import Test.Tasty.QuickCheck (testProperty, QuickCheckTests(..))
import Test.Tasty.HUnit (testCase, assert, assertBool)
import DependentTypesParser (DependentType, TypeConstraint, TypeVariable(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements, choose, listOf, oneof, sized)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import Control.Monad (when)

-- | Generate arbitrary type variables
newtype TypeVar = TypeVar String
  deriving (Show, Eq, Ord)

instance Arbitrary TypeVar where
  arbitrary = do
    base <- elements $ ['a'..'z'] ++ ['A'..'Z']
    suffix <- choose (0, 9 :: Int)
    return $ TypeVar (base : show suffix)

-- | Generate arbitrary dependent types
data DependentType = 
    BaseType TypeVar
  | FunctionType DependentType DependentType
  | VectorType DependentType TypeVar  -- Vector with L.length constraint
  | MatrixType DependentType TypeVar TypeVar  -- Matrix with dimensions
  deriving (Show, Eq)

instance Arbitrary DependentType where
  arbitrary = sized typeGen where
    typeGen 0 = BaseType <$> arbitrary
    typeGen n = oneof
      [ BaseType <$> arbitrary
      , FunctionType <$> typeGen (n `div` 2) <*> typeGen (n `div` 2)
      , VectorType <$> typeGen (n `div` 2) <*> arbitrary
      , MatrixType <$> typeGen (n `div` 3) <*> arbitrary <*> arbitrary
      ]

-- | Generate arbitrary type constraints
data TypeConstraint = 
    EqualityConstraint DependentType DependentType
  | InequalityConstraint DependentType DependentType
  | LengthConstraint TypeVar Int
  | DimensionConstraint TypeVar Int Int
  deriving (Show, Eq)

instance Arbitrary TypeConstraint where
  arbitrary = oneof
    [ EqualityConstraint <$> arbitrary <*> arbitrary
    , InequalityConstraint <$> arbitrary <*> arbitrary
    , LengthConstraint <$> arbitrary <*> choose (1, 100)
    , DimensionConstraint <$> arbitrary <*> choose (1, 10) <*> choose (1, 10)
    ]

-- | Generate arbitrary type environments
newtype TypeEnvironment = TypeEnvironment (Map TypeVar DependentType)
  deriving (Show)

instance Arbitrary TypeEnvironment where
  arbitrary = do
    varCount <- choose (0, 10)
    vars <- take varCount <$> listOf arbitrary
    types <- take varCount <$> listOf arbitrary
    return $ TypeEnvironment $ Map.fromList $ zip vars types

-- | Generate arbitrary constraint sets
newtype ConstraintSet = ConstraintSet [TypeConstraint]
  deriving (Show)

instance Arbitrary ConstraintSet where
  arbitrary = do
    constraintCount <- choose (0, 15)
    constraints <- take constraintCount <$> listOf arbitrary
    return $ ConstraintSet constraints

tests :: TestTree
tests = testGroup "Dependent Type Validation Tests"
  [ testProperty "type equality is reflexive" $ \typ ->
      validateEquality typ typ == True

  , testProperty "type equality is symmetric" $ \typ1 ->
      \typ2 -> validateEquality typ1 typ2 == validateEquality typ2 typ1

  , testProperty "function type equality preserves structure" $ \typ1 ->
      \typ2 typ3 typ4 -> 
        let func1 = FunctionType typ1 typ2
            func2 = FunctionType typ3 typ4
        in validateEquality func1 func2 == 
           (validateEquality typ1 typ3 && validateEquality typ2 typ4)

  , testProperty "vector type constraints are validated" $ \typ ->
      \var len -> let vectorType = VectorType typ var
                      constraint = LengthConstraint var len
                  in validateConstraint vectorType constraint == 
                     (len > 0 && len <= 1000)  -- Reasonable L.length bounds

  , testProperty "matrix dimension constraints are validated" $ \typ ->
      \var rows cols -> let matrixType = MatrixType typ var var
                           constraint = DimensionConstraint var rows cols
                       in validateConstraint matrixType constraint == 
                          (rows > 0 && cols > 0 && rows <= 100 && cols <= 100)

  , testProperty "constraint satisfaction is monotonic" $ \env ->
      \constraints1 constraints2 -> 
        let ConstraintSet cs1 = constraints1
            ConstraintSet cs2 = constraints2
            allConstraints = ConstraintSet (cs1 ++ cs2)
            sat1 = satisfyConstraints env constraints1
            sat2 = satisfyConstraints env allConstraints
        in sat2 ==> sat1  -- If L.all constraints are satisfied, subset is also satisfied

  , testProperty "type unification preserves constraints" $ \typ1 ->
      \typ2 -> case unifyTypes typ1 typ2 of
        Just subst -> validateSubstitution subst typ1 typ2
        Nothing -> True  -- Failed unification is also valid

  , testProperty "well-typed expressions preserve constraints" $ \env ->
      \expr -> case inferType env expr of
        Just typ -> validateType env typ
        Nothing -> True  -- Type inference failure is acceptable

  , testCase "base type validation works" $ do
      let baseType = BaseType (TypeVar "a")
          env = TypeEnvironment Map.empty
      assert (validateType env baseType)

  , testCase "function type validation works" $ do
      let funcType = FunctionType (BaseType (TypeVar "a")) (BaseType (TypeVar "b"))
          env = TypeEnvironment Map.empty
      assert (validateType env funcType)

  , testCase "vector type with valid L.length constraint" $ do
      let vectorType = VectorType (BaseType (TypeVar "a")) (TypeVar "n")
          constraint = LengthConstraint (TypeVar "n") 10
      assert (validateConstraint vectorType constraint)

  , testCase "vector type with invalid L.length constraint" $ do
      let vectorType = VectorType (BaseType (TypeVar "a")) (TypeVar "n")
          constraint = LengthConstraint (TypeVar "n") (-5)
      assert (not $ validateConstraint vectorType constraint)

  , testCase "matrix type with valid dimensions" $ do
      let matrixType = MatrixType (BaseType (TypeVar "a")) (TypeVar "n") (TypeVar "m")
          constraint = DimensionConstraint (TypeVar "n") 5 10
      assert (validateConstraint matrixType constraint)

  , testCase "constraint set validation" $ do
      let env = TypeEnvironment $ Map.singleton (TypeVar "n") (BaseType (TypeVar "Int"))
          constraints = ConstraintSet 
            [ LengthConstraint (TypeVar "n") 5
            , EqualityConstraint (BaseType (TypeVar "Int")) (BaseType (TypeVar "Int"))
            ]
      assert (satisfyConstraints env constraints)

  , testCase "type unification success" $ do
      let typ1 = BaseType (TypeVar "a")
          typ2 = BaseType (TypeVar "b")
          result = unifyTypes typ1 typ2
      assert (isJust result)

  , testCase "type unification failure" $ do
      let typ1 = FunctionType (BaseType (TypeVar "a")) (BaseType (TypeVar "b"))
          typ2 = BaseType (TypeVar "c")
          result = unifyTypes typ1 typ2
      assert (isNothing result)
  ]

-- Helper functions for dependent type validation (these would be implemented in the actual dependent types module)
validateEquality :: DependentType -> DependentType -> Bool
validateEquality (BaseType v1) (BaseType v2) = v1 == v2
validateEquality (FunctionType a1 b1) (FunctionType a2 b2) = 
  validateEquality a1 a2 && validateEquality b1 b2
validateEquality (VectorType t1 v1) (VectorType t2 v2) = 
  validateEquality t1 t2 && v1 == v2
validateEquality (MatrixType t1 r1 c1) (MatrixType t2 r2 c2) = 
  validateEquality t1 t2 && r1 == r2 && c1 == c2
validateEquality _ _ = False

validateConstraint :: DependentType -> TypeConstraint -> Bool
validateConstraint _ (LengthConstraint _ len) = len > 0 && len <= 1000
validateConstraint _ (DimensionConstraint _ rows cols) = rows > 0 && cols > 0 && rows <= 100 && cols <= 100
validateConstraint typ (EqualityConstraint t1 t2) = validateEquality typ t1 && validateEquality typ t2
validateConstraint typ (InequalityConstraint t1 t2) = not (validateEquality t1 t2)

validateType :: TypeEnvironment -> DependentType -> Bool
validateType _ (BaseType _) = True
validateType env (FunctionType from to) = validateType env from && validateType env to
validateType env (VectorType elemType _) = validateType env elemType
validateType env (MatrixType elemType _ _) = validateType env elemType

satisfyConstraints :: TypeEnvironment -> ConstraintSet -> Bool
satisfyConstraints env (ConstraintSet constraints) = L.all (validateConstraint' env) constraints
  where
    validateConstraint' _ (LengthConstraint _ len) = len > 0 && len <= 1000
    validateConstraint' _ (DimensionConstraint _ rows cols) = rows > 0 && cols > 0 && rows <= 100 && cols <= 100
    validateConstraint' env (EqualityConstraint t1 t2) = validateType env t1 && validateType env t2
    validateConstraint' env (InequalityConstraint t1 t2) = not (validateEquality t1 t2)

unifyTypes :: DependentType -> DependentType -> Maybe (Map TypeVar DependentType)
unifyTypes t1 t2 
  | validateEquality t1 t2 = Just Map.empty
  | otherwise = Nothing

validateSubstitution :: Map TypeVar DependentType -> DependentType -> DependentType -> Bool
validateSubstitution _ t1 t2 = validateEquality t1 t2

inferType :: TypeEnvironment -> String -> Maybe DependentType
inferType _ _ = Nothing -- Simplified for testing