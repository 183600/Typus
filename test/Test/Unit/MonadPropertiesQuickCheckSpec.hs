{-# LANGUAGE CPP #-}

module Test.Unit.MonadPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck

prop_maybe_left_identity :: Int -> Fun Int (Maybe Int) -> Property
prop_maybe_left_identity x (Fun _ f) =
  (return x >>= f) === f x

prop_maybe_right_identity :: Maybe Int -> Property
prop_maybe_right_identity m =
  (m >>= return) === m

prop_maybe_associativity :: Maybe Int -> Fun Int (Maybe Int) -> Fun Int (Maybe Int) -> Property
prop_maybe_associativity m (Fun _ f) (Fun _ g) =
  ((m >>= f) >>= g) === (m >>= (\x -> f x >>= g))

prop_list_left_identity :: Int -> Fun Int [Int] -> Property
prop_list_left_identity x (Fun _ f) =
  (return x >>= f) === f x

prop_list_right_identity :: [Int] -> Property
prop_list_right_identity xs =
  (xs >>= return) === xs

prop_either_fmap :: Fun Int Int -> Either String Int -> Property
prop_either_fL.map (Fun _ f) e =
  fmap f e === (e >>= return . f)

tests :: TestTree
tests = testGroup "Monad Properties QuickCheck"
  [ fastProperty "Maybe left identity" prop_maybe_left_identity
  , fastProperty "Maybe right identity" prop_maybe_right_identity
  , fastProperty "Maybe associativity" prop_maybe_associativity
  , fastProperty "List left identity" prop_list_left_identity
  , fastProperty "List right identity" prop_list_right_identity
  , fastProperty "Either fmap law" prop_either_fmap
  ]
