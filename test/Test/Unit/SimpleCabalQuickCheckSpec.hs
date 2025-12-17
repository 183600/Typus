{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.SimpleCabalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.List (sort)

tests :: TestTree
tests = testGroup "Simple Cabal QuickCheck Tests"
  [ basicListProperties
  , basicArithmeticProperties
  ]

basicListProperties :: TestTree
basicListProperties = testGroup "Basic List Properties"
  [ fastProperty "reverse twice is identity" $ \(xs :: [Int]) ->
      reverse (reverse xs) === xs
  
  , fastProperty "length is preserved by reverse" $ \(xs :: [Int]) ->
      length (reverse xs) === length xs
  
  , fastProperty "sort is idempotent" $ \(xs :: [Int]) ->
      sort (sort xs) === sort xs
  ]

basicArithmeticProperties :: TestTree
basicArithmeticProperties = testGroup "Basic Arithmetic Properties"
  [ fastProperty "addition is commutative" $ \(x :: Int) (y :: Int) ->
      x + y === y + x
  
  , fastProperty "multiplication is associative" $ \(x :: Int) (y :: Int) (z :: Int) ->
      (x * y) * z === x * (y * z)
  
  , fastProperty "zero is additive identity" $ \(x :: Int) ->
      x + 0 === x
  ]
