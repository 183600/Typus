{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CompactCabalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.Either (isLeft, isRight, either)

tests :: TestTree
tests = testGroup "Compact Cabal QuickCheck Tests"
  [ eitherProperties
  , comparisonProperties
  ]

eitherProperties :: TestTree
eitherProperties = testGroup "Either Properties"
  [ fastProperty "isLeft and isRight are opposites" $ \(e :: Either Int String) ->
      isLeft e === not (isRight e)
  
  , fastProperty "either with Left" $ \(x :: Int) ->
      either id (const 0) (Left x :: Either Int String) === x
  
  , fastProperty "either with Right" $ \(y :: String) ->
      either (const "") id (Right y :: Either Int String) === y
  ]

comparisonProperties :: TestTree
comparisonProperties = testGroup "Comparison Properties"
  [ fastProperty "max is commutative" $ \(x :: Int) (y :: Int) ->
      max x y === max y x
  
  , fastProperty "min is commutative" $ \(x :: Int) (y :: Int) ->
      min x y === min y x
  
  , fastProperty "max and min relationship" $ \(x :: Int) (y :: Int) ->
      max x y + min x y === x + y
  ]
