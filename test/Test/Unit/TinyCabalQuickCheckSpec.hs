{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.TinyCabalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck

tests :: TestTree
tests = testGroup "Tiny Cabal QuickCheck Tests"
  [ fundamentalProperties
  ]

fundamentalProperties :: TestTree
fundamentalProperties = testGroup "Fundamental Properties"
  [ fastProperty "identity function" $ \(x :: Int) ->
      id x === x
  
  , fastProperty "const function" $ \(x :: Int) (y :: String) ->
      const x y === x
  
  , fastProperty "flip function" $ \(x :: Int) (y :: String) ->
      let f = (,)
      in flip f x y === (y, x)
  
  , fastProperty "composition identity" $ \(x :: Int) ->
      (id . id) x === x
  ]
