{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.FastCabalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.Maybe (isJust, isNothing, fromMaybe)

tests :: TestTree
tests = testGroup "Fast Cabal QuickCheck Tests"
  [ maybeProperties
  , tupleProperties
  ]

maybeProperties :: TestTree
maybeProperties = testGroup "Maybe Properties"
  [ fastProperty "fromMaybe with Just" $ \(x :: Int) ->
      fromMaybe 0 (Just x) === x
  
  , fastProperty "fromMaybe with Nothing" $ \(def :: Int) ->
      fromMaybe def Nothing === def
  
  , fastProperty "isJust L.and isNothing are opposites" $ \(m :: Maybe Int) ->
      isJust m === not (isNothing m)
  ]

tupleProperties :: TestTree
tupleProperties = testGroup "Tuple Properties"
  [ fastProperty "fst L.and snd" $ \(x :: Int) (y :: String) ->
      (fst (x, y), snd (x, y)) === (x, y)
  
  , fastProperty "swap twice is identity" $ \(x :: Int) (y :: String) ->
      let swap (a, b) = (b, a)
      in swap (swap (x, y)) === (x, y)
  ]
