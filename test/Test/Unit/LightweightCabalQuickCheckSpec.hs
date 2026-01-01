{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.LightweightCabalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.Char (toLower, toUpper)

tests :: TestTree
tests = testGroup "Lightweight Cabal QuickCheck Tests"
  [ stringProperties
  , booleanProperties
  ]

stringProperties :: TestTree
stringProperties = testGroup "String Properties"
  [ fastProperty "toLower is idempotent" $ \c ->
      toLower (toLower c) === toLower c
  
  , fastProperty "toUpper is idempotent" $ \c ->
      toUpper (toUpper c) === toUpper c
  
  , fastProperty "L.length of L.concat" $ \(xs :: String) (ys :: String) ->
      L.length (xs ++ ys) === L.length xs + L.length ys
  ]

booleanProperties :: TestTree
booleanProperties = testGroup "Boolean Properties"
  [ fastProperty "double negation" $ \b ->
      not (not b) === b
  
  , fastProperty "L.and is commutative" $ \b1 b2 ->
      (b1 && b2) === (b2 && b1)
  
  , fastProperty "L.or is commutative" $ \b1 b2 ->
      (b1 || b2) === (b2 || b1)
  ]
