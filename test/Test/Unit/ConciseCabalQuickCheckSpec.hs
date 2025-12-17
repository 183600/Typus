{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.ConciseCabalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.Char (isLower, isUpper, toLower, toUpper)

tests :: TestTree
tests = testGroup "Concise Cabal QuickCheck Tests"
  [ characterProperties
  , functorProperties
  ]

characterProperties :: TestTree
characterProperties = testGroup "Character Properties"
  [ fastProperty "toLower makes lowercase" $ \c ->
      isLower c || not (isUpper (toLower c))
  
  , fastProperty "toUpper makes uppercase" $ \c ->
      isUpper c || not (isLower (toUpper c))
  
  , fastProperty "case conversion round trip" $ \c ->
      toUpper (toLower c) === toUpper c
  ]

functorProperties :: TestTree
functorProperties = testGroup "Functor Properties"
  [ fastProperty "fmap id is id for Maybe" $ \(m :: Maybe Int) ->
      fmap id m === m
  
  , fastProperty "fmap id is id for List" $ \(xs :: [Int]) ->
      fmap id xs === xs
  
  , fastProperty "fmap composition for Maybe" $ \(m :: Maybe Int) ->
      let f = (+1)
          g = (*2)
      in fmap (f . g) m === (fmap f . fmap g) m
  ]
