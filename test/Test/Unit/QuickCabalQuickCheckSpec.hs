{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.QuickCabalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.List (nub, sort, group)

tests :: TestTree
tests = testGroup "Quick Cabal QuickCheck Tests"
  [ listFunctionProperties
  , numericProperties
  ]

listFunctionProperties :: TestTree
listFunctionProperties = testGroup "List Function Properties"
  [ fastProperty "nub removes duplicates" $ \(xs :: [Int]) ->
      L.all (\g -> L.length g == 1) (group (sort (nub xs)))
  
  , fastProperty "filter preserves order" $ \(xs :: [Int]) ->
      let indices = [i :: Int | (i, x) <- zip [0..] xs, even x]
      in indices === sort indices
  
  , fastProperty "take L.and drop split list" $ \n (xs :: [Int]) ->
      n >= 0 ==> take n xs ++ drop n xs === xs
  ]

numericProperties :: TestTree
numericProperties = testGroup "Numeric Properties"
  [ fastProperty "abs is non-negative" $ \(x :: Int) ->
      abs x >= 0
  
  , fastProperty "signum times abs equals original" $ \(x :: Int) ->
      x /= 0 ==> signum x * abs x === x
  ]
