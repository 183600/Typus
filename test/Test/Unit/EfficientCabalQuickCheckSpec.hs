{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.EfficientCabalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)

tests :: TestTree
tests = testGroup "Efficient Cabal QuickCheck Tests"
  [ stringSearchProperties
  , listCombinationProperties
  ]

stringSearchProperties :: TestTree
stringSearchProperties = testGroup "String Search Properties"
  [ fastProperty "prefix of itself" $ \(s :: String) ->
      isPrefixOf s s === True
  
  , fastProperty "suffix of itself" $ \(s :: String) ->
      isSuffixOf s s === True
  
  , fastProperty "infix of itself" $ \(s :: String) ->
      isInfixOf s s === True
  ]

listCombinationProperties :: TestTree
listCombinationProperties = testGroup "List Combination Properties"
  [ fastProperty "zip and unzip" $ \(xs :: [Int]) (ys :: [String]) ->
      let paired = zip xs ys
          (xs', ys') = unzip paired
      in (xs', ys') === (take (length paired) xs, take (length paired) ys)
  
  , fastProperty "replicate length" $ \n (x :: Int) ->
      n >= 0 ==> length (replicate n x) === n
  ]
