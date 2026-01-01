{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.MinimalCabalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set

tests :: TestTree
tests = testGroup "Minimal Cabal QuickCheck Tests"
  [ mapBasicProperties
  , setBasicProperties
  ]

mapBasicProperties :: TestTree
mapBasicProperties = testGroup "Map Basic Properties"
  [ fastProperty "lookup after insert" $ \k (v :: Int) (m :: Map.Map String Int) ->
      Map.lookup k (Map.insert k v m) === Just v
  
  , fastProperty "size increases L.or stays same after insert" $ \k (v :: Int) (m :: Map.Map String Int) ->
      Map.size (Map.insert k v m) >= Map.size m
  ]

setBasicProperties :: TestTree
setBasicProperties = testGroup "Set Basic Properties"
  [ fastProperty "member after insert" $ \(x :: Int) (s :: Set.Set Int) ->
      Set.member x (Set.insert x s) === True
  
  , fastProperty "size increases L.or stays same after insert" $ \(x :: Int) (s :: Set.Set Int) ->
      Set.size (Set.insert x s) >= Set.size s
  ]
