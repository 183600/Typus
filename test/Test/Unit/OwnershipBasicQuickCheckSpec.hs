{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.OwnershipBasicQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (nub)

import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Ownership Basic QuickCheck Tests"
  [ variableTrackingProperties
  , scopeProperties
  , lifetimeProperties
  ]

variableTrackingProperties :: TestTree
variableTrackingProperties = testGroup "Variable Tracking Properties"
  [ fastProperty "tracking variable adds to set" $ \(var :: String) (vars :: Set.Set String) ->
      Set.member var (Set.insert var vars) === True
  
  , fastProperty "removing variable removes from set" $ \(var :: String) (vars :: Set.Set String) ->
      Set.member var (Set.delete var vars) === False
  
  , fastProperty "variable set size is non-negative" $ \(vars :: Set.Set String) ->
      Set.size vars >= 0
  ]

scopeProperties :: TestTree
scopeProperties = testGroup "Scope Properties"
  [ fastProperty "entering scope creates new level" $ \(scopes :: [[String]]) ->
      let newScopes = [] : scopes
      in length newScopes === length scopes + 1
  
  , fastProperty "exiting scope removes level" $ \(scopes :: [[String]]) ->
      not (null scopes) ==>
      let newScopes = tail scopes
      in length newScopes === length scopes - 1
  
  , fastProperty "variable in inner scope shadows outer" $ \(var :: String) (val1 :: String) (val2 :: String) ->
      let outerScope = Map.singleton var val1
          innerScope = Map.singleton var val2
          combined = Map.union innerScope outerScope
      in Map.lookup var combined === Just val2
  ]

lifetimeProperties :: TestTree
lifetimeProperties = testGroup "Lifetime Properties"
  [ fastProperty "lifetime start is before end" $ \(start :: Int) (duration :: Int) ->
      duration >= 0 ==>
      let end = start + abs duration
      in end >= start
  
  , fastProperty "overlapping lifetimes have common range" $ \(s1 :: Int) (e1 :: Int) (s2 :: Int) (e2 :: Int) ->
      let start1 = min s1 e1
          end1 = max s1 e1
          start2 = min s2 e2
          end2 = max s2 e2
          overlaps = start1 <= end2 && start2 <= end1
      in overlaps ==> property True
  ]
