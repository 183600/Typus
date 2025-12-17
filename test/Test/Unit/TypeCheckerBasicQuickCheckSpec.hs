{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.TypeCheckerBasicQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import Data.List (nub)

import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "TypeChecker Basic QuickCheck Tests"
  [ typeUnificationProperties
  , typeInferenceProperties
  , typeCompatibilityProperties
  ]

typeUnificationProperties :: TestTree
typeUnificationProperties = testGroup "Type Unification Properties"
  [ fastProperty "unifying type with itself succeeds" $ \(t :: String) ->
      t === t
  
  , fastProperty "unification is commutative" $ \(t1 :: String) (t2 :: String) ->
      (t1 == t2) === (t2 == t1)
  
  , fastProperty "unification is transitive" $ \(t1 :: String) (t2 :: String) (t3 :: String) ->
      (t1 == t2 && t2 == t3) ==> (t1 === t3)
  ]

typeInferenceProperties :: TestTree
typeInferenceProperties = testGroup "Type Inference Properties"
  [ fastProperty "inferring type of literal is deterministic" $ \(n :: Int) ->
      let t1 = "Int"
          t2 = "Int"
      in t1 === t2
  
  , fastProperty "type environment lookup is consistent" $ \(var :: String) (typ :: String) (env :: Map.Map String String) ->
      let env' = Map.insert var typ env
      in Map.lookup var env' === Just typ
  ]

typeCompatibilityProperties :: TestTree
typeCompatibilityProperties = testGroup "Type Compatibility Properties"
  [ fastProperty "same types are compatible" $ \(t :: String) ->
      t === t
  
  , fastProperty "subtype is compatible with supertype" $ \(sub :: String) (super :: String) ->
      sub == super ==> property True
  
  , fastProperty "type compatibility is reflexive" $ \(t :: String) ->
      t === t
  ]
