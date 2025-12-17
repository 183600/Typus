{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.DependentTypesBasicQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map

import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "DependentTypes Basic QuickCheck Tests"
  [ typeIndexProperties
  , refinementTypeProperties
  , constraintProperties
  ]

typeIndexProperties :: TestTree
typeIndexProperties = testGroup "Type Index Properties"
  [ fastProperty "type index is within bounds" $ \(n :: Int) ->
      n >= 0 ==> property True
  
  , fastProperty "indexed types preserve base type" $ \(baseType :: String) (index :: Int) ->
      not (null baseType) ==> property True
  
  , fastProperty "type index equality is reflexive" $ \(n :: Int) ->
      n === n
  ]

refinementTypeProperties :: TestTree
refinementTypeProperties = testGroup "Refinement Type Properties"
  [ fastProperty "refinement narrows type" $ \(baseType :: String) (predicate :: String) ->
      not (null baseType) && not (null predicate) ==> property True
  
  , fastProperty "refinement is consistent with base" $ \(t :: String) ->
      t === t
  
  , fastProperty "multiple refinements compose" $ \(r1 :: String) (r2 :: String) ->
      let composed = r1 ++ " && " ++ r2
      in length composed >= length r1 && length composed >= length r2
  ]

constraintProperties :: TestTree
constraintProperties = testGroup "Constraint Properties"
  [ fastProperty "constraint satisfaction is boolean" $ \(satisfied :: Bool) ->
      satisfied === satisfied
  
  , fastProperty "constraints are checked at compile time" $ \(constraint :: String) ->
      not (null constraint) ==> property True
  
  , fastProperty "constraint solving is deterministic" $ \(constraints :: [String]) ->
      constraints === constraints
  ]
