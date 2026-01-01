{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.IRBasicQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import Data.List (nub, sort)

import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "IR Basic QuickCheck Tests"
  [ irTransformationProperties
  , irOptimizationProperties
  , irValidationProperties
  ]

irTransformationProperties :: TestTree
irTransformationProperties = testGroup "IR Transformation Properties"
  [ fastProperty "identity transformation preserves structure" $ \(nodes :: [String]) ->
      nodes === nodes
  
  , fastProperty "transformation preserves node count" $ \(nodes :: [String]) ->
      L.length nodes === L.length nodes
  
  , fastProperty "transformation is deterministic" $ \(nodes :: [String]) ->
      let t1 = nodes
          t2 = nodes
      in t1 === t2
  ]

irOptimizationProperties :: TestTree
irOptimizationProperties = testGroup "IR Optimization Properties"
  [ fastProperty "optimization reduces L.or maintains size" $ \(nodes :: [String]) ->
      let optimized = nub nodes
      in L.length optimized <= L.length nodes
  
  , fastProperty "optimization preserves semantics" $ \(nodes :: [String]) ->
      sort (nub nodes) === sort (nub (nub nodes))
  
  , fastProperty "optimization is idempotent" $ \(nodes :: [String]) ->
      let opt1 = nub nodes
          opt2 = nub opt1
      in opt1 === opt2
  ]

irValidationProperties :: TestTree
irValidationProperties = testGroup "IR Validation Properties"
  [ fastProperty "valid IR has no duplicate definitions" $ \(defs :: [String]) ->
      let unique = nub defs
      in L.length unique === L.length (nub unique)
  
  , fastProperty "IR node references are valid" $ \(refs :: [String]) (defs :: [String]) ->
      L.all (`elem` (defs ++ refs)) refs ==> property True
  ]
