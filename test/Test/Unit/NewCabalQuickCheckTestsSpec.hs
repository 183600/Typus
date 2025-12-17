{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub)

-- | 新增的 10 个 QuickCheck 测试用例
tests :: TestTree
tests = testGroup "New Cabal QuickCheck Tests"
  [ basicPropertiesTests
  , collectionTests  
  , arithmeticTests
  ]

-- | 基础属性测试（4个测试）
basicPropertiesTests :: TestTree
basicPropertiesTests = testGroup "Basic Properties"
  [ fastProperty "reverse is involutive" $ \(xs :: [Int]) ->
      reverse (reverse xs) === xs
  
  , fastProperty "sort is idempotent" $ \(xs :: [Int]) ->
      sort (sort xs) === sort xs
  
  , fastProperty "nub removes duplicates" $ \(xs :: [Int]) ->
      let unique = nub xs
      in all (\x -> length (filter (== x) unique) == 1) unique
  
  , fastProperty "length of nub is at most length" $ \(xs :: [Int]) ->
      length (nub xs) <= length xs
  ]

-- | 集合操作测试（4个测试）
collectionTests :: TestTree
collectionTests = testGroup "Collection Operations"
  [ fastProperty "Map insert increases size" $ \(k :: Int) (v :: String) (m :: Map.Map Int String) ->
      not (Map.member k m) ==>
      Map.size (Map.insert k v m) === Map.size m + 1
  
  , fastProperty "Map lookup after insert" $ \(k :: Int) (v :: String) (m :: Map.Map Int String) ->
      Map.lookup k (Map.insert k v m) === Just v
  
  , fastProperty "Set union is commutative" $ \(s1 :: Set.Set Int) (s2 :: Set.Set Int) ->
      Set.union s1 s2 === Set.union s2 s1
  
  , fastProperty "Set intersection is idempotent" $ \(s :: Set.Set Int) ->
      Set.intersection s s === s
  ]

-- | 算术属性测试（2个测试）
arithmeticTests :: TestTree
arithmeticTests = testGroup "Arithmetic Properties"
  [ fastProperty "addition is commutative" $ \(x :: Int) (y :: Int) ->
      x + y === y + x
  
  , fastProperty "multiplication distributes over addition" $ \(x :: Int) (y :: Int) (z :: Int) ->
      x * (y + z) === x * y + x * z
  ]
