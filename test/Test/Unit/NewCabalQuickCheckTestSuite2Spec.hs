{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalQuickCheckTestSuite2Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub)

import Utils (trim, splitBy)
import SourceLocation (SourcePos(..), SourceSpan(..), mergeSpans)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "New Cabal QuickCheck Test Suite 2"
  [ coreDataStructureProperties
  , advancedStringProperties
  , sourceLocationAdvancedProperties
  , setOperationsProperties
  , compositionProperties
  ]

-- Test 1-2: 核心数据结构属性
coreDataStructureProperties :: TestTree
coreDataStructureProperties = testGroup "Core Data Structure Properties"
  [ fastProperty "Map.fromList then Map.toList preserves unique keys" $ \(kvs :: [(String, Int)]) ->
      let m = Map.fromList kvs
          kvs' = Map.toList m
          uniqueKeys = nub (map fst kvs)
      in length kvs' === length uniqueKeys
  
  , fastProperty "Set.union is commutative" $ \(s1 :: Set.Set Int) (s2 :: Set.Set Int) ->
      Set.union s1 s2 === Set.union s2 s1
  ]

-- Test 3-4: 高级字符串处理属性
advancedStringProperties :: TestTree
advancedStringProperties = testGroup "Advanced String Properties"
  [ fastProperty "trim preserves non-whitespace content" $ \s ->
      let t = trim s
          nonSpace = filter (not . (`elem` " \t\n\r")) s
      in t === nonSpace .||. null t
  
  , fastProperty "splitBy length equals number of delimiters plus one (non-empty)" $ \c s ->
      c /= '\0' && not (null s) ==>
      let parts = splitBy c s
          delimCount = length (filter (== c) s)
      in length parts === delimCount + 1
  ]

-- Test 5-6: 源位置高级属性
sourceLocationAdvancedProperties :: TestTree
sourceLocationAdvancedProperties = testGroup "SourceLocation Advanced Properties"
  [ fastProperty "mergeSpans is commutative" $ \s1 s2 ->
      mergeSpans s1 s2 === mergeSpans s2 s1
  
  , fastProperty "mergeSpans contains both original spans" $ \s1 s2 ->
      let merged = mergeSpans s1 s2
          start = spanStart merged
          end = spanEnd merged
      in posOffset start <= min (posOffset (spanStart s1)) (posOffset (spanStart s2)) .&&.
         posOffset end >= max (posOffset (spanEnd s1)) (posOffset (spanEnd s2))
  ]

-- Test 7-8: Set 操作属性
setOperationsProperties :: TestTree
setOperationsProperties = testGroup "Set Operations Properties"
  [ fastProperty "Set.intersection is commutative" $ \(s1 :: Set.Set Int) (s2 :: Set.Set Int) ->
      Set.intersection s1 s2 === Set.intersection s2 s1
  
  , fastProperty "Set.difference is not commutative" $ \(s1 :: Set.Set Int) (s2 :: Set.Set Int) ->
      not (Set.null s1) && not (Set.null s2) && s1 /= s2 ==>
      Set.difference s1 s2 =/= Set.difference s2 s1
  ]

-- Test 9-10: 函数组合属性
compositionProperties :: TestTree
compositionProperties = testGroup "Composition Properties"
  [ fastProperty "map composition distributes" $ \(Fun _ f :: Fun Int Int) (Fun _ g :: Fun Int Int) (xs :: [Int]) ->
      map (f . g) xs === map f (map g xs)
  
  , fastProperty "filter composition is conjunction" $ \(Fun _ p :: Fun Int Bool) (Fun _ q :: Fun Int Bool) (xs :: [Int]) ->
      filter (\x -> p x && q x) xs === filter p (filter q xs)
  ]
