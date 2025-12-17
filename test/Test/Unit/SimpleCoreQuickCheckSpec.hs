{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.SimpleCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub)

import Utils (trim, splitBy)
import SourceLocation (SourcePos(..), SourceSpan(..), mergeSpans, isValidSpan)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Simple Core QuickCheck Tests"
  [ stringUtilsTests
  , sourceLocationTests
  , dataStructureTests
  ]

stringUtilsTests :: TestTree
stringUtilsTests = testGroup "String Utils Properties"
  [ fastProperty "trim is idempotent" $ \s ->
      trim (trim s) == trim s
  
  , fastProperty "trim removes leading and trailing spaces" $ \s ->
      let trimmed = trim s
      in not (null trimmed) ==> head trimmed /= ' ' && last trimmed /= ' '
  
  , fastProperty "splitBy preserves content" $ \(c :: Char) (s :: String) ->
      concat (splitBy c s) == filter (/= c) s
  ]

sourceLocationTests :: TestTree
sourceLocationTests = testGroup "SourceLocation Properties"
  [ fastProperty "SourcePos line is non-negative" $ \l c o ->
      l >= 0 ==> posLine (SourcePos l c o) >= 0
  
  , fastProperty "mergeSpans creates valid span" $ \l1 c1 o1 l2 c2 o2 ->
      let pos1 = SourcePos l1 c1 o1
          pos2 = SourcePos l2 c2 o2
          span1 = SourceSpan pos1 pos1
          span2 = SourceSpan pos2 pos2
          merged = mergeSpans span1 span2
      in isValidSpan merged
  
  , fastProperty "isValidSpan checks order" $ \l1 c1 o1 l2 c2 o2 ->
      let pos1 = SourcePos l1 c1 o1
          pos2 = SourcePos l2 c2 o2
          span = SourceSpan pos1 pos2
      in isValidSpan span == (o1 <= o2)
  ]

dataStructureTests :: TestTree
dataStructureTests = testGroup "Data Structure Properties"
  [ fastProperty "Map insert is idempotent" $ \(k :: Int) (v :: String) (m :: Map.Map Int String) ->
      let m1 = Map.insert k v m
          m2 = Map.insert k v m1
      in m1 == m2
  
  , fastProperty "Set union is commutative" $ \(s1 :: Set.Set Int) (s2 :: Set.Set Int) ->
      Set.union s1 s2 == Set.union s2 s1
  
  , fastProperty "sort is idempotent" $ \(xs :: [Int]) ->
      let sorted = sort xs
      in sort sorted == sorted
  
  , fastProperty "nub preserves order" $ \(xs :: [Int]) ->
      let unique = nub xs
          indices = map (\x -> head [i | (i, y) <- zip [0..] xs, y == x]) unique
      in indices == sort indices
  ]
