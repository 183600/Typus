{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.AdditionalCabalTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub, group)

import Utils (trim, splitBy, normalizeIndentation, breakOn)
import SourceLocation (SourcePos(..), SourceSpan(..), mergeSpans, isValidSpan)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Additional Cabal QuickCheck Tests"
  [ utilsPropertiesTests
  , sourceLocationTests
  , dataStructureTests
  ]

utilsPropertiesTests :: TestTree
utilsPropertiesTests = testGroup "Utils Properties"
  [ fastProperty "trim removes whitespace symmetrically" $ \s ->
      let t = trim s
      in (null t || (not (head t `elem` " \t\n\r") && not (last t `elem` " \t\n\r")))
  
  , fastProperty "splitBy never creates more total chars" $ \c s ->
      c /= '\0' ==>
      let parts = splitBy c s
          totalChars = sum (map length parts)
      in totalChars <= length s
  
  , fastProperty "normalizeIndentation preserves line count" $ \s ->
      length (lines s) === length (lines (normalizeIndentation s))
  
  , fastProperty "breakOn result length is reasonable" $ \pat s ->
      not (null pat) ==>
      let (before, after) = breakOn pat s
      in length before <= length s && length after <= length s
  ]

sourceLocationTests :: TestTree
sourceLocationTests = testGroup "SourceLocation Properties"
  [ fastProperty "mergeSpans creates valid span" $ \s1 s2 ->
      let merged = mergeSpans s1 s2
      in isValidSpan merged
  
  , fastProperty "mergeSpans is idempotent" $ \s1 ->
      mergeSpans s1 s1 === s1
  ]

dataStructureTests :: TestTree
dataStructureTests = testGroup "Data Structure Properties"
  [ fastProperty "Map size after insert increases or stays same" $ \(k :: String) (v :: Int) (m :: Map.Map String Int) ->
      let m' = Map.insert k v m
      in Map.size m' >= Map.size m
  
  , fastProperty "Set union is commutative" $ \(s1 :: Set.Set Int) (s2 :: Set.Set Int) ->
      Set.union s1 s2 === Set.union s2 s1
  
  , fastProperty "nub removes duplicates" $ \(xs :: [Int]) ->
      let unique = nub xs
      in all (\x -> length (filter (== x) unique) == 1) unique
  
  , fastProperty "group preserves all elements" $ \(xs :: [Int]) ->
      concat (group (sort xs)) === sort xs
  ]
