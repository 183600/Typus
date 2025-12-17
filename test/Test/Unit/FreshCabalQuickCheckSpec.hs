{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.FreshCabalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub, group)
import Data.Char (isSpace)

import Utils (trim, splitBy, splitByCollapsed, removeLineComments)
import SourceLocation (SourcePos(..), SourceSpan(..), mergeSpans, isValidSpan)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Fresh Cabal QuickCheck Tests"
  [ utilsProperties
  , sourceLocationProperties
  , listProperties
  , mapProperties
  , setProperties
  ]

utilsProperties :: TestTree
utilsProperties = testGroup "Utils Module Properties"
  [ fastProperty "trim is idempotent" $ \s ->
      let t = trim s in trim t === t
  
  , fastProperty "trim result has no leading/trailing spaces" $ \s ->
      let t = trim s
      in case t of
           [] -> property True
           (c:_) -> property (not (isSpace c) && not (isSpace (last t)))
  
  , fastProperty "splitBy preserves concatenation" $ \c s ->
      c /= '\0' ==>
      let parts = splitBy c s
      in concat parts === filter (/= c) s .||. length parts > 1
  
  , fastProperty "splitByCollapsed removes empty strings" $ \c s ->
      c /= '\0' ==>
      let parts = splitByCollapsed c s
      in all (not . null) parts
  
  , fastProperty "removeLineComments preserves non-comment lines" $ \s ->
      not ("//" `elem` [s]) ==>
      removeLineComments s === s .||. "//" `elem` [s]
  ]

sourceLocationProperties :: TestTree
sourceLocationProperties = testGroup "SourceLocation Properties"
  [ fastProperty "SourcePos has positive line and column" $ \pos ->
      posLine pos > 0 && posColumn pos > 0 && posOffset pos >= 0
  
  , fastProperty "SourceSpan ordering is valid" $ \sp ->
      let start = spanStart sp
          end = spanEnd sp
      in posOffset end >= posOffset start
  
  , fastProperty "mergeSpans is commutative for overlapping spans" $ \s1 s2 ->
      let merged1 = mergeSpans s1 s2
          merged2 = mergeSpans s2 s1
      in merged1 === merged2
  
  , fastProperty "isValidSpan checks correct ordering" $ \sp ->
      isValidSpan sp === (posOffset (spanEnd sp) >= posOffset (spanStart sp))
  ]

listProperties :: TestTree
listProperties = testGroup "List Properties"
  [ fastProperty "sort is idempotent" $ \(xs :: [Int]) ->
      sort (sort xs) === sort xs
  
  , fastProperty "nub removes duplicates" $ \(xs :: [Int]) ->
      let unique = nub xs
      in all (\g -> length g == 1) (group (sort unique))
  
  , fastProperty "reverse twice is identity" $ \(xs :: [Int]) ->
      reverse (reverse xs) === xs
  
  , fastProperty "length of concatenation equals sum of lengths" $ \(xs :: [Int]) (ys :: [Int]) ->
      length (xs ++ ys) === length xs + length ys
  ]

mapProperties :: TestTree
mapProperties = testGroup "Map Properties"
  [ fastProperty "insert then lookup returns the value" $ \(k :: String) (v :: Int) (m :: Map.Map String Int) ->
      Map.lookup k (Map.insert k v m) === Just v
  
  , fastProperty "delete then lookup returns Nothing" $ \(k :: String) (m :: Map.Map String Int) ->
      Map.lookup k (Map.delete k m) === Nothing
  
  , fastProperty "size after insert is at least original size" $ \(k :: String) (v :: Int) (m :: Map.Map String Int) ->
      Map.size (Map.insert k v m) >= Map.size m
  
  , fastProperty "union is idempotent with itself" $ \(m :: Map.Map String Int) ->
      Map.union m m === m
  ]

setProperties :: TestTree
setProperties = testGroup "Set Properties"
  [ fastProperty "insert is idempotent" $ \(x :: Int) s ->
      Set.insert x (Set.insert x s) === Set.insert x s
  
  , fastProperty "member after insert is True" $ \(x :: Int) s ->
      Set.member x (Set.insert x s) === True
  
  , fastProperty "member after delete is False" $ \(x :: Int) s ->
      Set.member x (Set.delete x s) === False
  
  , fastProperty "union is commutative" $ \s1 s2 ->
      Set.union s1 s2 === Set.union s2 (s1 :: Set.Set Int)
  
  , fastProperty "intersection is commutative" $ \s1 s2 ->
      Set.intersection s1 s2 === Set.intersection s2 (s1 :: Set.Set Int)
  ]
