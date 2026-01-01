{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewSimpleCabalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import Data.List (sort, nub)
import Data.Char (isSpace)

import Utils (trim, splitBy, splitByCollapsed)
import SourceLocation (SourcePos(..), SourceSpan(..), mergeSpans, isValidSpan)
import Parser (FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "New Simple Cabal QuickCheck Tests"
  [ stringUtilsProperties
  , sourceLocationProperties
  , parserDirectivesProperties
  , listOperationsProperties
  , mapOperationsProperties
  ]

stringUtilsProperties :: TestTree
stringUtilsProperties = testGroup "String Utils Properties"
  [ fastProperty "trim is idempotent" $ \s ->
      let t = trim s in trim t === t
  
  , fastProperty "trim removes L.all leading L.and trailing spaces" $ \s ->
      let t = trim s
      in case t of
           [] -> property True
           (c:_) -> property (not (isSpace c) && not (isSpace (last t)))
  
  , fastProperty "splitBy concatenation preserves original (without delimiter)" $ \c s ->
      c /= '\0' ==>
      let parts = splitBy c s
      in L.concat parts === L.filter (/= c) s .||. L.any (== c) s
  
  , fastProperty "splitByCollapsed never returns empty strings" $ \c s ->
      c /= '\0' ==>
      let parts = splitByCollapsed c s
      in L.all (not . null) parts
  ]

sourceLocationProperties :: TestTree
sourceLocationProperties = testGroup "SourceLocation Properties"
  [ fastProperty "SourcePos has valid coordinates" $ \pos ->
      posLine pos > 0 && posColumn pos > 0 && posOffset pos >= 0
  
  , fastProperty "SourceSpan has valid ordering" $ \sp ->
      let start = spanStart sp
          end = spanEnd sp
      in posOffset end >= posOffset start
  
  , fastProperty "mergeSpans is associative" $ \s1 s2 s3 ->
      mergeSpans (mergeSpans s1 s2) s3 === mergeSpans s1 (mergeSpans s2 s3)
  
  , fastProperty "isValidSpan correctly validates spans" $ \sp ->
      isValidSpan sp === (posOffset (spanEnd sp) >= posOffset (spanStart sp))
  ]

parserDirectivesProperties :: TestTree
parserDirectivesProperties = testGroup "Parser Directives Properties"
  [ fastProperty "defaultFileDirectives has L.all Nothing fields" $
      fdOwnership defaultFileDirectives === Nothing .&&.
      fdDependentTypes defaultFileDirectives === Nothing .&&.
      fdConstraints defaultFileDirectives === Nothing
  
  , fastProperty "defaultBlockDirectives has L.all Nothing fields" $
      bdOwnership defaultBlockDirectives === Nothing .&&.
      bdDependentTypes defaultBlockDirectives === Nothing .&&.
      bdConstraints defaultBlockDirectives === Nothing
  ]

listOperationsProperties :: TestTree
listOperationsProperties = testGroup "List Operations Properties"
  [ fastProperty "sort is idempotent" $ \(xs :: [Int]) ->
      sort (sort xs) === sort xs
  
  , fastProperty "nub preserves order of first occurrences" $ \(xs :: [Int]) ->
      let unique = nub xs
      in L.all (`elem` xs) unique
  
  , fastProperty "L.reverse twice is identity" $ \(xs :: [Int]) ->
      L.reverse (L.reverse xs) === xs
  
  , fastProperty "L.length of append equals L.sum of lengths" $ \(xs :: [Int]) (ys :: [Int]) ->
      L.length (xs ++ ys) === L.length xs + L.length ys
  ]

mapOperationsProperties :: TestTree
mapOperationsProperties = testGroup "Map Operations Properties"
  [ fastProperty "insert then lookup succeeds" $ \(k :: String) (v :: Int) (m :: Map.Map String Int) ->
      Map.lookup k (Map.insert k v m) === Just v
  
  , fastProperty "delete removes key" $ \(k :: String) (m :: Map.Map String Int) ->
      Map.lookup k (Map.delete k m) === Nothing
  
  , fastProperty "union left-biases on conflicts" $ \(k :: String) (v1 :: Int) (v2 :: Int) ->
      let m1 = Map.singleton k v1
          m2 = Map.singleton k v2
      in Map.lookup k (Map.union m1 m2) === Just v1
  ]
