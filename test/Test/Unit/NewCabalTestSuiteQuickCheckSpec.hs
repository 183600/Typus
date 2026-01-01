{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalTestSuiteQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck hiding ((.&&.))
import Test.QuickCheck ((.&&.))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (isPrefixOf, isSuffixOf)
import Data.List (sort, nub)

import Parser (FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import Compiler (CompilerError(..), CompilationPhase(..))
import Utils (trim, splitBy, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), mergeSpans, isValidSpan)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "New Cabal Test Suite QuickCheck"
  [ parserDirectivesTests
  , compilerPhaseTests
  , utilsFunctionTests
  , sourceSpanTests
  , listOperationTests
  , mapOperationTests
  , setOperationTests
  , stringOperationTests
  ]

parserDirectivesTests :: TestTree
parserDirectivesTests = testGroup "Parser Directives Properties"
  [ fastProperty "defaultFileDirectives has no directives set" $
      let fd = defaultFileDirectives
      in (fdOwnership fd === Nothing) .&&. 
         (fdDependentTypes fd === Nothing) .&&. 
         (fdConstraints fd === Nothing)
  
  , fastProperty "defaultBlockDirectives has no directives set" $
      let bd = defaultBlockDirectives
      in (bdOwnership bd === Nothing) .&&. 
         (bdDependentTypes bd === Nothing) .&&. 
         (bdConstraints bd === Nothing)
  ]

compilerPhaseTests :: TestTree
compilerPhaseTests = testGroup "Compiler Phase Properties"
  [ fastProperty "CompilationPhase show is non-empty" $ \phase ->
      not (L.null (show (phase :: CompilationPhase)))
  ]

utilsFunctionTests :: TestTree
utilsFunctionTests = testGroup "Utils Function Properties"
  [ fastProperty "trim is idempotent" $ \s ->
      trim (trim s) === trim s
  
  , fastProperty "trim result has no leading/trailing whitespace" $ \s ->
      let t = trim s
      in property (null t || (not (L.head t `elem` " \t\n\r") && not (last t `elem` " \t\n\r")))
  
  , fastProperty "splitBy preserves total character count (minus separators)" $ \c s ->
      c /= '\0' ==>
      let parts = splitBy c s
          totalChars = L.sum (map L.length parts)
      in totalChars <= L.length s
  
  , fastProperty "normalizeIndentation preserves non-empty lines" $ \s ->
      let original = L.filter (not . null) (lines s)
          normalized = L.filter (not . null) (lines (normalizeIndentation s))
      in L.length original === L.length normalized
  ]

sourceSpanTests :: TestTree
sourceSpanTests = testGroup "SourceSpan Properties"
  [ fastProperty "mergeSpans is commutative" $ \s1 s2 ->
      mergeSpans s1 s2 === mergeSpans s2 s1
  
  , fastProperty "mergeSpans creates valid span" $ \s1 s2 ->
      isValidSpan (mergeSpans s1 s2)
  
  , fastProperty "mergeSpans is associative" $ \s1 s2 s3 ->
      mergeSpans (mergeSpans s1 s2) s3 === mergeSpans s1 (mergeSpans s2 s3)
  ]

listOperationTests :: TestTree
listOperationTests = testGroup "List Operation Properties"
  [ fastProperty "sort is idempotent" $ \(xs :: [Int]) ->
      sort (sort xs) === sort xs
  
  , fastProperty "nub preserves order of first occurrence" $ \(xs :: [Int]) ->
      let unique = nub xs
      in L.all (\x -> let idx = L.head [i | (i, y) <- zip [0..] xs, y == x]
                        idx' = L.head [i | (i, y) <- zip [0..] unique, y == x]
                    in idx' <= idx) unique
  ]

mapOperationTests :: TestTree
mapOperationTests = testGroup "Map Operation Properties"
  [ fastProperty "Map.insert then lookup returns value" $ \(k :: String) (v :: Int) (m :: Map.Map String Int) ->
      Map.lookup k (Map.insert k v m) === Just v
  
  , fastProperty "Map.delete then lookup returns Nothing" $ \(k :: String) (m :: Map.Map String Int) ->
      Map.lookup k (Map.delete k m) === Nothing
  ]

setOperationTests :: TestTree
setOperationTests = testGroup "Set Operation Properties"
  [ fastProperty "Set.insert is idempotent" $ \(x :: Int) (s :: Set.Set Int) ->
      Set.insert x (Set.insert x s) === Set.insert x s
  
  , fastProperty "Set.delete removes element" $ \(x :: Int) (s :: Set.Set Int) ->
      not (Set.member x (Set.delete x s))
  ]

stringOperationTests :: TestTree
stringOperationTests = testGroup "String Operation Properties"
  [ fastProperty "L.isPrefixOf is reflexive" $ \(s :: String) ->
      property (L.isPrefixOf s s)
  
  , fastProperty "L.isSuffixOf is reflexive" $ \(s :: String) ->
      property (L.isSuffixOf s s)
  ]
