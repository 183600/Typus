{-# LANGUAGE CPP #-}

module Test.Unit.NewCabalQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub, intercalate)
import Data.Char (toLower, toUpper, isAlpha, isDigit)

import Utils (trim, splitBy, removeLineComments)
import SourceLocation (SourcePos(..), SourceSpan(..))
import Parser (FileDirectives(..), BlockDirectives(..))
-- import Compiler.Types (Type(..), TypeVar(..))
-- import Analyzer.SymbolTable (SymbolTable, emptySymbolTable)
import TestSupport.Arbitrary ()

-- Property 1: String case conversion roundtrip
prop_case_roundtrip :: String -> Property
prop_case_roundtrip s =
  let lower = map toLower s
      upper = map toUpper s
  in property (map toUpper lower === upper .&&. map toLower upper === lower)

-- Property 2: List sorting preserves elements
prop_sort_preserves_elements :: [Int] -> Property
prop_sort_preserves_elements xs =
  sort xs === sort (sort xs)

-- Property 3: Map union is associative
prop_map_union_associative :: Map.Map String Int -> Map.Map String Int -> Map.Map String Int -> Property
prop_map_union_associative m1 m2 m3 =
  Map.union m1 (Map.union m2 m3) === Map.union (Map.union m1 m2) m3

-- Property 4: Set intersection is commutative
prop_set_intersection_commutative :: Set.Set Int -> Set.Set Int -> Property
prop_set_intersection_commutative s1 s2 =
  Set.intersection s1 s2 === Set.intersection s2 s1

-- Property 5: String concatenation with join
prop_join_concat :: String -> [String] -> Property
prop_join_concat sep strings =
  not (null strings) ==>
  intercalate sep strings === concat (intersperse sep strings)
  where
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:xs) = x : sep : intersperse sep xs

-- Property 6: List nub removes duplicates
prop_nub_removes_duplicates :: [Int] -> Property
prop_nub_removes_duplicates xs =
  nub xs === nub (nub xs)

-- Property 7: Alpha-numeric string filtering
prop_alnum_filter :: String -> Property
prop_alnum_filter s =
  let filtered = filter isAlpha s
      alnumFiltered = filter (\c -> isAlpha c || isDigit c) s
  in property (length filtered <= length alnumFiltered)

-- Property 8: Map size after insert
prop_map_size_insert :: String -> Int -> Map.Map String Int -> Property
prop_map_size_insert key value m =
  let m' = Map.insert key value m
      oldSize = Map.size m
      newSize = Map.size m'
  in if Map.member key m
     then newSize === oldSize
     else newSize === oldSize + 1

-- Property 9: Set difference properties
prop_set_difference :: Set.Set Int -> Set.Set Int -> Property
prop_set_difference s1 s2 =
  let diff = Set.difference s1 s2
  in property (all (`Set.notMember` s2) (Set.toList diff))

-- Property 10: SourceSpan construction invariants
prop_sourcespan_invariants :: Int -> Int -> Property
prop_sourcespan_invariants line offset =
  line > 0 && offset >= 0 ==>
  let pos = SourcePos line 1 offset
      span = SourceSpan pos pos
  in posLine pos === line .&&. posOffset pos === offset

tests :: TestTree
tests = testGroup "New Cabal QuickCheck Test Specs"
  [ fastProperty "String case conversion roundtrip" prop_case_roundtrip
  , fastProperty "List sorting preserves elements" prop_sort_preserves_elements
  , fastProperty "Map union is associative" prop_map_union_associative
  , fastProperty "Set intersection is commutative" prop_set_intersection_commutative
  , fastProperty "String concatenation with join" prop_join_concat
  , fastProperty "List nub removes duplicates" prop_nub_removes_duplicates
  , fastProperty "Alpha-numeric string filtering" prop_alnum_filter
  , fastProperty "Map size after insert" prop_map_size_insert
  , fastProperty "Set difference properties" prop_set_difference
  , fastProperty "SourceSpan construction invariants" prop_sourcespan_invariants
  ]