{-# LANGUAGE CPP #-}

module Test.Unit.NewCabalTestQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub, isPrefixOf, isSuffixOf)

import Parser (FileDirectives(..), BlockDirectives(..))
import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn, posOffset, spanStart, spanEnd)
import Utils (trim, splitBy, splitByCollapsed, splitByComma, removeLineComments, breakOn)
import TestSupport.Arbitrary ()

prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
  in trim trimmed === trimmed

prop_splitBy_join_roundtrip :: Char -> NonEmptyList Char -> Property
prop_splitBy_join_roundtrip delim (NonEmpty s) =
  delim `notElem` s ==>
  let parts = splitBy delim s
  in length parts === 1 .&&. head parts === s

prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s =
  let parts = splitByCollapsed delim s
  in property $ all (not . null) parts

prop_map_insert_lookup :: Int -> String -> Map.Map Int String -> Property
prop_map_insert_lookup k v m =
  Map.lookup k (Map.insert k v m) === Just v

prop_set_insert_member :: Int -> Set.Set Int -> Property
prop_set_insert_member x s =
  property $ Set.member x (Set.insert x s)

prop_sort_preserves_elements :: [Int] -> Property
prop_sort_preserves_elements xs =
  property $ sort xs `elem` permutations xs
  where
    permutations [] = [[]]
    permutations ys = [x:ps | x <- ys, ps <- permutations (filter (/= x) ys)]

prop_nub_idempotent :: [Int] -> Property
prop_nub_idempotent xs =
  let once = nub xs
  in nub once === once

prop_sourcepos_ordering :: SourcePos -> SourcePos -> Property
prop_sourcepos_ordering p1 p2 =
  let offset1 = posOffset p1
      offset2 = posOffset p2
  in (offset1 < offset2) === (posLine p1 < posLine p2 || (posLine p1 == posLine p2 && posColumn p1 < posColumn p2))

prop_sourcespan_valid :: SourceSpan -> Property
prop_sourcespan_valid span =
  let start = spanStart span
      end = spanEnd span
  in property $ posOffset start <= posOffset end

prop_breakOn_prefix :: String -> String -> Property
prop_breakOn_prefix needle haystack =
  needle `isPrefixOf` haystack ==>
  let (before, after) = breakOn needle haystack
  in before === "" .&&. after === haystack

tests :: TestTree
tests = testGroup "NewCabalTestQuickCheck"
  [ fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "splitBy/join roundtrip" prop_splitBy_join_roundtrip
  , fastProperty "splitByCollapsed has no empty strings" prop_splitByCollapsed_no_empty
  , fastProperty "map insert/lookup" prop_map_insert_lookup
  , fastProperty "set insert/member" prop_set_insert_member
  , fastProperty "sort preserves elements" prop_sort_preserves_elements
  , fastProperty "nub is idempotent" prop_nub_idempotent
  , fastProperty "sourcepos ordering" prop_sourcepos_ordering
  , fastProperty "sourcespan is valid" prop_sourcespan_valid
  , fastProperty "breakOn with prefix" prop_breakOn_prefix
  ]
