{-# LANGUAGE CPP #-}

module Test.Unit.CabalEnhancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub, isInfixOf)

import Utils (trim, splitBy, splitByCollapsed, removeComments, breakOn)
import Parser (FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, spanFrom, spanBetween, isValidSpan)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Cabal Enhanced QuickCheck Tests"
  [ testGroup "Utils Properties"
      [ fastProperty "trim removes leading/trailing spaces" prop_trim_spaces
      , fastProperty "splitBy preserves content" prop_splitBy_preserves
      , fastProperty "splitByCollapsed filters empty" prop_splitByCollapsed_nonempty
      ]
  , testGroup "SourceLocation Properties"
      [ fastProperty "posAfter increments offset" prop_posAfter_increments
      , fastProperty "spanBetween is valid" prop_spanBetween_valid
      , fastProperty "spanFrom creates valid span" prop_spanFrom_valid
      ]
  , testGroup "Data Structure Properties"
      [ fastProperty "Map insertion is idempotent" prop_map_insert_idempotent
      , fastProperty "Set union is commutative" prop_set_union_commutative
      , fastProperty "List sort is idempotent" prop_sort_idempotent
      ]
  ]

-- Utils Properties
prop_trim_spaces :: String -> Property
prop_trim_spaces s =
  let trimmed = trim s
      hasLeadingSpace = not (null s) && head s == ' '
      hasTrailingSpace = not (null s) && last s == ' '
  in (not hasLeadingSpace || not (null trimmed) && head trimmed /= ' ') .&&.
     (not hasTrailingSpace || not (null trimmed) && last trimmed /= ' ')

prop_splitBy_preserves :: Char -> NonEmptyList Char -> Property
prop_splitBy_preserves delim (NonEmpty s) =
  let parts = splitBy delim s
      rejoined = concat parts
      originalWithoutDelim = filter (/= delim) s
  in rejoined === originalWithoutDelim

prop_splitByCollapsed_nonempty :: Char -> String -> Property
prop_splitByCollapsed_nonempty delim s =
  let parts = splitByCollapsed delim s
  in all (not . null) parts === True

-- SourceLocation Properties
prop_posAfter_increments :: Char -> Positive Int -> Positive Int -> Positive Int -> Property
prop_posAfter_increments c (Positive line) (Positive col) (Positive offset) =
  let pos = SourcePos line col offset
      newPos = posAfter c pos
  in posOffset newPos === posOffset pos + 1

prop_spanBetween_valid :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_spanBetween_valid (Positive l1) (Positive c1) (Positive l2) (Positive c2) =
  let (startLine, startCol, endLine, endCol) = 
        if l1 < l2 then (l1, c1, l2, c2)
        else if l1 > l2 then (l2, c2, l1, c1)
        else if c1 <= c2 then (l1, c1, l2, c2)
        else (l1, c2, l2, c1)
      start = SourcePos startLine startCol 0
      end = SourcePos endLine endCol ((endLine - startLine) * 80 + (endCol - startCol))
      span = spanBetween start end
  in isValidSpan span === True

prop_spanFrom_valid :: Positive Int -> Positive Int -> Property
prop_spanFrom_valid (Positive line) (Positive col) =
  let pos = SourcePos line col 0
      span = spanFrom pos
  in isValidSpan span === True

-- Data Structure Properties
prop_map_insert_idempotent :: Int -> String -> Map.Map Int String -> Property
prop_map_insert_idempotent k v m =
  let m1 = Map.insert k v m
      m2 = Map.insert k v m1
  in m1 === m2

prop_set_union_commutative :: Set.Set Int -> Set.Set Int -> Property
prop_set_union_commutative s1 s2 =
  Set.union s1 s2 === Set.union s2 s1

prop_sort_idempotent :: [Int] -> Property
prop_sort_idempotent xs =
  let sorted = sort xs
  in sort sorted === sorted
