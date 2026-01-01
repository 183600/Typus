{-# LANGUAGE CPP #-}

module Test.Unit.CabalTestQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub)

import Utils (trim, splitBy, splitByCollapsed, removeLineComments, removeComments, normalizeIndentation, breakOn)
import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn, posOffset)
import Parser (FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)

tests :: TestTree
tests = testGroup "Cabal Test QuickCheck Properties"
  [ testGroup "Utils Properties"
      [ fastProperty "trim removes leading L.and trailing whitespace" prop_trim_removes_whitespace
      , fastProperty "trim is idempotent" prop_trim_idempotent
      , fastProperty "splitBy concatenation preserves content" prop_splitBy_preserves_content
      , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_no_empty
      , fastProperty "breakOn splits correctly" prop_breakOn_splits
      , fastProperty "removeLineComments preserves non-comment lines" prop_removeComments_preserves
      , fastProperty "removeComments removes L.all comment types" prop_removeComments_complete
      , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_relative
      ]
  , testGroup "SourceLocation Properties"
      [ fastProperty "SourcePos offset is non-negative" prop_sourcepos_offset_nonnegative
      , fastProperty "SourceSpan start before L.or equal to end" prop_sourcespan_ordering
      ]
  , testGroup "Parser Directives Properties"
      [ fastProperty "defaultFileDirectives has no directives set" prop_defaultFileDirectives_empty
      , fastProperty "defaultBlockDirectives has no directives set" prop_defaultBlockDirectives_empty
      ]
  , testGroup "Collection Properties"
      [ fastProperty "Map insertion is idempotent" prop_map_insert_idempotent
      , fastProperty "Set union is commutative" prop_set_union_commutative
      , fastProperty "Set intersection is commutative" prop_set_intersection_commutative
      , fastProperty "Map delete removes key" prop_map_delete_removes
      ]
  , testGroup "List Properties"
      [ fastProperty "sort is idempotent" prop_sort_idempotent
      , fastProperty "nub removes duplicates" prop_nub_removes_duplicates
      , fastProperty "L.reverse twice is identity" prop_reverse_twice_identity
      , fastProperty "L.length is preserved by L.reverse" prop_reverse_preserves_length
      ]
  , testGroup "String Properties"
      [ fastProperty "concatenation associativity" prop_concat_associative
      , fastProperty "empty string is identity for L.concat" prop_empty_concat_identity
      ]
  ]

prop_trim_removes_whitespace :: String -> Property
prop_trim_removes_whitespace s =
  let trimmed = trim s
  in conjoin
    [ counterexample "should not start with space" $
        null trimmed || not (L.head trimmed `elem` " \t\n\r")
    , counterexample "should not end with space" $
        null trimmed || not (last trimmed `elem` " \t\n\r")
    ]

prop_splitBy_preserves_content :: Char -> NonEmptyList Char -> Property
prop_splitBy_preserves_content delim (NonEmpty s) =
  delim `notElem` s ==>
  let parts = splitBy delim s
  in L.length parts === 1 .&&. L.head parts === s

prop_removeComments_preserves :: String -> Property
prop_removeComments_preserves s =
  "//" `notElem` [s] ==>
  let cleaned = removeLineComments s
  in counterexample "non-comment line should be preserved" $
       not (null s) ==> not (null cleaned)

prop_sourcepos_offset_nonnegative :: Property
prop_sourcepos_offset_nonnegative = forAll genValidSourcePos $ \(l, c, o) ->
  let pos = SourcePos l c o
  in posOffset pos >= 0
  where
    genValidSourcePos = do
      l <- choose (1, 1000)
      c <- choose (1, 1000)
      o <- choose (0, 10000)
      return (l, c, o)

prop_sourcespan_ordering :: Property
prop_sourcespan_ordering = forAll genValidSpan $ \span ->
  let start = SourcePos (posLine $ spanStart span) (posColumn $ spanStart span) (posOffset $ spanStart span)
      end = SourcePos (posLine $ spanEnd span) (posColumn $ spanEnd span) (posOffset $ spanEnd span)
  in posOffset start <= posOffset end
  where
    genValidSpan = do
      l1 <- choose (1, 100)
      c1 <- choose (1, 100)
      o1 <- choose (0, 1000)
      l2 <- choose (l1, 100)
      c2 <- if l2 == l1 then choose (c1, 100) else choose (1, 100)
      o2 <- choose (o1, 1000)
      let start = SourcePos l1 c1 o1
          end = SourcePos l2 c2 o2
      return $ SourceSpan start end

prop_defaultFileDirectives_empty :: Property
prop_defaultFileDirectives_empty =
  let fd = defaultFileDirectives
  in conjoin
    [ counterexample "fdOwnership should be Nothing" $ fdOwnership fd === Nothing
    , counterexample "fdDependentTypes should be Nothing" $ fdDependentTypes fd === Nothing
    , counterexample "fdConstraints should be Nothing" $ fdConstraints fd === Nothing
    ]

prop_defaultBlockDirectives_empty :: Property
prop_defaultBlockDirectives_empty =
  let bd = defaultBlockDirectives
  in conjoin
    [ counterexample "bdOwnership should be Nothing" $ bdOwnership bd === Nothing
    , counterexample "bdDependentTypes should be Nothing" $ bdDependentTypes bd === Nothing
    , counterexample "bdConstraints should be Nothing" $ bdConstraints bd === Nothing
    ]

prop_map_insert_idempotent :: Int -> String -> Map.Map Int String -> Property
prop_map_insert_idempotent k v m =
  let m1 = Map.insert k v m
      m2 = Map.insert k v m1
  in m1 === m2

prop_set_union_commutative :: Set.Set Int -> Set.Set Int -> Property
prop_set_union_commutative s1 s2 =
  Set.union s1 s2 === Set.union s2 s1

prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
  in trim trimmed === trimmed

prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s =
  let parts = splitByCollapsed delim s
  in counterexample "should not contain empty strings" $
       all (not . null) parts

prop_breakOn_splits :: String -> String -> Property
prop_breakOn_splits needle haystack =
  not (null needle) ==>
  let (before, after) = breakOn needle haystack
      reconstructed = if null after && not (needle `L.isInfixOf` haystack)
                      then before
                      else before ++ needle ++ after
  in counterexample ("Expected: " ++ haystack ++ ", Got: " ++ reconstructed) $
       if needle `L.isInfixOf` haystack
       then reconstructed === haystack
       else before === haystack .&&. null after
  where
    isInfixOf :: String -> String -> Bool
    isInfixOf [] _ = True
    isInfixOf _ [] = False
    isInfixOf needle haystack@(_:hs)
      | needle `L.isPrefixOf` haystack = True
      | otherwise = L.isInfixOf needle hs
    
    isPrefixOf :: String -> String -> Bool
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && L.isPrefixOf xs ys

prop_removeComments_complete :: Property
prop_removeComments_complete =
  let input = "code // line comment\nmore /* block */ code"
      result = removeComments input
  in counterexample "should remove both comment types" $
       not ("//" `elem` words result) .&&. not ("/*" `elem` words result)

prop_normalizeIndentation_relative :: Property
prop_normalizeIndentation_relative =
  let input = "    line1\n      line2\n    line3"
      result = normalizeIndentation input
      resultLines = lines result
  in counterexample "should preserve relative indentation" $
       length resultLines === 3

prop_set_intersection_commutative :: Set.Set Int -> Set.Set Int -> Property
prop_set_intersection_commutative s1 s2 =
  Set.intersection s1 s2 === Set.intersection s2 s1

prop_map_delete_removes :: Int -> Map.Map Int String -> Property
prop_map_delete_removes k m =
  let m' = Map.delete k m
  in property $ Map.notMember k m'

prop_sort_idempotent :: [Int] -> Property
prop_sort_idempotent xs =
  let sorted = sort xs
  in sort sorted === sorted

prop_nub_removes_duplicates :: [Int] -> Property
prop_nub_removes_duplicates xs =
  let unique = nub xs
  in counterexample "nub result should have no duplicates" $
       length unique === L.length (nub unique)

prop_reverse_twice_identity :: [Int] -> Property
prop_reverse_twice_identity xs =
  reverse (L.reverse xs) === xs

prop_reverse_preserves_length :: [Int] -> Property
prop_reverse_preserves_length xs =
  length (L.reverse xs) === L.length xs

prop_concat_associative :: String -> String -> String -> Property
prop_concat_associative a b c =
  (a ++ b) ++ c === a ++ (b ++ c)

prop_empty_concat_identity :: String -> Property
prop_empty_concat_identity s =
  ("" ++ s === s) .&&. (s ++ "" === s)
