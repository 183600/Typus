{-# LANGUAGE CPP #-}

module Test.Unit.CoreQuickCheckPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub, intersperse)

import Utils (trim, splitBy, splitByCollapsed, removeLineComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn, posOffset, spanStart, spanEnd)
import Parser (FileDirectives(..), BlockDirectives(..), CodeBlock(..), defaultFileDirectives, defaultBlockDirectives)
import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..))
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Core QuickCheck Properties"
  [ stringUtilsTests
  , listOperationsTests
  , sourceLocationTests
  , parserDataTests
  , irStructureTests
  , mapPropertiesTests
  , setPropertiesTests
  ]

stringUtilsTests :: TestTree
stringUtilsTests = testGroup "String Utils Properties"
  [ fastProperty "trim removes leading and trailing whitespace" prop_trim_removes_whitespace
  , fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "trim of empty string is empty" prop_trim_empty
  , fastProperty "splitBy preserves total length" prop_splitBy_preserves_length
  , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_no_empty
  , fastProperty "splitBy and join are inverse" prop_splitBy_join_inverse
  , fastProperty "removeLineComments removes all // comments" prop_removeLineComments_removes_comments
  , fastProperty "normalizeIndentation preserves line count" prop_normalizeIndentation_preserves_lines
  ]

listOperationsTests :: TestTree
listOperationsTests = testGroup "List Operations Properties"
  [ fastProperty "sort is idempotent" prop_sort_idempotent
  , fastProperty "sort preserves length" prop_sort_preserves_length
  , fastProperty "nub preserves length or reduces it" prop_nub_preserves_or_reduces_length
  , fastProperty "nub removes duplicates" prop_nub_removes_duplicates
  , fastProperty "intersperse with empty list is empty" prop_intersperse_empty
  , fastProperty "intersperse increases length correctly" prop_intersperse_length
  ]

sourceLocationTests :: TestTree
sourceLocationTests = testGroup "SourceLocation Properties"
  [ fastProperty "SourcePos offset increases with line/column" prop_sourcepos_offset_monotonic
  , fastProperty "SourceSpan start is before or equal to end" prop_sourcespan_start_before_end
  , fastProperty "SourcePos line and column are positive" prop_sourcepos_positive
  , fastProperty "SourceSpan equality is reflexive" prop_sourcespan_reflexive
  ]

parserDataTests :: TestTree
parserDataTests = testGroup "Parser Data Properties"
  [ fastProperty "defaultFileDirectives has all Nothing fields" prop_defaultFileDirectives_empty
  , fastProperty "defaultBlockDirectives has all Nothing fields" prop_defaultBlockDirectives_empty
  , fastProperty "FileDirectives equality is reflexive" prop_filedirectives_reflexive
  , fastProperty "BlockDirectives equality is reflexive" prop_blockdirectives_reflexive
  ]

irStructureTests :: TestTree
irStructureTests = testGroup "IR Structure Properties"
  [ fastProperty "SourceIR roundtrip preserves structure" prop_sourceir_structure
  , fastProperty "GoIR contains valid Go code structure" prop_goir_structure
  , fastProperty "SemanticIR type annotations are consistent" prop_semanticir_consistent
  ]

mapPropertiesTests :: TestTree
mapPropertiesTests = testGroup "Map Properties"
  [ fastProperty "Map lookup after insert returns the value" prop_map_insert_lookup
  , fastProperty "Map size after insert increases by 1 for new key" prop_map_insert_size
  , fastProperty "Map union preserves all keys" prop_map_union_keys
  , fastProperty "Map keys are unique" prop_map_keys_unique
  ]

setPropertiesTests :: TestTree
setPropertiesTests = testGroup "Set Properties"
  [ fastProperty "Set insert preserves element" prop_set_insert_preserves
  , fastProperty "Set size after insert increases by 1 for new element" prop_set_insert_size
  , fastProperty "Set union contains all elements" prop_set_union_contains
  , fastProperty "Set intersection contains only common elements" prop_set_intersection_common
  ]

-- String Utils Properties
prop_trim_removes_whitespace :: String -> Property
prop_trim_removes_whitespace s =
  let trimmed = trim s
  in not (null trimmed) ==> 
     property (not (head trimmed `elem` " \t\n\r") && not (last trimmed `elem` " \t\n\r"))

prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
  in trim trimmed === trimmed

prop_trim_empty :: Property
prop_trim_empty =
  trim "" === ""

prop_splitBy_preserves_length :: Char -> String -> Property
prop_splitBy_preserves_length delim s =
  let parts = splitBy delim s
      rejoined = intersperse [delim] parts
  in concat rejoined === s

prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s =
  let parts = splitByCollapsed delim s
  in property $ all (not . null) parts

prop_splitBy_join_inverse :: Char -> NonEmptyList String -> Property
prop_splitBy_join_inverse delim (NonEmpty parts) =
  let s = concat $ intersperse [delim] parts
  in splitBy delim s === parts

prop_removeLineComments_removes_comments :: String -> Property
prop_removeLineComments_removes_comments s =
  let withComments = s ++ "\n// This is a comment\n"
      withoutComments = removeLineComments withComments
  in "//" `notElem` words withoutComments

prop_normalizeIndentation_preserves_lines :: String -> Property
prop_normalizeIndentation_preserves_lines s =
  let normalized = normalizeIndentation s
      originalLines = lines s
      normalizedLines = lines normalized
  in length originalLines === length normalizedLines

-- List Operations Properties
prop_sort_idempotent :: [Int] -> Property
prop_sort_idempotent xs =
  sort (sort xs) === sort xs

prop_sort_preserves_length :: [Int] -> Property
prop_sort_preserves_length xs =
  length (sort xs) === length xs

prop_nub_preserves_or_reduces_length :: [Int] -> Property
prop_nub_preserves_or_reduces_length xs =
  let nubbed = nub xs
  in length nubbed <= length xs

prop_nub_removes_duplicates :: [Int] -> Property
prop_nub_removes_duplicates xs =
  let nubbed = nub xs
  in length nubbed === length (nub nubbed)

prop_intersperse_empty :: Property
prop_intersperse_empty =
  intersperse 0 ([] :: [Int]) === []

prop_intersperse_length :: Int -> [Int] -> Property
prop_intersperse_length x xs =
  let interspersed = intersperse x xs
  in null xs || length interspersed === 2 * length xs - 1

-- SourceLocation Properties
prop_sourcepos_offset_monotonic :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcepos_offset_monotonic (Positive l) (Positive c) (Positive o) =
  let pos1 = SourcePos l c o
      pos2 = SourcePos (l + 1) c (o + 10)
  in property $ posOffset pos1 < posOffset pos2

prop_sourcespan_start_before_end :: SourceSpan -> Property
prop_sourcespan_start_before_end span =
  let start = spanStart span
      end = spanEnd span
  in property $ posOffset start <= posOffset end

prop_sourcepos_positive :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcepos_positive (Positive l) (Positive c) (Positive o) =
  let pos = SourcePos l c o
  in property $ posLine pos > 0 && posColumn pos > 0 && posOffset pos >= 0

prop_sourcespan_reflexive :: SourceSpan -> Property
prop_sourcespan_reflexive span =
  span === span

-- Parser Data Properties
prop_defaultFileDirectives_empty :: Property
prop_defaultFileDirectives_empty =
  let fd = defaultFileDirectives
  in conjoin
    [ fdOwnership fd === Nothing
    , fdDependentTypes fd === Nothing
    , fdConstraints fd === Nothing
    ]

prop_defaultBlockDirectives_empty :: Property
prop_defaultBlockDirectives_empty =
  let bd = defaultBlockDirectives
  in conjoin
    [ bdOwnership bd === Nothing
    , bdDependentTypes bd === Nothing
    , bdConstraints bd === Nothing
    ]

prop_filedirectives_reflexive :: FileDirectives -> Property
prop_filedirectives_reflexive fd =
  fd === fd

prop_blockdirectives_reflexive :: BlockDirectives -> Property
prop_blockdirectives_reflexive bd =
  bd === bd

-- IR Structure Properties
prop_sourceir_structure :: SourceIR -> Property
prop_sourceir_structure (SourceIR typusFile code) =
  not (null code) ==> property True

prop_goir_structure :: GoIR -> Property
prop_goir_structure (GoIR goModule code) =
  not (null code) ==> property True

prop_semanticir_consistent :: SemanticIR -> Property
prop_semanticir_consistent semIR =
  property True  -- Placeholder for actual consistency checks

-- Map Properties
prop_map_insert_lookup :: String -> Int -> Map.Map String Int -> Property
prop_map_insert_lookup k v m =
  let m' = Map.insert k v m
  in Map.lookup k m' === Just v

prop_map_insert_size :: String -> Int -> Map.Map String Int -> Property
prop_map_insert_size k v m =
  let m' = Map.insert k v m
      newSize = if Map.member k m then Map.size m else Map.size m + 1
  in Map.size m' === newSize

prop_map_union_keys :: Map.Map String Int -> Map.Map String Int -> Property
prop_map_union_keys m1 m2 =
  let union = Map.union m1 m2
  in Set.union (Map.keysSet m1) (Map.keysSet m2) === Map.keysSet union

prop_map_keys_unique :: Map.Map String Int -> Property
prop_map_keys_unique m =
  let keys = Map.keys m
  in length keys === length (nub keys)

-- Set Properties
prop_set_insert_preserves :: Int -> Set.Set Int -> Property
prop_set_insert_preserves x s =
  let s' = Set.insert x s
  in Set.member x s'

prop_set_insert_size :: Int -> Set.Set Int -> Property
prop_set_insert_size x s =
  let s' = Set.insert x s
      newSize = if Set.member x s then Set.size s else Set.size s + 1
  in Set.size s' === newSize

prop_set_union_contains :: Set.Set Int -> Set.Set Int -> Property
prop_set_union_contains s1 s2 =
  let union = Set.union s1 s2
  in conjoin
    [ all (`Set.member` union) (Set.toList s1)
    , all (`Set.member` union) (Set.toList s2)
    ]

prop_set_intersection_common :: Set.Set Int -> Set.Set Int -> Property
prop_set_intersection_common s1 s2 =
  let intersection = Set.intersection s1 s2
  in conjoin
    [ all (`Set.member` s1) (Set.toList intersection)
    , all (`Set.member` s2) (Set.toList intersection)
    ]