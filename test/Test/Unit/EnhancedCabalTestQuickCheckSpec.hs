{-# LANGUAGE CPP #-}

module Test.Unit.EnhancedCabalTestQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub, isInfixOf, isPrefixOf)
import Data.Char (isAlpha, isDigit, isSpace)

import Parser (FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import Compiler.TypeChecker (Type(..), TypeEnv(..), buildTypeEnvFromPairs, typesEqual, isSubtype)
import SourceLocation (SourcePos(..), SourceSpan(..), spanStart, spanEnd, posLine, posColumn)
import Utils (trim, splitBy, splitByCollapsed, removeLineComments)
import TestSupport.Arbitrary ()
import TestSupport.ExtendedArbitrary ()

tests :: TestTree
tests = testGroup "Enhanced Cabal Test QuickCheck"
  [ utilsProperties
  , sourceLocationProperties
  , typeSystemProperties
  , parserDirectivesProperties
  , dataStructureProperties
  , stringManipulationProperties
  , typeRelationProperties
  , advancedTypeProperties
  ]

-- Utils module properties
utilsProperties :: TestTree
utilsProperties = testGroup "Utils Properties"
  [ fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "splitBy preserves content" prop_splitBy_preserves
  , fastProperty "splitByCollapsed removes empty" prop_splitByCollapsed_no_empty
  , fastProperty "removeLineComments preserves non-comments" prop_remove_comments_safe
  ]

prop_trim_idempotent :: String -> Bool
prop_trim_idempotent s = trim (trim s) == trim s

prop_splitBy_preserves :: Char -> NonEmptyList Char -> Bool
prop_splitBy_preserves delim (NonEmpty s) =
  let parts = splitBy delim s
      totalLen = sum (map length parts) + length parts - 1
  in totalLen >= length s - 1

prop_splitByCollapsed_no_empty :: Char -> String -> Bool
prop_splitByCollapsed_no_empty delim s =
  all (not . null) (splitByCollapsed delim s)

prop_remove_comments_safe :: String -> Bool
prop_remove_comments_safe s =
  let cleaned = removeLineComments s
  in length cleaned <= length s

-- SourceLocation properties
sourceLocationProperties :: TestTree
sourceLocationProperties = testGroup "SourceLocation Properties"
  [ fastProperty "SourceSpan start before end" prop_span_ordering
  , fastProperty "position line is positive" prop_pos_line_positive
  , fastProperty "position column is positive" prop_pos_column_positive
  ]

prop_span_ordering :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_span_ordering (Positive l1) (Positive c1) (Positive l2) (Positive c2) =
  let start = SourcePos l1 c1 0
      end = SourcePos (l1 + l2) (c1 + c2) 0
      span = SourceSpan start end
  in property $ posLine (spanStart span) <= posLine (spanEnd span)

prop_pos_line_positive :: Positive Int -> Positive Int -> Bool
prop_pos_line_positive (Positive l) (Positive c) =
  let pos = SourcePos l c 0
  in posLine pos > 0

prop_pos_column_positive :: Positive Int -> Positive Int -> Bool
prop_pos_column_positive (Positive l) (Positive c) =
  let pos = SourcePos l c 0
  in posColumn pos > 0

-- Type system properties
typeSystemProperties :: TestTree
typeSystemProperties = testGroup "Type System Properties"
  [ fastProperty "type equality is reflexive" prop_type_reflexive
  , fastProperty "type equality is symmetric" prop_type_symmetric
  , fastProperty "type equality is transitive" prop_type_transitive
  , fastProperty "subtype relation is reflexive" prop_subtype_reflexive
  ]

prop_type_reflexive :: Type -> Bool
prop_type_reflexive t = typesEqual t t

prop_type_symmetric :: Type -> Type -> Bool
prop_type_symmetric t1 t2 = typesEqual t1 t2 == typesEqual t2 t1

prop_type_transitive :: Type -> Type -> Type -> Property
prop_type_transitive t1 t2 t3 =
  (typesEqual t1 t2 && typesEqual t2 t3) ==> typesEqual t1 t3

prop_subtype_reflexive :: Type -> Bool
prop_subtype_reflexive t = isSubtype t t

-- Parser directives properties
parserDirectivesProperties :: TestTree
parserDirectivesProperties = testGroup "Parser Directives Properties"
  [ fastProperty "default file directives are empty" prop_default_file_directives
  , fastProperty "default block directives are empty" prop_default_block_directives
  ]

prop_default_file_directives :: Bool
prop_default_file_directives =
  let fd = defaultFileDirectives
  in fdOwnership fd == Nothing && fdDependentTypes fd == Nothing && fdConstraints fd == Nothing

prop_default_block_directives :: Bool
prop_default_block_directives =
  let bd = defaultBlockDirectives
  in bdOwnership bd == Nothing && bdDependentTypes bd == Nothing && bdConstraints bd == Nothing

-- Data structure properties
dataStructureProperties :: TestTree
dataStructureProperties = testGroup "Data Structure Properties"
  [ fastProperty "map insert idempotent" prop_map_insert_idempotent
  , fastProperty "set union commutative" prop_set_union_commutative
  , fastProperty "set intersection commutative" prop_set_intersection_commutative
  ]

prop_map_insert_idempotent :: Int -> String -> Map.Map Int String -> Bool
prop_map_insert_idempotent k v m =
  let m1 = Map.insert k v m
      m2 = Map.insert k v m1
  in m1 == m2

prop_set_union_commutative :: Set.Set Int -> Set.Set Int -> Bool
prop_set_union_commutative s1 s2 = Set.union s1 s2 == Set.union s2 s1

prop_set_intersection_commutative :: Set.Set Int -> Set.Set Int -> Bool
prop_set_intersection_commutative s1 s2 = Set.intersection s1 s2 == Set.intersection s2 s1

-- String manipulation properties
stringManipulationProperties :: TestTree
stringManipulationProperties = testGroup "String Manipulation Properties"
  [ fastProperty "reverse twice is identity" prop_reverse_identity
  , fastProperty "isPrefixOf reflexive" prop_isPrefixOf_reflexive
  , fastProperty "isInfixOf reflexive" prop_isInfixOf_reflexive
  ]

prop_reverse_identity :: [Int] -> Bool
prop_reverse_identity xs = reverse (reverse xs) == xs

prop_isPrefixOf_reflexive :: String -> Bool
prop_isPrefixOf_reflexive s = s `isPrefixOf` s

prop_isInfixOf_reflexive :: String -> Bool
prop_isInfixOf_reflexive s = s `isInfixOf` s

-- Type relation properties
typeRelationProperties :: TestTree
typeRelationProperties = testGroup "Type Relation Properties"
  [ fastProperty "TypeEnv lookup after insert" prop_typeenv_lookup
  , fastProperty "buildTypeEnv from pairs" prop_build_typeenv
  ]

prop_typeenv_lookup :: [(String, Type)] -> String -> Type -> Bool
prop_typeenv_lookup pairs k v =
  let env = buildTypeEnvFromPairs ((k, v) : pairs)
      found = Map.lookup k (varTypes env)
  in found == Just v

prop_build_typeenv :: [(String, Type)] -> Bool
prop_build_typeenv pairs =
  let env = buildTypeEnvFromPairs pairs
  in Map.size (varTypes env) <= length pairs

-- Advanced type properties
advancedTypeProperties :: TestTree
advancedTypeProperties = testGroup "Advanced Type Properties"
  [ fastProperty "sort is idempotent" prop_sort_idempotent
  , fastProperty "nub preserves order" prop_nub_order
  ]

prop_sort_idempotent :: [Int] -> Bool
prop_sort_idempotent xs = let sorted = sort xs in sort sorted == sorted

prop_nub_order :: [Int] -> Bool
prop_nub_order xs =
  let unique = nub xs
      indices = map (\x -> head [i | (i, y) <- zip [0..] xs, y == x]) unique
  in indices == sort indices
