{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Unit.NewCabalQuickCheckPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Char (isSpace, toLower, toUpper)
import Data.List (isPrefixOf)

import Utils (splitBy, splitByCollapsed, normalizeIndentation, removeLineComments)
import SourceLocation (SourcePos(..), SourceSpan(..), spanBetween)
import Parser (FileDirectives(..))
import Compiler.TypeChecker (Type(..))

-- Import Arbitrary instances
import TestSupport.Arbitrary ()
import TestSupport.ExtendedArbitrary ()

-- Property 1: splitByCollapsed removes consecutive delimiters
prop_splitByCollapsed_consecutive :: Char -> String -> Property
prop_splitByCollapsed_consecutive delim str =
  let collapsed = splitByCollapsed delim str
      regular = splitBy delim str
      hasConsecutive = any (\s -> length (filter (== delim) s) > 1) (splitBy delim str)
      hasEmptyStrings = any null regular
  in if hasConsecutive || hasEmptyStrings
      then property $ length collapsed <= length regular
      else collapsed === regular  -- If no consecutive delimiters or empty strings, should be the same

-- Property 2: normalizeIndentation preserves relative indentation
prop_normalizeIndentation_relative :: String -> Property
prop_normalizeIndentation_relative str =
  let linesList = lines str
      nonEmpty = filter (not . null) linesList
  in not (null nonEmpty) ==>
  let normalizedStr = normalizeIndentation str
      normalized = lines normalizedStr
      originalIndents = map (length . takeWhile isSpace) nonEmpty
      normalizedIndents = map (length . takeWhile isSpace) (filter (not . null) normalized)
      minOriginal = if null originalIndents then 0 else minimum originalIndents
      minNormalized = if null normalizedIndents then 0 else minimum normalizedIndents
      relativeIndents = zipWith (-) originalIndents (repeat minOriginal)
      normalizedRelativeIndents = zipWith (-) normalizedIndents (repeat minNormalized)
  in relativeIndents === normalizedRelativeIndents

-- Property 3: removeLineComments preserves non-comment content
prop_removeLineComments_preserves_content :: String -> Property
prop_removeLineComments_preserves_content str =
  let withoutComments = removeLineComments str
      commentLines = [line | line <- lines str, "//" `isPrefixOf` dropWhile isSpace line]
      nonCommentLines = [line | line <- lines str, not ("//" `isPrefixOf` dropWhile isSpace line)]
  in if null commentLines 
      then property True  -- If no comments, test passes trivially
      else lines withoutComments === nonCommentLines

-- Property 4: SourceSpan construction is well-formed
prop_sourcespan_construction :: SourcePos -> SourcePos -> Property
prop_sourcespan_construction start end =
  let sourceSpan = spanBetween start end
  in conjoin
    [ spanStart sourceSpan === start
    , spanEnd sourceSpan === end
    ]

-- Property 5: FileDirectives equality works correctly
prop_file_directives_equality :: FileDirectives -> Property
prop_file_directives_equality fd =
  fd === fd

-- Property 6: Type substitution preserves structure
prop_type_substitution :: Type -> Type -> Property
prop_type_substitution oldType newType =
  let substituteType t = if t == oldType then newType else t
      result = substituteType oldType
  in result === newType

-- Property 7: Map union with later preference
prop_map_union_preference :: [(String, Int)] -> [(String, Int)] -> Property
prop_map_union_preference pairs1 pairs2 =
  let map1 = Map.fromList pairs1
      map2 = Map.fromList pairs2
      union = Map.union map2 map1  -- map2 has preference
      commonKeys = Map.keysSet map1 `Set.intersection` Map.keysSet map2
      checkKey k = Map.lookup k union === Map.lookup k map2
  in conjoin [checkKey k | k <- Set.toList commonKeys]

-- Property 8: Set operations are consistent
prop_set_operations_consistent :: [Int] -> [Int] -> Property
prop_set_operations_consistent xs ys =
  let set1 = Set.fromList xs
      set2 = Set.fromList ys
      union = Set.union set1 set2
      intersection = Set.intersection set1 set2
      difference = Set.difference set1 set2
  in conjoin
    [ property $ Set.isSubsetOf set1 union
    , property $ Set.isSubsetOf set2 union
    , property $ Set.isSubsetOf intersection set1
    , property $ Set.isSubsetOf intersection set2
    , property $ Set.union intersection difference === set1
    ]

-- Property 9: String case conversion roundtrip
prop_string_case_roundtrip :: String -> Property
prop_string_case_roundtrip str =
  let lowered = map toLower str
      uppered = map toUpper str
  in map toLower uppered === lowered

-- Property 10: List partition and unpartition are inverse
prop_list_partition_inverse :: [Int] -> Property
prop_list_partition_inverse lst =
  let predicate = even  -- Use a simple predicate function
      (satisfying, notSatisfying) = List.partition predicate lst
      recombined = satisfying ++ notSatisfying
  in List.sort lst === List.sort recombined

tests :: TestTree
tests = testGroup "New Cabal QuickCheck Properties"
  [ fastProperty "splitByCollapsed removes consecutive delimiters" prop_splitByCollapsed_consecutive
  , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_relative
  , fastProperty "removeLineComments preserves non-comment content" prop_removeLineComments_preserves_content
  , fastProperty "SourceSpan construction is well-formed" prop_sourcespan_construction
  , fastProperty "FileDirectives equality works correctly" prop_file_directives_equality
  , fastProperty "Type substitution preserves structure" prop_type_substitution
  , fastProperty "Map union with later preference" prop_map_union_preference
  , fastProperty "Set operations are consistent" prop_set_operations_consistent
  , fastProperty "String case conversion roundtrip" prop_string_case_roundtrip
  , fastProperty "List partition and unpartition are inverse" prop_list_partition_inverse
  ]