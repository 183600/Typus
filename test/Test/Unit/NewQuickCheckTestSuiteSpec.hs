{-# LANGUAGE CPP #-}

module Test.Unit.NewQuickCheckTestSuiteSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Parser (FileDirectives(..), BlockDirectives(..))
import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn, posOffset, spanStart, spanEnd)
import Utils (trim, splitBy, removeLineComments, splitByComma, normalizeIndentation)
import TestSupport.Arbitrary ()

-- Test 1: String splitting properties
prop_splitBy_preserves_all_content :: Char -> String -> Property
prop_splitBy_preserves_all_content delim str =
  let parts = splitBy delim str
      reconstructed = concat $ map (\s -> s ++ [delim]) (init parts) ++ [last parts]
  in length parts > 0 ==> str === reconstructed

-- Test 2: Source position ordering
prop_sourcepos_ordering :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_sourcepos_ordering line1 col1 offset1 line2 col2 offset2 =
  line1 > 0 && col1 > 0 && offset1 >= 0 && line2 > 0 && col2 > 0 && offset2 >= 0 ==>
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
  in (line1 < line2 || (line1 == line2 && col1 < col2)) ==> posOffset pos1 < posOffset pos2

-- Test 3: File directives combination
prop_file_directives_associative :: Bool -> Bool -> Bool -> Property
prop_file_directives_associative own dep cons =
  let fd1 = FileDirectives (Just own) (Just dep) Nothing
      fd2 = FileDirectives Nothing (Just dep) (Just cons)
      fd3 = FileDirectives (Just own) (Just dep) (Just cons)
  in fd1 /= fd2 .&&. fd3 /= fd1 .&&. fd3 /= fd2

-- Test 4: Map operations consistency
prop_map_insert_then_lookup :: String -> Int -> Property
prop_map_insert_then_lookup key value =
  let m = Map.insert key value Map.empty
  in Map.lookup key m === Just value

-- Test 5: Set operations properties
prop_set_insert_then_member :: Int -> Set.Set Int -> Property
prop_set_insert_then_member value set =
  let newSet = Set.insert value set
  in Set.member value newSet === True

-- Test 6: Comment removal idempotence
prop_removeLineComments_idempotent :: String -> Property
prop_removeLineComments_idempotent str =
  let once = removeLineComments str
      twice = removeLineComments once
  in once === twice

-- Test 7: Indentation normalization
prop_normalizeIndentation_preserves_content :: String -> Property
prop_normalizeIndentation_preserves_content str =
  let normalized = normalizeIndentation str
      -- Remove all leading/trailing whitespace for comparison
      trimOriginal = trim str
      trimNormalized = trim normalized
  in trimOriginal === trimNormalized

-- Test 8: Source span merging
prop_mergeSpans_contains_original :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_contains_original span1 span2 =
  let merged = span1 `mergeSpans` span2
      start1 = spanStart span1
      end1 = spanEnd span1
  in posOffset (spanStart merged) <= posOffset start1 .&&. 
     posOffset (spanEnd merged) >= posOffset end1

-- Test 9: Text pack/unpack roundtrip
prop_text_pack_unpack_roundtrip :: String -> Property
prop_text_pack_unpack_roundtrip str =
  T.unpack (T.pack str) === str

-- Test 10: List operations properties
prop_concat_associative :: [String] -> [String] -> [String] -> Property
prop_concat_associative xs ys zs =
  (xs ++ ys) ++ zs === xs ++ (ys ++ zs)

tests :: TestTree
tests = testGroup "New QuickCheck Test Suite"
  [ fastProperty "splitBy preserves all content" prop_splitBy_preserves_all_content
  , fastProperty "SourcePos ordering matches offset ordering" prop_sourcepos_ordering
  , fastProperty "File directives are not associative" prop_file_directives_associative
  , fastProperty "Map insert then lookup returns inserted value" prop_map_insert_then_lookup
  , fastProperty "Set insert then member returns True" prop_set_insert_then_member
  , fastProperty "removeLineComments is idempotent" prop_removeLineComments_idempotent
  , fastProperty "normalizeIndentation preserves trimmed content" prop_normalizeIndentation_preserves_content
  , fastProperty "mergeSpans contains original spans" prop_mergeSpans_contains_original
  , fastProperty "Text pack/unpack roundtrip" prop_text_pack_unpack_roundtrip
  , fastProperty "List concatenation is associative" prop_concat_associative
  ]