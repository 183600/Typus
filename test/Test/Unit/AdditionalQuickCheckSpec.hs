{-# LANGUAGE CPP #-}

module Test.Unit.AdditionalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as L
import Data.List (isInfixOf)

import Parser (FileDirectives(..), BlockDirectives(..), CodeBlock(..), defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn, posOffset, spanStart, spanEnd, posAfter, mergeSpans, isValidSpan)
import Utils (trim, splitBy, removeLineComments, splitByComma, normalizeIndentation)
import TestSupport.Arbitrary ()

prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  trim (trim s) === trim s

prop_splitBy_concat :: Char -> [String] -> Property
prop_splitBy_concat delim parts =
  delim `notElem` L.concat parts ==>
  L.length (splitBy delim (L.concat parts)) === 1

prop_removeLineComments_preserves_code :: String -> Property
prop_removeLineComments_preserves_code s =
  not ("//" `L.isInfixOf` s) ==>
  removeLineComments s === s

prop_posAfter_newline_increments_line :: SourcePos -> Property
prop_posAfter_newline_increments_line pos =
  posLine pos > 0 && posColumn pos > 0 && posOffset pos >= 0 ==>
  let pos' = posAfter '\n' pos
  in posLine pos' === posLine pos + 1 .&&. posColumn pos' === 1

prop_mergeSpans_contains_both :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_contains_both span1 span2 =
  isValidSpan span1 && isValidSpan span2 ==>
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in posOffset mergedStart <= min (posOffset start1) (posOffset start2) .&&.
     posOffset mergedEnd >= max (posOffset end1) (posOffset end2)

prop_isValidSpan_reflexive :: SourceSpan -> Property
prop_isValidSpan_reflexive span =
  isValidSpan span === (posOffset (spanStart span) <= posOffset (spanEnd span))

prop_defaultFileDirectives_all_nothing :: Property
prop_defaultFileDirectives_all_nothing =
  let fd = defaultFileDirectives
  in fdOwnership fd === Nothing .&&.
     fdDependentTypes fd === Nothing .&&.
     fdConstraints fd === Nothing

prop_defaultBlockDirectives_all_nothing :: Property
prop_defaultBlockDirectives_all_nothing =
  let bd = defaultBlockDirectives
  in bdOwnership bd === Nothing .&&.
     bdDependentTypes bd === Nothing .&&.
     bdConstraints bd === Nothing

prop_normalizeIndentation_empty :: Property
prop_normalizeIndentation_empty =
  normalizeIndentation "" === ""

prop_map_insert_lookup :: String -> Int -> Property
prop_map_insert_lookup key value =
  Map.lookup key (Map.insert key value Map.empty) === Just value

tests :: TestTree
tests = testGroup "Additional QuickCheck Tests"
  [ fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "splitBy on concatenated parts without delimiter" prop_splitBy_concat
  , fastProperty "removeLineComments preserves code without //" prop_removeLineComments_preserves_code
  , fastProperty "posAfter newline increments line L.and resets column" prop_posAfter_newline_increments_line
  , fastProperty "mergeSpans contains both spans" prop_mergeSpans_contains_both
  , fastProperty "isValidSpan checks start <= end" prop_isValidSpan_reflexive
  , fastProperty "defaultFileDirectives has L.all Nothing" prop_defaultFileDirectives_all_nothing
  , fastProperty "defaultBlockDirectives has L.all Nothing" prop_defaultBlockDirectives_all_nothing
  , fastProperty "normalizeIndentation on empty string" prop_normalizeIndentation_empty
  , fastProperty "Map insert then lookup returns value" prop_map_insert_lookup
  ]
