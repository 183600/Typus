{-# LANGUAGE CPP #-}

module Test.Unit.AdditionalCabalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Property, (===), counterexample, property, (.&&.))
import TestSupport.Arbitrary ()

import Data.List (isInfixOf)
import Data.Char (isSpace)

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , removeLineComments
  , normalizeIndentation
  , breakOn
  )
import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , posAfter
  , spanBetween
  , mergeSpans
  , isValidSpan
  )

-- Test properties for Utils module

-- Property 1: trim should remove leading and trailing whitespace
prop_trim_roundtrip :: String -> Property
prop_trim_roundtrip s = 
  let trimmed = trim s
      hasLeadingOrTrailingSpace = case s of
        [] -> False
        (c:_) | isSpace c -> True
        _ -> case reverse s of
          [] -> False
          (c:_) -> isSpace c
  in if hasLeadingOrTrailingSpace
     then counterexample ("Original: " ++ show s ++ ", Trimmed: " ++ show trimmed) $
          length trimmed < length s
     else counterexample ("No spaces to trim: " ++ show s) $
          trimmed === s

-- Property 2: splitBy should preserve empty segments
prop_splitBy_preserves_empty :: Char -> String -> Property
prop_splitBy_preserves_empty delim str = 
  let parts = splitBy delim str
      expectedLength = length str + 1
  in counterexample ("splitBy '" ++ [delim] ++ "' " ++ show str ++ " = " ++ show parts) $
     length parts === expectedLength

-- Property 3: splitByCollapsed should remove consecutive delimiters
prop_splitByCollapsed_no_consecutive :: Char -> String -> Property
prop_splitByCollapsed_no_consecutive delim str = 
  let parts = splitByCollapsed delim str
      hasConsecutive = delim `elem` str && 
                      ([delim, delim]) `isInfixOf` str
  in if hasConsecutive
     then counterexample ("Should have no consecutive delimiters") $
          all (not . null) parts
     else property True

-- Property 4: splitByComma should be equivalent to splitBy ','
prop_splitByComma_equiv :: String -> Property
prop_splitByComma_equiv str = 
  splitByComma str === splitBy ',' str

-- Property 5: removeLineComments should preserve line structure
prop_removeLineComments_preserves_lines :: String -> Property
prop_removeLineComments_preserves_lines str = 
  let result = removeLineComments str
      originalLines = lines str
      resultLines = lines result
  in counterexample ("Original lines: " ++ show (length originalLines) ++ 
                    ", Result lines: " ++ show (length resultLines)) $
     length resultLines <= length originalLines

-- Property 6: normalizeIndentation should preserve relative indentation
prop_normalizeIndentation_preserves_structure :: String -> Property
prop_normalizeIndentation_preserves_structure str = 
  let result = normalizeIndentation str
      originalLines = lines str
      resultLines = lines result
      -- Check that non-empty lines are preserved
      originalNonEmpty = filter (not . all isSpace) originalLines
      resultNonEmpty = filter (not . all isSpace) resultLines
  in counterexample ("Non-empty lines should be preserved") $
     length resultNonEmpty === length originalNonEmpty

-- Property 7: breakOn should correctly split strings
prop_breakOn_correct_split :: String -> String -> Property
prop_breakOn_correct_split pat str = 
  let (before, after) = breakOn pat str
      combined = before ++ pat ++ after
  in if null pat
     then property True -- Empty pattern is a special case
     else counterexample ("breakOn " ++ show pat ++ " " ++ show str) $
          if pat `isInfixOf` str
          then combined === str
          else (before === str) .&&. (after === "")

-- Test properties for SourceLocation module

-- Property 8: SourcePos should advance correctly with characters
prop_posAfter_consistency :: Char -> SourcePos -> Property
prop_posAfter_consistency c pos = 
  let newPos = posAfter c pos
      offsetIncreased = posOffset newPos == posOffset pos + 1
      lineIncreased = c == '\n' && posLine newPos == posLine pos + 1
      columnIncreased = c /= '\n' && posColumn newPos > posColumn pos
  in counterexample ("posAfter '" ++ [c] ++ "' " ++ show pos ++ " = " ++ show newPos) $
     offsetIncreased && (lineIncreased || columnIncreased)

-- Property 9: spanBetween should create valid spans
prop_spanBetween_valid :: SourcePos -> SourcePos -> Property
prop_spanBetween_valid pos1 pos2 = 
  let sp = spanBetween pos1 pos2
      valid = isValidSpan sp
  in counterexample ("spanBetween " ++ show pos1 ++ " " ++ show pos2 ++ " = " ++ show sp) $
     valid

-- Property 10: mergeSpans should contain both original spans
prop_mergeSpans_contains_both :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_contains_both span1 span2 = 
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in counterexample ("mergeSpans should contain both spans") $
     mergedStart <= start1 && mergedEnd >= end1 &&
     mergedStart <= start2 && mergedEnd >= end2

tests :: TestTree
tests = testGroup "Additional Cabal QuickCheck Tests"
  [ testGroup "Utils Module Properties"
    [ testProperty "trim roundtrip property" prop_trim_roundtrip
    , testProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
    , testProperty "splitByCollapsed removes consecutive delimiters" prop_splitByCollapsed_no_consecutive
    , testProperty "splitByComma equivalence" prop_splitByComma_equiv
    , testProperty "removeLineComments preserves lines" prop_removeLineComments_preserves_lines
    , testProperty "normalizeIndentation preserves structure" prop_normalizeIndentation_preserves_structure
    , testProperty "breakOn correct split" prop_breakOn_correct_split
    ]
  , testGroup "SourceLocation Module Properties"
    [ testProperty "posAfter consistency" prop_posAfter_consistency
    , testProperty "spanBetween creates valid spans" prop_spanBetween_valid
    , testProperty "mergeSpans contains both spans" prop_mergeSpans_contains_both
    ]
  ]