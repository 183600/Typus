{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationNewQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, vectorOf, elements)
import qualified Data.List as List

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , posAtLineCol
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedSpan
  , locatedPos
  , mapLocated
  , advancePos
  , advancePosBy
  , advancePosByText
  , advancePosByLine
  , toErrorLocation
  , toErrorLocationWithSpan
  )

-- Arbitrary instances for SourceLocation types
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    offset <- choose (0, 10000)
    return $ SourcePos line col offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endOffset <- choose (0, 100)
    let end = SourcePos (posLine start) (posColumn start + endOffset) (posOffset start + endOffset)
    return $ SourceSpan start end

-- Generator for non-empty strings without newlines
genNonEmptyStringNoNewlines :: Gen String
genNonEmptyStringNoNewlines = do
  size <- choose (1, 20)
  vectorOf size $ elements $ filter (/= '\n') [' '..'~']

-- Generator for strings that may contain newlines
genStringWithNewlines :: Gen String
genStringWithNewlines = do
  size <- choose (0, 20)
  vectorOf size $ elements ['\n', ' ', 'a', 'b', 'c', '1', '2', '3']

-- Property: startPos has correct initial values
prop_startPos_correct :: Property
prop_startPos_correct =
  property $ posLine startPos === 1 .&&.
             posColumn startPos === 1 .&&.
             posOffset startPos === 0

-- Property: posAfter advances line correctly for newline
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let newPos = posAfter '\n' pos
  in property $ posLine newPos === posLine pos + 1 .&&.
             posColumn newPos === 1 .&&.
             posOffset newPos === posOffset pos + 1

-- Property: posAfter advances column correctly for regular characters
prop_posAfter_regular_char :: SourcePos -> Char -> Property
prop_posAfter_regular_char pos c =
  c /= '\n' && c /= '\t' ==> 
  let newPos = posAfter c pos
  in property $ posLine newPos === posLine pos .&&.
             posColumn newPos === posColumn pos + 1 .&&.
             posOffset newPos === posOffset pos + 1

-- Property: posAfter handles tab correctly (8-column alignment)
prop_posAfter_tab :: SourcePos -> Property
prop_posAfter_tab pos =
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine newPos === posLine pos .&&.
             posColumn newPos === expectedCol .&&.
             posOffset newPos === posOffset pos + 1

-- Property: posAt creates position with correct line and column
prop_posAt_correct :: Int -> Int -> Property
prop_posAt_correct line col =
  line > 0 && col > 0 ==>
  let pos = posAt line col
  in property $ posLine pos === line .&&.
             posColumn pos === col .&&.
             posOffset pos === 0

-- Property: posAtLineCol creates position with all fields set
prop_posAtLineCol_correct :: Int -> Int -> Int -> Property
prop_posAtLineCol_correct line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
  in property $ posLine pos === line .&&.
             posColumn pos === col .&&.
             posOffset pos === offset

-- Property: emptySpan creates span with same start and end
prop_emptySpan_same_start_end :: SourcePos -> Property
prop_emptySpan_same_start_end pos =
  let span = emptySpan pos
  in property $ spanStart span === pos .&&.
             spanEnd span === pos

-- Property: spanFrom creates empty span at position
prop_spanFrom_empty_at_pos :: SourcePos -> Property
prop_spanFrom_empty_at_pos pos =
  let span = spanFrom pos
  in property $ spanStart span === pos .&&.
             spanEnd span === pos

-- Property: spanTo creates empty span at position
prop_spanTo_empty_at_pos :: SourcePos -> Property
prop_spanTo_empty_at_pos pos =
  let span = spanTo pos
  in property $ spanStart span === pos .&&.
             spanEnd span === pos

-- Property: spanBetween creates span with correct start and end
prop_spanBetween_correct :: SourcePos -> SourcePos -> Property
prop_spanBetween_correct start end =
  let span = spanBetween start end
  in property $ spanStart span === start .&&.
             spanEnd span === end

-- Property: mergeSpans creates span with min start and max end
prop_mergeSpans_min_max :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_min_max span1 span2 =
  let merged = mergeSpans span1 span2
  in property $ spanStart merged === min (spanStart span1) (spanStart span2) .&&.
             spanEnd merged === max (spanEnd span1) (spanEnd span2)

-- Property: isValidSpan returns true when start <= end
prop_isValidSpan_true :: SourcePos -> Int -> Property
prop_isValidSpan_true pos offset =
  offset >= 0 ==>
  let end = SourcePos (posLine pos) (posColumn pos + offset) (posOffset pos + offset)
      span = SourceSpan pos end
  in property $ isValidSpan span === True

-- Property: isValidSpan returns false when start > end
prop_isValidSpan_false :: SourcePos -> Int -> Property
prop_isValidSpan_false pos offset =
  offset > 0 ==>
  let end = SourcePos (posLine pos) (max 1 (posColumn pos - offset)) (max 0 (posOffset pos - offset))
      span = SourceSpan pos end
  in pos < end ==> property $ isValidSpan span === False

-- Property: locatedAt creates located value with correct position
prop_locatedAt_correct :: SourcePos -> String -> Property
prop_locatedAt_correct pos value =
  let located = locatedAt pos value
  in property $ locatedValue located === value .&&.
             locatedPos located === pos .&&.
             locatedSpan located === emptySpan pos

-- Property: locatedWithSpan creates located value with correct span
prop_locatedWithSpan_correct :: SourceSpan -> String -> Property
prop_locatedWithSpan_correct span value =
  let located = locatedWithSpan span value
  in property $ locatedValue located === value .&&.
             locatedSpan located === span .&&.
             locatedPos located === spanStart span

-- Property: mapLocated applies function to value
prop_mapLocated_applies_function :: SourceSpan -> Int -> Property
prop_mapLocated_applies_function span value =
  let located = locatedWithSpan span value
      mapped = mapLocated (*2) located
  in property $ locatedValue mapped === value * 2 .&&.
             locatedSpan mapped === span

-- Property: advancePosBy advances position by string
prop_advancePosBy_string :: SourcePos -> String -> Property
prop_advancePosBy_string pos str =
  let advanced = advancePosBy str pos
      expected = foldl (flip posAfter) pos str
  in property $ advanced === expected

-- Property: advancePosByText advances position by text
prop_advancePosByText_text :: SourcePos -> String -> Property
prop_advancePosByText_text pos str =
  let advanced = advancePosByText (toEnum <$> str) pos
      expected = advancePosBy str pos
  in property $ advanced === expected

-- Property: advancePosByLine advances line number
prop_advancePosByLine_advances_line :: SourcePos -> Int -> Property
prop_advancePosByLine_advances_line pos numLines =
  numLines >= 0 ==>
  let advanced = advancePosByLine numLines pos
  in property $ posLine advanced === posLine pos + numLines .&&.
             posColumn advanced === 1

-- Property: toErrorLocation creates error location with correct fields
prop_toErrorLocation_correct :: SourcePos -> Property
prop_toErrorLocation_correct pos =
  let errLoc = toErrorLocation pos
  in property $ line errLoc === posLine pos .&&.
             column errLoc === posColumn pos

-- Property: toErrorLocationWithSpan creates error location with range
prop_toErrorLocationWithSpan_correct :: SourceSpan -> Property
prop_toErrorLocationWithSpan_correct span =
  let errLoc = toErrorLocationWithSpan span
  in property $ line errLoc === posLine (spanStart span) .&&.
             column errLoc === posColumn (spanStart span) .&&.
             endLine errLoc === Just (posLine (spanEnd span)) .&&.
             endColumn errLoc === Just (posColumn (spanEnd span))

-- Property: Located values are equal if values and positions are equal
prop_located_equality :: SourceSpan -> String -> SourceSpan -> String -> Property
prop_located_equality span1 value1 span2 value2 =
  let loc1 = locatedWithSpan span1 value1
      loc2 = locatedWithSpan span2 value2
  in property $ (loc1 == loc2) === (value1 == value2 && span1 == span2)

-- Property: Located functor law: fmap id = id
prop_located_functor_identity :: SourceSpan -> Int -> Property
prop_located_functor_identity span value =
  let located = locatedWithSpan span value
      mapped = mapLocated id located
  in property $ mapped === located

-- Property: Located functor law: fmap (f . g) = fmap f . fmap g
prop_located_functor_composition :: SourceSpan -> Int -> Property
prop_located_functor_composition span value =
  let located = locatedWithSpan span value
      f = (*2)
      g = (+1)
      mapped1 = mapLocated (f . g) located
      mapped2 = mapLocated f (mapLocated g located)
  in property $ mapped1 === mapped2

-- Property: mergeSpans is commutative
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 === merged2

-- Property: mergeSpans is associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  let merged1 = mergeSpans (mergeSpans span1 span2) span3
      merged2 = mergeSpans span1 (mergeSpans span2 span3)
  in property $ merged1 === merged2

-- Property: spanBetween with same positions creates empty span
prop_spanBetween_same_positions :: SourcePos -> Property
prop_spanBetween_same_positions pos =
  let span = spanBetween pos pos
  in property $ spanStart span === pos .&&.
             spanEnd span === pos

-- Property: advancePos with empty string returns same position
prop_advancePos_empty_string :: SourcePos -> Property
prop_advancePos_empty_string pos =
  let advanced = advancePosBy "" pos
  in property $ advanced === pos

-- Property: advancePosByLine with 0 lines returns same position with column 1
prop_advancePosByLine_zero :: SourcePos -> Property
prop_advancePosByLine_zero pos =
  let advanced = advancePosByLine 0 pos
  in property $ posLine advanced === posLine pos .&&.
             posColumn advanced === 1

tests :: TestTree
tests = testGroup "SourceLocation New QuickCheck Tests"
  [ fastProperty "startPos has correct initial values" prop_startPos_correct
  , fastProperty "posAfter advances line correctly for newline" prop_posAfter_newline
  , fastProperty "posAfter advances column correctly for regular characters" prop_posAfter_regular_char
  , fastProperty "posAfter handles tab correctly" prop_posAfter_tab
  , fastProperty "posAt creates position with correct line and column" prop_posAt_correct
  , fastProperty "posAtLineCol creates position with all fields set" prop_posAtLineCol_correct
  , fastProperty "emptySpan creates span with same start and end" prop_emptySpan_same_start_end
  , fastProperty "spanFrom creates empty span at position" prop_spanFrom_empty_at_pos
  , fastProperty "spanTo creates empty span at position" prop_spanTo_empty_at_pos
  , fastProperty "spanBetween creates span with correct start and end" prop_spanBetween_correct
  , fastProperty "mergeSpans creates span with min start and max end" prop_mergeSpans_min_max
  , fastProperty "isValidSpan returns true when start <= end" prop_isValidSpan_true
  , fastProperty "isValidSpan returns false when start > end" prop_isValidSpan_false
  , fastProperty "locatedAt creates located value with correct position" prop_locatedAt_correct
  , fastProperty "locatedWithSpan creates located value with correct span" prop_locatedWithSpan_correct
  , fastProperty "mapLocated applies function to value" prop_mapLocated_applies_function
  , fastProperty "advancePosBy advances position by string" prop_advancePosBy_string
  , fastProperty "advancePosByText advances position by text" prop_advancePosByText_text
  , fastProperty "advancePosByLine advances line number" prop_advancePosByLine_advances_line
  , fastProperty "toErrorLocation creates error location with correct fields" prop_toErrorLocation_correct
  , fastProperty "toErrorLocationWithSpan creates error location with range" prop_toErrorLocationWithSpan_correct
  , fastProperty "Located values are equal if values and positions are equal" prop_located_equality
  , fastProperty "Located functor law: fmap id = id" prop_located_functor_identity
  , fastProperty "Located functor law: fmap (f . g) = fmap f . fmap g" prop_located_functor_composition
  , fastProperty "mergeSpans is commutative" prop_mergeSpans_commutative
  , fastProperty "mergeSpans is associative" prop_mergeSpans_associative
  , fastProperty "spanBetween with same positions creates empty span" prop_spanBetween_same_positions
  , fastProperty "advancePos with empty string returns same position" prop_advancePos_empty_string
  , fastProperty "advancePosByLine with 0 lines returns same position with column 1" prop_advancePosByLine_zero
  ]