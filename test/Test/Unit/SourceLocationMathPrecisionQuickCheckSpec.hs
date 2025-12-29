{-# OPTIONS_GHC -Wno-deprecations #-}
module Test.Unit.SourceLocationMathPrecisionQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
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
  , Located(..)
  , locatedAt
  , locatedWithSpan
  , advancePos
  , advancePosBy
  , advancePosByText
  , advancePosByLine
  )

-- ============================================================================
-- Source Location Math Precision Property Tests
-- ============================================================================

-- | Test that position advancement is consistent for single characters
prop_posAfterConsistency :: Char -> SourcePos -> Property
prop_posAfterConsistency c pos =
  let newPos = posAfter c pos
      expectedLine = if c == '\n' then posLine pos + 1 else posLine pos
      expectedColumn = case c of
        '\n' -> 1
        '\t' -> ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
        _ -> posColumn pos + 1
      expectedOffset = posOffset pos + 1
  in counterexample ("Position advancement inconsistent for character: " ++ [c] ++ 
                     " Original: " ++ show pos ++ 
                     " New: " ++ show newPos)
     (posLine newPos === expectedLine .&&.
      posColumn newPos === expectedColumn .&&.
      posOffset newPos === expectedOffset)

-- | Test that tab position advancement follows 8-space tab rule
prop_tabAdvancementFollowsEightSpaceRule :: SourcePos -> Property
prop_tabAdvancementFollowsEightSpaceRule pos =
  let newPos = posAfter '\t' pos
      expectedColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in counterexample ("Tab advancement doesn't follow 8-space rule. " ++
                     "Original column: " ++ show (posColumn pos) ++
                     " New column: " ++ show (posColumn newPos) ++
                     " Expected: " ++ show expectedColumn)
     (posColumn newPos === expectedColumn)

-- | Test that newline resets column to 1 and increments line
prop_newlineResetsColumnAndIncrementsLine :: SourcePos -> Property
prop_newlineResetsColumnAndIncrementsLine pos =
  let newPos = posAfter '\n' pos
  in counterexample ("Newline doesn't reset column to 1 and increment line. " ++
                     "Original: " ++ show pos ++ 
                     " New: " ++ show newPos)
     (posColumn newPos === 1 .&&. posLine newPos === posLine pos + 1)

-- | Test that position advancement by text is cumulative
prop_advancePosByTextIsCumulative :: String -> SourcePos -> Property
prop_advancePosByTextIsCumulative text pos =
  let finalPos = advancePosByText text pos
      manualPos = foldl (flip posAfter) pos text
  in counterexample ("Text advancement not cumulative. Text: " ++ show text ++
                     " Final: " ++ show finalPos ++
                     " Manual: " ++ show manualPos)
     (finalPos === manualPos)

-- | Test that span merging creates valid spans
prop_mergeSpansCreatesValidSpans :: SourceSpan -> SourceSpan -> Property
prop_mergeSpansCreatesValidSpans span1 span2 =
  let merged = mergeSpans span1 span2
  in counterexample ("Merged span is invalid. Span1: " ++ show span1 ++
                     " Span2: " ++ show span2 ++
                     " Merged: " ++ show merged)
     (isValidSpan merged)

-- | Test that span between positions is ordered correctly
prop_spanBetweenIsOrdered :: SourcePos -> SourcePos -> Property
prop_spanBetweenIsOrdered pos1 pos2 =
  let span = spanBetween pos1 pos2
      start = spanStart span
      end = spanEnd span
  in counterexample ("Span between positions not ordered correctly. " ++
                     "Pos1: " ++ show pos1 ++ 
                     " Pos2: " ++ show pos2 ++
                     " Span: " ++ show span)
     (posLine start <= posLine end .&&.
      (posLine start < posLine end .||. posColumn start <= posColumn end))

-- | Test that position at line col creates consistent positions
prop_posAtLineColConsistency :: Int -> Int -> Int -> Property
prop_posAtLineColConsistency line col offset =
  let pos = posAtLineCol line col offset
  in counterexample ("Position at line/col/offset inconsistent. " ++
                     "Line: " ++ show line ++
                     " Col: " ++ show col ++
                     " Offset: " ++ show offset ++
                     " Pos: " ++ show pos)
     (posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === offset)
  .&&. label "positive values" (line > 0 .&&. col >= 0 .&&. offset >= 0 ==> posLine pos > 0)

-- | Test that advancePosBy handles multi-character strings correctly
prop_advancePosByHandlesMultipleChars :: String -> SourcePos -> Property
prop_advancePosByHandlesMultipleChars text pos =
  let advanced = advancePosBy (length text) pos
      manual = foldl (flip posAfter) pos text
  in counterexample ("advancePosBy doesn't handle multiple chars correctly. " ++
                     "Text: " ++ show text ++
                     " Advanced: " ++ show advanced ++
                     " Manual: " ++ show manual)
     (advanced === manual)

-- | Test that advancePosByLine correctly advances by complete lines
prop_advancePosByLineAdvancesByLines :: Int -> SourcePos -> Property
prop_advancePosByLineAdvancesByLines numLines pos =
  let advanced = advancePosByLine numLines pos
  in counterexample ("advancePosByLine doesn't advance by correct number of lines. " ++
                     "Lines: " ++ show numLines ++
                     " Original: " ++ show pos ++
                     " Advanced: " ++ show advanced)
     (posLine advanced === posLine pos + numLines .&&.
      posColumn advanced === posColumn pos .&&.
      posOffset advanced === posOffset pos)

-- | Test that located values preserve their spans
prop_locatedValuesPreserveSpans :: Int -> SourceSpan -> Property
prop_locatedValuesPreserveSpans value span =
  let located = locatedWithSpan span value
  in counterexample ("Located value doesn't preserve span. " ++
                     "Value: " ++ show value ++
                     " Span: " ++ show span ++
                     " Located: " ++ show located)
     (locatedSpan located === span)

-- | Test that span start is always before or equal to span end
prop_spanStartBeforeEnd :: SourceSpan -> Property
prop_spanStartBeforeEnd span =
  let start = spanStart span
      end = spanEnd span
  in counterexample ("Span start is after span end. " ++
                     "Span: " ++ show span)
     (posLine start < posLine end .||.
      (posLine start === posLine end .&&. posColumn start <= posColumn end))

-- | Test that empty span has start and end at same position
prop_emptySpanHasSameStartEnd :: SourcePos -> Property
prop_emptySpanHasSameStartEnd pos =
  let span = spanFrom pos
  in counterexample ("Empty span doesn't have same start and end. " ++
                     "Position: " ++ show pos ++
                     " Span: " ++ show span)
     (spanStart span === spanEnd span)

-- | Test that merging a span with empty span preserves the original
prop_mergeWithEmptySpanPreservesOriginal :: SourceSpan -> Property
prop_mergeWithEmptySpanPreservesOriginal span =
  let empty = emptySpan
      merged = mergeSpans span empty
  in counterexample ("Merging with empty span doesn't preserve original. " ++
                     "Original: " ++ show span ++
                     " Merged: " ++ show merged)
     (merged === span)

-- | Test that position calculation is consistent across different methods
prop_positionCalculationConsistency :: String -> Property
prop_positionCalculationConsistency text =
  let pos1 = advancePosByText text startPos
      pos2 = foldl (flip posAfter) startPos text
      pos3 = advancePosBy (length text) startPos
  in counterexample ("Position calculation inconsistent across methods. " ++
                     "Text: " ++ show text ++
                     " Method1: " ++ show pos1 ++
                     " Method2: " ++ show pos2 ++
                     " Method3: " ++ show pos3)
     (pos1 === pos2 .&&. pos1 === pos3)

-- | Test that locatedAt creates correct spans
prop_locatedAtCreatesCorrectSpans :: Int -> SourcePos -> Property
prop_locatedAtCreatesCorrectSpans value pos =
  let located = locatedAt pos value
      expectedSpan = spanFrom pos
  in counterexample ("locatedAt doesn't create correct spans. " ++
                     "Value: " ++ show value ++
                     " Position: " ++ show pos ++
                     " Expected span: " ++ show expectedSpan ++
                     " Actual span: " ++ show (locatedSpan located))
     (locatedSpan located === expectedSpan)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Source Location Math Precision QuickCheck Tests"
  [ testProperty "Position advancement is consistent for single characters" prop_posAfterConsistency
  , testProperty "Tab advancement follows 8-space rule" prop_tabAdvancementFollowsEightSpaceRule
  , testProperty "Newline resets column to 1 and increments line" prop_newlineResetsColumnAndIncrementsLine
  , testProperty "Text advancement is cumulative" prop_advancePosByTextIsCumulative
  , testProperty "Span merging creates valid spans" prop_mergeSpansCreatesValidSpans
  , testProperty "Span between positions is ordered correctly" prop_spanBetweenIsOrdered
  , testProperty "Position at line/col/offset is consistent" prop_posAtLineColConsistency
  , testProperty "advancePosBy handles multiple chars correctly" prop_advancePosByHandlesMultipleChars
  , testProperty "advancePosByLine advances by correct number of lines" prop_advancePosByLineAdvancesByLines
  , testProperty "Located values preserve their spans" prop_locatedValuesPreserveSpans
  , testProperty "Span start is before or equal to span end" prop_spanStartBeforeEnd
  , testProperty "Empty span has same start and end" prop_emptySpanHasSameStartEnd
  , testProperty "Merge with empty span preserves original" prop_mergeWithEmptySpanPreservesOriginal
  , testProperty "Position calculation is consistent across methods" prop_positionCalculationConsistency
  , testProperty "locatedAt creates correct spans" prop_locatedAtCreatesCorrectSpans
  ]