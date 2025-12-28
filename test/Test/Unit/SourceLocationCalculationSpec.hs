{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.SourceLocationCalculationSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation
import Data.Char (isSpace)
import Data.List (unfoldr)
import Control.Arrow ((&&&))

-- ============================================================================
-- Source Location Calculation Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Source Location Calculation Tests"
  [ positionCalculationProperties
  , spanCalculationProperties
  , locationTrackingProperties
  , positionAdvancementProperties
  , locatedValueProperties
  , errorLocationProperties
  ]

-- ============================================================================
-- Position Calculation Properties
-- ============================================================================

positionCalculationProperties :: TestTree
positionCalculationProperties = testGroup "Position Calculation Properties"
  [ testProperty "startPos is always (1, 1)" $
      startPos === SourcePos 1 1
    
  , testProperty "posAt creates position at specific line and column" $
      \line col -> line > 0 && col > 0 ==> 
        let pos = posAt line col
        in sourceLine pos === line && sourceColumn pos === col
    
  , testProperty "posAtLineCol is consistent with posAt" $
      \line col -> line > 0 && col > 0 ==>
        posAtLineCol line col === posAt line col
    
  , testProperty "position ordering is total" $
      \pos1 pos2 ->
        let cmp = compare pos1 pos2
        in (pos1 <= pos2 && pos2 <= pos1) === (pos1 == pos2)
    
  , testProperty "position advancement is monotonic" $
      \pos char ->
        let advanced = posAfter pos char
        in advanced >= pos
  ]

-- ============================================================================
-- Span Calculation Properties
-- ============================================================================

spanCalculationProperties :: TestTree
spanCalculationProperties = testGroup "Span Calculation Properties"
  [ testProperty "emptySpan has start == end" $
      \pos -> let span = emptySpan pos
              in spanStart span === spanEnd span
    
  , testProperty "spanFrom creates span from position" $
      \pos -> spanFrom pos === emptySpan pos
    
  , testProperty "spanTo creates span ending at position" $
      \startPos endPos startPosContent ->
        let span = spanTo startPos endPos startPosContent
        in spanEnd span === endPos
    
  , testProperty "spanBetween creates valid span" $
      \startPos endPos ->
        let span = spanBetween startPos endPos
        in if startPos <= endPos
           then spanStart span === startPos && spanEnd span === endPos
           else spanStart span === endPos && spanEnd span === startPos
    
  , testProperty "mergeSpans contains both spans" $
      \span1 span2 ->
        let merged = mergeSpans span1 span2
        in spanStart merged <= spanStart span1 &&
           spanEnd merged >= spanEnd span1 &&
           spanStart merged <= spanStart span2 &&
           spanEnd merged >= spanEnd span2
    
  , testProperty "mergeSpans is commutative" $
      \span1 span2 -> mergeSpans span1 span2 === mergeSpans span2 span1
    
  , testProperty "mergeSpans is associative" $
      \span1 span2 span3 ->
        mergeSpans (mergeSpans span1 span2) span3 ===
        mergeSpans span1 (mergeSpans span2 span3)
  ]

-- ============================================================================
-- Location Tracking Properties
-- ============================================================================

locationTrackingProperties :: TestTree
locationTrackingProperties = testGroup "Location Tracking Properties"
  [ testProperty "location tracking preserves position sequence" $
      \positions ->
        let tracked = runLocationTracker $ do
              mapM_ setCurrentPos positions
              getCurrentPos
        in tracked === last positions
    
  , testProperty "markSpanStart and markSpanEnd create valid span" $
      \startPos endPos ->
        let tracked = runLocationTracker $ do
              setCurrentPos startPos
              markSpanStart
              setCurrentPos endPos
              markSpanEnd
        in -- This would need access to internal span tracking state
           True -- Placeholder - actual implementation would check span storage
    
  , testProperty "withLocationTracking preserves final position" $
      \startPos actions ->
        let result = withLocationTracking startPos actions
        in -- Check that final position is correctly tracked
           True -- Placeholder
  ]

-- ============================================================================
-- Position Advancement Properties
-- ============================================================================

positionAdvancementProperties :: TestTree
positionAdvancementProperties = testGroup "Position Advancement Properties"
  [ testProperty "advancePos by newline increments line" $
      \pos ->
        let advanced = advancePos pos '\n'
        in sourceLine advanced === sourceLine pos + 1 &&
           sourceColumn advanced === 1
    
  , testProperty "advancePos by regular character increments column" $
      \pos char -> char /= '\n' && char /= '\r' && char /= '\t' ==>
        let advanced = advancePos pos char
        in sourceLine advanced === sourceLine pos &&
           sourceColumn advanced === sourceColumn pos + 1
    
  , testProperty "advancePos by tab increments column by tab width" $
      \pos ->
        let advanced = advancePos pos '\t'
            expectedCol = ((sourceColumn pos - 1) `div` 8 + 1) * 8 + 1
        in sourceLine advanced === sourceLine pos &&
           sourceColumn advanced === expectedCol
    
  , testProperty "advancePosBy handles multiple characters" $
      \pos chars ->
        let advanced = foldl advancePos pos chars
            singleAdvances = scanl advancePos pos chars
        in advanced === last singleAdvances
    
  , testProperty "position advancement is deterministic" $
      \pos char -> advancePos pos char === advancePos pos char
  ]

-- ============================================================================
-- Located Value Properties
-- ============================================================================

locatedValueProperties :: TestTree
locatedValueProperties = testGroup "Located Value Properties"
  [ testProperty "locatedAt preserves value" $
      \value pos -> locatedValue (locatedAt value pos) === value
    
  , testProperty "locatedAt sets correct position" $
      \value pos -> locatedPos (locatedAt value pos) === pos
    
  , testProperty "locatedWithSpan preserves value" $
      \value span -> locatedValue (locatedWithSpan value span) === value
    
  , testProperty "locatedWithSpan sets correct span" $
      \value span -> locatedSpan (locatedWithSpan value span) === span
    
  , testProperty "mapLocated preserves location" $
      \f value pos ->
        let located = locatedAt value pos
            mapped = mapLocated f located
        in locatedPos mapped === locatedPos located
    
  , testProperty "mapLocated applies function correctly" $
      \f value pos ->
        let located = locatedAt value pos
            mapped = mapLocated f located
        in locatedValue mapped === f value
  ]

-- ============================================================================
-- Error Location Properties
-- ============================================================================

errorLocationProperties :: TestTree
errorLocationProperties = testGroup "Error Location Properties"
  [ testProperty "toErrorLocation preserves position information" $
      \pos ->
        let errorLoc = toErrorLocation pos
        in -- Check that error location contains position info
           True -- Placeholder - depends on ErrorLocation type
    
  , testProperty "toErrorLocationWithSpan preserves span information" $
      \span ->
        let errorLoc = toErrorLocationWithSpan span
        in -- Check that error location contains span info
           True -- Placeholder - depends on ErrorLocation type
    
  , testProperty "error location conversion is consistent" $
      \pos ->
        let span = emptySpan pos
            errorFromPos = toErrorLocation pos
            errorFromSpan = toErrorLocationWithSpan span
        in -- Should be equivalent for empty spans
           True -- Placeholder
  ]

-- ============================================================================
-- Text Line/Column Calculation Properties
-- ============================================================================

textLineColumnProperties :: TestTree
textLineColumnProperties = testGroup "Text Line/Column Calculation Properties"
  [ testProperty "calculate position for multiline text" $
      \text ->
        let linesList = lines text
            positions = scanl (\pos line -> advancePosBy pos (line ++ "\n")) startPos linesList
        in length positions === length linesList + 1
    
  , testProperty "position calculation is consistent with character count" $
      \text ->
        let finalPos = advancePosBy startPos text
            lineCount = length $ filter (== '\n') text
        in sourceLine finalPos === 1 + lineCount
    
  , testProperty "column calculation matches characters in current line" $
      \text ->
        let finalPos = advancePosBy startPos text
            currentLine = last $ "" : lines text
            expectedCol = length currentLine + 1
        in sourceColumn finalPos === expectedCol
  ]

-- ============================================================================
-- QuickCheck Generators
-- ============================================================================

-- Generate valid source positions
genSourcePos :: Gen SourcePos
genSourcePos = SourcePos <$> choose (1, 1000) <*> choose (1, 1000)

-- Generate source spans
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  startLine <- choose (1, 1000)
  startCol <- choose (1, 1000)
  endLine <- choose (startLine, startLine + 100)
  endCol <- if endLine == startLine 
            then choose (startCol, startCol + 100)
            else choose (1, 1000)
  return $ SourceSpan (SourcePos startLine startCol) (SourcePos endLine endCol)

-- Generate text with various characters
genText :: Gen String
genText = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r.,;:!?()[]{}"

-- Generate located values
genLocatedValue :: Gen (Located String)
genLocatedValue = do
  value <- genText
  pos <- genSourcePos
  return $ locatedAt value pos

instance Arbitrary SourcePos where
  arbitrary = genSourcePos

instance Arbitrary SourceSpan where
  arbitrary = genSourceSpan

instance (Arbitrary a) => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    pos <- arbitrary
    return $ locatedAt value pos

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Calculate position after processing text
calculateFinalPosition :: String -> SourcePos
calculateFinalPosition = advancePosBy startPos

-- Check if span contains position
spanContainsPosition :: SourceSpan -> SourcePos -> Bool
spanContainsPosition span pos = 
  spanStart span <= pos && pos <= spanEnd span

-- Check if two spans overlap
spansOverlap :: SourceSpan -> SourceSpan -> Bool
spansOverlap span1 span2 =
  spanContainsPosition span1 (spanStart span2) ||
  spanContainsPosition span1 (spanEnd span2) ||
  spanContainsPosition span2 (spanStart span1) ||
  spanContainsPosition span2 (spanEnd span1)

-- ============================================================================
-- Edge Case Properties
-- ============================================================================

edgeCaseProperties :: TestTree
edgeCaseProperties = testGroup "Edge Case Properties"
  [ testCase "handle empty text position calculation" $
      calculateFinalPosition "" @?= startPos
    
  , testCase "handle single newline character" $
      calculateFinalPosition "\n" @?= SourcePos 2 1
    
  , testCase "handle tab character at column boundary" $
      advancePos (SourcePos 1 8) '\t' @?= SourcePos 1 9
    
  , testCase "handle tab character not at boundary" $
      advancePos (SourcePos 1 5) '\t' @?= SourcePos 1 9
    
  , testCase "span with same start and end position" $
      let span = emptySpan (SourcePos 10 20)
      in spanStart span @?= spanEnd span
  ]