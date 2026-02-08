{-# LANGUAGE DeriveGeneric #-}
module Test.Unit.SourceLocationQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T

import SourceLocation
import Compiler.Errors.Core (ErrorLocation(..))

-- Test data types
data TestLocated = TestLocated
  { testValue :: String
  , testLoc :: SourcePos
  } deriving (Show, Eq, Generic)

-- QuickCheck properties
prop_source_pos_creation :: Property
prop_source_pos_creation =
  forAll arbitrary $ \lineNum ->
  forAll arbitrary $ \colNum ->
  forAll arbitrary $ \offsetVal ->
    let pos = SourcePos lineNum colNum offsetVal
    in property $ 
      posLine pos == lineNum &&
      posColumn pos == colNum &&
      posOffset pos == offsetVal

prop_source_pos_ordering :: Property
prop_source_pos_ordering =
  forAll arbitrary $ \pos1 ->
  forAll arbitrary $ \pos2 ->
    let cmp = comparePos pos1 pos2
        (line1, col1, off1) = (posLine pos1, posColumn pos1, posOffset pos1)
        (line2, col2, off2) = (posLine pos2, posColumn pos2, posOffset pos2)
    in case cmp of
         LT -> property $ line1 < line2 || (line1 == line2 && (col1 < col2 || (col1 == col2 && off1 < off2)))
         EQ -> property $ line1 == line2 && col1 == col2 && off1 == off2
         GT -> property $ line1 > line2 || (line1 == line2 && (col1 > col2 || (col1 == col2 && off1 > off2)))

prop_pos_after_newline :: Property
prop_pos_after_newline =
  forAll arbitrary $ \pos ->
    let newPos = posAfter '\n' pos
    in property $
      posLine newPos == posLine pos + 1 &&
      posColumn newPos == 1 &&
      posOffset newPos == posOffset pos + 1

prop_pos_after_tab :: Property
prop_pos_after_tab =
  forAll arbitrary $ \pos ->
    let newPos = posAfter '\t' pos
        expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
    in property $
      posLine newPos == posLine pos &&
      posColumn newPos == expectedCol &&
      posOffset newPos == posOffset pos + 1

prop_pos_after_regular_char :: Property
prop_pos_after_regular_char =
  forAll arbitrary $ \pos ->
  forAll (arbitrary `suchThat` (`notElem` ['\n', '\t'])) $ \char ->
    let newPos = posAfter char pos
    in property $
      posLine newPos == posLine pos &&
      posColumn newPos == posColumn pos + 1 &&
      posOffset newPos == posOffset pos + 1

prop_advance_pos_by :: Property
prop_advance_pos_by =
  forAll arbitrary $ \pos ->
  forAll arbitrary $ \str ->
    let newPos = advancePosBy str pos
        expectedPos = foldl (flip advancePos) pos str
    in property $ newPos == expectedPos

prop_advance_pos_by_text :: Property
prop_advance_pos_by_text =
  forAll arbitrary $ \pos ->
  forAll arbitrary $ \text ->
    let newPos = advancePosByText text pos
        expectedPos = advancePosBy (T.unpack text) pos
    in property $ newPos == expectedPos

prop_advance_pos_by_line :: Property
prop_advance_pos_by_line =
  forAll arbitrary $ \pos ->
  forAll (arbitrary `suchThat` (> 0)) $ \numLines ->
    let newPos = advancePosByLine numLines pos
    in property $
      posLine newPos == posLine pos + numLines &&
      posColumn newPos == 1

prop_source_span_creation :: Property
prop_source_span_creation =
  forAll arbitrary $ \start ->
  forAll arbitrary $ \end ->
    let span = spanBetween start end
    in property $
      spanStart span == start &&
      spanEnd span == end

prop_source_span_ordered :: Property
prop_source_span_ordered =
  forAll arbitrary $ \pos1 ->
  forAll arbitrary $ \pos2 ->
    let span = spanBetweenOrdered pos1 pos2
        (actualStart, actualEnd) = if comparePos pos1 pos2 == LT
                                   then (pos1, pos2)
                                   else (pos2, pos1)
    in property $
      spanStart span == actualStart &&
      spanEnd span == actualEnd

prop_merge_spans :: Property
prop_merge_spans =
  forAll arbitrary $ \span1 ->
  forAll arbitrary $ \span2 ->
    let merged = mergeSpans span1 span2
        start1 = spanStart span1
        start2 = spanStart span2
        end1 = spanEnd span1
        end2 = spanEnd span2
        expectedStart = SourcePos
          { posLine = min (posLine start1) (posLine start2)
          , posColumn = min (posColumn start1) (posColumn start2)
          , posOffset = min (posOffset start1) (posOffset start2)
          }
        expectedEnd = SourcePos
          { posLine = max (posLine end1) (posLine end2)
          , posColumn = max (posColumn end1) (posColumn end2)
          , posOffset = max (posOffset end1) (posOffset end2)
          }
    in property $
      spanStart merged == expectedStart &&
      spanEnd merged == expectedEnd

prop_is_valid_span :: Property
prop_is_valid_span =
  forAll arbitrary $ \start ->
  forAll arbitrary $ \end ->
    let span = spanBetween start end
        isValid = comparePos start end /= GT
    in property $ isValidSpan span == isValid

prop_located_at :: Property
prop_located_at =
  forAll arbitrary $ \pos ->
  forAll arbitrary $ \value ->
    let located = locatedAt pos value
    in property $
      locValue located == value &&
      locPos located == pos &&
      locSpan located == emptySpan pos

prop_located_with_span :: Property
prop_located_with_span =
  forAll arbitrary $ \span ->
  forAll arbitrary $ \value ->
    let located = locatedWithSpan span value
    in property $
      locValue located == value &&
      locPos located == spanStart span &&
      locSpan located == span

prop_map_located :: Property
prop_map_located =
  forAll arbitrary $ \span ->
  forAll arbitrary $ \value ->
    let located = locatedWithSpan span value
        mapped = mapLocated (++ "suffix") located
    in property $
      locValue mapped == value ++ "suffix" &&
      locPos mapped == locPos located &&
      locSpan mapped == locSpan located

prop_to_error_location :: Property
prop_to_error_location =
  forAll arbitrary $ \pos ->
    let errLoc = toErrorLocation pos
    in property $
      filePath errLoc == Nothing &&
      line errLoc == posLine pos &&
      column errLoc == posColumn pos &&
      endLine errLoc == Nothing &&
      endColumn errLoc == Nothing

prop_to_error_location_with_span :: Property
prop_to_error_location_with_span =
  forAll arbitrary $ \span ->
    let errLoc = toErrorLocationWithSpan span
        start = spanStart span
        end = spanEnd span
    in property $
      filePath errLoc == Nothing &&
      line errLoc == posLine start &&
      column errLoc == posColumn start &&
      endLine errLoc == Just (posLine end) &&
      endColumn errLoc == Just (posColumn end)

prop_location_tracker :: Property
prop_location_tracker =
  forAll arbitrary $ \pos ->
  forAll arbitrary $ \value ->
    let (result, finalPos) = withLocationTracking pos $ do
          setCurrentPos pos
          getCurrentPos
    in property $ result == pos && finalPos == pos

prop_span_tracking :: Property
prop_span_tracking =
  forAll arbitrary $ \startPos ->
  forAll arbitrary $ \text ->
    let (span, finalPos) = withLocationTracking startPos $ do
          start <- markSpanStart
          advancePosByText text
          end <- markSpanEnd start
          return end
    in property $
      spanStart span == startPos &&
      spanEnd span == finalPos

-- Test suite
testSuite :: TestTree
testSuite = testGroup "SourceLocation QuickCheck Tests"
  [ testProperty "source pos creation" prop_source_pos_creation
  , testProperty "source pos ordering" prop_source_pos_ordering
  , testProperty "pos after newline" prop_pos_after_newline
  , testProperty "pos after tab" prop_pos_after_tab
  , testProperty "pos after regular char" prop_pos_after_regular_char
  , testProperty "advance pos by" prop_advance_pos_by
  , testProperty "advance pos by text" prop_advance_pos_by_text
  , testProperty "advance pos by line" prop_advance_pos_by_line
  , testProperty "source span creation" prop_source_span_creation
  , testProperty "source span ordered" prop_source_span_ordered
  , testProperty "merge spans" prop_merge_spans
  , testProperty "is valid span" prop_is_valid_span
  , testProperty "located at" prop_located_at
  , testProperty "located with span" prop_located_with_span
  , testProperty "map located" prop_map_located
  , testProperty "to error location" prop_to_error_location
  , testProperty "to error location with span" prop_to_error_location_with_span
  , testProperty "location tracker" prop_location_tracker
  , testProperty "span tracking" prop_span_tracking
  ]

-- Unit tests for specific edge cases
unitTests :: TestTree
unitTests = testGroup "SourceLocation Unit Tests"
  [ testCase "start position" $ do
      let pos = startPos
      assertEqual "Start position should be (1,1,0)" (SourcePos 1 1 0) pos

  , testCase "empty span" $ do
      let pos = posAt 5 10
          span = emptySpan pos
      assertEqual "Empty span should have same start and end" (SourceSpan pos pos) span

  , testCase "span from" $ do
      let pos = posAt 3 5
          span = spanFrom pos
      assertEqual "Span from should create empty span" (emptySpan pos) span

  , testCase "span to" $ do
      let pos = posAt 7 12
          span = spanTo pos
      assertEqual "Span to should create empty span" (emptySpan pos) span

  , testCase "source line" $ do
      let pos = posAt 10 20
      assertEqual "sourceLine should return line number" 10 (sourceLine pos)

  , testCase "source column" $ do
      let pos = posAt 10 20
      assertEqual "sourceColumn should return column number" 20 (sourceColumn pos)

  , testCase "source pos offset" $ do
      let pos = posAtLineCol 5 10 100
      assertEqual "sourcePosOffset should return offset" 100 (sourcePosOffset pos)

  , testCase "pos at with offset" $ do
      let pos = posAtWithOffset 5 10 100
      assertEqual "posAtWithOffset should create correct position" (SourcePos 5 10 100) pos

  , testCase "span start pos" $ do
      let start = posAt 1 1
          end = posAt 2 2
          span = spanBetween start end
      assertEqual "spanStartPos should return start position" start (spanStartPos span)

  , testCase "span end pos" $ do
      let start = posAt 1 1
          end = posAt 2 2
          span = spanBetween start end
      assertEqual "spanEndPos should return end position" end (spanEndPos span)
  ]

-- Combined test suite
tests :: TestTree
tests = testGroup "SourceLocation Tests"
  [ testSuite
  , unitTests
  ]