{-# LANGUAGE CPP #-}

module Test.Unit.SourceLocationTrackingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Data.Text (Text)
import qualified Data.Text as T

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , HasLocation(..)
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
  , runLocationTracker
  , getCurrentPos
  , setCurrentPos
  , markSpanStart
  , markSpanEnd
  , withLocationTracking
  , toErrorLocation
  , toErrorLocationWithSpan
  , advancePos
  , advancePosBy
  , advancePosByText
  , advancePosByLine
  )

tests :: TestTree
tests = testGroup "Source Location Tracking"
  [ sourcePositionTests
  , sourceSpanTests
  , locatedValueTests
  , locationTrackerTests
  , positionAdvancementTests
  , errorLocationConversionTests
  , utilityFunctionTests
  ]

sourcePositionTests :: TestTree
sourcePositionTests = testGroup "Source Position Tests"
  [ testCase "creates start position correctly" $ do
      posLine startPos @?= 1
      posColumn startPos @?= 1
      posOffset startPos @?= 0

  , testCase "advances position with regular character" $ do
      let pos1 = posAt 1 5
          pos2 = posAfter 'a' pos1
      posLine pos2 @?= 1
      posColumn pos2 @?= 6
      posOffset pos2 @?= posOffset pos1 + 1

  , testCase "advances position with newline" $ do
      let pos1 = posAt 1 10
          pos2 = posAfter '\n' pos1
      posLine pos2 @?= 2
      posColumn pos2 @?= 1
      posOffset pos2 @?= posOffset pos1 + 1

  , testCase "advances position with tab (8-space alignment)" $ do
      let pos1 = posAt 1 3
          pos2 = posAfter '\t' pos1
      posLine pos2 @?= 1
      posColumn pos2 @?= 9  -- Next tab stop at column 9
      posOffset pos2 @?= posOffset pos1 + 1

  , testCase "advances position with tab at column 8" $ do
      let pos1 = posAt 1 8
          pos2 = posAfter '\t' pos1
      posLine pos2 @?= 1
      posColumn pos2 @?= 9  -- Next tab stop
      posOffset pos2 @?= posOffset pos1 + 1

  , testCase "advances position with tab at column 16" $ do
      let pos1 = posAt 1 16
          pos2 = posAfter '\t' pos1
      posLine pos2 @?= 1
      posColumn pos2 @?= 17  -- Next tab stop
      posOffset pos2 @?= posOffset pos1 + 1

  , testCase "creates position at specific line and column" $ do
      let pos = posAt 5 10
      posLine pos @?= 5
      posColumn pos @?= 10
      posOffset pos @?= 0

  , testCase "creates position with specific offset" $ do
      let pos = posAtLineCol 3 7 42
      posLine pos @?= 3
      posColumn pos @?= 7
      posOffset pos @?= 42

  , testCase "compares positions correctly" $ do
      let pos1 = posAt 1 5
          pos2 = posAt 1 10
          pos3 = posAt 2 1
      pos1 @?= pos1  -- Equality
      assertBool "pos1 < pos2" $ pos1 < pos2
      assertBool "pos2 < pos3" $ pos2 < pos3
      assertBool "pos1 < pos3" $ pos1 < pos3
  ]

sourceSpanTests :: TestTree
sourceSpanTests = testGroup "Source Span Tests"
  [ testCase "creates empty span at position" $ do
      let pos = posAt 3 7
          span = emptySpan pos
      spanStart span @?= pos
      spanEnd span @?= pos

  , testCase "creates span from position" $ do
      let pos = posAt 2 5
          span = spanFrom pos
      spanStart span @?= pos
      spanEnd span @?= pos

  , testCase "creates span to position" $ do
      let pos = posAt 4 8
          span = spanTo pos
      spanStart span @?= pos
      spanEnd span @?= pos

  , testCase "creates span between two positions" $ do
      let start = posAt 1 5
          end = posAt 1 10
          span = spanBetween start end
      spanStart span @?= start
      spanEnd span @?= end

  , testCase "creates span across multiple lines" $ do
      let start = posAt 2 15
          end = posAt 4 3
          span = spanBetween start end
      spanStart span @?= start
      spanEnd span @?= end

  , testCase "merges spans correctly" $ do
      let span1 = spanBetween (posAt 1 5) (posAt 1 10)
          span2 = spanBetween (posAt 2 3) (posAt 2 8)
          merged = mergeSpans span1 span2
      spanStart merged @?= spanStart span1
      spanEnd merged @?= spanEnd span2

  , testCase "merges overlapping spans" $ do
      let span1 = spanBetween (posAt 1 5) (posAt 2 10)
          span2 = spanBetween (posAt 2 5) (posAt 3 8)
          merged = mergeSpans span1 span2
      spanStart merged @?= spanStart span1
      spanEnd merged @?= spanEnd span2

  , testCase "merges nested spans" $ do
      let outer = spanBetween (posAt 1 1) (posAt 5 20)
          inner = spanBetween (posAt 2 5) (posAt 4 10)
          merged = mergeSpans outer inner
      spanStart merged @?= spanStart outer
      spanEnd merged @?= spanEnd outer

  , testCase "validates spans correctly" $ do
      let validSpan = spanBetween (posAt 1 5) (posAt 1 10)
          invalidSpan = spanBetween (posAt 1 10) (posAt 1 5)  -- End before start
          equalSpan = spanBetween (posAt 1 5) (posAt 1 5)
      assertBool "valid span should be valid" $ isValidSpan validSpan
      assertBool "invalid span should not be valid" $ not $ isValidSpan invalidSpan
      assertBool "equal span should be valid" $ isValidSpan equalSpan

  , testCase "compares spans correctly" $ do
      let span1 = spanBetween (posAt 1 5) (posAt 1 10)
          span2 = spanBetween (posAt 1 15) (posAt 1 20)
          span3 = spanBetween (posAt 2 1) (posAt 2 5)
      span1 @?= span1  -- Equality
      assertBool "span1 < span2" $ span1 < span2
      assertBool "span2 < span3" $ span2 < span3
      assertBool "span1 < span3" $ span1 < span3
  ]

locatedValueTests :: TestTree
locatedValueTests = testGroup "Located Value Tests"
  [ testCase "creates located value at position" $ do
      let pos = posAt 3 7
          value = "test"
          located = locatedAt pos value
      locatedValue located @?= value
      locatedPos located @?= pos
      spanStart (locatedSpan located) @?= pos
      spanEnd (locatedSpan located) @?= pos

  , testCase "creates located value with span" $ do
      let start = posAt 2 5
          end = posAt 2 10
          span = spanBetween start end
          value = 42
          located = locatedWithSpan span value
      locatedValue located @?= value
      locatedSpan located @?= span
      locatedPos located @?= start

  , testCase "maps located value correctly" $ do
      let pos = posAt 1 5
          value = "hello"
          located = locatedAt pos value
          mapped = mapLocated length located
      locatedValue mapped @?= length value
      locatedPos mapped @?= pos
      locatedSpan mapped @?= locatedSpan located

  , testCase "functor instance works correctly" $ do
      let pos = posAt 3 2
          value = [1, 2, 3]
          located = locatedAt pos value
          doubled = fmap (*2) located
      locatedValue doubled @?= [2, 4, 6]
      locatedPos doubled @?= pos

  , testCase "HasLocation class works" $ do
      let span = spanBetween (posAt 1 5) (posAt 1 10)
          located = locatedWithSpan span "test"
      getLocation located @?= span
  ]

locationTrackerTests :: TestTree
locationTrackerTests = testGroup "Location Tracker Tests"
  [ testCase "runs location tracker with start position" $ do
      let result = runLocationTracker getCurrentPos
      result @?= startPos

  , testCase "sets and gets current position" $ do
      let newPos = posAt 5 10
          result = runLocationTracker $ do
              setCurrentPos newPos
              getCurrentPos
      result @?= newPos

  , testCase "marks span start and end" $ do
      let start = posAt 2 5
          result = runLocationTracker $ do
              setCurrentPos start
              spanStart <- markSpanStart
              setCurrentPos $ posAt 2 10
              markSpanEnd spanStart
      spanStart result @?= start
      spanEnd result @?= posAt 2 10

  , testCase "uses withLocationTracking correctly" $ do
      let start = posAt 3 7
          end = posAt 3 15
          (result, finalPos) = withLocationTracking start $ do
              setCurrentPos end
              getCurrentPos
      result @?= end
      finalPos @?= end

  , testCase "tracks position through multiple operations" $ do
      let result = runLocationTracker $ do
              pos1 <- getCurrentPos
              setCurrentPos $ posAt 2 5
              pos2 <- getCurrentPos
              setCurrentPos $ posAt 3 10
              pos3 <- getCurrentPos
              return (pos1, pos2, pos3)
      let (pos1, pos2, pos3) = result
      pos1 @?= startPos
      pos2 @?= posAt 2 5
      pos3 @?= posAt 3 10
  ]

positionAdvancementTests :: TestTree
positionAdvancementTests = testGroup "Position Advancement Tests"
  [ testCase "advances position by single character" $ do
      let start = posAt 1 5
          result = advancePos 'x' start
      posLine result @?= 1
      posColumn result @?= 6
      posOffset result @?= posOffset start + 1

  , testCase "advances position by multiple characters" $ do
      let start = posAt 1 3
          text = "hello"
          result = advancePosBy text start
      posLine result @?= 1
      posColumn result @?= 8
      posOffset result @?= posOffset start + length text

  , testCase "advances position by text with newline" $ do
      let start = posAt 1 5
          text = "hello\nworld"
          result = advancePosBy text start
      posLine result @?= 2
      posColumn result @?= 6
      posOffset result @?= posOffset start + length text

  , testCase "advances position by Text value" $ do
      let start = posAt 1 2
          text = T.pack "test"
          result = advancePosByText text start
      posLine result @?= 1
      posColumn result @?= 6
      posOffset result @?= posOffset start + T.length text

  , testCase "advances position by lines" $ do
      let start = posAt 3 15
          result = advancePosByLine 5 start
      posLine result @?= 8
      posColumn result @?= 1
      posOffset result @?= posOffset start + 5

  , testCase "advances position by zero lines" $ do
      let start = posAt 4 8
          result = advancePosByLine 0 start
      posLine result @?= 4
      posColumn result @?= 1
      posOffset result @?= posOffset start

  , testCase "advances position by negative lines" $ do
      let start = posAt 10 5
          result = advancePosByLine (-3) start
      posLine result @?= 7
      posColumn result @?= 1
      posOffset result @?= posOffset start - 3

  , testCase "handles complex text advancement" $ do
      let start = posAt 1 1
          text = "line1\n\tline2\n\nline3"
          result = advancePosBy text start
      posLine result @?= 4
      posColumn result @?= 6
      posOffset result @?= length text
  ]

errorLocationConversionTests :: TestTree
errorLocationConversionTests = testGroup "Error Location Conversion Tests"
  [ testCase "converts source position to error location" $ do
      let pos = posAt 5 12
          errorLoc = toErrorLocation pos
      filePath errorLoc @?= Nothing
      line errorLoc @?= 5
      column errorLoc @?= 12
      endLine errorLoc @?= Nothing
      endColumn errorLoc @?= Nothing

  , testCase "converts source span to error location with range" $ do
      let start = posAt 3 7
          end = posAt 3 15
          span = spanBetween start end
          errorLoc = toErrorLocationWithSpan span
      filePath errorLoc @?= Nothing
      line errorLoc @?= 3
      column errorLoc @?= 7
      endLine errorLoc @?= Just 3
      endColumn errorLoc @?= Just 15

  , testCase "converts multi-line span to error location" $ do
      let start = posAt 2 10
          end = posAt 4 5
          span = spanBetween start end
          errorLoc = toErrorLocationWithSpan span
      filePath errorLoc @?= Nothing
      line errorLoc @?= 2
      column errorLoc @?= 10
      endLine errorLoc @?= Just 4
      endColumn errorLoc @?= Just 5

  , testCase "converts single-character span to error location" $ do
      let pos = posAt 1 8
          span = emptySpan pos
          errorLoc = toErrorLocationWithSpan span
      filePath errorLoc @?= Nothing
      line errorLoc @?= 1
      column errorLoc @?= 8
      endLine errorLoc @?= Just 1
      endColumn errorLoc @?= Just 8
  ]

utilityFunctionTests :: TestTree
utilityFunctionTests = testGroup "Utility Function Tests"
  [ testCase "handles position comparison edge cases" $ do
      let pos1 = posAt 1 1
          pos2 = posAt 1 1
          pos3 = posAt 1 2
          pos4 = posAt 2 1
      assertBool "same positions are equal" $ pos1 == pos2
      assertBool "pos1 < pos3" $ pos1 < pos3
      assertBool "pos3 < pos4" $ pos3 < pos4

  , testCase "handles span validation edge cases" $ do
      let validSpan1 = spanBetween (posAt 1 1) (posAt 1 2)
          validSpan2 = spanBetween (posAt 1 1) (posAt 1 1)
          invalidSpan1 = spanBetween (posAt 1 2) (posAt 1 1)
          invalidSpan2 = spanBetween (posAt 2 1) (posAt 1 10)
      assertBool "forward span is valid" $ isValidSpan validSpan1
      assertBool "zero-length span is valid" $ isValidSpan validSpan2
      assertBool "backward span is invalid" $ not $ isValidSpan invalidSpan1
      assertBool "cross-line backward span is invalid" $ not $ isValidSpan invalidSpan2

  , testCase "handles span merging edge cases" $ do
      let span1 = spanBetween (posAt 1 5) (posAt 1 10)
          span2 = spanBetween (posAt 1 8) (posAt 1 15)  -- Overlapping
          span3 = spanBetween (posAt 2 1) (posAt 2 5)   -- Separate
          merged1 = mergeSpans span1 span2
          merged2 = mergeSpans span1 span3
      spanStart merged1 @?= spanStart span1
      spanEnd merged1 @?= spanEnd span2
      spanStart merged2 @?= spanStart span1
      spanEnd merged2 @?= spanEnd span3

  , testCase "handles located value mapping edge cases" $ do
      let pos = posAt 3 7
          original = locatedAt pos [1, 2, 3]
          mapped1 = mapLocated reverse original
          mapped2 = mapLocated (map (*2)) original
      locatedValue mapped1 @?= [3, 2, 1]
      locatedPos mapped1 @?= pos
      locatedValue mapped2 @?= [2, 4, 6]
      locatedPos mapped2 @?= pos

  , testCase "handles location tracker state changes" $ do
      let operations = runLocationTracker $ do
              initial <- getCurrentPos
              setCurrentPos $ posAt 2 5
              afterSet <- getCurrentPos
              setCurrentPos $ posAt 3 10
              final <- getCurrentPos
              return (initial, afterSet, final)
      let (initial, afterSet, final) = operations
      initial @?= startPos
      afterSet @?= posAt 2 5
      final @?= posAt 3 10

  , testCase "handles complex text advancement scenarios" $ do
      let start = posAt 1 1
          texts = ["", "a", "hello", "hello\nworld", "a\nb\nc", "\n\n", "hello\n\nworld"]
          results = map (`advancePosBy` start) texts
          expectedLines = [1, 1, 1, 2, 3, 3, 5]
          expectedColumns = [1, 2, 6, 6, 2, 1, 7]
      mapM_ (\((result, expectedLine, expectedCol), idx) -> 
        testCase ("complex text advancement " ++ show idx) $ do
          posLine result @?= expectedLine
          posColumn result @?= expectedCol
        ) (zip3 results expectedLines expectedColumns [0..])
  ]