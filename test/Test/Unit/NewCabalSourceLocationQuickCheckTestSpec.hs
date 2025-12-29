module Test.Unit.NewCabalSourceLocationQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll)
import Data.Text (Text)
import qualified Data.Text as T

import SourceLocation
import TestSupport.QuickCheck (fastProperty)

-- | QuickCheck tests for SourceLocation module position and span calculations
tests :: TestTree
tests =
  testGroup "New Cabal SourceLocation QuickCheck Tests"
    [ testProperty "posAfter newline increments line, resets column to 1" prop_posAfterNewline
    , testProperty "posAfter tab advances to next tab position" prop_posAfterTab
    , testProperty "posAfter regular char increments column" prop_posAfterRegularChar
    , testProperty "posAt creates position with given line and column" prop_posAtCorrectness
    , testProperty "posAtLineCol creates position with given line, column, offset" prop_posAtLineColCorrectness
    , testProperty "emptySpan has same start and end" prop_emptySpanSameStartEnd
    , testProperty "spanBetween creates span with given start and end" prop_spanBetweenCorrectness
    , testProperty "mergeSpans creates span covering both input spans" prop_mergeSpansCoverage
    , testProperty "isValidSpan checks start <= end" prop_isValidSpanLogic
    , testProperty "locatedAt creates located value at given position" prop_locatedAtCorrectness
    , testProperty "locatedWithSpan creates located value with given span" prop_locatedWithSpanCorrectness
    , testProperty "mapLocated applies function to value" prop_mapLocatedCorrectness
    , testProperty "advancePosByText advances position by text content" prop_advancePosByTextCorrectness
    , testProperty "advancePosByLine advances line number, resets column" prop_advancePosByLineCorrectness
    , testGroup "Edge cases"
        [ testCase "startPos has line 1, column 1, offset 0" $
            startPos @?= SourcePos 1 1 0
        , testCase "posAt with line 5 column 10 creates correct position" $
            posAt 5 10 @?= SourcePos 5 10 0
        , testCase "emptySpan at startPos creates span at (1,1)-(1,1)" $
            emptySpan startPos @?= SourceSpan startPos startPos
        , testCase "isValidSpan returns False for span with start > end" $ do
            let start = posAt 5 10
                end = posAt 3 20
            isValidSpan (SourceSpan start end) @?= False
        , testCase "isValidSpan returns True for span with start == end" $
            isValidSpan (emptySpan startPos) @?= True
        ]
    ]

-- | Property: posAfter '\n' increments line and resets column to 1
prop_posAfterNewline :: Int -> Int -> Property
prop_posAfterNewline line col = 
  line > 0 && col > 0 ==>
  let pos = SourcePos line col 0
      newPos = posAfter '\n' pos
  in posLine newPos === line + 1 .&&. posColumn newPos === 1 .&&. posOffset newPos === 1

-- | Property: posAfter '\t' advances to next tab position (multiple of 8 + 1)
prop_posAfterTab :: Int -> Int -> Property
prop_posAfterTab line col = 
  line > 0 && col > 0 ==>
  let pos = SourcePos line col 0
      newPos = posAfter '\t' pos
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in posLine newPos === line .&&. posColumn newPos === expectedCol .&&. posOffset newPos === 1

-- | Property: posAfter regular character increments column and offset
prop_posAfterRegularChar :: Int -> Int -> Char -> Property
prop_posAfterRegularChar line col ch = 
  line > 0 && col > 0 && ch /= '\n' && ch /= '\t' ==>
  let pos = SourcePos line col 0
      newPos = posAfter ch pos
  in posLine newPos === line .&&. posColumn newPos === col + 1 .&&. posOffset newPos === 1

-- | Property: posAt creates position with correct line and column
prop_posAtCorrectness :: Int -> Int -> Property
prop_posAtCorrectness line col = 
  line > 0 && col > 0 ==>
  let pos = posAt line col
  in posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === 0

-- | Property: posAtLineCol creates position with correct line, column, and offset
prop_posAtLineColCorrectness :: Int -> Int -> Int -> Property
prop_posAtLineColCorrectness line col offset = 
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
  in posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === offset

-- | Property: emptySpan has same start and end position
prop_emptySpanSameStartEnd :: Int -> Int -> Int -> Property
prop_emptySpanSameStartEnd line col offset = 
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
      span = emptySpan pos
  in spanStart span === pos .&&. spanEnd span === pos

-- | Property: spanBetween creates span with given start and end
prop_spanBetweenCorrectness :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_spanBetweenCorrectness line1 col1 offset1 line2 col2 offset2 = 
  line1 > 0 && col1 > 0 && offset1 >= 0 &&
  line2 > 0 && col2 > 0 && offset2 >= 0 ==>
  let start = posAtLineCol line1 col1 offset1
      end = posAtLineCol line2 col2 offset2
      span = spanBetween start end
  in spanStart span === start .&&. spanEnd span === end

-- | Property: mergeSpans creates span covering both input spans
prop_mergeSpansCoverage :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_mergeSpansCoverage line1 col1 offset1 line2 col2 offset2 line3 col3 offset3 line4 col4 = 
  all (>0) [line1, line2, line3, line4] &&
  all (>0) [col1, col2, col3, col4] &&
  all (>=0) [offset1, offset2, offset3, offset4] ==>
  let start1 = posAtLineCol line1 col1 offset1
      end1 = posAtLineCol line2 col2 offset2
      start2 = posAtLineCol line3 col3 offset3
      end2 = posAtLineCol line4 col4 offset4
      span1 = spanBetween start1 end1
      span2 = spanBetween start2 end2
      merged = mergeSpans span1 span2
      expectedStart = min start1 start2
      expectedEnd = max end1 end2
  in spanStart merged === expectedStart .&&. spanEnd merged === expectedEnd

-- | Property: isValidSpan returns True when start <= end, False otherwise
prop_isValidSpanLogic :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_isValidSpanLogic line1 col1 offset1 line2 col2 offset2 line3 col3 offset3 line4 col4 = 
  all (>0) [line1, line2, line3, line4] &&
  all (>0) [col1, col2, col3, col4] &&
  all (>=0) [offset1, offset2, offset3, offset4] ==>
  let start1 = posAtLineCol line1 col1 offset1
      end1 = posAtLineCol line2 col2 offset2
      start2 = posAtLineCol line3 col3 offset3
      end2 = posAtLineCol line4 col4 offset4
      span1 = spanBetween start1 end1
      span2 = spanBetween start2 end2
  in isValidSpan span1 === (start1 <= end1) .&&. isValidSpan span2 === (start2 <= end2)

-- | Property: locatedAt creates located value at given position
prop_locatedAtCorrectness :: Int -> Int -> Int -> String -> Property
prop_locatedAtCorrectness line col offset value = 
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
      located = locatedAt pos value
  in locValue located === value .&&. locPos located === pos .&&. locSpan located === emptySpan pos

-- | Property: locatedWithSpan creates located value with given span
prop_locatedWithSpanCorrectness :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> String -> Property
prop_locatedWithSpanCorrectness line1 col1 offset1 line2 col2 offset2 value = 
  all (>0) [line1, line2] &&
  all (>0) [col1, col2] &&
  all (>=0) [offset1, offset2] ==>
  let start = posAtLineCol line1 col1 offset1
      end = posAtLineCol line2 col2 offset2
      span = spanBetween start end
      located = locatedWithSpan span value
  in locValue located === value .&&. locPos located === start .&&. locSpan located === span

-- | Property: mapLocated applies function to value while preserving location
prop_mapLocatedCorrectness :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> String -> Property
prop_mapLocatedCorrectness line1 col1 offset1 line2 col2 offset2 value = 
  all (>0) [line1, line2] &&
  all (>0) [col1, col2] &&
  all (>=0) [offset1, offset2] ==>
  let start = posAtLineCol line1 col1 offset1
      end = posAtLineCol line2 col2 offset2
      span = spanBetween start end
      located = locatedWithSpan span value
      mapped = mapLocated length located
  in locValue mapped === length value .&&. locPos mapped === start .&&. locSpan mapped === span

-- | Property: advancePosByText advances position correctly based on text content
prop_advancePosByTextCorrectness :: Int -> Int -> Int -> String -> Property
prop_advancePosByTextCorrectness line col offset text = 
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAtLineCol line col offset
      advanced = advancePosByText (T.pack text) pos
      expected = advancePosBy text pos
  in advanced === expected

-- | Property: advancePosByLine advances line number and resets column to 1
prop_advancePosByLineCorrectness :: Int -> Int -> Int -> Int -> Property
prop_advancePosByLineCorrectness line col offset numLines = 
  line > 0 && col > 0 && offset >= 0 && numLines >= 0 ==>
  let pos = posAtLineCol line col offset
      advanced = advancePosByLine numLines pos
  in posLine advanced === line + numLines .&&. posColumn advanced === 1

-- Helper operator for composing properties
(.&&.) :: Property -> Property -> Property
(.&&.) = (&&)
