module Test.Unit.SourceLocationAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Property, (===), forAll, Gen, choose, arbitrary, listOf1, elements)
import TestSupport.QuickCheck (fastProperty)

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), 
                      startPos, posAfter, posAt, posAtLineCol,
                      emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan,
                      locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos, mapLocated,
                      advancePos, advancePosBy, advancePosByText, advancePosByLine)

-- ============================================================================
-- Generators
-- ============================================================================

-- Generate valid source positions (1-based line and column)
genValidSourcePos :: Gen SourcePos
genValidSourcePos = do
  line <- choose (1, 1000)
  column <- choose (1, 200)
  offset <- choose (0, 100000)
  return $ SourcePos line column offset

-- Generate valid source spans
genValidSourceSpan :: Gen SourceSpan
genValidSourceSpan = do
  startLine <- choose (1, 100)
  startColumn <- choose (1, 50)
  startOffset <- choose (0, 5000)
  let startPos = SourcePos startLine startColumn startOffset
  
  endLine <- choose (startLine, startLine + 10)
  endColumn <- if endLine == startLine 
               then choose (startColumn, startColumn + 50)
               else choose (1, 200)
  endOffset <- choose (startOffset, startOffset + 1000)
  let endPos = SourcePos endLine endColumn endOffset
  
  return $ SourceSpan startPos endPos

-- Generate characters for position advancement
genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n.,;:!()[]{}<>+-*/%=|&^~?@#"

-- Generate strings for text advancement
genString :: Gen String
genString = listOf1 genChar

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: startPos is always valid
prop_startPosValid :: Bool
prop_startPosValid = 
  let pos = startPos
  in posLine pos >= 1 && posColumn pos >= 1 && posOffset pos >= 0

-- Property: posAfter correctly handles newline
prop_posAfterNewline :: SourcePos -> Bool
prop_posAfterNewline pos =
  let newPos = posAfter '\n' pos
  in posLine newPos == posLine pos + 1 && 
     posColumn newPos == 1 &&
     posOffset newPos == posOffset pos + 1

-- Property: posAfter correctly handles tab
prop_posAfterTab :: SourcePos -> Bool
prop_posAfterTab pos =
  let newPos = posAfter '\t' pos
      expectedColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in posLine newPos == posLine pos &&
     posColumn newPos == expectedColumn &&
     posOffset newPos == posOffset pos + 1

-- Property: posAfter correctly handles regular characters
prop_posAfterRegular :: Char -> SourcePos -> Property
prop_posAfterRegular c pos = 
  (c /= '\n' && c /= '\t') ==> 
  let newPos = posAfter c pos
  in posLine newPos == posLine pos &&
     posColumn newPos == posColumn pos + 1 &&
     posOffset newPos == posOffset pos + 1

-- Property: posAt creates valid positions
prop_posAtValid :: Int -> Int -> Property
prop_posAtValid line col =
  line > 0 && col > 0 ==>
  let pos = posAt line col
  in posLine pos == line && posColumn pos == col && posOffset pos == 0

-- Property: emptySpan creates valid spans
prop_emptySpanValid :: SourcePos -> Bool
prop_emptySpanValid pos =
  let span = emptySpan pos
  in spanStart span == pos && spanEnd span == pos && isValidSpan span

-- Property: spanBetween creates valid spans when start <= end
prop_spanBetweenValid :: SourcePos -> SourcePos -> Property
prop_spanBetweenValid start end =
  start <= end ==>
  let span = spanBetween start end
  in spanStart span == start && spanEnd span == end && isValidSpan span

-- Property: mergeSpans contains both original spans
prop_mergeSpansContains :: SourceSpan -> SourceSpan -> Bool
prop_mergeSpansContains span1 span2 =
  let merged = mergeSpans span1 span2
  in spanStart merged <= spanStart span1 &&
     spanEnd merged >= spanEnd span1 &&
     spanStart merged <= spanStart span2 &&
     spanEnd merged >= spanEnd span2

-- Property: locatedAt creates valid located values
prop_locatedAtValid :: SourcePos -> Int -> Bool
prop_locatedAtValid pos value =
  let located = locatedAt pos value
  in locatedValue located == value &&
     locatedPos located == pos &&
     locatedSpan located == emptySpan pos

-- Property: mapLocated preserves location
prop_mapLocatedPreservesLocation :: SourceSpan -> Int -> Bool
prop_mapLocatedPreservesLocation span value =
  let located = locatedWithSpan span value
      mapped = mapLocated (+1) located
  in locatedSpan mapped == span &&
     locatedPos mapped == spanStart span &&
     locatedValue mapped == value + 1

-- Property: advancePosByText is consistent with advancePosBy
prop_advancePosByTextConsistent :: String -> SourcePos -> Bool
prop_advancePosByTextConsistent text pos =
  let result1 = advancePosBy text pos
      result2 = advancePosByText (read text) pos
  in result1 == result2

-- Property: advancePosByLine correctly advances line number
prop_advancePosByLineCorrect :: Int -> SourcePos -> Property
prop_advancePosByLineCorrect numLines pos =
  numLines > 0 ==>
  let newPos = advancePosByLine numLines pos
  in posLine newPos == posLine pos + numLines &&
     posColumn newPos == 1

-- Property: position advancement is associative for strings
prop_advancePosAssociative :: String -> String -> SourcePos -> Bool
prop_advancePosAssociative str1 str2 pos =
  let result1 = advancePosBy (str1 ++ str2) pos
      result2 = advancePosBy str2 (advancePosBy str1 pos)
  in result1 == result2

-- Property: span ordering is consistent with position ordering
prop_spanOrderingConsistent :: SourcePos -> SourcePos -> SourcePos -> Property
prop_spanOrderingConsistent p1 p2 p3 =
  (p1 <= p2 && p2 <= p3) ==>
  let span1 = spanBetween p1 p2
      span2 = spanBetween p2 p3
      merged = mergeSpans span1 span2
  in spanStart merged == p1 && spanEnd merged == p3

-- ============================================================================
-- Unit Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "SourceLocation Advanced QuickCheck Tests"
  [ testGroup "Position Properties"
    [ testProperty "startPos is always valid" prop_startPosValid
    , testProperty "posAfter correctly handles newline" prop_posAfterNewline
    , testProperty "posAfter correctly handles tab" prop_posAfterTab
    , testProperty "posAfter correctly handles regular characters" prop_posAfterRegular
    , testProperty "posAt creates valid positions" prop_posAtValid
    ]

  , testGroup "Span Properties"
    [ testProperty "emptySpan creates valid spans" prop_emptySpanValid
    , testProperty "spanBetween creates valid spans when start <= end" prop_spanBetweenValid
    , testProperty "mergeSpans contains both original spans" prop_mergeSpansContains
    , testProperty "span ordering is consistent with position ordering" prop_spanOrderingConsistent
    ]

  , testGroup "Located Value Properties"
    [ testProperty "locatedAt creates valid located values" prop_locatedAtValid
    , testProperty "mapLocated preserves location" prop_mapLocatedPreservesLocation
    ]

  , testGroup "Position Advancement Properties"
    [ testProperty "advancePosByText is consistent with advancePosBy" prop_advancePosByTextConsistent
    , testProperty "advancePosByLine correctly advances line number" prop_advancePosByLineCorrect
    , testProperty "position advancement is associative for strings" prop_advancePosAssociative
    ]

  , testGroup "Edge Case Tests"
    [ testCase "startPos has correct values" $ do
        posLine startPos @?= 1
        posColumn startPos @?= 1
        posOffset startPos @?= 0

    , testCase "empty span at startPos is valid" $ do
        let span = emptySpan startPos
        isValidSpan span @?= True
        spanStart span @?= startPos
        spanEnd span @?= startPos

    , testCase "span covering same position is valid" $ do
        let pos = posAt 5 10
            span = spanBetween pos pos
        isValidSpan span @?= True
        spanStart span @?= pos
        spanEnd span @?= pos

    , testCase "located value operations work correctly" $ do
        let pos = posAt 3 7
            span = spanFrom pos
            located = locatedWithSpan span "test"
        locatedValue located @?= "test"
        locatedPos located @?= pos
        locatedSpan located @?= span
        let doubled = mapLocated (*2) (locatedWithSpan span 21)
        locatedValue doubled @?= 42
        locatedSpan doubled @?= span
    ]
  ]