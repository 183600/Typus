{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.NewCabalSourceLocationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, oneof, elements, suchThat)
import Data.Ord (comparing)
import Data.List (sort)

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
  , advancePos
  , advancePosBy
  , advancePosByText
  , advancePosByLine
  , toErrorLocation
  , toErrorLocationWithSpan
  )

import qualified Data.Text as T

-- ============================================================================
-- Additional Arbitrary instances for SourceLocation testing
-- ============================================================================

-- Generate valid line numbers (1-based)
genLineNumber :: Gen Int
genLineNumber = choose (1, 1000)

-- Generate valid column numbers (1-based)
genColumnNumber :: Gen Int
genColumnNumber = choose (1, 200)

-- Generate valid offsets (0-based)
genOffset :: Gen Int
genOffset = choose (0, 100000)

-- Generate valid characters for position advancement
genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r.,;:!?()[]{}+-*/=<>'\""

-- Generate text for position advancement
genText :: Gen String
genText = listOf genChar

-- Generate valid source positions
genValidSourcePos :: Gen SourcePos
genValidSourcePos = SourcePos <$> genLineNumber <*> genColumnNumber <*> genOffset

-- Generate valid source spans (where start <= end)
genValidSourceSpan :: Gen SourceSpan
genValidSourceSpan = do
  startLine <- genLineNumber
  startCol <- genColumnNumber
  startOffset <- genOffset
  let start = SourcePos startLine startCol startOffset
  
  -- Ensure end position comes after start
  endLine <- choose (startLine, startLine + 100)
  endCol <- if endLine == startLine 
            then choose (startCol, startCol + 100)
            else genColumnNumber
  endOffset <- choose (startOffset, startOffset + 10000)
  let end = SourcePos endLine endCol endOffset
  
  return $ SourceSpan start end

-- Generate located values
genLocatedInt :: Gen (Located Int)
genLocatedInt = Located <$> choose (0, 1000) <*> genValidSourcePos <*> genValidSourceSpan

genLocatedString :: Gen (Located String)
genLocatedString = Located <$> listOf genChar <*> genValidSourcePos <*> genValidSourceSpan

-- ============================================================================
-- Property Tests for SourcePos
-- ============================================================================

-- Position properties
prop_posAfterAdvancesLineForNewline :: Char -> SourcePos -> Bool
prop_posAfterAdvancesLineForNewline pos sourcePos =
  let newPos = posAfter '\n' sourcePos
  in posLine newPos == posLine sourcePos + 1 && 
     posColumn newPos == 1 &&
     posOffset newPos == posOffset sourcePos + 1

prop_posAfterAdvancesColumnForRegularChar :: Char -> SourcePos -> Property
prop_posAfterAdvancesColumnForRegularChar char sourcePos =
  char /= '\n' && char /= '\t' ==>
    let newPos = posAfter char sourcePos
    in posLine newPos == posLine sourcePos &&
       posColumn newPos == posColumn sourcePos + 1 &&
       posOffset newPos == posOffset sourcePos + 1

prop_posAfterHandlesTabCorrectly :: SourcePos -> Bool
prop_posAfterHandlesTabCorrectly sourcePos =
  let newPos = posAfter '\t' sourcePos
      expectedCol = ((posColumn sourcePos - 1) `div` 8 + 1) * 8 + 1
  in posLine newPos == posLine sourcePos &&
     posColumn newPos == expectedCol &&
     posOffset newPos == posOffset sourcePos + 1

prop_advancePosByHandlesMultipleChars :: String -> SourcePos -> Bool
prop_advancePosByHandlesMultipleChars chars sourcePos =
  let finalPos = advancePosBy chars sourcePos
      manualPos = foldl (flip posAfter) sourcePos chars
  in finalPos == manualPos

prop_advancePosByTextMatchesStringVersion :: String -> SourcePos -> Bool
prop_advancePosByTextMatchesStringVersion text sourcePos =
  advancePosByText (T.pack text) sourcePos == advancePosBy text sourcePos

prop_advancePosByLineAdvancesCorrectly :: Int -> SourcePos -> Bool
prop_advancePosByLineAdvancesCorrectly numLines sourcePos =
  let newPos = advancePosByLine numLines sourcePos
  in posLine newPos == posLine sourcePos + numLines &&
     posColumn newPos == 1 &&
     posOffset newPos == posOffset sourcePos + numLines

-- ============================================================================
-- Property Tests for SourceSpan
-- ============================================================================

-- Span properties
prop_emptySpanHasZeroLength :: SourcePos -> Bool
prop_emptySpanHasZeroLength pos =
  let span = emptySpan pos
  in spanStart span == pos && spanEnd span == pos

prop_spanFromCreatesEmptySpan :: SourcePos -> Bool
prop_spanFromCreatesEmptySpan pos =
  let span = spanFrom pos
  in spanStart span == pos && spanEnd span == pos

prop_spanToCreatesEmptySpan :: SourcePos -> Bool
prop_spanToCreatesEmptySpan pos =
  let span = spanTo pos
  in spanStart span == pos && spanEnd span == pos

prop_spanBetweenOrdersPositions :: SourcePos -> SourcePos -> Property
prop_spanBetweenOrdersPositions pos1 pos2 =
  let span = spanBetween pos1 pos2
  in spanStart span <= spanEnd span

prop_mergeSpansContainsBothSpans :: SourceSpan -> SourceSpan -> Bool
prop_mergeSpansContainsBothSpans span1 span2 =
  let merged = mergeSpans span1 span2
  in spanStart merged <= spanStart span1 &&
     spanEnd merged >= spanEnd span1 &&
     spanStart merged <= spanStart span2 &&
     spanEnd merged >= spanEnd span2

prop_mergeSpansIsCommutative :: SourceSpan -> SourceSpan -> Bool
prop_mergeSpansIsCommutative span1 span2 =
  mergeSpans span1 span2 == mergeSpans span2 span1

prop_mergeSpansIsAssociative :: SourceSpan -> SourceSpan -> SourceSpan -> Bool
prop_mergeSpansIsAssociative span1 span2 span3 =
  mergeSpans span1 (mergeSpans span2 span3) == mergeSpans (mergeSpans span1 span2) span3

prop_isValidSpanChecksOrder :: SourcePos -> SourcePos -> Bool
prop_isValidSpanChecksOrder pos1 pos2 =
  let span = SourceSpan pos1 pos2
  in isValidSpan span == (pos1 <= pos2)

-- ============================================================================
-- Property Tests for Located
-- ============================================================================

-- Located properties
prop_locatedAtUsesPositionForSpan :: SourcePos -> Int -> Bool
prop_locatedAtUsesPositionForSpan pos value =
  let located = locatedAt pos value
  in locatedPos located == pos &&
     spanStart (locatedSpan located) == pos &&
     spanEnd (locatedSpan located) == pos

prop_locatedWithSpanPreservesSpan :: SourceSpan -> Int -> Bool
prop_locatedWithSpanPreservesSpan span value =
  let located = locatedWithSpan span value
  in locatedSpan located == span &&
     locatedPos located == spanStart span

prop_mapLocatedPreservesLocation :: Located Int -> Bool
prop_mapLocatedPreservesLocation located =
  let doubled = mapLocated (*2) located
  in locatedSpan doubled == locatedSpan located &&
     locatedPos doubled == locatedPos located

prop_locatedValueExtractsCorrectly :: Located Int -> Bool
prop_locatedValueExtractsCorrectly located =
  locatedValue located == locValue located

-- ============================================================================
-- Property Tests for Error Location Conversion
-- ============================================================================

-- Error location properties
prop_toErrorLocationPreservesPosition :: SourcePos -> Bool
prop_toErrorLocationPreservesPosition pos =
  let errLoc = toErrorLocation pos
  in line errLoc == posLine pos &&
     column errLoc == posColumn pos

prop_toErrorLocationWithSpanPreservesRange :: SourceSpan -> Bool
prop_toErrorLocationWithSpanPreservesRange span =
  let errLoc = toErrorLocationWithSpan span
      start = spanStart span
      end = spanEnd span
  in line errLoc == posLine start &&
     column errLoc == posColumn start &&
     endLine errLoc == Just (posLine end) &&
     endColumn errLoc == Just (posColumn end)

-- ============================================================================
-- Unit Tests for Edge Cases
-- ============================================================================

tests :: TestTree
tests = testGroup "SourceLocation QuickCheck Tests"
  [ testGroup "SourcePos properties"
    [ fastProperty "posAfter advances line for newline" prop_posAfterAdvancesLineForNewline
    , fastProperty "posAfter advances column for regular char" prop_posAfterAdvancesColumnForRegularChar
    , fastProperty "posAfter handles tab correctly" prop_posAfterHandlesTabCorrectly
    , fastProperty "advancePosBy handles multiple chars" prop_advancePosByHandlesMultipleChars
    , fastProperty "advancePosByText matches string version" prop_advancePosByTextMatchesStringVersion
    , fastProperty "advancePosByLine advances correctly" prop_advancePosByLineAdvancesCorrectly
    , testCase "startPos has correct initial values" $
        startPos @?= SourcePos 1 1 0
    , testCase "posAt creates position at specific line and column" $
        posAt 5 10 @?= SourcePos 5 10 0
    , testCase "posAtLineCol creates position with full info" $
        posAtLineCol 3 7 100 @?= SourcePos 3 7 100
    ]

  , testGroup "SourceSpan properties"
    [ fastProperty "emptySpan has zero length" prop_emptySpanHasZeroLength
    , fastProperty "spanFrom creates empty span" prop_spanFromCreatesEmptySpan
    , fastProperty "spanTo creates empty span" prop_spanToCreatesEmptySpan
    , fastProperty "spanBetween orders positions" prop_spanBetweenOrdersPositions
    , fastProperty "mergeSpans contains both spans" prop_mergeSpansContainsBothSpans
    , fastProperty "mergeSpans is commutative" prop_mergeSpansIsCommutative
    , fastProperty "mergeSpans is associative" prop_mergeSpansIsAssociative
    , fastProperty "isValidSpan checks order" prop_isValidSpanChecksOrder
    , testCase "isValidSpan handles equal positions" $
        let pos = posAt 5 10
            span = spanBetween pos pos
        in assertBool "span with equal start and end should be valid" $ isValidSpan span
    ]

  , testGroup "Located properties"
    [ fastProperty "locatedAt uses position for span" prop_locatedAtUsesPositionForSpan
    , fastProperty "locatedWithSpan preserves span" prop_locatedWithSpanPreservesSpan
    , fastProperty "mapLocated preserves location" prop_mapLocatedPreservesLocation
    , fastProperty "locatedValue extracts correctly" prop_locatedValueExtractsCorrectly
    , testCase "HasLocation instance works for Located" $
        let located = locatedAt startPos 42
        in getLocation located @?= locatedSpan located
    ]

  , testGroup "Error location conversion properties"
    [ fastProperty "toErrorLocation preserves position" prop_toErrorLocationPreservesPosition
    , fastProperty "toErrorLocationWithSpan preserves range" prop_toErrorLocationWithSpanPreservesRange
    , testCase "toErrorLocation sets optional fields to Nothing" $
        let pos = posAt 10 20
            errLoc = toErrorLocation pos
        in do
          endLine errLoc @?= Nothing
          endColumn errLoc @?= Nothing
          filePath errLoc @?= Nothing
    ]

  , testGroup "Edge case tests"
    [ testCase "advancePos handles empty string" $
        let pos = posAt 5 10
        in advancePosBy "" pos @?= pos

    , testCase "advancePosByText handles empty text" $
        let pos = posAt 5 10
        in advancePosByText T.empty pos @?= pos

    , testCase "advancePosByLine with zero lines" $
        let pos = posAt 5 10
        in advancePosByLine 0 pos @?= pos

    , testCase "mergeSpans with identical spans" $
        let span = spanBetween (posAt 1 1) (posAt 1 10)
        in mergeSpans span span @?= span

    , testCase "locatedWithSpan creates correct located value" $
        let span = spanBetween (posAt 3 5) (posAt 3 15)
            value = "test"
            located = locatedWithSpan span value
        in do
          locatedValue located @?= value
          locatedPos located @?= spanStart span
          locatedSpan located @?= span

    , testCase "mapLocated with identity function" $
        let located = locatedAt startPos 42
        in mapLocated id located @?= located

    , testCase "posAfter handles carriage return" $
        let pos = posAt 5 10
            newPos = posAfter '\r' pos
        in posLine newPos == posLine pos && 
           posColumn newPos == posColumn pos + 1 &&
           posOffset newPos == posOffset pos + 1

    , testCase "spanBetween with same position creates zero-length span" $
        let pos = posAt 10 20
            span = spanBetween pos pos
        in spanStart span == pos && spanEnd span == pos
    ]
  ]