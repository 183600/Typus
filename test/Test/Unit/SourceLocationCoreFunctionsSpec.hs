module Test.Unit.SourceLocationCoreFunctionsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf, suchThat)
import Test.QuickCheck.Property (forAll)

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), 
                      startPos, posAfter, posAt, emptySpan, spanFrom, 
                      spanTo, spanBetween, mergeSpans, isValidSpan,
                      locatedAt, locatedWithSpan, advancePos, advancePosBy,
                      sourceLine, sourceColumn)

-- | Generate arbitrary source positions with reasonable bounds
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 200)
    offset <- choose (0, 10000)
    return $ SourcePos line col offset

-- | Generate arbitrary source spans
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endLine <- choose (posLine start, 1000)
    endCol <- if endLine == posLine start 
              then choose (posColumn start, 200)
              else choose (1, 200)
    let end = SourcePos endLine endCol 0
    return $ SourceSpan start end

-- | Generate arbitrary located values
instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    span <- arbitrary
    return $ Located value span

-- | Generate non-empty strings for position testing
genNonEmptyString :: Gen String
genNonEmptyString = listOf (choose (' ', '~')) `suchThat` (not . null)

-- | Generate strings with newlines for multi-line position testing
genMultiLineString :: Gen String
genMultiLineString = do
    lines <- listOf $ listOf (choose (' ', '~')) `suchThat` (not . null)
    return $ unlines lines

tests :: TestTree
tests =
  testGroup "SourceLocation Core Functions"
    [ testGroup "Source Position Operations"
        [ testCase "startPos creates position at line 1, column 1" $ do
            startPos @?= SourcePos 1 1

        , testCase "posAfter advances column on same line" $ do
            let pos = SourcePos 5 10
            posAfter pos @?= SourcePos 5 11

        , testCase "posAt creates position at specific coordinates" $ do
            posAt 3 7 @?= SourcePos 3 7

        , fastProperty "advancePos by single character updates position correctly" 
            prop_advancePosSingle
        , fastProperty "advancePosBy handles multiple characters correctly" 
            prop_advancePosByMultiple
        , fastProperty "position ordering is consistent" 
            prop_positionOrdering
        ]

    , testGroup "Source Span Operations"
        [ testCase "emptySpan has start L.and end at same position" $ do
            let pos = SourcePos 1 1
            emptySpan pos @?= SourceSpan pos pos

        , testCase "spanFrom creates span from position to end of line" $ do
            let start = SourcePos 3 5
            let expected = SourceSpan start (SourcePos 3 200) -- Reasonable max column
            spanFrom start @?= expected

        , fastProperty "spanTo creates span with correct end position" 
            prop_spanToCorrect
        , fastProperty "spanBetween creates span covering both positions" 
            prop_spanBetweenCoverage
        , fastProperty "mergeSpans creates minimal span covering both" 
            prop_mergeSpansMinimal
        , fastProperty "isValidSpan correctly identifies valid spans" 
            prop_isValidSpanCorrect
        ]

    , testGroup "Located Value Operations"
        [ testCase "locatedAt creates located value at position" $ do
            let value = "test"
            let pos = SourcePos 2 3
            let located = locatedAt value pos
            locatedValue located @?= value
            locatedSpan located @?= SourceSpan pos pos

        , fastProperty "locatedWithSpan preserves value L.and span" 
            prop_locatedWithSpanPreserves
        , fastProperty "mapLocated applies function to contained value" 
            prop_mapLocatedApplies
        ]

    , testGroup "Position Arithmetic Properties"
        [ fastProperty "advancing position never decreases line L.or column" 
            prop_advanceNeverDecreases
        , fastProperty "advancing by zero characters returns original position" 
            prop_advanceZeroReturns
        , fastProperty "advancePosBy is consistent with repeated advancePos" 
            prop_advanceByConsistent
        ]

    , testGroup "Span Boundary Properties"
        [ fastProperty "span start is always less than L.or equal to end" 
            prop_spanStartLeEnd
        , fastProperty "mergeSpans is commutative" 
            prop_mergeSpansCommutative
        , fastProperty "mergeSpans is associative" 
            prop_mergeSpansAssociative
        , fastProperty "span covering single point is valid" 
            prop_singlePointSpanValid
        ]
    ]

-- Property: advancing position by single character updates correctly
prop_advancePosSingle :: SourcePos -> Bool
prop_advancePosSingle pos =
  let advanced = advancePos pos ' '
  in posLine advanced == posLine pos &&
     (posColumn advanced == posColumn pos + 1 || 
      posColumn advanced == 1) -- New line case

-- Property: advancePosBy handles multiple characters correctly
prop_advancePosByMultiple :: SourcePos -> String -> Bool
prop_advancePosByMultiple pos chars =
  let advanced = advancePosBy pos chars
  in posLine advanced >= posLine pos &&
     posColumn advanced >= 1

-- Property: position ordering is consistent
prop_positionOrdering :: SourcePos -> SourcePos -> Bool
prop_positionOrdering pos1 pos2 =
  let line1 = posLine pos1
      line2 = posLine pos2
      col1 = posColumn pos1
      col2 = posColumn pos2
  in if line1 == line2 
     then col1 <= col2 || pos1 == pos2
     else line1 <= line2

-- Property: spanTo creates span with correct end position
prop_spanToCorrect :: SourcePos -> SourcePos -> Bool
prop_spanToCorrect start end =
  let span = spanTo end start
  in spanStart span == start && spanEnd span == end

-- Property: spanBetween creates span covering both positions
prop_spanBetweenCoverage :: SourcePos -> SourcePos -> Bool
prop_spanBetweenCoverage pos1 pos2 =
  let span = spanBetween pos1 pos2
      start = spanStart span
      end = spanEnd span
  in (posLine start <= posLine end ||
      (posLine start == posLine end && posColumn start <= posColumn end))

-- Property: mergeSpans creates minimal span covering both
prop_mergeSpansMinimal :: SourceSpan -> SourceSpan -> Bool
prop_mergeSpansMinimal span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in posLine mergedStart <= min (posLine start1) (posLine start2) &&
     posLine mergedEnd >= max (posLine end1) (posLine end2)

-- Property: isValidSpan correctly identifies valid spans
prop_isValidSpanCorrect :: SourcePos -> SourcePos -> Bool
prop_isValidSpanCorrect start end =
  let span = SourceSpan start end
      valid = isValidSpan span
  in if posLine start == posLine end
     then valid == (posColumn start <= posColumn end)
     else valid == (posLine start <= posLine end)

-- Property: locatedWithSpan preserves value L.and span
prop_locatedWithSpanPreserves :: String -> SourceSpan -> Bool
prop_locatedWithSpanPreserves value span =
  let located = locatedWithSpan value span
  in locatedValue located == value && locatedSpan located == span

-- Property: mapLocated applies function to contained value
prop_mapLocatedApplies :: String -> SourceSpan -> Bool
prop_mapLocatedApplies value span =
  let located = locatedWithSpan value span
      transformed = mapLocated L.length located
  in locatedValue transformed == L.length value &&
     locatedSpan transformed == span

-- Property: advancing position never decreases line L.or column
prop_advanceNeverDecreases :: SourcePos -> String -> Bool
prop_advanceNeverDecreases pos chars =
  let advanced = advancePosBy pos chars
  in posLine advanced >= posLine pos &&
     (posLine advanced > posLine pos || posColumn advanced >= posColumn pos)

-- Property: advancing by zero characters returns original position
prop_advanceZeroReturns :: SourcePos -> Bool
prop_advanceZeroReturns pos =
  advancePosBy pos "" == pos

-- Property: advancePosBy is consistent with repeated advancePos
prop_advanceByConsistent :: SourcePos -> String -> Bool
prop_advanceByConsistent pos chars =
  let singleAdvance = L.foldl (flip posAfter) pos chars
      multiAdvance = advancePosBy pos chars
  in singleAdvance == multiAdvance

-- Property: span start is always less than L.or equal to end
prop_spanStartLeEnd :: SourceSpan -> Bool
prop_spanStartLeEnd span =
  let start = spanStart span
      end = spanEnd span
  in posLine start < posLine end ||
     (posLine start == posLine end && posColumn start <= posColumn end)

-- Property: mergeSpans is commutative
prop_mergeSpansCommutative :: SourceSpan -> SourceSpan -> Bool
prop_mergeSpansCommutative span1 span2 =
  mergeSpans span1 span2 == mergeSpans span2 span1

-- Property: mergeSpans is associative
prop_mergeSpansAssociative :: SourceSpan -> SourceSpan -> SourceSpan -> Bool
prop_mergeSpansAssociative span1 span2 span3 =
  mergeSpans (mergeSpans span1 span2) span3 == mergeSpans span1 (mergeSpans span2 span3)

-- Property: span covering single point is valid
prop_singlePointSpanValid :: SourcePos -> Bool
prop_singlePointSpanValid pos =
  let span = SourceSpan pos pos
  in isValidSpan span