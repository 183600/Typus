{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalSourceLocationMathQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import SourceLocation
import Compiler.Errors.Core (ErrorLocation(..))
import Data.Semigroup ((<>))

-- | Test source location mathematical properties
testSourceLocationMathProperties :: TestTree
testSourceLocationMathProperties = testGroup "Source Location Math Properties"
  [ testProperty "posAfter newline increments line" propPosAfterNewlineIncrementsLine
  , testProperty "posAfter tab aligns to next tab stop" propPosAfterTabAligns
  , testProperty "posAfter regular char increments column" propPosAfterRegularChar
  , testProperty "spanBetween creates valid span" propSpanBetweenValid
  , testProperty "mergeSpans contains both original spans" propMergeSpansContains
  , testProperty "mergeSpans is commutative" propMergeSpansCommutative
  , testProperty "advancePosBy is consistent with fold" propAdvancePosByConsistent
  , testProperty "locatedAt creates span at position" propLocatedAtCreatesSpan
  , testProperty "mapLocated preserves location" propMapLocatedPreserves
  ]

-- | Advancing position past newline should increment line number and reset column
propPosAfterNewlineIncrementsLine :: Positive Int -> Positive Int -> Bool
propPosAfterNewlineIncrementsLine (Positive line) (Positive col) =
  let pos = posAt line col
      newPos = posAfter '\n' pos
  in posLine newPos == line + 1 && posColumn newPos == 1 && posOffset newPos == posOffset pos + 1

-- | Advancing position past tab should align to next tab stop (8-character boundaries)
propPosAfterTabAligns :: Positive Int -> Positive Int -> Bool
propPosAfterTabAligns (Positive line) (Positive col) =
  let pos = posAt line col
      newPos = posAfter '\t' pos
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in posLine newPos == line && posColumn newPos == expectedCol && posOffset newPos == posOffset pos + 1

-- | Advancing position past regular character should increment column and offset
propPosAfterRegularChar :: Positive Int -> Positive Int -> Char -> Property
propPosAfterRegularChar (Positive line) (Positive col) c =
  c `notElem` "\n\t" ==> 
  let pos = posAt line col
      newPos = posAfter c pos
  in posLine newPos == line && 
     posColumn newPos == col + 1 && 
     posOffset newPos == posOffset pos + 1

-- | spanBetween should always create a valid span (start <= end)
propSpanBetweenValid :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
propSpanBetweenValid (Positive line1) (Positive col1) (Positive line2) (Positive col2) =
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      span = spanBetween pos1 pos2
  in isValidSpan span

-- | mergeSpans should create a span that contains both original spans
propMergeSpansContains :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> 
                         Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
propMergeSpansContains (Positive line1) (Positive col1) (Positive line2) (Positive col2)
                       (Positive line3) (Positive col3) (Positive line4) (Positive col4) =
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      pos3 = posAt line3 col3
      pos4 = posAt line4 col4
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos3 pos4
      merged = mergeSpans span1 span2
  in spanStart merged <= spanStart span1 &&
     spanEnd merged >= spanEnd span1 &&
     spanStart merged <= spanStart span2 &&
     spanEnd merged >= spanEnd span2

-- | mergeSpans should be commutative
propMergeSpansCommutative :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> 
                           Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
propMergeSpansCommutative (Positive line1) (Positive col1) (Positive line2) (Positive col2)
                          (Positive line3) (Positive col3) (Positive line4) (Positive col4) =
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      pos3 = posAt line3 col3
      pos4 = posAt line4 col4
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos3 pos4
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in merged1 == merged2

-- | advancePosBy should be consistent with folding over posAfter
propAdvancePosByConsistent :: Positive Int -> Positive Int -> String -> Property
propAdvancePosByConsistent (Positive line) (Positive col) s =
  let pos = posAt line col
      advanced1 = advancePosBy s pos
      advanced2 = foldl (flip posAfter) pos s
  in advanced1 == advanced2

-- | locatedAt should create a span that starts and ends at the given position
propLocatedAtCreatesSpan :: Positive Int -> Positive Int -> Int -> Property
propLocatedAtCreatesSpan (Positive line) (Positive col) value =
  let pos = posAt line col
      located = locatedAt pos value
  in locSpan located == emptySpan pos && locPos located == pos

-- | mapLocated should preserve the location information
propMapLocatedPreserves :: Positive Int -> Positive Int -> Int -> Int -> Property
propMapLocatedPreserves (Positive line) (Positive col) value transform =
  let pos = posAt line col
      located = locatedAt pos value
      mapped = mapLocated (+ transform) located
  in locSpan mapped == locSpan located && 
     locPos mapped == locPos located &&
     locValue mapped == value + transform

-- | Test edge cases and special scenarios
testSourceLocationEdgeCases :: TestTree
testSourceLocationEdgeCases = testGroup "Source Location Edge Cases"
  [ testCase "start position is 1:1:0" $ 
      startPos @?= SourcePos 1 1 0
  , testCase "empty span at start position" $
      let span = emptySpan startPos
      in spanStart span @?= startPos && spanEnd span @?= startPos
  , testCase "merge spans with same start/end" $
      let pos = posAt 5 10
          span1 = spanBetween pos pos
          span2 = spanBetween pos pos
          merged = mergeSpans span1 span2
      in merged @?= span1
  , testCase "advance position by empty string" $
      let pos = posAt 3 5
          advanced = advancePosBy "" pos
      in advanced @?= pos
  , testCase "located with span preserves span" $
      let start = posAt 2 3
          end = posAt 4 7
          span = spanBetween start end
          located = locatedWithSpan span "test"
      in locSpan located @?= span && locPos located @?= start
  ]

-- | Test error location conversion
testErrorLocationConversion :: TestTree
testErrorLocationConversion = testGroup "Error Location Conversion"
  [ testCase "position to error location" $
      let pos = posAt 10 20
          errLoc = toErrorLocation pos
      in line errLoc @?= 10 && column errLoc @?= 20 &&
         filePath errLoc @?= Nothing &&
         endLine errLoc @?= Nothing && endColumn errLoc @?= Nothing
  , testCase "span to error location with range" $
      let start = posAt 5 10
          end = posAt 7 15
          span = spanBetween start end
          errLoc = toErrorLocationWithSpan span
      in line errLoc @?= 5 && column errLoc @?= 10 &&
         endLine errLoc @?= Just 7 && endColumn errLoc @?= Just 15
  ]

-- | Test location tracking monad
testLocationTracking :: TestTree
testLocationTracking = testGroup "Location Tracking"
  [ testCase "run location tracker from start" $
      let result = runLocationTracker getCurrentPos
      in result @?= startPos
  , testCase "set and get current position" $
      let pos = posAt 3 7
          (_, finalPos) = withLocationTracking pos $ do
            setCurrentPos pos
            getCurrentPos
      in finalPos @?= pos
  , testCase "mark span start and end" $
      let start = posAt 2 5
          end = posAt 4 8
          (span, _) = withLocationTracking start $ do
            spanStart <- markSpanStart
            setCurrentPos end
            markSpanEnd spanStart
      in spanStart span @?= start && spanEnd span @?= end
  ]

-- | All source location math tests
testSourceLocationMathQuickCheck :: TestTree
testSourceLocationMathQuickCheck = testGroup "New Cabal Source Location Math QuickCheck Tests"
  [ testSourceLocationMathProperties
  , testSourceLocationEdgeCases
  , testErrorLocationConversion
  , testLocationTracking
  ]

-- Helper type for positive integers
newtype Positive a = Positive a
  deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Positive a) where
  arbitrary = Positive <$> arbitrary `suchThat` (> 0)