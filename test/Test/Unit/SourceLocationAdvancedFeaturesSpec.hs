{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationAdvancedFeaturesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, elements, listOf, oneof, sized)
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
import qualified Data.Text as T
import Data.Char (isSpace)

-- | 生成有效的SourcePos
genValidSourcePos :: Gen SourcePos
genValidSourcePos = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  offset <- choose (0, 1000000)
  return $ SourcePos line col offset

-- | 生成有效的SourceSpan
genValidSourceSpan :: Gen SourceSpan
genValidSourceSpan = do
  startLine <- choose (1, 100)
  startCol <- choose (1, 100)
  startOffset <- choose (0, 10000)
  endLine <- choose (startLine, startLine + 100)
  endCol <- if endLine == startLine then choose (startCol, startCol + 100) else choose (1, 100)
  endOffset <- choose (startOffset, startOffset + 10000)
  return $ SourceSpan (SourcePos startLine startCol startOffset) (SourcePos endLine endCol endOffset)

instance Arbitrary SourcePos where
  arbitrary = genValidSourcePos

instance Arbitrary SourceSpan where
  arbitrary = genValidSourceSpan

-- ============================================================================
-- Advanced Source Position Tests
-- ============================================================================

-- Property: posAfter correctly handles different character types
prop_posAfter_handles_chars :: Char -> SourcePos -> Property
prop_posAfter_handles_chars char pos = 
  let newPos = posAfter char pos
      expectedLine = if char == '\n' then posLine pos + 1 else posLine pos
      expectedCol = case char of
        '\n' -> 1
        '\t' -> ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
        _ -> posColumn pos + 1
      expectedOffset = posOffset pos + 1
  in property $ posLine newPos === expectedLine .&&.
               posColumn newPos === expectedCol .&&.
               posOffset newPos === expectedOffset

-- Property: advancePosBy correctly processes strings
prop_advancePosBy_processes_strings :: String -> SourcePos -> Property
prop_advancePosBy_processes_strings str pos =
  let finalPos = advancePosBy str pos
      expectedPos = L.foldl (flip posAfter) pos str
  in property $ finalPos === expectedPos

-- Property: advancePosByText works same as advancePosBy
prop_advancePosByText_matches_advancePosBy :: String -> SourcePos -> Property
prop_advancePosByText_matches_advancePosBy str pos =
  let textPos = advancePosByText (T.pack str) pos
      stringPos = advancePosBy str pos
  in property $ textPos === stringPos

-- Property: advancePosByLine correctly advances line numbers
prop_advancePosByLine_advances_lines :: Int -> Int -> Int -> Property
prop_advancePosByLine_advances_lines startLine startCol numLines =
  startLine > 0 && startCol > 0 && numLines >= 0 ==>
  let pos = posAtLineCol startLine startCol 0
      newPos = advancePosByLine numLines pos
  in property $ posLine newPos === startLine + numLines .&&.
               posColumn newPos === 1

-- ============================================================================
-- Advanced Source Span Tests
-- ============================================================================

-- Property: mergeSpans creates span covering both input spans
prop_mergeSpans_covers_both :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_covers_both span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in property $ mergedStart <= start1 .&&. mergedEnd >= end1 .&&.
               mergedStart <= start2 .&&. mergedEnd >= end2

-- Property: spanBetween creates valid span
prop_spanBetween_creates_valid :: SourcePos -> SourcePos -> Property
prop_spanBetween_creates_valid pos1 pos2 =
  let span = spanBetween pos1 pos2
      start = spanStart span
      end = spanEnd span
  in property $ (start <= pos1 && end >= pos1) .||. (start <= pos2 && end >= pos2)

-- Property: isValidSpan correctly identifies valid spans
prop_isValidSpan_identifies_valid :: SourceSpan -> Bool
prop_isValidSpan_identifies_valid span = 
  let start = spanStart span
      end = spanEnd span
  in isValidSpan span == (start <= end)

-- ============================================================================
-- Advanced Located Value Tests
-- ============================================================================

-- Property: mapLocated preserves location information
prop_mapLocated_preserves_location :: SourceSpan -> String -> String -> Property
prop_mapLocated_preserves_location span prefix suffix =
  let value = prefix ++ "content" ++ suffix
      located = locatedWithSpan span value
      mapped = mapLocated L.reverse located
  in property $ locatedSpan located === locatedSpan mapped .&&.
               locatedPos located === locatedPos mapped

-- Property: locatedAt creates span at position
prop_locatedAt_creates_span_at_pos :: SourcePos -> String -> Property
prop_locatedAt_creates_span_at_pos pos value =
  let located = locatedAt pos value
      span = locatedSpan located
  in property $ spanStart span === pos .&&. spanEnd span === pos

-- ============================================================================
-- Advanced Error Location Tests
-- ============================================================================

-- Property: toErrorLocation preserves position information
prop_toErrorLocation_preserves_position :: SourcePos -> Property
prop_toErrorLocation_preserves_position pos =
  let errLoc = toErrorLocation pos
  in property $ line errLoc === posLine pos .&&.
               column errLoc === posColumn pos

-- Property: toErrorLocationWithSpan preserves span information
prop_toErrorLocationWithSpan_preserves_span :: SourceSpan -> Property
prop_toErrorLocationWithSpan_preserves_span span =
  let errLoc = toErrorLocationWithSpan span
      start = spanStart span
      end = spanEnd span
  in property $ line errLoc === posLine start .&&.
               column errLoc === posColumn start .&&.
               endLine errLoc === Just (posLine end) .&&.
               endColumn errLoc === Just (posColumn end)

-- ============================================================================
-- Unit Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "SourceLocation Advanced Features Tests"
    [ testGroup "Source Position Properties"
        [ fastProperty "posAfter handles different character types" prop_posAfter_handles_chars
        , fastProperty "advancePosBy processes strings correctly" prop_advancePosBy_processes_strings
        , fastProperty "advancePosByText matches advancePosBy" prop_advancePosByText_matches_advancePosBy
        , fastProperty "advancePosByLine advances line numbers" prop_advancePosByLine_advances_lines
        ]
    , testGroup "Source Span Properties"
        [ fastProperty "mergeSpans creates span covering both input spans" prop_mergeSpans_covers_both
        , fastProperty "spanBetween creates valid span" prop_spanBetween_creates_valid
        , fastProperty "isValidSpan correctly identifies valid spans" prop_isValidSpan_identifies_valid
        ]
    , testGroup "Located Value Properties"
        [ fastProperty "mapLocated preserves location information" prop_mapLocated_preserves_location
        , fastProperty "locatedAt creates span at position" prop_locatedAt_creates_span_at_pos
        ]
    , testGroup "Error Location Properties"
        [ fastProperty "toErrorLocation preserves position information" prop_toErrorLocation_preserves_position
        , fastProperty "toErrorLocationWithSpan preserves span information" prop_toErrorLocationWithSpan_preserves_span
        ]
    , testGroup "Unit Tests"
        [ testCase "startPos has correct initial values" $ do
            posLine startPos @?= 1
            posColumn startPos @?= 1
            posOffset startPos @?= 0

        , testCase "posAt creates position at specified line L.and column" $ do
            let pos = posAt 5 10
            posLine pos @?= 5
            posColumn pos @?= 10

        , testCase "emptySpan creates span with same start L.and end" $ do
            let pos = posAt 3 7
                span = emptySpan pos
            spanStart span @?= pos
            spanEnd span @?= pos

        , testCase "spanFrom creates empty span at position" $ do
            let pos = posAt 2 4
                span = spanFrom pos
            spanStart span @?= pos
            spanEnd span @?= pos

        , testCase "spanTo creates empty span at position" $ do
            let pos = posAt 6 8
                span = spanTo pos
            spanStart span @?= pos
            spanEnd span @?= pos

        , testCase "locatedWithSpan creates located value with correct span" $ do
            let pos1 = posAt 1 1
                pos2 = posAt 1 5
                span = spanBetween pos1 pos2
                value = "test"
                located = locatedWithSpan span value
            locatedValue located @?= value
            locatedSpan located @?= span
            locatedPos located @?= pos1

        , testCase "mapLocated applies function to value but preserves location" $ do
            let pos = posAt 2 3
                span = emptySpan pos
                value = "hello"
                located = locatedWithSpan span value
                mapped = mapLocated L.reverse located
            locatedValue mapped @?= "olleh"
            locatedSpan mapped @?= span
            locatedPos mapped @?= pos
        ]
    ]