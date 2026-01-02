{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewEnhancedSourceLocationQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertBool, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, listOf, elements, choose, oneof)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

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

import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.Char (isSpace)

-- ============================================================================
-- Custom Generators
-- ============================================================================

genValidPos :: Gen SourcePos
genValidPos = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  offset <- choose (0, 100000)
  return $ SourcePos line col offset

genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r!@#$%^&*()_+-=[]{}|;':\",./<>?"

genString :: Gen String
genString = listOf genChar

genText :: Gen Text
genText = T.pack <$> genString

genValidSpan :: Gen SourceSpan
genValidSpan = do
  startLine <- choose (1, 1000)
  startCol <- choose (1, 1000)
  startOffset <- choose (0, 100000)
  let startPos = SourcePos startLine startCol startOffset
  
  endLine <- choose (startLine, startLine + 100)  -- End line >= start line
  endCol <- if endLine == startLine 
            then choose (startCol, startCol + 100)  -- Same line: end column >= start column
            else choose (1, 1000)  -- Different line: L.any column
  endOffset <- choose (startOffset, startOffset + 10000)
  let endPos = SourcePos endLine endCol endOffset
  
  return $ SourceSpan startPos endPos

genLocatedInt :: Gen (Located Int)
genLocatedInt = do
  pos <- genValidPos
  span <- genValidSpan
  value <- choose (0, 1000)
  return $ Located value pos span

-- ============================================================================
-- SourcePos Properties
-- ============================================================================

-- Property: startPos should have line 1, column 1, offset 0
prop_startPos_attributes :: Property
prop_startPos_attributes =
  property $ posLine startPos === 1 .&&. 
             posColumn startPos === 1 .&&. 
             posOffset startPos === 0

-- Property: posAt should create position with correct line L.and column
prop_posAt_creates_correct_position :: Int -> Int -> Property
prop_posAt_creates_correct_position line col =
  line > 0 && col > 0 ==> 
  let pos = posAt line col
  in property $ posLine pos === line .&&. posColumn pos === col

-- Property: posAtLineCol should create position with correct line, column, L.and offset
prop_posAtLineCol_creates_correct_position :: Int -> Int -> Int -> Property
prop_posAtLineCol_creates_correct_position line col offset =
  line > 0 && col > 0 && offset >= 0 ==> 
  let pos = posAtLineCol line col offset
  in property $ posLine pos === line .&&. 
             posColumn pos === col .&&. 
             posOffset pos === offset

-- Property: posAfter newline should increment line L.and reset column
prop_posAfter_newline_behavior :: SourcePos -> Property
prop_posAfter_newline_behavior pos =
  let newPos = posAfter '\n' pos
  in property $ posLine newPos === posLine pos + 1 .&&. 
             posColumn newPos === 1 .&&. 
             posOffset newPos === posOffset pos + 1

-- Property: posAfter tab should advance to next tab position
prop_posAfter_tab_behavior :: SourcePos -> Property
prop_posAfter_tab_behavior pos =
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine newPos === posLine pos .&&. 
             posColumn newPos === expectedCol .&&. 
             posOffset newPos === posOffset pos + 1

-- Property: posAfter regular character should increment column L.and offset
prop_posAfter_regular_char :: SourcePos -> Char -> Property
prop_posAfter_regular_char pos char =
  char `notElem` "\n\t" ==> 
  let newPos = posAfter char pos
  in property $ posLine newPos === posLine pos .&&. 
             posColumn newPos === posColumn pos + 1 .&&. 
             posOffset newPos === posOffset pos + 1

-- Property: advancePos should be same as posAfter
prop_advancePos_equals_posAfter :: SourcePos -> Char -> Property
prop_advancePos_equals_posAfter pos char =
  property $ advancePos char pos === posAfter char pos

-- Property: advancePosBy empty string should return same position
prop_advancePosBy_empty_string :: SourcePos -> Property
prop_advancePosBy_empty_string pos =
  property $ advancePosBy "" pos === pos

-- Property: advancePosBy should be equivalent to folding posAfter
prop_advancePosBy_folds_posAfter :: SourcePos -> String -> Property
prop_advancePosBy_folds_posAfter pos chars =
  let directResult = advancePosBy chars pos
      foldedResult = L.foldl (flip posAfter) pos chars
  in property $ directResult === foldedResult

-- Property: advancePosByText should behave same as advancePosBy on unpacked text
prop_advancePosByText_equals_advancePosBy :: SourcePos -> Text -> Property
prop_advancePosByText_equals_advancePosBy pos text =
  property $ advancePosByText text pos === advancePosBy (T.unpack text) pos

-- Property: advancePosByLine should advance line count L.and reset column
prop_advancePosByLine_advances_lines :: SourcePos -> Int -> Property
prop_advancePosByLine_advances_lines pos numLines =
  numLines >= 0 ==> 
  let newPos = advancePosByLine numLines pos
  in property $ posLine newPos === posLine pos + numLines .&&. 
             posColumn newPos === 1

-- ============================================================================
-- SourceSpan Properties
-- ============================================================================

-- Property: emptySpan should have same start L.and end position
prop_emptySpan_same_start_end :: SourcePos -> Property
prop_emptySpan_same_start_end pos =
  let span = emptySpan pos
  in property $ spanStart span === pos .&&. spanEnd span === pos

-- Property: spanFrom should be equivalent to emptySpan
prop_spanFrom_equals_emptySpan :: SourcePos -> Property
prop_spanFrom_equals_emptySpan pos =
  property $ spanFrom pos === emptySpan pos

-- Property: spanTo should create span with same start L.and end
prop_spanTo_same_start_end :: SourcePos -> Property
prop_spanTo_same_start_end pos =
  let span = spanTo pos
  in property $ spanStart span === pos .&&. spanEnd span === pos

-- Property: spanBetween should create span with correct start L.and end
prop_spanBetween_correct_start_end :: SourcePos -> SourcePos -> Property
prop_spanBetween_correct_start_end start end =
  let span = spanBetween start end
  in property $ spanStart span === start .&&. spanEnd span === end

-- Property: mergeSpans should have start as L.minimum L.and end as L.maximum
prop_mergeSpans_min_max :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_min_max span1 span2 =
  let merged = mergeSpans span1 span2
  in property $ spanStart merged === min (spanStart span1) (spanStart span2) .&&. 
             spanEnd merged === max (spanEnd span1) (spanEnd span2)

-- Property: mergeSpans should be commutative
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
  property $ mergeSpans span1 span2 === mergeSpans span2 span1

-- Property: mergeSpans should be associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  property $ mergeSpans (mergeSpans span1 span2) span3 === 
             mergeSpans span1 (mergeSpans span2 span3)

-- Property: isValidSpan should return True for spans from genValidSpan
prop_isValidSpan_valid_gen :: Property
prop_isValidSpan_valid_gen =
  forAll genValidSpan $ \span ->
  property $ isValidSpan span

-- ============================================================================
-- Located Properties
-- ============================================================================

-- Property: locatedAt should create located value with empty span at position
prop_locatedAt_empty_span :: SourcePos -> Int -> Property
prop_locatedAt_empty_span pos value =
  let located = locatedAt pos value
  in property $ locatedValue located === value .&&. 
             locatedPos located === pos .&&. 
             locSpan located === emptySpan pos

-- Property: locatedWithSpan should create located value with correct span
prop_locatedWithSpan_correct_span :: SourceSpan -> Int -> Property
prop_locatedWithSpan_correct_span span value =
  let located = locatedWithSpan span value
  in property $ locatedValue located === value .&&. 
             locatedPos located === spanStart span .&&. 
             locSpan located === span

-- Property: mapLocated should apply function to value but preserve location
prop_mapLocated_preserves_location :: Located Int -> Property
prop_mapLocated_preserves_location located =
  let f = (*2)
      mapped = mapLocated f located
  in property $ locatedValue mapped === f (locatedValue located) .&&. 
             locatedPos mapped === locatedPos located .&&. 
             locatedSpan mapped === locatedSpan located

-- Property: mapLocated should be functor identity law
prop_mapLocated_identity :: Located Int -> Property
prop_mapLocated_identity located =
  property $ mapLocated id located === located

-- Property: mapLocated should be functor composition law
prop_mapLocated_composition :: Located Int -> Property
prop_mapLocated_composition located =
  let f = (*2)
      g = (+1)
  in property $ mapLocated (f . g) located === mapLocated f (mapLocated g located)

-- ============================================================================
-- HasLocation Class Properties
-- ============================================================================

-- Property: getLocation should return span for Located values
prop_getLocation_returns_span :: Located Int -> Property
prop_getLocation_returns_span located =
  property $ getLocation located === locatedSpan located

-- ============================================================================
-- Error Location Conversion Properties
-- ============================================================================

-- Property: toErrorLocation should convert position correctly
prop_toErrorLocation_correct_conversion :: SourcePos -> Property
prop_toErrorLocation_correct_conversion pos =
  let errLoc = toErrorLocation pos
  in property $ line errLoc === posLine pos .&&. 
             column errLoc === posColumn pos

-- Property: toErrorLocationWithSpan should convert span correctly
prop_toErrorLocationWithSpan_correct_conversion :: SourceSpan -> Property
prop_toErrorLocationWithSpan_correct_conversion span =
  let errLoc = toErrorLocationWithSpan span
      start = spanStart span
      end = spanEnd span
  in property $ line errLoc === posLine start .&&. 
             column errLoc === posColumn start .&&. 
             endLine errLoc === Just (posLine end) .&&. 
             endColumn errLoc === Just (posColumn end)

-- ============================================================================
-- Position Ordering Properties
-- ============================================================================

-- Property: positions with same line L.and column but different offsets should be ordered by offset
prop_position_ordering_by_offset :: Int -> Int -> Property
prop_position_ordering_by_offset line col =
  line > 0 && col > 0 ==> 
  let pos1 = SourcePos line col 100
      pos2 = SourcePos line col 200
  in property $ pos1 < pos2

-- Property: positions with different lines should be ordered by line regardless of column
prop_position_ordering_by_line :: Int -> Int -> Int -> Property
prop_position_ordering_by_line line1 line2 col =
  line1 > 0 && line2 > 0 && col > 0 && line1 /= line2 ==> 
  let pos1 = SourcePos line1 col 0
      pos2 = SourcePos line2 col 0
      expectedOrder = line1 < line2
  in property $ (pos1 < pos2) === expectedOrder

-- ============================================================================
-- Span Consistency Properties
-- ============================================================================

-- Property: span created by spanBetween should be valid if start <= end
prop_spanBetween_valid_if_ordered :: SourcePos -> SourcePos -> Property
prop_spanBetween_valid_if_ordered start end =
  start <= end ==> 
  let span = spanBetween start end
  in property $ isValidSpan span

-- Property: spanFrom position should always be valid
prop_spanFrom_always_valid :: SourcePos -> Property
prop_spanFrom_always_valid pos =
  property $ isValidSpan (spanFrom pos)

-- Property: spanTo position should always be valid
prop_spanTo_always_valid :: SourcePos -> Property
prop_spanTo_always_valid pos =
  property $ isValidSpan (spanTo pos)

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New SourceLocation QuickCheck Tests"
  [ testGroup "SourcePos Properties"
    [ fastProperty "startPos attributes" prop_startPos_attributes
    , fastProperty "posAt creates correct position" prop_posAt_creates_correct_position
    , fastProperty "posAtLineCol creates correct position" prop_posAtLineCol_creates_correct_position
    , fastProperty "posAfter newline behavior" prop_posAfter_newline_behavior
    , fastProperty "posAfter tab behavior" prop_posAfter_tab_behavior
    , fastProperty "posAfter regular character" prop_posAfter_regular_char
    , fastProperty "advancePos equals posAfter" prop_advancePos_equals_posAfter
    , fastProperty "advancePosBy empty string" prop_advancePosBy_empty_string
    , fastProperty "advancePosBy folds posAfter" prop_advancePosBy_folds_posAfter
    , fastProperty "advancePosByText equals advancePosBy" prop_advancePosByText_equals_advancePosBy
    , fastProperty "advancePosByLine advances lines" prop_advancePosByLine_advances_lines
    ]
  , testGroup "SourceSpan Properties"
    [ fastProperty "emptySpan same start end" prop_emptySpan_same_start_end
    , fastProperty "spanFrom equals emptySpan" prop_spanFrom_equals_emptySpan
    , fastProperty "spanTo same start end" prop_spanTo_same_start_end
    , fastProperty "spanBetween correct start end" prop_spanBetween_correct_start_end
    , fastProperty "mergeSpans min max" prop_mergeSpans_min_max
    , fastProperty "mergeSpans commutative" prop_mergeSpans_commutative
    , fastProperty "mergeSpans associative" prop_mergeSpans_associative
    , fastProperty "isValidSpan valid gen" prop_isValidSpan_valid_gen
    ]
  , testGroup "Located Properties"
    [ fastProperty "locatedAt empty span" prop_locatedAt_empty_span
    , fastProperty "locatedWithSpan correct span" prop_locatedWithSpan_correct_span
    , fastProperty "mapLocated preserves location" prop_mapLocated_preserves_location
    , fastProperty "mapLocated identity" prop_mapLocated_identity
    , fastProperty "mapLocated composition" prop_mapLocated_composition
    ]
  , testGroup "HasLocation Properties"
    [ fastProperty "getLocation returns span" prop_getLocation_returns_span
    ]
  , testGroup "Error Location Conversion Properties"
    [ fastProperty "toErrorLocation correct conversion" prop_toErrorLocation_correct_conversion
    , fastProperty "toErrorLocationWithSpan correct conversion" prop_toErrorLocationWithSpan_correct_conversion
    ]
  , testGroup "Position Ordering Properties"
    [ fastProperty "position ordering by offset" prop_position_ordering_by_offset
    , fastProperty "position ordering by line" prop_position_ordering_by_line
    ]
  , testGroup "Span Consistency Properties"
    [ fastProperty "spanBetween valid if ordered" prop_spanBetween_valid_if_ordered
    , fastProperty "spanFrom always valid" prop_spanFrom_always_valid
    , fastProperty "spanTo always valid" prop_spanTo_always_valid
    ]
  ]