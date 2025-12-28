{-# LANGUAGE CPP #-}

module Test.Unit.SourceLocationAdvancedPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Text as T
import Data.List (sort)

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , posAtLineCol
  , advancePos
  , advancePosBy
  , advancePosByText
  , advancePosByLine
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
  , toErrorLocation
  , toErrorLocationWithSpan
  )

-- Newtype wrappers for better QuickCheck generation
newtype PositiveInt = PositiveInt Int
  deriving Show

instance Arbitrary PositiveInt where
  arbitrary = PositiveInt <$> getPositive <$> arbitrary

newtype NonEmptyString = NonEmptyString String
  deriving Show

instance Arbitrary NonEmptyString where
  arbitrary = NonEmptyString <$> listOf1 arbitrary `suchThat` (not . null)

tests :: TestTree
tests = testGroup "SourceLocation Advanced Properties"
  [ positionProperties
  , spanProperties  
  , locatedProperties
  , errorLocationProperties
  , textAdvancementProperties
  ]

positionProperties :: TestTree
positionProperties = testGroup "Position Properties"
  [ fastProperty "posAfter newline increases line by 1 and resets column to 1" prop_posAfter_newline
  , fastProperty "posAfter tab jumps to next tab stop (multiple of 8)" prop_posAfter_tab
  , fastProperty "posAfter regular character increments column by 1" prop_posAfter_regular
  , fastProperty "advancePosBy empty string returns original position" prop_advancePosBy_empty
  , fastProperty "advancePosBy is equivalent to repeated posAfter" prop_advancePosBy_consistent
  , fastProperty "advancePosByLine preserves offset but changes line and column" prop_advancePosByLine_properties
  , fastProperty "posAtLineCol creates valid position" prop_posAtLineCol_valid
  ]

spanProperties :: TestTree
spanProperties = testGroup "Span Properties"
  [ fastProperty "emptySpan has equal start and end" prop_emptySpan_equal
  , fastProperty "spanFrom creates valid span" prop_spanFrom_valid
  , fastProperty "spanTo creates valid span" prop_spanTo_valid
  , fastProperty "spanBetween creates span with correct bounds" prop_spanBetween_bounds
  , fastProperty "mergeSpans is commutative" prop_mergeSpans_commutative
  , fastProperty "mergeSpans is associative" prop_mergeSpans_associative
  , fastProperty "mergeSpans contains both original spans" prop_mergeSpans_contains
  , fastProperty "isValidSpan is true for properly constructed spans" prop_isValidSpan_constructed
  ]

locatedProperties :: TestTree
locatedProperties = testGroup "Located Properties"
  [ fastProperty "locatedAt creates span with equal start and end" prop_locatedAt_span
  , fastProperty "locatedWithSpan preserves span" prop_locatedWithSpan_preserves
  , fastProperty "mapLocated preserves position" prop_mapLocated_preserves_position
  , fastProperty "locatedPos returns span start" prop_locatedPos_span_start
  , fastProperty "locatedValue returns original value" prop_locatedValue_identity
  ]

errorLocationProperties :: TestTree
errorLocationProperties = testGroup "Error Location Properties"
  [ fastProperty "toErrorLocation preserves line and column" prop_toErrorLocation_preserves
  , fastProperty "toErrorLocationWithSpan preserves full range" prop_toErrorLocationWithSpan_preserves
  , fastProperty "toErrorLocation has no end positions" prop_toErrorLocation_no_end
  ]

textAdvancementProperties :: TestTree
textAdvancementProperties = testGroup "Text Advancement Properties"
  [ fastProperty "advancePosByText empty text returns original position" prop_advancePosByText_empty
  , fastProperty "advancePosByText is consistent with advancePosBy" prop_advancePosByText_consistent
  , fastProperty "advancePosByText multiline tracks correctly" prop_advancePosByText_multiline
  ]

-- Position property implementations
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let newPos = posAfter '\n' pos
  in conjoin
    [ posLine newPos === posLine pos + 1
    , posColumn newPos === 1
    , posOffset newPos === posOffset pos + 1
    ]

prop_posAfter_tab :: SourcePos -> Property
prop_posAfter_tab pos =
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in conjoin
    [ posLine newPos === posLine pos
    , posColumn newPos === expectedCol
    , posOffset newPos === posOffset pos + 1
    ]

prop_posAfter_regular :: SourcePos -> Char -> Property
prop_posAfter_regular pos c =
  c `notElem` "\n\t" ==>
  let newPos = posAfter c pos
  in conjoin
    [ posLine newPos === posLine pos
    , posColumn newPos === posColumn pos + 1
    , posOffset newPos === posOffset pos + 1
    ]

prop_advancePosBy_empty :: SourcePos -> Property
prop_advancePosBy_empty pos =
  advancePosBy "" pos === pos

prop_advancePosBy_consistent :: SourcePos -> NonEmptyString -> Property
prop_advancePosBy_consistent pos (NonEmptyString s) =
  let manual = foldl (flip posAfter) pos s
      auto = advancePosBy s pos
  in manual === auto

prop_advancePosByLine_properties :: PositiveInt -> SourcePos -> Property
prop_advancePosByLine_properties (PositiveInt n) pos =
  let newPos = advancePosByLine n pos
  in conjoin
    [ posLine newPos === posLine pos + n
    , posColumn newPos === 1
    , posOffset newPos === posOffset pos  -- offset preserved
    ]

prop_posAtLineCol_valid :: PositiveInt -> PositiveInt -> Property
prop_posAtLineCol_valid (PositiveInt line) (PositiveInt col) =
  let pos = posAtLineCol line col 0
  in conjoin
    [ posLine pos === line
    , posColumn pos === col
    , posOffset pos === 0
    ]

-- Span property implementations
prop_emptySpan_equal :: SourcePos -> Property
prop_emptySpan_equal pos =
  let span = emptySpan pos
  in spanStart span === spanEnd span

prop_spanFrom_valid :: SourcePos -> Property
prop_spanFrom_valid pos =
  let span = spanFrom pos
  in isValidSpan span && (spanStart span === spanEnd span)

prop_spanTo_valid :: SourcePos -> Property
prop_spanTo_valid pos =
  let span = spanTo pos
  in isValidSpan span && (spanStart span === spanEnd span)

prop_spanBetween_bounds :: SourcePos -> SourcePos -> Property
prop_spanBetween_bounds start end =
  let span = spanBetween start end
  in conjoin
    [ spanStart span === start
    , spanEnd span === end
    , if start <= end then isValidSpan span else property True
    ]

prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
  mergeSpans span1 span2 === mergeSpans span2 span1

prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  mergeSpans (mergeSpans span1 span2) span3 === mergeSpans span1 (mergeSpans span2 span3)

prop_mergeSpans_contains :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_contains span1 span2 =
  let merged = mergeSpans span1 span2
  in conjoin
    [ spanStart merged <= spanStart span1
    , spanEnd merged >= spanEnd span1
    , spanStart merged <= spanStart span2
    , spanEnd merged >= spanEnd span2
    ]

prop_isValidSpan_constructed :: SourcePos -> SourcePos -> Property
prop_isValidSpan_constructed start end =
  let span = spanBetween start end
  in isValidSpan span === (start <= end)

-- Located property implementations
prop_locatedAt_span :: SourcePos -> String -> Property
prop_locatedAt_span pos value =
  let loc = locatedAt pos value
      span = locatedSpan loc
  in spanStart span === spanEnd span

prop_locatedWithSpan_preserves :: SourceSpan -> String -> Property
prop_locatedWithSpan_preserves span value =
  let loc = locatedWithSpan span value
  in conjoin
    [ locatedSpan loc === span
    , locatedValue loc === value
    ]

prop_mapLocated_preserves_position :: SourceSpan -> [Int] -> Property
prop_mapLocated_preserves_position span values =
  let loc = locatedWithSpan span values
      mapped = mapLocated length loc
  in conjoin
    [ locatedSpan mapped === locatedSpan loc
    , locatedPos mapped === locatedPos loc
    ]

prop_locatedPos_span_start :: SourceSpan -> String -> Property
prop_locatedPos_span_start span value =
  let loc = locatedWithSpan span value
  in locatedPos loc === spanStart span

prop_locatedValue_identity :: SourceSpan -> String -> Property
prop_locatedValue_identity span value =
  let loc = locatedWithSpan span value
  in locatedValue loc === value

-- Error location property implementations
prop_toErrorLocation_preserves :: SourcePos -> Property
prop_toErrorLocation_preserves pos =
  let errLoc = toErrorLocation pos
  in conjoin
    [ line errLoc === posLine pos
    , column errLoc === posColumn pos
    ]

prop_toErrorLocationWithSpan_preserves :: SourceSpan -> Property
prop_toErrorLocationWithSpan_preserves span =
  let errLoc = toErrorLocationWithSpan span
  in conjoin
    [ line errLoc === posLine (spanStart span)
    , column errLoc === posColumn (spanStart span)
    , endLine errLoc === Just (posLine (spanEnd span))
    , endColumn errLoc === Just (posColumn (spanEnd span))
    ]

prop_toErrorLocation_no_end :: SourcePos -> Property
prop_toErrorLocation_no_end pos =
  let errLoc = toErrorLocation pos
  in conjoin
    [ endLine errLoc === Nothing
    , endColumn errLoc === Nothing
    ]

-- Text advancement property implementations
prop_advancePosByText_empty :: SourcePos -> Property
prop_advancePosByText_empty pos =
  advancePosByText T.empty pos === pos

prop_advancePosByText_consistent :: SourcePos -> NonEmptyString -> Property
prop_advancePosByText_consistent pos (NonEmptyString s) =
  let textAdv = advancePosByText (T.pack s) pos
      stringAdv = advancePosBy s pos
  in textAdv === stringAdv

prop_advancePosByText_multiline :: SourcePos -> NonEmptyString -> Property
prop_advancePosByText_multiline pos (NonEmptyString s) =
  let linesWithNewlines = length $ filter (== '\n') s
      finalPos = advancePosByText (T.pack s) pos
      expectedLine = posLine pos + linesWithNewlines
  in posLine finalPos >= expectedLine .&&. posLine finalPos <= expectedLine + 1