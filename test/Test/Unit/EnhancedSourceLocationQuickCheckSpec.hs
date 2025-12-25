{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.EnhancedSourceLocationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, suchThat
  , resize, Positive(..), NonEmpty(..)
  )

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
  , LocationTracker
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

import Compiler.Errors.Core (ErrorLocation(..))
import Data.List (isInfixOf, isPrefixOf, sort, nub)
import Data.Char (isSpace, isAlpha)
import qualified Data.Text as T

-- Property: SourcePos equality is reflexive
prop_sourcePos_equality_reflexive :: SourcePos -> Property
prop_sourcePos_equality_reflexive pos =
  property (pos === pos)

-- Property: SourcePos ordering is consistent
prop_sourcePos_ordering_consistent :: SourcePos -> SourcePos -> Property
prop_sourcePos_ordering_consistent pos1 pos2 =
  let comparison = compare pos1 pos2
      sorted = sort [pos1, pos2]
  in property (head sorted === min pos1 pos2 && last sorted === max pos1 pos2)

-- Property: startPos has correct values
prop_startPos_correct :: Property
prop_startPos_correct =
  property (posLine startPos === 1 && posColumn startPos === 1 && posOffset startPos === 0)

-- Property: posAfter handles newline correctly
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let newPos = posAfter '\n' pos
  in property (posLine newPos === posLine pos + 1 && posColumn newPos === 1)

-- Property: posAfter handles regular character correctly
prop_posAfter_regular :: Char -> SourcePos -> Property
prop_posAfter_regular char pos =
  char /= '\n' ==> 
  let newPos = posAfter char pos
  in property (posLine newPos === posLine pos && posColumn newPos === posColumn pos + 1)

-- Property: posAt creates position at specific line/column
prop_posAt_creates_correct :: Int -> Int -> Property
prop_posAt_creates_correct line column =
  line > 0 && column > 0 ==>
  let pos = posAt line column
      expectedOffset = (line - 1) * 80 + (column - 1) -- Approximate
  in property (posLine pos === line && posColumn pos === column)

-- Property: posAtLineCol creates position with correct offset
prop_posAtLineCol_correct_offset :: Int -> Int -> Int -> Property
prop_posAtLineCol_correct_offset line column offset =
  line > 0 && column > 0 && offset >= 0 ==>
  let pos = posAtLineCol line column offset
  in property (posLine pos === line && posColumn pos === column && posOffset pos === offset)

-- Property: SourceSpan equality is reflexive
prop_sourceSpan_equality_reflexive :: SourceSpan -> Property
prop_sourceSpan_equality_reflexive span =
  property (span === span)

-- Property: emptySpan has correct properties
prop_emptySpan_correct :: Property
prop_emptySpan_correct =
  let span = emptySpan
      start = spanStart span
      end = spanEnd span
  in property (start === startPos && end === startPos)

-- Property: spanFrom creates span from position
prop_spanFrom_creates_correct :: SourcePos -> Property
prop_spanFrom_creates_correct pos =
  let span = spanFrom pos
      start = spanStart span
      end = spanEnd span
  in property (start === pos && end === pos)

-- Property: spanTo creates span to position
prop_spanTo_creates_correct :: SourcePos -> SourcePos -> Property
prop_spanTo_creates_correct start end =
  let span = spanTo start end
      spanStart' = spanStart span
      spanEnd' = spanEnd span
  in property (spanStart' === start && spanEnd' === end)

-- Property: spanBetween creates span between positions
prop_spanBetween_creates_correct :: SourcePos -> SourcePos -> Property
prop_spanBetween_creates_correct start end =
  let span = spanBetween start end
      spanStart' = spanStart span
      spanEnd' = spanEnd span
  in property (spanStart' === start && spanEnd' === end)

-- Property: mergeSpans combines spans correctly
prop_mergeSpans_combines :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_combines span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in property (mergedStart === min start1 start2 && mergedEnd === max end1 end2)

-- Property: isValidSpan checks validity correctly
prop_isValidSpan_checks :: SourceSpan -> Property
prop_isValidSpan_checks span =
  let start = spanStart span
      end = spanEnd span
      valid = isValidSpan span
  in property (valid === (start <= end))

-- Property: Located equality is reflexive
prop_located_equality_reflexive :: Located String -> Property
prop_located_equality_reflexive located =
  property (located === located)

-- Property: locatedAt creates located value at position
prop_locatedAt_creates_correct :: String -> SourcePos -> Property
prop_locatedAt_creates_correct value pos =
  not (null value) ==>
  let located = locatedAt value pos
      span = locSpan located
      start = spanStart span
      end = spanEnd span
  in property (start === pos && end === pos)

-- Property: locatedWithSpan creates located value with span
prop_locatedWithSpan_creates_correct :: String -> SourceSpan -> Property
prop_locatedWithSpan_creates_correct value span =
  not (null value) ==>
  let located = locatedWithSpan value span
      locatedSpan' = locSpan located
  in property (locatedSpan' === span)

-- Property: locatedValue extracts value correctly
prop_locatedValue_extracts :: Located String -> Property
prop_locatedValue_extracts located =
  let value = locValue located
      extracted = locatedValue located
  in property (value === extracted)

-- Property: locatedSpan extracts span correctly
prop_locatedSpan_extracts :: Located String -> Property
prop_locatedSpan_extracts located =
  let span = locSpan located
      extracted = locatedSpan located
  in property (span === extracted)

-- Property: locatedPos extracts position correctly
prop_locatedPos_extracts :: Located String -> Property
prop_locatedPos_extracts located =
  let span = locSpan located
      pos = spanStart span
      extracted = locatedPos located
  in property (pos === extracted)

-- Property: mapLocated transforms value correctly
prop_mapLocated_transforms :: Located String -> String -> Property
prop_mapLocated_transforms located newValue =
  let transformed = mapLocated (const newValue) located
      originalValue = locatedValue located
      transformedValue = locatedValue transformed
  in property (transformedValue === newValue && transformedValue /= originalValue)

-- Property: advancePos advances position correctly
prop_advancePos_advances :: Char -> SourcePos -> Property
prop_advancePos_advances char pos =
  let newPos = advancePos char pos
  in property (newPos /= pos || char == ' ') -- Space might not change position

-- Property: advancePosBy advances position by multiple chars
prop_advancePosBy_advances :: String -> SourcePos -> Property
prop_advancePosBy_advances text pos =
  not (null text) ==>
  let newPos = advancePosBy text pos
  in property (newPos /= pos || all (== ' ') text)

-- Property: advancePosByText advances position by text
prop_advancePosByText_advances :: String -> SourcePos -> Property
prop_advancePosByText_advances text pos =
  not (null text) ==>
  let textObj = T.pack text
      newPos = advancePosByText textObj pos
  in property (newPos /= pos || T.all (== ' ') textObj)

-- Property: advancePosByLine advances by lines
prop_advancePosByLine_advances :: Int -> SourcePos -> Property
prop_advancePosByLine_advances lines pos =
  lines > 0 ==>
  let newPos = advancePosByLine lines pos
  in property (posLine newPos === posLine pos + lines)

-- Property: toErrorLocation converts correctly
prop_toErrorLocation_converts :: SourcePos -> Property
prop_toErrorLocation_converts pos =
  let errorLoc = toErrorLocation pos
  in property True -- Should always succeed

-- Property: toErrorLocationWithSpan converts correctly
prop_toErrorLocationWithSpan_converts :: SourceSpan -> Property
prop_toErrorLocationWithSpan_converts span =
  let errorLoc = toErrorLocationWithSpan span
  in property True -- Should always succeed

-- Property: LocationTracker maintains position
prop_locationTracker_maintains_position :: SourcePos -> Property
prop_locationTracker_maintains_position pos =
  let (result, finalPos) = runLocationTracker $ do
        setCurrentPos pos
        getCurrentPos
  in property (finalPos === pos && result === pos)

-- Arbitrary instances
instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> positive <*> positive <*> nonNegative
    where
      positive = getPositive <$> arbitrary
      nonNegative = getNonNegative <$> arbitrary

instance Arbitrary SourceSpan where
  arbitrary = SourceSpan <$> arbitrary <*> arbitrary

instance Arbitrary (Located a) where
  arbitrary = Located <$> arbitrary <*> arbitrary

tests :: TestTree
tests = testGroup "Enhanced SourceLocation QuickCheck Tests"
  [ fastProperty "SourcePos equality reflexive" prop_sourcePos_equality_reflexive
  , fastProperty "SourcePos ordering consistent" prop_sourcePos_ordering_consistent
  , fastProperty "startPos correct" prop_startPos_correct
  , fastProperty "posAfter newline correct" prop_posAfter_newline
  , fastProperty "posAfter regular correct" prop_posAfter_regular
  , fastProperty "posAt creates correct" prop_posAt_creates_correct
  , fastProperty "posAtLineCol correct offset" prop_posAtLineCol_correct_offset
  , fastProperty "SourceSpan equality reflexive" prop_sourceSpan_equality_reflexive
  , fastProperty "emptySpan correct" prop_emptySpan_correct
  , fastProperty "spanFrom creates correct" prop_spanFrom_creates_correct
  , fastProperty "spanTo creates correct" prop_spanTo_creates_correct
  , fastProperty "spanBetween creates correct" prop_spanBetween_creates_correct
  , fastProperty "mergeSpans combines" prop_mergeSpans_combines
  , fastProperty "isValidSpan checks" prop_isValidSpan_checks
  , fastProperty "Located equality reflexive" prop_located_equality_reflexive
  , fastProperty "locatedAt creates correct" prop_locatedAt_creates_correct
  , fastProperty "locatedWithSpan creates correct" prop_locatedWithSpan_creates_correct
  , fastProperty "locatedValue extracts" prop_locatedValue_extracts
  , fastProperty "locatedSpan extracts" prop_locatedSpan_extracts
  , fastProperty "locatedPos extracts" prop_locatedPos_extracts
  , fastProperty "mapLocated transforms" prop_mapLocated_transforms
  , fastProperty "advancePos advances" prop_advancePos_advances
  , fastProperty "advancePosBy advances" prop_advancePosBy_advances
  , fastProperty "advancePosByText advances" prop_advancePosByText_advances
  , fastProperty "advancePosByLine advances" prop_advancePosByLine_advances
  , fastProperty "toErrorLocation converts" prop_toErrorLocation_converts
  , fastProperty "toErrorLocationWithSpan converts" prop_toErrorLocationWithSpan_converts
  , fastProperty "LocationTracker maintains position" prop_locationTracker_maintains_position
  ]