{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(..), NonNegative(..))
import Test.QuickCheck.Gen (choose, listOf, elements, vectorOf)

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
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace, isLower, isUpper)

-- Property: startPos has correct initial values
prop_startPos_correct :: Property
prop_startPos_correct =
  property $ posLine startPos === 1 .&&.
             posColumn startPos === 1 .&&.
             posOffset startPos === 0

-- Property: posAfter correctly handles newline
prop_posAfter_newline :: Positive Int -> Property
prop_posAfter_newline (Positive line) =
  let pos = SourcePos line 5 10
      newPos = posAfter '\n' pos
  in property $ posLine newPos === line + 1 .&&.
             posColumn newPos === 1 .&&.
             posOffset newPos === 11

-- Property: posAfter correctly handles tab
prop_posAfter_tab :: Positive Int -> Property
prop_posAfter_tab (Positive col) =
  let pos = SourcePos 1 col 10
      newPos = posAfter '\t' pos
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine newPos === 1 .&&.
             posColumn newPos === expectedCol .&&.
             posOffset newPos === 11

-- Property: posAfter correctly handles regular character
prop_posAfter_regular :: Positive Int -> Property
prop_posAfter_regular (Positive col) =
  let pos = SourcePos 1 col 10
      newPos = posAfter 'a' pos
  in property $ posLine newPos === 1 .&&.
             posColumn newPos === col + 1 .&&.
             posOffset newPos === 11

-- Property: posAt creates correct position
prop_posAt_correct :: Positive Int -> Positive Int -> Property
prop_posAt_correct (Positive line) (Positive col) =
  let pos = posAt line col
  in property $ posLine pos === line .&&.
             posColumn pos === col .&&.
             posOffset pos === 0

-- Property: posAtLineCol creates correct position
prop_posAtLineCol_correct :: Positive Int -> Positive Int -> Positive Int -> Property
prop_posAtLineCol_correct (Positive line) (Positive col) (Positive offset) =
  let pos = posAtLineCol line col offset
  in property $ posLine pos === line .&&.
             posColumn pos === col .&&.
             posOffset pos === offset

-- Property: emptySpan has valid structure
prop_emptySpan_valid :: Property
prop_emptySpan_valid =
  let span = emptySpan startPos
  in property $ isValidSpan span === False -- Empty span is not considered valid

-- Property: spanFrom creates span from position
prop_spanFrom_correct :: Positive Int -> Positive Int -> Property
prop_spanFrom_correct (Positive line) (Positive col) =
  let pos = posAt line col
      span = spanFrom pos
  in property $ spanStart span === pos .&&.
             spanEnd span === pos

-- Property: spanTo creates span to position
prop_spanTo_correct :: Positive Int -> Positive Int -> Property
prop_spanTo_correct (Positive line) (Positive col) =
  let pos = posAt line col
      span = spanTo pos
  in property $ spanStart span === startPos .&&.
             spanEnd span === pos

-- Property: spanBetween creates correct span
prop_spanBetween_correct :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_spanBetween_correct (Positive line1) (Positive col1) (Positive line2) (Positive col2) =
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      span = spanBetween pos1 pos2
  in property $ spanStart span === pos1 .&&.
             spanEnd span === pos2

-- Property: mergeSpans correctly combines spans
prop_mergeSpans_correct :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> 
                           Positive Int -> Positive Int -> Property
prop_mergeSpans_correct (Positive line1) (Positive col1) (Positive line2) (Positive col2) 
                        (Positive line3) (Positive col3) =
  let pos1 = posAt line1 col1
      pos2 = posAt (line1 + line2) (col1 + col2)
      pos3 = posAt (line1 + line2 + line3) (col1 + col2 + col3)
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      merged = mergeSpans span1 span2
  in property $ spanStart merged === pos1 .&&.
             spanEnd merged === pos3

-- Property: locatedAt creates located value correctly
prop_locatedAt_correct :: Int -> String -> Property
prop_locatedAt_correct line value =
  let pos = posAt line 1
      located = locatedAt pos value
  in property $ locatedValue located === value .&&.
             locatedPos located === pos

-- Property: locatedWithSpan creates located value with span
prop_locatedWithSpan_correct :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> String -> Property
prop_locatedWithSpan_correct (Positive line1) (Positive col1) (Positive line2) (Positive col2) value =
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      span = spanBetween pos1 pos2
      located = locatedWithSpan span value
  in property $ locatedValue located === value .&&.
             locatedSpan located === span

-- Property: mapLocated applies function correctly
prop_mapLocated_correct :: Int -> String -> Property
prop_mapLocated_correct line value =
  let pos = posAt line 1
      located = locatedAt pos value
      mapped = mapLocated length located
  in property $ locatedValue mapped === length value .&&.
             locatedPos mapped === pos

-- Property: advancePos advances position correctly
prop_advancePos_single :: Positive Int -> Char -> Property
prop_advancePos_single (Positive col) char =
  let pos = posAt 1 col
      advanced = advancePos char pos
  in property $ posOffset advanced === posOffset pos + 1

-- Property: advancePosBy advances by multiple characters
prop_advancePosBy_multiple :: Positive Int -> String -> Property
prop_advancePosBy_multiple (Positive col) chars =
  not (null chars) ==>
  let pos = posAt 1 col
      advanced = advancePosBy chars pos
  in property $ posOffset advanced === posOffset pos + length chars

-- Property: advancePosByText handles text correctly
prop_advancePosByText_text :: Positive Int -> String -> Property
prop_advancePosByText_text (Positive col) text =
  not (null text) ==>
  let pos = posAt 1 col
      advanced = advancePosByText (T.pack text) pos
  in property $ posOffset advanced === posOffset pos + length text

-- Property: advancePosByLine advances by lines
prop_advancePosByLine_single :: Positive Int -> Property
prop_advancePosByLine_single (Positive line) =
  let pos = posAt line 1
      advanced = advancePosByLine 1 pos
  in property $ posLine advanced === line + 1 .&&.
             posColumn advanced === 1 .&&.
             posOffset advanced === posOffset pos + 1

-- Property: SourcePos ordering is consistent
prop_sourcepos_ordering :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcepos_ordering (Positive line1) (Positive col1) (Positive line2) (Positive col2) =
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      lineComparison = line1 `compare` line2
      colComparison = col1 `compare` col2
      expectedOrder = if line1 /= line2 then lineComparison else colComparison
  in property $ pos1 `compare` pos2 === expectedOrder

-- Property: isValidSpan correctly identifies valid spans
prop_isValidSpan_detection :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_isValidSpan_detection (Positive line1) (Positive col1) (Positive line2) (Positive col2) =
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      span = spanBetween pos1 pos2
      shouldBeValid = pos1 <= pos2
  in property $ isValidSpan span === shouldBeValid

-- Property: Located functor laws
prop_located_functor_identity :: Int -> String -> Property
prop_located_functor_identity line value =
  let pos = posAt line 1
      located = locatedAt pos value
      mapped = mapLocated id located
  in property $ locatedValue mapped === locatedValue located .&&.
             locatedPos mapped === locatedPos located

-- Property: Located functor composition
prop_located_functor_composition :: Int -> String -> Property
prop_located_functor_composition line value =
  let pos = posAt line 1
      located = locatedAt pos value
      f = length
      g = reverse
      mapped1 = mapLocated (f . g) located
      mapped2 = mapLocated f (mapLocated g located)
  in property $ locatedValue mapped1 === locatedValue mapped2 .&&.
             locatedPos mapped1 === locatedPos mapped2

-- Property: span merging is associative
prop_mergeSpans_associative :: Positive Int -> Positive Int -> Positive Int -> Positive Int ->
                              Positive Int -> Positive Int -> Property
prop_mergeSpans_associative (Positive line1) (Positive col1) (Positive line2) (Positive col2)
                           (Positive line3) (Positive col3) =
  let pos1 = posAt line1 col1
      pos2 = posAt (line1 + line2) (col1 + col2)
      pos3 = posAt (line1 + line2 + line3) (col1 + col2 + col3)
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      span3 = spanBetween pos1 pos3
      merged1 = mergeSpans span1 (mergeSpans span2 span3)
      merged2 = mergeSpans (mergeSpans span1 span2) span3
  in property $ spanStart merged1 === spanStart merged2 .&&.
             spanEnd merged1 === spanEnd merged2

-- Property: position advancement preserves order
prop_advancePos_preserves_order :: Positive Int -> Positive Int -> String -> Property
prop_advancePos_preserves_order (Positive line1) (Positive col1) text =
  not (null text) ==>
  let pos1 = posAt line1 col1
      pos2 = advancePosByText (T.pack text) pos1
  in property $ pos1 <= pos2

-- Property: locatedSpan provides consistent positions
prop_locatedSpan_consistency :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> String -> Property
prop_locatedSpan_consistency (Positive line1) (Positive col1) (Positive line2) (Positive col2) value =
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      span = spanBetween pos1 pos2
      located = locatedWithSpan span value
      retrievedSpan = locatedSpan located
  in property $ retrievedSpan === span

tests :: TestTree
tests =
  testGroup "SourceLocation Core QuickCheck Tests"
    [ fastProperty "startPos has correct initial values" prop_startPos_correct
    , fastProperty "posAfter correctly handles newline" prop_posAfter_newline
    , fastProperty "posAfter correctly handles tab" prop_posAfter_tab
    , fastProperty "posAfter correctly handles regular character" prop_posAfter_regular
    , fastProperty "posAt creates correct position" prop_posAt_correct
    , fastProperty "posAtLineCol creates correct position" prop_posAtLineCol_correct
    , fastProperty "emptySpan has valid structure" prop_emptySpan_valid
    , fastProperty "spanFrom creates span from position" prop_spanFrom_correct
    , fastProperty "spanTo creates span to position" prop_spanTo_correct
    , fastProperty "spanBetween creates correct span" prop_spanBetween_correct
    , fastProperty "mergeSpans correctly combines spans" prop_mergeSpans_correct
    , fastProperty "locatedAt creates located value correctly" prop_locatedAt_correct
    , fastProperty "locatedWithSpan creates located value with span" prop_locatedWithSpan_correct
    , fastProperty "mapLocated applies function correctly" prop_mapLocated_correct
    , fastProperty "advancePos advances position correctly" prop_advancePos_single
    , fastProperty "advancePosBy advances by multiple characters" prop_advancePosBy_multiple
    , fastProperty "advancePosByText handles text correctly" prop_advancePosByText_text
    , fastProperty "advancePosByLine advances by lines" prop_advancePosByLine_single
    , fastProperty "SourcePos ordering is consistent" prop_sourcepos_ordering
    , fastProperty "isValidSpan correctly identifies valid spans" prop_isValidSpan_detection
    , fastProperty "Located functor identity law" prop_located_functor_identity
    , fastProperty "Located functor composition law" prop_located_functor_composition
    , fastProperty "span merging is associative" prop_mergeSpans_associative
    , fastProperty "position advancement preserves order" prop_advancePos_preserves_order
    , fastProperty "locatedSpan provides consistent positions" prop_locatedSpan_consistency
    ]