{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.AdditionalSourceLocationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf1, elements)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

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
  , spanStart
  , spanEnd
  )

import Data.Char (isSpace)
import qualified Data.Text as T

-- Arbitrary instances for SourceLocation types
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    offset <- choose (0, 100000)
    return $ SourcePos line col offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endLine <- choose (sourcePosLine start, sourcePosLine start + 100)
    endCol <- if endLine == sourcePosLine start 
                then choose (sourcePosCol start, sourcePosCol start + 100)
                else choose (1, 1000)
    endOffset <- choose (sourcePosOffset start, sourcePosOffset start + 10000)
    let end = SourcePos endLine endCol endOffset
    return $ SourceSpan start end

-- Property: posAfter newline increments line and resets column to 1
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let newPos = posAfter '\n' pos
  in property $ sourcePosLine newPos === sourcePosLine pos + 1 .&&. 
               sourcePosCol newPos === 1 .&&.
               sourcePosOffset newPos === sourcePosOffset pos + 1

-- Property: posAfter tab advances to next tab stop (column + (8 - (column-1) mod 8))
prop_posAfter_tab :: SourcePos -> Property
prop_posAfter_tab pos =
  let newPos = posAfter '\t' pos
      expectedCol = sourcePosCol pos + (8 - (sourcePosCol pos - 1) `mod` 8)
  in property $ sourcePosLine newPos === sourcePosLine pos .&&. 
               sourcePosCol newPos === expectedCol .&&.
               sourcePosOffset newPos === sourcePosOffset pos + 1

-- Property: posAfter regular character increments column and offset
prop_posAfter_regular_char :: SourcePos -> Char -> Property
prop_posAfter_regular_char pos char =
  not (char `elem` "\n\t") ==>
  let newPos = posAfter char pos
  in property $ sourcePosLine newPos === sourcePosLine pos .&&. 
               sourcePosCol newPos === sourcePosCol pos + 1 .&&.
               sourcePosOffset newPos === sourcePosOffset pos + 1

-- Property: advancePosBy with zero offset returns same position
prop_advancePosBy_zero :: SourcePos -> Property
prop_advancePosBy_zero pos =
  let result = advancePosBy 0 pos
  in property $ result === pos

-- Property: advancePosBy with positive offset increases offset
prop_advancePosBy_positive :: SourcePos -> Int -> Property
prop_advancePosBy_positive pos offset =
  offset > 0 ==>
  let result = advancePosBy offset pos
  in property $ sourcePosOffset result === sourcePosOffset pos + offset

-- Property: emptySpan creates span where start equals end
prop_emptySpan_consistency :: SourcePos -> Property
prop_emptySpan_consistency pos =
  let span = emptySpan pos
  in property $ spanStart span === pos .&&. spanEnd span === pos

-- Property: spanFrom creates empty span at position
prop_spanFrom_equals_emptySpan :: SourcePos -> Property
prop_spanFrom_equals_emptySpan pos =
  let span1 = spanFrom pos
      span2 = emptySpan pos
  in property $ span1 === span2

-- Property: spanTo creates zero-length span ending at position
prop_spanTo_structure :: SourcePos -> Property
prop_spanTo_structure pos =
  let span = spanTo pos
  in property $ spanStart span === pos .&&. spanEnd span === pos

-- Property: spanBetween preserves start and end
prop_spanBetween_preserves_bounds :: SourcePos -> SourcePos -> Property
prop_spanBetween_preserves_bounds start end =
  let span = spanBetween start end
  in property $ spanStart span === start .&&. spanEnd span === end

-- Property: isValidSpan is true when start <= end, false otherwise
prop_isValidSpan_logic :: SourcePos -> SourcePos -> Property
prop_isValidSpan_logic start end =
  let span = spanBetween start end
      lineValid = sourcePosLine start <= sourcePosLine end
      colValid = sourcePosLine start < sourcePosLine end || 
                 sourcePosCol start <= sourcePosCol end
      expected = lineValid && colValid
  in property $ isValidSpan span === expected

-- Property: mergeSpans chooses earliest start and latest end
prop_mergeSpans_bounds :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_bounds span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      start2 = spanStart span2
      end1 = spanEnd span1
      end2 = spanEnd span2
      earliestStart = if sourcePosLine start1 < sourcePosLine start2 ||
                         (sourcePosLine start1 == sourcePosLine start2 && 
                          sourcePosCol start1 <= sourcePosCol start2)
                        then start1 else start2
      latestEnd = if sourcePosLine end1 > sourcePosLine end2 ||
                     (sourcePosLine end1 == sourcePosLine end2 && 
                      sourcePosCol end1 >= sourcePosCol end2)
                    then end1 else end2
  in property $ spanStart merged === earliestStart .&&. spanEnd merged === latestEnd

-- Property: mergeSpans is commutative
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ merged1 === merged2

-- Property: mergeSpans is associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  let merged1 = mergeSpans (mergeSpans span1 span2) span3
      merged2 = mergeSpans span1 (mergeSpans span2 span3)
  in property $ merged1 === merged2

-- Property: locatedAt creates located value with correct position
prop_locatedAt_structure :: SourcePos -> String -> Property
prop_locatedAt_structure pos value =
  let located = locatedAt pos value
  in property $ locatedPos located === pos .&&. locatedValue located === value

-- Property: locatedWithSpan creates located value with correct span
prop_locatedWithSpan_structure :: SourceSpan -> Int -> Property
prop_locatedWithSpan_structure span value =
  let located = locatedWithSpan span value
  in property $ locatedSpan located === span .&&. locatedValue located === value

-- Property: mapLocated preserves location
prop_mapLocated_preserves_location :: SourcePos -> String -> Property
prop_mapLocated_preserves_location pos value =
  let original = locatedAt pos value
      transformed = mapLocated length original
  in property $ locatedPos transformed === locatedPos original .&&.
               locatedValue transformed === length value

-- Property: posAt creates position with given values
prop_posAt_creates_correct_position :: Int -> Int -> Int -> Property
prop_posAt_creates_correct_position line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = posAt line col offset
  in property $ sourcePosLine pos === line .&&. 
               sourcePosCol pos === col .&&.
               sourcePosOffset pos === offset

tests :: TestTree
tests =
  testGroup "Additional SourceLocation QuickCheck tests"
    [ testGroup "Position properties"
        [ fastProperty "posAfter newline increments line and resets column" prop_posAfter_newline
        , fastProperty "posAfter tab advances to next tab stop" prop_posAfter_tab
        , fastProperty "posAfter regular character increments column and offset" prop_posAfter_regular_char
        , fastProperty "advancePosBy zero returns same position" prop_advancePosBy_zero
        , fastProperty "advancePosBy positive increases offset" prop_advancePosBy_positive
        , fastProperty "posAt creates correct position" prop_posAt_creates_correct_position
        ]

    , testGroup "Span properties"
        [ fastProperty "emptySpan consistency" prop_emptySpan_consistency
        , fastProperty "spanFrom equals emptySpan" prop_spanFrom_equals_emptySpan
        , fastProperty "spanTo structure" prop_spanTo_structure
        , fastProperty "spanBetween preserves bounds" prop_spanBetween_preserves_bounds
        , fastProperty "isValidSpan logic" prop_isValidSpan_logic
        ]

    , testGroup "Span merging properties"
        [ fastProperty "mergeSpans bounds" prop_mergeSpans_bounds
        , fastProperty "mergeSpans commutative" prop_mergeSpans_commutative
        , fastProperty "mergeSpans associative" prop_mergeSpans_associative
        ]

    , testGroup "Located value properties"
        [ fastProperty "locatedAt structure" prop_locatedAt_structure
        , fastProperty "locatedWithSpan structure" prop_locatedWithSpan_structure
        , fastProperty "mapLocated preserves location" prop_mapLocated_preserves_location
        ]
    ]