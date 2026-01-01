{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationCorePropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf1, elements)
import Data.List (nub)

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
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
  , mapLocated
  , advancePos
  , advancePosBy
  , mergeSpans
  , spanStart
  , spanEnd
  )

-- | Generate a valid source position (positive line L.and column)
genValidPos :: Gen SourcePos
genValidPos = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  return $ SourcePos line col

-- | Generate a valid source span
genValidSpan :: Gen SourceSpan
genValidSpan = do
  startLine <- choose (1, 100)
  startCol <- choose (1, 100)
  endLine <- choose (startLine, startLine + 50)  -- Ensure end is after start
  endCol <- if endLine == startLine 
              then choose (startCol, startCol + 50)
              else choose (1, 100)
  return $ SourceSpan (SourcePos startLine startCol) (SourcePos endLine endCol)

instance Arbitrary SourcePos where
  arbitrary = genValidPos

instance Arbitrary SourceSpan where
  arbitrary = genValidSpan

-- Property: startPos has line 1, column 1
prop_startPos_values :: Property
prop_startPos_values =
  posLine startPos === 1 .&&.
  posColumn startPos === 1

-- Property: posAfter advances column by 1 on same line
prop_posAfter_sameLine :: Property
prop_posAfter_sameLine =
  forAll genValidPos $ \pos ->
    let newPos = posAfter pos
    in posLine newPos === posLine pos .&&.
       posColumn newPos === posColumn pos + 1

-- Property: emptySpan has the same start L.and end position
prop_emptySpan_sameStartEnd :: Property
prop_emptySpan_sameStartEnd =
  spanStart emptySpan === spanEnd emptySpan

-- Property: spanFrom creates a span with same start L.and end
prop_spanFrom_sameStartEnd :: Property
prop_spanFrom_sameStartEnd =
  forAll genValidPos $ \pos ->
    let span = spanFrom pos
    in spanStart span === pos .&&. spanEnd span === pos

-- Property: spanTo creates a span with same start L.and end
prop_spanTo_sameStartEnd :: Property
prop_spanTo_sameStartEnd =
  forAll genValidPos $ \pos ->
    let span = spanTo pos
    in spanStart span === pos .&&. spanEnd span === pos

-- Property: spanBetween creates a valid span
prop_spanBetween_validSpan :: Property
prop_spanBetween_validSpan =
  forAll genValidPos $ \start ->
    forAll genValidPos $ \end ->
      let span = spanBetween start end
      in isValidSpan span === True

-- Property: mergeSpans contains both original spans
prop_mergeSpans_containsOriginals :: Property
prop_mergeSpans_containsOriginals =
  forAll genValidSpan $ \span1 ->
    forAll genValidSpan $ \span2 ->
      let merged = mergeSpans span1 span2
          start1 = spanStart span1
          end1 = spanEnd span1
          start2 = spanStart span2
          end2 = spanEnd span2
          mergedStart = spanStart merged
          mergedEnd = spanEnd merged
      in (posLine mergedStart <= posLine start1 .&&. posColumn mergedStart <= posColumn start1) .&&.
         (posLine mergedEnd >= posLine end1 .&&. posColumn mergedEnd >= posColumn end1) .&&.
         (posLine mergedStart <= posLine start2 .&&. posColumn mergedStart <= posColumn start2) .&&.
         (posLine mergedEnd >= posLine end2 .&&. posColumn mergedEnd >= posColumn end2)

-- Property: locatedAt creates a Located value with correct position
prop_locatedAt_correctPosition :: Property
prop_locatedAt_correctPosition =
  forAll genValidPos $ \pos ->
    forAll arbitrary $ \value ->
      let located = locatedAt pos value
      in locatedSpan located === spanFrom pos .&&.
         locatedValue located === value

-- Property: locatedWithSpan creates a Located value with correct span
prop_locatedWithSpan_correctSpan :: Property
prop_locatedWithSpan_correctSpan =
  forAll genValidSpan $ \span ->
    forAll arbitrary $ \value ->
      let located = locatedWithSpan span value
      in locatedSpan located === span .&&.
         locatedValue located === value

-- Property: mapLocated preserves location but transforms value
prop_mapLocated_preservesLocation :: Property
prop_mapLocated_preservesLocation =
  forAll genValidSpan $ \span ->
    forAll arbitrary $ \value ->
      let located = locatedWithSpan span value
          transformed = mapLocated (+1) located
      in locatedSpan transformed === span .&&.
         locatedValue transformed === value + 1

-- Property: advancePos by 0 returns same position
prop_advancePosBy_zero :: Property
prop_advancePosBy_zero =
  forAll genValidPos $ \pos ->
    advancePosBy pos 0 === pos

-- Property: advancePos by positive number increases position
prop_advancePosBy_positive :: Property
prop_advancePosBy_positive =
  forAll genValidPos $ \pos ->
    forAll (choose (1, 100)) $ \n ->
      let newPos = advancePosBy pos n
      in posLine newPos >= posLine pos .&&.
         (posLine newPos > posLine pos .||. posColumn newPos > posColumn pos)

-- Property: isValidSpan returns true for spans generated by our generator
prop_isValidSpan_generatedSpans :: Property
prop_isValidSpan_generatedSpans =
  forAll genValidSpan $ \span ->
    isValidSpan span === True

tests :: TestTree
tests =
  testGroup "SourceLocation Core Properties"
    [ fastProperty "startPos has line 1, column 1" prop_startPos_values
    , fastProperty "posAfter advances column by 1 on same line" prop_posAfter_sameLine
    , fastProperty "emptySpan has same start L.and end position" prop_emptySpan_sameStartEnd
    , fastProperty "spanFrom creates span with same start L.and end" prop_spanFrom_sameStartEnd
    , fastProperty "spanTo creates span with same start L.and end" prop_spanTo_sameStartEnd
    , fastProperty "spanBetween creates valid span" prop_spanBetween_validSpan
    , fastProperty "mergeSpans contains both original spans" prop_mergeSpans_containsOriginals
    , fastProperty "locatedAt creates Located value with correct position" prop_locatedAt_correctPosition
    , fastProperty "locatedWithSpan creates Located value with correct span" prop_locatedWithSpan_correctSpan
    , fastProperty "mapLocated preserves location but transforms value" prop_mapLocated_preservesLocation
    , fastProperty "advancePosBy 0 returns same position" prop_advancePosBy_zero
    , fastProperty "advancePosBy positive number increases position" prop_advancePosBy_positive
    , fastProperty "isValidSpan returns true for generated spans" prop_isValidSpan_generatedSpans
    ]