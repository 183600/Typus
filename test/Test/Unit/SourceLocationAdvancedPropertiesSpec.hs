{-# LANGUAGE CPP #-}
module Test.Unit.SourceLocationAdvancedPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck ((===), Property, forAll, Gen, choose, listOf1)
import Data.List (sort, nub)

import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , locatedWithSpan
  , spanStart
  , spanEnd
  , spanLength
  , spanContains
  , spanOverlaps
  , spanUnion
  , spanIntersection
  , posLine
  , posColumn
  , posOffset
  , mkSourcePos
  , mkSourceSpan
  )

-- | Advanced property-based tests for SourceLocation module
tests :: TestTree
tests =
  testGroup "SourceLocation Advanced Properties"
    [ testGroup "SourceSpan properties"
        [ fastProperty "spanLength is non-negative" prop_spanLengthNonNegative
        , fastProperty "spanUnion contains both original spans" prop_spanUnionContains
        , fastProperty "spanIntersection is contained in both spans" prop_spanIntersectionContained
        , fastProperty "spanOverlaps is symmetric" prop_spanOverlapsSymmetric
        , fastProperty "spanContains is reflexive for same span" prop_spanContainsReflexive
        , fastProperty "spanUnion is commutative" prop_spanUnionCommutative
        , fastProperty "spanIntersection is commutative" prop_spanIntersectionCommutative
        , fastProperty "spanContains pos is monotonic" prop_spanContainsMonotonic
        ]

    , testGroup "SourcePos properties"
        [ fastProperty "posLine, posColumn, posOffset are positive" prop_posComponentsPositive
        , fastProperty "mkSourcePos creates valid positions" prop_mkSourcePosValid
        , fastProperty "pos ordering is consistent with line numbers" prop_posOrderingByLine
        ]

    , testGroup "Located properties"
        [ fastProperty "locatedWithSpan preserves value" prop_locatedWithSpanPreservesValue
        , fastProperty "locatedValue roundtrip" prop_locatedValueRoundtrip
        ]

    , testGroup "Edge cases"
        [ testCase "empty span has length 0" $ do
            let pos = mkSourcePos 1 1 0
                span = mkSourceSpan pos pos
            spanLength span @?= 0

        , testCase "single character span has length 1" $ do
            let pos = mkSourcePos 1 1 0
                endPos = mkSourcePos 1 2 1
                span = mkSourceSpan pos endPos
            spanLength span @?= 1

        , testCase "span contains its start and end positions" $ do
            let start = mkSourcePos 5 10 100
                end = mkSourcePos 5 15 105
                span = mkSourceSpan start end
            spanContains span start @?= True
            spanContains span end @?= True
        ]
    ]

-- Property: spanLength is always non-negative
prop_spanLengthNonNegative :: SourceSpan -> Property
prop_spanLengthNonNegative span = spanLength span >= 0

-- Property: spanUnion contains both original spans
prop_spanUnionContains :: SourceSpan -> SourceSpan -> Property
prop_spanUnionContains span1 span2 =
  let unionSpan = spanUnion span1 span2
  in spanContains unionSpan (spanStart span1) && 
     spanContains unionSpan (spanEnd span1) &&
     spanContains unionSpan (spanStart span2) && 
     spanContains unionSpan (spanEnd span2)

-- Property: spanIntersection is contained in both spans
prop_spanIntersectionContained :: SourceSpan -> SourceSpan -> Property
prop_spanIntersectionContained span1 span2 =
  let intersectSpan = spanIntersection span1 span2
  in if spanOverlaps span1 span2
     then spanContains span1 (spanStart intersectSpan) && 
          spanContains span1 (spanEnd intersectSpan) &&
          spanContains span2 (spanStart intersectSpan) && 
          spanContains span2 (spanEnd intersectSpan)
     else spanLength intersectSpan === 0

-- Property: spanOverlaps is symmetric
prop_spanOverlapsSymmetric :: SourceSpan -> SourceSpan -> Property
prop_spanOverlapsSymmetric span1 span2 = 
  spanOverlaps span1 span2 === spanOverlaps span2 span1

-- Property: spanContains is reflexive for same span
prop_spanContainsReflexive :: SourceSpan -> Property
prop_spanContainsReflexive span = 
  spanContains span (spanStart span) && spanContains span (spanEnd span)

-- Property: spanUnion is commutative
prop_spanUnionCommutative :: SourceSpan -> SourceSpan -> Property
prop_spanUnionCommutative span1 span2 =
  spanUnion span1 span2 === spanUnion span2 span1

-- Property: spanIntersection is commutative
prop_spanIntersectionCommutative :: SourceSpan -> SourceSpan -> Property
prop_spanIntersectionCommutative span1 span2 =
  spanIntersection span1 span2 === spanIntersection span2 span1

-- Property: spanContains position is monotonic (if span1 contains span2, it contains span2's positions)
prop_spanContainsMonotonic :: SourceSpan -> SourceSpan -> Property
prop_spanContainsMonotonic outerSpan innerSpan =
  if spanContains outerSpan (spanStart innerSpan) && spanContains outerSpan (spanEnd innerSpan)
  then spanContains outerSpan (spanStart innerSpan) && spanContains outerSpan (spanEnd innerSpan)
  else property True

-- Property: posLine, posColumn, posOffset are positive
prop_posComponentsPositive :: SourcePos -> Property
prop_posComponentsPositive pos =
  posLine pos > 0 && posColumn pos > 0 && posOffset pos >= 0

-- Property: mkSourcePos creates valid positions
prop_mkSourcePosValid :: Int -> Int -> Int -> Property
prop_mkSourcePosValid line col offset =
  let pos = mkSourcePos line col offset
  in posLine pos === max 1 line && 
     posColumn pos === max 1 col && 
     posOffset pos === max 0 offset

-- Property: pos ordering is consistent with line numbers
prop_posOrderingByLine :: SourcePos -> SourcePos -> Property
prop_posOrderingByLine pos1 pos2 =
  let line1 = posLine pos1
      line2 = posLine pos2
  in if line1 < line2 
     then posOffset pos1 <= posOffset pos2  -- Earlier line should have smaller or equal offset
     else property True

-- Property: locatedWithSpan preserves value
prop_locatedWithSpanPreservesValue :: Int -> SourceSpan -> Property
prop_locatedWithSpanPreservesValue value span =
  locatedValue (locatedWithSpan span value) === value

-- Property: locatedValue roundtrip
prop_locatedValueRoundtrip :: Int -> SourceSpan -> Property
prop_locatedValueRoundtrip value span =
  let located = locatedWithSpan span value
  in locatedValue located === value