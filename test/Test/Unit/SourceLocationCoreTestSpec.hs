{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.SourceLocationCoreTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, arbitrary, choose, listOf, elements, oneof, sized, suchThat)

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

import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.Char (isSpace)

-- ============================================================================
-- Generators for QuickCheck
-- ============================================================================

-- Generate a valid source position (1-based line L.and column)
genValidSourcePos :: Gen SourcePos
genValidSourcePos = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  offset <- choose (0, 1000000)
  return $ SourcePos line col offset

-- Generate a source position with potentially invalid values
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (-10, 1000)
  col <- choose (-10, 1000)
  offset <- choose (-1000, 1000000)
  return $ SourcePos line col offset

-- Generate a valid source span
genValidSourceSpan :: Gen SourceSpan
genValidSourceSpan = do
  start <- genValidSourcePos
  endOffset <- choose (0, 100)
  let end = start { posOffset = posOffset start + endOffset, 
                   posColumn = posColumn start + endOffset }
  return $ SourceSpan start end

-- Generate L.any source span (potentially invalid)
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  end <- genSourcePos
  return $ SourceSpan start end

-- Generate a located value
genLocated :: Gen a -> Gen (Located a)
genLocated genValue = do
  value <- genValue
  span <- genValidSourceSpan
  return $ locatedWithSpan span value

-- Generate text for advancement testing
genText :: Gen Text
genText = T.pack <$> listOf (elements ['a'..'z', 'A'..'Z', '0'..'9', ' ', '\t', '\n'])

-- ============================================================================
-- Unit Tests
-- ============================================================================

-- Test basic position creation L.and properties
testPositionBasics :: TestTree
testPositionBasics = testGroup "Position Basics"
  [ testCase "startPos has correct values" $ do
      startPos @?= SourcePos 1 1 0
      
  , testCase "posAt creates position correctly" $ do
      let pos = posAt 5 10
      posLine pos @?= 5
      posColumn pos @?= 10
      posOffset pos @?= 0
      
  , testCase "posAtLineCol creates position correctly" $ do
      let pos = posAtLineCol 3 7 42
      posLine pos @?= 3
      posColumn pos @?= 7
      posOffset pos @?= 42
  ]

-- Test position advancement
testPositionAdvancement :: TestTree
testPositionAdvancement = testGroup "Position Advancement"
  [ testCase "posAfter handles newline correctly" $ do
      let start = posAt 1 5
          after = posAfter '\n' start
      posLine after @?= 2
      posColumn after @?= 1
      posOffset after @?= 6
      
  , testCase "posAfter handles tab correctly" $ do
      let start = posAt 1 3
          after = posAfter '\t' start
      posLine after @?= 1
      posColumn after @?= 9  -- Next tab position (3 -> 8 + 1)
      posOffset after @?= 4
      
  , testCase "posAfter handles regular character correctly" $ do
      let start = posAt 1 5
          after = posAfter 'a' start
      posLine after @?= 1
      posColumn after @?= 6
      posOffset after @?= 5
  ]

-- Test span operations
testSpanOperations :: TestTree
testSpanOperations = testGroup "Span Operations"
  [ testCase "emptySpan creates span with same start L.and end" $ do
      let pos = posAt 2 3
          span = emptySpan pos
      spanStart span @?= pos
      spanEnd span @?= pos
      
  , testCase "spanBetween creates correct span" $ do
      let start = posAt 1 5
          end = posAt 2 10
          span = spanBetween start end
      spanStart span @?= start
      spanEnd span @?= end
      
  , testCase "mergeSpans combines spans correctly" $ do
      let span1 = spanBetween (posAt 1 1) (posAt 1 10)
          span2 = spanBetween (posAt 1 5) (posAt 2 5)
          merged = mergeSpans span1 span2
      spanStart merged @?= posAt 1 1
      spanEnd merged @?= posAt 2 5
      
  , testCase "isValidSpan checks span validity" $ do
      let validSpan = spanBetween (posAt 1 1) (posAt 1 10)
          invalidSpan = spanBetween (posAt 2 10) (posAt 1 5)
      assertBool "validSpan should be valid" $ isValidSpan validSpan
      assertBool "invalidSpan should be invalid" $ not $ isValidSpan invalidSpan
  ]

-- Test located values
testLocatedValues :: TestTree
testLocatedValues = testGroup "Located Values"
  [ testCase "locatedAt creates located value at position" $ do
      let pos = posAt 3 7
          located = locatedAt pos "test"
      locatedValue located @?= "test"
      locatedPos located @?= pos
      spanStart (locatedSpan located) @?= pos
      spanEnd (locatedSpan located) @?= pos
      
  , testCase "locatedWithSpan creates located value with span" $ do
      let span = spanBetween (posAt 1 1) (posAt 1 5)
          located = locatedWithSpan span "content"
      locatedValue located @?= "content"
      locatedSpan located @?= span
      
  , testCase "mapLocated applies function to value" $ do
      let span = spanBetween (posAt 1 1) (posAt 1 5)
          located = locatedWithSpan span 42
          mapped = mapLocated (*2) located
      locatedValue mapped @?= 84
      locatedSpan mapped @?= span
  ]

-- Test location tracking
testLocationTracking :: TestTree
testLocationTracking = testGroup "Location Tracking"
  [ testCase "runLocationTracker starts at startPos" $ do
      let result = runLocationTracker getCurrentPos
      result @?= startPos
      
  , testCase "setCurrentPos L.and getCurrentPos work together" $ do
      let newPos = posAt 5 10
          (result, finalPos) = withLocationTracking startPos $ do
              setCurrentPos newPos
              getCurrentPos
      result @?= newPos
      finalPos @?= newPos
      
  , testCase "markSpanStart L.and markSpanEnd create span" $ do
      let start = posAt 1 1
          (result, _) = withLocationTracking start $ do
              setCurrentPos (posAt 1 5)
              markSpanEnd start
      spanStart result @?= start
      spanEnd result @?= posAt 1 5
  ]

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: posAfter advances offset by 1
prop_posAfter_advances_offset :: Char -> SourcePos -> Property
prop_posAfter_advances_offset char pos =
  let after = posAfter char pos
  in property $ posOffset after === posOffset pos + 1

-- Property: posAfter handles newline correctly
prop_posAfter_newline :: SourcePos -> Property
prop_posAfter_newline pos =
  let after = posAfter '\n' pos
  in property $ posLine after === posLine pos + 1 .&&.
                posColumn after === 1

-- Property: advancePosBy advances correctly for multiple characters
prop_advancePosBy_consistency :: String -> SourcePos -> Property
prop_advancePosBy_consistency chars pos =
  let advanced1 = advancePosBy chars pos
      advanced2 = L.foldl (flip advancePos) pos chars
  in property $ advanced1 === advanced2

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

-- Property: spanFrom creates empty span at position
prop_spanFrom_empty_at_position :: SourcePos -> Property
prop_spanFrom_empty_at_position pos =
  let span = spanFrom pos
  in property $ spanStart span === pos .&&. spanEnd span === pos

-- Property: locatedValue returns the original value
prop_locatedValue_identity :: Int -> SourceSpan -> Property
prop_locatedValue_identity value span =
  let located = locatedWithSpan span value
  in property $ locatedValue located === value

-- Property: mapLocated preserves span
prop_mapLocated_preserves_span :: Int -> SourceSpan -> Property
prop_mapLocated_preserves_span value span =
  let located = locatedWithSpan span value
      mapped = mapLocated (*2) located
  in property $ locatedSpan mapped === span

-- Property: advancePosByText is consistent with advancePosBy
prop_advancePosByText_consistency :: Text -> SourcePos -> Property
prop_advancePosByText_consistency text pos =
  let advanced1 = advancePosByText text pos
      advanced2 = advancePosBy (T.unpack text) pos
  in property $ advanced1 === advanced2

-- Property: advancePosByLine advances line numbers correctly
prop_advancePosByLine_advances_lines :: Int -> SourcePos -> Property
prop_advancePosByLine_advances_lines numLines pos =
  numLines >= 0 ==> 
  let advanced = advancePosByLine numLines pos
  in property $ posLine advanced === posLine pos + numLines .&&.
                posColumn advanced === 1

-- Property: error location conversion preserves position info
prop_toErrorLocation_preserves_position :: SourcePos -> Property
prop_toErrorLocation_preserves_position pos =
  let errLoc = toErrorLocation pos
  in property $ line errLoc === posLine pos .&&.
                column errLoc === posColumn pos

-- Property: error location conversion with span preserves range info
prop_toErrorLocationWithSpan_preserves_range :: SourceSpan -> Property
prop_toErrorLocationWithSpan_preserves_range span =
  let errLoc = toErrorLocationWithSpan span
      start = spanStart span
      end = spanEnd span
  in property $ line errLoc === posLine start .&&.
                column errLoc === posColumn start .&&.
                endLine errLoc === Just (posLine end) .&&.
                endColumn errLoc === Just (posColumn end)

-- Property: HasLocation instance works correctly
prop_hasLocation_instance :: Int -> SourceSpan -> Property
prop_hasLocation_instance value span =
  let located = locatedWithSpan span value
  in property $ getLocation located === span

-- Property: Located functor law: fmap id = id
prop_located_functor_identity :: Int -> SourceSpan -> Property
prop_located_functor_identity value span =
  let located = locatedWithSpan span value
  in property $ mapLocated id located === located

-- Property: Located functor law: fL.map (f . g) = fmap f . fmap g
prop_located_functor_composition :: Int -> SourceSpan -> Property
prop_located_functor_composition value span =
  let located = locatedWithSpan span value
      f = (*2)
      g = (+1)
  in property $ mapLocated (f . g) located === mapLocated f (mapLocated g located)

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "SourceLocation Core Tests"
  [ testPositionBasics
  , testPositionAdvancement
  , testSpanOperations
  , testLocatedValues
  , testLocationTracking
  , testGroup "QuickCheck Properties"
    [ fastProperty "posAfter advances offset" prop_posAfter_advances_offset
    , fastProperty "posAfter handles newline" prop_posAfter_newline
    , fastProperty "advancePosBy consistency" prop_advancePosBy_consistency
    , fastProperty "mergeSpans is commutative" prop_mergeSpans_commutative
    , fastProperty "mergeSpans is associative" prop_mergeSpans_associative
    , fastProperty "spanFrom creates empty span" prop_spanFrom_empty_at_position
    , fastProperty "locatedValue returns original" prop_locatedValue_identity
    , fastProperty "mapLocated preserves span" prop_mapLocated_preserves_span
    , fastProperty "advancePosByText consistency" prop_advancePosByText_consistency
    , fastProperty "advancePosByLine advances lines" prop_advancePosByLine_advances_lines
    , fastProperty "toErrorLocation preserves position" prop_toErrorLocation_preserves_position
    , fastProperty "toErrorLocationWithSpan preserves range" prop_toErrorLocationWithSpan_preserves_range
    , fastProperty "HasLocation instance works" prop_hasLocation_instance
    , fastProperty "Located functor identity" prop_located_functor_identity
    , fastProperty "Located functor composition" prop_located_functor_composition
    ]
  ]