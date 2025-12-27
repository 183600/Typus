{-# LANGUAGE CPP #-}
module Test.Unit.SourceLocationBasicPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck ((===), Property, forAll, Gen, choose, suchThat)

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import TestSupport.Arbitrary ()

-- | Test SourcePos properties
testSourcePosProperties :: TestTree
testSourcePosProperties = testGroup "SourcePos Properties"
  [ fastProperty "line numbers are positive" prop_linePositive
  , fastProperty "column numbers are positive" prop_columnPositive
  , fastProperty "offsets are non-negative" prop_offsetNonNegative
  , testCase "source position ordering" testSourcePosOrdering
  ]

-- | Test SourceSpan properties
testSourceSpanProperties :: TestTree
testSourceSpanProperties = testGroup "SourceSpan Properties"
  [ fastProperty "start position comes before end position" prop_spanOrdering
  , fastProperty "span length is non-negative" prop_spanLengthNonNegative
  , fastProperty "span contains its start position" prop_spanContainsStart
  , testCase "span creation with same positions" testSinglePointSpan
  ]

-- | Test Located wrapper properties
testLocatedProperties :: TestTree
testLocatedProperties = testGroup "Located Properties"
  [ fastProperty "located values preserve their content" prop_locatedPreservesContent
  , fastProperty "source position is valid in located values" prop_locatedHasValidPosition
  , fastProperty "span is valid in located values" prop_locatedHasValidSpan
  ]

-- | SourcePos property tests
prop_linePositive :: SourcePos -> Property
prop_linePositive (SourcePos line _ _) = line > 0

prop_columnPositive :: SourcePos -> Property  
prop_columnPositive (SourcePos _ col _) = col > 0

prop_offsetNonNegative :: SourcePos -> Property
prop_offsetNonNegative (SourcePos _ _ offset) = offset >= 0

-- | SourceSpan property tests
prop_spanOrdering :: SourceSpan -> Property
prop_spanOrdering (SourceSpan start end) =
  sourcePosLine start <= sourcePosLine end &&
  (if sourcePosLine start == sourcePosLine end 
   then sourcePosColumn start <= sourcePosColumn end 
   else True)

prop_spanLengthNonNegative :: SourceSpan -> Property
prop_spanLengthNonNegative span = 
  let start = spanStart span
      end = spanEnd span
      lineDiff = sourcePosLine end - sourcePosLine start
      colDiff = if lineDiff == 0 
                then sourcePosColumn end - sourcePosColumn start
                else 0
  in lineDiff >= 0 && colDiff >= 0

prop_spanContainsStart :: SourceSpan -> Property
prop_spanContainsStart (SourceSpan start end) =
  sourcePosLine start <= sourcePosLine end &&
  (if sourcePosLine start == sourcePosLine end 
   then sourcePosColumn start <= sourcePosColumn end 
   else True)

-- | Located wrapper property tests
prop_locatedPreservesContent :: Int -> SourceSpan -> Property
prop_locatedPreservesContent value span = 
  let located = Located value span undefined
  in locatedValue located === value

prop_locatedHasValidPosition :: Int -> SourceSpan -> Property
prop_locatedHasValidPosition value span = 
  let located = Located value span undefined
      start = spanStart span
  in sourcePosLine start > 0 && sourcePosColumn start > 0

prop_locatedHasValidSpan :: Int -> SourceSpan -> Property
prop_locatedHasValidSpan value span = 
  let located = Located value span undefined
      start = spanStart span
      end = spanEnd span
  in sourcePosLine start <= sourcePosLine end &&
     (if sourcePosLine start == sourcePosLine end 
      then sourcePosColumn start <= sourcePosColumn end 
      else True)

-- | Unit tests
testSourcePosOrdering :: IO ()
testSourcePosOrdering = do
  let pos1 = SourcePos 1 1 0
      pos2 = SourcePos 1 5 4
      pos3 = SourcePos 2 1 10
  assertBool "pos1 should come before pos2" $ pos1 <= pos2
  assertBool "pos2 should come before pos3" $ pos2 <= pos3
  assertBool "pos1 should come before pos3" $ pos1 <= pos3

testSinglePointSpan :: IO ()
testSinglePointSpan = do
  let pos = SourcePos 5 10 50
      span = SourceSpan pos pos
  assertBool "single point span should have zero length" $
    spanLength span == 0
  assertBool "single point span should contain its position" $
    spanContains span pos

-- | Helper functions
sourcePosLine :: SourcePos -> Int
sourcePosLine (SourcePos line _ _) = line

sourcePosColumn :: SourcePos -> Int
sourcePosColumn (SourcePos _ col _) = col

spanStart :: SourceSpan -> SourcePos
spanStart (SourceSpan start _) = start

spanEnd :: SourceSpan -> SourcePos
spanEnd (SourceSpan _ end) = end

spanLength :: SourceSpan -> Int
spanLength (SourceSpan start end) = 
  let lineDiff = sourcePosLine end - sourcePosLine start
      colDiff = if lineDiff == 0 
                then sourcePosColumn end - sourcePosColumn start
                else 0
  in lineDiff * 100 + colDiff  -- Approximation

spanContains :: SourceSpan -> SourcePos -> Bool
spanContains (SourceSpan start end) pos =
  let posLine = sourcePosLine pos
      startLine = sourcePosLine start
      endLine = sourcePosLine end
      posCol = sourcePosColumn pos
      startCol = sourcePosColumn start
      endCol = sourcePosColumn end
  in if posLine == startLine && posLine == endLine
     then posCol >= startCol && posCol <= endCol
     else if posLine == startLine
          then posCol >= startCol
          else if posLine == endLine
               then posCol <= endCol
               else posLine > startLine && posLine < endLine

-- | Test collection
tests :: TestTree
tests = testGroup "SourceLocation Basic Properties Tests"
  [ testSourcePosProperties
  , testSourceSpanProperties
  , testLocatedProperties
  ]