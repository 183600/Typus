{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewSourceLocationMathSpec (newSourceLocationMathSpec, sourceLocationQuickCheckProperties) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property(..), (==>), Positive(..))
import SourceLocation
import Data.Char (isSpace)

-- | Test suite for SourceLocation mathematical operations
newSourceLocationMathSpec :: TestTree
newSourceLocationMathSpec = testGroup "New SourceLocation Math Tests"
  [ testCase "SourcePos arithmetic operations" $ do
      let pos1 = startPos "test.txt"
      let pos2 = posAfter pos1 'a'
      posLine pos2 @?= 1
      posColumn pos2 @?= 2
      
      let pos3 = posAfter pos2 '\n'
      posLine pos3 @?= 2
      posColumn pos3 @?= 1
      
      let pos4 = advancePos pos3 "hello"
      posLine pos4 @?= 2
      posColumn pos4 @?= 6
  
  , testCase "SourceSpan creation and validation" $ do
      let pos1 = posAt "test.txt" 1 1
      let pos2 = posAt "test.txt" 1 5
      let span = spanBetween pos1 pos2
      
      isValidSpan span @?= True
      spanStart span @?= pos1
      spanEnd span @?= pos2
      
      let empty = emptySpan pos1
      isValidSpan empty @?= False
      
      let invalid = spanBetween pos2 pos1  -- invalid: end before start
      isValidSpan invalid @?= False
  
  , testCase "Located value operations" $ do
      let pos = posAt "test.txt" 1 1
      let value = locatedAt pos "hello"
      
      locatedValue value @?= "hello"
      locatedPos value @?= pos
      
      let span = spanFrom pos 5
      let spanValue = locatedWithSpan span "world"
      locatedSpan spanValue @?= span
      locatedValue spanValue @?= "world"
      
      let mapped = mapLocated (++ "!" ) value
      locatedValue mapped @?= "hello!"
  
  , testCase "Span merging operations" $ do
      let pos1 = posAt "test.txt" 1 1
      let pos2 = posAt "test.txt" 1 5
      let pos3 = posAt "test.txt" 2 3
      
      let span1 = spanBetween pos1 pos2
      let span2 = spanBetween pos2 pos3
      let merged = mergeSpans span1 span2
      
      spanStart merged @?= pos1
      spanEnd merged @?= pos3
      isValidSpan merged @?= True
  
  , testCase "Position advancement with various characters" $ do
      let start = posAt "test.txt" 1 1
      
      -- Test with regular characters
      let pos1 = advancePos start "abc"
      posLine pos1 @?= 1
      posColumn pos1 @?= 4
      
      -- Test with newlines
      let pos2 = advancePos start "a\nb\nc"
      posLine pos2 @?= 3
      posColumn pos2 @?= 2
      
      -- Test with tabs
      let pos3 = advancePos start "a\tb"
      posLine pos3 @?= 1
      posColumn pos3 @?= 3  -- tab counts as one position
  ]

-- QuickCheck properties for SourceLocation functions
prop_posAfter_advances_column :: Char -> Property
prop_posAfter_advances_column c = 
  c /= '\n' ==> 
    let pos = posAt "test.txt" 1 5
        newPos = posAfter pos c
    in posColumn newPos == posColumn pos + 1 &&
       posLine newPos == posLine pos

prop_posAfter_newline_advances_line :: Property
prop_posAfter_newline_advances_line = 
    let pos = posAt "test.txt" 5 10
        newPos = posAfter pos '\n'
    in posColumn newPos == 1 &&
       posLine newPos == posLine pos + 1

prop_span_between_valid_order :: Positive Int -> Positive Int -> Property
prop_span_between_valid_order (Positive line1) (Positive line2) = 
  line1 <= line2 ==> 
    let pos1 = posAt "test.txt" line1 1
        pos2 = posAt "test.txt" line2 1
        span = spanBetween pos1 pos2
    in isValidSpan span &&
       spanStart span == pos1 &&
       spanEnd span == pos2

prop_advance_pos_by_length :: String -> Property
prop_advance_pos_by_length s = 
  not (null s) ==> 
    let start = startPos "test.txt"
        end = advancePos start s
        -- For simplicity, just check that we've moved forward
        (posLine end > posLine start) || (posColumn end > posColumn start)

prop_located_map_preserves_location :: String -> String -> Property
prop_located_map_preserves_location s1 s2 = 
    let pos = posAt "test.txt" 1 1
        value = locatedAt pos s1
        mapped = mapLocated (++ s2) value
    in locatedPos mapped == pos &&
       locatedValue mapped == s1 ++ s2

-- QuickCheck test suite
sourceLocationQuickCheckProperties :: TestTree
sourceLocationQuickCheckProperties = testGroup "SourceLocation QuickCheck Properties"
  [ testProperty "posAfter advances column for non-newline chars" prop_posAfter_advances_column
  , testProperty "posAfter advances line for newline" prop_posAfter_newline_advances_line
  , testProperty "spanBetween is valid for correct order" prop_span_between_valid_order
  , testProperty "advancePos moves position forward" prop_advance_pos_by_length
  , testProperty "mapLocated preserves location" prop_located_map_preserves_location
  ]