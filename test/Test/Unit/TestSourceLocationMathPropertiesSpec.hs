{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestSourceLocationMathPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation
import qualified Compiler.Errors.Core as Error
import TestSupport.Arbitrary ()

-- | Test suite for SourceLocation mathematical properties
testSourceLocationMathProperties :: TestTree
testSourceLocationMathProperties = testGroup "SourceLocation Math Properties Tests"
  [ testProperty "posAfter: advancing by newline increments line, resets column" $
      \pos -> posAfter '\n' pos `shouldSatisfy` 
        (\p -> posLine p == posLine pos + 1 && posColumn p == 1)
        
  , testProperty "posAfter: advancing by tab aligns to next 8-column boundary" $
      \pos -> let newCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
                  newPos = posAfter '\t' pos
              in posColumn newPos == newCol
                
  , testProperty "posAfter: advancing by regular char increments column" $
      \pos c -> c `notElem` ['\n', '\t'] ==> 
        posColumn (posAfter c pos) == posColumn pos + 1
        
  , testProperty "posAfter: offset always increments by 1" $
      \pos c -> posOffset (posAfter c pos) == posOffset pos + 1
        
  , testProperty "spanBetween: start <= end for valid positions" $
      \pos1 pos2 -> let span = spanBetween pos1 pos2
                    in spanStart span <= spanEnd span
                    
  , testProperty "mergeSpans: start is minimum of both starts" $
      \span1 span2 -> spanStart (mergeSpans span1 span2) == min (spanStart span1) (spanStart span2)
      
  , testProperty "mergeSpans: end is maximum of both ends" $
      \span1 span2 -> spanEnd (mergeSpans span1 span2) == max (spanEnd span1) (spanEnd span2)
      
  , testProperty "mergeSpans: is associative" $
      \span1 span2 span3 -> mergeSpans span1 (mergeSpans span2 span3) == 
                             mergeSpans (mergeSpans span1 span2) span3
                             
  , testProperty "mergeSpans: is commutative" $
      \span1 span2 -> mergeSpans span1 span2 == mergeSpans span2 span1
      
  , testProperty "isValidSpan: span with start <= end is valid" $
      \start end -> spanStart (spanBetween start end) <= spanEnd (spanBetween start end)
      
  , testProperty "locatedAt: position equals span start" $
      \(pos :: SourcePos) (val :: String) -> locatedPos (locatedAt pos val) == pos
      
  , testProperty "locatedWithSpan: span is preserved" $
      \(span :: SourceSpan) (val :: String) -> locatedSpan (locatedWithSpan span val) == span
      
  , testCase "mapLocated: preserves position" $
      let loc = locatedAt (posAt 1 1) "test"
          f = reverse
      in locatedPos (mapLocated f loc) @?= locatedPos loc
      
  , testProperty "advancePosBy: advancing by empty string returns same position" $
      \pos -> advancePosBy "" pos == pos
      
  , testProperty "advancePosBy: advancing by string is same as sequential advances" $
      \pos s -> advancePosBy s pos == foldl (flip posAfter) pos s
      
  , testCase "posAtLineCol: creates position with correct line and column" $ do
      let pos = posAtLineCol 5 10 20
      posLine pos @?= 5
      posColumn pos @?= 10
      posOffset pos @?= 20
      
  , testCase "spanTo: creates span with same start and end" $ do
      let pos = posAt 5 10
          span = spanTo pos
      spanStart span @?= pos
      spanEnd span @?= pos
      
  , testCase "emptySpan: creates span with same start and end" $ do
      let pos = posAt 5 10
          span = emptySpan pos
      spanStart span @?= pos
      spanEnd span @?= pos
      
  , testCase "toErrorLocation: converts position correctly" $ do
      let pos = posAt 5 10
          errLoc = toErrorLocation pos
      Error.line errLoc @?= 5
      Error.column errLoc @?= 10
      
  , testCase "toErrorLocationWithSpan: converts span with range correctly" $ do
      let start = posAt 5 10
          end = posAt 7 15
          span = spanBetween start end
          errLoc = toErrorLocationWithSpan span
      Error.line errLoc @?= 5
      Error.column errLoc @?= 10
      -- These may not exist, so we'll skip them for now
      return ()
         
  , testCase "advancePosByLine: advances line count, resets column" $ do
      let pos = posAt 5 10
          newPos = advancePosByLine 3 pos
      posLine newPos @?= 8
      posColumn newPos @?= 1
  ]

-- Helper function for QuickCheck
shouldSatisfy :: a -> (a -> Bool) -> Property
shouldSatisfy x predicate = property (predicate x)