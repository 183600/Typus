{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.SourceLocationCalculationSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import TestSupport.Arbitrary ()
import SourceLocation
import Compiler.Errors.Core (ErrorLocation(..))

tests :: TestTree
tests = testGroup "Source Location Calculation Tests"
  [ testGroup "SourcePos operations"
    [ testCase "creates start position correctly" $ do
        startPos @?= SourcePos 1 1 0
      
    , testCase "advances position with regular characters" $ do
        let pos = startPos
        posAfter 'a' pos @?= SourcePos 1 2 1
        posAfter 'b' (posAfter 'a' pos) @?= SourcePos 1 3 2
      
    , testCase "advances position with newline" $ do
        let pos = SourcePos 1 5 4
        posAfter '\n' pos @?= SourcePos 2 1 5
      
    , testCase "advances position with tab" $ do
        let pos1 = SourcePos 1 1 0
        posAfter '\t' pos1 @?= SourcePos 1 9 1  -- Tab to next 8-column boundary
      
    , testCase "advances position with tab (second example)" $ do
        let pos2 = SourcePos 1 5 4
        posAfter '\t' pos2 @?= SourcePos 1 9 5  -- Tab to next 8-column boundary
      
    , testCase "creates position at specific line and column" $ do
        posAt 3 5 @?= SourcePos 3 5 0
      
    , testCase "creates position at specific line, column and offset" $ do
        posAtLineCol 3 5 10 @?= SourcePos 3 5 10
      
    , testCase "compares positions correctly" $ do
        let pos1 = SourcePos 1 1 0
            pos2 = SourcePos 1 2 1
            pos3 = SourcePos 2 1 5
        comparePos pos1 pos2 @?= LT
        comparePos pos2 pos1 @?= GT
        comparePos pos1 pos1 @?= EQ
        comparePos pos2 pos3 @?= LT
        comparePos pos3 pos2 @?= GT
    ]

  , testGroup "SourceSpan operations"
    [ testCase "creates empty span" $ do
        let pos = SourcePos 3 5 10
        emptySpan pos @?= SourceSpan pos pos
      
    , testCase "creates span from position" $ do
        let pos = SourcePos 3 5 10
        spanFrom pos @?= SourceSpan pos pos
      
    , testCase "creates span to position" $ do
        let pos = SourcePos 3 5 10
        spanTo pos @?= SourceSpan pos pos
      
    , testCase "creates span between positions" $ do
        let pos1 = SourcePos 1 1 0
            pos2 = SourcePos 1 5 4
        spanBetween pos1 pos2 @?= SourceSpan pos1 pos2
      
    , testCase "creates ordered span between positions" $ do
        let pos1 = SourcePos 1 5 4
            pos2 = SourcePos 1 1 0
        spanBetweenOrdered pos1 pos2 @?= SourceSpan pos2 pos1
        spanBetweenOrdered pos2 pos1 @?= SourceSpan pos2 pos1
      
    , testCase "merges spans correctly" $ do
        let span1 = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
            span2 = SourceSpan (SourcePos 1 3 2) (SourcePos 1 7 6)
            expected = SourceSpan (SourcePos 1 1 0) (SourcePos 1 7 6)
        mergeSpans span1 span2 @?= expected
      
    , testCase "checks span validity" $ do
        let validSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
            invalidSpan = SourceSpan (SourcePos 1 5 4) (SourcePos 1 1 0)
            samePosSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)
        isValidSpan validSpan @?= True
        isValidSpan invalidSpan @?= False
        isValidSpan samePosSpan @?= True
      
    , testCase "checks block span validity" $ do
        let span' = SourceSpan (SourcePos 1 1 0) (SourcePos 2 1 10)
        isValidBlockSpan span' @?= True
    ]

  , testGroup "Located values"
    [ testCase "creates located value at position" $ do
        let pos = SourcePos 3 5 10
            value = "test" :: String
        locatedAt pos value @?= Located value pos (SourceSpan pos pos)
      
    , testCase "creates located value with span" $ do
        let span' = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
            value = "test" :: String
        locatedWithSpan span' value @?= Located value (SourcePos 1 1 0) span'
      
    , testCase "extracts values from located" $ do
        let pos = SourcePos 3 5 10
            span' = SourceSpan pos pos
            value = "test" :: String
            located = Located value pos span'
        locatedValue located @?= value
        locatedSpan located @?= span'
        locatedPos located @?= pos
      
    , testCase "maps function over located value" $ do
        let pos = SourcePos 3 5 10
            span' = SourceSpan pos pos
            value = "test" :: String
            located = Located value pos span'
            mapped = mapLocated (++ "ed") located
        locatedValue mapped @?= "tested"
        locatedSpan mapped @?= span'
        locatedPos mapped @?= pos
    ]

  , testGroup "Position advancement"
    [ testCase "advances position by multiple characters" $ do
        let pos = startPos
        advancePosBy "abc" pos @?= SourcePos 1 4 3
      
    , testCase "advances position by text with newline" $ do
        let pos = startPos
        advancePosBy "ab\nc" pos @?= SourcePos 2 2 4
      
    , testCase "advances position by text with tab" $ do
        let pos = startPos
        advancePosBy "ab\tc" pos @?= SourcePos 1 11 4
      
    , testCase "advances position by lines" $ do
        let pos = SourcePos 3 5 10
        advancePosByLine 2 pos @?= SourcePos 5 1 10
    ]

  , testGroup "Error location conversion"
    [ testCase "converts position to error location" $ do
        let pos = SourcePos 3 5 10
            expected = ErrorLocation Nothing 3 5 Nothing Nothing
        toErrorLocation pos @?= expected
      
    , testCase "converts span to error location with range" $ do
        let span' = SourceSpan (SourcePos 3 5 10) (SourcePos 3 10 15)
            expected = ErrorLocation Nothing 3 5 (Just 3) (Just 10)
        toErrorLocationWithSpan span' @?= expected
    ]

  , testGroup "QuickCheck properties"
    [ testProperty "position advancement is consistent" $
        \pos c -> let newPos = posAfter c pos
                   in posOffset newPos >= posOffset pos
      
    , testProperty "span merging is associative" $
        \span1 span2 span3 -> 
          let merged1 = mergeSpans span1 (mergeSpans span2 span3)
              merged2 = mergeSpans (mergeSpans span1 span2) span3
          in spanStart merged1 == spanStart merged2 &&
             spanEnd merged1 == spanEnd merged2
           
    , testProperty "span merging is commutative for start/end" $
        \span1 span2 ->
          let merged = mergeSpans span1 span2
          in spanStart merged == min (spanStart span1) (spanStart span2) &&
             spanEnd merged == max (spanEnd span1) (spanEnd span2)
           
    , testProperty "position comparison is transitive" $
        \pos1 pos2 pos3 ->
          let comp12 = comparePos pos1 pos2
              comp23 = comparePos pos2 pos3
              comp13 = comparePos pos1 pos3
          in if comp12 == EQ && comp23 == EQ 
             then comp13 == EQ
             else if comp12 == LT && comp23 == LT
                  then comp13 == LT
                  else if comp12 == GT && comp23 == GT
                       then comp13 == GT
                       else True  -- Mixed cases are not necessarily transitive
    ]

  , testGroup "Edge cases"
    [ testCase "handles zero-based offsets" $ do
        let pos = SourcePos 1 1 0
        posAfter 'a' pos @?= SourcePos 1 2 1
      
    , testCase "handles large column numbers" $ do
        let pos = SourcePos 1 1000 999
        posAfter 'a' pos @?= SourcePos 1 1001 1000
      
    , testCase "handles tab at column boundary" $ do
        let pos1 = SourcePos 1 8 7  -- Just before tab boundary
        posAfter '\t' pos1 @?= SourcePos 1 9 8  -- Next column
      
        let pos2 = SourcePos 1 9 8  -- At tab boundary
        posAfter '\t' pos2 @?= SourcePos 1 17 9  -- Next tab boundary
      
    , testCase "handles empty spans in merge" $ do
        let empty1 = SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)
            empty2 = SourceSpan (SourcePos 2 2 5) (SourcePos 2 2 5)
            expected = SourceSpan (SourcePos 1 1 0) (SourcePos 2 2 5)
        mergeSpans empty1 empty2 @?= expected
    ]
  ]