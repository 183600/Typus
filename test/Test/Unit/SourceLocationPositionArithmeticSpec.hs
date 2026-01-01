module Test.Unit.SourceLocationPositionArithmeticSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (==>), forAll, Gen, arbitrary, choose)
import SourceLocation
  ( SourcePos(..)
  , startPos
  , posAfter
  , advancePosBy
  , advancePosByText
  , posAt
  , advancePosByLine
  )

-- | Tests for position arithmetic in SourceLocation module
tests :: TestTree
tests =
  testGroup "SourceLocation Position Arithmetic"
    [ testGroup "QuickCheck property tests"
        [ fastProperty "posAfter increases offset by 1" $
            \c pos -> posOffset (posAfter c pos) == posOffset pos + 1
        
        , fastProperty "posAfter newline resets column to 1" $
            \pos -> posColumn (posAfter '\n' pos) == 1
        
        , fastProperty "posAfter newline increments line by 1" $
            \pos -> posLine (posAfter '\n' pos) == posLine pos + 1
        
        , fastProperty "posAfter tab jumps to next tab stop (8-aligned)" $
            \pos -> let newCol = posColumn (posAfter '\t' pos)
                    in newCol `mod` 8 == 1 || newCol > posColumn pos
        
        , fastProperty "advancePosBy empty string returns same position" $
            \pos -> advancePosBy "" pos == pos
        
        , fastProperty "advancePosBy is consistent with repeated posAfter" $
            \s pos -> advancePosBy s pos == L.foldl (flip posAfter) pos s
        
        , fastProperty "advancePosByLine increases line number" $
            \n pos -> n >= 0 ==> posLine (advancePosByLine n pos) == posLine pos + n
        
        , fastProperty "advancePosByLine resets column to 1" $
            \n pos -> n >= 0 ==> posColumn (advancePosByLine n pos) == 1
        ]
    
    , testGroup "Specific edge case tests"
        [ testCase "posAfter handles basic characters correctly" $ do
            let pos = SourcePos 1 1 0
            posAfter 'a' pos @?= SourcePos 1 2 1
            posAfter 'b' (SourcePos 1 2 1) @?= SourcePos 1 3 2
        
        , testCase "posAfter handles newline correctly" $ do
            let pos = SourcePos 3 5 17
            posAfter '\n' pos @?= SourcePos 4 1 18
        
        , testCase "posAfter handles tab correctly" $ do
            let pos1 = SourcePos 2 5 40
            posAfter '\t' pos1 @?= SourcePos 2 9 41  -- jumps to next tab stop
            
            let pos2 = SourcePos 2 8 50
            posAfter '\t' pos2 @?= SourcePos 2 9 51  -- already at tab stop
            
            let pos3 = SourcePos 2 1 10
            posAfter '\t' pos3 @?= SourcePos 2 9 11  -- jumps to first tab stop
        
        , testCase "advancePosBy handles multiline strings" $ do
            let pos = startPos
                text = "hello\nworld"
            advancePosBy text pos @?= SourcePos 2 6 11
        
        , testCase "advancePosBy handles tabs correctly" $ do
            let pos = startPos
                text = "a\tb"
            advancePosBy text pos @?= SourcePos 1 9 3
        
        , testCase "advancePosByLine handles zero lines" $ do
            let pos = SourcePos 5 10 50
            advancePosByLine 0 pos @?= pos
        
        , testCase "advancePosByLine handles multiple lines" $ do
            let pos = SourcePos 3 5 20
            advancePosByLine 3 pos @?= SourcePos 6 1 23
        
        , testCase "posAt creates correct position" $ do
            posAt 5 10 @?= SourcePos 5 10 0
        
        , testCase "startPos has correct values" $ do
            startPos @?= SourcePos 1 1 0
        ]
    
    , testGroup "Regression L.and boundary tests"
        [ testCase "position arithmetic with large column numbers" $ do
            let pos = SourcePos 1 100 99
            posAfter 'x' pos @?= SourcePos 1 101 100
        
        , testCase "position arithmetic with tab at column 8" $ do
            let pos = SourcePos 1 8 7
            posAfter '\t' pos @?= SourcePos 1 9 8
        
        , testCase "position arithmetic with tab at column 7" $ do
            let pos = SourcePos 1 7 6
            posAfter '\t' pos @?= SourcePos 1 9 7
        
        , testCase "advancePosBy with empty string" $ do
            let pos = SourcePos 10 20 100
            advancePosBy "" pos @?= pos
        
        , testCase "advancePosBy with only newlines" $ do
            let pos = startPos
                text = "\n\n\n"
            advancePosBy text pos @?= SourcePos 4 1 3
        ]
    ]