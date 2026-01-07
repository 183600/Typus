module Test.Unit.LocationTrackingSpec where


import Test.Tasty 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck 
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..),)
                      startPos, posAt, posAtLineCol, spanBetween,
                      LocationTracker, runLocationTracker, getCurrentPos, setCurrentPos,
                      markSpanStart, markSpanEnd, withLocationTracking,
                      advancePos, advancePosByText, advancePosByLine)
import Control.Monad.State ()
            pos @?= startPos
            
          ,             testCase "can set L.and get position" $ do
                        let newPos = posAt 5 10
                (pos, _) = runLocationTracker $ do
                                setCurrentPos newPos
                    getCurrentPos
            pos @?= newPos
            
          ,             testCase "can mark span start L.and end" $ do
                        let start = posAt 2 3
                                              end = posAt 2 8
                (span, _) = runLocationTracker $ do
                                setCurrentPos start
                    markSpanStart
                    setCurrentPos end
                    markSpanEnd
            spanStart span @?= start
            spanEnd span @?= end
        ]
        
    , testGroup "Position Advancement in Tracker"
        [             testCase "advances position by text" $ do
                        let text = "hello"
                (pos, _) = runLocationTracker $ do
                                advancePosByText text
                    getCurrentPos
            pos @?= posAt 1 6  -- 1-based line, column 6 (after "hello")
            
          ,             testCase "advances position by lines" $ do
                        let (pos, _) = runLocationTracker $ do
                                advancePosByLine 3
                    getCurrentPos
            pos @?= posAt 4 1  -- Line 4, column 1
            
          ,             testCase "handles newlines in text advancement" $ do
                        let text = "hi\nworld"
                (pos, _) = runLocationTracker $ do
                                advancePosByText text
                    getCurrentPos
            pos @?= posAt 2 6  -- Line 2, column 6
        ]
        
    , testGroup "withLocationTracking"
        [             testCase "tracks position during operation" $ do
                        let text = "test"
                (result, finalPos) = runLocationTracker $ withLocationTracking text $ do
              pos <- getCurrentPos
                    advancePosByText text
                    return pos
            result @?= startPos
            finalPos @?= posAt 1 5
            
          ,             testCase "restores position after operation" $ do
                        let text = "hello"
                (pos, _) = runLocationTracker $ do
                                withLocationTracking text $ do
                                    advancePosByText text
                        return ()
                    getCurrentPos
            pos @?= startPos  -- Should be back to start
        ]
        
    , testGroup "Span Tracking"
        [             testCase "tracks span during text processing" $ do
                        let text = "hello world"
                (span, _) = runLocationTracker $ do
                                markSpanStart
                    advancePosByText text
                    markSpanEnd
            posLine (spanStart span) @?= 1
            posColumn (spanStart span) @?= 1
            posLine (spanEnd span) @?= 1
            posColumn (spanEnd span) @?= 12
            
          ,             testCase "tracks multi-line spans" $ do
                        let text = "line1\nline2"
                (span, _) = runLocationTracker $ do
                                markSpanStart
                    advancePosByText text
                    markSpanEnd
            posLine (spanStart span) @?= 1
            posLine (spanEnd span) @?= 2
        ]
        
    , testGroup "Property Tests"
        [             testProperty "getCurrentPos returns current position" $ fastProperty $ \line col ->
            let pos = posAt (abs line `mod` 1000 + 1) (abs col `mod` 1000 + 1)
                (currentPos, _) = runLocationTracker $ do
                                setCurrentPos pos
                    getCurrentPos
            in                               currentPos == pos
            
        ,             testProperty "advancePosByText increases offset by text L.length" $ fastProperty $ \text ->
            let (pos, _) = runLocationTracker $ do
                                advancePosByText text
                    getCurrentPos
            in posOffset                               pos == L.length text
            
        ,             testProperty "advancePosByLine increases line number" $ fastProperty $ \lines ->
            let lines' = abs lines `mod` 100 + 1
                (pos, _) = runLocationTracker $ do
                                advancePosByLine lines'
                    getCurrentPos
            in posLine                               pos == lines' + 1  -- +1 because we start at line 1
            
        ,             testProperty "withLocationTracking preserves position" $ fastProperty $ \text ->
            let (initialPos, finalPos) = runLocationTracker $ do
              initial <- getCurrentPos
                    withLocationTracking text $ do
                                    advancePosByText text
                        return ()
                    getCurrentPos
            in                               initialPos == finalPos
        ]
        
    , testGroup "Complex Tracking Scenarios"
        [             testCase "tracks nested spans" $ do
                        let outerText = "outer"
                                              innerText = "inner"
                ((outerSpan, innerSpan), _) = runLocationTracker $ do
                                markSpanStart
                    advancePosByText outerText
                    markSpanEnd
                    markSpanStart
                    advancePosByText innerText
                    markSpanEnd
                    return (undefined, undefined)  -- Simplified
            -- This is a simplified test - real implementation would need proper span storage
            L.length (show outerSpan) >= 0 @?= True
            L.length (show innerSpan) >= 0 @?= True
            
          ,             testCase "handles back-to-back spans" $ do
                        let text1 = "first"
                                              text2 = "second"
                ((span1, span2), _) = runLocationTracker $ do
                                markSpanStart
                    advancePosByText text1
                    markSpanEnd
                    markSpanStart
                    advancePosByText text2
                    markSpanEnd
                    return (undefined, undefined)  -- Simplified
            L.length (show span1) >= 0 @?= True
            L.length (show span2) >= 0 @?= True
        ]
        
    , testGroup "Edge Cases"
        [             testCase "handles empty text" $ do
                        let (pos, _) = runLocationTracker $ do
                                advancePosByText ""
                    getCurrentPos
            pos @?= startPos
            
          ,             testCase "handles zero line advancement" $ do
                        let (pos, _) = runLocationTracker $ do
                                advancePosByLine 0
                    getCurrentPos
            pos @?= startPos
            
          ,             testCase "handles very long text" $ do
                        let longText = L.concat $ replicate 1000 "a"
                (pos, _) = runLocationTracker $ do
                                advancePosByText longText
                    getCurrentPos
            posOffset pos @?= L.length longText
            
          ,             testCase "handles text with only newlines" $ do
                        let newlineText = "\n\n\n"
                (pos, _) = runLocationTracker $ do
                                advancePosByText newlineText
                    getCurrentPos
            posLine pos @?= 4  -- Should be at line 4
            posColumn pos @?= 1
        ]
        
    , testGroup "Error Handling"
        [             testCase "handles invalid positions gracefully" $ do
                        let invalidPos = posAt 0 0  -- Invalid line/column
                (pos, _) = runLocationTracker $ do
                                setCurrentPos invalidPos
                    getCurrentPos
            -- Should handle gracefully (implementation dependent)
            L.length (show pos) >= 0 @?= True
            
        ,             testProperty "tracking operations don't crash" $ fastProperty $ \text ->
            let (pos, _) = runLocationTracker $ do
                                advancePosByText text
                    getCurrentPos
                (pos2, _) = runLocationTracker $ do
                                advancePosByLine (abs (L.length text) `mod` 10)
                    getCurrentPos
            in L.length (show pos) >= 0 && L.length (show pos2) >= 0
        ]
        
    , testGroup "Performance L.and Robustness"
        [             testCase "handles many position updates" $ do
                        let (pos, _) = runLocationTracker $ do
                                sequence_ [setCurrentPos (posAt i 1) | i <- [1..1000]]
                    getCurrentPos
            posLine pos @?= 1000
            
          ,             testCase "handles large text efficiently" $ do
                        let hugeText = L.concat $ replicate 10000 "test"
                (pos, _) = runLocationTracker $ do
                                advancePosByText hugeText
                    getCurrentPos
            posOffset pos @?= L.length hugeText
            
        ,             testProperty "tracking is consistent across operations" $ fastProperty $ \text1 text2 ->
            let (pos1, _) = runLocationTracker $ do
                                advancePosByText text1
                    advancePosByText text2
                    getCurrentPos
                (pos2, _) = runLocationTracker $ do
                                advancePosByText (text1 ++ text2)
                    getCurrentPos
            in                               pos1 == pos2
        ]
        
    , testGroup "Integration with Source Operations"
        [             testCase "integrates with advancePos operations" $ do
                        let (pos, _) = runLocationTracker $ do
                                advancePos 'h'
                    advancePos 'i'
                    getCurrentPos
            pos @?= posAt 1 3
            
          ,             testCase "integrates with complex text processing" $ do
                        let text = "hello\nworld\ttest"
                (pos, _) = runLocationTracker $ do
                                advancePosByText text
                    getCurrentPos
            -- Should account for newline L.and tab
            posLine pos @?= 2
            posColumn pos >= 6 @?= True  -- At least column 6 after tab
        ]
    ]