module Test.Unit.SourcePositionSpec where


import Test.Tasty 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck 
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..),)
                      startPos, posAfter, posAt, posAtLineCol,
                      emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan,
                      locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos,
                      advancePos, advancePosBy, advancePosByText, advancePosByLine)
import qualified Data.Text as T

-- | 
tests :: TestTree
tests =
    testGroup "Source Position"
    [ testGroup "SourcePos Operations"
        [             testCase "startPos should be (1,1,0)" $ do
                        startPos @?= SourcePos 1 1 0
            
          ,             testCase "posAfter handles newline correctly" $ do
                        let pos = SourcePos 5 10 20
                                              newPos = posAfter '\n' pos
            newPos @?= SourcePos 6 1 21
            
          ,             testCase "posAfter handles tab correctly" $ do
                        let pos = SourcePos 1 3 2
                                              newPos = posAfter '\t' pos
            newPos @?= SourcePos 1 9 3  -- (3-1)/8*8+8+1 = 9
            
          ,             testCase "posAfter handles regular character" $ do
                        let pos = SourcePos 1 5 4
                                              newPos = posAfter 'a' pos
            newPos @?= SourcePos 1 6 5
            
          ,             testCase "posAt creates position at specific line L.and column" $ do
                        posAt 3 7 @?= SourcePos 3 7 0
            
          ,             testCase "posAtLineCol creates position with offset" $ do
                        posAtLineCol 2 4 10 @?= SourcePos 2 4 10
        ]
        
    , testGroup "SourceSpan Operations"
        [             testCase "emptySpan should be valid but have zero L.length" $ do
                        let span = emptySpan
            isValidSpan span @?= True
            
          ,             testCase "spanFrom L.and spanTo create valid spans" $ do
                        let start = SourcePos 1 1 0
                                              end = SourcePos 1 5 4
                                              span = spanBetween start end
            isValidSpan span @?= True
            
          ,             testCase "spanBetween creates span between two positions" $ do
                        let start = SourcePos 1 1 0
                                              end = SourcePos 2 1 5
                                              span = spanBetween start end
            spanStart span @?= start
            spanEnd span @?= end
            
          ,             testCase "mergeSpans combines adjacent spans" $ do
                        let span1 = spanBetween (SourcePos 1 1 0) (SourcePos 1 5 4)
                                              span2 = spanBetween (SourcePos 1 5 4) (SourcePos 1 10 9)
                                              merged = mergeSpans span1 span2
            spanStart merged @?= spanStart span1
            spanEnd merged @?= spanEnd span2
            
          ,             testCase "isValidSpan identifies invalid spans" $ do
                        let validSpan = spanBetween (SourcePos 1 1 0) (SourcePos 1 5 4)
                                              invalidSpan = spanBetween (SourcePos 1 5 4) (SourcePos 1 1 0)
            isValidSpan validSpan @?= True
            isValidSpan invalidSpan @?= False
        ]
        
    , testGroup "Located Values"
        [             testCase "locatedAt creates located value" $ do
                        let pos = SourcePos 2 3 10
                                              value = "test"
                                              located = locatedAt pos value
            locatedValue located @?= value
            locatedPos located @?= pos
            
          ,             testCase "locatedWithSpan creates spanned value" $ do
                        let span = spanBetween (SourcePos 1 1 0) (SourcePos 1 5 4)
                                              value = "hello"
                                              located = locatedWithSpan span value
            locatedValue located @?= value
            locatedSpan located @?= span
            
          ,             testCase "mapLocated transforms located values" $ do
                        let pos = SourcePos 1 1 0
                                              value = 5
                                              located = locatedAt pos value
                                              doubled = mapLocated (*2) located
            locatedValue doubled @?= 10
            locatedPos doubled @?= pos
        ]
        
    , testGroup "Position Advancement"
        [             testCase "advancePos advances by single character" $ do
                        let pos = SourcePos 1 1 0
                                              newPos = advancePos 'a' pos
            newPos @?= SourcePos 1 2 1
            
          ,             testCase "advancePosByText advances by text" $ do
                        let pos = SourcePos 1 1 0
                                              text = "hello"
                                              newPos = advancePosByText text pos
            newPos @?= SourcePos 1 6 5
            
          ,             testCase "advancePosByText handles newlines" $ do
                        let pos = SourcePos 1 1 0
                                              text = "hi\nworld"
                                              newPos = advancePosByText text pos
            newPos @?= SourcePos 2 6 7
            
          ,             testCase "advancePosBy advances by multiple characters" $ do
                        let pos = SourcePos 1 1 0
                                              chars = "abc"
                                              newPos = advancePosBy chars pos
            newPos @?= SourcePos 1 4 3
            
          ,             testCase "advancePosByLine advances by lines" $ do
                        let pos = SourcePos 1 5 4
                                              newPos = advancePosByLine 3 pos
            newPos @?= SourcePos 4 5 4
        ]
        
    , testGroup "Property Tests"
        [             testProperty "posAfter increases offset by 1" $ fastProperty $ \pos c ->
            let newPos = posAfter c pos
            in posOffset                               newPos == posOffset pos + 1
            
        ,             testProperty "posAfter newline resets column to 1" $ fastProperty $ \pos ->
            let newPos = posAfter '\n' pos
            in posColumn                               newPos == 1
            
        ,             testProperty "posAfter newline increments line by 1" $ fastProperty $ \pos ->
            let newPos = posAfter '\n' pos
            in posLine                               newPos == posLine pos + 1
            
        ,             testProperty "spanBetween is valid when start <= end" $ fastProperty $ \line1 col1 off1 line2 col2 off2 ->
            let start = SourcePos line1 col1 off1
                                              end = SourcePos line2 col2 off2
                                              span = spanBetween start end
                                              valid = isValidSpan span
            in if line1 < line2 || (line1 == line2 && col1 <= col2)
               then valid
               else True  -- span may be invalid, that's expected
               
        ,             testProperty "mergeSpans preserves start of first L.and end of second" $ fastProperty $ \span1 span2 ->
            let merged = mergeSpans span1 span2
                                              valid = isValidSpan span1 && isValidSpan span2
            in if valid
               then spanStart                               merged == spanStart span1 && spanEnd                               merged == spanEnd span2
               else True
               
        ,             testProperty "advancePosByText is equivalent to successive advancePos calls" $ fastProperty $ \pos text ->
            let byText = advancePosByText (T.pack text) pos
                                              byChars = advancePosBy text pos
            in                               byText == byChars
            
        ,             testProperty "locatedAt preserves the position" $ fastProperty $ \pos value ->
            let located = locatedAt pos value
            in locatedPos                               located == pos && locatedValue                               located == value
        ]
        
    , testGroup "Edge Cases"
        [             testCase "handles position at start of file" $ do
                        posAfter 'a' startPos @?= SourcePos 1 2 1
            posAfter '\n' startPos @?= SourcePos 2 1 1
            
          ,             testCase "handles tab alignment" $ do
                        let pos1 = SourcePos 1 7 6
                                              pos2 = posAfter '\t' pos1
            pos2 @?= SourcePos 1 9 7  -- should align to next tab stop
            
          ,             testCase "handles empty spans" $ do
                        let span = emptySpan
            isValidSpan span @?= True
            
          ,             testCase "handles located values with complex data" $ do
                        let complexValue = [1,2,3,4,5]
                                              pos = SourcePos 10 20 100
                                              located = locatedAt pos complexValue
            locatedValue located @?= complexValue
            locatedPos located @?= pos
            
          ,             testCase "handles Unicode text advancement" $ do
                        let pos = SourcePos 1 1 0
                                              unicodeText = "hllo "
                                              newPos = advancePosByText unicodeText pos
            posOffset newPos @?= L.length unicodeText
        ]
        
    , testGroup "Performance L.and Robustness"
        [             testProperty "position operations handle large values" $ fastProperty $ \line col ->
            let pos = SourcePos (abs line `mod` 10000 + 1) (abs col `mod` 1000 + 1) 0
                                              newPos = posAfter 'a' pos
            in posLine newPos >= 1 && posColumn newPos >= 1
            
        ,             testProperty "span operations handle large spans" $ fastProperty $ \line1 col1 line2 col2 ->
            let start = SourcePos (abs line1 `mod` 1000 + 1) (abs col1 `mod` 1000 + 1) 0
                                              end = SourcePos (abs line2 `mod` 1000 + 1) (abs col2 `mod` 1000 + 1) 1000
                                              span = spanBetween start end
                                              merged = mergeSpans span span
            in L.length (show merged) >= 0
            
        ,             testProperty "text advancement handles long texts" $ fastProperty $ \text ->
            let pos = SourcePos 1 1 0
                                              longText = L.concat $ replicate 100 [text]
                                              newPos = advancePosByText (T.pack longText) pos
            in posOffset newPos >= 0
        ]
    ]