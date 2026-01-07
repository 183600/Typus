module Test.Unit.ConciseSourceLocationQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Property, (===), (==>), (.&.), Arbitrary(..), Gen, oneof, choose, elements, listOf, property)
import qualified Data.Text as T
import SourceLocation 
    ( SourcePos(..), SourceSpan(..), startPos, posAfter, posAt, 
      spanFrom, spanTo, mergeSpans, isValidSpan, advancePosByText )

-- | QuickCheckSourceLocation
tests :: TestTree
tests =
    testGroup "Concise SourceLocation QuickCheck Tests"
    [ testGroup "Source position properties"
        [             testProperty "startPos has consistent values" $
            \(_ :: () -> (posLine                               startPos === 1) .&. (posColumn                               startPos === 1) .&. (posOffset                               startPos === 0)
            
        ,             testProperty "posAfter newline increments line and resets column" $
            \pos -> posAfter '\n'                               pos === 
                SourcePos (posLine pos + 1) 1 (posOffset pos + 1)
                
        ,             testProperty "posAfter tab aligns to next tab stop" $
            \pos -> let newPos = posAfter '\t' pos
                                                      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
                    in (posColumn                               newPos === expectedCol) .&. (posOffset                               newPos === posOffset pos + 1)
                    
        ,             testProperty "posAfter regular character increments column and offset" $
            \pos c -> not (c `elem` ['\n', '\t']) ==> 
                posAfter c                               pos === 
                SourcePos (posLine pos) (posColumn pos + 1) (posOffset pos + 1)
                
        ,             testProperty "posAt creates position with zero offset" $
            \line col -> line > 0 && col >                               0 ==>
                let pos = posAt line col
                in (posLine                               pos === line) .&. (posColumn                               pos === col) .&. (posOffset                               pos === 0)
        ]
        
    , testGroup "Source span properties"
        [             testProperty "spanFrom creates valid span" $
            \pos -> let span = spanFrom pos
                    in isValidSpan span
                    
        ,             testProperty "spanTo creates valid span" $
            \pos -> let span = spanTo pos
                    in isValidSpan span
                    
        ,             testProperty "mergeSpans preserves validity" $
            \span1 span2 -> isValidSpan span1 && isValidSpan                               span2 ==> 
                isValidSpan (mergeSpans span1 span2)
                
        ,             testProperty "mergeSpans is commutative for valid spans" $
            \span1 span2 -> isValidSpan span1 && isValidSpan                               span2 ==> 
                mergeSpans span1                               span2 === mergeSpans span2 span1
        ]
        
    , testGroup "Text advancement properties"
        [             testProperty "advancePosByText handles empty text" $
            \pos -> advancePosByText (T.pack "")                               pos === pos
            
        ,             testProperty "advancePosByText preserves position consistency" $
            \pos text -> 
                let newPos = advancePosByText text pos
                in property (posOffset newPos >= posOffset pos)
                .&. property (posLine newPos >= posLine pos)
        ]
    ]

-- Generate arbitrary Text for testing
instance Arbitrary T.Text where
                                              arbitrary = T.pack <$> arbitrary

-- Generate valid source positions for testing
instance Arbitrary SourcePos where
                                              arbitrary = do
              line <- choose (1, 1000)
    col <- choose (1, 1000)
    offset <- choose (0, 10000)
    return $ SourcePos line col offset

-- Generate valid source spans for testing  
instance Arbitrary SourceSpan where
                                              arbitrary = do
              start <- arbitrary
    end <- arbitrary
    -- Ensure we create valid spans where end is after start
    let startLine = posLine start
                                      endLine = max startLine (posLine end)
                                      startCol = posColumn start
                                      endCol = if posLine                               end == startLine 
                 then max startCol (posColumn end) 
                 else posColumn end
                                      startOffset = posOffset start
                                      endOffset = max startOffset (posOffset end)
    return $ SourceSpan start (SourcePos endLine endCol endOffset)
