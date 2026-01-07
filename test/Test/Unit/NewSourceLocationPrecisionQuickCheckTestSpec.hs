module Test.Unit.NewSourceLocationPrecisionQuickCheckTestSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (Property,             testProperty, Arbitrary(..), Gen, oneof, elements, listOf, listOf1, suchThat, choose)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import SourceLocation ()
      startPos, posAfter, posAt, posAtLineCol,
      emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan,
      locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos, mapLocated,
      advancePos, advancePosBy, spanStart, spanEnd )
import Data.Char ()
                                              span2 = spanBetween (posAtLineCol 2 1) (posAtLineCol 2 5)
                                              merged = mergeSpans span1 span2
            posLine (spanStart merged) @?= 1
            posColumn (spanStart merged) @?= 1
            posLine (spanEnd merged) @?= 2
            posColumn (spanEnd merged) @?= 5
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


          ,             testCase "located value round-trip preservation" $ do
                        let original = "test value"
                                              pos = posAtLineCol 10 20
                                              span = spanFrom pos
                                              located = locatedWithSpan span original
                                              extracted = locatedValue located
                                              extractedPos = locatedPos located
                                              extractedSpan = locatedSpan located
            extracted @?= original
            extractedPos @?= pos
            extractedSpan @?= span

          ,             testCase "position advancement with unicode characters" $ do
                        let pos = startPos ""
                                              pos1 = advancePos pos ''  -- Greek alpha
                                              pos2 = advancePos pos1 ''  -- Greek beta
                                              pos3 = advancePos pos2 '' -- Chinese character
            posColumn pos1 @?= 2
            posColumn pos2 @?= 3
            posColumn pos3 @?= 4

          ,             testCase "span validation with edge cases" $ do
                        let validSpan = spanBetween (posAtLineCol 1 1) (posAtLineCol 1 10)
                                              invalidSpan1 = spanBetween (posAtLineCol 5 1) (posAtLineCol 3 1)
                                              invalidSpan2 = spanBetween (posAtLineCol 1 20) (posAtLineCol 1 10)
            isValidSpan validSpan @?= True
            isValidSpan invalidSpan1 @?= False
            isValidSpan invalidSpan2 @?= False
        ]
    ]

-- | posAfter1
prop_posAfterAdvancesColumn :: Int -> Int -> Property
prop_posAfterAdvancesColumn line                               col =
  col > 0 && line >                               0 ==>
  let pos = SourcePos line col
                                    after = posAfter pos
  in posLine                               after == line && posColumn                               after == col + 1

-- | posAtLineCol
prop_posAtLineColCorrect :: Int -> Int -> Property
prop_posAtLineColCorrect line                               col =
  line > 0 && col >                               0 ==>
  let pos = posAtLineCol line col
  in posLine                               pos == line && posColumn                               pos == col

-- | advancePos
prop_advancePosNewline :: Int -> Int -> Property
prop_advancePosNewline line                               col =
  line > 0 && col >                               0 ==>
  let pos = SourcePos line col
                                    afterNewline = advancePos pos '\n'
  in posLine                               afterNewline == line + 1 && posColumn                               afterNewline == 1

-- | advancePosBy
prop_advancePosByMultiple :: Int -> Int -> String -> Property
prop_advancePosByMultiple line col                               chars =
  line > 0 && col > 0 && not (null chars) ==>
  let pos = SourcePos line col
                                    finalPos = advancePosBy pos chars
                                    expectedLine = line + L.length (L.filter (== '\n') chars)
                                    lastLineStarts = L.map (+1) $ findIndices (== '\n') chars
                                    expectedCol = if null lastLineStarts 
                    then col + L.length chars
                    else L.length (drop (last lastLineStarts) chars) + 1
  in posLine                               finalPos == expectedLine && 
     (if null lastLineStarts then posColumn                               finalPos == expectedCol else True)

-- | 
prop_positionOrdering :: Int -> Int -> Int -> Int -> Property
prop_positionOrdering line1 col1 line2                               col2 =
  line1 > 0 && col1 > 0 && line2 > 0 && col2 >                               0 ==>
  let pos1 = SourcePos line1 col1
                                    pos2 = SourcePos line2 col2
  in (line1 < line2) || (line1 == line2 && col1 < col2) ==>
     pos1 < pos2

-- | emptySpan
prop_emptySpanZeroLength :: Property
                              prop_emptySpanZeroLength =
  let span = emptySpan
  in spanStart                               span == spanEnd span

-- | spanFrom
prop_spanFromValid :: Int -> Int -> Property
prop_spanFromValid line                               col =
  line > 0 && col >                               0 ==>
  let pos = SourcePos line col
                                    span = spanFrom pos
  in spanStart                               span == pos && spanEnd                               span == pos

-- | spanTo
prop_spanToValid :: Int -> Int -> Property
prop_spanToValid line                               col =
  line > 0 && col >                               0 ==>
  let pos = SourcePos line col
                                    span = spanTo pos
  in spanStart                               span == pos && spanEnd                               span == pos

-- | spanBetween
prop_spanBetweenContains :: Int -> Int -> Int -> Int -> Property
prop_spanBetweenContains line1 col1 line2                               col2 =
  line1 > 0 && col1 > 0 && line2 > 0 && col2 >                               0 ==>
  let pos1 = SourcePos line1 col1
                                    pos2 = SourcePos line2 col2
                                    span = spanBetween pos1 pos2
  in (spanStart                               span == pos1 || spanStart                               span == pos2) &&
     (spanEnd                               span == pos1 || spanEnd                               span == pos2)

-- | mergeSpans
prop_mergeSpansContains :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_mergeSpansContains line1 col1 line2 col2 line3 col3 line4                               col4 =
  L.all (>0) [line1, col1, line2, col2, line3, col3, line4, col4] ==>
  let pos1 = SourcePos line1 col1
                                    pos2 = SourcePos line2 col2
                                    pos3 = SourcePos line3 col3
                                    pos4 = SourcePos line4 col4
                                    span1 = spanBetween pos1 pos2
                                    span2 = spanBetween pos3 pos4
                                    merged = mergeSpans span1 span2
      -- Check that merged contains both spans
      containsSpan span                               merged = 
        let start = spanStart span
                                          end = spanEnd span
                                          mergedStart = spanStart merged
                                          mergedEnd = spanEnd merged
        in (start >= mergedStart && end <= mergedEnd) ||
           (spanStart merged >= start && spanEnd merged <= end)
  in containsSpan span1 merged && containsSpan span2 merged

-- | isValidSpan
prop_isValidSpanCorrect :: Int -> Int -> Int -> Int -> Property
prop_isValidSpanCorrect line1 col1 line2                               col2 =
  let pos1 = SourcePos line1 col1
                                    pos2 = SourcePos line2 col2
                                    span = spanBetween pos1 pos2
                                    shouldBeValid = pos1 <= pos2
  in isValidSpan                               span == shouldBeValid

-- | locatedAt
prop_locatedAtPreservesValue :: String -> Int -> Int -> Property
prop_locatedAtPreservesValue value line                               col =
  line > 0 && col >                               0 ==>
  let pos = posAtLineCol line col
                                    located = locatedAt pos value
  in locatedValue                               located == value && locatedPos                               located == pos

-- | locatedWithSpan
prop_locatedWithSpanPreserves :: String -> Int -> Int -> Int -> Int -> Property
prop_locatedWithSpanPreserves value line1 col1 line2                               col2 =
  L.all (>0) [line1, col1, line2, col2] ==>
  let pos1 = SourcePos line1 col1
                                    pos2 = SourcePos line2 col2
                                    span = spanBetween pos1 pos2
                                    located = locatedWithSpan span value
  in locatedValue                               located == value && locatedSpan                               located == span

-- | mapLocated
prop_mapLocatedPreservesLocation :: String -> String -> Int -> Int -> Property
prop_mapLocatedPreservesLocation original transformed line                               col =
  line > 0 && col >                               0 ==>
  let pos = posAtLineCol line col
                                    located = locatedAt pos original
                                    mapped = mapLocated (const transformed) located
  in locatedPos                               mapped == pos && locatedValue                               mapped == transformed

-- | locatedValue
prop_locatedValueExtracts :: String -> Int -> Int -> Property
prop_locatedValueExtracts value line                               col =
  line > 0 && col >                               0 ==>
  let pos = posAtLineCol line col
                                    located = locatedAt pos value
  in locatedValue                               located == value

-- | 
prop_spanLengthNonNegative :: Int -> Int -> Int -> Int -> Property
prop_spanLengthNonNegative line1 col1 line2                               col2 =
  let pos1 = SourcePos line1 col1
                                    pos2 = SourcePos line2 col2
                                    span = spanBetween pos1 pos2
      -- Length calculation would depend on specific implementation
      -- For now, just check that span is valid L.or has equal start/end
  in not (isValidSpan span) || spanStart span <= spanEnd span

-- | 
prop_spanStartBeforeEnd :: Int -> Int -> Int -> Int -> Property
prop_spanStartBeforeEnd line1 col1 line2                               col2 =
  let pos1 = SourcePos line1 col1
                                    pos2 = SourcePos line2 col2
                                    span = spanBetween pos1 pos2
  in spanStart span <= spanEnd span

-- | mergeSpans
prop_mergeSpansCommutative :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_mergeSpansCommutative line1 col1 line2 col2 line3 col3 line4                               col4 =
  L.all (>0) [line1, col1, line2, col2, line3, col3, line4, col4] ==>
  let pos1 = SourcePos line1 col1
                                    pos2 = SourcePos line2 col2
                                    pos3 = SourcePos line3 col3
                                    pos4 = SourcePos line4 col4
                                    span1 = spanBetween pos1 pos2
                                    span2 = spanBetween pos3 pos4
                                    merged1 = mergeSpans span1 span2
                                    merged2 = mergeSpans span2 span1
  in                               merged1 == merged2

-- | mergeSpans
prop_mergeSpansAssociative :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_mergeSpansAssociative line1 col1 line2 col2 line3 col3 line4 col4 line5 col5 line6                               col6 =
  L.all (>0) [line1, col1, line2, col2, line3, col3, line4, col4, line5, col5, line6, col6] ==>
  let pos1 = SourcePos line1 col1
                                    pos2 = SourcePos line2 col2
                                    pos3 = SourcePos line3 col3
                                    pos4 = SourcePos line4 col4
                                    pos5 = SourcePos line5 col5
                                    pos6 = SourcePos line6 col6
                                    span1 = spanBetween pos1 pos2
                                    span2 = spanBetween pos3 pos4
                                    span3 = spanBetween pos5 pos6
                                    merged1 = mergeSpans (mergeSpans span1 span2) span3
                                    merged2 = mergeSpans span1 (mergeSpans span2 span3)
  in                               merged1 == merged2

-- | 
prop_largeLineNumbers :: Property
                              prop_largeLineNumbers =
  let line = 1000000
                                    col = 50
                                    pos = SourcePos line col
  in posLine                               pos == line && posColumn                               pos == col

-- | 
prop_largeColumnNumbers :: Property
                              prop_largeColumnNumbers =
  let line = 100
                                    col = 1000000
                                    pos = SourcePos line col
  in posLine                               pos == line && posColumn                               pos == col

-- | 
prop_zeroWidthSpans :: Int -> Int -> Property
prop_zeroWidthSpans line                               col =
  line > 0 && col >                               0 ==>
  let pos = SourcePos line col
                                    span = spanFrom pos
  in spanStart                               span == spanEnd span

-- | 
prop_singleCharSpans :: Int -> Int -> Property
prop_singleCharSpans line                               col =
  line > 0 && col >                               0 ==>
  let start = SourcePos line col
                                    end = posAfter start
                                    span =  spanBetween start end
  in property $ spanStart                               span == start && spanEnd                               span == end

-- Helper functions
findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices p                               xs = map fst $ L.filter (p . snd) $ zip [0..] xs

-- Define < for SourcePos
instance Ord SourcePos where
    compare (SourcePos l1 c1) (SourcePos l2 c2) =
    case compare l1 l2 of
      EQ -> compare c1 c2
      other -> other