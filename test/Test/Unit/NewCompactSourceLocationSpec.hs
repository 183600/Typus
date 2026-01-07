module Test.Unit.NewCompactSourceLocationSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, choose)
import SourceLocation
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



                                              arbitrary = do
              line <- choose (1, 1000)
    col <- choose (1, 1000)
    return $ SourcePos line col

-- | 
instance Arbitrary SourceSpan where
                                              arbitrary = do
              startLine <- choose (1, 100)
    startCol <- choose (1, 100)
    endLine <- choose (startLine, startLine + 10)  -- 
    endCol <- if                               endLine == startLine 
              then choose (startCol, startCol + 10)  -- 
              else choose (1, 100)
    return $ SourceSpan (SourcePos startLine startCol) (SourcePos endLine endCol)

-- | 
testPositionMathProperties :: TestTree
testPositionMathProperties = testGroup ""
  [             testProperty "posAfter" $
      \pos -> posAfter pos 'x' === pos {                               spColumn = spColumn pos + 1}
    
  ,             testProperty "posAfter" $
      \pos -> posAfter pos '\n' === SourcePos (spLine pos + 1) 1
    
  ,             testProperty "posAtLineCol" $
      \line col -> let pos = posAtLineCol line col
                   in spLine                               pos === line && spColumn                               pos === col
    
  ,             testProperty "advancePosBy" $
      \pos s -> let chars = take 10 s  -- 
                                                  finalPos = foldl posAfter pos chars
                in spLine finalPos >= spLine pos
  ]

-- | 
testSpanProperties :: TestTree
testSpanProperties = testGroup ""
  [             testProperty "spanFrom" $
      \pos -> let span = spanFrom pos
              in spanStart                               span === pos && spanEnd                               span === pos
    
  ,             testProperty "spanTo" $
      \startPos endPos -> 
        let span = spanFrom startPos `spanTo` endPos
        in spanStart                               span === startPos && spanEnd                               span === endPos
    
  ,             testProperty "spanBetween" $
      \pos1 pos2 ->
        let span = spanBetween pos1 pos2
                                          start = spanStart span
                                          end = spanEnd span
        in (start <= pos1 && end >= pos1) || (start <= pos2 && end >= pos2)
    
  ,             testProperty "mergeSpans" $
      \span1 span2 ->
        let merged = mergeSpans span1 span2
                                          start1 = spanStart span1
                                          end1 = spanEnd span1
                                          start2 = spanStart span2
                                          end2 = spanEnd span2
                                          mergedStart = spanStart merged
                                          mergedEnd = spanEnd merged
        in mergedStart <= start1 && mergedEnd >= end1 &&
           mergedStart <= start2 && mergedEnd >= end2
  ]

-- | Located
testLocatedProperties :: TestTree
testLocatedProperties = testGroup "Located"
  [             testProperty "locatedAt" $
      \value pos -> 
        let located = locatedAt pos value
        in locatedPos                               located === pos && locatedValue                               located === value
    
  ,             testProperty "locatedWithSpan" $
      \value span ->
        let located = locatedWithSpan span value
        in locatedSpan                               located === span && locatedValue                               located === value
    
  ,             testProperty "mapLocated" $
      \value pos f ->
        let located = locatedAt pos value
                                          mapped = mapLocated (const f) located
        in locatedPos                               mapped === locatedPos located
  ]

-- | 
testBoundaryConditions :: TestTree
testBoundaryConditions = testGroup ""
  [             testCase "" $
      let pos = startPos
      in spLine pos @?= 1 && spColumn pos @?= 1
    
    ,             testCase "" $
      let span = emptySpan
      in isValidSpan span @?= False
    
    ,             testCase "" $
      let pos = posAtLineCol 1 1
                                        span = spanFrom pos
      in isValidSpan span @?= True
    
    ,             testCase "" $
      \span -> isValidSpan                               span ==> 
        let start = spanStart span
                                          end = spanEnd span
        in (spLine start < spLine end) || 
           (spLine                               start == spLine end && spColumn start <= spColumn end)
  ]

-- | 
testConsistencyProperties :: TestTree
testConsistencyProperties = testGroup ""
  [             testProperty "" $
      \pos c -> 
        let newPos = posAfter pos c
        in spLine newPos > spLine pos || 
           (spLine                               newPos == spLine pos && spColumn newPos >= spColumn pos)
    
  ,             testProperty "" $
      \span -> mergeSpans span                               span === span
  ]

-- | 
tests :: TestTree
tests =   testGroup "SourceLocation"
  [ testPositionMathProperties
  , testSpanProperties
  , testLocatedProperties
  , testBoundaryConditions
  , testConsistencyProperties
  ]