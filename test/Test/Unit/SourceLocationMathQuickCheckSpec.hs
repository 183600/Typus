module Test.Unit.SourceLocationMathQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf1, suchThat, oneof, elements)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, posAt, spanBetween, mergeSpans, isValidSpan, advancePosBy)

-- | Generate arbitrary source positions with reasonable constraints
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    column <- choose (1, 200)
    offset <- choose (0, 100000)
    return $ SourcePos line column offset

-- | Generate arbitrary source spans
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endOffset <- choose (0, 100)
    let end = start { posOffset = posOffset start + endOffset, 
                     posColumn = posColumn start + endOffset }
    return $ SourceSpan start end

-- | Generate valid spans where start <= end
validSpan :: Gen SourceSpan
validSpan = do
  startLine <- choose (1, 100)
  startCol <- choose (1, 50)
  endLine <- choose (startLine, startLine + 10)
  endCol <- if endLine == startLine 
            then choose (startCol, startCol + 50)
            else choose (1, 100)
  let start = SourcePos startLine startCol (startLine * 100 + startCol)
      end = SourcePos endLine endCol (endLine * 100 + endCol)
  return $ SourceSpan start end

-- | Generate text strings for position advancement
genText :: Gen String
genText = listOf1 $ elements ['a'..'z', 'A'..'Z', '0'..'9', ' ', '\t', '\n']

tests :: TestTree
tests =
  testGroup "SourceLocation mathematical properties QuickCheck tests"
    [ testGroup "SourcePos properties"
        [ fastProperty "posAfter newline increments line and resets column" $
            \column -> 
              let pos = startPos { posColumn = column }
                  newPos = posAfter '\n' pos
              in posLine newPos == posLine pos + 1 && posColumn newPos == 1

        , fastProperty "posAfter tab advances to next tab stop (8 columns)" $
            \column ->
              let pos = startPos { posColumn = column }
                  newPos = posAfter '\t' pos
                  expectedCol = ((column - 1) `div` 8 + 1) * 8 + 1
              in posColumn newPos == expectedCol

        , fastProperty "posAfter regular char increments column" $
            \column ch ->
              ch `notElem` ['\n', '\t'] ==>
                let pos = startPos { posColumn = column }
                    newPos = posAfter ch pos
                in posColumn newPos == column + 1

        , fastProperty "advancePosBy preserves character count in offset" $
            \text ->
              let pos = startPos
                  newPos = advancePosBy text pos
                  expectedOffset = length text
              in posOffset newPos == expectedOffset
        ]

    , testGroup "SourceSpan properties"
        [ fastProperty "spanBetween creates valid span" $
            \start end ->
              let span = spanBetween start end
              in spanStart span == start && spanEnd span == end

        , fastProperty "mergeSpans is commutative" $
            \span1 span2 ->
              let merged1 = mergeSpans span1 span2
                  merged2 = mergeSpans span2 span1
              in merged1 == merged2

        , fastProperty "mergeSpans is associative" $
            \span1 span2 span3 ->
              let merged12 = mergeSpans span1 span2
                  merged123 = mergeSpans merged12 span3
                  merged23 = mergeSpans span2 span3
                  merged123' = mergeSpans span1 merged23
              in merged123 == merged123'

        , fastProperty "mergeSpans contains both original spans" $
            \span1 span2 ->
              let merged = mergeSpans span1 span2
              in spanStart merged <= spanStart span1 && 
                 spanEnd merged >= spanEnd span1 &&
                 spanStart merged <= spanStart span2 && 
                 spanEnd merged >= spanEnd span2

        , testCase "isValidSpan correctly identifies valid spans" $ do
            let validSpan1 = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
                validSpan2 = SourceSpan (SourcePos 1 1 0) (SourcePos 2 1 10)
                invalidSpan = SourceSpan (SourcePos 2 1 10) (SourcePos 1 1 0)
            isValidSpan validSpan1 @?= True
            isValidSpan validSpan2 @?= True
            isValidSpan invalidSpan @?= False

        , fastProperty "merged valid spans are always valid" $
            \span1 span2 ->
              isValidSpan span1 && isValidSpan span2 ==>
                let merged = mergeSpans span1 span2
                in isValidSpan merged
        ]

    , testGroup "Edge cases and boundary conditions"
        [ testCase "startPos has correct initial values" $ do
            posLine startPos @?= 1
            posColumn startPos @?= 1
            posOffset startPos @?= 0

        , fastProperty "spanBetween same positions creates zero-length span" $
            \pos ->
              let span = spanBetween pos pos
              in spanStart span == pos && spanEnd span == pos

        , fastProperty "mergeSpans with identical spans returns same span" $
            \span ->
              mergeSpans span span == span

        , testCase "position advancement handles multiline text correctly" $ do
            let text = "hello\nworld\ttest"
                pos = advancePosBy text startPos
                -- After "hello\nworld\ttest": 
                -- line 2 (due to \n), column after "world\ttest" = 6 + (8-6+1) + 4 = 14
                -- offset = length of text = 16
            posLine pos @?= 2
            posColumn pos @?= 14
            posOffset pos @?= 16
        ]
    ]