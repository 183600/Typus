{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.CustomSourceLocationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (==>), forAll, elements, listOf, listOf1, oneof, choose)
import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , posAtLineCol
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedSpan
  , locatedPos
  , mapLocated
  , advancePos
  , advancePosBy
  , advancePosByText
  , advancePosByLine
  )

-- | Generate valid source positions
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 100)
  col <- choose (1, 200)
  offset <- choose (0, 10000)
  return $ SourcePos line col offset

-- | Generate valid source spans
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  startLine <- choose (1, 100)
  startCol <- choose (1, 200)
  startOffset <- choose (0, 9000)
  endLine <- choose (startLine, startLine + 10)
  endCol <- if endLine == startLine 
            then choose (startCol, startCol + 50)
            else choose (1, 200)
  endOffset <- choose (startOffset, startOffset + 1000)
  return $ SourceSpan (SourcePos startLine startCol startOffset) 
                      (SourcePos endLine endCol endOffset)

-- | Generate located values
genLocated :: Gen (Located String)
genLocated = do
  value <- listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ']
  span <- genSourceSpan
  return $ Located span value

-- | Generate text with various characters
genText :: Gen String
genText = listOf $ oneof
  [ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t"
  , return '\n'
  ]

-- | Test startPos properties
prop_startPosValid :: Property
prop_startPosValid = 
  posLine startPos == 1 &&
  posColumn startPos == 1 &&
  posOffset startPos == 0

-- | Test posAfter with newline
prop_posAfterNewline :: Property
prop_posAfterNewline = forAll genSourcePos $ \pos ->
  let newPos = posAfter '\n' pos
  in posLine newPos == posLine pos + 1 &&
     posColumn newPos == 1 &&
     posOffset newPos == posOffset pos + 1

-- | Test posAfter with tab
prop_posAfterTab :: Property
prop_posAfterTab = forAll genSourcePos $ \pos ->
  let oldCol = posColumn pos
      expectedCol = ((oldCol - 1) `div` 8 + 1) * 8 + 1
      newPos = posAfter '\t' pos
  in posLine newPos == posLine pos &&
     posColumn newPos == expectedCol &&
     posOffset newPos == posOffset pos + 1

-- | Test posAfter with regular character
prop_posAfterRegularChar :: Property
prop_posAfterRegularChar = forAll genSourcePos $ \pos ->
  let newPos = posAfter 'x' pos
  in posLine newPos == posLine pos &&
     posColumn newPos == posColumn pos + 1 &&
     posOffset newPos == posOffset pos + 1

-- | Test posAt creates correct position
prop_posAtCorrect :: Property
prop_posAtCorrect = forAll (choose (1, 100)) $ \line ->
  forAll (choose (1, 200)) $ \col ->
    let pos = posAt line col
    in posLine pos == line && posColumn pos == col && posOffset pos == 0

-- | Test posAtLineCol creates correct position
prop_posAtLineColCorrect :: Property
prop_posAtLineColCorrect = forAll (choose (1, 100)) $ \line ->
  forAll (choose (1, 200)) $ \col ->
    forAll (choose (0, 10000)) $ \offset ->
      let pos = posAtLineCol line col offset
      in posLine pos == line && posColumn pos == col && posOffset pos == offset

-- | Test emptySpan is valid
prop_emptySpanValid :: Property
prop_emptySpanValid = 
  let span = emptySpan
      start = spanStart span
      end = spanEnd span
  in posLine start == 1 && posColumn start == 1 && posOffset start == 0 &&
     posLine end == 1 && posColumn end == 1 && posOffset end == 0

-- | Test spanFrom creates valid span
prop_spanFromValid :: Property
prop_spanFromValid = forAll genSourcePos $ \pos ->
  let span = spanFrom pos
      start = spanStart span
      end = spanEnd span
  in start == pos && end == pos

-- | Test spanTo creates valid span
prop_spanToValid :: Property
prop_spanToValid = forAll genSourcePos $ \pos ->
  let span = spanTo pos
      start = spanStart span
      end = spanEnd span
  in start == pos && end == pos

-- | Test spanBetween creates correct span
prop_spanBetweenCorrect :: Property
prop_spanBetweenCorrect = forAll genSourcePos $ \start ->
  forAll genSourcePos $ \end ->
    let span = spanBetween start end
        spanStartPos = spanStart span
        spanEndPos = spanEnd span
    in spanStartPos == start && spanEndPos == end

-- | Test mergeSpans property
prop_mergeSpansCorrect :: Property
prop_mergeSpansCorrect = forAll genSourceSpan $ \span1 ->
  forAll genSourceSpan $ \span2 ->
    let merged = mergeSpans span1 span2
        start1 = spanStart span1
        end1 = spanEnd span1
        start2 = spanStart span2
        end2 = spanEnd span2
        mergedStart = spanStart merged
        mergedEnd = spanEnd merged
    in mergedStart <= mergedEnd

-- | Test isValidSpan property
prop_isValidSpanCorrect :: Property
prop_isValidSpanCorrect = forAll genSourceSpan $ \span ->
  let start = spanStart span
      end = spanEnd span
      isValid = isValidSpan span
  in (start <= end) == isValid

-- | Test locatedAt creates correct located value
prop_locatedAtCorrect :: Property
prop_locatedAtCorrect = forAll genSourcePos $ \pos ->
  forAll (listOf1 $ elements $ ['a'..'z']) $ \value ->
    let located = locatedAt pos value
        span = locatedSpan located
        retrievedValue = locatedValue located
    in spanStart span == pos && spanEnd span == pos && retrievedValue == value

-- | Test locatedWithSpan creates correct located value
prop_locatedWithSpanCorrect :: Property
prop_locatedWithSpanCorrect = forAll genSourceSpan $ \span ->
  forAll (listOf1 $ elements $ ['a'..'z']) $ \value ->
    let located = locatedWithSpan span value
        retrievedSpan = locatedSpan located
        retrievedValue = locatedValue located
    in retrievedSpan == span && retrievedValue == value

-- | Test locatedPos returns correct position
prop_locatedPosCorrect :: Property
prop_locatedPosCorrect = forAll genLocated $ \located ->
  let span = locatedSpan located
      pos = locatedPos located
  in pos == spanStart span

-- | Test mapLocated preserves span
prop_mapLocatedPreservesSpan :: Property
prop_mapLocatedPreservesSpan = forAll genLocated $ \located ->
  let originalSpan = locatedSpan located
      mapped = mapLocated (map toUpper) located
      mappedSpan = locatedSpan mapped
  in originalSpan == mappedSpan
  where
    toUpper c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

-- | Test advancePos with newline
prop_advancePosNewline :: Property
prop_advancePosNewline = forAll genSourcePos $ \pos ->
  let newPos = advancePos '\n' pos
  in posLine newPos == posLine pos + 1 &&
     posColumn newPos == 1 &&
     posOffset newPos == posOffset pos + 1

-- | Test advancePosByText with empty text
prop_advancePosByTextEmpty :: Property
prop_advancePosByTextEmpty = forAll genSourcePos $ \pos ->
  advancePosByText "" pos == pos

-- | Test advancePosByText with newline text
prop_advancePosByTextNewlines :: Property
prop_advancePosByTextNewlines = forAll genSourcePos $ \pos ->
  forAll (choose (1, 10)) $ \numLines ->
    let newText = L.concat $ replicate numLines "\n"
        newPos = advancePosByText newText pos
    in posLine newPos == posLine pos + numLines &&
       posColumn newPos == 1 &&
       posOffset newPos == posOffset pos + numLines

-- | Test advancePosByLine
prop_advancePosByLineCorrect :: Property
prop_advancePosByLineCorrect = forAll genSourcePos $ \pos ->
  forAll (choose (1, 10)) $ \numLines ->
    let newPos = advancePosByLine numLines pos
    in posLine newPos == posLine pos + numLines &&
       posColumn newPos == posColumn pos

-- | Test SourcePos ordering
prop_sourcePosOrdering :: Property
prop_sourcePosOrdering = forAll genSourcePos $ \pos1 ->
  forAll genSourcePos $ \pos2 ->
    let cmp = compare pos1 pos2
    in (cmp == LT) == (pos1 < pos2) &&
       (cmp == EQ) == (pos1 == pos2) &&
       (cmp == GT) == (pos1 > pos2)

tests :: TestTree
tests = testGroup "Custom SourceLocation QuickCheck Tests"
  [ testProperty "startPos valid" prop_startPosValid
  , testProperty "posAfter newline" prop_posAfterNewline
  , testProperty "posAfter tab" prop_posAfterTab
  , testProperty "posAfter regular char" prop_posAfterRegularChar
  , testProperty "posAt correct" prop_posAtCorrect
  , testProperty "posAtLineCol correct" prop_posAtLineColCorrect
  , testProperty "emptySpan valid" prop_emptySpanValid
  , testProperty "spanFrom valid" prop_spanFromValid
  , testProperty "spanTo valid" prop_spanToValid
  , testProperty "spanBetween correct" prop_spanBetweenCorrect
  , testProperty "mergeSpans correct" prop_mergeSpansCorrect
  , testProperty "isValidSpan correct" prop_isValidSpanCorrect
  , testProperty "locatedAt correct" prop_locatedAtCorrect
  , testProperty "locatedWithSpan correct" prop_locatedWithSpanCorrect
  , testProperty "locatedPos correct" prop_locatedPosCorrect
  , testProperty "mapLocated preserves span" prop_mapLocatedPreservesSpan
  , testProperty "advancePos newline" prop_advancePosNewline
  , testProperty "advancePosByText empty" prop_advancePosByTextEmpty
  , testProperty "advancePosByText newlines" prop_advancePosByTextNewlines
  , testProperty "advancePosByLine correct" prop_advancePosByLineCorrect
  , testProperty "SourcePos ordering" prop_sourcePosOrdering
  ]