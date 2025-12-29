{-# LANGUAGE CPP #-}

module Test.Unit.NewSourceLocationQuickCheckPropertiesSpec (tests) where

import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.QuickCheck (Property, (===), forAll, Gen, choose, listOf1, elements)

import TestSupport.QuickCheck (fastProperty)

import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , advancePosBy
  , advancePosByLine
  , advancePosByText
  , emptySpan
  , locatedAt
  , locatedPos
  , locatedSpan
  , locatedValue
  , mapLocated
  , mergeSpans
  , posAfter
  , spanBetween
  , spanFrom
  , spanTo
  , startPos
  , isValidSpan
  , spanStart
  , spanEnd
  )

-- QuickCheck generators
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  offset <- choose (0, 10000)
  return $ SourcePos line col offset

genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  startLine <- choose (1, 100)
  startCol <- choose (1, 100)
  startOffset <- choose (0, 5000)
  let start = SourcePos startLine startCol startOffset
  
  -- Ensure end is after start
  endLineExtra <- choose (0, 10)
  endColExtra <- choose (0, 50)
  endOffsetExtra <- choose (0, 1000)
  
  let endLine = if endLineExtra == 0 && endColExtra == 0 then startLine else startLine + endLineExtra
      endCol = if endLineExtra == 0 then startCol + endColExtra else endColExtra
      endOffset = startOffset + endOffsetExtra
      end = SourcePos endLine endCol endOffset
      
  return $ SourceSpan start end

genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r"

genString :: Gen String
genString = listOf1 genChar

-- | QuickCheck property tests for SourceLocation module
tests :: TestTree
tests =
  testGroup "NewSourceLocation QuickCheck Properties"
    [ testGroup "Position properties"
        [ fastProperty "posAfter newline increments line and resets column" $
            forAll genSourcePos $ \pos ->
              let newPos = posAfter '\n' pos
              in posLine newPos === posLine pos + 1 &&
                 posColumn newPos === 1 &&
                 posOffset newPos === posOffset pos + 1

        , fastProperty "posAfter regular character increments column" $
            forAll genSourcePos $ \pos ->
              forAll (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ") $ \c ->
                let newPos = posAfter c pos
                in posLine newPos === posLine pos &&
                   posColumn newPos === posColumn pos + 1 &&
                   posOffset newPos === posOffset pos + 1

        , fastProperty "posAfter tab jumps to next tab stop (8-column aligned)" $
            forAll genSourcePos $ \pos ->
              let newPos = posAfter '\t' pos
                  expectedCol = ((posColumn pos + 7) `div` 8) * 8 + 1
              in posLine newPos === posLine pos &&
                 posColumn newPos === expectedCol &&
                 posOffset newPos === posOffset pos + 1

        , fastProperty "advancePosByText correctly tracks position" $
            forAll genString $ \str ->
              let finalPos = advancePosByText (T.pack str) startPos
                  expectedOffset = length str
              in posOffset finalPos === expectedOffset

        , fastProperty "advancePosByLine preserves offset, increments line, resets column" $
            forAll genSourcePos $ \pos ->
              forAll (choose (1, 100)) $ \lines ->
                let newPos = advancePosByLine lines pos
                in posLine newPos === posLine pos + lines &&
                   posColumn newPos === 1 &&
                   posOffset newPos === posOffset pos
        ]

    , testGroup "Span properties"
        [ fastProperty "spanFrom creates valid span" $
            forAll genSourcePos $ \pos ->
              let span = spanFrom pos
              in spanStart span === pos && spanEnd span === pos && isValidSpan span

        , fastProperty "spanTo creates valid span" $
            forAll genSourcePos $ \pos ->
              let span = spanTo pos
              in spanStart span === pos && spanEnd span === pos && isValidSpan span

        , fastProperty "spanBetween preserves bounds" $
            forAll genSourcePos $ \start ->
              forAll genSourcePos $ \end ->
                let span = spanBetween start end
                in spanStart span === start && spanEnd span === end &&
                   (if posLine start < posLine end || 
                       (posLine start == posLine end && posColumn start <= posColumn end)
                    then isValidSpan span 
                    else not (isValidSpan span))

        , fastProperty "mergeSpans creates span with earliest start and latest end" $
            forAll genSourceSpan $ \span1 ->
              forAll genSourceSpan $ \span2 ->
                let merged = mergeSpans span1 span2
                    earliestStart = if posLine (spanStart span1) < posLine (spanStart span2) ||
                                       (posLine (spanStart span1) == posLine (spanStart span2) && 
                                        posColumn (spanStart span1) <= posColumn (spanStart span2))
                                    then spanStart span1 else spanStart span2
                    latestEnd = if posLine (spanEnd span1) > posLine (spanEnd span2) ||
                                   (posLine (spanEnd span1) == posLine (spanEnd span2) && 
                                    posColumn (spanEnd span1) >= posColumn (spanEnd span2))
                                then spanEnd span1 else spanEnd span2
                in spanStart merged === earliestStart && spanEnd merged === latestEnd

        , fastProperty "mergeSpans is associative" $
            forAll genSourceSpan $ \span1 ->
              forAll genSourceSpan $ \span2 ->
                forAll genSourceSpan $ \span3 ->
                  let merged12 = mergeSpans span1 span2
                      merged23 = mergeSpans span2 span3
                      final1 = mergeSpans merged12 span3
                      final2 = mergeSpans span1 merged23
                  in final1 === final2
        ]

    , testGroup "Located properties"
        [ fastProperty "locatedAt creates correct location" $
            forAll genSourcePos $ \pos ->
              forAll genString $ \value ->
                let loc = locatedAt pos value
                in locatedPos loc === pos &&
                   locatedSpan loc === emptySpan pos &&
                   locatedValue loc === value

        , fastProperty "mapLocated preserves span" $
            forAll genSourcePos $ \pos ->
              forAll genString $ \value ->
                let loc = locatedAt pos value
                    mapped = mapLocated length loc
                in locatedSpan mapped === locatedSpan loc &&
                   locatedValue mapped === length value

        , fastProperty "mapLocated is function composition" $
            forAll genSourcePos $ \pos ->
              forAll genString $ \value ->
                let loc = locatedAt pos value
                    mapped1 = mapLocated length loc
                    mapped2 = mapLocated (*2) mapped1
                    mapped3 = mapLocated ((*2) . length) loc
                in locatedValue mapped2 === locatedValue mapped3
        ]

    , testGroup "Arithmetic properties"
        [ fastProperty "advancePosBy newline is equivalent to posAfter newline" $
            forAll genSourcePos $ \pos ->
              let advanced = advancePosBy "\n" pos
                  after = posAfter '\n' pos
              in advanced === after

        , fastProperty "advancePosBy empty string returns original position" $
            forAll genSourcePos $ \pos ->
              advancePosBy "" pos === pos

        , fastProperty "advancePosBy is additive for concatenated strings" $
            forAll genSourcePos $ \pos ->
              forAll genString $ \str1 ->
                forAll genString $ \str2 ->
                  let pos1 = advancePosBy str1 pos
                      pos2 = advancePosBy str2 pos1
                      posDirect = advancePosBy (str1 ++ str2) pos
                  in pos2 === posDirect

        , fastProperty "posAfter is idempotent for position calculations" $
            forAll genSourcePos $ \pos ->
              forAll genChar $ \c ->
                let pos1 = posAfter c pos
                    pos2 = posAfter c pos1
                    pos3 = advancePosBy [c, c] pos
                in pos2 === pos3
        ]

    , testGroup "Edge case properties"
        [ fastProperty "emptySpan always valid" $
            forAll genSourcePos $ \pos ->
              let span = emptySpan pos
              in isValidSpan span

        , fastProperty "span with same start and end is valid" $
            forAll genSourcePos $ \pos ->
              let span = SourceSpan pos pos
              in isValidSpan span

        , fastProperty "advancePosByText with empty text returns start position" $
            forAll genSourcePos $ \pos ->
              advancePosByText T.empty pos === pos

        , fastProperty "advancePosByLine with 0 lines returns original position" $
            forAll genSourcePos $ \pos ->
              advancePosByLine 0 pos === pos
        ]
  ]