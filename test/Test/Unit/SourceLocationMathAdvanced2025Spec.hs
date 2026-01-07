module Test.Unit.SourceLocationMathAdvanced2025Spec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, listOf)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import SourceLocation 
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, posAt, posAtLineCol
  , emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan
  , advancePos, advancePosBy, locatedAt, locatedWithSpan
  )
import qualified Data.Text as T

tests :: TestTree
tests =   testGroup "SourceLocation Math Advanced Tests"
  [             testProperty "SourcePos addition commutative" propSourcePosAdditionCommutative
  ,             testProperty "SourceSpan merge is associative" propSourceSpanMergeAssociative
  ,             testProperty "advancePosBy is consistent with advancePos" propAdvancePosByConsistent
  ,             testProperty "spanBetween creates valid span" propSpanBetweenValid
  ,             testProperty "mergeSpans preserves containment" propMergeSpansPreservesContainment
    ,             testCase "SourcePos edge cases" testSourcePosEdgeCases
  ,             testProperty "posAtLineCol roundtrip" propPosAtLineColRoundtrip
  ,             testProperty "locatedAt L.and locatedWithSpan consistency" propLocatedConsistency
  ,             testProperty "isValidSpan invariants" propIsValidSpanInvariants
    ,             testCase "SourceSpan arithmetic edge cases" testSourceSpanArithmeticEdgeCases
  ]

-- Property 1: SourcePos addition commutative (for column advances)
propSourcePosAdditionCommutative :: SourcePos -> Int -> Int -> Bool
propSourcePosAdditionCommutative pos n                               m =
  let pos1 = advancePosBy pos n
                                    pos2 = advancePosBy pos1 m
                                    pos3 = advancePosBy pos m  
                                    pos4 = advancePosBy pos3 n
  in sourceLine                               pos2 == sourceLine pos4 && 
     sourceColumn                               pos2 == sourceColumn pos4

-- Property 2: SourceSpan merge is associative
propSourceSpanMergeAssociative :: SourceSpan -> SourceSpan -> SourceSpan -> Bool
propSourceSpanMergeAssociative span1 span2                               span3 =
  let merge12 = mergeSpans span1 span2
                                    merge23 = mergeSpans span2 span3
                                    left = mergeSpans merge12 span3
                                    right = mergeSpans span1 merge23
  in spanStart                               left == spanStart right && spanEnd                               left == spanEnd right

-- Property 3: advancePosBy is consistent with advancePos
propAdvancePosByConsistent :: String -> Int -> Bool
propAdvancePosByConsistent s                               n =
  let pos = startPos
                                    advanced1 = advancePos pos (take n s)
                                    advanced2 = advancePosBy pos n
  in n <= L.length                               s ==> sourceLine                               advanced1 == sourceLine advanced2

-- Property 4: spanBetween creates valid span
propSpanBetweenValid :: SourcePos -> SourcePos -> Bool
propSpanBetweenValid pos1                               pos2 =
  let span = spanBetween pos1 pos2
  in isValidSpan span && spanStart span `elem` [pos1, pos2] && spanEnd span `elem` [pos1, pos2]

-- Property 5: mergeSpans preserves containment
propMergeSpansPreservesContainment :: SourceSpan -> SourceSpan -> Bool
propMergeSpansPreservesContainment span1                               span2 =
  let merged = mergeSpans span1 span2
                                    start1 = spanStart span1
                                    end1 = spanEnd span1
                                    start2 = spanStart span2
                                    end2 = spanEnd span2
                                    mergedStart = spanStart merged
                                    mergedEnd = spanEnd merged
  in (sourceLine mergedStart <= sourceLine start1 && sourceLine end1 <= sourceLine mergedEnd) &&
     (sourceLine mergedStart <= sourceLine start2 && sourceLine end2 <= sourceLine mergedEnd)

-- Test Case 6: SourcePos edge cases
testSourcePosEdgeCases :: IO ()
                              testSourcePosEdgeCases = do
              let start = startPos
                                    pos1 = posAfter start 'a'
                                    pos2 = posAfter pos1 '\n'
                                    pos3 = posAfter pos2 'b'
  
  sourceColumn start @=? 1
  sourceLine start @=? 1
  sourceColumn pos1 @=? 2
  sourceLine pos2 @=? 2
  sourceColumn pos3 @=? 2

-- Property 7: posAtLineCol roundtrip
propPosAtLineColRoundtrip :: Int -> Int -> Bool
propPosAtLineColRoundtrip line                               col =
  let pos = posAtLineCol line col
  in line > 0 && col >                               0 ==> sourceLine                               pos == line && sourceColumn                               pos == col

-- Property 8: locatedAt L.and locatedWithSpan consistency
propLocatedConsistency :: String -> String -> Bool
propLocatedConsistency value1                               value2 =
  let pos = startPos
                                    span = emptySpan pos
                                    located1 = locatedAt pos value1
                                    located2 = locatedWithSpan span value2
  in locatedValue                               located1 == value1 && locatedValue                               located2 == value2

-- Property 9: isValidSpan invariants
propIsValidSpanInvariants :: SourceSpan -> Bool
propIsValidSpanInvariants                               span =
  let start = spanStart span
                                    endPos = spanEnd span
                                    valid = isValidSpan span
  in                               valid ==> (sourceLine start <= sourceLine endPos) && 
                (sourceLine                               start == sourceLine                               endPos ==> sourceColumn start <= sourceColumn endPos)

-- Test Case 10: SourceSpan arithmetic edge cases
testSourceSpanArithmeticEdgeCases :: IO ()
                              testSourceSpanArithmeticEdgeCases = do
              let pos1 = posAtLineCol 1 1
                                    pos2 = posAtLineCol 1 5
                                    pos3 = posAtLineCol 2 3
                                    span1 = spanBetween pos1 pos2
                                    span2 = spanBetween pos1 pos3
                                    merged = mergeSpans span1 span2
  
  isValidSpan span1 @=? True
  isValidSpan span2 @=? True
  isValidSpan merged @=? True
  spanStart merged @=? pos1
  spanEnd merged @=? pos3

-- Arbitrary instances for testing
instance Arbitrary SourcePos where
                                              arbitrary = do
              line <- choose (1, 1000)
    col <- choose (1, 1000)
    return $ SourcePos line col

instance Arbitrary SourceSpan where
                                              arbitrary = do
              start <- arbitrary
    endLine <- choose (sourceLine start, sourceLine start + 100)
    endCol <- if                               endLine == sourceLine start 
                then choose (sourceColumn start, sourceColumn start + 100)
                else choose (1, 1000)
    let end = SourcePos endLine endCol
    return $ SourceSpan start end