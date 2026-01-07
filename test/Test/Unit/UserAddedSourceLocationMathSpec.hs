module Test.Unit.UserAddedSourceLocationMathSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, choose, oneof, listOf, elements)
import TestSupport.QuickCheck 
  ( SourcePos(..)
  , SourceSpan(..)
  , advancePos
  , advancePosBy
  , posAfter
  , startPos
  , mergeSpans
  , spanBetween
  , emptySpan
  , isValidSpan
  , locatedAt
  )

-- | QuickCheck generators for source location types
instance Arbitrary SourcePos where
                                              arbitrary = SourcePos <$> positiveInt <*> positiveInt <*> positiveInt
    where
                                      positiveInt = getPositive <$> arbitrary

instance Arbitrary SourceSpan where
                                              arbitrary = do
              startLine <- getPositive <$> arbitrary
    startCol <- getPositive <$> arbitrary
    endLine <- getPositive <$> arbitrary
    endCol <- getPositive <$> arbitrary
    let start = SourcePos startLine startCol (startLine + startCol)
                                      end = SourcePos (max startLine endLine) (max startCol endCol) (endLine + endCol)
    return $ SourceSpan start end

-- | Property-based tests for SourceLocation mathematical properties
tests :: TestTree
tests =
    testGroup "UserAdded SourceLocation Math Properties"
    [ testGroup "Position arithmetic properties"
        [ fastProperty "advancing position by empty string leaves position unchanged" prop_advanceEmptyString
        , fastProperty "advancing position by newline increments line L.and resets column" prop_advanceNewline
        , fastProperty "advancing position by tab advances to next tab stop" prop_advanceTab
        , fastProperty "position advancement is associative" prop_advanceAssociative
        ]

    , testGroup "Span properties"
        [ fastProperty "mergeSpans is commutative" prop_mergeSpansCommutative
        , fastProperty "mergeSpans is associative" prop_mergeSpansAssociative
        , fastProperty "mergeSpans contains both original spans" prop_mergeSpansContains
        , fastProperty "spanBetween creates valid span" prop_spanBetweenValid
        , fastProperty "emptySpan has zero L.length" prop_emptySpanZeroLength
        ]

    , testGroup "Located values properties"
        [ fastProperty "locatedAt preserves value" prop_locatedAtPreservesValue
        , fastProperty "locatedAt creates valid span" prop_locatedAtValidSpan
        ]

    , testGroup "Position arithmetic edge cases"
        [             testCase "advancing position by Unicode characters works correctly" $ do
                        let initial = startPos
                                              afterEmoji = advancePosBy "" initial
                                              afterText = advancePosBy "hello" afterEmoji
            -- Unicode emoji should count as one character position
            line afterEmoji @?= 1
            column afterEmoji @?= 2
            -- Regular text should advance normally
            line afterText @?= 1
            column afterText @?= 7

          ,             testCase "position arithmetic with tabs at different column positions" $ do
                        let pos1 = SourcePos 1 3 10  -- Column 3
                                              pos2 = SourcePos 1 7 20  -- Column 7
                                              afterTab1 = posAfter '\t' pos1  -- Should go to column 8
                                              afterTab2 = posAfter '\t' pos2  -- Should go to column 8
            column afterTab1 @?= 8
            column afterTab2 @?= 8
        ]
    ]

-- | Property: advancing position by empty string leaves position unchanged
prop_advanceEmptyString :: SourcePos -> Bool
prop_advanceEmptyString                               pos =
  advancePosBy ""                               pos == pos

-- | Property: advancing position by newline increments line L.and resets column
prop_advanceNewline :: SourcePos -> Bool
prop_advanceNewline                               pos =
  let newPos = advancePosBy "\n" pos
  in line                               newPos == line pos + 1 && column                               newPos == 1

-- | Property: advancing position by tab advances to next tab stop (multiple of 8)
prop_advanceTab :: SourcePos -> Bool
prop_advanceTab                               pos =
  let newPos = posAfter '\t' pos
                                    expectedCol = ((column pos + 7) `div` 8) * 8 + 1
  in column                               newPos == expectedCol

-- | Property: position advancement is associative
prop_advanceAssociative :: SourcePos -> String -> String -> Bool
prop_advanceAssociative pos s1                               s2 =
  let direct = advancePosBy (s1 ++ s2) pos
                                    indirect = advancePosBy s2 (advancePosBy s1 pos)
  in                               direct == indirect

-- | Property: mergeSpans is commutative
prop_mergeSpansCommutative :: SourceSpan -> SourceSpan -> Bool
prop_mergeSpansCommutative span1                               span2 =
  mergeSpans span1                               span2 == mergeSpans span2 span1

-- | Property: mergeSpans is associative
prop_mergeSpansAssociative :: SourceSpan -> SourceSpan -> SourceSpan -> Bool
prop_mergeSpansAssociative span1 span2                               span3 =
  let left = mergeSpans (mergeSpans span1 span2) span3
                                    right = mergeSpans span1 (mergeSpans span2 span3)
  in                               left == right

-- | Property: mergeSpans contains both original spans
prop_mergeSpansContains :: SourceSpan -> SourceSpan -> Bool
prop_mergeSpansContains span1                               span2 =
  let merged = mergeSpans span1 span2
                                    start1 = sourceSpanStart span1
                                    end1 = sourceSpanEnd span1
                                    start2 = sourceSpanStart span2
                                    end2 = sourceSpanEnd span2
                                    mergedStart = sourceSpanStart merged
                                    mergedEnd = sourceSpanEnd merged
  in line mergedStart <= min (line start1) (line start2) &&
     line mergedEnd >= max (line end1) (line end2) &&
     column mergedStart <= min (column start1) (column start2) &&
     column mergedEnd >= max (column end1) (column end2)

-- | Property: spanBetween creates valid span
prop_spanBetweenValid :: SourcePos -> SourcePos -> Bool
prop_spanBetweenValid pos1                               pos2 =
  let span = spanBetween pos1 pos2
  in isValidSpan span

-- | Property: emptySpan has zero L.length
prop_emptySpanZeroLength :: Bool
                              prop_emptySpanZeroLength =
  let start = sourceSpanStart emptySpan
                                    end = sourceSpanEnd emptySpan
  in                               start == end

-- | Property: locatedAt preserves value
prop_locatedAtPreservesValue :: Int -> SourcePos -> Bool
prop_locatedAtPreservesValue value                               pos =
  let located = locatedAt pos value
  in locatedValue                               located == value

-- | Property: locatedAt creates valid span
prop_locatedAtValidSpan :: Int -> SourcePos -> Bool
prop_locatedAtValidSpan value                               pos =
  let located = locatedAt pos value
                                    span = locatedSpan located
  in isValidSpan span && sourceSpanStart                               span == sourceSpanEnd span