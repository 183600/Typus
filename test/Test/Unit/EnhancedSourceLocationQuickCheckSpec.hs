module Test.Unit.EnhancedSourceLocationQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, listOf, elements, choose, suchThat, (===), (.&&.), forAll)
import TestSupport.QuickCheck 
                              expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in posLine                               newPos == line && posColumn                               newPos == expectedCol && posOffset                               newPos == 1

-- Property: posAfter advances correctly for regular character
prop_posAfterRegular :: Int -> Int -> Char -> Bool
prop_posAfterRegular line col char
  |                               char == '\n' ||                               char == '\t' = True  -- Skip special cases
  |                               otherwise =
      let pos = SourcePos line col 0
                                        newPos = posAfter char pos
      in posLine                               newPos == line && posColumn                               newPos == col + 1 && posOffset                               newPos == 1

-- Property: posAt creates valid position
prop_posAtValid :: Int -> Int -> Bool
prop_posAtValid line                               col =
  let pos = posAt line col
  in posLine                               pos == line && posColumn                               pos == col && posOffset                               pos == 0

-- Property: posAtLineCol creates position with correct values
prop_posAtLineColCorrect :: Int -> Int -> Int -> Bool
prop_posAtLineColCorrect line col                               offset =
  let pos = posAtLineCol line col offset
  in posLine                               pos == line && posColumn                               pos == col && posOffset                               pos == offset

-- ============================================================================
-- Source Span Properties
-- ============================================================================

-- Property: emptySpan has same start L.and end
prop_emptySpanSameStartEnd :: SourcePos -> Bool
prop_emptySpanSameStartEnd                               pos =
  let span = emptySpan pos
  in spanStart                               span == pos && spanEnd                               span == pos

-- Property: spanBetween creates valid span
prop_spanBetweenValid :: SourcePos -> SourcePos -> Bool
prop_spanBetweenValid start                               end =
  let span = spanBetween start end
  in spanStart                               span == start && spanEnd                               span == end

-- Property: mergeSpans contains both original spans
prop_mergeSpansContainsBoth :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Bool
prop_mergeSpansContainsBoth start1 end1 start2                               end2 =
  let span1 = spanBetween start1 end1
                                    span2 = spanBetween start2 end2
                                    merged = mergeSpans span1 span2
  in spanStart merged <= spanStart span1 && spanEnd merged >= spanEnd span1 &&
     spanStart merged <= spanStart span2 && spanEnd merged >= spanEnd span2

-- Property: isValidSpan checks start <= end
prop_isValidSpanCorrect :: SourcePos -> SourcePos -> Bool
prop_isValidSpanCorrect start                               end =
  let span = spanBetween start end
  in isValidSpan                               span == (start <= end)

-- Property: spanFrom creates empty span at position
prop_spanFromEmpty :: SourcePos -> Bool
prop_spanFromEmpty                               pos =
  let span = spanFrom pos
  in spanStart                               span == pos && spanEnd                               span == pos

-- ============================================================================
-- Located Value Properties
-- ============================================================================

-- Property: locatedAt creates located value at position
prop_locatedAtCorrect :: SourcePos -> Int -> Bool
prop_locatedAtCorrect pos                               value =
  let located = locatedAt pos value
  in locValue                               located == value && locPos                               located == pos && locSpan                               located == emptySpan pos

-- Property: locatedWithSpan creates located value with span
prop_locatedWithSpanCorrect :: SourceSpan -> String -> Bool
prop_locatedWithSpanCorrect span                               value =
  let located = locatedWithSpan span value
  in locValue                               located == value && locSpan                               located == span && locPos                               located == spanStart span

-- Property: mapLocated preserves location
prop_mapLocatedPreservesLocation :: SourceSpan -> String -> Bool
prop_mapLocatedPreservesLocation span                               value =
  let located = locatedWithSpan span value
                                    mapped = mapLocated (++ " suffix") located
  in locSpan                               mapped == span && locPos                               mapped == spanStart span

-- Property: locatedValue extracts original value
prop_locatedValueExtracts :: SourceSpan -> Int -> Bool
prop_locatedValueExtracts span                               value =
  let located = locatedWithSpan span value
  in locatedValue                               located == value

-- ============================================================================
-- Position Advancement Properties
-- ============================================================================

-- Property: advancePosBy advances correctly for multiple characters
prop_advancePosByCorrect :: SourcePos -> String -> Bool
prop_advancePosByCorrect pos                               chars =
  let advanced = advancePosBy chars pos
                                    expected = L.foldl (flip advancePos) pos chars
  in                               advanced == expected

-- Property: advancePosByText advances correctly for text
prop_advancePosByTextCorrect :: SourcePos -> Text -> Bool
prop_advancePosByTextCorrect pos                               text =
  let advanced = advancePosByText text pos
                                    expected = advancePosBy (T.unpack text) pos
  in                               advanced == expected

-- Property: advancePosByLine advances line number
prop_advancePosByLineCorrect :: SourcePos -> Int -> Bool
prop_advancePosByLineCorrect pos                               numLines =
  let advanced = advancePosByLine numLines pos
  in posLine                               advanced == posLine pos + numLines && 
     posColumn                               advanced == 1 &&
     posOffset                               advanced == posOffset pos + numLines

-- Property: position advancement is consistent
prop_positionAdvancementConsistent :: SourcePos -> String -> Bool
prop_positionAdvancementConsistent pos                               chars =
  let byString = advancePosBy chars pos
                                    byIndividual = L.foldl (flip advancePos) pos chars
  in                               byString == byIndividual

-- ============================================================================
-- Error Location Conversion Properties
-- ============================================================================

-- Property: toErrorLocation creates correct error location
prop_toErrorLocationCorrect :: SourcePos -> Bool
prop_toErrorLocationCorrect                               pos =
  let errLoc = toErrorLocation pos
  in filePath                               errLoc == Nothing &&
     line                               errLoc == posLine pos &&
     column                               errLoc == posColumn pos &&
     endLine                               errLoc == Nothing &&
     endColumn                               errLoc == Nothing

-- Property: toErrorLocationWithSpan creates correct error location with range
prop_toErrorLocationWithSpanCorrect :: SourceSpan -> Bool
prop_toErrorLocationWithSpanCorrect                               span =
  let errLoc = toErrorLocationWithSpan span
                                    start = spanStart span
                                    end = spanEnd span
  in filePath                               errLoc == Nothing &&
     line                               errLoc == posLine start &&
     column                               errLoc == posColumn start &&
     endLine                               errLoc == Just (posLine end) &&
     endColumn                               errLoc == Just (posColumn end)

-- Property: error location conversion preserves position info
prop_errorLocationPreservesInfo :: SourcePos -> Bool
prop_errorLocationPreservesInfo                               pos =
  let errLoc = toErrorLocation pos
  in line                               errLoc == posLine pos && column                               errLoc == posColumn pos

-- ============================================================================
-- Utility Properties
-- ============================================================================

-- Property: comparePos orders positions correctly
prop_comparePosCorrect :: SourcePos -> SourcePos -> Bool
prop_comparePosCorrect pos1                               pos2 =
  let ordering = pos1 `compare` pos2
                                    offset1 = posOffset pos1
                                    offset2 = posOffset pos2
  in                               ordering == (offset1 `compare` offset2)

-- Property: position arithmetic is consistent
prop_positionArithmeticConsistent :: SourcePos -> String -> String -> Bool
prop_positionArithmeticConsistent pos s1                               s2 =
  let pos1 = advancePosBy s1 pos
                                    pos2 = advancePosBy s2 pos
                                    posCombined = advancePosBy (s1 ++ s2) pos
                                    pos2From1 = advancePosBy s2 pos1
  in                               posCombined == pos2From1

-- Property: span operations maintain invariants
prop_spanOperationsMaintainInvariants :: SourcePos -> SourcePos -> SourcePos -> Bool
prop_spanOperationsMaintainInvariants pos1 pos2                               pos3 =
  let span1 = spanBetween pos1 pos2
                                    span2 = spanBetween pos2 pos3
                                    merged = mergeSpans span1 span2
  in spanStart merged <= spanEnd merged &&
     spanStart merged <= spanStart span1 &&
     spanEnd merged >= spanEnd span2

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
                                              arbitrary = do
              line <- choose (1, 1000)
    col <- choose (1, 1000)
    offset <- choose (0, 1000000)
    return $ SourcePos line col offset

instance Arbitrary SourceSpan where
                                              arbitrary = do
              start <- arbitrary
    end <- arbitrary
    return $ spanBetween start end

instance Arbitrary Char where
                                              arbitrary = oneof
    [ elements ['a'..'z']
    , elements ['A'..'Z']
    , elements ['0'..'9']
    , elements " \t\n\r!@#$%^&*()_+-=[]{}|;':\",./<>?"
    ]

instance Arbitrary Text where
                                              arbitrary = T.pack <$> arbitrary