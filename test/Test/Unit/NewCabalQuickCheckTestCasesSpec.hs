{-# LANGUAGE CPP #-}
module Test.Unit.NewCabalQuickCheckTestCasesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, choose, listOf, listOf1, elements, suchThat, (.&&.))
import Utils (trim, splitBy, splitByCollapsed, removeLineComments, removeComments, normalizeIndentation, breakOn)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, posAt, spanFrom, spanBetween, mergeSpans, isValidSpan, locatedAt, locatedValue, Located(..))
import Data.Char (isSpace)

-- | Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = genSourcePos

-- | Arbitrary instance for SourceSpan  
instance Arbitrary SourceSpan where
  arbitrary = genSourceSpan

-- | Generate arbitrary non-empty strings
genNonEmptyString :: Gen String
genNonEmptyString = listOf1 $ choose ('\32', '\126')

-- | Generate arbitrary strings that may include whitespace and newlines
genStringWithNewlines :: Gen String
genStringWithNewlines = listOf $ elements $ ['\32'..'\126'] ++ ['\n', '\t']

-- | Generate arbitrary characters for split tests
genSplitChar :: Gen Char
genSplitChar = elements [' ',',',';','|',':']

-- | Generate arbitrary positions
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 100)
  col <- choose (1, 100)
  offset <- choose (0, 10000)
  return $ SourcePos line col offset

-- | Generate arbitrary spans
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  end <- genSourcePos `suchThat` (\pos -> posOffset pos >= posOffset start)
  return $ SourceSpan start end

-- | Test 1: trim function property
prop_trim_roundtrip :: String -> Property
prop_trim_roundtrip s = 
  let trimmed = trim s
  in (trim trimmed) === trimmed

-- | Test 2: splitBy preserves empty segments
prop_splitBy_preserves_empty :: Char -> String -> Property
prop_splitBy_preserves_empty delim str =
  let parts = splitBy delim str
      rejoined = concat $ map (\p -> if null p then "" else p ++ [delim]) (init parts) ++ [last parts]
  in (length (filter (== delim) str) + 1) === length parts

-- | Test 3: splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim str =
  let parts = splitByCollapsed delim str
  in all (not . null) parts === True

-- | Test 4: removeLineComments preserves code structure
prop_removeLineComments_preserves_structure :: String -> Property
prop_removeLineComments_preserves_structure code =
  let withoutComments = removeLineComments code
      originalLines = lines code
      processedLines = lines withoutComments
  in (length processedLines <= length originalLines) === True

-- | Test 5: SourcePos ordering is consistent
prop_sourcePos_ordering :: SourcePos -> SourcePos -> Property
prop_sourcePos_ordering pos1 pos2 =
  if posOffset pos1 < posOffset pos2
  then (pos1 < pos2) === True
  else if posOffset pos1 > posOffset pos2
       then (pos1 > pos2) === True
       else pos1 === pos2

-- | Test 6: spanFrom creates valid spans
prop_spanFrom_valid :: SourcePos -> Property
prop_spanFrom_valid pos =
  let span = spanFrom pos
  in isValidSpan span === True

-- | Test 7: mergeSpans contains both original spans
prop_mergeSpans_contains_both :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_contains_both span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in (mergedStart <= start1 && mergedEnd >= end1 && mergedStart <= start2 && mergedEnd >= end2) === True

-- | Test 8: normalizeIndentation preserves relative structure
prop_normalizeIndentation_relative :: String -> Property
prop_normalizeIndentation_relative code =
  let normalized = normalizeIndentation code
      originalLines = filter (not . all isSpace) $ lines code
      normalizedLines = filter (not . all isSpace) $ lines normalized
  in length originalLines === length normalizedLines

-- | Test 9: breakOn finds correct split point
prop_breakOn_correct_split :: String -> String -> Property
prop_breakOn_correct_split pat str =
  let (before, after) = breakOn pat str
      combined = before ++ pat ++ after
  in if null pat
     then (before === "" .&&. after === str)
     else combined === str

-- | Test 10: locatedAt preserves value and position
prop_locatedAt_preserves :: SourcePos -> String -> Property
prop_locatedAt_preserves pos value =
  let located = locatedAt pos value
  in (locatedValue located === value) .&&. (spanStart (locSpan located) === pos)

tests :: TestTree
tests = testGroup "New Cabal QuickCheck Test Cases"
  [ testProperty "trim roundtrip" prop_trim_roundtrip
  , testProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
  , testProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
  , testProperty "removeLineComments preserves structure" prop_removeLineComments_preserves_structure
  , testProperty "SourcePos ordering" prop_sourcePos_ordering
  , testProperty "spanFrom creates valid spans" prop_spanFrom_valid
  , testProperty "mergeSpans contains both spans" prop_mergeSpans_contains_both
  , testProperty "normalizeIndentation preserves relative structure" prop_normalizeIndentation_relative
  , testProperty "breakOn correct split" prop_breakOn_correct_split
  , testProperty "locatedAt preserves value and position" prop_locatedAt_preserves
  ]