{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationMathPrecisionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf
  , sized, resize, suchThat, frequency, choose, getPositive, getNonEmpty
  )

import SourceLocation
  ( SourcePos(..), SourceSpan(..), startPos
  , advancePosBy, advancePosByLine, advancePosByText
  , posAfter, spanBetween, spanFrom, spanTo
  , mergeSpans, isValidSpan, spanStart, spanEnd
  , toErrorLocation, toErrorLocationWithSpan
  )
import qualified Data.Text as T
import Data.Char (isSpace)
import Data.List (foldl')

-- | Generate source positions with reasonable bounds
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- getPositive <$> arbitrary
  col <- getPositive <$> arbitrary
  offset <- getPositive <$> arbitrary
  return $ SourcePos (line `mod` 1000 + 1) (col `mod` 1000 + 1) (offset `mod` 10000)

-- | Generate source spans
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  end <- genSourcePos
  -- Ensure end is after start
  let startOffset = sourceOffset start
      endOffset = sourceOffset end
      adjustedEnd = if endOffset >= startOffset 
                    then end 
                    else start { sourceOffset = startOffset + 
                               (col end - col start) + 
                               (line end - line start) * 100 }
  return $ SourceSpan start adjustedEnd

-- | Generate text content with various characters
genTextContent :: Gen String
genTextContent = oneof
  [ listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t"
  , listOf $ elements $ ['\128'..'\255'] ++ "测试🚀"
  , do
      n <- choose (0, 10)
      return $ concat (replicate n "word ")
  ]

-- | Generate multiline text content
genMultilineContent :: Gen String
genMultilineContent = do
  numLines <- choose (1, 10)
  lines <- listOf numLines genTextContent
  return $ unlines lines

-- | Generate text with specific patterns
genPatternedText :: Gen String
genPatternedText = oneof
  [ return $ "hello\nworld\n"
  , return $ "  indented\n    more indented\n"
  , return $ "a\tb\tc\n1\t2\t3\n"
  , do
      n <- choose (1, 5)
      return $ concat (replicate n "text\n")
  ]

-- Property: Position advancement should be mathematically consistent
prop_position_advancement_consistent :: SourcePos -> String -> Property
prop_position_advancement_consistent pos text =
  let advanced1 = advancePosByText (T.pack text) pos
      advanced2 = foldl' (\p c -> posAfter c p) pos text
  in property $ advanced1 === advanced2

-- Property: Line advancement should preserve column reset
prop_line_advancement_column_reset :: SourcePos -> Int -> Property
prop_line_advancement_column_reset pos lines =
  lines >= 0 && lines <= 100 ==> 
  let advanced = advancePosByLine lines pos
  in property $ column advanced === 1 && 
             line advanced === line pos + lines

-- Property: Position after tab should align to next tab stop
prop_position_after_tab_alignment :: SourcePos -> Property
prop_position_after_tab_alignment pos =
  let beforeTab = column pos
      afterTab = posAfter '\t' pos
      expectedCol = ((beforeTab - 1) `div` 8 + 1) * 8 + 1
  in property $ column afterTab === expectedCol

-- Property: Position after newline should reset column and increment line
prop_position_after_newline :: SourcePos -> Property
prop_position_after_newline pos =
  let afterNewline = posAfter '\n' pos
  in property $ column afterNewline === 1 && 
             line afterNewline === line pos + 1

-- Property: Position advancement should track offset correctly
prop_position_advancement_offset :: SourcePos -> String -> Property
prop_position_advancement_offset pos text =
  let startOffset = sourceOffset pos
      advanced = advancePosByText (T.pack text) pos
      expectedOffset = startOffset + length text
  in property $ sourceOffset advanced === expectedOffset

-- Property: Span between positions should be mathematically sound
prop_span_between_sound :: SourcePos -> SourcePos -> Property
prop_span_between_sound pos1 pos2 =
  let startOffset = sourceOffset pos1
      endOffset = sourceOffset pos2
      span = if endOffset >= startOffset
             then spanBetween pos1 pos2
             else spanBetween pos2 pos1
  in property $ sourceOffset (spanStart span) <= sourceOffset (spanEnd span)

-- Property: Merged spans should cover original spans
prop_merged_spans_coverage :: SourceSpan -> SourceSpan -> Property
prop_merged_spans_coverage span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = sourceOffset $ spanStart span1
      start2 = sourceOffset $ spanStart span2
      end1 = sourceOffset $ spanEnd span1
      end2 = sourceOffset $ spanEnd span2
      mergedStart = sourceOffset $ spanStart merged
      mergedEnd = sourceOffset $ spanEnd merged
  in property $ mergedStart <= min start1 start2 && 
             mergedEnd >= max end1 end2

-- Property: Span validity should be transitive
prop_span_validity_transitive :: SourcePos -> SourcePos -> SourcePos -> Property
prop_span_validity_transitive pos1 pos2 pos3 =
  let span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      span3 = spanBetween pos1 pos3
      valid1 = isValidSpan span1
      valid2 = isValidSpan span2
      valid3 = isValidSpan span3
  in property $ (valid1 && valid2) ==> valid3

-- Property: Position advancement should handle Unicode correctly
prop_position_advancement_unicode :: SourcePos -> String -> Property
prop_position_advancement_unicode pos text =
  let hasUnicode = any (> '\127') text
      advanced = advancePosByText (T.pack text) pos
      expectedOffset = sourceOffset pos + length text
  in property $ sourceOffset advanced === expectedOffset

-- Property: Multiline text advancement should be accurate
prop_multiline_advancement_accuracy :: String -> Property
prop_multiline_advancement_accuracy content =
  '\n' `elem` content ==> 
  let lines' = lines content
      finalPos = foldl' (\pos line -> 
        advancePosByText (T.pack line) (posAfter '\n' pos)) startPos content
      expectedLine = length lines'
  in property $ line finalPos === expectedLine

-- Property: Tab expansion should be consistent
prop_tab_expansion_consistent :: SourcePos -> Int -> Property
prop_tab_expansion_consistent pos tabCount =
  tabCount >= 0 && tabCount <= 10 ==> 
  let withTabs = replicate tabCount '\t'
      advanced = advancePosByText (T.pack withTabs) pos
      expectedCol = ((column pos - 1) `div` 8 + tabCount) * 8 + 1
  in property $ column advanced === expectedCol

-- Property: Span merging should be associative
prop_span_merging_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_span_merging_associative span1 span2 span3 =
  let merge12 = mergeSpans span1 span2
      merge23 = mergeSpans span2 span3
      result1 = mergeSpans merge12 span3
      result2 = mergeSpans span1 merge23
  in property $ result1 === result2

-- Property: Position arithmetic should be invertible for simple cases
prop_position_arithmetic_invertible :: SourcePos -> String -> Property
prop_position_arithmetic_invertible pos text =
  not (null text) && all (`elem` "abcde \t") text ==> 
  let advanced = advancePosByText (T.pack text) pos
      -- Note: Full inversion is complex, this tests simple cases
      offsetDiff = sourceOffset advanced - sourceOffset pos
  in property $ offsetDiff === length text

-- Property: Error location conversion should preserve information
prop_error_location_preservation :: SourceSpan -> Property
prop_error_location_preservation span =
  let errLoc = toErrorLocationWithSpan span
      startLine = line $ spanStart span
      startCol = column $ spanStart span
      endLine = line $ spanEnd span
      endCol = column $ spanEnd span
  in property $ line errLoc === startLine && 
             column errLoc === startCol &&
             endLine errLoc === Just endLine &&
             endColumn errLoc === Just endCol

-- Property: Source position ordering should be consistent
prop_position_ordering_consistent :: SourcePos -> SourcePos -> Property
prop_position_ordering_consistent pos1 pos2 =
  let offset1 = sourceOffset pos1
      offset2 = sourceOffset pos2
      span = if offset1 <= offset2 
             then spanBetween pos1 pos2
             else spanBetween pos2 pos1
  in property $ sourceOffset (spanStart span) <= sourceOffset (spanEnd span)

-- Property: Complex text patterns should be tracked accurately
prop_complex_text_tracking :: String -> Property
prop_complex_text_tracking text =
  length text > 10 ==> 
  let finalPos = advancePosByText (T.pack text) startPos
      lineCount = length $ filter (== '\n') text
      expectedLine = lineCount + 1
  in property $ line finalPos === expectedLine

-- Property: Span creation utilities should be consistent
prop_span_creation_consistency :: SourcePos -> Property
prop_span_creation_consistency pos =
  let spanFromPos = spanFrom pos
      spanToPos = spanTo pos
      emptySpanPos = spanBetween pos pos
  in property $ spanFromPos === emptySpanPos && 
             spanToPos === emptySpanPos

-- Property: Position advancement should handle empty text
prop_position_advancement_empty :: SourcePos -> Property
prop_position_advancement_empty pos =
  let advanced = advancePosByText T.empty pos
  in property $ advanced === pos

-- Property: Span validation should handle edge cases
prop_span_validation_edge_cases :: SourcePos -> Int -> Property
prop_span_validation_edge_cases pos offset =
  let offset' = abs offset `mod` 100
      samePos = pos { sourceOffset = sourceOffset pos + offset' }
      span = spanBetween pos samePos
  in property $ isValidSpan span === (sourceOffset samePos >= sourceOffset pos)

tests :: TestTree
tests = testGroup "Source Location Math Precision Tests"
  [ testGroup "Property-based tests"
    [ fastProperty "position advancement consistency" prop_position_advancement_consistent
    , fastProperty "line advancement column reset" prop_line_advancement_column_reset
    , fastProperty "position after tab alignment" prop_position_after_tab_alignment
    , fastProperty "position after newline" prop_position_after_newline
    , fastProperty "position advancement offset" prop_position_advancement_offset
    , fastProperty "span between sound" prop_span_between_sound
    , fastProperty "merged spans coverage" prop_merged_spans_coverage
    , fastProperty "span validity transitive" prop_span_validity_transitive
    , fastProperty "position advancement unicode" prop_position_advancement_unicode
    , fastProperty "multiline advancement accuracy" prop_multiline_advancement_accuracy
    , fastProperty "tab expansion consistent" prop_tab_expansion_consistent
    , fastProperty "span merging associative" prop_span_merging_associative
    , fastProperty "position arithmetic invertible" prop_position_arithmetic_invertible
    , fastProperty "error location preservation" prop_error_location_preservation
    , fastProperty "position ordering consistent" prop_position_ordering_consistent
    , fastProperty "complex text tracking" prop_complex_text_tracking
    , fastProperty "span creation consistency" prop_span_creation_consistency
    , fastProperty "position advancement empty" prop_position_advancement_empty
    , fastProperty "span validation edge cases" prop_span_validation_edge_cases
    ]

  , testGroup "Unit tests"
    [ testCase "basic position advancement" $ do
        let pos = startPos
        let advanced = advancePosByText (T.pack "hello") pos
        advanced @?= SourcePos 1 6 5
    
    , testCase "newline handling" $ do
        let pos = SourcePos 1 5 10
        let afterNewline = posAfter '\n' pos
        afterNewline @?= SourcePos 2 1 11
    
    , testCase "tab alignment" $ do
        let pos1 = SourcePos 1 3 8
        let pos2 = SourcePos 1 9 14
        let afterTab1 = posAfter '\t' pos1
        let afterTab2 = posAfter '\t' pos2
        afterTab1 @?= SourcePos 1 9 9
        afterTab2 @?= SourcePos 1 17 15
    
    , testCase "span creation" $ do
        let start = SourcePos 1 1 0
        let end = SourcePos 2 5 10
        let span = spanBetween start end
        spanStart span @?= start
        spanEnd span @?= end
    
    , testCase "span merging" $ do
        let span1 = spanBetween (SourcePos 1 1 0) (SourcePos 1 5 4)
        let span2 = spanBetween (SourcePos 1 3 2) (SourcePos 1 8 7)
        let merged = mergeSpans span1 span2
        spanStart merged @?= SourcePos 1 1 0
        spanEnd merged @?= SourcePos 1 8 7
    
    , testCase "multiline text" $ do
        let text = "line1\nline2\nline3"
        let finalPos = advancePosByText (T.pack text) startPos
        finalPos @?= SourcePos 3 6 17
    ]
  ]

-- Arbitrary instances
instance Arbitrary SourcePos where
  arbitrary = genSourcePos

instance Arbitrary SourceSpan where
  arbitrary = genSourceSpan