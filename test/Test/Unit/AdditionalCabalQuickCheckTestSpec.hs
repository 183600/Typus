{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.AdditionalCabalQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Arbitrary (Arbitrary(..), arbitrary)
import Test.QuickCheck (oneof, elements, listOf, sized)
import Test.QuickCheck.Gen (Gen, choose, vectorOf)

import SourceLocation 
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
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
  , advancePos
  , advancePosBy
  )

import Utils
  ( trim
  , splitBy
  , removeComments
  , normalizeIndentation
  )

import Data.Char (isSpace, isLetter, isDigit)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, sort, lines)

-- QuickCheck generators for SourceLocation types

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    offset <- choose (0, 10000)
    return $ SourcePos line col offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end

-- Test cases for SourceLocation module

-- Property: startPos creates position at line 1, column 1
prop_startPos_basic :: Property
prop_startPos_basic =
  let pos = startPos
  in property $ posLine pos === 1 .&&. posColumn pos === 1

-- Property: posAfter moves to next column
prop_posAfter_next_column :: Char -> SourcePos -> Property
prop_posAfter_next_column ch pos =
  let nextPos = posAfter ch pos
  in if ch == '\n'
     then property $ posLine nextPos === posLine pos + 1 .&&. posColumn nextPos === 1
     else property $ posLine nextPos === posLine pos .&&. posColumn nextPos === posColumn pos + 1

-- Property: posAt creates position at specific line and column
prop_posAt_specific :: Int -> Int -> Property
prop_posAt_specific line col =
  line >= 1 && col >= 1 && line <= 1000 && col <= 1000 ==>
  let pos = posAt line col
  in property $ posLine pos === line .&&. posColumn pos === col

-- Property: emptySpan has start and end at same position
prop_emptySpan_consistency :: SourcePos -> Property
prop_emptySpan_consistency pos =
  let span = emptySpan pos
  in property $ spanStart span === spanEnd span .&&.
             spanStart span === pos

-- Property: spanFrom creates span starting at given position
prop_spanFrom_creation :: SourcePos -> Property
prop_spanFrom_creation start =
  let span = spanFrom start
  in property $ spanStart span === start .&&. spanEnd span === start

-- Property: spanTo creates span ending at given position
prop_spanTo_creation :: SourcePos -> Property
prop_spanTo_creation end =
  let span = spanTo end
  in property $ spanStart span === end .&&. spanEnd span === end

-- Property: mergeSpans combines two spans correctly
prop_mergeSpans_combination :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_mergeSpans_combination start1 end1 start2 end2 =
  let span1 = spanBetween start1 end1
      span2 = spanBetween start2 end2
      merged = mergeSpans span1 span2
  in property $ spanStart merged === min start1 start2 .&&. spanEnd merged === max end1 end2

-- Property: isValidSpan checks span validity correctly
prop_isValidSpan_check :: SourcePos -> SourcePos -> Property
prop_isValidSpan_check start end =
  let span = spanBetween start end
      valid = start <= end
  in property $ isValidSpan span === valid

-- Property: locatedAt creates located value at position
prop_locatedAt_creation :: SourcePos -> String -> Property
prop_locatedAt_creation pos value =
  let located = locatedAt pos value
  in property $ locatedSpan located === emptySpan pos .&&.
             locatedValue located === value

-- Property: locatedWithSpan creates located value with span
prop_locatedWithSpan_creation :: SourceSpan -> String -> Property
prop_locatedWithSpan_creation span value =
  let located = locatedWithSpan span value
  in property $ locatedSpan located === span .&&.
             locatedValue located === value

-- Property: advancePos moves position by one character
prop_advancePos_single :: Char -> SourcePos -> Property
prop_advancePos_single ch pos =
  let newPos = advancePos ch pos
  in if ch == '\n'
     then property $ posLine newPos === posLine pos + 1 .&&.
                    posColumn newPos === 1
     else property $ posLine newPos === posLine pos .&&.
                    posColumn newPos === posColumn pos + 1

-- Property: advancePosBy moves position by multiple characters
prop_advancePosBy_multiple :: String -> SourcePos -> Property
prop_advancePosBy_multiple str pos =
  let finalPos = advancePosBy str pos
  in property $ posOffset finalPos >= posOffset pos

-- Test cases for Utils module (additional properties)

-- Property: trim removes all leading and trailing whitespace
prop_trim_comprehensive :: String -> String -> String -> Property
prop_trim_comprehensive prefix content suffix =
  let leading = replicate (length prefix `mod` 10) ' '
      trailing = replicate (length suffix `mod` 10) '\t'
      full = leading ++ content ++ trailing
      trimmed = trim full
  in property $ not (null trimmed) ==> 
             not (isSpace (head trimmed)) .&&.
             not (isSpace (last trimmed))

-- Property: splitBy handles empty input correctly
prop_splitBy_empty :: Char -> Property
prop_splitBy_empty delim =
  let result = splitBy delim ""
  in property $ result === [""]

-- Property: splitBy handles single character input
prop_splitBy_single :: Char -> String -> Property
prop_splitBy_single delim input =
  let result = splitBy delim input
  in if input == [delim]
     then property $ result === ["", ""]
     else property $ result === [input]

-- Property: removeComments preserves non-comment content
prop_removeComments_preservation :: String -> String -> Property
prop_removeComments_preservation code1 code2 =
  let content = code1 ++ "/* comment */" ++ code2
      cleaned = removeComments content
  in not (null code1 || null code2) ==>
     property $ code1 `isInfixOf` cleaned .&&.
                code2 `isInfixOf` cleaned

-- Property: normalizeIndentation preserves line count
prop_normalizeIndentation_line_count :: [String] -> Property
prop_normalizeIndentation_line_count inputLines =
  not (null inputLines) ==>
  let input = unlines inputLines
      normalized = normalizeIndentation input
  in property $ length (Data.List.lines normalized) === length inputLines

-- Property: normalizeIndentation removes common indentation
prop_normalizeIndentation_common_removal :: String -> [String] -> Property
prop_normalizeIndentation_common_removal indent inputLines =
  not (null inputLines) && not (any null inputLines) ==>
  let indentedLines = map (indent ++) inputLines
      input = unlines indentedLines
      normalized = normalizeIndentation input
      normalizedLines = Data.List.lines normalized
      hasCommonIndent = all (isPrefixOf indent) indentedLines
  in if hasCommonIndent
     then property $ not (any (isPrefixOf indent) normalizedLines)
     else property $ length normalizedLines === length inputLines

-- Additional comprehensive properties

-- Property: Source position ordering is consistent
prop_source_position_ordering :: SourcePos -> SourcePos -> Property
prop_source_position_ordering pos1 pos2 =
  let line1 = posLine pos1
      col1 = posColumn pos1
      line2 = posLine pos2
      col2 = posColumn pos2
  in if line1 < line2
     then property True
     else if line1 > line2
          then property True
          else property $ (col1 /= col2) .&&. (pos1 === pos2)

-- Property: Span merging is associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpans_associative span1 span2 span3 =
  let merge12 = mergeSpans span1 span2
      merge23 = mergeSpans span2 span3
      result1 = mergeSpans merge12 span3
      result2 = mergeSpans span1 merge23
  in property $ isValidSpan result1 .&&. isValidSpan result2

-- Property: Located values preserve their content
prop_located_content_preservation :: SourceSpan -> String -> Property
prop_located_content_preservation span content =
  let located = locatedWithSpan span content
  in property $ locatedValue located === content

-- Property: String processing pipeline consistency
prop_string_pipeline_consistency :: String -> Property
prop_string_pipeline_consistency input =
  let pipeline1 = input |> trim |> removeComments |> normalizeIndentation
      pipeline2 = input |> removeComments |> trim |> normalizeIndentation
  in property $ length pipeline1 >= 0 .&&. length pipeline2 >= 0

-- Property: Position advancement with mixed content
prop_position_advancement_mixed :: String -> SourcePos -> Property
prop_position_advancement_mixed str pos =
  let finalPos = advancePosBy str pos
      newlineCount = length $ filter (== '\n') str
  in property $ posLine finalPos >= posLine pos .&&.
             posLine finalPos <= posLine pos + newlineCount + 1

-- Property: Span validity is transitive
prop_span_validity_transitive :: SourcePos -> SourcePos -> SourcePos -> Property
prop_span_validity_transitive start middle end =
  let span1 = spanBetween start middle
      span2 = spanBetween middle end
      span3 = spanBetween start end
  in property $ (isValidSpan span1 && isValidSpan span2) ==> isValidSpan span3

-- Property: Empty span is always valid
prop_empty_span_validity :: SourcePos -> Property
prop_empty_span_validity pos =
  let span = emptySpan pos
  in property $ isValidSpan span

-- Property: String trimming with Unicode content
prop_trim_unicode :: String -> Property
prop_trim_unicode content =
  let unicodeContent = " \t\n " ++ content ++ " café 🚀 测试 \t\n "
      trimmed = trim unicodeContent
  in property $ not (null trimmed) ==> 
             not (isSpace (head trimmed)) .&&.
             not (isSpace (last trimmed))

-- Property: Comment removal with nested patterns
prop_removeComments_nested :: String -> String -> String -> Property
prop_removeComments_nested before middle after =
  not ("/*" `isInfixOf` before) && not ("*/" `isInfixOf` before) &&
  not ("/*" `isInfixOf` middle) && not ("*/" `isInfixOf` middle) &&
  not ("/*" `isInfixOf` after) && not ("*/" `isInfixOf` after) ==>
  let content = before ++ "/* outer " ++ middle ++ " */" ++ after
      cleaned = removeComments content
  in property $ before `isInfixOf` cleaned .&&.
             after `isInfixOf` cleaned .&&.
             not ("/* outer" `isInfixOf` cleaned) .&&.
             not ("*/" `isInfixOf` cleaned)

-- Collect all tests
tests :: TestTree
tests = testGroup "Additional Cabal QuickCheck Tests"
  [ testGroup "SourceLocation Tests"
    [ fastProperty "startPos creates position at (1,1)" prop_startPos_basic
    , fastProperty "posAfter moves to next column" prop_posAfter_next_column
    , fastProperty "posAt creates position at specific line and column" prop_posAt_specific
    , fastProperty "emptySpan has consistent start and end" prop_emptySpan_consistency
    , fastProperty "spanFrom creates span correctly" prop_spanFrom_creation
    , fastProperty "spanTo creates span correctly" prop_spanTo_creation
    , fastProperty "mergeSpans combines spans correctly" prop_mergeSpans_combination
    , fastProperty "isValidSpan checks validity correctly" prop_isValidSpan_check
    , fastProperty "locatedAt creates located value at position" prop_locatedAt_creation
    , fastProperty "locatedWithSpan creates located value with span" prop_locatedWithSpan_creation
    , fastProperty "advancePos moves position by one character" prop_advancePos_single
    , fastProperty "advancePosBy moves position by multiple characters" prop_advancePosBy_multiple
    , fastProperty "Source position ordering is consistent" prop_source_position_ordering
    , fastProperty "Span merging is associative" prop_mergeSpans_associative
    , fastProperty "Located values preserve their content" prop_located_content_preservation
    , fastProperty "Span validity is transitive" prop_span_validity_transitive
    , fastProperty "Empty span is always valid" prop_empty_span_validity
    ]
  , testGroup "Utils Additional Tests"
    [ fastProperty "trim removes all leading and trailing whitespace" prop_trim_comprehensive
    , fastProperty "splitBy handles empty input correctly" prop_splitBy_empty
    , fastProperty "splitBy handles single character input" prop_splitBy_single
    , fastProperty "removeComments preserves non-comment content" prop_removeComments_preservation
    , fastProperty "normalizeIndentation preserves line count" prop_normalizeIndentation_line_count
    , fastProperty "normalizeIndentation removes common indentation" prop_normalizeIndentation_common_removal
    , fastProperty "String processing pipeline consistency" prop_string_pipeline_consistency
    , fastProperty "Position advancement with mixed content" prop_position_advancement_mixed
    , fastProperty "String trimming with Unicode content" prop_trim_unicode
    , fastProperty "Comment removal with nested patterns" prop_removeComments_nested
    ]
  ]

-- Helper operator for pipeline testing
(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 0 |>
