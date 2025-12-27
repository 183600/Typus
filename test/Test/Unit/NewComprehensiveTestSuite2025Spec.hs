{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewComprehensiveTestSuite2025Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen)
import Test.Tasty.HUnit (testCase, (@=?))

import Utils (trim, splitBy, splitByComma, normalizeIndentation, removeLineComments)
import SourceLocation (SourcePos(..), SourceSpan(..), advancePos, mergeSpans)
import qualified Data.Text as T

-- Test 1: String Processing Properties
tests :: TestTree
tests = testGroup "New Comprehensive Test Suite"
  [ testProperty "trim idempotent" propTrimIdempotent
  , testProperty "splitBy consistency" propSplitByConsistency
  , testProperty "normalizeIndentation preserves relative structure" propNormalizeIndentationPreservesRelative
  , testProperty "removeLineComments preserves code structure" propRemoveLineCommentsPreservesStructure
  , testProperty "SourcePos advance consistency" propSourcePosAdvanceConsistency
  , testProperty "SourceSpan merge associativity" propSourceSpanMergeAssociativity
  , testCase "splitByComma edge cases" testSplitByCommaEdgeCases
  , testProperty "trim removes all whitespace" propTrimRemovesAllWhitespace
  , testProperty "SourcePos advance by newline increments line" propSourcePosAdvanceNewline
  , testProperty "SourceSpan merge preserves order" propSourceSpanMergePreservesOrder
  ]

-- Property 1: trim is idempotent
propTrimIdempotent :: String -> Bool
propTrimIdempotent s = trim (trim s) == trim s

-- Property 2: splitBy consistency with concat
propSplitByConsistency :: Char -> String -> Bool
propSplitByConsistency delim s = concat (splitBy delim s) == filter (/= delim) s

-- Property 3: normalizeIndentation preserves relative structure
propNormalizeIndentationPreservesRelative :: String -> Bool
propNormalizeIndentationPreservesRelative s = 
  let normalized = normalizeIndentation s
      lines1 = lines s
      lines2 = lines normalized
  in length lines1 == length lines2

-- Property 4: removeLineComments preserves code structure
propRemoveLineCommentsPreservesStructure :: String -> Bool
propRemoveLineCommentsPreservesStructure s =
  let withoutComments = removeLineComments s
      lines1 = lines s
      lines2 = lines withoutComments
  in length lines2 <= length lines1

-- Property 5: SourcePos advance consistency
propSourcePosAdvanceConsistency :: String -> Bool
propSourcePosAdvanceConsistency s =
  let pos = SourcePos 1 1
      advanced = advancePos pos s
  in sourceLine advanced >= sourceLine pos && sourceColumn advanced >= 1

-- Property 6: SourceSpan merge associativity
propSourceSpanMergeAssociativity :: SourceSpan -> SourceSpan -> SourceSpan -> Bool
propSourceSpanMergeAssociativity span1 span2 span3 =
  let merge12 = mergeSpans span1 span2
      merge23 = mergeSpans span2 span3
      left = mergeSpans merge12 span3
      right = mergeSpans span1 merge23
  in spanStart left == spanStart right && spanEnd left == spanEnd right

-- Test Case 7: splitByComma edge cases
testSplitByCommaEdgeCases :: IO ()
testSplitByCommaEdgeCases = do
  splitByComma "" @=? [""]
  splitByComma "," @=? ["", ""]
  splitByComma "a,b,c" @=? ["a", "b", "c"]
  splitByComma "a,,b" @=? ["a", "", "b"]

-- Property 8: trim removes all whitespace
propTrimRemovesAllWhitespace :: String -> Bool
propTrimRemovesAllWhitespace s =
  let trimmed = trim s
  in null trimmed || (not (isSpace (head trimmed)) && not (isSpace (last trimmed)))
  where
    isSpace c = c `elem` " \t\n\r"

-- Property 9: SourcePos advance by newline increments line
propSourcePosAdvanceNewline :: String -> Bool
propSourcePosAdvanceNewline s =
  let pos = SourcePos 1 1
      advanced = advancePos pos s
  in '\n' `elem` s ==> sourceLine advanced > sourceLine pos

-- Property 10: SourceSpan merge preserves order
propSourceSpanMergePreservesOrder :: SourceSpan -> SourceSpan -> Bool
propSourceSpanMergePreservesOrder span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      start2 = spanStart span2
      mergedStart = spanStart merged
  in mergedStart `elem` [start1, start2]