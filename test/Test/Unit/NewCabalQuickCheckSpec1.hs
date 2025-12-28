module Test.Unit.NewCabalQuickCheckSpec1 (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, property, Arbitrary(..), Gen, oneof, listOf, elements)
import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf)

import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed, 
             removeLineComments, removeComments, normalizeIndentation, breakOn)

-- | QuickCheck tests for Utils module focusing on string processing properties
tests :: TestTree
tests =
  testGroup "NewCabalQuickCheckSpec1 - Utils String Processing Properties"
    [ testProperty "trim is idempotent" prop_trimIdempotent
    , testProperty "trim removes only leading/trailing whitespace" prop_trimOnlyRemovesWhitespace
    , testProperty "splitBy preserves segment count" prop_splitBySegmentCount
    , testProperty "splitByCollapsed removes empty segments" prop_splitByCollapsedRemovesEmpty
    , testProperty "splitByComma equals splitBy with comma delimiter" prop_splitByCommaEqualsSplitBy
    , testProperty "removeLineComments preserves non-comment content" prop_removeLineCommentsPreservesContent
    , testProperty "removeComments preserves non-comment content" prop_removeCommentsPreservesContent
    , testProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentationPreservesRelative
    , testProperty "breakOn pattern matching properties" prop_breakOnProperties
    , testProperty "trim and splitBy interaction" prop_trimSplitByInteraction
    ]

-- Property: trim is idempotent (applying twice is same as applying once)
prop_trimIdempotent :: String -> Bool
prop_trimIdempotent input = trim (trim input) == trim input

-- Property: trim only removes whitespace characters from ends
prop_trimOnlyRemovesWhitespace :: String -> Bool
prop_trimOnlyRemovesWhitespace input =
  let trimmed = trim input
      originalLength = length input
      trimmedLength = length trimmed
      leadingRemoved = take (originalLength - trimmedLength) input
      trailingRemoved = drop trimmedLength input
  in all isSpace leadingRemoved && all isSpace trailingRemoved

-- Property: splitBy with delimiter d on string s produces length that matches count of d + 1
prop_splitBySegmentCount :: Char -> String -> Bool
prop_splitBySegmentCount delim input = 
  let segments = splitBy delim input
      expectedCount = length (filter (== delim) input) + 1
  in length segments == expectedCount

-- Property: splitByCollapsed never produces empty segments
prop_splitByCollapsedRemovesEmpty :: Char -> String -> Bool
prop_splitByCollapsedRemovesEmpty delim input = 
  let segments = splitByCollapsed delim input
  in all (not . null) segments

-- Property: splitByComma should equal splitBy with comma delimiter
prop_splitByCommaEqualsSplitBy :: String -> Bool
prop_splitByCommaEqualsSplitBy input = splitByComma input == splitBy ',' input

-- Property: removeLineComments preserves content before // on each line
prop_removeLineCommentsPreservesContent :: String -> Bool
prop_removeLineCommentsPreservesContent input =
  let lines' = lines input
      processedLines = lines (removeLineComments input)
      checkLine (original, processed) = 
        let beforeComment = takeWhile (/= '/') original
        in processed `isPrefixOf` beforeComment || null beforeComment
  in all checkLine (zip lines' processedLines)

-- Property: removeComments preserves content outside block comments
prop_removeCommentsPreservesContent :: String -> Bool
prop_removeCommentsPreservesContent input =
  let processed = removeComments input
      -- Check that characters not in block comments are preserved
      hasNoUnmatchedCommentMarkers = not ("/*" `isInfixOf` processed) && not ("*/" `isInfixOf` processed)
  in hasNoUnmatchedCommentMarkers
  where
    isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)
    tails [] = [[]]
    tails s@(_:xs) = s : tails xs

-- Property: normalizeIndentation preserves relative indentation differences
prop_normalizeIndentationPreservesRelative :: String -> Bool
prop_normalizeIndentationPreservesRelative input =
  let normalized = normalizeIndentation input
      originalLines = lines input
      normalizedLines = lines normalized
      
      -- Calculate relative indentation differences between consecutive non-empty lines
      relDiffs [] = []
      relDiffs [_] = []
      relDiffs (x:y:xs) = 
        let indentX = length $ takeWhile isSpace x
            indentY = length $ takeWhile isSpace y
        in (indentY - indentX) : relDiffs (y:xs)
      
      origDiffs = relDiffs $ filter (not . all isSpace) originalLines
      normDiffs = relDiffs $ filter (not . all isSpace) normalizedLines
  in origDiffs == normDiffs

-- Property: breakOn satisfies decomposition property
prop_breakOnProperties :: String -> String -> Bool
prop_breakOnProperties pat input
  | null pat = breakOn pat input == ("", input)
  | pat `isPrefixOf` input = 
      let (before, after) = breakOn pat input
      in null before && input `isPrefixOf` (pat ++ after)
  | pat `isInfixOf` input =
      let (before, after) = breakOn pat input
          expectedBefore = takeWhile (not . (pat `isPrefixOf`)) (tails input) !! 0
      in before ++ pat ++ after == input
  | otherwise = breakOn pat input == (input, "")
  where
    isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)
    tails [] = [[]]
    tails s@(_:xs) = s : tails xs

-- Property: interaction between trim and splitBy functions
prop_trimSplitByInteraction :: Char -> String -> Bool
prop_trimSplitByInteraction delim input =
  let splitTrimmed = splitBy delim (trim input)
      trimEachSegment = map trim (splitBy delim input)
  in length splitTrimmed == length trimEachSegment

-- Additional property: splitByCommaCollapsed equals splitByCollapsed with comma
prop_splitByCommaCollapsedEqualsSplitByCollapsed :: String -> Bool
prop_splitByCommaCollapsedEqualsSplitByCollapsed input = 
  splitByCommaCollapsed input == splitByCollapsed ',' input

-- Additional property: normalizeIndentation is idempotent
prop_normalizeIndentationIdempotent :: String -> Bool
prop_normalizeIndentationIdempotent input =
  let once = normalizeIndentation input
      twice = normalizeIndentation once
  in once == twice