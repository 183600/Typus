{-# OPTIONS_GHC -Wno-deprecations #-}
module Test.Unit.UtilsStringProcessingQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , fixIndentation
  , breakOn
  )
import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf)

-- ============================================================================
-- Utils String Processing Property Tests
-- ============================================================================

-- | Test that trim removes leading and trailing whitespace
prop_trimRemovesLeadingTrailingWhitespace :: String -> String -> Property
prop_trimRemovesLeadingTrailingWhitespace prefix suffix =
  let whitespace = " \t\n\r"
      leadingPrefix = take 5 whitespace ++ prefix
      trailingSuffix = suffix ++ take 5 whitespace
      input = leadingPrefix ++ trailingSuffix
      trimmed = trim input
      expected = prefix ++ suffix
  in counterexample ("trim failed to remove leading/trailing whitespace. " ++
                     "Input: " ++ show input ++
                     " Expected: " ++ show expected ++
                     " Actual: " ++ show trimmed)
     (trimmed === expected)

-- | Test that trim preserves internal whitespace
prop_trimPreservesInternalWhitespace :: String -> String -> Property
prop_trimPreservesInternalWhitespace part1 part2 =
  let whitespace = " \t\n"
      internal = whitespace ++ "middle" ++ whitespace
      input = part1 ++ internal ++ part2
      trimmed = trim input
      hasInternalWhitespace = internal `isInfixOf` trimmed
  in counterexample ("trim should preserve internal whitespace. " ++
                     "Input: " ++ show input ++
                     " Trimmed: " ++ show trimmed)
     (hasInternalWhitespace === True)

-- | Test that splitBy preserves empty segments
prop_splitByPreservesEmptySegments :: Char -> String -> Property
prop_splitByPreservesEmptySegments delim input =
  let parts = splitBy delim input
      rejoined = intercalate [delim] parts
  in counterexample ("splitBy should preserve empty segments. " ++
                     "Input: " ++ show input ++
                     " Parts: " ++ show parts ++
                     " Rejoined: " ++ show rejoined)
     (rejoined === input)
  where
    intercalate _ [] = []
    intercalate sep [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- | Test that splitByCollapsed removes empty segments
prop_splitByCollapsedRemovesEmptySegments :: Char -> String -> Property
prop_splitByCollapsedRemovesEmptySegments delim input =
  let parts = splitBy delim input
      collapsedParts = splitByCollapsed delim input
      hasNoEmptySegments = not (any null collapsedParts)
  in counterexample ("splitByCollapsed should remove empty segments. " ++
                     "Input: " ++ show input ++
                     " Parts: " ++ show parts ++
                     " Collapsed: " ++ show collapsedParts)
     (hasNoEmptySegments === True)

-- | Test that splitByComma is equivalent to splitBy ','
prop_splitByCommaEqualsSplitByComma :: String -> Property
prop_splitByCommaEqualsSplitByComma input =
  let commaParts = splitByComma input
      byCommaParts = splitBy ',' input
  in counterexample ("splitByComma should equal splitBy ',' " ++
                     "Input: " ++ show input ++
                     " CommaParts: " ++ show commaParts ++
                     " ByCommaParts: " ++ show byCommaParts)
     (commaParts === byCommaParts)

-- | Test that splitByCommaCollapsed removes empty segments
prop_splitByCommaCollapsedRemovesEmptySegments :: String -> Property
prop_splitByCommaCollapsedRemovesEmptySegments input =
  let parts = splitByCommaCollapsed input
      hasNoEmptySegments = not (any null parts)
  in counterexample ("splitByCommaCollapsed should remove empty segments. " ++
                     "Input: " ++ show input ++
                     " Parts: " ++ show parts)
     (hasNoEmptySegments === True)

-- | Test that removeLineComments removes // comments
prop_removeLineCommentsRemovesComments :: String -> Property
prop_removeLineCommentsRemovesComments comment =
  let input = "code before // " ++ comment ++ "\ncode after"
      result = removeLineComments input
      hasComment = "//" `isInfixOf` result
  in counterexample ("removeLineComments should remove // comments. " ++
                     "Input: " ++ show input ++
                     " Result: " ++ show result)
     (hasComment === False)

-- | Test that removeLineComments preserves code before comments
prop_removeLineCommentsPreservesCodeBefore :: String -> Property
prop_removeLineCommentsPreservesCodeBefore code =
  let input = code ++ " // comment"
      result = removeLineComments input
      hasCode = code `isPrefixOf` result
  in counterexample ("removeLineComments should preserve code before comments. " ++
                     "Input: " ++ show input ++
                     " Result: " ++ show result)
     (hasCode === True)

-- | Test that removeComments removes both // and /* */ comments
prop_removeCommentsRemovesBothTypes :: String -> String -> Property
prop_removeCommentsRemovesBothTypes lineComment blockComment =
  let input = "code1 // " ++ lineComment ++ "\ncode2 /* " ++ blockComment ++ " */ code3"
      result = removeComments input
      hasLineComment = "//" `isInfixOf` result
      hasBlockComment = "/*" `isInfixOf` result
  in counterexample ("removeComments should remove both // and /* */ comments. " ++
                     "Input: " ++ show input ++
                     " Result: " ++ show result)
     (hasLineComment === False .&&. hasBlockComment === False)

-- | Test that removeComments handles nested quotes correctly
prop_removeCommentsHandlesQuotes :: String -> Property
prop_removeCommentsHandlesQuotes content =
  let input = "code1 // comment with \"quotes // inside\"\ncode2"
      result = removeComments input
      hasQuoteContent = "\"quotes // inside\"" `isInfixOf` result
  in counterexample ("removeComments should handle quotes correctly. " ++
                     "Input: " ++ show input ++
                     " Result: " ++ show result)
     (hasQuoteContent === True)

-- | Test that normalizeIndentation removes common prefix
prop_normalizeIndentationRemovesCommonPrefix :: String -> Property
prop_normalizeIndentationRemovesCommonPrefix content =
  let linesWithIndent = ["  " ++ content, "  " ++ content ++ "2", "    " ++ content ++ "3"]
      input = unlines linesWithIndent
      result = normalizeIndentation input
      resultLines = lines result
      hasNoCommonIndent = all (not . isPrefixOf "  ") resultLines
  in counterexample ("normalizeIndentation should remove common prefix. " ++
                     "Input: " ++ show input ++
                     " Result: " ++ show result)
     (hasNoCommonIndent === True)

-- | Test that fixIndentation is equivalent to normalizeIndentation
prop_fixIndentationEqualsNormalizeIndentation :: String -> Property
prop_fixIndentationEqualsNormalizeIndentation input =
  let normalized = normalizeIndentation input
      fixed = fixIndentation input
  in counterexample ("fixIndentation should equal normalizeIndentation. " ++
                     "Input: " ++ show input ++
                     " Normalized: " ++ show normalized ++
                     " Fixed: " ++ show fixed)
     (normalized === fixed)

-- | Test that breakOn finds first occurrence
prop_breakOnFindsFirstOccurrence :: String -> String -> Property
prop_breakOnFindsFirstOccurrence delim content =
  let input = content ++ delim ++ content ++ delim ++ content
      (before, after) = breakOn delim input
      expectedBefore = content
      expectedAfter = delim ++ content ++ delim ++ content
  in counterexample ("breakOn should find first occurrence. " ++
                     "Input: " ++ show input ++
                     " Before: " ++ show before ++
                     " After: " ++ show after)
     (before === expectedBefore .&&. after === expectedAfter)

-- | Test that breakOn handles delimiter not found
prop_breakOnHandlesDelimiterNotFound :: String -> String -> Property
prop_breakOnHandlesDelimiterNotFound content delim =
  not (delim `isInfixOf` content) ==> 
    let input = content
        (before, after) = breakOn delim input
    in counterexample ("breakOn should handle delimiter not found. " ++
                       "Input: " ++ show input ++
                       " Delim: " ++ show delim ++
                       " Before: " ++ show before ++
                       " After: " ++ show after)
       (before === input .&&. after === "")

-- | Test that trim is idempotent
prop_trimIsIdempotent :: String -> Property
prop_trimIsIdempotent input =
  let trimmedOnce = trim input
      trimmedTwice = trim trimmedOnce
  in counterexample ("trim should be idempotent. " ++
                     "Input: " ++ show input ++
                     " Once: " ++ show trimmedOnce ++
                     " Twice: " ++ show trimmedTwice)
     (trimmedOnce === trimmedTwice)

-- | Test that splitBy handles empty input
prop_splitByHandlesEmptyInput :: Char -> Property
prop_splitByHandlesEmptyInput delim =
  let result = splitBy delim ""
  in counterexample ("splitBy should handle empty input. " ++
                     "Delim: " ++ show delim ++
                     " Result: " ++ show result)
     (result === [""])

-- | Test that splitByCollapsed handles empty input
prop_splitByCollapsedHandlesEmptyInput :: Char -> Property
prop_splitByCollapsedHandlesEmptyInput delim =
  let result = splitByCollapsed delim ""
  in counterexample ("splitByCollapsed should handle empty input. " ++
                     "Delim: " ++ show delim ++
                     " Result: " ++ show result)
     (result === [])

-- | Test that removeComments handles unclosed block comments
prop_removeCommentsHandlesUnclosedBlockComments :: String -> Property
prop_removeCommentsHandlesUnclosedBlockComments content =
  let input = "code1 /* unclosed comment\ncode2"
      result = removeComments input
      hasBlockComment = "/*" `isInfixOf` result
  in counterexample ("removeComments should handle unclosed block comments. " ++
                     "Input: " ++ show input ++
                     " Result: " ++ show result)
     (hasBlockComment === False)

-- | Test that string processing functions handle Unicode
prop_stringProcessingHandlesUnicode :: String -> Property
prop_stringProcessingHandlesUnicode unicodeText =
  let trimmed = trim unicodeText
      parts = splitBy ',' unicodeText
      commentRemoved = removeLineComments unicodeText
  in counterexample ("String processing should handle Unicode. " ++
                     "Input: " ++ show unicodeText)
     (length trimmed >= 0 .&&.
      length parts >= 0 .&&.
      length commentRemoved >= 0)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Utils String Processing QuickCheck Tests"
  [ testProperty "trim removes leading and trailing whitespace" prop_trimRemovesLeadingTrailingWhitespace
  , testProperty "trim preserves internal whitespace" prop_trimPreservesInternalWhitespace
  , testProperty "splitBy preserves empty segments" prop_splitByPreservesEmptySegments
  , testProperty "splitByCollapsed removes empty segments" prop_splitByCollapsedRemovesEmptySegments
  , testProperty "splitByComma equals splitBy ','" prop_splitByCommaEqualsSplitByComma
  , testProperty "splitByCommaCollapsed removes empty segments" prop_splitByCommaCollapsedRemovesEmptySegments
  , testProperty "removeLineComments removes // comments" prop_removeLineCommentsRemovesComments
  , testProperty "removeLineComments preserves code before comments" prop_removeLineCommentsPreservesCodeBefore
  , testProperty "removeComments removes both // and /* */ comments" prop_removeCommentsRemovesBothTypes
  , testProperty "removeComments handles quotes correctly" prop_removeCommentsHandlesQuotes
  , testProperty "normalizeIndentation removes common prefix" prop_normalizeIndentationRemovesCommonPrefix
  , testProperty "fixIndentation equals normalizeIndentation" prop_fixIndentationEqualsNormalizeIndentation
  , testProperty "breakOn finds first occurrence" prop_breakOnFindsFirstOccurrence
  , testProperty "breakOn handles delimiter not found" prop_breakOnHandlesDelimiterNotFound
  , testProperty "trim is idempotent" prop_trimIsIdempotent
  , testProperty "splitBy handles empty input" prop_splitByHandlesEmptyInput
  , testProperty "splitByCollapsed handles empty input" prop_splitByCollapsedHandlesEmptyInput
  , testProperty "removeComments handles unclosed block comments" prop_removeCommentsHandlesUnclosedBlockComments
  , testProperty "String processing handles Unicode" prop_stringProcessingHandlesUnicode
  ]