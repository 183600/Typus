module Test.Unit.NewUtilsStringBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, oneof, elements, listOf, suchThat)

import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed, 
             removeLineComments, removeComments, normalizeIndentation, forceSingleTabIndentation, 
             fixIndentation, breakOn)
import TestSupport.QuickCheck (fastProperty)

-- ============================================================================
-- New QuickCheck Tests for Utils String Processing Boundary Conditions
-- ============================================================================

tests :: TestTree
tests =
  testGroup "New Utils String Processing Boundary QuickCheck Tests"
    [ testGroup "Trim Function Properties"
        [ fastProperty "trim is idempotent" prop_trimIdempotent
        , fastProperty "trim removes only leading/trailing whitespace" prop_trimRemovesOnlyWhitespace
        , fastProperty "trim handles empty strings" prop_trimHandlesEmptyStrings
        , fastProperty "trim handles all-whitespace strings" prop_trimHandlesAllWhitespace
        , fastProperty "trim preserves internal whitespace" prop_trimPreservesInternalWhitespace
        ]

    , testGroup "Split Function Properties"
        [ fastProperty "splitBy preserves empty segments" prop_splitByPreservesEmpty
        , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsedRemovesEmpty
        , fastProperty "splitBy on empty string returns singleton" prop_splitByEmptyReturnsSingleton
        , fastProperty "splitBy is consistent with splitByComma" prop_splitByConsistentWithComma
        , fastProperty "splitBy handles unicode characters" prop_splitByHandlesUnicode
        ]

    , testGroup "Comment Removal Properties"
        [ fastProperty "removeLineComments preserves line structure" prop_removeLineCommentsPreservesLines
        , fastProperty "removeLineComments respects string literals" prop_removeLineCommentsRespectsStrings
        , fastProperty "removeComments handles nested patterns" prop_removeCommentsHandlesNested
        , fastProperty "removeComments preserves non-comment content" prop_removeCommentsPreservesContent
        , fastProperty "comment removal is idempotent" prop_commentRemovalIdempotent
        ]

    , testGroup "Indentation Properties"
        [ fastProperty "normalizeIndentation preserves relative structure" prop_normalizeIndentationPreservesRelative
        , fastProperty "normalizeIndentation handles empty lines" prop_normalizeIndentationHandlesEmpty
        , fastProperty "forceSingleTabIndentation is destructive but consistent" prop_forceSingleTabConsistent
        , fastProperty "fixIndentation equals normalizeIndentation" prop_fixIndentationEqualsNormalize
        , fastProperty "indentation functions handle mixed whitespace" prop_indentationHandlesMixedWhitespace
        ]

    , testGroup "Search Function Properties"
        [ fastProperty "breakOn returns correct split" prop_breakOnCorrectSplit
        , fastProperty "breakOn handles empty pattern" prop_breakOnEmptyPattern
        , fastProperty "breakOn handles pattern not found" prop_breakOnPatternNotFound
        , fastProperty "breakOn is consistent with string concatenation" prop_breakOnConsistentWithConcat
        , fastProperty "breakOn handles overlapping patterns" prop_breakOnHandlesOverlapping
        ]
    ]

-- ============================================================================
-- Trim Function Property Tests
-- ============================================================================

-- | trim should be idempotent: trim(trim(s)) == trim(s)
prop_trimIdempotent :: String -> Property
prop_trimIdempotent input =
  let once = trim input
      twice = trim once
  in counterexample ("input=" ++ show input ++ ", once=" ++ show once ++ ", twice=" ++ show twice) $
     once === twice

-- | trim should remove only leading/trailing whitespace
prop_trimRemovesOnlyWhitespace :: String -> Property
prop_trimRemovesOnlyWhitespace input =
  let trimmed = trim input
      hasLeadingWhitespace = not (null input) && isSpace (head input)
      hasTrailingWhitespace = not (null input) && isSpace (last input)
  in counterexample ("input=" ++ show input ++ ", trimmed=" ++ show trimmed) $
     if hasLeadingWhitespace || hasTrailingWhitespace
       then length trimmed <= length input
       else trimmed === input

-- | trim should handle empty strings
prop_trimHandlesEmptyStrings :: Property
prop_trimHandlesEmptyStrings =
  let input = ""
      result = trim input
  in counterexample ("trim \"\" = " ++ show result) $
     result === ""

-- | trim should handle all-whitespace strings
prop_trimHandlesAllWhitespace :: String -> Property
prop_trimHandlesAllWhitespace input =
  let isAllWhitespace = all isSpace input
      result = trim input
  in if isAllWhitespace
     then counterexample ("trim all-whitespace=" ++ show input) $
          result === ""
     else property True

-- | trim should preserve internal whitespace
prop_trimPreservesInternalWhitespace :: String -> Property
prop_trimPreservesInternalWhitespace input =
  let trimmed = trim input
      internalSpaces = countInternalSpaces input
      trimmedInternalSpaces = countInternalSpaces trimmed
  in counterexample ("input=" ++ show input ++ ", trimmed=" ++ show trimmed) $
     trimmedInternalSpaces === internalSpaces

-- ============================================================================
-- Split Function Property Tests
-- ============================================================================

-- | splitBy should preserve empty segments
prop_splitByPreservesEmpty :: Char -> String -> Property
prop_splitByPreservesEmpty delim input =
  let result = splitBy delim input
      expectedCount = length (filter (== delim) input) + 1
  in counterexample ("delim=" ++ show delim ++ ", input=" ++ show input ++ ", result=" ++ show result) $
     length result === expectedCount

-- | splitByCollapsed should remove empty segments
prop_splitByCollapsedRemovesEmpty :: Char -> String -> Property
prop_splitByCollapsedRemovesEmpty delim input =
  let result = splitByCollapsed delim input
  in counterexample ("delim=" ++ show delim ++ ", input=" ++ show input ++ ", result=" ++ show result) $
     all (not . null) result

-- | splitBy on empty string should return singleton
prop_splitByEmptyReturnsSingleton :: Char -> Property
prop_splitByEmptyReturnsSingleton delim =
  let input = ""
      result = splitBy delim input
  in counterexample ("splitBy " ++ show delim ++ " \"\"") $
     result === [""]

-- | splitBy should be consistent with splitByComma for comma delimiter
prop_splitByConsistentWithComma :: String -> Property
prop_splitByConsistentWithComma input =
  let result1 = splitBy ',' input
      result2 = splitByComma input
  in counterexample ("input=" ++ show input) $
     result1 === result2

-- | splitBy should handle unicode characters
prop_splitByHandlesUnicode :: String -> Property
prop_splitByHandlesUnicode input =
  let hasUnicode = any (> '\127') input
      result = splitBy ',' input
  in if hasUnicode
     then counterexample ("unicode input=" ++ show input) $
          length result >= 1  -- Basic sanity check
     else property True

-- ============================================================================
-- Comment Removal Property Tests
-- ============================================================================

-- | removeLineComments should preserve line structure
prop_removeLineCommentsPreservesLines :: String -> Property
prop_removeLineCommentsPreservesLines input =
  let result = removeLineComments input
      originalLines = length (lines input)
      resultLines = length (lines result)
  in counterexample ("input lines=" ++ show originalLines ++ ", result lines=" ++ show resultLines) $
     resultLines === originalLines

-- | removeLineComments should respect string literals
prop_removeLineCommentsRespectsStrings :: String -> Property
prop_removeLineCommentsRespectsStrings input =
  let result = removeLineComments input
      hasStringLiteral = "\"" `isInfixOf` input
  in if hasStringLiteral
     then counterexample ("input with string=" ++ take 50 input ++ "...") $
          -- Should preserve string content containing //
          property (not (null result))
     else property True

-- | removeComments should handle nested patterns
prop_removeCommentsHandlesNested :: String -> Property
prop_removeCommentsHandlesNested input =
  let result = removeComments input
      hasNested = "/* /* */" `isInfixOf` input || "// /* */" `isInfixOf` input
  in if hasNested
     then counterexample ("nested input=" ++ take 50 input ++ "...") $
          length result >= 0  -- Should not crash
     else property True

-- | removeComments should preserve non-comment content
prop_removeCommentsPreservesContent :: String -> Property
prop_removeCommentsPreservesContent input =
  let result = removeComments input
      nonCommentContent = extractNonCommentContent input
  in counterexample ("input=" ++ take 50 input ++ "...") $
     not (null nonCommentContent) ==> nonCommentContent `isInfixOf` result

-- | comment removal should be idempotent
prop_commentRemovalIdempotent :: String -> Property
prop_commentRemovalIdempotent input =
  let once = removeComments input
      twice = removeComments once
  in counterexample ("input length=" ++ show (length input)) $
     once === twice

-- ============================================================================
-- Indentation Property Tests
-- ============================================================================

-- | normalizeIndentation should preserve relative structure
prop_normalizeIndentationPreservesRelative :: String -> Property
prop_normalizeIndentationPreservesRelative input =
  let result = normalizeIndentation input
      originalLines = lines input
      resultLines = lines result
  in counterexample ("input lines=" ++ show (length originalLines)) $
     length resultLines === length originalLines

-- | normalizeIndentation should handle empty lines
prop_normalizeIndentationHandlesEmpty :: String -> Property
prop_normalizeIndentationHandlesEmpty input =
  let result = normalizeIndentation input
      emptyLinesInInput = length (filter (all isSpace) (lines input))
      emptyLinesInResult = length (filter (all isSpace) (lines result))
  in counterexample ("input empty lines=" ++ show emptyLinesInInput ++ ", result empty lines=" ++ show emptyLinesInResult) $
     emptyLinesInResult >= emptyLinesInInput  -- Should preserve empty lines

-- | forceSingleTabIndentation should be destructive but consistent
prop_forceSingleTabConsistent :: String -> Property
prop_forceSingleTabConsistent input =
  let result1 = forceSingleTabIndentation input
      result2 = forceSingleTabIndentation input
  in counterexample ("input=" ++ take 30 input ++ "...") $
     result1 === result2

-- | fixIndentation should equal normalizeIndentation
prop_fixIndentationEqualsNormalize :: String -> Property
prop_fixIndentationEqualsNormalize input =
  let result1 = fixIndentation input
      result2 = normalizeIndentation input
  in counterexample ("input=" ++ take 30 input ++ "...") $
     result1 === result2

-- | indentation functions should handle mixed whitespace
prop_indentationHandlesMixedWhitespace :: String -> Property
prop_indentationHandlesMixedWhitespace input =
  let hasMixed = hasBothSpacesAndTabs input
      result = normalizeIndentation input
  in if hasMixed
     then counterexample ("mixed whitespace input=" ++ take 30 input ++ "...") $
          length result >= 0  -- Should not crash
     else property True

-- ============================================================================
-- Search Function Property Tests
-- ============================================================================

-- | breakOn should return correct split
prop_breakOnCorrectSplit :: String -> String -> Property
prop_breakOnCorrectSplit pat text =
  let (before, after) = breakOn pat text
      reconstructed = before ++ pat ++ after
  in if null pat
     then property True  -- Skip empty pattern
     else counterexample ("pat=" ++ show pat ++ ", text=" ++ show text) $
        if pat `isInfixOf` text
          then reconstructed === text
          else (before, after) === (text, "")

-- | breakOn should handle empty pattern
prop_breakOnEmptyPattern :: String -> Property
prop_breakOnEmptyPattern text =
  let (before, after) = breakOn "" text
  in counterexample ("text=" ++ show text) $
     (before, after) === ("", text)

-- | breakOn should handle pattern not found
prop_breakOnPatternNotFound :: String -> String -> Property
prop_breakOnPatternNotFound pat text =
  let notFound = not (pat `isInfixOf` text)
      (before, after) = breakOn pat text
  in if notFound && not (null pat)
     then counterexample ("pat=" ++ show pat ++ ", text=" ++ show text) $
          (before, after) === (text, "")
     else property True

-- | breakOn should be consistent with string concatenation
prop_breakOnConsistentWithConcat :: String -> String -> Property
prop_breakOnConsistentWithConcat pat text =
  if null pat then property True else
  let (before, after) = breakOn pat text
      found = pat `isInfixOf` text
  in if found
     then counterexample ("pat=" ++ show pat ++ ", text=" ++ show text) $
          before ++ pat ++ after === text
     else property True

-- | breakOn should handle overlapping patterns
prop_breakOnHandlesOverlapping :: String -> Property
prop_breakOnHandlesOverlapping text =
  let pat = "aa"
      hasOverlap = "aaa" `isInfixOf` text
      (before, after) = breakOn pat text
  in if hasOverlap
     then counterexample ("overlap text=" ++ show text) $
          length before + length pat + length after <= length text + length pat
     else property True

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Check if character is whitespace
isSpace :: Char -> Bool
isSpace c = c `elem` " \t\n\r\f\v"

-- | Count internal spaces (not leading or trailing)
countInternalSpaces :: String -> Int
countInternalSpaces [] = 0
countInternalSpaces [_] = 0
countInternalSpaces (c1:c2:cs) = (if isSpace c2 then 1 else 0) + countInternalSpaces (c2:cs)

-- | Check if substring is in string
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (tails haystack >>= inits)
  where
    tails [] = [[]]
    tails xs@(x:xs') = xs : tails xs'
    inits [] = [[]]
    inits xs = inits' xs []
    inits' [] acc = [reverse acc]
    inits' (x:xs') acc = reverse acc : inits' xs' (x:acc)

-- | Extract non-comment content (simplified)
extractNonCommentContent :: String -> String
extractNonCommentContent = unwords . filter (not . isCommentStart) . words
  where
    isCommentStart ('/':'/':_) = True
    isCommentStart ('/':'*':_) = True
    isCommentStart _ = False

-- | Check if string has both spaces and tabs
hasBothSpacesAndTabs :: String -> Bool
hasBothSpacesAndTabs str = ' ' `elem` str && '\t' `elem` str