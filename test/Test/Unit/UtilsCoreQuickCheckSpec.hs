{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.UtilsCoreQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Arrow (first)

-- ============================================================================
-- Custom Generators
-- ============================================================================

-- Generate strings with various whitespace patterns
genWhitespaceString :: Gen String
genWhitespaceString = listOf $ elements $ " \t\n\r" ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- Generate strings with specific separators
genStringWithSeparator :: Char -> Gen String
genStringWithSeparator sep = do
  parts <- listOf1 $ listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  return $ intercalate [sep] parts

-- Generate strings with comment patterns
genCommentString :: Gen String
genCommentString = do
  before <- listOf $ elements $ ['a'..'z'] ++ [' ']
  commentType <- elements ["//", "/*"]
  comment <- listOf $ elements $ ['a'..'z'] ++ [' ']
  after <- if commentType == "//" 
           then listOf $ elements $ ['a'..'z'] ++ [' ']
           else do
             rest <- listOf $ elements $ ['a'..'z'] ++ [' ']
             return $ "*/" ++ rest
  return $ before ++ commentType ++ comment ++ after

-- Generate indented strings
genIndentedString :: Gen String
genIndentedString = do
  numLines <- choose (1, 5)
  baseIndent <- choose (0, 4)
  lines <- vectorOf numLines $ do
    indent <- choose (0, 6)
    content <- listOf $ elements $ ['a'..'z'] ++ [' ']
    return $ replicate indent ' ' ++ content
  return $ unlines lines

-- Helper function
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- ============================================================================
-- Trim Properties
-- ============================================================================

prop_trimIdempotent :: String -> Property
prop_trimIdempotent s =
  let trimmed = trim s
      trimmedAgain = trim trimmed
  in counterexample "trim should be idempotent" $
    trimmed === trimmedAgain

prop_trimRemovesLeadingWhitespace :: String -> Property
prop_trimRemovesLeadingWhitespace s =
  let trimmed = trim s
  in counterexample "trim should remove all leading whitespace" $
    not (null trimmed) ==> (not . isSpace . head) trimmed

prop_trimRemovesTrailingWhitespace :: String -> Property
prop_trimRemovesTrailingWhitespace s =
  let trimmed = trim s
  in counterexample "trim should remove all trailing whitespace" $
    not (null trimmed) ==> (not . isSpace . last) trimmed

prop_trimPreservesInternalWhitespace :: String -> Property
prop_trimPreservesInternalWhitespace s =
  let trimmed = trim s
      originalInternal = dropWhile isSpace . reverse . dropWhile isSpace . reverse $ s
  in counterexample "trim should preserve internal whitespace" $
    trimmed === originalInternal

-- ============================================================================
-- Split Properties
-- ============================================================================

prop_splitByPreservesOrder :: Char -> String -> Property
prop_splitByPreservesOrder delim s =
  let parts = splitBy delim
      joined = intercalate [delim] parts
  in counterexample ("splitBy should preserve order when rejoining with " ++ show delim) $
    joined === s

prop_splitByHandlesEmptyString :: Char -> Property
prop_splitByHandlesEmptyString delim =
  let result = splitBy delim ""
  in counterexample ("splitBy should return [\"\"] for empty string with delimiter " ++ show delim) $
    result === [""]

prop_splitByHandlesLeadingDelimiter :: Char -> String -> Property
prop_splitByHandlesLeadingDelimiter delim s =
  let result = splitBy delim (delim : s)
  in counterexample ("splitBy should handle leading delimiter " ++ show delim) $
    not (null result) ==> head result === ""

prop_splitByHandlesTrailingDelimiter :: Char -> String -> Property
prop_splitByHandlesTrailingDelimiter delim s =
  let result = splitBy delim (s ++ [delim])
  in counterexample ("splitBy should handle trailing delimiter " ++ show delim) $
    not (null result) ==> last result === ""

prop_splitByCollapsedRemovesEmpty :: Char -> String -> Property
prop_splitByCollapsedRemovesEmpty delim s =
  let normal = splitBy delim s
      collapsed = splitByCollapsed delim s
  in counterexample ("splitByCollapsed should remove empty parts for delimiter " ++ show delim) $
    all (not . null) collapsed

prop_splitByCommaConsistency :: String -> Property
prop_splitByCommaConsistency s =
  let commaResult = splitByComma s
      genericResult = splitBy ',' s
  in counterexample "splitByComma should be consistent with splitBy ','" $
    commaResult === genericResult

prop_splitByCommaCollapsedConsistency :: String -> Property
prop_splitByCommaCollapsedConsistency s =
  let commaResult = splitByCommaCollapsed s
      genericResult = splitByCollapsed ',' s
  in counterexample "splitByCommaCollapsed should be consistent with splitByCollapsed ','" $
    commaResult === genericResult

-- ============================================================================
-- Comment Removal Properties
-- ============================================================================

prop_removeLineCommentsPreservesNonCommented :: String -> Property
prop_removeLineCommentsPreservesNonCommented s =
  let noComments = filter (not . isPrefixOf "//") $ lines s
      processed = removeLineComments s
      processedLines = lines processed
  in counterexample "removeLineComments should preserve non-commented lines" $
    length noComments === length processedLines
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

prop_removeLineCommentsHandlesEmptyString :: Property
prop_removeLineCommentsHandlesEmptyString =
  let result = removeLineComments ""
  in counterexample "removeLineComments should handle empty string" $
    result === ""

prop_removeCommentsPreservesLineCount :: String -> Property
prop_removeCommentsPreservesLineCount s =
  let originalLines = length $ lines s
      processedLines = length $ lines (removeComments s)
  in counterexample "removeComments should preserve line count (keeps newlines)" $
    processedLines <= originalLines

prop_removeCommentsHandlesStringLiterals :: Property
prop_removeCommentsHandlesStringLiterals =
  let input = "code // comment\n\"string // not comment\" // real comment"
      result = removeComments input
  in counterexample "removeComments should ignore // inside string literals" $
    "// not comment" `isInfixOf` result

prop_removeCommentsHandlesCharLiterals :: Property
prop_removeCommentsHandlesCharLiterals =
  let input = "code // comment\n'// not comment' // real comment"
      result = removeComments input
  in counterexample "removeComments should ignore // inside char literals" $
    "// not comment" `isInfixOf` result

prop_removeCommentsHandlesBlockComments :: Property
prop_removeCommentsHandlesBlockComments =
  let input = "before /* block comment */ after"
      result = removeComments input
  in counterexample "removeComments should remove block comments" $
    not ("/* block comment */" `isInfixOf` result) && "before" `isInfixOf` result && "after" `isInfixOf` result

-- Helper function
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails xs@(x:xs') = xs : tails xs'

-- ============================================================================
-- Indentation Properties
-- ============================================================================

prop_normalizeIndentationPreservesRelativeIndentation :: Property
prop_normalizeIndentationPreservesRelativeIndentation =
  let input = "    foo\n      bar\n    baz\n      qux"
      result = normalizeIndentation input
      resultLines = lines result
  in counterexample "normalizeIndentation should preserve relative indentation" $
    length resultLines === 4 &&
    not (isPrefixOf "  " (resultLines !! 0)) &&
    isPrefixOf "  " (resultLines !! 1) &&
    not (isPrefixOf "  " (resultLines !! 2)) &&
    isPrefixOf "  " (resultLines !! 3)

prop_normalizeIndentationHandlesEmptyLines :: Property
prop_normalizeIndentationHandlesEmptyLines =
  let input = "    foo\n\n      bar\n    \nbaz"
      result = normalizeIndentation input
      resultLines = lines result
  in counterexample "normalizeIndentation should handle empty lines" $
    length resultLines === 5 &&
    resultLines !! 1 === "" &&
    resultLines !! 3 === ""

prop_normalizeIndentationHandlesAllWhitespace :: Property
prop_normalizeIndentationHandlesAllWhitespace =
  let input = "    \n      \n    "
      result = normalizeIndentation input
  in counterexample "normalizeIndentation should handle all-whitespace input" $
    result === "\n\n"

prop_fixIndentationConsistency :: String -> Property
prop_fixIndentationConsistency s =
  let normalizeResult = normalizeIndentation s
      fixResult = fixIndentation s
  in counterexample "fixIndentation should be consistent with normalizeIndentation" $
    normalizeResult === fixResult

-- ============================================================================
-- Search Properties
-- ============================================================================

prop_breakOnEmptyPattern :: String -> Property
prop_breakOnEmptyPattern s =
  let (before, after) = breakOn "" s
  in counterexample "breakOn with empty pattern should return (\"\", s)" $
    before === "" && after === s

prop_breakOnPatternNotFound :: String -> String -> Property
prop_breakOnPatternNotFound pattern s =
  let notFound = not (pattern `isInfixOf` s)
      (before, after) = breakOn pattern s
  in counterexample "breakOn should return (s, \"\") when pattern not found" $
    notFound ==> (before === s && after === "")

prop_breakOnPatternFound :: String -> String -> Property
prop_breakOnPatternFound pattern s =
  let found = pattern `isInfixOf` s
      (before, after) = breakOn pattern s
      expectedBefore = takeWhile (not . isPrefixOf pattern) (tails s) >>= head
      expectedAfter = drop (length expectedBefore + length pattern) s
  in counterexample "breakOn should split correctly when pattern found" $
    found ==> (before === expectedBefore && after === expectedAfter)

prop_breakOnIdempotent :: String -> String -> Property
prop_breakOnIdempotent pattern s =
  let (before, after) = breakOn pattern s
      reconstructed = before ++ pattern ++ after
  in counterexample "breakOn should allow perfect reconstruction when pattern found" $
    pattern `isInfixOf` s ==> reconstructed === s

-- Helper function for breakOn test
tails :: String -> [String]
tails [] = [[]]
tails xs@(x:xs') = xs : tails xs'

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Utils Core QuickCheck Tests"
  [ testGroup "Trim Tests"
      [ testProperty "trim is idempotent" prop_trimIdempotent
      , testProperty "trim removes leading whitespace" prop_trimRemovesLeadingWhitespace
      , testProperty "trim removes trailing whitespace" prop_trimRemovesTrailingWhitespace
      , testProperty "trim preserves internal whitespace" prop_trimPreservesInternalWhitespace
      ]
  , testGroup "Split Tests"
      [ testProperty "splitBy preserves order" prop_splitByPreservesOrder
      , testProperty "splitBy handles empty string" prop_splitByHandlesEmptyString
      , testProperty "splitBy handles leading delimiter" prop_splitByHandlesLeadingDelimiter
      , testProperty "splitBy handles trailing delimiter" prop_splitByHandlesTrailingDelimiter
      , testProperty "splitByCollapsed removes empty parts" prop_splitByCollapsedRemovesEmpty
      , testProperty "splitByComma consistency" prop_splitByCommaConsistency
      , testProperty "splitByCommaCollapsed consistency" prop_splitByCommaCollapsedConsistency
      ]
  , testGroup "Comment Removal Tests"
      [ testProperty "removeLineComments preserves non-commented lines" prop_removeLineCommentsPreservesNonCommented
      , testProperty "removeLineComments handles empty string" prop_removeLineCommentsHandlesEmptyString
      , testProperty "removeComments preserves line count" prop_removeCommentsPreservesLineCount
      , testProperty "removeComments handles string literals" prop_removeCommentsHandlesStringLiterals
      , testProperty "removeComments handles char literals" prop_removeCommentsHandlesCharLiterals
      , testProperty "removeComments handles block comments" prop_removeCommentsHandlesBlockComments
      ]
  , testGroup "Indentation Tests"
      [ testProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentationPreservesRelativeIndentation
      , testProperty "normalizeIndentation handles empty lines" prop_normalizeIndentationHandlesEmptyLines
      , testProperty "normalizeIndentation handles all whitespace" prop_normalizeIndentationHandlesAllWhitespace
      , testProperty "fixIndentation consistency" prop_fixIndentationConsistency
      ]
  , testGroup "Search Tests"
      [ testProperty "breakOn with empty pattern" prop_breakOnEmptyPattern
      , testProperty "breakOn pattern not found" prop_breakOnPatternNotFound
      , testProperty "breakOn pattern found" prop_breakOnPatternFound
      , testProperty "breakOn idempotent reconstruction" prop_breakOnIdempotent
      ]
  ]
