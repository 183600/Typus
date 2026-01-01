module Test.Unit.NewUtilsStringPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Property, (===), Arbitrary(..), Gen, choose, listOf, elements, forAll, oneof, suchThat)

import Utils
  ( trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed
  , removeLineComments, removeComments, normalizeIndentation, forceSingleTabIndentation
  , fixIndentation, breakOn
  )
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (intercalate)
import qualified Data.Text as T

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate strings with various whitespace patterns
genWhitespaceString :: Gen String
genWhitespaceString = listOf $ elements " \t\n\r"

-- Generate regular characters (excluding comment symbols for safety)
genRegularChar :: Gen Char
genRegularChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ".,;:(){}[]+-*/="

-- Generate strings with regular characters
genRegularString :: Gen String
genRegularString = listOf genRegularChar

-- Generate delimiter characters for splitting
genDelimiter :: Gen Char
genDelimiter = elements ",;:|"

-- Generate strings that may contain the delimiter
genStringWithDelimiter :: Gen String
genStringWithDelimiter = do
  delim <- genDelimiter
  parts <- listOf genRegularString
  pure $ intercalate [delim] parts

-- Generate strings with potential comment patterns
genCommentString :: Gen String
genCommentString = do
  base <- genRegularString
  comment <- genRegularString
  oneof
    [ pure $ base ++ " // " ++ comment
    , pure $ base ++ " /* " ++ comment ++ " */ " ++ base
    , pure $ "// " ++ comment ++ "\n" ++ base
    , pure $ "/* " ++ comment ++ " */\n" ++ base
    , pure base  -- No comment
    ]

-- Generate strings with quote literals
genQuotedString :: Gen String
genQuotedString = do
  content <- genRegularString
  oneof
    [ pure $ "\"" ++ content ++ "\""
    , pure $ "'" ++ content ++ "'"
    , pure $ "\"" ++ content ++ " // not a comment\""
    , pure $ "'" ++ content ++ " /* not a comment*/'"
    ]

-- Generate indented strings
genIndentedString :: Gen String
genIndentedString = do
  baseIndent <- choose (0, 8)
  lineIndent <- choose (-2, 5)  -- Can be less L.or more than base
  content <- genRegularString
  let indentStr = replicate (max 0 (baseIndent + lineIndent)) ' '
  pure $ indentStr ++ content

-- Generate multi-line indented content
genMultiLineIndented :: Gen String
genMultiLineIndented = do
  numLines <- choose (1, 5)
  lines <- listOf genIndentedString
  pure $ unlines lines

-- Generate patterns for breakOn
genBreakPattern :: Gen String
genBreakPattern = oneof
  [ listOf1 genRegularChar
  , pure ""
  , pure " "
  , pure "\n"
  ]

-- ============================================================================
-- Property Tests for String Splitting
-- ============================================================================

-- Property: splitBy should preserve empty segments
prop_split_by_preserves_empty :: Property
prop_split_by_preserves_empty = 
  forAll genDelimiter $ \delim ->
    forAll (listOf (pure delim)) $ \delims ->
      let input = delims
          result = splitBy delim input
      in L.length result === L.length delims + 1

-- Property: splitByCollapsed should remove empty segments
prop_split_by_collapsed_removes_empty :: Property
prop_split_by_collapsed_removes_empty = 
  forAll genDelimiter $ \delim ->
    forAll (listOf (pure delim)) $ \delims ->
      let input = delims
          result = splitByCollapsed delim input
      in L.all (not . null) result

-- Property: splitBy L.and splitByComma should be equivalent for comma delimiter
prop_split_by_comma_equivalence :: Property
prop_split_by_comma_equivalence = 
  forAll genRegularString $ \input ->
    splitBy ',' input === splitByComma input

-- Property: splitByCollapsed L.and splitByCommaCollapsed should be equivalent for comma delimiter
prop_split_by_collapsed_comma_equivalence :: Property
prop_split_by_collapsed_comma_equivalence = 
  forAll genRegularString $ \input ->
    splitByCollapsed ',' input === splitByCommaCollapsed input

-- Property: splitBy followed by join should reconstruct original string (for non-empty delimiter)
prop_split_by_join_roundtrip :: Property
prop_split_by_join_roundtrip = 
  forAll genDelimiter $ \delim ->
    forAll genStringWithDelimiter $ \input ->
      let parts = splitBy delim input
          reconstructed = intercalate [delim] parts
      in reconstructed === input

-- ============================================================================
-- Property Tests for Comment Removal
-- ============================================================================

-- Property: removeLineComments should not affect strings without // comments
prop_remove_line_comments_no_effect :: Property
prop_remove_line_comments_no_effect = 
  forAll genRegularString $ \input ->
    let result = removeLineComments input
    in result === input

-- Property: removeLineComments should preserve line structure
prop_remove_line_comments_preserve_lines :: Property
prop_remove_line_comments_preserve_lines = 
  forAll genCommentString $ \input ->
    let result = removeLineComments input
        originalLines = lines input
        resultLines = lines result
    in L.length resultLines === L.length originalLines

-- Property: removeComments should handle nested quotes correctly
prop_remove_comments_nested_quotes :: Property
prop_remove_comments_nested_quotes = 
  forAll genQuotedString $ \input ->
    let result = removeComments input
    in input `L.isInfixOf` result || result === input

-- Property: removeLineComments should be idempotent
prop_remove_line_comments_idempotent :: Property
prop_remove_line_comments_idempotent = 
  forAll genCommentString $ \input ->
    let once = removeLineComments input
        twice = removeLineComments once
    in once === twice

-- Property: removeComments should be idempotent
prop_remove_comments_idempotent :: Property
prop_remove_comments_idempotent = 
  forAll genCommentString $ \input ->
    let once = removeComments input
        twice = removeComments once
    in once === twice

-- ============================================================================
-- Property Tests for Indentation
-- ============================================================================

-- Property: normalizeIndentation should preserve relative indentation differences
prop_normalize_indentation_preserve_relative :: Property
prop_normalize_indentation_preserve_relative = 
  forAll genMultiLineIndented $ \input ->
    let normalized = normalizeIndentation input
        inputLines = L.filter (not . L.all isSpace) (lines input)
        normLines = L.filter (not . L.all isSpace) (lines normalized)
        
        getIndent l = L.length $ takeWhile isSpace l
        
        relativeDiffs inputLines' = 
          case inputLines' of
            [] -> []
            (x:xs) -> L.map (\l -> getIndent l - getIndent x) xs
            
        normDiffs = relativeDiffs normLines
        inputDiffs = relativeDiffs inputLines
    in normDiffs === inputDiffs

-- Property: forceSingleTabIndentation should start non-empty lines with tab
prop_force_single_tab_indentation_structure :: Property
prop_force_single_tab_indentation_structure = 
  forAll genMultiLineIndented $ \input ->
    let result = forceSingleTabIndentation input
        resultLines = lines result
        nonEmptyLines = L.filter (not . null) resultLines
    in L.all ("\t" `L.isPrefixOf`) nonEmptyLines

-- Property: fixIndentation should be equivalent to normalizeIndentation
prop_fix_indentation_equivalence :: Property
prop_fix_indentation_equivalence = 
  forAll genMultiLineIndented $ \input ->
    fixIndentation input === normalizeIndentation input

-- Property: normalizeIndentation should not increase total line count
prop_normalize_indentation_line_count :: Property
prop_normalize_indentation_line_count = 
  forAll genMultiLineIndented $ \input ->
    let normalized = normalizeIndentation input
        originalLines = lines input
        normalizedLines = lines normalized
    in L.length normalizedLines <= L.length originalLines

-- ============================================================================
-- Property Tests for Search Functions
-- ============================================================================

-- Property: breakOn with empty pattern should return ("", input)
prop_break_on_empty_pattern :: Property
prop_break_on_empty_pattern = 
  forAll genRegularString $ \input ->
    breakOn "" input === ("", input)

-- Property: breakOn with non-existent pattern should return (input, "")
prop_break_on_nonexistent_pattern :: Property
prop_break_on_nonexistent_pattern = 
  forAll genRegularString $ \input ->
    forAll genBreakPattern $ \pattern ->
      let (before, after) = breakOn pattern input
      in if not (pattern `L.isInfixOf` input) && not (null pattern)
         then (before, after) === (input, "")
         else property True

-- Property: breakOn should concatenate to original string when pattern exists
prop_break_on_concatenation :: Property
prop_break_on_concatenation = 
  forAll genRegularString $ \input ->
    forAll genBreakPattern $ \pattern ->
      let (before, after) = breakOn pattern input
      in if pattern `L.isInfixOf` input && not (null pattern)
         then before ++ pattern ++ after === input
         else property True

-- Property: breakOn should find first occurrence
prop_break_on_first_occurrence :: Property
prop_break_on_first_occurrence = 
  forAll genRegularString $ \input ->
    forAll genBreakPattern $ \pattern ->
      let (before, after) = breakOn pattern input
      in if pattern `L.isInfixOf` input && not (null pattern)
         then not (pattern `L.isInfixOf` before)
         else property True

-- ============================================================================
-- Property Tests for Trim Function
-- ============================================================================

-- Property: trim should be idempotent
prop_trim_idempotent :: Property
prop_trim_idempotent = 
  forAll genWhitespaceString $ \ws ->
    forAll genRegularString $ \content ->
      let input = ws ++ content ++ ws
          once = trim input
          twice = trim once
      in once === twice

-- Property: trim should remove L.all leading L.and trailing whitespace
prop_trim_removes_whitespace :: Property
prop_trim_removes_whitespace = 
  forAll genWhitespaceString $ \ws1 ->
    forAll genRegularString $ \content ->
      forAll genWhitespaceString $ \ws2 ->
        let input = ws1 ++ content ++ ws2
            result = trim input
        in not (null result) ==> (L.head result `notElem` " \t\n\r") && (last result `notElem` " \t\n\r")

-- ============================================================================
-- Unit Tests
-- ============================================================================

test_complex_comment_removal :: IO ()
test_complex_comment_removal = do
  let input = unlines
        [ "value := 42 // line comment"
        , "str := \"text // not comment\" // real comment"
        , "char := '/' // another comment"
        , "block := \"text /* not block */\" // line comment"
        ]
      expected = unlines
        [ "value := 42 "
        , "str := \"text // not comment\" "
        , "char := '/' "
        , "block := \"text /* not block */\" "
        ]
  removeLineComments input @?= expected

test_block_comment_removal :: IO ()
test_block_comment_removal = do
  let input = "code /* block comment\nspanning multiple lines */ more code"
      expected = "code  more code"
  removeComments input @?= expected

test_indentation_normalization :: IO ()
test_indentation_normalization = do
  let input = unlines
        [ "    func main() {"
        , "        return 42"
        , "    }"
        ]
      expected = unlines
        [ "func main() {"
        , "    return 42"
        , "}"
        ]
  normalizeIndentation input @?= expected

test_break_on_examples :: IO ()
test_break_on_examples = do
  breakOn "ll" "hello" @?= ("he", "o")
  breakOn "xyz" "hello" @?= ("hello", "")
  breakOn "" "abc" @?= ("", "abc")
  breakOn "abc" "abc" @?= ("", "")

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Utils String Properties Tests"
  [ -- String splitting properties
    testProperty "Split by preserves empty segments" prop_split_by_preserves_empty
  , testProperty "Split by collapsed removes empty segments" prop_split_by_collapsed_removes_empty
  , testProperty "Split by comma equivalence" prop_split_by_comma_equivalence
  , testProperty "Split by collapsed comma equivalence" prop_split_by_collapsed_comma_equivalence
  , testProperty "Split by join roundtrip" prop_split_by_join_roundtrip
  
  -- Comment removal properties
  , testProperty "Remove line comments no effect" prop_remove_line_comments_no_effect
  , testProperty "Remove line comments preserve lines" prop_remove_line_comments_preserve_lines
  , testProperty "Remove comments nested quotes" prop_remove_comments_nested_quotes
  , testProperty "Remove line comments idempotent" prop_remove_line_comments_idempotent
  , testProperty "Remove comments idempotent" prop_remove_comments_idempotent
  
  -- Indentation properties
  , testProperty "Normalize indentation preserve relative" prop_normalize_indentation_preserve_relative
  , testProperty "Force single tab indentation structure" prop_force_single_tab_indentation_structure
  , testProperty "Fix indentation equivalence" prop_fix_indentation_equivalence
  , testProperty "Normalize indentation line count" prop_normalize_indentation_line_count
  
  -- Search function properties
  , testProperty "Break on empty pattern" prop_break_on_empty_pattern
  , testProperty "Break on nonexistent pattern" prop_break_on_nonexistent_pattern
  , testProperty "Break on concatenation" prop_break_on_concatenation
  , testProperty "Break on first occurrence" prop_break_on_first_occurrence
  
  -- Trim properties
  , testProperty "Trim idempotent" prop_trim_idempotent
  , testProperty "Trim removes whitespace" prop_trim_removes_whitespace
  
  -- Unit tests
  , testCase "Complex comment removal" test_complex_comment_removal
  , testCase "Block comment removal" test_block_comment_removal
  , testCase "Indentation normalization" test_indentation_normalization
  , testCase "Break on examples" test_break_on_examples
  ]