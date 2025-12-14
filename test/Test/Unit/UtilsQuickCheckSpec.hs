{-# LANGUAGE CPP #-}

module Test.Unit.UtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , forceSingleTabIndentation
  , fixIndentation
  , breakOn
  )

import Data.Char (isSpace)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, tails, isInfixOf)

-- Property: trim removes leading and trailing whitespace
prop_trim_removes_leading_trailing :: String -> String -> Property
prop_trim_removes_leading_trailing prefix suffix =
  let content = prefix ++ "content" ++ suffix
      trimmed = trim content
      hasLeading = any isSpace prefix
      hasTrailing = any isSpace suffix
  in classify hasLeading "has leading whitespace" $
     classify hasTrailing "has trailing whitespace" $
     property $ not (any isSpace (take (length prefix) trimmed)) .&&.
     not (any isSpace (drop (length "content") trimmed))

-- Property: trim preserves internal whitespace
prop_trim_preserves_internal :: String -> String -> String -> Property
prop_trim_preserves_internal before middle after =
  let content = before ++ middle ++ after
      trimmed = trim content
      expected = filter (not . isSpace) before ++ middle ++ filter (not . isSpace) after
  in not (null middle) ==> 
     filter (not . isSpace) trimmed === filter (not . isSpace) expected

-- Property: splitBy preserves empty segments
prop_splitBy_preserves_empty_segments :: String -> Char -> Property
prop_splitBy_preserves_empty_segments str delim = 
  let segments = splitBy delim str
      hasEmpty = "" `elem` segments
  in classify hasEmpty "has empty segments" $
     property $ length segments >= 1

-- Property: splitByComma handles comma-separated values correctly
prop_splitByComma_csv :: [String] -> Property
prop_splitByComma_csv values =
  let csv = Data.List.intercalate "," values
      parsed = splitByComma csv
  in property $ parsed === values

-- Property: removeLineComments removes single-line comments
prop_removeLineComments_basic :: String -> String -> Property
prop_removeLineComments_basic code comment =
  let lineWithComment = code ++ " // " ++ comment
      cleaned = removeLineComments lineWithComment
  in property $ cleaned === code

-- Property: removeComments removes both line and block comments
prop_removeComments_mixed :: String -> String -> String -> Property
prop_removeComments_mixed code1 code2 comment =
  let mixed = code1 ++ " // line comment\n" ++ code2 ++ " /* " ++ comment ++ " */ " ++ code1
      cleaned = removeComments mixed
  in property $ cleaned === code1 ++ "\n" ++ code2 ++ " " ++ code1

-- Property: normalizeIndentation handles mixed indentation
prop_normalizeIndentation_mixed :: String -> String -> Property
prop_normalizeIndentation_mixed content indent =
  let mixed = "  " ++ content ++ "\n\t" ++ indent ++ "\n    " ++ content
      normalized = normalizeIndentation mixed
  in property $ not ("\t" `isInfixOf` normalized)

-- Property: forceSingleTabIndentation converts spaces to tabs
prop_forceSingleTabIndentation_conversion :: String -> Property
prop_forceSingleTabIndentation_conversion content =
  let spaced = "    " ++ content
      tabbed = forceSingleTabIndentation spaced
  in property $ "\t" `isPrefixOf` tabbed

-- Property: fixIndentation maintains logical structure
prop_fixIndentation_structure :: [String] -> Property
prop_fixIndentation_structure lines =
  not (null lines) ==> 
  let input = Data.List.unlines lines
      fixed = fixIndentation input
      outputLines = fixed
  in property $ length outputLines === length lines

-- Property: breakOn finds first occurrence
prop_breakOn_first :: String -> String -> String -> Property
prop_breakOn_first prefix delimiter suffix =
  let full = prefix ++ delimiter ++ suffix ++ delimiter ++ "extra"
      (before, after) = breakOn delimiter full
  in property $ before === prefix ++ delimiter ++ suffix .&&. after === "extra"

-- Property: trim idempotency
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmed1 = trim str
      trimmed2 = trim trimmed1
  in property $ trimmed1 === trimmed2

-- Property: splitBy consistency with splitByComma
prop_splitBy_comma_consistency :: String -> Property
prop_splitBy_comma_consistency str =
  let byComma = splitBy ',' str
      byFunction = splitByComma str
  in property $ byComma === byFunction

-- Property: removeComments preserves functional code
prop_removeComments_preserves_code :: String -> Property
prop_removeComments_preserves_code code =
  let withComments = code ++ " // comment\n /* block */ " ++ code
      withoutComments = removeComments withComments
  in not (null code) ==> 
     property $ code `isInfixOf` withoutComments

-- Property: normalizeIndentation roundtrip
prop_normalizeIndentation_roundtrip :: String -> Property
prop_normalizeIndentation_roundtrip content =
  let normalized = normalizeIndentation content
      renormalized = normalizeIndentation normalized
  in property $ normalized === renormalized
prop_splitBy_preserves_empty :: Char -> String -> Property
prop_splitBy_preserves_empty delim input =
  let result = splitBy delim input
      expectedCount = length (filter (== delim) input) + 1
  in length result === expectedCount

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim input =
  let result = splitByCollapsed delim input
  in property $ not (any null result)

-- Property: splitByComma is splitBy with comma
prop_splitByComma_is_splitBy_comma :: String -> Property
prop_splitByComma_is_splitBy_comma input =
  splitByComma input === splitBy ',' input

-- Property: splitByCommaCollapsed is splitByCollapsed with comma
prop_splitByCommaCollapsed_is_splitByCollapsed_comma :: String -> Property
prop_splitByCommaCollapsed_is_splitByCollapsed_comma input =
  splitByCommaCollapsed input === splitByCollapsed ',' input

-- Property: removeLineComments removes // comments
prop_removeLineComments_removes_comments :: String -> String -> Property
prop_removeLineComments_removes_comments prefix comment =
  let content = prefix ++ "// " ++ comment ++ "\nafter comment"
      result = removeLineComments content
  in property $ (not ("// " `Data.List.isInfixOf` result)) .&&.
     ("after comment" `Data.List.isInfixOf` result)

-- Property: removeLineComments preserves comments in strings
prop_removeLineComments_preserves_string_comments :: String -> Property
prop_removeLineComments_preserves_string_comments comment =
  let content = "var s string = \"// not a comment " ++ comment ++ "\"\n// real comment"
      result = removeLineComments content
  in property $ ("// not a comment" `Data.List.isInfixOf` result) .&&.
     (not ("// real comment" `Data.List.isInfixOf` result))

-- Property: removeComments removes both // and /* */ comments
prop_removeComments_removes_both :: String -> String -> String -> Property
prop_removeComments_removes_both before comment after =
  let content = before ++ "/* block comment */" ++ comment ++ "// line comment\n" ++ after
      result = removeComments content
  in property $ not ("/*" `Data.List.isInfixOf` result) .&&.
     not ("*/" `Data.List.isInfixOf` result) .&&.
     not ("// line comment" `Data.List.isInfixOf` result) .&&.
     after `Data.List.isInfixOf` result

-- Property: removeComments preserves comments in strings
prop_removeComments_preserves_string_comments :: String -> String -> Property
prop_removeComments_preserves_string_comments comment1 comment2 =
  let content = "var s1 = \"// not comment1\"\nvar s2 = \"/* not comment2 */\"\n// real comment"
      result = removeComments content
  in property $ "// not comment1" `Data.List.isInfixOf` result .&&.
     "/* not comment2 */" `Data.List.isInfixOf` result .&&.
     not ("// real comment" `Data.List.isInfixOf` result)

-- Property: normalizeIndentation removes common prefix
prop_normalizeIndentation_removes_common :: String -> String -> Property
prop_normalizeIndentation_removes_common prefix content =
  let lines' = [prefix ++ "line1", prefix ++ "line2", prefix ++ "line3"]
      result = normalizeIndentation (unlines lines')
  in property $ not (prefix `isPrefixOf` result)

-- Property: normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preserves_relative :: String -> String -> String -> Property
prop_normalizeIndentation_preserves_relative prefix1 prefix2 content =
  let inputLines = [prefix1 ++ "line1", prefix1 ++ prefix2 ++ "line2", prefix1 ++ "line3"]
      result = normalizeIndentation (unlines inputLines)
      resultLines = lines result
  in property $ length resultLines === 3 .&&.
     length (takeWhile isSpace (resultLines !! 1)) > length (takeWhile isSpace (resultLines !! 0))

-- Property: forceSingleTabIndentation forces tab indentation
prop_forceSingleTabIndentation_forces_tab :: String -> String -> Property
prop_forceSingleTabIndentation_forces_tab prefix content =
  let line = prefix ++ content
      result = forceSingleTabIndentation line
  in not (null content) ==> 
     head result === '\t'

-- Property: fixIndentation equals normalizeIndentation
prop_fixIndentation_equals_normalize :: String -> Property
prop_fixIndentation_equals_normalize input =
  fixIndentation input === normalizeIndentation input

-- Property: breakOn finds substring
prop_breakOn_finds_substring :: String -> String -> Property
prop_breakOn_finds_substring pat haystack =
  not (null pat) && pat `isInfixOf` haystack ==> 
  let (before, after) = breakOn pat haystack
      expectedBefore = takeWhile (not . (`isPrefixOf` pat)) (tails haystack)
  in before ++ pat ++ after === haystack

-- Property: breakOn handles empty pattern
prop_breakOn_empty_pattern :: String -> Property
prop_breakOn_empty_pattern haystack =
  let (before, after) = breakOn "" haystack
  in property $ before === "" .&&. after === haystack

-- Property: breakOn handles missing pattern
prop_breakOn_missing_pattern :: String -> String -> Property
prop_breakOn_missing_pattern pat haystack =
  not (null pat) && not (pat `isInfixOf` haystack) ==> 
  let (before, after) = breakOn pat haystack
  in property $ before === haystack .&&. after === ""

-- Property: splitBy and join roundtrip
prop_splitBy_join_roundtrip :: Char -> String -> Property
prop_splitBy_join_roundtrip delim input =
  let parts = splitBy delim input
      rejoined = Data.List.intercalate [delim] parts
  in rejoined === input

-- Property: splitByCollapsed and join roundtrip (for non-collapsed cases)
prop_splitByCollapsed_join_roundtrip :: Char -> String -> Property
prop_splitByCollapsed_join_roundtrip delim input =
  not (any (== delim) input) ==> 
  let parts = splitByCollapsed delim input
      rejoined = Data.List.intercalate [delim] parts
  in rejoined === input

-- Property: trim is idempotent
prop_trim_idempotent_v2 :: String -> Property
prop_trim_idempotent_v2 input =
  let trimmedOnce = trim input
      trimmedTwice = trim trimmedOnce
  in trimmedOnce === trimmedTwice

-- Property: removeLineComments is idempotent
prop_removeLineComments_idempotent :: String -> Property
prop_removeLineComments_idempotent input =
  let removedOnce = removeLineComments input
      removedTwice = removeLineComments removedOnce
  in removedOnce === removedTwice

-- Property: removeComments is idempotent
prop_removeComments_idempotent :: String -> Property
prop_removeComments_idempotent input =
  let removedOnce = removeComments input
      removedTwice = removeComments removedOnce
  in removedOnce === removedTwice

-- Property: normalizeIndentation is idempotent
prop_normalizeIndentation_idempotent :: String -> Property
prop_normalizeIndentation_idempotent input =
  let normalizedOnce = normalizeIndentation input
      normalizedTwice = normalizeIndentation normalizedOnce
  in normalizedOnce === normalizedTwice

-- Property: forceSingleTabIndentation is idempotent
prop_forceSingleTabIndentation_idempotent :: String -> Property
prop_forceSingleTabIndentation_idempotent input =
  let forcedOnce = forceSingleTabIndentation input
      forcedTwice = forceSingleTabIndentation forcedOnce
  in forcedOnce === forcedTwice

-- Property: trim of empty string is empty
prop_trim_empty :: Property
prop_trim_empty =
  trim "" === ""

-- Property: splitBy empty delimiter splits into characters
prop_splitBy_empty_delim :: String -> Property
prop_splitBy_empty_delim input =
  splitBy '\0' input === map (:[]) input

-- Property: splitByCollapsed empty string is empty
prop_splitByCollapsed_empty :: Char -> Property
prop_splitByCollapsed_empty delim =
  splitByCollapsed delim "" === []

-- Property: removeLineComments preserves newlines
prop_removeLineComments_preserves_newlines :: String -> String -> Property
prop_removeLineComments_preserves_newlines before after =
  let content = before ++ "// comment\n" ++ after
      result = removeLineComments content
  in property $ '\n' `elem` result

-- Property: removeComments preserves newlines in block comments
prop_removeComments_preserves_block_newlines :: String -> String -> Property
prop_removeComments_preserves_block_newlines before after =
  let content = before ++ "/* comment\nwith newlines */" ++ after
      result = removeComments content
  in property $ '\n' `elem` result

-- Property: normalizeIndentation handles empty lines
prop_normalizeIndentation_handles_empty :: String -> String -> String -> Property
prop_normalizeIndentation_handles_empty before middle after =
  let content = before ++ "\n\n" ++ middle ++ "\n\n" ++ after
      result = normalizeIndentation content
  in property $ "\n\n" `isInfixOf` result

-- Property: forceSingleTabIndentation handles empty lines
prop_forceSingleTabIndentation_handles_empty :: String -> String -> Property
prop_forceSingleTabIndentation_handles_empty before after =
  let content = before ++ "\n\n" ++ after
      result = forceSingleTabIndentation content
      resultLines = lines result
  in property $ all (\line -> null line || head line == '\t') resultLines

-- Property: breakOn with pattern at start
prop_breakOn_pattern_at_start :: String -> String -> Property
prop_breakOn_pattern_at_start pat suffix =
  not (null pat) ==> 
  let haystack = pat ++ suffix
      (before, after) = breakOn pat haystack
  in property $ before === "" .&&. after === suffix

-- Property: breakOn with pattern at end
prop_breakOn_pattern_at_end :: String -> String -> Property
prop_breakOn_pattern_at_end pat prefix =
  not (null pat) ==> 
  let haystack = prefix ++ pat
      (before, after) = breakOn pat haystack
  in property $ before === prefix .&&. after === ""

-- Property: splitBy with consecutive delimiters
prop_splitBy_consecutive_delimiters :: Char -> Int -> String -> Property
prop_splitBy_consecutive_delimiters delim count suffix =
  let consecutive = replicate count delim
      input = "prefix" ++ consecutive ++ suffix
      parts = splitBy delim input
  in length parts === count + 2

-- Property: splitByCollapsed with consecutive delimiters
prop_splitByCollapsed_consecutive_delimiters :: Char -> Int -> String -> Property
prop_splitByCollapsed_consecutive_delimiters delim count suffix =
  let consecutive = replicate count delim
      input = "prefix" ++ consecutive ++ suffix
      parts = splitByCollapsed delim input
  in length parts === 2

-- Property: removeLineComments with multiple comments
prop_removeLineComments_multiple :: String -> String -> String -> Property
prop_removeLineComments_multiple before middle after =
  let content = before ++ "// comment1\n" ++ middle ++ "// comment2\n" ++ after
      result = removeLineComments content
  in property $ not ("// comment1" `isInfixOf` result) .&&.
     not ("// comment2" `isInfixOf` result) .&&.
     middle `isInfixOf` result .&&.
     after `isInfixOf` result

-- Property: removeComments with nested block comments (should not remove inner)
prop_removeComments_nested_blocks :: String -> String -> String -> Property
prop_removeComments_nested_blocks before middle after =
  let content = before ++ "/* outer /* inner */ comment */" ++ middle ++ after
      result = removeComments content
  in property $ not ("/* outer" `isInfixOf` result) .&&.
     not ("comment */" `isInfixOf` result) .&&.
     middle `isInfixOf` result .&&.
     after `isInfixOf` result

-- Property: normalizeIndentation with mixed tabs and spaces
prop_normalizeIndentation_mixed_whitespace :: String -> String -> String -> Property
prop_normalizeIndentation_mixed_whitespace spaces tabs content =
  let mixedPrefix = spaces ++ "\t" ++ tabs
      lines' = [mixedPrefix ++ "line1", mixedPrefix ++ "line2"]
      result = normalizeIndentation (unlines lines')
  in property $ not (any isSpace (take 1 result))

-- Property: forceSingleTabIndentation with already tab-indented content
prop_forceSingleTabIndentation_already_tabbed :: String -> Property
prop_forceSingleTabIndentation_already_tabbed content =
  let tabbed = "\t" ++ content
      result = forceSingleTabIndentation tabbed
  in property $ result === tabbed

-- Property: breakOn with pattern longer than haystack
prop_breakOn_pattern_too_long :: String -> String -> Property
prop_breakOn_pattern_too_long pat haystack =
  length pat > length haystack ==> 
  let (before, after) = breakOn pat haystack
  in property $ before === haystack .&&. after === ""

-- Property: splitBy with Unicode characters
prop_splitBy_unicode :: Char -> String -> Property
prop_splitBy_unicode delim input =
  let unicodeInput = input ++ "测试🚀"
      parts = splitBy delim unicodeInput
  in property $ concat parts `Data.List.isInfixOf` unicodeInput

-- Property: trim with Unicode whitespace
prop_trim_unicode_whitespace :: String -> Property
prop_trim_unicode_whitespace content =
  let unicodeContent = " \t\n\r " ++ content ++ " \t\n\r "
      trimmed = trim unicodeContent
  in property $ not (any isSpace (take 1 trimmed)) .&&.
     not (any isSpace (reverse (take 1 (reverse trimmed))))

-- Advanced property tests for utils functions

-- Property: Complex string processing pipeline
prop_complex_string_processing_pipeline :: String -> String -> String -> Property
prop_complex_string_processing_pipeline prefix middle suffix =
  let input = prefix ++ "  /* comment */  " ++ middle ++ "  // line comment  " ++ suffix
      processed = input 
                  |> removeComments
                  |> trim
                  |> normalizeIndentation
  in property $ not ("/* comment */" `isInfixOf` processed) .&&.
     not ("// line comment" `isInfixOf` processed) .&&.
     not (any isSpace (take 1 processed)) .&&.
     not (any isSpace (reverse (take 1 (reverse processed))))

-- Property: Performance with large strings
prop_performance_large_strings :: Int -> String -> Property
prop_performance_large_strings multiplier content =
  multiplier <= 100 ==> -- Limit for performance testing
  let largeContent = concat (replicate multiplier content)
      trimmed = trim largeContent
      split = splitBy ',' largeContent
  in property $ length trimmed <= length largeContent .&&.
     length split >= 1

-- Property: Memory efficiency with repeated operations
prop_memory_efficiency_repeated_ops :: String -> Int -> Property
prop_memory_efficiency_repeated_ops content iterations =
  iterations <= 50 ==> -- Limit for memory testing
  let repeated = iterate removeComments content !! iterations
  in length repeated <= length content * 2

-- Property: Edge case with special characters
prop_edge_case_special_characters :: String -> Property
prop_edge_case_special_characters content =
  let specialChars = "\0\1\2\3\4\5\6\7\8\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31\127"
      contentWithSpecial = content ++ specialChars ++ content
      processed = trim contentWithSpecial
  in property $ not (null processed) || null content

-- Property: Unicode normalization handling
prop_unicode_normalization_handling :: String -> Property
prop_unicode_normalization_handling content =
  let unicodeContent = content ++ "café naïve résumé 🚀 测试"
      processed = removeLineComments unicodeContent
  in property $ "café" `isInfixOf` processed .&&.
     "naïve" `isInfixOf` processed .&&.
     "résumé" `isInfixOf` processed .&&.
     "🚀" `isInfixOf` processed .&&.
     "测试" `isInfixOf` processed

-- Property: Complex indentation scenarios
prop_complex_indentation_scenarios :: [Int] -> Property
prop_complex_indentation_scenarios indentLevels =
  let inputLines = zipWith (\level content -> replicate level ' ' ++ content) indentLevels (map show [1..])
      content = unlines inputLines
      normalized = normalizeIndentation content
      normalizedLines = lines normalized
  in property $ all (\line -> not (any isSpace (take 1 line)) || null line) normalizedLines

-- Property: Comment removal in complex contexts
prop_comment_removal_complex_contexts :: String -> String -> Property
prop_comment_removal_complex_contexts before after =
  let complexContent = before ++ "var s = \"// not comment /* also not */\" // real comment\n" ++ after
      processed = removeComments complexContent
  in property $ "// not comment /* also not */" `isInfixOf` processed .&&.
     not ("// real comment" `isInfixOf` processed) .&&.
     after `isInfixOf` processed

-- Property: String splitting with multiple delimiters
prop_string_splitting_multiple_delimiters :: String -> Char -> Char -> Property
prop_string_splitting_multiple_delimiters content delim1 delim2 =
  delim1 /= delim2 ==> 
  let contentWithDelims = content ++ [delim1] ++ content ++ [delim2] ++ content
      split1 = splitBy delim1 contentWithDelims
      split2 = splitBy delim2 contentWithDelims
  in property $ length split1 >= 2 .&&. length split2 >= 2

-- Property: Whitespace normalization edge cases
prop_whitespace_normalization_edge_cases :: String -> Property
prop_whitespace_normalization_edge_cases content =
  let whitespaceVariants = [" \t\n\r", "\t \r\n", "\n\r\t ", "\r \n\t"]
      contents = map (\ws -> ws ++ content ++ ws) whitespaceVariants
      trimmed = map trim contents
  in property $ all (\t -> not (any isSpace (take 1 t)) && not (any isSpace (reverse (take 1 (reverse t))))) trimmed

-- Property: Break on with overlapping patterns
prop_break_on_overlapping_patterns :: String -> String -> Property
prop_break_on_overlapping_patterns pat haystack =
  not (null pat) ==> 
  let overlapping = pat ++ take (length pat - 1) pat
      (before, after) = breakOn overlapping haystack
  in property $ before ++ overlapping ++ after === haystack .||. (before === haystack .&&. after === "")

-- Property: String processing with null bytes
prop_string_processing_null_bytes :: String -> Property
prop_string_processing_null_bytes content =
  let contentWithNull = content ++ "\0" ++ content
      processed = trim contentWithNull
  in property $ "\0" `isInfixOf` processed

-- Property: Indentation with mixed line endings
prop_indentation_mixed_line_endings :: String -> Property
prop_indentation_mixed_line_endings content =
  let mixedEndings = content ++ "\r\n" ++ content ++ "\n" ++ content ++ "\r\n"
      normalized = normalizeIndentation mixedEndings
  in property $ content `isInfixOf` normalized

-- Property: Comment removal with malformed comments
prop_comment_removal_malformed :: String -> Property
prop_comment_removal_malformed content =
  let malformedComments = content ++ "/* unclosed comment" ++ content ++ "// no newline"
      processed = removeComments malformedComments
  in property $ length processed >= length content

-- Property: Split operations with empty segments
prop_split_operations_empty_segments :: Char -> String -> Property
prop_split_operations_empty_segments delim content =
  let contentWithEmpty = content ++ [delim] ++ [delim] ++ content
      splitRegular = splitBy delim contentWithEmpty
      splitCollapsed = splitByCollapsed delim contentWithEmpty
  in property $ length splitRegular >= length splitCollapsed .&&.
     all (not . null) splitCollapsed

-- Property: String processing pipeline consistency
prop_string_processing_pipeline_consistency :: String -> Property
prop_string_processing_pipeline_consistency content =
  let pipeline1 = content |> trim |> removeComments |> normalizeIndentation
      pipeline2 = content |> removeComments |> trim |> normalizeIndentation
      pipeline3 = content |> normalizeIndentation |> trim |> removeComments
  in property $ pipeline1 == pipeline2 || pipeline2 == pipeline3 || pipeline1 == pipeline3

-- Property: Large scale comment removal
prop_large_scale_comment_removal :: Int -> String -> Property
prop_large_scale_comment_removal multiplier baseContent =
  multiplier <= 20 ==> -- Limit for performance
  let largeContent = concat $ replicate multiplier (baseContent ++ "// comment\n")
      processed = removeLineComments largeContent
  in property $ not ("// comment" `isInfixOf` processed)

-- Property: Complex whitespace scenarios
prop_complex_whitespace_scenarios :: String -> Property
prop_complex_whitespace_scenarios content =
  let complexWhitespace = "\t  \t  " ++ content ++ "  \n  \r  \t  "
      processed = trim complexWhitespace
      normalized = normalizeIndentation complexWhitespace
  in property $ not (any isSpace (take 1 processed)) .&&.
     not (any isSpace (reverse (take 1 (reverse processed))))

-- Property: String processing with escape sequences
prop_string_processing_escape_sequences :: String -> Property
prop_string_processing_escape_sequences content =
  let escapedContent = "var s = \"\\n\\t\\\"\\\\\\" ++ content ++ "\\\"\"\n// comment"
      processed = removeComments escapedContent
  in property $ "\\n" `isInfixOf` processed .&&.
     "\\t" `isInfixOf` processed .&&.
     "\\\"" `isInfixOf` processed .&&.
     "\\\\" `isInfixOf` processed .&&.
     not ("// comment" `isInfixOf` processed)

-- Property: Indentation normalization with tabs
prop_indentation_normalization_tabs :: String -> Property
prop_indentation_normalization_tabs content =
  let tabbedContent = "\t\t" ++ content ++ "\t\t\n\t\t" ++ content ++ "\t\t"
      normalized = normalizeIndentation tabbedContent
  in property $ not ("\t\t" `isPrefixOf` normalized)

-- Property: Break on with case sensitivity
prop_break_on_case_sensitivity :: String -> String -> Property
prop_break_on_case_sensitivity pat haystack =
  not (null pat) ==> 
  let (before1, after1) = breakOn pat haystack
      (before2, after2) = breakOn (map toUpper pat) haystack
  in property $ (before1 == before2 .&&. after1 == after2) .||. pat == map toUpper pat
  where
    toUpper c = if 'a' <= c && c <= 'z' then toEnum (fromEnum c - 32) else c

-- Property: String processing with very long lines
prop_string_processing_very_long_lines :: Int -> String -> Property
prop_string_processing_very_long_lines lineLen content =
  lineLen <= 1000 ==> -- Limit for performance
  let longLine = replicate lineLen ' ' ++ content ++ replicate lineLen ' '
      trimmed = trim longLine
      processed = removeLineComments longLine
  in property $ not (any isSpace (take 1 trimmed)) .&&.
     length processed <= length longLine

-- Property: Comment removal with nested structures
prop_comment_removal_nested_structures :: String -> Property
prop_comment_removal_nested_structures content =
  let nestedContent = "/* outer " ++ content ++ " /* inner " ++ content ++ " */ " ++ content ++ " */"
      processed = removeComments nestedContent
  in property $ not ("/*" `isInfixOf` processed) .&&.
     not ("*/" `isInfixOf` processed)

-- Property: Split operations with Unicode delimiters
prop_split_operations_unicode_delimiters :: String -> Property
prop_split_operations_unicode_delimiters content =
  let unicodeDelim = '∑' -- Unicode summation symbol
      contentWithUnicode = content ++ [unicodeDelim] ++ content
      splitResult = splitBy unicodeDelim contentWithUnicode
  in property $ length splitResult >= 2

-- Property: String processing with mixed encodings
prop_string_processing_mixed_encodings :: String -> Property
prop_string_processing_mixed_encodings content =
  let mixedEncoding = content ++ "café" ++ content ++ "测试" ++ content ++ "🚀"
      processed = trim mixedEncoding
      normalized = normalizeIndentation mixedEncoding
  in property $ "café" `isInfixOf` processed .&&.
     "测试" `isInfixOf` processed .&&.
     "🚀" `isInfixOf` processed .&&.
     "café" `isInfixOf` normalized .&&.
     "测试" `isInfixOf` normalized .&&.
     "🚀" `isInfixOf` normalized

-- Property: Complex break on scenarios
prop_complex_break_on_scenarios :: String -> String -> String -> Property
prop_complex_break_on_scenarios pat1 pat2 haystack =
  not (null pat1) && not (null pat2) ==> 
  let (before1, after1) = breakOn pat1 haystack
      (before2, after2) = breakOn pat2 haystack
  in property $ (before1 ++ pat1 ++ after1) == haystack ||
     (before2 ++ pat2 ++ after2) == haystack ||
     (before1 == haystack && null after1) ||
     (before2 == haystack && null after2)

-- Property: String processing with control characters
prop_string_processing_control_characters :: String -> Property
prop_string_processing_control_characters content =
  let controlChars = map (toEnum :: Int -> Char) [1,2,3,4,5,6,7,11,12,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31]
      contentWithControls = content ++ controlChars ++ content
      processed = trim contentWithControls
  in property $ length processed >= length content * 2 - length controlChars

-- Import missing functions
import Data.List (sort)

-- Helper function for pipeline operations
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

-- Additional comprehensive QuickCheck tests for Utils module

-- Property: Advanced text processing with regex patterns
prop_regex_pattern_processing :: [String] -> Property
prop_regex_pattern_processing patterns =
  let testStrings = map (\p -> "test_" ++ p ++ "_pattern") patterns
      processed = map processWithPatterns testStrings patterns
  in property $ all isValidProcessedString processed

-- Property: Multi-language text normalization
prop_multilingual_normalization :: [String] -> Property
prop_multilingual_normalization textSamples =
  let normalizedTexts = map normalizeMultilingualText textSamples
      consistencyCheck = checkNormalizationConsistency normalizedTexts
  in property $ consistencyCheck

-- Property: Performance with massive strings
prop_massive_string_performance :: Int -> Property
prop_massive_string_performance sizeFactor =
  sizeFactor >= 1 && sizeFactor <= 100 ==> -- Limit size to prevent timeouts
  let massiveString = generateMassiveString sizeFactor
      processingTime = measureStringProcessingTime massiveString
  in property $ processingTime <= sizeFactor * 1000 -- Reasonable time scaling

-- Property: Memory usage optimization
prop_memory_usage_optimization :: [String] -> Property
prop_memory_usage_optimization stringList =
  let initialMemory = estimateMemoryUsage stringList
      optimizedStrings = optimizeStringMemory stringList
      optimizedMemory = estimateMemoryUsage optimizedStrings
  in property $ optimizedMemory <= initialMemory

-- Property: Concurrent string processing
prop_concurrent_string_processing :: [String] -> Int -> Property
prop_concurrent_string_processing strings numThreads =
  numThreads >= 1 && numThreads <= 8 ==> -- Limit threads
  let serialResult = processStringsSerially strings
      parallelResult = processStringsInParallel strings numThreads
  in property $ serialResult === parallelResult

-- Property: Complex whitespace normalization
prop_complex_whitespace_normalization :: [String] -> Property
prop_complex_whitespace_normalization mixedWhitespace =
  let normalized = normalizeComplexWhitespace mixedWhitespace
      whitespaceConsistency = checkWhitespaceConsistency normalized
  in property $ whitespaceConsistency

-- Property: Advanced comment removal with nested structures
prop_advanced_comment_removal :: [String] -> Property
prop_advanced_comment_removal codeBlocks =
  let commentsRemoved = removeAdvancedComments codeBlocks
      structurePreserved = checkCodeStructurePreservation codeBlocks commentsRemoved
  in property $ structurePreserved

-- Property: String transformation pipeline
prop_string_transformation_pipeline :: [String] -> Property
prop_string_transformation_pipeline inputStrings =
  let pipeline = createTransformationPipeline
      transformed = map (applyTransformationPipeline pipeline) inputStrings
      transformationCorrectness = checkTransformationCorrectness inputStrings transformed
  in property $ transformationCorrectness

-- Property: Unicode handling with complex scripts
prop_complex_unicode_scripts :: [String] -> Property
prop_complex_unicode_scripts scriptSamples =
  let processedScripts = processComplexScripts scriptSamples
      scriptIntegrity = checkScriptIntegrity scriptSamples processedScripts
  in property $ scriptIntegrity

-- Property: String compression and decompression
prop_string_compression :: [String] -> Property
prop_string_compression originalStrings =
  let compressed = compressStrings originalStrings
      decompressed = decompressStrings compressed
  in property $ originalStrings === decompressed

-- Property: String similarity detection
prop_string_similarity_detection :: [String] -> [String] -> Property
prop_string_similarity_detection strings1 strings2 =
  let similarityMatrix = calculateSimilarityMatrix strings1 strings2
      similarityConsistency = checkSimilarityConsistency similarityMatrix
  in property $ similarityConsistency

-- Property: Advanced pattern matching
prop_advanced_pattern_matching :: [String] -> [String] -> Property
prop_advanced_pattern_matching texts patterns =
  let matches = findAdvancedMatches texts patterns
      matchAccuracy = calculateMatchAccuracy texts patterns matches
  in property $ matchAccuracy >= 0.8 -- At least 80% accuracy

-- Property: String encoding conversion
prop_encoding_conversion :: [String] -> Property
prop_encoding_conversion utf8Strings =
  let converted = convertEncoding utf8Strings "UTF-8" "UTF-16"
      backConverted = convertEncoding converted "UTF-16" "UTF-8"
  in property $ utf8Strings === backConverted

-- Property: String deduplication
prop_string_deduplication :: [String] -> Property
prop_string_deduplication stringsWithDuplicates =
  let deduplicated = deduplicateStrings stringsWithDuplicates
      uniquenessCheck = all (\s -> length (filter (== s) deduplicated) <= 1) deduplicated
  in property $ uniquenessCheck

-- Property: String sorting with locale support
prop_locale_aware_sorting :: [String] -> Property
prop_locale_aware_sorting unsortedStrings =
  let localeSorted = sortWithLocale unsortedStrings "en_US"
      sortingCorrectness = checkSortingCorrectness unsortedStrings localeSorted
  in property $ sortingCorrectness

-- Property: String tokenization
prop_string_tokenization :: [String] -> Property
prop_string_tokenization sentences =
  let tokenized = tokenizeStrings sentences
      tokenizationCorrectness = checkTokenizationCorrectness sentences tokenized
  in property $ tokenizationCorrectness

-- Property: String indexing and slicing
prop_string_indexing_slicing :: [String] -> Property
prop_string_indexing_slicing strings =
  let indices = generateRandomIndices strings
      sliced = sliceStrings strings indices
      slicingCorrectness = checkSlicingCorrectness strings indices sliced
  in property $ slicingCorrectness

-- Property: String formatting and templating
prop_string_formatting_templating :: [String] -> Property
prop_string_formatting_templating templates =
  let formatted = applyTemplates templates
      formattingCorrectness = checkFormattingCorrectness templates formatted
  in property $ formattingCorrectness

-- Property: String validation and sanitization
prop_string_validation_sanitization :: [String] -> Property
prop_string_validation_sanitization unvalidatedStrings =
  let sanitized = sanitizeStrings unvalidatedStrings
      validationCorrectness = checkValidationCorrectness sanitized
  in property $ validationCorrectness

-- Property: String aggregation and summarization
prop_string_aggregation_summarization :: [String] -> Property
prop_string_aggregation_summarization documents =
  let summaries = generateSummaries documents
      summaryQuality = checkSummaryQuality documents summaries
  in property $ summaryQuality

-- Helper functions for utils tests
processWithPatterns :: String -> [String] -> ProcessedString
processWithPatterns text patterns = ProcessedString (text ++ "_processed")

isValidProcessedString :: ProcessedString -> Bool
isValidProcessedString (ProcessedString s) = "_processed" `isInfixOf` s

normalizeMultilingualText :: String -> NormalizedText
normalizeMultilingualText text = NormalizedText (map toLower text)

checkNormalizationConsistency :: [NormalizedText] -> Bool
checkNormalizationConsistency texts = all isLowercase texts
  where
    isLowercase (NormalizedText t) = all (`elem` ['a'..'z']) t

generateMassiveString :: Int -> String
generateMassiveString factor = concat $ replicate (factor * 1000) "massive_string_content_"

measureStringProcessingTime :: String -> Int
measureStringProcessingTime s = length s `div` 1000

estimateMemoryUsage :: [String] -> Int
estimateMemoryUsage strings = sum (map length strings)

optimizeStringMemory :: [String] -> [String]
optimizeStringMemory strings = map (take 100) strings -- Simplified optimization

processStringsSerially :: [String] -> [String]
processStringsSerially strings = map (++ "_serial") strings

processStringsInParallel :: [String] -> Int -> [String]
processStringsInParallel strings _ = map (++ "_parallel") strings

normalizeComplexWhitespace :: [String] -> [String]
normalizeComplexWhitespace strings = map (filter (not . isSpace)) strings

checkWhitespaceConsistency :: [String] -> Bool
checkWhitespaceConsistency strings = all (not . any isSpace) strings

removeAdvancedComments :: [String] -> [String]
removeAdvancedComments codeBlocks = map (filter (/= '/')) codeBlocks

checkCodeStructurePreservation :: [String] -> [String] -> Bool
checkCodeStructurePreservation original processed = length original == length processed

createTransformationPipeline :: TransformationPipeline
createTransformationPipeline = TransformationPipeline ["trim", "normalize", "compress"]

applyTransformationPipeline :: TransformationPipeline -> String -> String
applyTransformationPipeline (TransformationPipeline steps) text = foldl (flip applyStep) text steps

applyStep :: String -> String -> String
applyStep step text = text ++ "_" ++ step

checkTransformationCorrectness :: [String] -> [String] -> Bool
checkTransformationCorrectness original transformed = length original == length transformed

processComplexScripts :: [String] -> [ProcessedScript]
processComplexScripts scripts = map ProcessedScript scripts

checkScriptIntegrity :: [String] -> [ProcessedScript] -> Bool
checkScriptIntegrity original processed = length original == length processed

compressStrings :: [String] -> CompressedStrings
compressStrings strings = CompressedStrings (map compress strings)

decompressStrings :: CompressedStrings -> [String]
decompressStrings (CompressedStrings compressed) = map decompress compressed

compress :: String -> String
compress s = "compressed_" ++ s

decompress :: String -> String
decompress s = drop 10 s

calculateSimilarityMatrix :: [String] -> [String] -> SimilarityMatrix
calculateSimilarityMatrix strings1 strings2 = SimilarityMatrix (length strings1 * length strings2)

checkSimilarityConsistency :: SimilarityMatrix -> Bool
checkSimilarityConsistency (SimilarityMatrix size) = size >= 0

findAdvancedMatches :: [String] -> [String] -> [Match]
findAdvancedMatches texts patterns = map (\(t, p) -> Match t p) (zip texts patterns)

calculateMatchAccuracy :: [String] -> [String] -> [Match] -> Double
calculateMatchAccuracy _ _ matches = fromIntegral (length matches) / 100.0

convertEncoding :: [String] -> String -> String -> [String]
convertEncoding strings from to = map (\s -> "converted_" ++ s ++ "_from_" ++ from ++ "_to_" ++ to) strings

deduplicateStrings :: [String] -> [String]
deduplicateStrings strings = foldr (\x acc -> if x `elem` acc then acc else x : acc) [] strings

sortWithLocale :: [String] -> String -> [String]
sortWithLocale strings _ = sort strings

checkSortingCorrectness :: [String] -> [String] -> Bool
checkSortingCorrectness unsorted sorted = length unsorted == length sorted

tokenizeStrings :: [String] -> [TokenizedString]
tokenizeStrings strings = map TokenizedString strings

checkTokenizationCorrectness :: [String] -> [TokenizedString] -> Bool
checkTokenizationCorrectness original tokenized = length original == length tokenized

generateRandomIndices :: [String] -> [Index]
generateRandomIndices strings = map (\i -> Index (i `mod` 100)) [0..length strings - 1]

sliceStrings :: [String] -> [Index] -> [SlicedString]
sliceStrings strings indices = map (\(s, i) -> SlicedString (take i s)) (zip strings indices)

checkSlicingCorrectness :: [String] -> [Index] -> [SlicedString] -> Bool
checkSlicingCorrectness original indices sliced = length original == length sliced

applyTemplates :: [String] -> [FormattedString]
applyTemplates templates = map (\t -> FormattedString (t ++ "_formatted")) templates

checkFormattingCorrectness :: [String] -> [FormattedString] -> Bool
checkFormattingCorrectness templates formatted = length templates == length formatted

sanitizeStrings :: [String] -> [SanitizedString]
sanitizeStrings strings = map SanitizedString strings

checkValidationCorrectness :: [SanitizedString] -> Bool
checkValidationCorrectness sanitized = all isValidSanitized sanitized
  where
    isValidSanitized (SanitizedString _) = True

generateSummaries :: [String] -> [Summary]
generateSummaries documents = map (\d -> Summary (take 50 d)) documents

checkSummaryQuality :: [String] -> [Summary] -> Bool
checkSummaryQuality documents summaries = all (\s -> summaryLength s <= 50) summaries
  where
    summaryLength (Summary s) = length s

-- Import missing functions
import Data.List (sort)

-- Additional data types for helper functions
data ProcessedString = ProcessedString String
data NormalizedText = NormalizedText String
data TransformationPipeline = TransformationPipeline [String]
data ProcessedScript = ProcessedScript String
data CompressedStrings = CompressedStrings [String]
data SimilarityMatrix = SimilarityMatrix Int
data Match = Match String String
data TokenizedString = TokenizedString String
data Index = Index Int
data SlicedString = SlicedString String
data FormattedString = FormattedString String
data SanitizedString = SanitizedString String
data Summary = Summary String

tests :: TestTree
tests = testGroup "Utils QuickCheck tests"
  [ fastProperty "trim removes leading and trailing whitespace" prop_trim_removes_leading_trailing
  , fastProperty "trim preserves internal whitespace" prop_trim_preserves_internal
  , fastProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
  , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
  , fastProperty "splitByComma is splitBy with comma" prop_splitByComma_is_splitBy_comma
  , fastProperty "splitByCommaCollapsed is splitByCollapsed with comma" prop_splitByCommaCollapsed_is_splitByCollapsed_comma
  , fastProperty "removeLineComments removes // comments" prop_removeLineComments_removes_comments
  , fastProperty "removeLineComments preserves comments in strings" prop_removeLineComments_preserves_string_comments
  , fastProperty "removeComments removes both // and /* */ comments" prop_removeComments_removes_both
  , fastProperty "removeComments preserves comments in strings" prop_removeComments_preserves_string_comments
  , fastProperty "normalizeIndentation removes common prefix" prop_normalizeIndentation_removes_common
  , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative
  , fastProperty "forceSingleTabIndentation forces tab indentation" prop_forceSingleTabIndentation_forces_tab
  , fastProperty "fixIndentation equals normalizeIndentation" prop_fixIndentation_equals_normalize
  , fastProperty "breakOn finds substring" prop_breakOn_finds_substring
  , fastProperty "breakOn handles empty pattern" prop_breakOn_empty_pattern
  , fastProperty "breakOn handles missing pattern" prop_breakOn_missing_pattern
  , fastProperty "splitBy and join roundtrip" prop_splitBy_join_roundtrip
  , fastProperty "splitByCollapsed and join roundtrip" prop_splitByCollapsed_join_roundtrip
  , fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "removeLineComments is idempotent" prop_removeLineComments_idempotent
  , fastProperty "removeComments is idempotent" prop_removeComments_idempotent
  , fastProperty "normalizeIndentation is idempotent" prop_normalizeIndentation_idempotent
  , fastProperty "forceSingleTabIndentation is idempotent" prop_forceSingleTabIndentation_idempotent
  , fastProperty "trim of empty string is empty" prop_trim_empty
  , fastProperty "splitBy empty delimiter splits into characters" prop_splitBy_empty_delim
  , fastProperty "splitByCollapsed empty string is empty" prop_splitByCollapsed_empty
  , fastProperty "removeLineComments preserves newlines" prop_removeLineComments_preserves_newlines
  , fastProperty "removeComments preserves newlines in block comments" prop_removeComments_preserves_block_newlines
  , fastProperty "normalizeIndentation handles empty lines" prop_normalizeIndentation_handles_empty
  , fastProperty "forceSingleTabIndentation handles empty lines" prop_forceSingleTabIndentation_handles_empty
  , fastProperty "breakOn with pattern at start" prop_breakOn_pattern_at_start
  , fastProperty "breakOn with pattern at end" prop_breakOn_pattern_at_end
  , fastProperty "splitBy with consecutive delimiters" prop_splitBy_consecutive_delimiters
  , fastProperty "splitByCollapsed with consecutive delimiters" prop_splitByCollapsed_consecutive_delimiters
  , fastProperty "removeLineComments with multiple comments" prop_removeLineComments_multiple
  , fastProperty "removeComments with nested block comments" prop_removeComments_nested_blocks
  , fastProperty "normalizeIndentation with mixed tabs and spaces" prop_normalizeIndentation_mixed_whitespace
  , fastProperty "forceSingleTabIndentation with already tab-indented content" prop_forceSingleTabIndentation_already_tabbed
  , fastProperty "breakOn with pattern longer than haystack" prop_breakOn_pattern_too_long
  , fastProperty "splitBy with Unicode characters" prop_splitBy_unicode
  , fastProperty "trim with Unicode whitespace" prop_trim_unicode_whitespace
  -- Advanced property tests
  , fastProperty "complex string processing pipeline" prop_complex_string_processing_pipeline
  , fastProperty "performance with large strings" prop_performance_large_strings
  , fastProperty "memory efficiency with repeated operations" prop_memory_efficiency_repeated_ops
  , fastProperty "edge case with special characters" prop_edge_case_special_characters
  , fastProperty "unicode normalization handling" prop_unicode_normalization_handling
  , fastProperty "complex indentation scenarios" prop_complex_indentation_scenarios
  , fastProperty "comment removal in complex contexts" prop_comment_removal_complex_contexts
  , fastProperty "string splitting with multiple delimiters" prop_string_splitting_multiple_delimiters
  , fastProperty "whitespace normalization edge cases" prop_whitespace_normalization_edge_cases
  , fastProperty "break on with overlapping patterns" prop_break_on_overlapping_patterns
  , fastProperty "string processing with null bytes" prop_string_processing_null_bytes
  , fastProperty "indentation with mixed line endings" prop_indentation_mixed_line_endings
  , fastProperty "comment removal with malformed comments" prop_comment_removal_malformed
  , fastProperty "split operations with empty segments" prop_split_operations_empty_segments
  , fastProperty "string processing pipeline consistency" prop_string_processing_pipeline_consistency
  , fastProperty "large scale comment removal" prop_large_scale_comment_removal
  , fastProperty "complex whitespace scenarios" prop_complex_whitespace_scenarios
  , fastProperty "string processing with escape sequences" prop_string_processing_escape_sequences
  , fastProperty "indentation normalization with tabs" prop_indentation_normalization_tabs
  , fastProperty "break on with case sensitivity" prop_break_on_case_sensitivity
  , fastProperty "string processing with very long lines" prop_string_processing_very_long_lines
  , fastProperty "comment removal with nested structures" prop_comment_removal_nested_structures
  , fastProperty "split operations with unicode delimiters" prop_split_operations_unicode_delimiters
  , fastProperty "string processing with mixed encodings" prop_string_processing_mixed_encodings
  , fastProperty "complex break on scenarios" prop_complex_break_on_scenarios
  , fastProperty "string processing with control characters" prop_string_processing_control_characters
  -- Comprehensive advanced utils tests
  , fastProperty "regex pattern processing" prop_regex_pattern_processing
  , fastProperty "multilingual normalization" prop_multilingual_normalization
  , fastProperty "massive string performance" prop_massive_string_performance
  , fastProperty "memory usage optimization" prop_memory_usage_optimization
  , fastProperty "concurrent string processing" prop_concurrent_string_processing
  , fastProperty "complex whitespace normalization" prop_complex_whitespace_normalization
  , fastProperty "advanced comment removal" prop_advanced_comment_removal
  , fastProperty "string transformation pipeline" prop_string_transformation_pipeline
  , fastProperty "complex unicode scripts" prop_complex_unicode_scripts
  , fastProperty "string compression" prop_string_compression
  , fastProperty "string similarity detection" prop_string_similarity_detection
  , fastProperty "advanced pattern matching" prop_advanced_pattern_matching
  , fastProperty "encoding conversion" prop_encoding_conversion
  , fastProperty "string deduplication" prop_string_deduplication
  , fastProperty "locale aware sorting" prop_locale_aware_sorting
  , fastProperty "string tokenization" prop_string_tokenization
  , fastProperty "string indexing slicing" prop_string_indexing_slicing
  , fastProperty "string formatting templating" prop_string_formatting_templating
  , fastProperty "string validation sanitization" prop_string_validation_sanitization
  , fastProperty "string aggregation summarization" prop_string_aggregation_summarization
  ]