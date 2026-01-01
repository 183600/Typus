{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.UtilsNewQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, vectorOf, elements, oneof)
import Data.Char (isSpace, toLower, toUpper, isAlpha, isDigit)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified Data.Text as T

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

-- Helper generators for more complex test cases

-- Generate strings with various whitespace combinations
genWhitespaceString :: Gen String
genWhitespaceString = do
  size <- choose (0, 20)
  vectorOf size $ elements $ " \t\n\r\f\v"

-- Generate strings with mixed content L.and whitespace
genMixedContentString :: Gen String
genMixedContentString = do
  whitespace <- genWhitespaceString
  content <- vectorOf 5 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-+*"
  moreWhitespace <- genWhitespaceString
  return $ whitespace ++ content ++ moreWhitespace

-- Generate strings with potential comment patterns
genCommentString :: Gen String
genCommentString = do
  before <- vectorOf 5 $ elements $ ['a'..'z'] ++ [' '] ++ ['\t']
  commentType <- elements ["//", "/*"]
  comment <- vectorOf 10 $ elements $ ['a'..'z'] ++ [' ']
  after <- case commentType of
             "//" -> vectorOf 5 $ elements $ ['a'..'z'] ++ [' '] ++ ['\t']
             "/*" -> do
               end <- elements ["*/", "*/", "*/", "*/", "*/"] -- Mostly proper ending
               rest <- vectorOf 5 $ elements $ ['a'..'z'] ++ [' '] ++ ['\t']
               return $ end ++ rest
  return $ before ++ commentType ++ comment ++ after

-- Generate strings with various indentation patterns
genIndentedString :: Gen String
genIndentedString = do
  numLines <- choose (1, 5)
  lines' <- vectorOf numLines $ do
    indent <- choose (0, 8)
    content <- vectorOf 3 $ elements $ ['a'..'z'] ++ [' ']
    return $ replicate indent ' ' ++ content
  return $ unlines lines'

-- Generate strings with multiple delimiters
genMultiDelimiterString :: Char -> Gen String
genMultiDelimiterString delim = do
  parts <- vectorOf 3 $ vectorOf 3 $ elements $ ['a'..'z']
  return $ Data.List.intercalate [delim] parts

-- Property: trim handles L.all whitespace types correctly
prop_trim_all_whitespace :: String -> String -> Property
prop_trim_all_whitespace prefix suffix =
  let whitespace = " \t\n\r\f\v"
      content = "content"
      fullString = prefix ++ whitespace ++ content ++ whitespace ++ suffix
      trimmed = trim fullString
  in property $ trimmed === content

-- Property: trim handles strings with only whitespace
prop_trim_only_whitespace :: Property
prop_trim_only_whitespace =
  forAll genWhitespaceString $ \whitespace ->
  let trimmed = trim whitespace
  in property $ null trimmed

-- Property: trim preserves internal whitespace structure
prop_trim_preserves_internal_structure :: String -> String -> String -> Property
prop_trim_preserves_internal_structure before middle after =
  let internalWhitespace = " \t "
      content = before ++ internalWhitespace ++ middle ++ internalWhitespace ++ after
      trimmed = trim content
      expected = L.filter (not . isSpace) before ++ internalWhitespace ++ middle ++ internalWhitespace ++ L.filter (not . isSpace) after
  in not (null before || null middle || null after) ==>
     property $ L.filter (not . isSpace) trimmed === L.filter (not . isSpace) expected

-- Property: splitBy handles Unicode characters correctly
prop_splitBy_unicode :: Char -> String -> Property
prop_splitBy_unicode delim input =
  let unicodeInput = input ++ "测试🚀ñáéíóú"
      parts = splitBy delim unicodeInput
  in if delim `elem` unicodeInput
     then property $ not (null parts) .&&. L.all (L.notElem delim) parts
     else property $ L.concat parts === unicodeInput

-- Property: splitByCollapsed handles consecutive delimiters correctly
prop_splitByCollapsed_consecutive :: Char -> Int -> String -> Property
prop_splitByCollapsed_consecutive delim count suffix =
  count > 0 && not (delim `elem` suffix) && not (null suffix) ==>
  let consecutive = replicate count delim
      input = "prefix" ++ consecutive ++ suffix
      parts = splitByCollapsed delim input
  in property $ L.length parts === 2 .&&. L.all (not . null) parts

-- Property: splitByComma handles empty segments correctly
prop_splitByComma_empty_segments :: String -> String -> String -> Property
prop_splitByComma_empty_segments before middle after =
  let csv = before ++ "," ++ middle ++ "," ++ after
      parts = splitByComma csv
  in property $ L.length parts === 3 .&&. parts !! 0 === before .&&. parts !! 1 === middle .&&. parts !! 2 === after

-- Property: splitByCommaCollapsed removes empty segments
prop_splitByCommaCollapsed_removes_empty :: String -> String -> String -> Property
prop_splitByCommaCollapsed_removes_empty before middle after =
  let csv = before ++ ",," ++ middle ++ ",," ++ after
      parts = splitByCommaCollapsed csv
      expectedParts = L.filter (not . null) [before, "", middle, "", after]
  in property $ parts === expectedParts

-- Property: removeLineComments handles strings with quotes correctly
prop_removeLine_comments_quotes :: String -> String -> Property
prop_removeLine_comments_quotes before comment =
  not ("//" `L.isInfixOf` before) && not ("\"" `L.isInfixOf` before) ==>
  let withQuotes = before ++ "\"string with // not comment\" // real comment"
      result = removeLineComments withQuotes
  in property $ "\"string with // not comment\"" `L.isInfixOf` result .&&.
             not ("// real comment" `L.isInfixOf` result)

-- Property: removeLineComments handles character literals correctly
prop_removeLine_comments_char_literals :: String -> String -> Property
prop_removeLine_comments_char_literals before comment =
  not ("//" `L.isInfixOf` before) && not ("'" `L.isInfixOf` before) ==>
  let withChar = before ++ "'// not a comment' // real comment"
      result = removeLineComments withChar
  in property $ "'// not a comment'" `L.isInfixOf` result .&&.
             not ("// real comment" `L.isInfixOf` result)

-- Property: removeComments handles nested block comments correctly (C-style)
prop_remove_comments_nested_blocks :: String -> String -> String -> Property
prop_remove_comments_nested_blocks before middle after =
  not ("/*" `L.isInfixOf` before) && not ("/*" `L.isInfixOf` middle) && not ("/*" `L.isInfixOf` after) ==>
  let nested = before ++ "/* outer /* inner */ still outer */" ++ middle ++ after
      result = removeComments nested
  in property $ not ("/* outer" `L.isInfixOf` result) .&&.
             not ("/* inner" `L.isInfixOf` result) .&&.
             middle `L.isInfixOf` result .&&.
             after `L.isInfixOf` result

-- Property: removeComments preserves comments in strings
prop_remove_comments_preserves_string_comments :: String -> String -> Property
prop_remove_comments_preserves_string_comments comment1 comment2 =
  let content = "var s1 = \"// not comment1\"\nvar s2 = \"/* not comment2 */\"\n// real comment"
      result = removeComments content
  in property $ "// not comment1" `L.isInfixOf` result .&&.
             "/* not comment2 */" `L.isInfixOf` result .&&.
             not ("// real comment" `L.isInfixOf` result)

-- Property: normalizeIndentation handles mixed tabs L.and spaces
prop_normalize_indentation_mixed_tabs_spaces :: [Int] -> Property
prop_normalize_indentation_mixed_tabs_spaces indentLevels =
  not (null indentLevels) ==>
  let inputLines = zipWith (\level content -> 
                            let spaces = replicate (abs level `mod` 4) ' '
                                tabs = replicate (abs level `mod` 3) '\t'
                            in spaces ++ tabs ++ content) 
                          indentLevels 
                          (map show ([1..] :: [Integer]))
      content = unlines inputLines
      normalized = normalizeIndentation content
      normalizedLines = lines normalized
      minIndent = if null normalizedLines then 0 else 
                  L.minimum [L.length (takeWhile isSpace line) | line <- normalizedLines]
  in property $ minIndent === 0

-- Property: normalizeIndentation preserves relative indentation
prop_normalize_indentation_preserves_relative :: Int -> Int -> Int -> Property
prop_normalize_indentation_preserves_relative base1 extra1 extra2 =
  base1 >= 0 && extra1 >= 0 && extra2 >= 0 && extra1 /= extra2 ==>
  let baseIndent = replicate base1 ' '
      line1 = baseIndent ++ replicate extra1 ' ' ++ "content1"
      line2 = baseIndent ++ replicate extra2 ' ' ++ "content2"
      content = unlines [line1, line2]
      normalized = normalizeIndentation content
      normalizedLines = lines normalized
      indent1 = L.length (takeWhile isSpace (normalizedLines !! 0))
      indent2 = L.length (takeWhile isSpace (normalizedLines !! 1))
  in property $ indent1 /= indent2

-- Property: forceSingleTabIndentation converts L.all non-empty lines to tab format
prop_force_single_tab_indentation_conversion :: [String] -> Property
prop_force_single_tab_indentation_conversion lines =
  not (null lines) ==>
  let content = unlines lines
      tabbed = forceSingleTabIndentation content
      tabbedLines = lines tabbed
      nonEmptyLines = L.filter (not . null . trim) tabbedLines
  in property $ L.all (\line -> case line of ('\t':_) -> True; _ -> False) nonEmptyLines

-- Property: forceSingleTabIndentation preserves empty lines
prop_force_single_tab_indentation_preserves_empty :: [String] -> Property
prop_force_single_tab_indentation_preserves_empty lines =
  let content = unlines lines
      tabbed = forceSingleTabIndentation content
      tabbedLines = lines tabbed
      originalEmpty = filter null lines
      tabbedEmpty = filter null tabbedLines
  in property $ L.length originalEmpty === L.length tabbedEmpty

-- Property: fixIndentation equals normalizeIndentation
prop_fix_indentation_equals_normalize :: String -> Property
prop_fix_indentation_equals_normalize input =
  fixIndentation input === normalizeIndentation input

-- Property: breakOn handles overlapping patterns
prop_break_on_overlapping :: String -> String -> Property
prop_break_on_overlapping pat haystack =
  not (null pat) ==> 
  let overlapping = pat ++ take (L.length pat - 1) pat
      (before, after) = breakOn overlapping haystack
  in property $ before ++ overlapping ++ after === haystack .||. 
             (before === haystack .&&. after === "")

-- Property: breakOn with pattern longer than haystack
prop_break_on_pattern_too_long :: String -> String -> Property
prop_break_on_pattern_too_long pat haystack =
  L.length pat > L.length haystack ==> 
  let (before, after) = breakOn pat haystack
  in property $ before === haystack .&&. after === ""

-- Property: breakOn with empty pattern
prop_break_on_empty_pattern :: String -> Property
prop_break_on_empty_pattern haystack =
  let (before, after) = breakOn "" haystack
  in property $ before === "" .&&. after === haystack

-- Property: breakOn with pattern at start
prop_break_on_pattern_at_start :: String -> String -> Property
prop_break_on_pattern_at_start pat suffix =
  not (null pat) ==> 
  let haystack = pat ++ suffix
      (before, after) = breakOn pat haystack
  in property $ before === "" .&&. after === suffix

-- Property: breakOn with pattern at end
prop_break_on_pattern_at_end :: String -> String -> Property
prop_break_on_pattern_at_end pat prefix =
  not (null pat) && not (pat `L.isInfixOf` prefix) ==> 
  let haystack = prefix ++ pat
      (before, after) = breakOn pat haystack
  in property $ before === prefix .&&. after === ""

-- Property: Complex string processing pipeline consistency
prop_complex_pipeline_consistency :: String -> Property
prop_complex_pipeline_consistency content =
  let pipeline1 = content |> trim |> removeComments |> normalizeIndentation
      pipeline2 = content |> removeComments |> trim |> normalizeIndentation
      pipeline3 = content |> normalizeIndentation |> trim |> removeComments
  in property $ pipeline1 == pipeline2 || pipeline2 == pipeline3 || pipeline1 == pipeline3

-- Property: String processing with Unicode L.and special characters
prop_unicode_special_characters :: String -> Property
prop_unicode_special_characters content =
  let unicodeContent = content ++ "café naïve résumé 🚀 测试 ñáéíóú"
      processed = removeLineComments unicodeContent
      trimmed = trim processed
  in property $ "café" `L.isInfixOf` processed .&&.
             "naïve" `L.isInfixOf` processed .&&.
             "résumé" `L.isInfixOf` processed .&&.
             "🚀" `L.isInfixOf` processed .&&.
             "测试" `L.isInfixOf` processed .&&.
             "ñáéíóú" `L.isInfixOf` processed

-- Property: Edge case with null bytes L.and control characters
prop_null_bytes_control_chars :: String -> Property
prop_null_bytes_control_chars content =
  let controlChars = "\0\1\2\3\4\5\6\7\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31\127"
      contentWithControl = content ++ controlChars ++ content
      processed = trim contentWithControl
  in property $ "\0" `L.isInfixOf` processed

-- Property: Performance with large inputs
prop_performance_large_input :: Int -> String -> Property
prop_performance_large_input multiplier baseContent =
  multiplier > 0 && multiplier <= 100 ==> -- Limit for performance testing
  let largeContent = L.concat (replicate multiplier baseContent)
      trimmed = trim largeContent
      split = splitBy ',' largeContent
  in property $ L.length trimmed <= L.length largeContent .&&.
             L.length split >= 1

-- Property: Memory efficiency with repeated operations
prop_memory_efficiency_repeated :: String -> Int -> Property
prop_memory_efficiency_repeated content iterations =
  iterations >= 0 && iterations <= 50 ==> -- Limit for memory testing
  let repeated = iterate removeComments content !! iterations
  in L.length repeated <= L.length content * 2

-- Helper function for pipeline testing
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

tests :: TestTree
tests = testGroup "Utils New QuickCheck Tests"
  [ fastProperty "trim handles L.all whitespace types correctly" prop_trim_all_whitespace
  , fastProperty "trim handles strings with only whitespace" prop_trim_only_whitespace
  , fastProperty "trim preserves internal whitespace structure" prop_trim_preserves_internal_structure
  , fastProperty "splitBy handles Unicode characters correctly" prop_splitBy_unicode
  , fastProperty "splitByCollapsed handles consecutive delimiters correctly" prop_splitByCollapsed_consecutive
  , fastProperty "splitByComma handles empty segments correctly" prop_splitByComma_empty_segments
  , fastProperty "splitByCommaCollapsed removes empty segments" prop_splitByCommaCollapsed_removes_empty
  , fastProperty "removeLineComments handles strings with quotes correctly" prop_removeLine_comments_quotes
  , fastProperty "removeLineComments handles character literals correctly" prop_removeLine_comments_char_literals
  , fastProperty "removeComments handles nested block comments correctly" prop_remove_comments_nested_blocks
  , fastProperty "removeComments preserves comments in strings" prop_remove_comments_preserves_string_comments
  , fastProperty "normalizeIndentation handles mixed tabs L.and spaces" prop_normalize_indentation_mixed_tabs_spaces
  , fastProperty "normalizeIndentation preserves relative indentation" prop_normalize_indentation_preserves_relative
  , fastProperty "forceSingleTabIndentation converts L.all non-empty lines to tab format" prop_force_single_tab_indentation_conversion
  , fastProperty "forceSingleTabIndentation preserves empty lines" prop_force_single_tab_indentation_preserves_empty
  , fastProperty "fixIndentation equals normalizeIndentation" prop_fix_indentation_equals_normalize
  , fastProperty "breakOn handles overlapping patterns" prop_break_on_overlapping
  , fastProperty "breakOn with pattern longer than haystack" prop_break_on_pattern_too_long
  , fastProperty "breakOn with empty pattern" prop_break_on_empty_pattern
  , fastProperty "breakOn with pattern at start" prop_break_on_pattern_at_start
  , fastProperty "breakOn with pattern at end" prop_break_on_pattern_at_end
  , fastProperty "Complex string processing pipeline consistency" prop_complex_pipeline_consistency
  , fastProperty "String processing with Unicode L.and special characters" prop_unicode_special_characters
  , fastProperty "Edge case with null bytes L.and control characters" prop_null_bytes_control_chars
  , fastProperty "Performance with large inputs" prop_performance_large_input
  , fastProperty "Memory efficiency with repeated operations" prop_memory_efficiency_repeated
  ]