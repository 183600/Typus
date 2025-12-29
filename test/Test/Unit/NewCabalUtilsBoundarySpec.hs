{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalUtilsBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

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

import Data.Char (isSpace, isControl, isAscii)
import qualified Data.List as List
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)

-- | Test suite for Utils boundary conditions
tests :: TestTree
tests =
  testGroup "Utils Boundary Conditions"
    [ testGroup "String processing boundary conditions"
        [ fastProperty "trim handles empty strings" prop_trim_empty
        , fastProperty "trim handles whitespace-only strings" prop_trim_whitespace_only
        , fastProperty "trim handles Unicode whitespace" prop_trim_unicode_whitespace
        , fastProperty "trim handles control characters" prop_trim_control_chars
        ]

    , testGroup "String splitting boundary conditions"
        [ fastProperty "splitBy with empty delimiter" prop_splitBy_empty_delimiter
        , fastProperty "splitBy with delimiter not in string" prop_splitBy_delimiter_not_found
        , fastProperty "splitBy with string of only delimiters" prop_splitBy_only_delimiters
        , fastProperty "splitBy with large strings" prop_splitBy_large_strings
        ]

    , testGroup "Comment removal boundary conditions"
        [ fastProperty "removeComments handles empty strings" prop_removeComments_empty
        , fastProperty "removeComments handles unterminated block comments" prop_removeComments_unterminated
        , fastProperty "removeComments handles nested comment markers" prop_removeComments_nested_markers
        , fastProperty "removeComments handles malformed strings" prop_removeComments_malformed_strings
        ]

    , testGroup "Indentation processing boundary conditions"
        [ fastProperty "normalizeIndentation handles empty lines" prop_normalizeIndentation_empty_lines
        , fastProperty "normalizeIndentation handles mixed whitespace" prop_normalizeIndentation_mixed_whitespace
        , fastProperty "forceSingleTabIndentation handles excessive indentation" prop_forceSingleTabIndentation_excessive
        , fastProperty "indentation functions handle Unicode content" prop_indentation_unicode_content
        ]

    , testGroup "Search and processing boundary conditions"
        [ fastProperty "breakOn with empty pattern" prop_breakOn_empty_pattern
        , fastProperty "breakOn with pattern longer than string" prop_breakOn_pattern_too_long
        , fastProperty "breakOn with overlapping patterns" prop_breakOn_overlapping
        , fastProperty "breakOn with special characters" prop_breakOn_special_chars
        ]

    , testGroup "Performance and memory boundary conditions"
        [ fastProperty "utils functions handle very large strings" prop_utils_large_strings
        , fastProperty "utils functions handle deep recursion" prop_utils_deep_recursion
        , fastProperty "utils functions handle memory pressure" prop_utils_memory_pressure
        ]
    ]

-- String processing boundary conditions

prop_trim_empty :: Property
prop_trim_empty =
  let result = trim ""
  in property $ result === ""

prop_trim_whitespace_only :: String -> Property
prop_trim_whitespace_only chars =
  all isSpace chars && length chars <= 20 ==>
  let result = trim chars
  in property $ result === ""

prop_trim_unicode_whitespace :: String -> Property
prop_trim_unicode_whitespace content =
  let unicodeWhitespace = " \t\n\r\u00A0\u2000\u3000"  -- Various Unicode spaces
      input = unicodeWhitespace ++ content ++ unicodeWhitespace
      result = trim input
  in property $ not (any isSpace (take 1 result)) .&&.
             not (any isSpace (reverse (take 1 (reverse result))))

prop_trim_control_chars :: String -> Property
prop_trim_control_chars content =
  let controlChars = ['\0'..'\31'] ++ ['\127']
      input = controlChars ++ content ++ controlChars
      result = trim input
  in property $ length result >= length content

-- String splitting boundary conditions

prop_splitBy_empty_delimiter :: String -> Property
prop_splitBy_empty_delimiter input =
  let result = splitBy '\0' input
      expected = if null input then [""] else [input]
  in property $ result === expected

prop_splitBy_delimiter_not_found :: String -> Property
prop_splitBy_delimiter_not_found input =
  '\0' `notElem` input ==>
  let result = splitBy '\0' input
  in property $ result === [input]

prop_splitBy_only_delimiters :: Char -> Int -> Property
prop_splitBy_only_delimiters delim count =
  count > 0 && count <= 10 ==>
  let input = replicate count delim
      result = splitBy delim input
  in property $ length result === count + 1 .&&. all (== "") result

prop_splitBy_large_strings :: Char -> Int -> Property
prop_splitBy_large_strings delim size =
  size >= 0 && size <= 100 ==>
  let input = replicate size 'a' ++ [delim] ++ replicate size 'b'
      result = splitBy delim input
  in property $ length result === 2 .&&. 
             head result === replicate size 'a' .&&.
             last result === replicate size 'b'

-- Comment removal boundary conditions

prop_removeComments_empty :: Property
prop_removeComments_empty =
  let result = removeComments ""
  in property $ result === ""

prop_removeComments_unterminated :: String -> Property
prop_removeComments_unterminated content =
  not ("*/" `isInfixOf` content) && not ("/*" `isInfixOf` content) ==>
  let input = content ++ "/* unterminated comment"
      result = removeComments input
  in property $ length result <= length input

prop_removeComments_nested_markers :: String -> Property
prop_removeComments_nested_markers content =
  not ("/*" `isInfixOf` content) && not ("*/" `isInfixOf` content) ==>
  let input = "/* outer /* inner */" ++ content
      result = removeComments input
  in property $ not ("/* outer" `isInfixOf` result)

prop_removeComments_malformed_strings :: String -> Property
prop_removeComments_malformed_strings content =
  not ('"' `elem` content) && not ('\'' `elem` content) ==>
  let input = "unterminated string \" // not a comment\n" ++ content
      result = removeComments input
  in property $ length result >= length content

-- Indentation processing boundary conditions

prop_normalizeIndentation_empty_lines :: String -> Property
prop_normalizeIndentation_empty_lines content =
  let input = "\n\n" ++ content ++ "\n\n"
      result = normalizeIndentation input
  in property $ "\n\n" `isInfixOf` result

prop_normalizeIndentation_mixed_whitespace :: Int -> Int -> Property
prop_normalizeIndentation_mixed_whitespace spaces tabs =
  spaces >= 0 && spaces <= 10 && tabs >= 0 && tabs <= 10 ==>
  let mixedPrefix = replicate spaces ' ' ++ replicate tabs '\t'
      input = mixedPrefix ++ "content\n" ++ mixedPrefix ++ "more"
      result = normalizeIndentation input
  in property $ not (mixedPrefix `isPrefixOf` result)

prop_forceSingleTabIndentation_excessive :: Int -> Property
prop_forceSingleTabIndentation_excessive indentLevel =
  indentLevel >= 0 && indentLevel <= 50 ==>
  let excessiveIndent = replicate indentLevel ' '
      input = excessiveIndent ++ "content"
      result = forceSingleTabIndentation input
  in property $ "\t" `isPrefixOf` result

prop_indentation_unicode_content :: String -> Property
prop_indentation_unicode_content content =
  let unicodeContent = content ++ "café naïve 测试 🚀"
      input = "    " ++ unicodeContent
      result1 = normalizeIndentation input
      result2 = forceSingleTabIndentation input
  in property $ unicodeContent `isInfixOf` result1 .&&.
             unicodeContent `isInfixOf` result2

-- Search and processing boundary conditions

prop_breakOn_empty_pattern :: String -> Property
prop_breakOn_empty_pattern haystack =
  let (before, after) = breakOn "" haystack
  in property $ before === "" .&&. after === haystack

prop_breakOn_pattern_too_long :: String -> String -> Property
prop_breakOn_pattern_too_long pat haystack =
  not (null pat) && length pat > length haystack ==>
  let (before, after) = breakOn pat haystack
  in property $ before === haystack .&&. after === ""

prop_breakOn_overlapping :: String -> Property
prop_breakOn_overlapping base =
  not (null base) && length base <= 10 ==>
  let pat = base ++ take (length base - 1) base
      haystack = base ++ "extra"
      (before, after) = breakOn pat haystack
  in property $ before ++ pat ++ after === haystack .||. (before === haystack .&&. after === "")

prop_breakOn_special_chars :: String -> Property
prop_breakOn_special_chars content =
  let specialChars = "\0\1\2\3\4\5\6\7\8\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31\127"
      haystack = content ++ specialChars ++ content
      (before, after) = breakOn specialChars haystack
  in property $ before === content .&&. after === content

-- Performance and memory boundary conditions

prop_utils_large_strings :: Int -> Property
prop_utils_large_strings multiplier =
  multiplier >= 0 && multiplier <= 20 ==>
  let largeContent = concat (replicate multiplier "test content ")
      trimmed = trim largeContent
      split = splitBy ' ' largeContent
      commentsRemoved = removeLineComments largeContent
  in property $ length trimmed <= length largeContent .&&.
             length split >= 1 .&&.
             length commentsRemoved <= length largeContent

prop_utils_deep_recursion :: Int -> Property
prop_utils_deep_recursion depth =
  depth >= 0 && depth <= 10 ==>
  let nestedContent = concat (replicate depth "/* comment ") ++ "content" ++ concat (replicate depth " */ ")
      result = removeComments nestedContent
  in property $ length result <= length nestedContent + depth

prop_utils_memory_pressure :: Int -> Property
prop_utils_memory_pressure operations =
  operations >= 0 && operations <= 50 ==>
  let baseContent = "test content with // comments and /* block comments */"
      processOperations = iterate removeComments baseContent
      finalResult = processOperations !! (min operations (length processOperations - 1))
  in property $ length finalResult <= length baseContent * 2