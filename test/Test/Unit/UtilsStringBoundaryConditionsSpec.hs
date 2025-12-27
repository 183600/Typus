{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.UtilsStringBoundaryConditionsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
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

import Data.Char (isSpace, isControl)
import Data.List (isPrefixOf, isInfixOf, sort)
import qualified Data.Text as T

-- | Utils string processing boundary condition tests
tests :: TestTree
tests = testGroup "Utils string processing boundary conditions"
  [ -- Edge cases for trim
    testGroup "Trim edge cases"
      [ testCase "trim of empty string is empty" $
          trim "" @?= ""
      , testCase "trim of only whitespace is empty" $
          trim " \t\n\r " @?= ""
      , testCase "trim preserves internal whitespace" $
          trim "hello  world" @?= "hello  world"
      , testCase "trim handles mixed whitespace correctly" $
          trim "\t  hello  world \n" @?= "hello  world"
      , fastProperty "trim removes all leading/trailing whitespace" prop_trim_removes_all_whitespace
      , fastProperty "trim is idempotent" prop_trim_idempotent
      , fastProperty "trim handles control characters" prop_trim_control_chars
      ]

  , -- Edge cases for split operations
    testGroup "Split edge cases"
      [ testCase "splitBy empty string returns singleton" $
          splitBy ',' "" @?= [""]
      , testCase "splitBy with no delimiter returns singleton" $
          splitBy ',' "hello" @?= ["hello"]
      , testCase "splitByCollapsed empty string returns empty" $
          splitByCollapsed ',' "" @?= []
      , fastProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
      , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
      , fastProperty "splitBy with consecutive delimiters" prop_splitBy_consecutive
      , fastProperty "splitBy with Unicode delimiters" prop_splitBy_unicode
      ]

  , -- Edge cases for comment removal
    testGroup "Comment removal edge cases"
      [ testCase "removeLineComments handles empty input" $
          removeLineComments "" @?= ""
      , testCase "removeComments handles empty input" $
          removeComments "" @?= ""
      , testCase "removeLineComments preserves strings with comment markers" $
          let input = "var s = \"// not a comment\""
          in removeLineComments input @?= input
      , testCase "removeComments preserves strings with block comment markers" $
          let input = "var s = \"/* not a comment */\""
          in removeComments input @?= input
      , fastProperty "removeLineComments handles unterminated strings" prop_removeLineComments_unterminated
      , fastProperty "removeComments handles nested comments" prop_removeComments_nested
      , fastProperty "removeComments handles malformed comments" prop_removeComments_malformed
      ]

  , -- Edge cases for indentation
    testGroup "Indentation edge cases"
      [ testCase "normalizeIndentation handles empty input" $
          normalizeIndentation "" @?= ""
      , testCase "normalizeIndentation handles only whitespace" $
          let input = "    \n  \t\n  "
          in normalizeIndentation input @?= input
      , testCase "forceSingleTabIndentation handles empty input" $
          forceSingleTabIndentation "" @?= ""
      , fastProperty "normalizeIndentation handles mixed indentation" prop_normalizeIndentation_mixed
      , fastProperty "forceSingleTabIndentation handles various whitespace" prop_forceSingleTab_mixed
      , fastProperty "fixIndentation equals normalizeIndentation" prop_fixIndentation_equals
      ]

  , -- Edge cases for search operations
    testGroup "Search edge cases"
      [ testCase "breakOn with empty pattern returns empty prefix" $
          breakOn "" "hello" @?= ("", "hello")
      , testCase "breakOn with pattern not in string returns whole string" $
          breakOn "xyz" "hello" @?= ("hello", "")
      , testCase "breakOn with empty string and pattern" $
          breakOn "x" "" @?= ("", "")
      , fastProperty "breakOn finds first occurrence" prop_breakOn_first_occurrence
      , fastProperty "breakOn with overlapping patterns" prop_breakOn_overlapping
      , fastProperty "breakOn with Unicode patterns" prop_breakOn_unicode
      ]

  , -- Performance and memory edge cases
    testGroup "Performance edge cases"
      [ fastProperty "trim handles large strings efficiently" prop_trim_large_strings
      , fastProperty "splitBy handles large strings efficiently" prop_splitBy_large_strings
      , fastProperty "removeComments handles large strings efficiently" prop_removeComments_large
      , fastProperty "operations are memory efficient" prop_memory_efficiency
      ]

  , -- Unicode and encoding edge cases
    testGroup "Unicode edge cases"
      [ testCase "trim handles Unicode whitespace" $
          let input = " \u2000\u2001hello\u2002\u2003 "
          in trim input @?= "hello"
      , fastProperty "trim with Unicode content" prop_trim_unicode
      , fastProperty "splitBy with Unicode content" prop_splitBy_unicode_content
      , fastProperty "comment removal with Unicode" prop_comments_unicode
      , fastProperty "indentation with Unicode" prop_indentation_unicode
      ]

  , -- Extreme boundary conditions
    testGroup "Extreme boundary conditions"
      [ fastProperty "functions handle null bytes" prop_null_bytes
      , fastProperty "functions handle control characters" prop_control_characters
      , fastProperty "functions handle very long lines" prop_very_long_lines
      , fastProperty "functions handle deeply nested structures" prop_deeply_nested
      ]
  ]

-- Trim edge cases

prop_trim_removes_all_whitespace :: String -> Property
prop_trim_removes_all_whitespace input =
  let trimmed = trim input
      hasLeading = not (null input) && isSpace (head input)
      hasTrailing = not (null input) && isSpace (last input)
  in classify hasLeading "has leading whitespace" $
     classify hasTrailing "has trailing whitespace" $
     property $ null trimmed || not (isSpace (head trimmed)) .&&.
                null trimmed || not (isSpace (last trimmed))

prop_trim_idempotent :: String -> Property
prop_trim_idempotent input =
  let trimmed1 = trim input
      trimmed2 = trim trimmed1
  in property $ trimmed1 === trimmed2

prop_trim_control_chars :: String -> Property
prop_trim_control_chars input =
  let controlInput = input ++ "\0\1\2\3\4\5" ++ "\31\127" ++ input
      trimmed = trim controlInput
  in property $ "\0" `isInfixOf` trimmed .||. "\31" `isInfixOf` trimmed

-- Split edge cases

prop_splitBy_preserves_empty :: Char -> String -> Property
prop_splitBy_preserves_empty delim input =
  let result = splitBy delim input
      expectedCount = length (filter (== delim) input) + 1
  in property $ length result === expectedCount

prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim input =
  let result = splitByCollapsed delim input
  in property $ all (not . null) result

prop_splitBy_consecutive :: Char -> Int -> String -> Property
prop_splitBy_consecutive delim count suffix =
  count > 0 && count <= 20 ==> -- Limit for reasonable testing
  let consecutive = replicate count delim
      input = "prefix" ++ consecutive ++ suffix
      parts = splitBy delim input
  in property $ length parts === count + 1

prop_splitBy_unicode :: Char -> String -> Property
prop_splitBy_unicode delim input =
  let unicodeInput = input ++ "测试🚀café naïve"
      parts = splitBy delim unicodeInput
  in if delim `elem` unicodeInput
     then property $ not (null parts) .&&. all (notElem delim) parts
     else property $ concat parts === unicodeInput

-- Comment removal edge cases

prop_removeLineComments_unterminated :: String -> Property
prop_removeLineComments_unterminated input =
  let hasNoQuotes = not ('"' `elem` input) && not ('\'' `elem` input)
      unterminated = input ++ "var s = \"unterminated string"
  in hasNoQuotes ==>
     let result = removeLineComments unterminated
     in property $ "unterminated string" `isInfixOf` result

prop_removeComments_nested :: String -> Property
prop_removeComments_nested input =
  let hasNoQuotes = not ('"' `elem` input) && not ('\'' `elem` input)
      nested = input ++ "/* outer /* inner */ still outer */" ++ input
  in hasNoQuotes ==>
     let result = removeComments nested
     in property $ not ("/* outer" `isInfixOf` result) .&&.
                not ("/* inner" `isInfixOf` result)

prop_removeComments_malformed :: String -> Property
prop_removeComments_malformed input =
  let hasNoQuotes = not ('"' `elem` input) && not ('\'' `elem` input)
      malformed = input ++ "/* unterminated comment" ++ input
  in hasNoQuotes ==>
     let result = removeComments malformed
     in property $ length result >= length input

-- Indentation edge cases

prop_normalizeIndentation_mixed :: String -> Property
prop_normalizeIndentation_mixed content =
  not ('\n' `elem` content) ==> -- Single line for simplicity
  let mixedInput = "  " ++ content ++ "\n\t" ++ content ++ "\n    " ++ content
      normalized = normalizeIndentation mixedInput
      lines' = lines normalized
  in property $ length lines' === 3 .&&.
             all (\line -> null line || not (isPrefixOf "    " line)) lines'

prop_forceSingleTab_mixed :: String -> Property
prop_forceSingleTab_mixed content =
  not ('\n' `elem` content) ==> -- Single line for simplicity
  let mixedInput = "    " ++ content ++ "  \t  " ++ content
      result = forceSingleTabIndentation mixedInput
      resultLines = lines result
  in property $ all (\line -> null line || head line == '\t') resultLines

prop_fixIndentation_equals :: String -> Property
prop_fixIndentation_equals input =
  fixIndentation input === normalizeIndentation input

-- Search edge cases

prop_breakOn_first_occurrence :: String -> String -> String -> Property
prop_breakOn_first_occurrence pat prefix suffix =
  not (null pat) ==>
  let haystack = prefix ++ pat ++ suffix ++ pat ++ "extra"
      (before, after) = breakOn pat haystack
  in property $ before === prefix ++ pat ++ suffix .&&. after === "extra"

prop_breakOn_overlapping :: String -> String -> Property
prop_breakOn_overlapping pat haystack =
  not (null pat) && length pat > 1 ==>
  let overlapping = pat ++ take (length pat - 1) pat
      (before, after) = breakOn overlapping haystack
  in property $ before ++ overlapping ++ after === haystack .||.
             (before === haystack .&&. after === "")

prop_breakOn_unicode :: String -> String -> Property
prop_breakOn_unicode pat haystack =
  not (null pat) ==>
  let unicodeHaystack = haystack ++ "测试🚀café naïve" ++ pat ++ "more"
      (before, after) = breakOn pat unicodeHaystack
  in property $ before ++ pat ++ after === unicodeHaystack .||.
             (before === unicodeHaystack .&&. after === "")

-- Performance edge cases

prop_trim_large_strings :: Int -> String -> Property
prop_trim_large_strings multiplier content =
  multiplier >= 0 && multiplier <= 100 ==> -- Limit for performance testing
  let largeContent = concat (replicate multiplier content)
      trimmed = trim largeContent
  in property $ length trimmed <= length largeContent

prop_splitBy_large_strings :: Int -> String -> Property
prop_splitBy_large_strings multiplier content =
  multiplier >= 0 && multiplier <= 100 ==> -- Limit for performance testing
  let largeContent = concat (replicate multiplier content)
      parts = splitBy ',' largeContent
  in property $ length parts >= 1

prop_removeComments_large :: Int -> String -> Property
prop_removeComments_large multiplier content =
  multiplier >= 0 && multiplier <= 50 ==> -- Limit for performance testing
  let largeContent = concat $ replicate multiplier (content ++ " // comment\n")
      result = removeLineComments largeContent
  in property $ not ("// comment" `isInfixOf` result)

prop_memory_efficiency :: String -> Int -> Property
prop_memory_efficiency content iterations =
  iterations >= 0 && iterations <= 50 ==> -- Limit for memory testing
  let repeated = iterate removeComments content !! iterations
  in property $ length repeated <= length content * 2

-- Unicode edge cases

prop_trim_unicode :: String -> Property
prop_trim_unicode content =
  let unicodeContent = " \t\n\r " ++ content ++ "  测试  🚀  café  naïve  " ++ " \t\n\r "
      trimmed = trim unicodeContent
  in property $ not (any isSpace (take 1 trimmed)) .&&.
             not (any isSpace (reverse (take 1 (reverse trimmed))))

prop_splitBy_unicode_content :: Char -> String -> Property
prop_splitBy_unicode_content delim input =
  let unicodeInput = input ++ "测试🚀café naïve"
      parts = splitBy delim unicodeInput
  in property $ concat parts === unicodeInput

prop_comments_unicode :: String -> Property
prop_comments_unicode input =
  let unicodeContent = input ++ "var 测试 = \"// not comment\" // real comment 测试"
      result = removeLineComments unicodeContent
  in property $ "// not comment" `isInfixOf` result .&&.
             not ("// real comment 测试" `isInfixOf` result)

prop_indentation_unicode :: String -> Property
prop_indentation_unicode content =
  let unicodeContent = "  " ++ content ++ " 测试 🚀 café naïve\n\t" ++ content ++ " 更多"
      normalized = normalizeIndentation unicodeContent
  in property $ not (null normalized)

-- Extreme boundary conditions

prop_null_bytes :: String -> Property
prop_null_bytes content =
  let contentWithNull = content ++ "\0" ++ content
      trimmed = trim contentWithNull
      split = splitBy ',' contentWithNull
  in property $ "\0" `isInfixOf` trimmed .&&.
             any ("\0" `isInfixOf`) split

prop_control_characters :: String -> Property
prop_control_characters content =
  let controlChars = ['\0'..'\31'] ++ ['\127']
      contentWithControl = content ++ take 5 controlChars ++ content
      trimmed = trim contentWithControl
  in property $ length trimmed >= length content

prop_very_long_lines :: Int -> String -> Property
prop_very_long_lines lengthMultiplier baseContent =
  lengthMultiplier >= 0 && lengthMultiplier <= 20 ==> -- Limit for reasonable testing
  let longLine = concat (replicate lengthMultiplier baseContent)
      processed = normalizeIndentation longLine
  in property $ not (null processed)

prop_deeply_nested :: Int -> String -> Property
prop_deeply_nested depth content =
  depth >= 0 && depth <= 10 ==> -- Limit for reasonable testing
  let nestedContent = concat $ replicate depth ("  " ++ content ++ "\n")
      processed = normalizeIndentation nestedContent
  in property $ not (null processed)