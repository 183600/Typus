{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CoreUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, vectorOf, elements, oneof)

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

import Data.Char (isSpace, isAlphaNum, isLetter, isDigit, toUpper, toLower)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, isInfixOf, sort, nub)
import Data.Text (Text)
import qualified Data.Text as T

-- ============================================================================
-- Generators
-- ============================================================================

genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r.,;:!?()[]{}<>+-*/%=|&^~'\"@#$_`\\"

genString :: Gen String
genString = listOf genChar

genNonEmptyString :: Gen String
genNonEmpty = listOf1 genChar

genWhitespace :: Gen String
genWhitespace = listOf $ elements " \t\n\r"

genAlphaNumString :: Gen String
genAlphaNumString = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

genIdentifier :: Gen String
genIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
  return $ first : rest

genDelimChar :: Gen Char
genDelimChar = elements $ ",;:|&^%$#@!~`"

genLineContent :: Gen String
genLineContent = do
  words <- listOf $ listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  return $ unwords words

-- ============================================================================
-- Properties for String Trimming
-- ============================================================================

prop_trim_removes_leading_whitespace :: String -> String -> Property
prop_trim_removes_leading_whitespace prefix content =
  let whitespace = take 5 $ repeat ' '
      input = whitespace ++ prefix ++ content
      trimmed = trim input
  in property $ not (null trimmed) ==> not (isSpace (head trimmed))

prop_trim_removes_trailing_whitespace :: String -> String -> Property
prop_trim_removes_trailing_whitespace content suffix =
  let whitespace = take 5 $ repeat ' '
      input = content ++ suffix ++ whitespace
      trimmed = trim input
  in property $ not (null trimmed) ==> not (isSpace (last trimmed))

prop_trim_preserves_internal_whitespace :: String -> String -> String -> Property
prop_trim_preserves_internal_whitespace before middle after =
  let input = before ++ "  " ++ middle ++ "  " ++ after
      trimmed = trim input
      expected = filter (not . isSpace) before ++ "  " ++ middle ++ "  " ++ filter (not . isSpace) after
  in not (null middle) ==> property $ "  " `isInfixOf` trimmed

prop_trim_is_idempotent :: String -> Property
prop_trim_is_idempotent input =
  let trimmedOnce = trim input
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce === trimmedTwice

prop_trim_empty_string :: Property
prop_trim_empty_string =
  trim "" === ""

-- ============================================================================
-- Properties for String Splitting
-- ============================================================================

prop_splitBy_preserves_empty_segments :: Char -> String -> Property
prop_splitBy_preserves_empty_segments delim input =
  let result = splitBy delim input
      expectedCount = length (filter (== delim) input) + 1
  in property $ length result === expectedCount

prop_splitByCollapsed_removes_empty_segments :: Char -> String -> Property
prop_splitByCollapsed_removes_empty_segments delim input =
  let result = splitByCollapsed delim input
  in property $ all (not . null) result

prop_splitByComma_is_splitBy_with_comma :: String -> Property
prop_splitByComma_is_splitBy_with_comma input =
  splitByComma input === splitBy ',' input

prop_splitByCommaCollapsed_is_splitByCollapsed_with_comma :: String -> Property
prop_splitByCommaCollapsed_is_splitByCollapsed_with_comma input =
  splitByCommaCollapsed input === splitByCollapsed ',' input

prop_splitBy_and_join_roundtrip :: Char -> String -> Property
prop_splitBy_and_join_roundtrip delim input =
  let parts = splitBy delim input
      rejoined = Data.List.intercalate [delim] parts
  in rejoined === input

-- ============================================================================
-- Properties for Comment Removal
-- ============================================================================

prop_removeLineComments_removes_single_line_comments :: String -> String -> Property
prop_removeLineComments_removes_single_line_comments code comment =
  not ('"' `elem` code) && not ('\'' `elem` code) ==>
  let lineWithComment = code ++ " // " ++ comment
      cleaned = removeLineComments lineWithComment
  in property $ not ("// " `isInfixOf` cleaned)

prop_removeLineComments_preserves_comments_in_strings :: String -> Property
prop_removeLineComments_preserves_comments_in_strings comment =
  let content = "var s string = \"// not a comment " ++ comment ++ "\"\n// real comment"
      result = removeLineComments content
  in property $ "// not a comment" `isInfixOf` result .&&.
               not ("// real comment" `isInfixOf` result)

prop_removeComments_removes_both_line_and_block_comments :: String -> String -> String -> Property
prop_removeComments_removes_both_line_and_block_comments code1 code2 comment =
  not ('"' `elem` code1) && not ('\'' `elem` code1) && 
  not ('"' `elem` code2) && not ('\'' `elem` code2) &&
  not ("/" `isInfixOf` code1) && not ("/" `isInfixOf` code2) ==>
  let mixed = code1 ++ " // line comment\n" ++ code2 ++ " /* " ++ comment ++ " */ " ++ code1
      cleaned = removeComments mixed
  in property $ not ("// line comment" `isInfixOf` cleaned) .&&.
               not ("/* " `isInfixOf` cleaned) .&&.
               not (" */" `isInfixOf` cleaned)

prop_removeComments_preserves_comments_in_strings :: String -> String -> Property
prop_removeComments_preserves_comments_in_strings comment1 comment2 =
  let content = "var s1 = \"// not comment1\"\nvar s2 = \"/* not comment2 */\"\n// real comment"
      result = removeComments content
  in property $ "// not comment1" `isInfixOf` result .&&.
               "/* not comment2 */" `isInfixOf` result .&&.
               not ("// real comment" `isInfixOf` result)

-- ============================================================================
-- Properties for Indentation
-- ============================================================================

prop_normalizeIndentation_removes_common_prefix :: String -> String -> Property
prop_normalizeIndentation_removes_common_prefix prefix content =
  let indentedLines = [prefix ++ "line1", prefix ++ "line2", prefix ++ "line3"]
      input = unlines indentedLines
      normalized = normalizeIndentation input
      normalizedLines = lines normalized
  in property $ all (not . isPrefixOf prefix) normalizedLines

prop_normalizeIndentation_preserves_relative_indentation :: Int -> Int -> Property
prop_normalizeIndentation_preserves_relative_indentation n1 n2 =
  n1 >= 0 && n1 <= 10 && n2 >= 0 && n2 <= 10 ==>
  let baseIndent = replicate n1 ' '
      extraIndent = replicate n2 ' '
      inputLines = [baseIndent ++ "line1", baseIndent ++ extraIndent ++ "line2", baseIndent ++ "line3"]
      input = unlines inputLines
      normalized = normalizeIndentation input
      normalizedLines = lines normalized
      indent1 = length (takeWhile isSpace (normalizedLines !! 0))
      indent2 = length (takeWhile isSpace (normalizedLines !! 1))
  in property $ indent1 === 0 .&&. (if n2 > 0 then indent2 === n2 else indent2 === 0)

prop_forceSingleTabIndentation_enforces_tab_indentation :: String -> Property
prop_forceSingleTabIndentation_enforces_tab_indentation content =
  not (null (trim content)) ==> 
  let result = forceSingleTabIndentation content
      resultLines = lines result
      nonEmptyLines = filter (not . null . trim) resultLines
  in property $ all (\line -> case line of ('\t':_) -> True; _ -> False) nonEmptyLines

prop_fixIndentation_equals_normalizeIndentation :: String -> Property
prop_fixIndentation_equals_normalizeIndentation input =
  fixIndentation input === normalizeIndentation input

-- ============================================================================
-- Properties for String Searching
-- ============================================================================

prop_breakOn_finds_first_occurrence :: String -> String -> String -> Property
prop_breakOn_finds_first_occurrence prefix delimiter suffix =
  not (null delimiter) ==> 
  let haystack = prefix ++ delimiter ++ suffix ++ delimiter ++ "extra"
      (before, after) = breakOn delimiter haystack
  in property $ before === prefix ++ delimiter ++ suffix .&&. after === "extra"

prop_breakOn_with_empty_pattern :: String -> Property
prop_breakOn_with_empty_pattern haystack =
  let (before, after) = breakOn "" haystack
  in property $ before === "" .&&. after === haystack

prop_breakOn_with_missing_pattern :: String -> String -> Property
prop_breakOn_with_missing_pattern pat haystack =
  not (null pat) && not (pat `isInfixOf` haystack) ==> 
  let (before, after) = breakOn pat haystack
  in property $ before === haystack .&&. after === ""



-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Core Utils QuickCheck Tests"
  [ testGroup "String Trimming Properties"
    [ fastProperty "trim removes leading whitespace" prop_trim_removes_leading_whitespace
    , fastProperty "trim removes trailing whitespace" prop_trim_removes_trailing_whitespace
    , fastProperty "trim preserves internal whitespace" prop_trim_preserves_internal_whitespace
    , fastProperty "trim is idempotent" prop_trim_is_idempotent
    , fastProperty "trim empty string" prop_trim_empty_string
    ]

  , testGroup "String Splitting Properties"
    [ fastProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty_segments
    , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty_segments
    , fastProperty "splitByComma is splitBy with comma" prop_splitByComma_is_splitBy_with_comma
    , fastProperty "splitByCommaCollapsed is splitByCollapsed with comma" prop_splitByCommaCollapsed_is_splitByCollapsed_with_comma
    , fastProperty "splitBy and join roundtrip" prop_splitBy_and_join_roundtrip
    ]

  , testGroup "Comment Removal Properties"
    [ fastProperty "removeLineComments removes single line comments" prop_removeLineComments_removes_single_line_comments
    , fastProperty "removeLineComments preserves comments in strings" prop_removeLineComments_preserves_comments_in_strings
    , fastProperty "removeComments removes both line and block comments" prop_removeComments_removes_both_line_and_block_comments
    , fastProperty "removeComments preserves comments in strings" prop_removeComments_preserves_comments_in_strings
    ]

  , testGroup "Indentation Properties"
    [ fastProperty "normalizeIndentation removes common prefix" prop_normalizeIndentation_removes_common_prefix
    , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative_indentation
    , fastProperty "forceSingleTabIndentation enforces tab indentation" prop_forceSingleTabIndentation_enforces_tab_indentation
    , fastProperty "fixIndentation equals normalizeIndentation" prop_fixIndentation_equals_normalizeIndentation
    ]

  , testGroup "String Searching Properties"
    [ fastProperty "breakOn finds first occurrence" prop_breakOn_finds_first_occurrence
    , fastProperty "breakOn with empty pattern" prop_breakOn_with_empty_pattern
    , fastProperty "breakOn with missing pattern" prop_breakOn_with_missing_pattern
    ]
  ]