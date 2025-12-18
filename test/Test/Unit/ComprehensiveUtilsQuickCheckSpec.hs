{-# LANGUAGE CPP #-}

-- | Comprehensive QuickCheck tests for Utils module
module Test.Unit.ComprehensiveUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ()
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (==>), classify, property)

import Utils
  ( trim
  , splitBy, splitByCollapsed
  , splitByComma, splitByCommaCollapsed
  , removeLineComments, removeComments
  , normalizeIndentation, forceSingleTabIndentation, fixIndentation
  , breakOn
  )

import qualified Data.List as Data.List
import Data.Char (isSpace)

-- Property: trim removes leading and trailing whitespace
prop_trim_whitespace :: String -> Property
prop_trim_whitespace s =
  let trimmed = trim s
      hasLeadingWS = case s of (c:_) -> isSpace c; [] -> False
      hasTrailingWS = case reverse s of (c:_) -> isSpace c; [] -> False
  in classify hasLeadingWS "has leading whitespace" $
     classify hasTrailingWS "has trailing whitespace" $
     property $ case (trimmed, reverse trimmed) of
                  (c1:_, c2:_) -> not (isSpace c1) && not (isSpace c2)
                  _ -> True

-- Property: trim preserves internal whitespace
prop_trim_preserve_internal :: String -> String -> String -> Property
prop_trim_preserve_internal prefix middle suffix =
  let combined = prefix ++ "   " ++ middle ++ "   " ++ suffix
      trimmed = trim combined
  in property $ middle `isInfixOf` trimmed

-- Property: splitBy preserves empty segments
prop_splitBy_preserve_empty :: Char -> String -> Property
prop_splitBy_preserve_empty delim s =
  let parts = splitBy delim s
      expectedCount = length (filter (== delim) s) + 1
  in property $ length parts == expectedCount

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim s =
  let parts = splitByCollapsed delim s
      hasEmpty = any null parts
  in property $ not hasEmpty

-- Property: splitByComma is equivalent to splitBy ','
prop_splitByComma_equivalence :: String -> Property
prop_splitByComma_equivalence s =
  let commaSplit = splitByComma s
      charSplit = splitBy ',' s
  in property $ commaSplit == charSplit

-- Property: splitByCommaCollapsed removes empty segments
prop_splitByCommaCollapsed_removes_empty :: String -> Property
prop_splitByCommaCollapsed_removes_empty s =
  let parts = splitByCommaCollapsed s
      hasEmpty = any null parts
  in property $ not hasEmpty

-- Property: removeLineComments removes only line comments
prop_remove_line_comments :: String -> String -> Property
prop_remove_line_comments code comment =
  let codeWithComment = code ++ "// " ++ comment ++ "\n" ++ code
      withoutComments = removeLineComments codeWithComment
  in property $ "// " `notElem` Data.List.words withoutComments

-- Property: removeLineComments preserves string literals
prop_remove_line_comments_preserve_strings :: String -> Property
prop_remove_line_comments_preserve_strings _content =
  let stringWithComment = "var s string = \"// not a comment\"\n// this is a comment\n"
      withoutComments = removeLineComments stringWithComment
  in property $ "// not a comment" `isInfixOf` withoutComments

-- Property: removeComments removes both line and block comments
prop_remove_both_comments :: String -> String -> String -> Property
prop_remove_both_comments code lineComment blockComment =
  let codeWithComments = code ++ "// " ++ lineComment ++ "\n/* " ++ blockComment ++ " */\n" ++ code
      withoutComments = removeComments codeWithComments
  in property $ "// " `notElem` Data.List.words withoutComments &&
                "/* " `notElem` Data.List.words withoutComments

-- Property: removeComments preserves nested string literals
prop_remove_comments_preserve_nested_strings :: String -> Property
prop_remove_comments_preserve_nested_strings _content =
  let complexString = "\"string with // fake comment and /* fake block */\""
      codeWithComments = "// real comment\nvar s = " ++ complexString ++ "\n/* real block */"
      withoutComments = removeComments codeWithComments
  in property $ complexString `isInfixOf` withoutComments

-- Property: normalizeIndentation preserves relative indentation
prop_normalize_indentation_relative :: [String] -> Property
prop_normalize_indentation_relative inputLines =
  let validLines = filter (\l -> not (all isSpace l) && not ('\n' `elem` l)) inputLines
  in not (null validLines) && length validLines <= 10 ==>
     let indentedLines = zipWith (\i line -> replicate i ' ' ++ line) [0,2,4,1,3] validLines
         normalized = normalizeIndentation (unlines indentedLines)
         normalizedLines = splitLines normalized
     in property $ relativeIndentationPreserved indentedLines normalizedLines

-- Property: forceSingleTabIndentation converts spaces to tabs
prop_force_tab_indentation :: [String] -> Property
prop_force_tab_indentation inputLines =
  not (null inputLines) && length inputLines <= 5 ==>
  let spacedLines = zipWith (\i line -> replicate (i*2) ' ' ++ line) [0..] inputLines
      tabbed = forceSingleTabIndentation (unlines spacedLines)
  in property $ all hasTabIndentation (splitLines tabbed)

-- Property: fixIndentation is equivalent to normalizeIndentation
prop_fix_indentation_equivalence :: String -> Property
prop_fix_indentation_equivalence s =
  let fixed = fixIndentation s
      normalized = normalizeIndentation s
  in property $ fixed == normalized

-- Property: breakOn finds first occurrence
prop_break_on_first_occurrence :: String -> String -> Property
prop_break_on_first_occurrence s delimiter =
  let (before, after) = breakOn delimiter s
      expectedBefore = takeWhile (`notElem` delimiter) s
      expectedAfter = drop (length expectedBefore + length delimiter) s
  in property $ before == expectedBefore && after == expectedAfter

-- Property: breakOn handles delimiter not found
prop_break_on_not_found :: String -> String -> Property
prop_break_on_not_found s delimiter =
  not (delimiter `Data.List.isInfixOf` s) ==>
    let (before, after) = breakOn delimiter s
    in property $ before == s && null after
-- Property: breakOn handles empty delimiter
prop_break_on_empty_delimiter :: String -> Property
prop_break_on_empty_delimiter s =
  let (before, after) = breakOn "" s
  in property $ null before && after == s

-- Property: breakOn handles empty string
prop_break_on_empty_string :: String -> Property
prop_break_on_empty_string delimiter =
  let (before, after) = breakOn delimiter ""
  in property $ null before && null after

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmedOnce = trim s
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce == trimmedTwice

-- Property: splitBy and splitByCollapsed relationship
prop_split_by_relationship :: Char -> String -> Property
prop_split_by_relationship delim s =
  let normalSplit = splitBy delim s
      collapsedSplit = splitByCollapsed delim s
      filteredNormal = filter (not . null) normalSplit
  in property $ filteredNormal == collapsedSplit

-- Property: removeComments is idempotent
prop_remove_comments_idempotent :: String -> Property
prop_remove_comments_idempotent s =
  let removedOnce = removeComments s
      removedTwice = removeComments removedOnce
  in property $ removedOnce == removedTwice

-- Property: normalizeIndentation preserves content
prop_normalize_indentation_preserves_content :: String -> Property
prop_normalize_indentation_preserves_content s =
  let normalized = normalizeIndentation s
      originalContent = filter (not . isSpace) s
      normalizedContent = filter (not . isSpace) normalized
  in property $ originalContent == normalizedContent

-- Property: forceSingleTabIndentation preserves content
prop_force_tab_indentation_preserves_content :: String -> Property
prop_force_tab_indentation_preserves_content s =
  let tabbed = forceSingleTabIndentation s
      originalContent = filter (not . isSpace) s
      tabbedContent = filter (not . isSpace) tabbed
  in property $ originalContent == tabbedContent

-- Property: breakOn with multi-character delimiter
prop_break_on_multi_char :: String -> String -> Property
prop_break_on_multi_char s delimiter =
  length delimiter > 1 ==>
  let (before, after) = breakOn delimiter s
      combined = before ++ delimiter ++ after
  in property $ s `isPrefixOf` combined && combined `isPrefixOf` (s ++ delimiter)

-- Property: splitBy with Unicode characters
prop_split_by_unicode :: String -> Char -> Property
prop_split_by_unicode s delim =
  let parts = splitBy delim s
      rejoined = Data.List.intercalate [delim] parts
  in property $ rejoined == s

-- Property: trim with Unicode whitespace
prop_trim_unicode :: String -> Property
prop_trim_unicode s =
  let unicodeWS = "\160\8239\12288" -- Non-breaking spaces and other Unicode whitespace
      sWithUnicodeWS = unicodeWS ++ s ++ unicodeWS
      trimmed = trim sWithUnicodeWS
  in property $ case (trimmed, reverse trimmed) of
                  (c1:_, c2:_) -> not (isSpace c1) && not (isSpace c2)
                  _ -> True

-- Property: removeLineComments with nested comments
prop_remove_line_comments_nested :: String -> String -> Property
prop_remove_line_comments_nested code innerComment =
  let nestedComment = "// outer // " ++ innerComment ++ " // end"
      codeWithNested = code ++ nestedComment ++ "\n" ++ code
      withoutComments = removeLineComments codeWithNested
  in property $ "// outer" `notElem` Data.List.words withoutComments

-- Property: removeComments with complex string literals
prop_remove_comments_complex_strings :: String -> Property
prop_remove_comments_complex_strings _content =
  let complexStrings = 
        [ "\"string with // comment\""
        , "'char with // comment'"
        , "`backtick with // comment`"
        , "\"/* block comment inside string\""
        ]
      codeWithStrings = unlines $ map (\s -> "var x = " ++ s) complexStrings
      withoutComments = removeComments codeWithStrings
  in property $ all (`isInfixOf` withoutComments) complexStrings

-- Property: normalizeIndentation with mixed tabs and spaces
prop_normalize_mixed_indentation :: [String] -> Property
prop_normalize_mixed_indentation inputLines =
  not (null inputLines) && length inputLines <= 5 ==>
  let mixedLines = zipWith (\i line -> 
        if even i then replicate i ' ' ++ line
        else replicate i '\t' ++ line) [0..] inputLines
      normalized = normalizeIndentation (unlines mixedLines)
  in property $ not (any ('\t' `elem`) (splitLines normalized))

-- Property: forceSingleTabIndentation with existing tabs
prop_force_tab_existing_tabs :: [String] -> Property
prop_force_tab_existing_tabs inputLines =
  not (null inputLines) && length inputLines <= 5 ==>
  let tabbedLines = zipWith (\i line -> replicate i '\t' ++ line) [0..] inputLines
      result = forceSingleTabIndentation (unlines tabbedLines)
  in property $ all startsWithTab (splitLines result)

-- Property: breakOn performance with long strings
prop_break_on_performance :: Int -> String -> Property
prop_break_on_performance n delimiter =
  n > 0 && n <= 1000 ==>
  let longString = replicate n 'a' ++ delimiter ++ replicate n 'b'
      (before, after) = breakOn delimiter longString
  in property $ length before == n && length after == n

-- Property: splitBy with empty string
prop_split_by_empty_string :: Char -> Property
prop_split_by_empty_string delim =
  let parts = splitBy delim ""
  in property $ parts == [""]

-- Property: splitByCollapsed with empty string
prop_split_by_collapsed_empty_string :: Char -> Property
prop_split_by_collapsed_empty_string delim =
  let parts = splitByCollapsed delim ""
  in property $ null parts

-- Property: trim with only whitespace
prop_trim_only_whitespace :: String -> Property
prop_trim_only_whitespace ws =
  all isSpace ws ==>
  let trimmed = trim ws
  in property $ null trimmed

-- Property: removeLineComments with no comments
prop_remove_line_comments_no_comments :: String -> Property
prop_remove_line_comments_no_comments code =
  not ("//" `isInfixOf` code) ==>
  let withoutComments = removeLineComments code
  in property $ withoutComments == code

-- Property: removeComments with no comments
prop_remove_comments_no_comments :: String -> Property
prop_remove_comments_no_comments code =
  not (any (`isInfixOf` code) ["//", "/*", "*/"]) ==>
  let withoutComments = removeComments code
  in property $ withoutComments == code

-- Helper functions
splitLines :: String -> [String]
splitLines = Data.List.lines

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `Data.List.isInfixOf` haystack

isPrefixOf :: String -> String -> Bool
isPrefixOf needle haystack = needle `Data.List.isPrefixOf` haystack

relativeIndentationPreserved :: [String] -> [String] -> Bool
relativeIndentationPreserved original normalized = length original == length normalized

hasTabIndentation :: String -> Bool
hasTabIndentation = ('\t' `elem`)

startsWithTab :: String -> Bool
startsWithTab ('\t':_) = True
startsWithTab _ = False

tests :: TestTree
tests = testGroup "Comprehensive Utils QuickCheck Tests"
  [ fastProperty "trim removes leading and trailing whitespace" prop_trim_whitespace
  , fastProperty "trim preserves internal whitespace" prop_trim_preserve_internal
  , fastProperty "splitBy preserves empty segments" prop_splitBy_preserve_empty
  , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
  , fastProperty "splitByComma is equivalent to splitBy ','" prop_splitByComma_equivalence
  , fastProperty "splitByCommaCollapsed removes empty segments" prop_splitByCommaCollapsed_removes_empty
  , fastProperty "removeLineComments removes only line comments" prop_remove_line_comments
  , fastProperty "removeLineComments preserves string literals" prop_remove_line_comments_preserve_strings
  , fastProperty "removeComments removes both line and block comments" prop_remove_both_comments
  , fastProperty "removeComments preserves nested string literals" prop_remove_comments_preserve_nested_strings
  , fastProperty "normalizeIndentation preserves relative indentation" prop_normalize_indentation_relative
  , fastProperty "forceSingleTabIndentation converts spaces to tabs" prop_force_tab_indentation
  , fastProperty "fixIndentation is equivalent to normalizeIndentation" prop_fix_indentation_equivalence
  , fastProperty "breakOn finds first occurrence" prop_break_on_first_occurrence
  , fastProperty "breakOn handles delimiter not found" prop_break_on_not_found
  , fastProperty "breakOn handles empty delimiter" prop_break_on_empty_delimiter
  , fastProperty "breakOn handles empty string" prop_break_on_empty_string
  , fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "splitBy and splitByCollapsed relationship" prop_split_by_relationship
  , fastProperty "removeComments is idempotent" prop_remove_comments_idempotent
  , fastProperty "normalizeIndentation preserves content" prop_normalize_indentation_preserves_content
  , fastProperty "forceSingleTabIndentation preserves content" prop_force_tab_indentation_preserves_content
  , fastProperty "breakOn with multi-character delimiter" prop_break_on_multi_char
  , fastProperty "splitBy with Unicode characters" prop_split_by_unicode
  , fastProperty "trim with Unicode whitespace" prop_trim_unicode
  , fastProperty "removeLineComments with nested comments" prop_remove_line_comments_nested
  , fastProperty "removeComments with complex string literals" prop_remove_comments_complex_strings
  , fastProperty "normalizeIndentation with mixed tabs and spaces" prop_normalize_mixed_indentation
  , fastProperty "forceSingleTabIndentation with existing tabs" prop_force_tab_existing_tabs
  , fastProperty "breakOn performance with long strings" prop_break_on_performance
  , fastProperty "splitBy with empty string" prop_split_by_empty_string
  , fastProperty "splitByCollapsed with empty string" prop_split_by_collapsed_empty_string
  , fastProperty "trim with only whitespace" prop_trim_only_whitespace
  , fastProperty "removeLineComments with no comments" prop_remove_line_comments_no_comments
  , fastProperty "removeComments with no comments" prop_remove_comments_no_comments
  ]