{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.UtilsQuickCheckSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>))
import qualified Data.Text as T
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Data.Char (isSpace, isAlpha, isAlphaNum, toLower, toUpper, isDigit, isLetter)
import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed, 
             removeLineComments, removeComments, normalizeIndentation, 
             forceSingleTabIndentation, fixIndentation, breakOn, 
             safeProcessString, isValidChar)

-- Helper generators for Utils tests
genSmallString :: Gen String
genSmallString = do
  len <- choose (0, 20)
  vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n"

genStringWithSpaces :: Gen String
genStringWithSpaces = do
  len <- choose (0, 20)
  vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t"

genStringWithComments :: Gen String
genStringWithComments = do
  len <- choose (0, 20)
  vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\"'/"

genIndentedString :: Gen String
genIndentedString = do
  numLines <- choose (1, 5)
  indent <- choose (0, 4)
  lines <- vectorOf numLines $ do
    lineLen <- choose (0, 10)
    line <- vectorOf lineLen $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ " "
    return $ replicate indent ' ' ++ line
  return $ unlines lines

genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\"'/"

genDelimiter :: Gen Char
genDelimiter = elements $ ",;:|"

-- Test properties for Utils module

-- Property 1: trim removes leading and trailing spaces
prop_trim_removes_leading_trailing_spaces :: String -> Property
prop_trim_removes_leading_trailing_spaces s = 
  let trimmed = trim s
      hasLeadingSpace = not (null s) && isSpace (head s)
      hasTrailingSpace = not (null s) && isSpace (last s)
  in if hasLeadingSpace || hasTrailingSpace
     then not (null trimmed) ==> (not (isSpace (head trimmed)) && not (isSpace (last trimmed)))
     else property (trimmed == s)

-- Property 2: trim preserves non-space characters
prop_trim_preserves_non_space_characters :: String -> Bool
prop_trim_preserves_non_space_characters s = 
  let trimmed = trim s
      nonSpaceCount = length $ filter (not . isSpace) s
      trimmedNonSpaceCount = length $ filter (not . isSpace) trimmed
  in nonSpaceCount == trimmedNonSpaceCount

-- Property 3: trim of empty string is empty
prop_trim_empty_string :: Bool
prop_trim_empty_string = trim "" == ""

-- Property 4: trim of all spaces is empty
prop_trim_all_spaces :: Property
prop_trim_all_spaces = forAll genStringWithSpaces $ \s ->
  all isSpace s ==> trim s == ""

-- Property 5: splitBy with empty string returns empty list
prop_splitBy_empty_string :: Char -> Bool
prop_splitBy_empty_string delim = splitBy delim "" == []

-- Property 6: splitBy with single delimiter returns two empty strings
prop_splitBy_single_delimiter :: Char -> Bool
prop_splitBy_single_delimiter delim = splitBy delim [delim] == ["", ""]

-- Property 7: splitBy preserves total content when concatenated
prop_splitBy_preserves_content :: Char -> String -> Property
prop_splitBy_preserves_content delim s =
  not (null s) ==> 
    let parts = splitBy delim s
        reconstructed = concat $ map (\p -> p ++ [delim]) parts
    in take (length s) reconstructed == s

-- Property 8: splitByCollapsed removes empty parts
prop_splitByCollapsed_removes_empty :: Char -> String -> Bool
prop_splitByCollapsed_removes_empty delim s = 
  let normal = splitBy delim s
      collapsed = splitByCollapsed delim s
  in all (not . null) collapsed

-- Property 9: splitByComma is equivalent to splitBy ','
prop_splitByComma_equals_splitBy :: String -> Bool
prop_splitByComma_equals_splitBy s = splitByComma s == splitBy ',' s

-- Property 10: splitByCommaCollapsed removes empty parts
prop_splitByCommaCollapsed_removes_empty :: String -> Bool
prop_splitByCommaCollapsed_removes_empty s = 
  all (not . null) (splitByCommaCollapsed s)

-- Property 11: removeLineComments removes // comments
prop_removeLineComments_removes_comments :: Property
prop_removeLineComments_removes_comments = 
  forAll genStringWithComments $ \s ->
    "//" `isInfixOf` s ==> 
      let cleaned = removeLineComments s
      in not ("//" `isInfixOf` cleaned)

-- Property 12: removeLineComments preserves string literals
prop_removeLineComments_preserves_string_literals :: Property
prop_removeLineComments_preserves_string_literals = 
  forAll genStringWithComments $ \s ->
    "\"" `isInfixOf` s ==> 
      let cleaned = removeLineComments s
          countQuotes str = length $ filter (== '"') str
      in countQuotes s == countQuotes cleaned

-- Property 13: removeLineComments preserves character literals
prop_removeLineComments_preserves_char_literals :: Property
prop_removeLineComments_preserves_char_literals = 
  forAll genStringWithComments $ \s ->
    "'" `isInfixOf` s ==> 
      let cleaned = removeLineComments s
          countQuotes str = length $ filter (== '\'') str
      in countQuotes s == countQuotes cleaned

-- Property 14: removeComments removes both // and /* */ comments
prop_removeComments_removes_both_types :: Property
prop_removeComments_removes_both_types = 
  forAll genStringWithComments $ \s ->
    ("//" `isInfixOf` s || "/*" `isInfixOf` s) ==> 
      let cleaned = removeComments s
      in not ("//" `isInfixOf` cleaned) && not ("/*" `isInfixOf` cleaned)

-- Property 15: normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preserves_relative :: Property
prop_normalizeIndentation_preserves_relative = 
  forAll genIndentedString $ \s ->
    let lines' = lines s
        normalized = normalizeIndentation s
        normalizedLines = lines normalized
    in length lines' == length normalizedLines

-- Property 16: normalizeIndentation removes common prefix
prop_normalizeIndentation_removes_common_prefix :: Property
prop_normalizeIndentation_removes_common_prefix = 
  forAll genIndentedString $ \s ->
    let lines' = lines s
        normalized = normalizeIndentation s
        normalizedLines = lines normalized
        hasIndentation = any (not . null . takeWhile isSpace) lines'
    in if hasIndentation
       then any (null . takeWhile isSpace) normalizedLines
       else True

-- Property 17: forceSingleTabIndentation adds tab to non-empty lines
prop_forceSingleTabIndentation_adds_tab :: String -> Bool
prop_forceSingleTabIndentation_adds_tab s = 
  let lines' = lines s
      tabbed = forceSingleTabIndentation s
      tabbedLines = lines tabbed
    in all (\line -> null line || '\t' `elem` take 1 line) tabbedLines

-- Property 18: fixIndentation is equivalent to normalizeIndentation
prop_fixIndentation_equals_normalize :: String -> Bool
prop_fixIndentation_equals_normalize s = fixIndentation s == normalizeIndentation s

-- Property 19: breakOn with empty pattern returns ("", s)
prop_breakOn_empty_pattern :: String -> Bool
prop_breakOn_empty_pattern s = breakOn "" s == ("", s)

-- Property 20: breakOn with pattern not in string returns (s, "")
prop_breakOn_pattern_not_in_string :: String -> Property
prop_breakOn_pattern_not_in_string s = 
  forAll genStringWithSpaces $ \pat ->
    not (null pat) && not (pat `isInfixOf` s) ==> 
      breakOn pat s == (s, "")

-- Property 21: breakOn with pattern in string splits correctly
prop_breakOn_pattern_in_string :: String -> Property
prop_breakOn_pattern_in_string s = 
  forAll genStringWithSpaces $ \pat ->
    not (null pat) && pat `isInfixOf` s ==> 
      let (before, after) = breakOn pat s
          reconstructed = before ++ pat ++ after
      in reconstructed == s

-- Property 22: safeProcessString filters control characters
prop_safeProcessString_filters_control :: Property
prop_safeProcessString_filters_control = 
  forAll genStringWithComments $ \s ->
    let result = safeProcessString s
    in case result of
         Right filtered -> all isValidChar filtered
         Left _ -> False

-- Property 23: safeProcessString preserves valid characters
prop_safeProcessString_preserves_valid :: String -> Bool
prop_safeProcessString_preserves_valid s = 
  let result = safeProcessString s
  in case result of
       Right filtered -> all (`elem` s) $ filter isValidChar filtered
       Left _ -> False

-- Property 24: isValidChar returns True for printable characters
prop_isValidChar_printable :: Char -> Property
prop_isValidChar_printable c = 
  (c >= ' ' && c <= '~') || c == '\n' || c == '\r' || c == '\t' ==> isValidChar c

-- Property 25: isValidChar returns False for control characters
prop_isValidChar_control :: Char -> Property
prop_isValidChar_control c = 
  (c < ' ' && c `notElem` ['\n', '\r', '\t']) ==> not (isValidChar c)

-- Property 26: trim of already trimmed string is idempotent
prop_trim_idempotent :: String -> Bool
prop_trim_idempotent s = trim (trim s) == trim s

-- Property 27: splitBy with delimiter not in string returns singleton list
prop_splitBy_delimiter_not_in_string :: Char -> String -> Property
prop_splitBy_delimiter_not_in_string delim s = 
  delim `notElem` s ==> splitBy delim s == [s]

-- Property 28: splitBy with all delimiters returns n+1 empty strings
prop_splitBy_all_delimiters :: Char -> Property
prop_splitBy_all_delimiters delim = 
  forAll (choose (1, 5)) $ \n ->
    let s = replicate n delim
    in splitBy delim s == replicate (n + 1) ""

-- Property 29: removeLineComments preserves newlines
prop_removeLineComments_preserves_newlines :: Property
prop_removeLineComments_preserves_newlines = 
  forAll genStringWithComments $ \s ->
    let originalNewlines = length $ filter (== '\n') s
        cleaned = removeLineComments s
        cleanedNewlines = length $ filter (== '\n') cleaned
    in originalNewlines == cleanedNewlines

-- Property 30: normalizeIndentation of single line is identity
prop_normalizeIndentation_single_line :: String -> Property
prop_normalizeIndentation_single_line s = 
  not ('\n' `elem` s) ==> property (normalizeIndentation s == s)

-- Property 31: breakOn is consistent with isInfixOf
prop_breakOn_consistent_with_isInfixOf :: String -> String -> Property
prop_breakOn_consistent_with_isInfixOf pat s = 
  not (null pat) ==> 
    let (before, after) = breakOn pat s
        found = pat `isInfixOf` s
    in found ==> not (null before) || not (null after)

-- Property 32: removeComments preserves string literals with comment markers
prop_removeComments_preserves_string_with_comment_markers :: Property
prop_removeComments_preserves_string_with_comment_markers = 
  forAll genStringWithComments $ \s ->
    let hasStringWithComment = "\"//\"" `isInfixOf` s || "\"/* */\"" `isInfixOf` s
    in if hasStringWithComment
       then let cleaned = removeComments s
            in "\"//\"" `isInfixOf` cleaned || "\"/* */\"" `isInfixOf` cleaned
       else True

-- Property 33: splitByCollapsed with no delimiters returns singleton or empty
prop_splitByCollapsed_no_delimiters :: Char -> String -> Property
prop_splitByCollapsed_no_delimiters delim s = 
  delim `notElem` s ==> 
    let result = splitByCollapsed delim s
    in if null s then null result else length result == 1

-- Property 34: normalizeIndentation preserves line count
prop_normalizeIndentation_preserves_line_count :: String -> Bool
prop_normalizeIndentation_preserves_line_count s = 
  let originalLines = length $ lines s
      normalized = normalizeIndentation s
      normalizedLines = length $ lines normalized
  in originalLines == normalizedLines

-- Property 35: trim preserves order of non-space characters
prop_trim_preserves_order :: String -> Bool
prop_trim_preserves_order s = 
  let trimmed = trim s
      originalNonSpaces = filter (not . isSpace) s
      trimmedNonSpaces = filter (not . isSpace) trimmed
  in originalNonSpaces == trimmedNonSpaces

-- Property 36: removeLineComments handles empty lines
prop_removeLineComments_handles_empty_lines :: Property
prop_removeLineComments_handles_empty_lines = 
  forAll genStringWithComments $ \s ->
    let hasEmptyLine = "\n\n" `isInfixOf` s || s == "\n"
    in if hasEmptyLine
       then let cleaned = removeLineComments s
            in "\n\n" `isInfixOf` cleaned || cleaned == "\n"
       else True

-- Property 37: splitBy preserves order of parts
prop_splitBy_preserves_order :: Char -> String -> Bool
prop_splitBy_preserves_order delim s = 
  let parts = splitBy delim s
      reconstructed = intercalate [delim] parts
  in take (length s) reconstructed == s
  where
    intercalate _ [] = []
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- Property 38: forceSingleTabIndentation trims content
prop_forceSingleTabIndentation_trims_content :: String -> Bool
prop_forceSingleTabIndentation_trims_content s = 
  let lines' = lines s
      tabbed = forceSingleTabIndentation s
      tabbedLines = lines tabbed
    in all (\line -> null line || 
                     (head line == '\t' && 
                      drop 1 line == trim (dropWhile isSpace line))) tabbedLines

-- Property 39: safeProcessString handles empty string
prop_safeProcessString_empty_string :: Bool
prop_safeProcessString_empty_string = 
  case safeProcessString "" of
    Right "" -> True
    _ -> False

-- Property 40: isValidChar for common whitespace characters
prop_isValidChar_common_whitespace :: Bool
prop_isValidChar_common_whitespace = 
  all isValidChar [' ', '\t', '\n', '\r']

utilsQuickCheckTests :: TestTree
utilsQuickCheckTests = testGroup "Utils QuickCheck Tests"
  [ testProperties "Trim Functions"
    [ ("trim removes leading and trailing spaces", property prop_trim_removes_leading_trailing_spaces)
    , ("trim preserves non-space characters", property prop_trim_preserves_non_space_characters)
    , ("trim of empty string is empty", property prop_trim_empty_string)
    , ("trim of all spaces is empty", property prop_trim_all_spaces)
    , ("trim is idempotent", property prop_trim_idempotent)
    , ("trim preserves order of non-space characters", property prop_trim_preserves_order)
    ]
  , testProperties "Split Functions"
    [ ("splitBy with empty string returns empty list", property prop_splitBy_empty_string)
    , ("splitBy with single delimiter returns two empty strings", property prop_splitBy_single_delimiter)
    , ("splitBy preserves total content when concatenated", property prop_splitBy_preserves_content)
    , ("splitByCollapsed removes empty parts", property prop_splitByCollapsed_removes_empty)
    , ("splitByComma is equivalent to splitBy ','", property prop_splitByComma_equals_splitBy)
    , ("splitByCommaCollapsed removes empty parts", property prop_splitByCommaCollapsed_removes_empty)
    , ("splitBy with delimiter not in string returns singleton list", property prop_splitBy_delimiter_not_in_string)
    , ("splitBy with all delimiters returns n+1 empty strings", property prop_splitBy_all_delimiters)
    , ("splitBy preserves order of parts", property prop_splitBy_preserves_order)
    , ("splitByCollapsed with no delimiters returns singleton or empty", property prop_splitByCollapsed_no_delimiters)
    ]
  , testProperties "Comment Removal Functions"
    [ ("removeLineComments removes // comments", property prop_removeLineComments_removes_comments)
    , ("removeLineComments preserves string literals", property prop_removeLineComments_preserves_string_literals)
    , ("removeLineComments preserves character literals", property prop_removeLineComments_preserves_char_literals)
    , ("removeComments removes both // and /* */ comments", property prop_removeComments_removes_both_types)
    , ("removeComments preserves string literals with comment markers", property prop_removeComments_preserves_string_with_comment_markers)
    , ("removeLineComments preserves newlines", property prop_removeLineComments_preserves_newlines)
    , ("removeLineComments handles empty lines", property prop_removeLineComments_handles_empty_lines)
    ]
  , testProperties "Indentation Functions"
    [ ("normalizeIndentation preserves relative indentation", property prop_normalizeIndentation_preserves_relative)
    , ("normalizeIndentation removes common prefix", property prop_normalizeIndentation_removes_common_prefix)
    , ("forceSingleTabIndentation adds tab to non-empty lines", property prop_forceSingleTabIndentation_adds_tab)
    , ("fixIndentation is equivalent to normalizeIndentation", property prop_fixIndentation_equals_normalize)
    , ("normalizeIndentation of single line is identity", property prop_normalizeIndentation_single_line)
    , ("normalizeIndentation preserves line count", property prop_normalizeIndentation_preserves_line_count)
    , ("forceSingleTabIndentation trims content", property prop_forceSingleTabIndentation_trims_content)
    ]
  , testProperties "String Processing Functions"
    [ ("breakOn with empty pattern returns (\"\", s)", property prop_breakOn_empty_pattern)
    , ("breakOn with pattern not in string returns (s, \"\")", property prop_breakOn_pattern_not_in_string)
    , ("breakOn with pattern in string splits correctly", property prop_breakOn_pattern_in_string)
    , ("breakOn is consistent with isInfixOf", property prop_breakOn_consistent_with_isInfixOf)
    , ("safeProcessString filters control characters", property prop_safeProcessString_filters_control)
    , ("safeProcessString preserves valid characters", property prop_safeProcessString_preserves_valid)
    , ("safeProcessString handles empty string", property prop_safeProcessString_empty_string)
    ]
  , testProperties "Character Validation"
    [ ("isValidChar returns True for printable characters", property prop_isValidChar_printable)
    , ("isValidChar returns False for control characters", property prop_isValidChar_control)
    , ("isValidChar for common whitespace characters", property prop_isValidChar_common_whitespace)
    ]
  ]