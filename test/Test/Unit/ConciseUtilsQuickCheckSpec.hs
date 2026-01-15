{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.ConciseUtilsQuickCheckSpec where

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

-- Trim tests
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

prop_trim_no_leading_trailing_spaces :: String -> Property
prop_trim_no_leading_trailing_spaces s = 
  not (null (trim s)) ==> 
  not (isSpace (head (trim s))) && not (isSpace (last (trim s)))

-- Split tests
prop_split_by_length :: String -> Char -> Property
prop_split_by_length s c = length (splitBy c s) >= 0

prop_split_by_comma_consistency :: String -> Property
prop_split_by_comma_consistency s = splitBy ',' s === splitByComma s

prop_split_by_collapsed_no_empty_adjacent :: String -> Char -> Property
prop_split_by_collapsed_no_empty_adjacent s c = 
  not (null c) ==> 
  not (elem "" (take (length (splitByCollapsed c s) - 1) (splitByCollapsed c s)))

-- Comment removal tests
prop_remove_line_comments_no_crash :: String -> Property
prop_remove_line_comments_no_crash s = property $ length (removeLineComments s) >= 0

prop_remove_comments_no_crash :: String -> Property
prop_remove_comments_no_crash s = property $ length (removeComments s) >= 0

-- Indentation tests
prop_normalize_indentation_no_crash :: String -> Property
prop_normalize_indentation_no_crash s = property $ length (normalizeIndentation s) >= 0

prop_fix_indentation_consistency :: String -> Property
prop_fix_indentation_consistency s = fixIndentation s === normalizeIndentation s

-- Break on tests
prop_break_on_finds_delimiter :: String -> String -> Property
prop_break_on_finds_delimiter s delim = 
  not (null delim) && delim `isInfixOf` s ==>
  let (left, right) = breakOn delim s
  in delim `isSuffixOf` left && delim `isPrefixOf` right

-- String processing tests
prop_safe_process_string_idempotent :: String -> Property
prop_safe_process_string_idempotent s = safeProcessString (safeProcessString s) === safeProcessString s

prop_is_valid_char_check :: Char -> Property
prop_is_valid_char_check c = property $ isValidChar c == isAlpha c || isDigit c || c `elem` "_-"

tests :: TestTree
tests = testGroup "Concise Utils QuickCheck Tests"
  [ testProperties "Trim Tests"
    [ ("trim idempotent", prop_trim_idempotent)
    , ("trim no leading/trailing spaces", prop_trim_no_leading_trailing_spaces)
    ]
  , testProperties "Split Tests"
    [ ("split by length", prop_split_by_length)
    , ("split by comma consistency", prop_split_by_comma_consistency)
    , ("split by collapsed no empty adjacent", prop_split_by_collapsed_no_empty_adjacent)
    ]
  , testProperties "Comment Removal Tests"
    [ ("remove line comments no crash", prop_remove_line_comments_no_crash)
    , ("remove comments no crash", prop_remove_comments_no_crash)
    ]
  , testProperties "Indentation Tests"
    [ ("normalize indentation no crash", prop_normalize_indentation_no_crash)
    , ("fix indentation consistency", prop_fix_indentation_consistency)
    ]
  , testProperties "String Processing Tests"
    [ ("break on finds delimiter", prop_break_on_finds_delimiter)
    , ("safe process string idempotent", prop_safe_process_string_idempotent)
    , ("is valid char check", prop_is_valid_char_check)
    ]
  ]