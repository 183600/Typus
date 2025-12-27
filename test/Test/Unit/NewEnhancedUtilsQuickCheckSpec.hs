{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewEnhancedUtilsQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import Utils (trim, splitBy, splitByComma, removeLineComments, removeComments, normalizeIndentation, breakOn)
import Data.Char (isSpace)
import Data.List (isPrefixOf)

-- Test trim function properties
prop_trim_idempotent :: String -> Bool
prop_trim_idempotent s = trim (trim s) == trim s

prop_trim_no_leading_trailing_spaces :: String -> Bool
prop_trim_no_leading_trailing_spaces s = 
  let trimmed = trim s
  in null trimmed || 
     (not (isSpace (head trimmed)) && not (isSpace (last trimmed)))

prop_trim_empty_string :: Bool
prop_trim_empty_string = trim "" == ""

-- Test splitBy function properties
prop_splitBy_consistency :: Char -> String -> Bool
prop_splitBy_consistency c s = concat (splitBy c s) == s

prop_splitBy_empty_segments :: Char -> String -> Bool
prop_splitBy_empty_segments c s = 
  let segments = splitBy c s
      hasConsecutiveDelim = any (\(x:y:_) -> x == c && y == c) (zip s (tail s))
  in if hasConsecutiveDelim 
     then any null segments
     else True

prop_splitBy_comma_special :: String -> Bool
prop_splitBy_comma_special s = splitBy ',' s == splitByComma s

-- Test removeLineComments function properties
prop_removeLineComments_preserves_non_comment_lines :: String -> Bool
prop_removeLineComments_preserves_non_comment_lines s = 
  let linesWithoutComments = lines (removeLineComments s)
      originalLines = lines s
      nonCommentLines = filter (not . isPrefixOf "//") originalLines
  in length linesWithoutComments == length nonCommentLines

prop_removeLine_comments_no_string_interference :: String -> Bool
prop_removeLine_comments_no_string_interference s = 
  let result = removeLineComments s
      hasStringLiteral = "\"" `isInfixOf` s
  in if hasStringLiteral
     then "//" `isInfixOf` result || not ("//" `isInfixOf` s)
     else True

-- Test removeComments function properties
prop_remove_comments_preserves_line_structure :: String -> Bool
prop_remove_comments_preserves_line_structure s = 
  let result = removeComments s
      originalLines = length (lines s)
      resultLines = length (lines result)
  in resultLines <= originalLines

-- Test normalizeIndentation function properties
prop_normalize_indentation_preserves_relative_structure :: String -> Bool
prop_normalize_indentation_preserves_relative_structure s = 
  let normalized = normalizeIndentation s
      originalLines = filter (not . all isSpace) (lines s)
      normalizedLines = filter (not . all isSpace) (lines normalized)
  in length originalLines == length normalizedLines

prop_normalize_indentation_removes_common_prefix :: String -> Bool
prop_normalize_indentation_removes_common_prefix s = 
  let normalized = normalizeIndentation s
      nonEmptyLines = filter (not . null . trim) (lines normalized)
  in if null nonEmptyLines
     then True
     else all (\line -> null line || not (isSpace (head line))) nonEmptyLines

-- Test breakOn function properties
prop_break_on_empty_pattern :: String -> Bool
prop_break_on_empty_pattern s = breakOn "" s == ("", s)

prop_break_on_found_pattern :: String -> String -> Property
prop_break_on_found_pattern pat s = 
  (pat /= "" && pat `isInfixOf` s) ==> 
  let (before, after) = breakOn pat s
  in before ++ pat ++ after == s

prop_break_on_not_found_pattern :: String -> String -> Property
prop_break_on_not_found_pattern pat s = 
  (pat /= "" && not (pat `isInfixOf` s)) ==> 
  let (before, after) = breakOn pat s
  in before == s && after == ""

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests