module Test.Unit.BasicQuickCheckTestsSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
import SourceLocation (SourcePos(..), startPos)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (listToMaybe)

-- | Test that trim removes leading and trailing whitespace
prop_trim_removes_whitespace :: String -> Property
prop_trim_removes_whitespace s = 
  let trimmed = trim s
      hasLeadingSpace = case listToMaybe s of
                          Nothing -> False
                          Just h -> isSpace h
      hasTrailingSpace = not (null s) && isSpace (last s)
      trimmedHasNoLeadingSpace = case listToMaybe trimmed of
                                   Nothing -> True
                                   Just h -> not (isSpace h)
      trimmedHasNoTrailingSpace = not (null trimmed) && not (isSpace (last trimmed))
  in property $ 
    if hasLeadingSpace || hasTrailingSpace
    then not (null trimmed) ==> (trimmedHasNoLeadingSpace && trimmedHasNoTrailingSpace)
    else property (trimmed == s)

-- | Test that trim doesn't change non-whitespace characters
prop_trim_preserves_content :: String -> Property
prop_trim_preserves_content s = 
  let trimmed = trim s
      filtered = filter (not . isSpace) s
      filteredTrimmed = filter (not . isSpace) trimmed
  in property $ filtered == filteredTrimmed

-- | Test that splitBy preserves all characters
prop_splitBy_preserves_content :: Char -> String -> Property
prop_splitBy_preserves_content delim s = 
  let parts = splitBy delim s
      rejoined = intercalate [delim] parts
  in property $ rejoined == s

-- | Test that startPos has correct initial values
prop_start_pos_values :: Property
prop_start_pos_values = property $
  posLine startPos == 1 &&
  posColumn startPos == 1 &&
  posOffset startPos == 0

-- | Test that removeLineComments removes // comments
prop_remove_line_comments_removes_comments :: String -> Property
prop_remove_line_comments_removes_comments s = 
  let withComment = s ++ "// this is a comment"
      withoutComment = removeLineComments withComment
  in property $ not ("//" `isInfixOf` withoutComment)

-- | Test that removeLineComments preserves content before //
prop_remove_line_comments_preserves_before :: String -> Property
prop_remove_line_comments_preserves_before s = 
  let comment = "// this is a comment"
      withComment = s ++ comment
      withoutComment = removeLineComments withComment
  in property $ s `isPrefixOf` withoutComment

-- | Test that splitByCommaCollapsed removes empty parts
prop_split_by_comma_collapsed_no_empty :: String -> Property
prop_split_by_comma_collapsed_no_empty s = 
  let parts = splitByCommaCollapsed s
  in property $ all (not . null) parts

-- | Test that splitByComma preserves empty parts
prop_split_by_comma_preserves_empty :: String -> Property
prop_split_by_comma_preserves_empty s = 
  let parts = splitByComma s
      hasConsecutiveCommas = ",," `isInfixOf` s
      startsWithComma = case listToMaybe s of
                          Nothing -> False
                          Just h -> h == ','
      endsWithComma = not (null s) && last s == ','
  in property $ 
    if hasConsecutiveCommas || startsWithComma || endsWithComma
    then any null parts
    else not (any null parts)

-- | Test that normalizeIndentation preserves relative indentation
prop_normalize_indentation_preserves_relative :: String -> Property
prop_normalize_indentation_preserves_relative s = 
  let indented = "  " ++ s ++ "\n    " ++ s ++ "\n  " ++ s
      normalized = normalizeIndentation indented
      linesNormalized = lines normalized
  in property $ length linesNormalized == 3 &&
                all (s `isSuffixOf`) linesNormalized

-- | Test that safeProcessString handles special characters
prop_safe_process_string_handles_special :: String -> Property
prop_safe_process_string_handles_special s = 
  let specialChars = "\n\t\r\\\"'"
      withSpecial = s ++ specialChars
      processed = safeProcessString withSpecial
  in case processed of
       Right str -> property $ length str >= length s
       Left _ -> property False

-- | Test that isValidChar correctly identifies valid characters
prop_is_valid_char_properties :: Char -> Property
prop_is_valid_char_properties c = 
  let isValid = isValidChar c
  in property $ 
    if isAlphaNum c 
    then isValid
    else isValid == (c `elem` " \t\n\r.,;:!()[]{}+-*/=<>&|^~%")

-- | Test that removeLineComments handles multiple lines
prop_remove_line_comments_multiline :: String -> String -> Property
prop_remove_line_comments_multiline s1 s2 = 
  let line1 = s1 ++ "// comment1"
      line2 = s2 ++ "// comment2"
      multiline = line1 ++ "\n" ++ line2
      result = removeLineComments multiline
      linesResult = lines result
  in property $ length linesResult == 2 && 
                not (any ("//" `isInfixOf`) linesResult)

tests :: TestTree
tests = testGroup "Basic QuickCheck Tests"
  [ testProperty "trim removes whitespace" prop_trim_removes_whitespace
  , testProperty "trim preserves content" prop_trim_preserves_content
  , testProperty "splitBy preserves content" prop_splitBy_preserves_content
  , testProperty "startPos values" prop_start_pos_values
  , testProperty "removeLineComments removes comments" prop_remove_line_comments_removes_comments
  , testProperty "removeLineComments preserves before" prop_remove_line_comments_preserves_before
  , testProperty "splitByCommaCollapsed removes empty parts" prop_split_by_comma_collapsed_no_empty
  , testProperty "splitByComma preserves empty parts" prop_split_by_comma_preserves_empty
  , testProperty "normalizeIndentation preserves relative" prop_normalize_indentation_preserves_relative
  , testProperty "safeProcessString handles special" prop_safe_process_string_handles_special
  , testProperty "isValidChar properties" prop_is_valid_char_properties
  , testProperty "removeLineComments handles multiple lines" prop_remove_line_comments_multiline
  ]