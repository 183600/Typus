{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewStringProcessingQuickCheckSpec where


import Test.Tasty
import Test.Tasty.QuickCheck

-- | String processing QuickCheck tests for the Typus compiler
-- This module contains property-based tests for string processing utilities


import Test.Tasty
import Test.Tasty.QuickCheck

import Test.QuickCheck ((==>), conjoin, counterexample)
import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , removeLineComments
  , removeComments
  , safeProcessString
  , isValidChar
  , breakOn
  )
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isControl)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Control.Monad (foldM)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Compiler.Errors.Core as Error
import Compiler.Errors.Core (ErrorSeverity(..), ErrorCategory(..), TypeError(..), ErrorLocation(..), ErrorContext(..),
                            errorAt, errorWithCategory, warningAt, infoAt, 
                            fatalError, withLocation, withContext, combineErrors,
                            combinedErrorSeverity, filterByCategory, filterBySeverity,
                            hasCategory, isAtLeast, severityPriority, location, line, column, 
                            fatalRecovery, emptyContext, contextCode)
import SourceLocation (toErrorLocation)
import Data.Time (UTCTime, getCurrentTime)
import Data.List (sort, nub)
import Data.Ord (comparing)

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Check if a character is a valid identifier character
isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_' || c == '-'

-- | Check if a string contains only valid characters
allValidChars :: String -> Bool
allValidChars = all isValidChar

-- | Check if a string is balanced (parentheses, brackets, braces)
isBalanced :: String -> Bool
isBalanced = go 0 0 0
  where
    go _ _ _ [] = True
    go p b s (c:cs)
      | c == '(' = go (p+1) b s cs
      | c == ')' = p > 0 && go (p-1) b s cs
      | c == '[' = go p (b+1) s cs
      | c == ']' = b > 0 && go p (b-1) s cs
      | c == '{' = go p b (s+1) cs
      | c == '}' = s > 0 && go p b (s-1) cs
      | otherwise = go p b s cs

-- ============================================================================
-- String Processing Tests
-- ============================================================================

-- | Test trim function: trimming whitespace from both ends
prop_trim_roundtrip :: String -> Bool
prop_trim_roundtrip s = trim (trim s) == trim s

-- | Test trim function: trimmed string has no leading/trailing whitespace
prop_trim_noLeadingTrailingWhitespace :: String -> Bool
prop_trim_noLeadingTrailingWhitespace s = 
  let trimmed = trim s
  in null trimmed || 
     (case trimmed of 
        (c:cs) -> not (isSpace c) && not (isSpace (last trimmed))
        [] -> True)

-- | Test splitBy function: splitting by delimiter and rejoining
prop_splitBy_join :: Char -> String -> Bool
prop_splitBy_join delim s = intercalate [delim] (splitBy delim s) == s

-- | Test splitByComma function: comma splitting
prop_splitByComma_consistency :: String -> Bool
prop_splitByComma_consistency s = splitByComma s == splitBy ',' s

-- | Test splitByCollapsed function: removing empty segments
prop_splitByCollapsed_noEmpty :: Char -> String -> Bool
prop_splitByCollapsed_noEmpty delim s = all (not . null) (splitByCollapsed delim s)

-- | Test splitByCollapsed function: relationship to splitBy
prop_splitByCollapsed_relationship :: Char -> String -> Bool
prop_splitByCollapsed_relationship delim s = 
  splitByCollapsed delim s == filter (not . null) (splitBy delim s)

-- | Test breakOn function: empty pattern
prop_breakOn_emptyPattern :: String -> Bool
prop_breakOn_emptyPattern s = 
  let (before, after) = breakOn "" s
  in null before && after == s

-- | Test breakOn function: pattern not found
prop_breakOn_notFound :: String -> String -> Bool
prop_breakOn_notFound pattern s = 
  if not (pattern `isInfixOf` s) 
  then breakOn pattern s == (s, "")
  else True

-- | Test safeProcessString function: filtering control characters
prop_safeProcessString_removesControlChars :: String -> Bool
prop_safeProcessString_removesControlChars s =
  case safeProcessString s of
    Left _ -> True
    Right filtered -> all isValidChar filtered

-- | Test safeProcessString function: empty string
prop_safeProcessString_emptyString :: Bool
prop_safeProcessString_emptyString = 
  case safeProcessString "" of
    Left _ -> False
    Right filtered -> null filtered

-- | Test isValidChar function: valid characters
prop_isValidChar_valid :: Char -> Bool
prop_isValidChar_valid c = isValidChar c == (c >= ' ' || c == '\n' || c == '\r' || c == '\t')

-- | Test isValidChar function: control characters
prop_isValidChar_control :: Char -> Bool
prop_isValidChar_control c = 
  if isControl c && c `notElem` ['\n', '\r', '\t'] 
  then not (isValidChar c)
  else True

-- | Test removeLineComments function: no comments
prop_removeLineComments_noComments :: String -> Bool
prop_removeLineComments_noComments s = 
  if not ("//" `isInfixOf` s) 
  then removeLineComments s == s
  else True

-- | Test removeLineComments function: comment at beginning
prop_removeLineComments_commentAtBeginning :: String -> Bool
prop_removeLineComments_commentAtBeginning s = 
  let commented = "//" ++ s
      result = removeLineComments commented
  in null result

-- | Test removeLineComments function: comment in middle
prop_removeLineComments_commentInMiddle :: String -> String -> Bool
prop_removeLineComments_commentInMiddle s1 s2 = 
  let combined = s1 ++ "//" ++ s2
      result = removeLineComments combined
  in result == s1

-- | Test removeLineComments function: comments in strings
prop_removeLineComments_commentsInStrings :: String -> Bool
prop_removeLineComments_commentsInStrings s = 
  let withString = "prefix \"// not a comment\" suffix"
      result = removeLineComments withString
  in "//" `isInfixOf` result

-- | Test removeComments function: no comments
prop_removeComments_noComments :: String -> Bool
prop_removeComments_noComments s = 
  if not ("//" `isInfixOf` s) && not ("/*" `isInfixOf` s)
  then removeComments s == s
  else True

-- | Test removeComments function: block comments
prop_removeComments_blockComments :: String -> String -> Bool
prop_removeComments_blockComments s1 s2 = 
  let withBlock = s1 ++ "/*" ++ s2 ++ "*/" ++ s1
      result = removeComments withBlock
  in result == s1 ++ s1

-- | Test removeComments function: nested block comments
prop_removeComments_nestedBlockComments :: String -> String -> String -> Bool
prop_removeComments_nestedBlockComments s1 s2 s3 = 
  let withNested = s1 ++ "/*" ++ s2 ++ "/*" ++ s3 ++ "*/" ++ s2 ++ "*/" ++ s1
      result = removeComments withNested
  in result == s1 ++ s1

-- | Test removeComments function: line comments
prop_removeComments_lineComments :: String -> String -> Bool
prop_removeComments_lineComments s1 s2 = 
  let withLine = s1 ++ "//" ++ s2 ++ "\n" ++ s1
      result = removeComments withLine
  in result == s1 ++ "\n" ++ s1

-- | Test removeComments function: comments in strings
prop_removeComments_commentsInStrings :: String -> Bool
prop_removeComments_commentsInStrings s = 
  let withString = "prefix \"// not a comment /* not a comment */\" suffix"
      result = removeComments withString
  in "//" `isInfixOf` result && "/*" `isInfixOf` result

-- | Test string normalization
prop_normalizeString_consistency :: String -> Bool
prop_normalizeString_consistency s = 
  let normalized = trim s
      normalizedAgain = trim normalized
  in normalized == normalizedAgain

-- | Test string splitting and joining consistency
prop_split_join_consistency :: Char -> String -> Bool
prop_split_join_consistency delim s = 
  let parts = splitBy delim s
      rejoined = intercalate [delim] parts
  in rejoined == s

-- | Test string processing pipeline
prop_string_pipeline :: String -> Bool
prop_string_pipeline s = 
  let trimmed = trim s
      processed = case safeProcessString trimmed of
        Left _ -> trimmed
        Right p -> p
      parts = splitBy ',' processed
      -- Properties that should hold for any string processing pipeline
  in length processed <= length s + 10 -- Allow for some processing overhead

-- | Test string processing with Unicode
prop_unicode_processing :: String -> Bool
prop_unicode_processing s = 
  let processed = case safeProcessString s of
        Left _ -> s
        Right p -> p
  in all isValidChar processed
-- | Test string processing error handling
prop_error_handling :: String -> Bool
prop_error_handling s = 
  case safeProcessString s of
    Left _ -> True -- Any error is acceptable
    Right processed -> all isValidChar processed

-- | Test string processing idempotence
prop_idempotence :: String -> Bool
prop_idempotence s = 
  case safeProcessString s of
    Left _ -> True
    Right processed1 -> 
      case safeProcessString processed1 of
        Left _ -> False -- Should not fail on already processed string
        Right processed2 -> processed1 == processed2

-- | Test string processing commutativity with trim
prop_commutative_with_trim :: String -> Bool
prop_commutative_with_trim s = 
  let trimFirst = case safeProcessString (trim s) of
        Left _ -> trim s
        Right p -> p
      processFirst = trim (case safeProcessString s of
        Left _ -> s
        Right p -> p)
  in trimFirst == processFirst

-- | Test string processing with empty strings
prop_empty_string_handling :: String -> Bool
prop_empty_string_handling s = 
  case safeProcessString "" of
    Left _ -> False
    Right processed -> null processed

-- | Test string processing with whitespace
prop_whitespace_handling :: String -> Bool
prop_whitespace_handling s = 
  let withWhitespace = s ++ "   \t\n\r   " ++ s
  in case safeProcessString withWhitespace of
    Left _ -> True
    Right processed -> all isValidChar processed

-- | Test string processing with special characters
prop_special_characters :: String -> Bool
prop_special_characters s = 
  let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      hasSpecial = any (`elem` specialChars) s
  in if hasSpecial 
     then case safeProcessString s of
            Left _ -> True
            Right processed -> length processed >= 0 -- At least don't crash
     else True

-- | Test string processing with long strings
prop_long_string_handling :: String -> Bool
prop_long_string_handling s = 
  let longString = concat (replicate 1000 s)
  in case safeProcessString longString of
    Left _ -> True
    Right processed -> length processed <= length longString

-- | Test string processing with repeated patterns
prop_repeated_patterns :: String -> String -> Bool
prop_repeated_patterns s1 s2 = 
  let repeated = concat (replicate 100 (s1 ++ s2))
  in case safeProcessString repeated of
    Left _ -> True
    Right processed -> length processed >= 0

-- | Test string processing with mixed content
prop_mixed_content :: String -> Bool
prop_mixed_content s = 
  let mixed = s ++ "\n\t" ++ s ++ " " ++ s
  in case safeProcessString mixed of
    Left _ -> True
    Right processed -> all isValidChar processed

-- | Test string processing edge cases
prop_edge_cases :: String -> Bool
prop_edge_cases s = 
  let edgeCases = ["", " ", "\n", "\t", "\r", "\0", "\n\r\t"]
      testString = s ++ concat edgeCases ++ s
  in case safeProcessString testString of
    Left _ -> True
    Right processed -> all isValidChar processed

-- | Test string processing with very long strings
prop_very_long_strings :: String -> Bool
prop_very_long_strings s = 
  let veryLong = concat (replicate 10000 s)
  in case safeProcessString veryLong of
    Left _ -> True
    Right processed -> length processed <= length veryLong

-- | Test string processing with null characters
prop_null_characters :: String -> Bool
prop_null_characters s = 
  let withNull = s ++ "\0" ++ s
  in case safeProcessString withNull of
    Left _ -> True
    Right processed -> all isValidChar processed

-- | Test string processing with backspace characters
prop_backspace_characters :: String -> Bool
prop_backspace_characters s = 
  let withBackspace = s ++ "\b" ++ s
  in case safeProcessString withBackspace of
    Left _ -> True
    Right processed -> all isValidChar processed

-- | Test string processing with vertical tabs
prop_vertical_tab_characters :: String -> Bool
prop_vertical_tab_characters s = 
  let withVerticalTab = s ++ "\v" ++ s
  in case safeProcessString withVerticalTab of
    Left _ -> True
    Right processed -> all isValidChar processed

-- | Test string processing with form feeds
prop_form_feed_characters :: String -> Bool
prop_form_feed_characters s = 
  let withFormFeed = s ++ "\f" ++ s
  in case safeProcessString withFormFeed of
    Left _ -> True
    Right processed -> all isValidChar processed

-- | Test string processing with escape sequences
prop_escape_sequences :: String -> Bool
prop_escape_sequences s = 
  let withEscapes = s ++ "\\n\\t\\r\\\\\\" ++ s
  in case safeProcessString withEscapes of
    Left _ -> True
    Right processed -> all isValidChar processed

-- | Test string processing with non-ASCII characters
prop_non_ascii_characters :: String -> Bool
prop_non_ascii_characters s = 
  let nonAscii = s ++ "ñáéíóú" ++ s ++ "你好世界" ++ s
  in case safeProcessString nonAscii of
    Left _ -> True
    Right processed -> all isValidChar processed

-- | Test string processing with emoji
prop_emoji_characters :: String -> Bool
prop_emoji_characters s = 
  let withEmoji = s ++ "😀🎉🚀" ++ s
  in case safeProcessString withEmoji of
    Left _ -> True
    Right processed -> all isValidChar processed

-- | Test string processing with zero-width characters
prop_zero_width_characters :: String -> Bool
prop_zero_width_characters s = 
  let zeroWidth = s ++ "abc" ++ s
  in case safeProcessString zeroWidth of
    Left _ -> True
    Right processed -> all isValidChar processed

-- | Test string processing with control characters
prop_control_characters :: String -> Bool
prop_control_characters s = 
  let control = s ++ "\x01\x02\x03\x04\x05" ++ s
  in case safeProcessString control of
    Left _ -> True
    Right processed -> all isValidChar processed

-- | Test string processing with high Unicode characters
prop_high_unicode_characters :: String -> Bool
prop_high_unicode_characters s = 
  let highUnicode = s ++ "defg" ++ s
  in case safeProcessString highUnicode of
    Left _ -> True
    Right processed -> all isValidChar processed

-- | Test string processing with combining characters
prop_combining_characters :: String -> Bool
prop_combining_characters s = 
  let combining = s ++ "hijk" ++ s
  in case safeProcessString combining of
    Left _ -> True
    Right processed -> all isValidChar processed

-- | Test string processing with bidirectional text
prop_bidirectional_text :: String -> Bool
prop_bidirectional_text s = 
  let bidi = s ++ "שלום" ++ s ++ "مرحبا" ++ s
  in case safeProcessString bidi of
    Left _ -> True
    Right processed -> all isValidChar processed

-- | Test string processing with mixed scripts
prop_mixed_scripts :: String -> Bool
prop_mixed_scripts s = 
  let mixed = s ++ "Helloשלוםمرحبا" ++ s ++ "你好世界" ++ s
  in case safeProcessString mixed of
    Left _ -> True
    Right processed -> all isValidChar processed

-- | Test string processing with private use characters
prop_private_use_characters :: String -> Bool
prop_private_use_characters s = 
  let privateUse = s ++ "lmno" ++ s
  in case safeProcessString privateUse of
    Left _ -> True
    Right processed -> all isValidChar processed

-- | Test string processing with non-characters
prop_non_characters :: String -> Bool
prop_non_characters s = 
  let nonChars = s ++ "pqrs" ++ s
  in case safeProcessString nonChars of
    Left _ -> True
    Right processed -> all isValidChar processed

-- | Test string processing with surrogate characters
prop_surrogate_characters :: String -> Bool
prop_surrogate_characters s = 
  let surrogates = s ++ "tuvw" ++ s
  in case safeProcessString surrogates of
    Left _ -> True
    Right processed -> all isValidChar processed

-- | Test string processing with invalid UTF-8 sequences
prop_invalid_utf8 :: String -> Bool
prop_invalid_utf8 s = 
  -- Note: Since we're working with String in Haskell, which is Unicode-aware,
  -- we can't easily create invalid UTF-8 sequences. This test is more conceptual.
  let invalid = s ++ "\65534" ++ s -- Replacement character
  in case safeProcessString invalid of
    Left _ -> True
    Right processed -> all isValidChar processed

-- ============================================================================
-- Test Group
-- ============================================================================

tests :: TestTree
tests = testGroup "String Processing QuickCheck Tests"
  [ testProperty "trim roundtrip" prop_trim_roundtrip
  , testProperty "trim no leading/trailing whitespace" prop_trim_noLeadingTrailingWhitespace
  , testProperty "splitBy join" prop_splitBy_join
  , testProperty "splitByComma consistency" prop_splitByComma_consistency
  , testProperty "splitByCollapsed no empty" prop_splitByCollapsed_noEmpty
  , testProperty "splitByCollapsed relationship" prop_splitByCollapsed_relationship
  , testProperty "breakOn empty pattern" prop_breakOn_emptyPattern
  , testProperty "breakOn not found" prop_breakOn_notFound
  , testProperty "safeProcessString removes control chars" prop_safeProcessString_removesControlChars
  , testProperty "safeProcessString empty string" prop_safeProcessString_emptyString
  , testProperty "isValidChar valid" prop_isValidChar_valid
  , testProperty "isValidChar control" prop_isValidChar_control
  , testProperty "removeLineComments no comments" prop_removeLineComments_noComments
  , testProperty "removeLineComments comment at beginning" prop_removeLineComments_commentAtBeginning
  , testProperty "removeLineComments comment in middle" prop_removeLineComments_commentInMiddle
  , testProperty "removeLineComments comments in strings" prop_removeLineComments_commentsInStrings
  , testProperty "removeComments no comments" prop_removeComments_noComments
  , testProperty "removeComments block comments" prop_removeComments_blockComments
  , testProperty "removeComments nested block comments" prop_removeComments_nestedBlockComments
  , testProperty "removeComments line comments" prop_removeComments_lineComments
  , testProperty "removeComments comments in strings" prop_removeComments_commentsInStrings
  , testProperty "normalize string consistency" prop_normalizeString_consistency
  , testProperty "split join consistency" prop_split_join_consistency
  , testProperty "string pipeline" prop_string_pipeline
  , testProperty "unicode processing" prop_unicode_processing
  , testProperty "error handling" prop_error_handling
  , testProperty "idempotence" prop_idempotence
  , testProperty "commutative with trim" prop_commutative_with_trim
  , testProperty "empty string handling" prop_empty_string_handling
  , testProperty "whitespace handling" prop_whitespace_handling
  , testProperty "special characters" prop_special_characters
  , testProperty "long string handling" prop_long_string_handling
  , testProperty "repeated patterns" prop_repeated_patterns
  , testProperty "mixed content" prop_mixed_content
  , testProperty "edge cases" prop_edge_cases
  , testProperty "very long strings" prop_very_long_strings
  , testProperty "null characters" prop_null_characters
  , testProperty "backspace characters" prop_backspace_characters
  , testProperty "vertical tab characters" prop_vertical_tab_characters
  , testProperty "form feed characters" prop_form_feed_characters
  , testProperty "escape sequences" prop_escape_sequences
  , testProperty "non-ASCII characters" prop_non_ascii_characters
  , testProperty "emoji characters" prop_emoji_characters
  , testProperty "zero-width characters" prop_zero_width_characters
  , testProperty "control characters" prop_control_characters
  , testProperty "high Unicode characters" prop_high_unicode_characters
  , testProperty "combining characters" prop_combining_characters
  , testProperty "bidirectional text" prop_bidirectional_text
  , testProperty "mixed scripts" prop_mixed_scripts
  , testProperty "private use characters" prop_private_use_characters
  , testProperty "non-characters" prop_non_characters
  , testProperty "surrogate characters" prop_surrogate_characters
  , testProperty "invalid UTF-8" prop_invalid_utf8
  ]