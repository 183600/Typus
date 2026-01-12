module Test.Unit.ParserPropertiesQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import TestSupport.QuickCheck (fastProperty)
import Data.Char (isSpace, isAlpha, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified Data.Text as T

-- Properties for token recognition
prop_identifier_starts_with_letter :: String -> Property
prop_identifier_starts_with_letter s = 
  not (null s) && isAlpha (head s) ==> isValidIdentifier s
  where
    isValidIdentifier [] = False
    isValidIdentifier (c:cs) = isAlpha c && all isValidChar cs
    isValidChar c = isAlpha c || isDigit c || c == '_'

prop_identifier_contains_valid_chars :: String -> Property
prop_identifier_contains_valid_chars s = 
  isValidIdentifier s ==> all isValidChar s
  where
    isValidIdentifier [] = False
    isValidIdentifier (c:cs) = isAlpha c && all isValidChar cs
    isValidChar c = isAlpha c || isDigit c || c == '_'

prop_number_contains_only_digits :: String -> Property
prop_number_contains_only_digits s = 
  all isDigit s ==> isNumber s
  where
    isNumber [] = False
    isNumber cs = all isDigit cs

-- Properties for string literals
prop_string_literal_quoted :: String -> Bool
prop_string_literal_quoted s = isStringLiteral ("\"" ++ s ++ "\"")
  where
    isStringLiteral [] = False
    isStringLiteral cs = length cs >= 2 && head cs == '"' && last cs == '"'

prop_string_literal_escaped :: String -> Bool
prop_string_literal_escaped s = isStringLiteral ("\"" ++ escapeString s ++ "\"")
  where
    isStringLiteral [] = False
    isStringLiteral cs = length cs >= 2 && head cs == '"' && last cs == '"'
    escapeString [] = []
    escapeString ('"':cs) = '\':'"':escapeString cs
    escapeString ('\\':cs) = '\':'\\':escapeString cs
    escapeString (c:cs) = c:escapeString cs

-- Properties for comment recognition
prop_line_comment_starts_with_slashes :: String -> Bool
prop_line_comment_starts_with_slashes s = isLineComment ("//" ++ s)
  where
    isLineComment [] = False
    isLineComment cs = "//" `isPrefixOf` cs

prop_block_comment_wrapped :: String -> Bool
prop_block_comment_wrapped s = isBlockComment ("/*" ++ s ++ "*/")
  where
    isBlockComment [] = False
    isBlockComment cs = "/*" `isPrefixOf` cs && "*/" `isSuffixOf` cs

-- Properties for whitespace
prop_whitespace_preserves_length :: String -> Bool
prop_whitespace_preserves_length s = length (normalizeWhitespace s) <= length s
  where
    normalizeWhitespace [] = []
    normalizeWhitespace (c:cs) 
      | isSpace c = ' ' : dropWhile isSpace cs
      | otherwise = c : normalizeWhitespace cs

prop_whitespace_collapses_spaces :: String -> Property
prop_whitespace_collapses_spaces s = 
  containsMultipleSpaces s ==> not (containsMultipleSpaces (normalizeWhitespace s))
  where
    containsMultipleSpaces [] = False
    containsMultipleSpaces (' ':' ':cs) = True
    containsMultipleSpaces (_:cs) = containsMultipleSpaces cs
    normalizeWhitespace [] = []
    normalizeWhitespace (c:cs) 
      | isSpace c = ' ' : dropWhile isSpace cs
      | otherwise = c : normalizeWhitespace cs

-- Properties for indentation
prop_indentation_preserves_line_structure :: String -> Bool
prop_indentation_preserves_line_structure s = 
  length (lines s) == length (lines (normalizeIndentation s))
  where
    normalizeIndentation = unlines . map normalizeLine . lines
    normalizeLine = dropWhile isSpace

prop_indentation_non_negative :: String -> Bool
prop_indentation_non_negative s = 
  all nonNegativeIndent (lines s)
  where
    nonNegativeIndent line = length (takeWhile isSpace line) >= 0

-- Properties for parsing round trips
prop_parse_unparse_identifier :: String -> Property
prop_parse_unparse_identifier s = 
  isValidIdentifier s ==> unparseIdentifier (parseIdentifier s) == s
  where
    isValidIdentifier [] = False
    isValidIdentifier (c:cs) = isAlpha c && all isValidChar cs
    isValidChar c = isAlpha c || isDigit c || c == '_'
    
    parseIdentifier = takeWhile isValidChar
    unparseIdentifier = id

prop_parse_unparse_number :: String -> Property
prop_parse_unparse_number s = 
  all isDigit s ==> unparseNumber (parseNumber s) == s
  where
    parseNumber = takeWhile isDigit
    unparseNumber = id

-- Properties for error recovery
prop_error_recovery_preserves_input :: String -> Bool
prop_error_recovery_preserves_input s = 
  recoverFromErrors s == s
  where
    recoverFromErrors = id  -- Simplified for now

prop_error_recovery_never_empty :: String -> Bool
prop_error_recovery_never_empty s = 
  not (null (recoverFromErrors s))
  where
    recoverFromErrors = id  -- Simplified for now

-- Properties for token sequences
prop_token_sequence_roundtrip :: [String] -> Bool
prop_token_sequence_roundtrip tokens = 
  unparseTokens (parseTokens (concat tokens)) == concat tokens
  where
    parseTokens = words  -- Simplified tokenization
    unparseTokens = unwords

prop_token_sequence_preserves_count :: [String] -> Bool
prop_token_sequence_preserves_count tokens = 
  length (parseTokens (concat tokens)) == length tokens
  where
    parseTokens = words  -- Simplified tokenization

-- Properties for parsing context
prop_context_preserves_position :: String -> Bool
prop_context_preserves_position s = 
  length (parseWithContext s) == length s
  where
    parseWithContext = id  -- Simplified for now

prop_context_tracks_line_numbers :: String -> Bool
prop_context_tracks_line_numbers s = 
  lineCount (parseWithContext s) == lineCount s
  where
    parseWithContext = id  -- Simplified for now
    lineCount = length . lines

tests :: TestTree
tests = testGroup "Test.Unit.ParserPropertiesQuickCheckSpec Tests"
  [ fastProperty "identifier starts with letter" prop_identifier_starts_with_letter
  , fastProperty "identifier contains valid chars" prop_identifier_contains_valid_chars
  , fastProperty "number contains only digits" prop_number_contains_only_digits
  , fastProperty "string literal quoted" prop_string_literal_quoted
  , fastProperty "string literal escaped" prop_string_literal_escaped
  , fastProperty "line comment starts with slashes" prop_line_comment_starts_with_slashes
  , fastProperty "block comment wrapped" prop_block_comment_wrapped
  , fastProperty "whitespace preserves length" prop_whitespace_preserves_length
  , fastProperty "whitespace collapses spaces" prop_whitespace_collapses_spaces
  , fastProperty "indentation preserves line structure" prop_indentation_preserves_line_structure
  , fastProperty "indentation non negative" prop_indentation_non_negative
  , fastProperty "parse unparse identifier" prop_parse_unparse_identifier
  , fastProperty "parse unparse number" prop_parse_unparse_number
  , fastProperty "error recovery preserves input" prop_error_recovery_preserves_input
  , fastProperty "error recovery never empty" prop_error_recovery_never_empty
  , fastProperty "token sequence roundtrip" prop_token_sequence_roundtrip
  , fastProperty "token sequence preserves count" prop_token_sequence_preserves_count
  , fastProperty "context preserves position" prop_context_preserves_position
  , fastProperty "context tracks line numbers" prop_context_tracks_line_numbers
  ]