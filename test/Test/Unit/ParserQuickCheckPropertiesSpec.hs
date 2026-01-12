module Test.Unit.ParserQuickCheckPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser
import SourceLocation (SourceSpan(..), SourcePos(..))
import Data.Char (isSpace)

-- ============================================================================
-- Directive Parsing Properties
-- ============================================================================

-- Property: parseBool should handle valid boolean values correctly
prop_parse_bool_valid :: Bool -> Property
prop_parse_bool_valid b = 
  let input = if b then "on" else "off"
      result = parseBool input
  in property $ result == Right b

-- Property: parseBool should handle alternative boolean values
prop_parse_bool_alternatives :: Bool -> Property
prop_parse_bool_alternatives b = 
  let input = if b then "true" else "false"
      result = parseBool input
  in property $ result == Right b

-- Property: parseBool should reject invalid values
prop_parse_bool_invalid :: String -> Property
prop_parse_bool_invalid s = 
  let validValues = ["on", "off", "true", "false"]
      normalized = map toLower (filter (not . isSpace) s)
  in property $ 
    if normalized `elem` validValues
    then case parseBool normalized of
           Right _ -> property True
           Left _ -> property False  -- Should not happen for valid values
    else case parseBool s of
           Right _ -> property False  -- Should not succeed for invalid values
           Left _ -> property True
  where
    toLower [] = []
    toLower (c:cs) = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) : toLower cs else c : toLower cs

-- ============================================================================
-- String Processing Properties
-- ============================================================================

-- Property: trimRight should not add characters
prop_trim_right_no_addition :: String -> Property
prop_trim_right_no_addition s = length (trimRight s) <= length s

-- Property: trimRight should remove trailing whitespace
prop_trim_right_removes_trailing :: String -> Property
prop_trim_right_removes_trailing s = 
  let trimmed = trimRight s
  in property $ 
    if null trimmed 
    then True  -- All characters were whitespace
    else not (isSpace (last trimmed))

-- Property: leadingIndentation should count leading spaces/tabs
prop_leading_indentation_counts_leading :: String -> Property
prop_leading_indentation_counts_leading s = 
  let indent = leadingIndentation s
      leadingChars = takeWhile isIndentChar s
  in property $ indent == length leadingChars
  where
    isIndentChar c = c == ' ' || c == '\t'

-- Property: leadingIndentation should ignore non-indent characters
prop_leading_indentation_ignores_non_indent :: String -> Char -> Property
prop_leading_indentation_ignores_non_indent s c = 
  let s' = if c `elem` [' ', '\t'] then 'x' else c
      input = s' : s
      indent = leadingIndentation input
  in property $ indent == 0

-- ============================================================================
-- Curly Brace Delta Properties
-- ============================================================================

-- Property: curlyDelta should count opening braces
prop_curly_delta_opening :: String -> Property
prop_curly_delta_opening s = 
  let openBraces = "{"}
      result = curlyDelta openBraces
  in property $ result == 1

-- Property: curlyDelta should count closing braces
prop_curly_delta_closing :: String -> Property
prop_curly_delta_closing s = 
  let closeBraces = "}"
      result = curlyDelta closeBraces
  in property $ result == -1

-- Property: curlyDelta should handle balanced braces
prop_curly_delta_balanced :: String -> Property
prop_curly_delta_balanced s = 
  let balanced = "{some {code} here}"
      result = curlyDelta balanced
  in property $ result == 0

-- Property: curlyDelta should ignore braces in strings
prop_curly_delta_ignores_strings :: String -> Property
prop_curly_delta_ignores_strings s = 
  let withString = "\"{not a brace}\" {real brace}"
      result = curlyDelta withString
  in property $ result == 1

-- Property: curlyDelta should ignore braces in comments
prop_curly_delta_ignores_comments :: String -> Property
prop_curly_delta_ignores_comments s = 
  let withComment = "// {not a brace}\n {real brace}"
      result = curlyDelta withComment
  in property $ result == 1

-- ============================================================================
-- File Directives Properties
-- ============================================================================

-- Property: defaultFileDirectives should have all Nothing values
prop_default_file_directives :: Property
prop_default_file_directives = 
  let FileDirectives ownership deps constraints = defaultFileDirectives
  in property $ ownership == Nothing && deps == Nothing && constraints == Nothing

-- Property: defaultBlockDirectives should have all Nothing values
prop_default_block_directives :: Property
prop_default_block_directives = 
  let BlockDirectives ownership deps constraints = defaultBlockDirectives
  in property $ ownership == Nothing && deps == Nothing && constraints == Nothing

-- ============================================================================
-- Parser Round-trip Properties
-- ============================================================================

-- Property: Simple file directives should parse correctly
prop_simple_file_directive :: Bool -> Property
prop_simple_file_directive b = 
  let input = "//! ownership: " ++ (if b then "on" else "off")
      result = parseTypus input
  in property $ 
    case result of
      Left _ -> property False
      Right typusFile -> 
        case fdOwnership (tfDirectives typusFile) of
          Nothing -> property False
          Just locatedValue -> locValue locatedValue == b

-- Property: Simple block directives should parse correctly
prop_simple_block_directive :: Bool -> Property
prop_simple_block_directive b = 
  let input = "{//! ownership: " ++ (if b then "on" else "off") ++ "}\nint x = 0;"
      result = parseTypus input
  in property $ 
    case result of
      Left _ -> property False
      Right typusFile -> 
        case tfBlocks typusFile of
          [] -> property False
          (block:_) -> 
            case bdOwnership (cbDirectives block) of
              Nothing -> property False
              Just locatedValue -> locValue locatedValue == b

-- Property: Empty content should parse to file with no blocks
prop_empty_content :: Property
prop_empty_content = 
  let input = ""
      result = parseTypus input
  in property $ 
    case result of
      Left _ -> property False
      Right typusFile -> null (tfBlocks typusFile)

-- Property: Content without directives should parse to blocks with default directives
prop_content_without_directives :: String -> Property
prop_content_without_directives s = 
  let input = if null s then "int x = 0;" else s
      result = parseTypus input
  in property $ 
    case result of
      Left _ -> property False
      Right typusFile -> 
        case tfBlocks typusFile of
          [] -> property False
          (block:_) -> cbDirectives block == defaultBlockDirectives

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: Unclosed block should result in error
prop_unclosed_block_error :: String -> Property
prop_unclosed_block_error s = 
  let input = "{//! ownership: on}\n" ++ s
      result = parseTypus input
  in property $ 
    case result of
      Left _ -> property True  -- Expected error
      Right _ -> property False  -- Should not succeed

-- Property: Invalid boolean value should result in error
prop_invalid_boolean_error :: String -> Property
prop_invalid_boolean_error s = 
  let validValues = ["on", "off", "true", "false"]
      normalized = map toLower (filter (not . isSpace) s)
  in property $ 
    if normalized `elem` validValues
    then property True  -- Skip valid values
    else case parseTypus ("//! ownership: " ++ s) of
           Left _ -> property True  -- Expected error
           Right _ -> property False  -- Should not succeed
  where
    toLower [] = []
    toLower (c:cs) = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) : toLower cs else c : toLower cs

tests :: TestTree
tests = testGroup "Parser QuickCheck Properties Tests"
  [ testProperty "parseBool valid" prop_parse_bool_valid
  , testProperty "parseBool alternatives" prop_parse_bool_alternatives
  , testProperty "parseBool invalid" prop_parse_bool_invalid
  , testProperty "trimRight no addition" prop_trim_right_no_addition
  , testProperty "trimRight removes trailing" prop_trim_right_removes_trailing
  , testProperty "leadingIndentation counts leading" prop_leading_indentation_counts_leading
  , testProperty "leadingIndentation ignores non-indent" prop_leading_indentation_ignores_non_indent
  , testProperty "curlyDelta opening" prop_curly_delta_opening
  , testProperty "curlyDelta closing" prop_curly_delta_closing
  , testProperty "curlyDelta balanced" prop_curly_delta_balanced
  , testProperty "curlyDelta ignores strings" prop_curly_delta_ignores_strings
  , testProperty "curlyDelta ignores comments" prop_curly_delta_ignores_comments
  , testProperty "defaultFileDirectives" prop_default_file_directives
  , testProperty "defaultBlockDirectives" prop_default_block_directives
  , testProperty "simple file directive" prop_simple_file_directive
  , testProperty "simple block directive" prop_simple_block_directive
  , testProperty "empty content" prop_empty_content
  , testProperty "content without directives" prop_content_without_directives
  , testProperty "unclosed block error" prop_unclosed_block_error
  , testProperty "invalid boolean error" prop_invalid_boolean_error
  ]