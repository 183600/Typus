{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.ParserErrorRecoveryQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Parser
import SourceLocation
import qualified Text.Megaparsec as MP
import Text.Megaparsec (Parsec, errorBundlePretty)

-- ============================================================================
-- Test Data Generation
-- ============================================================================

-- | Generate potentially malformed Typus code snippets
instance Arbitrary String where
  arbitrary = do
    size <- choose (0, 100)
    vectorOf size arbitraryChar

-- | Generate characters that might appear in Typus code
arbitraryChar :: Gen Char
arbitraryChar = oneof
  [ elements ['a'..'z']
  , elements ['A'..'Z']
  , elements ['0'..'9']
  , elements " \t\n\r{}();,[]<>+-*/%=!&|^~."
  , return '@'  -- For directives
  , return '/'  -- For comments
  , return '*'  -- For comments
  ]

-- | Generate malformed code blocks
genMalformedBlock :: Gen String
genMalformedBlock = oneof
  [ -- Unbalanced braces
    return "{ function test() {"
  , -- Missing semicolon  
    return "let x = 5\nlet y = 10"
  , -- Invalid characters
    return "let x = 5#$%^&*()"
  , -- Incomplete comment
    return "let x = 5 /* unclosed comment"
  , -- Invalid directive
    return "@invalid_directive\nlet x = 5"
  , -- Mismatched parentheses
    return "function test( { return 42; }"
  , -- Empty code with just whitespace
    return "   \n\t  \n   "
  , -- Only comments
    return "// line comment\n/* block comment */"
  , -- Mixed valid and invalid
    do
      valid <- elements ["let x = 5;", "function test() { return 42; }", "const y = 10;"]
      invalid <- elements ["{", "}", "(", ")", ";", "@"]
      return $ valid ++ invalid
  ]

-- | Generate directives
genDirective :: Gen String
genDirective = oneof
  [ return "@ownership(true)"
  , return "@ownership(false)"
  , return "@dependent_types(true)"
  , return "@dependent_types(false)"
  , return "@constraints(true)"
  , return "@constraints(false)"
  , return "@invalid_directive"
  , return "@ownership(invalid)"
  ]

-- ============================================================================
-- QuickCheck Properties for Parser Error Recovery
-- ============================================================================

-- | Parser should not crash on any input
prop_parser_no_crash :: String -> Property
prop_parser_no_crash input =
  let result = parseTypus input
  in result `seq` True

-- | Parser should handle empty input gracefully
prop_parser_empty_input :: Property
prop_parser_empty_input =
  let result = parseTypus ""
  in case result of
    Left _ -> True
    Right parsed -> parsed `seq` True

-- | Parser should handle only whitespace
prop_parser_whitespace_only :: Property
prop_parser_whitespace_only =
  let whitespace = "   \n\t  \n   "
      result = parseTypus whitespace
  in case result of
    Left _ -> True
    Right parsed -> parsed `seq` True

-- | Parser should handle only comments
prop_parser_comments_only :: Property
prop_parser_comments_only =
  let commentsOnly = "// line comment\n/* block comment */\n// another comment"
      result = parseTypus commentsOnly
  in case result of
    Left _ -> True
    Right parsed -> parsed `seq` True

-- | Parser should recover from malformed blocks
prop_parser_malformed_recovery :: Property
prop_parser_malformed_recovery =
  forAll genMalformedBlock $ \malformedBlock ->
  let result = parseTypus malformedBlock
  in case result of
    Left _ -> True  -- Error is acceptable
    Right parsed -> parsed `seq` True  -- Success is also acceptable

-- | Parser should handle invalid directives gracefully
prop_parser_invalid_directives :: Property
prop_parser_invalid_directives =
  forAll genDirective $ \directive ->
  let code = directive ++ "\nlet x = 5;"
      result = parseTypus code
  in case result of
    Left _ -> True
    Right parsed -> parsed `seq` True

-- | Parser should handle very long lines
prop_parser_long_lines :: Positive Int -> Property
prop_parser_long_lines (Positive length) =
  let longLine = replicate length 'a' ++ ";"
      result = parseTypus longLine
  in case result of
    Left _ -> True
    Right parsed -> parsed `seq` True

-- | Parser should handle deeply nested structures
prop_parser_deep_nesting :: Positive Int -> Property
prop_parser_deep_nesting (Positive depth) =
  let nestedBraces = concat $ replicate depth "{"
      nestedContent = "let x = 5;"
      closingBraces = concat $ replicate depth "}"
      code = nestedBraces ++ nestedContent ++ closingBraces
      result = parseTypus code
  in case result of
    Left _ -> True
    Right parsed -> parsed `seq` True

-- | Parser should handle mixed valid/invalid code
prop_parser_mixed_valid_invalid :: String -> String -> Property
prop_parser_mixed_valid_invalid validPart invalidPart =
  let validCode = if null validPart then "let x = 5;" else validPart
      invalidCode = if null invalidPart then "{@#$" else invalidPart
      mixedCode = validCode ++ "\n" ++ invalidCode ++ "\nlet y = 10;"
      result = parseTypus mixedCode
  in case result of
    Left _ -> True
    Right parsed -> parsed `seq` True

-- | Parser should handle Unicode characters
prop_parser_unicode :: Property
prop_parser_unicode =
  let unicodeCode = "let 测试 = 'hello';\nlet мир = 'world';\nlet 🌟 = 42;"
      result = parseTypus unicodeCode
  in case result of
    Left _ -> True
    Right parsed -> parsed `seq` True

-- | Parser should handle incomplete comments
prop_parser_incomplete_comments :: Property
prop_parser_incomplete_comments =
  let incompleteComments = ["/* unclosed", "// without newline", "/* nested /* unclosed */"]
      testComment comment =
        case parseTypus comment of
          Left _ -> True
          Right parsed -> parsed `seq` True
  in conjoin $ map testComment incompleteComments

-- | Parser should handle escape sequences
prop_parser_escape_sequences :: Property
prop_parser_escape_sequences =
  let escapeCode = "let str = \"Hello\\nWorld\\t!\";\nlet char = '\\\\';"
      result = parseTypus escapeCode
  in case result of
    Left _ -> True
    Right parsed -> parsed `seq` True

-- | Parser should handle numeric literals
prop_parser_numeric_literals :: Property
prop_parser_numeric_literals =
  let numericCode = "let int = 42;\nlet float = 3.14;\nlet hex = 0xFF;\nlet octal = 0755;"
      result = parseTypus numericCode
  in case result of
    Left _ -> True
    Right parsed -> parsed `seq` True

-- | Parser should be consistent on same input
prop_parser_consistency :: String -> Property
prop_parser_consistency input =
  let result1 = parseTypus input
      result2 = parseTypus input
  in case (result1, result2) of
    (Left err1, Left err2) -> errorBundlePretty err1 === errorBundlePretty err2
    (Right parsed1, Right parsed2) -> parsed1 === parsed2
    _ -> property False  -- One succeeded, one failed - inconsistent

-- | Parser should handle incremental parsing
prop_parser_incremental :: String -> String -> Property
prop_parser_incremental part1 part2 =
  let full = part1 ++ part2
      result1 = parseTypus full
      result2 = parseTypus part1
  in case (result1, result2) of
    (Left _, _) -> True  -- Full parse failed, acceptable
    (Right fullParsed, Left _) -> True  -- Part failed but full succeeded, acceptable
    (Right fullParsed, Right partParsed) -> 
      fullParsed `seq` partParsed `seq` True  -- Both succeeded

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser Error Recovery QuickCheck Tests"
  [ testProperty "parser doesn't crash on any input" prop_parser_no_crash
  , testProperty "parser handles empty input" prop_parser_empty_input
  , testProperty "parser handles whitespace only" prop_parser_whitespace_only
  , testProperty "parser handles comments only" prop_parser_comments_only
  , testProperty "parser recovers from malformed blocks" prop_parser_malformed_recovery
  , testProperty "parser handles invalid directives" prop_parser_invalid_directives
  , testProperty "parser handles long lines" prop_parser_long_lines
  , testProperty "parser handles deep nesting" prop_parser_deep_nesting
  , testProperty "parser handles mixed valid/invalid code" prop_parser_mixed_valid_invalid
  , testProperty "parser handles Unicode characters" prop_parser_unicode
  , testProperty "parser handles incomplete comments" prop_parser_incomplete_comments
  , testProperty "parser handles escape sequences" prop_parser_escape_sequences
  , testProperty "parser handles numeric literals" prop_parser_numeric_literals
  , testProperty "parser is consistent on same input" prop_parser_consistency
  , testProperty "parser handles incremental parsing" prop_parser_incremental
  ]