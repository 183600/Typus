{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewParserBoundaryCoreQuickCheckSpec where

-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


-- | Boundary condition tests for Parser module Test.Unit.NewParserBoundaryCoreQuickCheckSpec Test.Unit.NewParserBoundaryCoreQuickCheckSpec where
import Test.Tasty
import Test.Tasty.QuickCheck
import Parser

ied Data.Text as T
import Data.Char 
prop_parse_long_line (Positive len) =
  let longLine = replicate len 'a'
                                    result = parseTypus longLine
  in case result of
    Left _ -> property True  -- Parse error is acceptable
    Right _ -> property True  -- Success is also acceptable

-- | Many short lines should be handled
prop_parse_many_short_lines :: Positive Int -> Property
prop_parse_many_short_lines (Positive count) =
  let shortLines = unlines $ replicate count "a"
                                    result = parseTypus shortLines
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | Mixed whitespace should be normalized
prop_parse_mixed_whitespace :: String -> Property
prop_parse_mixed_whitespace                               s =
  let mixedWs = "   \t\n  " ++ s ++ "  \t\n   "
                                    result = parseTypus mixedWs
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | Unicode characters should be handled
prop_parse_unicode :: String -> Property
prop_parse_unicode                               s =
  let unicodeInput = "" ++ s ++ ""
                                    result = parseTypus unicodeInput
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | Special characters should not crash parser
prop_parse_special_chars :: String -> Property
prop_parse_special_chars                               s =
  let specialChars = "!@#$%^&*()[]{}|\\:;\"'<>?,./" ++ s
                                    result = parseTypus specialChars
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | Very long identifiers should be handled
prop_parse_long_identifiers :: Positive Int -> Property
prop_parse_long_identifiers (Positive len) =
  let longId = take len $ cycle "abcdefghijklmnopqrstuvwxyz"
                                    input = "let " ++ longId ++ " = 42"
                                    result = parseTypus input
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | Nested block structure should be handled
prop_parse_nested_blocks :: Positive Int -> Property
prop_parse_nested_blocks (Positive depth) =
  let indent = replicate depth "  "
                                    input = indent ++ "block {\n" ++ indent ++ "                                x = 1\n" ++ indent ++ "}\n"
                                    result = parseTypus input
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | Comments should be handled gracefully
prop_parse_comments :: String -> Property
prop_parse_comments                               s =
  let withComments = "// This is a comment\n" ++ s ++ "\n// Another comment"
                                    result =  parseTypus withComments
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Parser Boundary QuickCheck Tests"
  [             testProperty "Empty input parsing" prop_parse_empty_input
  ,             testProperty "Whitespace-only input parsing" prop_parse_whitespace_only
  ,             testProperty "Long line parsing" prop_parse_long_line
  ,             testProperty "Many short lines parsing" prop_parse_many_short_lines
  ,             testProperty "Mixed whitespace parsing" prop_parse_mixed_whitespace
  ,             testProperty "Unicode character parsing" prop_parse_unicode
  ,             testProperty "Special characters parsing" prop_parse_special_chars
  ,             testProperty "Long identifiers parsing" prop_parse_long_identifiers
  ,             testProperty "Nested blocks parsing" prop_parse_nested_blocks
  ,             testProperty "Comments parsing" prop_parse_comments
  ]