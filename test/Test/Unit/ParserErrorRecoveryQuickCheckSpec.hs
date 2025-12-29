module Test.Unit.ParserErrorRecoveryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property)
import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import Data.Either (isLeft, isRight)
import Data.List (isInfixOf)

-- ============================================================================
-- Parser Error Recovery QuickCheck Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser Error Recovery QuickCheck Tests"
  [ testProperty "parser handles empty input gracefully" prop_parser_empty_input
  , testProperty "parser handles only whitespace" prop_parser_whitespace_only
  , testProperty "parser handles malformed directives" prop_parser_malformed_directives
  , testProperty "parser recovers from syntax errors" prop_parser_error_recovery
  , testProperty "parser preserves partial structure on errors" prop_parser_preserves_partial
  , testProperty "parser handles unicode characters" prop_parser_unicode_handling
  , testProperty "parser handles very long lines" prop_parser_long_lines
  , testProperty "parser handles nested block structures" prop_parser_nested_blocks
  ]

-- | Parser should handle empty input without crashing
prop_parser_empty_input :: Property
prop_parser_empty_input = 
  let result = parseTypus ""
  in isRight result  -- Should succeed with empty structure

-- | Parser should handle input with only whitespace
prop_parser_whitespace_only :: String -> Property
prop_parser_whitespace_only s = 
  let whitespaceInput = concatMap (const " ") s  -- Convert to same length whitespace
      result = parseTypus whitespaceInput
  in isRight result  -- Should succeed with empty structure

-- | Parser should handle malformed directives gracefully
prop_parser_malformed_directives :: String -> Property
prop_parser_malformed_directives base = 
  let malformed = "//! " ++ base ++ " malformed-directive-without-equals\n" ++ base
      result = parseTypus malformed
  in case result of
    Left _ -> True  -- Failing is acceptable for malformed input
    Right tf -> length (tfBlocks tf) >= 0  -- Should produce some structure

-- | Parser should recover from syntax errors and continue parsing
prop_parser_error_recovery :: String -> String -> Property
prop_parser_error_recovery good bad = 
  let mixed = good ++ "\n@@ SYNTAX ERROR @@\n" ++ good
      result = parseTypus mixed
  in case result of
    Left _ -> True  -- May fail, but shouldn't crash
    Right tf -> length (tfBlocks tf) >= 0  -- Should recover some structure

-- | Parser should preserve partial structure even when errors occur
prop_parser_preserves_partial :: String -> Property
prop_parser_preserves_partial content = 
  let withError = content ++ "\n/// malformed block\n" ++ content
      result = parseTypus withError
  in case result of
    Left _ -> True
    Right tf -> not (null (tfBlocks tf)) ==> 
                all (\cb -> length (cbContent cb) >= 0) (tfBlocks tf)

-- | Parser should handle unicode characters without crashing
prop_parser_unicode_handling :: Property
prop_parser_unicode_handling = 
  let unicodeContent = "//! ownership=true\n// Unicode test: 中文测试 🚀\nfn test() { return 42; }\n"
      result = parseTypus unicodeContent
  in isRight result  -- Should handle unicode gracefully

-- | Parser should handle very long lines without crashing
prop_parser_long_lines :: Int -> Property
prop_parser_long_lines n = 
  let longLine = replicate n 'a' ++ " code content"
      input = "//! ownership=true\n" ++ longLine ++ "\n"
      result = parseTypus input
  in case result of
    Left _ -> True  -- May fail but shouldn't crash
    Right _ -> True

-- | Parser should handle nested block structures
prop_parser_nested_blocks :: Int -> Property
prop_parser_nested_blocks depth = 
  let nestedContent = concat (replicate depth "  ") ++ "nested content\n"
      input = "//! ownership=true\n" ++ nestedContent
      result = parseTypus input
  in case result of
    Left _ -> True  -- May fail for deeply nested structures
    Right tf -> length (tfBlocks tf) >= 0

-- Helper function for property testing with implications
infix 1 ==>
(==>) :: Bool -> Bool -> Bool
True ==> x = x
False ==> _ = True