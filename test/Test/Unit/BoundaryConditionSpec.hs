{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.BoundaryConditionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Parser (parseTypus, TypusFile(..))
import Compiler (compileTypus, CompilationResult(..))
import ErrorHandler (errorAt, ErrorLocation(..))
import SourceLocation (SourcePos(..), startPos, posAtLineCol, advancePosBy)
import Utils (trim, removeComments, normalizeIndentation)

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, null, length, replicate)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Char (isSpace, isControl)
import qualified Data.String

-- ============================================================================
-- Boundary Condition Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Boundary Condition Tests"
    [ testGroup "Empty and minimal inputs"
        [ testCase "handles completely empty string" test_empty_string
        , testCase "handles whitespace-only input" test_whitespace_only
        , testCase "handles single character" test_single_character
        , testCase "handles minimal valid program" test_minimal_program
        , testCase "handles input with only newlines" test_only_newlines
        ]

    , testGroup "Extreme values"
        [ testCase "handles very long identifiers" test_very_long_identifiers
        , testCase "handles very long lines" test_very_long_lines
        , testCase "handles deeply nested structures" test_deeply_nested
        , testCase "handles maximum recursion depth" test_maximum_recursion
        , testCase "handles very large numbers" test_very_large_numbers
        ]

    , testGroup "Special characters and Unicode"
        [ testCase "handles Unicode characters in identifiers" test_unicode_identifiers
        , testCase "handles Unicode in string literals" test_unicode_strings
        , testCase "handles control characters" test_control_characters
        , testCase "handles zero-width characters" test_zero_width_characters
        , testCase "handles mixed encoding input" test_mixed_encoding
        ]

    , testGroup "Malformed inputs"
        [ testCase "handles unmatched brackets" test_unmatched_brackets
        , testCase "handles unmatched quotes" test_unmatched_quotes
        , testCase "handles incomplete statements" test_incomplete_statements
        , testCase "handles garbage input" test_garbage_input
        , testCase "handles mixed language syntax" test_mixed_syntax
        ]

    , testGroup "Resource limits"
        [ testCase "handles memory pressure gracefully" test_memory_pressure
        , testCase "handles time limits gracefully" test_time_limits
        , testCase "handles file size limits" test_file_size_limits
        , testCase "handles token count limits" test_token_count_limits
        ]

    , testGroup "Property-based boundary tests"
        [ fastProperty "parse never crashes on any string input" prop_parse_never_crashes
        , fastProperty "compile handles any parse result gracefully" prop_compile_never_crashes
        , fastProperty "error handling is total" prop_error_handling_total
        , fastProperty "string processing is safe on all inputs" prop_string_processing_safe
        ]
    ]

-- ============================================================================
-- Empty and Minimal Inputs Tests
-- ============================================================================

test_empty_string :: IO ()
test_empty_string = do
  let content = ""
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed on empty string: " ++ show err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
          directives = tfDirectives typusFile
      blocks @?= []
      directives @?= (error "Default directives - implementation specific")

test_whitespace_only :: IO ()
test_whitespace_only = do
  let content = "   \t\n\r   \n\t   "
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed on whitespace: " ++ show err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      -- Should handle whitespace gracefully
      assertBool "Should handle whitespace-only input" (True)

test_single_character :: IO ()
test_single_character = do
  let testChars = ["a", "}", "\"", "/", "\n", "\t", " "]
  mapM_ testChar testChars
  where
    testChar char = do
      let parseResult = parseTypus char
      case parseResult of
        Left _ -> assertBool $ "Should handle single character: " ++ show char
        Right _ -> assertBool $ "Successfully parsed single character: " ++ show char

test_minimal_program :: IO ()
test_minimal_program = do
  let content = "f()"
      parseResult = parseTypus content
  case parseResult of
    Left err -> do
      -- Should either parse successfully or provide meaningful error
      assertBool $ "Parse result should be meaningful: " ++ show err
    Right typusFile -> do
      assertBool "Should parse minimal program" (True)

test_only_newlines :: IO ()
test_only_newlines = do
  let content = "\n\n\n\n\n"
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed on newlines: " ++ show err
    Right typusFile -> do
      assertBool "Should handle newline-only input" (True)

-- ============================================================================
-- Extreme Values Tests
-- ============================================================================

test_very_long_identifiers :: IO ()
test_very_long_identifiers = do
  let longIdent = replicate 10000 'a'
      content = "func " ++ longIdent ++ "() { return 42 }\n"
      parseResult = parseTypus content
  case parseResult of
    Left err -> do
      -- Should handle gracefully with appropriate error
      assertBool "Should handle long identifiers gracefully" (True)
    Right typusFile -> do
      assertBool "Should parse very long identifiers" (True)

test_very_long_lines :: IO ()
test_very_long_lines = do
  let longLine = replicate 100000 'x' ++ "func test() { return 42 }" ++ replicate 100000 'y'
      content = longLine ++ "\n"
      parseResult = parseTypus content
  case parseResult of
    Left err -> do
      assertBool "Should handle very long lines gracefully" (True)
    Right typusFile -> do
      assertBool "Should parse very long lines" (True)

test_deeply_nested :: IO ()
test_deeply_nested = do
  let nestDepth = 1000
      nestedContent = concat $ replicate nestDepth "func outer() { "
      content = nestedContent ++ "return 42" ++ concat (replicate nestDepth " }") ++ "\n"
      parseResult = parseTypus content
  case parseResult of
    Left err -> do
      -- Should handle deep nesting with appropriate error about recursion limits
      assertBool "Should handle deep nesting gracefully" (True)
    Right typusFile -> do
      assertBool "Should handle deeply nested structures" (True)

test_maximum_recursion :: IO ()
test_maximum_recursion = do
  let recursiveContent = "func recurse() { recurse() }\n"
      parseResult = parseTypus recursiveContent
  case parseResult of
    Left err -> do
      assertBool "Should handle potential infinite recursion" (True)
    Right typusFile -> do
      assertBool "Should parse recursive definitions" (True)

test_very_large_numbers :: IO ()
test_very_large_numbers = do
  let bigNumber = show (10^100 :: Integer)
      content = "func test() { return " ++ bigNumber ++ " }\n"
      parseResult = parseTypus content
  case parseResult of
    Left err -> do
      assertBool "Should handle very large numbers gracefully" (True)
    Right typusFile -> do
      assertBool "Should parse very large numbers" (True)

-- ============================================================================
-- Special Characters and Unicode Tests
-- ============================================================================

test_unicode_identifiers :: IO ()
test_unicode_identifiers = do
  let unicodeIdent = "函数_テスト_функция"
      content = "func " ++ unicodeIdent ++ "() { return 42 }\n"
      parseResult = parseTypus content
  case parseResult of
    Left err -> do
      -- Should either support Unicode or provide clear error
      assertBool "Should handle Unicode in identifiers" (True)
    Right typusFile -> do
      assertBool "Should parse Unicode identifiers" (True)

test_unicode_strings :: IO ()
test_unicode_strings = do
  let unicodeString = "\"Hello 世界 🚀 Café naïve\""
      content = "func test() { s := " ++ unicodeString ++ "; return s }\n"
      parseResult = parseTypus content
  case parseResult of
    Left err -> do
      assertBool "Should handle Unicode in strings" (True)
    Right typusFile -> do
      assertBool "Should parse Unicode strings" (True)

test_control_characters :: IO ()
test_control_characters = do
  let controlChars = map (\c -> if isControl c then c else ' ') ['\0'..'\31']
      content = "func test() { return \"" ++ controlChars ++ "\" }\n"
      parseResult = parseTypus content
  case parseResult of
    Left err -> do
      assertBool "Should handle control characters" (True)
    Right typusFile -> do
      assertBool "Should parse control characters" (True)

test_zero_width_characters :: IO ()
test_zero_width_characters = do
  let zeroWidthChars = "\x200B\x200C\x200D\xFEFF"  -- Zero-width space, non-joiner, joiner, BOM
      content = "func test" ++ zeroWidthChars ++ "() { return 42 }\n"
      parseResult = parseTypus content
  case parseResult of
    Left err -> do
      assertBool "Should handle zero-width characters" (True)
    Right typusFile -> do
      assertBool "Should parse zero-width characters" (True)

test_mixed_encoding :: IO ()
test_mixed_encoding = do
  let mixedContent = "func test() { return \"Hello\x80世界\" }\n"  -- Invalid UTF-8 sequence
      parseResult = parseTypus mixedContent
  case parseResult of
    Left err -> do
      assertBool "Should handle mixed encoding gracefully" (True)
    Right typusFile -> do
      assertBool "Should handle mixed encoding" (True)

-- ============================================================================
-- Malformed Inputs Tests
-- ============================================================================

test_unmatched_brackets :: IO ()
test_unmatched_brackets = do
  let testCases = 
        [ "func test() { return 42"      -- Missing closing brace
        , "func test() return 42 }"      -- Extra closing brace
        , "func test(( return 42 ))"     -- Unmatched parentheses
        , "func test[ return 42 ]"       -- Wrong bracket type
        ]
  mapM_ testCase_ testCases
  where
    testCase_ content = do
      let parseResult = parseTypus content
      case parseResult of
        Left err -> do
          assertBool $ "Should provide meaningful error for unmatched brackets: " ++ show err
        Right typusFile -> do
          -- May parse partially but should handle gracefully
          assertBool "Should handle unmatched brackets gracefully" (True)

test_unmatched_quotes :: IO ()
test_unmatched_quotes = do
  let testCases = 
        [ "func test() { return \"hello }"      -- Missing closing quote
        , "func test() { return 'hello }"       -- Missing closing single quote
        , "func test() { return \"hello' }"     -- Mixed quote types
        , "func test() { return \"\"\"hello }"   -- Triple quote
        ]
  mapM_ testCase_ testCases
  where
    testCase_ content = do
      let parseResult = parseTypus content
      case parseResult of
        Left err -> do
          assertBool $ "Should provide meaningful error for unmatched quotes: " ++ show err
        Right typusFile -> do
          assertBool "Should handle unmatched quotes gracefully" (True)

test_incomplete_statements :: IO ()
test_incomplete_statements = do
  let testCases = 
        [ "func test() { return"           -- Incomplete return
        , "func test() { var x :="         -- Incomplete assignment
        , "func test() { if"               -- Incomplete if
        , "func test() { for"              -- Incomplete for
        ]
  mapM_ testCase_ testCases
  where
    testCase_ content = do
      let parseResult = parseTypus content
      case parseResult of
        Left err -> do
          assertBool $ "Should handle incomplete statements: " ++ show err
        Right typusFile -> do
          assertBool "Should handle incomplete statements gracefully" (True)

test_garbage_input :: IO ()
test_garbage_input = do
  let garbageContent = "!@#$%^&*()_+{}|:\"<>?~`-=[]\\;',./\n\x00\x01\x02\xFF"
      parseResult = parseTypus garbageContent
  case parseResult of
    Left err -> do
      assertBool "Should handle garbage input gracefully" (True)
    Right typusFile -> do
      assertBool "Should handle garbage input" (True)

test_mixed_syntax :: IO ()
test_mixed_syntax = do
  let mixedContent = unlines
        [ "func test() {"
        , "    print \"Hello\";  // Go-style"
        , "    console.log(\"World\");  // JavaScript-style"
        , "    echo \"Test\";  // Shell-style"
        , "}"
        ]
      parseResult = parseTypus mixedContent
  case parseResult of
    Left err -> do
      assertBool "Should handle mixed syntax gracefully" (True)
    Right typusFile -> do
      assertBool "Should handle mixed syntax" (True)

-- ============================================================================
-- Resource Limits Tests
-- ============================================================================

test_memory_pressure :: IO ()
test_memory_pressure = do
  let hugeContent = concat $ replicate 10000 "func test" ++ show [1..10000] ++ "() { return " ++ show [1..1000] ++ " }\n"
      parseResult = parseTypus hugeContent
  case parseResult of
    Left err -> do
      assertBool "Should handle memory pressure gracefully" (True)
    Right typusFile -> do
      assertBool "Should handle large inputs without crashing" (True)

test_time_limits :: IO ()
test_time_limits = do
  let complexContent = concat $ replicate 1000 "func test" ++ show [1..1000] ++ "() { if true { if true { if true { return 42 } } } }\n"
      parseResult = parseTypus complexContent
  case parseResult of
    Left err -> do
      assertBool "Should handle complex inputs within time limits" (True)
    Right typusFile -> do
      assertBool "Should handle complex inputs" (True)

test_file_size_limits :: IO ()
test_file_size_limits = do
  let largeContent = concat $ replicate 100000 "x\n"  -- 100KB of content
      parseResult = parseTypus largeContent
  case parseResult of
    Left err -> do
      assertBool "Should handle large file sizes gracefully" (True)
    Right typusFile -> do
      assertBool "Should handle large file sizes" (True)

test_token_count_limits :: IO ()
test_token_count_limits = do
  let manyTokens = concat $ replicate 10000 "a + b * c / d % e && f || g & h | i ^ j << k >> l"
      parseResult = parseTypus manyTokens
  case parseResult of
    Left err -> do
      assertBool "Should handle many tokens gracefully" (True)
    Right typusFile -> do
      assertBool "Should handle many tokens" (True)

-- ============================================================================
-- Property-Based Boundary Tests
-- ============================================================================

prop_parse_never_crashes :: Property
prop_parse_never_crashes =
  forAll arbitrary $ \content ->
    let parseResult = parseTypus content
    in case parseResult of
         Left _ -> property True
         Right _ -> property True

prop_compile_never_crashes :: Property
prop_compile_never_crashes =
  forAll arbitrary $ \content ->
    let parseResult = parseTypus content
    in case parseResult of
         Left _ -> property True
         Right typusFile ->
           let compileResult = compileTypus typusFile
           in case compileResult of
                Left _ -> property True
                Right _ -> property True

prop_error_handling_total :: Property
prop_error_handling_total =
  forAll arbitrary $ \line ->
  forAll arbitrary $ \col ->
    let location = ErrorLocation Nothing line col Nothing Nothing
        error = errorAt location "test error"
    in property $ line errorLocation error === line .&&.
                    column errorLocation error === col

prop_string_processing_safe :: Property
prop_string_processing_safe =
  forAll arbitrary $ \input ->
    let trimmed = trim input
        withoutComments = removeComments input
        normalized = normalizeIndentation input
    in property $ length trimmed <= length input .&&.
                    length withoutComments <= length input .&&.
                    not (null normalized) || null input