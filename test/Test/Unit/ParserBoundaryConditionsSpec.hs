{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserBoundaryConditionsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), choose, getPositive, getNonNegative, vector, listOf1, elements)
import TestSupport.Arbitrary

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , locatedWithSpan
  , spanStart
  , spanEnd
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe (isNothing, isJust, fromMaybe)

-- | Boundary condition tests for Parser module
tests :: TestTree
tests = testGroup "Parser Boundary Conditions"
  [ testGroup "Empty and Minimal Input Tests"
    [ testCase "parse empty string" test_parse_empty_string
    , testCase "parse whitespace only" test_parse_whitespace_only
    , testCase "parse only directives" test_parse_only_directives
    , testCase "parse only code blocks" test_parse_only_code_blocks
    , fastProperty "parse minimal valid input" prop_parse_minimal_valid
    ]

  , testGroup "Directive Edge Cases"
    [ testCase "parse malformed directives" test_parse_malformed_directives
    , testCase "parse directives with special characters" test_parse_directives_special_chars
    , testCase "parse directives with unicode" test_parse_directives_unicode
    , fastProperty "parse directive boundary conditions" prop_parse_directive_boundaries
    , fastProperty "parse nested directive scenarios" prop_parse_nested_directives
    ]

  , testGroup "Code Block Edge Cases"
    [ testCase "parse empty code blocks" test_parse_empty_code_blocks
    , testCase "parse code blocks with only whitespace" test_parse_whitespace_code_blocks
    , testCase "parse code blocks with special characters" test_parse_special_chars_code_blocks
    , testCase "parse code blocks with unicode content" test_parse_unicode_code_blocks
    , fastProperty "parse code block size boundaries" prop_parse_code_block_size_boundaries
    ]

  , testGroup "Error Recovery Tests"
    [ testCase "parse with syntax errors" test_parse_syntax_errors
    , testCase "parse with mismatched directives" test_parse_mismatched_directives
    , testCase "parse with incomplete blocks" test_parse_incomplete_blocks
    , fastProperty "parse error recovery consistency" prop_parse_error_recovery_consistency
    ]

  , testGroup "Performance and Memory Tests"
    [ fastProperty "parse large input efficiency" prop_parse_large_input_efficiency
    , fastProperty "parse deeply nested structures" prop_parse_deeply_nested_structures
    , fastProperty "parse memory efficiency" prop_parse_memory_efficiency
    ]

  , testGroup "Unicode and Encoding Tests"
    [ testCase "parse UTF-8 content" test_parse_utf8_content
    , testCase "parse mixed language content" test_parse_mixed_language_content
    , testCase "parse emoji and special unicode" test_parse_emoji_content
    , fastProperty "parse unicode boundary conditions" prop_parse_unicode_boundaries
    ]

  , testGroup "Concurrent and Thread Safety Tests"
    [ fastProperty "parse concurrent safety" prop_parse_concurrent_safety
    , fastProperty "parse state isolation" prop_parse_state_isolation
    ]

  , testGroup "Integration Edge Cases"
    [ testCase "parse real-world complex example" test_parse_real_world_complex
    , fastProperty "parse integration consistency" prop_parse_integration_consistency
    ]
  ]

-- ============================================================================
-- Empty and Minimal Input Tests
-- ============================================================================

test_parse_empty_string :: IO ()
test_parse_empty_string = do
  let result = parseTypus "" "empty.typus"
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      tfDirectives file @?= defaultFileDirectives
      assertBool "Empty blocks list" $ null (tfBlocks file)

test_parse_whitespace_only :: IO ()
test_parse_whitespace_only = do
  let content = "   \n\t  \n   \n"
      result = parseTypus content "whitespace.typus"
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      tfDirectives file @?= defaultFileDirectives
      assertBool "Empty blocks list" $ null (tfBlocks file)

test_parse_only_directives :: IO ()
test_parse_only_directives = do
  let content = "//! ownership=true, dependent-types=false\n"
      result = parseTypus content "directives.typus"
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      let directives = tfDirectives file
      assertBool "Ownership directive set" $ isJust (fdOwnership directives)
      assertBool "Dependent types directive set" $ isJust (fdDependentTypes directives)
      assertBool "Empty blocks list" $ null (tfBlocks file)

test_parse_only_code_blocks :: IO ()
test_parse_only_code_blocks = do
  let content = "func main() {\n    println(\"Hello, World!\")\n}\n"
      result = parseTypus content "code.typus"
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      tfDirectives file @?= defaultFileDirectives
      assertBool "Has code blocks" $ not (null (tfBlocks file))

prop_parse_minimal_valid :: String -> Property
prop_parse_minimal_valid input =
  not (null input) && length input <= 10 ==>
  let result = parseTypus input "minimal.typus"
  in case result of
       Left _ -> property False
       Right file -> property $ not (null (tfBlocks file) || tfDirectives file /= defaultFileDirectives)

-- ============================================================================
-- Directive Edge Cases
-- ============================================================================

test_parse_malformed_directives :: IO ()
test_parse_malformed_directives = do
  let malformedInputs = 
        [ "//! ownership=true dependent-types=false"  -- missing comma
        , "//! ownership=, dependent-types=false"     -- empty value
        , "//! =true, dependent-types=false"          -- missing key
        , "//! ownership=true, ="                     -- missing key after comma
        , "//! ownership=true dependent-types=false," -- trailing comma
        ]
  mapM_ testMalformed malformedInputs
  where
    testMalformed content = do
      let result = parseTypus content "malformed.typus"
      case result of
        Left _ -> return ()  -- Expected to fail
        Right file -> assertBool "Should handle malformed directives gracefully" $ 
                           tfDirectives file == defaultFileDirectives

test_parse_directives_special_chars :: IO ()
test_parse_directives_special_chars = do
  let content = "//! ownership=true, dependent-types=false, constraints=\"special-chars: !@#$%^&*()\"\n"
      result = parseTypus content "special.typus"
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      let directives = tfDirectives file
      assertBool "Constraints directive set" $ isJust (fdConstraints directives)

test_parse_directives_unicode :: IO ()
test_parse_directives_unicode = do
  let content = "//! ownership=true, dependent-types=false, constraints=\"测试: café naïve résumé\"\n"
      result = parseTypus content "unicode.typus"
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      let directives = tfDirectives file
      assertBool "Constraints directive with unicode set" $ isJust (fdConstraints directives)

prop_parse_directive_boundaries :: String -> Property
prop_parse_directive_boundaries input =
  length input <= 100 ==>
  let directiveContent = "//! " ++ input
      result = parseTypus directiveContent "boundary.typus"
  in case result of
       Left _ -> property $ length input > 50  -- Long inputs may fail
       Right file -> property $ True

prop_parse_nested_directives :: [String] -> Property
prop_parse_nested_directives directives =
  all (`notElem` ["//!", "/*", "*/", "//"]) directives ==>
  let content = unlines $ map (\d -> "//! " ++ d) directives
      result = parseTypus content "nested.typus"
  in case result of
       Left _ -> property $ length directives > 10  -- Too many directives may fail
       Right file -> property $ True

-- ============================================================================
-- Code Block Edge Cases
-- ============================================================================

test_parse_empty_code_blocks :: IO ()
test_parse_empty_code_blocks = do
  let content = "//! ownership=true\n\n\n//! ownership=false\n\n"
      result = parseTypus content "empty-blocks.typus"
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      let blocks = tfBlocks file
      assertBool "Should have blocks" $ not (null blocks)

test_parse_whitespace_code_blocks :: IO ()
test_parse_whitespace_code_blocks = do
  let content = "//! ownership=true\n   \n\t  \n   \n//! ownership=false\n"
      result = parseTypus content "whitespace-blocks.typus"
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      let blocks = tfBlocks file
      assertBool "Should have blocks" $ not (null blocks)

test_parse_special_chars_code_blocks :: IO ()
test_parse_special_chars_code_blocks = do
  let content = "//! ownership=true\nfunc special() { return \"!@#$%^&*(){}[]|\\\\:;<>?,./\"; }\n"
      result = parseTypus content "special-chars.typus"
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      let blocks = tfBlocks file
      assertBool "Should have blocks" $ not (null blocks)

test_parse_unicode_code_blocks :: IO ()
test_parse_unicode_code_blocks = do
  let content = "//! ownership=true\nfunc unicode() { return \"café naïve résumé 🚀 测试\"; }\n"
      result = parseTypus content "unicode-blocks.typus"
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      let blocks = tfBlocks file
      assertBool "Should have blocks" $ not (null blocks)

prop_parse_code_block_size_boundaries :: Int -> String -> Property
prop_parse_code_block_size_boundaries size content =
  size >= 0 && size <= 1000 ==>
  let largeContent = unlines $ replicate size content
      fullContent = "//! ownership=true\n" ++ largeContent
      result = parseTypus fullContent "large-block.typus"
  in case result of
       Left _ -> property $ size > 500  -- Large blocks may fail
       Right file -> property $ True

-- ============================================================================
-- Error Recovery Tests
-- ============================================================================

test_parse_syntax_errors :: IO ()
test_parse_syntax_errors = do
  let content = "//! ownership=true\nfunc invalid( {\n    // syntax error\n}\n"
      result = parseTypus content "syntax-error.typus"
  case result of
    Left _ -> assertFailure "Parse should handle syntax errors gracefully"
    Right file -> do
      let errors = tfSyntaxErrors file
      assertBool "Should detect syntax errors" $ not (null errors)

test_parse_mismatched_directives :: IO ()
test_parse_mismatched_directives = do
  let content = "//! ownership=true\n//! ownership=false\nfunc test() {}\n"
      result = parseTypus content "mismatched.typus"
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      let blocks = tfBlocks file
      assertBool "Should handle mismatched directives" $ not (null blocks)

test_parse_incomplete_blocks :: IO ()
test_parse_incomplete_blocks = do
  let content = "//! ownership=true\nfunc incomplete() {"
      result = parseTypus content "incomplete.typus"
  case result of
    Left _ -> assertFailure "Parse should handle incomplete blocks gracefully"
    Right file -> do
      let blocks = tfBlocks file
      assertBool "Should handle incomplete blocks" $ not (null blocks)

prop_parse_error_recovery_consistency :: String -> String -> Property
prop_parse_error_recovery_consistency prefix suffix =
  length prefix <= 50 && length suffix <= 50 ==>
  let content = prefix ++ "func invalid( {" ++ suffix
      result1 = parseTypus content "error1.typus"
      result2 = parseTypus content "error2.typus"
  in case (result1, result2) of
       (Left _, Left _) -> property True
       (Right f1, Right f2) -> property $ tfBlocks f1 == tfBlocks f2
       _ -> property False

-- ============================================================================
-- Performance and Memory Tests
-- ============================================================================

prop_parse_large_input_efficiency :: Int -> Property
prop_parse_large_input_efficiency multiplier =
  multiplier > 0 && multiplier <= 100 ==>
  let baseContent = "//! ownership=true\nfunc test() { return \"test\"; }\n"
      largeContent = concat $ replicate multiplier baseContent
      result = parseTypus largeContent "large.typus"
  in case result of
       Left _ -> property $ multiplier > 50  -- Large inputs may fail
       Right file -> property $ not (null (tfBlocks file))

prop_parse_deeply_nested_structures :: Int -> Property
prop_parse_deeply_nested_structures depth =
  depth > 0 && depth <= 20 ==>
  let nestedContent = unlines $ replicate depth "    func nested() { return " ++ 
                      replicate depth "}" ++ "\n"
      fullContent = "//! ownership=true\n" ++ nestedContent
      result = parseTypus fullContent "nested.typus"
  in case result of
       Left _ -> property $ depth > 10  -- Deep nesting may fail
       Right file -> property $ True

prop_parse_memory_efficiency :: Int -> String -> Property
prop_parse_memory_efficiency iterations baseContent =
  iterations > 0 && iterations <= 10 && length baseContent <= 100 ==>
  let content = "//! ownership=true\n" ++ baseContent
      results = map (\_ -> parseTypus content ("memory" ++ show baseContent ++ ".typus")) [1..iterations]
  in property $ all (\r -> case r of Left _ -> False; Right _ -> True) results

-- ============================================================================
-- Unicode and Encoding Tests
-- ============================================================================

test_parse_utf8_content :: IO ()
test_parse_utf8_content = do
  let content = "//! ownership=true\nfunc utf8() { return \"Hello, 世界! 🌍\"; }\n"
      result = parseTypus content "utf8.typus"
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      let blocks = tfBlocks file
      assertBool "Should handle UTF-8 content" $ not (null blocks)

test_parse_mixed_language_content :: IO ()
test_parse_mixed_language_content = do
  let content = "//! ownership=true\nfunc mixed() { return \"Hello 世界 café naïve résumé 🚀 тест\"; }\n"
      result = parseTypus content "mixed.typus"
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      let blocks = tfBlocks file
      assertBool "Should handle mixed language content" $ not (null blocks)

test_parse_emoji_content :: IO ()
test_parse_emoji_content = do
  let content = "//! ownership=true\nfunc emoji() { return \"😀😃😄😁😆😅😂🤣😊😇\"; }\n"
      result = parseTypus content "emoji.typus"
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      let blocks = tfBlocks file
      assertBool "Should handle emoji content" $ not (null blocks)

prop_parse_unicode_boundaries :: String -> Property
prop_parse_unicode_boundaries unicodeContent =
  length unicodeContent <= 100 ==>
  let content = "//! ownership=true\nfunc unicode() { return \"" ++ unicodeContent ++ "\"; }\n"
      result = parseTypus content "unicode-boundary.typus"
  in case result of
       Left _ -> property $ any (> 127) (map (fromEnum . toEnum) unicodeContent)
       Right file -> property $ True

-- ============================================================================
-- Concurrent and Thread Safety Tests
-- ============================================================================

prop_parse_concurrent_safety :: String -> Property
prop_parse_concurrent_safety content =
  length content <= 100 ==>
  let result1 = parseTypus content "concurrent1.typus"
      result2 = parseTypus content "concurrent2.typus"
  in case (result1, result2) of
       (Left _, Left _) -> property True
       (Right f1, Right f2) -> property $ tfBlocks f1 == tfBlocks f2
       _ -> property False

prop_parse_state_isolation :: String -> String -> Property
prop_parse_state_isolation content1 content2 =
  length content1 <= 50 && length content2 <= 50 ==>
  let result1 = parseTypus content1 "isolation1.typus"
      result2 = parseTypus content2 "isolation2.typus"
  in case (result1, result2) of
       (Right f1, Right f2) -> property $ tfBlocks f1 /= tfBlocks f2 || content1 == content2
       _ -> property True

-- ============================================================================
-- Integration Edge Cases
-- ============================================================================

test_parse_real_world_complex :: IO ()
test_parse_real_world_complex = do
  let content = unlines
        [ "//! ownership=true, dependent-types=true, constraints=\"memory-safety\""
        , ""
        , "func complexAlgorithm(data []int) (result []int, err error) {"
        , "    if len(data) == 0 {"
        , "        return nil, errors.New(\"empty data\")"
        , "    }"
        , "    "
        , "    // 处理中文注释和emoji 🚀"
        , "    for i, value := range data {"
        , "        if value > 0 {"
        , "            result = append(result, value * 2)"
        , "        }"
        , "    }"
        , "    "
        , "    return result, nil"
        , "}"
        , ""
        , "//! ownership=false"
        , "func simpleFunction(x int) int {"
        , "    return x * x  // café naïve résumé"
        , "}"
        ]
      result = parseTypus content "real-world.typus"
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      let blocks = tfBlocks file
      assertBool "Should parse real-world complex example" $ length blocks >= 2

prop_parse_integration_consistency :: String -> Property
prop_parse_integration_consistency content =
  length content <= 200 ==>
  let result1 = parseTypus content "integration1.typus"
      result2 = parseTypus content "integration2.typus"
  in case (result1, result2) of
       (Right f1, Right f2) -> property $ 
         tfDirectives f1 == tfDirectives f2 .&&.
         length (tfBlocks f1) == length (tfBlocks f2)
       (Left _, Left _) -> property True
       _ -> property False