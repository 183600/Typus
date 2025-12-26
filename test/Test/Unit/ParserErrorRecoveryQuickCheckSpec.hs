{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.ParserErrorRecoveryQuickCheckSpec (tests) where

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (==>))
import Test.Tasty.HUnit (testCase, assert, (@?=))
import qualified Data.Text as T
import Data.Char (isSpace, isAlpha, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import SourceLocation (SourcePos(..), SourceSpan(..))

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate malformed Typus code snippets
genMalformedCode :: Gen String
genMalformedCode = do
  malformedType <- elements
    [ "unclosed_bracket", "unclosed_paren", "invalid_syntax"
    , "missing_semicolon", "invalid_identifier", "unbalanced_braces"
    ]
  case malformedType of
    "unclosed_bracket" -> do
      content <- listOf $ arbitrary `suchThat` (/= ']')
      return $ "[" ++ content ++ "\nmore code"
    "unclosed_paren" -> do
      content <- listOf $ arbitrary `suchThat` (/= ')')
      return $ "(" ++ content ++ "\nmore code"
    "invalid_syntax" -> do
      return $ "invalid @#$ syntax\nmore code"
    "missing_semicolon" -> do
      return $ "let x = 42\nlet y = 13"
    "invalid_identifier" -> do
      return $ "let 123invalid = 42"
    "unbalanced_braces" -> do
      return $ "{{{\nlet x = 42\n}"
    _ -> return "default malformed"

-- Generate code with syntax errors but recoverable
genRecoverableCode :: Gen String
genRecoverableCode = do
  validLines <- listOf $ elements
    [ "let x = 42"
    , "func test() { return 1; }"
    , "type MyType = int"
    , "import std"
    ]
  errorLine <- elements
    [ "let invalid @#$ = 42"
    , "unclosed [ bracket"
    , "missing semicolon"
    ]
  moreValidLines <- listOf $ elements validLines
  let allLines = take 3 validLines ++ [errorLine] ++ take 2 moreValidLines
  return $ unlines allLines

-- Generate partially valid Typus files
genPartialTypusFile :: Gen String
genPartialTypusFile = do
  hasHeader <- arbitrary
  hasDirectives <- arbitrary
  hasCode <- arbitrary
  
  let header = if hasHeader then "// This is a header\n" else ""
  let directives = if hasDirectives 
                   then "// @ownership: true\n// @dependent-types: true\n"
                   else ""
  code <- if hasCode 
          then do
            lines <- listOf $ elements
              [ "let x = 42"
              , "func test() { return x; }"
              , "type MyType = int"
              ]
            return $ unlines lines
          else return ""
  
  return $ header ++ directives ++ code

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Property: parseTypus should return a result even for malformed input
prop_parse_returns_result :: String -> Property
prop_parse_returns_result input =
  let result = parseTypus input
      hasResult = case result of
        Left _ -> False
        Right _ -> True
  in hasResult === True

-- Property: parseTypus should handle empty input gracefully
prop_parse_handles_empty :: Property
prop_parse_handles_empty =
  let result = parseTypus ""
      hasResult = case result of
        Left _ -> False
        Right _ -> True
  in hasResult === True

-- Property: parseTypus should preserve some structure from partial input
prop_parse_preserves_partial_structure :: String -> Property
prop_parse_preserves_partial_structure input =
  let result = parseTypus input
      preservesSomeStructure = case result of
        Left _ -> False
        Right (TypusFile _ _ blocks) -> not (null blocks)
  in length (lines input) > 0 ==> preservesSomeStructure ||. length input < 10
  where
    (||.) = (||)

-- Property: parseTypus should handle input with only comments
prop_parse_handles_comments_only :: Property
prop_parse_handles_comments_only =
  let commentOnly = "// This is a comment\n// Another comment\n"
      result = parseTypus commentOnly
      hasResult = case result of
        Left _ -> False
        Right _ -> True
  in hasResult === True

-- Property: parseTypus should handle input with only whitespace
prop_parse_handles_whitespace_only :: Property
prop_parse_handles_whitespace_only =
  let whitespaceOnly = "   \n  \t  \n   "
      result = parseTypus whitespaceOnly
      hasResult = case result of
        Left _ -> False
        Right _ -> True
  in hasResult === True

-- Property: parseTypus should be tolerant of line ending variations
prop_parse_tolerates_line_endings :: String -> Property
prop_parse_tolerates_line_endings content =
  let unixContent = content
      windowsContent = unlines $ lines content  -- Normalize to \n
      macContent = unlines $ map (++ "\r") $ lines content
      
      unixResult = parseTypus unixContent
      windowsResult = parseTypus windowsContent
      macResult = parseTypus macContent
      
      allHaveResults = case (unixResult, windowsResult, macResult) of
        (Left _, Left _, Left _) -> False
        _ -> True
  in allHaveResults === True

-- Property: parseTypus should handle very long lines
prop_parse_handles_long_lines :: Property
prop_parse_handles_long_lines =
  let longLine = replicate 1000 'a' ++ " let x = 42"
      result = parseTypus longLine
      hasResult = case result of
        Left _ -> False
        Right _ -> True
  in hasResult === True

-- Property: parseTypus should handle unicode characters
prop_parse_handles_unicode :: Property
prop_parse_handles_unicode =
  let unicodeContent = "let 测试 = 42\nlet 🦀 = \"crab\"\n"
      result = parseTypus unicodeContent
      hasResult = case result of
        Left _ -> False
        Right _ -> True
  in hasResult === True

-- ============================================================================
-- Unit Tests
-- ============================================================================

test_parse_empty_input :: TestTree
test_parse_empty_input = testCase "parse empty input" $ do
  let result = parseTypus ""
  case result of
    Left _ -> assert False
    Right (TypusFile directives blocks) -> do
      directives @?= defaultFileDirectives
      blocks @?= []

test_parse_comments_only :: TestTree
test_parse_comments_only = testCase "parse comments only" $ do
  let input = "// This is a comment\n// Another comment\n"
  let result = parseTypus input
  case result of
    Left _ -> assert False
    Right (TypusFile directives blocks) -> do
      length blocks @?= 0  -- Comments should not create blocks

test_parse_simple_valid :: TestTree
test_parse_simple_valid = testCase "parse simple valid code" $ do
  let input = "let x = 42\n"
  let result = parseTypus input
  case result of
    Left _ -> assert False
    Right (TypusFile directives blocks) -> do
      length blocks @?= 1  -- Should create one block

test_parse_with_directives :: TestTree
test_parse_with_directives = testCase "parse with directives" $ do
  let input = "// @ownership: true\n// @dependent-types: false\nlet x = 42\n"
  let result = parseTypus input
  case result of
    Left _ -> assert False
    Right (TypusFile directives blocks) -> do
      -- Check that directives are parsed
      fdOwnership directives @?= Just True  -- This may need adjustment based on actual parser behavior

test_parse_malformed_recovery :: TestTree
test_parse_malformed_recovery = testCase "parse malformed with recovery" $ do
  let input = "let valid = 42\nlet invalid @#$ = 13\nlet another = 7\n"
  let result = parseTypus input
  case result of
    Left _ -> assert False
    Right (TypusFile directives blocks) -> do
      -- Should recover and parse some blocks despite the error
      length blocks @?= 3  -- Or at least > 1

test_parse_unclosed_structures :: TestTree
test_parse_unclosed_structures = testCase "parse unclosed structures" $ do
  let input = "let x = {\n  y = 42\n// missing closing brace\nlet z = 7\n"
  let result = parseTypus input
  case result of
    Left _ -> assert False
    Right (TypusFile directives blocks) -> do
      -- Should handle unclosed braces gracefully
      length blocks @?= 2  -- Should parse both declarations

test_parse_edge_cases :: TestTree
test_parse_edge_cases = testCase "parse edge cases" $ do
  let tests = 
        [ ("", 0)
        , ("   ", 0)
        , ("// comment", 0)
        , ("let x = 42", 1)
        , ("let x = 42\nlet y = 13", 2)
        ]
  
  mapM_ (\(input, expectedBlocks) -> do
    let result = parseTypus input
    case result of
      Left _ -> assert $ expectedBlocks == 0  -- Only allow failure for empty cases
      Right (TypusFile directives blocks) -> 
        length blocks @?= expectedBlocks
    ) tests

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser Error Recovery QuickCheck Tests"
  [ testProperty "parseTypus returns result even for malformed input" prop_parse_returns_result
  , testProperty "parseTypus handles empty input gracefully" prop_parse_handles_empty
  , testProperty "parseTypus preserves partial structure" prop_parse_preserves_partial_structure
  , testProperty "parseTypus handles comments only" prop_parse_handles_comments_only
  , testProperty "parseTypus handles whitespace only" prop_parse_handles_whitespace_only
  , testProperty "parseTypus tolerates line ending variations" prop_parse_tolerates_line_endings
  , testProperty "parseTypus handles very long lines" prop_parse_handles_long_lines
  , testProperty "parseTypus handles unicode characters" prop_parse_handles_unicode
  , test_parse_empty_input
  , test_parse_comments_only
  , test_parse_simple_valid
  , test_parse_with_directives
  , test_parse_malformed_recovery
  , test_parse_unclosed_structures
  , test_parse_edge_cases
  ]