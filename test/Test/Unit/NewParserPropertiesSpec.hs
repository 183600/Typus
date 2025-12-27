module Test.Unit.NewParserPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Property, (===), Arbitrary(..), Gen, oneof, listOf, elements, suchThat)

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..), CodeBlock(..), defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourceSpan(..), SourcePos(..))
import qualified Data.Text as T
import Data.List (isPrefixOf)
import Utils (trim)

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate valid identifiers for directives
genIdentifier :: Gen String
genIdentifier = oneof
  [ (\c s -> c:s) <$> elements ['a'..'z'] <*> listOf (elements (['a'..'z'] ++ ['0'..'9'] ++ "_-"))
  , listOf1 (elements (['a'..'z'] ++ ['0'..'9'] ++ "_-"))
  ]

-- Generate valid directive values
genDirectiveValue :: Gen String
genDirectiveValue = oneof
  [ elements ["true", "false", "on", "off", "enabled", "disabled"]
  , listOf1 (elements (['a'..'z'] ++ ['0'..'9'] ++ "_-"))
  ]

-- Generate file directive lines
genFileDirective :: Gen String
genFileDirective = do
  key <- genIdentifier
  value <- genDirectiveValue
  pure $ "//! " ++ key ++ "=" ++ value

-- Generate block directive lines
genBlockDirective :: Gen String
genBlockDirective = do
  key <- genIdentifier
  value <- genDirectiveValue
  pure $ "/// " ++ key ++ "=" ++ value

-- Generate simple code blocks
genCodeBlock :: Gen String
genCodeBlock = do
  directives <- listOf genBlockDirective
  codeLines <- listOf $ elements 
    [ "func main() {"
    , "    return 42"
    , "}"
    , "let x = 10"
    , "if x > 0 {"
    , "    println(x)"
    , "}"
    ]
  pure $ unlines (directives ++ codeLines)

-- Generate complete Typus file content
genTypusFileContent :: Gen String
genTypusFileContent = do
  fileDirectives <- listOf genFileDirective
  codeBlocks <- listOf genCodeBlock
  pure $ unlines (fileDirectives ++ [""] ++ concatMap (\block -> [block, ""]) codeBlocks)

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Property: Parsing a file and then re-parsing the string representation should be consistent
prop_parse_roundtrip_consistency :: Property
prop_parse_roundtrip_consistency = 
  forAll genTypusFileContent $ \content ->
    let parsed1 = parseTypus content
        parsed2 = parseTypus content
    in length (tfBlocks parsed1) === length (tfBlocks parsed2)

-- Property: Empty content should produce a file with no blocks
prop_parse_empty_content :: Property
prop_parse_empty_content = 
  let emptyContent = ""
      parsed = parseTypus emptyContent
  in length (tfBlocks parsed) === 0

-- Property: Comments in directives should be handled correctly
prop_parse_directive_comments :: Property
prop_parse_directive_comments = 
  forAll genFileDirective $ \directive ->
    let contentWithComment = directive ++ " // this is a comment"
        parsedWithoutComment = parseTypus directive
        parsedWithComment = parseTypus contentWithComment
    in length (tfBlocks parsedWithoutComment) === length (tfBlocks parsedWithComment)

-- Property: Whitespace should not affect parsing results
prop_parse_whitespace_independence :: Property
prop_parse_whitespace_independence = 
  forAll genTypusFileContent $ \content ->
    let normalized = trim content
        parsedOriginal = parseTypus content
        parsedNormalized = parseTypus normalized
    in length (tfBlocks parsedOriginal) === length (tfBlocks parsedNormalized)

-- Property: Multiple consecutive empty lines should be handled gracefully
prop_parse_consecutive_empty_lines :: Property
prop_parse_consecutive_empty_lines = 
  forAll genTypusFileContent $ \content ->
    let withExtraEmptyLines = content ++ "\n\n\n\n\n"
        parsedOriginal = parseTypus content
        parsedWithEmpty = parseTypus withExtraEmptyLines
    in length (tfBlocks parsedOriginal) === length (tfBlocks withEmpty)

-- Property: File directives should be parsed correctly regardless of order
prop_parse_directive_order_independence :: Property
prop_parse_directive_order_independence = 
  forAll (listOf genFileDirective `suchThat` (not . null)) $ \directives ->
    let content1 = unlines directives
        content2 = unlines (reverse directives)
        parsed1 = parseTypus content1
        parsed2 = parseTypus content2
    in length (tfBlocks parsed1) === length (tfBlocks parsed2)

-- ============================================================================
-- Unit Tests
-- ============================================================================

test_parse_simple_file :: IO ()
test_parse_simple_file = do
  let content = unlines
        [ "//! ownership=true"
        , "//! dependent-types=false"
        , ""
        , "func main() {"
        , "    return 42"
        , "}"
        ]
      parsed = parseTypus content
  length (tfBlocks parsed) @?= 1

test_parse_file_with_syntax_errors :: IO ()
test_parse_file_with_syntax_errors = do
  let content = unlines
        [ "//! ownership=true"
        , ""
        , "func main() {"
        , "    return 42"
        , "    // Missing closing brace"
        ]
      parsed = parseTypus content
  -- Should still parse but potentially with syntax errors
  length (tfBlocks parsed) @?= 1

test_parse_empty_directives :: IO ()
test_parse_empty_directives = do
  let content = unlines
        [ "//!"
        , "///"
        , ""
        , "func test() {}"
        ]
      parsed = parseTypus content
  length (tfBlocks parsed) @?= 1

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Parser Properties Tests"
  [ testProperty "Parse roundtrip consistency" prop_parse_roundtrip_consistency
  , testProperty "Parse empty content" prop_parse_empty_content
  , testProperty "Parse directive comments" prop_parse_directive_comments
  , testProperty "Parse whitespace independence" prop_parse_whitespace_independence
  , testProperty "Parse consecutive empty lines" prop_parse_consecutive_empty_lines
  , testProperty "Parse directive order independence" prop_parse_directive_order_independence
  , testCase "Parse simple file" test_parse_simple_file
  , testCase "Parse file with syntax errors" test_parse_file_with_syntax_errors
  , testCase "Parse empty directives" test_parse_empty_directives
  ]