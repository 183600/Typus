module Test.Unit.ParserAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, (===), forAll, Gen, choose, arbitrary, listOf, elements, oneof, suchThat)
import TestSupport.QuickCheck (fastProperty)

import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), 
              defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Utils (trim)

-- ============================================================================
-- Generators
-- ============================================================================

-- Generate valid identifiers (alphanumeric + underscore + dash)
genIdentifier :: Gen String
genIdentifier = do
  first <- elements ['a'..'z'] ++ ['A'..'Z'] ++ "_"
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-"
  return $ first : rest

-- Generate directive key-value pairs
genDirectivePair :: Gen (String, String)
genDirectivePair = do
  key <- genIdentifier
  value <- genIdentifier
  return (key, value)

-- Generate file directive content
genFileDirective :: Gen String
genFileDirective = do
  pairs <- listOf genDirectivePair
  return $ "//! " ++ unlines (L.map (\(k, v) -> k ++ ": " ++ v) pairs)

-- Generate block directive content
genBlockDirective :: Gen String
genBlockDirective = do
  pairs <- listOf genDirectivePair
  return $ "{//! " ++ unwords (L.map (\(k, v) -> k ++ ":" ++ v) pairs) ++ "}"

-- Generate simple code content
genCodeContent :: Gen String
genCodeContent = do
  lines' <- listOf $ oneof
    [ return "    x := 1"
    , return "    y := x + 2"
    , return "    if x > 0 {"
    , return "        return x"
    , return "    }"
    , return "    func test() {"
    , return "        return 42"
    , return "    }"
    , return ""
    ]
  return $ unlines lines'

-- Generate complete code blocks
genCodeBlock :: Gen String
genCodeBlock = do
  directive <- oneof [return "", genBlockDirective]
  content <- genCodeContent
  return $ directive ++ "\n" ++ content

-- Generate complete Typus file content
genTypusFileContent :: Gen String
genTypusFileContent = do
  fileDirective <- oneof [return "", genFileDirective]
  buildTags <- listOf $ do
    tag <- genIdentifier
    return $ "// +build " ++ tag
  blocks <- listOf genCodeBlock
  return $ unlines $ fileDirective : buildTags ++ blocks

-- Generate strings that should parse successfully
genValidTypusContent :: Gen String
genValidTypusContent = genTypusFileContent

-- Generate strings that might fail parsing
genInvalidTypusContent :: Gen String
genInvalidTypusContent = oneof
  [ return "if x > 0\n    return x\n"  -- Missing opening brace
  , return "{//! invalid:syntax content"  -- Unclosed block directive
  , return "//! malformed directive without colon"
  , do
      lines' <- listOf $ elements ["    x := 1", "    y := 2", "    if condition {"]
      return $ unlines lines'  -- Missing closing brace
  ]

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: Parsing empty content returns a file with no blocks
prop_parseEmptyContent :: Bool
prop_parseEmptyContent =
  case parseTypus "" of
    Left _ -> False
    Right file -> L.null (tfBlocks file) && tfDirectives file == defaultFileDirectives

-- Property: Parsing content with only file directives preserves directives
prop_parseFileDirectives :: [(String, String)] -> Property
prop_parseFileDirectives pairs =
  not (null pairs) ==>
  let directiveStr = "//! " ++ unlines (L.map (\(k, v) -> k ++ ": " ++ v) pairs)
  in case parseTypus directiveStr of
       Left _ -> False
       Right file -> tfDirectives file /= defaultFileDirectives

-- Property: Parsing content with build tags preserves build tags
prop_parseBuildTags :: [String] -> Property
prop_parseBuildTags tags =
  not (null tags) ==>
  let tagLines = L.map (\t -> "// +build " ++ t) tags
      content = unlines tagLines
  in case parseTypus content of
       Left _ -> False
       Right file -> not (L.null (tfBuildTags file))

-- Property: Round-trip property: if parsing succeeds, basic structure is preserved
prop_parseRoundTrip :: String -> Property
prop_parseRoundTrip content =
  case parseTypus content of
    Left _ -> property True  -- Invalid input is allowed to fail
    Right file -> 
      let reconstructed = reconstructTypusFile file
      in case parseTypus reconstructed of
           Left _ -> False  -- Should be able to parse our own output
           Right parsedAgain -> tfBlocks parsedAgain == tfBlocks file

-- Property: Parsing preserves line count in blocks
prop_parsePreservesLineCount :: String -> Property
prop_parsePreservesLineCount content =
  case parseTypus content of
    Left _ -> property True
    Right file ->
      let originalLines = L.length $ L.filter (not . null) $ lines content
          blockLines = L.sum $ L.map (L.length . lines . cbContent) (tfBlocks file)
      in blockLines <= originalLines  -- Blocks should not have more lines than original

-- Property: File directives are parsed correctly
prop_fileDirectiveParsing :: String -> String -> Property
prop_fileDirectiveParsing key value =
  not (null key) && not (null value) && L.all isIdentifierChar (key ++ value) ==>
  let content = "//! " ++ key ++ ": " ++ value ++ "\n"
  in case parseTypus content of
       Left _ -> False
       Right file -> tfDirectives file /= defaultFileDirectives

-- Property: Block directives are parsed correctly
prop_blockDirectiveParsing :: String -> String -> Property
prop_blockDirectiveParsing key value =
  not (null key) && not (null value) && L.all isIdentifierChar (key ++ value) ==>
  let content = "{//! " ++ key ++ ": " ++ value ++ "}\n    x := 1\n"
  in case parseTypus content of
       Left _ -> False
       Right file -> 
         case tfBlocks file of
           [] -> False
           (block:_) -> cbDirectives block /= defaultBlockDirectives

-- Property: Invalid if statements are detected
prop_invalidIfStatements :: String -> Property
prop_invalidIfStatements condition =
  "if " `L.isPrefixOf` condition && not ("{" `L.isInfixOf` condition) ==>
  let content = condition ++ "\n    x := 1\n"
  in case parseTypus content of
       Left errMsg -> "missing opening brace" `L.isInfixOf` errMsg
       Right _ -> False  -- Should fail to parse

-- Property: Multiple blocks are parsed correctly
prop_multipleBlocksParsing :: [String] -> Property
prop_multipleBlocksParsing blockContents =
  not (null blockContents) && L.length blockContents <= 5 ==>  -- Limit size
  let content = unlines $ concatMap (\b -> ["", "    " ++ b, ""]) blockContents
  in case parseTypus content of
       Left _ -> property True  -- May fail due to syntax issues
       Right file -> L.length (tfBlocks file) >= 1

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Check if character is valid identifier character
isIdentifierChar :: Char -> Bool
isIdentifierChar c = L.elem c (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-")

-- Reconstruct a TypusFile back to string format (simplified)
reconstructTypusFile :: TypusFile -> String
reconstructTypusFile file = unlines $ 
  (if tfDirectives file /= defaultFileDirectives then ["//! directives present"] else []) ++
  L.map (("// +build " ++) . locValue) (tfBuildTags file) ++
  concatMap reconstructCodeBlock (tfBlocks file)

-- Reconstruct a CodeBlock to string format
reconstructCodeBlock :: CodeBlock -> [String]
reconstructCodeBlock block = 
  ["", "    " ++ cbContent block, ""]

-- ============================================================================
-- Unit Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser Advanced QuickCheck Tests"
  [ testGroup "Basic Parsing Properties"
    [ testProperty "Parsing empty content returns a file with no blocks" prop_parseEmptyContent
    , testProperty "Parsing content with file directives preserves directives" prop_parseFileDirectives
    , testProperty "Parsing content with build tags preserves build tags" prop_parseBuildTags
    ]

  , testGroup "Round-trip L.and Structure Properties"
    [ testProperty "Round-trip property: if parsing succeeds, basic structure is preserved" prop_parseRoundTrip
    , testProperty "Parsing preserves line count in blocks" prop_parsePreservesLineCount
    ]

  , testGroup "Directive Parsing Properties"
    [ testProperty "File directives are parsed correctly" prop_fileDirectiveParsing
    , testProperty "Block directives are parsed correctly" prop_blockDirectiveParsing
    ]

  , testGroup "Error Detection Properties"
    [ testProperty "Invalid if statements are detected" prop_invalidIfStatements
    ]

  , testGroup "Complex Parsing Properties"
    [ testProperty "Multiple blocks are parsed correctly" prop_multipleBlocksParsing
    ]

  , testGroup "Unit Tests"
    [ testCase "Parse empty file" $ do
        let result = parseTypus ""
        case result of
          Left err -> assertBool $ "Should parse empty file: " ++ err
          Right file -> do
            tfBlocks file @?= []
            tfDirectives file @?= defaultFileDirectives
            tfBuildTags file @?= []

    , testCase "Parse simple file directive" $ do
        let content = "//! ownership: true\n"
        case parseTypus content of
          Left err -> assertBool $ "Should parse file directive: " ++ err
          Right file -> do
            assertBool "File directives should be set" $ tfDirectives file /= defaultFileDirectives

    , testCase "Parse build tags" $ do
        let content = "// +build linux\n// +build amd64\n"
        case parseTypus content of
          Left err -> assertBool $ "Should parse build tags: " ++ err
          Right file -> do
            L.length (tfBuildTags file) @?= 2
            locValue (L.head (tfBuildTags file)) @?= "linux"

    , testCase "Parse simple code block" $ do
        let content = "    x := 1\n    return x\n"
        case parseTypus content of
          Left err -> assertBool $ "Should parse simple code block: " ++ err
          Right file -> do
            assertBool "Should have at least one block" $ not (L.null (tfBlocks file))
            let firstBlock = L.head (tfBlocks file)
            assertBool "Block should contain content" $ not (L.null (cbContent firstBlock))

    , testCase "Parse block directive" $ do
        let content = "{//! ownership: false}\n    x := 1\n"
        case parseTypus content of
          Left err -> assertBool $ "Should parse block directive: " ++ err
          Right file -> do
            case tfBlocks file of
              [] -> assertBool "Should have at least one block" False
              (block:_) -> do
                assertBool "Block directives should be set" $ cbDirectives block /= defaultBlockDirectives

    , testCase "Detect invalid if statement" $ do
        let content = "if x > 0\n    return x\n"
        case parseTypus content of
          Left errMsg -> 
            assertBool "Should detect missing opening brace" $ 
              "missing opening brace" `L.isInfixOf` errMsg
          Right _ -> 
            assertBool "Should fail to parse invalid if statement" False

    , testCase "Parse complete file with L.all elements" $ do
        let content = unlines
              [ "//! ownership: true"
              , "// +build linux"
              , ""
              , "{//! dependent-types: true}"
              , "    x := 1"
              , "    if x > 0 {"
              , "        return x"
              , "    }"
              ]
        case parseTypus content of
          Left err -> assertBool $ "Should parse complete file: " ++ err
          Right file -> do
            assertBool "File directives should be set" $ tfDirectives file /= defaultFileDirectives
            L.length (tfBuildTags file) @?= 1
            assertBool "Should have at least one block" $ not (L.null (tfBlocks file))
    ]
  ]