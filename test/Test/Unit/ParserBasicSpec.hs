{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.ParserBasicSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, Assertion, assertFailure)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>))
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), 
              defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locValue)
import qualified Data.Text as T
import Data.Char (isSpace)

-- Helper generators for Parser tests
genDirective :: Gen String
genDirective = do
  key <- elements ["ownership", "dependent_types", "dependent-types", "constraints"]
  value <- elements ["on", "off", "true", "false"]
  return $ key ++ ": " ++ value

genFileDirective :: Gen String
genFileDirective = do
  directive <- genDirective
  return $ "//! " ++ directive

genBlockDirective :: Gen String
genBlockDirective = do
  directive <- genDirective
  return $ "{//! " ++ directive ++ "}"

genMarkdownDirective :: Gen String
genMarkdownDirective = do
  directive <- genDirective
  return $ "// @ " ++ directive

genCodeLine :: Gen String
genCodeLine = do
  len <- choose (1, 20)
  vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t;,.(){}[]+-*/"

genBuildTag :: Gen String
genBuildTag = do
  tag <- elements ["linux", "windows", "darwin", "amd64", "arm64"]
  return $ "//go:build " ++ tag

-- Test cases for Parser module

-- Test 1: Parse empty file
test_parse_empty_file :: Assertion
test_parse_empty_file = do
  let input = ""
      result = parseTypus input
  case result of
    Right file -> assertEqual "Empty file should have no blocks" [] (tfBlocks file)
    Left err -> assertFailure $ "Failed to parse empty file: " ++ err

-- Test 2: Parse simple code without directives
test_parse_simple_code :: Assertion
test_parse_simple_code = do
  let input = "let x = 42\nlet y = x + 1\n"
      result = parseTypus input
  case result of
    Right file -> do
      assertEqual "Should have one block" 1 (length (tfBlocks file))
      assertEqual "Should have default directives" defaultFileDirectives (tfDirectives file)
    Left err -> assertFailure $ "Failed to parse simple code: " ++ err

-- Test 3: Parse file with ownership directive
test_parse_file_ownership_directive :: Assertion
test_parse_file_ownership_directive = do
  let input = "//! ownership: on\nlet x = 42\n"
      result = parseTypus input
  case result of
    Right file -> do
      let directives = tfDirectives file
      case fdOwnership directives of
        Just loc | locValue loc == True -> return ()
        _ -> assertFailure "Expected ownership directive to be on"
    Left err -> assertFailure $ "Failed to parse file with ownership directive: " ++ err

-- Test 4: Parse file with dependent_types directive
test_parse_file_dependent_types_directive :: Assertion
test_parse_file_dependent_types_directive = do
  let input = "//! dependent_types: true\nlet x = 42\n"
      result = parseTypus input
  case result of
    Right file -> do
      let directives = tfDirectives file
      case fdDependentTypes directives of
        Just loc | locValue loc == True -> return ()
        _ -> assertFailure "Expected dependent_types directive to be true"
    Left err -> assertFailure $ "Failed to parse file with dependent_types directive: " ++ err

-- Test 5: Parse file with constraints directive
test_parse_file_constraints_directive :: Assertion
test_parse_file_constraints_directive = do
  let input = "//! constraints: off\nlet x = 42\n"
      result = parseTypus input
  case result of
    Right file -> do
      let directives = tfDirectives file
      case fdConstraints directives of
        Just loc | locValue loc == False -> return ()
        _ -> assertFailure "Expected constraints directive to be off"
    Left err -> assertFailure $ "Failed to parse file with constraints directive: " ++ err

-- Test 6: Parse file with multiple directives
test_parse_file_multiple_directives :: Assertion
test_parse_file_multiple_directives = do
  let input = "//! ownership: on, dependent_types: true, constraints: off\nlet x = 42\n"
      result = parseTypus input
  case result of
    Right file -> do
      let directives = tfDirectives file
      case fdOwnership directives of
        Just loc | locValue loc == True -> return ()
        _ -> assertFailure "Expected ownership directive to be on"
      case fdDependentTypes directives of
        Just loc | locValue loc == True -> return ()
        _ -> assertFailure "Expected dependent_types directive to be true"
      case fdConstraints directives of
        Just loc | locValue loc == False -> return ()
        _ -> assertFailure "Expected constraints directive to be off"
    Left err -> assertFailure $ "Failed to parse file with multiple directives: " ++ err

-- Test 7: Parse file with build tags
test_parse_file_build_tags :: Assertion
test_parse_file_build_tags = do
  let input = "//go:build linux\n// +build amd64\nlet x = 42\n"
      result = parseTypus input
  case result of
    Right file -> do
      let buildTags = tfBuildTags file
      assertEqual "Should have 2 build tags" 2 (length buildTags)
      case buildTags of 
        (firstTag:secondTag:_) -> do
          assertEqual "First tag" "//go:build linux" (locValue firstTag)
          assertEqual "Second tag" "// +build amd64" (locValue secondTag)
        _ -> assertFailure "Expected at least 2 build tags"
    Left err -> assertFailure $ "Failed to parse file with build tags: " ++ err

-- Test 8: Parse block with directives
test_parse_block_with_directives :: Assertion
test_parse_block_with_directives = do
  let input = "{//! ownership: on, dependent_types: true}\nlet x = 42\n}\nlet y = 43\n"
      result = parseTypus input
  case result of
    Right file -> do
      let blocks = tfBlocks file
      assertEqual "Should have 2 blocks" 2 (length blocks)
      case blocks of 
        (firstBlock:_) -> do
          let directives = cbDirectives firstBlock
          case bdOwnership directives of
            Just loc | locValue loc == True -> return ()
            _ -> assertFailure "Expected block ownership directive to be on"
          case bdDependentTypes directives of
            Just loc | locValue loc == True -> return ()
            _ -> assertFailure "Expected block dependent_types directive to be true"
    Left err -> assertFailure $ "Failed to parse block with directives: " ++ err

-- Test 9: Parse markdown block with directives
test_parse_markdown_block_with_directives :: Assertion
test_parse_markdown_block_with_directives = do
  let input = "```typus\n// @ ownership: on\n// @ dependent_types: true\nlet x = 42\n```\nlet y = 43\n"
      result = parseTypus input
  case result of
    Right file -> do
      let blocks = tfBlocks file
      assertEqual "Should have 2 blocks" 2 (length blocks)
      case blocks of 
        (firstBlock:_) -> do
          let directives = cbDirectives firstBlock
          case bdOwnership directives of
            Just loc | locValue loc == True -> return ()
            _ -> assertFailure "Expected block ownership directive to be on"
          case bdDependentTypes directives of
            Just loc | locValue loc == True -> return ()
            _ -> assertFailure "Expected block dependent_types directive to be true"
    Left err -> assertFailure $ "Failed to parse markdown block with directives: " ++ err

-- Test 10: Parse code with if statement and braces
test_parse_if_with_braces :: Assertion
test_parse_if_with_braces = do
  let input = "if condition {\n  doSomething()\n}\n"
      result = parseTypus input
  case result of
    Right file -> return ()
    Left err -> assertFailure $ "Failed to parse if with braces: " ++ err

-- Test 11: Parse code with if statement without braces (should fail)
test_parse_if_without_braces :: Assertion
test_parse_if_without_braces = do
  let input = "if condition\n  doSomething()\n"
      result = parseTypus input
  case result of
    Right _ -> assertFailure "Expected parsing to fail for if without braces"
    Left _ -> return ()

-- Test 12: Parse code with package declaration
test_parse_package_declaration :: Assertion
test_parse_package_declaration = do
  let input = "package main\nlet x = 42\n"
      result = parseTypus input
  case result of
    Right file -> return ()
    Left err -> assertFailure $ "Failed to parse package declaration: " ++ err

-- Test 13: Parse code with multiple package declarations (should fail)
test_parse_multiple_package_declarations :: Assertion
test_parse_multiple_package_declarations = do
  let input = "package main\npackage utils\nlet x = 42\n"
      result = parseTypus input
  case result of
    Right _ -> assertFailure "Expected parsing to fail for multiple package declarations"
    Left _ -> return ()

-- Test 14: Parse code with incomplete expression (should fail)
test_parse_incomplete_expression :: Assertion
test_parse_incomplete_expression = do
  let input = "let x =\nlet y = 42\n"
      result = parseTypus input
  case result of
    Right _ -> assertFailure "Expected parsing to fail for incomplete expression"
    Left _ -> return ()

-- Test 15: Parse code with function declaration (should succeed)
test_parse_function_declaration :: Assertion
test_parse_function_declaration = do
  let input = "func add(x int, y int) int {\n  return x + y\n}\n"
      result = parseTypus input
  case result of
    Right file -> return ()
    Left err -> assertFailure $ "Failed to parse function declaration: " ++ err

-- Test 16: Parse code with nested blocks
test_parse_nested_blocks :: Assertion
test_parse_nested_blocks = do
  let input = "{//! ownership: on\nlet x = 42\n{//! dependent_types: true\nlet y = x + 1\n}\n}\n"
      result = parseTypus input
  case result of
    Right file -> do
      let blocks = tfBlocks file
      assertEqual "Should have 2 blocks" 2 (length blocks)
    Left err -> assertFailure $ "Failed to parse nested blocks: " ++ err

-- Test 17: Parse code with markdown and directive blocks mixed
test_parse_mixed_blocks :: Assertion
test_parse_mixed_blocks = do
  let input = "{//! ownership: on\nlet x = 42\n}\n```typus\n// @ dependent_types: true\nlet y = 43\n```\nlet z = 44\n"
      result = parseTypus input
  case result of
    Right file -> do
      let blocks = tfBlocks file
      assertEqual "Should have 3 blocks" 3 (length blocks)
    Left err -> assertFailure $ "Failed to parse mixed blocks: " ++ err

-- Test 18: Parse code with comments
test_parse_comments :: Assertion
test_parse_comments = do
  let input = "// This is a comment\nlet x = 42\n// Another comment\nlet y = 43\n"
      result = parseTypus input
  case result of
    Right file -> do
      let blocks = tfBlocks file
      assertEqual "Should have 2 blocks" 2 (length blocks)
    Left err -> assertFailure $ "Failed to parse code with comments: " ++ err

-- Test 19: Parse code with empty lines
test_parse_empty_lines :: Assertion
test_parse_empty_lines = do
  let input = "\nlet x = 42\n\nlet y = 43\n\n"
      result = parseTypus input
  case result of
    Right file -> do
      let blocks = tfBlocks file
      assertEqual "Should have 2 blocks" 2 (length blocks)
    Left err -> assertFailure $ "Failed to parse code with empty lines: " ++ err

-- Test 20: Parse code with whitespace
test_parse_whitespace :: Assertion
test_parse_whitespace = do
  let input = "  let x = 42\n\tlet y = 43\n  let z = 44\n"
      result = parseTypus input
  case result of
    Right file -> do
      let blocks = tfBlocks file
      assertEqual "Should have 3 blocks" 3 (length blocks)
    Left err -> assertFailure $ "Failed to parse code with whitespace: " ++ err

-- Property tests for Parser module

-- Property 1: Parsing empty input should result in empty file
prop_parse_empty_input :: Property
prop_parse_empty_input = 
  let input = ""
      result = parseTypus input
  in case result of
    Right file -> property (null (tfBlocks file))
    Left _ -> property False

-- Property 2: Parsing code without directives should result in default directives
prop_parse_no_directives :: String -> Property
prop_parse_no_directives code = 
  not ("//!" `isInfixOf` code) && 
  not ("{//!" `isInfixOf` code) && 
  not ("// @" `isInfixOf` code) && 
  not ("```typus" `isInfixOf` code) ==>
    let result = parseTypus code
    in case result of
      Right file -> tfDirectives file == defaultFileDirectives
      Left _ -> False

-- Property 3: Parsing code with ownership directive should set ownership
prop_parse_ownership_directive :: String -> Property
prop_parse_ownership_directive code = 
  "//! ownership: on" `isInfixOf` code ==>
    let result = parseTypus code
    in case result of
      Right file -> case fdOwnership (tfDirectives file) of
        Just loc | locValue loc == True -> True
        _ -> False
      Left _ -> False

-- Property 4: Parsing code with dependent_types directive should set dependent_types
prop_parse_dependent_types_directive :: String -> Property
prop_parse_dependent_types_directive code = 
  "//! dependent_types: true" `isInfixOf` code ==>
    let result = parseTypus code
    in case result of
      Right file -> case fdDependentTypes (tfDirectives file) of
        Just loc | locValue loc == True -> True
        _ -> False
      Left _ -> False

-- Property 5: Parsing code with constraints directive should set constraints
prop_parse_constraints_directive :: String -> Property
prop_parse_constraints_directive code = 
  "//! constraints: off" `isInfixOf` code ==>
    let result = parseTypus code
    in case result of
      Right file -> case fdConstraints (tfDirectives file) of
        Just loc | locValue loc == False -> True
        _ -> False
      Left _ -> False

-- Property 6: Parsing code with build tags should extract build tags
prop_parse_build_tags :: String -> Property
prop_parse_build_tags code = 
  ("//go:build" `isInfixOf` code || "// +build" `isInfixOf` code) ==>
    let result = parseTypus code
    in case result of
      Right file -> not (null (tfBuildTags file))
      Left _ -> False

-- Property 7: Parsing code with block directives should create blocks with directives
prop_parse_block_directives :: String -> Property
prop_parse_block_directives code = 
  "{//! ownership: on" `isInfixOf` code ==>
    let result = parseTypus code
    in case result of
      Right file -> case tfBlocks file of
        (block:_) -> case bdOwnership (cbDirectives block) of
          Just loc | locValue loc == True -> True
          _ -> False
        [] -> False
      Left _ -> False

-- Property 8: Parsing code with markdown directives should create blocks with directives
prop_parse_markdown_directives :: String -> Property
prop_parse_markdown_directives code = 
  "```typus" `isInfixOf` code && "// @ ownership: on" `isInfixOf` code ==>
    let result = parseTypus code
    in case result of
      Right file -> case tfBlocks file of
        (block:_) -> case bdOwnership (cbDirectives block) of
          Just loc | locValue loc == True -> True
          _ -> False
        [] -> False
      Left _ -> False

-- Property 9: Parsing code with if without braces should fail
prop_parse_if_without_braces_fails :: String -> Property
prop_parse_if_without_braces_fails code = 
  "if " `isInfixOf` code && not ("{" `isInfixOf` code) ==>
    let result = parseTypus code
    in case result of
      Right _ -> False
      Left _ -> True

-- Property 10: Parsing code with package declaration should succeed
prop_parse_package_declaration_succeeds :: String -> Property
prop_parse_package_declaration_succeeds code = 
  "package " `isInfixOf` code && not (isPrefixOf "//" (trim code)) ==>
    let result = parseTypus code
    in case result of
      Right _ -> True
      Left _ -> False

-- Helper functions
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    tails [] = [[]]
    tails s@(_:t) = s : tails t

parserBasicTests :: TestTree
parserBasicTests = testGroup "Parser Basic Tests"
  [ testGroup "Unit Tests"
    [ testCase "Parse empty file" test_parse_empty_file
    , testCase "Parse simple code" test_parse_simple_code
    , testCase "Parse file with ownership directive" test_parse_file_ownership_directive
    , testCase "Parse file with dependent_types directive" test_parse_file_dependent_types_directive
    , testCase "Parse file with constraints directive" test_parse_file_constraints_directive
    , testCase "Parse file with multiple directives" test_parse_file_multiple_directives
    , testCase "Parse file with build tags" test_parse_file_build_tags
    , testCase "Parse block with directives" test_parse_block_with_directives
    , testCase "Parse markdown block with directives" test_parse_markdown_block_with_directives
    , testCase "Parse code with if statement and braces" test_parse_if_with_braces
    , testCase "Parse code with if statement without braces (should fail)" test_parse_if_without_braces
    , testCase "Parse code with package declaration" test_parse_package_declaration
    , testCase "Parse code with multiple package declarations (should fail)" test_parse_multiple_package_declarations
    , testCase "Parse code with incomplete expression (should fail)" test_parse_incomplete_expression
    , testCase "Parse code with function declaration (should succeed)" test_parse_function_declaration
    , testCase "Parse code with nested blocks" test_parse_nested_blocks
    , testCase "Parse code with markdown and directive blocks mixed" test_parse_mixed_blocks
    , testCase "Parse code with comments" test_parse_comments
    , testCase "Parse code with empty lines" test_parse_empty_lines
    , testCase "Parse code with whitespace" test_parse_whitespace
    ]
  , testProperties "Property Tests"
    [ ("Parsing empty input should result in empty file", property prop_parse_empty_input)
    , ("Parsing code without directives should result in default directives", property prop_parse_no_directives)
    , ("Parsing code with ownership directive should set ownership", property prop_parse_ownership_directive)
    , ("Parsing code with dependent_types directive should set dependent_types", property prop_parse_dependent_types_directive)
    , ("Parsing code with constraints directive should set constraints", property prop_parse_constraints_directive)
    , ("Parsing code with build tags should extract build tags", property prop_parse_build_tags)
    , ("Parsing code with block directives should create blocks with directives", property prop_parse_block_directives)
    , ("Parsing code with markdown directives should create blocks with directives", property prop_parse_markdown_directives)
    , ("Parsing code with if without braces should fail", property prop_parse_if_without_braces_fails)
    , ("Parsing code with package declaration should succeed", property prop_parse_package_declaration_succeeds)
    ]
  ]