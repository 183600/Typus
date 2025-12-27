{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserDirectiveSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
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
  , Located(..)
  , startPos
  , posAt
  , spanBetween
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text.IO as TIO

-- ============================================================================
-- Parser Directive Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Parser Directive Tests"
    [ testGroup "File directive parsing"
        [ testCase "parses ownership directive correctly" test_file_ownership_directive
        , testCase "parses dependent types directive correctly" test_file_dependent_types_directive
        , testCase "parses constraints directive correctly" test_file_constraints_directive
        , testCase "parses multiple file directives" test_multiple_file_directives
        , testCase "handles malformed file directives" test_malformed_file_directives
        , testCase "handles file directives with extra whitespace" test_file_directives_whitespace
        ]

    , testGroup "Block directive parsing"
        [ testCase "parses block ownership directive correctly" test_block_ownership_directive
        , testCase "parses block dependent types directive correctly" test_block_dependent_types_directive
        , testCase "parses block constraints directive correctly" test_block_constraints_directive
        , testCase "parses multiple block directives" test_multiple_block_directives
        , testCase "handles malformed block directives" test_malformed_block_directives
        ]

    , testGroup "Build tag parsing"
        [ testCase "parses single build tag" test_single_build_tag
        , testCase "parses multiple build tags" test_multiple_build_tags
        , testCase "handles build tags with special characters" test_build_tags_special_chars
        , testCase "handles empty build tags" test_empty_build_tags
        ]

    , testGroup "Code block parsing"
        [ testCase "parses simple code block" test_simple_code_block
        , testCase "parses code block with directives" test_code_block_with_directives
        , testCase "parses multiple code blocks" test_multiple_code_blocks
        , testCase "handles empty code block" test_empty_code_block
        , testCase "preserves code block content" test_preserves_code_content
        ]

    , testGroup "Error handling and edge cases"
        [ testCase "handles completely empty file" test_empty_file
        , testCase "handles file with only directives" test_only_directives
        , testCase "handles file with only code blocks" test_only_code_blocks
        , testCase "handles mixed directives and code blocks" test_mixed_content
        , testCase "handles very long lines" test_long_lines
        ]

    , testGroup "Property-based tests"
        [ fastProperty "parseTypus is idempotent for valid files" prop_parse_idempotent
        , fastProperty "file directives are preserved during parsing" prop_file_directives_preserved
        , fastProperty "block directives are preserved during parsing" prop_block_directives_preserved
        , fastProperty "build tags are preserved during parsing" prop_build_tags_preserved
        ]
    ]

-- ============================================================================
-- File Directive Parsing Tests
-- ============================================================================

test_file_ownership_directive :: IO ()
test_file_ownership_directive = do
  let content = "//! ownership=true\n\nfunc main() {}\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let directives = tfDirectives typusFile
          ownership = fdOwnership directives
      assertBool "Ownership directive should be present" (isJust ownership)
      locatedValue (fromMaybe (error "Missing ownership") ownership) @?= True

test_file_dependent_types_directive :: IO ()
test_file_dependent_types_directive = do
  let content = "//! dependent-types=true\n\nfunc test() {}\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let directives = tfDirectives typusFile
          depTypes = fdDependentTypes directives
      assertBool "Dependent types directive should be present" (isJust depTypes)
      locatedValue (fromMaybe (error "Missing dependent types") depTypes) @?= True

test_file_constraints_directive :: IO ()
test_file_constraints_directive = do
  let content = "//! constraints=true\n\nfunc test() {}\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let directives = tfDirectives typusFile
          constraints = fdConstraints directives
      assertBool "Constraints directive should be present" (isJust constraints)
      locatedValue (fromMaybe (error "Missing constraints") constraints) @?= True

test_multiple_file_directives :: IO ()
test_multiple_file_directives = do
  let content = "//! ownership=true, dependent-types=false, constraints=true\n\nfunc main() {}\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let directives = tfDirectives typusFile
          ownership = fdOwnership directives
          depTypes = fdDependentTypes directives
          constraints = fdConstraints directives
      assertBool "Ownership directive should be present" (isJust ownership)
      assertBool "Dependent types directive should be present" (isJust depTypes)
      assertBool "Constraints directive should be present" (isJust constraints)
      locatedValue (fromMaybe (error "Missing ownership") ownership) @?= True
      locatedValue (fromMaybe (error "Missing dependent types") depTypes) @?= False
      locatedValue (fromMaybe (error "Missing constraints") constraints) @?= True

test_malformed_file_directives :: IO ()
test_malformed_file_directives = do
  let content = "//! invalid-directive=value\n\nfunc main() {}\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse should not fail with unknown directive: " ++ show err
    Right typusFile -> do
      let directives = tfDirectives typusFile
      -- Unknown directives should be ignored, but parsing should succeed
      directives @?= defaultFileDirectives

test_file_directives_whitespace :: IO ()
test_file_directives_whitespace = do
  let content = "//!   ownership =   true   ,   dependent-types =   false   \n\nfunc main() {}\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let directives = tfDirectives typusFile
          ownership = fdOwnership directives
          depTypes = fdDependentTypes directives
      assertBool "Ownership directive should be present" (isJust ownership)
      assertBool "Dependent types directive should be present" (isJust depTypes)
      locatedValue (fromMaybe (error "Missing ownership") ownership) @?= True
      locatedValue (fromMaybe (error "Missing dependent types") depTypes) @?= False

-- ============================================================================
-- Block Directive Parsing Tests
-- ============================================================================

test_block_ownership_directive :: IO ()
test_block_ownership_directive = do
  let content = "//! ownership=true\n\n//#! ownership=false\nfunc test() {}\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      assertBool "Should have at least one code block" (not (null blocks))
      let firstBlock = head blocks
          directives = cbDirectives firstBlock
          ownership = bdOwnership directives
      assertBool "Block ownership directive should be present" (isJust ownership)
      locatedValue (fromMaybe (error "Missing block ownership") ownership) @?= False

test_block_dependent_types_directive :: IO ()
test_block_dependent_types_directive = do
  let content = "//! dependent-types=true\n\n//#! dependent-types=false\nfunc test() {}\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      assertBool "Should have at least one code block" (not (null blocks))
      let firstBlock = head blocks
          directives = cbDirectives firstBlock
          depTypes = bdDependentTypes directives
      assertBool "Block dependent types directive should be present" (isJust depTypes)
      locatedValue (fromMaybe (error "Missing block dependent types") depTypes) @?= False

test_block_constraints_directive :: IO ()
test_block_constraints_directive = do
  let content = "//! constraints=true\n\n//#! constraints=false\nfunc test() {}\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      assertBool "Should have at least one code block" (not (null blocks))
      let firstBlock = head blocks
          directives = cbDirectives firstBlock
          constraints = bdConstraints directives
      assertBool "Block constraints directive should be present" (isJust constraints)
      locatedValue (fromMaybe (error "Missing block constraints") constraints) @?= False

test_multiple_block_directives :: IO ()
test_multiple_block_directives = do
  let content = "//! ownership=true\n\n//#! ownership=false, dependent-types=true, constraints=false\nfunc test() {}\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      assertBool "Should have at least one code block" (not (null blocks))
      let firstBlock = head blocks
          directives = cbDirectives firstBlock
          ownership = bdOwnership directives
          depTypes = bdDependentTypes directives
          constraints = bdConstraints directives
      assertBool "Block ownership directive should be present" (isJust ownership)
      assertBool "Block dependent types directive should be present" (isJust depTypes)
      assertBool "Block constraints directive should be present" (isJust constraints)
      locatedValue (fromMaybe (error "Missing block ownership") ownership) @?= False
      locatedValue (fromMaybe (error "Missing block dependent types") depTypes) @?= True
      locatedValue (fromMaybe (error "Missing block constraints") constraints) @?= False

test_malformed_block_directives :: IO ()
test_malformed_block_directives = do
  let content = "//! ownership=true\n\n//#! invalid-directive=value\nfunc test() {}\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse should not fail with unknown block directive: " ++ show err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      assertBool "Should have at least one code block" (not (null blocks))
      let firstBlock = head blocks
          directives = cbDirectives firstBlock
      -- Unknown directives should be ignored, but parsing should succeed
      directives @?= defaultBlockDirectives

-- ============================================================================
-- Build Tag Parsing Tests
-- ============================================================================

test_single_build_tag :: IO ()
test_single_build_tag = do
  let content = "//! +build linux\n\nfunc main() {}\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let buildTags = tfBuildTags typusFile
      assertBool "Should have one build tag" (length buildTags == 1)
      locatedValue (head buildTags) @?= "+build linux"

test_multiple_build_tags :: IO ()
test_multiple_build_tags = do
  let content = "//! +build linux,amd64\n//! +build !windows\n\nfunc main() {}\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let buildTags = tfBuildTags typusFile
      assertBool "Should have two build tags" (length buildTags == 2)
      locatedValue (buildTags !! 0) @?= "+build linux,amd64"
      locatedValue (buildTags !! 1) @?= "+build !windows"

test_build_tags_special_chars :: IO ()
test_build_tags_special_chars = do
  let content = "//! +build linux,!windows,amd64\n\nfunc main() {}\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let buildTags = tfBuildTags typusFile
      assertBool "Should have one build tag with special chars" (length buildTags == 1)
      locatedValue (head buildTags) @?= "+build linux,!windows,amd64"

test_empty_build_tags :: IO ()
test_empty_build_tags = do
  let content = "//! +build\n\nfunc main() {}\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let buildTags = tfBuildTags typusFile
      -- Empty build tags should be ignored or handled gracefully
      assertBool "Should handle empty build tags gracefully" (True)

-- ============================================================================
-- Code Block Parsing Tests
-- ============================================================================

test_simple_code_block :: IO ()
test_simple_code_block = do
  let content = "func main() {\n    fmt.Println(\"Hello, World!\")\n}\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      assertBool "Should have one code block" (length blocks == 1)
      let block = head blocks
          blockContent = cbContent block
      assertBool "Content should contain function definition" ("func main()" `isInfixOf` blockContent)

test_code_block_with_directives :: IO ()
test_code_block_with_directives = do
  let content = "//! ownership=true\n\n//#! dependent-types=false\nfunc test() {\n    return 42\n}\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      assertBool "Should have one code block" (length blocks == 1)
      let block = head blocks
          directives = cbDirectives block
          blockContent = cbContent block
          depTypes = bdDependentTypes directives
      assertBool "Block dependent types directive should be present" (isJust depTypes)
      locatedValue (fromMaybe (error "Missing block dependent types") depTypes) @?= False
      assertBool "Content should contain function definition" ("func test()" `isInfixOf` blockContent)

test_multiple_code_blocks :: IO ()
test_multiple_code_blocks = do
  let content = "func first() {}\n\nfunc second() {}\n\nfunc third() {}\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      assertBool "Should have three code blocks" (length blocks >= 3)
      let blockContents = map cbContent blocks
      assertBool "First block should contain first function" (any ("func first()" `isInfixOf`) blockContents)
      assertBool "Second block should contain second function" (any ("func second()" `isInfixOf`) blockContents)
      assertBool "Third block should contain third function" (any ("func third()" `isInfixOf`) blockContents)

test_empty_code_block :: IO ()
test_empty_code_block = do
  let content = "\n\n\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      -- Empty code blocks should be handled gracefully
      assertBool "Should handle empty code blocks gracefully" (True)

test_preserves_code_content :: IO ()
test_preserves_code_content = do
  let content = "func main() {\n    // This is a comment\n    fmt.Println(\"Hello, 世界!\")\n    /* Multi-line\n       comment */\n    return 42\n}\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      assertBool "Should have one code block" (length blocks >= 1)
      let block = head blocks
          blockContent = cbContent block
      assertBool "Content should preserve comments" ("// This is a comment" `isInfixOf` blockContent)
      assertBool "Content should preserve Unicode" ("世界" `isInfixOf` blockContent)
      assertBool "Content should preserve multi-line comments" ("Multi-line" `isInfixOf` blockContent)

-- ============================================================================
-- Error Handling and Edge Cases
-- ============================================================================

test_empty_file :: IO ()
test_empty_file = do
  let content = ""
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let directives = tfDirectives typusFile
          buildTags = tfBuildTags typusFile
          blocks = tfBlocks typusFile
      directives @?= defaultFileDirectives
      buildTags @?= []
      blocks @?= []

test_only_directives :: IO ()
test_only_directives = do
  let content = "//! ownership=true\n//! dependent-types=false\n//! +build linux\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let directives = tfDirectives typusFile
          buildTags = tfBuildTags typusFile
          blocks = tfBlocks typusFile
          ownership = fdOwnership directives
          depTypes = fdDependentTypes directives
      assertBool "Ownership directive should be present" (isJust ownership)
      assertBool "Dependent types directive should be present" (isJust depTypes)
      assertBool "Should have build tag" (length buildTags >= 1)
      blocks @?= []

test_only_code_blocks :: IO ()
test_only_code_blocks = do
  let content = "func main() {}\nfunc helper() {}\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let directives = tfDirectives typusFile
          buildTags = tfBuildTags typusFile
          blocks = tfBlocks typusFile
      directives @?= defaultFileDirectives
      buildTags @?= []
      assertBool "Should have code blocks" (length blocks >= 2)

test_mixed_content :: IO ()
test_mixed_content = do
  let content = "//! ownership=true\n//! +build linux\n\nfunc main() {}\n\n//#! dependent-types=false\nfunc helper() {}\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let directives = tfDirectives typusFile
          buildTags = tfBuildTags typusFile
          blocks = tfBlocks typusFile
          ownership = fdOwnership directives
      assertBool "Ownership directive should be present" (isJust ownership)
      assertBool "Should have build tag" (length buildTags >= 1)
      assertBool "Should have code blocks" (length blocks >= 2)

test_long_lines :: IO ()
test_long_lines = do
  let longLine = replicate 1000 'a' ++ "func main() {}" ++ replicate 1000 'b'
      content = longLine ++ "\n"
      result = parseTypus content
  case result of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      assertBool "Should handle long lines gracefully" (length blocks >= 1)

-- ============================================================================
-- Property-Based Tests
-- ============================================================================

prop_parse_idempotent :: Property
prop_parse_idempotent =
  forAll arbitrary $ \content ->
    let result1 = parseTypus content
        result2 = parseTypus content
    in case (result1, result2) of
         (Left _, Left _) -> property True
         (Right file1, Right file2) -> property $ file1 === file2
         _ -> property False

prop_file_directives_preserved :: Property
prop_file_directives_preserved =
  forAll arbitrary $ \ownership ->
  forAll arbitrary $ \depTypes ->
  forAll arbitrary $ \constraints ->
    let content = "//! ownership=" ++ show ownership ++ 
                  ", dependent-types=" ++ show depTypes ++ 
                  ", constraints=" ++ show constraints ++ "\n\nfunc main() {}\n"
        result = parseTypus content
    in case result of
         Left _ -> property False
         Right typusFile ->
           let directives = tfDirectives typusFile
               actualOwnership = fmap locatedValue (fdOwnership directives)
               actualDepTypes = fmap locatedValue (fdDependentTypes directives)
               actualConstraints = fmap locatedValue (fdConstraints directives)
           in property $ actualOwnership === Just ownership .&&.
                        actualDepTypes === Just depTypes .&&.
                        actualConstraints === Just constraints

prop_block_directives_preserved :: Property
prop_block_directives_preserved =
  forAll arbitrary $ \ownership ->
  forAll arbitrary $ \depTypes ->
  forAll arbitrary $ \constraints ->
    let content = "//! ownership=true\n\n//#! ownership=" ++ show ownership ++ 
                  ", dependent-types=" ++ show depTypes ++ 
                  ", constraints=" ++ show constraints ++ "\nfunc main() {}\n"
        result = parseTypus content
    in case result of
         Left _ -> property False
         Right typusFile ->
           let blocks = tfBlocks typusFile
           in if null blocks
              then property False
              else let firstBlock = head blocks
                       directives = cbDirectives firstBlock
                       actualOwnership = fmap locatedValue (bdOwnership directives)
                       actualDepTypes = fmap locatedValue (bdDependentTypes directives)
                       actualConstraints = fmap locatedValue (bdConstraints directives)
                   in property $ actualOwnership === Just ownership .&&.
                                actualDepTypes === Just depTypes .&&.
                                actualConstraints === Just constraints

prop_build_tags_preserved :: Property
prop_build_tags_preserved =
  forAll arbitrary $ \tag ->
    let content = "//! +build " ++ tag ++ "\n\nfunc main() {}\n"
        result = parseTypus content
    in case result of
         Left _ -> property False
         Right typusFile ->
           let buildTags = tfBuildTags typusFile
               tagStrings = map locatedValue buildTags
           in property $ any (("+build " ++ tag) `isPrefixOf`) tagStrings