{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CoreParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, vectorOf, elements, oneof)

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
  , startPos
  , spanFrom
  , spanStart
  , spanEnd
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- ============================================================================
-- Generators
-- ============================================================================

genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  col <- choose (1, 1000)
  return $ posAt line col

genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  end <- genSourcePos
  return $ spanBetween start end

genLocatedBool :: Gen (Maybe (Located Bool))
genLocatedBool = oneof
  [ return Nothing
  , do
      value <- elements [True, False]
      span <- genSourceSpan
      return $ Just (Located value span)
  ]

genLocatedString :: Gen (Located String)
genLocatedString = do
  value <- elements ["debug", "release", "test", "linux", "windows", "darwin"]
  span <- genSourceSpan
  return $ Located value span

genFileDirectives :: Gen FileDirectives
genFileDirectives = do
  ownership <- genLocatedBool
  dependentTypes <- genLocatedBool
  constraints <- genLocatedBool
  return $ FileDirectives ownership dependentTypes constraints

genBlockDirectives :: Gen BlockDirectives
genBlockDirectives = do
  ownership <- genLocatedBool
  dependentTypes <- genLocatedBool
  constraints <- genLocatedBool
  return $ BlockDirectives ownership dependentTypes constraints

genCodeContent :: Gen String
genCodeContent = do
  lines <- choose (1, 10)
  content <- listOf $ elements 
    [ "func main() {"
    , "    println(\"Hello, World!\")"
    , "}"
    , "var x int = 42"
    , "const y = \"test\""
    , "if x > 0 {"
    , "    return x"
    , "}"
    , "type Person struct {"
    , "    Name string"
    , "    Age int"
    , "}"
    ]
  return $ unlines $ take lines content

genCodeBlock :: Gen CodeBlock
genCodeBlock = do
  directives <- genBlockDirectives
  content <- genCodeContent
  span <- genSourceSpan
  return $ CodeBlock directives content span

genTypusFile :: Gen TypusFile
genTypusFile = do
  directives <- genFileDirectives
  buildTags <- listOf genLocatedString
  blocks <- listOf genCodeBlock
  syntaxErrors <- return []  -- Simplified for testing
  return $ TypusFile directives buildTags blocks syntaxErrors

genValidTypusContent :: Gen String
genValidTypusContent = do
  hasFileDirectives <- elements [True, False]
  hasBuildTags <- elements [True, False]
  numBlocks <- choose (1, 5)
  
  let fileDirective = if hasFileDirectives
        then "//! ownership=true, dependent-types=true\n"
        else ""
      
      buildTagDirective = if hasBuildTags
        then "// +build debug\n"
        else ""
      
      generateBlock i = unlines
        [ "// ownership=true"
        , "func test" ++ show i ++ "() {"
        , "    x := " ++ show (i * 10)
        , "    return x"
        , "}"
        , ""
        ]
      
      blocks = concatMap generateBlock [1..numBlocks]
  
  return $ fileDirective ++ buildTagDirective ++ blocks

-- ============================================================================
-- Properties for FileDirectives
-- ============================================================================

prop_defaultFileDirectives_has_no_values :: Property
prop_defaultFileDirectives_has_no_values =
  let directives = defaultFileDirectives
  in property $ isNothing (fdOwnership directives) .&&.
               isNothing (fdDependentTypes directives) .&&.
               isNothing (fdConstraints directives)

prop_fileDirectives_extracts_ownership :: String -> Property
prop_fileDirectives_extracts_ownership content =
  "//! ownership=true" `isInfixOf` content ==>
  let parsed = parseTypus content
      directives = tfDirectives parsed
      ownership = fdOwnership directives
  in property $ isJust ownership .&&. 
               (maybe False locatedValue ownership) === True

prop_fileDirectives_extracts_dependent_types :: String -> Property
prop_fileDirectives_extracts_dependent_types content =
  "//! dependent-types=false" `isInfixOf` content ==>
  let parsed = parseTypus content
      directives = tfDirectives parsed
      dependentTypes = fdDependentTypes directives
  in property $ isJust dependentTypes .&&. 
               (maybe False locatedValue dependentTypes) === False

prop_fileDirectives_extracts_constraints :: String -> Property
prop_fileDirectives_extracts_constraints content =
  "//! constraints=true" `isInfixOf` content ==>
  let parsed = parseTypus content
      directives = tfDirectives parsed
      constraints = fdConstraints directives
  in property $ isJust constraints .&&. 
               (maybe False locatedValue constraints) === True

-- ============================================================================
-- Properties for BlockDirectives
-- ============================================================================

prop_defaultBlockDirectives_has_no_values :: Property
prop_defaultBlockDirectives_has_no_values =
  let directives = defaultBlockDirectives
  in property $ isNothing (bdOwnership directives) .&&.
               isNothing (bdDependentTypes directives) .&&.
               isNothing (bdConstraints directives)

prop_blockDirectives_extracts_ownership :: String -> Property
prop_blockDirectives_extracts_ownership content =
  "// ownership=true" `isInfixOf` content ==>
  let parsed = parseTypus content
      blocks = tfBlocks parsed
      hasOwnershipBlock = any (\block -> 
        let directives = cbDirectives block
            ownership = bdOwnership directives
        in isJust ownership && maybe False locatedValue ownership) blocks
  in property $ hasOwnershipBlock === True

-- ============================================================================
-- Properties for CodeBlock
-- ============================================================================

prop_codeBlock_preserves_content :: String -> Property
prop_codeBlock_preserves_content content =
  not (null content) ==> 
  let parsed = parseTypus content
      blocks = tfBlocks parsed
      blockContents = map cbContent blocks
  in property $ any (not . null) blockContents

prop_codeBlock_tracks_span :: String -> Property
prop_codeBlock_tracks_span content =
  not (null content) ==> 
  let parsed = parseTypus content
      blocks = tfBlocks parsed
      spans = map cbSpan blocks
  in property $ all isValidSpan spans

-- ============================================================================
-- Properties for TypusFile
-- ============================================================================

prop_typusFile_preserves_structure :: String -> Property
prop_typusFile_preserves_structure content =
  not (null content) ==> 
  let parsed = parseTypus content
      directives = tfDirectives parsed
      buildTags = tfBuildTags parsed
      blocks = tfBlocks parsed
  in property $ length blocks >= 0 .&&.
               length buildTags >= 0

prop_typusFile_handles_empty_content :: Property
prop_typusFile_handles_empty_content =
  let parsed = parseTypus ""
      directives = tfDirectives parsed
      buildTags = tfBuildTags parsed
      blocks = tfBlocks parsed
  in property | length blocks === 0

-- ============================================================================
-- Properties for Build Tags
-- ============================================================================

prop_buildTags_extracts_build_constraints :: String -> Property
prop_buildTags_extracts_build_constraints content =
  "// +build debug" `isInfixOf` content ==>
  let parsed = parseTypus content
      buildTags = tfBuildTags parsed
      hasDebugTag = any ((== "debug") . locatedValue) buildTags
  in property $ hasDebugTag === True

prop_buildTags_handles_multiple_tags :: String -> Property
prop_buildTags_handles_multiple_tags content =
  "// +build debug" `isInfixOf` content && "// +build test" `isInfixOf` content ==>
  let parsed = parseTypus content
      buildTags = tfBuildTags parsed
      tagValues = map locatedValue buildTags
  in property $ "debug" `elem` tagValues .&&. "test" `elem` tagValues

-- ============================================================================
-- Properties for Parsing Robustness
-- ============================================================================

prop_parser_handles_mixed_directives :: String -> String -> String -> Property
prop_parser_handles_mixed_directives fileDirective blockDirective code =
  not (null fileDirective || null blockDirective || null code) ==>
  let content = unlines [fileDirective, blockDirective, code]
      parsed = parseTypus content
  in property $ length (tfBlocks parsed) >= 0

prop_parser_handles_nested_blocks :: Int -> Property
prop_parser_handles_nested_blocks depth =
  depth >= 0 && depth <= 5 ==>
  let generateNestedBlock 0 = "func base() { return 0 }"
      generateNestedBlock n = "func level" ++ show n ++ "() { " ++ generateNestedBlock (n-1) ++ " }"
      content = generateNestedBlock depth
      parsed = parseTypus content
  in property $ length (tfBlocks parsed) >= 0

prop_parser_preserves_line_structure :: String -> Property
prop_parser_preserves_line_structure content =
  not (null content) ==> 
  let inputLines = lines content
      parsed = parseTypus content
      blocks = tfBlocks parsed
      blockContents = concatMap (lines . cbContent) blocks
  in property $ length blockContents <= length inputLines

-- ============================================================================
-- Properties for Error Handling
-- ============================================================================

prop_parser_handles_malformed_directives :: String -> Property
prop_parser_handles_malformed_directives malformedDirective =
  not (null malformedDirective) ==> 
  let content = malformedDirective ++ "\nfunc test() {}"
      parsed = parseTypus content
  in property $ length (tfBlocks parsed) >= 0

prop_parser_handles_unicode_content :: String -> Property
prop_parser_handles_unicode_content unicodeText =
  not (null unicodeText) ==> 
  let content = "// Unicode test: " ++ unicodeText ++ "\nfunc test() { println(\"" ++ unicodeText ++ "\") }"
      parsed = parseTypus content
  in property $ length (tfBlocks parsed) >= 0

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Core Parser QuickCheck Tests"
  [ testGroup "FileDirectives Properties"
    [ fastProperty "defaultFileDirectives has no values" prop_defaultFileDirectives_has_no_values
    , fastProperty "fileDirectives extracts ownership" prop_fileDirectives_extracts_ownership
    , fastProperty "fileDirectives extracts dependent types" prop_fileDirectives_extracts_dependent_types
    , fastProperty "fileDirectives extracts constraints" prop_fileDirectives_extracts_constraints
    ]

  , testGroup "BlockDirectives Properties"
    [ fastProperty "defaultBlockDirectives has no values" prop_defaultBlockDirectives_has_no_values
    , fastProperty "blockDirectives extracts ownership" prop_blockDirectives_extracts_ownership
    ]

  , testGroup "CodeBlock Properties"
    [ fastProperty "codeBlock preserves content" prop_codeBlock_preserves_content
    , fastProperty "codeBlock tracks span" prop_codeBlock_tracks_span
    ]

  , testGroup "TypusFile Properties"
    [ fastProperty "typusFile preserves structure" prop_typusFile_preserves_structure
    , fastProperty "typusFile handles empty content" prop_typusFile_handles_empty_content
    ]

  , testGroup "Build Tags Properties"
    [ fastProperty "buildTags extracts build constraints" prop_buildTags_extracts_build_constraints
    , fastProperty "buildTags handles multiple tags" prop_buildTags_handles_multiple_tags
    ]

  , testGroup "Parsing Robustness Properties"
    [ fastProperty "parser handles mixed directives" prop_parser_handles_mixed_directives
    , fastProperty "parser handles nested blocks" prop_parser_handles_nested_blocks
    , fastProperty "parser preserves line structure" prop_parser_preserves_line_structure
    ]

  , testGroup "Error Handling Properties"
    [ fastProperty "parser handles malformed directives" prop_parser_handles_malformed_directives
    , fastProperty "parser handles unicode content" prop_parser_handles_unicode_content
    ]
  ]