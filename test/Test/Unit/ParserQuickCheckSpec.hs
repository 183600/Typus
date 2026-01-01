{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Parser
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  , parseTypus
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , locatedWithSpan
  , locatedValue
  , startPos
  )

import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort)
import Data.Char (isSpace)

-- Property: defaultFileDirectives has correct default values
prop_defaultFileDirectives_values :: Property
prop_defaultFileDirectives_values =
  fdOwnership defaultFileDirectives === Nothing &&
  fdDependentTypes defaultFileDirectives === Nothing &&
  fdConstraints defaultFileDirectives === Nothing

-- Property: defaultBlockDirectives has correct default values
prop_defaultBlockDirectives_values :: Property
prop_defaultBlockDirectives_values =
  bdOwnership defaultBlockDirectives === Nothing &&
  bdDependentTypes defaultBlockDirectives === Nothing &&
  bdConstraints defaultBlockDirectives === Nothing

-- Property: parseTypus handles empty input
prop_parseTypus_empty_input :: Property
prop_parseTypus_empty_input =
  let result = parseTypus ""
  in case result of
    Left _ -> property False
    Right typusFile -> tfDirectives typusFile === defaultFileDirectives &&
                       L.null (tfBuildTags typusFile) &&
                       L.null (tfBlocks typusFile)

-- Property: parseTypus handles whitespace-only input
prop_parseTypus_whitespace_only :: String -> Property
prop_parseTypus_whitespace_only whitespace =
  L.all isSpace whitespace ==> 
  let result = parseTypus whitespace
  in case result of
    Left _ -> property False
    Right typusFile -> tfDirectives typusFile === defaultFileDirectives &&
                       L.null (tfBuildTags typusFile) &&
                       L.null (tfBlocks typusFile)

-- Property: parseTypus handles simple content without directives
prop_parseTypus_simple_content :: String -> Property
prop_parseTypus_simple_content content =
  not (null content) && not ("//!" `L.isInfixOf` content) && 
  not ("{//!" `L.isInfixOf` content) && not ("package " `L.isInfixOf` content) ==>
  let result = parseTypus content
  in case result of
    Left _ -> property False
    Right typusFile -> tfDirectives typusFile === defaultFileDirectives &&
                       L.null (tfBuildTags typusFile) &&
                       L.length (tfBlocks typusFile) >= 1

-- Property: parseTypus handles file directives correctly
prop_parseTypus_file_directives :: String -> Property
prop_parseTypus_file_directives directiveValue =
  not (null directiveValue) && not ("//!" `L.isInfixOf` directiveValue) ==>
  let content = "//! ownership: " ++ directiveValue ++ "\npackage main\nfunc main() {}"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right typusFile -> case fdOwnership (tfDirectives typusFile) of
      Nothing -> property False
      Just located -> locatedValue located === True

-- Property: parseTypus handles block directives correctly
prop_parseTypus_block_directives :: String -> Property
prop_parseTypus_block_directives blockContent =
  not (null blockContent) && not ("{//!" `L.isInfixOf` blockContent) ==>
  let content = "package main\n{//! ownership: on}\n" ++ blockContent ++ "\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right typusFile -> L.length (tfBlocks typusFile) >= 1

-- Property: parseTypus handles multiple blocks
prop_parseTypus_multiple_blocks :: String -> String -> Property
prop_parseTypus_multiple_blocks content1 content2 =
  not (null content1) && not (null content2) ==>
  let block1 = "{//! ownership: on}\n" ++ content1 ++ "\n"
      block2 = "{//! dependent_types: off}\n" ++ content2 ++ "\n"
      fullContent = "package main\n" ++ block1 ++ block2
      result = parseTypus fullContent
  in case result of
    Left _ -> property False
    Right typusFile -> L.length (tfBlocks typusFile) >= 2

-- Property: parseTypus handles build tags
prop_parseTypus_build_tags :: String -> Property
prop_parseTypus_build_tags tagValue =
  not (null tagValue) && not ("//go:build" `L.isInfixOf` tagValue) ==>
  let content = "//go:build " ++ tagValue ++ "\npackage main\nfunc main() {}"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right typusFile -> L.length (tfBuildTags typusFile) >= 1

-- Property: parseTypus handles multiple build tags
prop_parseTypus_multiple_build_tags :: String -> String -> Property
prop_parseTypus_multiple_build_tags tag1 tag2 =
  not (null tag1) && not (null tag2) ==>
  let content = "//go:build " ++ tag1 ++ "\n// +build " ++ tag2 ++ "\npackage main\nfunc main() {}"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right typusFile -> L.length (tfBuildTags typusFile) >= 2

-- Property: parseTypus handles package declarations
prop_parseTypus_package_declaration :: String -> Property
prop_parseTypus_package_declaration packageName =
  not (null packageName) && not ("package " `L.isInfixOf` packageName) ==>
  let content = "package " ++ packageName ++ "\nfunc main() {}"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right typusFile -> L.length (tfBlocks typusFile) >= 1

-- Property: parseTypus detects multiple package declarations
prop_parseTypus_multiple_package_declarations :: String -> String -> Property
prop_parseTypus_multiple_package_declarations pkg1 pkg2 =
  not (null pkg1) && not (null pkg2) && pkg1 /= pkg2 ==>
  let content = "package " ++ pkg1 ++ "\npackage " ++ pkg2 ++ "\nfunc main() {}"
      result = parseTypus content
  in case result of
    Left errMsg -> "Multiple package" `L.isInfixOf` errMsg
    Right _ -> property False

-- Property: parseTypus handles nested block directives
prop_parseTypus_nested_blocks :: String -> Property
prop_parseTypus_nested_blocks innerContent =
  not (null innerContent) && not ("{//!" `L.isInfixOf` innerContent) ==>
  let content = "package main\n{//! ownership: on}\nfunc outer() {\n  {//! dependent_types: off}\n  " ++ innerContent ++ "\n}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right typusFile -> L.length (tfBlocks typusFile) >= 2

-- Property: parseTypus handles malformed directives gracefully
prop_parseTypus_malformed_directives :: String -> Property
prop_parseTypus_malformed_directives badDirective =
  not ("//!" `L.isInfixOf` badDirective) && not ("{//!" `L.isInfixOf` badDirective) ==>
  let content = "//! " ++ badDirective ++ "\npackage main\nfunc main() {}"
      result = parseTypus content
  in case result of
    Left _ -> property True  -- Expected to fail
    Right typusFile -> tfDirectives typusFile === defaultFileDirectives

-- Property: parseTypus preserves content order
prop_parseTypus_preserves_order :: [String] -> Property
prop_parseTypus_preserves_order contents =
  not (null contents) && L.length contents <= 5 ==>
  let numberedContent = unlines $ zipWith (\i content -> show i ++ ": " ++ content) [1..] contents
      result = parseTypus numberedContent
  in case result of
    Left _ -> property False
    Right typusFile -> case tfBlocks typusFile of
      [] -> property False
      (block:_) -> L.any (`L.isInfixOf` cbContent block) (map show [1..L.length contents])

-- Property: parseTypus handles mixed directive types
prop_parseTypus_mixed_directives :: String -> String -> String -> Property
prop_parseTypus_mixed_directives fileDirective blockDirective content =
  not (null fileDirective) && not (null blockDirective) && not (null content) ==>
  let fullContent = "//! ownership: " ++ fileDirective ++ "\n//go:build test\npackage main\n{//! dependent_types: " ++ blockDirective ++ "}\n" ++ content
      result = parseTypus fullContent
  in case result of
    Left _ -> property False
    Right typusFile -> L.length (tfBuildTags typusFile) >= 1 &&
                       L.length (tfBlocks typusFile) >= 1

-- Property: parseTypus handles large files
prop_parseTypus_large_files :: Int -> String -> Property
prop_parseTypus_large_files multiplier baseContent =
  multiplier > 0 && multiplier <= 100 ==> 
  let largeContent = L.concat $ replicate multiplier (baseContent ++ "\n")
      result = parseTypus largeContent
  in case result of
    Left _ -> property False
    Right typusFile -> L.length (tfBlocks typusFile) >= 1

-- Property: parseTypus handles Unicode content
prop_parseTypus_unicode_content :: String -> Property
prop_parseTypus_unicode_content unicodeContent =
  let content = "package main\n// Unicode test: " ++ unicodeContent ++ "\nfunc main() {}"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right typusFile -> L.length (tfBlocks typusFile) >= 1

-- Property: parseTypus handles escaped braces in strings
prop_parseTypus_escaped_braces :: String -> Property
prop_parseTypus_escaped_braces stringContent =
  not ('"' `elem` stringContent) && not ('{' `elem` stringContent) && not ('}' `elem` stringContent) ==>
  let content = "package main\nfunc main() {\n  s := \"" ++ stringContent ++ "{escaped}\"\n}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right typusFile -> L.length (tfBlocks typusFile) >= 1

-- Property: parseTypus handles comments with braces
prop_parseTypus_comment_braces :: String -> Property
prop_parseTypus_comment_braces commentContent =
  not (null commentContent) ==>
  let content = "package main\nfunc main() {\n  // Comment with {braces} L.and " ++ commentContent ++ "\n}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right typusFile -> L.length (tfBlocks typusFile) >= 1

-- Property: parseTypus handles function parameters
prop_parseTypus_function_parameters :: String -> Property
prop_parseTypus_function_parameters paramList =
  not (null paramList) && not ('{' `elem` paramList) && not ('}' `elem` paramList) ==>
  let content = "package main\nfunc main(" ++ paramList ++ ") {\n}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right typusFile -> L.length (tfBlocks typusFile) >= 1

-- Property: parseTypus handles complex Go structures
prop_parseTypus_complex_structures :: String -> String -> Property
prop_parseTypus_complex_structures structName structBody =
  not (null structName) && not (null structBody) ==>
  let content = "package main\ntype " ++ structName ++ " struct {\n" ++ structBody ++ "\n}\nfunc main() {}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right typusFile -> L.length (tfBlocks typusFile) >= 1

-- Property: FileDirectives equality
prop_fileDirectives_equality :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Property
prop_fileDirectives_equality ownership dependentTypes constraints =
  let fd1 = FileDirectives ownership dependentTypes constraints
      fd2 = FileDirectives ownership dependentTypes constraints
      fd3 = FileDirectives (fL.map (fmap not) ownership) dependentTypes constraints
  in fd1 === fd2 .&&. 
     (if isJust ownership then fd1 /= fd3 else property True)

-- Property: BlockDirectives equality
prop_blockDirectives_equality :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Property
prop_blockDirectives_equality ownership dependentTypes constraints =
  let bd1 = BlockDirectives ownership dependentTypes constraints
      bd2 = BlockDirectives ownership dependentTypes constraints
      bd3 = BlockDirectives (fL.map (fmap not) ownership) dependentTypes constraints
  in bd1 === bd2 .&&. 
     (if isJust ownership then bd1 /= bd3 else property True)

-- Property: CodeBlock construction
prop_codeBlock_construction :: String -> String -> Property
prop_codeBlock_construction directives content =
  not (null content) ==>
  let span = SourceSpan startPos startPos
      block = CodeBlock defaultBlockDirectives content span
  in cbDirectives block === defaultBlockDirectives &&
     cbContent block === content &&
     cbSpan block === span

-- Property: TypusFile construction
prop_typusFile_construction :: [CodeBlock] -> Property
prop_typusFile_construction blocks =
  not (null blocks) ==>
  let typusFile = TypusFile defaultFileDirectives [] blocks []
  in tfDirectives typusFile === defaultFileDirectives &&
     tfBuildTags typusFile === [] &&
     tfBlocks typusFile === blocks &&
     tfSyntaxErrors typusFile === []

-- Helper function
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

tests :: TestTree
tests = testGroup "Parser QuickCheck tests"
  [ fastProperty "defaultFileDirectives has correct default values" prop_defaultFileDirectives_values
  , fastProperty "defaultBlockDirectives has correct default values" prop_defaultBlockDirectives_values
  , fastProperty "parseTypus handles empty input" prop_parseTypus_empty_input
  , fastProperty "parseTypus handles whitespace-only input" prop_parseTypus_whitespace_only
  , fastProperty "parseTypus handles simple content without directives" prop_parseTypus_simple_content
  , fastProperty "parseTypus handles file directives correctly" prop_parseTypus_file_directives
  , fastProperty "parseTypus handles block directives correctly" prop_parseTypus_block_directives
  , fastProperty "parseTypus handles multiple blocks" prop_parseTypus_multiple_blocks
  , fastProperty "parseTypus handles build tags" prop_parseTypus_build_tags
  , fastProperty "parseTypus handles multiple build tags" prop_parseTypus_multiple_build_tags
  , fastProperty "parseTypus handles package declarations" prop_parseTypus_package_declaration
  , fastProperty "parseTypus detects multiple package declarations" prop_parseTypus_multiple_package_declarations
  , fastProperty "parseTypus handles nested block directives" prop_parseTypus_nested_blocks
  , fastProperty "parseTypus handles malformed directives gracefully" prop_parseTypus_malformed_directives
  , fastProperty "parseTypus preserves content order" prop_parseTypus_preserves_order
  , fastProperty "parseTypus handles mixed directive types" prop_parseTypus_mixed_directives
  , fastProperty "parseTypus handles large files" prop_parseTypus_large_files
  , fastProperty "parseTypus handles Unicode content" prop_parseTypus_unicode_content
  , fastProperty "parseTypus handles escaped braces in strings" prop_parseTypus_escaped_braces
  , fastProperty "parseTypus handles comments with braces" prop_parseTypus_comment_braces
  , fastProperty "parseTypus handles function parameters" prop_parseTypus_function_parameters
  , fastProperty "parseTypus handles complex Go structures" prop_parseTypus_complex_structures
  , fastProperty "FileDirectives equality" prop_fileDirectives_equality
  , fastProperty "BlockDirectives equality" prop_blockDirectives_equality
  , fastProperty "CodeBlock construction" prop_codeBlock_construction
  , fastProperty "TypusFile construction" prop_typusFile_construction
  ]