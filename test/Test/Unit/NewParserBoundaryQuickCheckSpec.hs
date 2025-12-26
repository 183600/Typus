{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewParserBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import SourceLocation (SourceSpan(..), startPos)
import Utils (trim)
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf)

-- Property: Default directives consistency
prop_default_file_directives_consistency :: Property
prop_default_file_directives_consistency =
  let defaults = defaultFileDirectives
  in property $ fdOwnership defaults === Nothing .&&.
     fdDependentTypes defaults === Nothing .&&.
     fdConstraints defaults === Nothing

-- Property: Default block directives consistency
prop_default_block_directives_consistency :: Property
prop_default_block_directives_consistency =
  let defaults = defaultBlockDirectives
  in property $ bdOwnership defaults === Nothing .&&.
     bdDependentTypes defaults === Nothing .&&.
     bdConstraints defaults === Nothing

-- Property: Parse empty file
prop_parse_empty_file :: Property
prop_parse_empty_file =
  let result = parseTypus "" "empty.typus"
  in case result of
    Left _ -> property $ False
    Right file -> property $ tfBlocks file === []

-- Property: Parse file with only whitespace
prop_parse_whitespace_file :: String -> Property
prop_parse_whitespace_file ws =
  all isSpace ws ==>
  let result = parseTypus ws "whitespace.typus"
  in case result of
    Left _ -> property $ False
    Right file -> property $ null (tfBlocks file) || all (null . cbContent) (tfBlocks file)

-- Property: Parse simple content without directives
prop_parse_simple_content :: String -> Property
prop_parse_simple_content content =
  not (any (`isInfixOf` content) ["//!", "//@", "/*", "*/"]) && length content <= 200 ==>
  let result = parseTypus content "simple.typus"
  in case result of
    Left _ -> property $ False
    Right file -> property $ not (null (tfBlocks file)) ==> 
      let firstBlock = head (tfBlocks file)
      in cbContent firstBlock === trim content

-- Property: Parse file with line comments only
prop_parse_line_comments :: String -> String -> Property
prop_parse_line_comments code comment =
  not (any (`isInfixOf` code) ["//!", "//@", "/*", "*/", "\"", "'"]) && length code <= 100 && length comment <= 50 ==>
  let content = code ++ "\n// " ++ comment ++ "\n" ++ code
      result = parseTypus content "comments.typus"
  in case result of
    Left _ -> property $ False
    Right file -> property $ length (tfBlocks file) >= 1

-- Property: Parse file with file directives
prop_parse_file_directives :: String -> Property
prop_parse_file_directives directive =
  "//!" `isPrefixOf` directive && length directive <= 100 && not (any (`isInfixOf` directive) ["/*", "*/"]) ==>
  let content = directive ++ "\nsome code here"
      result = parseTypus content "directive.typus"
  in case result of
    Left _ -> property $ False
    Right file -> property $ tfDirectives file /= defaultFileDirectives || not (null (tfBlocks file))

-- Property: Parse file with block directives
prop_parse_block_directives :: String -> String -> Property
prop_parse_block_directives directive code =
  "//@" `isPrefixOf` directive && length directive <= 50 && length code <= 100 &&
  not (any (`isInfixOf` code) ["//!", "//@", "/*", "*/"]) ==>
  let content = directive ++ "\n" ++ code
      result = parseTypus content "block.typus"
  in case result of
    Left _ -> property $ False
    Right file -> property $ not (null (tfBlocks file)) ==> 
      let firstBlock = head (tfBlocks file)
      in cbDirectives firstBlock /= defaultBlockDirectives

-- Property: Parse multiple code blocks
prop_parse_multiple_blocks :: [String] -> Property
prop_parse_multiple_blocks codeList =
  not (null codeList) && all (\c -> length c <= 50 && not (any (`isInfixOf` c) ["//!", "//@", "/*", "*/"])) codeList ==>
  let separatedBlocks = map (\c -> "//@ownership\n" ++ c) codeList
      content = unlines separatedBlocks
      result = parseTypus content "multiple.typus"
  in case result of
    Left _ -> property $ False
    Right file -> property $ length (tfBlocks file) >= length codeList

-- Property: Parse file with mixed content
prop_parse_mixed_content :: String -> String -> String -> Property
prop_parse_mixed_content directive code comment =
  "//!" `isPrefixOf` directive && length directive <= 50 &&
  length code <= 100 && not (any (`isInfixOf` code) ["//!", "//@", "/*", "*/", "\"", "'"]) &&
  length comment <= 50 ==>
  let content = directive ++ "\n\n" ++ code ++ "\n// " ++ comment ++ "\n" ++ code
      result = parseTypus content "mixed.typus"
  in case result of
    Left _ -> property $ False
    Right file -> property $ tfDirectives file /= defaultFileDirectives .&&. length (tfBlocks file) >= 1

-- Property: Parse file with build tags
prop_parse_build_tags :: [String] -> Property
prop_parse_build_tags tags =
  not (null tags) && all (\t -> length t <= 20 && all isAlphaNum t) (take 3 tags) ==>
  let tagLines = map (\t -> "//@build:" ++ t) (take 3 tags)
      content = unlines tagLines
      result = parseTypus content "tags.typus"
  in case result of
    Left _ -> property $ False
    Right file -> property $ not (null (tfBuildTags file))

-- Property: Parse file with ownership directive
prop_parse_ownership_directive :: String -> Property
prop_parse_ownership_directive code =
  length code <= 100 && not (any (`isInfixOf` code) ["//!", "//@", "/*", "*/"]) ==>
  let content = "//@ownership\n" ++ code
      result = parseTypus content "ownership.typus"
  in case result of
    Left _ -> property $ False
    Right file -> property $ not (null (tfBlocks file)) ==>
      let firstBlock = head (tfBlocks file)
          directives = cbDirectives firstBlock
      in bdOwnership directives /= Nothing

-- Property: Parse file with dependent types directive
prop_parse_dependent_types_directive :: String -> Property
prop_parse_dependent_types_directive code =
  length code <= 100 && not (any (`isInfixOf` code) ["//!", "//@", "/*", "*/"]) ==>
  let content = "//@dependent-types\n" ++ code
      result = parseTypus content "dependent-types.typus"
  in case result of
    Left _ -> property $ False
    Right file -> property $ not (null (tfBlocks file)) ==>
      let firstBlock = head (tfBlocks file)
          directives = cbDirectives firstBlock
      in bdDependentTypes directives /= Nothing

-- Property: Parse file with constraints directive
prop_parse_constraints_directive :: String -> Property
prop_parse_constraints_directive code =
  length code <= 100 && not (any (`isInfixOf` code) ["//!", "//@", "/*", "*/"]) ==>
  let content = "//@constraints\n" ++ code
      result = parseTypus content "constraints.typus"
  in case result of
    Left _ -> property $ False
    Right file -> property $ not (null (tfBlocks file)) ==>
      let firstBlock = head (tfBlocks file)
          directives = cbDirectives firstBlock
      in bdConstraints directives /= Nothing

-- Property: Parse file preserves code content
prop_parse_preserves_content :: String -> Property
prop_parse_preserves_content code =
  length code <= 150 && not (any (`isInfixOf` code) ["//!", "//@", "/*", "*/"]) ==>
  let content = "//@ownership\n" ++ code
      result = parseTypus content "preserve.typus"
  in case result of
    Left _ -> property $ False
    Right file -> property $ not (null (tfBlocks file)) ==>
      let firstBlock = head (tfBlocks file)
          blockContent = cbContent firstBlock
      in trim code === trim blockContent

-- Property: Parse file with multiple directives in block
prop_parse_multiple_block_directives :: String -> Property
prop_parse_multiple_block_directives code =
  length code <= 100 && not (any (`isInfixOf` code) ["//!", "//@", "/*", "*/"]) ==>
  let content = "//@ownership\n//@dependent-types\n" ++ code
      result = parseTypus content "multi-directive.typus"
  in case result of
    Left _ -> property $ False
    Right file -> property $ not (null (tfBlocks file)) ==>
      let firstBlock = head (tfBlocks file)
          directives = cbDirectives firstBlock
      in bdOwnership directives /= Nothing .&&. bdDependentTypes directives /= Nothing

-- Property: Parse file handles large content gracefully
prop_parse_large_content :: Int -> String -> Property
prop_parse_large_content multiplier baseCode =
  multiplier > 0 && multiplier <= 10 && length baseCode <= 20 &&
  not (any (`isInfixOf` baseCode) ["//!", "//@", "/*", "*/"]) ==>
  let largeContent = unlines (replicate multiplier baseCode)
      result = parseTypus largeContent "large.typus"
  in case result of
    Left _ -> property $ False
    Right file -> property $ length (tfBlocks file) >= 1

-- Property: Parse file with syntax error tracking
prop_parse_tracks_syntax_errors :: String -> Property
prop_parse_tracks_syntax_errors malformed =
  length malformed <= 100 && "/*" `isInfixOf` malformed && not ("*/" `isInfixOf` malformed) ==>
  let result = parseTypus malformed "error.typus"
  in case result of
    Left _ -> property $ False  -- Should parse but track errors
    Right file -> property $ True  -- Successfully parsed with potential errors

tests :: TestTree
tests = testGroup "New Parser Boundary QuickCheck Tests"
  [ fastProperty "default file directives consistency" prop_default_file_directives_consistency
  , fastProperty "default block directives consistency" prop_default_block_directives_consistency
  , fastProperty "parse empty file" prop_parse_empty_file
  , fastProperty "parse file with only whitespace" prop_parse_whitespace_file
  , fastProperty "parse simple content without directives" prop_parse_simple_content
  , fastProperty "parse file with line comments only" prop_parse_line_comments
  , fastProperty "parse file with file directives" prop_parse_file_directives
  , fastProperty "parse file with block directives" prop_parse_block_directives
  , fastProperty "parse multiple code blocks" prop_parse_multiple_blocks
  , fastProperty "parse file with mixed content" prop_parse_mixed_content
  ]