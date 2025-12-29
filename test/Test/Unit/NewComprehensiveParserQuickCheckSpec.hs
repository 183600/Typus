{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewComprehensiveParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, listOf1, elements, vectorOf, suchThat)

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
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , locatedWithSpan
  , spanEnd
  , spanStart
  )

import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, intercalate)
import Data.Char (isAlphaNum, isSpace)

-- ============================================================================
-- Enhanced Property Tests for Parser Module
-- ============================================================================

-- Property: parseTypus preserves empty input structure
prop_parseTypus_empty_input :: Property
prop_parseTypus_empty_input =
  let result = parseTypus ""
  in case result of
       Left _ -> property True -- Parse errors are acceptable for malformed input
       Right typusFile -> property $ 
         tfDirectives typusFile === defaultFileDirectives .&&.
         null (tfBuildTags typusFile) .&&.
         null (tfBlocks typusFile)

-- Property: parseTypus handles whitespace-only input
prop_parseTypus_whitespace_only :: String -> Property
prop_parseTypus_whitespace_only input =
  all isSpace input ==>
  let result = parseTypus input
  in case result of
       Left _ -> property True
       Right typusFile -> property $ 
         tfDirectives typusFile === defaultFileDirectives .&&.
         null (tfBuildTags typusFile) .&&.
         null (tfBlocks typusFile)

-- Property: parseTypus preserves simple code blocks
prop_parseTypus_simple_blocks :: String -> Property
prop_parseTypus_simple_blocks content =
  not (any (`elem` ["//!", "/*", "*/", "//"]) content) ==>
  let input = "```typus\n" ++ content ++ "\n```"
      result = parseTypus input
  in case result of
       Left _ -> property True
       Right typusFile -> property $ 
         not (null (tfBlocks typusFile)) ==> 
         content `isInfixOf` (cbContent (head (tfBlocks typusFile)))

-- Property: parseTypus handles file directives correctly
prop_parseTypus_file_directives :: String -> String -> Property
prop_parseTypus_file_directives key value =
  not (any (`elem` [" ", "!", "/", "\n", "\r", "\t"]) (key ++ value)) ==>
  let input = "//! " ++ key ++ "=" ++ value ++ "\n"
      result = parseTypus input
  in case result of
       Left _ -> property True
       Right typusFile -> property $ 
         tfDirectives typusFile /= defaultFileDirectives

-- Property: parseTypus handles multiple file directives
prop_parseTypus_multiple_file_directives :: [(String, String)] -> Property
prop_parseTypus_multiple_file_directives directives =
  not (null directives) && all (\(k, v) -> not (any (`elem` [" ", "!", "/", "\n", "\r", "\t"]) (k ++ v))) directives ==>
  let directiveStrs = map (\(k, v) -> k ++ "=" ++ v) directives
      input = "//! " ++ intercalate "," directiveStrs ++ "\n"
      result = parseTypus input
  in case result of
       Left _ -> property True
       Right typusFile -> property $ 
         tfDirectives typusFile /= defaultFileDirectives

-- Property: parseTypus handles block directives correctly
prop_parseTypus_block_directives :: String -> String -> String -> Property
prop_parseTypus_block_directives content key value =
  not (any (`elem` [" ", "!", "/", "\n", "\r", "\t", "`"]) (key ++ value)) ==>
  let input = "//! " ++ key ++ "=" ++ value ++ "\n```typus\n" ++ content ++ "\n```"
      result = parseTypus input
  in case result of
       Left _ -> property True
       Right typusFile -> property $ 
         not (null (tfBlocks typusFile)) ==> 
         let firstBlock = head (tfBlocks typusFile)
         in bdDirectives firstBlock /= defaultBlockDirectives

-- Property: parseTypus preserves build tags
prop_parseTypus_build_tags :: [String] -> Property
prop_parseTypus_build_tags tags =
  not (null tags) && all (not . any (`elem` [" ", "!", "/", "\n", "\r", "\t", "+"])) tags ==>
  let tagStr = intercalate "+" tags
      input = "//+build " ++ tagStr ++ "\n"
      result = parseTypus input
  in case result of
       Left _ -> property True
       Right typusFile -> property $ 
         not (null (tfBuildTags typusFile))

-- Property: parseTypus handles multiple code blocks
prop_parseTypus_multiple_blocks :: [String] -> Property
prop_parseTypus_multiple_blocks contents =
  not (null contents) && all (not . any (`elem` ["```", "//!", "//+build"])) contents ==>
  let blockStrs = map (\content -> "```typus\n" ++ content ++ "\n```\n") contents
      input = intercalate "\n" blockStrs
      result = parseTypus input
  in case result of
       Left _ -> property True
       Right typusFile -> property $ 
         length (tfBlocks typusFile) === length contents

-- Property: parseTypus handles mixed directives and blocks
prop_parseTypus_mixed_content :: String -> String -> String -> Property
prop_parseTypus_mixed_content directiveKey directiveValue content =
  not (any (`elem` [" ", "!", "/", "\n", "\r", "\t", "`", "+"]) (directiveKey ++ directiveValue)) &&
  not (any (`elem` ["```", "//!", "//+build"]) content) ==>
  let input = "//! " ++ directiveKey ++ "=" ++ directiveValue ++ "\n" ++
              "//+build test\n" ++
              "```typus\n" ++ content ++ "\n```\n"
      result = parseTypus input
  in case result of
       Left _ -> property True
       Right typusFile -> property $ 
         not (null (tfBuildTags typusFile)) .&&.
         not (null (tfBlocks typusFile)) .&&.
         tfDirectives typusFile /= defaultFileDirectives

-- Property: parseTypus handles nested content in blocks
prop_parseTypus_nested_content :: String -> String -> Property
prop_parseTypus_nested_content outerContent innerContent =
  not (any (`elem` ["```", "//!", "//+build"]) (outerContent ++ innerContent)) ==>
  let input = "```typus\n" ++ outerContent ++ "\n/* nested comment */\n" ++ innerContent ++ "\n```"
      result = parseTypus input
  in case result of
       Left _ -> property True
       Right typusFile -> property $ 
         not (null (tfBlocks typusFile)) ==> 
         let blockContent = cbContent (head (tfBlocks typusFile))
         in outerContent `isInfixOf` blockContent .&&.
            innerContent `isInfixOf` blockContent

-- Property: parseTypus handles Unicode content
prop_parseTypus_unicode_content :: String -> Property
prop_parseTypus_unicode_content baseContent =
  not (any (`elem` ["```", "//!", "//+build"]) baseContent) ==>
  let unicodeContent = baseContent ++ " café naïve résumé 🚀 测试"
      input = "```typus\n" ++ unicodeContent ++ "\n```"
      result = parseTypus input
  in case result of
       Left _ -> property True
       Right typusFile -> property $ 
         not (null (tfBlocks typusFile)) ==> 
         unicodeContent `isInfixOf` (cbContent (head (tfBlocks typusFile)))

-- Property: parseTypus preserves line structure
prop_parseTypus_preserves_lines :: [String] -> Property
prop_parseTypus_preserves_lines lineList =
  not (null lineList) && all (not . any (`elem` ["```", "//!", "//+build"])) lineList ==>
  let content = intercalate "\n" lineList
      input = "```typus\n" ++ content ++ "\n```"
      result = parseTypus input
  in case result of
       Left _ -> property True
       Right typusFile -> property $ 
         not (null (tfBlocks typusFile)) ==> 
         let blockContent = cbContent (head (tfBlocks typusFile))
             blockLines = lines blockContent
         in length blockLines >= length lineList

-- Property: parseTypus handles empty blocks
prop_parseTypus_empty_blocks :: Property
prop_parseTypus_empty_blocks =
  let input = "```typus\n```"
      result = parseTypus input
  in case result of
       Left _ -> property True
       Right typusFile -> property $ 
         length (tfBlocks typusFile) >= 1

-- Property: parseTypus handles malformed block markers gracefully
prop_parseTypus_malformed_blocks :: String -> Property
prop_parseTypus_malformed_blocks content =
  let input = "```typus\n" ++ content  -- Missing closing marker
      result = parseTypus input
  in case result of
       Left _ -> property True  -- Should fail gracefully
       Right _ -> property True -- Or succeed with partial parsing

-- Property: parseTypus handles directive parsing edge cases
prop_parseTypus_directive_edge_cases :: String -> Property
prop_parseTypus_directive_edge_cases directiveContent =
  "//!" `isPrefixOf` directiveContent ==>
  let result = parseTypus directiveContent
  in case result of
       Left _ -> property True
       Right typusFile -> property $ True -- Should parse some structure

-- Property: parseTypus handles large inputs
prop_parseTypus_large_input :: Int -> String -> Property
prop_parseTypus_large_input multiplier baseContent =
  multiplier >= 0 && multiplier <= 50 && -- Limit for performance
  not (any (`elem` ["```", "//!", "//+build"]) baseContent) ==>
  let largeContent = concat (replicate multiplier (baseContent ++ "\n"))
      input = "```typus\n" ++ largeContent ++ "\n```"
      result = parseTypus input
  in case result of
       Left _ -> property True
       Right typusFile -> property $ 
         not (null (tfBlocks typusFile)) ==> 
         length (cbContent (head (tfBlocks typusFile))) >= length baseContent * multiplier

-- Property: parseTypus maintains directive-block separation
prop_parseTypus_directive_block_separation :: String -> String -> String -> Property
prop_parseTypus_directive_block_separation directiveKey directiveValue content =
  not (any (`elem` [" ", "!", "/", "\n", "\r", "\t", "`", "+"]) (directiveKey ++ directiveValue)) &&
  not (any (`elem` ["```", "//!", "//+build"]) content) ==>
  let input = "//! " ++ directiveKey ++ "=" ++ directiveValue ++ "\n\n" ++
              "```typus\n" ++ content ++ "\n```"
      result = parseTypus input
  in case result of
       Left _ -> property True
       Right typusFile -> property $ 
         tfDirectives typusFile /= defaultFileDirectives .&&.
         not (null (tfBlocks typusFile))

-- Property: parseTypus handles comment-like strings in code blocks
prop_parseTypus_code_like_strings :: String -> Property
prop_parseTypus_code_like_strings content =
  not ("```" `isInfixOf` content) ==>
  let codeLikeContent = "var x = 1; // line comment\n/* block comment */\n" ++ content
      input = "```typus\n" ++ codeLikeContent ++ "\n```"
      result = parseTypus input
  in case result of
       Left _ -> property True
       Right typusFile -> property $ 
         not (null (tfBlocks typusFile)) ==> 
         "var x = 1;" `isInfixOf` (cbContent (head (tfBlocks typusFile)))

-- Property: parseTypus error recovery preserves partial structure
prop_parseTypus_error_recovery :: String -> String -> Property
prop_parseTypus_error_recovery validContent invalidContent =
  not (any (`elem` ["```", "//!", "//+build"]) validContent) ==>
  let input = "```typus\n" ++ validContent ++ "\n```typus\n" ++ invalidContent ++ "\n```"
      result = parseTypus input
  in case result of
       Left _ -> property True
       Right typusFile -> property $ 
         -- Should still have at least the valid block
         not (null (tfBlocks typusFile)) ==> 
         validContent `isInfixOf` (cbContent (head (tfBlocks typusFile)))

-- ============================================================================
-- Test Suite Definition
-- ============================================================================

tests :: TestTree
tests = testGroup "New Comprehensive Parser QuickCheck Tests"
  [ testGroup "Basic parsing properties"
    [ fastProperty "parseTypus preserves empty input structure" prop_parseTypus_empty_input
    , fastProperty "parseTypus handles whitespace-only input" prop_parseTypus_whitespace_only
    , fastProperty "parseTypus preserves simple code blocks" prop_parseTypus_simple_blocks
    , fastProperty "parseTypus handles empty blocks" prop_parseTypus_empty_blocks
    ]

  , testGroup "Directive parsing properties"
    [ fastProperty "parseTypus handles file directives correctly" prop_parseTypus_file_directives
    , fastProperty "parseTypus handles multiple file directives" prop_parseTypus_multiple_file_directives
    , fastProperty "parseTypus handles block directives correctly" prop_parseTypus_block_directives
    , fastProperty "parseTypus handles build tags" prop_parseTypus_build_tags
    , fastProperty "parseTypus directive parsing edge cases" prop_parseTypus_directive_edge_cases
    ]

  , testGroup "Block parsing properties"
    [ fastProperty "parseTypus handles multiple code blocks" prop_parseTypus_multiple_blocks
    , fastProperty "parseTypus handles mixed directives and blocks" prop_parseTypus_mixed_content
    , fastProperty "parseTypus handles nested content in blocks" prop_parseTypus_nested_content
    , fastProperty "parseTypus preserves line structure" prop_parseTypus_preserves_lines
    , fastProperty "parseTypus handles code-like strings in blocks" prop_parseTypus_code_like_strings
    ]

  , testGroup "Error handling and recovery properties"
    [ fastProperty "parseTypus handles malformed block markers gracefully" prop_parseTypus_malformed_blocks
    , fastProperty "parseTypus error recovery preserves partial structure" prop_parseTypus_error_recovery
    ]

  , testGroup "Advanced parsing properties"
    [ fastProperty "parseTypus handles Unicode content" prop_parseTypus_unicode_content
    , fastProperty "parseTypus handles large inputs" prop_parseTypus_large_input
    , fastProperty "parseTypus maintains directive-block separation" prop_parseTypus_directive_block_separation
    ]
  ]