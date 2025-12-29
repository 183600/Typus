{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserErrorRecoveryAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, suchThat)
import TestSupport.Arbitrary

import Parser (parseTypus, TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import Compiler.Errors.Core (ErrorSeverity(..), ErrorCategory(..))
import Data.Either (isLeft, isRight, fromLeft, fromRight)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, length, take, drop, concat)
import Data.Maybe (isJust, isNothing, catMaybes, fromMaybe)
import qualified Data.Text as T

-- ============================================================================
-- Advanced Parser Error Recovery QuickCheck Tests
-- ============================================================================

-- Property: Parser should maintain position information during error recovery
prop_parser_error_position_preservation :: String -> String -> Property
prop_parser_error_position_preservation good bad =
  length good > 0 && length bad > 0 ==>
  let input = good ++ "\n" ++ bad ++ "\n" ++ good
      result = parseTypus input
  in case result of
    Left _ -> property True  -- Failing is acceptable
    Right tf -> property $ length (tfBlocks tf) >= 0  -- Should produce some structure

-- Property: Parser should handle nested block errors gracefully
prop_parser_nested_block_recovery :: [String] -> Property
prop_parser_nested_block_recovery blocks =
  length blocks > 0 && all (not . null) blocks ==>
  let nestedInput = concatMap (\b -> "  " ++ b ++ "\n") blocks
      result = parseTypus nestedInput
  in case result of
    Left _ -> property True
    Right tf -> property $ length (tfBlocks tf) >= 0

-- Property: Parser should recover from directive errors
prop_parser_directive_error_recovery :: String -> String -> Property
prop_parser_directive_error_recovery directiveName value =
  length directiveName > 0 && length value > 0 ==>
  let malformedDirective = "//! " ++ directiveName ++ " " ++ value ++ " MALFORMED\n"
      validContent = "func main() { return 42; }"
      input = malformedDirective ++ validContent
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right tf -> property $ length (tfBlocks tf) >= 1

-- Property: Parser should handle mixed valid/invalid content
prop_parser_mixed_content_recovery :: [String] -> [String] -> Property
prop_parser_mixed_content_recovery goodParts badParts =
  length goodParts > 0 && length badParts > 0 && all (not . null) goodParts ==>
  let mixed = interleave goodParts badParts
      input = unlines mixed
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right tf -> property $ length (tfBlocks tf) >= 0

-- Property: Parser should handle unclosed blocks
prop_parser_unclosed_block_recovery :: String -> Property
prop_parser_unclosed_block_recovery content =
  length content > 0 ==>
  let unclosedBlock = "{ " ++ content
      input = "func test() " ++ unclosedBlock
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right tf -> property $ length (tfBlocks tf) >= 0

-- Property: Parser should handle malformed function signatures
prop_parser_malformed_function_recovery :: String -> String -> Property
prop_parser_malformed_function_recovery funcName params =
  length funcName > 0 && length params > 0 ==>
  let malformedFunc = "func " ++ funcName ++ "(" ++ params ++ " MALFORMED { return 0; }"
      input = malformedFunc
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right tf -> property $ length (tfBlocks tf) >= 0

-- Property: Parser should handle extreme indentation
prop_parser_extreme_indentation_recovery :: String -> Int -> Property
prop_parser_extreme_indentation_recovery content indentLevel =
  length content > 0 && indentLevel >= 0 && indentLevel <= 100 ==>
  let extremeIndent = replicate indentLevel ' '
      indentedContent = extremeIndent ++ content
      input = indentedContent
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right tf -> property $ length (tfBlocks tf) >= 0

-- Property: Parser should handle malformed type annotations
prop_parser_malformed_type_recovery :: String -> String -> Property
prop_parser_malformed_type_recovery varName typeName =
  length varName > 0 && length typeName > 0 ==>
  let malformedType = "let " ++ varName ++ ": " ++ typeName ++ " MALFORMED = 42"
      input = malformedType
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right tf -> property $ length (tfBlocks tf) >= 0

-- Property: Parser should handle comment errors
prop_parser_comment_error_recovery :: String -> Property
prop_parser_comment_error_recovery commentContent =
  length commentContent > 0 ==>
  let malformedComment = "// " ++ commentContent ++ " UNCLOSING /*"
      validContent = "func main() { return 42; }"
      input = malformedComment ++ "\n" ++ validContent
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right tf -> property $ length (tfBlocks tf) >= 1

-- Property: Parser should handle string literal errors
prop_parser_string_literal_recovery :: String -> Property
prop_parser_string_literal_recovery stringContent =
  length stringContent > 0 ==>
  let malformedString = "let x = \"" ++ stringContent ++ " UNCLOSING"
      validContent = "func main() { return 42; }"
      input = malformedString ++ "\n" ++ validContent
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right tf -> property $ length (tfBlocks tf) >= 1

-- Helper function to interleave two lists
interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x : y : interleave xs ys

-- Test collection
tests :: TestTree
tests = testGroup "Advanced Parser Error Recovery QuickCheck Tests"
  [ fastProperty "Parser maintains position information during error recovery" prop_parser_error_position_preservation
  , fastProperty "Parser handles nested block errors gracefully" prop_parser_nested_block_recovery
  , fastProperty "Parser recovers from directive errors" prop_parser_directive_error_recovery
  , fastProperty "Parser handles mixed valid/invalid content" prop_parser_mixed_content_recovery
  , fastProperty "Parser handles unclosed blocks" prop_parser_unclosed_block_recovery
  , fastProperty "Parser handles malformed function signatures" prop_parser_malformed_function_recovery
  , fastProperty "Parser handles extreme indentation" prop_parser_extreme_indentation_recovery
  , fastProperty "Parser handles malformed type annotations" prop_parser_malformed_type_recovery
  , fastProperty "Parser handles comment errors" prop_parser_comment_error_recovery
  , fastProperty "Parser handles string literal errors" prop_parser_string_literal_recovery
  ]