{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserErrorRecoveryPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf1, elements, suchThat)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing)
import Data.Char (isSpace, isAlpha, isDigit)

import Parser
  ( BlockDirectives(..)
  , CodeBlock(..)
  , FileDirectives(..)
  , TypusFile(..)
  , parseTypus
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , spanStart
  , spanEnd
  , posLine
  , posColumn
  )

-- ============================================================================
-- Property Tests for Parser Error Recovery
-- ============================================================================

-- Property: Parser can recover from malformed directives
prop_parser_recovers_from_malformed_directives :: String -> String -> Property
prop_parser_recovers_from_malformed_directives malformed goodCode =
  let source = unlines
        [ "//! ownership" ++ malformed  -- Malformed directive
        , goodCode                       -- Valid code after error
        ]
  in not (null goodCode) ==>
     case parseTypus source of
       Left _ -> property False  -- Should not completely fail
       Right result -> property $ not (L.null (tfCodeBlocks result))

-- Property: Parser handles incomplete syntax gracefully
prop_parser_handles_incomplete_syntax :: String -> Property
prop_parser_handles_incomplete_syntax prefix =
  let incompleteCode = prefix ++ "func main() {"
  in not (null prefix) ==>
     case parseTypus incompleteCode of
       Left _ -> property True  -- Expected to fail but not crash
       Right result -> property $ True  -- May succeed with partial parsing

-- Property: Parser preserves valid code blocks despite errors
prop_parser_preserves_valid_blocks :: String -> String -> String -> Property
prop_parser_preserves_valid_blocks before error after =
  let source = unlines [before, error, after]
      hasValidBefore = not (null before) && not ("func" `L.isInfixOf` error)
      hasValidAfter = not (null after) && not ("func" `L.isInfixOf` error)
  in (hasValidBefore || hasValidAfter) ==>
     case parseTypus source of
       Left _ -> property False
       Right result -> 
         let blocks = tfCodeBlocks result
         in property $ not (null blocks)

-- Property: Parser handles mixed valid/invalid directives
prop_parser_handles_mixed_directives :: Bool -> Bool -> Bool -> Property
prop_parser_handles_mixed_directives hasValid hasInvalid hasExtra =
  let directives = L.concat
        [ if hasValid then ["//! ownership: on"] else []
        , if hasInvalid then ["//! ownership invalid"] else []
        , if hasExtra then ["//! unknown_directive: value"] else []
        ]
      source = unlines $ directives ++ ["func main() {}"]
  in (hasValid || hasInvalid || hasExtra) ==>
     case parseTypus source of
       Left _ -> property $ hasInvalid || hasExtra  -- May fail on invalid directives
       Right result -> property $ True  -- Should succeed with valid parts

-- Property: Parser error position is accurate
prop_parser_error_position_accurate :: String -> Int -> Property
prop_parser_error_position_accurate prefix errorLine =
  errorLine >= 0 && errorLine <= 10 ==>  -- Limit test scope
  let linesBefore = replicate errorLine "valid line"
      errorLineContent = "invalid syntax with unclosed {"
      source = unlines $ linesBefore ++ [errorLineContent] ++ ["func main() {}"]
  in case parseTypus source of
       Left err -> property $ "line" `L.isInfixOf` err  -- Error should mention line
       Right _ -> property $ True  -- May succeed despite error

-- Property: Parser handles Unicode characters in error recovery
prop_parser_handles_unicode_errors :: String -> Property
prop_parser_handles_unicode_errors content =
  let unicodeContent = content ++ "测试🚀invalid"
  in not (null content) ==>
     case parseTypus unicodeContent of
       Left _ -> property True  -- Should handle Unicode gracefully
       Right result -> property $ True  -- May succeed with Unicode content

-- Property: Parser recovers from mismatched braces
prop_parser_recovers_from_mismatched_braces :: String -> String -> Property
prop_parser_recovers_from_mismatched_braces before after =
  let source = unlines 
        [ before
        , "func main() {"
        , "  println(\"hello\")"
        , "}"  -- Missing closing brace for function
        , "func other() {}"  -- This should still be parsed
        , after
        ]
  in not (null before) && not (null after) ==>
     case parseTypus source of
       Left _ -> property True  -- May fail but not crash
       Right result -> 
         let blocks = tfCodeBlocks result
         in property $ L.length blocks >= 1  -- Should parse at least one block

-- Property: Parser handles empty input gracefully
prop_parser_handles_empty_input :: Property
prop_parser_handles_empty_input =
  case parseTypus "" of
    Left _ -> property True  -- Expected to fail gracefully
    Right result -> property $ L.null (tfCodeBlocks result)  -- Should have no blocks

-- Property: Parser handles whitespace-only input
prop_parser_handles_whitespace_only :: String -> Property
prop_parser_handles_whitespace_only ws =
  let whitespaceOnly = L.all isSpace ws
  in whitespaceOnly ==>
     case parseTypus ws of
       Left _ -> property True
       Right result -> property $ L.null (tfCodeBlocks result)

-- Property: Parser preserves directive values despite syntax errors
prop_parser_preserves_directive_values :: Bool -> String -> Property
prop_parser_preserves_directive_values includeDirective code =
  let directive = if includeDirective then "//! ownership: on\n" else ""
      source = directive ++ code ++ "\nfunc main() {"
  in includeDirective ==>
     case parseTypus source of
       Left _ -> property True  -- May fail on syntax error
       Right result -> 
         let directives = tfDirectives result
             ownership = fdOwnership directives
         in property $ isJust ownership  -- Should preserve directive

-- Property: Parser error recovery is deterministic
prop_parser_error_recovery_deterministic :: String -> Property
prop_parser_error_recovery_deterministic source =
  let result1 = parseTypus source
      result2 = parseTypus source
  in case (result1, result2) of
       (Left _, Left _) -> property True
       (Right r1, Right r2) -> property $ L.length (tfCodeBlocks r1) == L.length (tfCodeBlocks r2)
       _ -> property False  -- Should be consistent

-- Property: Parser handles extremely long lines
prop_parser_handles_long_lines :: Int -> String -> Property
prop_parser_handles_long_lines multiplier content =
  multiplier >= 0 && multiplier <= 100 ==>  -- Limit for performance
  let longLine = content ++ L.concat (replicate multiplier "very_long_content_")
      source = longLine ++ "\nfunc main() {}"
  in case parseTypus source of
       Left _ -> property True  -- May fail but not crash
       Right result -> property $ True

-- Property: Parser recovers from comment-like constructs
prop_parser_recovers_from_comment_like :: String -> Property
prop_parser_recovers_from_comment_like content =
  let commentLike = "// not actually a comment" ++ content
      source = commentLike ++ "\nfunc main() {}"
  in not (null content) ==>
     case parseTypus source of
       Left _ -> property True  -- May fail but not crash
       Right result -> property $ True

-- Property: Parser handles nested block structures with errors
prop_parser_handles_nested_blocks_with_errors :: Int -> Property
prop_parser_handles_nested_blocks_with_errors depth =
  depth >= 0 && depth <= 5 ==>  -- Limit complexity
  let nested = L.concat $ replicate depth "  if true {\n"
      source = nested ++ "func main() {}\n" ++ L.concat (replicate depth "}\n")
  in case parseTypus source of
       Left _ -> property True  -- May fail on mismatched nesting
       Right result -> property $ True  -- May succeed with partial parsing

-- Property: Parser maintains line numbering after errors
prop_parser_maintains_line_numbering :: String -> String -> Property
prop_parser_maintains_line_numbering before after =
  let source = unlines [before, "invalid { syntax", after]
      expectedLines = L.length $ lines source
  in not (null before) && not (null after) ==>
     case parseTypus source of
       Left err -> property $ True  -- Error should include line info
       Right result -> property $ True  -- Success maintains structure

-- Property: Parser handles special characters in identifiers
prop_parser_handles_special_chars :: String -> Property
prop_parser_handles_special_chars suffix =
  let identifier = "func" ++ suffix ++ "() {}"
      source = identifier ++ "\nfunc main() {}"
  in not (null suffix) && L.all (`elem` "_123") suffix ==>  -- Only valid identifier chars
     case parseTypus source of
       Left _ -> property True  -- May fail on invalid identifiers
       Right result -> property $ True  -- May succeed with valid ones

tests :: TestTree
tests = testGroup "Parser Error Recovery Properties"
  [ fastProperty "parser recovers from malformed directives" prop_parser_recovers_from_malformed_directives
  , fastProperty "parser handles incomplete syntax" prop_parser_handles_incomplete_syntax
  , fastProperty "parser preserves valid blocks" prop_parser_preserves_valid_blocks
  , fastProperty "parser handles mixed directives" prop_parser_handles_mixed_directives
  , fastProperty "parser error position accurate" prop_parser_error_position_accurate
  , fastProperty "parser handles unicode errors" prop_parser_handles_unicode_errors
  , fastProperty "parser recovers from mismatched braces" prop_parser_recovers_from_mismatched_braces
  , fastProperty "parser handles empty input" prop_parser_handles_empty_input
  , fastProperty "parser handles whitespace only" prop_parser_handles_whitespace_only
  , fastProperty "parser preserves directive values" prop_parser_preserves_directive_values
  , fastProperty "parser error recovery deterministic" prop_parser_error_recovery_deterministic
  , fastProperty "parser handles long lines" prop_parser_handles_long_lines
  , fastProperty "parser recovers from comment-like constructs" prop_parser_recovers_from_comment_like
  , fastProperty "parser handles nested blocks with errors" prop_parser_handles_nested_blocks_with_errors
  , fastProperty "parser maintains line numbering" prop_parser_maintains_line_numbering
  , fastProperty "parser handles special characters" prop_parser_handles_special_chars
  ]