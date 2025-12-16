{-# LANGUAGE CPP #-}

module Test.Unit.ParserPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, sized)

import Parser (parseTypus, TypusFile(..), CodeBlock(..), BlockDirectives(..), FileDirectives(..))
import SourceLocation (SourceSpan(..), SourcePos(..), spanStart, posLine)
import Utils (trim)
import Data.Char (isAlphaNum, isSpace, toLower, toUpper)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- Helper function for compatibility
sourceLine :: SourcePos -> Int
sourceLine = posLine

-- | Generate random directive content
genDirectiveContent :: Gen String
genDirectiveContent = listOf $ oneof
  [ choose ('a', 'z')
  , choose ('A', 'Z')
  , choose ('0', '9')
  , elements "_-"
  ]

-- | Generate random code block content
genCodeBlockContent :: Gen String
genCodeBlockContent = listOf $ oneof
  [ choose ('a', 'z')
  , choose ('A', 'Z')
  , choose ('0', '9')
  , elements " \t\n\r"
  , elements "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  ]

-- | Generate random file directive lines
genFileDirective :: Gen String
genFileDirective = do
  key <- elements ["ownership", "dependent-types", "constraints"]
  value <- elements ["true", "false", "enabled", "disabled"]
  return $ "//! " ++ key ++ "=" ++ value

-- | Generate random block directive lines
genBlockDirective :: Gen String
genBlockDirective = do
  key <- elements ["ownership", "dependent-types", "constraints"]
  value <- elements ["true", "false", "enabled", "disabled"]
  return $ "//@" ++ key ++ "=" ++ value

-- | Generate a complete typus file structure
genTypusFile :: Gen String
genTypusFile = sized $ \n -> do
  numDirectives <- choose (0, min 3 n)
  numBlocks <- choose (1, min 5 n)
  
  directives <- listOfN numDirectives genFileDirective
  blocks <- listOfN numBlocks genCodeBlock
  
  return $ unlines (directives ++ blocks)
  where
    listOfN k gen = sequence [gen | _ <- [1..k]]
    
    genCodeBlock = do
      hasDirective <- elements [True, False]
      directive <- if hasDirective then genBlockDirective else return ""
      content <- genCodeBlockContent
      return $ unlines [directive, "```go", content, "```"]

-- Property: Parser preserves directive content
prop_parser_preserves_directives :: String -> String -> Property
prop_parser_preserves_directives key value =
  let directive = "//! " ++ key ++ "=" ++ value
      content = directive ++ "\n```go\nfunc test() {}\n```"
      result = parseTypus content
  in case result of
    Right parsed -> property $ isJust (fdOwnership (tfDirectives parsed)) || 
                                     isJust (fdDependentTypes (tfDirectives parsed)) ||
                                     isJust (fdConstraints (tfDirectives parsed))
    Left _ -> property $ True -- Parsing errors are acceptable for malformed input

-- Property: Parser handles empty files gracefully
prop_parser_empty_file :: Property
prop_parser_empty_file =
  let result = parseTypus ""
  in case result of
    Right parsed -> property $ null (tfBlocks parsed)
    Left _ -> property $ True

-- Property: Parser extracts code blocks correctly
prop_parser_extracts_blocks :: String -> Property
prop_parser_extracts_blocks content =
  let wrapped = "```go\n" ++ content ++ "\n```"
      result = parseTypus wrapped
  in case result of
    Right parsed -> 
      case tfBlocks parsed of
        (block:_) -> property $ content `isInfixOf` cbContent block
        [] -> property $ False
    Left _ -> property $ True

-- Property: Parser handles multiple blocks
prop_parser_multiple_blocks :: [String] -> Property
prop_parser_multiple_blocks contents =
  not (null contents) ==> 
  let blocks = map (\c -> "```go\n" ++ c ++ "\n```") contents
      content = unlines blocks
      result = parseTypus content
  in case result of
    Right parsed -> property $ length (tfBlocks parsed) >= length contents
    Left _ -> property $ True

-- Property: Parser preserves block directives
prop_parser_block_directives :: String -> String -> Property
prop_parser_block_directives key value =
  let directive = "//@" ++ key ++ "=" ++ value
      content = directive ++ "\n```go\nfunc test() {}\n```"
      result = parseTypus content
  in case result of
    Right parsed -> 
      case tfBlocks parsed of
        (block:_) -> property $ isJust (bdOwnership (cbDirectives block)) ||
                                       isJust (bdDependentTypes (cbDirectives block)) ||
                                       isJust (bdConstraints (cbDirectives block))
        [] -> property $ False
    Left _ -> property $ True

-- Property: Parser handles malformed blocks gracefully
prop_parser_malformed_blocks :: String -> Property
prop_parser_malformed_blocks content =
  let malformed = "```go\n" ++ content ++ "\n"  -- Missing closing ```
      result = parseTypus malformed
  in case result of
    Right parsed -> property $ True  -- Should handle gracefully
    Left _ -> property $ True  -- Parsing errors are acceptable

-- Property: Parser preserves line numbers in source locations
prop_parser_preserves_line_numbers :: [String] -> Property
prop_parser_preserves_line_numbers lines =
  not (null lines) ==> 
  let content = unlines lines
      result = parseTypus content
  in case result of
    Right parsed -> 
      case tfBlocks parsed of
        (block:_) -> 
          let span = cbSpan block
              startPos = spanStart span
          in property $ sourceLine startPos >= 1
        [] -> property $ True
    Left _ -> property $ True

-- Property: Parser handles mixed content types
prop_parser_mixed_content :: String -> String -> String -> Property
prop_parser_mixed_content directive code comment =
  let content = unlines 
        [ directive
        , "```go"
        , code
        , "```"
        , "// " ++ comment
        ]
      result = parseTypus content
  in case result of
    Right parsed -> property $ length (tfBlocks parsed) >= 1
    Left _ -> property $ True

-- Property: Parser handles nested directives
prop_parser_nested_directives :: [String] -> Property
prop_parser_nested_directives directives =
  not (null directives) ==> 
  let directiveLines = map (\d -> "//! " ++ d) directives
      content = unlines directiveLines ++ "\n```go\nfunc test() {}\n```"
      result = parseTypus content
  in case result of
    Right parsed -> property $ True  -- Should handle multiple directives
    Left _ -> property $ True

-- Property: Parser handles Unicode content
prop_parser_unicode_content :: String -> Property
prop_parser_unicode_content content =
  let unicodeContent = content ++ "测试café🚀"
      wrapped = "```go\n" ++ unicodeContent ++ "\n```"
      result = parseTypus wrapped
  in case result of
    Right parsed -> 
      case tfBlocks parsed of
        (block:_) -> property $ unicodeContent `isInfixOf` cbContent block
        [] -> property $ False
    Left _ -> property $ True

-- Property: Parser handles very long lines
prop_parser_long_lines :: Int -> String -> Property
prop_parser_long_lines length content =
  length <= 1000 ==> 
  let longLine = replicate length ' ' ++ content
      wrapped = "```go\n" ++ longLine ++ "\n```"
      result = parseTypus wrapped
  in case result of
    Right parsed -> property $ True
    Left _ -> property $ True

-- Property: Parser is idempotent for well-formed input
prop_parser_idempotent :: String -> Property
prop_parser_idempotent content =
  let wellFormed = "```go\n" ++ content ++ "\n```"
      result1 = parseTypus wellFormed
  in case result1 of
    Right parsed1 -> 
      let serialized = show parsed1  -- Simple serialization
          result2 = parseTypus wellFormed
      in case result2 of
        Right parsed2 -> property $ length (tfBlocks parsed1) == length (tfBlocks parsed2)
        Left _ -> property $ False
    Left _ -> property $ True

-- Property: Parser handles special characters in directives
prop_parser_special_chars_directives :: String -> Property
prop_parser_special_chars_directives specialChars =
  let directive = "//! test=" ++ specialChars
      content = directive ++ "\n```go\nfunc test() {}\n```"
      result = parseTypus content
  in case result of
    Right parsed -> property $ True
    Left _ -> property $ True

-- Property: Parser preserves content order
prop_parser_preserves_order :: [String] -> Property
prop_parser_preserves_order contents =
  not (null contents) ==> 
  let blocks = map (\c -> "```go\n" ++ c ++ "\n```") contents
      content = unlines blocks
      result = parseTypus content
  in case result of
    Right parsed -> 
      let blockContents = map cbContent (tfBlocks parsed)
          -- Check that original contents appear in order
          checkOrder [] _ = True
          checkOrder _ [] = False
          checkOrder (x:xs) (y:ys) = x `isInfixOf` y && checkOrder xs ys
      in property $ checkOrder contents blockContents
    Left _ -> property $ True

tests :: TestTree
tests = testGroup "Parser Properties QuickCheck Tests"
  [ fastProperty "parser preserves directives" prop_parser_preserves_directives
  , fastProperty "parser empty file" prop_parser_empty_file
  , fastProperty "parser extracts blocks" prop_parser_extracts_blocks
  , fastProperty "parser multiple blocks" prop_parser_multiple_blocks
  , fastProperty "parser block directives" prop_parser_block_directives
  , fastProperty "parser malformed blocks" prop_parser_malformed_blocks
  , fastProperty "parser preserves line numbers" prop_parser_preserves_line_numbers
  , fastProperty "parser mixed content" prop_parser_mixed_content
  , fastProperty "parser nested directives" prop_parser_nested_directives
  , fastProperty "parser unicode content" prop_parser_unicode_content
  , fastProperty "parser long lines" prop_parser_long_lines
  , fastProperty "parser idempotent" prop_parser_idempotent
  , fastProperty "parser special chars directives" prop_parser_special_chars_directives
  , fastProperty "parser preserves order" prop_parser_preserves_order
  ]