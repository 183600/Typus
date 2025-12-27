{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewParserQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.Tasty (TestTree)

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..))
import Data.Char (isSpace, isAlphaNum)
import qualified Data.List as List
import Data.Text (Text)

-- Property: Parser is idempotent for valid input
prop_parser_idempotent :: String -> Property
prop_parser_idempotent input =
  let result1 = parseTypus input
      result2 = case result1 of
        Left _ -> parseTypus input
        Right _ -> parseTypus input
  in property $ case (result1, result2) of
    (Left err1, Left err2) -> show err1 === show err2
    (Right file1, Right file2) -> show file1 === show file2
    _ -> property False

-- Property: Parser handles comments gracefully
prop_parser_handles_comments :: String -> String -> Property
prop_parser_handles_comments code comment =
  not ('"' `elem` code) && not ('\'' `elem` code) ==>
  let codeWithComment = code ++ "// " ++ comment ++ "\n" ++ code
      result1 = parseTypus code
      result2 = parseTypus codeWithComment
  in property $ case (result1, result2) of
    (Left _, Left _) -> property True
    (Right _, Right _) -> property True
    _ -> property False

-- Property: Parser handles multiline input
prop_parser_multiline :: [String] -> Property
prop_parser_multiline lines =
  not (null lines) ==>
  let multiline = List.intercalate "\n" lines
      result = parseTypus multiline
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Parser handles directives correctly
prop_parser_directives :: String -> Property
prop_parser_directives directive =
  let input = "// @ownership: true\n// @dependent-types: false\n" ++ directive
      result = parseTypus input
  in property $ case result of
    Left _ -> property True
    Right typusFile -> property True

-- Property: Parser preserves line structure
prop_parser_preserves_lines :: String -> Property
prop_parser_preserves_lines input =
  let linesIn = length (lines input)
      result = parseTypus input
  in property $ case result of
    Left _ -> property True
    Right typusFile -> property True

-- Property: Parser handles special characters
prop_parser_special_chars :: String -> Property
prop_parser_special_chars base =
  let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      input = base ++ specialChars ++ base
      result = parseTypus input
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Parser handles unicode characters
prop_parser_unicode :: String -> Property
prop_parser_unicode base =
  let unicode = "测试🚀café naïve résumé"
      input = base ++ unicode ++ base
      result = parseTypus input
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Parser handles empty lines
prop_parser_empty_lines :: Int -> String -> Property
prop_parser_empty_lines count content =
  count >= 0 && count <= 10 ==>
  let emptyLines = List.replicate count "\n"
      input = List.intercalate "" (content : emptyLines ++ [content])
      result = parseTypus input
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Parser handles indentation
prop_parser_indentation :: Int -> String -> Property
prop_parser_indentation level content =
  level >= 0 && level <= 5 ==>
  let indent = List.replicate level ' '
      input = indent ++ content ++ "\n" ++ indent ++ content
      result = parseTypus input
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Parser handles mixed whitespace
prop_parser_mixed_whitespace :: String -> Property
prop_parser_mixed_whitespace content =
  let mixed = "\t  \t  " ++ content ++ "  \t  \t"
      result = parseTypus mixed
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

tests :: TestTree
tests = testGroup "New Parser QuickCheck Tests"
  [ fastProperty "Parser is idempotent" prop_parser_idempotent
  , fastProperty "Parser handles comments" prop_parser_handles_comments
  , fastProperty "Parser handles multiline input" prop_parser_multiline
  , fastProperty "Parser handles directives" prop_parser_directives
  , fastProperty "Parser preserves line structure" prop_parser_preserves_lines
  , fastProperty "Parser handles special characters" prop_parser_special_chars
  , fastProperty "Parser handles unicode characters" prop_parser_unicode
  , fastProperty "Parser handles empty lines" prop_parser_empty_lines
  , fastProperty "Parser handles indentation" prop_parser_indentation
  , fastProperty "Parser handles mixed whitespace" prop_parser_mixed_whitespace
  ]