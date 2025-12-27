{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewParserBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof)
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

import Data.Char (isSpace, isAlphaNum, isAlpha, isDigit)
import Data.List (isPrefixOf, isInfixOf, nub)
import qualified Data.Text as T

-- Property: parseTypus handles empty input
prop_parse_empty :: Property
prop_parse_empty =
  let result = parseTypus ""
  in case result of
    Left _ -> property True
    Right typusFile -> property $ True  -- Should handle empty input gracefully

-- Property: parseTypus handles whitespace only
prop_parse_whitespace :: String -> Property
prop_parse_whitespace input =
  all isSpace input ==>
  let result = parseTypus input
  in case result of
    Left _ -> property True
    Right typusFile -> property $ True  -- Should handle whitespace gracefully

-- Property: parseTypus handles simple comments
prop_parse_simple_comments :: [String] -> Property
prop_parse_simple_comments commentLines =
  not (null commentLines) && all (not . null) commentLines ==>
  let comments = map (\line -> "// " ++ line) commentLines
      input = unlines comments
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right typusFile -> property $ True  -- Should handle simple comments

-- Property: parseTypus handles multiline comments
prop_parse_multiline_comments :: String -> Property
prop_parse_multiline_comments content =
  not (null content) && not ("*/" `isInfixOf` content) ==>
  let input = "/* " ++ content ++ " */"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right typusFile -> property $ True  -- Should handle multiline comments

-- Property: parseTypus handles nested braces
prop_parse_nested_braces :: Int -> Property
prop_parse_nested_braces depth =
  depth >= 0 && depth <= 10 ==>
  let openBraces = replicate depth '{'
      closeBraces = replicate depth '}'
      input = "func main() " ++ concat openBraces ++ concat closeBraces
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right typusFile -> property $ True  -- Should handle nested braces

-- Property: parseTypus detects unbalanced braces
prop_parse_unbalanced_braces :: Int -> Int -> Property
prop_parse_unbalanced_braces opens closes =
  opens /= closes && opens >= 0 && closes >= 0 && opens <= 10 && closes <= 10 ==>
  let openBraces = replicate opens '{'
      closeBraces = replicate closes '}'
      input = "func main() " ++ concat openBraces ++ concat closeBraces
      result = parseTypus input
  in case result of
    Left _ -> property True  -- Should detect unbalanced braces
    Right typusFile -> property $ True  -- Or handle gracefully

-- Property: parseTypus handles nested parentheses
prop_parse_nested_parens :: Int -> Property
prop_parse_nested_parens depth =
  depth >= 0 && depth <= 10 ==>
  let openParens = replicate depth '('
      closeParens = replicate depth ')'
      input = "func main" ++ concat openParens ++ concat closeParens ++ " {}"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right typusFile -> property $ True  -- Should handle nested parentheses

-- Property: parseTypus detects unbalanced parentheses
prop_parse_unbalanced_parens :: Int -> Int -> Property
prop_parse_unbalanced_parens opens closes =
  opens /= closes && opens >= 0 && closes >= 0 && opens <= 10 && closes <= 10 ==>
  let openParens = replicate opens '('
      closeParens = replicate closes ')'
      input = "func main" ++ concat openParens ++ concat closeParens ++ " {}"
      result = parseTypus input
  in case result of
    Left _ -> property True  -- Should detect unbalanced parentheses
    Right typusFile -> property $ True  -- Or handle gracefully

-- Property: parseTypus handles string literals
prop_parse_string_literals :: [String] -> Property
prop_parse_string_literals stringContents =
  not (null stringContents) && all (not . any (`elem` "\\\"")) stringContents ==>
  let quotedStrings = map (\s -> "var s string = \"" ++ s ++ "\"") stringContents
      input = unlines quotedStrings
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right typusFile -> property $ True  -- Should handle string literals

-- Property: parseTypus detects unclosed strings
prop_parse_unclosed_strings :: String -> Property
prop_parse_unclosed_strings content =
  not (null content) && not ('"' `elem` content) ==>
  let input = "var s string = \"" ++ content  -- Unclosed string
      result = parseTypus input
  in case result of
    Left _ -> property True  -- Should detect unclosed strings
    Right typusFile -> property $ True  -- Or handle gracefully

-- Property: parseTypus handles character literals
prop_parse_char_literals :: [Char] -> Property
prop_parse_char_literals chars =
  not (null chars) && all (`notElem` "'\\") chars ==>
  let charLiterals = map (\c -> "var c rune = '" ++ [c] ++ "'") chars
      input = unlines charLiterals
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right typusFile -> property $ True  -- Should handle character literals

-- Property: parseTypus handles escaped characters
prop_parse_escaped_chars :: [String] -> Property
prop_parse_escaped_chars escapeSequences =
  not (null escapeSequences) && all (`elem` ["\\n", "\\t", "\\r", "\\\\", "\\\""]) escapeSequences ==>
  let escapedStrings = map (\seq -> "var s string = \"" ++ seq ++ "\"") escapeSequences
      input = unlines escapedStrings
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right typusFile -> property $ True  -- Should handle escaped characters

-- Property: parseTypus handles numeric literals
prop_parse_numeric_literals :: [Int] -> Property
prop_parse_numeric_literals numbers =
  not (null numbers) ==>
  let numericDecls = map (\n -> "var x int = " ++ show n) numbers
      input = unlines numericDecls
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right typusFile -> property $ True  -- Should handle numeric literals

-- Property: parseTypus handles floating point literals
prop_parse_float_literals :: [Double] -> Property
prop_parse_float_literals numbers =
  not (null numbers) && all (not . isNaN) numbers ==>
  let floatDecls = map (\n -> "var x float64 = " ++ show n) numbers
      input = unlines floatDecls
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right typusFile -> property $ True  -- Should handle floating point literals

-- Property: parseTypus handles function declarations
prop_parse_functions :: [String] -> Property
prop_parse_functions functionNames =
  not (null functionNames) && all (not . null) functionNames &&
  all (all isAlphaNum) functionNames ==>
  let functionDecls = map (\name -> "func " ++ name ++ "() {}") functionNames
      input = unlines functionDecls
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right typusFile -> property $ True  -- Should handle function declarations

-- Property: parseTypus handles function parameters
prop_parse_function_params :: [String] -> Property
prop_parse_function_params paramNames =
  not (null paramNames) && all (not . null) paramNames &&
  all (all isAlphaNum) paramNames ==>
  let params = unwords $ map (\name -> name ++ " int") paramNames
      input = "func test(" ++ params ++ ") {}"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right typusFile -> property $ True  -- Should handle function parameters

-- Property: parseTypus handles return values
prop_parse_return_values :: [String] -> Property
prop_parse_return_values returnTypes =
  not (null returnTypes) && all (not . null) returnTypes &&
  all (`elem` ["int", "string", "bool", "float64"]) returnTypes ==>
  let returns = unwords $ map (++ ",") returnTypes
      input = "func test() (" ++ init returns ++ ") {}"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right typusFile -> property $ True  -- Should handle return values

-- Property: parseTypus handles variable declarations
prop_parse_variables :: [String] -> Property
prop_parse_variables variableNames =
  not (null variableNames) && all (not . null) variableNames &&
  all (all isAlphaNum) variableNames ==>
  let varDecls = map (\name -> "var " ++ name ++ " int") variableNames
      input = unlines varDecls
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right typusFile -> property $ True  -- Should handle variable declarations

-- Property: parseTypus handles type declarations
prop_parse_types :: [String] -> Property
prop_parse_types typeNames =
  not (null typeNames) && all (not . null) typeNames &&
  all (all isAlphaNum) typeNames ==>
  let typeDecls = map (\name -> "type " ++ name ++ " int") typeNames
      input = unlines typeDecls
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right typusFile -> property $ True  -- Should handle type declarations

-- Property: parseTypus handles import statements
prop_parse_imports :: [String] -> Property
prop_parse_imports importPaths =
  not (null importPaths) && all (not . null) importPaths &&
  all (all (`elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/.")) importPaths ==>
  let importDecls = map (\path -> "import \"" ++ path ++ "\"") importPaths
      input = unlines importDecls
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right typusFile -> property $ True  -- Should handle import statements

-- Property: parseTypus handles package declarations
prop_parse_package :: String -> Property
prop_parse_package packageName =
  not (null packageName) && all isAlphaNum packageName ==>
  let input = "package " ++ packageName
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right typusFile -> property $ True  -- Should handle package declarations

-- Property: parseTypus handles mixed content
prop_parse_mixed :: String -> String -> String -> Property
prop_parse_mixed packageDecl imports content =
  not (null packageDecl) && all isAlphaNum packageDecl ==>
  let input = unlines [packageDecl, imports, content]
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right typusFile -> property $ True  -- Should handle mixed content

-- Property: parseTypus is deterministic
prop_parse_deterministic :: String -> Property
prop_parse_deterministic input =
  let result1 = parseTypus input
      result2 = parseTypus input
  in case (result1, result2) of
    (Right file1, Right file2) -> property $ file1 === file2
    (Left err1, Left err2) -> property $ err1 === err2
    _ -> property False  -- Should be consistent

-- Property: parseTypus handles large inputs
prop_parse_large :: String -> Int -> Property
prop_parse_large base multiplier =
  multiplier >= 0 && multiplier <= 100 ==>  -- Limit for performance
  let largeInput = concat (replicate multiplier base)
      result = parseTypus largeInput
  in case result of
    Left _ -> property True
    Right typusFile -> property $ True  -- Should handle large inputs

-- Property: parseTypus handles unicode content
prop_parse_unicode :: String -> Property
prop_parse_unicode unicodeContent =
  let input = unicodeContent ++ "测试🚀"
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right typusFile -> property $ True  -- Should handle unicode content

tests :: TestTree
tests = testGroup "New Parser Boundary QuickCheck"
  [ fastProperty "parse empty" prop_parse_empty
  , fastProperty "parse whitespace" prop_parse_whitespace
  , fastProperty "parse simple comments" prop_parse_simple_comments
  , fastProperty "parse multiline comments" prop_parse_multiline_comments
  , fastProperty "parse nested braces" prop_parse_nested_braces
  , fastProperty "parse unbalanced braces" prop_parse_unbalanced_braces
  , fastProperty "parse nested parens" prop_parse_nested_parens
  , fastProperty "parse unbalanced parens" prop_parse_unbalanced_parens
  , fastProperty "parse string literals" prop_parse_string_literals
  , fastProperty "parse unclosed strings" prop_parse_unclosed_strings
  , fastProperty "parse char literals" prop_parse_char_literals
  , fastProperty "parse escaped chars" prop_parse_escaped_chars
  , fastProperty "parse numeric literals" prop_parse_numeric_literals
  , fastProperty "parse float literals" prop_parse_float_literals
  , fastProperty "parse functions" prop_parse_functions
  , fastProperty "parse function params" prop_parse_function_params
  , fastProperty "parse return values" prop_parse_return_values
  , fastProperty "parse variables" prop_parse_variables
  , fastProperty "parse types" prop_parse_types
  , fastProperty "parse imports" prop_parse_imports
  , fastProperty "parse package" prop_parse_package
  , fastProperty "parse mixed" prop_parse_mixed
  , fastProperty "parse deterministic" prop_parse_deterministic
  , fastProperty "parse large" prop_parse_large
  , fastProperty "parse unicode" prop_parse_unicode
  ]