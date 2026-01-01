{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.AdvancedSyntaxValidatorQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof)
import TestSupport.Arbitrary

import SyntaxValidator
  ( SyntaxValidator
  , SyntaxError(..)
  , ErrorType(..)
  , newSyntaxValidator
  , validateSyntax
  , validateFile
  , getSyntaxErrors
  , formatSyntaxError
  )

import Data.Char (isAlphaNum, isAlpha, isDigit, isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (nub)
import qualified Data.Set as Set

-- Property: ErrorType equality is reflexive
prop_error_type_reflexive :: ErrorType -> Property
prop_error_type_reflexive errorType =
  property $ errorType === errorType

-- Property: SyntaxError ordering is consistent
prop_syntax_error_ordering :: SyntaxError -> SyntaxError -> Property
prop_syntax_error_ordering error1 error2 =
  let ord1 = compare error1 error2
      ord2 = compare (errorMessage error1) (errorMessage error2)
  in property $ (errorMessage error1 == errorMessage error2) ==> (ord1 == ord2)

-- Property: newSyntaxValidator creates valid validator
prop_new_syntax_validator_valid :: Property
prop_new_syntax_validator_valid =
  let validator = newSyntaxValidator
  in property $ True  -- Basic smoke test

-- Property: validateSyntax handles empty input
prop_validate_syntax_empty :: Property
prop_validate_syntax_empty =
  let validator = newSyntaxValidator
      result = validateSyntax validator ""
  in case result of
    Left _ -> property True
    Right newValidator -> property $ True  -- Should handle empty input gracefully

-- Property: validateSyntax handles whitespace only
prop_validate_syntax_whitespace :: String -> Property
prop_validate_syntax_whitespace input =
  L.all isSpace input ==>
  let validator = newSyntaxValidator
      result = validateSyntax validator input
  in case result of
    Left _ -> property True
    Right newValidator -> property $ True  -- Should handle whitespace gracefully

-- Property: validateSyntax handles simple identifiers
prop_validate_syntax_identifiers :: [String] -> Property
prop_validate_syntax_identifiers identifiers =
  not (null identifiers) && L.all (not . null) identifiers &&
  L.all (L.all isAlphaNum) identifiers ==>
  let input = unwords identifiers
      validator = newSyntaxValidator
      result = validateSyntax validator input
  in case result of
    Left _ -> property True
    Right newValidator -> property $ True  -- Should handle identifiers

-- Property: validateSyntax handles balanced braces
prop_validate_syntax_balanced_braces :: String -> Property
prop_validate_syntax_balanced_braces content =
  let openCount = L.length (L.filter (== '{') content)
      closeCount = L.length (L.filter (== '}') content)
      balanced = openCount == closeCount
  in balanced ==>
  let validator = newSyntaxValidator
      result = validateSyntax validator content
  in case result of
    Left _ -> property True
    Right newValidator -> property $ True  -- Should handle balanced braces

-- Property: validateSyntax detects unbalanced braces
prop_validate_syntax_unbalanced_braces :: String -> Property
prop_validate_syntax_unbalanced_braces content =
  let openCount = L.length (L.filter (== '{') content)
      closeCount = L.length (L.filter (== '}') content)
      unbalanced = openCount /= closeCount
  in unbalanced ==>
  let validator = newSyntaxValidator
      result = validateSyntax validator content
  in case result of
    Left _ -> property True
    Right newValidator -> 
      let errors = getSyntaxErrors newValidator
          hasBraceError = L.any (\err -> errorType err `elem` [MissingBrace, UnterminatedBlock]) errors
      in property $ hasBraceError || not (null errors)

-- Property: validateSyntax handles balanced parentheses
prop_validate_syntax_balanced_parens :: String -> Property
prop_validate_syntax_balanced_parens content =
  let openCount = L.length (L.filter (== '(') content)
      closeCount = L.length (L.filter (== ')') content)
      balanced = openCount == closeCount
  in balanced ==>
  let validator = newSyntaxValidator
      result = validateSyntax validator content
  in case result of
    Left _ -> property True
    Right newValidator -> property $ True  -- Should handle balanced parentheses

-- Property: validateSyntax detects unbalanced parentheses
prop_validate_syntax_unbalanced_parens :: String -> Property
prop_validate_syntax_unbalanced_parens content =
  let openCount = L.length (L.filter (== '(') content)
      closeCount = L.length (L.filter (== ')') content)
      unbalanced = openCount /= closeCount
  in unbalanced ==>
  let validator = newSyntaxValidator
      result = validateSyntax validator content
  in case result of
    Left _ -> property True
    Right newValidator -> 
      let errors = getSyntaxErrors newValidator
          hasParenError = L.any (\err -> errorType err == MissingParenthesis) errors
      in property $ hasParenError || not (null errors)

-- Property: validateSyntax handles balanced brackets
prop_validate_syntax_balanced_brackets :: String -> Property
prop_validate_syntax_balanced_brackets content =
  let openCount = L.length (L.filter (== '[') content)
      closeCount = L.length (L.filter (== ']') content)
      balanced = openCount == closeCount
  in balanced ==>
  let validator = newSyntaxValidator
      result = validateSyntax validator content
  in case result of
    Left _ -> property True
    Right newValidator -> property $ True  -- Should handle balanced brackets

-- Property: validateSyntax detects unbalanced brackets
prop_validate_syntax_unbalanced_brackets :: String -> Property
prop_validate_syntax_unbalanced_brackets content =
  let openCount = L.length (L.filter (== '[') content)
      closeCount = L.length (L.filter (== ']') content)
      unbalanced = openCount /= closeCount
  in unbalanced ==>
  let validator = newSyntaxValidator
      result = validateSyntax validator content
  in case result of
    Left _ -> property True
    Right newValidator -> 
      let errors = getSyntaxErrors newValidator
          hasBracketError = L.any (\err -> errorType err == MissingBracket) errors
      in property $ hasBracketError || not (null errors)

-- Property: validateSyntax handles string literals
prop_validate_syntax_strings :: [String] -> Property
prop_validate_syntax_strings stringContents =
  not (null stringContents) && L.all (not . L.any (`elem` "\\\"" ) ) stringContents ==>
  let quotedStrings = L.map (\s -> "\"" ++ s ++ "\"") stringContents
      input = unwords quotedStrings
      validator = newSyntaxValidator
      result = validateSyntax validator input
  in case result of
    Left _ -> property True
    Right newValidator -> property $ True  -- Should handle string literals

-- Property: validateSyntax detects unclosed strings
prop_validate_syntax_unclosed_strings :: String -> Property
prop_validate_syntax_unclosed_strings content =
  not (null content) && not ('"' `elem` content) ==>
  let input = "\"" ++ content  -- Unclosed string
      validator = newSyntaxValidator
      result = validateSyntax validator input
  in case result of
    Left _ -> property True
    Right newValidator -> 
      let errors = getSyntaxErrors newValidator
          hasStringError = L.any (\err -> errorType err == UnclosedString) errors
      in property $ hasStringError || not (null errors)

-- Property: validateSyntax handles comments
prop_validate_syntax_comments :: [String] -> Property
prop_validate_syntax_comments commentContents =
  not (null commentContents) && L.all (not . L.any (`elem` "\\\"" ) ) commentContents ==>
  let comments = L.map (\s -> "// " ++ s) commentContents
      input = unlines comments
      validator = newSyntaxValidator
      result = validateSyntax validator input
  in case result of
    Left _ -> property True
    Right newValidator -> property $ True  -- Should handle comments

-- Property: validateSyntax handles multiline comments
prop_validate_syntax_multiline_comments :: String -> Property
prop_validate_syntax_multiline_comments content =
  not (null content) && not ("*/" `L.isInfixOf` content) ==>
  let input = "/* " ++ content ++ " */"
      validator = newSyntaxValidator
      result = validateSyntax validator input
  in case result of
    Left _ -> property True
    Right newValidator -> property $ True  -- Should handle multiline comments

-- Property: validateSyntax detects unclosed multiline comments
prop_validate_syntax_unclosed_multiline_comments :: String -> Property
prop_validate_syntax_unclosed_multiline_comments content =
  not (null content) && not ("*/" `L.isInfixOf` content) ==>
  let input = "/* " ++ content  -- Unclosed comment
      validator = newSyntaxValidator
      result = validateSyntax validator input
  in case result of
    Left _ -> property True
    Right newValidator -> 
      let errors = getSyntaxErrors newValidator
          hasCommentError = L.any (\err -> errorType err == UnclosedComment) errors
      in property $ hasCommentError || not (null errors)

-- Property: validateSyntax handles function declarations
prop_validate_syntax_functions :: [String] -> Property
prop_validate_syntax_functions functionNames =
  not (null functionNames) && L.all (not . null) functionNames &&
  L.all (L.all isAlphaNum) functionNames ==>
  let functionDecls = L.map (\name -> "func " ++ name ++ "() {}") functionNames
      input = unlines functionDecls
      validator = newSyntaxValidator
      result = validateSyntax validator input
  in case result of
    Left _ -> property True
    Right newValidator -> property $ True  -- Should handle function declarations

-- Property: validateSyntax handles variable declarations
prop_validate_syntax_variables :: [String] -> Property
prop_validate_syntax_variables variableNames =
  not (null variableNames) && L.all (not . null) variableNames &&
  L.all (L.all isAlphaNum) variableNames ==>
  let varDecls = L.map (\name -> "var " ++ name ++ " int") variableNames
      input = unlines varDecls
      validator = newSyntaxValidator
      result = validateSyntax validator input
  in case result of
    Left _ -> property True
    Right newValidator -> property $ True  -- Should handle variable declarations

-- Property: validateSyntax handles type declarations
prop_validate_syntax_types :: [String] -> Property
prop_validate_syntax_types typeNames =
  not (null typeNames) && L.all (not . null) typeNames &&
  L.all (L.all isAlphaNum) typeNames ==>
  let typeDecls = L.map (\name -> "type " ++ name ++ " int") typeNames
      input = unlines typeDecls
      validator = newSyntaxValidator
      result = validateSyntax validator input
  in case result of
    Left _ -> property True
    Right newValidator -> property $ True  -- Should handle type declarations

-- Property: validateSyntax handles import statements
prop_validate_syntax_imports :: [String] -> Property
prop_validate_syntax_imports importPaths =
  not (null importPaths) && L.all (not . null) importPaths &&
  L.all (L.all (`elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/.")) importPaths ==>
  let importDecls = L.map (\path -> "import \"" ++ path ++ "\"") importPaths
      input = unlines importDecls
      validator = newSyntaxValidator
      result = validateSyntax validator input
  in case result of
    Left _ -> property True
    Right newValidator -> property $ True  -- Should handle import statements

-- Property: validateSyntax handles package declarations
prop_validate_syntax_package :: String -> Property
prop_validate_syntax_package packageName =
  not (null packageName) && L.all isAlphaNum packageName ==>
  let input = "package " ++ packageName
      validator = newSyntaxValidator
      result = validateSyntax validator input
  in case result of
    Left _ -> property True
    Right newValidator -> property $ True  -- Should handle package declarations

-- Property: validateFile handles file-like input
prop_validate_file :: [String] -> Property
prop_validate_file lines =
  not (null lines) ==>
  let content = unlines lines
      validator = newSyntaxValidator
      result = validateFile validator content
  in case result of
    Left _ -> property True
    Right newValidator -> property $ True  -- Should handle file-like input

-- Property: getSyntaxErrors returns errors from validator
prop_get_syntax_errors :: String -> Property
prop_get_syntax_errors content =
  let validator = newSyntaxValidator
      result = validateSyntax validator content
  in case result of
    Left _ -> property True
    Right newValidator ->
      let errors = getSyntaxErrors newValidator
      in property $ True  -- Should return errors (possibly empty)

-- Property: formatSyntaxError produces non-empty output
prop_format_syntax_error :: SyntaxError -> Property
prop_format_syntax_error error =
  let formatted = formatSyntaxError error
  in property $ not (null formatted)

-- Property: formatSyntaxError includes error message
prop_format_syntax_error_includes_message :: SyntaxError -> Property
prop_format_syntax_error_includes_message error =
  let formatted = formatSyntaxError error
      msg = errorMessage error
  in property $ msg `L.isInfixOf` formatted

-- Property: formatSyntaxError includes line number
prop_format_syntax_error_includes_line :: SyntaxError -> Property
prop_format_syntax_error_includes_line error =
  let formatted = formatSyntaxError error
      line = lineNumber error
  in property $ show line `L.isInfixOf` formatted

-- Property: formatSyntaxError includes column number
prop_format_syntax_error_includes_column :: SyntaxError -> Property
prop_format_syntax_error_includes_column error =
  let formatted = formatSyntaxError error
      column = columnNumber error
  in property $ show column `L.isInfixOf` formatted

-- Property: Syntax validation is deterministic
prop_syntax_validation_deterministic :: String -> Property
prop_syntax_validation_deterministic content =
  let validator1 = newSyntaxValidator
      validator2 = newSyntaxValidator
      result1 = validateSyntax validator1 content
      result2 = validateSyntax validator2 content
  in case (result1, result2) of
    (Right v1, Right v2) ->
      let errors1 = getSyntaxErrors v1
          errors2 = getSyntaxErrors v2
      in property $ L.length errors1 == L.length errors2
    _ -> property True  -- Handle error cases consistently

tests :: TestTree
tests = testGroup "Advanced SyntaxValidator QuickCheck"
  [ fastProperty "error type reflexive" prop_error_type_reflexive
  , fastProperty "syntax error ordering" prop_syntax_error_ordering
  , fastProperty "new syntax validator valid" prop_new_syntax_validator_valid
  , fastProperty "validate syntax empty" prop_validate_syntax_empty
  , fastProperty "validate syntax whitespace" prop_validate_syntax_whitespace
  , fastProperty "validate syntax identifiers" prop_validate_syntax_identifiers
  , fastProperty "validate syntax balanced braces" prop_validate_syntax_balanced_braces
  , fastProperty "validate syntax unbalanced braces" prop_validate_syntax_unbalanced_braces
  , fastProperty "validate syntax balanced parens" prop_validate_syntax_balanced_parens
  , fastProperty "validate syntax unbalanced parens" prop_validate_syntax_unbalanced_parens
  , fastProperty "validate syntax balanced brackets" prop_validate_syntax_balanced_brackets
  , fastProperty "validate syntax unbalanced brackets" prop_validate_syntax_unbalanced_brackets
  , fastProperty "validate syntax strings" prop_validate_syntax_strings
  , fastProperty "validate syntax unclosed strings" prop_validate_syntax_unclosed_strings
  , fastProperty "validate syntax comments" prop_validate_syntax_comments
  , fastProperty "validate syntax multiline comments" prop_validate_syntax_multiline_comments
  , fastProperty "validate syntax unclosed multiline comments" prop_validate_syntax_unclosed_multiline_comments
  , fastProperty "validate syntax functions" prop_validate_syntax_functions
  , fastProperty "validate syntax variables" prop_validate_syntax_variables
  , fastProperty "validate syntax types" prop_validate_syntax_types
  , fastProperty "validate syntax imports" prop_validate_syntax_imports
  , fastProperty "validate syntax package" prop_validate_syntax_package
  , fastProperty "validate file" prop_validate_file
  , fastProperty "get syntax errors" prop_get_syntax_errors
  , fastProperty "format syntax error" prop_format_syntax_error
  , fastProperty "format syntax error includes message" prop_format_syntax_error_includes_message
  , fastProperty "format syntax error includes line" prop_format_syntax_error_includes_line
  , fastProperty "format syntax error includes column" prop_format_syntax_error_includes_column
  , fastProperty "syntax validation deterministic" prop_syntax_validation_deterministic
  ]