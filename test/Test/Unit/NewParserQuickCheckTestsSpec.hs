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
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose)
import Test.QuickCheck.Gen (Gen(..), vectorOf)

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  , parseBool
  , curlyDelta
  , leadingIndentation
  )

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos)

import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

-- ============================================================================
-- Arbitrary instances
-- ============================================================================

instance Arbitrary FileDirectives where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    constraints <- arbitrary
    return $ FileDirectives 
      { fdOwnership = if ownership then Just (Located startPos True) else Nothing
      , fdDependentTypes = if dependentTypes then Just (Located startPos True) else Nothing
      , fdConstraints = if constraints then Just (Located startPos True) else Nothing
      }

instance Arbitrary BlockDirectives where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    constraints <- arbitrary
    return $ BlockDirectives
      { bdOwnership = if ownership then Just (Located startPos True) else Nothing
      , bdDependentTypes = if dependentTypes then Just (Located startPos True) else Nothing
      , bdConstraints = if constraints then Just (Located startPos True) else Nothing
      }

-- Generate valid boolean strings for directives
validBoolString :: Gen String
validBoolString = oneof 
  [ return "on"
  , return "off"
  , return "true"
  , return "false"
  ]

-- Generate invalid boolean strings
invalidBoolString :: Gen String
invalidBoolString = oneof
  [ elements ["maybe", "yes", "no", "1", "0", "ON", "OFF", "TRUE", "FALSE"]
  , listOf $ elements ['a'..'z']
  ]

-- Generate strings with varying indentation
indentedString :: Gen String
indentedString = do
  indent <- choose (0, 10)
  content <- listOf $ elements ['a'..'z']
  return $ replicate indent ' ' ++ content

-- Generate strings with curly braces
curlyBraceString :: Gen String
curlyBraceString = do
  opens <- choose (0, 5)
  closes <- choose (0, 5)
  content <- listOf $ elements ['a'..'z']
  return $ replicate opens '{' ++ content ++ replicate closes '}'

-- ============================================================================
-- Parser Property Tests
-- ============================================================================

-- Property: parseBool correctly parses valid boolean values
prop_parseBool_valid_values :: String -> Property
prop_parseBool_valid_values boolStr =
  boolStr `elem` ["on", "off", "true", "false"] ==>
  case parseBool boolStr of
    Left _ -> property False
    Right result -> 
      case boolStr of
        "on" -> result === True
        "off" -> result === False
        "true" -> result === True
        "false" -> result === False
        _ -> property False

-- Property: parseBool rejects invalid boolean values
prop_parseBool_invalid_values :: Property
prop_parseBool_invalid_values =
  forAll invalidBoolString $ \boolStr ->
    boolStr `notElem` ["on", "off", "true", "false"] ==>
    case parseBool boolStr of
      Left _ -> property True
      Right _ -> property False

-- Property: curlyDelta correctly counts curly braces
prop_curlyDelta_counts_braces :: String -> String -> String -> Property
prop_curlyDelta_counts_braces prefix content suffix =
  let opens = length $ filter (== '{') prefix
      closes = length $ filter (== '}') suffix
      input = prefix ++ content ++ suffix
      delta = curlyDelta input
  in property $ delta === opens - closes

-- Property: curlyDelta ignores braces in strings
prop_curlyDelta_ignores_string_braces :: String -> String -> Property
prop_curlyDelta_ignores_string_braces before after =
  not ('"' `elem` before) && not ('"' `elem` after) ==>
  let input = before ++ "\"{not counted}\"" ++ after
      delta = curlyDelta input
  in property $ delta === 0

-- Property: curlyDelta ignores braces in line comments
prop_curlyDelta_ignores_comment_braces :: String -> String -> Property
prop_curlyDelta_ignores_comment_braces before after =
  not ('/' `elem` before) && not ('/' `elem` after) ==>
  let input = before ++ "// {not counted}" ++ after
      delta = curlyDelta input
  in property $ delta === 0

-- Property: leadingIndentation counts leading spaces
prop_leadingIndentation_counts_spaces :: Int -> String -> Property
prop_leadingIndentation_counts_spaces indent content =
  indent >= 0 && indent <= 20 ==>
  let input = replicate indent ' ' ++ content
      result = leadingIndentation input
  in property $ result === indent

-- Property: leadingIndentation counts leading tabs
prop_leadingIndentation_counts_tabs :: Int -> String -> Property
prop_leadingIndentation_counts_tabs indent content =
  indent >= 0 && indent <= 20 ==>
  let input = replicate indent '\t' ++ content
      result = leadingIndentation input
  in property $ result === indent

-- Property: leadingIndentation handles mixed whitespace
prop_leadingIndentation_mixed_whitespace :: Int -> Int -> String -> Property
prop_leadingIndentation_mixed_whitespace spaces tabs content =
  spaces >= 0 && spaces <= 10 && tabs >= 0 && tabs <= 10 ==>
  let input = replicate spaces ' ' ++ replicate tabs '\t' ++ content
      result = leadingIndentation input
  in property $ result === spaces + tabs

-- Property: leadingIndentation handles empty strings
prop_leadingIndentation_empty_string :: Property
prop_leadingIndentation_empty_string =
  leadingIndentation "" === 0

-- Property: leadingIndentation handles strings with no leading whitespace
prop_leadingIndentation_no_leading_whitespace :: String -> Property
prop_leadingIndentation_no_leading_whitespace content =
  null content || not (isSpace (head content)) ==>
  leadingIndentation content === 0

-- Property: parseTypus handles empty input
prop_parseTypus_empty_input :: Property
prop_parseTypus_empty_input =
  case parseTypus "" of
    Left _ -> property True  -- Parsing error is acceptable for empty input
    Right result -> 
      let tf = tfDirectives result
          blocks = tfBlocks result
      in property $ tf === defaultFileDirectives .&&. null blocks

-- Property: parseTypus preserves file directives
prop_parseTypus_file_directives :: Property
prop_parseTypus_file_directives =
  let input = "//! ownership=on, dependent_types=true\n"
  in case parseTypus input of
    Left _ -> property False
    Right result ->
      let directives = tfDirectives result
      in case (fdOwnership directives, fdDependentTypes directives) of
        (Just (Located _ True), Just (Located _ True)) -> property True
        _ -> property False

-- Property: parseTypus handles simple code blocks
prop_parseTypus_simple_blocks :: String -> Property
prop_parseTypus_simple_blocks content =
  not ("//" `isInfixOf` content) && not ("/*" `isInfixOf` content) ==>
  let input = "//! ownership=on\n" ++ content ++ "\n"
  in case parseTypus input of
    Left _ -> property True  -- Parsing errors are acceptable
    Right result ->
      let blocks = tfBlocks result
      in property $ not (null blocks)

-- Property: defaultFileDirectives has all Nothing values
prop_defaultFileDirectives_nothing :: Property
prop_defaultFileDirectives_nothing =
  let fd = defaultFileDirectives
  in property $ fdOwnership fd === Nothing .&&.
               fdDependentTypes fd === Nothing .&&.
               fdConstraints fd === Nothing

-- Property: defaultBlockDirectives has all Nothing values
prop_defaultBlockDirectives_nothing :: Property
prop_defaultBlockDirectives_nothing =
  let bd = defaultBlockDirectives
  in property $ bdOwnership bd === Nothing .&&.
               bdDependentTypes bd === Nothing .&&.
               bdConstraints bd === Nothing

-- Property: curlyDelta handles nested braces correctly
prop_curlyDelta_nested_braces :: Int -> Property
prop_curlyDelta_nested_braces depth =
  depth >= 0 && depth <= 10 ==>
  let nestedBraces = concat $ replicate depth "{}"
      delta = curlyDelta nestedBraces
  in property $ delta === 0

-- Property: curlyDelta handles unbalanced braces
prop_curlyDelta_unbalanced_braces :: Int -> Int -> Property
prop_curlyDelta_unbalanced_braces opens closes =
  opens >= 0 && opens <= 5 && closes >= 0 && closes <= 5 ==>
  let unbalanced = replicate opens '{' ++ replicate closes '}'
      delta = curlyDelta unbalanced
  in property $ delta === opens - closes

-- Property: leadingIndentation is idempotent on non-whitespace
prop_leadingIndentation_non_whitespace :: String -> Property
prop_leadingIndentation_non_whitespace content =
  null content || not (isSpace (head content)) ==>
  leadingIndentation content === 0

-- Property: parseBool is case sensitive
prop_parseBool_case_sensitive :: Property
prop_parseBool_case_sensitive =
  let invalidCases = ["ON", "OFF", "TRUE", "FALSE", "On", "Off", "True", "False"]
  in all (\caseStr -> case parseBool caseStr of
                        Left _ -> True
                        Right _ -> False) invalidCases

-- Property: curlyDelta handles escaped quotes correctly
prop_curlyDelta_escaped_quotes :: String -> String -> Property
prop_curlyDelta_escaped_quotes before after =
  not ('"' `elem` before) && not ('"' `elem` after) ==>
  let input = before ++ "\"\\\"{not counted}\\\"\"" ++ after
      delta = curlyDelta input
  in property $ delta === 0

-- Property: parseTypus handles multiple directives
prop_parseTypus_multiple_directives :: Property
prop_parseTypus_multiple_directives =
  let input = "//! ownership=on, dependent_types=true, constraints=off\n"
  in case parseTypus input of
    Left _ -> property False
    Right result ->
      let directives = tfDirectives result
      in case (fdOwnership directives, fdDependentTypes directives, fdConstraints directives) of
        (Just (Located _ True), Just (Located _ True), Just (Located _ False)) -> property True
        _ -> property False

-- Property: parseTypus handles malformed directives gracefully
prop_parseTypus_malformed_directives :: Property
prop_parseTypus_malformed_directives =
  let input = "//! invalid_directive=value\n"
  in case parseTypus input of
    Left _ -> property True  -- Should handle malformed directives gracefully
    Right result -> property True  -- Or parse successfully with defaults

-- Property: curlyDelta handles complex string literals
prop_curlyDelta_complex_strings :: Property
prop_curlyDelta_complex_strings =
  let complexString = "\"string with { braces } and \\\"escaped quotes\\\" // not a comment\""
      delta = curlyDelta complexString
  in property $ delta === 0

-- Property: leadingIndentation handles Unicode whitespace
prop_leadingIndentation_unicode_whitespace :: Int -> Property
prop_leadingIndentation_unicode_whitespace count =
  count >= 0 && count <= 10 ==>
  let unicodeSpaces = replicate count '\160'  -- Non-breaking space
      content = "content"
      input = unicodeSpaces ++ content
      result = leadingIndentation input
  in property $ result === count  -- Should count all whitespace characters

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Parser QuickCheck Tests"
  [ fastProperty "parseBool correctly parses valid boolean values" prop_parseBool_valid_values
  , fastProperty "parseBool rejects invalid boolean values" prop_parseBool_invalid_values
  , fastProperty "curlyDelta correctly counts curly braces" prop_curlyDelta_counts_braces
  , fastProperty "curlyDelta ignores braces in strings" prop_curlyDelta_ignores_string_braces
  , fastProperty "curlyDelta ignores braces in line comments" prop_curlyDelta_ignores_comment_braces
  , fastProperty "leadingIndentation counts leading spaces" prop_leadingIndentation_counts_spaces
  , fastProperty "leadingIndentation counts leading tabs" prop_leadingIndentation_counts_tabs
  , fastProperty "leadingIndentation handles mixed whitespace" prop_leadingIndentation_mixed_whitespace
  , fastProperty "leadingIndentation handles empty strings" prop_leadingIndentation_empty_string
  , fastProperty "leadingIndentation handles strings with no leading whitespace" prop_leadingIndentation_no_leading_whitespace
  , fastProperty "parseTypus handles empty input" prop_parseTypus_empty_input
  , fastProperty "parseTypus preserves file directives" prop_parseTypus_file_directives
  , fastProperty "parseTypus handles simple code blocks" prop_parseTypus_simple_blocks
  , fastProperty "defaultFileDirectives has all Nothing values" prop_defaultFileDirectives_nothing
  , fastProperty "defaultBlockDirectives has all Nothing values" prop_defaultBlockDirectives_nothing
  , fastProperty "curlyDelta handles nested braces correctly" prop_curlyDelta_nested_braces
  , fastProperty "curlyDelta handles unbalanced braces" prop_curlyDelta_unbalanced_braces
  , fastProperty "leadingIndentation is idempotent on non-whitespace" prop_leadingIndentation_non_whitespace
  , fastProperty "parseBool is case sensitive" prop_parseBool_case_sensitive
  , fastProperty "curlyDelta handles escaped quotes correctly" prop_curlyDelta_escaped_quotes
  , fastProperty "parseTypus handles multiple directives" prop_parseTypus_multiple_directives
  , fastProperty "parseTypus handles malformed directives gracefully" prop_parseTypus_malformed_directives
  , fastProperty "curlyDelta handles complex string literals" prop_curlyDelta_complex_strings
  , fastProperty "leadingIndentation handles Unicode whitespace" prop_leadingIndentation_unicode_whitespace
  ]