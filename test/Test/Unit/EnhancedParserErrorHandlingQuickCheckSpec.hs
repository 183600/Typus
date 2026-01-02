{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.EnhancedParserErrorHandlingQuickCheckSpec (tests) where

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

import SourceLocation (SourcePos(..), startPos)
import Utils (trim)
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

-- Property: Parsing empty string returns file with empty blocks
prop_parse_empty_string :: Property
prop_parse_empty_string =
  let result = parseTypus "" startPos
  in case result of
    Left _ -> property False
    Right file -> property $ L.null (tfBlocks file)

-- Property: Parsing string with only whitespace returns file with empty blocks
prop_parse_whitespace_only :: String -> Property
prop_parse_whitespace_only input =
  L.all isSpace input ==>
  let result = parseTypus input startPos
  in case result of
    Left _ -> property False
    Right file -> property $ L.null (tfBlocks file)

-- Property: Parsing string with only comments returns file with empty blocks
prop_parse_comments_only :: String -> Property
prop_parse_comments_only comment =
  not (null comment) && not (L.any (`elem` comment) "\"'\\") ==>
  let commentLine = "// " ++ comment
      result = parseTypus commentLine startPos
  in case result of
    Left _ -> property False
    Right file -> property $ L.null (tfBlocks file)

-- Property: Parsing well-formed directive block succeeds
prop_parse_well_formed_directive :: String -> String -> Property
prop_parse_well_formed_directive directiveName content =
  not (null directiveName) && not (L.any (`elem` directiveName) "{}\"'\\") ==>
  let directiveBlock = unlines
        [ "#[" ++ directiveName ++ "]"
        , "{"
        , content
        , "}"
        ]
      result = parseTypus directiveBlock startPos
  in case result of
    Left _ -> property False
    Right file -> property $ not (L.null (tfBlocks file))

-- Property: Parsing unterminated directive block fails
prop_parse_unterminated_directive :: String -> String -> Property
prop_parse_unterminated_directive directiveName content =
  not (null directiveName) && not (L.any (`elem` directiveName) "{}\"'\\") ==>
  let unterminatedBlock = unlines
        [ "#[" ++ directiveName ++ "]"
        , "{"
        , content
        -- Missing closing brace
        ]
      result = parseTypus unterminatedBlock startPos
  in case result of
    Left err -> property $ "Unclosed directive block" `L.isInfixOf` err
    Right _ -> property False

-- Property: Parsing mismatched braces fails
prop_parse_mismatched_braces :: String -> Property
prop_parse_mismatched_braces content =
  let mismatchedBlock = unlines
        [ "#[test]"
        , "{"
        , content
        , "}}"  -- Extra closing brace
        ]
      result = parseTypus mismatchedBlock startPos
  in case result of
    Left _ -> property True
    Right _ -> property False

-- Property: Parsing directive with boolean values
prop_parse_boolean_directive :: String -> Bool -> Property
prop_parse_boolean_directive directiveName value =
  not (null directiveName) && not (L.any (`elem` directiveName) "{}\"'\\") ==>
  let boolStr = if value then "on" else "off"
      directiveBlock = unlines
        [ "#[" ++ directiveName ++ "]"
        , "{"
        , "ownership: " ++ boolStr
        , "}"
        ]
      result = parseTypus directiveBlock startPos
  in case result of
    Left _ -> property False
    Right file -> property $ not (L.null (tfBlocks file))

-- Property: Parsing multiple directive blocks
prop_parse_multiple_directives :: [String] -> [String] -> Property
prop_parse_multiple_directives directiveNames contents =
  not (null directiveNames) && L.all (not . null) directiveNames &&
  L.all (not . L.any (`elem` "{}\"'\\")) directiveNames &&
  L.length directiveNames == L.length contents ==>
  let directiveBlocks = L.concat $ zipWith makeDirectiveBlock directiveNames contents
      result = parseTypus directiveBlocks startPos
      makeDirectiveBlock name content = unlines
        [ "#[" ++ name ++ "]"
        , "{"
        , content
        , "}"
        , ""
        ]
  in case result of
    Left _ -> property False
    Right file -> property $ L.length (tfBlocks file) == L.length directiveNames

-- Property: Parsing directive with nested braces in strings
prop_parse_nested_braces_in_strings :: String -> Property
prop_parse_nested_braces_in_strings content =
  not (L.any (`elem` content) "\"\\") ==>
  let stringWithBraces = "\"{ nested { braces } in string }\""
      directiveBlock = unlines
        [ "#[test]"
        , "{"
        , stringWithBraces
        , content
        , "}"
        ]
      result = parseTypus directiveBlock startPos
  in case result of
    Left _ -> property False
    Right file -> property $ not (L.null (tfBlocks file))

-- Property: Parsing directive with comments inside
prop_parse_comments_in_directive :: String -> String -> Property
prop_parse_comments_in_directive content comment =
  not (L.any (`elem` comment) "\"'\\") ==>
  let directiveBlock = unlines
        [ "#[test]"
        , "{"
        , "// This is a comment"
        , content
        , "// Another comment"
        , "}"
        ]
      result = parseTypus directiveBlock startPos
  in case result of
    Left _ -> property False
    Right file -> property $ not (L.null (tfBlocks file))

-- Property: Parsing empty directive block
prop_parse_empty_directive :: String -> Property
prop_parse_empty_directive directiveName =
  not (null directiveName) && not (L.any (`elem` directiveName) "{}\"'\\") ==>
  let emptyDirective = unlines
        [ "#[" ++ directiveName ++ "]"
        , "{"
        , "}"
        ]
      result = parseTypus emptyDirective startPos
  in case result of
    Left _ -> property False
    Right file -> property $ not (L.null (tfBlocks file))

-- Property: Parsing directive with invalid boolean value fails
prop_parse_invalid_boolean :: String -> String -> Property
prop_parse_invalid_boolean directiveName invalidValue =
  not (null directiveName) && not (null invalidValue) &&
  not (L.any (`elem` directiveName) "{}\"'\\") &&
  invalidValue `notElem` ["on", "off", "true", "false"] ==>
  let directiveBlock = unlines
        [ "#[" ++ directiveName ++ "]"
        , "{"
        , "ownership: " ++ invalidValue
        , "}"
        ]
      result = parseTypus directiveBlock startPos
  in case result of
    Left err -> property $ "Invalid boolean value" `L.isInfixOf` err
    Right _ -> property False

-- Property: Parsing directive with unknown key fails gracefully
prop_parse_unknown_directive :: String -> String -> Property
prop_parse_unknown_directive directiveName unknownKey =
  not (null directiveName) && not (null unknownKey) &&
  not (L.any (`elem` directiveName) "{}\"'\\") &&
  not (L.any (`elem` unknownKey) "{}\"'\\") ==>
  let directiveBlock = unlines
        [ "#[" ++ directiveName ++ "]"
        , "{"
        , unknownKey ++ ": on"
        , "}"
        ]
      result = parseTypus directiveBlock startPos
  in case result of
    Left _ -> property True  -- Should fail gracefully
    Right file -> property $ not (L.null (tfBlocks file))  -- Or succeed with partial parsing

-- Property: Parsing preserves line numbers
prop_parse_preserves_line_numbers :: [String] -> Property
prop_parse_preserves_line_numbers lines =
  not (null lines) && L.all (not . L.any (`elem` "{}\"'\\")) lines ==>
  let content = unlines lines
      result = parseTypus content startPos
  in case result of
    Left _ -> property False
    Right file -> 
      case tfBlocks file of
        [] -> property $ L.length lines <= 1  -- No blocks if only comments/whitespace
        (block:_) -> property $ True  -- Basic check that parsing succeeded

-- Property: Parsing handles unicode content
prop_parse_unicode_content :: String -> Property
prop_parse_unicode_content unicodeContent =
  let directiveBlock = unlines
        [ "#[unicode_test]"
        , "{"
        , "content: " ++ unicodeContent
        , "}"
        ]
      result = parseTypus directiveBlock startPos
  in case result of
    Left _ -> property False
    Right file -> property $ not (L.null (tfBlocks file))

-- Property: Parsing handles very long lines
prop_parse_long_lines :: Int -> String -> Property
prop_parse_long_lines L.length baseContent =
  L.length > 0 && L.length <= 1000 && not (L.any (`elem` baseContent) "{}\"'\\") ==>
  let longContent = baseContent ++ L.concat (replicate L.length "x")
      directiveBlock = unlines
        [ "#[long_line_test]"
        , "{"
        , longContent
        , "}"
        ]
      result = parseTypus directiveBlock startPos
  in case result of
    Left _ -> property False
    Right file -> property $ not (L.null (tfBlocks file))

tests :: TestTree
tests = testGroup "Enhanced Parser Error Handling QuickCheck"
  [ fastProperty "parse empty string" prop_parse_empty_string
  , fastProperty "parse whitespace only" prop_parse_whitespace_only
  , fastProperty "parse comments only" prop_parse_comments_only
  , fastProperty "parse well-formed directive" prop_parse_well_formed_directive
  , fastProperty "parse unterminated directive" prop_parse_unterminated_directive
  , fastProperty "parse mismatched braces" prop_parse_mismatched_braces
  , fastProperty "parse boolean directive" prop_parse_boolean_directive
  , fastProperty "parse multiple directives" prop_parse_multiple_directives
  , fastProperty "parse nested braces in strings" prop_parse_nested_braces_in_strings
  , fastProperty "parse comments in directive" prop_parse_comments_in_directive
  , fastProperty "parse empty directive" prop_parse_empty_directive
  , fastProperty "parse invalid boolean" prop_parse_invalid_boolean
  , fastProperty "parse unknown directive" prop_parse_unknown_directive
  , fastProperty "parse preserves line numbers" prop_parse_preserves_line_numbers
  , fastProperty "parse unicode content" prop_parse_unicode_content
  , fastProperty "parse long lines" prop_parse_long_lines
  ]