{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserNewQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
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

import SourceLocation (SourcePos(..), SourceSpan(..), startPos, spanBetween)
import Data.Char (isSpace, isAlphaNum)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Maybe (isJust, isNothing)

-- ============================================================================
-- Parser Basic Properties
-- ============================================================================

-- Property: Parsing empty string returns valid TypusFile
prop_parse_empty_string :: Property
prop_parse_empty_string =
  case parseTypus "" of
    Left _ -> property False
    Right file -> property $ tfDirectives file === defaultFileDirectives .&&.
                        null (tfBlocks file) .&&.
                        null (tfSyntaxErrors file)

-- Property: Parsing string with only whitespace returns valid TypusFile
prop_parse_whitespace_only :: String -> Property
prop_parse_whitespace_only whitespace =
  all isSpace whitespace ==>
  case parseTypus whitespace of
    Left _ -> property False
    Right file -> property $ tfDirectives file === defaultFileDirectives .&&.
                        null (tfBlocks file) .&&.
                        null (tfSyntaxErrors file)

-- Property: Parsing simple Go code returns valid TypusFile
prop_parse_simple_go_code :: String -> Property
prop_parse_simple_go_code code =
  not (any (`elem` "\r\n{}") code) && not (null code) && all isAlphaNum code ==>
  let simpleCode = "package main\n\nfunc main() {\n    " ++ code ++ "\n}\n"
  in case parseTypus simpleCode of
       Left _ -> property False
       Right file -> property $ not (null (tfBlocks file))

-- Property: Parsing with package directive preserves package name
prop_parse_package_directive :: String -> Property
prop_parse_package_directive packageName =
  not (null packageName) && all isAlphaNum packageName ==>
  let code = "package " ++ packageName ++ "\n\nfunc main() {}\n"
  in case parseTypus code of
       Left _ -> property False
       Right file -> property $ any ("package " `isPrefixOf`) (map cbContent (tfBlocks file))

-- Property: Parsing with import directive preserves import
prop_parse_import_directive :: String -> Property
prop_parse_import_directive importName =
  not (null importName) && all isAlphaNum importName ==>
  let code = "package main\n\nimport \"" ++ importName ++ "\"\n\nfunc main() {}\n"
  in case parseTypus code of
       Left _ -> property False
       Right file -> property $ any ("import \"" `isPrefixOf`) (map cbContent (tfBlocks file))

-- Property: Parsing preserves line structure
prop_parse_preserves_lines :: [String] -> Property
prop_parse_preserves_lines lines =
  not (null lines) && all (not . null) lines ==>
  let code = unlines lines
  in case parseTypus code of
       Left _ -> property False
       Right file -> property $ length (tfBlocks file) >= length (filter (not . all isSpace) lines)

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: Parsing with unmatched braces produces syntax errors
prop_parse_unmatched_braces :: String -> Property
prop_parse_unmatched_braces content =
  not (null content) && not ('{' `elem` content) && '}' `elem` content ==>
  let code = "package main\n\nfunc main() {\n    " ++ content ++ "\n}\n}"
  in case parseTypus code of
       Left _ -> property True
       Right file -> property $ not (null (tfSyntaxErrors file))

-- Property: Parsing with if without brace produces error
prop_parse_if_without_brace :: String -> Property
prop_parse_if_without_brace condition =
  not (null condition) && not ('{' `elem` condition) ==>
  let code = "package main\n\nfunc main() {\n    if " ++ condition ++ "\n        // do something\n    }\n"
  in case parseTypus code of
       Left _ -> property True
       Right file -> property $ not (null (tfSyntaxErrors file))

-- Property: Parsing with invalid syntax produces errors
prop_parse_invalid_syntax :: String -> Property
prop_parse_invalid_syntax invalidCode =
  not (null invalidCode) && any (`elem` "@#$%^&*") invalidCode ==>
  let code = "package main\n\nfunc main() {\n    " ++ invalidCode ++ "\n}\n"
  in case parseTypus code of
       Left _ -> property True
       Right file -> property $ not (null (tfSyntaxErrors file))

-- ============================================================================
-- Directive Properties
-- ============================================================================

-- Property: File directives are parsed correctly
prop_parse_file_directives :: String -> Property
prop_parse_file_directives directive =
  not (null directive) && all isAlphaNum directive ==>
  let code = "// //!ownership:" ++ directive ++ "\npackage main\n\nfunc main() {}\n"
  in case parseTypus code of
       Left _ -> property False
       Right file -> property $ isJust (fdOwnership (tfDirectives file))

-- Property: Block directives are parsed correctly
prop_parse_block_directives :: String -> Property
prop_parse_block_directives directive =
  not (null directive) && all isAlphaNum directive ==>
  let code = "package main\n\n// {!//!ownership:" ++ directive ++ "}\nfunc main() {}\n"
  in case parseTypus code of
       Left _ -> property False
       Right file -> property $ any (isJust . bdOwnership . cbDirectives) (tfBlocks file)

-- Property: Multiple directives are preserved
prop_parse_multiple_directives :: String -> String -> Property
prop_parse_multiple_directives dir1 dir2 =
  not (null dir1) && not (null dir2) && all isAlphaNum dir1 && all isAlphaNum dir2 ==>
  let code = "// //!ownership:" ++ dir1 ++ "\n// //!dependent:" ++ dir2 ++ "\npackage main\n\nfunc main() {}\n"
  in case parseTypus code of
       Left _ -> property False
       Right file -> property $ isJust (fdOwnership (tfDirectives file)) .&&.
                           isJust (fdDependentTypes (tfDirectives file))

-- ============================================================================
-- Content Preservation Properties
-- ============================================================================

-- Property: Parsing preserves function names
prop_parse_preserves_function_names :: String -> Property
prop_parse_preserves_function_names funcName =
  not (null funcName) && all isAlphaNum funcName ==>
  let code = "package main\n\nfunc " ++ funcName ++ "() {}\n"
  in case parseTypus code of
       Left _ -> property False
       Right file -> property $ any (funcName `isInfixOf`) (map cbContent (tfBlocks file))

-- Property: Parsing preserves variable names
prop_parse_preserves_variable_names :: String -> String -> Property
prop_parse_preserves_variable_names varName value =
  not (null varName) && not (null value) && all isAlphaNum varName && all isAlphaNum value ==>
  let code = "package main\n\nfunc main() {\n    " ++ varName ++ " := " ++ value ++ "\n}\n"
  in case parseTypus code of
       Left _ -> property False
       Right file -> property $ any (varName `isInfixOf`) (map cbContent (tfBlocks file))

-- Property: Parsing preserves string literals
prop_parse_preserves_string_literals :: String -> Property
prop_parse_preserves_string_literals strLiteral =
  not (null strLiteral) && not ('"' `elem` strLiteral) ==>
  let code = "package main\n\nfunc main() {\n    s := \"" ++ strLiteral ++ "\"\n}\n"
  in case parseTypus code of
       Left _ -> property False
       Right file -> property $ any ("\"" ++ strLiteral ++ "\"") (map cbContent (tfBlocks file))

-- Property: Parsing preserves numeric literals
prop_parse_preserves_numeric_literals :: Int -> Property
prop_parse_preserves_numeric_literals num =
  num >= 0 && num <= 1000 ==>
  let code = "package main\n\nfunc main() {\n    x := " ++ show num ++ "\n}\n"
  in case parseTypus code of
       Left _ -> property False
       Right file -> property $ any (show num `isInfixOf`) (map cbContent (tfBlocks file))

-- ============================================================================
-- Complex Structure Properties
-- ============================================================================

-- Property: Parsing nested structures preserves hierarchy
prop_parse_nested_structures :: String -> String -> Property
prop_parse_nested_structures outer inner =
  not (null outer) && not (null inner) && all isAlphaNum outer && all isAlphaNum inner ==>
  let code = "package main\n\nfunc " ++ outer ++ "() {\n    func " ++ inner ++ "() {}\n}\n"
  in case parseTypus code of
       Left _ -> property False
       Right file -> property $ any (outer `isInfixOf`) (map cbContent (tfBlocks file)) .&&.
                           any (inner `isInfixOf`) (map cbContent (tfBlocks file))

-- Property: Parsing multiple functions preserves all
prop_parse_multiple_functions :: [String] -> Property
prop_parse_multiple_functions funcNames =
  not (null funcNames) && all (not . null) funcNames && all (all isAlphaNum) funcNames ==>
  let funcDefs = map (\name -> "func " ++ name ++ "() {}") funcNames
      code = "package main\n\n" ++ unlines funcDefs ++ "\n"
  in case parseTypus code of
       Left _ -> property False
       Right file -> property $ all (\name -> any (name `isInfixOf`) (map cbContent (tfBlocks file))) funcNames

-- Property: Parsing with comments preserves comments
prop_parse_preserves_comments :: String -> Property
prop_parse_preserves_comments comment =
  not (null comment) && not ("//" `isInfixOf` comment) ==>
  let code = "package main\n\n// " ++ comment ++ "\nfunc main() {}\n"
  in case parseTypus code of
       Left _ -> property False
       Right file -> property $ any (comment `isInfixOf`) (map cbContent (tfBlocks file))

-- ============================================================================
-- Edge Case Properties
-- ============================================================================

-- Property: Parsing very long lines works
prop_parse_long_lines :: Int -> Property
prop_parse_long_lines length =
  length > 0 && length <= 1000 ==>
  let longLine = replicate length 'x'
      code = "package main\n\nfunc main() {\n    " ++ longLine ++ "\n}\n"
  in case parseTypus code of
       Left _ -> property False
       Right file -> property $ not (null (tfBlocks file))

-- Property: Parsing with Unicode characters works
prop_parse_unicode :: String -> Property
prop_parse_unicode unicodeText =
  not (null unicodeText) ==>
  let code = "package main\n\nfunc main() {\n    // " ++ unicodeText ++ "\n}\n"
  in case parseTypus code of
       Left _ -> property False
       Right file -> property $ any (unicodeText `isInfixOf`) (map cbContent (tfBlocks file))

-- Property: Parsing with tabs and spaces mixed works
prop_parse_mixed_whitespace :: String -> String -> Property
prop_parse_mixed_whitespace content1 content2 =
  not (null content1) && not (null content2) ==>
  let code = "package main\n\nfunc main() {\n\t" ++ content1 ++ "\n    " ++ content2 ++ "\n}\n"
  in case parseTypus code of
       Left _ -> property False
       Right file -> property $ not (null (tfBlocks file))

-- ============================================================================
-- Idempotency Properties
-- ============================================================================

-- Property: Parsing and re-parsing extracted content gives similar results
prop_parse_idempotent :: String -> Property
prop_parse_idempotent originalCode =
  not (null originalCode) ==>
  case parseTypus originalCode of
    Left _ -> property False
    Right file -> 
      let extractedCode = unlines (map cbContent (tfBlocks file))
      in case parseTypus extractedCode of
           Left _ -> property False
           Right reparsedFile -> property $ length (tfBlocks reparsedFile) >= 1

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser New QuickCheck Tests"
  [ testGroup "Basic Properties"
    [ fastProperty "parse empty string" prop_parse_empty_string
    , fastProperty "parse whitespace only" prop_parse_whitespace_only
    , fastProperty "parse simple Go code" prop_parse_simple_go_code
    , fastProperty "parse package directive" prop_parse_package_directive
    , fastProperty "parse import directive" prop_parse_import_directive
    , fastProperty "parse preserves lines" prop_parse_preserves_lines
    ]
  , testGroup "Error Handling"
    [ fastProperty "unmatched braces produce errors" prop_parse_unmatched_braces
    , fastProperty "if without brace produces error" prop_parse_if_without_brace
    , fastProperty "invalid syntax produces errors" prop_parse_invalid_syntax
    ]
  , testGroup "Directives"
    [ fastProperty "file directives parsed correctly" prop_parse_file_directives
    , fastProperty "block directives parsed correctly" prop_parse_block_directives
    , fastProperty "multiple directives preserved" prop_parse_multiple_directives
    ]
  , testGroup "Content Preservation"
    [ fastProperty "preserves function names" prop_parse_preserves_function_names
    , fastProperty "preserves variable names" prop_parse_preserves_variable_names
    , fastProperty "preserves string literals" prop_parse_preserves_string_literals
    , fastProperty "preserves numeric literals" prop_parse_preserves_numeric_literals
    ]
  , testGroup "Complex Structures"
    [ fastProperty "nested structures preserved" prop_parse_nested_structures
    , fastProperty "multiple functions preserved" prop_parse_multiple_functions
    , fastProperty "comments preserved" prop_parse_preserves_comments
    ]
  , testGroup "Edge Cases"
    [ fastProperty "long lines work" prop_parse_long_lines
    , fastProperty "unicode characters work" prop_parse_unicode
    , fastProperty "mixed whitespace works" prop_parse_mixed_whitespace
    ]
  , testGroup "Idempotency"
    [ fastProperty "parse idempotent" prop_parse_idempotent
    ]
  ]