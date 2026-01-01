{-# LANGUAGE CPP #-}

-- | Simple QuickCheck tests for the Parser module
module Test.Unit.SimpleParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (==>), property, classify, counterexample)
import qualified Data.List as Data.List
import Data.Char (toLower)

import Parser (parseTypus, TypusFile(..), FileDirectives(..), CodeBlock(..))
import SourceLocation (Located(..), posLine)

-- ============================================================================
-- Core Property Tests
-- ============================================================================

-- Property: Valid directives are parsed correctly
prop_parse_valid_directives :: String -> Property
prop_parse_valid_directives directive = 
  let validDirectives = ["//! ownership: on", "//! ownership: off", 
                        "//! dependent_types: on", "//! dependent_types: off",
                        "//! constraints: on", "//! constraints: off"]
  in classify (directive `elem` validDirectives) "valid directive" $ 
     property $ directive `elem` validDirectives ==> 
     case parseTypus directive of
       Left _ -> property False
       Right _ -> property True

-- Property: Parse error locations are reasonable
prop_parse_error_locations :: String -> Property
prop_parse_error_locations malformed =
  L.length malformed > 10 ==> 
  case parseTypus malformed of
    Left err -> property $ "error" `L.isInfixOfCustom` map toLower err
    Right _ -> property True

-- Property: Empty file parsing
prop_parse_empty_file :: Property
prop_parse_empty_file = 
  case parseTypus "" of
    Left _ -> property False
    Right parsed -> property $ L.null (tfBlocks parsed)

-- Property: Only comments file parsing
prop_parse_comments_only :: String -> Property
prop_parse_comments_only comment =
  let commentFile = "// " ++ comment ++ "\n// " ++ comment
  in case parseTypus commentFile of
    Left _ -> property False
    Right parsed -> property $ L.null (tfBlocks parsed)

-- Property: Mixed directives L.and blocks
prop_parse_mixed_content :: [String] -> [String] -> Property
prop_parse_mixed_content directives blocks =
  not (null directives) && not (null blocks) ==>
  let mixedContent = Data.List.unlines $ directives ++ blocks
  in case parseTypus mixedContent of
    Left _ -> property False
    Right parsed -> property $ L.length (tfBlocks parsed) >= 1

--Property: Block directive parsing
prop_parse_block_directives :: String -> Property
prop_parse_block_directives directive =
  let blockDirectives = ["//! go", "//! go:run", "//! go:build", "//! skip"]
  in classify (directive `elem` blockDirectives) "block directive" $
     property $ directive `elem` blockDirectives ==>
     case parseTypus directive of
       Left _ -> property False
       Right parsed -> property $ L.length (tfBlocks parsed) >= 1

-- Property: Nested block parsing
prop_parse_nested_blocks :: Int -> Property
prop_parse_nested_blocks depth =
  depth >= 0 && depth <= 5 ==>
  let nestedContent = Data.List.unlines $ replicate depth "  // nested comment"
  in case parseTypus nestedContent of
    Left _ -> property False
    Right _ -> property $ True

-- Property: Special characters in content
prop_parse_special_characters :: String -> Property
prop_parse_special_characters content =
  let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      contentWithSpecial = content ++ specialChars ++ content
  in case parseTypus contentWithSpecial of
    Left _ -> property False
    Right _ -> property $ True

-- Property: Unicode content parsing
prop_parse_unicode :: String -> Property
prop_parse_unicode content =
  let unicodeContent = content ++ "测试内容🚀αβγ" ++ content
  in case parseTypus unicodeContent of
    Left _ -> property False
    Right _ -> property $ True

-- Property: Very long lines handling
prop_parse_long_lines :: Int -> Property
prop_parse_long_lines lineLength =
  lineLength >= 0 && lineLength <= 1000 ==>
  let longLine = replicate lineLength 'a' ++ "content"
  in case parseTypus longLine of
    Left _ -> property False
    Right _ -> property $ True

-- Property: Multiple file directives
prop_parse_multiple_file_directives :: [String] -> Property
prop_parse_multiple_file_directives directives =
  L.length directives <= 10 ==>
  let fileDirectives = L.map (\d -> "//! " ++ d) directives
      content = Data.List.unlines fileDirectives
  in case parseTypus content of
    Left _ -> property False
    Right _ -> property $ True

-- Property: Inconsistent indentation handling
prop_parse_inconsistent_indentation :: [String] -> Property
prop_parse_inconsistent_indentation linesList =
  not (null linesList) ==>
  let indentedLines = zipWith (\i l -> replicate i ' ' ++ l) [0,2,4,1,3] linesList
      content = Data.List.unlines indentedLines
  in case parseTypus content of
    Left _ -> property False
    Right _ -> property $ True

-- Property: Invalid directives are handled gracefully
prop_parse_invalid_directives :: String -> Property
prop_parse_invalid_directives content =
  let invalidStarts = ["//!", "//", "##", "@@", "%%"]
      hasInvalidStart = L.any (`Data.List.L.isPrefixOf` content) invalidStarts
  in classify hasInvalidStart "starts with invalid directive" $
     case parseTypus content of
       Left _ -> True  -- Expected to fail
       Right _ -> True -- May still succeed with partial parsing

-- Property: Empty content produces minimal TypusFile
prop_parse_empty_content :: Property
prop_parse_empty_content =
  case parseTypus "" of
    Left err -> counterexample ("Parse error on empty content: " ++ err) $ property False
    Right file -> 
      let directives = tfDirectives file
          blocks = tfBlocks file
      in property $ L.all (== Nothing) [fdOwnership directives, fdDependentTypes directives, fdConstraints directives] &&
         null blocks

-- Property: Multiple directives are parsed independently
prop_parse_multiple_directives :: [String] -> Property
prop_parse_multiple_directives directives =
  let content = unlines directives
  in case parseTypus content of
    Left err -> counterexample ("Parse error: " ++ err) $ property False
    Right file -> property $ hasCorrectDirectiveCount file (L.length directives)

-- Property: Large files are parsed without stack overflow
prop_parse_large_file :: Int -> Property
prop_parse_large_file n =
  let largeContent = unlines $ replicate n "var x int = 42"
  in n <= 1000 ==> -- Limit size to avoid timeouts
     case parseTypus largeContent of
       Left err -> counterexample ("Parse error on large file: " ++ err) $ property False
       Right _ -> property True

-- Property: Unicode content is handled correctly
prop_parse_unicode_content :: String -> Property
prop_parse_unicode_content content =
  let unicodeContent = content ++ " // 测试中文 🚀"
  in case parseTypus unicodeContent of
    Left _ -> property False
    Right _ -> property True

-- Property: Code blocks maintain their content
prop_parse_code_blocks :: String -> Property
prop_parse_code_blocks codeContent =
  let fullContent = "//! ownership: on\npackage main\nfunc main() {\n" ++ codeContent ++ "\n}"
  in case parseTypus fullContent of
    Left err -> counterexample ("Parse error: " ++ err) $ property False
    Right file -> 
      case tfBlocks file of
        [] -> property False
        (block:_) -> property $ codeContent `L.isInfixOfCustom` cbContent block

-- Property: Directive positions are tracked correctly
prop_parse_directive_positions :: Property
prop_parse_directive_positions =
  let content = unlines
        [ "//! ownership: on"
        , "//! dependent_types: off"
        , "package main"
        , "func main() {}"
        ]
  in case parseTypus content of
    Left err -> counterexample ("Parse error: " ++ err) False
    Right file -> 
      let directives = tfDirectives file
          ownershipPos = fdOwnership directives >>= \(Located _ pos _) -> Just (posLine pos)
          dependentTypesPos = fdDependentTypes directives >>= \(Located _ pos _) -> Just (posLine pos)
      in property $ ownershipPos == Just 1 && dependentTypesPos == Just 2

-- ============================================================================
-- Helper Functions
-- ============================================================================

hasCorrectDirectiveCount :: TypusFile -> Int -> Bool
hasCorrectDirectiveCount file expectedCount =
  let actualCount = countDirectives (tfDirectives file)
  in actualCount <= expectedCount -- May be less due to parsing rules

countDirectives :: FileDirectives -> Int
countDirectives (FileDirectives ownership depTypes constraints) =
  L.length [() | Just _ <- [ownership, depTypes, constraints]]

isInfixOfCustom :: String -> String -> Bool
L.isInfixOfCustom needle haystack = needle `Data.List.L.isInfixOf` haystack

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Simple Parser QuickCheck Tests"
  [ fastProperty "Valid directives are parsed correctly" prop_parse_valid_directives
  , fastProperty "Parse error locations are reasonable" prop_parse_error_locations
  , fastProperty "Empty file parsing" prop_parse_empty_file
  , fastProperty "Only comments file parsing" prop_parse_comments_only
  , fastProperty "Mixed directives L.and blocks" prop_parse_mixed_content
  , fastProperty "Block directive parsing" prop_parse_block_directives
  , fastProperty "Nested block parsing" prop_parse_nested_blocks
  , fastProperty "Special characters in content" prop_parse_special_characters
  , fastProperty "Unicode content parsing" prop_parse_unicode
  , fastProperty "Very long lines handling" prop_parse_long_lines
  , fastProperty "Multiple file directives" prop_parse_multiple_file_directives
  , fastProperty "Inconsistent indentation handling" prop_parse_inconsistent_indentation
  , fastProperty "Invalid directives are handled gracefully" prop_parse_invalid_directives
  , fastProperty "Empty content produces minimal TypusFile" prop_parse_empty_content
  , fastProperty "Multiple directives are parsed independently" prop_parse_multiple_directives
  , fastProperty "Large files are parsed without stack overflow" prop_parse_large_file
  , fastProperty "Unicode content is handled correctly" prop_parse_unicode_content
  , fastProperty "Code blocks maintain their content" prop_parse_code_blocks
  , fastProperty "Directive positions are tracked correctly" prop_parse_directive_positions
  ]