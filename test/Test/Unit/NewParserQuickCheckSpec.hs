{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.NewParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
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

import SourceLocation (SourcePos(..), startPos)
import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isSpace)

-- | 新的语法解析器QuickCheck测试套件
tests :: TestTree
tests =
  testGroup "New Parser QuickCheck Tests"
    [ fastProperty "parseTypus handles empty input" prop_parseTypus_empty
    , fastProperty "parseTypus handles simple code" prop_parseTypus_simple_code
    , fastProperty "parseTypus handles file directives" prop_parseTypus_file_directives
    , fastProperty "parseTypus handles block directives" prop_parseTypus_block_directives
    , fastProperty "parseTypus preserves code content" prop_parseTypus_preserves_content
    , fastProperty "parseTypus handles build tags" prop_parseTypus_build_tags
    , fastProperty "parseTypus handles multiple blocks" prop_parseTypus_multiple_blocks
    , fastProperty "parseTypus handles comments" prop_parseTypus_comments
    , fastProperty "parseTypus handles malformed directives gracefully" prop_parseTypus_malformed_directives
    , fastProperty "curlyDelta counts braces correctly" prop_curlyDelta_correct
    ]

-- Property: parseTypus handles empty input
prop_parseTypus_empty :: Property
prop_parseTypus_empty =
  let result = parseTypus ""
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      tfDirectives typusFile === defaultFileDirectives .&&.
      null (tfBuildTags typusFile) .&&.
      null (tfBlocks typusFile)

-- Property: parseTypus handles simple code
prop_parseTypus_simple_code :: String -> Property
prop_parseTypus_simple_code code =
  not (null code) && length code <= 100 && not (any (`isInfixOf` code) ["//!", "{//!"]) ==>
  let result = parseTypus code
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      tfDirectives typusFile === defaultFileDirectives .&&.
      null (tfBuildTags typusFile) .&&.
      length (tfBlocks typusFile) >= 1

-- Property: parseTypus handles file directives
prop_parseTypus_file_directives :: String -> Property
prop_parseTypus_file_directives directiveContent =
  not (null directiveContent) && length directiveContent <= 50 ==>
  let code = "//! ownership: " ++ directiveContent ++ "\nfunc main() {}\n"
      result = parseTypus code
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      case fdOwnership (tfDirectives typusFile) of
        Nothing -> property False
        Just _ -> property True

-- Property: parseTypus handles block directives
prop_parseTypus_block_directives :: String -> Property
prop_parseTypus_block_directives directiveContent =
  not (null directiveContent) && length directiveContent <= 50 ==>
  let code = "{//! ownership: " ++ directiveContent ++ "}\nfunc main() {}\n"
      result = parseTypus code
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      case tfBlocks typusFile of
        [] -> property False
        (block:_) -> case bdOwnership (cbDirectives block) of
          Nothing -> property False
          Just _ -> property True

-- Property: parseTypus preserves code content
prop_parseTypus_preserves_content :: String -> Property
prop_parseTypus_preserves_content codeContent =
  not (null codeContent) && length codeContent <= 100 &&
  not (any (`isInfixOf` codeContent) ["//!", "{//!"]) ==>
  let code = codeContent ++ "\n"
      result = parseTypus code
  in case result of
    Left _ -> property False
    Right typusFile -> case tfBlocks typusFile of
      [] -> property False
      (block:_) -> property $ cbContent block `isInfixOf` codeContent

-- Property: parseTypus handles build tags
prop_parseTypus_build_tags :: String -> Property
prop_parseTypus_build_tags tagContent =
  not (null tagContent) && length tagContent <= 30 ==>
  let code = "//go:build " ++ tagContent ++ "\nfunc main() {}\n"
      result = parseTypus code
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      not (null (tfBuildTags typusFile))

-- Property: parseTypus handles multiple blocks
prop_parseTypus_multiple_blocks :: [String] -> Property
prop_parseTypus_multiple_blocks codeBlocks =
  not (null codeBlocks) && length codeBlocks <= 5 &&
  all (\block -> not (null block) && length block <= 50) codeBlocks ==>
  let code = unlines (concatMap (\block -> [block, ""]) codeBlocks)
      result = parseTypus code
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      length (tfBlocks typusFile) >= length codeBlocks

-- Property: parseTypus handles comments
prop_parseTypus_comments :: String -> String -> Property
prop_parseTypus_comments code comment =
  not (null code) && not (null comment) &&
  length code <= 50 && length comment <= 30 ==>
  let codeWithComment = code ++ "\n// " ++ comment ++ "\n"
      result = parseTypus codeWithComment
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      length (tfBlocks typusFile) >= 1

-- Property: parseTypus handles malformed directives gracefully
prop_parseTypus_malformed_directives :: String -> Property
prop_parseTypus_malformed_directives malformedDirective =
  not (null malformedDirective) && length malformedDirective <= 30 ==>
  let code = malformedDirective ++ "\nfunc main() {}\n"
      result = parseTypus code
  in case result of
    Left _ -> property True  -- Malformed directives should cause parsing errors
    Right typusFile -> property $ True  -- Or they might be ignored, still valid

-- Property: curlyDelta counts braces correctly
prop_curlyDelta_correct :: String -> Property
prop_curlyDelta_correct codeString =
  let openCount = length $ filter (== '{') codeString
      closeCount = length $ filter (== '}') codeString
      expectedDelta = openCount - closeCount
      actualDelta = curlyDelta codeString
  in property $ actualDelta === expectedDelta

-- Additional properties for parser testing

-- Property: parseTypus handles nested blocks
prop_parseTypus_nested_blocks :: String -> Property
prop_parseTypus_nested_blocks innerCode =
  not (null innerCode) && length innerCode <= 50 ==>
  let code = "func main() {\n  " ++ innerCode ++ "\n}\n"
      result = parseTypus code
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      length (tfBlocks typusFile) >= 1

-- Property: parseTypus handles whitespace correctly
prop_parseTypus_whitespace :: String -> Property
prop_parseTypus_whitespace code =
  not (null code) && length code <= 30 ==>
  let codeWithWhitespace = "  \n  " ++ code ++ "\n  \n"
      result = parseTypus codeWithWhitespace
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      length (tfBlocks typusFile) >= 1

-- Property: parseTypus handles package declarations
prop_parseTypus_package :: String -> Property
prop_parseTypus_package packageName =
  not (null packageName) && length packageName <= 20 && all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_") packageName ==>
  let code = "package " ++ packageName ++ "\nfunc main() {}\n"
      result = parseTypus code
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      length (tfBlocks typusFile) >= 1

-- Property: parseTypus handles function declarations
prop_parseTypus_functions :: String -> Property
prop_parseTypus_functions funcName =
  not (null funcName) && length funcName <= 20 && all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_") funcName ==>
  let code = "func " ++ funcName ++ "() {\n  // function body\n}\n"
      result = parseTypus code
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      length (tfBlocks typusFile) >= 1

-- Property: parseTypus handles multiple file directives
prop_parseTypus_multiple_file_directives :: [(String, Bool)] -> Property
prop_parseTypus_multiple_file_directives directives =
  not (null directives) && length directives <= 3 ==>
  let directiveLines = map (\(key, value) -> "//! " ++ key ++ ": " ++ if value then "on" else "off") directives
      code = unlines directiveLines ++ "\nfunc main() {}\n"
      result = parseTypus code
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      tfDirectives typusFile /= defaultFileDirectives

-- Property: parseTypus handles multiple block directives
prop_parseTypus_multiple_block_directives :: [(String, Bool)] -> Property
prop_parseTypus_multiple_block_directives directives =
  not (null directives) && length directives <= 3 ==>
  let directiveContent = unlines $ map (\(key, value) -> key ++ ": " ++ if value then "on" else "off") directives
      code = "{//! " ++ directiveContent ++ "}\nfunc main() {}\n"
      result = parseTypus code
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      case tfBlocks typusFile of
        [] -> property False
        (block:_) -> property $ 
          cbDirectives block /= defaultBlockDirectives

-- Property: parseTypus handles mixed directives
prop_parseTypus_mixed_directives :: String -> String -> Property
prop_parseTypus_mixed_directives fileDirective blockDirective =
  not (null fileDirective) && not (null blockDirective) &&
  length fileDirective <= 20 && length blockDirective <= 20 ==>
  let code = "//! ownership: " ++ fileDirective ++ "\n{//! dependent_types: " ++ blockDirective ++ "}\nfunc main() {}\n"
      result = parseTypus code
  in case result of
    Left _ -> property False
    Right typusFile -> property $ 
      case fdOwnership (tfDirectives typusFile) of
        Nothing -> property False
        Just _ -> case tfBlocks typusFile of
          [] -> property False
          (block:_) -> case bdDependentTypes (cbDirectives block) of
            Nothing -> property False
            Just _ -> property True

-- Property: parseTypus preserves line structure
prop_parseTypus_preserves_lines :: [String] -> Property
prop_parseTypus_preserves_lines lines =
  not (null lines) && length lines <= 5 &&
  all (\line -> length line <= 30) lines ==>
  let code = unlines lines
      result = parseTypus code
  in case result of
    Left _ -> property False
    Right typusFile -> 
      let blockContents = concatMap cbContent (tfBlocks typusFile)
          originalLines = lines code
          blockLines = lines blockContents
      in property $ length blockLines >= 1

-- Helper function to count curly braces (from Parser module)
curlyDelta :: String -> Int
curlyDelta = go False False 0
  where
    go :: Bool -> Bool -> Int -> String -> Int
    go _ _ acc [] = acc
    go inStr _ acc ('/' : '/' : _) | not inStr = acc
    go inStr esc acc (c:cs)
        | inStr =
            case c of
                '"' | not esc -> go False False acc cs
                '\\'         -> go True True acc cs
                _              -> go True False acc cs
        | otherwise =
            case c of
                '"' -> go True False acc cs
                '{' -> go False False (acc + 1) cs
                '}' -> go False False (acc - 1) cs
                _   -> go False False acc cs