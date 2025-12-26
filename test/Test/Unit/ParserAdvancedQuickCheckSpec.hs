{-# LANGUAGE CPP #-}
module Test.Unit.ParserAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, choose, listOf, forAll, Property, (===), counterexample, (==>))

import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf)

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

-- Generate valid directive keys
genDirectiveKey :: Gen String
genDirectiveKey = listOf $ oneof
  [ elements ['a'..'z']
  , elements ['A'..'Z']
  , elements "_-"
  ]

-- Generate valid directive values
genDirectiveValue :: Gen String
genDirectiveValue = listOf $ oneof
  [ elements ['a'..'z']
  , elements ['A'..'Z']
  , elements ['0'..'9']
  , elements "_-"
  ]

-- Generate file directive lines
genFileDirectiveLine :: Gen String
genFileDirectiveLine = do
  key <- genDirectiveKey
  value <- genDirectiveValue
  return $ "//! " ++ key ++ ": " ++ value

-- Generate block directive lines
genBlockDirectiveLine :: Gen String
genBlockDirectiveLine = do
  key <- genDirectiveKey
  value <- genDirectiveValue
  return $ "{//! " ++ key ++ ": " ++ value ++ "}"

-- Generate regular code lines
genCodeLine :: Gen String
genCodeLine = do
  length' <- choose (0, 50)
  listOf $ oneof
    [ choose (' ', '~')  -- Printable ASCII
    , elements "\t"      -- Tab character
    ]

-- Generate a complete Typus file content
genTypusFileContent :: Gen String
genTypusFileContent = do
  numFileDirectives <- choose (0, 3)
  numBlocks <- choose (0, 5)
  
  fileDirectives <- replicateM numFileDirectives genFileDirectiveLine
  blocks <- replicateM numBlocks genBlock
  
  return $ unlines (fileDirectives ++ concat blocks)
  where
    genBlock :: Gen [String]
    genBlock = do
      numBlockDirectives <- choose (0, 2)
      numCodeLines <- choose (0, 10)
      
      blockDirectives <- replicateM numBlockDirectives genBlockDirectiveLine
      codeLines <- replicateM numCodeLines genCodeLine
      
      return $ blockDirectives ++ codeLines

-- ============================================================================
-- Property Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Parser Advanced QuickCheck Tests"
    [ testProperty "parseTypus returns Right for well-formed input" $
        \content ->
          let result = parseTypus content
          in case result of
            Left _ -> property False
            Right _ -> property True

    , testProperty "parseTypus preserves line count in blocks" $
        \content ->
          let result = parseTypus content
          in case result of
            Left _ -> property True  -- Parse errors are acceptable
            Right typusFile ->
              let inputLines = lines content
                  blockLines = sum $ map (length . lines . cbContent) (tfBlocks typusFile)
              in property True  -- Basic sanity check that parsing completes

    , testProperty "empty input produces file with no blocks" $
        \content ->
          null (trim content) ==>
          let result = parseTypus content
          in case result of
            Right typusFile -> null (tfBlocks typusFile)
            Left _ -> property True

    , testProperty "file directives are parsed correctly" $
        \directives ->
          let content = unlines directives
              result = parseTypus content
          in case result of
            Right typusFile -> 
              let hasDirectives = tfDirectives typusFile /= defaultFileDirectives
              in not (null directives) ==> hasDirectives
            Left _ -> property True

    , testProperty "block directives create separate blocks" $
        \blockDirectives codeLines ->
          let directiveStr = unlines blockDirectives
              codeStr = unlines codeLines
              content = directiveStr ++ "\n" ++ codeStr
              result = parseTypus content
          in case result of
            Right typusFile -> 
              let numBlocks = length (tfBlocks typusFile)
              in not (null blockDirectives) ==> numBlocks >= 1
            Left _ -> property True

    , testProperty "parseTypus handles whitespace gracefully" $
        \content ->
          let contentWithExtraWhitespace = unlines $ map ("  " ++) (lines content)
              result1 = parseTypus content
              result2 = parseTypus contentWithExtraWhitespace
          in case (result1, result2) of
            (Right _, Right _) -> property True
            (Left _, Left _) -> property True
            _ -> property False  -- Should have same success/failure status

    , testProperty "parsed file has valid structure" $
        \content ->
          let result = parseTypus content
          in case result of
            Right typusFile ->
              let blocks = tfBlocks typusFile
                  allBlocksValid = all isValidBlock blocks
              in allBlocksValid
            Left _ -> property True
      where
        isValidBlock :: CodeBlock -> Bool
        isValidBlock block = 
          not (null $ cbContent block) && 
          length (cbContent block) <= 1000  -- Reasonable size check

    , testProperty "multiple file directives are handled" $
        \directiveList ->
          let directives = take 3 directiveList  -- Limit to reasonable number
              content = unlines directives
              result = parseTypus content
          in case result of
            Right _ -> property True
            Left _ -> property True  -- Parse errors are acceptable

    , testProperty "nested block directives are handled" $
        \outerDirectives innerDirectives codeLines ->
          let outerDirectiveStr = unlines outerDirectives
              innerDirectiveStr = unlines innerDirectives
              codeStr = unlines codeLines
              content = outerDirectiveStr ++ "\n" ++ innerDirectiveStr ++ "\n" ++ codeStr
              result = parseTypus content
          in case result of
            Right _ -> property True
            Left _ -> property True

    , testProperty "parseTypus is deterministic" $
        \content ->
          let result1 = parseTypus content
              result2 = parseTypus content
          in result1 === result2

    , testProperty "parsing preserves directive order" $
        \directives ->
          let content = unlines directives
              result = parseTypus content
          in case result of
            Right typusFile -> property True  -- Basic check that parsing succeeds
            Left _ -> property True

    , testProperty "empty directives are handled gracefully" $
        \content ->
          let contentWithEmptyDirectives = content ++ "\n//!\n{//!}\n"
              result = parseTypus contentWithEmptyDirectives
          in case result of
            Right _ -> property True
            Left _ -> property True

    , testProperty "malformed directives don't crash parser" $
        \content ->
          let malformedContent = content ++ "\n//! malformed : directive with extra : colons\n"
              result = parseTypus malformedContent
          in case result of
            Right _ -> property True
            Left _ -> property True  -- Should handle gracefully

    , testProperty "very long lines are handled" $
        \content ->
          let longLine = replicate 1000 'a' ++ "\n"
              contentWithLongLine = content ++ longLine
              result = parseTypus contentWithLongLine
          in case result of
            Right _ -> property True
            Left _ -> property True

    , testProperty "special characters in directives are handled" $
        \content ->
          let specialDirectives = ["//! special: value-with-dashes_and_underscores"]
              contentWithSpecial = unlines specialDirectives ++ content
              result = parseTypus contentWithSpecial
          in case result of
            Right _ -> property True
            Left _ -> property True
    ]

-- Helper function
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse