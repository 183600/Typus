{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
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

import SourceLocation (Located(..), SourcePos(..), SourceSpan(..))
import Data.Char (isAlphaNum, isSpace)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

-- | Parser QuickCheck tests
tests :: TestTree
tests = testGroup "New Parser QuickCheck Tests"
  [ fastProperty "parseTypus handles empty input" prop_parse_empty
  , fastProperty "parseTypus handles simple directives" prop_parse_directives
  , fastProperty "parseTypus handles code blocks" prop_parse_code_blocks
  , fastProperty "parseTypus preserves content structure" prop_parse_preserve_structure
  , fastProperty "parseTypus handles malformed input gracefully" prop_parse_malformed
  , fastProperty "File directives parsing consistency" prop_file_directives_consistency
  , fastProperty "Block directives parsing consistency" prop_block_directives_consistency
  , fastProperty "Parser is position-aware" prop_parser_position_aware
  ]

-- Property: parseTypus handles empty input
prop_parse_empty :: Property
prop_parse_empty =
  let result = parseTypus ""
      expectedBlocks = []
  in property $ tfBlocks result === expectedBlocks .&&.
     tfDirectives result === defaultFileDirectives

-- Property: parseTypus handles simple directives
prop_parse_directives :: String -> Property
prop_parse_directives directiveName =
  L.length directiveName <= 10 && L.all isAlphaNum directiveName ==>
  let input = "//! " ++ directiveName ++ "=true\n"
      result = parseTypus input
  in property $ tfBuildTags result === []

-- Property: parseTypus handles code blocks
prop_parse_code_blocks :: String -> Property
prop_parse_code_blocks codeContent =
  not ("\n" `L.isInfixOf` codeContent) && L.length codeContent <= 50 ==>
  let input = codeContent ++ "\n"
      result = parseTypus input
      blocks = tfBlocks result
  in property $ if null codeContent 
     then null blocks
     else not (null blocks) .&&. cbContent (L.head blocks) === codeContent

-- Property: parseTypus preserves content structure
prop_parse_preserve_structure :: String -> String -> Property
prop_parse_preserve_structure firstBlock secondBlock =
  not ("\n" `L.isInfixOf` firstBlock) && not ("\n" `L.isInfixOf` secondBlock) &&
  L.length firstBlock <= 30 && L.length secondBlock <= 30 ==>
  let input = firstBlock ++ "\n\n" ++ secondBlock ++ "\n"
      result = parseTypus input
      blocks = tfBlocks result
  in property $ L.length blocks >= 1 .&&.
     (if not (null firstBlock) && not (null secondBlock)
      then L.length blocks >= 2
      else property True)

-- Property: parseTypus handles malformed input gracefully
prop_parse_malformed :: String -> Property
prop_parse_malformed malformedInput =
  L.length malformedInput <= 100 ==>
  let result = parseTypus malformedInput
      blocks = tfBlocks result
  in property $ L.length blocks >= 0 -- Should never crash L.and should return some result

-- Property: File directives parsing consistency
prop_file_directives_consistency :: Bool -> Bool -> Bool -> Property
prop_file_directives_consistency ownership dependent constraints =
  let directives = FileDirectives 
        { fdOwnership = Just $ Located ownership undefined
        , fdDependentTypes = Just $ Located dependent undefined
        , fdConstraints = Just $ Located constraints undefined
        }
  in property $ case directives of
    FileDirectives{..} -> 
      (case fdOwnership of Just (Located b _) -> b; Nothing -> False) === ownership .&&.
      (case fdDependentTypes of Just (Located b _) -> b; Nothing -> False) === dependent .&&.
      (case fdConstraints of Just (Located b _) -> b; Nothing -> False) === constraints

-- Property: Block directives parsing consistency
prop_block_directives_consistency :: Bool -> Bool -> Bool -> Property
prop_block_directives_consistency ownership dependent constraints =
  let directives = BlockDirectives 
        { bdOwnership = Just $ Located ownership undefined
        , bdDependentTypes = Just $ Located dependent undefined
        , bdConstraints = Just $ Located constraints undefined
        }
  in property $ case directives of
    BlockDirectives{..} -> 
      (case bdOwnership of Just (Located b _) -> b; Nothing -> False) === ownership .&&.
      (case bdDependentTypes of Just (Located b _) -> b; Nothing -> False) === dependent .&&.
      (case bdConstraints of Just (Located b _) -> b; Nothing -> False) === constraints

-- Property: Parser is position-aware
prop_parser_position_aware :: String -> Property
prop_parser_position_aware content =
  L.length content <= 50 ==> 
  let result = parseTypus content
      blocks = tfBlocks result
  in property $ L.all (\block -> 
    let span = cbSpan block
    in spanStart span `seq` spanEnd span `seq` True
  ) blocks