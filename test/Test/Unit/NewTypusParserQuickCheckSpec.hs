{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewTypusParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Parser (parseTypusFile, TypusFile(..))
import Utils (trim, removeComments)

-- Property: Parser produces valid file structure
prop_parser_produces_valid_structure :: String -> Property
prop_parser_produces_valid_structure input =
  let result = parseTypusFile "test.typus" input
      isValid = either (const False) (const True) result
  in classify (L.length input > 0) "non-empty input" $
     property $ isValid

-- Property: Comment removal preserves meaningful content
prop_comment_removal_preserves_content :: String -> String -> Property
prop_comment_removal_preserves_content code comments =
  let codeWithComments = code ++ "\n// " ++ comments
      withoutComments = removeComments codeWithComments
      codeTrimmed = trim code
      withoutCommentsTrimmed = trim withoutComments
  in property $ not (null code) ==> (codeTrimmed `L.isInfixOf` withoutCommentsTrimmed)

-- Property: Parser handles empty input gracefully
prop_parser_handles_empty_input :: Property
prop_parser_handles_empty_input =
  let result = parseTypusFile "empty.typus" ""
      isEmpty = either (const False) (\file -> L.null (getFileBlocks file)) result
  in property $ isEmpty

-- Property: Parser preserves file directives
prop_parser_preserves_directives :: Bool -> Bool -> String -> Property
prop_parser_preserves_directives ownership constraints content =
  let input = if ownership then "//! ownership: on\n" else "" ++
              if constraints then "//! dependent_types: on\n" else "" ++
              content
      result = parseTypusFile "directives.typus" input
      extractedOwnership = either (const False) getFileOwnership result
      extractedConstraints = either (const False) getFileConstraints result
  in property $ (extractedOwnership === ownership) .&&. (extractedConstraints === constraints)

-- Property: Parser handles nested blocks
prop_parser_handles_nested_blocks :: String -> String -> Property
prop_parser_handles_nested_blocks outer inner =
  let input = outer ++ "\n{//! ownership: on\n" ++ inner ++ "\n}\n"
      result = parseTypusFile "nested.typus" input
      hasBlocks = either (const False) (not . null . getFileBlocks) result
  in property $ hasBlocks

-- Helper functions
isInfixOf :: String -> String -> Bool
L.isInfixOf = undefined  -- Simplified for test

getFileBlocks :: TypusFile -> [CodeBlock]
getFileBlocks (TypusFile _ _ _ blocks) = blocks

getFileOwnership :: TypusFile -> Bool
getFileOwnership (TypusFile _ ownership _ _) = ownership

getFileConstraints :: TypusFile -> Bool
getFileConstraints (TypusFile _ _ constraints _) = constraints

tests :: TestTree
tests = testGroup "New Typus Parser QuickCheck Tests"
  [ fastProperty "Parser produces valid structure" prop_parser_produces_valid_structure
  , fastProperty "Comment removal preserves content" prop_comment_removal_preserves_content
  , fastProperty "Parser handles empty input" prop_parser_handles_empty_input
  , fastProperty "Parser preserves directives" prop_parser_preserves_directives
  , fastProperty "Parser handles nested blocks" prop_parser_handles_nested_blocks
  ]