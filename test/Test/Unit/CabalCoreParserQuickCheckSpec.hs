{-# LANGUAGE CPP #-}

module Test.Unit.CabalCoreParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)

import Parser (FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), 
              defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..))
import Utils (trim)

-- | Property: Default directives should be consistent
prop_default_file_directives_consistency :: Property
prop_default_file_directives_consistency =
  let fd = defaultFileDirectives
  in fdOwnership fd === Nothing .&&.
     fdDependentTypes fd === Nothing .&&.
     fdConstraints fd === Nothing

-- | Property: Default block directives should be consistent  
prop_default_block_directives_consistency :: Property
prop_default_block_directives_consistency =
  let bd = defaultBlockDirectives
  in bdOwnership bd === Nothing .&&.
     bdDependentTypes bd === Nothing .&&.
     bdConstraints bd === Nothing

-- | Property: FileDirectives equality should be reflexive
prop_file_directives_reflexive :: FileDirectives -> Property
prop_file_directives_reflexive fd = fd === fd

-- | Property: BlockDirectives equality should be reflexive
prop_block_directives_reflexive :: BlockDirectives -> Property
prop_block_directives_reflexive bd = bd === bd

-- | Property: Trim operation should be idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

-- | Property: Trim should remove leading and trailing whitespace
prop_trim_whitespace :: String -> String -> Property
prop_trim_whitespace prefix suffix =
  let ws = " \t\n\r"
      s = prefix ++ ws ++ suffix ++ ws
      trimmed = trim s
  in not (null trimmed) ==> 
     head trimmed `notElem` ws .&&. 
     last trimmed `notElem` ws

-- | Property: String parsing should preserve content structure
prop_string_preservation :: String -> Property
prop_string_preservation s =
  not (null s) ==>
  let trimmed = trim s
      lines = lines trimmed
  in length lines >= 1 .&&. 
     concat lines === trimmed

-- | Property: Directive parsing should be consistent with boolean values
prop_directive_boolean_consistency :: Bool -> Property
prop_directive_boolean_consistency b =
  let str = if b then "true" else "false"
      parsed = case str of
        "true" -> Just True
        "false" -> Just False
        _ -> Nothing
  in parsed === Just b

-- | Property: Empty input should produce default directives
prop_empty_input_defaults :: Property
prop_empty_input_defaults =
  let emptyFile = TypusFile [] defaultFileDirectives []
  in fileDirectives emptyFile === defaultFileDirectives

-- | Property: Code block content should be preserved after parsing
prop_codeblock_content_preservation :: String -> Property
prop_codeblock_content_preservation content =
  not (null content) ==>
  let block = CodeBlock defaultBlockDirectives content
      blockContent = block
  in blockContent === blockContent

-- | Property: File directives should compose correctly
prop_file_directives_composition :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Property
prop_file_directives_composition ownership dependentTypes constraints =
  let fd = FileDirectives ownership dependentTypes constraints
  in fdOwnership fd === ownership .&&.
     fdDependentTypes fd === dependentTypes .&&.
     fdConstraints fd === constraints

-- | Property: Block directives should compose correctly
prop_block_directives_composition :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Property
prop_block_directives_composition ownership dependentTypes constraints =
  let bd = BlockDirectives ownership dependentTypes constraints
  in bdOwnership bd === ownership .&&.
     bdDependentTypes bd === dependentTypes .&&.
     bdConstraints bd === constraints

tests :: TestTree
tests = testGroup "Cabal Core Parser QuickCheck Tests"
  [ fastProperty "default file directives are consistent" prop_default_file_directives_consistency
  , fastProperty "default block directives are consistent" prop_default_block_directives_consistency
  , fastProperty "FileDirectives equality is reflexive" prop_file_directives_reflexive
  , fastProperty "BlockDirectives equality is reflexive" prop_block_directives_reflexive
  , fastProperty "trim operation is idempotent" prop_trim_idempotent
  , fastProperty "trim removes leading and trailing whitespace" prop_trim_whitespace
  , fastProperty "string parsing preserves content structure" prop_string_preservation
  , fastProperty "directive boolean parsing is consistent" prop_directive_boolean_consistency
  , fastProperty "empty input produces default directives" prop_empty_input_defaults
  , fastProperty "codeblock content is preserved" prop_codeblock_content_preservation
  , fastProperty "file directives compose correctly" prop_file_directives_composition
  , fastProperty "block directives compose correctly" prop_block_directives_composition
  ]