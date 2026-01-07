module Test.Unit.ParserDirectiveSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser
import SourceLocation

-- Test file directives parsing
prop_file_directives_roundtrip :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Property
prop_file_directives_roundtrip ownership dependentTypes constraints =
  let directives = FileDirectives 
        { fdOwnership = fmap (locatedAt startPos) ownership
        , fdDependentTypes = fmap (locatedAt startPos) dependentTypes
        , fdConstraints = fmap (locatedAt startPos) constraints
        }
  in property $ directives === directives

-- Test block directives parsing
prop_block_directives_roundtrip :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Property
prop_block_directives_roundtrip ownership dependentTypes constraints =
  let directives = BlockDirectives 
        { bdOwnership = fmap (locatedAt startPos) ownership
        , bdDependentTypes = fmap (locatedAt startPos) dependentTypes
        , bdConstraints = fmap (locatedAt startPos) constraints
        }
  in property $ directives === directives

-- Test default directives
prop_default_file_directives_consistency :: Property
prop_default_file_directives_consistency =
  let defaults = defaultFileDirectives
  in property $ 
    fdOwnership defaults === Nothing &&
    fdDependentTypes defaults === Nothing &&
    fdConstraints defaults === Nothing

prop_default_block_directives_consistency :: Property
prop_default_block_directives_consistency =
  let defaults = defaultBlockDirectives
  in property $ 
    bdOwnership defaults === Nothing &&
    bdDependentTypes defaults === Nothing &&
    bdConstraints defaults === Nothing

tests :: TestTree
tests = testGroup "Parser Directive Tests"
  [ testProperty "file directives roundtrip" prop_file_directives_roundtrip
  , testProperty "block directives roundtrip" prop_block_directives_roundtrip
  , testProperty "default file directives consistency" prop_default_file_directives_consistency
  , testProperty "default block directives consistency" prop_default_block_directives_consistency
  ]