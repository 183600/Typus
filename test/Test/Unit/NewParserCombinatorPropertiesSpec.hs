{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewParserCombinatorPropertiesSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (property) as QC
import Parser
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import SourceLocation (Located(..), SourcePos(..), startPos)
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf)

-- ============================================================================
-- Parser Module QuickCheck Property Tests
-- ============================================================================

-- | Test that default file directives have L.all fields as Nothing
prop_default_file_directives_nothing :: Bool
prop_default_file_directives_nothing = 
    let FileDirectives{..} = defaultFileDirectives
    in fdOwnership == Nothing &&
       fdDependentTypes == Nothing &&
       fdConstraints == Nothing

-- | Test that default block directives have L.all fields as Nothing
prop_default_block_directives_nothing :: Bool
prop_default_block_directives_nothing = 
    let BlockDirectives{..} = defaultBlockDirectives
    in bdOwnership == Nothing &&
       bdDependentTypes == Nothing &&
       bdConstraints == Nothing

-- | Test that file directives with L.all Nothing are equal to default
prop_file_directives_all_nothing_equals_default :: Bool
prop_file_directives_all_nothing_equals_default = 
    let customDirectives = FileDirectives Nothing Nothing Nothing
    in customDirectives == defaultFileDirectives

-- | Test that block directives with L.all Nothing are equal to default
prop_block_directives_all_nothing_equals_default :: Bool
prop_block_directives_all_nothing_equals_default = 
    let customDirectives = BlockDirectives Nothing Nothing Nothing
    in customDirectives == defaultBlockDirectives

-- | Test that file directives equality works correctly
prop_file_directives_equality :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Bool
prop_file_directives_equality ownership deps constraints = 
    let directives1 = FileDirectives ownership deps constraints
        directives2 = FileDirectives ownership deps constraints
    in directives1 == directives2

-- | Test that block directives equality works correctly
prop_block_directives_equality :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Bool
prop_block_directives_equality ownership deps constraints = 
    let directives1 = BlockDirectives ownership deps constraints
        directives2 = BlockDirectives ownership deps constraints
    in directives1 == directives2

-- | Test that file directives with different ownership are not equal
prop_file_directives_different_ownership :: Maybe Bool -> Maybe Bool -> Bool
prop_file_directives_different_ownership ownership1 ownership2 = 
    ownership1 /= ownership2 ==> 
    let directives1 = FileDirectives ownership1 Nothing Nothing
        directives2 = FileDirectives ownership2 Nothing Nothing
    in directives1 /= directives2

-- | Test that block directives with different dependent types are not equal
prop_block_directives_different_deps :: Maybe Bool -> Maybe Bool -> Bool
prop_block_directives_different_deps deps1 deps2 = 
    deps1 /= deps2 ==> 
    let directives1 = BlockDirectives Nothing deps1 Nothing
        directives2 = BlockDirectives Nothing deps2 Nothing
    in directives1 /= directives2

-- | Test that located value preserves the value
prop_located_value_preserves_value :: String -> SourcePos -> Bool
prop_located_value_preserves_value value pos = 
    let located = Located value pos
    in locatedValue located == value

-- | Test that located value preserves the position
prop_located_value_preserves_position :: String -> SourcePos -> Bool
prop_located_value_preserves_position value pos = 
    let located = Located value pos
    in locatedPos located == pos

-- | Test that located values with same value L.and position are equal
prop_located_equality :: String -> SourcePos -> Bool
prop_located_equality value pos = 
    let located1 = Located value pos
        located2 = Located value pos
    in located1 == located2

-- | Test that located values with different values are not equal
prop_located_different_values :: String -> String -> SourcePos -> Bool
prop_located_different_values value1 value2 pos = 
    value1 /= value2 ==> 
    let located1 = Located value1 pos
        located2 = Located value2 pos
    in located1 /= located2

-- | Test that located values with different positions are not equal
prop_located_different_positions :: String -> SourcePos -> SourcePos -> Bool
prop_located_different_positions value pos1 pos2 = 
    pos1 /= pos2 ==> 
    let located1 = Located value pos1
        located2 = Located value pos2
    in located1 /= located2

-- | Test that file directives show is deterministic
prop_file_directives_show_deterministic :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Bool
prop_file_directives_show_deterministic ownership deps constraints = 
    let directives = FileDirectives ownership deps constraints
        show1 = show directives
        show2 = show directives
    in show1 == show2

-- | Test that block directives show is deterministic
prop_block_directives_show_deterministic :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Bool
prop_block_directives_show_deterministic ownership deps constraints = 
    let directives = BlockDirectives ownership deps constraints
        show1 = show directives
        show2 = show directives
    in show1 == show2

-- ============================================================================
-- Test Collection
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Parser Combinator Properties QuickCheck Tests"
  [ QC.testProperty "default file directives have L.all fields as Nothing" prop_default_file_directives_nothing
  , QC.testProperty "default block directives have L.all fields as Nothing" prop_default_block_directives_nothing
  , QC.testProperty "file directives with L.all Nothing equals default" prop_file_directives_all_nothing_equals_default
  , QC.testProperty "block directives with L.all Nothing equals default" prop_block_directives_all_nothing_equals_default
  , QC.testProperty "file directives equality works correctly" prop_file_directives_equality
  , QC.testProperty "block directives equality works correctly" prop_block_directives_equality
  , QC.testProperty "file directives with different ownership are not equal" prop_file_directives_different_ownership
  , QC.testProperty "block directives with different dependent types are not equal" prop_block_directives_different_deps
  , QC.testProperty "located value preserves the value" prop_located_value_preserves_value
  , QC.testProperty "located value preserves the position" prop_located_value_preserves_position
  , QC.testProperty "located values with same value L.and position are equal" prop_located_equality
  , QC.testProperty "located values with different values are not equal" prop_located_different_values
  , QC.testProperty "located values with different positions are not equal" prop_located_different_positions
  , QC.testProperty "file directives show is deterministic" prop_file_directives_show_deterministic
  , QC.testProperty "block directives show is deterministic" prop_block_directives_show_deterministic
  ]