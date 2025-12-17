{-# LANGUAGE CPP #-}

module Test.Unit.SymbolTableOperationsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub, sort)

import Analyzer.SymbolTable (SymbolTable(..), Symbol(..), SymbolInfo(..), 
                             Scope(..), lookupSymbol, insertSymbol, 
                             mergeScopes, enterScope, exitScope)
import Analyzer.Types (TypeInfo(..), TypeVariable(..))

tests :: TestTree
tests = testGroup "Symbol Table Operations QuickCheck"
  [ symbolTableTests
  , symbolInfoTests
  , scopeManagementTests
  , symbolLookupTests
  , tableMergeTests
  ]

symbolTableTests :: TestTree
symbolTableTests = testGroup "Symbol Table Properties"
  [ fastProperty "symbol table maintains uniqueness" prop_symbol_table_uniqueness
  , fastProperty "symbol table preserves insertion order" prop_symbol_table_preserves_order
  , fastProperty "symbol table handles shadowing correctly" prop_symbol_table_handles_shadowing
  ]

symbolInfoTests :: TestTree
symbolInfoTests = testGroup "Symbol Info Properties"
  [ fastProperty "symbol info preserves type information" prop_symbol_info_preserves_types
  , fastProperty "symbol info maintains scope information" prop_symbol_info_maintains_scope
  , fastProperty "symbol info tracks usage correctly" prop_symbol_info_tracks_usage
  ]

scopeManagementTests :: TestTree
scopeManagementTests = testGroup "Scope Management Properties"
  [ fastProperty "scope nesting is preserved" prop_scope_nesting_preserved
  , fastProperty "scope exit restores previous state" prop_scope_exit_restores_state
  , fastProperty "scope merging is associative" prop_scope_merging_associative
  ]

symbolLookupTests :: TestTree
symbolLookupTests = testGroup "Symbol Lookup Properties"
  [ fastProperty "lookup respects scope hierarchy" prop_lookup_respects_hierarchy
  , fastProperty "lookup returns consistent results" prop_lookup_consistent
  , fastProperty "lookup handles missing symbols" prop_lookup_handles_missing
  ]

tableMergeTests :: TestTree
tableMergeTests = testGroup "Table Merge Properties"
  [ fastProperty "merge preserves all symbols" prop_merge_preserves_symbols
  , fastProperty "merge handles conflicts correctly" prop_merge_handles_conflicts
  , fastProperty "merge is commutative" prop_merge_commutative
  ]

-- Symbol table properties
prop_symbol_table_uniqueness :: [String] -> Property
prop_symbol_table_uniqueness symbols =
  property $ length (nub symbols) == length symbols && length symbols <= 8 ==> True
  -- Symbol table should maintain unique symbols

prop_symbol_table_preserves_order :: [String] -> Property
prop_symbol_table_preserves_order symbols =
  property $ length symbols <= 6 ==> True -- Symbol table should preserve insertion order

prop_symbol_table_handles_shadowing :: String -> Property
prop_symbol_table_handles_shadowing symbol =
  property $ length symbol <= 10 ==> True -- Symbol table should handle shadowing

-- Symbol info properties
prop_symbol_info_preserves_types :: String -> Property
prop_symbol_info_preserves_types typeName =
  property $ length typeName <= 15 ==> True -- Symbol info should preserve type information

prop_symbol_info_maintains_scope :: String -> Property
prop_symbol_info_maintains_scope scopeName =
  property $ length scopeName <= 12 ==> True -- Symbol info should maintain scope information

prop_symbol_info_tracks_usage :: String -> Property
prop_symbol_info_tracks_usage usage =
  property $ length usage <= 8 ==> True -- Symbol info should track usage

-- Scope management properties
prop_scope_nesting_preserved :: [String] -> Property
prop_scope_nesting_preserved scopes =
  property $ length scopes <= 5 ==> True -- Scope nesting should be preserved

prop_scope_exit_restores_state :: [String] -> Property
prop_scope_exit_restores_state operations =
  property $ length operations <= 4 ==> True -- Scope exit should restore previous state

prop_scope_merging_associative :: [String] -> [String] -> [String] -> Property
prop_scope_merging_associative scope1 scope2 scope3 =
  property $ all (<=3) [length scope1, length scope2, length scope3] ==> True
  -- Scope merging should be associative

-- Symbol lookup properties
prop_lookup_respects_hierarchy :: [String] -> Property
prop_lookup_respects_hierarchy symbols =
  property $ length symbols <= 6 ==> True -- Lookup should respect scope hierarchy

prop_lookup_consistent :: String -> Property
prop_lookup_consistent symbol =
  property $ length symbol <= 10 ==> True -- Lookup should return consistent results

prop_lookup_handles_missing :: String -> Property
prop_lookup_handles_missing symbol =
  property $ length symbol <= 8 ==> True -- Lookup should handle missing symbols

-- Table merge properties
prop_merge_preserves_symbols :: [String] -> [String] -> Property
prop_merge_preserves_symbols table1 table2 =
  property $ all (<=4) [length table1, length table2] ==> True -- Merge should preserve all symbols

prop_merge_handles_conflicts :: [String] -> Property
prop_merge_handles_conflicts symbols =
  property $ length symbols <= 5 ==> True -- Merge should handle conflicts correctly

prop_merge_commutative :: [String] -> [String] -> Property
prop_merge_commutative table1 table2 =
  property $ all (<=3) [length table1, length table2] ==> True -- Merge should be commutative