{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SymbolTableInvariantQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(..), NonEmptyList(..))

import Analyzer.Types
  ( SymbolKind(..)
  , AnalysisPhase(..)
  , AnalysisContext(..)
  , AnalyzerState(..)
  , SymbolTable
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  )

import Data.Char (isAlphaNum)
import Data.List (nub, sort)
import qualified Data.Map as Map

-- Property: empty symbol table has no symbols
prop_empty_symbol_table_has_no_symbols :: Property
prop_empty_symbol_table_has_no_symbols =
  let empty = Map.empty :: SymbolTable
  in property $ Map.null empty

-- Property: inserting a symbol increases table size
prop_insert_increases_size :: String -> SymbolKind -> Property
prop_insert_increases_size name kind =
  let empty = Map.empty :: SymbolTable
      withSymbol = Map.insert name kind empty
  in property $ Map.size withSymbol === 1

-- Property: symbol lookup returns inserted value
prop_symbol_lookup_returns_inserted :: String -> SymbolKind -> Property
prop_symbol_lookup_returns_inserted name kind =
  let empty = Map.empty :: SymbolTable
      withSymbol = Map.insert name kind empty
      lookupResult = Map.lookup name withSymbol
  in property $ lookupResult === Just kind

-- Property: inserting same symbol twice overwrites
prop_insert_overwrites :: String -> SymbolKind -> SymbolKind -> Property
prop_insert_overwrites name kind1 kind2 =
  let empty = Map.empty :: SymbolTable
      withFirst = Map.insert name kind1 empty
      withSecond = Map.insert name kind2 withFirst
      lookupResult = Map.lookup name withSecond
  in property $ lookupResult === Just kind2

-- Property: removing non-existent symbol doesn't change table
prop_remove_non_existent_no_change :: String -> SymbolTable -> Property
prop_remove_non_existent_no_change name table =
  let beforeSize = Map.size table
      afterRemove = Map.delete name table
      afterSize = Map.size afterRemove
  in property $ beforeSize === afterSize

-- Property: removing existing symbol decreases table size
prop_remove_existing_decreases_size :: String -> SymbolKind -> Property
prop_remove_existing_decreases_size name kind =
  let empty = Map.empty :: SymbolTable
      withSymbol = Map.insert name kind empty
      beforeSize = Map.size withSymbol
      afterRemove = Map.delete name withSymbol
      afterSize = Map.size afterRemove
  in property $ beforeSize === 1 .&&. afterSize === 0

-- Property: symbol table keys are unique
prop_symbol_table_keys_unique :: [(String, SymbolKind)] -> Property
prop_symbol_table_keys_unique pairs =
  let table = Map.fromList pairs
      keys = Map.keys table
      uniqueKeys = nub keys
  in property $ L.length keys === L.length uniqueKeys

-- Property: filtering preserves symbol types
prop_filter_preserves_types :: String -> SymbolKind -> Property
prop_filter_preserves_types name kind =
  let table = Map.insert name kind Map.empty
      filterFunc _ k = k == kind
      filtered = Map.filter filterFunc table
  in property $ if filterFunc name kind
                   then Map.size filtered === 1
                   else Map.size filtered === 0

-- Property: union combines tables correctly
prop_union_combines_tables :: [(String, SymbolKind)] -> [(String, SymbolKind)] -> Property
prop_union_combines_tables pairs1 pairs2 =
  let table1 = Map.fromList pairs1
      table2 = Map.fromList pairs2
      unioned = Map.union table1 table2
      expectedSize = L.length $ nub $ map fst pairs1 ++ map fst pairs2
  in property $ Map.size unioned === expectedSize

-- Property: intersection finds common symbols
prop_intersection_finds_common :: [(String, SymbolKind)] -> [(String, SymbolKind)] -> Property
prop_intersection_finds_common pairs1 pairs2 =
  let table1 = Map.fromList pairs1
      table2 = Map.fromList pairs2
      intersected = Map.intersection table1 table2
      commonKeys = nub $ L.filter (`elem` map fst pairs2) (map fst pairs1)
  in property $ Map.size intersected === L.length commonKeys

-- Property: difference removes common symbols
prop_difference_removes_common :: [(String, SymbolKind)] -> [(String, SymbolKind)] -> Property
prop_difference_removes_common pairs1 pairs2 =
  let table1 = Map.fromList pairs1
      table2 = Map.fromList pairs2
      differenced = Map.difference table1 table2
      remainingKeys = L.filter (`notElem` map fst pairs2) (map fst pairs1)
  in property $ Map.size differenced === L.length remainingKeys

-- Property: mapping preserves table size
prop_mapping_preserves_size :: [(String, SymbolKind)] -> Property
prop_mapping_preserves_size pairs =
  let table = Map.fromList pairs
      mapFunc _ k = k -- Identity function
      mapped = Map.map mapFunc table
  in property $ Map.size mapped === Map.size table

-- Property: keys are sorted consistently
prop_keys_sorted_consistent :: [(String, SymbolKind)] -> Property
prop_keys_sorted_consistent pairs =
  let table = Map.fromList pairs
      keys1 = Map.keys table
      keys2 = sort $ Map.keys table
  in property $ keys1 === keys2

-- Property: valid symbol names are alphanumeric
prop_valid_symbol_names_alphanumeric :: NonEmptyList Char -> Property
prop_valid_symbol_names_alphanumeric (NonEmpty c) =
  let name = take 10 $ filter isAlphaNum $ repeat c
      kind = FunctionSymbol
      table = Map.insert name kind Map.empty
  in property $ Map.member name table

tests :: TestTree
tests = testGroup "Symbol Table Invariant QuickCheck"
  [ fastProperty "empty symbol table has no symbols" prop_empty_symbol_table_has_no_symbols
  , fastProperty "insert increases size" prop_insert_increases_size
  , fastProperty "symbol lookup returns inserted" prop_symbol_lookup_returns_inserted
  , fastProperty "insert overwrites" prop_insert_overwrites
  , fastProperty "remove non-existent no change" prop_remove_non_existent_no_change
  , fastProperty "remove existing decreases size" prop_remove_existing_decreases_size
  , fastProperty "symbol table keys unique" prop_symbol_table_keys_unique
  , fastProperty "filter preserves types" prop_filter_preserves_types
  , fastProperty "union combines tables" prop_union_combines_tables
  , fastProperty "intersection finds common" prop_intersection_finds_common
  , fastProperty "difference removes common" prop_difference_removes_common
  , fastProperty "mapping preserves size" prop_mapping_preserves_size
  , fastProperty "keys sorted consistent" prop_keys_sorted_consistent
  , fastProperty "valid symbol names alphanumeric" prop_valid_symbol_names_alphanumeric
  ]