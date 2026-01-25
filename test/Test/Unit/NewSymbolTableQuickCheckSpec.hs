{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Test.Unit.NewSymbolTableQuickCheckSpec where


import Test.Tasty
import Test.Tasty.QuickCheck

-- | Symbol table QuickCheck tests for the Typus compiler
-- This module contains property-based tests for symbol table utilities


import Test.Tasty
import Test.Tasty.QuickCheck

import Test.QuickCheck ((==>), conjoin, counterexample)
import Utils
  ( trim
  , splitBy
  , splitByComma
  , removeLineComments
  , removeComments
  , safeProcessString
  , isValidChar
  , breakOn
  )
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Control.Monad (foldM)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Compiler.Errors.Core as Error
import Compiler.Errors.Core (ErrorSeverity(..), ErrorCategory(..), TypeError(..), ErrorLocation(..), ErrorContext(..),
                            errorAt, errorWithCategory, warningAt, infoAt, 
                            fatalError, withLocation, withContext, combineErrors,
                            combinedErrorSeverity, filterByCategory, filterBySeverity,
                            hasCategory, isAtLeast, severityPriority, location, line, column, 
                            fatalRecovery, emptyContext, contextCode)
import SourceLocation (toErrorLocation)
import Data.Time (UTCTime, getCurrentTime)
import Data.List (sort, nub)
import Data.Ord (comparing)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- ============================================================================
-- Helper Types and Functions
-- ============================================================================

-- | Simple symbol type for testing
data SymbolType = Variable | Function | Type | Constant | Module deriving (Show, Eq, Ord)

-- | Arbitrary instance for SymbolType
instance Arbitrary SymbolType where
  arbitrary = elements [Variable, Function, Type, Constant, Module]

-- | Simple symbol scope for testing
data SymbolScope = Local | Global | Parameter deriving (Show, Eq, Ord)

-- | Arbitrary instance for SymbolScope
instance Arbitrary SymbolScope where
  arbitrary = elements [Local, Global, Parameter]

-- | Simple symbol info for testing
data SymbolInfo = SymbolInfo
  { symbolName :: String
  , symbolType :: SymbolType
  , symbolScope :: SymbolScope
  , symbolValue :: Maybe String
  } deriving (Show, Eq)

-- | Arbitrary instance for SymbolInfo
instance Arbitrary SymbolInfo where
  arbitrary = do
    name <- elements ["x", "y", "z", "foo", "bar", "baz"]
    typ <- arbitrary
    scope <- arbitrary
    value <- arbitrary
    return $ SymbolInfo name typ scope value

-- SymbolTable uses the default Arbitrary instance for Map

-- | Simple symbol table for testing
type SymbolTable = Map.Map String SymbolInfo

-- | Create an empty symbol table
emptySymbolTable :: SymbolTable
emptySymbolTable = Map.empty

-- | Add a symbol to the table
addSymbol :: String -> SymbolInfo -> SymbolTable -> SymbolTable
addSymbol name info = Map.insert name info

-- | Remove a symbol from the table
removeSymbol :: String -> SymbolTable -> SymbolTable
removeSymbol = Map.delete

-- | Look up a symbol in the table
lookupSymbol :: String -> SymbolTable -> Maybe SymbolInfo
lookupSymbol = Map.lookup

-- | Check if a symbol exists in the table
symbolExists :: String -> SymbolTable -> Bool
symbolExists name = Map.member name

-- | Check if a symbol is in a specific scope
symbolInScope :: SymbolScope -> String -> SymbolTable -> Bool
symbolInScope scope name table = 
  case lookupSymbol name table of
    Just info -> symbolScope info == scope
    Nothing -> False

-- | Get all symbol types in the table
symbolTypes :: SymbolTable -> [SymbolType]
symbolTypes = nub . map symbolType . Map.elems

-- | Get all symbol names in the table
symbolNames :: SymbolTable -> [String]
symbolNames = Map.keys

-- | Get the count of symbols in the table
symbolCount :: SymbolTable -> Int
symbolCount = Map.size

-- | Merge two symbol tables
mergeSymbolTables :: SymbolTable -> SymbolTable -> SymbolTable
mergeSymbolTables = Map.union

-- | Filter symbols by type
filterSymbolsByType :: SymbolType -> SymbolTable -> SymbolTable
filterSymbolsByType typ = Map.filter (\info -> symbolType info == typ)

-- | Filter symbols by scope
filterSymbolsByScope :: SymbolScope -> SymbolTable -> SymbolTable
filterSymbolsByScope scope = Map.filter (\info -> symbolScope info == scope)

-- | Validate a symbol table
validateSymbolTable :: SymbolTable -> Bool
validateSymbolTable table = 
  let symbols = Map.elems table
      names = Map.keys table
  in length names == length (nub names) -- No duplicate names

-- | Check if a symbol table is consistent
symbolTableConsistent :: SymbolTable -> Bool
symbolTableConsistent = validateSymbolTable

-- | Check if two symbol tables are equivalent
symbolTableEquivalence :: SymbolTable -> SymbolTable -> Bool
symbolTableEquivalence table1 table2 = 
  symbolCount table1 == symbolCount table2 &&
  sort (symbolNames table1) == sort (symbolNames table2)

-- | Shadow a symbol in the table
shadowSymbol :: String -> SymbolInfo -> SymbolTable -> SymbolTable
shadowSymbol = addSymbol

-- | Unshadow a symbol from the table
unshadowSymbol :: String -> SymbolTable -> SymbolTable
unshadowSymbol = removeSymbol

-- ============================================================================
-- Symbol Table Creation Tests
-- ============================================================================

-- | Test empty symbol table
prop_empty_symbol_table :: Bool
prop_empty_symbol_table = 
  let table = emptySymbolTable
  in symbolCount table == 0 && null (symbolNames table)

-- | Test add symbol
prop_add_symbol :: String -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_add_symbol name typ scope table = 
  let info = SymbolInfo name typ scope Nothing
      newTable = addSymbol name info table
  in symbolExists name newTable && 
     lookupSymbol name newTable == Just info

-- | Test add symbol override
prop_add_symbol_override :: String -> SymbolType -> SymbolScope -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_add_symbol_override name typ1 scope1 typ2 scope2 table = 
  let info1 = SymbolInfo name typ1 scope1 Nothing
      info2 = SymbolInfo name typ2 scope2 Nothing
      table1 = addSymbol name info1 table
      table2 = addSymbol name info2 table1
  in lookupSymbol name table2 == Just info2

-- | Test remove symbol
prop_remove_symbol :: String -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_remove_symbol name typ scope table = 
  let info = SymbolInfo name typ scope Nothing
      table1 = addSymbol name info table
      table2 = removeSymbol name table1
  in not (symbolExists name table2)

-- | Test remove symbol missing
prop_remove_symbol_missing :: String -> SymbolTable -> Property
prop_remove_symbol_missing name table = 
  not (symbolExists name table) ==> property $
    let table2 = removeSymbol name table
    in table2 == table
-- ============================================================================

-- | Test lookup symbol
prop_lookup_symbol :: String -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_lookup_symbol name typ scope table = 
  let info = SymbolInfo name typ scope Nothing
      table1 = addSymbol name info table
  in lookupSymbol name table1 == Just info

-- | Test lookup symbol missing
prop_lookup_symbol_missing :: String -> SymbolTable -> Property
prop_lookup_symbol_missing name table = 
  not (symbolExists name table) ==> property $
    lookupSymbol name table == Nothing

-- | Test symbol exists
prop_symbol_exists :: String -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_symbol_exists name typ scope table = 
  let info = SymbolInfo name typ scope Nothing
      table1 = addSymbol name info table
  in symbolExists name table1

-- | Test symbol exists missing
prop_symbol_exists_missing :: String -> SymbolTable -> Property
prop_symbol_exists_missing name table = 
  not (symbolExists name table) ==> property $
    not (symbolExists name table)

-- | Test symbol in scope
prop_symbol_in_scope :: String -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_symbol_in_scope name typ scope table = 
  let info = SymbolInfo name typ scope Nothing
      table1 = addSymbol name info table
  in symbolInScope scope name table1

-- | Test symbol not in scope
prop_symbol_not_in_scope :: String -> SymbolType -> SymbolScope -> SymbolScope -> SymbolTable -> Property
prop_symbol_not_in_scope name typ scope1 scope2 table = 
  scope1 /= scope2 ==> property $
    let info = SymbolInfo name typ scope1 Nothing
        table1 = addSymbol name info table
    in not (symbolInScope scope2 name table1)

-- ============================================================================
-- Symbol Table Query Tests
-- ============================================================================

-- | Test symbol types
prop_symbol_types :: String -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_symbol_types name typ scope table = 
  let info = SymbolInfo name typ scope Nothing
      table1 = addSymbol name info table
  in typ `elem` symbolTypes table1

-- | Test symbol names
prop_symbol_names :: String -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_symbol_names name typ scope table = 
  let info = SymbolInfo name typ scope Nothing
      table1 = addSymbol name info table
  in name `elem` symbolNames table1

-- | Test symbol count
prop_symbol_count :: String -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_symbol_count name typ scope table = 
  let info = SymbolInfo name typ scope Nothing
      table1 = addSymbol name info table
  in symbolCount table1 == symbolCount table + 1

-- | Test filter symbols by type
prop_filter_symbols_by_type :: String -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_filter_symbols_by_type name typ scope table = 
  let info = SymbolInfo name typ scope Nothing
      table1 = addSymbol name info table
      filtered = filterSymbolsByType typ table1
  in all (\info' -> symbolType info' == typ) (Map.elems filtered)

-- | Test filter symbols by scope
prop_filter_symbols_by_scope :: String -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_filter_symbols_by_scope name typ scope table = 
  let info = SymbolInfo name typ scope Nothing
      table1 = addSymbol name info table
      filtered = filterSymbolsByScope scope table1
  in all (\info' -> symbolScope info' == scope) (Map.elems filtered)

-- ============================================================================
-- Symbol Table Merge Tests
-- ============================================================================

-- | Test merge symbol tables
prop_merge_symbol_tables :: String -> SymbolType -> SymbolScope -> String -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_merge_symbol_tables name1 typ1 scope1 name2 typ2 scope2 table = 
  let info1 = SymbolInfo name1 typ1 scope1 Nothing
      info2 = SymbolInfo name2 typ2 scope2 Nothing
      table1 = addSymbol name1 info1 table
      table2 = addSymbol name2 info2 table
      merged = mergeSymbolTables table1 table2
  in symbolExists name1 merged && symbolExists name2 merged

-- | Test merge symbol tables conflict
prop_merge_symbol_tables_conflict :: String -> SymbolType -> SymbolScope -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_merge_symbol_tables_conflict name typ1 scope1 typ2 scope2 table = 
  let info1 = SymbolInfo name typ1 scope1 Nothing
      info2 = SymbolInfo name typ2 scope2 Nothing
      table1 = addSymbol name info1 table
      table2 = addSymbol name info2 table
      merged = mergeSymbolTables table1 table2
  in symbolExists name merged && 
     lookupSymbol name merged `elem` [Just info1, Just info2]

-- | Test merge symbol tables empty
prop_merge_symbol_tables_empty :: SymbolTable -> Bool
prop_merge_symbol_tables_empty table = 
  let merged = mergeSymbolTables emptySymbolTable table
  in merged == table

-- | Test merge symbol tables identity
prop_merge_symbol_tables_identity :: SymbolTable -> Bool
prop_merge_symbol_tables_identity table = 
  let merged1 = mergeSymbolTables table emptySymbolTable
      merged2 = mergeSymbolTables emptySymbolTable table
  in merged1 == table && merged2 == table

-- ============================================================================
-- Symbol Table Validation Tests
-- ============================================================================

-- | Test validate symbol table
prop_validate_symbol_table :: String -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_validate_symbol_table name typ scope table = 
  let info = SymbolInfo name typ scope Nothing
      table1 = addSymbol name info table
  in validateSymbolTable table1

-- | Test validate symbol table duplicate
prop_validate_symbol_table_duplicate :: String -> SymbolType -> SymbolScope -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_validate_symbol_table_duplicate name typ1 scope1 typ2 scope2 table = 
  let info1 = SymbolInfo name typ1 scope1 Nothing
      info2 = SymbolInfo name typ2 scope2 Nothing
      table1 = addSymbol name info1 table
      table2 = addSymbol name info2 table1
  in validateSymbolTable table2 -- Still valid because we override

-- | Test symbol table consistent
prop_symbol_table_consistent :: String -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_symbol_table_consistent name typ scope table = 
  let info = SymbolInfo name typ scope Nothing
      table1 = addSymbol name info table
  in symbolTableConsistent table1

-- | Test symbol table equivalence
prop_symbol_table_equivalence :: String -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_symbol_table_equivalence name typ scope table = 
  let info = SymbolInfo name typ scope Nothing
      table1 = addSymbol name info table
      table2 = addSymbol name info emptySymbolTable
  in symbolTableEquivalence table1 table2

-- ============================================================================
-- Symbol Shadowing Tests
-- ============================================================================

-- | Test shadow symbol
prop_shadow_symbol :: String -> SymbolType -> SymbolScope -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_shadow_symbol name typ1 scope1 typ2 scope2 table = 
  let info1 = SymbolInfo name typ1 scope1 Nothing
      info2 = SymbolInfo name typ2 scope2 Nothing
      table1 = addSymbol name info1 table
      table2 = shadowSymbol name info2 table1
  in lookupSymbol name table2 == Just info2

-- | Test unshadow symbol
prop_unshadow_symbol :: String -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_unshadow_symbol name typ scope table = 
  let info = SymbolInfo name typ scope Nothing
      table1 = addSymbol name info table
      table2 = unshadowSymbol name table1
  in not (symbolExists name table2)

-- ============================================================================
-- Symbol Table Properties Tests
-- ============================================================================

-- | Test symbol table associativity
prop_symbol_table_associative :: String -> SymbolType -> SymbolScope -> String -> SymbolType -> SymbolScope -> String -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_symbol_table_associative name1 typ1 scope1 name2 typ2 scope2 name3 typ3 scope3 table = 
  let info1 = SymbolInfo name1 typ1 scope1 Nothing
      info2 = SymbolInfo name2 typ2 scope2 Nothing
      info3 = SymbolInfo name3 typ3 scope3 Nothing
      table1 = addSymbol name1 info1 table
      table2 = addSymbol name2 info2 table1
      table3 = addSymbol name3 info3 table2
      merged1 = mergeSymbolTables (mergeSymbolTables table1 table2) table3
      merged2 = mergeSymbolTables table1 (mergeSymbolTables table2 table3)
  in symbolTableEquivalence merged1 merged2

-- | Test symbol table commutativity
prop_symbol_table_commutative :: String -> SymbolType -> SymbolScope -> String -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_symbol_table_commutative name1 typ1 scope1 name2 typ2 scope2 table = 
  let info1 = SymbolInfo name1 typ1 scope1 Nothing
      info2 = SymbolInfo name2 typ2 scope2 Nothing
      table1 = addSymbol name1 info1 table
      table2 = addSymbol name2 info2 table
      merged1 = mergeSymbolTables table1 table2
      merged2 = mergeSymbolTables table2 table1
  in symbolTableEquivalence merged1 merged2

-- | Test symbol table idempotence
prop_symbol_table_idempotent :: String -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_symbol_table_idempotent name typ scope table = 
  let info = SymbolInfo name typ scope Nothing
      table1 = addSymbol name info table
      table2 = addSymbol name info table1
  in symbolTableEquivalence table1 table2

-- | Test symbol table distributivity
prop_symbol_table_distributive :: String -> SymbolType -> SymbolScope -> SymbolTable -> Bool
prop_symbol_table_distributive name typ scope table = 
  let info = SymbolInfo name typ scope Nothing
      table1 = addSymbol name info table
      filtered1 = filterSymbolsByType typ table1
      filtered2 = filterSymbolsByType typ (addSymbol name info emptySymbolTable)
  in symbolCount filtered1 == symbolCount filtered2 + 
     (if typ `elem` symbolTypes table then 1 else 0)

-- ============================================================================
-- Test Group
-- ============================================================================

tests :: TestTree
tests = testGroup "Symbol Table QuickCheck Tests"
  [ -- Symbol Table Creation Tests
    testProperty "empty symbol table" prop_empty_symbol_table
  , testProperty "add symbol" prop_add_symbol
  , testProperty "add symbol override" prop_add_symbol_override
  , testProperty "remove symbol" prop_remove_symbol
  , testProperty "remove symbol missing" prop_remove_symbol_missing
  
  -- Symbol Lookup Tests
  , testProperty "lookup symbol" prop_lookup_symbol
  , testProperty "lookup symbol missing" prop_lookup_symbol_missing
  , testProperty "symbol exists" prop_symbol_exists
  , testProperty "symbol exists missing" prop_symbol_exists_missing
  , testProperty "symbol in scope" prop_symbol_in_scope
  , testProperty "symbol not in scope" prop_symbol_not_in_scope
  
  -- Symbol Table Query Tests
  , testProperty "symbol types" prop_symbol_types
  , testProperty "symbol names" prop_symbol_names
  , testProperty "symbol count" prop_symbol_count
  , testProperty "filter symbols by type" prop_filter_symbols_by_type
  , testProperty "filter symbols by scope" prop_filter_symbols_by_scope
  
  -- Symbol Table Merge Tests
  , testProperty "merge symbol tables" prop_merge_symbol_tables
  , testProperty "merge symbol tables conflict" prop_merge_symbol_tables_conflict
  , testProperty "merge symbol tables empty" prop_merge_symbol_tables_empty
  , testProperty "merge symbol tables identity" prop_merge_symbol_tables_identity
  
  -- Symbol Table Validation Tests
  , testProperty "validate symbol table" prop_validate_symbol_table
  , testProperty "validate symbol table duplicate" prop_validate_symbol_table_duplicate
  , testProperty "symbol table consistent" prop_symbol_table_consistent
  , testProperty "symbol table equivalence" prop_symbol_table_equivalence
  
  -- Symbol Shadowing Tests
  , testProperty "shadow symbol" prop_shadow_symbol
  , testProperty "unshadow symbol" prop_unshadow_symbol
  
  -- Symbol Table Properties Tests
  , testProperty "symbol table associative" prop_symbol_table_associative
  , testProperty "symbol table commutative" prop_symbol_table_commutative
  , testProperty "symbol table idempotent" prop_symbol_table_idempotent
  , testProperty "symbol table distributive" prop_symbol_table_distributive
  ]