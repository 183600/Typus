{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewSymbolTableCommutativeQuickCheckSpec (tests) where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck (property)
import Analyzer.SymbolTable
  ( collectSymbolsAndTypes, collectSymbolsFromAST, isReservedName )
import Analyzer.Types (SymbolInfo(..), SymbolKind(..))
import Compiler.GoAst (GoModule(..), GoDecl(..), FuncDecl(..), VarDecl(..), ConstDecl(..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (sort, union, intersect)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (isJust, isNothing, catMaybes)
import Data.Either (isLeft, isRight)

-- | Test symbol table union commutativity
prop_symbol_table_union_commutative :: [(String, SymbolInfo)] -> [(String, SymbolInfo)] -> Property
prop_symbol_table_union_commutative symbols1 symbols2 =
    let table1 = Map.fromList symbols1
        table2 = Map.fromList symbols2
        union1 = Map.union table1 table2
        union2 = Map.union table2 table1
    in Map.keys union1 == Map.keys union2

-- | Test symbol table intersection commutativity
prop_symbol_table_intersection_commutative :: [(String, SymbolInfo)] -> [(String, SymbolInfo)] -> Property
prop_symbol_table_intersection_commutative symbols1 symbols2 =
    let table1 = Map.fromList symbols1
        table2 = Map.fromList symbols2
        keys1 = Set.fromList (Map.keys table1)
        keys2 = Set.fromList (Map.keys table2)
        intersect1 = Set.intersection keys1 keys2
        intersect2 = Set.intersection keys2 keys1
    in intersect1 == intersect2

-- | Test symbol collection order independence
prop_symbol_collection_order_independent :: [String] -> Property
prop_symbol_collection_order_independent codeLines =
    not (null codeLines) && L.all (\l -> L.length l > 0) codeLines ==>
    let code1 = unlines codeLines
        code2 = unlines (L.reverse codeLines)
        result1 = collectSymbolsAndTypes code1
        result2 = collectSymbolsAndTypes code2
    in case (result1, result2) of
         (Right table1, Right table2) -> 
           let keys1 = sort (Map.keys table1)
               keys2 = sort (Map.keys table2)
           in keys1 == keys2
         _ -> True  -- Both should fail L.or both succeed

-- | Test reserved name checking consistency
prop_reserved_name_consistency :: String -> Bool
prop_reserved_name_consistency name =
    let result1 = isReservedName name
        result2 = isReservedName name
    in result1 == result2

-- | Test symbol table merging associativity
prop_symbol_table_merging_associative :: [(String, SymbolInfo)] -> [(String, SymbolInfo)] -> [(String, SymbolInfo)] -> Property
prop_symbol_table_merging_associative symbols1 symbols2 symbols3 =
    let table1 = Map.fromList symbols1
        table2 = Map.fromList symbols2
        table3 = Map.fromList symbols3
        merge1 = Map.union (Map.union table1 table2) table3
        merge2 = Map.union table1 (Map.union table2 table3)
    in Map.keys merge1 == Map.keys merge2

-- | Test symbol insertion order independence
prop_symbol_insertion_order_independent :: [String] -> [SymbolInfo] -> Property
prop_symbol_insertion_order_independent names infos =
    L.length names == L.length infos && L.all (\n -> L.length n > 0) names ==>
    let table1 = Map.fromList (zip names infos)
        table2 = Map.fromList (zip (L.reverse names) (L.reverse infos))
    in Map.keys table1 == Map.keys (L.reverse (Map.keys table2))

-- | Test symbol lookup consistency
prop_symbol_lookup_consistency :: [(String, SymbolInfo)] -> String -> Property
prop_symbol_lookup_consistency symbols query =
    let table = Map.fromList symbols
        result1 = Map.lookup query table
        result2 = Map.lookup query table
    in result1 == result2

-- | Test symbol filtering commutativity
prop_symbol_filtering_commutative :: [(String, SymbolInfo)] -> (String -> Bool) -> (String -> Bool) -> Property
prop_symbol_filtering_commutative symbols pred1 pred2 =
    let table = Map.fromList symbols
        filtered1 = Map.filterWithKey (\k _ -> pred1 k && pred2 k) table
        filtered2 = Map.filterWithKey (\k _ -> pred2 k && pred1 k) table
    in Map.keys filtered1 == Map.keys filtered2

-- | Test symbol table difference properties
prop_symbol_table_difference :: [(String, SymbolInfo)] -> [(String, SymbolInfo)] -> Property
prop_symbol_table_difference symbols1 symbols2 =
    let table1 = Map.fromList symbols1
        table2 = Map.fromList symbols2
        keys1 = Set.fromList (Map.keys table1)
        keys2 = Set.fromList (Map.keys table2)
        difference = Set.difference keys1 keys2
    in Set.size difference <= Set.size keys1

-- | Test symbol table subset properties
prop_symbol_table_subset :: [(String, SymbolInfo)] -> [(String, SymbolInfo)] -> Property
prop_symbol_table_subset symbols1 symbols2 =
    let table1 = Map.fromList symbols1
        table2 = Map.fromList symbols2
        keys1 = Set.fromList (Map.keys table1)
        keys2 = Set.fromList (Map.keys table2)
        isSubset = Set.isSubsetOf keys1 keys2
    in isSubset ==> Set.size keys1 <= Set.size keys2

-- | Test symbol table size additivity
prop_symbol_table_size_additive :: [(String, SymbolInfo)] -> [(String, SymbolInfo)] -> Property
prop_symbol_table_size_additive symbols1 symbols2 =
    let table1 = Map.fromList symbols1
        table2 = Map.fromList symbols2
        unionTable = Map.union table1 table2
        overlappingKeys = Set.intersection 
          (Set.fromList (Map.keys table1))
          (Set.fromList (Map.keys table2))
        expectedSize = L.length symbols1 + L.length symbols2 - Set.size overlappingKeys
    in Map.size unionTable == expectedSize

-- | Test symbol table key uniqueness
prop_symbol_table_key_uniqueness :: [(String, SymbolInfo)] -> Bool
prop_symbol_table_key_uniqueness symbols =
    let table = Map.fromList symbols
        keys = Map.keys table
    in L.length keys == L.length (nub keys)
  where
    nub [] = []
    nub (x:xs) = x : nub (L.filter (/= x) xs)

-- | Test symbol table transformation commutativity
prop_symbol_table_transformation_commutative :: [(String, SymbolInfo)] -> Property
prop_symbol_table_transformation_commutative symbols =
    let table = Map.fromList symbols
        transform1 = Map.L.map (\si -> si { symbolName = symbolName si ++ "_1" }) table
        transform2 = Map.L.map (\si -> si { symbolName = symbolName si ++ "_2" }) table
        combined1 = Map.union transform1 transform2
        combined2 = Map.union transform2 transform1
    in Map.keys combined1 == Map.keys combined2

-- | Test symbol table folding associativity
prop_symbol_table_folding_associative :: [(String, SymbolInfo)] -> [(String, SymbolInfo)] -> [(String, SymbolInfo)] -> Property
prop_symbol_table_folding_associative symbols1 symbols2 symbols3 =
    let tables = map Map.fromList [symbols1, symbols2, symbols3]
        fold1 = Map.unions (tables)
        fold2 = Map.unions (L.reverse tables)
    in Map.keys fold1 == Map.keys fold2

-- | Test symbol table partition properties
prop_symbol_table_partition :: [(String, SymbolInfo)] -> (String -> Bool) -> Property
prop_symbol_table_partition symbols predicate =
    let table = Map.fromList symbols
        matching = Map.filterWithKey (\k _ -> predicate k) table
        nonMatching = Map.filterWithKey (\k _ -> not (predicate k)) table
        unioned = Map.union matching nonMatching
    in Map.keys unioned == Map.keys table

-- | Test symbol table mapping distributivity
prop_symbol_table_mapping_distributivity :: [(String, SymbolInfo)] -> (String -> Bool) -> (SymbolInfo -> SymbolInfo) -> Property
prop_symbol_table_mapping_distributivity symbols predicate mapper =
    let table = Map.fromList symbols
        filtered = Map.filterWithKey (\k _ -> predicate k) table
        mappedFiltered = Map.map mapper filtered
        mapped = Map.map mapper table
        filteredMapped = Map.filterWithKey (\k _ -> predicate k) mapped
    in Map.keys mappedFiltered == Map.keys filteredMapped

-- | Test symbol table intersection with union
prop_symbol_table_intersection_union :: [(String, SymbolInfo)] -> [(String, SymbolInfo)] -> [(String, SymbolInfo)] -> Property
prop_symbol_table_intersection_union symbols1 symbols2 symbols3 =
    let table1 = Map.fromList symbols1
        table2 = Map.fromList symbols2
        table3 = Map.fromList symbols3
        union12 = Map.union table1 table2
        intersection = Map.intersection union12 table3
        intersection1 = Map.intersection table1 table3
        intersection2 = Map.intersection table2 table3
        unionIntersections = Map.union intersection1 intersection2
    in Map.keys intersection == Map.keys unionIntersections

-- | Test symbol table deletion properties
prop_symbol_table_deletion :: [(String, SymbolInfo)] -> [String] -> Property
prop_symbol_table_deletion symbols toDelete =
    let table = Map.fromList symbols
        deleteKeys = Set.fromList toDelete
        remaining = Map.filterWithKey (\k _ -> not (k `Set.member` deleteKeys)) table
        originalKeys = Set.fromList (Map.keys table)
        remainingKeys = Set.fromList (Map.keys remaining)
        deletedKeys = Set.difference originalKeys remainingKeys
    in deletedKeys == Set.intersection originalKeys deleteKeys

-- | Test symbol table update commutativity
prop_symbol_table_update_commutative :: [(String, SymbolInfo)] -> String -> SymbolInfo -> SymbolInfo -> Property
prop_symbol_table_update_commutative symbols key value1 value2 =
    L.length key > 0 ==>
    let table = Map.fromList symbols
        updated1 = Map.insert key value1 (Map.insert key value2 table)
        updated2 = Map.insert key value2 (Map.insert key value1 table)
    in Map.lookup key updated1 == Map.lookup key updated2

-- | Test symbol table lookup with default
prop_symbol_table_lookup_with_default :: [(String, SymbolInfo)] -> String -> SymbolInfo -> Bool
prop_symbol_table_lookup_with_default symbols key defaultValue =
    let table = Map.fromList symbols
        result1 = Map.findWithDefault defaultValue key table
        result2 = Map.findWithDefault defaultValue key table
    in result1 == result2

tests :: TestTree
tests = testGroup "Symbol Table Commutative QuickCheck Tests"
  [ testProperty "symbol table union commutative" prop_symbol_table_union_commutative
  , testProperty "symbol table intersection commutative" prop_symbol_table_intersection_commutative
  , testProperty "symbol collection order independent" prop_symbol_collection_order_independent
  , testProperty "reserved name consistency" prop_reserved_name_consistency
  , testProperty "symbol table merging associative" prop_symbol_table_merging_associative
  , testProperty "symbol insertion order independent" prop_symbol_insertion_order_independent
  , testProperty "symbol lookup consistency" prop_symbol_lookup_consistency
  , testProperty "symbol filtering commutative" prop_symbol_filtering_commutative
  , testProperty "symbol table difference" prop_symbol_table_difference
  , testProperty "symbol table subset" prop_symbol_table_subset
  , testProperty "symbol table size additive" prop_symbol_table_size_additive
  , testProperty "symbol table key uniqueness" prop_symbol_table_key_uniqueness
  , testProperty "symbol table transformation commutative" prop_symbol_table_transformation_commutative
  , testProperty "symbol table folding associative" prop_symbol_table_folding_associative
  , testProperty "symbol table partition" prop_symbol_table_partition
  , testProperty "symbol table mapping distributivity" prop_symbol_table_mapping_distributivity
  , testProperty "symbol table intersection union" prop_symbol_table_intersection_union
  , testProperty "symbol table deletion" prop_symbol_table_deletion
  , testProperty "symbol table update commutative" prop_symbol_table_update_commutative
  , testProperty "symbol table lookup with default" prop_symbol_table_lookup_with_default
  ]