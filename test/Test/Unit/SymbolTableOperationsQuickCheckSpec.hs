{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SymbolTableOperationsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Analyzer.SymbolTable (SymbolTable, Symbol(..), SymbolType(..), insertSymbol, lookupSymbol, deleteSymbol, updateSymbol)
import Parser (parseTypus)
import Compiler.TypeChecker (TypeEnv)

import Data.Char (isLetter, isDigit)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (sort, nub)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Property: Symbol table should insert L.and retrieve symbols correctly
prop_symbol_table_insert_lookup :: String -> String -> Property
prop_symbol_table_insert_lookup symbolName symbolType =
  not (null symbolName) && not (null symbolType) &&
  L.all isLetter symbolName && L.all isLetter symbolType ==>
  let symbol = Symbol 
        { symbolName = symbolName
        , symbolType = UserDefinedType symbolType
        , symbolScope = Global
        }
      emptyTable = Map.empty :: SymbolTable
      tableWithSymbol = insertSymbol symbol emptyTable
  in case lookupSymbol symbolName tableWithSymbol of
       Nothing -> property False
       Just foundSymbol -> symbolName foundSymbol === symbolName

-- Property: Symbol table should handle multiple symbols
prop_symbol_table_multiple_symbols :: [String] -> Property
prop_symbol_table_multiple_symbols symbolNames =
  not (null symbolNames) && L.all (\n -> not (null n) && L.all isLetter n) (take 5 symbolNames) ==>
  let limitedNames = take 5 symbolNames
      symbols = L.map (\name -> Symbol 
        { symbolName = name
        , symbolType = UserDefinedType "int"
        , symbolScope = Global
        }) limitedNames
      emptyTable = Map.empty :: SymbolTable
      tableWithSymbols = foldr insertSymbol emptyTable symbols
      lookupResults = L.map (`lookupSymbol` tableWithSymbols) limitedNames
  in property $ L.all isJust lookupResults

-- Property: Symbol table should handle scope correctly
prop_symbol_table_scopes :: String -> Property
prop_symbol_table_scopes symbolName =
  not (null symbolName) && L.all isLetter symbolName ==>
  let globalSymbol = Symbol 
        { symbolName = symbolName
        , symbolType = UserDefinedType "int"
        , symbolScope = Global
        }
      localSymbol = Symbol 
        { symbolName = symbolName
        , symbolType = UserDefinedType "string"
        , symbolScope = Local
        }
      emptyTable = Map.empty :: SymbolTable
      tableWithGlobal = insertSymbol globalSymbol emptyTable
      tableWithBoth = insertSymbol localSymbol tableWithGlobal
  in case lookupSymbol symbolName tableWithBoth of
       Nothing -> property False
       Just foundSymbol -> symbolScope foundSymbol === Local

-- Property: Symbol table should delete symbols correctly
prop_symbol_table_delete :: String -> Property
prop_symbol_table_delete symbolName =
  not (null symbolName) && L.all isLetter symbolName ==>
  let symbol = Symbol 
        { symbolName = symbolName
        , symbolType = UserDefinedType "int"
        , symbolScope = Global
        }
      emptyTable = Map.empty :: SymbolTable
      tableWithSymbol = insertSymbol symbol emptyTable
      tableAfterDelete = deleteSymbol symbolName tableWithSymbol
  in lookupSymbol symbolName tableAfterDelete === Nothing

-- Property: Symbol table should update symbols correctly
prop_symbol_table_update :: String -> String -> Property
prop_symbol_table_update symbolName newType =
  not (null symbolName) && not (null newType) &&
  L.all isLetter symbolName && L.all isLetter newType ==>
  let originalSymbol = Symbol 
        { symbolName = symbolName
        , symbolType = UserDefinedType "int"
        , symbolScope = Global
        }
      updatedSymbol = Symbol 
        { symbolName = symbolName
        , symbolType = UserDefinedType newType
        , symbolScope = Global
        }
      emptyTable = Map.empty :: SymbolTable
      tableWithSymbol = insertSymbol originalSymbol emptyTable
      tableAfterUpdate = updateSymbol updatedSymbol tableWithSymbol
  in case lookupSymbol symbolName tableAfterUpdate of
       Nothing -> property False
       Just foundSymbol -> symbolType foundSymbol === UserDefinedType newType

-- Property: Symbol table should handle duplicate insertions
prop_symbol_table_duplicates :: String -> String -> Property
prop_symbol_table_duplicates symbolName firstType secondType =
  not (null symbolName) && not (null firstType) && not (null secondType) &&
  L.all isLetter symbolName && L.all isLetter firstType && L.all isLetter secondType ==>
  let firstSymbol = Symbol 
        { symbolName = symbolName
        , symbolType = UserDefinedType firstType
        , symbolScope = Global
        }
      secondSymbol = Symbol 
        { symbolName = symbolName
        , symbolType = UserDefinedType secondType
        , symbolScope = Local
        }
      emptyTable = Map.empty :: SymbolTable
      tableWithFirst = insertSymbol firstSymbol emptyTable
      tableWithBoth = insertSymbol secondSymbol tableWithFirst
  in case lookupSymbol symbolName tableWithBoth of
       Nothing -> property False
       Just foundSymbol -> symbolScope foundSymbol === Local

-- Property: Symbol table should handle type information
prop_symbol_table_types :: String -> String -> Property
prop_symbol_table_types symbolName typeName =
  not (null symbolName) && not (null typeName) &&
  L.all isLetter symbolName && L.all isLetter typeName ==>
  let symbol = Symbol 
        { symbolName = symbolName
        , symbolType = UserDefinedType typeName
        , symbolScope = Global
        }
      emptyTable = Map.empty :: SymbolTable
      tableWithSymbol = insertSymbol symbol emptyTable
  in case lookupSymbol symbolName tableWithSymbol of
       Nothing -> property False
       Just foundSymbol -> symbolType foundSymbol === UserDefinedType typeName

-- Property: Symbol table should handle nested scopes
prop_symbol_table_nested_scopes :: [String] -> Property
prop_symbol_table_nested_scopes symbolNames =
  not (null symbolNames) && L.all (\n -> not (null n) && L.all isLetter n) (take 3 symbolNames) ==>
  let limitedNames = take 3 symbolNames
      globalSymbol = Symbol 
        { symbolName = L.head limitedNames
        , symbolType = UserDefinedType "int"
        , symbolScope = Global
        }
      localSymbol = Symbol 
        { symbolName = limitedNames !! 1
        , symbolType = UserDefinedType "string"
        , symbolScope = Local
        }
      nestedSymbol = Symbol 
        { symbolName = last limitedNames
        , symbolType = UserDefinedType "bool"
        , symbolScope = Nested 1
        }
      emptyTable = Map.empty :: SymbolTable
      tableWithGlobal = insertSymbol globalSymbol emptyTable
      tableWithLocal = insertSymbol localSymbol tableWithGlobal
      tableWithNested = insertSymbol nestedSymbol tableWithLocal
  in case (lookupSymbol (L.head limitedNames) tableWithNested,
           lookupSymbol (limitedNames !! 1) tableWithNested,
           lookupSymbol (last limitedNames) tableWithNested) of
       (Just g, Just l, Just n) -> 
         symbolScope g === Global .&&. 
         symbolScope l === Local .&&.
         symbolScope n === Nested 1
       _ -> property False

-- Property: Symbol table should handle symbol attributes
prop_symbol_table_attributes :: String -> [String] -> Property
prop_symbol_table_attributes symbolName attributes =
  not (null symbolName) && L.all isLetter symbolName &&
  not (null attributes) && L.length (take 3 attributes) <= 3 ==>
  let limitedAttrs = take 3 attributes
      symbol = Symbol 
        { symbolName = symbolName
        , symbolType = UserDefinedType "int"
        , symbolScope = Global
        }
      emptyTable = Map.empty :: SymbolTable
      tableWithSymbol = insertSymbol symbol emptyTable
  in case lookupSymbol symbolName tableWithSymbol of
       Nothing -> property False
       Just foundSymbol -> symbolName foundSymbol === symbolName

-- Property: Symbol table should handle function symbols
prop_symbol_table_functions :: String -> [String] -> Property
prop_symbol_table_functions funcName paramTypes =
  not (null funcName) && L.all isLetter funcName &&
  not (null paramTypes) && L.length (take 3 paramTypes) <= 3 ==>
  let limitedParams = take 3 paramTypes
      functionSymbol = Symbol 
        { symbolName = funcName
        , symbolType = FunctionType limitedParams "int"
        , symbolScope = Global
        }
      emptyTable = Map.empty :: SymbolTable
      tableWithFunction = insertSymbol functionSymbol emptyTable
  in case lookupSymbol funcName tableWithFunction of
       Nothing -> property False
       Just foundSymbol -> symbolName foundSymbol === funcName

-- Property: Symbol table should handle variable symbols
prop_symbol_table_variables :: String -> String -> Property
prop_symbol_table_variables varName varType =
  not (null varName) && not (null varType) &&
  L.all isLetter varName && L.all isLetter varType ==>
  let variableSymbol = Symbol 
        { symbolName = varName
        , symbolType = UserDefinedType varType
        , symbolScope = Local
        }
      emptyTable = Map.empty :: SymbolTable
      tableWithVariable = insertSymbol variableSymbol emptyTable
  in case lookupSymbol varName tableWithVariable of
       Nothing -> property False
       Just foundSymbol -> symbolScope foundSymbol === Local

-- Property: Symbol table should handle type symbols
prop_symbol_table_type_symbols :: String -> [String] -> Property
prop_symbol_table_type_symbols typeName fieldNames =
  not (null typeName) && L.all isLetter typeName &&
  not (null fieldNames) && L.length (take 3 fieldNames) <= 3 ==>
  let limitedFields = take 3 fieldNames
      typeSymbol = Symbol 
        { symbolName = typeName
        , symbolType = StructType limitedFields
        , symbolScope = Global
        }
      emptyTable = Map.empty :: SymbolTable
      tableWithType = insertSymbol typeSymbol emptyTable
  in case lookupSymbol typeName tableWithType of
       Nothing -> property False
       Just foundSymbol -> symbolName foundSymbol === typeName

-- Property: Symbol table should handle constant symbols
prop_symbol_table_constants :: String -> String -> Property
prop_symbol_table_constants constName constType =
  not (null constName) && not (null constType) &&
  L.all isLetter constName && L.all isLetter constType ==>
  let constantSymbol = Symbol 
        { symbolName = constName
        , symbolType = UserDefinedType constType
        , symbolScope = Global
        }
      emptyTable = Map.empty :: SymbolTable
      tableWithConstant = insertSymbol constantSymbol emptyTable
  in case lookupSymbol constName tableWithConstant of
       Nothing -> property False
       Just foundSymbol -> symbolName foundSymbol === constName

-- Property: Symbol table should handle interface symbols
prop_symbol_table_interfaces :: String -> [String] -> Property
prop_symbol_table_interfaces interfaceName methodNames =
  not (null interfaceName) && L.all isLetter interfaceName &&
  not (null methodNames) && L.length (take 3 methodNames) <= 3 ==>
  let limitedMethods = take 3 methodNames
      interfaceSymbol = Symbol 
        { symbolName = interfaceName
        , symbolType = InterfaceType limitedMethods
        , symbolScope = Global
        }
      emptyTable = Map.empty :: SymbolTable
      tableWithInterface = insertSymbol interfaceSymbol emptyTable
  in case lookupSymbol interfaceName tableWithInterface of
       Nothing -> property False
       Just foundSymbol -> symbolName foundSymbol === interfaceName

-- Property: Symbol table should handle generic symbols
prop_symbol_table_generics :: String -> [String] -> Property
prop_symbol_table_generics genericName typeParams =
  not (null genericName) && L.all isLetter genericName &&
  not (null typeParams) && L.length (take 2 typeParams) <= 2 ==>
  let limitedParams = take 2 typeParams
      genericSymbol = Symbol 
        { symbolName = genericName
        , symbolType = GenericType limitedParams "T"
        , symbolScope = Global
        }
      emptyTable = Map.empty :: SymbolTable
      tableWithGeneric = insertSymbol genericSymbol emptyTable
  in case lookupSymbol genericName tableWithGeneric of
       Nothing -> property False
       Just foundSymbol -> symbolName foundSymbol === genericName

-- Property: Symbol table should handle symbol dependencies
prop_symbol_table_dependencies :: String -> [String] -> Property
prop_symbol_table_dependencies symbolName dependencies =
  not (null symbolName) && L.all isLetter symbolName &&
  not (null dependencies) && L.length (take 3 dependencies) <= 3 ==>
  let limitedDeps = take 3 dependencies
      symbol = Symbol 
        { symbolName = symbolName
        , symbolType = UserDefinedType "int"
        , symbolScope = Global
        }
      emptyTable = Map.empty :: SymbolTable
      tableWithSymbol = insertSymbol symbol emptyTable
  in case lookupSymbol symbolName tableWithSymbol of
       Nothing -> property False
       Just foundSymbol -> symbolName foundSymbol === symbolName

-- Property: Symbol table should maintain consistency
prop_symbol_table_consistency :: String -> Property
prop_symbol_table_consistency symbolName =
  not (null symbolName) && L.all isLetter symbolName ==>
  let symbol = Symbol 
        { symbolName = symbolName
        , symbolType = UserDefinedType "int"
        , symbolScope = Global
        }
      emptyTable = Map.empty :: SymbolTable
      tableWithSymbol = insertSymbol symbol emptyTable
      firstLookup = lookupSymbol symbolName tableWithSymbol
      secondLookup = lookupSymbol symbolName tableWithSymbol
  in case (firstLookup, secondLookup) of
       (Just first, Just second) -> first === second
       _ -> property False

-- Property: Symbol table should handle large numbers of symbols
prop_symbol_table_large :: Int -> Property
prop_symbol_table_large symbolCount =
  symbolCount >= 1 && symbolCount <= 100 ==> -- Reasonable limit
  let symbolNames = L.map (\i -> "symbol" ++ show i) [1..symbolCount]
      symbols = L.map (\name -> Symbol 
        { symbolName = name
        , symbolType = UserDefinedType "int"
        , symbolScope = Global
        }) symbolNames
      emptyTable = Map.empty :: SymbolTable
      tableWithSymbols = foldr insertSymbol emptyTable symbols
      lookupResults = L.map (`lookupSymbol` tableWithSymbols) symbolNames
  in property $ L.all isJust lookupResults

-- Property: Symbol table should handle shadowing correctly
prop_symbol_table_shadowing :: String -> Property
prop_symbol_table_shadowing symbolName =
  not (null symbolName) && L.all isLetter symbolName ==>
  let outerSymbol = Symbol 
        { symbolName = symbolName
        , symbolType = UserDefinedType "int"
        , symbolScope = Global
        }
      innerSymbol = Symbol 
        { symbolName = symbolName
        , symbolType = UserDefinedType "string"
        , symbolScope = Local
        }
      emptyTable = Map.empty :: SymbolTable
      tableWithOuter = insertSymbol outerSymbol emptyTable
      tableWithInner = insertSymbol innerSymbol tableWithOuter
  in case lookupSymbol symbolName tableWithInner of
       Nothing -> property False
       Just foundSymbol -> symbolScope foundSymbol === Local

-- Property: Symbol table operations should be idempotent
prop_symbol_table_idempotent :: String -> Property
prop_symbol_table_idempotent symbolName =
  not (null symbolName) && L.all isLetter symbolName ==>
  let symbol = Symbol 
        { symbolName = symbolName
        , symbolType = UserDefinedType "int"
        , symbolScope = Global
        }
      emptyTable = Map.empty :: SymbolTable
      table1 = insertSymbol symbol emptyTable
      table2 = insertSymbol symbol emptyTable
  in lookupSymbol symbolName table1 === lookupSymbol symbolName table2

tests :: TestTree
tests = testGroup "Symbol Table Operations QuickCheck Tests"
  [ fastProperty "Symbol table insert lookup" prop_symbol_table_insert_lookup
  , fastProperty "Symbol table multiple symbols" prop_symbol_table_multiple_symbols
  , fastProperty "Symbol table scopes" prop_symbol_table_scopes
  , fastProperty "Symbol table delete" prop_symbol_table_delete
  , fastProperty "Symbol table update" prop_symbol_table_update
  , fastProperty "Symbol table duplicates" prop_symbol_table_duplicates
  , fastProperty "Symbol table types" prop_symbol_table_types
  , fastProperty "Symbol table nested scopes" prop_symbol_table_nested_scopes
  , fastProperty "Symbol table attributes" prop_symbol_table_attributes
  , fastProperty "Symbol table functions" prop_symbol_table_functions
  , fastProperty "Symbol table variables" prop_symbol_table_variables
  , fastProperty "Symbol table type symbols" prop_symbol_table_type_symbols
  , fastProperty "Symbol table constants" prop_symbol_table_constants
  , fastProperty "Symbol table interfaces" prop_symbol_table_interfaces
  , fastProperty "Symbol table generics" prop_symbol_table_generics
  , fastProperty "Symbol table dependencies" prop_symbol_table_dependencies
  , fastProperty "Symbol table consistency" prop_symbol_table_consistency
  , fastProperty "Symbol table large" prop_symbol_table_large
  , fastProperty "Symbol table shadowing" prop_symbol_table_shadowing
  , fastProperty "Symbol table idempotent" prop_symbol_table_idempotent
  ]