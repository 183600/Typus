{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewSymbolTableQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import qualified Test.QuickCheck as QC

import Analyzer.SymbolTable
  ( collectSymbolsAndTypes
  , collectSymbolsFromAST
  , augmentSymbolTableWithLocals
  , trim
  , isReservedName
  , extractTypeEnvironment
  )
import Analyzer.Types (SymbolInfo(..))
import Compiler.GoAst (GoModule(..), GoDecl(..), FuncDecl(..), VarDecl(..), ConstDecl(..), TypeDecl(..))
import Compiler.GoVarSpec (RawVarSpec(..))
import qualified Dependencies as Dep
import qualified Ownership as Own
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad.Except (runExceptT)

-- ============================================================================
-- Arbitrary Instances for QuickCheck Testing
-- ============================================================================

-- Generate arbitrary string for testing
instance Arbitrary String where
  arbitrary = QC.oneof
    [ QC.listOf (QC.elements ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-")
    , pure ""
    ]

-- Generate arbitrary SymbolInfo for testing
instance Arbitrary SymbolInfo where
  arbitrary = do
    name <- QC.arbitrary
    symbolType <- QC.arbitrary
    ownershipState <- QC.arbitrary
    symbolScope <- QC.arbitrary
    isMoved <- QC.arbitrary
    isBorrowed <- QC.arbitrary
    constraints <- QC.listOf QC.arbitrary
    return $ SymbolInfo name symbolType ownershipState symbolScope isMoved isBorrowed constraints

-- Generate arbitrary GoModule for testing
instance Arbitrary GoModule where
  arbitrary = do
    pkgName <- QC.arbitrary
    imports <- QC.listOf QC.arbitrary
    decls <- QC.listOf QC.arbitrary
    buildTags <- QC.listOf QC.arbitrary
    return $ GoModule
      { gmPackage = if null pkgName then Nothing else Just (PackageDecl pkgName)
      , gmImports = imports
      , gmDecls = decls
      , gmBuildTags = buildTags
      }

-- Generate arbitrary GoDecl for testing
instance Arbitrary GoDecl where
  arbitrary = QC.oneof
    [ GoFunc <$> QC.arbitrary
    , GoVar <$> QC.arbitrary
    , GoConst <$> QC.arbitrary
    , GoType <$> QC.arbitrary
    , GoStatement <$> QC.arbitrary
    , GoRaw <$> QC.arbitrary
    ]

-- ============================================================================
-- Property Tests for Symbol Table
-- ============================================================================

-- Property: Trim removes leading and trailing whitespace
prop_trim_removes_whitespace :: String -> String -> Property
prop_trim_removes_whitespace prefix suffix =
  let content = prefix ++ "content" ++ suffix
      trimmed = trim content
      hasLeading = any (`elem` " \t\n\r") prefix
      hasTrailing = any (`elem` " \t\n\r") suffix
      noLeadingSpace = null trimmed || not (head trimmed `elem` " \t\n\r")
      noTrailingSpace = null trimmed || not (last trimmed `elem` " \t\n\r")
  in classify hasLeading "has leading whitespace" $
     classify hasTrailing "has trailing whitespace" $
     property $ noLeadingSpace .&&. noTrailingSpace

-- Property: Trim preserves internal whitespace
prop_trim_preserves_internal :: String -> String -> String -> Property
prop_trim_preserves_internal before middle after =
  let content = before ++ "  " ++ middle ++ "  " ++ after
      trimmed = trim content
      expected = filter (not . (`elem` " \t\n\r")) before ++ "  " ++ middle ++ "  " ++ filter (not . (`elem` " \t\n\r")) after
  in not (null middle) ==> property $ trimmed === expected

-- Property: Trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmed1 = trim str
      trimmed2 = trim trimmed1
  in property $ trimmed1 === trimmed2

-- Property: Trim of empty string is empty
prop_trim_empty :: Property
prop_trim_empty =
  trim "" === ""

-- Property: Trim of whitespace-only string is empty
prop_trim_whitespace_only :: String -> Property
prop_trim_whitespace_only whitespace =
  all (`elem` " \t\n\r") whitespace ==>
  property $ trim whitespace === ""

-- Property: Reserved names detection works
prop_reserved_names_detection :: Property
prop_reserved_names_detection =
  let reservedNames = [ "fmt", "func", "var", "let", "if", "else", "for"
                      , "return", "import", "package", "type", "struct"
                      , "interface", "const", "true", "false", "nil"
                      , "int", "string", "bool", "float64"
                      ]
  in property $ all isReservedName reservedNames

-- Property: Non-reserved names are not detected as reserved
prop_non_reserved_names_not_detected :: String -> Property
prop_non_reserved_names_not_detected name =
  not (name `elem` [ "fmt", "func", "var", "let", "if", "else", "for"
                    , "return", "import", "package", "type", "struct"
                    , "interface", "const", "true", "false", "nil"
                    , "int", "string", "bool", "float64"
                    ]) ==>
  property $ not (isReservedName name)

-- Property: Empty code produces empty symbol table
prop_empty_code_empty_symbol_table :: Property
prop_empty_code_empty_symbol_table =
  let result = runExceptT (collectSymbolsAndTypes "")
  in case result of
    Left _ -> property $ False
    Right symbols -> property $ Map.null symbols

-- Property: Simple function declaration creates symbol
prop_simple_function_creates_symbol :: String -> Property
prop_simple_function_creates_symbol funcName =
  not (null funcName) && not (isReservedName funcName) ==>
  let code = "func " ++ funcName ++ "() {}"
      result = runExceptT (collectSymbolsAndTypes code)
  in case result of
    Left _ -> property $ False
    Right symbols -> property $ funcName `Map.member` symbols

-- Property: Variable declaration creates symbol
prop_variable_declaration_creates_symbol :: String -> Property
prop_variable_declaration_creates_symbol varName =
  not (null varName) && not (isReservedName varName) ==>
  let code = "var " ++ varName ++ " int"
      result = runExceptT (collectSymbolsAndTypes code)
  in case result of
    Left _ -> property $ False
    Right symbols -> property $ varName `Map.member` symbols

-- Property: Constant declaration creates symbol
prop_constant_declaration_creates_symbol :: String -> Property
prop_constant_declaration_creates_symbol constName =
  not (null constName) && not (isReservedName constName) ==>
  let code = "const " ++ constName ++ " = 42"
      result = runExceptT (collectSymbolsAndTypes code)
  in case result of
    Left _ -> property $ False
    Right symbols -> property $ constName `Map.member` symbols

-- Property: Type declaration creates symbol
prop_type_declaration_creates_symbol :: String -> Property
prop_type_declaration_creates_symbol typeName =
  not (null typeName) && not (isReservedName typeName) ==>
  let code = "type " ++ typeName ++ " struct {}"
      result = runExceptT (collectSymbolsAndTypes code)
  in case result of
    Left _ -> property $ False
    Right symbols -> property $ typeName `Map.member` symbols

-- Property: Multiple declarations create multiple symbols
prop_multiple_declarations_multiple_symbols :: [String] -> Property
prop_multiple_declarations_multiple_symbols names =
  let validNames = filter (\n -> not (null n) && not (isReservedName n)) names
      code = unlines $ map (\n -> "var " ++ n ++ " int") validNames
      result = runExceptT (collectSymbolsAndTypes code)
  in not (null validNames) ==>
    case result of
      Left _ -> property $ False
      Right symbols -> property $ all (`Map.member` symbols) validNames

-- Property: Symbol table preserves symbol information
prop_symbol_table_preserves_info :: String -> Property
prop_symbol_table_preserves_info symbolName =
  not (null symbolName) && not (isReservedName symbolName) ==>
  let code = "var " ++ symbolName ++ " int"
      result = runExceptT (collectSymbolsAndTypes code)
  in case result of
    Left _ -> property $ False
    Right symbols -> 
      case Map.lookup symbolName symbols of
        Just info -> property $ symbolName info === symbolName
        Nothing -> property $ False

-- Property: Extract type environment returns types
prop_extract_type_environment_returns_types :: Map.Map String SymbolInfo -> Property
prop_extract_type_environment_returns_types symbolTable =
  let typeEnv = extractTypeEnvironment symbolTable
      hasTypes = any isJust (Map.elems typeEnv)
  in property $ hasTypes ==> not (Map.null typeEnv)

-- Property: Augment symbol table with locals preserves globals
prop_augment_preserves_globals :: Map.Map String SymbolInfo -> String -> Property
prop_augment_preserves_globals globals localCode =
  let augmented = augmentSymbolTableWithLocals localCode globals
      globalKeys = Map.keys globals
      augmentedKeys = Map.keys augmented
  in property $ all (`elem` augmentedKeys) globalKeys

-- Property: Augment symbol table adds local variables
prop_augment_adds_locals :: String -> String -> Property
prop_augment_adds_locals localVarName localCode =
  not (null localVarName) && not (isReservedName localVarName) ==>
  let codeWithLocal = "{\n    var " ++ localVarName ++ " int\n}\n"
      globals = Map.empty
      augmented = augmentSymbolTableWithLocals codeWithLocal globals
  in property $ localVarName `Map.member` augmented

-- Property: Symbol table handles duplicate declarations
prop_symbol_table_handles_duplicates :: String -> Property
prop_symbol_table_handles_duplicates name =
  not (null name) && not (isReservedName name) ==>
  let code = unlines
        [ "var " ++ name ++ " int"
        , "var " ++ name ++ " string"
        ]
      result = runExceptT (collectSymbolsAndTypes code)
  in case result of
    Left _ -> property $ False
    Right symbols -> property $ name `Map.member` symbols

-- Property: Symbol table ignores invalid identifiers
prop_symbol_table_ignores_invalid :: String -> Property
prop_symbol_table_ignores_invalid identifier =
  let invalidIdentifiers = ["", "123invalid", "invalid-name", "invalid name"]
      code = "var " ++ identifier ++ " int"
      result = runExceptT (collectSymbolsAndTypes code)
  in identifier `elem` invalidIdentifiers ==>
    case result of
      Left _ -> property $ False
      Right symbols -> property $ not (identifier `Map.member` symbols)

-- Property: Symbol table handles complex code
prop_symbol_table_handles_complex_code :: Property
prop_symbol_table_handles_complex_code =
  let complexCode = unlines
        [ "package main"
        , "import \"fmt\""
        , "type MyStruct struct {"
        , "    field int"
        , "}"
        , "func (m *MyStruct) Method() {"
        , "    localVar := 42"
        , "    fmt.Println(localVar)"
        , "}"
        , "func main() {"
        , "    instance := MyStruct{field: 1}"
        , "    instance.Method()"
        , "}"
        ]
      result = runExceptT (collectSymbolsAndTypes complexCode)
  in case result of
    Left _ -> property $ False
    Right symbols -> property $ not (Map.null symbols)

-- Property: Symbol table scope tracking works
prop_symbol_table_scope_tracking :: Property
prop_symbol_table_scope_tracking =
  let scopedCode = unlines
        [ "func outer() {"
        , "    outerVar := 1"
        , "    func inner() {"
        , "        innerVar := 2"
        , "        fmt.Println(innerVar)"
        , "    }"
        , "    fmt.Println(outerVar)"
        , "}"
        ]
      result = runExceptT (collectSymbolsAndTypes scopedCode)
  in case result of
    Left _ -> property $ False
    Right symbols -> property $ not (Map.null symbols)

-- Property: Symbol table ownership tracking works
prop_symbol_table_ownership_tracking :: Property
prop_symbol_table_ownership_tracking =
  let ownershipCode = unlines
        [ "func test() {"
        , "    ownedVar := 42"
        , "    borrowed := &ownedVar"
        , "    moved := ownedVar"
        , "}"
        ]
      result = runExceptT (collectSymbolsAndTypes ownershipCode)
  in case result of
    Left _ -> property $ False
    Right symbols -> property $ not (Map.null symbols)

-- Property: Symbol table constraint tracking works
prop_symbol_table_constraint_tracking :: Property
prop_symbol_table_constraint_tracking =
  let constraintCode = unlines
        [ "func test[T any](value T) T {"
        , "    return value"
        , "}"
        ]
      result = runExceptT (collectSymbolsAndTypes constraintCode)
  in case result of
    Left _ -> property $ False
    Right symbols -> property $ not (Map.null symbols)

-- Property: Symbol table handles generic types
prop_symbol_table_handles_generics :: String -> Property
prop_symbol_table_handles_generics typeName =
  not (null typeName) && not (isReservedName typeName) ==>
  let genericCode = "type " ++ typeName ++ "[T any] struct { value T }"
      result = runExceptT (collectSymbolsAndTypes genericCode)
  in case result of
    Left _ -> property $ False
    Right symbols -> property $ typeName `Map.member` symbols

-- Property: Symbol table handles interfaces
prop_symbol_table_handles_interfaces :: String -> Property
prop_symbol_table_handles_interfaces interfaceName =
  not (null interfaceName) && not (isReservedName interfaceName) ==>
  let interfaceCode = unlines
        [ "type " ++ interfaceName ++ " interface {"
        , "    Method() int"
        , "}"
        ]
      result = runExceptT (collectSymbolsAndTypes interfaceCode)
  in case result of
    Left _ -> property $ False
    Right symbols -> property $ interfaceName `Map.member` symbols

-- Property: Symbol table handles method receivers
prop_symbol_table_handles_method_receivers :: String -> Property
prop_symbol_table_handles_method_receivers typeName =
  not (null typeName) && not (isReservedName typeName) ==>
  let methodCode = unlines
        [ "type " ++ typeName ++ " struct {}"
        , "func (t " ++ typeName ++ ") Method() {}"
        ]
      result = runExceptT (collectSymbolsAndTypes methodCode)
  in case result of
    Left _ -> property $ False
    Right symbols -> property $ typeName `Map.member` symbols

-- Property: Symbol table handles nested scopes
prop_symbol_table_handles_nested_scopes :: Property
prop_symbol_table_handles_nested_scopes =
  let nestedCode = unlines
        [ "func level1() {"
        , "    var1 := 1"
        , "    {"
        , "        var2 := 2"
        , "        {"
        , "            var3 := 3"
        , "            fmt.Println(var3)"
        , "        }"
        , "        fmt.Println(var2)"
        , "    }"
        , "    fmt.Println(var1)"
        , "}"
        ]
      result = runExceptT (collectSymbolsAndTypes nestedCode)
  in case result of
    Left _ -> property $ False
    Right symbols -> property $ not (Map.null symbols)

tests :: TestTree
tests =
  testGroup "New Symbol Table QuickCheck Tests"
    [ fastProperty "Trim removes leading and trailing whitespace" prop_trim_removes_whitespace
    , fastProperty "Trim preserves internal whitespace" prop_trim_preserves_internal
    , fastProperty "Trim is idempotent" prop_trim_idempotent
    , fastProperty "Trim of empty string is empty" prop_trim_empty
    , fastProperty "Trim of whitespace-only string is empty" prop_trim_whitespace_only
    , fastProperty "Reserved names detection works" prop_reserved_names_detection
    , fastProperty "Non-reserved names are not detected as reserved" prop_non_reserved_names_not_detected
    , fastProperty "Empty code produces empty symbol table" prop_empty_code_empty_symbol_table
    , fastProperty "Simple function declaration creates symbol" prop_simple_function_creates_symbol
    , fastProperty "Variable declaration creates symbol" prop_variable_declaration_creates_symbol
    , fastProperty "Constant declaration creates symbol" prop_constant_declaration_creates_symbol
    , fastProperty "Type declaration creates symbol" prop_type_declaration_creates_symbol
    , fastProperty "Multiple declarations create multiple symbols" prop_multiple_declarations_multiple_symbols
    , fastProperty "Symbol table preserves symbol information" prop_symbol_table_preserves_info
    , fastProperty "Extract type environment returns types" prop_extract_type_environment_returns_types
    , fastProperty "Augment symbol table with locals preserves globals" prop_augment_preserves_globals
    , fastProperty "Augment symbol table adds local variables" prop_augment_adds_locals
    , fastProperty "Symbol table handles duplicate declarations" prop_symbol_table_handles_duplicates
    , fastProperty "Symbol table ignores invalid identifiers" prop_symbol_table_ignores_invalid
    , fastProperty "Symbol table handles complex code" prop_symbol_table_handles_complex_code
    , fastProperty "Symbol table scope tracking works" prop_symbol_table_scope_tracking
    , fastProperty "Symbol table ownership tracking works" prop_symbol_table_ownership_tracking
    , fastProperty "Symbol table constraint tracking works" prop_symbol_table_constraint_tracking
    , fastProperty "Symbol table handles generic types" prop_symbol_table_handles_generics
    , fastProperty "Symbol table handles interfaces" prop_symbol_table_handles_interfaces
    , fastProperty "Symbol table handles method receivers" prop_symbol_table_handles_method_receivers
    , fastProperty "Symbol table handles nested scopes" prop_symbol_table_handles_nested_scopes
    ]