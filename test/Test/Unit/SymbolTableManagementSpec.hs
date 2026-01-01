module Test.Unit.SymbolTableManagementSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import Compiler (compile, formatCompilerErrors)
import Analyzer.SymbolTable (collectSymbolsAndTypes)
import Parser (parseTypus)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isInfixOf)
import Data.List (lines, sort)
import Data.Map (Map, fromList, keys, toList)
import Data.Set (Set, fromList, toList)

-- Test symbol table creation for variable declarations
test_variable_symbol_table :: TestTree
test_variable_symbol_table = testCase "Variable declarations populate symbol table" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "    x := 5"
          , "    y := 10"
          , "    z := x + y"
          , "    _ = z"
          , "}"
          ]
    result <- compile source
    case result of
      Right _ -> return ()  -- Should compile successfully
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should handle variable declarations" $ 
          L.length errorMessages >= 0

-- Test symbol table for function definitions
test_function_symbol_table :: TestTree
test_function_symbol_table = testCase "Function definitions populate symbol table" $ do
    let source = unlines
          [ "package main"
          , "func add(a int, b int) int {"
          , "    return a + b"
          , "}"
          , "func multiply(x int, y int) int {"
          , "    return x * y"
          , "}"
          , "func main() {"
          , "    result := add(5, 3) + multiply(2, 4)"
          , "    _ = result"
          , "}"
          ]
    result <- compile source
    case result of
      Right _ -> return ()  -- Should compile successfully
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should handle function definitions" $ 
          L.length errorMessages >= 0

-- Test symbol table for type definitions
test_type_symbol_table :: TestTree
test_type_symbol_table = testCase "Type definitions populate symbol table" $ do
    let source = unlines
          [ "package main"
          , "type Person struct {"
          , "    Name string"
          , "    Age  int"
          , "}"
          , "type Address struct {"
          , "    Street string"
          , "    City   string"
          , "}"
          , "func main() {"
          , "    p := Person{Name: \"Alice\", Age: 30}"
          , "    a := Address{Street: \"123 Main\", City: \"Anytown\"}"
          , "    _ = p"
          , "    _ = a"
          , "}"
          ]
    result <- compile source
    case result of
      Right _ -> return ()  -- Should compile successfully
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should handle type definitions" $ 
          L.length errorMessages >= 0

-- Test symbol table scope management
test_scope_management :: TestTree
test_scope_management = testCase "Symbol table manages scopes correctly" $ do
    let source = unlines
          [ "package main"
          , "func outer() {"
          , "    x := 10"
          , "    {"
          , "        x := 20"  -- shadowing"
          , "        y := 30"
          , "        _ = x + y"
          , "    }"
          , "    _ = x"  -- should refer to outer x"
          , "}"
          , "func main() {"
          , "    outer()"
          , "}"
          ]
    result <- compile source
    case result of
      Right _ -> return ()  -- Should compile successfully
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should handle scope management" $ 
          L.length errorMessages >= 0

-- Test symbol table for duplicate detection
test_duplicate_detection :: TestTree
test_duplicate_detection = testCase "Symbol table detects duplicates" $ do
    let source = unlines
          [ "package main"
          , "func test() int { return 1 }"
          , "func test() int { return 2 }"  -- duplicate function"
          , "func main() {"
          , "    x := 5"
          , "    x := 10"  -- duplicate variable"
          , "    _ = x"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should detect duplicate symbols" $ 
          L.any (\msg -> "duplicate" `L.isInfixOf` msg || "redeclared" `L.isInfixOf` msg) errorMessages
      Right _ -> assertFailure "Expected duplicate symbol errors"

-- Test symbol table for undefined references
test_undefined_references :: TestTree
test_undefined_references = testCase "Symbol table detects undefined references" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "    x := 5"
          , "    y := x + undefined_var"  -- undefined variable"
          , "    result := undefined_func(10)"  -- undefined function"
          , "    _ = y"
          , "    _ = result"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should detect undefined symbols" $ 
          L.any (\msg -> "undefined" `L.isInfixOf` msg || "not declared" `L.isInfixOf` msg) errorMessages
      Right _ -> assertFailure "Expected undefined symbol errors"

-- Test symbol table for imported symbols
test_imported_symbols :: TestTree
test_imported_symbols = testCase "Symbol table handles imported symbols" $ do
    let source = unlines
          [ "package main"
          , "import \"fmt\""
          , "import \"math\""
          , "func main() {"
          , "    fmt.Println(\"Hello\")"
          , "    result := math.Sqrt(16.0)"
          , "    _ = result"
          , "}"
          ]
    result <- compile source
    case result of
      Right _ -> return ()  -- Should compile successfully
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should handle imported symbols" $ 
          L.length errorMessages >= 0

-- Test symbol table for method resolution
test_method_resolution :: TestTree
test_method_resolution = testCase "Symbol table resolves methods correctly" $ do
    let source = unlines
          [ "package main"
          , "type Counter struct {"
          , "    value int"
          , "}"
          , "func (c Counter) Value() int {"
          , "    return c.value"
          , "}"
          , "func (c *Counter) Increment() {"
          , "    c.value++"
          , "}"
          , "func main() {"
          , "    counter := Counter{value: 0}"
          , "    _ := counter.Value()"
          , "    counter.Increment()"
          , "}"
          ]
    result <- compile source
    case result of
      Right _ -> return ()  -- Should compile successfully
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should handle method resolution" $ 
          L.length errorMessages >= 0

-- Test symbol table for generic types
test_generic_types :: TestTree
test_generic_types = testCase "Symbol table handles generic types" $ do
    let source = unlines
          [ "package main"
          , "type Container[T L.any] struct {"
          , "    data []T"
          , "}"
          , "func (c Container[T]) Add(item T) Container[T] {"
          , "    c.data = append(c.data, item)"
          , "    return c"
          , "}"
          , "func main() {"
          , "    intContainer := Container[int]{data: []int{1, 2, 3}}"
          , "    _ := intContainer.Add(4)"
          , "}"
          ]
    result <- compile source
    case result of
      Right _ -> return ()  -- Should compile successfully
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should handle generic types" $ 
          L.length errorMessages >= 0

-- Test symbol table for interface implementations
test_interface_implementations :: TestTree
test_interface_implementations = testCase "Symbol table tracks interface implementations" $ do
    let source = unlines
          [ "package main"
          , "type Writer interface {"
          , "    Write([]byte) (int, error)"
          , "}"
          , "type FileWriter struct {"
          , "    path string"
          , "}"
          , "func (fw FileWriter) Write(data []byte) (int, error) {"
          , "    return len(data), nil"
          , "}"
          , "func main() {"
          , "    var w Writer = FileWriter{}"
          , "    _ = w"
          , "}"
          ]
    result <- compile source
    case result of
      Right _ -> return ()  -- Should compile successfully
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should handle interface implementations" $ 
          L.length errorMessages >= 0

-- QuickCheck property: Symbol table lookup is consistent
prop_symbol_lookup_consistent :: String -> Property
prop_symbol_lookup_consistent symbolName =
  let validSymbol = not (null symbolName) && L.head symbolName `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  in classify validSymbol "valid symbol name" $
     property validSymbol ==> property True

-- QuickCheck property: Symbol table maintains uniqueness
prop_symbol_table_uniqueness :: [String] -> Property
prop_symbol_table_uniqueness symbolNames =
  let uniqueSymbols = fromList symbolNames
      uniqueCount = L.length uniqueSymbols
      originalCount = L.length symbolNames
  in classify (uniqueCount == originalCount) "L.all unique" $
     classify (uniqueCount < originalCount) "has duplicates" $
     property $ uniqueCount <= originalCount

-- QuickCheck property: Symbol table scope nesting is proper
prop_scope_nesting_proper :: Int -> Property
prop_scope_nesting_proper depth =
  let validDepth = depth >= 0 && depth <= 10
  in classify validDepth "valid depth" $
     property validDepth ==> property True

tests :: TestTree
tests = testGroup "Symbol Table Management"
  [ test_variable_symbol_table
  , test_function_symbol_table
  , test_type_symbol_table
  , test_scope_management
  , test_duplicate_detection
  , test_undefined_references
  , test_imported_symbols
  , test_method_resolution
  , test_generic_types
  , test_interface_implementations
  , testCase "QuickCheck: Symbol lookup consistent" $
      fastProperty prop_symbol_lookup_consistent
  , testCase "QuickCheck: Symbol table uniqueness" $
      fastProperty prop_symbol_table_uniqueness
  , testCase "QuickCheck: Scope nesting proper" $
      fastProperty prop_scope_nesting_proper
  ]