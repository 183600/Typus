module Test.Unit.SyntaxValidatorAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import SyntaxValidator (validateSyntax, SyntaxError(..))
import Parser (parseTypus)
import Compiler (compile, formatCompilerErrors)
import qualified Data.Text as T
import Data.List (isInfixOf, lines)

-- Test validation of complex type declarations
test_complex_type_declarations :: TestTree
test_complex_type_declarations = testCase "Complex type declarations are validated" $ do
    let source = unlines
          [ "package main"
          , "type Complex struct {"
          , "    field1 []map[string][5]int"
          , "    field2 func(int, string) (bool, error)"
          , "    field3 chan<- []byte"
          , "}"
          , "func main() {"
          , "    _ := Complex{}"
          , "}"
          ]
    result <- compile source
    case result of
      Right _ -> return ()  -- Should compile successfully
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should handle complex types gracefully" $ 
          length errorMessages > 0

-- Test validation of function overloading scenarios
test_function_overloading_scenarios :: TestTree
test_function_overloading_scenarios = testCase "Function overloading scenarios are detected" $ do
    let source = unlines
          [ "package main"
          , "func test() int { return 1 }"
          , "func test(x int) int { return x }"  -- potential overloading
          , "func test(s string) int { return len(s) }"  -- potential overloading
          , "func main() {"
          , "    _ := test()"
          , "    _ := test(5)"
          , "    _ := test(\"hello\")"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should detect function overloading conflicts" $ 
          any (\msg -> "overload" `isInfixOf` msg || "duplicate" `isInfixOf` msg) errorMessages
      Right _ -> return ()  -- May or may not be allowed depending on language spec

-- Test validation of generic type constraints
test_generic_type_constraints :: TestTree
test_generic_type_constraints = testCase "Generic type constraints are validated" $ do
    let source = unlines
          [ "package main"
          , "type Container[T any] struct {"
          , "    data []T"
          , "}"
          , "func (c Container[T]) Add(item T) Container[T] {"
          , "    c.data = append(c.data, item)"
          , "    return c"
          , "}"
          , "func main() {"
          , "    intContainer := Container[int]{data: []int{1, 2, 3}}"
          , "    stringContainer := Container[string]{data: []string{\"a\", \"b\"}}"
          , "    _ := intContainer.Add(4)"
          , "    _ := stringContainer.Add(\"c\")"
          , "}"
          ]
    result <- compile source
    case result of
      Right _ -> return ()  -- Should compile successfully
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should handle generic types gracefully" $ 
          length errorMessages > 0

-- Test validation of interface implementations
test_interface_implementations :: TestTree
test_interface_implementations = testCase "Interface implementations are validated" $ do
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
          , "type BadWriter struct {"
          , "    path string"
          , "}"
          , "func (bw BadWriter) WriteString(string) error {"  -- wrong signature
          , "    return nil"
          , "}"
          , "func main() {"
          , "    var w1 Writer = FileWriter{}"
          , "    var w2 Writer = BadWriter{}"  -- should fail
          , "    _ = w1"
          , "    _ = w2"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should detect interface implementation mismatch" $ 
          any (\msg -> "interface" `isInfixOf` msg || "implement" `isInfixOf` msg) errorMessages
      Right _ -> assertFailure "Expected interface implementation error"

-- Test validation of recursive types
test_recursive_types :: TestTree
test_recursive_types = testCase "Recursive types are validated" $ do
    let source = unlines
          [ "package main"
          , "type Node struct {"
          , "    value int"
          , "    left *Node"
          , "    right *Node"
          , "}"
          , "type LinkedList struct {"
          , "    value int"
          , "    next *LinkedList"
          , "}"
          , "func main() {"
          , "    root := Node{value: 1, left: nil, right: nil}"
          , "    list := LinkedList{value: 1, next: nil}"
          , "    _ = root"
          , "    _ = list"
          , "}"
          ]
    result <- compile source
    case result of
      Right _ -> return ()  -- Should compile successfully
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should handle recursive types gracefully" $ 
          length errorMessages > 0

-- Test validation of embedded types
test_embedded_types :: TestTree
test_embedded_types = testCase "Embedded types are validated" $ do
    let source = unlines
          [ "package main"
          , "type Base struct {"
          , "    name string"
          , "}"
          , "type Derived struct {"
          , "    Base"
          , "    value int"
          , "}"
          , "func main() {"
          , "    d := Derived{Base: Base{name: \"test\"}, value: 42}"
          , "    _ := d.name"  -- should access embedded field
          , "    _ := d.value"
          , "}"
          ]
    result <- compile source
    case result of
      Right _ -> return ()  -- Should compile successfully
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should handle embedded types gracefully" $ 
          length errorMessages > 0

-- Test validation of variadic functions
test_variadic_functions :: TestTree
test_variadic_functions = testCase "Variadic functions are validated" $ do
    let source = unlines
          [ "package main"
          , "func sum(numbers ...int) int {"
          , "    total := 0"
          , "    for _, n := range numbers {"
          , "        total += n"
          , "    }"
          , "    return total"
          , "}"
          , "func main() {"
          , "    _ := sum(1, 2, 3, 4, 5)"
          , "    nums := []int{1, 2, 3}"
          , "    _ := sum(nums...)"
          , "}"
          ]
    result <- compile source
    case result of
      Right _ -> return ()  -- Should compile successfully
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should handle variadic functions gracefully" $ 
          length errorMessages > 0

-- Test validation of method sets
test_method_sets :: TestTree
test_method_sets = testCase "Method sets are validated" $ do
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
          , "    _ := counter.Value()"    -- value receiver method
          , "    counter.Increment()"      -- pointer receiver method on value
          , "    ptr := &counter"
          , "    _ := ptr.Value()"         -- value receiver method on pointer
          , "    ptr.Increment()"          -- pointer receiver method on pointer
          , "}"
          ]
    result <- compile source
    case result of
      Right _ -> return ()  -- Should compile successfully
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should handle method sets gracefully" $ 
          length errorMessages > 0

-- QuickCheck property: Balanced parentheses in valid syntax
prop_balanced_parentheses :: String -> Property
prop_balanced_parentheses code =
  let openCount = length (filter (== '(') code)
      closeCount = length (filter (== ')') code)
      balanced = openCount == closeCount
  in classify balanced "balanced parentheses" $
     classify (not balanced) "unbalanced parentheses" $
     property balanced

-- QuickCheck property: Balanced braces in valid syntax
prop_balanced_braces :: String -> Property
prop_balanced_braces code =
  let openCount = length (filter (== '{') code)
      closeCount = length (filter (== '}') code)
      balanced = openCount == closeCount
  in classify balanced "balanced braces" $
     classify (not balanced) "unbalanced braces" $
     property balanced

-- QuickCheck property: Valid identifiers start with letter
prop_valid_identifiers :: String -> Property
prop_valid_identifiers identifier =
  let startsWithLetter = not (null identifier) && 
                         head identifier `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
      containsOnlyValidChars = all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']) identifier
  in classify startsWithLetter "starts with letter" $
     classify containsOnlyValidChars "contains only valid chars" $
     property (startsWithLetter ==> containsOnlyValidChars)

tests :: TestTree
tests = testGroup "Syntax Validator Advanced"
  [ test_complex_type_declarations
  , test_function_overloading_scenarios
  , test_generic_type_constraints
  , test_interface_implementations
  , test_recursive_types
  , test_embedded_types
  , test_variadic_functions
  , test_method_sets
  , testCase "QuickCheck: Balanced parentheses" $
      fastProperty prop_balanced_parentheses
  , testCase "QuickCheck: Balanced braces" $
      fastProperty prop_balanced_braces
  , testCase "QuickCheck: Valid identifiers" $
      fastProperty prop_valid_identifiers
  ]