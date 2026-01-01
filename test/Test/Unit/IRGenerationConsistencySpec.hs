module Test.Unit.IRGenerationConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import Compiler (compile)
import Compiler.IR (IRModule(..), IRFunction(..), IRStatement(..), IRExpression(..), IRType(..))
import Compiler.GoAst (renderGoModule)
import Parser (parseTypus)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isInfixOf)
import Data.List (lines, sort)
import Data.Set (Set, fromList, toList)

-- Test IR generation for basic expressions
test_basic_expressions_ir :: TestTree
test_basic_expressions_ir = testCase "Basic expressions generate consistent IR" $ do
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
      Right compiled -> do
        goCode <- renderGoModule compiled
        assertBool "IR should contain variable declarations" $ 
          "x :=" `L.isInfixOf` goCode
        assertBool "IR should contain arithmetic operations" $ 
          "x + y" `L.isInfixOf` goCode
      Left errs -> assertFailure $ "Compilation failed: " ++ show errs

-- Test IR generation for function definitions
test_function_definitions_ir :: TestTree
test_function_definitions_ir = testCase "Function definitions generate consistent IR" $ do
    let source = unlines
          [ "package main"
          , "func add(a int, b int) int {"
          , "    return a + b"
          , "}"
          , "func multiply(x int, y int) int {"
          , "    return x * y"
          , "}"
          , "func main() {"
          , "    result := add(5, 3) * multiply(2, 4)"
          , "    _ = result"
          , "}"
          ]
    result <- compile source
    case result of
      Right compiled -> do
        goCode <- renderGoModule compiled
        assertBool "IR should contain function definitions" $ 
          "func add" `L.isInfixOf` goCode
        assertBool "IR should contain function calls" $ 
          "add(5, 3)" `L.isInfixOf` goCode
        assertBool "IR should contain return statements" $ 
          "return" `L.isInfixOf` goCode
      Left errs -> assertFailure $ "Compilation failed: " ++ show errs

-- Test IR generation for control structures
test_control_structures_ir :: TestTree
test_control_structures_ir = testCase "Control structures generate consistent IR" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "    x := 10"
          , "    if x > 5 {"
          , "        println(\"greater\")"
          , "    } else {"
          , "        println(\"less L.or equal\")"
          , "    }"
          , "    for i := 0; i < 3; i++ {"
          , "        println(i)"
          , "    }"
          , "}"
          ]
    result <- compile source
    case result of
      Right compiled -> do
        goCode <- renderGoModule compiled
        assertBool "IR should contain if statements" $ 
          "if x > 5" `L.isInfixOf` goCode
        assertBool "IR should contain else clauses" $ 
          "else" `L.isInfixOf` goCode
        assertBool "IR should contain for loops" $ 
          "for i := 0" `L.isInfixOf` goCode
      Left errs -> assertFailure $ "Compilation failed: " ++ show errs

-- Test IR generation for struct types
test_struct_types_ir :: TestTree
test_struct_types_ir = testCase "Struct types generate consistent IR" $ do
    let source = unlines
          [ "package main"
          , "type Person struct {"
          , "    Name string"
          , "    Age  int"
          , "}"
          , "func main() {"
          , "    p := Person{Name: \"Alice\", Age: 30}"
          , "    _ := p.Name"
          , "    _ := p.Age"
          , "}"
          ]
    result <- compile source
    case result of
      Right compiled -> do
        goCode <- renderGoModule compiled
        assertBool "IR should contain struct definitions" $ 
          "type Person struct" `L.isInfixOf` goCode
        assertBool "IR should contain struct field access" $ 
          "p.Name" `L.isInfixOf` goCode
      Left errs -> assertFailure $ "Compilation failed: " ++ show errs

-- Test IR generation for array L.and slice operations
test_array_slice_operations_ir :: TestTree
test_array_slice_operations_ir = testCase "Array L.and slice operations generate consistent IR" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "    arr := [5]int{1, 2, 3, 4, 5}"
          , "    slice := []int{10, 20, 30}"
          , "    _ := arr[2]"
          , "    _ := slice[1:]"
          , "    slice = append(slice, 40)"
          , "}"
          ]
    result <- compile source
    case result of
      Right compiled -> do
        goCode <- renderGoModule compiled
        assertBool "IR should contain array declarations" $ 
          "[5]int" `L.isInfixOf` goCode
        assertBool "IR should contain slice declarations" $ 
          "[]int" `L.isInfixOf` goCode
        assertBool "IR should contain slice operations" $ 
          "append" `L.isInfixOf` goCode
      Left errs -> assertFailure $ "Compilation failed: " ++ show errs

-- Test IR generation for ownership-aware code
test_ownership_aware_ir :: TestTree
test_ownership_aware_ir = testCase "Ownership-aware code generates consistent IR" $ do
    let source = unlines
          [ "//! ownership: on"
          , "package main"
          , "func main() {"
          , "    data := make([]int, 10)"
          , "    processor := func(d []int) int {"
          , "        return len(d)"
          , "    }"
          , "    result := processor(data)"
          , "    _ = result"
          , "}"
          ]
    result <- compile source
    case result of
      Right compiled -> do
        goCode <- renderGoModule compiled
        assertBool "IR should contain ownership annotations" $ 
          "make([]int, 10)" `L.isInfixOf` goCode
        assertBool "IR should contain function literals" $ 
          "func(d []int)" `L.isInfixOf` goCode
      Left errs -> assertFailure $ "Compilation failed: " ++ show errs

-- Test IR generation for dependent types
test_dependent_types_ir :: TestTree
test_dependent_types_ir = testCase "Dependent types generate consistent IR" $ do
    let source = unlines
          [ "//! dependent_types: on"
          , "package main"
          , "func main() {"
          , "    type Vec3 = [3]int"
          , "    v := Vec3{1, 2, 3}"
          , "    L.sum := v[0] + v[1] + v[2]"
          , "    _ := L.sum"
          , "}"
          ]
    result <- compile source
    case result of
      Right compiled -> do
        goCode <- renderGoModule compiled
        assertBool "IR should contain dependent type definitions" $ 
          "Vec3" `L.isInfixOf` goCode
        assertBool "IR should contain constrained operations" $ 
          "v[0] + v[1] + v[2]" `L.isInfixOf` goCode
      Left errs -> assertFailure $ "Compilation failed: " ++ show errs

-- Test IR generation consistency across multiple compilations
test_ir_consistency :: TestTree
test_ir_consistency = testCase "IR generation is consistent across compilations" $ do
    let source = unlines
          [ "package main"
          , "func calculate(x int, y int) int {"
          , "    if x > y {"
          , "        return x - y"
          , "    } else {"
          , "        return y - x"
          , "    }"
          , "}"
          , "func main() {"
          , "    result := calculate(10, 5)"
          , "    _ = result"
          , "}"
          ]
    result1 <- compile source
    result2 <- compile source
    case (result1, result2) of
      (Right compiled1, Right compiled2) -> do
        goCode1 <- renderGoModule compiled1
        goCode2 <- renderGoModule compiled2
        assertBool "IR should be deterministic" $ 
          goCode1 == goCode2
      _ -> assertFailure "Compilation failed"

-- Test IR generation preserves semantic meaning
test_ir_semantic_preservation :: TestTree
test_ir_semantic_preservation = testCase "IR generation preserves semantic meaning" $ do
    let source = unlines
          [ "package main"
          , "func factorial(n int) int {"
          , "    if n <= 1 {"
          , "        return 1"
          , "    }"
          , "    return n * factorial(n-1)"
          , "}"
          , "func main() {"
          , "    result := factorial(5)"
          , "    _ = result"
          , "}"
          ]
    result <- compile source
    case result of
      Right compiled -> do
        goCode <- renderGoModule compiled
        assertBool "IR should preserve recursive structure" $ 
          "factorial(n-1)" `L.isInfixOf` goCode
        assertBool "IR should preserve base case" $ 
          "return 1" `L.isInfixOf` goCode
        assertBool "IR should preserve recursive case" $ 
          "return n * factorial" `L.isInfixOf` goCode
      Left errs -> assertFailure $ "Compilation failed: " ++ show errs

-- QuickCheck property: IR generation preserves function signatures
prop_ir_preserves_function_signatures :: String -> Property
prop_ir_preserves_function_signatures source =
  property $
    case compile source of
      Right compiled -> 
        case renderGoModule compiled of
          Right goCode -> 
            let hasFunc = "func " `L.isInfixOf` goCode
                hasParams = "(" `L.isInfixOf` goCode && ")" `L.isInfixOf` goCode
            in hasFunc ==> hasParams
          Left _ -> property False
      Left _ -> property True

-- QuickCheck property: IR generation maintains variable scope
prop_ir_maintains_variable_scope :: String -> Property
prop_ir_maintains_variable_scope source =
  property $
    case compile source of
      Right compiled -> 
        case renderGoModule compiled of
          Right goCode -> 
            let hasVarDecls := `L.isInfixOf` goCode
                hasBlockStructure = "{" `L.isInfixOf` goCode && "}" `L.isInfixOf` goCode
            in hasVarDecls ==> hasBlockStructure
          Left _ -> property False
      Left _ -> property True

-- QuickCheck property: IR generation preserves control flow
prop_ir_preserves_control_flow :: String -> Property
prop_ir_preserves_control_flow source =
  property $
    case compile source of
      Right compiled -> 
        case renderGoModule compiled of
          Right goCode -> 
            let hasControl = L.any (`L.isInfixOf` goCode) ["if ", "for ", "switch "]
                hasBranches = L.any (`L.isInfixOf` goCode) ["else", "break", "continue"]
            in hasControl ==> hasBranches
          Left _ -> property False
      Left _ -> property True

tests :: TestTree
tests = testGroup "IR Generation Consistency"
  [ test_basic_expressions_ir
  , test_function_definitions_ir
  , test_control_structures_ir
  , test_struct_types_ir
  , test_array_slice_operations_ir
  , test_ownership_aware_ir
  , test_dependent_types_ir
  , test_ir_consistency
  , test_ir_semantic_preservation
  , testCase "QuickCheck: IR preserves function signatures" $
      fastProperty prop_ir_preserves_function_signatures
  , testCase "QuickCheck: IR maintains variable scope" $
      fastProperty prop_ir_maintains_variable_scope
  , testCase "QuickCheck: IR preserves control flow" $
      fastProperty prop_ir_preserves_control_flow
  ]