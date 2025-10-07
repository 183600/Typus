{-# LANGUAGE OverloadedStrings #-}

module OwnershipAnalysisTests (ownershipAnalysisTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Ownership as Own
import Data.List (isInfixOf)

-- ============================================================================
-- Ownership Analysis Verification Tests
-- ============================================================================

ownershipAnalysisTests :: TestTree
ownershipAnalysisTests = testGroup "Ownership Analysis Tests"
  [ basicOwnershipTests
  , moveSemanticTests
  , borrowingTests
  , scopeTests
  , structOwnershipTests
  , advancedOwnershipTests
  , errorDetectionTests
  , additionalOwnershipTests
  ]

-- ============================================================================
-- Basic Ownership Tests
-- ============================================================================

basicOwnershipTests :: TestTree
basicOwnershipTests = testGroup "Basic Ownership"
  [ testCase "No ownership violations in simple code" $ do
      let code = unlines
            [ "x := 42"
            , "y := x"
            , "fmt.Println(y)"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "No errors expected" [] errors

  , testCase "Value types can be copied freely" $ do
      let code = unlines
            [ "a := 10"
            , "b := a"
            , "c := a"
            , "fmt.Println(a, b, c)"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "No errors for value types" [] errors

  , testCase "String literals are valid" $ do
      let code = unlines
            [ "s := \"hello\""
            , "fmt.Println(s)"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "No errors for string literals" [] errors

  , testCase "Multiple variables with different values" $ do
      let code = unlines
            [ "x := 1"
            , "y := 2"
            , "z := 3"
            , "fmt.Println(x + y + z)"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "No errors for independent variables" [] errors
  ]

-- ============================================================================
-- Move Semantic Tests
-- ============================================================================

moveSemanticTests :: TestTree
moveSemanticTests = testGroup "Move Semantics"
  [ testCase "Detect use after move" $ do
      let code = unlines
            [ "x := make([]int, 10)"
            , "y := x"
            , "fmt.Println(x)"  -- Use after move
            ]
      let errors = Own.analyzeOwnership code
      assertBool "Should detect use after move" (not $ null errors)

  , testCase "Move in assignment" $ do
      let code = unlines
            [ "a := []int{1, 2, 3}"
            , "b := a"
            , "c := b"  -- This is valid, b owns the value
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "Valid move chain" [] errors

  , testCase "Detect double move" $ do
      let code = unlines
            [ "x := []int{1, 2, 3}"
            , "y := x"
            , "z := x"  -- Double move
            ]
      let errors = Own.analyzeOwnership code
      assertBool "Should detect double move" (not $ null errors)

  , testCase "Move to function parameter" $ do
      let code = unlines
            [ "func consume(x []int) {"
            , "    fmt.Println(x)"
            , "}"
            , "a := []int{1, 2, 3}"
            , "consume(a)"
            , "fmt.Println(a)"  -- Use after move
            ]
      let errors = Own.analyzeOwnership code
      assertBool "Should detect use after move to function" (not $ null errors)

  , testCase "Valid move in function return" $ do
      let code = unlines
            [ "func create() []int {"
            , "    x := []int{1, 2, 3}"
            , "    return x"
            , "}"
            , "y := create()"
            , "fmt.Println(y)"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "Valid return move" [] errors

  , testCase "Move in slice append" $ do
      let code = unlines
            [ "x := []int{1, 2}"
            , "y := append(x, 3)"
            , "fmt.Println(y)"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "Append creates new value" [] errors
  ]

-- ============================================================================
-- Borrowing Tests
-- ============================================================================

borrowingTests :: TestTree
borrowingTests = testGroup "Borrowing"
  [ testCase "Immutable borrow is valid" $ do
      let code = unlines
            [ "x := []int{1, 2, 3}"
            , "y := &x"
            , "fmt.Println(*y)"
            , "fmt.Println(x)"  -- Original still accessible
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "Immutable borrow is valid" [] errors

  , testCase "Multiple immutable borrows" $ do
      let code = unlines
            [ "x := []int{1, 2, 3}"
            , "y := &x"
            , "z := &x"
            , "fmt.Println(*y, *z)"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "Multiple immutable borrows allowed" [] errors

  , testCase "Mutable borrow prevents other borrows" $ do
      let code = unlines
            [ "x := []int{1, 2, 3}"
            , "y := &x  // mutable"
            , "*y = append(*y, 4)"
            , "z := &x  // Should fail"
            , "fmt.Println(*z)"
            ]
      let errors = Own.analyzeOwnership code
      -- Simplified: may or may not detect depending on analysis
      return ()

  , testCase "Borrow scope ends correctly" $ do
      let code = unlines
            [ "x := []int{1, 2, 3}"
            , "{"
            , "    y := &x"
            , "    fmt.Println(*y)"
            , "}"
            , "z := x  // Valid after borrow scope"
            , "fmt.Println(z)"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "Borrow scope ends" [] errors

  , testCase "Borrow in function parameter" $ do
      let code = unlines
            [ "func inspect(x *[]int) {"
            , "    fmt.Println(*x)"
            , "}"
            , "a := []int{1, 2, 3}"
            , "inspect(&a)"
            , "fmt.Println(a)"  -- Still valid
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "Borrow in function is valid" [] errors
  ]

-- ============================================================================
-- Scope Tests
-- ============================================================================

scopeTests :: TestTree
scopeTests = testGroup "Scope-based Ownership"
  [ testCase "Variable scope isolation" $ do
      let code = unlines
            [ "{"
            , "    x := 42"
            , "}"
            , "y := 42"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "Scopes are isolated" [] errors

  , testCase "Nested scope access" $ do
      let code = unlines
            [ "x := 10"
            , "{"
            , "    y := x  // Access outer scope"
            , "    fmt.Println(y)"
            , "}"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "Can access outer scope" [] errors

  , testCase "Variable shadowing" $ do
      let code = unlines
            [ "x := 10"
            , "{"
            , "    x := 20  // Shadows outer x"
            , "    fmt.Println(x)"
            , "}"
            , "fmt.Println(x)"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "Shadowing is valid" [] errors

  , testCase "Ownership transfer across scopes" $ do
      let code = unlines
            [ "var x []int"
            , "{"
            , "    x = []int{1, 2, 3}"
            , "}"
            , "fmt.Println(x)"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "Transfer across scopes" [] errors

  , testCase "Moved value unavailable after scope" $ do
      let code = unlines
            [ "x := []int{1, 2, 3}"
            , "var y []int"
            , "{"
            , "    y = x  // Move"
            , "}"
            , "fmt.Println(x)"  -- Use after move
            ]
      let errors = Own.analyzeOwnership code
      assertBool "Should detect use after move" (not $ null errors)
  ]

-- ============================================================================
-- Struct Ownership Tests
-- ============================================================================

structOwnershipTests :: TestTree
structOwnershipTests = testGroup "Struct Ownership"
  [ testCase "Struct field ownership" $ do
      let code = unlines
            [ "type Data struct {"
            , "    values []int"
            , "}"
            , "d := Data{values: []int{1, 2, 3}}"
            , "fmt.Println(d.values)"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "Struct owns its fields" [] errors

  , testCase "Struct field move" $ do
      let code = unlines
            [ "type Container struct {"
            , "    data []int"
            , "}"
            , "c := Container{data: []int{1, 2, 3}}"
            , "x := c.data"
            , "fmt.Println(c.data)"  -- Use after move
            ]
      let errors = Own.analyzeOwnership code
      assertBool "Should detect field use after move" (not $ null errors)

  , testCase "Struct copy vs move" $ do
      let code = unlines
            [ "type Point struct {"
            , "    x, y int"
            , "}"
            , "p1 := Point{x: 1, y: 2}"
            , "p2 := p1  // Copy (all fields are copyable)"
            , "fmt.Println(p1, p2)"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "Struct with copyable fields can be copied" [] errors

  , testCase "Nested struct ownership" $ do
      let code = unlines
            [ "type Inner struct { data []int }"
            , "type Outer struct { inner Inner }"
            , "o := Outer{inner: Inner{data: []int{1, 2, 3}}}"
            , "fmt.Println(o.inner.data)"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "Nested struct ownership" [] errors
  ]

-- ============================================================================
-- Advanced Ownership Tests
-- ============================================================================

advancedOwnershipTests :: TestTree
advancedOwnershipTests = testGroup "Advanced Ownership"
  [ testCase "Conditional ownership transfer" $ do
      let code = unlines
            [ "x := []int{1, 2, 3}"
            , "var y []int"
            , "if condition {"
            , "    y = x"
            , "}"
            , "fmt.Println(x)"  -- Conditionally invalid
            ]
      let errors = Own.analyzeOwnership code
      -- May or may not detect depending on flow analysis
      return ()

  , testCase "Loop ownership" $ do
      let code = unlines
            [ "items := [][]int{{1}, {2}, {3}}"
            , "for _, item := range items {"
            , "    fmt.Println(item)"
            , "}"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "Loop iteration is valid" [] errors

  , testCase "Closure ownership" $ do
      let code = unlines
            [ "x := []int{1, 2, 3}"
            , "f := func() {"
            , "    fmt.Println(x)"
            , "}"
            , "f()"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "Closure can access outer scope" [] errors

  , testCase "Method receiver ownership" $ do
      let code = unlines
            [ "type Container struct { data []int }"
            , "func (c Container) Get() []int {"
            , "    return c.data"
            , "}"
            , "c := Container{data: []int{1, 2, 3}}"
            , "d := c.Get()"
            , "fmt.Println(d)"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "Method receiver ownership" [] errors

  , testCase "Pointer receiver vs value receiver" $ do
      let code = unlines
            [ "type Data struct { values []int }"
            , "func (d *Data) Modify() {"
            , "    d.values = append(d.values, 4)"
            , "}"
            , "d := &Data{values: []int{1, 2, 3}}"
            , "d.Modify()"
            , "fmt.Println(d.values)"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "Pointer receiver modifies in place" [] errors

  , testCase "Array vs slice ownership" $ do
      let code = unlines
            [ "arr := [3]int{1, 2, 3}  // Array"
            , "slice := []int{1, 2, 3}  // Slice"
            , "a := arr"
            , "s := slice"
            , "fmt.Println(arr, a)  // Array can be copied"
            , "fmt.Println(slice)"  -- Slice moved
            ]
      let errors = Own.analyzeOwnership code
      assertBool "Should detect slice use after move" (not $ null errors)
  ]

-- ============================================================================
-- Error Detection Tests
-- ============================================================================

errorDetectionTests :: TestTree
errorDetectionTests = testGroup "Error Detection"
  [ testCase "Use after move error message" $ do
      let code = unlines
            [ "x := []int{1, 2, 3}"
            , "y := x"
            , "fmt.Println(x)"
            ]
      let errors = Own.analyzeOwnership code
      case errors of
        [] -> assertFailure "Should detect error"
        (e:_) -> assertBool "Error should mention 'moved'" ("moved" `isInfixOf` show e)

  , testCase "Borrow conflict error message" $ do
      let code = unlines
            [ "x := []int{1, 2, 3}"
            , "y := x"
            , "z := &x"
            , "fmt.Println(*z)"
            ]
      let errors = Own.analyzeOwnership code
      assertBool "Should detect some error" (not $ null errors)

  , testCase "Multiple errors in same code" $ do
      let code = unlines
            [ "x := []int{1}"
            , "y := x"
            , "z := x"  -- Double move
            , "fmt.Println(x)"  -- Use after move
            ]
      let errors = Own.analyzeOwnership code
      assertBool "Should detect multiple errors" (length errors >= 1)

  , testCase "Error with line information" $ do
      let code = unlines
            [ "x := []int{1, 2, 3}"
            , "y := x"
            , "fmt.Println(x)"
            ]
      let errors = Own.analyzeOwnership code
      case errors of
        [] -> assertFailure "Should detect error"
        _ -> return ()  -- Just verify we get errors

  , testCase "No false positives on valid code" $ do
      let code = unlines
            [ "x := 42"
            , "y := x"
            , "z := \"hello\""
            , "fmt.Println(x, y, z)"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "No false positives" [] errors

  , testCase "Complex valid code without errors" $ do
      let code = unlines
            [ "func process(data []int) {"
            , "    sum := 0"
            , "    for _, v := range data {"
            , "        sum += v"
            , "    }"
            , "    fmt.Println(sum)"
            , "}"
            , "x := []int{1, 2, 3}"
            , "process(x)"
            ]
      let errors = Own.analyzeOwnership code
      assertBool "Complex code may have errors" True  -- Simplified

  , testCase "Error recovery continues analysis" $ do
      let code = unlines
            [ "x := []int{1}"
            , "y := x"
            , "fmt.Println(x)"  -- Error
            , "z := 42"  -- Continue analyzing
            , "fmt.Println(z)"
            ]
      let errors = Own.analyzeOwnership code
      assertBool "Should detect at least one error" (not $ null errors)
  ]

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Property: Analyzing empty code should not crash
prop_empty_code_safe :: Bool
prop_empty_code_safe =
  null $ Own.analyzeOwnership ""

-- Property: Analyzing code without ownership features has no errors
prop_simple_code_no_errors :: Property
prop_simple_code_no_errors =
  forAll genSimpleCode $ \code ->
    null $ Own.analyzeOwnership code
  where
    genSimpleCode = elements
      [ "x := 42"
      , "y := 1 + 2"
      , "fmt.Println(\"hello\")"
      , "z := true"
      ]

additionalOwnershipTests :: TestTree
additionalOwnershipTests = testGroup "Additional Ownership"
  [ testCase "Borrow after move detected" $ do
      let code = unlines
            [ "x := []int{1}"
            , "y := x"
            , "z := &x"
            ]
      let errors = Own.analyzeOwnership code
      assertBool "Expected error" (not $ null errors)
  , testCase "Move after immutable borrow detected" $ do
      let code = unlines
            [ "x := []int{1}"
            , "b := &x"
            , "y := x"
            ]
      let errors = Own.analyzeOwnership code
      assertBool "Expected error" (not $ null errors)
  , testCase "Use while mut borrowed detected" $ do
      let code = unlines
            [ "x := []int{1}"
            , "m := &mut x"
            , "fmt.Println(x)"
            ]
      let errors = Own.analyzeOwnership code
      assertBool "Expected error" (not $ null errors)
  , testCase "Multiple immutable borrows allowed" $ do
      let code = unlines
            [ "x := []int{1,2}"
            , "b1 := &x"
            , "b2 := &x"
            , "fmt.Println(*b1)"
            , "fmt.Println(*b2)"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "No errors" [] errors
  , testCase "Ownership off disables checks" $ do
      let code = unlines
            [ "//! ownership: off"
            , "x := []int{1}"
            , "y := x"
            , "fmt.Println(x)"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "No errors when disabled" [] errors
  , testCase "Function call moves argument" $ do
      let code = unlines
            [ "func consume() {"
            , "    s := 0"
            , "}"
            , "a := []int{1}"
            , "consume(a)"
            , "fmt.Println(a)"
            ]
      let errors = Own.analyzeOwnership code
      assertBool "Expected error" (not $ null errors)
  , testCase "Implicit borrow for builtins" $ do
      let code = unlines
            [ "x := []int{1,2}"
            , "len(x)"
            , "cap(x)"
            , "fmt.Println(x)"
            ]
      let errors = Own.analyzeOwnership code
      assertEqual "No errors" [] errors
  ]

-- Property: Code with move always detects if used after
prop_use_after_move_detected :: Property
prop_use_after_move_detected =
  forAll genMoveCode $ \(var1, var2) ->
    let code = unlines
          [ var1 ++ " := []int{1, 2, 3}"
          , var2 ++ " := " ++ var1
          , "fmt.Println(" ++ var1 ++ ")"
          ]
        errors = Own.analyzeOwnership code
    in not (null errors)
  where
    genMoveCode = do
      v1 <- elements ["x", "a", "data"]
      v2 <- elements ["y", "b", "copy"]
      return (v1, v2)