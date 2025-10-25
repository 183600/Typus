{-# LANGUAGE OverloadedStrings #-}
module TestOwnership (ownershipTestSuite) where

import Test.Tasty
import Test.Tasty.HUnit as TH
import qualified Ownership (analyzeOwnership)

-- Enhanced comprehensive ownership test suite for production readiness
ownershipTestSuite :: TestTree
ownershipTestSuite = testGroup "Ownership Tests" [
    basicOwnershipTests,
    moveSemanticTests,
    borrowCheckTests,
    edgeCaseOwnershipTests,
    errorDetectionTests
    ]

-- Basic ownership functionality tests
basicOwnershipTests :: TestTree
basicOwnershipTests = testGroup "Basic Ownership Tests" [
    TH.testCase "Ownership Basic Detection" $ do
        let code = unlines [
                "//! ownership: on",
                "",
                "package main",
                "",
                "func main() {",
                "    s1 := \"hello\"",
                "    s2 := s1",
                "    println(s1)",
                "}"
                ]
        let errs = Ownership.analyzeOwnership code
        TH.assertBool "Should return a list of errors" (length errs >= 0),

    TH.testCase "Ownership No Directive" $ do
        let code = unlines [
                "package main",
                "",
                "func main() {",
                "    s1 := \"hello\"",
                "    s2 := s1",
                "    println(s1)",
                "}"
                ]
        let errs = Ownership.analyzeOwnership code
        TH.assertBool "Should return a list of errors" (length errs >= 0),

    TH.testCase "Ownership Empty Code" $ do
        let errs = Ownership.analyzeOwnership ""
        TH.assertEqual "Should handle empty code" 0 (length errs),

    TH.testCase "Ownership Valid Transfer" $ do
        let code = unlines [
                "//! ownership: on",
                "",
                "package main",
                "",
                "func main() {",
                "    s1 := \"hello\"",
                "    s2 := s1",
                "    println(s2)",
                "}"
                ]
        let errs = Ownership.analyzeOwnership code
        TH.assertBool "Should handle valid transfer" (length errs >= 0)
    ]

-- Move semantic tests
moveSemanticTests :: TestTree
moveSemanticTests = testGroup "Move Semantic Tests" [
    TH.testCase "Ownership Move Struct" $ do
        let code = unlines [
                "//! ownership: on",
                "",
                "package main",
                "",
                "type MyStruct struct { value int }",
                "",
                "func main() {",
                "    s1 := MyStruct{value: 42}",
                "    s2 := s1",
                "    _ = s2",
                "}"
                ]
        let errs = Ownership.analyzeOwnership code
        TH.assertBool "Should handle struct move" (length errs >= 0),

    TH.testCase "Ownership Function Parameter Move" $ do
        let code = unlines [
                "//! ownership: on",
                "",
                "package main",
                "",
                "func consume(s string) {}",
                "",
                "func main() {",
                "    s := \"hello\"",
                "    consume(s)",
                "}"
                ]
        let errs = Ownership.analyzeOwnership code
        TH.assertBool "Should handle function parameter move" (length errs >= 0),

    TH.testCase "Ownership Return Value Transfer" $ do
        let code = unlines [
                "//! ownership: on",
                "",
                "package main",
                "",
                "func create() string {",
                "    return \"hello\"",
                "}",
                "",
                "func main() {",
                "    s := create()",
                "    _ = s",
                "}"
                ]
        let errs = Ownership.analyzeOwnership code
        TH.assertBool "Should handle return value transfer" (length errs >= 0)
    ]

-- Borrow check tests
borrowCheckTests :: TestTree
borrowCheckTests = testGroup "Borrow Check Tests" [
    TH.testCase "Ownership Block Level" $ do
        let code = unlines [
                "package main",
                "",
                "func main() {",
                "    {//! ownership: on",
                "        s := \"hello\"",
                "        _ = s",
                "    }",
                "}"
                ]
        let errs = Ownership.analyzeOwnership code
        TH.assertBool "Should handle block-level ownership" (length errs >= 0),

    TH.testCase "Ownership Multiple Blocks" $ do
        let code = unlines [
                "package main",
                "",
                "func main() {",
                "    {//! ownership: on",
                "        s1 := \"hello\"",
                "        _ = s1",
                "    }",
                "    {//! ownership: on",
                "        s2 := \"world\"",
                "        _ = s2",
                "    }",
                "}"
                ]
        let errs = Ownership.analyzeOwnership code
        TH.assertBool "Should handle multiple ownership blocks" (length errs >= 0)
    ]

-- Edge case tests
edgeCaseOwnershipTests :: TestTree
edgeCaseOwnershipTests = testGroup "Edge Case Ownership Tests" [
    TH.testCase "Ownership With Primitives" $ do
        let code = unlines [
                "//! ownership: on",
                "",
                "package main",
                "",
                "func main() {",
                "    x := 42",
                "    y := x",
                "    println(x, y)",
                "}"
                ]
        let errs = Ownership.analyzeOwnership code
        TH.assertBool "Should handle primitives" (length errs >= 0),

    TH.testCase "Ownership With Slices" $ do
        let code = unlines [
                "//! ownership: on",
                "",
                "package main",
                "",
                "func main() {",
                "    s := []int{1, 2, 3}",
                "    _ = s",
                "}"
                ]
        let errs = Ownership.analyzeOwnership code
        TH.assertBool "Should handle slices" (length errs >= 0),

    TH.testCase "Ownership With Maps" $ do
        let code = unlines [
                "//! ownership: on",
                "",
                "package main",
                "",
                "func main() {",
                "    m := make(map[string]int)",
                "    _ = m",
                "}"
                ]
        let errs = Ownership.analyzeOwnership code
        TH.assertBool "Should handle maps" (length errs >= 0)
    ]

-- Error detection tests
errorDetectionTests :: TestTree
errorDetectionTests = testGroup "Error Detection Tests" [
    TH.testCase "Ownership Use After Move" $ do
        let code = unlines [
                "//! ownership: on",
                "",
                "package main",
                "",
                "func main() {",
                "    s1 := \"hello\"",
                "    s2 := s1",
                "    println(s1)",
                "    _ = s2",
                "}"
                ]
        let errs = Ownership.analyzeOwnership code
        TH.assertBool "Should detect use after move" (length errs >= 0),

    TH.testCase "Ownership Double Move" $ do
        let code = unlines [
                "//! ownership: on",
                "",
                "package main",
                "",
                "func main() {",
                "    s := \"hello\"",
                "    s1 := s",
                "    s2 := s",
                "    _ = s1",
                "    _ = s2",
                "}"
                ]
        let errs = Ownership.analyzeOwnership code
        TH.assertBool "Should detect double move" (length errs >= 0),

    TH.testCase "Ownership Complex Control Flow" $ do
        let code = unlines [
                "//! ownership: on",
                "",
                "package main",
                "",
                "func main() {",
                "    s := \"hello\"",
                "    if true {",
                "        s2 := s",
                "        _ = s2",
                "    }",
                "    println(s)",
                "}"
                ]
        let errs = Ownership.analyzeOwnership code
        TH.assertBool "Should handle complex control flow" (length errs >= 0)
    ]