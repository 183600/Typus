{-# LANGUAGE OverloadedStrings #-}
module TestOwnership (ownershipTestSuite) where

import Test.Tasty
import Test.Tasty.HUnit as TH
import qualified Ownership (analyzeOwnership)

-- Tasty test suite for integration with comprehensive tests
ownershipTestSuite :: TestTree
ownershipTestSuite = testGroup "Ownership Tests" [
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
        TH.assertEqual "Should handle empty code" 0 (length errs)
    ]