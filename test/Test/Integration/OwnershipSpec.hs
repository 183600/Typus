module Test.Integration.OwnershipSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Ownership (OwnershipError(..), analyzeOwnership)

-- | Integration-oriented ownership scenarios that exercise the analyser on
-- more complete program snippets. These tests are only enabled in the
-- comprehensive Cabal configurations to keep the default test run fast.
tests :: TestTree
tests =
  testGroup "Ownership integration"
    [ testCase "reports double move across chained bindings" $ do
        let source = unlines
              [ "//! ownership: on"
              , ""
              , "package main"
              , ""
              , "func acquire() string {"
              , "    return \"payload\""
              , "}"
              , ""
              , "func main() {"
              , "    resource := acquire()"
              , "    alias := resource"
              , "    duplicate := resource"
              , "    println(alias, duplicate)"
              , "}"
              ]
        analyzeOwnership source @?= [DoubleMove "resource" "resource"]

    , testCase "borrow after move via call surfaces borrow-while-moved" $ do
        let source = unlines
              [ "//! ownership: on"
              , ""
              , "package main"
              , ""
              , "func consume(ptr &String) {"
              , "}"
              , ""
              , "func create() string {"
              , "    return \"value\""
              , "}"
              , ""
              , "func main() {"
              , "    handle := create()"
              , "    sink := handle"
              , "    consume(&handle)"
              , "}"
              ]
        analyzeOwnership source @?= [BorrowWhileMoved "handle"]

    , testCase "usage after scope exit is flagged as out-of-scope" $ do
        let source = unlines
              [ "//! ownership: on"
              , ""
              , "package main"
              , ""
              , "func build() string {"
              , "    return \"value\""
              , "}"
              , ""
              , "func main() {"
              , "    {"
              , "        temp := build()"
              , "        println(temp)"
              , "    }"
              , "    println(temp)"
              , "}"
              ]
        analyzeOwnership source @?= [OutOfScope "temp"]
    ]
