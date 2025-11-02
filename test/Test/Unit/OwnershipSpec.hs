module Test.Unit.OwnershipSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ( (@?=), assertBool, assertFailure, testCase )

import Ownership (OwnershipError(..), analyzeOwnership)

tests :: TestTree
tests =
  testGroup "Ownership analysis"
    [ testCase "does not report errors for simple value usage" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    value := \"hello\""
              , "    println(value)"
              , "}"
              ]
        analyzeOwnership source @?= []

    , testCase "detects use-after-move patterns" $ do
        let source = unlines
              [ "package main"
              , "func read() string {"
              , "    return \"payload\""
              , "}"
              , "func take_value(data string) string {"
              , "    return data"
              , "}"
              , "func main() {"
              , "    data := read()"
              , "    take_value(data)"
              , "    println(data)"
              , "}"
              ]
            errors = analyzeOwnership source
        assertBool ("expected at least one ownership error, got: " <> show errors) (not (null errors))
        let hasUseAfterMove = any (\e -> case e of UseAfterMove v -> v == "data"; _ -> False) errors
        assertBool ("expected UseAfterMove error for 'data', got: " <> show errors) hasUseAfterMove
    ]
