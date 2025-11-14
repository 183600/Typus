module Test.Unit.OwnershipSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ( (@?=), assertBool, testCase )

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

    , testCase "detects borrow while mutable borrow is active" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    data := \"payload\""
              , "    mutRef := &mut data"
              , "    consume(&data)"
              , "}"
              ]
        analyzeOwnership source @?= [BorrowWhileMutBorrowed "data"]

    , testCase "detects mutable borrow while immutable borrows exist" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    data := \"payload\""
              , "    ref1 := &data"
              , "    ref2 := &mut data"
              , "}"
              ]
        analyzeOwnership source @?= [MutBorrowWhileBorrowed "data"]

    , testCase "detects borrow after a move operation" $ do
        let source = unlines
              [ "package main"
              , "func take_value(input string) string {"
              , "    return input"
              , "}"
              , "func main() {"
              , "    data := \"payload\""
              , "    take_value(data)"
              , "    ref := &data"
              , "}"
              ]
        analyzeOwnership source @?= [BorrowWhileMoved "data"]

    , testCase "detects use of value while it has a mutable borrow" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    data := \"payload\""
              , "    mutRef := &mut data"
              , "    println(data)"
              , "}"
              ]
        analyzeOwnership source @?= [UseWhileMutBorrowed "data"]

    , testCase "detects multiple mutable borrows" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    data := \"payload\""
              , "    mutRef1 := &mut data"
              , "    mutRef2 := &mut data"
              , "    println(mutRef1)"
              , "}"
              ]
        analyzeOwnership source @?= [MultipleMutBorrows "data"]
    ]
