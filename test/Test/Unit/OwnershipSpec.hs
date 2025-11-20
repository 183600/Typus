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

    , testCase "detects double move attempts" $ do
        let source = unlines
              [ "package main"
              , ""
              , "func consume(x string) string {"
              , "    return x"
              , "}"
              , ""
              , "func main() {"
              , "    data := \"payload\""
              , "    consume(data)"
              , "    consume(data)"
              , "}"
              ]
        analyzeOwnership source @?= [DoubleMove "data" "data"]

    , testCase "reports out-of-scope variable usage" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    println(value)"
              , "}"
              ]
        analyzeOwnership source @?= [OutOfScope "value"]

    , testCase "allows reborrowing after scoped mutable borrow ends" $ do
        let source = unlines
              [ "package main"
              , ""
              , "func main() {"
              , "    data := \"payload\""
              , "    {"
              , "        mutRef := &mut data"
              , "        println(mutRef)"
              , "    }"
              , "    println(data)"
              , "    mutRef2 := &mut data"
              , "    println(mutRef2)"
              , "}"
              ]
        analyzeOwnership source @?= []

    , testCase "fmt.Println is treated as pure usage" $ do
        let source = unlines
              [ "package main"
              , ""
              , "func produce() string {"
              , "    return \"payload\""
              , "}"
              , ""
              , "func main() {"
              , "    data := produce()"
              , "    fmt.Println(data)"
              , "    println(data)"
              , "}"
              ]
        analyzeOwnership source @?= []

    , testCase "mutex locking and unlocking are treated as safe usages" $ do
        let source = unlines
              [ "package main"
              , ""
              , "type Mutex struct {}"
              , ""
              , "func (m *Mutex) Lock() {}"
              , "func (m *Mutex) Unlock() {}"
              , ""
              , "func newMutex() Mutex {"
              , "    return Mutex{}"
              , "}"
              , ""
              , "func main() {"
              , "    mu := newMutex()"
              , "    mu.Lock()"
              , "    println(\"work\")"
              , "    mu.Unlock()"
              , "    println(\"done\")"
              , "}"
              ]
        analyzeOwnership source @?= []

    , testCase "respects ownership off directive" $ do
        let source = unlines
              [ "//! ownership: off"
              , ""
              , "package main"
              , ""
              , "func produce() string {"
              , "    return \"payload\""
              , "}"
              , ""
              , "func consume(x string) string {"
              , "    return x"
              , "}"
              , ""
              , "func use(_ string) {}"
              , ""
              , "func main() {"
              , "    data := produce()"
              , "    consume(data)"
              , "    use(data)"
              , "}"
              ]
        analyzeOwnership source @?= []

    , testCase "line directive toggles ownership tracking back on" $ do
        let source = unlines
              [ "package main"
              , ""
              , "func produce() string {"
              , "    return \"payload\""
              , "}"
              , ""
              , "func consume(x string) string {"
              , "    return x"
              , "}"
              , ""
              , "func use(_ string) {}"
              , ""
              , "func main() {"
              , "    //! ownership: off"
              , "    ignored := produce()"
              , "    consume(ignored)"
              , "    use(ignored)"
              , ""
              , "    //! ownership: on"
              , "    tracked := produce()"
              , "    consume(tracked)"
              , "    println(tracked)"
              , "}"
              ]
        analyzeOwnership source @?= [UseAfterMove "tracked"]
    ]
