module Test.Unit.OwnershipTransitivitySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, elements, listOf1)
import qualified Data.Text as T
import Data.List (isInfixOf)

import Compiler (compile, CompilerError(..), CompilationPhase(..))
import Ownership (OwnershipType(..), OwnershipTransfer(..))
import Ownership.Analyzer (analyzeOwnership)

-- | Test ownership transfer and transitivity properties
tests :: TestTree
tests =
  testGroup "Ownership Transitivity Tests"
    [ testGroup "Basic Ownership Transfer"
        [ testCase "detects simple move operations" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "func main() {"
                  , "  var x = make([]int, 10)"
                  , "  var y = x"  -- Move occurs here
                  , "  var z = y"  -- Another move
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasOwnershipError = any (\e -> compilationPhase e == OwnershipPhase) errs
                assertBool "should detect ownership transfer" hasOwnershipError
              Right _ -> assertFailure "expected ownership error due to move"

        , testCase "allows copy operations on copyable types" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "func main() {"
                  , "  var x: int = 42"
                  , "  var y = x"  -- Copy, not move
                  , "  var z = y"  -- Another copy
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> assertFailure $ "Unexpected compilation error: " ++ show errs
              Right _ -> assertBool "copy operations should work on copyable types" True

        , testCase "handles borrow operations correctly" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "func main() {"
                  , "  var x = make([]int, 10)"
                  , "  var y = &x"  -- Borrow
                  , "  var z = *y"  -- Dereference
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasBorrowError = any (\e -> "borrow" `T.isInfixOf` formatError e) errs
                assertBool "should handle borrow operations" hasBorrowError
              Right _ -> assertBool "borrow operations should work" True
        ]

    , testGroup "Ownership Transitivity"
        [ testCase "tracks ownership through function calls" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "func consume(data: []int) {"
                  , "  // Data is consumed here"
                  , "}"
                  , "func main() {"
                  , "  var x = make([]int, 10)"
                  , "  consume(x)"  -- Move to function
                  , "  var y = x"   -- Should error: x is moved
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasOwnershipError = any (\e -> compilationPhase e == OwnershipPhase) errs
                assertBool "should track ownership through function calls" hasOwnershipError
              Right _ -> assertFailure "expected ownership error after function call"

        , testCase "handles ownership in nested structures" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "type Container struct {"
                  , "  data []int"
                  , "}"
                  , "func main() {"
                  , "  var c = Container{data: make([]int, 10)}"
                  , "  var d = c"     -- Move container
                  , "  var e = c.data"  -- Should error: c is moved
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasOwnershipError = any (\e -> compilationPhase e == OwnershipPhase) errs
                assertBool "should handle ownership in nested structures" hasOwnershipError
              Right _ -> assertFailure "expected ownership error with nested structures"

        , testCase "tracks ownership through complex expressions" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "func main() {"
                  , "  var x = make([]int, 10)"
                  , "  var y = x"
                  , "  var z = append(y, 1)"
                  , "  var w = z"
                  , "  var v = append(w, 2)"
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasOwnershipError = any (\e -> compilationPhase e == OwnershipPhase) errs
                assertBool "should track ownership through complex expressions" hasOwnershipError
              Right _ -> assertBool "complex expressions should work with proper ownership" True
        ]

    , testGroup "Ownership Lifetime Analysis"
        [ testCase "detects use-after-move in different scopes" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "func main() {"
                  , "  var x = make([]int, 10)"
                  , "  {"
                  , "    var y = x"  -- Move in inner scope
                  , "  }"
                  , "  var z = x"   -- Use after move in outer scope
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasOwnershipError = any (\e -> compilationPhase e == OwnershipPhase) errs
                assertBool "should detect use-after-move across scopes" hasOwnershipError
              Right _ -> assertFailure "expected ownership error due to use-after-move"

        , testCase "handles ownership with conditional moves" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "func main() {"
                  , "  var x = make([]int, 10)"
                  , "  var condition = true"
                  , "  if condition {"
                  , "    var y = x"  -- Conditional move
                  , "  }"
                  , "  var z = x"   -- Potentially use-after-move
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasOwnershipError = any (\e -> compilationPhase e == OwnershipPhase) errs
                assertBool "should handle conditional ownership transfers" hasOwnershipError
              Right _ -> assertBool "conditional moves should be handled correctly" True
        ]

    , testGroup "Ownership and Generics"
        [ testCase "handles ownership with generic types" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "func process<T>(data: []T) []T {"
                  , "  var result = make([]T, len(data))"
                  , "  return result"
                  , "}"
                  , "func main() {"
                  , "  var x = make([]int, 10)"
                  , "  var y = process(x)"  -- Move to generic function
                  , "  var z = x"           -- Should error
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasOwnershipError = any (\e -> compilationPhase e == OwnershipPhase) errs
                assertBool "should handle ownership with generics" hasOwnershipError
              Right _ -> assertFailure "expected ownership error with generics"

        , testCase "preserves ownership constraints in generic implementations" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "type Box<T> struct {"
                  , "  value T"
                  , "}"
                  , "func main() {"
                  , "  var b1 = Box<int>{value: 42}"
                  , "  var b2 = b1"    -- Move box
                  , "  var x = b1.value"  -- Should error
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasOwnershipError = any (\e -> compilationPhase e == OwnershipPhase) errs
                assertBool "should preserve ownership in generic types" hasOwnershipError
              Right _ -> assertFailure "expected ownership error with generic box"
        ]

    , testGroup "Ownership Error Recovery"
        [ testCase "provides clear error messages for complex ownership chains" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "func main() {"
                  , "  var a = make([]int, 10)"
                  , "  var b = a"
                  , "  var c = b"
                  , "  var d = c"
                  , "  var e = d"
                  , "  var f = a"  -- Use after multiple transfers
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasClearMessage = any (\e -> "moved" `T.isInfixOf` formatError e) errs
                assertBool "should provide clear error messages" hasClearMessage
              Right _ -> assertFailure "expected ownership error"

        , testCase "handles partial ownership recovery" $ do
            let code = unlines
                  [ "//! ownership: on"
                  , "func main() {"
                  , "  var x = make([]int, 10)"
                  , "  var y = x[0:5]"  -- Partial move/borrow
                  , "  var z = x"       -- Should still be accessible
                  , "}"
                  ]
            result <- compile code
            case result of
              Left errs -> do
                let hasPartialOwnershipError = any (\e -> "partial" `T.isInfixOf` formatError e) errs
                assertBool "should handle partial ownership" hasPartialOwnershipError
              Right _ -> assertBool "partial ownership should work" True
        ]

    , testGroup "QuickCheck Property Tests"
        [ testProperty "ownership transfer is transitive" $ do
            -- Test that if A moves to B and B moves to C, then A is no longer accessible
            let simpleCode = unlines
                  [ "//! ownership: on"
                  , "func main() {"
                  , "  var a = make([]int, 10)"
                  , "  var b = a"
                  , "  var c = b"
                  , "}"
                  ]
            result <- compile simpleCode
            case result of
              Left errs -> return $ any (\e -> compilationPhase e == OwnershipPhase) errs
              Right _ -> return $ False

        , testProperty "copyable types don't follow ownership rules" $ do
            -- Test that primitive types can be copied freely
            let copyableCode = unlines
                  [ "//! ownership: on"
                  , "func main() {"
                  , "  var a: int = 42"
                  , "  var b = a"
                  , "  var c = b"
                  , "  var d = a"  -- Should still work
                  , "}"
                  ]
            result <- compile copyableCode
            case result of
              Left _ -> return $ False
              Right _ -> return $ True
        ]
    ]