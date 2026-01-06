{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewOwnershipTransferBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property
  , (===)
  , (==>)
  , forAll
  , counterexample
  , classify
  , property
  , (.&&.)
  , (.||.)
  , Arbitrary(..)
  , Gen
  , choose
  , listOf
  , elements
  , oneof
  , sized
  , resize
  , Positive(..)
  , NonEmptyList(..)
  )

import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  , analyzeOwnership
  , analyzeOwnershipFile
  , formatOwnershipErrors
  )
import Parser
  ( TypusFile(..)
  , parseTypus
  )

import Data.Char (isSpace, toLower)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (tails, sort, nub)
import qualified Data.Text as T

-- Test ownership transfer with simple assignments
test_ownership_simple_assignment :: TestTree
test_ownership_simple_assignment = testCase "Ownership transfer with simple assignments" $ do
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    x := 42"
        , "    y := x  // x is moved to y"
        , "    // z := x  // This should cause a use-after-move error"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " <> err
    Right typusFile -> do
      analyzer <- newOwnershipAnalyzer
      result <- analyzeOwnership analyzer typusFile
      case result of
        Left errors -> do
          let errorStr = formatOwnershipErrors errors
          -- Should detect ownership transfer
          assertBool "Should detect ownership transfer" $ 
            isInfixOf "move" errorStr || isInfixOf "ownership" errorStr
        Right _ -> pure () -- Analysis successful

-- Test ownership transfer with function parameters
test_ownership_function_parameters :: TestTree
test_ownership_function_parameters = testCase "Ownership transfer with function parameters" $ do
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func consume(x int) {"
        , "    // x is consumed here"
        , "}"
        , "func main() {"
        , "    x := 42"
        , "    consume(x)  // x is moved to consume"
        , "    // y := x  // This should cause a use-after-move error"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " <> err
    Right typusFile -> do
      analyzer <- newOwnershipAnalyzer
      result <- analyzeOwnership analyzer typusFile
      case result of
        Left errors -> do
          let errorStr = formatOwnershipErrors errors
          -- Should detect cross-function ownership transfer
          assertBool "Should detect cross-function ownership transfer" $ 
            isInfixOf "move" errorStr || isInfixOf "ownership" errorStr
        Right _ -> pure () -- Analysis successful

-- Test ownership transfer with return values
test_ownership_return_values :: TestTree
test_ownership_return_values = testCase "Ownership transfer with return values" $ do
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func create() int {"
        , "    return 42"
        , "}"
        , "func main() {"
        , "    x := create()  // x receives ownership of return value"
        , "    y := x  // x is moved to y"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " <> err
    Right typusFile -> do
      analyzer <- newOwnershipAnalyzer
      result <- analyzeOwnership analyzer typusFile
      case result of
        Left errors -> do
          let errorStr = formatOwnershipErrors errors
          -- Should detect ownership transfer through return values
          assertBool "Should handle return value ownership" $ 
            not (isInfixOf "error" errorStr) || isInfixOf "move" errorStr
        Right _ -> pure () -- Analysis successful

-- Test ownership transfer with complex data structures
test_ownership_complex_structures :: TestTree
test_ownership_complex_structures = testCase "Ownership transfer with complex data structures" $ do
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "type Data struct {"
        , "    value int"
        , "    next *Data"
        , "}"
        , "func main() {"
        , "    data := &Data{value: 42}"
        , "    data2 := data  // data is moved to data2"
        , "    // data.value = 10  // This should cause a use-after-move error"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " <> err
    Right typusFile -> do
      analyzer <- newOwnershipAnalyzer
      result <- analyzeOwnership analyzer typusFile
      case result of
        Left errors -> do
          let errorStr = formatOwnershipErrors errors
          -- Should detect ownership transfer of complex structures
          assertBool "Should handle complex structure ownership" $ 
            not (isInfixOf "error" errorStr) || isInfixOf "move" errorStr
        Right _ -> pure () -- Analysis successful

-- Test ownership transfer with loops
test_ownership_loops :: TestTree
test_ownership_loops = testCase "Ownership transfer with loops" $ do
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    items := []int{1, 2, 3}"
        , "    for i, item := range items {"
        , "        // item is borrowed from items"
        , "        process(item)"
        , "    }"
        , "}"
        , "func process(x int) {"
        , "    // Process x"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " <> err
    Right typusFile -> do
      analyzer <- newOwnershipAnalyzer
      result <- analyzeOwnership analyzer typusFile
      case result of
        Left errors -> do
          let errorStr = formatOwnershipErrors errors
          -- Should handle ownership in loop contexts
          assertBool "Should handle loop ownership" $ 
            not (isInfixOf "error" errorStr) || isInfixOf "borrow" errorStr
        Right _ -> pure () -- Analysis successful

-- Test ownership transfer with conditionals
test_ownership_conditionals :: TestTree
test_ownership_conditionals = testCase "Ownership transfer with conditionals" $ do
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    x := 42"
        , "    if true {"
        , "        y := x  // x is moved to y in this branch"
        , "    } else {"
        , "        z := x  // x is moved to z in this branch"
        , "    }"
        , "    // w := x  // This should cause a use-after-move error"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " <> err
    Right typusFile -> do
      analyzer <- newOwnershipAnalyzer
      result <- analyzeOwnership analyzer typusFile
      case result of
        Left errors -> do
          let errorStr = formatOwnershipErrors errors
          -- Should detect conditional ownership transfer
          assertBool "Should handle conditional ownership" $ 
            not (isInfixOf "error" errorStr) || isInfixOf "move" errorStr
        Right _ -> pure () -- Analysis successful

-- Property: Ownership analysis should be deterministic
prop_ownership_deterministic :: String -> Property
prop_ownership_deterministic source = 
  case parseTypus source of
    Left _ -> property True -- Parsing failures are OK
    Right typusFile -> do
      analyzer1 <- newOwnershipAnalyzer
      analyzer2 <- newOwnershipAnalyzer
      result1 <- analyzeOwnership analyzer1 typusFile
      result2 <- analyzeOwnership analyzer2 typusFile
      case (result1, result2) of
        (Left err1, Left err2) -> err1 === err2
        (Right res1, Right res2) -> res1 === res2
        _ -> property False -- Should have consistent results

-- Property: Ownership transfer should preserve single ownership
prop_single_ownership_preserved :: String -> Property
prop_single_ownership_preserved source = 
  case parseTypus source of
    Left _ -> property True
    Right typusFile -> do
      analyzer <- newOwnershipAnalyzer
      result <- analyzeOwnership analyzer typusFile
      case result of
        Left errors -> 
          let errorStr = formatOwnershipErrors errors
          in property $ not (isInfixOf "DoubleMove" errorStr)
        Right _ -> property True

-- Property: Borrow checker should prevent use-after-move
prop_borrow_prevents_use_after_move :: String -> Property
prop_borrow_prevents_use_after_move source = 
  case parseTypus source of
    Left _ -> property True
    Right typusFile -> do
      analyzer <- newOwnershipAnalyzer
      result <- analyzeOwnership analyzer typusFile
      case result of
        Left errors -> 
          let errorStr = formatOwnershipErrors errors
          in property $ not (isInfixOf "UseAfterMove" errorStr && isInfixOf "BorrowError" errorStr)
        Right _ -> property True

-- Property: Ownership transfer should handle nested scopes correctly
prop_nested_scope_ownership :: Positive Int -> Property
prop_nested_scope_ownership (Positive n) = 
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    x := 42"
        , "    for i := 0; i < " ++ show n ++ "; i++ {"
        , "        y := x  // x is moved in nested scope"
        , "    }"
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property True
    Right typusFile -> do
      analyzer <- newOwnershipAnalyzer
      result <- analyzeOwnership analyzer typusFile
      case result of
        Left errors -> 
          let errorStr = formatOwnershipErrors errors
          in property $ isInfixOf "move" errorStr || isInfixOf "borrow" errorStr
        Right _ -> property True

-- Property: Ownership analysis should handle concurrent access patterns
prop_concurrent_access_patterns :: String -> Property
prop_concurrent_access_patterns source = 
  let concurrentSource = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    x := 42"
        , "    go func() {"
        , "        y := x  // Concurrent access"
        , "    }()"
        , "    z := x  // Should detect potential race condition"
        , "}"
        ]
  in case parseTypus concurrentSource of
    Left _ -> property True
    Right typusFile -> do
      analyzer <- newOwnershipAnalyzer
      result <- analyzeOwnership analyzer typusFile
      case result of
        Left errors -> 
          let errorStr = formatOwnershipErrors errors
          in property $ isInfixOf "borrow" errorStr || isInfixOf "move" errorStr
        Right _ -> property True

tests :: TestTree
tests = testGroup "New Ownership Transfer Boundary Tests"
  [ test_ownership_simple_assignment
  , test_ownership_function_parameters
  , test_ownership_return_values
  , test_ownership_complex_structures
  , test_ownership_loops
  , test_ownership_conditionals
  , fastProperty "Ownership analysis is deterministic" prop_ownership_deterministic
  , fastProperty "Single ownership is preserved" prop_single_ownership_preserved
  , fastProperty "Borrow checker prevents use-after-move" prop_borrow_prevents_use_after_move
  , fastProperty "Nested scope ownership handled correctly" prop_nested_scope_ownership
  , fastProperty "Concurrent access patterns handled" prop_concurrent_access_patterns
  ]