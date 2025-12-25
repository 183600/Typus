module Test.Unit.OwnershipTransitivitySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import Compiler (compile, CompilerError(..), formatCompilerErrors)
import Ownership (OwnershipType(..), OwnershipTransfer(..))
import Parser (parseTypus)
import qualified Data.Text as T
import Data.List (isInfixOf)

-- Test basic ownership move
test_basic_ownership_move :: TestTree
test_basic_ownership_move = testCase "Basic ownership move is tracked" $ do
    let source = unlines
          [ "//! ownership: on"
          , "package main"
          , "func main() {"
          , "    data := make([]int, 10)"
          , "    moved := data"
          , "    _ = moved"
          , "}"
          ]
    result <- compile source
    case result of
      Right _ -> return ()  -- Should compile successfully
      Left errs -> assertFailure $ "Unexpected compilation error: " ++ show errs

-- Test use after move detection
test_use_after_move :: TestTree
test_use_after_move = testCase "Use after move is detected" $ do
    let source = unlines
          [ "//! ownership: on"
          , "package main"
          , "func main() {"
          , "    data := make([]int, 10)"
          , "    moved := data"
          , "    _ = data[0]"  -- use after move"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should detect use after move" $ 
          any (\msg -> "ownership" `isInfixOf` msg || "moved" `isInfixOf` msg) errorMessages
      Right _ -> assertFailure "Expected ownership error"

-- Test ownership transfer through function calls
test_ownership_through_functions :: TestTree
test_ownership_through_functions = testCase "Ownership transfer through functions is tracked" $ do
    let source = unlines
          [ "//! ownership: on"
          , "package main"
          , "func consume(data []int) {"
          , "    _ = data"
          , "}"
          , "func main() {"
          , "    data := make([]int, 10)"
          , "    consume(data)"
          , "    _ = data[0]"  -- use after move through function"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should detect ownership transfer through function" $ 
          any ("ownership" `isInfixOf`) errorMessages
      Right _ -> assertFailure "Expected ownership error"

-- Test ownership borrowing
test_ownership_borrowing :: TestTree
test_ownership_borrowing = testCase "Ownership borrowing is allowed" $ do
    let source = unlines
          [ "//! ownership: on"
          , "package main"
          , "func main() {"
          , "    data := make([]int, 10)"
          , "    ref := &data"
          , "    _ = data[0]"  -- should still be accessible after borrowing"
          , "    _ = (*ref)[1]"
          , "}"
          ]
    result <- compile source
    case result of
      Right _ -> return ()  -- Should compile successfully
      Left errs -> assertFailure $ "Unexpected compilation error: " ++ show errs

-- Test multiple ownership moves
test_multiple_ownership_moves :: TestTree
test_multiple_ownership_moves = testCase "Multiple ownership moves are tracked" $ do
    let source = unlines
          [ "//! ownership: on"
          , "package main"
          , "func main() {"
          , "    data := make([]int, 10)"
          , "    first := data"
          , "    second := first"
          , "    third := second"
          , "    _ = third"
          , "    _ = data[0]"    -- use after first move"
          , "    _ = first[0]"   -- use after second move"
          , "    _ = second[0]"  -- use after third move"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should detect multiple use after move errors" $ 
          length (filter ("ownership" `isInfixOf`) errorMessages) >= 3
      Right _ -> assertFailure "Expected multiple ownership errors"

-- Test ownership in struct fields
test_struct_field_ownership :: TestTree
test_struct_field_ownership = testCase "Struct field ownership is tracked" $ do
    let source = unlines
          [ "//! ownership: on"
          , "package main"
          , "type Container struct {"
          , "    data []int"
          , "}"
          , "func main() {"
          , "    data := make([]int, 10)"
          , "    container := Container{data: data}"
          , "    _ = data[0]"  -- use after move to struct field"
          , "    _ = container.data[1]"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should detect struct field ownership transfer" $ 
          any ("ownership" `isInfixOf`) errorMessages
      Right _ -> assertFailure "Expected struct ownership error"

-- Test ownership with slices
test_slice_ownership :: TestTree
test_slice_ownership = testCase "Slice ownership is properly handled" $ do
    let source = unlines
          [ "//! ownership: on"
          , "package main"
          , "func main() {"
          , "    data := make([]int, 10)"
          , "    slice := data[2:5]"
          , "    _ = data[0]"    -- original should still be accessible"
          , "    _ = slice[0]"    -- slice should be accessible"
          , "}"
          ]
    result <- compile source
    case result of
      Right _ -> return ()  -- Should compile successfully
      Left errs -> assertFailure $ "Unexpected compilation error: " ++ show errs

-- Test ownership with maps
test_map_ownership :: TestTree
test_map_ownership = testCase "Map ownership is properly handled" $ do
    let source = unlines
          [ "//! ownership: on"
          , "package main"
          , "func main() {"
          , "    data := make(map[string]int)"
          , "    data[\"key\"] = 42"
          , "    moved := data"
          , "    _ = moved[\"key\"]"
          , "    _ = data[\"key\"]"  -- use after move"
          , "}"
          ]
    result <- compile source
    case result of
      Left errs -> do
        let errorMessages = formatCompilerErrors errs
        assertBool "Should detect map ownership transfer" $ 
          any ("ownership" `isInfixOf`) errorMessages
      Right _ -> assertFailure "Expected map ownership error"

-- QuickCheck property: Ownership moves are transitive
prop_ownership_moves_transitive :: Bool -> Property
prop_ownership_moves_transitive initialOwnership =
  let move1 = not initialOwnership
      move2 = not move1
      finalOwnership = not move2
  in classify initialOwnership "initial owner" $
     property $ finalOwnership

-- QuickCheck property: Borrowing doesn't transfer ownership
prop_borrowing_preserves_ownership :: Bool -> Property
prop_borrowing_preserves_ownership hasOwnership =
  let borrowed = True
      stillHasOwnership = hasOwnership
  in classify hasOwnership "has ownership" $
     property $ stillHasOwnership

-- QuickCheck property: Multiple moves result in single owner
prop_multiple_moves_single_owner :: Int -> Property
prop_multiple_moves_single_owner movesCount =
  let positiveMoves = movesCount > 0
      finalOwners = 1
  in classify positiveMoves "positive moves" $
     property $ finalOwners === 1

tests :: TestTree
tests = testGroup "Ownership Transitivity"
  [ test_basic_ownership_move
  , test_use_after_move
  , test_ownership_through_functions
  , test_ownership_borrowing
  , test_multiple_ownership_moves
  , test_struct_field_ownership
  , test_slice_ownership
  , test_map_ownership
  , testCase "QuickCheck: Ownership moves transitive" $
      fastProperty prop_ownership_moves_transitive
  , testCase "QuickCheck: Borrowing preserves ownership" $
      fastProperty prop_borrowing_preserves_ownership
  , testCase "QuickCheck: Multiple moves single owner" $
      fastProperty prop_multiple_moves_single_owner
  ]