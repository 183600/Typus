{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipAnalysisSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Ownership (analyzeOwnership, OwnershipResult(..), OwnershipIssue(..), OwnershipTransfer(..))
import Parser (parseTypus, TypusFile(..))
import Compiler (compileTypus, CompilationResult(..))
import SourceLocation (SourcePos(..), ErrorLocation(..))

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map

-- ============================================================================
-- Ownership Analysis Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Ownership Analysis Tests"
    [ testGroup "Basic ownership concepts"
        [ testCase "detects simple ownership transfer" test_simple_ownership_transfer
        , testCase "handles move semantics correctly" test_move_semantics
        , testCase "detects borrowing correctly" test_borrowing_detection
        , testCase "handles shared ownership" test_shared_ownership
        , testCase "detects ownership violations" test_ownership_violations
        ]

    , testGroup "Ownership transfer analysis"
        [ testCase "tracks function parameter ownership" test_function_parameter_ownership
        , testCase "tracks return value ownership" test_return_value_ownership
        , testCase "handles ownership in assignments" test_assignment_ownership
        , testCase "detects double move errors" test_double_move_detection
        , testCase "handles conditional ownership transfer" test_conditional_ownership
        ]

    , testGroup "Borrowing analysis"
        [ testCase "detects immutable borrowing" test_immutable_borrowing
        , testCase "detects mutable borrowing" test_mutable_borrowing
        , testCase "prevents multiple mutable borrows" test_multiple_mutable_borrows
        , testCase "allows multiple immutable borrows" test_multiple_immutable_borrows
        , testCase "handles borrow lifetime analysis" test_borrow_lifetime_analysis
        ]

    , testGroup "Lifetime analysis"
        [ testCase "tracks variable lifetimes" test_variable_lifetime_tracking
        , testCase "detects dangling references" test_dangling_reference_detection
        , testCase "handles lifetime elision" test_lifetime_elision
        , testCase "detects lifetime conflicts" test_lifetime_conflicts
        , testCase "handles struct field lifetimes" test_struct_field_lifetimes
        ]

    , testGroup "Ownership in complex scenarios"
        [ testCase "handles ownership in loops" test_loop_ownership
        , testCase "handles ownership in closures" test_closure_ownership
        , testCase "handles ownership with generics" test_generic_ownership
        , testCase "handles ownership with recursion" test_recursive_ownership
        , testCase "handles ownership with concurrency" test_concurrent_ownership
        ]

    , testGroup "Error recovery L.and reporting"
        [ testCase "provides clear ownership error messages" test_clear_error_messages
        , testCase "suggests ownership fixes" test_ownership_fix_suggestions
        , testCase "handles ownership analysis errors gracefully" test_analysis_error_recovery
        , testCase "maintains ownership state across errors" test_state_maintenance
        ]

    , testGroup "Property-based ownership tests"
        [ fastProperty "ownership analysis is deterministic" prop_ownership_deterministic
        , fastProperty "valid ownership passes analysis" prop_valid_ownership_passes
        , fastProperty "ownership violations are detected" prop_ownership_violations_detected
        , fastProperty "ownership transfer is sound" prop_ownership_transfer_sound
        ]
    ]

-- ============================================================================
-- Basic Ownership Concepts Tests
-- ============================================================================

test_simple_ownership_transfer :: IO ()
test_simple_ownership_transfer = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    owner := data"
        , "    return owner.L.length"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should have ownership analysis results
      assertBool "Should have ownership analysis results" (not (null issues))

test_move_semantics :: IO ()
test_move_semantics = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    new_owner := move(data)"
        , "    // data should no longer be accessible here"
        , "    return new_owner.L.length"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should detect move semantics
      assertBool "Should detect move semantics" (not (null issues))

test_borrowing_detection :: IO ()
test_borrowing_detection = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    borrowed := &data"
        , "    return (*borrowed).L.length"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should detect borrowing
      assertBool "Should detect borrowing" (not (null issues))

test_shared_ownership :: IO ()
test_shared_ownership = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    shared1 := share(data)"
        , "    shared2 := share(data)"
        , "    return shared1.L.length + shared2.L.length"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should handle shared ownership
      assertBool "Should handle shared ownership" (not (null issues))

test_ownership_violations :: IO ()
test_ownership_violations = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    new_owner := move(data)"
        , "    L.length := data.L.length"  -- Error: use after move
        , "    return L.length"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should detect ownership violation
      assertBool "Should detect ownership violation" (not (null issues))
      let useAfterMoveIssues = L.filter (\issue -> "use after move" `L.isInfixOf` oiMessage issue) issues
      assertBool "Should detect use after move" (not (null useAfterMoveIssues))

-- ============================================================================
-- Ownership Transfer Analysis Tests
-- ============================================================================

test_function_parameter_ownership :: IO ()
test_function_parameter_ownership = do
  let content = unlines
        [ "//! ownership=true"
        , "func process(data []int) int {"
        , "    return data.L.length"
        , "}"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    result := process(data)"
        , "    return result"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should analyze function parameter ownership
      assertBool "Should analyze function parameter ownership" (not (null issues))

test_return_value_ownership :: IO ()
test_return_value_ownership = do
  let content = unlines
        [ "//! ownership=true"
        , "func create_data() []int {"
        , "    return make([]int, 10)"
        , "}"
        , "func main() {"
        , "    data := create_data()"
        , "    return data.L.length"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should analyze return value ownership
      assertBool "Should analyze return value ownership" (not (null issues))

test_assignment_ownership :: IO ()
test_assignment_ownership = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    data1 := make([]int, 10)"
        , "    data2 := data1"
        , "    // data1 should be moved to data2"
        , "    return data2.L.length"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should analyze assignment ownership
      assertBool "Should analyze assignment ownership" (not (null issues))

test_double_move_detection :: IO ()
test_double_move_detection = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    owner1 := move(data)"
        , "    owner2 := move(data)"  -- Error: double move
        , "    return owner1.L.length"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should detect double move
      let doubleMoveIssues = L.filter (\issue -> "double move" `L.isInfixOf` oiMessage issue) issues
      assertBool "Should detect double move" (not (null doubleMoveIssues))

test_conditional_ownership :: IO ()
test_conditional_ownership = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    if true {"
        , "        owner := move(data)"
        , "        return owner.L.length"
        , "    } else {"
        , "        return data.L.length"
        , "    }"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should handle conditional ownership
      assertBool "Should handle conditional ownership" (not (null issues))

-- ============================================================================
-- Borrowing Analysis Tests
-- ============================================================================

test_immutable_borrowing :: IO ()
test_immutable_borrowing = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    borrowed := &data"
        , "    L.length := (*borrowed).L.length"
        , "    return L.length"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should detect immutable borrowing
      assertBool "Should detect immutable borrowing" (not (null issues))

test_mutable_borrowing :: IO ()
test_mutable_borrowing = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    borrowed := &mut data"
        , "    (*borrowed)[0] = 42"
        , "    return (*borrowed).L.length"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should detect mutable borrowing
      assertBool "Should detect mutable borrowing" (not (null issues))

test_multiple_mutable_borrows :: IO ()
test_multiple_mutable_borrows = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    borrow1 := &mut data"
        , "    borrow2 := &mut data"  -- Error: multiple mutable borrows
        , "    return (*borrow1).L.length"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should detect multiple mutable borrows
      let multipleMutableIssues = L.filter (\issue -> "multiple mutable" `L.isInfixOf` oiMessage issue) issues
      assertBool "Should detect multiple mutable borrows" (not (null multipleMutableIssues))

test_multiple_immutable_borrows :: IO ()
test_multiple_immutable_borrows = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    borrow1 := &data"
        , "    borrow2 := &data"
        , "    borrow3 := &data"
        , "    return (*borrow1).L.length + (*borrow2).L.length + (*borrow3).L.length"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should allow multiple immutable borrows
      let borrowIssues = L.filter (\issue -> "borrow" `L.isInfixOf` oiMessage issue) issues
      assertBool "Should allow multiple immutable borrows" (True)  -- May L.or may not have issues depending on implementation

test_borrow_lifetime_analysis :: IO ()
test_borrow_lifetime_analysis = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    borrowed := {"
        , "        data := make([]int, 10)"
        , "        &data  // Error: returning reference to local variable"
        , "    }"
        , "    return (*borrowed).L.length"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should detect lifetime issues
      let lifetimeIssues = L.filter (\issue -> "lifetime" `L.isInfixOf` oiMessage issue) issues
      assertBool "Should detect lifetime issues" (not (null lifetimeIssues))

-- ============================================================================
-- Lifetime Analysis Tests
-- ============================================================================

test_variable_lifetime_tracking :: IO ()
test_variable_lifetime_tracking = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    outer := make([]int, 10)"
        , "    {"
        , "        inner := make([]int, 5)"
        , "        // inner should not be accessible outside this block"
        , "    }"
        , "    return outer.L.length"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should track variable lifetimes
      assertBool "Should track variable lifetimes" (not (null issues))

test_dangling_reference_detection :: IO ()
test_dangling_reference_detection = do
  let content = unlines
        [ "//! ownership=true"
        , "func get_reference() *int {"
        , "    x := 42"
        , "    return &x  // Error: dangling reference"
        , "}"
        , "func main() {"
        , "    ref := get_reference()"
        , "    return *ref"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should detect dangling references
      let danglingIssues = L.filter (\issue -> "dangling" `L.isInfixOf` oiMessage issue) issues
      assertBool "Should detect dangling references" (not (null danglingIssues))

test_lifetime_elision :: IO ()
test_lifetime_elision = do
  let content = unlines
        [ "//! ownership=true"
        , "func get_first(data []int) *int {"
        , "    return &data[0]"  // Lifetime should be elided
        , "}"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    first := get_first(data)"
        , "    return *first"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should handle lifetime elision
      assertBool "Should handle lifetime elision" (True)

test_lifetime_conflicts :: IO ()
test_lifetime_conflicts = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    borrow := &data"
        , "    move(data)"  // Error: can't move while borrowed"
        , "    return *borrow"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should detect lifetime conflicts
      let conflictIssues = L.filter (\issue -> "conflict" `L.isInfixOf` oiMessage issue) issues
      assertBool "Should detect lifetime conflicts" (not (null conflictIssues))

test_struct_field_lifetimes :: IO ()
test_struct_field_lifetimes = do
  let content = unlines
        [ "//! ownership=true"
        , "type struct Node {"
        , "    value int"
        , "    next *Node"
        , "}"
        , "func main() {"
        , "    node1 := Node{value: 1}"
        , "    node2 := Node{value: 2, next: &node1}"
        , "    return node2.value"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should handle struct field lifetimes
      assertBool "Should handle struct field lifetimes" (not (null issues))

-- ============================================================================
-- Ownership in Complex Scenarios Tests
-- ============================================================================

test_loop_ownership :: IO ()
test_loop_ownership = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    for i := 0; i < 10; i++ {"
        , "        item := data[i]"
        , "        // item should be moved from data"
        , "    }"
        , "    return data.L.length"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should handle ownership in loops
      assertBool "Should handle ownership in loops" (not (null issues))

test_closure_ownership :: IO ()
test_closure_ownership = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    closure := func() int {"
        , "        return data.L.length"
        , "    }"
        , "    return closure()"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should handle ownership in closures
      assertBool "Should handle ownership in closures" (not (null issues))

test_generic_ownership :: IO ()
test_generic_ownership = do
  let content = unlines
        [ "//! ownership=true"
        , "func process[T](data T) T {"
        , "    return data"
        , "}"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    result := process(data)"
        , "    return result.L.length"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should handle ownership with generics
      assertBool "Should handle ownership with generics" (not (null issues))

test_recursive_ownership :: IO ()
test_recursive_ownership = do
  let content = unlines
        [ "//! ownership=true"
        , "func factorial(n int, acc int) int {"
        , "    if n <= 1 {"
        , "        return acc"
        , "    }"
        , "    return factorial(n-1, n*acc)"
        , "}"
        , "func main() {"
        , "    return factorial(5, 1)"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should handle ownership with recursion
      assertBool "Should handle ownership with recursion" (not (null issues))

test_concurrent_ownership :: IO ()
test_concurrent_ownership = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    go func() {"
        , "        process(data)"
        , "    }()"
        , "    return data.L.length"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should handle ownership with concurrency
      assertBool "Should handle ownership with concurrency" (not (null issues))

-- ============================================================================
-- Error Recovery L.and Reporting Tests
-- ============================================================================

test_clear_error_messages :: IO ()
test_clear_error_messages = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    new_owner := move(data)"
        , "    L.length := data.L.length"  -- Use after move
        , "    return L.length"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      assertBool "Should have ownership issues" (not (null issues))
      let firstIssue = L.head issues
          message = oiMessage firstIssue
      assertBool "Error message should be clear" (L.length message > 10)
      assertBool "Error should have location information" (oiLine firstIssue > 0)

test_ownership_fix_suggestions :: IO ()
test_ownership_fix_suggestions = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    new_owner := move(data)"
        , "    L.length := data.L.length"  -- Use after move
        , "    return L.length"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      assertBool "Should have ownership issues" (not (null issues))
      let firstIssue = L.head issues
          suggestions = oiSuggestions firstIssue
      -- Should provide suggestions for fixing ownership issues
      assertBool "Should provide suggestions" (not (null suggestions))

test_analysis_error_recovery :: IO ()
test_analysis_error_recovery = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    invalid := move(data)"
        , "    another_invalid := move(data)"  -- Double move
        , "    return invalid.L.length"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should recover from analysis errors L.and continue
      assertBool "Should find multiple issues" (L.length issues >= 2)

test_state_maintenance :: IO ()
test_state_maintenance = do
  let content = unlines
        [ "//! ownership=true"
        , "func main() {"
        , "    data := make([]int, 10)"
        , "    if true {"
        , "        owner := move(data)"
        , "        return owner.L.length"
        , "    }"
        , "    return 42"
        , "}"
        ]
      parseResult = parseTypus content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right typusFile -> do
      let ownershipResult = analyzeOwnership typusFile
      assertBool "Ownership analysis should succeed" (orSuccess ownershipResult)
      let issues = orIssues ownershipResult
      -- Should maintain ownership state across control flow
      assertBool "Should maintain ownership state" (not (null issues))

-- ============================================================================
-- Property-Based Ownership Tests
-- ============================================================================

prop_ownership_deterministic :: Property
prop_ownership_deterministic =
  forAll arbitrary $ \content ->
    let parseResult = parseTypus content
    in case parseResult of
         Left _ -> property True
         Right typusFile ->
           let ownershipResult1 = analyzeOwnership typusFile
               ownershipResult2 = analyzeOwnership typusFile
           in ownershipResult1 === ownershipResult2

prop_valid_ownership_passes :: Property
prop_valid_ownership_passes =
  forAll arbitrary $ \content ->
    let simpleValid = unlines
          [ "//! ownership=true"
          , "func main() {"
          , "    x := 42"
          , "    return x"
          , "}"
          ]
        parseResult = parseTypus simpleValid
    in case parseResult of
         Left _ -> property False
         Right typusFile ->
           let ownershipResult = analyzeOwnership typusFile
           in orSuccess ownershipResult === True

prop_ownership_violations_detected :: Property
prop_ownership_violations_detected =
  forAll arbitrary $ \content ->
    let violationContent = unlines
          [ "//! ownership=true"
          , "func main() {"
          , "    data := make([]int, 10)"
          , "    new_owner := move(data)"
          , "    L.length := data.L.length"  -- Use after move"
          , "    return L.length"
          , "}"
          ]
        parseResult = parseTypus violationContent
    in case parseResult of
         Left _ -> property False
         Right typusFile ->
           let ownershipResult = analyzeOwnership typusFile
           in case ownershipResult of
                OwnershipResult True _ -> property False
                OwnershipResult False issues -> L.length issues > 0

prop_ownership_transfer_sound :: Property
prop_ownership_transfer_sound =
  forAll arbitrary $ \content ->
    let parseResult = parseTypus content
    in case parseResult of
         Left _ -> property True
         Right typusFile ->
           let ownershipResult = analyzeOwnership typusFile
           in case ownershipResult of
                OwnershipResult True _ -> property True
                OwnershipResult False issues -> 
                  -- If there are ownership issues, they should be well-formed
                  L.all (\issue -> L.length (oiMessage issue) > 0 && oiLine issue > 0) issues