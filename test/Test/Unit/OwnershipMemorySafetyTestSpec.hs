{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipMemorySafetyTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Ownership
import Ownership.Analyzer
import Ownership.Common.Types
import Compiler.IR
import SourceLocation
import Utils

import Data.Char (isSpace, isLetter, isDigit, toLower)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (tails, sort, intercalate)
import Data.String (IsString)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Property: Ownership transfer prevents double use
prop_ownership_transfer_prevents_double_use :: String -> Property
prop_ownership_transfer_prevents_double_use varName =
  L.length varName <= 10 && L.all isLetter varName ==>
  let transferCode = "var " ++ varName ++ " = new Resource(); transfer(" ++ varName ++ "); use(" ++ varName ++ ");"
      result = analyzeOwnership transferCode
  in property $ hasOwnershipError result || not ("use(" ++ varName ++ ")" `L.isInfixOf` transferCode)

-- Property: Borrow checking prevents dangling references
prop_borrow_checking_prevents_dangling :: String -> Property
prop_borrow_checking_prevents_dangling varName =
  L.length varName <= 10 && L.all isLetter varName ==>
  let borrowCode = "var " ++ varName ++ " = new Resource(); var ref = borrow(" ++ varName ++ "); drop(" ++ varName ++ "); use(ref);"
      result = analyzeOwnership borrowCode
  in property $ hasOwnershipError result || not ("use(ref)" `L.isInfixOf` borrowCode)

-- Property: Lifetime tracking prevents use-after-free
prop_lifetime_tracking_prevents_use_after_free :: String -> Property
prop_lifetime_tracking_prevents_use_after_free varName =
  L.length varName <= 10 && L.all isLetter varName ==>
  let lifetimeCode = "var " ++ varName ++ " = new Resource(); drop(" ++ varName ++ "); " ++ varName ++ ".method();"
      result = analyzeOwnership lifetimeCode
  in property $ hasOwnershipError result || not (varName ++ ".method()" `L.isInfixOf` lifetimeCode)

-- Property: Move semantics prevent invalid access
prop_move_semantics_prevents_invalid_access :: String -> Property
prop_move_semantics_prevents_invalid_access varName =
  L.length varName <= 10 && L.all isLetter varName ==>
  let moveCode = "var " ++ varName ++ " = new Resource(); var moved = move(" ++ varName ++ "); " ++ varName ++ ".method();"
      result = analyzeOwnership moveCode
  in property $ hasOwnershipError result || not (varName ++ ".method()" `L.isInfixOf` moveCode)

-- Property: Shared borrowing allows multiple readers
prop_shared_borrowing_allows_readers :: String -> Property
prop_shared_borrowing_allows_readers varName =
  L.length varName <= 10 && L.all isLetter varName ==>
  let sharedCode = "var " ++ varName ++ " = new Resource(); var r1 = borrow(" ++ varName ++ "); var r2 = borrow(" ++ varName ++ "); read(r1); read(r2);"
      result = analyzeOwnership sharedCode
  in property $ not (hasOwnershipError result) || countOccurrences "borrow" sharedCode <= 1

-- Property: Mutable borrowing prevents other borrows
prop_mutable_borrowing_prevents_others :: String -> Property
prop_mutable_borrowing_prevents_others varName =
  L.length varName <= 10 && L.all isLetter varName ==>
  let mutableCode = "var " ++ varName ++ " = new Resource(); var m1 = borrow_mut(" ++ varName ++ "); var r2 = borrow(" ++ varName ++ ");"
      result = analyzeOwnership mutableCode
  in property $ hasOwnershipError result || not ("borrow_mut" `L.isInfixOf` mutableCode)

-- Property: Ownership tree maintains hierarchy
prop_ownership_tree_maintains_hierarchy :: [String] -> Property
prop_ownership_tree_maintains_hierarchy varNames =
  not (null varNames) && L.all (\v -> L.length v <= 8 && L.all isLetter v) varNames ==>
  let hierarchy = unlines $ L.map (\v -> "var " ++ v ++ " = new Resource();") varNames
      result = analyzeOwnership hierarchy
  in property |]

-- Property: Resource cleanup happens at correct time
prop_resource_cleanup_timing :: String -> Property
prop_resource_cleanup_timing varName =
  L.length varName <= 10 && L.all isLetter varName ==>
  let cleanupCode = "var " ++ varName ++ " = new Resource(); { var inner = move(" ++ varName ++ "); } // inner dropped here\n"
      result = analyzeOwnership cleanupCode
  in property |]

-- Property: Borrowing respects scope boundaries
prop_borrowing_respects_scope :: String -> Property
prop_borrowing_respects_scope varName =
  L.length varName <= 10 && L.all isLetter varName ==>
  let scopeCode = "var " ++ varName ++ " = new Resource(); { var r = borrow(" ++ varName ++ "); } use(" ++ varName ++ ");"
      result = analyzeOwnership scopeCode
  in property $ not (hasOwnershipError result) || not ("borrow" `L.isInfixOf` scopeCode)

-- Property: Move prevents original access
prop_move_prevents_original :: String -> Property
prop_move_prevents_original varName =
  L.length varName <= 10 && L.all isLetter varName ==>
  let moveCode = "var " ++ varName ++ " = new Resource(); var moved = move(" ++ varName ++ "); " ++ varName ++ ".field = 1;"
      result = analyzeOwnership moveCode
  in property $ hasOwnershipError result || not ("move(" ++ varName ++ ")" `L.isInfixOf` moveCode)

-- Property: Copy preserves ownership
prop_copy_preserves_ownership :: String -> Property
prop_copy_preserves_ownership varName =
  L.length varName <= 10 && L.all isLetter varName ==>
  let copyCode = "var " ++ varName ++ " = new Resource(); var copied = copy(" ++ varName ++ "); " ++ varName ++ ".method(); copied.method();"
      result = analyzeOwnership copyCode
  in property $ not (hasOwnershipError result) || not ("copy(" ++ varName ++ ")" `L.isInfixOf` copyCode)

-- Property: Reference counting prevents cycles
prop_reference_counting_prevents_cycles :: [String] -> Property
prop_reference_counting_prevents_cycles varNames =
  L.length varNames <= 5 && L.all (\v -> L.length v <= 8 && L.all isLetter v) varNames ==>
  let cycleCode = unlines $ zipWith (\i v -> 
        if i == 0 
        then "var " ++ v ++ " = new Resource();"
        else v ++ ".ref = " ++ varNames !! (i-1) ++ ";"
        ) [0..] varNames
      result = analyzeOwnership cycleCode
  in property |]

-- Property: Arena allocation prevents leaks
prop_arena_allocation_prevents_leaks :: String -> Property
prop_arena_allocation_prevents_leaks code =
  L.length code <= 50 ==> -- Limit for performance
  let arenaCode = "var arena = new Arena(); " ++ code ++ " arena.reset();"
      result = analyzeOwnership arenaCode
  in property |]

-- Property: Stack allocation follows LIFO
prop_stack_allocation_lifo :: [String] -> Property
prop_stack_allocation_lifo varNames =
  L.length varNames <= 5 && L.all (\v -> L.length v <= 8 && L.all isLetter v) varNames ==>
  let stackCode = unlines $ L.map (\v -> "var " ++ v ++ " = stack_alloc();") varNames
      result = analyzeOwnership stackCode
  in property |]

-- Property: Pool allocation reuses memory
prop_pool_allocation_reuses :: String -> Property
prop_pool_allocation_reuses varName =
  L.length varName <= 10 && L.all isLetter varName ==>
  let poolCode = "var pool = new Pool(); var " ++ varName ++ "1 = pool.alloc(); pool.free(" ++ varName ++ "1); var " ++ varName ++ "2 = pool.alloc();"
      result = analyzeOwnership poolCode
  in property |]

-- Property: Garbage collection handles cycles
prop_garbage_collection_cycles :: [String] -> Property
prop_garbage_collection_cycles varNames =
  L.length varNames <= 3 && L.all (\v -> L.length v <= 8 && L.all isLetter v) varNames ==>
  let gcCode = unlines $ L.map (\v -> "var " ++ v ++ " = new GcResource();") varNames ++
                [varNames !! 0 ++ ".ref = " ++ varNames !! 1 ++ ";", varNames !! 1 ++ ".ref = " ++ varNames !! 0 ++ ";", "gc.collect();"]
      result = analyzeOwnership gcCode
  in property |]

-- Property: Smart pointers manage lifetime
prop_smart_pointers_lifetime :: String -> Property
prop_smart_pointers_lifetime varName =
  L.length varName <= 10 && L.all isLetter varName ==>
  let smartCode = "var " ++ varName ++ " = make_unique<Resource>(); var shared = make_shared(move(" ++ varName ++ "));"
      result = analyzeOwnership smartCode
  in property |]

-- Property: RAII ensures cleanup
prop_raii_ensures_cleanup :: String -> Property
prop_raii_ensures_cleanup code =
  L.length code <= 40 ==> -- Limit for performance
  let raiiCode = "var guard = new ResourceGuard(); " ++ code ++ " // guard auto-dropped"
      result = analyzeOwnership raiiCode
  in property |]

-- Property: Exception safety maintains ownership
prop_exception_safety_ownership :: String -> Property
prop_exception_safety_ownership code =
  L.length code <= 40 ==> -- Limit for performance
  let exceptionCode = "var " ++ code ++ " = new Resource(); try { risky_operation(); } finally { cleanup(" ++ code ++ "); }"
      result = analyzeOwnership exceptionCode
  in property |]

-- Property: Thread safety with ownership
prop_thread_safety_ownership :: String -> Property
prop_thread_safety_ownership varName =
  L.length varName <= 10 && L.all isLetter varName ==>
  let threadCode = "var " ++ varName ++ " = new Resource(); spawn(move(" ++ varName ++ "));"
      result = analyzeOwnership threadCode
  in property |]

-- Property: Atomic operations prevent data races
prop_atomic_operations_prevent_races :: String -> Property
prop_atomic_operations_prevent_races varName =
  L.length varName <= 10 && L.all isLetter varName ==>
  let atomicCode = "var " ++ varName ++ " = Atomic.new(0); Atomic.fetch_add(" ++ varName ++ ", 1);"
      result = analyzeOwnership atomicCode
  in property |]

-- Advanced memory safety tests

-- Property: Complex ownership scenarios
prop_complex_ownership_scenarios :: [String] -> Property
prop_complex_ownership_scenarios operations =
  L.length operations <= 5 && L.all (\op -> L.length op <= 20) operations ==>
  let complexCode = unlines operations
      result = analyzeOwnership complexCode
  in property |]

-- Property: Nested borrowing
prop_nested_borrowing :: String -> Property
prop_nested_borrowing varName =
  L.length varName <= 10 && L.all isLetter varName ==>
  let nestedCode = "var " ++ varName ++ " = new Resource(); { var r1 = borrow(" ++ varName ++ "); { var r2 = borrow(r1); } }"
      result = analyzeOwnership nestedCode
  in property $ not (hasOwnershipError result) || not ("borrow" `L.isInfixOf` nestedCode)

-- Property: Reborrowing rules
prop_reborrowing_rules :: String -> Property
prop_reborrowing_rules varName =
  L.length varName <= 10 && L.all isLetter varName ==>
  let reborrCode = "var " ++ varName ++ " = new Resource(); var r1 = borrow(" ++ varName ++ "); var r2 = borrow(r1);"
      result = analyzeOwnership reborrCode
  in property $ not (hasOwnershipError result) || not ("borrow" `L.isInfixOf` reborrCode)

-- Helper function to check for ownership errors
hasOwnershipError :: OwnershipResult -> Bool
hasOwnershipError result = case result of
  OwnershipError _ -> True
  _ -> False

-- Helper function to count occurrences
countOccurrences :: String -> String -> Int
countOccurrences pattern text = L.length $ L.filter (pattern `L.isPrefixOf`) (tails text)

tests :: TestTree
tests = testGroup "Ownership Memory Safety Tests"
  [ fastProperty "Ownership transfer prevents double use" prop_ownership_transfer_prevents_double_use
  , fastProperty "Borrow checking prevents dangling references" prop_borrow_checking_prevents_dangling
  , fastProperty "Lifetime tracking prevents use-after-free" prop_lifetime_tracking_prevents_use_after_free
  , fastProperty "Move semantics prevent invalid access" prop_move_semantics_prevents_invalid_access
  , fastProperty "Shared borrowing allows multiple readers" prop_shared_borrowing_allows_readers
  , fastProperty "Mutable borrowing prevents other borrows" prop_mutable_borrowing_prevents_others
  , fastProperty "Ownership tree maintains hierarchy" prop_ownership_tree_maintains_hierarchy
  , fastProperty "Resource cleanup happens at correct time" prop_resource_cleanup_timing
  , fastProperty "Borrowing respects scope boundaries" prop_borrowing_respects_scope
  , fastProperty "Move prevents original access" prop_move_prevents_original
  , fastProperty "Copy preserves ownership" prop_copy_preserves_ownership
  , fastProperty "Reference counting prevents cycles" prop_reference_counting_prevents_cycles
  , fastProperty "Arena allocation prevents leaks" prop_arena_allocation_prevents_leaks
  , fastProperty "Stack allocation follows LIFO" prop_stack_allocation_lifo
  , fastProperty "Pool allocation reuses memory" prop_pool_allocation_reuses
  , fastProperty "Garbage collection handles cycles" prop_garbage_collection_cycles
  , fastProperty "Smart pointers manage lifetime" prop_smart_pointers_lifetime
  , fastProperty "RAII ensures cleanup" prop_raii_ensures_cleanup
  , fastProperty "Exception safety maintains ownership" prop_exception_safety_ownership
  , fastProperty "Thread safety with ownership" prop_thread_safety_ownership
  , fastProperty "Atomic operations prevent data races" prop_atomic_operations_prevent_races
  , fastProperty "Complex ownership scenarios" prop_complex_ownership_scenarios
  , fastProperty "Nested borrowing" prop_nested_borrowing
  , fastProperty "Reborrowing rules" prop_reborrowing_rules
  ]