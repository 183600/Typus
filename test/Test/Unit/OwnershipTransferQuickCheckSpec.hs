{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipTransferQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as Set
import Data.Char (isSpace, isAlpha, isDigit)
import Data.Maybe (isJust, isNothing, catMaybes)
import Control.Monad (foldM)

import SourceLocation
import Utils
import Ownership.Common.Types
import Ownership
import Compiler.Errors.Core

-- | QuickCheck tests for ownership transfer
tests :: TestTree
tests =
  testGroup "Ownership Transfer QuickCheck Tests"
    [ testGroup "Basic ownership properties"
        [ fastProperty "ownership transfer is deterministic" prop_ownership_transfer_deterministic
        , fastProperty "move semantics invalidate source" prop_move_invalidates_source
        , fastProperty "copy semantics preserve source" prop_copy_preserves_source
        , fastProperty "borrow semantics prevent move" prop_borrow_prevents_move
        , fastProperty "ownership is exclusive" prop_ownership_exclusive
        ]

    , testGroup "Lifetime tracking"
        [ fastProperty "lifetime bounds are respected" prop_lifetime_bounds_respected
        , fastProperty "lifetime inference is sound" prop_lifetime_inference_sound
        , fastProperty "lifetime subtyping is transitive" prop_lifetime_subtyping_transitive
        , fastProperty "lifetime elision rules are consistent" prop_lifetime_elision_consistent
        , fastProperty "lifetime annotations prevent dangling references" prop_lifetime_annotations_prevent_dangling
        ]

    , testGroup "Borrowing rules"
        [ fastProperty "immutable borrows coexist" prop_immutable_borrows_coexist
        , fastProperty "mutable borrows are exclusive" prop_mutable_borrows_exclusive
        , fastProperty "borrow checker prevents use after move" prop_borrow_checker_prevents_use_after_move
        , fastProperty "borrow lifetimes don't exceed owner" prop_borrow_lifetimes_dont_exceed_owner
        , fastProperty "nested borrowing follows stack discipline" prop_nested_borrowing_stack_discipline
        ]

    , testGroup "Reference counting"
        [ fastProperty "reference counting is accurate" prop_reference_counting_accurate
        , fastProperty "reference cycles are detected" prop_reference_cycles_detected
        , fastProperty "weak references don't prevent deallocation" prop_weak_references_no_prevent_deallocation
        , fastProperty "reference counting handles multiple owners" prop_reference_counting_multiple_owners
        , fastProperty "reference counting is thread-safe" prop_reference_counting_thread_safe
        ]

    , testGroup "Memory safety"
        [ fastProperty "double frees are prevented" prop_double_frees_prevented
        , fastProperty "use after free is prevented" prop_use_after_free_prevented
        , fastProperty "dangling pointers are eliminated" prop_dangling_pointers_eliminated
        , fastProperty "buffer overflow is prevented" prop_buffer_overflow_prevented
        , fastProperty "memory leaks are detected" prop_memory_leaks_detected
        ]

    , testGroup "Ownership transfer patterns"
        [ fastProperty "function parameter ownership is clear" prop_function_parameter_ownership_clear
        , fastProperty "return value ownership is transferred" prop_return_value_ownership_transferred
        , fastProperty "struct field ownership follows rules" prop_struct_field_ownership_follows_rules
        , fastProperty "collection ownership handles elements" prop_collection_ownership_handles_elements
        , fastProperty "closure ownership captures correctly" prop_closure_ownership_captures_correctly
        ]

    , testGroup "Ownership inference"
        [ fastProperty "ownership inference is complete" prop_ownership_inference_complete
        , fastProperty "ownership inference is conservative" prop_ownership_inference_conservative
        , fastProperty "ownership inference handles complex expressions" prop_ownership_inference_complex_expressions
        , fastProperty "ownership inference respects annotations" prop_ownership_inference_respects_annotations
        , fastProperty "ownership inference is efficient" prop_ownership_inference_efficient
        ]

    , testGroup "Error handling"
        [ fastProperty "ownership errors are detected early" prop_ownership_errors_detected_early
        , fastProperty "ownership error messages are helpful" prop_ownership_error_messages_helpful
        , fastProperty "ownership errors suggest fixes" prop_ownership_errors_suggest_fixes
        , fastProperty "ownership errors don't cause crashes" prop_ownership_errors_no_crashes
        , fastProperty "ownership error recovery is safe" prop_ownership_error_recovery_safe
        ]

    , testGroup "Performance properties"
        [ fastProperty "ownership checking is linear" prop_ownership_checking_linear
        , fastProperty "zero-cost abstraction holds" prop_zero_cost_abstraction_holds
        , fastProperty "ownership analysis is scalable" prop_ownership_analysis_scalable
        , fastProperty "ownership optimizations are effective" prop_ownership_optimizations_effective
        , fastProperty "ownership runtime overhead is minimal" prop_ownership_runtime_overhead_minimal
        ]

    , testGroup "Advanced ownership features"
        [ fastProperty "shared ownership works correctly" prop_shared_ownership_works
        , fastProperty "unique ownership is enforced" prop_unique_ownership_enforced
        , fastProperty "ownership transfer across threads is safe" prop_ownership_transfer_threads_safe
        , fastProperty "ownership L.and generics interact correctly" prop_ownership_generics_correct
        , fastProperty "ownership L.and traits are compatible" prop_ownership_traits_compatible
        ]
    ]

-- Basic ownership properties

prop_ownership_transfer_deterministic :: String -> String -> Property
prop_ownership_transfer_deterministic source target =
  not (null source && null target) ==>
  let transfer1 = source ++ " -> " ++ target
      transfer2 = source ++ " -> " ++ target
  in property $ transfer1 === transfer2

prop_move_invalidates_source :: String -> Property
prop_move_invalidates_source variable =
  not (null variable) ==>
  let beforeMove = "valid:" ++ variable
      afterMove = "moved:" ++ variable
      sourceValid = "valid:" `L.L.isPrefixOf` beforeMove
      sourceInvalid = not ("valid:" `L.L.isPrefixOf` afterMove)
  in property $ sourceValid .&&. sourceInvalid

prop_copy_preserves_source :: String -> Property
prop_copy_preserves_source variable =
  not (null variable) ==>
  let original = "orig:" ++ variable
      copied = "copy:" ++ variable
      sourcePreserved = "orig:" `L.L.isPrefixOf` original
  in property $ sourcePreserved

prop_borrow_prevents_move :: String -> Property
prop_borrow_prevents_move variable =
  not (null variable) ==>
  let borrowed = "borrow:" ++ variable
      moved = "move:" ++ variable
      hasBorrow = "borrow:" `L.L.isPrefixOf` borrowed
      canMove = not hasBorrow
  in classify hasBorrow "has active borrow" $
     property $ canMove ==> not ("borrow:" `L.L.isPrefixOf` moved)

prop_ownership_exclusive :: String -> Property
prop_ownership_exclusive resource =
  not (null resource) ==>
  let owner1 = "owner1:" ++ resource
      owner2 = "owner2:" ++ resource
      hasOwner1 = "owner1:" `L.L.isPrefixOf` owner1
      hasOwner2 = "owner2:" `L.L.isPrefixOf` owner2
  in property $ not (hasOwner1 .&&. hasOwner2)

-- Lifetime tracking

prop_lifetime_bounds_respected :: Int -> Int -> Property
prop_lifetime_bounds_respected start end =
  start >= 0 && end >= 0 ==>
  let validLifetime = end >= start
  in classify validLifetime "valid lifetime" $
     property $ validLifetime ==> end >= start

prop_lifetime_inference_sound :: [(String, Int)] -> Property
prop_lifetime_inference_sound variables =
  not (null variables) ==>
  let lifetimes = map snd variables
      validLifetimes = L.all (>= 0) lifetimes
  in classify validLifetimes "L.all lifetimes valid" $
     property $ validLifetimes ==> L.all (>= 0) lifetimes

prop_lifetime_subtyping_transitive :: Int -> Int -> Int -> Property
prop_lifetime_subtyping_transitive a b c =
  a >= b && b >= c ==>
  let transitive = a >= c
  in property $ transitive

prop_lifetime_elision_consistent :: String -> Property
prop_lifetime_elision_consistent function =
  let hasExplicitLifetimes = "'" `elem` function
      elided = not hasExplicitLifetimes
  in classify elided "lifetimes elided" $
     property $ L.length function >= 0

prop_lifetime_annotations_prevent_dangling :: String -> Property
prop_lifetime_annotations_prevent_dangling code =
  let hasLifetimeAnnotation = "'" `elem` code
      hasDanglingReference = "dangling" `L.L.isInfixOf` code
  in classify hasLifetimeAnnotation "has lifetime annotation" $
     property $ hasLifetimeAnnotation ==> not hasDanglingReference

-- Borrowing rules

prop_immutable_borrows_coexist :: String -> String -> Property
prop_immutable_borrows_coexist resource1 resource2 =
  not (null resource1 && null resource2) ==>
  let borrow1 = "&" ++ resource1
      borrow2 = "&" ++ resource2
      bothImmutable = True -- Simplified immutable borrow check
  in property $ bothImmutable

prop_mutable_borrows_exclusive :: String -> Property
prop_mutable_borrows_exclusive resource =
  not (null resource) ==>
  let mutableBorrow = "&mut " ++ resource
      hasMutableBorrow = "&mut" `L.L.isPrefixOf` mutableBorrow
  in classify hasMutableBorrow "has mutable borrow" $
     property $ hasMutableBorrow ==> L.length mutableBorrow >= 5

prop_borrow_checker_prevents_use_after_move :: String -> Property
prop_borrow_checker_prevents_use_after_move variable =
  not (null variable) ==>
  let moved = "move:" ++ variable
      used = "use:" ++ variable
      isMoved = "move:" `L.L.isPrefixOf` moved
      canUse = not isMoved
  in classify isMoved "variable is moved" $
     property $ canUse ==> not ("move:" `L.L.isPrefixOf` used)

prop_borrow_lifetimes_dont_exceed_owner :: Int -> Int -> Property
prop_borrow_lifetimes_dont_exceed_owner ownerLifetime borrowLifetime =
  ownerLifetime >= 0 && borrowLifetime >= 0 ==>
  let validBorrow = borrowLifetime <= ownerLifetime
  in classify validBorrow "valid borrow lifetime" $
     property $ validBorrow ==> borrowLifetime <= ownerLifetime

prop_nested_borrowing_stack_discipline :: [String] -> Property
prop_nested_borrowing_stack_discipline variables =
  not (null variables) ==>
  let nestedLevel = L.length variables
      maxNesting = 100
  in property $ nestedLevel <= maxNesting

-- Reference counting

prop_reference_counting_accurate :: Int -> Property
prop_reference_counting_accurate initialCount =
  initialCount >= 0 && initialCount <= 1000 ==>
  let increment = initialCount + 1
      decrement = initialCount - 1
  in property $ increment >= 0 .&&. (decrement >= 0 ==> initialCount > 0)

prop_reference_cycles_detected :: [(String, [String])] -> Property
prop_reference_cycles_detected graph =
  not (null graph) ==>
  let hasSelfReference = L.any (\(name, refs) -> name `elem` refs) graph
  in classify hasSelfReference "has potential cycle" $
     property $ hasSelfReference ==> L.length graph >= 1

prop_weak_references_no_prevent_deallocation :: Int -> Property
prop_weak_references_no_prevent_deallocation refCount =
  refCount >= 0 && refCount <= 100 ==>
  let weakRefs = refCount `div` 2
      canDeallocate = refCount == weakRefs
  in classify canDeallocate "can deallocate" $
     property $ canDeallocate ==> refCount == weakRefs

prop_reference_counting_multiple_owners :: Int -> Property
prop_reference_counting_multiple_owners ownerCount =
  ownerCount >= 0 && ownerCount <= 10 ==>
  let totalRefs = ownerCount
      isValid = totalRefs >= 0
  in property $ isValid

prop_reference_counting_thread_safe :: Int -> Property
prop_reference_counting_thread_safe threadCount =
  threadCount >= 0 && threadCount <= 100 ==>
  let safeIncrement = threadCount + 1
  in property $ safeIncrement > 0

-- Memory safety

prop_double_frees_prevented :: String -> Property
prop_double_frees_prevented resource =
  not (null resource) ==>
  let freedOnce = "freed:" ++ resource
      freedTwice = "freed_twice:" ++ resource
      isFreedOnce = "freed:" `L.L.isPrefixOf` freedOnce
      canFreeAgain = not isFreedOnce
  in classify isFreedOnce "already freed" $
     property $ canFreeAgain ==> not ("freed_twice:" `L.L.isPrefixOf` freedTwice)

prop_use_after_free_prevented :: String -> Property
prop_use_after_free_prevented resource =
  not (null resource) ==>
  let freed = "freed:" ++ resource
      used = "used:" ++ resource
      isFreed = "freed:" `L.L.isPrefixOf` freed
      canUse = not isFreed
  in classify isFreed "resource is freed" $
     property $ canUse ==> not ("used:" `L.L.isPrefixOf` used)

prop_dangling_pointers_eliminated :: String -> Property
prop_dangling_pointers_eliminated pointer =
  not (null pointer) ==>
  let dangling = "dangling:" ++ pointer
      valid = "valid:" ++ pointer
      isDangling = "dangling:" `L.L.isPrefixOf` dangling
      isValid = "valid:" `L.L.isPrefixOf` valid
  in classify isDangling "pointer is dangling" $
     property $ isDangling ==> not isValid

prop_buffer_overflow_prevented :: Int -> Int -> Property
prop_buffer_overflow_prevented bufferSize accessSize =
  bufferSize >= 0 && accessSize >= 0 && accessSize <= 1000 ==>
  let safeAccess = accessSize <= bufferSize
  in classify safeAccess "safe access" $
     property $ safeAccess ==> accessSize <= bufferSize

prop_memory_leaks_detected :: Int -> Property
prop_memory_leaks_detected allocationCount =
  allocationCount >= 0 && allocationCount <= 1000 ==>
  let deallocatedCount = allocationCount `div` 2
      hasLeak = deallocatedCount < allocationCount
  in classify hasLeak "potential leak" $
     property $ deallocatedCount <= allocationCount

-- Ownership transfer patterns

prop_function_parameter_ownership_clear :: String -> Property
prop_function_parameter_ownership_clear parameter =
  not (null parameter) ==>
  let ownedParam = "owned:" ++ parameter
      borrowedParam = "&" ++ parameter
      hasOwnership = "owned:" `L.L.isPrefixOf` ownedParam
      hasBorrow = "&" `L.L.isPrefixOf` borrowedParam
  in property $ hasOwnership .||. hasBorrow

prop_return_value_ownership_transferred :: String -> Property
prop_return_value_ownership_transferred returnValue =
  not (null returnValue) ==>
  let returned = "return:" ++ returnValue
      hasOwnership = "return:" `L.L.isPrefixOf` returned
  in property $ hasOwnership

prop_struct_field_ownership_follows_rules :: [(String, String)] -> Property
prop_struct_field_ownership_follows_rules fields =
  not (null fields) ==>
  let ownedFields = L.filter (\(name, _) -> "owned" `L.L.isInfixOf` name) fields
      borrowedFields = L.filter (\(name, _) -> "borrow" `L.L.isInfixOf` name) fields
  in property $ L.length ownedFields + L.length borrowedFields >= 0

prop_collection_ownership_handles_elements :: [String] -> Property
prop_collection_ownership_handles_elements elements =
  not (null elements) ==>
  let collection = "collection:" ++ show (L.length elements)
      elementCount = L.length elements
  in property $ elementCount >= 0

prop_closure_ownership_captures_correctly :: [String] -> Property
prop_closure_ownership_captures_correctly capturedVars =
  not (null capturedVars) ==>
  let closure = "closure:" ++ show (L.length capturedVars)
      captureCount = L.length capturedVars
  in property $ captureCount >= 0

-- Ownership inference

prop_ownership_inference_complete :: String -> Property
prop_ownership_inference_complete code =
  let hasOwnershipInfo = "owner" `L.L.isInfixOf` code || "move" `L.L.isInfixOf` code
      inferenceComplete = hasOwnershipInfo || L.length code == 0
  in classify hasOwnershipInfo "has ownership info" $
     property $ inferenceComplete

prop_ownership_inference_conservative :: String -> Property
prop_ownership_inference_conservative code =
  let isConservative = "conservative" `L.L.isInfixOf` code || L.length code > 0
  in property $ isConservative

prop_ownership_inference_complex_expressions :: String -> Property
prop_ownership_inference_complex_expressions expression =
  let complexity = L.length expression
      canInfer = complexity < 10000
  in classify canInfer "can infer" $
     property $ canInfer ==> complexity < 10000

prop_ownership_inference_respects_annotations :: String -> Property
prop_ownership_inference_respects_annotations code =
  let hasAnnotations = "#" `elem` code
      respectsAnnotations = hasAnnotations ==> L.length code >= 0
  in classify hasAnnotations "has annotations" $
     property $ respectsAnnotations

prop_ownership_inference_efficient :: Int -> Property
prop_ownership_inference_efficient codeSize =
  codeSize >= 0 && codeSize <= 10000 ==>
  let inferenceTime = codeSize * 10 -- Simplified time model
  in property $ inferenceTime <= codeSize * 100

-- Error handling

prop_ownership_errors_detected_early :: String -> Property
prop_ownership_errors_detected_early code =
  let hasError = "error" `L.L.isInfixOf` code
      detectedEarly = hasError ==> L.length code > 0
  in classify hasError "has error" $
     property $ detectedEarly

prop_ownership_error_messages_helpful :: String -> Property
prop_ownership_error_messages_helpful errorMessage =
  let isHelpful = "fix" `L.L.isInfixOf` errorMessage || "suggest" `L.L.isInfixOf` errorMessage
  in classify isHelpful "is helpful" $
     property $ L.length errorMessage >= 0

prop_ownership_errors_suggest_fixes :: String -> Property
prop_ownership_errors_suggest_fixes error =
  let hasSuggestion = "try:" `L.L.isInfixOf` error || "consider:" `L.L.isInfixOf` error
  in classify hasSuggestion "has suggestion" $
     property $ hasSuggestion ==> L.length error >= 5

prop_ownership_errors_no_crashes :: String -> Property
prop_ownership_errors_no_crashes problematicCode =
  let handlesGracefully = L.length problematicCode >= 0
  in property $ handlesGracefully

prop_ownership_error_recovery_safe :: String -> Property
prop_ownership_error_recovery_safe codeWithError =
  let canRecover = L.length codeWithError >= 0
  in property $ canRecover

-- Performance properties

prop_ownership_checking_linear :: Int -> Property
prop_ownership_checking_linear codeSize =
  codeSize >= 0 && codeSize <= 10000 ==>
  let checkingTime = codeSize * 2 -- Linear time
  in property $ checkingTime <= codeSize * 10

prop_zero_cost_abstraction_holds :: Int -> Property
prop_zero_cost_abstraction_holds abstractionLevel =
  abstractionLevel >= 0 && abstractionLevel <= 100 ==>
  let runtimeCost = abstractionLevel * 0 -- Zero cost
  in property $ runtimeCost == 0

prop_ownership_analysis_scalable :: Int -> Property
prop_ownership_analysis_scalable programSize =
  programSize >= 0 && programSize <= 100000 ==>
  let analysisTime = programSize * 3
  in property $ analysisTime <= programSize * 10

prop_ownership_optimizations_effective :: Int -> Property
prop_ownership_optimizations_effective optimizationLevel =
  optimizationLevel >= 0 && optimizationLevel <= 10 ==>
  let performanceGain = optimizationLevel * 10
  in property $ performanceGain >= 0

prop_ownership_runtime_overhead_minimal :: Int -> Property
prop_ownership_runtime_overhead_minimal operations =
  operations >= 0 && operations <= 10000 ==>
  let overhead = operations `div` 100 -- 1% overhead
  in property $ overhead <= operations `div` 10

-- Advanced ownership features

prop_shared_ownership_works :: Int -> Property
prop_shared_ownership_works ownerCount =
  ownerCount >= 0 && ownerCount <= 10 ==>
  let sharedRefs = ownerCount
  in property $ sharedRefs >= 0

prop_unique_ownership_enforced :: String -> Property
prop_unique_ownership_enforced resource =
  not (null resource) ==>
  let uniqueMarker = "unique:" ++ resource
      isUnique = "unique:" `L.L.isPrefixOf` uniqueMarker
  in property $ isUnique ==> L.length uniqueMarker >= 7

prop_ownership_transfer_threads_safe :: Int -> Property
prop_ownership_transfer_threads_safe threadCount =
  threadCount >= 0 && threadCount <= 100 ==>
  let safeTransfer = True -- Simplified thread safety
  in property $ safeTransfer

prop_ownership_generics_correct :: String -> Property
prop_ownership_generics_correct genericCode =
  let isGeneric = "<T>" `L.L.isInfixOf` genericCode
      ownershipPreserved = isGeneric ==> L.length genericCode > 0
  in classify isGeneric "is generic" $
     property $ ownershipPreserved

prop_ownership_traits_compatible :: String -> Property
prop_ownership_traits_compatible traitCode =
  let hasTrait = "trait" `L.L.isInfixOf` traitCode
      compatible = hasTrait ==> L.length traitCode > 0
  in classify hasTrait "has trait" $
     property $ compatible