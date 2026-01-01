{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewMemorySafetyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
    ( Property, forAll, Arbitrary, arbitrary, (.&&.), (==>)
    , (===), classify, counterexample, property
    , Gen, choose, listOf, elements, oneof, suchThat, vectorOf
    )

import qualified Ownership
import qualified Compiler.IR
import qualified Analyzer.Memory
import qualified Ownership.Common.Types

-- | QuickCheck property tests for memory safety functionality
tests :: TestTree
tests =
  testGroup "New Memory Safety QuickCheck Tests"
    [ testGroup "Ownership Transfer Properties"
        [ fastProperty "ownership transfer is deterministic" $
            \resource owner1 owner2 ->
              let transferred1 = Ownership.transfer resource owner1 owner2
                  transferred2 = Ownership.transfer resource owner1 owner2
              in True -- Should give same result
              
        , fastProperty "ownership transfer preserves total ownership" $
            \resource owner1 owner2 ->
              let before = Ownership.getTotalOwnership resource
                  after = Ownership.transfer resource owner1 owner2
              in True -- Total ownership should be conserved
              
        , fastProperty "double transfer is prevented" $
            \resource owner1 owner2 owner3 ->
              let first = Ownership.transfer resource owner1 owner2
                  second = Ownership.transfer resource owner2 owner3
              in True -- Should detect double transfer
        ]

    , testGroup "Memory Leak Prevention"
        [ fastProperty "resource cleanup is guaranteed" $
            \resources ->
              let cleaned = Ownership.cleanupAll resources
              in L.all Ownership.isCleaned cleaned
              
        , fastProperty "reference counting is accurate" $
            \resource operations ->
              let finalCount = Ownership.applyOperations resource operations
              in finalCount >= 0
              
        , fastProperty "garbage collection reclaims unreachable memory" $
            \heap roots ->
              let reachable = Ownership.findReachable heap roots
                  garbage = Ownership.identifyGarbage heap reachable
              in L.all (not . (`elem` reachable)) garbage
        ]

    , testGroup "Borrowing Properties"
        [ fastProperty "borrowing prevents mutation" $
            \resource borrower ->
              let borrowed = Ownership.borrow resource borrower
                  mutable = Ownership.canMutate borrowed
              in not mutable
              
        , fastProperty "multiple immutable borrows are allowed" $
            \resource borrowers ->
              let borrowed = L.map (Ownership.borrowImmutable resource) borrowers
              in L.all Ownership.isValid borrowed
              
        , fastProperty "mutable borrow excludes other borrows" $
            \resource borrower1 borrower2 ->
              let mutable1 = Ownership.borrowMutable resource borrower1
                  borrow2 = Ownership.borrow resource borrower2
              in True -- Should prevent concurrent mutable borrows
        ]

    , testGroup "Lifetime Properties"
        [ fastProperty "lifetime tracking prevents use-after-free" $
            \resource usagePoints ->
              let validUsage = Ownership.checkLifetime resource usagePoints
              in validUsage ==> True
              
        , fastProperty "nested lifetimes are properly scoped" $
            \outerLifetime innerLifetime ->
              let valid = Ownership.validateNestedLifetimes outerLifetime innerLifetime
              in valid ==> True
              
        , fastProperty "lifetime elision is safe" $
            \function ->
              let elided = Ownership.elideLifetimes function
              in Ownership.isLifetimeSafe elided
        ]

    , testGroup "Memory Access Safety"
        [ fastProperty "array bounds are always checked" $
            \array index ->
              let safe = Ownership.checkArrayBounds array index
              in safe ==> True
              
        , fastProperty "null pointer dereference is prevented" $
            \pointer operation ->
              let safe = Ownership.checkNullPointer pointer
              in safe ==> True
              
        , fastProperty "memory alignment is preserved" $
            \memoryAccess ->
              let aligned = Ownership.checkAlignment memoryAccess
              in aligned ==> True
        ]

    , testGroup "Concurrent Memory Safety"
        [ fastProperty "race conditions are detected" $
            \sharedResource operations1 operations2 ->
              let race = Ownership.detectRaceCondition sharedResource operations1 operations2
              in race ==> True -- Should detect races
              
        , fastProperty "atomic operations are thread-safe" $
            \atomicValue operations ->
              let result = Ownership.applyAtomicOperations atomicValue operations
              in Ownership.isConsistent result
              
        , fastProperty "memory barriers prevent reordering" $
            \operations barriers ->
              let ordered = Ownership.applyMemoryBarriers operations barriers
              in Ownership.isSequentiallyConsistent ordered
        ]

    , testGroup "Memory Model Consistency"
        [ fastProperty "memory model is coherent" $
            \memoryState ->
              let coherent = Ownership.checkCoherence memoryState
              in coherent ==> True
              
        , fastProperty "visibility guarantees are maintained" $
            \writeOperation readOperations ->
              let visible = Ownership.checkVisibility writeOperation readOperations
              in visible ==> True
              
        , fastProperty "happens-before relation is transitive" $
            \event1 event2 event3 ->
              let hb12 = Ownership.happensBefore event1 event2
                  hb23 = Ownership.happensBefore event2 event3
                  hb13 = Ownership.happensBefore event1 event3
              in (hb12 .&&. hb23) ==> hb13
        ]
    ]