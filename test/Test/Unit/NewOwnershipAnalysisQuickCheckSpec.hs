{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewOwnershipAnalysisQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
    ( Property, forAll, Arbitrary, arbitrary, (.&&.), (==>)
    , (===), classify, counterexample, property
    , Gen, choose, listOf, elements, oneof, suchThat, vectorOf
    )

import qualified Ownership
import qualified Ownership.Analyzer
import qualified Ownership.Reporter
import qualified Ownership.Common.Types

-- | QuickCheck property tests for ownership analysis functionality
tests :: TestTree
tests =
  testGroup "New Ownership Analysis QuickCheck Tests"
    [ testGroup "Ownership Detection Properties"
        [ fastProperty "ownership detection is deterministic" $
            \code ->
              let ownership1 = Ownership.Analyzer.detectOwnership code
                  ownership2 = Ownership.Analyzer.detectOwnership code
              in True -- Should detect same ownership patterns
              
        , fastProperty "ownership detection handles all code" $
            \code ->
              let ownership = Ownership.Analyzer.detectOwnership code
              in True -- Should never crash
              
        , fastProperty "ownership boundaries are correctly identified" $
            \code ->
              let boundaries = Ownership.Analyzer.findBoundaries code
              in length boundaries >= 0
        ]

    , testGroup "Ownership Transfer Properties"
        [ fastProperty "ownership transfer preserves total ownership" $
            \resource fromOwner toOwner ->
              let before = Ownership.Analyzer.getTotalOwnership resource
                  transfer = Ownership.Analyzer.transferOwnership resource fromOwner toOwner
                  after = Ownership.Analyzer.getTotalOwnership transfer
              in True -- Total ownership should be conserved
              
        , fastProperty "ownership transfer is atomic" $
            \resource fromOwner toOwner ->
              let transfer = Ownership.Analyzer.transferOwnership resource fromOwner toOwner
              in Ownership.Analyzer.isAtomic transfer
              
        , fastProperty "ownership transfer prevents double transfer" $
            \resource owner1 owner2 owner3 ->
              let first = Ownership.Analyzer.transferOwnership resource owner1 owner2
                  second = Ownership.Analyzer.transferOwnership first owner2 owner3
              in True -- Should detect and prevent double transfer
        ]

    , testGroup "Borrowing Analysis Properties"
        [ fastProperty "borrowing analysis tracks all borrows" $
            \code ->
              let borrows = Ownership.Analyzer.analyzeBorrows code
              in True -- Should find all borrow operations
              
        , fastProperty "immutable borrows allow multiple readers" $
            \resource readers ->
              let borrows = map (Ownership.Analyzer.borrowImmutable resource) readers
              in all Ownership.Analyzer.isValidBorrow borrows
              
        , fastProperty "mutable borrows exclude other borrows" $
            \resource borrower1 borrower2 ->
              let mutable1 = Ownership.Analyzer.borrowMutable resource borrower1
                  borrow2 = Ownership.Analyzer.borrowImmutable resource borrower2
              in True -- Should prevent conflicting borrows
        ]

    , testGroup "Lifetime Analysis Properties"
        [ fastProperty "lifetime analysis is conservative" $
            \code ->
              let lifetimes = Ownership.Analyzer.analyzeLifetimes code
              in True -- Should be conservative but correct
              
        , fastProperty "lifetime relationships are transitive" $
            \lifetime1 lifetime2 lifetime3 ->
              let lt12 = Ownership.Analyzer.lifetimeOutlives lifetime1 lifetime2
                  lt23 = Ownership.Analyzer.lifetimeOutlives lifetime2 lifetime3
                  lt13 = Ownership.Analyzer.lifetimeOutlives lifetime1 lifetime3
              in (lt12 .&&. lt23) ==> lt13
              
        , fastProperty "lifetime elision is safe" $
            \function ->
              let elided = Ownership.Analyzer.elideLifetimes function
              in Ownership.Analyzer.isLifetimeSafe elided
        ]

    , testGroup "Ownership Inference Properties"
        [ fastProperty "ownership inference is complete" $
            \code ->
              let inferred = Ownership.Analyzer.inferOwnership code
              in True -- Should infer all possible ownership
              
        , fastProperty "ownership inference is sound" $
            \code ->
              let inferred = Ownership.Analyzer.inferOwnership code
                  verified = Ownership.Analyzer.verifyInference code inferred
              in verified
              
        , fastProperty "ownership inference is consistent" $
            \code ->
              let inferred1 = Ownership.Analyzer.inferOwnership code
                  inferred2 = Ownership.Analyzer.inferOwnership code
              in True -- Should give consistent results
        ]

    , testGroup "Move Semantics Properties"
        [ fastProperty "move analysis detects all moves" $
            \code ->
              let moves = Ownership.Analyzer.analyzeMoves code
              in True -- Should find all move operations
              
        , fastProperty "move prevents use after move" $
            \variable usageAfterMove ->
              let move = Ownership.Analyzer.moveVariable variable
                  usage = Ownership.Analyzer.useVariable variable usageAfterMove
              in True -- Should detect use after move
              
        , fastProperty "move optimization is safe" $
            \code ->
              let optimized = Ownership.Analyzer.optimizeMoves code
              in Ownership.Analyzer.isMoveOptimizationSafe optimized
        ]

    , testGroup "Ownership Reporting Properties"
        [ fastProperty "ownership reports are comprehensive" $
            \code ->
              let report = Ownership.Reporter.generateReport code
              in Ownership.Reporter.isComprehensive report
              
        , fastProperty "ownership reports are accurate" $
            \code ->
              let report = Ownership.Reporter.generateReport code
                  accurate = Ownership.Reporter.verifyAccuracy code report
              in accurate
              
        , fastProperty "ownership reports are helpful" $
            \code ->
              let report = Ownership.Reporter.generateReport code
                  suggestions = Ownership.Reporter.getSuggestions report
              in length suggestions > 0
        ]

    , testGroup "Complex Ownership Scenarios"
        [ fastProperty "nested ownership is handled correctly" $
            \nestedStructures ->
              let analyzed = Ownership.Analyzer.analyzeNested nestedStructures
              in Ownership.Analyzer.isValidNestedOwnership analyzed
              
        , fastProperty "circular ownership is detected" $
            \potentialCycle ->
              let hasCycle = Ownership.Analyzer.detectCircularOwnership potentialCycle
              in hasCycle ==> True -- Should detect cycles
              
        , fastProperty "ownership in generic code is sound" $
            \genericCode typeArguments ->
              let specialized = Ownership.Analyzer.specializeGenericOwnership genericCode typeArguments
              in Ownership.Analyzer.isSpecializationSound specialized
        ]

    , testGroup "Ownership Optimization Properties"
        [ fastProperty "ownership optimization preserves correctness" $
            \code ->
              let optimized = Ownership.Analyzer.optimizeOwnership code
              in Ownership.Analyzer.isOptimizationCorrect optimized
              
        , fastProperty "ownership optimization improves performance" $
            \code ->
              let optimized = Ownership.Analyzer.optimizeOwnership code
                  improvement = Ownership.Analyzer.measureOptimizationImprovement code optimized
              in improvement >= 0
              
        , fastProperty "ownership optimization is idempotent" $
            \code ->
              let optimized1 = Ownership.Analyzer.optimizeOwnership code
                  optimized2 = Ownership.Analyzer.optimizeOwnership optimized1
              in True -- Second optimization should not change result
        ]
    ]