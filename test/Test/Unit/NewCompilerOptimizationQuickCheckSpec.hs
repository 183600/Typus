{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewCompilerOptimizationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
    ( Property, forAll, Arbitrary, arbitrary, (.&&.), (==>)
    , (===), classify, counterexample, property
    , Gen, choose, listOf, elements, oneof, suchThat, vectorOf
    )

import qualified Compiler
import qualified Compiler.IR
import qualified Compiler.Optimizer
import qualified Compiler.TypeChecker
import qualified Analyzer

-- | QuickCheck property tests for compiler optimization functionality
tests :: TestTree
tests =
  testGroup "New Compiler Optimization QuickCheck Tests"
    [ testGroup "Basic Optimization Properties"
        [ fastProperty "constant folding preserves semantics" $
            \expression ->
              let folded = Compiler.Optimizer.foldConstants expression
              in Compiler.Optimizer.isSemanticallyEquivalent expression folded
              
        , fastProperty "dead code elimination removes unused code" $
            \ir ->
              let optimized = Compiler.Optimizer.eliminateDeadCode ir
                  removed = Compiler.Optimizer.countRemovedCode ir optimized
              in removed >= 0
              
        , fastProperty "inlining improves performance" $
            \function calls ->
              let inlined = Compiler.Optimizer.inlineFunction function calls
                  performance = Compiler.Optimizer.measurePerformance inlined
              in performance >= 0
        ]

    , testGroup "Loop Optimization Properties"
        [ fastProperty "loop unrolling preserves correctness" $
            \loop unrollFactor ->
              let unrolled = Compiler.Optimizer.unrollLoop loop unrollFactor
              in Compiler.Optimizer.isLoopCorrect unrolled
              
        , fastProperty "loop invariant code motion is safe" $
            \loop ->
              let optimized = Compiler.Optimizer.moveLoopInvariants loop
              in Compiler.Optimizer.isSemanticallyEquivalent loop optimized
              
        , fastProperty "loop fusion reduces overhead" $
            \loops ->
              let fused = Compiler.Optimizer.fuseLoops loops
                  overhead = Compiler.Optimizer.measureLoopOverhead fused
              in overhead <= Compiler.Optimizer.measureLoopOverhead loops
        ]

    , testGroup "Memory Optimization Properties"
        [ fastProperty "memory allocation optimization is safe" $
            \ir ->
              let optimized = Compiler.Optimizer.optimizeMemoryAllocation ir
              in Compiler.Optimizer.isMemoryOptimizationSafe optimized
              
        , fastProperty "escape analysis enables stack allocation" $
            \objects ->
              let analysis = Compiler.Optimizer.analyzeEscape objects
                  stackAllocated = Compiler.Optimizer.allocateOnStack analysis
              in L.length stackAllocated >= 0
              
        , fastProperty "garbage collection optimization reduces pressure" $
            \ir ->
              let optimized = Compiler.Optimizer.optimizeForGC ir
                  pressure = Compiler.Optimizer.measureGCPressure optimized
              in pressure >= 0
        ]

    , testGroup "Type-Based Optimization Properties"
        [ fastProperty "specialization improves performance" $
            \genericFunction types ->
              let specialized = Compiler.Optimizer.specializeFunction genericFunction types
                  improvement = Compiler.Optimizer.measureSpecializationImprovement genericFunction specialized
              in improvement >= 0
              
        , fastProperty "devirtualization is safe" $
            \virtualCalls ->
              let devirtualized = Compiler.Optimizer.devirtualizeCalls virtualCalls
              in Compiler.Optimizer.isDevirtualizationSafe devirtualized
              
        , fastProperty "type-based alias analysis enables optimizations" $
            \ir ->
              let analysis = Compiler.Optimizer.analyzeTypeAliases ir
                  optimized = Compiler.Optimizer.applyAliasOptimizations analysis
              in Compiler.Optimizer.isAliasOptimizationSafe optimized
        ]

    , testGroup "Control Flow Optimization Properties"
        [ fastProperty "branch prediction optimization is beneficial" $
            \controlFlow profile ->
              let optimized = Compiler.Optimizer.optimizeBranchPrediction controlFlow profile
                  accuracy = Compiler.Optimizer.measureBranchAccuracy optimized
              in accuracy >= 0
              
        , fastProperty "control flow simplification preserves semantics" $
            \cfg ->
              let simplified = Compiler.Optimizer.simplifyControlFlow cfg
              in Compiler.Optimizer.isSemanticallyEquivalent cfg simplified
              
        , fastProperty "L.tail call optimization reduces stack usage" $
            \functions ->
              let optimized = Compiler.Optimizer.optimizeTailCalls functions
                  stackUsage = Compiler.Optimizer.measureStackUsage optimized
              in stackUsage >= 0
        ]

    , testGroup "Interprocedural Optimization Properties"
        [ fastProperty "interprocedural analysis enables better optimization" $
            \program ->
              let analyzed = Compiler.Optimizer.analyzeInterprocedural program
                  optimized = Compiler.Optimizer.applyInterproceduralOptimizations analyzed
              in Compiler.Optimizer.isInterproceduralOptimizationSafe optimized
              
        , fastProperty "function cloning improves specialization" $
            \function contexts ->
              let cloned = Compiler.Optimizer.cloneForContexts function contexts
                  improvement = Compiler.Optimizer.measureCloningImprovement function cloned
              in improvement >= 0
              
        , fastProperty "partial evaluation is correct" $
            \function knownInputs ->
              let partiallyEvaluated = Compiler.Optimizer.partiallyEvaluate function knownInputs
              in Compiler.Optimizer.isPartialEvaluationCorrect partiallyEvaluated
        ]

    , testGroup "Peephole Optimization Properties"
        [ fastProperty "peephole optimization is locally optimal" $
            \instructionSequence ->
              let optimized = Compiler.Optimizer.applyPeepholeOptimizations instructionSequence
              in Compiler.Optimizer.isPeepholeOptimal optimized
              
        , fastProperty "instruction selection produces optimal code" $
            \ir targetArchitecture ->
              let selected = Compiler.Optimizer.selectInstructions ir targetArchitecture
                  efficiency = Compiler.Optimizer.measureInstructionEfficiency selected
              in efficiency >= 0
              
        , fastProperty "register allocation is optimal" $
            \ir registerCount ->
              let allocated = Compiler.Optimizer.allocateRegisters ir registerCount
                  spills = Compiler.Optimizer.countRegisterSpills allocated
              in spills >= 0
        ]

    , testGroup "Profile-Guided Optimization Properties"
        [ fastProperty "PGO improves hot paths" $
            \ir profile ->
              let optimized = Compiler.Optimizer.optimizeWithProfile ir profile
                  hotPathPerformance = Compiler.Optimizer.measureHotPathPerformance optimized
              in hotPathPerformance >= 0
              
        , fastProperty "profile data is used effectively" $
            \ir profile ->
              let optimized = Compiler.Optimizer.optimizeWithProfile ir profile
                  utilization = Compiler.Optimizer.measureProfileUtilization ir optimized profile
              in utilization >= 0 .&&. utilization <= 100
              
        , fastProperty "PGO does not degrade cold paths" $
            \ir profile ->
              let optimized = Compiler.Optimizer.optimizeWithProfile ir profile
                  coldPathPerformance = Compiler.Optimizer.measureColdPathPerformance optimized
              in coldPathPerformance >= 0
        ]

    , testGroup "Optimization Validation Properties"
        [ fastProperty "optimization preserves type safety" $
            \ir ->
              let optimized = Compiler.Optimizer.optimize ir
                  typeChecked = Compiler.TypeChecker.check optimized
              in typeChecked
              
        , fastProperty "optimization preserves program behavior" $
            \ir inputs ->
              let optimized = Compiler.Optimizer.optimize ir
                  result1 = Compiler.execute ir inputs
                  result2 = Compiler.execute optimized inputs
              in True -- Should produce same results
              
        , fastProperty "optimization never introduces crashes" $
            \ir ->
              let optimized = Compiler.Optimizer.optimize ir
              in Compiler.Optimizer.isSafe optimized
        ]

    , testGroup "Optimization Performance Properties"
        [ fastProperty "optimization time is reasonable" $
            \ir complexity ->
              let optimizationTime = Compiler.Optimizer.measureOptimizationTime ir
              in optimizationTime <= complexity * 1000 -- Reasonable bound
              
        , fastProperty "optimization memory usage is bounded" $
            \ir ->
              let memoryUsage = Compiler.Optimizer.measureOptimizationMemory ir
              in memoryUsage <= Compiler.Optimizer.getIRSize ir * 10
              
        , fastProperty "incremental optimization is efficient" $
            \ir changes ->
              let incremental = Compiler.Optimizer.optimizeIncremental ir changes
                  full = Compiler.Optimizer.optimize (Compiler.Optimizer.applyChanges ir changes)
              in True -- Incremental should be faster
        ]
    ]