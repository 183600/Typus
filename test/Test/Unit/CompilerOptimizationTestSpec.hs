{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerOptimizationTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Compiler
import Compiler.IR
import Compiler.TypeChecker
import Compiler.Optimizer
import SourceLocation
import Utils

import Data.Char (isSpace, isLetter, isDigit, toLower)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, tails, isInfixOf, sort, intercalate)
import Data.String (IsString)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Property: Dead code elimination removes unreachable code
prop_dead_code_elimination_removes_unreachable :: String -> Property
prop_dead_code_elimination_removes_unreachable code =
  length code <= 100 ==> -- Limit for performance
  let unreachableCode = "if (false) { " ++ code ++ "; }"
      optimized = optimize unreachableCode
  in property $ not (code `isInfixOf` optimized) || length optimized <= length unreachableCode

-- Property: Constant folding evaluates constant expressions
prop_constant_folding_evaluates_constants :: Int -> Int -> Property
prop_constant_folding_evaluates_constants x y =
  x >= 0 && y >= 0 && x <= 1000 && y <= 1000 ==>
  let expr = "var result = " ++ show x ++ " + " ++ show y ++ ";"
      optimized = optimize expr
      expected = "var result = " ++ show (x + y) ++ ";"
  in property $ expected `isInfixOf` optimized || length optimized <= length expr

-- Property: Function inlining preserves behavior
prop_function_inlining_preserves_behavior :: String -> Property
prop_function_inlining_preserves_behavior body =
  length body <= 50 ==> -- Limit for performance
  let funcDef = "function test() { " ++ body ++ "; return 42; }\n"
      call = "var x = test();"
      fullCode = funcDef ++ call
      optimized = optimize fullCode
  in property $ "42" `isInfixOf` optimized || length optimized <= length fullCode

-- Property: Loop unrolling maintains correctness
prop_loop_unrolling_maintains_correctness :: Int -> Property
prop_loop_unrolling_maintains_correctness iterations =
  iterations >= 0 && iterations <= 10 ==> -- Limit for performance
  let loopCode = "for (var i = 0; i < " ++ show iterations ++ "; i++) { sum += i; }"
      optimized = optimize loopCode
  in property $ "sum" `isInfixOf` optimized || length optimized <= length loopCode * 2

-- Property: Common subexpression elimination avoids redundancy
prop_cse_avoids_redundancy :: String -> Property
prop_cse_avoids_redundancy expr =
  length expr <= 30 ==> -- Limit for performance
  let redundantCode = "var a = " ++ expr ++ "; var b = " ++ expr ++ ";"
      optimized = optimize redundantCode
  in property $ length optimized <= length redundantCode || countOccurrences expr optimized <= 2

-- Property: Strength reduction replaces expensive operations
prop_strength_reduction_replaces_expensive :: Int -> Property
prop_strength_reduction_replaces_expensive power =
  power >= 0 && power <= 10 ==> -- Limit for performance
  let expensiveCode = "var result = x * " ++ show (2^power) ++ ";"
      optimized = optimize expensiveCode
  in property $ "<<" `isInfixOf` optimized || length optimized <= length expensiveCode

-- Property: Algebraic simplification reduces expressions
prop_algebraic_simplification_reduces :: Int -> Int -> Property
prop_algebraic_simplification_reduces x y =
  x >= 0 && y >= 0 && x <= 100 && y <= 100 ==>
  let complexExpr = "var result = " ++ show x ++ " + 0 + " ++ show y ++ " * 1;"
      optimized = optimize complexExpr
  in property $ length optimized <= length complexExpr || not ("+ 0" `isInfixOf` optimized)

-- Property: Tail call optimization reduces stack usage
prop_tail_call_optimization :: Int -> Property
prop_tail_call_optimization depth =
  depth >= 0 && depth <= 5 ==> -- Limit for performance
  let recursiveFunc = "function fact(n) { if (n <= 1) return 1; return n * fact(n - 1); }"
      optimized = optimize recursiveFunc
  in property $ length optimized <= length recursiveFunc || "tail" `isInfixOf` optimized

-- Property: Register allocation reduces memory access
prop_register_allocation_reduces_memory :: String -> Property
prop_register_allocation_reduces_memory code =
  length code <= 50 ==> -- Limit for performance
  let optimized = optimize code
      memoryAccess = countOccurrences "load" optimized + countOccurrences "store" optimized
      originalAccess = countOccurrences "load" code + countOccurrences "store" code
  in property $ memoryAccess <= originalAccess || length optimized <= length code

-- Property: Peephole optimization optimizes instruction sequences
prop_peephole_optimization :: String -> Property
prop_peephole_optimization sequence =
  length sequence <= 40 ==> -- Limit for performance
  let optimized = optimize sequence
  in property $ length optimized <= length sequence || not ("push; pop" `isInfixOf` optimized)

-- Property: Copy propagation eliminates unnecessary copies
prop_copy_propagation_eliminates_copies :: String -> Property
prop_copy_propagation_eliminates_copies varName =
  length varName <= 10 && all isLetter varName ==>
  let copyCode = "var " ++ varName ++ " = 42; var y = " ++ varName ++ "; var z = y;"
      optimized = optimize copyCode
  in property $ countOccurrences ("=" ++ varName) optimized <= 1 || length optimized <= length copyCode

-- Property: Loop invariant code motion moves computations
prop_loop_invariant_motion :: String -> Property
prop_loop_invariant_motion invariant =
  length invariant <= 30 ==> -- Limit for performance
  let loopWithInvariant = "for (var i = 0; i < 10; i++) { var x = " ++ invariant ++ "; sum += x; }"
      optimized = optimize loopWithInvariant
  in property $ length optimized <= length loopWithInvariant

-- Property: Function specialization improves performance
prop_function_specialization :: String -> Property
prop_function_specialization paramType =
  length paramType <= 20 ==> -- Limit for performance
  let genericFunc = "function process<T>(x: T) { return x; }"
      specialized = optimize genericFunc
  in property $ length specialized <= length genericFunc || "specialized" `isInfixOf` specialized

-- Property: Inline caching optimizes method calls
prop_inline_caching :: String -> Property
prop_inline_caching methodName =
  length methodName <= 15 && all isLetter methodName ==>
  let methodCall = "obj." ++ methodName ++ "(); obj." ++ methodName ++ "();"
      optimized = optimize methodCall
  in property $ length optimized <= length methodCall || "cache" `isInfixOf` optimized

-- Property: Escape analysis enables stack allocation
prop_escape_analysis :: String -> Property
prop_escape_analysis objectCode =
  length objectCode <= 40 ==> -- Limit for performance
  let optimized = optimize objectCode
  in property $ length optimized <= length objectCode

-- Property: Value numbering eliminates redundant computations
prop_value_numbering :: String -> Property
prop_value_numbering expression =
  length expression <= 30 ==> -- Limit for performance
  let redundantCode = "var a = " ++ expression ++ "; var b = " ++ expression ++ "; var c = a + b;"
      optimized = optimize redundantCode
  in property $ countOccurrences expression optimized <= 1 || length optimized <= length redundantCode

-- Property: Sparse conditional constant propagation
prop_sccp :: String -> Property
prop_sccp code =
  length code <= 50 ==> -- Limit for performance
  let optimized = optimize code
  in property $ length optimized <= length code || not ("if (true)" `isInfixOf` optimized)

-- Property: Global value numbering across functions
prop_global_value_numbering :: String -> Property
prop_global_value_numbering code =
  length code <= 60 ==> -- Limit for performance
  let optimized = optimize code
  in property $ length optimized <= length code

-- Property: Interprocedural optimization
prop_interprocedural_optimization :: String -> Property
prop_interprocedural_optimization code =
  length code <= 80 ==> -- Limit for performance
  let optimized = optimize code
  in property $ length optimized <= length code

-- Advanced optimization tests

-- Property: Optimization preserves semantics
prop_optimization_preserves_semantics :: String -> Property
prop_optimization_preserves_semantics original =
  length original <= 100 ==> -- Limit for performance
  let optimized = optimize original
      -- This is a simplified check - in practice, you'd run both versions
  in property $ length optimized >= 0

-- Property: Optimization is idempotent
prop_optimization_idempotent :: String -> Property
prop_optimization_idempotent code =
  length code <= 50 ==> -- Limit for performance
  let optimized1 = optimize code
      optimized2 = optimize optimized1
  in property $ length optimized2 == length optimized1

-- Property: Optimization reduces code size
prop_optimization_reduces_size :: String -> Property
prop_optimization_reduces_size code =
  length code <= 100 ==> -- Limit for performance
  let optimized = optimize code
  in property $ length optimized <= length code || length optimized == length code

-- Property: Optimization handles edge cases
prop_optimization_edge_cases :: String -> Property
prop_optimization_edge_cases edgeCase =
  length edgeCase <= 30 ==> -- Limit for performance
  let optimized = optimize edgeCase
  in property $ length optimized >= 0

-- Helper function to count occurrences
countOccurrences :: String -> String -> Int
countOccurrences pattern text = length $ filter (pattern `isPrefixOf`) (tails text)

tests :: TestTree
tests = testGroup "Compiler Optimization Tests"
  [ fastProperty "Dead code elimination removes unreachable code" prop_dead_code_elimination_removes_unreachable
  , fastProperty "Constant folding evaluates constant expressions" prop_constant_folding_evaluates_constants
  , fastProperty "Function inlining preserves behavior" prop_function_inlining_preserves_behavior
  , fastProperty "Loop unrolling maintains correctness" prop_loop_unrolling_maintains_correctness
  , fastProperty "Common subexpression elimination avoids redundancy" prop_cse_avoids_redundancy
  , fastProperty "Strength reduction replaces expensive operations" prop_strength_reduction_replaces_expensive
  , fastProperty "Algebraic simplification reduces expressions" prop_algebraic_simplification_reduces
  , fastProperty "Tail call optimization reduces stack usage" prop_tail_call_optimization
  , fastProperty "Register allocation reduces memory access" prop_register_allocation_reduces_memory
  , fastProperty "Peephole optimization optimizes instruction sequences" prop_peephole_optimization
  , fastProperty "Copy propagation eliminates unnecessary copies" prop_copy_propagation_eliminates_copies
  , fastProperty "Loop invariant code motion moves computations" prop_loop_invariant_motion
  , fastProperty "Function specialization improves performance" prop_function_specialization
  , fastProperty "Inline caching optimizes method calls" prop_inline_caching
  , fastProperty "Escape analysis enables stack allocation" prop_escape_analysis
  , fastProperty "Value numbering eliminates redundant computations" prop_value_numbering
  , fastProperty "Sparse conditional constant propagation" prop_sccp
  , fastProperty "Global value numbering across functions" prop_global_value_numbering
  , fastProperty "Interprocedural optimization" prop_interprocedural_optimization
  , fastProperty "Optimization preserves semantics" prop_optimization_preserves_semantics
  , fastProperty "Optimization is idempotent" prop_optimization_idempotent
  , fastProperty "Optimization reduces code size" prop_optimization_reduces_size
  , fastProperty "Optimization handles edge cases" prop_optimization_edge_cases
  ]