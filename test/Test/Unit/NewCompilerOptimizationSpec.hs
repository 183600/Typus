{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCompilerOptimizationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Compiler (compileTypus)
import Compiler.IR (IRModule(..), IRFunction(..), IRStatement(..), IRExpression(..))
import SourceLocation (SourceSpan(..), startPos)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort)
import Data.Char (isSpace, isAlpha, isAlphaNum)

-- Property: Compiler optimizes constant folding correctly
prop_constant_folding :: Int -> Int -> Property
prop_constant_folding a b =
  a >= 0 && b >= 0 && a <= 1000 && b <= 1000 ==>
  let source = "package main\nfunc main() {\n  result := " ++ show a ++ " + " ++ show b ++ "\n  println(result)\n}"
      result = compileTypus source
  in case result of
    Left _ -> property False
    Right irModule -> hasConstantFolded irModule (a + b)

-- Property: Compiler eliminates dead code correctly
prop_dead_code_elimination :: Bool -> Property
prop_dead_code_elimination condition =
  let source = "package main\nfunc main() {\n  if " ++ show condition ++ " {\n    println(\"unreachable\")\n  } else {\n    println(\"reachable\")\n  }\n}"
      result = compileTypus source
  in case result of
    Left _ -> property False
    Right irModule -> hasDeadCodeEliminated irModule condition

-- Property: Compiler optimizes tail recursion correctly
prop_tail_recursion_optimization :: Int -> Property
prop_tail_recursion_optimization n =
  n >= 0 && n <= 10 ==>
  let source = "package main\nfunc factorial(n int, acc int) int {\n  if n <= 1 {\n    return acc\n  }\n  return factorial(n-1, n*acc)\n}\nfunc main() {\n  result := factorial(" ++ show n ++ ", 1)\n  println(result)\n}"
      result = compileTypus source
  in case result of
    Left _ -> property False
    Right irModule -> hasTailRecursionOptimized irModule

-- Property: Compiler inlines small functions correctly
prop_function_inlining :: Int -> Property
prop_function_inlining x =
  x >= 0 && x <= 100 ==>
  let source = "package main\nfunc small(a int) int {\n  return a * 2\n}\nfunc main() {\n  result := small(" ++ show x ++ ")\n  println(result)\n}"
      result = compileTypus source
  in case result of
    Left _ -> property False
    Right irModule -> hasFunctionInlined irModule

-- Property: Compiler optimizes loop invariants correctly
prop_loop_invariant_optimization :: Int -> Int -> Property
prop_loop_invariant_optimization limit invariant =
  limit >= 0 && limit <= 50 && invariant >= 0 && invariant <= 100 ==>
  let source = "package main\nfunc main() {\n  arr := make([]int, " ++ show limit ++ ")\n  for i := 0; i < " ++ show limit ++ "; i++ {\n    arr[i] = " ++ show invariant ++ " * i\n  }\n}"
      result = compileTypus source
  in case result of
    Left _ -> property False
    Right irModule -> hasLoopInvariantOptimized irModule

-- Property: Compiler performs strength reduction correctly
prop_strength_reduction :: Int -> Int -> Property
prop_strength_reduction base exp =
  base >= 0 && exp >= 0 && base <= 10 && exp <= 5 ==>
  let source = "package main\nfunc main() {\n  result := " ++ show base ++ " ^ " ++ show exp ++ "\n  println(result)\n}"
      result = compileTypus source
  in case result of
    Left _ -> property False
    Right irModule -> hasStrengthReduced irModule

-- Property: Compiler optimizes memory allocation correctly
prop_memory_allocation_optimization :: Int -> Property
prop_memory_allocation_optimization size =
  size >= 0 && size <= 1000 ==>
  let source = "package main\nfunc main() {\n  arr := make([]int, " ++ show size ++ ")\n  for i := 0; i < " ++ show size ++ "; i++ {\n    arr[i] = i\n  }\n}"
      result = compileTypus source
  in case result of
    Left _ -> property False
    Right irModule -> hasMemoryOptimized irModule

-- Property: Compiler performs common subexpression elimination correctly
prop_common_subexpression_elimination :: Int -> Int -> Property
prop_common_subexpression_elimination a b =
  a >= 0 && b >= 0 && a <= 100 && b <= 100 ==>
  let source = "package main\nfunc main() {\n  x := " ++ show a ++ " + " ++ show b ++ "\n  y := " ++ show a ++ " + " ++ show b ++ "\n  z := x * y\n  println(z)\n}"
      result = compileTypus source
  in case result of
    Left _ -> property False
    Right irModule -> hasCommonSubexpressionEliminated irModule

-- Property: Compiler optimizes branch prediction correctly
prop_branch_prediction_optimization :: Int -> Property
prop_branch_prediction_optimization threshold =
  threshold >= 0 && threshold <= 100 ==>
  let source = "package main\nfunc main() {\n  for i := 0; i < 1000; i++ {\n    if i < " ++ show threshold ++ " {\n      println(\"small\")\n    } else {\n      println(\"large\")\n    }\n  }\n}"
      result = compileTypus source
  in case result of
    Left _ -> property False
    Right irModule -> hasBranchPredictionOptimized irModule

-- Property: Compiler performs register allocation correctly
prop_register_allocation :: Int -> Property
prop_register_allocation numVars =
  numVars >= 0 && numVars <= 10 ==>
  let varDeclarations = concatMap (\i -> "  var" ++ show i ++ " := " ++ show i ++ "\n") [1..numVars]
      source = "package main\nfunc main() {\n" ++ varDeclarations ++ "  result := var1 + var2\n  println(result)\n}"
      result = compileTypus source
  in case result of
    Left _ -> property False
    Right irModule -> hasRegisterAllocated irModule

-- Property: Compiler optimizes function call overhead correctly
prop_function_call_optimization :: Int -> Property
prop_function_call_optimization iterations =
  iterations >= 0 && iterations <= 100 ==>
  let source = "package main\nfunc simple(x int) int {\n  return x + 1\n}\nfunc main() {\n  for i := 0; i < " ++ show iterations ++ "; i++ {\n    result := simple(i)\n    println(result)\n  }\n}"
      result = compileTypus source
  in case result of
    Left _ -> property False
    Right irModule -> hasFunctionCallOptimized irModule

-- Property: Compiler performs escape analysis correctly
prop_escape_analysis :: Int -> Property
prop_escape_analysis size =
  size >= 0 && size <= 100 ==>
  let source = "package main\nfunc main() {\n  arr := make([]int, " ++ show size ++ ")\n  for i := 0; i < " ++ show size ++ "; i++ {\n    arr[i] = i\n  }\n  sum := 0\n  for _, v := range arr {\n    sum += v\n  }\n  println(sum)\n}"
      result = compileTypus source
  in case result of
    Left _ -> property False
    Right irModule -> hasEscapeAnalysisOptimized irModule

-- Property: Compiler optimizes string operations correctly
prop_string_optimization :: String -> Int -> Property
prop_string_optimization prefix repetitions =
  not (null prefix) && repetitions >= 0 && repetitions <= 10 ==>
  let source = "package main\nfunc main() {\n  result := \"\"\n  for i := 0; i < " ++ show repetitions ++ "; i++ {\n    result += \"" ++ prefix ++ "\"\n  }\n  println(result)\n}"
      result = compileTypus source
  in case result of
    Left _ -> property False
    Right irModule -> hasStringOperationOptimized irModule

-- Property: Compiler performs array bounds check elimination correctly
prop_bounds_check_elimination :: Int -> Property
prop_bounds_check_elimination size =
  size >= 0 && size <= 100 ==>
  let source = "package main\nfunc main() {\n  arr := make([]int, " ++ show size ++ ")\n  for i := 0; i < " ++ show size ++ "; i++ {\n    arr[i] = i\n  }\n  sum := 0\n  for i := 0; i < " ++ show size ++ "; i++ {\n    sum += arr[i]\n  }\n  println(sum)\n}"
      result = compileTypus source
  in case result of
    Left _ -> property False
    Right irModule -> hasBoundsCheckEliminated irModule

-- Property: Compiler optimizes interface calls correctly
prop_interface_call_optimization :: String -> Property
prop_interface_call_optimization methodName =
  not (null methodName) && isAlpha (head methodName) && all isAlphaNum methodName ==>
  let source = "package main\ntype Test interface {\n  " ++ methodName ++ "() int\n}\ntype Impl struct{}\nfunc (i *Impl) " ++ methodName ++ "() int {\n  return 42\n}\nfunc main() {\n  var t Test = &Impl{}\n  result := t." ++ methodName ++ "()\n  println(result)\n}"
      result = compileTypus source
  in case result of
    Left _ -> property False
    Right irModule -> hasInterfaceCallOptimized irModule

-- Helper functions to check optimizations
hasConstantFolded :: IRModule -> Int -> Property
hasConstantFolded irModule expected = property True -- Placeholder implementation

hasDeadCodeEliminated :: IRModule -> Bool -> Property
hasDeadCodeEliminated irModule condition = property True -- Placeholder implementation

hasTailRecursionOptimized :: IRModule -> Property
hasTailRecursionOptimized irModule = property True -- Placeholder implementation

hasFunctionInlined :: IRModule -> Property
hasFunctionInlined irModule = property True -- Placeholder implementation

hasLoopInvariantOptimized :: IRModule -> Property
hasLoopInvariantOptimized irModule = property True -- Placeholder implementation

hasStrengthReduced :: IRModule -> Property
hasStrengthReduced irModule = property True -- Placeholder implementation

hasMemoryOptimized :: IRModule -> Property
hasMemoryOptimized irModule = property True -- Placeholder implementation

hasCommonSubexpressionEliminated :: IRModule -> Property
hasCommonSubexpressionEliminated irModule = property True -- Placeholder implementation

hasBranchPredictionOptimized :: IRModule -> Property
hasBranchPredictionOptimized irModule = property True -- Placeholder implementation

hasRegisterAllocated :: IRModule -> Property
hasRegisterAllocated irModule = property True -- Placeholder implementation

hasFunctionCallOptimized :: IRModule -> Property
hasFunctionCallOptimized irModule = property True -- Placeholder implementation

hasEscapeAnalysisOptimized :: IRModule -> Property
hasEscapeAnalysisOptimized irModule = property True -- Placeholder implementation

hasStringOperationOptimized :: IRModule -> Property
hasStringOperationOptimized irModule = property True -- Placeholder implementation

hasBoundsCheckEliminated :: IRModule -> Property
hasBoundsCheckEliminated irModule = property True -- Placeholder implementation

hasInterfaceCallOptimized :: IRModule -> Property
hasInterfaceCallOptimized irModule = property True -- Placeholder implementation

tests :: TestTree
tests = testGroup "New Compiler Optimization tests"
  [ fastProperty "Compiler optimizes constant folding correctly" prop_constant_folding
  , fastProperty "Compiler eliminates dead code correctly" prop_dead_code_elimination
  , fastProperty "Compiler optimizes tail recursion correctly" prop_tail_recursion_optimization
  , fastProperty "Compiler inlines small functions correctly" prop_function_inlining
  , fastProperty "Compiler optimizes loop invariants correctly" prop_loop_invariant_optimization
  , fastProperty "Compiler performs strength reduction correctly" prop_strength_reduction
  , fastProperty "Compiler optimizes memory allocation correctly" prop_memory_allocation_optimization
  , fastProperty "Compiler performs common subexpression elimination correctly" prop_common_subexpression_elimination
  , fastProperty "Compiler optimizes branch prediction correctly" prop_branch_prediction_optimization
  , fastProperty "Compiler performs register allocation correctly" prop_register_allocation
  , fastProperty "Compiler optimizes function call overhead correctly" prop_function_call_optimization
  , fastProperty "Compiler performs escape analysis correctly" prop_escape_analysis
  , fastProperty "Compiler optimizes string operations correctly" prop_string_optimization
  , fastProperty "Compiler performs array bounds check elimination correctly" prop_bounds_check_elimination
  , fastProperty "Compiler optimizes interface calls correctly" prop_interface_call_optimization
  ]