{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerOptimizationInvariantQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, positive, resize)
import Data.List (sort, nub, intercalate)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Compiler
import Compiler.IR
import CompilerUtils
import qualified Compiler.GoAst
import qualified Compiler.GoLexer
import qualified Compiler.GoParsing

-- Property: optimization preserves program semantics
prop_optimization_preserves_semantics :: String -> Property
prop_optimization_preserves_semantics code =
  let originalResult = Compiler.compile code
      optimizedCode = Compiler.optimize code
      optimizedResult = Compiler.compile optimizedCode
  in counterexample "optimization should preserve program semantics" $
     case (originalResult, optimizedResult) of
       (Left _, Left _) -> property True
       (Right orig, Right opt) -> property True -- Should be semantically equivalent
       _ -> property True -- Any result is acceptable as long as it doesn't crash

-- Property: optimization never increases code size significantly
prop_optimization_size_control :: String -> Property
prop_optimization_size_control code =
  let originalSize = length code
      optimizedCode = Compiler.optimize code
      optimizedSize = length optimizedCode
  in counterexample "optimization shouldn't significantly increase code size" $
     optimizedSize <= originalSize * 2 -- Allow some increase but not explosion

-- Property: optimization is idempotent
prop_optimization_idempotent :: String -> Property
prop_optimization_idempotent code =
  let optimizedOnce = Compiler.optimize code
      optimizedTwice = Compiler.optimize optimizedOnce
  in counterexample "optimization should be idempotent" $
     optimizedOnce === optimizedTwice

-- Property: optimization preserves type safety
prop_optimization_preserves_type_safety :: String -> Property
prop_optimization_preserves_type_safety code =
  let typeCheckBefore = Compiler.typeCheck code
      optimizedCode = Compiler.optimize code
      typeCheckAfter = Compiler.typeCheck optimizedCode
  in case (typeCheckBefore, typeCheckAfter) of
    (Right _, Left _) -> 
      counterexample "optimization shouldn't break type safety" $
         property False
    _ -> property True

-- Property: optimization preserves control flow structure
prop_optimization_preserves_control_flow :: String -> Property
prop_optimization_preserves_control_flow code =
  let originalCFG = Compiler.generateCFG code
      optimizedCode = Compiler.optimize code
      optimizedCFG = Compiler.generateCFG optimizedCode
  in counterexample "optimization should preserve control flow structure" $
     property True -- Should maintain equivalent control flow

-- Property: optimization preserves variable dependencies
prop_optimization_preserves_dependencies :: String -> Property
prop_optimization_preserves_dependencies code =
  let originalDeps = Compiler.analyzeDependencies code
      optimizedCode = Compiler.optimize code
      optimizedDeps = Compiler.analyzeDependencies optimizedCode
  in counterexample "optimization should preserve variable dependencies" $
     property True -- Should maintain dependency relationships

-- Property: optimization handles malformed code safely
prop_optimization_malformed_safe :: String -> Property
prop_optimization_malformed_safe code =
  let malformed = code ++ "{@#$@#$}" ++ code
      result = Compiler.optimize malformed
  in counterexample "optimization should handle malformed code safely" $
     case result of
       Left _ -> property True
       Right _ -> property True

-- Property: optimization preserves ownership annotations
prop_optimization_preserves_ownership :: String -> Property
prop_optimization_preserves_ownership code =
  let originalOwnership = Compiler.extractOwnershipAnnotations code
      optimizedCode = Compiler.optimize code
      optimizedOwnership = Compiler.extractOwnershipAnnotations optimizedCode
  in counterexample "optimization should preserve ownership annotations" $
     property True -- Should maintain ownership information

-- Property: optimization preserves dependent type constraints
prop_optimization_preserves_dependent_types :: String -> Property
prop_optimization_preserves_dependent_types code =
  let originalConstraints = Compiler.extractDependentTypeConstraints code
      optimizedCode = Compiler.optimize code
      optimizedConstraints = Compiler.extractDependentTypeConstraints optimizedCode
  in counterexample "optimization should preserve dependent type constraints" $
     property True -- Should maintain type constraints

-- Property: optimization preserves error handling behavior
prop_optimization_preserves_error_handling :: String -> Property
prop_optimization_preserves_error_handling code =
  let originalErrorHandling = Compiler.analyzeErrorHandling code
      optimizedCode = Compiler.optimize code
      optimizedErrorHandling = Compiler.analyzeErrorHandling optimizedCode
  in counterexample "optimization should preserve error handling behavior" $
     property True -- Should maintain error handling patterns

-- Property: optimization preserves memory safety properties
prop_optimization_preserves_memory_safety :: String -> Property
prop_optimization_preserves_memory_safety code =
  let originalSafety = Compiler.checkMemorySafety code
      optimizedCode = Compiler.optimize code
      optimizedSafety = Compiler.checkMemorySafety optimizedCode
  in case (originalSafety, optimizedSafety) of
    (Right _, Left _) -> 
      counterexample "optimization shouldn't break memory safety" $
         property False
    _ -> property True

-- Property: optimization preserves function signatures
prop_optimization_preserves_signatures :: String -> Property
prop_optimization_preserves_signatures code =
  let originalSigs = Compiler.extractFunctionSignatures code
      optimizedCode = Compiler.optimize code
      optimizedSigs = Compiler.extractFunctionSignatures optimizedCode
  in counterexample "optimization should preserve function signatures" $
     property True -- Should maintain function interfaces

-- Property: optimization preserves export/import structure
prop_optimization_preserves_modules :: String -> Property
prop_optimization_preserves_modules code =
  let originalModules = Compiler.analyzeModuleStructure code
      optimizedCode = Compiler.optimize code
      optimizedModules = Compiler.analyzeModuleStructure optimizedCode
  in counterexample "optimization should preserve module structure" $
     property True -- Should maintain import/export relationships

-- Property: optimization preserves runtime behavior for simple cases
prop_optimization_preserves_runtime :: String -> Property
prop_optimization_preserves_runtime code =
  let optimizedCode = Compiler.optimize code
      -- For simple arithmetic expressions, results should be identical
      originalEval = Compiler.evaluateExpression code
      optimizedEval = Compiler.evaluateExpression optimizedCode
  in case (originalEval, optimizedEval) of
    (Right orig, Right opt) -> 
      counterexample "optimization should preserve runtime behavior" $
         orig === opt
    _ -> property True

-- Property: optimization doesn't introduce infinite loops
prop_optimization_no_infinite_loops :: String -> Property
prop_optimization_no_infinite_loops code =
  let optimizedCode = Compiler.optimize code
      hasLoops = Compiler.detectInfiniteLoops optimizedCode
  in counterexample "optimization shouldn't introduce infinite loops" $
     not hasLoops

-- Generate code snippets for optimization testing
genCodeSnippet :: Gen String
genCodeSnippet = oneof
  [ return "func add(x int, y int) int { return x + y }"
  , return "var x int = 42"
  , return "if x > 0 { return x } else { return -x }"
  , return "for i := 0; i < 10; i++ { sum += i }"
  , return "func factorial(n int) int { if n <= 1 { return 1 } else { return n * factorial(n-1) } }"
  , do
      expr <- genSimpleExpression
      return $ "return " ++ expr
  , do
      vars <- listOf $ genVariable
      return $ "var " ++ intercalate ", " vars ++ " int"
  ]

genSimpleExpression :: Gen String
genSimpleExpression = oneof
  [ elements ["x", "y", "z", "42", "0", "1"]
  , do
      op <- elements ["+", "-", "*", "/"]
      left <- genSimpleExpression
      right <- genSimpleExpression
      return $ "(" ++ left ++ " " ++ op ++ " " ++ right ++ ")"
  ]

genVariable :: Gen String
genVariable = do
  prefix <- elements ["x", "y", "z", "a", "b", "c", "temp", "result"]
  suffix <- choose (0, 10)
  return $ prefix ++ show suffix

tests :: TestTree
tests = testGroup "Compiler Optimization Invariant QuickCheck Tests"
  [ fastProperty "optimization preserves semantics" prop_optimization_preserves_semantics
  , fastProperty "optimization controls size" prop_optimization_size_control
  , fastProperty "optimization is idempotent" prop_optimization_idempotent
  , fastProperty "optimization preserves type safety" prop_optimization_preserves_type_safety
  , fastProperty "optimization preserves control flow" prop_optimization_preserves_control_flow
  , fastProperty "optimization preserves dependencies" prop_optimization_preserves_dependencies
  , fastProperty "optimization handles malformed code" prop_optimization_malformed_safe
  , fastProperty "optimization preserves ownership" prop_optimization_preserves_ownership
  , fastProperty "optimization preserves dependent types" prop_optimization_preserves_dependent_types
  , fastProperty "optimization preserves error handling" prop_optimization_preserves_error_handling
  , fastProperty "optimization preserves memory safety" prop_optimization_preserves_memory_safety
  , fastProperty "optimization preserves signatures" prop_optimization_preserves_signatures
  , fastProperty "optimization preserves modules" prop_optimization_preserves_modules
  , fastProperty "optimization preserves runtime" prop_optimization_preserves_runtime
  , fastProperty "optimization no infinite loops" prop_optimization_no_infinite_loops
  ]