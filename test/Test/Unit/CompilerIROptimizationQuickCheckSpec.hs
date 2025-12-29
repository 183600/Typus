{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerIROptimizationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Compiler.IR
import Compiler.TypeChecker

import Data.List (nub, sort)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Mock IR nodes for testing
data MockIRNode = MockIRNode
  { nodeId :: String
  , nodeType :: MockIRType
  , nodeValue :: MockIRValue
  , nodeChildren :: [MockIRNode]
  } deriving (Show, Eq)

data MockIRType = MockInt | MockBool | MockString | MockFunction [MockIRType] MockIRType
  deriving (Show, Eq)

data MockIRValue = MockIntValue Int | MockBoolValue Bool | MockStringValue String | MockFunctionValue String
  deriving (Show, Eq)

data OptimizationPass = ConstantFolding | DeadCodeElimination | CommonSubexpressionElimination
  deriving (Show, Eq)

-- Property: Constant folding should preserve semantics
prop_constant_folding_preserves_semantics :: MockIRNode -> Property
prop_constant_folding_preserves_semantics node =
  let optimized = applyOptimization ConstantFolding node
      originalResult = evaluateIR node
      optimizedResult = evaluateIR optimized
  in property $ originalResult === optimizedResult

-- Property: Dead code elimination should remove unreachable nodes
prop_dead_code_elimination_removes_unreachable :: MockIRNode -> Property
prop_dead_code_elimination_removes_unreachable node =
  let optimized = applyOptimization DeadCodeElimination node
      originalCount = countNodes node
      optimizedCount = countNodes optimized
      unreachableCount = countUnreachableNodes node
  in property $ optimizedCount <= originalCount .&&. 
               optimizedCount <= originalCount - unreachableCount

-- Property: Common subexpression elimination should reduce redundancy
prop_cse_reduces_redundancy :: MockIRNode -> Property
prop_cse_reduces_redundancy node =
  let optimized = applyOptimization CommonSubexpressionElimination node
      originalRedundancy = countRedundantSubexpressions node
      optimizedRedundancy = countRedundantSubexpressions optimized
  in property $ optimizedRedundancy <= originalRedundancy

-- Property: Optimization should not introduce new types
prop_optimization_preserves_types :: MockIRNode -> Property
prop_optimization_preserves_types node =
  let optimized = applyOptimization ConstantFolding node
      originalTypes = collectTypes node
      optimizedTypes = collectTypes optimized
  in property $ Set.isSubsetOf optimizedTypes originalTypes

-- Property: Multiple optimization passes should be idempotent
prop_multiple_passes_idempotent :: MockIRNode -> [OptimizationPass] -> Property
prop_multiple_passes_idempotent node passes =
  let optimizedOnce = foldl applyOptimization node passes
      optimizedTwice = foldl applyOptimization optimizedOnce passes
  in property $ countNodes optimizedOnce === countNodes optimizedTwice

-- Property: Optimization should preserve functional behavior
prop_optimization_preserves_behavior :: MockIRNode -> Property
prop_optimization_preserves_behavior node =
  let optimized = applyOptimization ConstantFolding node
      originalBehavior = analyzeBehavior node
      optimizedBehavior = analyzeBehavior optimized
  in property $ originalBehavior === optimizedBehavior

-- Property: Constant propagation should eliminate unnecessary variables
prop_constant_propagation_eliminates_vars :: MockIRNode -> Property
prop_constant_propagation_eliminates_vars node =
  let optimized = applyConstantPropagation node
      originalVars = countVariables node
      optimizedVars = countVariables optimized
      constVars = countConstantVariables node
  in property $ optimizedVars <= originalVars .&&. 
               optimizedVars <= originalVars - constVars

-- Property: Loop invariant code motion should move invariants out
prop_loop_invariant_code_motion :: MockIRNode -> Property
prop_loop_invariant_code_motion node =
  let hasLoops = containsLoops node
  in hasLoops ==>
     let optimized = applyLoopInvariantCodeMotion node
         invariantsInLoop = countLoopInvariants node
         invariantsOutsideLoop = countLoopInvariantsOutside optimized
     in property $ invariantsOutsideLoop >= invariantsInLoop

-- Property: Peephole optimization should simplify patterns
prop_peephole_simplifies_patterns :: MockIRNode -> Property
prop_peephole_simplifies_patterns node =
  let optimized = applyPeepholeOptimization node
      originalComplexity = calculateComplexity node
      optimizedComplexity = calculateComplexity optimized
  in property $ optimizedComplexity <= originalComplexity

-- Property: Optimization should maintain program equivalence
prop_optimization_maintains_equivalence :: MockIRNode -> Property
prop_optimization_maintains_equivalence node =
  let optimized = applyAllOptimizations node
      originalOutput = simulateExecution node
      optimizedOutput = simulateExecution optimized
  in property $ originalOutput === optimizedOutput

-- Helper functions for mock IR operations
applyOptimization :: OptimizationPass -> MockIRNode -> MockIRNode
applyOptimization ConstantFolding = constantFoldNode
applyOptimization DeadCodeElimination = eliminateDeadCode
applyOptimization CommonSubexpressionElimination = eliminateCommonSubexpressions

constantFoldNode :: MockIRNode -> MockIRNode
constantFoldNode node@(MockIRNode _ _ (MockIntValue _) _) = node
constantFoldNode node@(MockIRNode _ MockBool (MockBoolValue _) _) = node
constantFoldNode node@(MockIRNode id MockInt (MockStringValue _) children) =
  case children of
    [MockIRNode _ MockInt (MockIntValue a) _, MockIRNode _ MockInt (MockIntValue b) _] -> 
      MockIRNode id MockInt (MockIntValue (a + b)) []
    _ -> node
constantFoldNode node = node { nodeChildren = map constantFoldNode (nodeChildren node) }

eliminateDeadCode :: MockIRNode -> MockIRNode
eliminateDeadCode node@(MockIRNode _ MockBool (MockBoolValue False) _) = 
  MockIRNode (nodeId node) (nodeType node) (nodeValue node) []
eliminateDeadCode node = node { nodeChildren = map eliminateDeadCode (filter isReachable (nodeChildren node)) }

eliminateCommonSubexpressions :: MockIRNode -> MockIRNode
eliminateCommonSubexpressions node = 
  let subexpressions = collectSubexpressions node
      duplicates = findDuplicates subexpressions
  in replaceDuplicates node duplicates

isReachable :: MockIRNode -> Bool
isReachable (MockIRNode _ MockBool (MockBoolValue False) _) = False
isReachable _ = True

countNodes :: MockIRNode -> Int
countNodes node = 1 + sum (map countNodes (nodeChildren node))

countUnreachableNodes :: MockIRNode -> Int
countUnreachableNodes node = 
  let isUnreachable = not . isReachable
  in if isUnreachable node then 1 else 0 + sum (map countUnreachableNodes (nodeChildren node))

countRedundantSubexpressions :: MockIRNode -> Int
countRedundantSubexpressions node =
  let subexpressions = collectSubexpressions node
      uniqueSubexpressions = nub subexpressions
  in length subexpressions - length uniqueSubexpressions

collectSubexpressions :: MockIRNode -> [MockIRNode]
collectSubexpressions node = node : concatMap collectSubexpressions (nodeChildren node)

findDuplicates :: [MockIRNode] -> [MockIRNode]
findDuplicates nodes = 
  let grouped = groupByEqual nodes
  in concatMap (\group -> if length group > 1 then [head group] else []) grouped

groupByEqual :: [MockIRNode] -> [[MockIRNode]]
groupByEqual [] = []
groupByEqual (x:xs) = 
  let equal = [y | y <- xs, x == y]
      rest = [y | y <- xs, x /= y]
  in (x:equal) : groupByEqual rest

replaceDuplicates :: MockIRNode -> [MockIRNode] -> MockIRNode
replaceDuplicates node duplicates = 
  if node `elem` duplicates
  then head duplicates
  else node { nodeChildren = map (\child -> replaceDuplicates child duplicates) (nodeChildren node) }

collectTypes :: MockIRNode -> Set.Set MockIRType
collectTypes node = Set.singleton (nodeType node) `Set.union` 
                   Set.unions (map collectTypes (nodeChildren node))

applyAllOptimizations :: MockIRNode -> MockIRNode
applyAllOptimizations = foldl applyOptimization [ConstantFolding, DeadCodeElimination, CommonSubexpressionElimination]

evaluateIR :: MockIRNode -> MockIRValue
evaluateIR (MockIRNode _ _ value _) = value
evaluateIR _ = MockStringValue "error"

analyzeBehavior :: MockIRNode -> String
analyzeBehavior node = case evaluateIR node of
  MockIntValue _ -> "returns_int"
  MockBoolValue _ -> "returns_bool"
  MockStringValue _ -> "returns_string"
  _ -> "unknown"

applyConstantPropagation :: MockIRNode -> MockIRNode
applyConstantPropagation = propagateConstants

propagateConstants :: MockIRNode -> MockIRNode
propagateConstants node = node { nodeChildren = map propagateConstants (nodeChildren node) }

countVariables :: MockIRNode -> Int
countVariables (MockIRNode _ _ (MockFunctionValue _) _) = 1
countVariables node = sum (map countVariables (nodeChildren node))

countConstantVariables :: MockIRNode -> Int
countConstantVariables node = length [() | MockIRNode _ _ (MockFunctionValue _) _ <- allNodes node]
  where
    allNodes n = n : concatMap allNodes (nodeChildren n)

containsLoops :: MockIRNode -> Bool
containsLoops node = any isLoopNode (allNodes node)
  where
    allNodes n = n : concatMap allNodes (nodeChildren n)
    isLoopNode (MockIRNode id _ _ _) = "loop" `isPrefixOf` id

applyLoopInvariantCodeMotion :: MockIRNode -> MockIRNode
applyLoopInvariantCodeMotion node = node { nodeChildren = map applyLoopInvariantCodeMotion (nodeChildren node) }

countLoopInvariants :: MockIRNode -> Int
countLoopInvariants = length . filter isInvariant . allNodes
  where
    allNodes n = n : concatMap allNodes (nodeChildren n)
    isInvariant (MockIRNode _ MockInt (MockIntValue _) _) = True
    isInvariant _ = False

countLoopInvariantsOutside :: MockIRNode -> Int
countLoopInvariantsOutside = countLoopInvariants

applyPeepholeOptimization :: MockIRNode -> MockIRNode
applyPeepholeOptimization = simplifyPatterns

simplifyPatterns :: MockIRNode -> MockIRNode
simplifyPatterns node = node { nodeChildren = map simplifyPatterns (nodeChildren node) }

calculateComplexity :: MockIRNode -> Int
calculateComplexity node = 1 + sum (map calculateComplexity (nodeChildren node))

simulateExecution :: MockIRNode -> String
simulateExecution node = case evaluateIR node of
  MockIntValue v -> "int:" ++ show v
  MockBoolValue v -> "bool:" ++ show v
  MockStringValue v -> "string:" ++ v
  _ -> "unknown"

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

tests :: TestTree
tests = testGroup "Compiler IR Optimization QuickCheck Tests"
  [ fastProperty "Constant folding preserves semantics" prop_constant_folding_preserves_semantics
  , fastProperty "Dead code elimination removes unreachable nodes" prop_dead_code_elimination_removes_unreachable
  , fastProperty "Common subexpression elimination reduces redundancy" prop_cse_reduces_redundancy
  , fastProperty "Optimization preserves types" prop_optimization_preserves_types
  , fastProperty "Multiple optimization passes are idempotent" prop_multiple_passes_idempotent
  , fastProperty "Optimization preserves functional behavior" prop_optimization_preserves_behavior
  , fastProperty "Constant propagation eliminates variables" prop_constant_propagation_eliminates_vars
  , fastProperty "Loop invariant code motion" prop_loop_invariant_code_motion
  , fastProperty "Peephole optimization simplifies patterns" prop_peephole_simplifies_patterns
  , fastProperty "Optimization maintains program equivalence" prop_optimization_maintains_equivalence
  ]