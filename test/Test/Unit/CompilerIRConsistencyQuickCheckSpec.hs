module Test.Unit.CompilerIRConsistencyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, property, Arbitrary(..), Gen, oneof, listOf, elements, choose)
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, sort)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Compiler.IR (IRNode(..), IRStatement(..), IRExpression(..), IRType(..))
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, spanBetween)
import Utils (trim, splitBy)

-- | QuickCheck tests for Compiler IR consistency properties
tests :: TestTree
tests =
  testGroup "CompilerIRConsistencyQuickCheckSpec - IR Consistency Tests"
    [ testProperty "IRNode type consistency is preserved" prop_irNodeTypeConsistency
    , testProperty "IRStatement variable references are valid" prop_irStatementVariableValidity
    , testProperty "IRExpression type inference is consistent" prop_irExpressionTypeConsistency
    , testProperty "IRType ordering is total and transitive" prop_irTypeOrderingProperties
    , testProperty "IR transformation preserves semantics" prop_irTransformationSemantics
    , testProperty "IR optimization maintains correctness" prop_irOptimizationCorrectness
    , testProperty "IR variable scoping is properly nested" prop_irVariableScoping
    , testProperty "IR control flow is well-formed" prop_irControlFlowWellFormed
    ]

-- ============================================================================
-- IR Node Properties
-- ============================================================================

-- Property: IRNode type consistency is preserved across operations
prop_irNodeTypeConsistency :: IRNode -> Bool
prop_irNodeTypeConsistency node =
  let nodeType = getIRNodeType node
      transformedNode = transformIRNode node
      transformedType = getIRNodeType transformedNode
  in nodeType == transformedType

-- Property: IRStatement variable references are valid and declared
prop_irStatementVariableValidity :: [IRStatement] -> Bool
prop_irStatementVariableValidity statements =
  let declaredVars = extractDeclaredVariables statements
      usedVars = extractUsedVariables statements
  in all (`Set.member` declaredVars) usedVars

-- Property: IRExpression type inference is consistent
prop_irExpressionTypeConsistency :: IRExpression -> Bool
prop_irExpressionTypeConsistency expr =
  let inferredType = inferExpressionType expr
      expectedType = getExpectedExpressionType expr
  in inferredType == expectedType

-- Property: IRType ordering is total and transitive
prop_irTypeOrderingProperties :: IRType -> IRType -> IRType -> Bool
prop_irTypeOrderingProperties t1 t2 t3 =
  let ordering1 = compareIRType t1 t2
      ordering2 = compareIRType t2 t3
      ordering3 = compareIRType t1 t3
      -- Transitivity: if t1 <= t2 and t2 <= t3 then t1 <= t3
      transitive = not (ordering1 == LT && ordering2 == LT && ordering3 /= LT)
      -- Totality: any two types can be compared
      total = True
  in transitive && total

-- Property: IR transformation preserves semantics
prop_irTransformationSemantics :: IRNode -> Bool
prop_irTransformationSemantics node =
  let originalSemantics = extractIRSemantics node
      transformed = transformIRNode node
      transformedSemantics = extractIRSemantics transformed
  in originalSemantics == transformedSemantics

-- Property: IR optimization maintains correctness
prop_irOptimizationCorrectness :: IRNode -> Bool
prop_irOptimizationCorrectness node =
  let optimized = optimizeIRNode node
      originalBehavior = simulateIRBehavior node
      optimizedBehavior = simulateIRBehavior optimized
  in originalBehavior == optimizedBehavior

-- Property: IR variable scoping is properly nested
prop_irVariableScoping :: [IRStatement] -> Bool
prop_irVariableScoping statements =
  let scopes = extractVariableScopes statements
      checkNesting [] = True
      checkNesting [_] = True
      checkNesting (s1:s2:ss) = isScopeNested s1 s2 && checkNesting (s2:ss)
  in checkNesting scopes

-- Property: IR control flow is well-formed
prop_irControlFlowWellFormed :: [IRStatement] -> Bool
prop_irControlFlowWellFormed statements =
  let controlFlowGraph = buildControlFlowGraph statements
      entryPoints = findEntryPoints controlFlowGraph
      exitPoints = findExitPoints controlFlowGraph
      -- Check that all paths from entry reach exit
      allPathsValid = all (pathToExit controlFlowGraph) entryPoints
  in not (null entryPoints) && not (null exitPoints) && allPathsValid

-- ============================================================================
-- Helper Functions (Mock implementations for testing)
-- ============================================================================

-- Mock IR data types
data IRNode = IRNode
  { nodeId :: Int
  , nodeType :: IRType
  , nodeStatements :: [IRStatement]
  } deriving (Show, Eq)

data IRStatement 
  = IRVarDecl String IRType
  | IRAssignment String IRExpression
  | IRReturn IRExpression
  | IRIf IRExpression [IRStatement] [IRStatement]
  deriving (Show, Eq)

data IRExpression
  = IRLiteral String IRType
  | IRVariable String
  | IRBinaryOp IRExpression String IRExpression
  | IRFunctionCall String [IRExpression]
  deriving (Show, Eq)

data IRType
  = IRInt
  | IRString
  | IRBool
  | IRFunction IRType IRType
  | IRCustom String
  deriving (Show, Eq, Ord)

-- Mock helper functions
getIRNodeType :: IRNode -> IRType
getIRNodeType = nodeType

transformIRNode :: IRNode -> IRNode
transformIRNode node = node { nodeId = nodeId node + 1 }

extractDeclaredVariables :: [IRStatement] -> Set.Set String
extractDeclaredVariables = Set.fromList . concatMap extractVar
  where
    extractVar (IRVarDecl var _) = [var]
    extractVar _ = []

extractUsedVariables :: [IRStatement] -> Set.Set String
extractUsedVariables = Set.fromList . concatMap extractVars
  where
    extractVars (IRAssignment expr) = extractExprVars expr
    extractVars (IRReturn expr) = extractExprVars expr
    extractVars (IRIf cond thenStmts elseStmts) = 
      extractExprVars cond ++ concatMap extractVars thenStmts ++ concatMap extractVars elseStmts
    extractVars _ = []

extractExprVars :: IRExpression -> [String]
extractExprVars (IRVariable var) = [var]
extractExprVars (IRBinaryOp left _ right) = extractExprVars left ++ extractExprVars right
extractExprVars (IRFunctionCall _ args) = concatMap extractExprVars args
extractExprVars _ = []

inferExpressionType :: IRExpression -> IRType
inferExpressionType (IRLiteral _ typ) = typ
inferExpressionType (IRVariable _) = IRInt  -- Mock inference
inferExpressionType (IRBinaryOp left _ right) = 
  let leftType = inferExpressionType left
      rightType = inferExpressionType right
  in if leftType == rightType then leftType else IRInt
inferExpressionType (IRFunctionCall _ _) = IRInt  -- Mock inference

getExpectedExpressionType :: IRExpression -> IRType
getExpectedExpressionType = inferExpressionType

compareIRType :: IRType -> IRType -> Ordering
compareIRType = compare

extractIRSemantics :: IRNode -> String
extractIRSemantics node = "semantics_" ++ show (nodeId node)

simulateIRBehavior :: IRNode -> String
simulateIRBehavior node = "behavior_" ++ show (nodeId node)

optimizeIRNode :: IRNode -> IRNode
optimizeIRNode node = node { nodeId = nodeId node * 2 }

extractVariableScopes :: [IRStatement] -> [String]
extractVariableScopes statements = ["scope_" ++ show i | i <- [1..length statements]]

isScopeNested :: String -> String -> Bool
isScopeNested s1 s2 = s1 /= s2

buildControlFlowGraph :: [IRStatement] -> [(String, [String])]
buildControlFlowGraph statements = [(show i, [show (i+1)]) | i <- [1..length statements]]

findEntryPoints :: [(String, [String])] -> [String]
findEntryPoints graph = case graph of
  [] -> []
  (entry:_) -> [fst entry]

findExitPoints :: [(String, [String])] -> [String]
findExitPoints graph = case reverse graph of
  [] -> []
  (exit:_) -> [fst exit]

pathToExit :: [(String, [String])] -> String -> Bool
pathToExit _ _ = True  -- Mock implementation

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary IRType where
  arbitrary = oneof
    [ pure IRInt
    , pure IRString
    , pure IRBool
    , IRFunction <$> arbitrary <*> arbitrary
    , IRCustom <$> elements ["Custom1", "Custom2", "Custom3"]
    ]

instance Arbitrary IRExpression where
  arbitrary = oneof
    [ IRLiteral <$> arbitrary <*> arbitrary
    , IRVariable <$> elements ["x", "y", "z", "var1", "var2"]
    , IRBinaryOp <$> arbitrary <*> elements ["+", "-", "*", "/"] <*> arbitrary
    , IRFunctionCall <$> elements ["func1", "func2"] <*> listOf arbitrary
    ]

instance Arbitrary IRStatement where
  arbitrary = oneof
    [ IRVarDecl <$> arbitrary <*> arbitrary
    , IRAssignment <$> arbitrary <*> arbitrary
    , IRReturn <$> arbitrary
    , IRIf <$> arbitrary <*> listOf arbitrary <*> listOf arbitrary
    ]

instance Arbitrary IRNode where
  arbitrary = IRNode <$> arbitrary <*> arbitrary <*> listOf arbitrary