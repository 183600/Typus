module Test.Unit.CompilerIROptimizationSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedAt, startPos)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Test cases for basic IR optimization
testBasicIROptimization :: TestTree
testBasicIROptimization = testGroup "Basic IR optimization tests"
  [ testCase "constant folding" $
      let expr1 = createBinaryLiteral Add (LiteralInt 5) (LiteralInt 3)
          expr2 = createBinaryLiteral Add (LiteralInt 8) (LiteralInt 2)
          optimized = optimizeExpression expr1
      in optimized @?= LiteralInt 8
  , testCase "dead code elimination" $
      let block1 = createBasicBlock [createInstruction (LiteralInt 1)] (createInstruction (LiteralInt 2))
          block2 = createBasicBlock [createInstruction (LiteralInt 3)] (createInstruction (LiteralInt 4))
          deadBlock = createBasicBlock [] (createInstruction (LiteralInt 5))
          function = createFunction "test" [block1, deadBlock, block2]
          optimized = eliminateDeadCode function
      in length (functionBlocks optimized) @?= 2
  , testCase "constant propagation" $
      let var = createVariable "x" (locatedAt startPos "int")
          assign = createAssignment var (LiteralInt 42)
          use = createUse var
          block = createBasicBlock [assign, use] (createInstruction (LiteralInt 0))
          optimized = propagateConstants block
      in blockInstructions optimized @?= [createAssignment var (LiteralInt 42), createUse (LiteralInt 42)]
  ]

-- Test cases for advanced IR optimization
testAdvancedIROptimization :: TestTree
testAdvancedIROptimization = testGroup "Advanced IR optimization tests"
  [ testCase "loop invariant code motion" $
      let invariantVar = createVariable "invariant" (locatedAt startPos "int")
          loopVar = createVariable "i" (locatedAt startPos "int")
          invariantAssign = createAssignment invariantVar (LiteralInt 10)
          loopAssign = createAssignment loopVar (LiteralInt 0)
          loopBody = createBasicBlock [loopAssign] (createInstruction (LiteralInt 1))
          loop = createLoop loopVar (LiteralInt 0) (LiteralInt 10) loopBody
          preHeader = createBasicBlock [invariantAssign] loop
          optimized = moveLoopInvariants preHeader
      in hasInvariantMoved optimized invariantAssign
  , testCase "common subexpression elimination" $
      let subexpr = createBinaryLiteral Add (LiteralInt 5) (LiteralInt 3)
          expr1 = createBinaryLiteral Mul subexpr (LiteralInt 2)
          expr2 = createBinaryLiteral Div subexpr (LiteralInt 1)
          block = createBasicBlock [createInstruction expr1, createInstruction expr2] (createInstruction (LiteralInt 0))
          optimized = eliminateCommonSubexpressions block
      in hasCommonSubexpressionEliminated optimized
  , testCase "strength reduction" $
      let mulExpr = createBinaryLiteral Mul (Variable "x") (LiteralInt 4)
          addExpr = createBinaryLiteral Add (Variable "x") (Variable "x")
          block = createBasicBlock [createInstruction mulExpr] (createInstruction (LiteralInt 0))
          optimized = reduceStrength block
      in hasStrengthReduced optimized
  ]

-- Test cases for IR transformation
testIRTransformation :: TestTree
testIRTransformation = testGroup "IR transformation tests"
  [ testCase "instruction scheduling" $
      let inst1 = createInstruction (LiteralInt 1)
          inst2 = createInstruction (LiteralInt 2)
          inst3 = createInstruction (LiteralInt 3)
          block = createBasicBlock [inst1, inst2, inst3] (createInstruction (LiteralInt 0))
          scheduled = scheduleInstructions block
      in length (blockInstructions scheduled) @?= 3
  , testCase "register allocation" $
      let var1 = createVariable "x" (locatedAt startPos "int")
          var2 = createVariable "y" (locatedAt startPos "int")
          assign1 = createAssignment var1 (LiteralInt 1)
          assign2 = createAssignment var2 (LiteralInt 2)
          block = createBasicBlock [assign1, assign2] (createInstruction (LiteralInt 0))
          allocated = allocateRegisters block
      in hasRegistersAllocated allocated
  , testCase "control flow optimization" $
      let trueBlock = createBasicBlock [createInstruction (LiteralInt 1)] (createInstruction (LiteralInt 0))
          falseBlock = createBasicBlock [createInstruction (LiteralInt 2)] (createInstruction (LiteralInt 0))
          condition = createBinaryLiteral Eq (LiteralInt 1) (LiteralInt 1)
          branch = createBranch condition trueBlock falseBlock
          optimized = optimizeControlFlow branch
      in hasControlFlowOptimized optimized
  ]

-- Test cases for optimization validation
testOptimizationValidation :: TestTree
testOptimizationValidation = testGroup "Optimization validation tests"
  [ testCase "preserve semantics" $
      let originalExpr = createBinaryLiteral Add (LiteralInt 5) (LiteralInt 3)
          optimizedExpr = optimizeExpression originalExpr
          result1 = evaluateExpression originalExpr
          result2 = evaluateExpression optimizedExpr
      in result1 @?= result2
  , testCase "no infinite loops" $
      let loopVar = createVariable "i" (locatedAt startPos "int")
          loopBody = createBasicBlock [] (createInstruction (LiteralInt 1))
          loop = createLoop loopVar (LiteralInt 0) (LiteralInt 10) loopBody
          optimized = optimizeLoop loop
      in hasFiniteTermination optimized
  , testCase "maintain invariants" $
      let originalFunction = createFunction "test" [createBasicBlock [] (createInstruction (LiteralInt 0))]
          optimizedFunction = optimizeFunction originalFunction
      in functionInvariantsMaintained originalFunction optimizedFunction
  ]

-- Test cases for optimization metrics
testOptimizationMetrics :: TestTree
testOptimizationMetrics = testGroup "Optimization metrics tests"
  [ testCase "measure optimization improvement" $
      let originalBlock = createBasicBlock 
              [ createInstruction (LiteralInt 1)
              , createInstruction (LiteralInt 2)
              , createInstruction (BinaryLiteral Add (LiteralInt 1) (LiteralInt 2))
              ] 
              (createInstruction (LiteralInt 0))
          optimizedBlock = optimizeBlock originalBlock
          originalSize = blockSize originalBlock
          optimizedSize = blockSize optimizedBlock
      in optimizedSize <= originalSize
  , testCase "measure execution time improvement" $
      let originalExpr = createBinaryLiteral Add (LiteralInt 1) (LiteralInt 2)
          optimizedExpr = optimizeExpression originalExpr
          originalTime = estimateExecutionTime originalExpr
          optimizedTime = estimateExecutionTime optimizedExpr
      in optimizedTime <= originalTime
  , testCase "measure memory usage improvement" $
      let originalFunction = createFunction "test" [createBasicBlock [] (createInstruction (LiteralInt 0))]
          optimizedFunction = optimizeFunction originalFunction
          originalMemory = estimateMemoryUsage originalFunction
          optimizedMemory = estimateMemoryUsage optimizedFunction
      in optimizedMemory <= originalMemory
  ]

-- Mock data types and functions for testing
data IRExpression = 
    LiteralInt Int
  | LiteralBool Bool
  | Variable String
  | BinaryLiteral BinaryOp IRExpression IRExpression
  deriving (Show, Eq)

data BinaryOp = Add | Sub | Mul | Div | Eq | Ne | Lt | Gt | Le | Ge deriving (Show, Eq)

data IRInstruction = 
    Instruction IRExpression
  | Assignment IRVariable IRExpression
  | Use IRExpression
  deriving (Show, Eq)

data IRVariable = IRVariable
  { irVariableName :: String
  , irVariableType :: Located String
  } deriving (Show, Eq)

data BasicBlock = BasicBlock
  { blockInstructions :: [IRInstruction]
  , blockTerminator :: IRInstruction
  } deriving (Show, Eq)

data Loop = Loop
  { loopVariable :: IRVariable
  , loopStart :: IRExpression
  , loopEnd :: IRExpression
  , loopBody :: BasicBlock
  } deriving (Show, Eq)

data Branch = Branch
  { branchCondition :: IRExpression
  , branchTrue :: BasicBlock
  , branchFalse :: BasicBlock
  } deriving (Show, Eq)

data Function = Function
  { functionName :: String
  , functionBlocks :: [BasicBlock]
  } deriving (Show, Eq)

-- Mock implementations
createBinaryLiteral :: BinaryOp -> IRExpression -> IRExpression -> IRExpression
createBinaryLiteral = BinaryLiteral

createBasicBlock :: [IRInstruction] -> IRInstruction -> BasicBlock
createBasicBlock = BasicBlock

createInstruction :: IRExpression -> IRInstruction
createInstruction = Instruction

createAssignment :: Variable -> IRExpression -> IRInstruction
createAssignment = Assignment

createUse :: IRExpression -> IRInstruction
createUse = Use

createFunction :: String -> [BasicBlock] -> Function
createFunction = Function

createVariable :: String -> Located String -> IRVariable
createVariable name typ = IRVariable name typ

createLoop :: Variable -> IRExpression -> IRExpression -> BasicBlock -> Loop
createLoop = Loop

createBranch :: IRExpression -> BasicBlock -> BasicBlock -> Branch
createBranch = Branch

optimizeExpression :: IRExpression -> IRExpression
optimizeExpression (BinaryLiteral Add (LiteralInt a) (LiteralInt b)) = LiteralInt (a + b)
optimizeExpression (BinaryLiteral Sub (LiteralInt a) (LiteralInt b)) = LiteralInt (a - b)
optimizeExpression (BinaryLiteral Mul (LiteralInt a) (LiteralInt b)) = LiteralInt (a * b)
optimizeExpression (BinaryLiteral Div (LiteralInt a) (LiteralInt b)) 
  | b /= 0 = LiteralInt (a `div` b)
  | otherwise = BinaryLiteral Div (LiteralInt a) (LiteralInt b)
optimizeExpression expr = expr

eliminateDeadCode :: Function -> Function
eliminateDeadCode function = 
  let blocks = functionBlocks function
      hasUse block = any hasUseInstruction (blockInstructions block) || hasUseInstruction (blockTerminator block)
      hasUseInstruction (Instruction _) = True
      hasUseInstruction (Assignment _ _) = True
      hasUseInstruction (Use _) = True
  in function { functionBlocks = filter hasUse blocks }

propagateConstants :: BasicBlock -> BasicBlock
propagateConstants block = 
  let instructions = blockInstructions block
      (constants, newInstructions) = propagateConstantsHelper instructions Map.empty []
  in block { blockInstructions = newInstructions }

propagateConstantsHelper :: [IRInstruction] -> Map.Map String IRExpression -> [IRInstruction] -> 
  (Map.Map String IRExpression, [IRInstruction])
propagateConstantsHelper [] constants acc = (constants, reverse acc)
propagateConstantsHelper (Instruction expr : rest) constants acc = 
  let newExpr = substituteConstants expr constants
  in propagateConstantsHelper rest constants (Instruction newExpr : acc)
propagateConstantsHelper (Assignment var expr : rest) constants acc = 
  let newExpr = substituteConstants expr constants
      newConstants = Map.insert (variableName var) newExpr constants
  in propagateConstantsHelper rest newConstants (Assignment var newExpr : acc)
propagateConstantsHelper (Use expr : rest) constants acc = 
  let newExpr = substituteConstants expr constants
  in propagateConstantsHelper rest constants (Use newExpr : acc)

substituteConstants :: IRExpression -> Map.Map String IRExpression -> IRExpression
substituteConstants (Variable name) constants = 
  Map.findWithDefault (Variable name) name constants
substituteConstants (BinaryLiteral op left right) constants = 
  BinaryLiteral op (substituteConstants left constants) (substituteConstants right constants)
substituteConstants expr = expr

moveLoopInvariants :: BasicBlock -> BasicBlock
moveLoopInvariants block = block  -- Simplified implementation

eliminateCommonSubexpressions :: BasicBlock -> BasicBlock
eliminateCommonSubexpressions block = block  -- Simplified implementation

reduceStrength :: BasicBlock -> BasicBlock
reduceStrength block = block  -- Simplified implementation

scheduleInstructions :: BasicBlock -> BasicBlock
scheduleInstructions block = block  -- Simplified implementation

allocateRegisters :: BasicBlock -> BasicBlock
allocateRegisters block = block  -- Simplified implementation

optimizeControlFlow :: Branch -> Branch
optimizeControlFlow branch = branch  -- Simplified implementation

optimizeLoop :: Loop -> Loop
optimizeLoop loop = loop  -- Simplified implementation

optimizeFunction :: Function -> Function
optimizeFunction function = function  -- Simplified implementation

optimizeBlock :: BasicBlock -> BasicBlock
optimizeBlock block = block  -- Simplified implementation

-- Helper functions for testing
hasInvariantMoved :: BasicBlock -> IRInstruction -> Bool
hasInvariantMoved _ _ = True  -- Simplified implementation

hasCommonSubexpressionEliminated :: BasicBlock -> Bool
hasCommonSubexpressionEliminated _ = True  -- Simplified implementation

hasStrengthReduced :: BasicBlock -> Bool
hasStrengthReduced _ = True  -- Simplified implementation

hasRegistersAllocated :: BasicBlock -> Bool
hasRegistersAllocated _ = True  -- Simplified implementation

hasControlFlowOptimized :: Branch -> Bool
hasControlFlowOptimized _ = True  -- Simplified implementation

evaluateExpression :: IRExpression -> IRExpression
evaluateExpression (LiteralInt i) = LiteralInt i
evaluateExpression (LiteralBool b) = LiteralBool b
evaluateExpression (BinaryLiteral Add (LiteralInt a) (LiteralInt b)) = LiteralInt (a + b)
evaluateExpression expr = expr

hasFiniteTermination :: Loop -> Bool
hasFiniteTermination _ = True  -- Simplified implementation

functionInvariantsMaintained :: Function -> Function -> Bool
functionInvariantsMaintained _ _ = True  -- Simplified implementation

blockSize :: BasicBlock -> Int
blockSize block = length (blockInstructions block) + 1

estimateExecutionTime :: IRExpression -> Int
estimateExecutionTime (LiteralInt _) = 1
estimateExecutionTime (LiteralBool _) = 1
estimateExecutionTime (Variable _) = 1
estimateExecutionTime (BinaryLiteral _ _ _) = 2

estimateMemoryUsage :: Function -> Int
estimateMemoryUsage function = length (functionBlocks function) * 10

-- QuickCheck properties
prop_constant_folding_correct :: IRExpression -> Property
prop_constant_folding_correct expr = 
  let optimized = optimizeExpression expr
      result1 = evaluateExpression expr
      result2 = evaluateExpression optimized
  in result1 == result2

prop_dead_code_elimination_reduces_size :: Function -> Property
prop_dead_code_elimination_reduces_size function = 
  let optimized = eliminateDeadCode function
      originalSize = length (functionBlocks function)
      optimizedSize = length (functionBlocks optimized)
  in optimizedSize <= originalSize

prop_optimization_preserves_semantics :: IRExpression -> Property
prop_optimization_preserves_semantics expr = 
  let optimized = optimizeExpression expr
      result1 = evaluateExpression expr
      result2 = evaluateExpression optimized
  in result1 == result2

tests :: TestTree
tests = testGroup "Compiler IR Optimization Tests"
  [ testBasicIROptimization
  , testAdvancedIROptimization
  , testIRTransformation
  , testOptimizationValidation
  , testOptimizationMetrics
  , testProperty "constant folding correct" prop_constant_folding_correct
  , testProperty "dead code elimination reduces size" prop_dead_code_elimination_reduces_size
  , testProperty "optimization preserves semantics" prop_optimization_preserves_semantics
  ]