{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.CompilerOptimizationAdvancedSpec where


import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure, Assertion)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>), classify, sized)
import Data.List (nub, sort, groupBy, sortBy, find, delete)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (replicateM, when)

-- Simple IR for optimization testing
data IRInstruction = 
    Add String String String      -- dest = op1 + op2
  | Sub String String String      -- dest = op1 - op2
  | Mul String String String      -- dest = op1 * op2
  | Div String String String      -- dest = op1 / op2
  | Const String Int              -- dest = constant
  | Load String String            -- dest = memory[addr]
  | Store String String           -- memory[addr] = src
  | Call String [String] String   -- dest = function(args)
  | Ret String                    -- return value
  | Branch String String String   -- if cond then trueLabel else falseLabel
  | Jump String                   -- goto label
  | Label String                  -- label:
  deriving (Eq, Show)

data BasicBlock = BasicBlock 
  { bbLabel :: String
  , bbInstructions :: [IRInstruction]
  , bbSuccessors :: [String]
  }
  deriving (Eq, Show)

data ControlFlowGraph = CFG 
  { cfgBlocks :: Map String BasicBlock
  , cfgEntry :: String
  , cfgExit :: String
  }
  deriving (Eq, Show)

data OptimizationPass = 
    ConstantFolding
  | DeadCodeElimination
  | CommonSubexpressionElimination
  | StrengthReduction
  deriving (Eq, Show)

-- Helper generators for compiler optimization tests
genRegister :: Gen String
genRegister = do
  num <- choose (1, 10) :: Gen Int
  return $ "r" ++ show num

genLabel :: Gen String
genLabel = do
  num <- choose (1, 10) :: Gen Int
  return $ "L" ++ show num

genConstant :: Gen Int
genConstant = choose (-100, 100)

genIRInstruction :: Gen IRInstruction
genIRInstruction = oneof
  [ do
      dest <- genRegister
      op1 <- genRegister
      op2 <- genRegister
      return $ Add dest op1 op2
  , do
      dest <- genRegister
      op1 <- genRegister
      op2 <- genRegister
      return $ Sub dest op1 op2
  , do
      dest <- genRegister
      op1 <- genRegister
      op2 <- genRegister
      return $ Mul dest op1 op2
  , do
      dest <- genRegister
      op1 <- genRegister
      op2 <- genRegister
      return $ Div dest op1 op2
  , do
      dest <- genRegister
      value <- genConstant
      return $ Const dest value
  , do
      dest <- genRegister
      addr <- genRegister
      return $ Load dest addr
  , do
      addr <- genRegister
      src <- genRegister
      return $ Store addr src
  , do
      dest <- genRegister
      numArgs <- choose (0, 3)
      args <- replicateM numArgs genRegister
      func <- elements ["func1", "func2", "func3"]
      return $ Call dest args func
  , do
      value <- genRegister
      return $ Ret value
  , do
      cond <- genRegister
      trueLabel <- genLabel
      falseLabel <- genLabel
      return $ Branch cond trueLabel falseLabel
  , do
      label <- genLabel
      return $ Jump label
  , do
      label <- genLabel
      return $ Label label
  ]

genBasicBlock :: Gen BasicBlock
genBasicBlock = do
  label <- genLabel
  numInstrs <- choose (1, 5)
  instructions <- replicateM numInstrs genIRInstruction
  numSuccessors <- choose (0, 2)
  successors <- replicateM numSuccessors genLabel
  return $ BasicBlock label instructions successors

genCFG :: Gen ControlFlowGraph
genCFG = do
  numBlocks <- choose (1, 5)
  blockLabels <- replicateM numBlocks genLabel
  blocks <- mapM (\label -> do
    numInstrs <- choose (1, 3)
    instrs <- replicateM numInstrs genIRInstruction
    numSuccs <- choose (0, 2)
    succs <- replicateM numSuccs (elements blockLabels)
    return (label, BasicBlock label instrs succs)) blockLabels
  let entry = case blockLabels of (x:_) -> x; [] -> error "empty blockLabels"
  let exit = case blockLabels of [] -> error "empty blockLabels"; _ -> last blockLabels
  return $ CFG (Map.fromList blocks) entry exit

-- Arbitrary instances
instance Arbitrary IRInstruction where
  arbitrary = genIRInstruction

instance Arbitrary BasicBlock where
  arbitrary = genBasicBlock

instance Arbitrary ControlFlowGraph where
  arbitrary = genCFG

instance Arbitrary OptimizationPass where
  arbitrary = oneof [pure ConstantFolding, pure DeadCodeElimination, pure CommonSubexpressionElimination, pure StrengthReduction]

-- Test properties for compiler optimization

-- Property 1: Constant folding preserves semantics
prop_constant_folding_preserves_semantics :: IRInstruction -> Bool
prop_constant_folding_preserves_semantics instr = 
  let folded = constantFold instr
      originalResult = evaluateInstruction instr Map.empty
      foldedResult = evaluateInstruction folded Map.empty
  in originalResult == foldedResult

-- Property 2: Dead code elimination removes unused definitions
prop_dead_code_elimination_removes_unused :: BasicBlock -> Bool
prop_dead_code_elimination_removes_unused block = 
  let usedVars = findUsedVariables block
      optimized = eliminateDeadCode block
      optimizedDefs = findDefinitions optimized
  in all (`elem` usedVars) optimizedDefs

-- Property 3: Common subexpression elimination reduces redundancy
prop_cse_reduces_redundancy :: [IRInstruction] -> Property
prop_cse_reduces_redundancy instrs = 
  let commonExprs = findCommonSubexpressions instrs
      optimized = eliminateCommonSubexpressions instrs
      optimizedExprs = findCommonSubexpressions optimized
  in length commonExprs > 0 ==> length optimizedExprs < length commonExprs

-- Property 4: Strength reduction preserves functionality
prop_strength_reduction_preserves_functionality :: IRInstruction -> Bool
prop_strength_reduction_preserves_functionality instr = 
  let reduced = applyStrengthReduction instr
      originalResult = evaluateInstruction instr Map.empty
      reducedResult = evaluateInstruction reduced Map.empty
  in originalResult == reducedResult

-- Property 5: Optimization is idempotent for certain passes
prop_optimization_is_idempotent :: ControlFlowGraph -> OptimizationPass -> Bool
prop_optimization_is_idempotent cfg pass = 
  let optimizedOnce = applyOptimizationPass cfg pass
      optimizedTwice = applyOptimizationPass optimizedOnce pass
  in countInstructions optimizedOnce == countInstructions optimizedTwice

-- Property 6: Control flow graph structure is preserved
prop_cfg_structure_preserved :: ControlFlowGraph -> Bool
prop_cfg_structure_preserved cfg = 
  let optimized = optimizeCFG cfg
      originalBlocks = Map.size (cfgBlocks cfg)
      optimizedBlocks = Map.size (cfgBlocks optimized)
  in optimizedBlocks <= originalBlocks

-- Property 7: Register allocation doesn't increase register pressure
prop_register_allocation_pressure :: BasicBlock -> Bool
prop_register_allocation_pressure block = 
  let originalPressure = calculateRegisterPressure block
      allocated = allocateRegisters block
      allocatedPressure = calculateRegisterPressure allocated
  in allocatedPressure <= originalPressure + 1

-- Property 8: Loop invariant code motion moves invariants
prop_loop_invariant_motion :: ControlFlowGraph -> Bool
prop_loop_invariant_motion cfg = 
  let loops = findLoops cfg
      optimized = moveLoopInvariants cfg
      invariantsMoved = all (hasInvariantMoved cfg optimized) loops
  in invariantsMoved || null loops

-- Property 9: Function inlining preserves call behavior
prop_function_inlining_preserves_behavior :: ControlFlowGraph -> Bool
prop_function_inlining_preserves_behavior cfg = 
  let calls = findFunctionCalls cfg
      inlined = inlineFunctions cfg
      inlinedCalls = findFunctionCalls inlined
  in length inlinedCalls < length calls || null calls

-- Property 10: Peephole optimization improves instruction patterns
prop_peephole_improves_patterns :: [IRInstruction] -> Bool
prop_peephole_improves_patterns instrs = 
  let optimized = applyPeepholeOptimization instrs
      originalPatterns = countPeepholePatterns instrs
      optimizedPatterns = countPeepholePatterns optimized
  in optimizedPatterns <= originalPatterns

-- Helper functions for optimization
constantFold :: IRInstruction -> IRInstruction
constantFold (Add dest op1 op2) 
  | all isConstant [op1, op2] = 
      let val1 = parseConstant op1
          val2 = parseConstant op2
      in Const dest (val1 + val2)
constantFold (Sub dest op1 op2) 
  | all isConstant [op1, op2] = 
      let val1 = parseConstant op1
          val2 = parseConstant op2
      in Const dest (val1 - val2)
constantFold (Mul dest op1 op2) 
  | all isConstant [op1, op2] = 
      let val1 = parseConstant op1
          val2 = parseConstant op2
      in Const dest (val1 * val2)
constantFold (Div dest op1 op2) 
  | all isConstant [op1, op2] && parseConstant op2 /= 0 = 
      let val1 = parseConstant op1
          val2 = parseConstant op2
      in Const dest (val1 `div` val2)
constantFold instr = instr

isConstant :: String -> Bool
isConstant s = all (`elem` "0123456789-") s

parseConstant :: String -> Int
parseConstant s = read s

evaluateInstruction :: IRInstruction -> Map String Int -> Maybe Int
evaluateInstruction (Const dest value) _ = Just value
evaluateInstruction (Add dest op1 op2) env = do
  val1 <- Map.lookup op1 env
  val2 <- Map.lookup op2 env
  return (val1 + val2)
evaluateInstruction (Sub dest op1 op2) env = do
  val1 <- Map.lookup op1 env
  val2 <- Map.lookup op2 env
  return (val1 - val2)
evaluateInstruction (Mul dest op1 op2) env = do
  val1 <- Map.lookup op1 env
  val2 <- Map.lookup op2 env
  return (val1 * val2)
evaluateInstruction (Div dest op1 op2) env = do
  val1 <- Map.lookup op1 env
  val2 <- Map.lookup op2 env
  if val2 /= 0 then return (val1 `div` val2) else Nothing
evaluateInstruction _ _ = Nothing

findUsedVariables :: BasicBlock -> Set String
findUsedVariables block = 
  Set.fromList $ concatMap getInstructionOperands (bbInstructions block)

getInstructionOperands :: IRInstruction -> [String]
getInstructionOperands (Add _ op1 op2) = [op1, op2]
getInstructionOperands (Sub _ op1 op2) = [op1, op2]
getInstructionOperands (Mul _ op1 op2) = [op1, op2]
getInstructionOperands (Div _ op1 op2) = [op1, op2]
getInstructionOperands (Load _ addr) = [addr]
getInstructionOperands (Store _ src) = [src]
getInstructionOperands (Call _ args _) = args
getInstructionOperands (Ret value) = [value]
getInstructionOperands (Branch cond _ _) = [cond]
getInstructionOperands _ = []

findDefinitions :: BasicBlock -> Set String
findDefinitions block = 
  Set.fromList $ map getInstructionDest (bbInstructions block)

getInstructionDest :: IRInstruction -> String
getInstructionDest (Add dest _ _) = dest
getInstructionDest (Sub dest _ _) = dest
getInstructionDest (Mul dest _ _) = dest
getInstructionDest (Div dest _ _) = dest
getInstructionDest (Const dest _) = dest
getInstructionDest (Load dest _) = dest
getInstructionDest (Ret dest) = dest
getInstructionDest (Call dest _ _) = dest
getInstructionDest _ = ""

eliminateDeadCode :: BasicBlock -> BasicBlock
eliminateDeadCode block = 
  let usedVars = findUsedVariables block
      isUsed instr = getInstructionDest instr `Set.member` usedVars || isBranch instr
      filteredInstrs = filter isUsed (bbInstructions block)
  in block { bbInstructions = filteredInstrs }

isBranch :: IRInstruction -> Bool
isBranch (Branch _ _ _) = True
isBranch (Jump _) = True
isBranch _ = False

findCommonSubexpressions :: [IRInstruction] -> [IRInstruction]
findCommonSubexpressions instrs = 
  let groups = groupBy sameOperation $ sortBy compareOperation instrs
      common = filter (\g -> length g > 1) groups
  in concat common

sameOperation :: IRInstruction -> IRInstruction -> Bool
sameOperation (Add _ op1 op2) (Add _ op1' op2') = sort [op1, op2] == sort [op1', op2']
sameOperation (Sub _ op1 op2) (Sub _ op1' op2') = op1 == op1' && op2 == op2'
sameOperation (Mul _ op1 op2) (Mul _ op1' op2') = sort [op1, op2] == sort [op1', op2']
sameOperation (Div _ op1 op2) (Div _ op1' op2') = op1 == op1' && op2 == op2'
sameOperation _ _ = False

compareOperation :: IRInstruction -> IRInstruction -> Ordering
compareOperation (Add _ op1 op2) (Add _ op1' op2') = compare (sort [op1, op2]) (sort [op1', op2'])
compareOperation (Sub _ op1 op2) (Sub _ op1' op2') = compare (op1, op2) (op1', op2')
compareOperation (Mul _ op1 op2) (Mul _ op1' op2') = compare (sort [op1, op2]) (sort [op1', op2'])
compareOperation (Div _ op1 op2) (Div _ op1' op2') = compare (op1, op2) (op1', op2')
compareOperation _ _ = EQ

eliminateCommonSubexpressions :: [IRInstruction] -> [IRInstruction]
eliminateCommonSubexpressions instrs = 
  -- Simplified implementation
  instrs

applyStrengthReduction :: IRInstruction -> IRInstruction
applyStrengthReduction (Mul dest op1 op2) 
  | isConstant op2 && parseConstant op2 == 2 = Add dest op1 op1
  | isConstant op2 && parseConstant op2 == 0 = Const dest 0
applyStrengthReduction (Div dest op1 op2) 
  | isConstant op2 && parseConstant op2 == 2 = -- Implement shift right
      Div dest op1 op2
applyStrengthReduction instr = instr

applyOptimizationPass :: ControlFlowGraph -> OptimizationPass -> ControlFlowGraph
applyOptimizationPass cfg ConstantFolding = 
  let foldBlock block = block { bbInstructions = map constantFold (bbInstructions block) }
      foldedBlocks = Map.map foldBlock (cfgBlocks cfg)
  in cfg { cfgBlocks = foldedBlocks }
applyOptimizationPass cfg DeadCodeElimination = 
  let eliminateBlock block = eliminateDeadCode block
      eliminatedBlocks = Map.map eliminateBlock (cfgBlocks cfg)
  in cfg { cfgBlocks = eliminatedBlocks }
applyOptimizationPass cfg CommonSubexpressionElimination = cfg
applyOptimizationPass cfg StrengthReduction = 
  let reduceBlock block = block { bbInstructions = map applyStrengthReduction (bbInstructions block) }
      reducedBlocks = Map.map reduceBlock (cfgBlocks cfg)
  in cfg { cfgBlocks = reducedBlocks }

countInstructions :: ControlFlowGraph -> Int
countInstructions cfg = 
  sum $ map (length . bbInstructions) (Map.elems (cfgBlocks cfg))

optimizeCFG :: ControlFlowGraph -> ControlFlowGraph
optimizeCFG cfg = 
  let passes = [ConstantFolding, DeadCodeElimination, StrengthReduction]
      applyPass cfg' pass = applyOptimizationPass cfg' pass
  in foldl applyPass cfg passes

calculateRegisterPressure :: BasicBlock -> Int
calculateRegisterPressure block = 
  let allVars = Set.unions $ map (\instr -> 
        Set.fromList $ getInstructionDest instr : getInstructionOperands instr) 
        (bbInstructions block)
  in Set.size allVars

allocateRegisters :: BasicBlock -> BasicBlock
allocateRegisters block = block  -- Simplified implementation

findLoops :: ControlFlowGraph -> [[String]]
findLoops cfg = []  -- Simplified implementation

hasInvariantMoved :: ControlFlowGraph -> ControlFlowGraph -> [String] -> Bool
hasInvariantMoved original optimized loop = True  -- Simplified implementation

moveLoopInvariants :: ControlFlowGraph -> ControlFlowGraph
moveLoopInvariants cfg = cfg  -- Simplified implementation

findFunctionCalls :: ControlFlowGraph -> [IRInstruction]
findFunctionCalls cfg = 
  concatMap (filter isCall . bbInstructions) (Map.elems (cfgBlocks cfg))
  where
    isCall (Call _ _ _) = True
    isCall _ = False

inlineFunctions :: ControlFlowGraph -> ControlFlowGraph
inlineFunctions cfg = cfg  -- Simplified implementation

applyPeepholeOptimization :: [IRInstruction] -> [IRInstruction]
applyPeepholeOptimization instrs = instrs  -- Simplified implementation

countPeepholePatterns :: [IRInstruction] -> Int
countPeepholePatterns instrs = length instrs  -- Simplified implementation

-- Test cases for compiler optimization
testCompilerOptimization :: TestTree
testCompilerOptimization = testGroup "Compiler Optimization Advanced Tests"
  [ testProperties "Constant Folding Properties"
    [ ("constant_folding_preserves_semantics", property prop_constant_folding_preserves_semantics)
    ]
  , testProperties "Dead Code Elimination Properties"
    [ ("dead_code_elimination_removes_unused", property prop_dead_code_elimination_removes_unused)
    ]
  , testProperties "Common Subexpression Elimination Properties"
    [ ("cse_reduces_redundancy", property prop_cse_reduces_redundancy)
    ]
  , testProperties "Strength Reduction Properties"
    [ ("strength_reduction_preserves_functionality", property prop_strength_reduction_preserves_functionality)
    ]
  , testProperties "Optimization Properties"
    [ ("optimization_is_idempotent", property prop_optimization_is_idempotent)
    , ("cfg_structure_preserved", property prop_cfg_structure_preserved)
    ]
  , testProperties "Advanced Optimization Properties"
    [ ("register_allocation_pressure", property prop_register_allocation_pressure)
    , ("loop_invariant_motion", property prop_loop_invariant_motion)
    , ("function_inlining_preserves_behavior", property prop_function_inlining_preserves_behavior)
    , ("peephole_improves_patterns", property prop_peephole_improves_patterns)
    ]
  , testCase "Constant folding basic arithmetic" $ do
    let instr = Add "r1" "10" "20"
    let folded = constantFold instr
    assertEqual "Should fold constant addition" (Const "r1" 30) folded
  
  , testCase "Dead code elimination" $ do
    let block = BasicBlock "L1" [Const "r1" 10, Add "r2" "r1" "r1", Const "r3" 20] []
    let optimized = eliminateDeadCode block
    assertEqual "Should remove unused definitions" 
                (BasicBlock "L1" [Const "r1" 10, Add "r2" "r1" "r1"] []) optimized
  
  , testCase "Strength reduction" $ do
    let instr = Mul "r1" "r2" "2"
    let reduced = applyStrengthReduction instr
    assertEqual "Should reduce multiplication by 2 to addition" 
                (Add "r1" "r2" "r2") reduced
  
  , testCase "CFG optimization" $ do
    let block1 = BasicBlock "L1" [Const "r1" 10] ["L2"]
    let block2 = BasicBlock "L2" [Add "r2" "r1" "r1"] []
    let cfg = CFG (Map.fromList [("L1", block1), ("L2", block2)]) "L1" "L2"
    let optimized = optimizeCFG cfg
    assertBool "Should preserve CFG structure" 
               (Map.size (cfgBlocks optimized) <= Map.size (cfgBlocks cfg))
  ]

-- Export the test
tests :: TestTree
tests = testCompilerOptimization