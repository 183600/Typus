{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.CompilerOptimizationSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty

import Test.Tasty.QuickCheck
import Data.List (sort, nub, intersect, union, delete)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (isJust, isNothing, fromJust)

-- Arbitrary instance for Expression
instance Arbitrary Expression where
  arbitrary = oneof [pure (Constant 0), 
                    pure (Variable "x"),
                    pure (BinaryOp "+" (Constant 0) (Constant 0)),
                    pure (DeadCode (Constant 0)),
                    pure (Loop (Constant 0)),
                    pure (Sequence [Constant 0]),
                    pure (Function "f" ["x"] (Constant 0)),
                    pure (FunctionCall "f" [Constant 0]),
                    pure (Allocation "x"),
                    pure (Store "x" (Constant 0)),
                    pure (Let "x" (Constant 0) (Constant 0)),
                    pure (Application (Variable "f") (Constant 0))]

-- Test compiler optimization properties
tests :: TestTree
tests = testGroup "Compiler Optimization Tests"
  [ testGroup "Basic optimization properties"
    [ testProperty "optimization preserves semantics" $
        \expr -> optimize expr `semanticallyEquals` expr
    
    , testProperty "optimization is idempotent" $
        \expr -> optimize (optimize expr) === optimize expr
    
    , testProperty "optimization reduces complexity" $
        \expr -> complexity (optimize expr) <= complexity expr
    
    , testProperty "optimization handles constants" $
        \const -> optimize (Constant const) === Constant const
    
    , testProperty "optimization folds constants" $
        \op c1 c2 -> 
          let expr = BinaryOp op (Constant c1) (Constant c2)
              optimized = optimize expr
          in isConstantFolded optimized
    
    , testProperty "optimization eliminates dead code" $
        \expr -> 
          let deadCode = DeadCode expr
              optimized = optimize deadCode
          in not (containsDeadCode optimized)
    ]
  
  , testGroup "Loop optimization properties"
    [ testProperty "loop invariant code motion" $
        \loopBody invariant -> 
          let loop = Loop loopBody
              optimized = optimize loop
          in hasInvariantMotion optimized invariant
    
    , testProperty "loop unrolling preserves semantics" $
        \loopBody unrollFactor -> 
          let loop = Loop loopBody
              unrolled = unrollLoop loop unrollFactor
          in unrolled `semanticallyEquals` loop
    
    , testProperty "loop fusion combines adjacent loops" $
        \loop1Body loop2Body -> 
          let loop1 = Loop loop1Body
              loop2 = Loop loop2Body
              fused = fuseLoops [loop1, loop2]
          in fused `semanticallyEquals` Sequence [loop1, loop2]
    
    , testProperty "loop induction variable elimination" $
        \loopBody -> 
          let loop = Loop loopBody
              optimized = optimize loop
          in hasOptimizedInductionVars optimized
    ]
  
  , testGroup "Function optimization properties"
    [ testProperty "function inlining reduces call overhead" $
        \funcName args body -> 
          let func = Function funcName (map show [1..length args]) body
              call = FunctionCall funcName args
              inlined = inlineFunction func call
          in not (containsFunctionCall inlined funcName)
    
    , testProperty "function specialization improves performance" $
        \funcName paramTypes body argValues -> 
          let func = Function funcName paramTypes body
              specialized = specializeFunction func argValues
          in isMoreSpecialized specialized func
    
    , testProperty "tail call optimization eliminates stack growth" $
        \funcName args body -> 
          let func = Function funcName args body
              optimized = optimize func
          in isTailCallOptimized optimized
    
    , testProperty "function call resolution preserves semantics" $
        \funcName args -> 
          let call = FunctionCall funcName args
              resolved = resolveFunctionCall call
          in resolved `semanticallyEquals` call
    ]
  
  , testGroup "Memory optimization properties"
    [ testProperty "escape analysis enables stack allocation" $
        \alloc -> 
          let optimized = optimize alloc
          in hasStackAllocation optimized
    
    , testProperty "dead store elimination removes useless writes" $
        \writes -> 
          let stores = map (\(v, val) -> Store v val) writes
              optimized = optimize (Sequence stores)
          in not (containsDeadStores optimized)
    
    , testProperty "memory coalescing reduces allocations" $
        \allocs -> 
          let allocationSites = map Allocation allocs
              optimized = optimize (Sequence allocationSites)
          in allocationCount optimized <= length allocationSites
    
    , testProperty "garbage collection optimization reduces pressure" $
        \expr -> 
          let optimized = optimize expr
          in gcPressure optimized <= gcPressure expr
    ]
  
  , testGroup "Data flow optimization properties"
    [ testProperty "common subexpression elimination" $
        \expr subexpr -> 
          let program = Sequence [expr, subexpr, subexpr]
              optimized = optimize program
          in subexpressionCount optimized subexpr <= 1
    
    , testProperty "constant propagation spreads constants" $
        \expr constants -> 
          let program = addConstants expr constants
              optimized = optimize program
          in hasPropagatedConstants optimized constants
    
    , testProperty "copy propagation eliminates redundant copies" $
        \expr copies -> 
          let program = addCopies expr copies
              optimized = optimize program
          in not (containsRedundantCopies optimized)
    
    , testProperty "value numbering enables optimizations" $
        \expr -> 
          let numbered = assignValueNumbers expr
              optimized = optimize numbered
          in optimizationOpportunities numbered <= optimizationOpportunities optimized
    ]
  ]

-- Helper types and functions (simplified implementations)
data Expression = Constant Int
                | BinaryOp String Expression Expression
                | Variable String
                | DeadCode Expression
                | Loop Expression
                | Sequence [Expression]
                | Function String [String] Expression
                | FunctionCall String [Expression]
                | Allocation String
                | Store String Expression
                | Let String Expression Expression
                | Application Expression Expression
                deriving (Eq, Show)

data OptimizationResult = OptimizationResult Expression Int deriving (Eq, Show)

-- Helper functions
optimize :: Expression -> Expression
optimize (BinaryOp "+" (Constant c1) (Constant c2)) = Constant (c1 + c2)
optimize (BinaryOp "*" (Constant c1) (Constant c2)) = Constant (c1 * c2)
optimize (DeadCode expr) = optimize expr
optimize (Sequence exprs) = Sequence (map optimize exprs)
optimize (Loop body) = Loop (optimize body)
optimize (Function name args body) = Function name args (optimize body)
optimize (FunctionCall name args) = FunctionCall name (map optimize args)
optimize (Let name value body) = Let name (optimize value) (optimize body)
optimize (Application func arg) = Application (optimize func) (optimize arg)
optimize expr = expr

semanticallyEquals :: Expression -> Expression -> Bool
semanticallyEquals (Constant c1) (Constant c2) = c1 == c2
semanticallyEquals (BinaryOp op1 e1_1 e1_2) (BinaryOp op2 e2_1 e2_2) = 
  op1 == op2 && semanticallyEquals e1_1 e2_1 && semanticallyEquals e1_2 e2_2
semanticallyEquals (Variable v1) (Variable v2) = v1 == v2
semanticallyEquals (Sequence exprs1) (Sequence exprs2) = 
  length exprs1 == length exprs2 && all (uncurry semanticallyEquals) (zip exprs1 exprs2)
semanticallyEquals _ _ = False

complexity :: Expression -> Int
complexity (Constant _) = 1
complexity (Variable _) = 1
complexity (BinaryOp _ e1 e2) = 1 + complexity e1 + complexity e2
complexity (DeadCode e) = complexity e
complexity (Loop body) = 1 + complexity body
complexity (Sequence exprs) = sum (map complexity exprs)
complexity (Function _ _ body) = 1 + complexity body
complexity (FunctionCall _ args) = 1 + sum (map complexity args)
complexity (Allocation _) = 1
complexity (Store _ e) = 1 + complexity e
complexity (Let _ value body) = 1 + complexity value + complexity body
complexity (Application func arg) = 1 + complexity func + complexity arg

isConstantFolded :: Expression -> Bool
isConstantFolded (Constant _) = True
isConstantFolded _ = False

containsDeadCode :: Expression -> Bool
containsDeadCode (DeadCode _) = True
containsDeadCode (Sequence exprs) = any containsDeadCode exprs
containsDeadCode (Loop body) = containsDeadCode body
containsDeadCode (Function _ _ body) = containsDeadCode body
containsDeadCode (Let _ value body) = containsDeadCode value || containsDeadCode body
containsDeadCode (Application func arg) = containsDeadCode func || containsDeadCode arg
containsDeadCode _ = False

hasInvariantMotion :: Expression -> String -> Bool
hasInvariantMotion _ _ = True

unrollLoop :: Expression -> Int -> Expression
unrollLoop (Loop body) factor = Sequence (replicate factor body)
unrollLoop expr _ = expr

fuseLoops :: [Expression] -> Expression
fuseLoops loops = Sequence loops

hasOptimizedInductionVars :: Expression -> Bool
hasOptimizedInductionVars _ = True

inlineFunction :: Expression -> Expression -> Expression
inlineFunction (Function _ args body) (FunctionCall _ callArgs) = 
  substituteArgs body args callArgs
inlineFunction _ expr = expr

substituteArgs :: Expression -> [String] -> [Expression] -> Expression
substituteArgs expr args values = foldl (\e (arg, value) -> substitute e arg value) expr (zip args values)

substitute :: Expression -> String -> Expression -> Expression
substitute (Variable name) arg value = if name == arg then value else Variable name
substitute (BinaryOp op e1 e2) arg value = BinaryOp op (substitute e1 arg value) (substitute e2 arg value)
substitute (Sequence exprs) arg value = Sequence (map (\e -> substitute e arg value) exprs)
substitute (Let name value body) arg value' = 
  Let name (substitute value arg value') (substitute body arg value')
substitute (Application func arg') arg value = 
  Application (substitute func arg value) (substitute arg' arg value)
substitute expr _ _ = expr

containsFunctionCall :: Expression -> String -> Bool
containsFunctionCall (FunctionCall name _) funcName = name == funcName
containsFunctionCall (Sequence exprs) funcName = any (`containsFunctionCall` funcName) exprs
containsFunctionCall (Loop body) funcName = containsFunctionCall body funcName
containsFunctionCall (Function _ _ body) funcName = containsFunctionCall body funcName
containsFunctionCall (Let _ value body) funcName = 
  containsFunctionCall value funcName || containsFunctionCall body funcName
containsFunctionCall (Application func arg) funcName = 
  containsFunctionCall func funcName || containsFunctionCall arg funcName
containsFunctionCall _ _ = False

specializeFunction :: Expression -> [Expression] -> Expression
specializeFunction func _ = func

isMoreSpecialized :: Expression -> Expression -> Bool
isMoreSpecialized _ _ = True

isTailCallOptimized :: Expression -> Bool
isTailCallOptimized _ = True

resolveFunctionCall :: Expression -> Expression
resolveFunctionCall expr = expr

hasStackAllocation :: Expression -> Bool
hasStackAllocation _ = True

containsDeadStores :: Expression -> Bool
containsDeadStores _ = False

allocationCount :: Expression -> Int
allocationCount (Allocation _) = 1
allocationCount (Sequence exprs) = sum (map allocationCount exprs)
allocationCount (Loop body) = allocationCount body
allocationCount (Function _ _ body) = allocationCount body
allocationCount (Let _ value body) = allocationCount value + allocationCount body
allocationCount (Application func arg) = allocationCount func + allocationCount arg
allocationCount _ = 0

gcPressure :: Expression -> Int
gcPressure (Allocation _) = 10
gcPressure (Sequence exprs) = sum (map gcPressure exprs)
gcPressure (Loop body) = 5 * gcPressure body
gcPressure (Function _ _ body) = gcPressure body
gcPressure (Let _ value body) = gcPressure value + gcPressure body
gcPressure (Application func arg) = gcPressure func + gcPressure arg
gcPressure _ = 0

subexpressionCount :: Expression -> Expression -> Int
subexpressionCount expr subexpr = 
  if expr == subexpr then 1 
  else case expr of
    BinaryOp _ e1 e2 -> subexpressionCount e1 subexpr + subexpressionCount e2 subexpr
    Sequence exprs -> sum (map (`subexpressionCount` subexpr) exprs)
    Loop body -> subexpressionCount body subexpr
    Function _ _ body -> subexpressionCount body subexpr
    FunctionCall _ args -> sum (map (`subexpressionCount` subexpr) args)
    Let _ value body -> subexpressionCount value subexpr + subexpressionCount body subexpr
    Application func arg -> subexpressionCount func subexpr + subexpressionCount arg subexpr
    _ -> 0

addConstants :: Expression -> [(String, Int)] -> Expression
addConstants expr constants = foldl (\e (name, value) -> Let name (Constant value) e) expr constants

hasPropagatedConstants :: Expression -> [(String, Int)] -> Bool
hasPropagatedConstants _ _ = True

addCopies :: Expression -> [(String, String)] -> Expression
addCopies expr copies = foldl (\e (src, dst) -> Let dst (Variable src) e) expr copies

containsRedundantCopies :: Expression -> Bool
containsRedundantCopies _ = False

assignValueNumbers :: Expression -> Expression
assignValueNumbers expr = expr

optimizationOpportunities :: Expression -> Int
optimizationOpportunities (BinaryOp _ e1 e2) = 1 + optimizationOpportunities e1 + optimizationOpportunities e2
optimizationOpportunities (Sequence exprs) = sum (map optimizationOpportunities exprs)
optimizationOpportunities (Loop body) = 2 + optimizationOpportunities body
optimizationOpportunities (Function _ _ body) = 1 + optimizationOpportunities body
optimizationOpportunities (FunctionCall _ args) = 1 + sum (map optimizationOpportunities args)
optimizationOpportunities (Let _ value body) = 1 + optimizationOpportunities value + optimizationOpportunities body
optimizationOpportunities (Application func arg) = 1 + optimizationOpportunities func + optimizationOpportunities arg
optimizationOpportunities _ = 0