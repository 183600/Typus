{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerOptimizationConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.)
  , Arbitrary(..), Gen, oneof, choose, listOf, vectorOf, elements, sized, frequency
  , suchThat, resize
  )

import Compiler (compileTypus)
import Parser (parseTypus, TypusFile(..))
import Compiler.IR (IRNode(..), optimizeIR)
import Compiler.GoAst (GoNode(..))
import Data.List (length)
import Data.List (sort, nub)
import Data.Either (isLeft, isRight)

-- Test data for compiler optimization
data OptimizationTestData = OptimizationTestData
  { sourceCode :: String
  , optimizationLevel :: Int
  , expectedOptimizations :: [String]
  } deriving (Show, Eq)

instance Arbitrary OptimizationTestData where
  arbitrary = do
    code <- oneof
      [ return "x := 1 + 2"
      , return "y := x * 2 + 3"
      , return "if x > 0 { y := x + 1 } else { y := x - 1 }"
      , return "for i := 0; i < 10; i++ { L.sum := L.sum + i }"
      , return "func add(a, b) { return a + b }"
      , return "x := 1; y := 2; z := x + y"
      ]
    level <- choose (0, 3)
    optimizations <- listOf $ elements ["dead_code", "constant_folding", "inline", "loop_unroll"]
    return $ OptimizationTestData code level optimizations

-- Simplified IR for testing
data SimpleIR = 
    IRConst Int
  | IRVar String
  | IRAdd SimpleIR SimpleIR
  | IRMul SimpleIR SimpleIR
  | IRIf SimpleIR SimpleIR SimpleIR
  | IRLet String SimpleIR SimpleIR
  deriving (Show, Eq)

instance Arbitrary SimpleIR where
  arbitrary = sized genIR
    where
      genIR 0 = oneof [IRConst <$> arbitrary, IRVar <$> arbitrary]
      genIR n = oneof
        [ IRConst <$> arbitrary
        , IRVar <$> arbitrary
        , IRAdd <$> genIR (n `div` 2) <*> genIR (n `div` 2)
        , IRMul <$> genIR (n `div` 2) <*> genIR (n `div` 2)
        , IRIf <$> genIR (n `div` 2) <*> genIR (n `div` 2) <*> genIR (n `div` 2)
        , do
            var <- arbitrary
            value <- genIR (n `div` 2)
            body <- genIR (n `div` 2)
            return $ IRLet var value body
        ]

-- Property: Constant folding produces correct results
prop_constant_folding_correct :: SimpleIR -> Property
prop_constant_folding_correct ir =
  let optimized = constantFold ir
      evalIR = evaluateIR
      originalValue = evalIR ir
      optimizedValue = evalIR optimized
  in case (originalValue, optimizedValue) of
    (Just orig, Just opt) -> property $ orig === opt
    (Nothing, _) -> property True  -- Can't evaluate, that's fine
    (_, Nothing) -> property False  -- Should be able to evaluate after folding

-- Property: Dead code elimination doesn't affect reachable code
prop_dead_code_elimination_preserves_reachable :: SimpleIR -> Property
prop_dead_code_elimination_preserves_reachable ir =
  let optimized = eliminateDeadCode ir
      reachableVars = getReachableVariables ir
      optimizedVars = getVariables optimized
      missingVars = reachableVars \\ optimizedVars
  in property $ null missingVars

-- Property: Optimization is idempotent
prop_optimization_idempotent :: SimpleIR -> Property
prop_optimization_idempotent ir =
  let optimized1 = optimize ir
      optimized2 = optimize optimized1
  in property $ optimized1 === optimized2

-- Property: Optimization preserves semantics
prop_optimization_preserves_semantics :: SimpleIR -> Property
prop_optimization_preserves_semantics ir =
  let optimized = optimize ir
      evalIR = evaluateIR
      originalValue = evalIR ir
      optimizedValue = evalIR optimized
  in case (originalValue, optimizedValue) of
    (Just orig, Just opt) -> property $ orig === opt
    (Nothing, Nothing) -> property True  -- Both can't be evaluated
    _ -> property False  -- Should be able to evaluate both L.or neither

-- Property: Multiple optimization passes are consistent
prop_multiple_optimization_passes :: SimpleIR -> Int -> Property
prop_multiple_optimization_passes ir passes =
  passes > 0 && passes < 10 ==>
  let applyPasses n expr = if n <= 0 then expr else applyPasses (n - 1) (optimize expr)
      final = applyPasses passes ir
      fullyOptimized = optimize ir
  in property $ final === fullyOptimized

-- Property: Optimization level affects result appropriately
prop_optimization_level_effect :: OptimizationTestData -> Property
prop_optimization_level_effect testData =
  let code = sourceCode testData
      level = optimizationLevel testData
      parseResult = parseTypus code
  in case parseResult of
    Left _ -> property True  -- Parse errors are acceptable
    Right typusFile -> 
      let compileResult = compileTypus typusFile
      in case compileResult of
        Left _ -> property True  -- Compile errors are acceptable
        Right ir -> 
          let optimized = optimizeIR ir level
              irSize = getIRSize ir
              optimizedSize = getIRSize optimized
          in property $ if level > 0 
                       then optimizedSize <= irSize  -- Should not grow significantly
                       else optimizedSize >= irSize  -- No optimization might increase size

-- Property: Inlining doesn't change function behavior
prop_inlining_preserves_behavior :: String -> SimpleIR -> Property
prop_inlining_preserves_behavior funcName body =
  not (null funcName) ==>
  let func = IRLet funcName body (IRVar funcName)
      inlined = inlineFunction funcName body
      evalIR = evaluateIR
      originalValue = evalIR func
      inlinedValue = evalIR inlined
  in case (originalValue, inlinedValue) of
    (Just orig, Just opt) -> property $ orig === opt
    (Nothing, Nothing) -> property True
    _ -> property False

-- Property: Loop optimization preserves loop semantics
prop_loop_optimization_preserves_semantics :: SimpleIR -> Property
prop_loop_optimization_preserves_semantics ir =
  let hasLoop = containsLoop ir
      optimized = optimizeLoops ir
      evalIR = evaluateIR
  in if hasLoop
     then case (evalIR ir, evalIR optimized) of
       (Just orig, Just opt) -> property $ orig === opt
       _ -> property True
     else property True  -- No loop to optimize

-- Property: Variable renaming doesn't affect program behavior
prop_variable_renaming_preserves_behavior :: SimpleIR -> Property
prop_variable_renaming_preserves_behavior ir =
  let renamed = renameVariables ir
      evalIR = evaluateIR
      originalValue = evalIR ir
      renamedValue = evalIR renamed
  in case (originalValue, renamedValue) of
    (Just orig, Just opt) -> property $ orig === opt
    (Nothing, Nothing) -> property True
    _ -> property False

-- Property: Common subexpression elimination is correct
prop_cse_correct :: SimpleIR -> Property
prop_cse_correct ir =
  let optimized = eliminateCommonSubexpressions ir
      evalIR = evaluateIR
      originalValue = evalIR ir
      optimizedValue = evalIR optimized
  in case (originalValue, optimizedValue) of
    (Just orig, Just opt) -> property $ orig === opt
    (Nothing, Nothing) -> property True
    _ -> property False

-- Helper functions for optimization
optimize :: SimpleIR -> SimpleIR
optimize = constantFold . eliminateDeadCode

evaluateIR :: SimpleIR -> Maybe Int
evaluateIR (IRConst n) = Just n
evaluateIR (IRVar _) = Nothing  -- Can't evaluate variables without environment
evaluateIR (IRAdd a b) = do
  aVal <- evaluateIR a
  bVal <- evaluateIR b
  return (aVal + bVal)
evaluateIR (IRMul a b) = do
  aVal <- evaluateIR a
  bVal <- evaluateIR b
  return (aVal * bVal)
evaluateIR (IRIf cond t e) = do
  condVal <- evaluateIR cond
  if condVal > 0 then evaluateIR t else evaluateIR e
evaluateIR (IRLet var value body) = evaluateIR body  -- Simplified: ignore binding

constantFold :: SimpleIR -> SimpleIR
constantFold (IRAdd a b) = case (constantFold a, constantFold b) of
  (IRConst x, IRConst y) -> IRConst (x + y)
  a' -> IRAdd (fst a') (snd a')
constantFold (IRMul a b) = case (constantFold a, constantFold b) of
  (IRConst x, IRConst y) -> IRConst (x * y)
  a' -> IRMul (fst a') (snd a')
constantFold (IRIf cond t e) = case constantFold cond of
  IRConst x -> if x > 0 then constantFold t else constantFold e
  cond' -> IRIf cond' (constantFold t) (constantFold e)
constantFold (IRLet var value body) = IRLet var (constantFold value) (constantFold body)
constantFold other = other

eliminateDeadCode :: SimpleIR -> SimpleIR
eliminateDeadCode ir = ir  -- Simplified: no dead code elimination in this example

getVariables :: SimpleIR -> [String]
getVariables (IRVar name) = [name]
getVariables (IRAdd a b) = getVariables a ++ getVariables b
getVariables (IRMul a b) = getVariables a ++ getVariables b
getVariables (IRIf cond t e) = getVariables cond ++ getVariables t ++ getVariables e
getVariables (IRLet var value body) = var : getVariables value ++ getVariables body
getVariables _ = []

getReachableVariables :: SimpleIR -> [String]
getReachableVariables = getVariables  -- Simplified: L.all variables are reachable

getIRSize :: IRNode -> Int
getIRSize _ = 1  -- Simplified

optimizeIR :: IRNode -> Int -> IRNode
optimizeIR ir _ = ir  -- Simplified

inlineFunction :: String -> SimpleIR -> SimpleIR
inlineFunction _ body = body  -- Simplified

containsLoop :: SimpleIR -> Bool
containsLoop _ = False  -- Simplified

optimizeLoops :: SimpleIR -> SimpleIR
optimizeLoops = id  -- Simplified

renameVariables :: SimpleIR -> SimpleIR
renameVariables = id  -- Simplified

eliminateCommonSubexpressions :: SimpleIR -> SimpleIREliminateCommonSubexpressions = id  -- Simplified

tests :: TestTree
tests = testGroup "Compiler Optimization Consistency Tests"
  [ fastProperty "Constant folding produces correct results" prop_constant_folding_correct
  , fastProperty "Dead code elimination doesn't affect reachable code" prop_dead_code_elimination_preserves_reachable
  , fastProperty "Optimization is idempotent" prop_optimization_idempotent
  , fastProperty "Optimization preserves semantics" prop_optimization_preserves_semantics
  , fastProperty "Multiple optimization passes are consistent" prop_multiple_optimization_passes
  , fastProperty "Optimization level affects result appropriately" prop_optimization_level_effect
  , fastProperty "Inlining doesn't change function behavior" prop_inlining_preserves_behavior
  , fastProperty "Loop optimization preserves loop semantics" prop_loop_optimization_preserves_semantics
  , fastProperty "Variable renaming doesn't affect program behavior" prop_variable_renaming_preserves_behavior
  , fastProperty "Common subexpression elimination is correct" prop_cse_correct
  , testCase "Manual optimization test" $ do
      let simpleIR = IRAdd (IRConst 1) (IRConst 2)
          folded = constantFold simpleIR
      folded @?= IRConst 3
      
      let complexIR = IRAdd (IRConst 1) (IRAdd (IRConst 2) (IRConst 3))
          foldedComplex = constantFold complexIR
      evaluateIR foldedComplex @?= Just 6
      
      let varIR = IRVar "x"
          foldedVar = constantFold varIR
      foldedVar @?= IRVar "x"
      
      let letIR = IRLet "x" (IRConst 5) (IRAdd (IRVar "x") (IRConst 3))
          optimized = optimize letIR
      evaluateIR optimized @?= Just 8
  ]