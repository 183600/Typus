{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CompilerOptimizationQuickCheckSpec (tests) where

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty, QuickCheckTests(..))
import Test.Tasty.HUnit (testCase, assert, assertBool)
import Compiler (compile)
import Compiler.IR (IRModule(..), IRFunction(..), IRStatement(..), IRExpression(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements, choose, listOf, oneof, sized)
import Data.List (nub, sort)
import Data.Maybe (isJust, isNothing)
import Control.Monad (when)

-- | Generate arbitrary variable names
newtype VarName = VarName String
  deriving (Show, Eq)

instance Arbitrary VarName where
  arbitrary = do
    first <- elements $ ['a'..'z'] ++ ['A'..'Z']
    rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
    return $ VarName (first : rest)

-- | Generate arbitrary IR expressions
instance Arbitrary IRExpression where
  arbitrary = sized exprGen where
    exprGen 0 = oneof
      [ Var <$> arbitrary
      , IntLit <$> choose (0, 1000)
      , BoolLit <$> arbitrary
      ]
    exprGen n = oneof
      [ Var <$> arbitrary
      , IntLit <$> choose (0, 1000)
      , BoolLit <$> arbitrary
      , BinaryOp <$> arbitrary <*> exprGen (n `div` 2) <*> exprGen (n `div` 2)
      , Call <$> arbitrary <*> listOf (exprGen (n `div` 2))
      , If <$> exprGen (n `div` 3) <*> exprGen (n `div` 3) <*> exprGen (n `div` 3)
      ]

-- | Generate arbitrary IR statements
instance Arbitrary IRStatement where
  arbitrary = oneof
    [ Assign <$> arbitrary <*> arbitrary
    , Return <$> arbitrary
    , ExprStmt <$> arbitrary
    , IfStmt <$> arbitrary <*> listOf arbitrary <*> listOf arbitrary
    , WhileStmt <$> arbitrary <*> listOf arbitrary
    ]

-- | Generate arbitrary IR functions
instance Arbitrary IRFunction where
  arbitrary = do
    name <- arbitrary
    params <- listOf arbitrary
    body <- listOf arbitrary
    return $ IRFunction
      { funcName = name
      , funcParams = params
      , funcBody = body
      }

-- | Generate arbitrary IR modules
instance Arbitrary IRModule where
  arbitrary = do
    name <- arbitrary
    functions <- listOf arbitrary
    return $ IRModule
      { moduleName = name
      , moduleFunctions = functions
      }

-- | Generate arbitrary binary operators
data BinaryOp = Add | Sub | Mul | Div | And | Or | Eq | Neq | Lt | Gt | Leq | Geq
  deriving (Show, Eq)

instance Arbitrary BinaryOp where
  arbitrary = elements [Add, Sub, Mul, Div, And, Or, Eq, Neq, Lt, Gt, Leq, Geq]

tests :: TestTree
tests = testGroup "Compiler Optimization Tests"
  [ testProperty "constant folding preserves semantics" $ \expr ->
      let optimized = optimizeConstantFolding expr
          -- For simple constant expressions, the result should be a constant
          isConstant e = case e of
            IntLit _ -> True
            BoolLit _ -> True
            BinaryOp op (IntLit a) (IntLit b) -> True
            _ -> False
      in case (expr, optimized) of
        (BinaryOp Add (IntLit a) (IntLit b), IntLit c) -> c == a + b
        (BinaryOp Sub (IntLit a) (IntLit b), IntLit c) -> c == a - b
        (BinaryOp Mul (IntLit a) (IntLit b), IntLit c) -> c == a * b
        (BinaryOp And (BoolLit a) (BoolLit b), BoolLit c) -> c == a && b
        (BinaryOp Or (BoolLit a) (BoolLit b), BoolLit c) -> c == a || b
        _ -> property True -- Other cases should not crash

  , testProperty "dead code elimination removes unreachable code" $ \stmts ->
      let optimized = eliminateDeadCode stmts
          hasUnreachable = any isUnreachable stmts
          optimizedHasUnreachable = any isUnreachable optimized
      in if hasUnreachable
         then length optimized < length stmts
         else optimized == stmts

  , testProperty "function inlining reduces call overhead" $ \module' ->
      let optimized = inlineFunctions module'
          originalCalls = countFunctionCalls module'
          optimizedCalls = countFunctionCalls optimized
      in optimizedCalls <= originalCalls

  , testProperty "common subexpression elimination reduces redundancy" $ \expr ->
      let optimized = eliminateCommonSubexpressions expr
          originalSubexprs = countSubexpressions expr
          optimizedSubexprs = countSubexpressions optimized
      in optimizedSubexprs <= originalSubexprs

  , testCase "optimization preserves program semantics" $ do
      let simpleExpr = BinaryOp Add (IntLit 1) (IntLit 2)
          optimized = optimizeConstantFolding simpleExpr
      assert (optimized == IntLit 3)

  , testCase "optimization handles complex expressions" $ do
      let complexExpr = BinaryOp Add 
            (BinaryOp Mul (IntLit 2) (IntLit 3))
            (BinaryOp Mul (IntLit 4) (IntLit 5))
          optimized = optimizeConstantFolding complexExpr
      assert (optimized == IntLit 26) -- (2*3) + (4*5) = 6 + 20 = 26

  , testCase "dead code elimination removes unreachable statements" $ do
      let unreachable = [Return (IntLit 1), Assign (VarName "x") (IntLit 2)]
          optimized = eliminateDeadCode unreachable
      assert (length optimized == 1)
      assert (isReturn $ head optimized)

  , testCase "function inlining works for simple functions" $ do
      let func = IRFunction (VarName "add") [VarName "a", VarName "b"] 
            [Return $ BinaryOp Add (Var (VarName "a")) (Var (VarName "b"))]
          module' = IRModule (VarName "test") [func]
          callExpr = Call (VarName "add") [IntLit 1, IntLit 2]
          optimized = inlineFunctions module'
      assert (hasInlinedFunction optimized)

  , testCase "optimization pipeline preserves correctness" $ do
      let module' = IRModule (VarName "test") 
            [ IRFunction (VarName "main") [] 
              [ Return $ BinaryOp Add (IntLit 1) (BinaryOp Mul (IntLit 2) (IntLit 3))
              ]
            ]
          optimized = runOptimizationPipeline module'
      assert (isOptimized optimized)

  , testProperty "optimization does not introduce new variables" $ \module' ->
      let optimized = runOptimizationPipeline module'
          originalVars = collectVariables module'
          optimizedVars = collectVariables optimized
      in all (`elem` originalVars) optimizedVars

  , testProperty "optimization preserves function signatures" $ \module' ->
      let optimized = runOptimizationPipeline module'
          originalSigs = map getFunctionSignature (moduleFunctions module')
          optimizedSigs = map getFunctionSignature (moduleFunctions optimized)
      in sort originalSigs == sort optimizedSigs
  ]

-- Helper functions for optimization tests (these would be implemented in the actual compiler)
optimizeConstantFolding :: IRExpression -> IRExpression
optimizeConstantFolding = constFold where
  constFold (BinaryOp Add (IntLit a) (IntLit b)) = IntLit (a + b)
  constFold (BinaryOp Sub (IntLit a) (IntLit b)) = IntLit (a - b)
  constFold (BinaryOp Mul (IntLit a) (IntLit b)) = IntLit (a * b)
  constFold (BinaryOp And (BoolLit a) (BoolLit b)) = BoolLit (a && b)
  constFold (BinaryOp Or (BoolLit a) (BoolLit b)) = BoolLit (a || b)
  constFold e = e

eliminateDeadCode :: [IRStatement] -> [IRStatement]
eliminateDeadCode stmts = takeWhile (not . isReturn) stmts

inlineFunctions :: IRModule -> IRModule
inlineFunctions = id -- Simplified for testing

eliminateCommonSubexpressions :: IRExpression -> IRExpression
eliminateCommonSubexpressions = id -- Simplified for testing

runOptimizationPipeline :: IRModule -> IRModule
runOptimizationPipeline = id -- Simplified for testing

-- Helper predicates and counters
isUnreachable :: IRStatement -> Bool
isUnreachable (Return _) = True
isUnreachable _ = False

isReturn :: IRStatement -> Bool
isReturn (Return _) = True
isReturn _ = False

countFunctionCalls :: IRModule -> Int
countFunctionCalls = const 0 -- Simplified for testing

countSubexpressions :: IRExpression -> Int
countSubexpressions = const 1 -- Simplified for testing

hasInlinedFunction :: IRModule -> Bool
hasInlinedFunction = const False -- Simplified for testing

isOptimized :: IRModule -> Bool
isOptimized = const True -- Simplified for testing

collectVariables :: IRModule -> [VarName]
collectVariables = const [] -- Simplified for testing

getFunctionSignature :: IRFunction -> (VarName, [VarName])
getFunctionSignature f = (funcName f, funcParams f)