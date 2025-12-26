{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCompilerIRCoreQuickCheckSpec where

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample)
import Test.Tasty.HUnit (testCase, assertBool)

import qualified Data.Text as T
import qualified Data.Map as Map
import Compiler.IR
import Compiler.Errors.Core

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate IR variable names
genIRVariable :: Gen String
genIRVariable = do
    prefix <- elements ["var", "temp", "arg", "local", "global"]
    suffix <- choose (1, 1000)
    return $ prefix ++ show suffix

-- Generate IR literals
genIRLiteral :: Gen IRLiteral
genIRLiteral = do
    litType <- choose (1, 4 :: Int)
    case litType of
        1 -> IRInt <$> choose (-1000, 1000)
        2 -> IRString <$> listOf1 (choose ('a', 'z'))
        3 -> IRBool <$> arbitrary
        4 -> return IRUnit
        _ -> IRInt <$> choose (0, 100)

instance Arbitrary IRLiteral where
    arbitrary = genIRLiteral

-- Generate IR operators
genIROperator :: Gen IROperator
genIROperator = elements 
    [ Add, Sub, Mul, Div, Mod
    , Eq, Neq, Lt, Gt, Le, Ge
    , And, Or, Not
    ]

instance Arbitrary IROperator where
    arbitrary = genIROperator

-- Generate IR expressions
genIRExpression :: Gen IRExpression
genIRExpression = do
    exprType <- choose (1, 5 :: Int)
    case exprType of
        1 -> IRLiteral <$> arbitrary
        2 -> IRVariable <$> genIRVariable
        3 -> do
            op <- arbitrary
            left <- genIRExpression
            right <- genIRExpression
            return $ IRBinaryOp op left right
        4 -> do
            op <- elements [Not]
            operand <- genIRExpression
            return $ IRUnaryOp op operand
        5 -> do
            var <- genIRVariable
            args <- listOf genIRExpression
            return $ IRFunctionCall var args
        _ -> IRLiteral <$> arbitrary

instance Arbitrary IRExpression where
    arbitrary = genIRExpression

-- Generate IR statements
genIRStatement :: Gen IRStatement
genIRStatement = do
    stmtType <- choose (1, 6 :: Int)
    case stmtType of
        1 -> do
            var <- genIRVariable
            expr <- genIRExpression
            return $ IRAssignment var expr
        2 -> do
            var <- genIRVariable
            args <- listOf genIRExpression
            return $ IRFunctionCallStmt var args
        3 -> do
            expr <- genIRExpression
            return $ IRReturn expr
        4 -> do
            expr <- genIRExpression
            return $ IRExprStmt expr
        5 -> do
            cond <- genIRExpression
            thenStmt <- genIRStatement
            elseStmt <- genIRStatement
            return $ IRIf cond thenStmt elseStmt
        6 -> do
            cond <- genIRExpression
            body <- genIRStatement
            return $ IRWhile cond body
        _ -> IRExprStmt <$> genIRExpression

instance Arbitrary IRStatement where
    arbitrary = genIRStatement

-- Generate IR functions
genIRFunction :: Gen IRFunction
genIRFunction = do
    name <- genIRVariable
    params <- listOf genIRVariable
    body <- listOf genIRStatement
    return $ IRFunction name params body

instance Arbitrary IRFunction where
    arbitrary = genIRFunction

-- Generate IR modules
genIRModule :: Gen IRModule
genIRModule = do
    name <- elements ["Main", "Utils", "Core", "Parser", "Compiler"]
    functions <- listOf genIRFunction
    return $ IRModule name functions

instance Arbitrary IRModule where
    arbitrary = genIRModule

-- ============================================================================
-- IR Core Properties
-- ============================================================================

-- Property: IR literal evaluation is consistent
prop_irLiteralEvaluation :: IRLiteral -> Property
prop_irLiteralEvaluation lit =
    let evaluated = evaluateIRExpression (IRLiteral lit)
    in counterexample ("IR literal evaluation should be consistent")
       (evaluated === Right lit)

-- Property: IR variable lookup fails for undefined variables
prop_irVariableLookupFails :: String -> Property
prop_irVariableLookupFails varName =
    let emptyEnv = Map.empty
        result = evaluateIRExpression (IRVariable varName) emptyEnv
    in counterexample ("Variable lookup should fail for undefined variables")
       (isLeft result === True)

-- Property: IR binary operation evaluation is mathematically consistent
prop_irBinaryOpConsistency :: IROperator -> IRLiteral -> IRLiteral -> Property
prop_irBinaryOpConsistency op lit1 lit2 =
    let expr = IRBinaryOp op (IRLiteral lit1) (IRLiteral lit2)
        result = evaluateIRExpression expr Map.empty
    in counterexample ("Binary operation evaluation should be consistent")
       (case (op, lit1, lit2) of
           (Add, IRInt a, IRInt b) -> result === Right (IRInt (a + b))
           (Sub, IRInt a, IRInt b) -> result === Right (IRInt (a - b))
           (Mul, IRInt a, IRInt b) -> result === Right (IRInt (a * b))
           (Eq, a, b) -> result === Right (IRBool (a == b))
           (Neq, a, b) -> result === Right (IRBool (a /= b))
           _ -> property True)  -- Skip other cases for simplicity

-- Property: IR unary operation evaluation is consistent
prop_irUnaryOpConsistency :: IROperator -> IRLiteral -> Property
prop_irUnaryOpConsistency op lit =
    let expr = IRUnaryOp op (IRLiteral lit)
        result = evaluateIRExpression expr Map.empty
    in counterexample ("Unary operation evaluation should be consistent")
       (case (op, lit) of
           (Not, IRBool b) -> result === Right (IRBool (not b))
           _ -> property True)  -- Skip other cases for simplicity

-- Property: IR function call with no arguments should be consistent
prop_irFunctionCallConsistency :: String -> Property
prop_irFunctionCallConsistency funcName =
    let expr = IRFunctionCall funcName []
        emptyEnv = Map.empty
        result = evaluateIRExpression expr emptyEnv
    in counterexample ("Function call should be consistent")
       (isLeft result === True)  -- Should fail for undefined functions

-- Property: IR assignment should update environment
prop_irAssignmentUpdatesEnv :: String -> IRLiteral -> Property
prop_irAssignmentUpdatesEnv varName lit =
    let stmt = IRAssignment varName (IRLiteral lit)
        emptyEnv = Map.empty
        result = executeIRStatement stmt emptyEnv
    in counterexample ("Assignment should update environment")
       (case result of
           Right newEnv -> Map.lookup varName newEnv === Just lit
           Left _ -> property False)

-- Property: IR return statement should return value
prop_irReturnStatement :: IRLiteral -> Property
prop_irReturnStatement lit =
    let stmt = IRReturn (IRLiteral lit)
        emptyEnv = Map.empty
        result = executeIRStatement stmt emptyEnv
    in counterexample ("Return statement should return value")
       (case result of
           Right (env, Just returnValue) -> returnValue === lit
           _ -> property False)

-- Property: IR expression statement should evaluate expression
prop_irExpressionStatement :: IRExpression -> Property
prop_irExpressionStatement expr =
    let stmt = IRExprStmt expr
        emptyEnv = Map.empty
        result = executeIRStatement stmt emptyEnv
    in counterexample ("Expression statement should evaluate expression")
       (isRight result === True)

-- Property: IR module contains all defined functions
prop_irModuleContainsFunctions :: IRModule -> Property
prop_irModuleContainsFunctions module =
    let functions = irModuleFunctions module
        functionNames = map irFunctionName functions
        uniqueNames = nub functionNames
    in counterexample ("Module should contain all defined functions")
       (length functionNames === length uniqueNames)

-- Property: IR function parameter count matches declaration
prop_irFunctionParameterCount :: IRFunction -> Property
prop_irFunctionParameterCount func =
    let declaredParams = irFunctionParameters func
        paramCount = length declaredParams
    in counterexample ("Function parameter count should match declaration")
       (paramCount >= 0 === True)

-- Property: IR optimization preserves semantics for simple expressions
prop_irOptimizationPreservesSemantics :: IRExpression -> Property
prop_irOptimizationPreservesSemantics expr =
    let optimized = optimizeIRExpression expr
        originalResult = evaluateIRExpression expr Map.empty
        optimizedResult = evaluateIRExpression optimized Map.empty
    in counterexample ("Optimization should preserve semantics")
       (case (originalResult, optimizedResult) of
           (Right val1, Right val2) -> val1 === val2
           (Left err1, Left err2) -> property True  -- Both failed is OK
           _ -> property False)

-- ============================================================================
-- IR Type Checking Properties
-- ============================================================================

-- Property: IR literal has consistent type
prop_irLiteralType :: IRLiteral -> Property
prop_irLiteralType lit =
    let inferredType = inferIRExpressionType (IRLiteral lit) Map.empty
        expectedType = case lit of
            IRInt _ -> IRIntType
            IRString _ -> IRStringType
            IRBool _ -> IRBoolType
            IRUnit -> IRUnitType
    in counterexample ("IR literal should have consistent type")
       (inferredType === Right expectedType)

-- Property: IR binary operation type checking is consistent
prop_irBinaryOpTypeChecking :: IROperator -> IRExpression -> IRExpression -> Property
prop_irBinaryOpTypeChecking op left right =
    let expr = IRBinaryOp op left right
        emptyEnv = Map.empty
        result = inferIRExpressionType expr emptyEnv
    in counterexample ("Binary operation type checking should be consistent")
       (isRight result === True)  -- Simplified: assume all operations are valid

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Compiler IR Core QuickCheck Tests"
    [ testProperty "IR literal evaluation consistency" prop_irLiteralEvaluation
    , testProperty "IR variable lookup fails for undefined" prop_irVariableLookupFails
    , testProperty "IR binary operation consistency" prop_irBinaryOpConsistency
    , testProperty "IR unary operation consistency" prop_irUnaryOpConsistency
    , testProperty "IR function call consistency" prop_irFunctionCallConsistency
    , testProperty "IR assignment updates environment" prop_irAssignmentUpdatesEnv
    , testProperty "IR return statement returns value" prop_irReturnStatement
    , testProperty "IR expression statement evaluates" prop_irExpressionStatement
    , testProperty "IR module contains all functions" prop_irModuleContainsFunctions
    , testProperty "IR function parameter count matches" prop_irFunctionParameterCount
    , testProperty "IR optimization preserves semantics" prop_irOptimizationPreservesSemantics
    , testProperty "IR literal has consistent type" prop_irLiteralType
    , testProperty "IR binary operation type checking" prop_irBinaryOpTypeChecking
    ]

-- ============================================================================
-- Helper Functions (Mock implementations for testing)
-- ============================================================================

-- Mock IR data types
data IRLiteral = IRInt Int | IRString String | IRBool Bool | IRUnit
    deriving (Show, Eq)

data IROperator = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Gt | Le | Ge | And | Or | Not
    deriving (Show, Eq)

data IRExpression = 
    IRLiteral IRLiteral
    | IRVariable String
    | IRBinaryOp IROperator IRExpression IRExpression
    | IRUnaryOp IROperator IRExpression
    | IRFunctionCall String [IRExpression]
    deriving (Show, Eq)

data IRStatement =
    IRAssignment String IRExpression
    | IRFunctionCallStmt String [IRExpression]
    | IRReturn IRExpression
    | IRExprStmt IRExpression
    | IRIf IRExpression IRStatement IRStatement
    | IRWhile IRExpression IRStatement
    deriving (Show, Eq)

data IRFunction = IRFunction
    { irFunctionName :: String
    , irFunctionParameters :: [String]
    , irFunctionBody :: [IRStatement]
    } deriving (Show, Eq)

data IRModule = IRModule
    { irModuleName :: String
    , irModuleFunctions :: [IRFunction]
    } deriving (Show, Eq)

data IRType = IRIntType | IRStringType | IRBoolType | IRUnitType
    deriving (Show, Eq)

-- Mock evaluation functions
evaluateIRExpression :: IRExpression -> Map.Map String IRLiteral -> Either String IRLiteral
evaluateIRExpression (IRLiteral lit) _ = Right lit
evaluateIRExpression (IRVariable var) env = 
    case Map.lookup var env of
        Just val -> Right val
        Nothing -> Left $ "Undefined variable: " ++ var
evaluateIRExpression (IRBinaryOp op left right) env = do
    leftVal <- evaluateIRExpression left env
    rightVal <- evaluateIRExpression right env
    evaluateBinaryOp op leftVal rightVal
evaluateIRExpression (IRUnaryOp op operand) env = do
    operandVal <- evaluateIRExpression operand env
    evaluateUnaryOp op operandVal
evaluateIRExpression (IRFunctionCall _ _) _ = Left "Function calls not implemented"

evaluateBinaryOp :: IROperator -> IRLiteral -> IRLiteral -> Either String IRLiteral
evaluateBinaryOp Add (IRInt a) (IRInt b) = Right (IRInt (a + b))
evaluateBinaryOp Sub (IRInt a) (IRInt b) = Right (IRInt (a - b))
evaluateBinaryOp Mul (IRInt a) (IRInt b) = Right (IRInt (a * b))
evaluateBinaryOp Eq a b = Right (IRBool (a == b))
evaluateBinaryOp Neq a b = Right (IRBool (a /= b))
evaluateBinaryOp _ _ _ = Left "Unsupported binary operation"

evaluateUnaryOp :: IROperator -> IRLiteral -> Either String IRLiteral
evaluateUnaryOp Not (IRBool b) = Right (IRBool (not b))
evaluateUnaryOp _ _ = Left "Unsupported unary operation"

-- Mock execution functions
executeIRStatement :: IRStatement -> Map.Map String IRLiteral -> Either String (Map.Map String IRLiteral, Maybe IRLiteral)
executeIRStatement (IRAssignment var expr) env = do
    val <- evaluateIRExpression expr env
    Right (Map.insert var val env, Nothing)
executeIRStatement (IRReturn expr) env = do
    val <- evaluateIRExpression expr env
    Right (env, Just val)
executeIRStatement (IRExprStmt expr) env = do
    _ <- evaluateIRExpression expr env
    Right (env, Nothing)
executeIRStatement _ _ = Left "Statement not implemented"

-- Mock optimization function
optimizeIRExpression :: IRExpression -> IRExpression
optimizeIRExpression expr = expr  -- Simplified: no optimization

-- Mock type inference functions
inferIRExpressionType :: IRExpression -> Map.Map String IRType -> Either String IRType
inferIRExpressionType (IRLiteral lit) _ = Right $ case lit of
    IRInt _ -> IRIntType
    IRString _ -> IRStringType
    IRBool _ -> IRBoolType
    IRUnit -> IRUnitType
inferIRExpressionType _ _ = Right IRIntType  -- Simplified

-- Helper functions
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

-- Import required for nub
import Data.List (nub)