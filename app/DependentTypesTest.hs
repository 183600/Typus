{-# LANGUAGE OverloadedStrings #-}

module Main where

import Dependencies (
    DependentTypeChecker,
    DependentTypeError(..),
    TypeVar(..),
    Constraint(..),
    newDependentTypeChecker,
    analyzeDependentTypes,
    checkType,
    addType,
    addConstraint,
    solveConstraints,
    checkTypeInstantiation,
    getDependentTypeErrors
    )
import DependentTypesParser (
    DependentTypesParser,
    DependentType(..),
    TypeParameter(..),
    TypeConstraint(..),
    parseDependentType,
    parseTypeDeclaration,
    validateDependentTypeSyntax,
    DependentParseResult,
    runDependentTypesParser
    )
import Control.Monad.State
import qualified Data.Map.Strict as Map

-- 测试依赖类型分析的主函数
main :: IO ()
main = do
    putStrLn "Testing dependent types implementation..."
    
    -- 测试1: 基本依赖类型解析
    putStrLn "\nTest 1: Basic dependent type parsing"
    testBasicParsing
    
    -- 测试2: 依赖类型语义分析
    putStrLn "\nTest 2: Dependent type semantic analysis"
    testSemanticAnalysis
    
    -- 测试3: 约束求解
    putStrLn "\nTest 3: Constraint solving"
    testConstraintSolving
    
    -- 测试4: 类型检查
    putStrLn "\nTest 4: Type checking"
    testTypeChecking
    
    putStrLn "\nAll tests completed."

-- 测试基本依赖类型解析
testBasicParsing :: IO ()
testBasicParsing = do
    let testCode1 = "type NonEmptySlice<T> where len T > 0 struct { data []T }"
    case runDependentTypesParser testCode1 of
        Left err -> putStrLn $ "  Parsing failed: " ++ err
        Right (dependentType, _) -> putStrLn $ "  Parsed successfully: " ++ show dependentType

    let testCode2 = "type Vector<N int> where N >= 0 struct { data []int }"
    case runDependentTypesParser testCode2 of
        Left err -> putStrLn $ "  Parsing failed: " ++ err
        Right (dependentType, _) -> putStrLn $ "  Parsed successfully: " ++ show dependentType

-- 测试依赖类型语义分析
testSemanticAnalysis :: IO ()
testSemanticAnalysis = do
    let testCode = unlines [
            "//! dependent_types: on",
            "",
            "package main",
            "",
            "type NonEmptySlice<T> where len T > 0 struct {",
            "    data []T",
            "}",
            "",
            "type Vector struct {",
            "    data []int",
            "}",
            "",
            "func main() {",
            "    var slice NonEmptySlice<int>",
            "    var vec Vector",
            "}"
            ]
    
    let errors = analyzeDependentTypes testCode
    if null errors
        then putStrLn "  Semantic analysis passed with no errors"
        else do
            putStrLn $ "  Semantic analysis found " ++ show (length errors) ++ " errors:"
            mapM_ (\err -> putStrLn $ "    " ++ show err) (take 5 errors)

-- 测试约束求解
testConstraintSolving :: IO ()
testConstraintSolving = do
    -- 创建一个简单的约束求解测试
    let checker = newDependentTypeChecker
    let result = execState (addConstraintsAndSolve checker) checker
    putStrLn $ "  Constraint solving completed"

-- 添加约束并求解的辅助函数
addConstraintsAndSolve :: DependentTypeChecker -> State DependentTypeChecker Bool
addConstraintsAndSolve checker = do
    -- 添加一些约束
    addConstraint (Equal (Concrete "int") (Concrete "int"))
    addConstraint (Subtype (Concrete "int") (Concrete "interface{}"))
    -- 尝试求解约束
    solved <- solveConstraints
    return solved

-- 测试类型检查
testTypeChecking :: IO ()
testTypeChecking = do
    -- 创建一个简单的类型检查测试
    let checker = newDependentTypeChecker
    let result = execState (checkSomeTypes checker) checker
    
    let errors = getDependentTypeErrors result
    if null errors
        then putStrLn "  Type checking passed with no errors"
        else do
            putStrLn $ "  Type checking found " ++ show (length errors) ++ " errors:"
            mapM_ (\err -> putStrLn $ "    " ++ show err) (take 5 errors)

-- 检查一些类型的辅助函数
checkSomeTypes :: DependentTypeChecker -> State DependentTypeChecker ()
checkSomeTypes checker = do
    -- 检查一些类型
    checkType (Concrete "int")
    checkType (Slice (Concrete "int"))
    checkType (Application (Concrete "Vector") [Concrete "int"])