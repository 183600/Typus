{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.OwnershipQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import TestSupport.MemoryLimits 
  ( withMemoryLimits
  , memoryLimitedTestGroup
  , memoryLevelTestGroup
  , MemoryLevel(..)
  , withMemoryLevel
  , gcBetweenTests
  )
import TestSupport.EnhancedMemoryOptimization 
  ( enhancedMemoryCleanup
  , strategicMemoryCleanup
  , cleanupBetweenTests
  , withEnhancedMemoryControl
  , withStrictMemoryLimits
  , applyMemoryOptimizations
  )
import TestSupport.OptimizedStringOperations 
  ( genMinimalString
  , genUltraMinimalString
  , safeTake
  , safeLength
  , efficientTrim
  , efficientIsEmpty
  , withUltraStringLimit
  , minimizeStringUsage
  , optimizeStringProperty
  )
import TestSupport.TestPropertyMemoryCleanup 
  ( testGroupWithCleanup
  , testGroupWithStrategicCleanup
  , memoryAwareProperty
  , memoryOptimizedProperty
  , withPropertyMemoryCleanup
  )

import Ownership
import Parser (parseTypus)
import Data.List (isInfixOf, isPrefixOf)
import Data.Char (isSpace, isDigit)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing)

-- | 测试所有权指令的解析
prop_ownership_directive_parsing :: String -> Property
prop_ownership_directive_parsing directiveValue =
  let validDirective = directiveValue `elem` ["on", "off"]
      directiveExpr = "//! ownership: " ++ directiveValue
  in if not validDirective
     then property $ isLeft (parseTypus directiveExpr)
     else property $ isRight (parseTypus directiveExpr)

-- | 测试块级所有权指令的解析
prop_block_ownership_directive_parsing :: String -> Property
prop_block_ownership_directive_parsing directiveValue =
  let validDirective = directiveValue `elem` ["on", "off"]
      blockExpr = "{//! ownership: " ++ directiveValue ++ "\n  // code\n}"
  in if not validDirective
     then property $ isLeft (parseTypus blockExpr)
     else property $ isRight (parseTypus blockExpr)

-- | 测试移动语义的解析
prop_move_semantics_parsing :: String -> String -> Property
prop_move_semantics_parsing varName1 varName2 =
  let validNames = not (null varName1) && not (null varName2) && 
                   all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") varName1 ++ varName2
      moveExpr = varName1 ++ " := " ++ varName2
  in if not validNames
     then property $ isLeft (parseTypus moveExpr)
     else property $ isRight (parseTypus moveExpr)

-- | 测试不可变借用的解析
prop_immutable_borrow_parsing :: String -> String -> Property
prop_immutable_borrow_parsing varName1 varName2 =
  let validNames = not (null varName1) && not (null varName2) && 
                   all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") (varName1 ++ varName2)
      borrowExpr = varName1 ++ " := &" ++ varName2
  in if not validNames
     then property $ isLeft (parseTypus borrowExpr)
     else property $ isRight (parseTypus borrowExpr)

-- | 测试可变借用的解析
prop_mutable_borrow_parsing :: String -> String -> Property
prop_mutable_borrow_parsing varName1 varName2 =
  let validNames = not (null varName1) && not (null varName2) && 
                   all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") (varName1 ++ varName2)
      borrowExpr = varName1 ++ " := &mut " ++ varName2
  in if not validNames
     then property $ isLeft (parseTypus borrowExpr)
     else property $ isRight (parseTypus borrowExpr)

-- | 测试借用规则检查
prop_borrow_rules_checking :: String -> String -> String -> Property
prop_borrow_rules_checking varName1 varName2 varName3 =
  let validNames = not (null varName1) && not (null varName2) && not (null varName3) && 
                   all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") (varName1 ++ varName2 ++ varName3)
      -- 创建同时存在多个不可变借用的代码
      borrowExpr = varName1 ++ " := &" ++ varName2 ++ "\n" ++ varName3 ++ " := &" ++ varName2
  in if not validNames
     then property $ isLeft (parseTypus borrowExpr)
     else property $ isRight (parseTypus borrowExpr)

-- | 测试可变借用排他性检查
prop_mutable_borrow_exclusivity :: String -> String -> String -> Property
prop_mutable_borrow_exclusivity varName1 varName2 varName3 =
  let validNames = not (null varName1) && not (null varName2) && not (null varName3) && 
                   all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") (varName1 ++ varName2 ++ varName3)
      -- 创建同时存在可变借用和不可变借用的代码（应该失败）
      borrowExpr = varName1 ++ " := &mut " ++ varName2 ++ "\n" ++ varName3 ++ " := &" ++ varName2
  in if not validNames
     then property $ isLeft (parseTypus borrowExpr)
     else property $ isLeft (parseTypus borrowExpr)  -- 这应该总是失败，因为违反了借用规则

-- | 测试所有权转移的解析
prop_ownership_transfer_parsing :: String -> String -> Property
prop_ownership_transfer_parsing funcName varName =
  let validNames = not (null funcName) && not (null varName) && 
                   all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") (funcName ++ varName)
      transferExpr = funcName ++ "(" ++ varName ++ ")"
  in if not validNames
     then property $ isLeft (parseTypus transferExpr)
     else property $ isRight (parseTypus transferExpr)

-- | 测试生命周期推断的解析
prop_lifetime_inference_parsing :: String -> String -> Property
prop_lifetime_inference_parsing funcName paramName =
  let validNames = not (null funcName) && not (null paramName) && 
                   all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") (funcName ++ paramName)
      lifetimeExpr = "func " ++ funcName ++ "(" ++ paramName ++ ": &string) -> string"
  in if not validNames
     then property $ isLeft (parseTypus lifetimeExpr)
     else property $ isRight (parseTypus lifetimeExpr)

-- | 测试跨goroutine所有权转移的解析
prop_cross_goroutine_ownership_parsing :: String -> String -> Property
prop_cross_goroutine_ownership_parsing chanName varName =
  let validNames = not (null chanName) && not (null varName) && 
                   all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") (chanName ++ varName)
      goroutineExpr = "go func() {\n  " ++ chanName ++ " <- " ++ varName ++ "\n}()"
  in if not validNames
     then property $ isLeft (parseTypus goroutineExpr)
     else property $ isRight (parseTypus goroutineExpr)

-- | 测试所有权与接口交互的解析
prop_ownership_interface_interaction_parsing :: String -> String -> Property
prop_ownership_interface_interaction_parsing interfaceName methodName =
  let validNames = not (null interfaceName) && not (null methodName) && 
                   all (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_") (interfaceName ++ methodName)
      interfaceExpr = "func (" ++ interfaceName ++ ") " ++ methodName ++ "()"
  in if not validNames
     then property $ isLeft (parseTypus interfaceExpr)
     else property $ isRight (parseTypus interfaceExpr)

-- | 测试所有权机制的边界情况
test_ownership_edge_cases :: Assertion
test_ownership_edge_cases = do
  -- 测试空变量名
  assertBool "Empty variable name should fail" $ isLeft (parseTypus " := &x")
  
  -- 测试无效的指令值
  assertBool "Invalid directive value should fail" $ isLeft (parseTypus "//! ownership: maybe")
  
  -- 测试无效的借用表达式
  assertBool "Invalid borrow expression should fail" $ isLeft (parseTypus "x := &")
  
  -- 测试无效的转移表达式
  assertBool "Invalid transfer expression should fail" $ isLeft (parseTypus "func()")

-- | 测试所有权机制的复杂表达式
test_ownership_complex_expressions :: Assertion
test_ownership_complex_expressions = do
  -- 测试复杂的所有权转移
  assertBool "Complex ownership transfer should succeed" $ isRight (parseTypus "result := process(move(data))")
  
  -- 测试复杂的借用链
  assertBool "Complex borrow chain should succeed" $ isRight (parseTypus "r := &s\nm := &mut r.data")
  
  -- 测试复杂的生命周期
  assertBool "Complex lifetime should succeed" $ isRight (parseTypus "func process(data: &mut Data) -> &Result")

-- | 所有权测试套件
tests :: TestTree
tests = testGroupWithStrategicCleanup "Ownership QuickCheck Tests"
  [ -- 基本指令解析测试
    memoryOptimizedProperty "Ownership directive parsing" (property prop_ownership_directive_parsing)
  , memoryOptimizedProperty "Block ownership directive parsing" (property prop_block_ownership_directive_parsing)
  
  -- 移动语义测试
  , memoryOptimizedProperty "Move semantics parsing" (property prop_move_semantics_parsing)
  , memoryOptimizedProperty "Ownership transfer parsing" (property prop_ownership_transfer_parsing)
  
  -- 借用测试
  , memoryOptimizedProperty "Immutable borrow parsing" (property prop_immutable_borrow_parsing)
  , memoryOptimizedProperty "Mutable borrow parsing" (property prop_mutable_borrow_parsing)
  , memoryOptimizedProperty "Borrow rules checking" (property prop_borrow_rules_checking)
  , memoryOptimizedProperty "Mutable borrow exclusivity" (property prop_mutable_borrow_exclusivity)
  
  -- 高级特性测试
  , memoryOptimizedProperty "Lifetime inference parsing" (property prop_lifetime_inference_parsing)
  , memoryOptimizedProperty "Cross goroutine ownership parsing" (property prop_cross_goroutine_ownership_parsing)
  , memoryOptimizedProperty "Ownership interface interaction parsing" (property prop_ownership_interface_interaction_parsing)
  
  -- 单元测试
  , testCase "Ownership edge cases" test_ownership_edge_cases
  , testCase "Ownership complex expressions" test_ownership_complex_expressions
  ]

-- | 内存优化的测试套件
memoryOptimizedTests :: TestTree
memoryOptimizedTests = memoryLevelTestGroup Minimal "Ownership Memory Optimized Tests"
  [ testProperty "Ownership directive" prop_ownership_directive_parsing
  , testProperty "Move semantics" prop_move_semantics_parsing
  , testProperty "Immutable borrow" prop_immutable_borrow_parsing
  , testProperty "Mutable borrow" prop_mutable_borrow_parsing
  , testProperty "Borrow rules" prop_borrow_rules_checking
  ]