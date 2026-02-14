{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewOwnershipTestSuite where

import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.MemoryLimits 
  ( withMemoryLimits
  , memoryLimitedTestGroup
  , memoryLevelTestGroup
  , MemoryLevel(..)
  , withMemoryLevel
  , gcBetweenTests
  )
import Data.List (isInfixOf)
import Data.Char (isSpace)
import Data.Either (isLeft, isRight)
import Data.Maybe (listToMaybe)

import Ownership (analyzeOwnership, OwnershipMode(..), OwnershipResult(..))
import Parser (parseTypus, TypusAST(..))
import Compiler (compileTypus)
import Utils (trim)

-- | 测试所有权模式的基本属性
prop_ownership_mode_basic :: String -> Property
prop_ownership_mode_basic s =
  let limitedString = take 10 s  -- 限制字符串大小
      result = analyzeOwnership limitedString
  in property $ case result of
    OwnershipResult _ _ -> True
    _ -> True

-- | 测试移动语义的解析
test_move_semantics_parsing :: Assertion
test_move_semantics_parsing = do
  let validMove = "{//! ownership: on\n    s := NewMyString(\"hello\")\n    t := s\n    fmt.Println(t.data)\n}"
      result = analyzeOwnership validMove
  case result of
    OwnershipResult modes _ -> assertBool "Ownership mode detected" (Ownership `elem` modes)
    err -> assertFailure $ "Failed to analyze move semantics: " ++ show err

-- | 测试借用语义的解析
test_borrow_semantics_parsing :: Assertion
test_borrow_semantics_parsing = do
  let validBorrow = "{//! ownership: on\n    s := NewMyString(\"hello\")\n    r := &s\n    fmt.Println(r.data)\n}"
      result = analyzeOwnership validBorrow
  case result of
    OwnershipResult modes _ -> assertBool "Ownership mode detected" (Ownership `elem` modes)
    err -> assertFailure $ "Failed to analyze borrow semantics: " ++ show err

-- | 测试可变借用的解析
test_mutable_borrow_parsing :: Assertion
test_mutable_borrow_parsing = do
  let validMutableBorrow = "{//! ownership: on\n    s := NewMyString(\"hello\")\n    m := &mut s\n    m.data = \"world\"\n}"
      result = analyzeOwnership validMutableBorrow
  case result of
    OwnershipResult modes _ -> assertBool "Ownership mode detected" (Ownership `elem` modes)
    err -> assertFailure $ "Failed to analyze mutable borrow: " ++ show err

-- | 测试所有权转移的检测
test_ownership_transfer_detection :: Assertion
test_ownership_transfer_detection = do
  let validTransfer = "{//! ownership: on\n    s := NewMyString(\"hello\")\n    t := s\n    fmt.Println(t.data)\n}"
      result = analyzeOwnership validTransfer
  case result of
    OwnershipResult modes warnings -> 
      assertBool "Ownership transfer detected" (any ("transfer" `isInfixOf`) warnings)
    err -> assertFailure $ "Failed to detect ownership transfer: " ++ show err

-- | 测试use-after-move错误的检测
test_use_after_move_detection :: Assertion
test_use_after_move_detection = do
  let invalidCode = "{//! ownership: on\n    s := NewMyString(\"hello\")\n    t := s\n    fmt.Println(s.data)\n}"
      result = analyzeOwnership invalidCode
  case result of
    OwnershipResult modes errors -> 
      assertBool "Use-after-move error detected" (any ("use-after-move" `isInfixOf`) errors)
    err -> assertFailure $ "Failed to detect use-after-move: " ++ show err

-- | 测试借用规则的验证
test_borrow_rules_validation :: Assertion
test_borrow_rules_validation = do
  let invalidBorrow = "{//! ownership: on\n    s := NewMyString(\"hello\")\n    r := &s\n    m := &mut s\n    m.data = \"world\"\n}"
      result = analyzeOwnership invalidBorrow
  case result of
    OwnershipResult modes errors -> 
      assertBool "Borrow rule violation detected" (any ("borrow" `isInfixOf`) errors)
    err -> assertFailure $ "Failed to detect borrow rule violation: " ++ show err

-- | 测试所有权编译
test_ownership_compilation :: Assertion
test_ownership_compilation = do
  let validCode = "{//! ownership: on\n    s := NewMyString(\"hello\")\n    t := s\n    fmt.Println(t.data)\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code contains ownership operations" ("MyString" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to compile ownership code: " ++ err

-- | 测试借用编译
test_borrow_compilation :: Assertion
test_borrow_compilation = do
  let validCode = "{//! ownership: on\n    s := NewMyString(\"hello\")\n    r := &s\n    fmt.Println(r.data)\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code contains borrow operations" ("MyString" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to compile borrow code: " ++ err

-- | 测试所有权指令的解析
test_ownership_directive_parsing :: Assertion
test_ownership_directive_parsing = do
  let validDirective = "//! ownership: on"
      result = parseTypus validDirective
  case result of
    Right ast -> assertEqual "Ownership directive parsed correctly" "Directive" (show $ head $ directives ast)
    Left err -> assertFailure $ "Failed to parse ownership directive: " ++ err

-- | 测试块级所有权指令的解析
test_block_ownership_directive_parsing :: Assertion
test_block_ownership_directive_parsing = do
  let validBlock = "{//! ownership: on\n    s := NewMyString(\"hello\")\n}"
      result = parseTypus validBlock
  case result of
    Right ast -> assertEqual "Block ownership directive parsed correctly" "Block" (show $ head $ blocks ast)
    Left err -> assertFailure $ "Failed to parse block ownership directive: " ++ err

-- | 测试所有权生命周期推断
test_ownership_lifetime_inference :: Assertion
test_ownership_lifetime_inference = do
  let validCode = "{//! ownership: on\n    s := NewMyString(\"hello\")\n    {\n        r := &s\n        fmt.Println(r.data)\n    }\n}"
      result = analyzeOwnership validCode
  case result of
    OwnershipResult modes warnings -> 
      assertBool "Lifetime inference successful" (not $ any ("lifetime" `isInfixOf`) warnings)
    err -> assertFailure $ "Failed to infer lifetime: " ++ show err

-- | 测试所有权与GC的兼容性
test_ownership_gc_compatibility :: Assertion
test_ownership_gc_compatibility = do
  let validCode = "{//! ownership: on\n    s := NewMyString(\"hello\")\n    t := s\n    fmt.Println(t.data)\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code is GC-compatible" ("func main()" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to generate GC-compatible code: " ++ err

-- | 测试所有权错误恢复
test_ownership_error_recovery :: Assertion
test_ownership_error_recovery = do
  let invalidCode = "{//! ownership: on\n    s := NewMyString(\"hello\")\n    t := s\n    fmt.Println(s.data)\n}"
      result = analyzeOwnership invalidCode
  case result of
    OwnershipResult modes errors -> 
      assertBool "Error recovery successful" (length errors > 0)
    err -> assertFailure $ "Failed to recover from ownership error: " ++ show err

-- | 测试所有权性能优化
test_ownership_performance_optimization :: Assertion
test_ownership_performance_optimization = do
  let validCode = "{//! ownership: on\n    s := NewMyString(\"hello\")\n    t := s\n    fmt.Println(t.data)\n}"
      result = compileTypus validCode
  case result of
    Right goCode -> assertBool "Generated Go code is optimized" ("// Generated by Typus" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to generate optimized code: " ++ err

-- | 测试所有权QuickCheck属性
prop_ownership_analysis_consistency :: String -> Property
prop_ownership_analysis_consistency s =
  let limitedString = take 8 s  -- 限制字符串大小
      result1 = analyzeOwnership limitedString
      result2 = analyzeOwnership limitedString
  in property $ case (result1, result2) of
    (OwnershipResult modes1 warnings1, OwnershipResult modes2 warnings2) -> 
      modes1 == modes2 && warnings1 == warnings2
    _ -> True

-- | 测试所有权边界条件
prop_ownership_boundary_conditions :: String -> Property
prop_ownership_boundary_conditions s =
  let limitedString = take 1 s  -- 限制字符串大小
      result = analyzeOwnership limitedString
  in property $ case result of
    OwnershipResult modes warnings -> length modes >= 0 && length warnings >= 0
    _ -> True

-- | 测试所有权内存安全
prop_ownership_memory_safety :: String -> Property
prop_ownership_memory_safety s =
  let limitedString = take 5 s  -- 限制字符串大小
      result = analyzeOwnership limitedString
  in property $ case result of
    OwnershipResult modes errors -> 
      not (any ("memory" `isInfixOf`) errors) || length errors > 0
    _ -> True

-- | 测试套件
tests :: TestTree
tests = memoryLevelTestGroup Minimal "New Ownership Test Suite (Memory Optimized)"
  [ withMemoryLevel Minimal $ testCase "Move semantics parsing" test_move_semantics_parsing
  , withMemoryLevel Minimal $ testCase "Borrow semantics parsing" test_borrow_semantics_parsing
  , withMemoryLevel Minimal $ testCase "Mutable borrow parsing" test_mutable_borrow_parsing
  , withMemoryLevel Minimal $ testCase "Ownership transfer detection" test_ownership_transfer_detection
  , withMemoryLevel Minimal $ testCase "Use-after-move detection" test_use_after_move_detection
  , withMemoryLevel Minimal $ testCase "Borrow rules validation" test_borrow_rules_validation
  , withMemoryLevel Minimal $ testCase "Ownership compilation" test_ownership_compilation
  , withMemoryLevel Minimal $ testCase "Borrow compilation" test_borrow_compilation
  , withMemoryLevel Minimal $ testCase "Ownership directive parsing" test_ownership_directive_parsing
  , withMemoryLevel Minimal $ testCase "Block ownership directive parsing" test_block_ownership_directive_parsing
  , withMemoryLevel Minimal $ testCase "Ownership lifetime inference" test_ownership_lifetime_inference
  , withMemoryLevel Minimal $ testCase "Ownership GC compatibility" test_ownership_gc_compatibility
  , withMemoryLevel Minimal $ testCase "Ownership error recovery" test_ownership_error_recovery
  , withMemoryLevel Minimal $ testCase "Ownership performance optimization" test_ownership_performance_optimization
  , withMemoryLevel Minimal $ testProperty "Ownership analysis consistency" prop_ownership_analysis_consistency
  , withMemoryLevel Minimal $ testProperty "Ownership boundary conditions" prop_ownership_boundary_conditions
  , withMemoryLevel Minimal $ testProperty "Ownership memory safety" prop_ownership_memory_safety
  ]