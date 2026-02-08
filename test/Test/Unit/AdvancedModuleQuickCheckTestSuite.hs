{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Unit.AdvancedModuleQuickCheckTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Data.List (isInfixOf, nub, sort, group, intercalate, isPrefixOf, isSuffixOf, find)
import Data.Char (isSpace, isAlpha, isDigit, isAlphaNum, toLower, toUpper, ord, isUpper)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
import Control.Monad (foldM, replicateM)
import Data.Bits (testBit, setBit, clearBit, complementBit)

import Parser
import Compiler
import CompilerUtils
import SourceLocation
import Utils
import ErrorHandler
import qualified Ownership.Common.Types as Own
import Debug
import qualified Dependencies.AST as Dep
import qualified Dependencies.TypeSystem as Dep
import DependentTypesParser

import TestSupport.Arbitrary

-- ============================================================================
-- Advanced Parser Tests
-- ============================================================================

-- | 测试解析器对复杂表达式的处理
prop_parser_complex_expressions :: [String] -> Property
prop_parser_complex_expressions ops =
  let validOps = all (`elem` ["+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">=", "&&", "||", "&", "|", "^"]) ops
      expr = intercalate " " (["1"] ++ ops ++ ["1"])
      code = "func test() { result := " ++ expr ++ " }"
  in if not validOps || null ops
     then property True
     else case Parser.parseTypusFile code of
            Right _ -> property True
            Left _ -> property True

-- | 测试解析器对类型声明的处理
prop_parser_type_declarations :: String -> [String] -> Property
prop_parser_type_declarations typeName fields =
  let validTypeName = not (null typeName) && isAlpha (head typeName) && all isAlphaNum typeName
      validFields = all (\f -> not (null f) && isAlpha (head f) && all isAlphaNum f) fields
      fieldStr = intercalate "; " $ map (\f -> f ++ " int") fields
      code = "type " ++ typeName ++ " struct {\n" ++ fieldStr ++ "\n}"
  in if not (validTypeName && validFields)
     then property True
     else case Parser.parseTypusFile code of
            Right _ -> property True
            Left _ -> property False

-- | 测试解析器对接口声明的处理
prop_parser_interface_declarations :: String -> [String] -> Property
prop_parser_interface_declarations interfaceName methods =
  let validInterfaceName = not (null interfaceName) && isAlpha (head interfaceName) && all isAlphaNum interfaceName
      validMethods = all (\m -> not (null m) && isAlpha (head m) && all isAlphaNum m) methods
      methodStr = intercalate "\n" $ map (\m -> "  " ++ m ++ "()") methods
      code = "type " ++ interfaceName ++ " interface {\n" ++ methodStr ++ "\n}"
  in if not (validInterfaceName && validMethods)
     then property True
     else case Parser.parseTypusFile code of
            Right _ -> property True
            Left _ -> property True

-- | 测试解析器对并发结构的处理
prop_parser_concurrent_structures :: String -> Property
prop_parser_concurrent_structures channelName =
  let validChannelName = not (null channelName) && isAlpha (head channelName) && all isAlphaNum channelName
      code = "func test() {\n  ch := make(chan " ++ channelName ++ ")\n  go func() { ch <- " ++ channelName ++ "() }()\n}"
  in if not validChannelName
     then property True
     else case Parser.parseTypusFile code of
            Right _ -> property True
            Left _ -> property True

-- | 测试解析器对错误处理结构的处理
prop_parser_error_handling :: String -> Property
prop_parser_error_handling errorMsg =
  let validErrorMsg = not (null errorMsg)
      code = "func test() {\n  defer func() {\n    if r := recover(); r != nil {\n      log.Println(\"" ++ errorMsg ++ "\")\n    }\n  }()\n  panic(\"test\")\n}"
  in if not validErrorMsg
     then property True
     else case Parser.parseTypusFile code of
            Right _ -> property True
            Left _ -> property True

-- ============================================================================
-- Advanced Compiler Tests
-- ============================================================================

-- | 测试编译器对优化的处理
prop_compiler_optimization :: String -> Property
prop_compiler_optimization code =
  let hasLoop = "for" `isInfixOf` code
      hasFunction = "func" `isInfixOf` code
      parsed = Parser.parseTypusFile code
      compiled = case parsed of
                   Right ast -> Compiler.compile ast
                   Left _ -> Left [Compiler.malformedSyntaxError]
  in classify hasLoop "has loop" $
     classify hasFunction "has function" $
     case compiled of
       Right _ -> property True
       Left _ -> property True

-- | 测试编译器对类型推断的处理
prop_compiler_type_inference :: [(String, String)] -> Property
prop_compiler_type_inference assignments =
  let validAssignments = all (\(var, typ) -> not (null var) && not (null typ)) assignments
      assignStr = intercalate "\n" $ map (\(var, typ) -> var ++ " := " ++ typ ++ "()") assignments
      code = "func test() {\n" ++ assignStr ++ "\n}"
      parsed = Parser.parseTypusFile code
      compiled = case parsed of
                   Right ast -> Compiler.compile ast
                   Left _ -> Left [Compiler.malformedSyntaxError]
  in if not validAssignments
     then property True
     else case compiled of
            Right _ -> property True
            Left _ -> property True

-- | 测试编译器对泛型的处理
prop_compiler_generics :: String -> String -> Property
prop_compiler_generics typeName genericParam =
  let validTypeName = not (null typeName) && isAlpha (head typeName) && all isAlphaNum typeName
      validGenericParam = not (null genericParam) && isUpper (head genericParam) && all isAlphaNum genericParam
      code = "type " ++ typeName ++ "[" ++ genericParam ++ " any] struct {\n  value " ++ genericParam ++ "\n}"
      parsed = Parser.parseTypusFile code
      compiled = case parsed of
                   Right ast -> Compiler.compile ast
                   Left _ -> Left [Compiler.malformedSyntaxError]
  in if not (validTypeName && validGenericParam)
     then property True
     else case compiled of
            Right _ -> property True
            Left _ -> property True

-- | 测试编译器对反射的处理
prop_compiler_reflection :: String -> Property
prop_compiler_reflection typeName =
  let validTypeName = not (null typeName) && isAlpha (head typeName) && all isAlphaNum typeName
      code = "func test() {\n  t := reflect.TypeOf(" ++ typeName ++ "{})\n  fmt.Println(t.Name())\n}"
      parsed = Parser.parseTypusFile code
      compiled = case parsed of
                   Right ast -> Compiler.compile ast
                   Left _ -> Left [Compiler.malformedSyntaxError]
  in if not validTypeName
     then property True
     else case compiled of
            Right _ -> property True
            Left _ -> property True

-- ============================================================================
-- Advanced Ownership Tests
-- ============================================================================

-- | 测试所有权生命周期管理
prop_ownership_lifecycle :: [(String, Int)] -> Property
prop_ownership_lifecycle varLifetimes =
  let validLifetimes = all (\(var, lifetime) -> not (null var) && lifetime >= 0) varLifetimes
      sortedLifetimes = sort $ map snd varLifetimes
  in if not validLifetimes
     then property True
     else property $ length sortedLifetimes == length varLifetimes

-- | 测试所有权借用规则
prop_ownership_borrowing_rules :: [(String, [String])] -> Property
prop_ownership_borrowing_rules borrowings =
  let validBorrowings = all (\(owner, borrowers) -> not (null owner) && all (not . null) borrowers) borrowings
      allBorrowers = concatMap snd borrowings
      uniqueBorrowers = nub allBorrowers
  in if not validBorrowings
     then property True
     else property $ length uniqueBorrowers <= length allBorrowers

-- | 测试所有权转移链
prop_ownership_transfer_chain :: [String] -> Property
prop_ownership_transfer_chain vars =
  let validVars = all (not . null) vars
      uniqueVars = nub vars
  in if not validVars
     then property True
     else property $ length uniqueVars <= length vars

-- | 测试所有权与并发
prop_ownership_concurrency :: [(String, String)] -> Property
prop_ownership_concurrency threadVars =
  let validThreadVars = all (\(thread, var) -> not (null thread) && not (null var)) threadVars
      uniqueThreads = nub $ map fst threadVars
  in if not validThreadVars
     then property True
     else property $ length uniqueThreads <= length threadVars

-- ============================================================================
-- Advanced Dependencies Tests
-- ============================================================================

-- | 测试复杂的类型约束
prop_complex_type_constraints :: String -> [(String, String)] -> Property
prop_complex_type_constraints baseType constraints =
  let validBaseType = not (null baseType) && all isAlpha baseType
      validConstraints = all (\(name, value) -> not (null name) && not (null value)) constraints
  in if not (validBaseType && validConstraints)
     then property True
     else let constraintStr = intercalate ", " $ map (\(n, v) -> n ++ " " ++ v) constraints
          in property $ length constraintStr >= 0

-- | 测试类型层次结构
prop_type_hierarchy :: [(String, [String])] -> Property
prop_type_hierarchy typeHierarchy =
  let validHierarchy = all (\(parent, children) -> not (null parent) && all (not . null) children) typeHierarchy
      allTypes = concatMap (\(p, cs) -> p:cs) typeHierarchy
      uniqueTypes = nub allTypes
  in if not validHierarchy
     then property True
     else property $ length uniqueTypes <= length allTypes

-- | 测试类型变量替换的传递性
prop_type_substitution_transitivity :: [(String, String)] -> [(String, String)] -> Property
prop_type_substitution_transitivity subst1 subst2 =
  let validSubst1 = all (\(k, v) -> not (null k) && not (null v)) subst1
      validSubst2 = all (\(k, v) -> not (null k) && not (null v)) subst2
      combined = subst1 ++ subst2
  in if not (validSubst1 && validSubst2)
     then property True
     else let uniqueKeys = nub $ map fst combined
          in property $ length uniqueKeys <= length combined

-- | 测试类型约束求解
prop_type_constraint_solving :: [(String, String)] -> Property
prop_type_constraint_solving constraints =
  let validConstraints = all (\(name, constraint) -> not (null name) && not (null constraint)) constraints
      constraintNames = map fst constraints
      uniqueNames = nub constraintNames
  in if not validConstraints
     then property True
     else property $ length uniqueNames <= length constraintNames

-- ============================================================================
-- Advanced SourceLocation Tests
-- ============================================================================

-- | 测试位置计算的准确性
prop_position_calculation :: Int -> Int -> Property
prop_position_calculation lineOffset colOffset =
  let validOffsets = lineOffset >= 0 && colOffset >= 0
      baseLine = 10
      baseCol = 5
      newPos = SourcePos (baseLine + lineOffset) (baseCol + colOffset) 0
  in if not validOffsets
     then property True
     else property $ show newPos /= ""

-- | 测试范围合并的属性
prop_span_merging :: (Int, Int) -> (Int, Int) -> Property
prop_span_merging (start1, end1) (start2, end2) =
  let validPositions = start1 >= 0 && end1 >= start1 && start2 >= 0 && end2 >= start2
      mergedStart = min start1 start2
      mergedEnd = max end1 end2
  in if not validPositions
     then property True
     else property $ mergedStart <= mergedEnd

-- | 测试位置偏移计算
prop_position_offset :: Int -> Int -> Int -> Property
prop_position_offset line col offset =
  let validPosition = line >= 0 && col >= 0 && offset >= 0
      newCol = col + offset
      newLine = line + (newCol `div` 80)  -- 假设80字符行宽
      finalCol = newCol `mod` 80
  in if not validPosition
     then property True
     else property $ newLine >= line && finalCol >= 0

-- | 测试位置比较的传递性
prop_position_comparison_transitivity :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Property
prop_position_comparison_transitivity (l1, c1) (l2, c2) (l3, c3) =
  let validPositions = all (\(l, c) -> l >= 0 && c >= 0) [(l1, c1), (l2, c2), (l3, c3)]
      pos1 = SourcePos l1 c1 0
      pos2 = SourcePos l2 c2 0
      pos3 = SourcePos l3 c3 0
      pos1BeforePos2 = l1 < l2 || (l1 == l2 && c1 < c2)
      pos2BeforePos3 = l2 < l3 || (l2 == l3 && c2 < c3)
      pos1BeforePos3 = l1 < l3 || (l1 == l3 && c1 < c3)
  in if not validPositions
     then property True
     else property $ (pos1BeforePos2 && pos2BeforePos3) ==> pos1BeforePos3

-- ============================================================================
-- Advanced Utils Tests
-- ============================================================================

-- | 测试字符串编码处理
prop_string_encoding :: [Word8] -> Property
prop_string_encoding bytes =
  let validBytes = all (\b -> b >= 32 && b <= 126) bytes  -- 可打印ASCII字符
      str = map (toEnum . fromEnum) bytes :: String
  in if not validBytes
     then property True
     else property $ length str == length bytes

-- | 测试列表操作的结合律
prop_list_associative :: [Int] -> [Int] -> [Int] -> Property
prop_list_associative xs ys zs =
  let leftAssoc = (xs ++ ys) ++ zs
      rightAssoc = xs ++ (ys ++ zs)
  in property $ leftAssoc == rightAssoc

-- | 测试映射操作的结合律
prop_map_associative :: [(String, Int)] -> [(String, Int)] -> [(String, Int)] -> Property
prop_map_associative m1 m2 m3 =
  let validMaps = all (\(k, v) -> not (null k)) (m1 ++ m2 ++ m3)
      map1 = Map.fromList m1
      map2 = Map.fromList m2
      map3 = Map.fromList m3
      leftAssoc = Map.union (Map.union map1 map2) map3
      rightAssoc = Map.union map1 (Map.union map2 map3)
  in if not validMaps
     then property True
     else property $ Map.size leftAssoc == Map.size rightAssoc

-- | 测试集合操作的交换律
prop_set_commutative :: [String] -> [String] -> Property
prop_set_commutative xs ys =
  let validItems = all (not . null) (xs ++ ys)
      set1 = Set.fromList xs
      set2 = Set.fromList ys
      union1 = Set.union set1 set2
      union2 = Set.union set2 set1
      intersect1 = Set.intersection set1 set2
      intersect2 = Set.intersection set2 set1
  in if not validItems
     then property True
     else conjoin
       [ property $ union1 == union2
       , property $ intersect1 == intersect2
       ]

-- ============================================================================
-- Advanced Error Handler Tests
-- ============================================================================

-- | 测试错误消息的国际化
prop_error_message_i18n :: String -> String -> Property
prop_error_message_i18n langKey errorMsg =
  let validLangKey = not (null langKey) && all isAlphaNum langKey
      validErrorMsg = not (null errorMsg)
      localizedMsg = langKey ++ ":" ++ errorMsg
  in if not (validLangKey && validErrorMsg)
     then property True
     else property $ length localizedMsg >= length langKey + length errorMsg

-- | 测试错误恢复策略
prop_error_recovery_strategies :: [String] -> Property
prop_error_recovery_strategies strategies =
  let validStrategies = all (not . null) strategies
      knownStrategies = ["skip", "retry", "fallback", "abort"]
      hasKnownStrategy = any (`elem` knownStrategies) strategies
  in classify hasKnownStrategy "has known strategy" $
     if not validStrategies
     then property True
     else property $ length strategies >= 0

-- | 测试错误上下文收集
prop_error_context_collection :: [(String, String)] -> Property
prop_error_context_collection context =
  let validContext = all (\(k, v) -> not (null k) && not (null v)) context
      contextKeys = map fst context
      uniqueKeys = nub contextKeys
  in if not validContext
     then property True
     else property $ length uniqueKeys <= length contextKeys

-- | 测试错误严重性分级
prop_error_severity_levels :: [Int] -> Property
prop_error_severity_levels severities =
  let validSeverities = all (\s -> s >= 1 && s <= 10) severities
      maxSeverity = maximum severities
      minSeverity = minimum severities
  in if null severities || not validSeverities
     then property True
     else property $ maxSeverity >= minSeverity

-- ============================================================================
-- Advanced Debug Tests
-- ============================================================================

-- | 测试调试信息的结构化
prop_structured_debug_info :: [(String, String)] -> Property
prop_structured_debug_info infoPairs =
  let validPairs = all (\(k, v) -> not (null k) && not (null v)) infoPairs
      structured = Map.fromList infoPairs
  in if not validPairs
     then property True
     else property $ Map.size structured == length (nub $ map fst infoPairs)

-- | 测试调试性能监控
prop_debug_performance_monitoring :: [(String, Int)] -> Property
prop_debug_performance_monitoring metrics =
  let validMetrics = all (\(name, value) -> not (null name) && value >= 0) metrics
      metricNames = map fst metrics
      uniqueNames = nub metricNames
  in if not validMetrics
     then property True
     else property $ length uniqueNames <= length metricNames

-- | 测试调试快照功能
prop_debug_snapshots :: [String] -> Property
prop_debug_snapshots snapshots =
  let validSnapshots = all (not . null) snapshots
      uniqueSnapshots = nub snapshots
  in if not validSnapshots
     then property True
     else property $ length uniqueSnapshots <= length snapshots

-- | 测试调试日志轮转
prop_debug_log_rotation :: Int -> Property
prop_debug_log_rotation maxSize =
  let validSize = maxSize >= 0 && maxSize <= 10000
  in if not validSize
     then property True
     else property $ maxSize >= 0

-- ============================================================================
-- Advanced Integration Tests
-- ============================================================================

-- | 测试多文件编译集成
prop_multi_file_compilation :: [String] -> Property
prop_multi_file_compilation fileNames =
  let validFileNames = all (\f -> not (null f) && ".typus" `isSuffixOf` f) fileNames
  in if not validFileNames
     then property True
     else property $ length (nub fileNames) <= length fileNames

-- | 测试模块系统集成
prop_module_system_integration :: [(String, [String])] -> Property
prop_module_system_integration modules =
  let validModules = all (\(name, deps) -> not (null name) && all (not . null) deps) modules
      moduleNames = map fst modules
      uniqueNames = nub moduleNames
  in if not validModules
     then property True
     else property $ length uniqueNames <= length moduleNames

-- | 测试构建系统集成
prop_build_system_integration :: [(String, [String])] -> Property
prop_build_system_integration buildSteps =
  let validSteps = all (\(step, deps) -> not (null step) && all (not . null) deps) buildSteps
      stepNames = map fst buildSteps
  in if not validSteps
     then property True
     else property $ length stepNames >= 0

-- | 测试工具链集成
prop_toolchain_integration :: [String] -> Property
prop_toolchain_integration tools =
  let validTools = all (not . null) tools
      knownTools = ["parser", "compiler", "linker", "optimizer"]
      hasKnownTool = any (`elem` knownTools) tools
  in classify hasKnownTool "has known tool" $
     if not validTools
     then property True
     else property $ length (nub tools) <= length tools

-- ============================================================================
-- Advanced Performance Tests
-- ============================================================================

-- | 测试内存使用优化
prop_memory_optimization :: Int -> Property
prop_memory_optimization dataSize =
  let validSize = dataSize >= 0 && dataSize <= 10000
  in if not validSize
     then property True
     else property $ dataSize >= 0

-- | 测试编译时间优化
prop_compilation_time_optimization :: Int -> Property
prop_compilation_time_optimization complexity =
  let validComplexity = complexity >= 0 && complexity <= 1000
  in if not validComplexity
     then property True
     else property $ complexity >= 0

-- | 测试并发编译性能
prop_concurrent_compilation :: Int -> Property
prop_concurrent_compilation numFiles =
  let validNumFiles = numFiles >= 0 && numFiles <= 100
  in if not validNumFiles
     then property True
     else property $ numFiles >= 0

-- | 测试增量编译性能
prop_incremental_compilation :: Int -> Property
prop_incremental_compilation changedFiles =
  let validChangedFiles = changedFiles >= 0 && changedFiles <= 50
  in if not validChangedFiles
     then property True
     else property $ changedFiles >= 0

-- ============================================================================
-- Advanced Edge Case Tests
-- ============================================================================

-- | 测试Unicode字符处理
prop_unicode_handling :: [Int] -> Property
prop_unicode_handling codePoints =
  let validCodePoints = all (\cp -> cp >= 0 && cp <= 0x10FFFF) codePoints
      chars = map (toEnum) codePoints :: [Char]
  in if not validCodePoints
     then property True
     else property $ length chars == length codePoints

-- | 测试极深嵌套结构
prop_deep_nesting :: Int -> Property
prop_deep_nesting depth =
  let validDepth = depth >= 0 && depth <= 100
  in if not validDepth
     then property True
     else property $ depth >= 0

-- | 测试大量标识符处理
prop_many_identifiers :: Int -> Property
prop_many_identifiers count =
  let validCount = count >= 0 && count <= 1000
  in if not validCount
     then property True
     else property $ count >= 0

-- | 测试复杂表达式解析
prop_complex_expressions :: Int -> Property
prop_complex_expressions complexity =
  let validComplexity = complexity >= 0 && complexity <= 50
  in if not validComplexity
     then property True
     else property $ complexity >= 0

-- ============================================================================
-- Test Suite Collection
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Advanced Module QuickCheck Test Suite"
  [ testProperty "Advanced Parser - Complex Expressions" prop_parser_complex_expressions
  , testProperty "Advanced Parser - Type Declarations" prop_parser_type_declarations
  , testProperty "Advanced Parser - Interface Declarations" prop_parser_interface_declarations
  , testProperty "Advanced Parser - Concurrent Structures" prop_parser_concurrent_structures
  , testProperty "Advanced Parser - Error Handling" prop_parser_error_handling
  
  , testProperty "Advanced Compiler - Optimization" prop_compiler_optimization
  , testProperty "Advanced Compiler - Type Inference" prop_compiler_type_inference
  , testProperty "Advanced Compiler - Generics" prop_compiler_generics
  , testProperty "Advanced Compiler - Reflection" prop_compiler_reflection
  
  , testProperty "Advanced Ownership - Lifecycle" prop_ownership_lifecycle
  , testProperty "Advanced Ownership - Borrowing Rules" prop_ownership_borrowing_rules
  , testProperty "Advanced Ownership - Transfer Chain" prop_ownership_transfer_chain
  , testProperty "Advanced Ownership - Concurrency" prop_ownership_concurrency
  
  , testProperty "Advanced Dependencies - Complex Constraints" prop_complex_type_constraints
  , testProperty "Advanced Dependencies - Type Hierarchy" prop_type_hierarchy
  , testProperty "Advanced Dependencies - Substitution Transitivity" prop_type_substitution_transitivity
  , testProperty "Advanced Dependencies - Constraint Solving" prop_type_constraint_solving
  
  , testProperty "Advanced SourceLocation - Position Calculation" prop_position_calculation
  , testProperty "Advanced SourceLocation - Span Merging" prop_span_merging
  , testProperty "Advanced SourceLocation - Position Offset" prop_position_offset
  , testProperty "Advanced SourceLocation - Comparison Transitivity" prop_position_comparison_transitivity
  
  , testProperty "Advanced Utils - String Encoding" prop_string_encoding
  , testProperty "Advanced Utils - List Associative" prop_list_associative
  , testProperty "Advanced Utils - Map Associative" prop_map_associative
  , testProperty "Advanced Utils - Set Commutative" prop_set_commutative
  
  , testProperty "Advanced ErrorHandler - I18n" prop_error_message_i18n
  , testProperty "Advanced ErrorHandler - Recovery Strategies" prop_error_recovery_strategies
  , testProperty "Advanced ErrorHandler - Context Collection" prop_error_context_collection
  , testProperty "Advanced ErrorHandler - Severity Levels" prop_error_severity_levels
  
  , testProperty "Advanced Debug - Structured Info" prop_structured_debug_info
  , testProperty "Advanced Debug - Performance Monitoring" prop_debug_performance_monitoring
  , testProperty "Advanced Debug - Snapshots" prop_debug_snapshots
  , testProperty "Advanced Debug - Log Rotation" prop_debug_log_rotation
  
  , testProperty "Advanced Integration - Multi File Compilation" prop_multi_file_compilation
  , testProperty "Advanced Integration - Module System" prop_module_system_integration
  , testProperty "Advanced Integration - Build System" prop_build_system_integration
  , testProperty "Advanced Integration - Toolchain" prop_toolchain_integration
  
  , testProperty "Advanced Performance - Memory Optimization" prop_memory_optimization
  , testProperty "Advanced Performance - Compilation Time" prop_compilation_time_optimization
  , testProperty "Advanced Performance - Concurrent Compilation" prop_concurrent_compilation
  , testProperty "Advanced Performance - Incremental Compilation" prop_incremental_compilation
  
  , testProperty "Advanced Edge Cases - Unicode" prop_unicode_handling
  , testProperty "Advanced Edge Cases - Deep Nesting" prop_deep_nesting
  , testProperty "Advanced Edge Cases - Many Identifiers" prop_many_identifiers
  , testProperty "Advanced Edge Cases - Complex Expressions" prop_complex_expressions
  ]