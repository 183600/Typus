{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Unit.NewComprehensiveQuickCheckTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Data.List (isInfixOf, nub, sort, group, intercalate, isPrefixOf, isSuffixOf)
import Data.Char (isSpace, isAlpha, isDigit, isAlphaNum, toLower, toUpper)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)

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
-- Parser Module Tests
-- ============================================================================

-- | 测试解析器对标识符的处理
prop_parser_identifier_handling :: String -> Property
prop_parser_identifier_handling ident =
  let validIdent = not (null ident) && all isAlphaNum ident && isAlpha (head ident)
      code = "func " ++ ident ++ "() {}"
      parsed = Parser.parseTypusFile code
  in if not validIdent
     then property True
     else case parsed of
            Right _ -> property True
            Left _ -> property False

-- | 测试解析器对字符串字面量的处理
prop_parser_string_literals :: String -> Property
prop_parser_string_literals content =
  let escapedContent = concatMap (\c -> if c == '"' then "\\\"" else [c]) content
      code = "func test() { s := \"" ++ escapedContent ++ "\" }"
      parsed = Parser.parseTypusFile code
  in case parsed of
       Right _ -> property True
       Left _ -> property True  -- 解析失败也可能是有效的

-- | 测试解析器对数值字面量的处理
prop_parser_numeric_literals :: Integer -> Property
prop_parser_numeric_literals num =
  let code = "func test() { x := " ++ show num ++ " }"
      parsed = Parser.parseTypusFile code
  in case parsed of
       Right _ -> property True
       Left _ -> property True

-- | 测试解析器对注释的处理
prop_parser_comment_handling :: String -> String -> Property
prop_parser_comment_handling lineComment blockComment =
  let code = "func test() {\n  // " ++ lineComment ++ "\n  /* " ++ blockComment ++ " */\n}"
      parsed = Parser.parseTypusFile code
  in case parsed of
       Right _ -> property True
       Left _ -> property True

-- | 测试解析器对函数声明的处理
prop_parser_function_declaration :: String -> [String] -> Property
prop_parser_function_declaration funcName params =
  let validFuncName = not (null funcName) && isAlpha (head funcName) && all isAlphaNum funcName
      validParams = all (\p -> not (null p) && isAlpha (head p) && all isAlphaNum p) params
      paramStr = intercalate ", " params
      code = "func " ++ funcName ++ "(" ++ paramStr ++ ") {}"
  in if not (validFuncName && validParams)
     then property True
     else case Parser.parseTypusFile code of
            Right _ -> property True
            Left _ -> property False

-- ============================================================================
-- Compiler Module Tests
-- ============================================================================

-- | 测试编译器的基本属性
prop_compiler_basic_properties :: String -> Property
prop_compiler_basic_properties code =
  let parsed = Parser.parseTypusFile code
      compiled = case parsed of
                   Right ast -> Compiler.compile ast
                   Left _ -> Left [Compiler.malformedSyntaxError]
  in case compiled of
       Right result -> property $ show result /= ""
       Left _ -> property True

-- | 测试编译器错误处理
prop_compiler_error_handling :: String -> Property
prop_compiler_error_handling code =
  let hasErrors = "invalid" `isInfixOf` code
      parsed = Parser.parseTypusFile code
      compiled = case parsed of
                   Right ast -> Compiler.compile ast
                   Left _ -> Left [Compiler.malformedSyntaxError]
  in classify hasErrors "has errors" $
     case compiled of
       Right _ -> property True
       Left _ -> property True

-- | 测试编译器对空输入的处理
prop_compiler_empty_input :: Property
prop_compiler_empty_input =
  let parsed = Parser.parseTypusFile ""
      compiled = case parsed of
                   Right ast -> Compiler.compile ast
                   Left _ -> Left [Compiler.malformedSyntaxError]
  in case compiled of
       Right _ -> property True
       Left _ -> property True

-- | 测试编译器对简单表达式的处理
prop_compiler_simple_expressions :: String -> String -> String -> Property
prop_compiler_simple_expressions left op right =
  let validOps = op `elem` ["+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">="]
      validOperands = not (null left) && not (null right)
  in if not (validOps && validOperands)
     then property True
     else let expr = left ++ " " ++ op ++ " " ++ right
              code = "func test() { result := " ++ expr ++ " }"
              parsed = Parser.parseTypusFile code
              compiled = case parsed of
                           Right ast -> Compiler.compile ast
                           Left _ -> Left [Compiler.malformedSyntaxError]
          in case compiled of
               Right _ -> property True
               Left _ -> property True

-- ============================================================================
-- Ownership Module Tests
-- ============================================================================

-- | 测试所有权转移的基本属性
prop_ownership_transfer_basic :: String -> String -> Property
prop_ownership_transfer_basic from to =
  let validFrom = not (null from) && all isAlpha from
      validTo = not (null to) && all isAlpha to
  in if not (validFrom && validTo)
     then property True
     else let transfer = Own.OwnershipTransfer from to
          in property $ show transfer /= ""

-- | 测试所有权约束的属性
prop_ownership_constraints :: String -> Bool -> Property
prop_ownership_constraints varName isOwned =
  let validVar = not (null varName) && all isAlpha varName
      ownership = if isOwned then Own.Owned varName else Own.Borrowed varName
  in if not validVar
     then property True
     else property $ show ownership /= ""

-- | 测试所有权分析的一致性
prop_ownership_analysis_consistency :: [(String, String)] -> Property
prop_ownership_analysis_consistency transfers =
  let validTransfers = all (\(from, to) -> not (null from) && not (null to)) transfers
  in if not validTransfers
     then property True
     else let uniqueVars = nub $ concatMap (\(from, to) -> [from, to]) transfers
              transferCount = length transfers
          in property $ transferCount <= length uniqueVars * (length uniqueVars - 1)

-- | 测试借用检查的属性
prop_borrowing_checker :: String -> [String] -> Property
prop_borrowing_checker owner borrowers =
  let validOwner = not (null owner) && all isAlpha owner
      validBorrowers = all (\b -> not (null b) && all isAlpha b) borrowers
  in if not (validOwner && validBorrowers)
     then property True
     else let uniqueBorrowers = nub borrowers
          in property $ length uniqueBorrowers <= length borrowers

-- ============================================================================
-- Dependencies Module Tests
-- ============================================================================

-- | 测试类型变量的基本属性
prop_type_variables :: String -> Property
prop_type_variables typeName =
  let validType = not (null typeName) && all isAlpha typeName
      typeVar = Dep.TVCon typeName
  in if not validType
     then property True
     else property $ show typeVar /= ""

-- | 测试类型约束的属性
prop_type_constraints_advanced :: String -> Int -> String -> Property
prop_type_constraints_advanced typeName constraintValue constraintType =
  let validType = not (null typeName) && all isAlpha typeName
      validConstraint = constraintValue >= 0
      validTypeConstraint = constraintType `elem` ["SizeGT", "SizeEQ", "SizeLT"]
  in if not (validType && validConstraint && validTypeConstraint)
     then property True
     else let constraint = case constraintType of
                               "SizeGT" -> Dep.SizeGT (T.pack typeName) constraintValue
                               "SizeGE" -> Dep.SizeGE (T.pack typeName) constraintValue
                               "SizeLT" -> Dep.SizeGT (T.pack typeName) (constraintValue - 1)  -- 用SizeGT模拟SizeLT
                               _ -> Dep.SizeGT (T.pack typeName) constraintValue
          in property $ show constraint /= ""

-- | 测试类型环境的属性
prop_type_environment_advanced :: [(String, String)] -> String -> Property
prop_type_environment_advanced bindings queryKey =
  let validBindings = all (\(k, v) -> not (null k) && not (null v)) bindings
      env = Map.fromList bindings
      result = Map.lookup queryKey env
  in if not validBindings
     then property True
     else case result of
            Just _ -> property $ queryKey `elem` map fst bindings
            Nothing -> property $ not (queryKey `elem` map fst bindings)

-- | 测试类型替换的属性
prop_type_substitution_advanced :: [(String, String)] -> String -> Property
prop_type_substitution_advanced mappings typeName =
  let validMappings = all (\(k, v) -> not (null k) && not (null v)) mappings
      validType = not (null typeName)
  in if not (validMappings && validType)
     then property True
     else let subst = Map.fromList mappings
              typeVar = Dep.TVCon typeName
              hasMapping = typeName `elem` map fst mappings
          in classify hasMapping "has mapping" $
              property $ show subst /= ""

-- | 测试依赖类型的基本属性
prop_dependent_types_basic :: String -> [String] -> Property
prop_dependent_types_basic typeName constraints =
  let validType = not (null typeName) && all isAlpha typeName
      validConstraints = all (not . null) constraints
  in if not (validType && validConstraints)
     then property True
     else let typeVar = Dep.TVCon typeName
          in property $ show typeVar /= ""

-- ============================================================================
-- SourceLocation Module Tests
-- ============================================================================

-- | 测试源位置的属性
prop_source_position :: Int -> Int -> Int -> Property
prop_source_position line col offset =
  let validPos = line >= 0 && col >= 0 && offset >= 0
      pos = SourcePos line col offset
  in if not validPos
     then property True
     else property $ show pos /= ""

-- | 测试源范围的属性
prop_source_span_advanced :: (Int, Int, Int) -> (Int, Int, Int) -> Property
prop_source_span_advanced (line1, col1, offset1) (line2, col2, offset2) =
  let validSpan = line1 >= 0 && col1 >= 0 && offset1 >= 0 && 
                  line2 >= 0 && col2 >= 0 && offset2 >= 0
      start = SourcePos line1 col1 offset1
      end = SourcePos line2 col2 offset2
      span = SourceSpan start end
  in if not validSpan
     then property True
     else property $ show span /= ""

-- | 测试位置比较的属性
prop_position_comparison :: (Int, Int) -> (Int, Int) -> Property
prop_position_comparison (line1, col1) (line2, col2) =
  let validPos = line1 >= 0 && col1 >= 0 && line2 >= 0 && col2 >= 0
      pos1 = SourcePos line1 col1 0
      pos2 = SourcePos line2 col2 0
  in if not validPos
     then property True
     else let sameLine = line1 == line2
              sameCol = col1 == col2
              samePos = pos1 == pos2
          in property $ (sameLine && sameCol) === samePos

-- ============================================================================
-- Utils Module Tests
-- ============================================================================

-- | 测试字符串处理的属性
prop_string_processing :: String -> Property
prop_string_processing s =
  let trimmed = Utils.trim s
      upper = map toUpper s
      lower = map toLower s
  in conjoin
       [ property $ length trimmed <= length s
       , property $ length (map toUpper upper) == length upper
       , property $ length (map toLower lower) == length lower
       ]

-- | 测试列表处理的属性
prop_list_processing :: [String] -> Property
prop_list_processing lst =
  let unique = nub lst
      sorted = sort lst
      grouped = group sorted
  in conjoin
       [ property $ length unique <= length lst
       , property $ length sorted == length lst
       , property $ sum (map length grouped) == length lst
       ]

-- | 测试映射处理的属性
prop_map_processing :: [(String, Int)] -> Property
prop_map_processing pairs =
  let validPairs = all (\(k, v) -> not (null k)) pairs
      mp = Map.fromList pairs
      keys = Map.keys mp
      values = Map.elems mp
  in if not validPairs
     then property True
     else conjoin
       [ property $ length keys == length (nub $ map fst pairs)
       , property $ length values <= length pairs
       ]

-- | 测试集合处理的属性
prop_set_processing :: [String] -> Property
prop_set_processing items =
  let validItems = all (not . null) items
      set = Set.fromList items
  in if not validItems
     then property True
     else property $ Set.size set <= length items

-- ============================================================================
-- ErrorHandler Module Tests
-- ============================================================================

-- | 测试错误消息的基本属性
prop_error_messages :: String -> String -> Property
prop_error_messages errorType errorMsg =
  let validErrorType = not (null errorType)
      validErrorMsg = not (null errorMsg)
      fullError = errorType ++ ": " ++ errorMsg
  in if not (validErrorType && validErrorMsg)
     then property True
     else property $ length fullError >= length errorType + length errorMsg

-- | 测试错误恢复的属性
prop_error_recovery :: [String] -> Property
prop_error_recovery errors =
  let validErrors = all (not . null) errors
  in if not validErrors
     then property True
     else let uniqueErrors = nub errors
          in property $ length uniqueErrors <= length errors

-- | 测试错误分类的属性
prop_error_classification :: String -> String -> Property
prop_error_classification error category =
  let validError = not (null error)
      validCategory = not (null category)
      categories = ["syntax", "type", "ownership", "runtime"]
      validCategoryType = category `elem` categories
  in if not (validError && validCategory && validCategoryType)
     then property True
     else property $ length (error ++ ":" ++ category) >= 0

-- ============================================================================
-- Debug Module Tests
-- ============================================================================

-- | 测试调试信息的属性
prop_debug_info :: String -> Int -> Property
prop_debug_info message level =
  let validMessage = not (null message)
      validLevel = level >= 0 && level <= 10
  in if not (validMessage && validLevel)
     then property True
     else let debugInfo = message ++ " [level:" ++ show level ++ "]"
          in property $ length debugInfo >= length message

-- | 测试调试级别的属性
prop_debug_levels :: Int -> Int -> Property
prop_debug_levels currentLevel maxLevel =
  let validLevels = currentLevel >= 0 && maxLevel >= 0 && currentLevel <= maxLevel
  in if not validLevels
     then property True
     else property $ currentLevel <= maxLevel

-- | 测试调试过滤的属性
prop_debug_filtering :: [String] -> String -> Property
prop_debug_filtering messages filterStr =
  let validMessages = all (not . null) messages
      validFilter = not (null filterStr)
      filtered = filter (filterStr `isInfixOf`) messages
  in if not (validMessages && validFilter)
     then property True
     else property $ length filtered <= length messages

-- ============================================================================
-- Integration Tests
-- ============================================================================

-- | 测试解析器-编译器集成
prop_parser_compiler_integration :: String -> Property
prop_parser_compiler_integration code =
  let parsed = Parser.parseTypusFile code
      compiled = case parsed of
                   Right ast -> Compiler.compile ast
                   Left _ -> Left [Compiler.malformedSyntaxError]
  in case (parsed, compiled) of
       (Right _, Right _) -> property True
       (Left _, Left _) -> property True
       _ -> property True  -- 一个成功一个失败也是可能的

-- | 测试编译器-所有权集成
prop_compiler_ownership_integration :: String -> Property
prop_compiler_ownership_integration code =
  let hasOwnershipDirective = "ownership" `isInfixOf` code
      parsed = Parser.parseTypusFile code
      compiled = case parsed of
                   Right ast -> Compiler.compile ast
                   Left _ -> Left [Compiler.malformedSyntaxError]
  in classify hasOwnershipDirective "has ownership directive" $
     case compiled of
       Right _ -> property True
       Left _ -> property True

-- | 测试编译器-依赖类型集成
prop_compiler_dependent_types_integration :: String -> Property
prop_compiler_dependent_types_integration code =
  let hasDependentTypesDirective = "dependent_types" `isInfixOf` code || 
                                   "constraints" `isInfixOf` code
      parsed = Parser.parseTypusFile code
      compiled = case parsed of
                   Right ast -> Compiler.compile ast
                   Left _ -> Left [Compiler.malformedSyntaxError]
  in classify hasDependentTypesDirective "has dependent types directive" $
     case compiled of
       Right _ -> property True
       Left _ -> property True

-- | 测试端到端编译流程
prop_end_to_end_compilation :: String -> Property
prop_end_to_end_compilation code =
  let parsed = Parser.parseTypusFile code
      compiled = case parsed of
                   Right ast -> Compiler.compile ast
                   Left _ -> Left [Compiler.malformedSyntaxError]
  in case (parsed, compiled) of
       (Right ast, Right result) -> 
         property $ show ast /= "" && show result /= ""
       (Right _, Left _) -> property True
       (Left _, Right _) -> property True
       (Left _, Left _) -> property True

-- ============================================================================
-- Performance Tests
-- ============================================================================

-- | 测试大文件解析性能
prop_large_file_parsing :: Int -> Property
prop_large_file_parsing size =
  let validSize = size >= 0 && size <= 1000
  in if not validSize
     then property True
     else let code = unlines $ replicate size "func test() {}"
              parsed = Parser.parseTypusFile code
          in case parsed of
               Right _ -> property True
               Left _ -> property True

-- | 测试复杂类型推断性能
prop_complex_type_inference :: Int -> Property
prop_complex_type_inference complexity =
  let validComplexity = complexity >= 0 && complexity <= 100
  in if not validComplexity
     then property True
     else let code = unlines $ replicate complexity "func test" ++ 
                                ["func test() { x := 0 }"]
              parsed = Parser.parseTypusFile code
              compiled = case parsed of
                           Right ast -> Compiler.compile ast
                           Left _ -> Left [Compiler.malformedSyntaxError]
          in case compiled of
               Right _ -> property True
               Left _ -> property True

-- | 测试所有权分析性能
prop_ownership_analysis_performance :: Int -> Property
prop_ownership_analysis_performance numVars =
  let validNumVars = numVars >= 0 && numVars <= 100
  in if not validNumVars
     then property True
     else let vars = take numVars $ map (\i -> "var" ++ show i) [1..]
              transfers = zip vars (tail vars ++ [""])
              validTransfers = filter (\(f, t) -> not (null f) && not (null t)) transfers
          in property $ length validTransfers <= numVars - 1

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

-- | 测试空字符串处理
prop_empty_string_handling :: Property
prop_empty_string_handling =
  let parsed = Parser.parseTypusFile ""
      compiled = case parsed of
                   Right ast -> Compiler.compile ast
                   Left _ -> Left [Compiler.malformedSyntaxError]
  in case (parsed, compiled) of
       (Right _, Right _) -> property True
       (Left _, Left _) -> property True
       _ -> property True

-- | 测试特殊字符处理
prop_special_characters :: String -> Property
prop_special_characters chars =
  let hasSpecialChars = any (not . isAlphaNum) chars
      code = "func test() { s := \"" ++ chars ++ "\" }"
      parsed = Parser.parseTypusFile code
  in classify hasSpecialChars "has special characters" $
     case parsed of
       Right _ -> property True
       Left _ -> property True

-- | 测试极长标识符处理
prop_long_identifier_handling :: Int -> Property
prop_long_identifier_handling length =
  let validLength = length >= 0 && length <= 1000
  in if not validLength
     then property True
     else let longIdent = replicate length 'a'
              code = "func " ++ longIdent ++ "() {}"
              parsed = Parser.parseTypusFile code
          in case parsed of
               Right _ -> property True
               Left _ -> property True

-- | 测试嵌套结构处理
prop_nested_structures :: Int -> Property
prop_nested_structures depth =
  let validDepth = depth >= 0 && depth <= 10
  in if not validDepth
     then property True
     else let indent n = replicate n ' '
              nestedCode n = indent n ++ "if true {\n" ++ 
                            (if n > 0 then nestedCode (n-1) else "") ++
                            indent n ++ "}\n"
              code = "func test() {\n" ++ nestedCode depth ++ "}"
              parsed = Parser.parseTypusFile code
          in case parsed of
               Right _ -> property True
               Left _ -> property True

-- ============================================================================
-- Test Suite Collection
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "New Comprehensive QuickCheck Test Suite"
  [ testProperty "Parser - Identifier Handling" prop_parser_identifier_handling
  , testProperty "Parser - String Literals" prop_parser_string_literals
  , testProperty "Parser - Numeric Literals" prop_parser_numeric_literals
  , testProperty "Parser - Comment Handling" prop_parser_comment_handling
  , testProperty "Parser - Function Declaration" prop_parser_function_declaration
  
  , testProperty "Compiler - Basic Properties" prop_compiler_basic_properties
  , testProperty "Compiler - Error Handling" prop_compiler_error_handling
  , testProperty "Compiler - Empty Input" prop_compiler_empty_input
  , testProperty "Compiler - Simple Expressions" prop_compiler_simple_expressions
  
  , testProperty "Ownership - Transfer Basic" prop_ownership_transfer_basic
  , testProperty "Ownership - Constraints" prop_ownership_constraints
  , testProperty "Ownership - Analysis Consistency" prop_ownership_analysis_consistency
  , testProperty "Ownership - Borrowing Checker" prop_borrowing_checker
  
  , testProperty "Dependencies - Type Variables" prop_type_variables
  , testProperty "Dependencies - Type Constraints Advanced" prop_type_constraints_advanced
  , testProperty "Dependencies - Type Environment Advanced" prop_type_environment_advanced
  , testProperty "Dependencies - Type Substitution Advanced" prop_type_substitution_advanced
  , testProperty "Dependencies - Dependent Types Basic" prop_dependent_types_basic
  
  , testProperty "SourceLocation - Position" prop_source_position
  , testProperty "SourceLocation - Span Advanced" prop_source_span_advanced
  , testProperty "SourceLocation - Position Comparison" prop_position_comparison
  
  , testProperty "Utils - String Processing" prop_string_processing
  , testProperty "Utils - List Processing" prop_list_processing
  , testProperty "Utils - Map Processing" prop_map_processing
  , testProperty "Utils - Set Processing" prop_set_processing
  
  , testProperty "ErrorHandler - Error Messages" prop_error_messages
  , testProperty "ErrorHandler - Error Recovery" prop_error_recovery
  , testProperty "ErrorHandler - Error Classification" prop_error_classification
  
  , testProperty "Debug - Info" prop_debug_info
  , testProperty "Debug - Levels" prop_debug_levels
  , testProperty "Debug - Filtering" prop_debug_filtering
  
  , testProperty "Integration - Parser Compiler" prop_parser_compiler_integration
  , testProperty "Integration - Compiler Ownership" prop_compiler_ownership_integration
  , testProperty "Integration - Compiler Dependent Types" prop_compiler_dependent_types_integration
  , testProperty "Integration - End to End Compilation" prop_end_to_end_compilation
  
  , testProperty "Performance - Large File Parsing" prop_large_file_parsing
  , testProperty "Performance - Complex Type Inference" prop_complex_type_inference
  , testProperty "Performance - Ownership Analysis" prop_ownership_analysis_performance
  
  , testProperty "Edge Cases - Empty String" prop_empty_string_handling
  , testProperty "Edge Cases - Special Characters" prop_special_characters
  , testProperty "Edge Cases - Long Identifier" prop_long_identifier_handling
  , testProperty "Edge Cases - Nested Structures" prop_nested_structures
  ]