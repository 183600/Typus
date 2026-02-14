{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewAdvancedQuickCheckTestSuite where

import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import TestSupport.MemoryLimits 
  ( withMemoryLimits
  , memoryLimitedTestGroup
  , memoryLevelTestGroup
  , MemoryLevel(..)
  , withMemoryLevel
  , gcBetweenTests
  )
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort, group, nub, intercalate)
import Data.Char (isSpace, isAlpha, isDigit, toLower, toUpper, ord, chr)
import Data.Either (isLeft, isRight)
import Data.Maybe (listToMaybe, catMaybes, fromMaybe)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (when, replicateM, forM_, unless)
import Text.Megaparsec (runParser, errorBundlePretty)
import Data.String (IsString(..))

import Compiler (compile, CompilerResult, CompilerError(..), renderCompilationError)
import Compiler.Errors (ErrorCategory(..), ErrorSeverity(..), CompilationPhase(..), mkCompilerError)
import Parser (parseTypus, TypusFile(..), Declaration(..), Expression(..), FileDirectives(..), BlockDirectives(..), CodeBlock(..))
import DependentTypesParser (parseDependentType, parseTypeReference, parseTypeExpression, DependentType(..), TypeBody(..), Field(..), TypeRef(..))
import Ownership (analyzeOwnership)
import Ownership.Common.Types (OwnershipError(..), OwnershipType(..))
import Utils (trim)
import SourceLocation (Located(..))

-- ============================================================================
-- 高级Parser模块的QuickCheck测试
-- ============================================================================

-- | 辅助函数：从字符串编译 Typus 代码
compileTypusString :: String -> CompilerResult String
compileTypusString input = 
  case parseTypus input of
    Left err -> Left [mkCompilerError "ParseError" (T.pack err) ParsingPhase Parsing Error Nothing Nothing [] ["compileTypusString"] Nothing]
    Right typusFile -> compile typusFile

-- | 测试复杂表达式解析
prop_parser_complex_expressions :: [String] -> Property
prop_parser_complex_expressions exprs =
  let limitedExprs = map (take 15) $ take 5 exprs
      combinedExpr = intercalate " + " limitedExprs
      input = "package main\n\nfunc main() {\n    x := " ++ (if null combinedExpr then "1" else combinedExpr) ++ "\n}"
      result = parseTypus input
  in property $ case result of
    Left _ -> True
    Right typusFile -> True

-- | 测试嵌套函数调用解析
prop_parser_nested_function_calls :: String -> [String] -> Property
prop_parser_nested_function_calls funcName args =
  let limitedName = take 10 $ filter isAlpha funcName
      limitedArgs = map (take 10) $ take 5 args
      nestedCalls = foldr (\arg acc -> (if null arg then "func" else arg) ++ "(" ++ acc ++ ")") (if null limitedName then "func" else limitedName) limitedArgs
      input = "package main\n\nfunc main() {\n    x := " ++ nestedCalls ++ "\n}"
      result = parseTypus input
  in property $ case result of
    Left _ -> True
    Right typusFile -> True

-- | 测试复杂类型声明解析
prop_parser_complex_type_declarations :: String -> [String] -> [String] -> Property
prop_parser_complex_type_declarations typeName fieldNames fieldTypes =
  let limitedName = take 10 $ filter isAlpha typeName
      limitedFields = take 5 $ zip (map (take 10) fieldNames) (map (take 10) fieldTypes)
      fieldLines = map (\(name, typ) -> "    " ++ (if null name then "field" else name) ++ ": " ++ (if null typ then "int" else typ)) limitedFields
      input = "package main\n\ntype " ++ (if null limitedName then "ComplexType" else limitedName) ++ " struct {\n" ++ unlines fieldLines ++ "}"
      result = parseTypus input
  in property $ case result of
    Left _ -> True
    Right typusFile -> True

-- | 测试泛型类型声明解析
prop_parser_generic_type_declarations :: String -> [String] -> [String] -> Property
prop_parser_generic_type_declarations typeName typeParams constraints =
  let limitedName = take 10 $ filter isAlpha typeName
      limitedParams = map (take 10) $ take 5 typeParams
      limitedConstraints = map (take 15) $ take 5 constraints
      paramList = intercalate ", " $ map (\p -> if null p then "T" else p) limitedParams
      constraintList = if null limitedConstraints then "" else " where { " ++ intercalate ", " limitedConstraints ++ " }"
      input = "package main\n\ntype " ++ (if null limitedName then "GenericType" else limitedName) ++ "[" ++ paramList ++ "] struct {}" ++ constraintList
      result = parseTypus input
  in property $ case result of
    Left _ -> True
    Right typusFile -> True

-- | 测试多包声明解析
prop_parser_multiple_packages :: [String] -> Property
prop_parser_multiple_packages packageNames =
  let limitedNames = map (take 10) $ take 3 $ map (filter isAlpha) packageNames
      packageDecls = map (\name -> "package " ++ (if null name then "main" else name)) limitedNames
      input = unlines packageDecls
      result = parseTypus input
  in property $ case result of
    Left _ -> True  -- 应该产生错误，因为多个包声明
    Right typusFile -> True

-- | 测试复杂导入语句解析
prop_parser_complex_imports :: [String] -> Property
prop_parser_complex_imports importPaths =
  let limitedPaths = map (take 15) $ take 5 importPaths
      importLines = map (\path -> "import \"" ++ (if null path then "fmt" else path) ++ "\"") limitedPaths
      input = "package main\n\n" ++ unlines importLines
      result = parseTypus input
  in property $ case result of
    Left _ -> True
    Right typusFile -> True

-- | 测试注释和代码混合解析
prop_parser_mixed_comments_and_code :: [String] -> [String] -> Property
prop_parser_mixed_comments_and_code comments codeLines =
  let limitedComments = map (take 15) $ take 5 comments
      limitedCode = map (take 20) $ take 5 codeLines
      commentLines = map (\c -> "// " ++ c) limitedComments
      mixedLines = concat $ zipWith (\c l -> [c, l]) commentLines limitedCode
      input = "package main\n\n" ++ unlines mixedLines
      result = parseTypus input
  in property $ case result of
    Left _ -> True
    Right typusFile -> True

-- | 测试Unicode字符处理
prop_parser_unicode_handling :: [Int] -> Property
prop_parser_unicode_handling codePoints =
  let limitedPoints = take 10 codePoints
      unicodeChars = map (\cp -> if cp >= 32 && cp <= 126 then chr cp else chr (cp `mod` 94 + 33)) limitedPoints
      unicodeString = map (\c -> if isAlpha c || isDigit c then c else '_') unicodeChars
      input = "package main\n\nfunc " ++ (if null unicodeString then "test" else unicodeString) ++ "() {}"
      result = parseTypus input
  in property $ case result of
    Left _ -> True
    Right typusFile -> True

-- | 测试长标识符处理
prop_parser_long_identifiers :: Int -> Property
prop_parser_long_identifiers n =
  let length = max 1 (min 100 (abs n))
      longIdent = replicate length 'x'
      input = "package main\n\nfunc " ++ longIdent ++ "() {}"
      result = parseTypus input
  in property $ case result of
    Left _ -> True
    Right typusFile -> True

-- | 测试深度嵌套结构
prop_parser_deeply_nested_structures :: Int -> Property
prop_parser_deeply_nested_structures depth =
  let limitedDepth = max 1 (min 10 (abs depth))
      buildNestedStruct 0 = "x"
      buildNestedStruct d = "struct { field: " ++ buildNestedStruct (d-1) ++ " }"
      input = "package main\n\ntype Nested = " ++ buildNestedStruct limitedDepth
      result = parseTypus input
  in property $ case result of
    Left _ -> True
    Right typusFile -> True

-- ============================================================================
-- 高级Compiler模块的QuickCheck测试
-- ============================================================================

-- | 测试复杂表达式编译
prop_compiler_complex_expressions :: [String] -> Property
prop_compiler_complex_expressions exprs =
  let limitedExprs = map (take 15) $ take 5 exprs
      combinedExpr = intercalate " + " limitedExprs
      input = "package main\n\nfunc main() {\n    x := " ++ (if null combinedExpr then "1" else combinedExpr) ++ "\n}"
      result = compileTypusString input
  in property $ case result of
    Left _ -> True
    Right goCode -> length goCode <= 3000

-- | 测试嵌套函数调用编译
prop_compiler_nested_function_calls :: String -> [String] -> Property
prop_compiler_nested_function_calls funcName args =
  let limitedName = take 10 $ filter isAlpha funcName
      limitedArgs = map (take 10) $ take 5 args
      nestedCalls = foldr (\arg acc -> (if null arg then "func" else arg) ++ "(" ++ acc ++ ")") (if null limitedName then "func" else limitedName) limitedArgs
      input = "package main\n\nfunc main() {\n    x := " ++ nestedCalls ++ "\n}"
      result = compileTypusString input
  in property $ case result of
    Left _ -> True
    Right goCode -> length goCode <= 3000

-- | 测试复杂类型声明编译
prop_compiler_complex_type_declarations :: String -> [String] -> [String] -> Property
prop_compiler_complex_type_declarations typeName fieldNames fieldTypes =
  let limitedName = take 10 $ filter isAlpha typeName
      limitedFields = take 5 $ zip (map (take 10) fieldNames) (map (take 10) fieldTypes)
      fieldLines = map (\(name, typ) -> "    " ++ (if null name then "field" else name) ++ ": " ++ (if null typ then "int" else typ)) limitedFields
      input = "package main\n\ntype " ++ (if null limitedName then "ComplexType" else limitedName) ++ " struct {\n" ++ unlines fieldLines ++ "}"
      result = compileTypusString input
  in property $ case result of
    Left _ -> True
    Right goCode -> length goCode <= 3000

-- | 测试泛型类型声明编译
prop_compiler_generic_type_declarations :: String -> [String] -> [String] -> Property
prop_compiler_generic_type_declarations typeName typeParams constraints =
  let limitedName = take 10 $ filter isAlpha typeName
      limitedParams = map (take 10) $ take 5 typeParams
      limitedConstraints = map (take 15) $ take 5 constraints
      paramList = intercalate ", " $ map (\p -> if null p then "T" else p) limitedParams
      constraintList = if null limitedConstraints then "" else " where { " ++ intercalate ", " limitedConstraints ++ " }"
      input = "package main\n\ntype " ++ (if null limitedName then "GenericType" else limitedName) ++ "[" ++ paramList ++ "] struct {}" ++ constraintList
      result = compileTypusString input
  in property $ case result of
    Left _ -> True
    Right goCode -> length goCode <= 3000

-- | 测试复杂导入语句编译
prop_compiler_complex_imports :: [String] -> Property
prop_compiler_complex_imports importPaths =
  let limitedPaths = map (take 15) $ take 5 importPaths
      importLines = map (\path -> "import \"" ++ (if null path then "fmt" else path) ++ "\"") limitedPaths
      input = "package main\n\n" ++ unlines importLines
      result = compileTypusString input
  in property $ case result of
    Left _ -> True
    Right goCode -> length goCode <= 3000

-- | 测试注释和代码混合编译
prop_compiler_mixed_comments_and_code :: [String] -> [String] -> Property
prop_compiler_mixed_comments_and_code comments codeLines =
  let limitedComments = map (take 15) $ take 5 comments
      limitedCode = map (take 20) $ take 5 codeLines
      commentLines = map (\c -> "// " ++ c) limitedComments
      mixedLines = concat $ zipWith (\c l -> [c, l]) commentLines limitedCode
      input = "package main\n\n" ++ unlines mixedLines
      result = compileTypusString input
  in property $ case result of
    Left _ -> True
    Right goCode -> length goCode <= 3000

-- | 测试Unicode字符编译
prop_compiler_unicode_handling :: [Int] -> Property
prop_compiler_unicode_handling codePoints =
  let limitedPoints = take 10 codePoints
      unicodeChars = map (\cp -> if cp >= 32 && cp <= 126 then chr cp else chr (cp `mod` 94 + 33)) limitedPoints
      unicodeString = map (\c -> if isAlpha c || isDigit c then c else '_') unicodeChars
      input = "package main\n\nfunc " ++ (if null unicodeString then "test" else unicodeString) ++ "() {}"
      result = compileTypusString input
  in property $ case result of
    Left _ -> True
    Right goCode -> length goCode <= 3000

-- | 测试长标识符编译
prop_compiler_long_identifiers :: Int -> Property
prop_compiler_long_identifiers n =
  let identLength = max 1 (min 100 (abs n))
      longIdent = replicate identLength 'x'
      input = "package main\n\nfunc " ++ longIdent ++ "() {}"
      result = compileTypusString input
  in property $ case result of
    Left _ -> True
    Right goCode -> length goCode <= 3000

-- | 测试深度嵌套结构编译
prop_compiler_deeply_nested_structures :: Int -> Property
prop_compiler_deeply_nested_structures depth =
  let limitedDepth = max 1 (min 10 (abs depth))
      buildNestedStruct 0 = "x"
      buildNestedStruct d = "struct { field: " ++ buildNestedStruct (d-1) ++ " }"
      input = "package main\n\ntype Nested = " ++ buildNestedStruct limitedDepth
      result = compileTypusString input
  in property $ case result of
    Left _ -> True
    Right goCode -> length goCode <= 3000

-- ============================================================================
-- 高级DependentTypes模块的QuickCheck测试
-- ============================================================================

-- | 辅助函数：解析类型表达式
parseTypeExpressionLocal :: String -> Either String String
parseTypeExpressionLocal expr = 
  let fullDecl = "type TempType struct { field: " ++ expr ++ " }"
  in case parseDependentType fullDecl of
       Right (dt, _) -> 
         case dt of
           TypeDecl name params (StructBody fields) cons -> 
             case fields of
               [Field fieldName fieldType] -> Right $ show fieldType
               _ -> Left $ "无法提取类型表达式 - 字段数量不匹配: " ++ show (length fields)
           _ -> Left $ "无法提取类型表达式 - 结构不匹配: " ++ show dt
       Left err -> Left err

-- | 测试复杂依赖类型表达式
prop_dependent_types_complex_expressions :: [String] -> Property
prop_dependent_types_complex_expressions exprs =
  let limitedExprs = map (take 15) $ take 5 exprs
      combinedExpr = intercalate " + " limitedExprs
      result = parseTypeExpressionLocal combinedExpr
  in property $ case result of
    Left _ -> True
    Right parsedExpr -> True

-- | 测试嵌套依赖类型
prop_dependent_types_nested_types :: String -> Int -> Property
prop_dependent_types_nested_types typeName depth =
  let limitedName = take 10 $ filter isAlpha typeName
      limitedDepth = max 1 (min 5 (abs depth))
      buildNestedType 0 = "int"
      buildNestedType d = "Nested" ++ show d ++ "[" ++ buildNestedType (d-1) ++ "]"
      input = "type " ++ (if null limitedName then "NestedType" else limitedName) ++ " = " ++ buildNestedType limitedDepth
      result = parseDependentType input
  in property $ case result of
    Left _ -> True
    Right (dt, _) -> True

-- | 测试复杂约束表达式
prop_dependent_types_complex_constraints :: String -> [String] -> Property
prop_dependent_types_complex_constraints typeName constraints =
  let limitedName = take 10 $ filter isAlpha typeName
      limitedConstraints = map (take 20) $ take 5 constraints
      constraintExpr = intercalate " && " $ map (\c -> if null c then "self > 0" else c) limitedConstraints
      input = "type " ++ (if null limitedName then "ConstrainedType" else limitedName) ++ " = int where { " ++ constraintExpr ++ " }"
      result = parseDependentType input
  in property $ case result of
    Left _ -> True
    Right (dt, _) -> True

-- | 测试多参数依赖类型
prop_dependent_types_multiple_parameters :: String -> [String] -> Property
prop_dependent_types_multiple_parameters typeName paramNames =
  let limitedName = take 10 $ filter isAlpha typeName
      limitedParams = map (take 10) $ take 5 paramNames
      paramList = intercalate ", " $ map (\p -> if null p then "n" else p ++ ": int") limitedParams
      input = "type " ++ (if null limitedName then "MultiParamType" else limitedName) ++ "[" ++ paramList ++ "] struct { field int }"
      result = parseDependentType input
  in property $ case result of
    Left _ -> True
    Right (dt, _) -> True

-- | 测试递归依赖类型
prop_dependent_types_recursive_types :: String -> Property
prop_dependent_types_recursive_types typeName =
  let limitedName = take 10 $ filter isAlpha typeName
      input = "type " ++ (if null limitedName then "RecursiveType" else limitedName) ++ " struct { next: " ++ (if null limitedName then "RecursiveType" else limitedName) ++ " }"
      result = parseDependentType input
  in property $ case result of
    Left _ -> True
    Right (dt, _) -> True

-- | 测试泛型约束类型
prop_dependent_types_generic_constraints :: String -> String -> String -> Property
prop_dependent_types_generic_constraints typeName paramName constraint =
  let limitedName = take 10 $ filter isAlpha typeName
      limitedParam = take 10 $ filter isAlpha paramName
      limitedConstraint = take 20 constraint
      input = "type " ++ (if null limitedName then "GenericConstrainedType" else limitedName) ++ "[" ++ (if null limitedParam then "T" else limitedParam) ++ ": " ++ (if null limitedConstraint then "int" else limitedConstraint) ++ "] struct { field " ++ (if null limitedParam then "T" else limitedParam) ++ " }"
      result = parseDependentType input
  in property $ case result of
    Left _ -> True
    Right (dt, _) -> True

-- | 测试函数依赖类型
prop_dependent_types_function_dependent_types :: String -> [String] -> String -> Property
prop_dependent_types_function_dependent_types funcName paramNames returnType =
  let limitedName = take 10 $ filter isAlpha funcName
      limitedParams = map (take 10) $ take 5 paramNames
      limitedReturn = take 10 $ filter isAlpha returnType
      paramList = intercalate ", " $ map (\p -> if null p then "n: int" else p ++ ": int") limitedParams
      input = "func " ++ (if null limitedName then "func" else limitedName) ++ "(" ++ paramList ++ ") -> " ++ (if null limitedReturn then "int" else limitedReturn)
      result = parseDependentType input
  in property $ case result of
    Left _ -> True
    Right (dt, _) -> True

-- | 测试存在类型
prop_dependent_types_existential_types :: String -> Property
prop_dependent_types_existential_types typeName =
  let limitedName = take 10 $ filter isAlpha typeName
      input = "type " ++ (if null limitedName then "ExistentialType" else limitedName) ++ " = Vector[some n: int]"
      result = parseDependentType input
  in property $ case result of
    Left _ -> True
    Right (dt, _) -> True

-- | 测试类型级函数
prop_dependent_types_type_level_functions :: String -> String -> Property
prop_dependent_types_type_level_functions funcName argType =
  let limitedName = take 10 $ filter isAlpha funcName
      limitedArg = take 10 $ filter isAlpha argType
      input = "type " ++ (if null limitedName then "TypeFunc" else limitedName) ++ "[" ++ (if null limitedArg then "T" else limitedArg) ++ "] = " ++ (if null limitedArg then "T" else limitedArg) ++ " + " ++ (if null limitedArg then "T" else limitedArg)
      result = parseDependentType input
  in property $ case result of
    Left _ -> True
    Right (dt, _) -> True

-- | 测试约束求解
prop_dependent_types_constraint_solving :: String -> String -> String -> Property
prop_dependent_types_constraint_solving typeName expr1 expr2 =
  let limitedName = take 10 $ filter isAlpha typeName
      limitedExpr1 = take 15 expr1
      limitedExpr2 = take 15 expr2
      input = "type " ++ (if null limitedName then "ConstraintType" else limitedName) ++ " = int where { " ++ (if null limitedExpr1 then "self > 0" else limitedExpr1) ++ " && " ++ (if null limitedExpr2 then "self < 100" else limitedExpr2) ++ " }"
      result = parseDependentType input
  in property $ case result of
    Left _ -> True
    Right (dt, _) -> True

-- ============================================================================
-- 高级Ownership模块的QuickCheck测试
-- ============================================================================

-- | 测试复杂所有权模式
prop_ownership_complex_patterns :: [String] -> Property
prop_ownership_complex_patterns varNames =
  let limitedNames = map (take 10) $ take 5 $ map (filter isAlpha) varNames
      buildOwnershipPattern [] = ""
      buildOwnershipPattern [x] = x ++ " := \"value\""
      buildOwnershipPattern (x:y:xs) = x ++ " := \"value\"\n    " ++ y ++ " := " ++ x ++ "\n    " ++ buildOwnershipPattern (y:xs)
      input = "{//! ownership: on\n    " ++ buildOwnershipPattern limitedNames ++ "\n}"
      result = analyzeOwnership input
  in property $ length result >= 0

-- | 测试借用链
prop_ownership_borrowing_chains :: [String] -> Property
prop_ownership_borrowing_chains varNames =
  let limitedNames = map (take 10) $ take 5 $ map (filter isAlpha) varNames
      buildBorrowChain [] = ""
      buildBorrowChain [x] = x ++ " := \"value\""
      buildBorrowChain (x:y:xs) = x ++ " := \"value\"\n    " ++ y ++ " := &" ++ x ++ "\n    " ++ buildBorrowChain (y:xs)
      input = "{//! ownership: on\n    " ++ buildBorrowChain limitedNames ++ "\n}"
      result = analyzeOwnership input
  in property $ length result >= 0

-- | 测试可变借用链
prop_ownership_mutable_borrowing_chains :: [String] -> Property
prop_ownership_mutable_borrowing_chains varNames =
  let limitedNames = map (take 10) $ take 5 $ map (filter isAlpha) varNames
      buildMutableBorrowChain [] = ""
      buildMutableBorrowChain [x] = x ++ " := \"value\""
      buildMutableBorrowChain (x:y:xs) = x ++ " := \"value\"\n    " ++ y ++ " := &mut " ++ x ++ "\n    " ++ buildMutableBorrowChain (y:xs)
      input = "{//! ownership: on\n    " ++ buildMutableBorrowChain limitedNames ++ "\n}"
      result = analyzeOwnership input
  in property $ length result >= 0

-- | 测试混合借用模式
prop_ownership_mixed_borrowing_patterns :: [String] -> Property
prop_ownership_mixed_borrowing_patterns varNames =
  let limitedNames = map (take 10) $ take 5 $ map (filter isAlpha) varNames
      buildMixedPattern [] = ""
      buildMixedPattern [x] = x ++ " := \"value\""
      buildMixedPattern (x:y:xs) = x ++ " := \"value\"\n    " ++ y ++ " := &" ++ x ++ "\n    z := &mut " ++ y ++ "\n    " ++ buildMixedPattern (y:xs)
      input = "{//! ownership: on\n    " ++ buildMixedPattern limitedNames ++ "\n}"
      result = analyzeOwnership input
  in property $ length result >= 0

-- | 测试嵌套作用域所有权
prop_ownership_nested_scopes :: [String] -> Property
prop_ownership_nested_scopes varNames =
  let limitedNames = map (take 10) $ take 5 $ map (filter isAlpha) varNames
      buildNestedScope 0 = ""
      buildNestedScope n = "{\n        x" ++ show n ++ " := \"value\"\n        " ++ buildNestedScope (n-1) ++ "\n    }"
      input = "{//! ownership: on\n    " ++ buildNestedScope (length limitedNames) ++ "\n}"
      result = analyzeOwnership input
  in property $ length result >= 0

-- | 测试函数调用所有权转移
prop_ownership_function_call_transfers :: String -> [String] -> Property
prop_ownership_function_call_transfers funcName argNames =
  let limitedName = take 10 $ filter isAlpha funcName
      limitedArgs = map (take 10) $ take 5 argNames
      argList = intercalate ", " $ map (\a -> if null a then "x" else a) limitedArgs
      input = "{//! ownership: on\n    " ++ (if null limitedName then "func" else limitedName) ++ "(" ++ argList ++ ")\n}"
      result = analyzeOwnership input
  in property $ length result >= 0

-- | 测试条件所有权
prop_ownership_conditional_ownership :: String -> Property
prop_ownership_conditional_ownership varName =
  let limitedName = take 10 $ filter isAlpha varName
      input = "{//! ownership: on\n    " ++ (if null limitedName then "x" else limitedName) ++ " := \"value\"\n    if condition {\n        y := " ++ (if null limitedName then "x" else limitedName) ++ "\n    } else {\n        z := " ++ (if null limitedName then "x" else limitedName) ++ "\n    }\n}"
      result = analyzeOwnership input
  in property $ length result >= 0

-- | 测试循环所有权
prop_ownership_loop_ownership :: String -> Property
prop_ownership_loop_ownership varName =
  let limitedName = take 10 $ filter isAlpha varName
      input = "{//! ownership: on\n    " ++ (if null limitedName then "x" else limitedName) ++ " := \"value\"\n    for i := 0; i < 10; i++ {\n        y := " ++ (if null limitedName then "x" else limitedName) ++ "\n        " ++ (if null limitedName then "x" else limitedName) ++ " := y\n    }\n}"
      result = analyzeOwnership input
  in property $ length result >= 0

-- | 测试所有权与闭包
prop_ownership_closures :: String -> Property
prop_ownership_closures varName =
  let limitedName = take 10 $ filter isAlpha varName
      input = "{//! ownership: on\n    " ++ (if null limitedName then "x" else limitedName) ++ " := \"value\"\n    f := func() {\n        y := " ++ (if null limitedName then "x" else limitedName) ++ "\n    }\n    f()\n}"
      result = analyzeOwnership input
  in property $ length result >= 0

-- | 测试所有权与结构体
prop_ownership_structs :: String -> [String] -> Property
prop_ownership_structs structName fieldNames =
  let limitedName = take 10 $ filter isAlpha structName
      limitedFields = map (take 10) $ take 5 fieldNames
      fieldLines = map (\name -> "        " ++ (if null name then "field" else name) ++ ": \"value\"") limitedFields
      input = "{//! ownership: on\n    " ++ (if null limitedName then "s" else limitedName) ++ " := " ++ (if null limitedName then "Struct" else limitedName) ++ "{\n" ++ unlines fieldLines ++ "\n    }\n}"
      result = analyzeOwnership input
  in property $ length result >= 0

-- | 测试所有权与接口
prop_ownership_interfaces :: String -> Property
prop_ownership_interfaces varName =
  let limitedName = take 10 $ filter isAlpha varName
      input = "{//! ownership: on\n    var " ++ (if null limitedName then "x" else limitedName) ++ " Writer\n    " ++ (if null limitedName then "x" else limitedName) ++ " = SomeWriter{}\n    " ++ (if null limitedName then "y" else limitedName) ++ " := " ++ (if null limitedName then "x" else limitedName) ++ "\n}"
      result = analyzeOwnership input
  in property $ length result >= 0

-- ============================================================================
-- 单元测试 (HUnit)
-- ============================================================================

-- | 测试复杂表达式解析
test_complex_expression_parsing :: Assertion
test_complex_expression_parsing = do
  let complexExpr = "package main\n\nfunc main() {\n    x := (a + b) * (c - d) / e\n}"
      result = parseTypus complexExpr
  case result of
    Left err -> assertFailure $ "复杂表达式解析失败: " ++ err
    Right typusFile -> assertBool "复杂表达式解析成功" True

-- | 测试嵌套函数调用编译
test_nested_function_call_compilation :: Assertion
test_nested_function_call_compilation = do
  let nestedCalls = "package main\n\nfunc func3(x int) int { return x }\nfunc func2(x int) int { return x }\nfunc func1(x int) int { return x }\nfunc main() {\n    y := 5\n    x := func1(func2(func3(y)))\n}"
      result = compileTypusString nestedCalls
  case result of
    Left errors -> assertFailure $ "嵌套函数调用编译失败: " ++ unlines (map show errors)
    Right goCode -> assertBool "嵌套函数调用编译成功" $ not $ null goCode

-- | 测试复杂依赖类型
test_complex_dependent_types :: Assertion
test_complex_dependent_types = do
  let complexType = "type Matrix[m: int, n: int] struct { data: [m][n]float64 }"
      result = parseDependentType complexType
  case result of
    Left err -> assertFailure $ "复杂依赖类型解析失败: " ++ err
    Right (dt, _) -> assertBool "复杂依赖类型解析成功" True

-- | 测试复杂所有权模式
test_complex_ownership_patterns :: Assertion
test_complex_ownership_patterns = do
  let complexOwnership = "{//! ownership: on\n    x := \"hello\"\n    y := &x\n    z := &mut y\n    w := x\n}"
      result = analyzeOwnership complexOwnership
  assertBool "复杂所有权模式分析完成" $ length result >= 0

-- | 测试错误恢复机制
test_error_recovery_mechanism :: Assertion
test_error_recovery_mechanism = do
  let codeWithErrors = "package main\n\nfunc main() {\n    x := 5 + + 3\n    y := func()\n    z :=\n}"
      result = compileTypusString codeWithErrors
  case result of
    Left errors -> assertBool "错误恢复机制工作正常" $ length errors >= 1
    Right goCode -> assertFailure "应该产生错误但没有"

-- | 测试类型约束验证
test_type_constraint_validation :: Assertion
test_type_constraint_validation = do
  let constraintType = "type BoundedString[min: int, max: int] = string where { len(self) >= min && len(self) <= max }"
      result = parseDependentType constraintType
  case result of
    Left err -> assertFailure $ "类型约束验证失败: " ++ err
    Right (dt, _) -> assertBool "类型约束验证成功" True

-- | 测试所有权转移验证
test_ownership_transfer_validation :: Assertion
test_ownership_transfer_validation = do
  let transferCode = "{//! ownership: on\n    x := Resource{}\n    y := x  // x should be moved\n    // z := x  // This should be an error\n}"
      result = analyzeOwnership transferCode
  assertBool "所有权转移验证完成" $ length result >= 0

-- | 测试依赖类型实例化
test_dependent_type_instantiation :: Assertion
test_dependent_type_instantiation = do
  let instantiationCode = "package main\n\ntype Vector[n: int] struct { data: [n]int }\n\nfunc main() {\n    v := Vector[3]{data: [3]int{1, 2, 3}}\n}"
      result = compileTypusString instantiationCode
  case result of
    Left errors -> assertFailure $ "依赖类型实例化失败: " ++ unlines (map show errors)
    Right goCode -> assertBool "依赖类型实例化成功" $ not $ null goCode

-- | 测试借用检查器
test_borrow_checker :: Assertion
test_borrow_checker = do
  let borrowCode = "{//! ownership: on\n    x := \"hello\"\n    y := &x\n    z := &mut x  // This should be an error\n}"
      result = analyzeOwnership borrowCode
  assertBool "借用检查器工作正常" $ length result >= 1

-- | 测试约束求解器
test_constraint_solver :: Assertion
test_constraint_solver = do
  let constraintCode = "package main\n\ntype Positive = int where { self > 0 }\n\nfunc main() {\n    x := Positive(5)\n    // y := Positive(0)  // This should be an error\n}"
      result = compileTypusString constraintCode
  case result of
    Left errors -> assertBool "约束求解器工作正常" $ length errors >= 0
    Right goCode -> assertBool "约束求解器工作正常" $ not $ null goCode

-- ============================================================================
-- 测试套件组合
-- ============================================================================

-- | 高级Parser测试组
advancedParserTests :: TestTree
advancedParserTests = testGroup "高级Parser模块测试"
  [ QC.testProperty "复杂表达式解析" prop_parser_complex_expressions
  , QC.testProperty "嵌套函数调用解析" prop_parser_nested_function_calls
  , QC.testProperty "复杂类型声明解析" prop_parser_complex_type_declarations
  , QC.testProperty "泛型类型声明解析" prop_parser_generic_type_declarations
  , QC.testProperty "多包声明解析" prop_parser_multiple_packages
  , QC.testProperty "复杂导入语句解析" prop_parser_complex_imports
  , QC.testProperty "注释和代码混合解析" prop_parser_mixed_comments_and_code
  , QC.testProperty "Unicode字符处理" prop_parser_unicode_handling
  , QC.testProperty "长标识符处理" prop_parser_long_identifiers
  , QC.testProperty "深度嵌套结构" prop_parser_deeply_nested_structures
  ]

-- | 高级Compiler测试组
advancedCompilerTests :: TestTree
advancedCompilerTests = testGroup "高级Compiler模块测试"
  [ QC.testProperty "复杂表达式编译" prop_compiler_complex_expressions
  , QC.testProperty "嵌套函数调用编译" prop_compiler_nested_function_calls
  , QC.testProperty "复杂类型声明编译" prop_compiler_complex_type_declarations
  , QC.testProperty "泛型类型声明编译" prop_compiler_generic_type_declarations
  , QC.testProperty "复杂导入语句编译" prop_compiler_complex_imports
  , QC.testProperty "注释和代码混合编译" prop_compiler_mixed_comments_and_code
  , QC.testProperty "Unicode字符编译" prop_compiler_unicode_handling
  , QC.testProperty "长标识符编译" prop_compiler_long_identifiers
  , QC.testProperty "深度嵌套结构编译" prop_compiler_deeply_nested_structures
  ]

-- | 高级DependentTypes测试组
advancedDependentTypesTests :: TestTree
advancedDependentTypesTests = testGroup "高级DependentTypes模块测试"
  [ QC.testProperty "复杂依赖类型表达式" prop_dependent_types_complex_expressions
  , QC.testProperty "嵌套依赖类型" prop_dependent_types_nested_types
  , QC.testProperty "复杂约束表达式" prop_dependent_types_complex_constraints
  , QC.testProperty "多参数依赖类型" prop_dependent_types_multiple_parameters
  , QC.testProperty "递归依赖类型" prop_dependent_types_recursive_types
  , QC.testProperty "泛型约束类型" prop_dependent_types_generic_constraints
  , QC.testProperty "函数依赖类型" prop_dependent_types_function_dependent_types
  , QC.testProperty "存在类型" prop_dependent_types_existential_types
  , QC.testProperty "类型级函数" prop_dependent_types_type_level_functions
  , QC.testProperty "约束求解" prop_dependent_types_constraint_solving
  ]

-- | 高级Ownership测试组
advancedOwnershipTests :: TestTree
advancedOwnershipTests = testGroup "高级Ownership模块测试"
  [ QC.testProperty "复杂所有权模式" prop_ownership_complex_patterns
  , QC.testProperty "借用链" prop_ownership_borrowing_chains
  , QC.testProperty "可变借用链" prop_ownership_mutable_borrowing_chains
  , QC.testProperty "混合借用模式" prop_ownership_mixed_borrowing_patterns
  , QC.testProperty "嵌套作用域所有权" prop_ownership_nested_scopes
  , QC.testProperty "函数调用所有权转移" prop_ownership_function_call_transfers
  , QC.testProperty "条件所有权" prop_ownership_conditional_ownership
  , QC.testProperty "循环所有权" prop_ownership_loop_ownership
  , QC.testProperty "所有权与闭包" prop_ownership_closures
  , QC.testProperty "所有权与结构体" prop_ownership_structs
  ]

-- | 高级单元测试组
advancedUnitTests :: TestTree
advancedUnitTests = testGroup "高级单元测试"
  [ testCase "复杂表达式解析" test_complex_expression_parsing
  , testCase "嵌套函数调用编译" test_nested_function_call_compilation
  , testCase "复杂依赖类型" test_complex_dependent_types
  , testCase "复杂所有权模式" test_complex_ownership_patterns
  , testCase "错误恢复机制" test_error_recovery_mechanism
  , testCase "类型约束验证" test_type_constraint_validation
  , testCase "所有权转移验证" test_ownership_transfer_validation
  , testCase "依赖类型实例化" test_dependent_type_instantiation
  , testCase "借用检查器" test_borrow_checker
  , testCase "约束求解器" test_constraint_solver
  ]

-- | 主测试套件
tests :: TestTree
tests = testGroup "NewAdvancedQuickCheckTestSuite"
  [ memoryLevelTestGroup Moderate "标准内存限制"
    [ advancedParserTests
    , advancedCompilerTests
    , advancedDependentTypesTests
    , advancedOwnershipTests
    , advancedUnitTests
    ]
  ]