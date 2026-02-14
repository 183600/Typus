{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.EnhancedTypusFeaturesTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, intercalate, sort, nub, foldl', group)
import Data.Char (isSpace, isLetter, isDigit, ord, toLower, toUpper, isPrint, isControl)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (foldM, when)
import qualified Parser as P
import qualified Compiler as C
import qualified DependentTypesParser as DTP
import qualified Ownership as O
import qualified Utils as U
import qualified SyntaxValidator as SV

-- | 测试值参数化类型的解析
prop_value_parameterized_types :: Int -> String -> Property
prop_value_parameterized_types n typeName = 
  let validName = all (\c -> isLetter c || c == '_' || isDigit c) typeName && 
                  not (null typeName) && 
                  not (isDigit (head typeName))
      input = "type " ++ typeName ++ "[" ++ show n ++ "] struct { data [" ++ show n ++ "]int }"
  in if validName && n > 0 && n <= 100
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试精确类型的解析
prop_refined_types :: String -> Property
prop_refined_types typeName = 
  let validName = all (\c -> isLetter c || c == '_' || isDigit c) typeName && 
                  not (null typeName) && 
                  not (isDigit (head typeName))
      constraint = "self > 0"
      input = "type " ++ typeName ++ " = int where { " ++ constraint ++ " }"
  in if validName
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试依赖函数签名的解析
prop_dependent_function_signatures :: Int -> String -> Property
prop_dependent_function_signatures n funcName = 
  let validName = all (\c -> isLetter c || c == '_' || isDigit c) funcName && 
                  not (null funcName) && 
                  not (isDigit (head funcName))
      input = "func " ++ funcName ++ "(n: int) -> Vector[" ++ show n ++ "] { return Vector[" ++ show n ++ "]{} }"
  in if validName && n > 0 && n <= 100
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试函数前置条件的解析
prop_function_preconditions :: String -> Property
prop_function_preconditions funcName = 
  let validName = all (\c -> isLetter c || c == '_' || isDigit c) funcName && 
                  not (null funcName) && 
                  not (isDigit (head funcName))
      condition = "n > 0"
      input = "func " ++ funcName ++ "(n: int) -> int where { " ++ condition ++ " } { return n }"
  in if validName
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试所有权移动语义的解析
prop_ownership_move_semantics :: String -> Property
prop_ownership_move_semantics varName = 
  let validName = all (\c -> isLetter c || c == '_' || isDigit c) varName && 
                  not (null varName) && 
                  not (isDigit (head varName))
      input = "{//! ownership: on\n" ++
              "  " ++ varName ++ " := NewMyString(\"hello\")\n" ++
              "  " ++ varName ++ "2 := " ++ varName ++ "  // 移动\n" ++
              "}"
  in if validName
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试借用语法的解析
prop_borrowing_syntax :: String -> Property
prop_borrowing_syntax varName = 
  let validName = all (\c -> isLetter c || c == '_' || isDigit c) varName && 
                  not (null varName) && 
                  not (isDigit (head varName))
      input = "{//! ownership: on\n" ++
              "  " ++ varName ++ " := NewMyString(\"hello\")\n" ++
              "  r := &" ++ varName ++ "  // 不可变借用\n" ++
              "  m := &mut " ++ varName ++ "  // 可变借用\n" ++
              "}"
  in if validName
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试文件级指令的解析
prop_file_level_directives :: String -> Property
prop_file_level_directives packageName = 
  let validName = all (\c -> isLetter c || c == '_' || isDigit c) packageName && 
                  not (null packageName) && 
                  not (isDigit (head packageName))
      directive = "//! ownership: on"
      input = directive ++ "\npackage " ++ packageName
  in if validName
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试块级指令的解析
prop_block_level_directives :: String -> Property
prop_block_level_directives varName = 
  let validName = all (\c -> isLetter c || c == '_' || isDigit c) varName && 
                  not (null varName) && 
                  not (isDigit (head varName))
      input = "func test() {\n" ++
              "  // 普通 Go 代码\n" ++
              "  {//! ownership: on\n" ++
              "    " ++ varName ++ " := NewMyString(\"hello\")\n" ++
              "  }\n" ++
              "}"
  in if validName
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试断言语法的解析
prop_assert_syntax :: String -> Property
prop_assert_syntax varName = 
  let validName = all (\c -> isLetter c || c == '_' || isDigit c) varName && 
                  not (null varName) && 
                  not (isDigit (head varName))
      assertType = "assert " ++ varName ++ " > 0"
      input = "func test() {\n" ++
              "  " ++ varName ++ " := readInt()\n" ++
              "  " ++ assertType ++ "\n" ++
              "}"
  in if validName
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试存在类型的解析
prop_existential_types :: String -> Property
prop_existential_types funcName = 
  let validName = all (\c -> isLetter c || c == '_' || isDigit c) funcName && 
                  not (null funcName) && 
                  not (isDigit (head funcName))
      input = "func " ++ funcName ++ "(input: []float64) -> Vector[some n: int] {\n" ++
              "  return Vector[len(input)]{data: input}\n" ++
              "}"
  in if validName
     then case P.parseTypus input of
            Right _ -> property True
            Left _ -> property False
     else property True

-- | 测试套件
testSuite :: TestTree
testSuite = testGroup "Enhanced Typus Features Test Suite"
  [ testProperty "Value parameterized types" prop_value_parameterized_types
  , testProperty "Refined types" prop_refined_types
  , testProperty "Dependent function signatures" prop_dependent_function_signatures
  , testProperty "Function preconditions" prop_function_preconditions
  , testProperty "Ownership move semantics" prop_ownership_move_semantics
  , testProperty "Borrowing syntax" prop_borrowing_syntax
  , testProperty "File level directives" prop_file_level_directives
  , testProperty "Block level directives" prop_block_level_directives
  , testProperty "Assert syntax" prop_assert_syntax
  , testProperty "Existential types" prop_existential_types
  ]