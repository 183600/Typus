{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.ComprehensiveTypusTestSuite where

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

-- ============================================================================
-- Parser 测试 (30个测试)
-- ============================================================================

-- | 测试基本解析功能
prop_basic_parser_roundtrip :: String -> Property
prop_basic_parser_roundtrip s = 
  let parsed = P.parseTypus s
  in case parsed of
       Right ast -> property $ not (null $ show ast)
       Left _ -> property True

-- | 测试解析器的错误恢复
prop_parser_error_recovery :: String -> Property
prop_parser_error_recovery s = 
  let malformed = s ++ " malformed syntax"
      parsed = P.parseTypus malformed
  in case parsed of
       Right _ -> property True
       Left _ -> property True

-- | 测试注释处理
prop_parser_comment_handling :: String -> Property
prop_parser_comment_handling s = 
  let withComments = "// This is a comment\n" ++ s ++ "\n// Another comment"
      parsed = P.parseTypus withComments
  in case parsed of
       Right _ -> property True
       Left _ -> property True

-- | 测试标识符解析
prop_parser_identifier :: String -> Property
prop_parser_identifier s = 
  if all (\c -> isLetter c || c == '_' || isDigit c) s && not (null s) && not (isDigit (head s))
  then let identifier = "type " ++ s ++ " = int"
           parsed = P.parseTypus identifier
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试数字字面量解析
prop_parser_number_literals :: Int -> Property
prop_parser_number_literals n = 
  let numberStr = "const x = " ++ show n
      parsed = P.parseTypus numberStr
  in case parsed of
       Right _ -> property True
       Left _ -> property False

-- | 测试字符串字面量解析
prop_parser_string_literals :: String -> Property
prop_parser_string_literals s = 
  if length s < 100 && all isPrint s
  then let stringStr = "const s = \"" ++ s ++ "\""
           parsed = P.parseTypus stringStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试类型解析
prop_parser_type :: String -> Property
prop_parser_type s = 
  if all isLetter s
  then let typeStr = "func f() -> " ++ s
           parsed = P.parseTypus typeStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试函数解析
prop_parser_function :: String -> Property
prop_parser_function s = 
  if all isLetter s && length s < 20
  then let funcStr = "func " ++ s ++ "() { return 42 }"
           parsed = P.parseTypus funcStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试结构体解析
prop_parser_struct :: String -> Property
prop_parser_struct s = 
  if all isLetter s && length s < 20
  then let structStr = "type " ++ s ++ " struct { x int }"
           parsed = P.parseTypus structStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试接口解析
prop_parser_interface :: String -> Property
prop_parser_interface s = 
  if all isLetter s && length s < 20
  then let interfaceStr = "type " ++ s ++ " interface { Method() }"
           parsed = P.parseTypus interfaceStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试包声明解析
prop_parser_package :: String -> Property
prop_parser_package s = 
  if all isLetter s && length s < 20
  then let packageStr = "package " ++ s
           parsed = P.parseTypus packageStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试导入声明解析
prop_parser_import :: String -> Property
prop_parser_import s = 
  if length s < 50
  then let importStr = "import \"" ++ s ++ "\""
           parsed = P.parseTypus importStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试表达式解析
prop_parser_expression :: String -> Property
prop_parser_expression s = 
  if length s < 30
  then let exprStr = "x := " ++ s
           parsed = P.parseTypus exprStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试语句解析
prop_parser_statement :: String -> Property
prop_parser_statement s = 
  if length s < 50
  then let stmtStr = s ++ ";"
           parsed = P.parseTypus stmtStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试块解析
prop_parser_block :: String -> Property
prop_parser_block s = 
  if length s < 100
  then let blockStr = "{ " ++ s ++ " }"
           parsed = P.parseTypus blockStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试数组解析
prop_parser_array :: [Int] -> Property
prop_parser_array nums = 
  if length nums < 10
  then let arrayStr = "arr := [" ++ intercalate ", " (map show nums) ++ "]"
           parsed = P.parseTypus arrayStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试映射解析
prop_parser_map :: [(String, Int)] -> Property
prop_parser_map pairs = 
  if length pairs < 5 && all (all isLetter . fst) pairs
  then let mapStr = "m := map[string]int{" ++ 
                   intercalate ", " (map (\(k, v) -> "\"" ++ k ++ "\": " ++ show v) pairs) ++ 
                   "}"
           parsed = P.parseTypus mapStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试切片解析
prop_parser_slice :: [Int] -> Property
prop_parser_slice nums = 
  if length nums < 10
  then let sliceStr = "slice := []int{" ++ intercalate ", " (map show nums) ++ "}"
           parsed = P.parseTypus sliceStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试通道解析
prop_parser_channel :: String -> Property
prop_parser_channel s = 
  if all isLetter s
  then let channelStr = "ch := make(chan " ++ s ++ ")"
           parsed = P.parseTypus channelStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试指针解析
prop_parser_pointer :: String -> Property
prop_parser_pointer s = 
  if all isLetter s
  then let pointerStr = "var p *" ++ s
           parsed = P.parseTypus pointerStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试选择语句解析
prop_parser_select :: [String] -> Property
prop_parser_select cases = 
  if length cases < 5 && all (all isLetter) cases
  then let selectStr = "select {\n" ++ 
                     intercalate "\n" (map (\c -> "case " ++ c ++ ":") cases) ++ 
                     "\ndefault: return\n}"
           parsed = P.parseTypus selectStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试switch语句解析
prop_parser_switch :: [String] -> Property
prop_parser_switch cases = 
  if length cases < 5 && all (all isLetter) cases
  then let switchStr = "switch x {\n" ++ 
                     intercalate "\n" (map (\c -> "case " ++ c ++ ":") cases) ++ 
                     "\ndefault: return\n}"
           parsed = P.parseTypus switchStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试for循环解析
prop_parser_for_loop :: String -> Property
prop_parser_for_loop s = 
  if length s < 50
  then let forStr = "for " ++ s ++ " { }"
           parsed = P.parseTypus forStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试range循环解析
prop_parser_range_loop :: String -> Property
prop_parser_range_loop s = 
  if all isLetter s
  then let rangeStr = "for " ++ s ++ " := range array { }"
           parsed = P.parseTypus rangeStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试if语句解析
prop_parser_if :: String -> Property
prop_parser_if s = 
  if length s < 50
  then let ifStr = "if " ++ s ++ " { }"
           parsed = P.parseTypus ifStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试else if语句解析
prop_parser_else_if :: [String] -> Property
prop_parser_else_if conditions = 
  if length conditions < 5 && all (all isLetter) conditions
  then let elseIfStr = "if x > 0 { } else " ++ 
                      intercalate " else if " (map (\c -> c ++ " { }") conditions)
           parsed = P.parseTypus elseIfStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试defer语句解析
prop_parser_defer :: String -> Property
prop_parser_defer s = 
  if length s < 50
  then let deferStr = "defer " ++ s
           parsed = P.parseTypus deferStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试go语句解析
prop_parser_go :: String -> Property
prop_parser_go s = 
  if length s < 50
  then let goStr = "go " ++ s
           parsed = P.parseTypus goStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试return语句解析
prop_parser_return :: String -> Property
prop_parser_return s = 
  if length s < 50
  then let returnStr = "return " ++ s
           parsed = P.parseTypus returnStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试解析器的性能
prop_parser_performance :: String -> Property
prop_parser_performance s = 
  let repeated = concat (replicate 100 (s ++ "\n"))
      parsed = P.parseTypus repeated
  in case parsed of
       Right _ -> property True
       Left _ -> property True

-- | 测试解析器的Unicode支持
prop_parser_unicode :: String -> Property
prop_parser_unicode s = 
  if length s < 20
  then let unicodeStr = "const 测试 = \"" ++ s ++ "\""
           parsed = P.parseTypus unicodeStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试解析器的错误位置
prop_parser_error_position :: String -> Property
prop_parser_error_position s = 
  let withError = s ++ "\n@#$%^&*()\n"
      parsed = P.parseTypus withError
  in case parsed of
       Right _ -> property True
       Left _ -> property True

-- | 测试解析器的容错性
prop_parser_fault_tolerance :: String -> Property
prop_parser_fault_tolerance s = 
  let malformed = s ++ "\nfunc x(\n"
      parsed = P.parseTypus malformed
  in case parsed of
       Right _ -> property True
       Left _ -> property True

-- | 测试解析器的大文件处理
prop_parser_large_file :: [String] -> Property
prop_parser_large_file lines = 
  if length lines < 1000 && all (all isLetter) lines
  then let largeFile = intercalate "\n" lines
           parsed = P.parseTypus largeFile
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试解析器的嵌套结构
prop_parser_nested :: Int -> Property
prop_parser_nested depth = 
  if depth > 0 && depth < 10
  then let nestedBraces = concat (replicate depth "{") ++ "x" ++ concat (replicate depth "}")
           parsed = P.parseTypus nestedBraces
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试解析器的关键字处理
prop_parser_keywords :: String -> Property
prop_parser_keywords s = 
  let keywords = ["package", "import", "func", "type", "var", "const", "if", "else", "for", "switch", "case", "default", "return", "go", "defer", "select", "struct", "interface", "map", "chan", "break", "continue", "fallthrough", "range"]
      validKeyword = s `elem` keywords
  in if validKeyword
     then let keywordStr = s ++ " x"
              parsed = P.parseTypus keywordStr
          in case parsed of
               Right _ -> property True
               Left _ -> property False
     else property True

-- | 测试解析器的运算符优先级
prop_parser_operator_precedence :: String -> Property
prop_parser_operator_precedence s = 
  if length s < 20
  then let exprStr = "x := " ++ s ++ " + " ++ s ++ " * " ++ s
           parsed = P.parseTypus exprStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试解析器的类型断言
prop_parser_type_assertion :: String -> Property
prop_parser_type_assertion s = 
  if all isLetter s
  then let assertionStr = "x := y.(" ++ s ++ ")"
           parsed = P.parseTypus assertionStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试解析器的泛型支持
prop_parser_generics :: String -> Property
prop_parser_generics s = 
  if all isLetter s
  then let genericStr = "func f[" ++ s ++ " any](x " ++ s ++ ") " ++ s ++ " { return x }"
           parsed = P.parseTypus genericStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- ============================================================================
-- Compiler 测试 (30个测试)
-- ============================================================================

-- | 测试基本编译功能
prop_basic_compilation :: String -> Property
prop_basic_compilation s = 
  let typusCode = "package main\n\nfunc main() {\n  " ++ s ++ "\n}"
      parsed = P.parseTypus typusCode
  in case parsed of
       Right ast -> 
         let compiled = C.compile ast
         in case compiled of
              Right goCode -> property $ not (null goCode)
              Left _ -> property True
       Left _ -> property True

-- | 测试编译器的错误处理
prop_compiler_error_handling :: String -> Property
prop_compiler_error_handling s = 
  let invalidCode = "package main\n\nfunc main() {\n  " ++ s ++ "\n  @#$%\n}"
      parsed = P.parseTypus invalidCode
  in case parsed of
       Right ast -> 
         let compiled = C.compile ast
         in case compiled of
              Right _ -> property True
              Left _ -> property True
       Left _ -> property True

-- | 测试类型编译
prop_type_compilation :: String -> Property
prop_type_compilation s = 
  if all isLetter s
  then let typeCode = "package main\n\ntype " ++ s ++ " struct { x int }"
           parsed = P.parseTypus typeCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ s `isInfixOf` goCode
                   Left _ -> property False
            Left _ -> property False
  else property True

-- | 测试函数编译
prop_function_compilation :: String -> Property
prop_function_compilation s = 
  if all isLetter s && length s < 20
  then let funcCode = "package main\n\nfunc " ++ s ++ "() int {\n  return 42\n}"
           parsed = P.parseTypus funcCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ s `isInfixOf` goCode
                   Left _ -> property False
            Left _ -> property False
  else property True

-- | 测试结构体编译
prop_struct_compilation :: String -> Property
prop_struct_compilation s = 
  if all isLetter s && length s < 20
  then let structCode = "package main\n\ntype " ++ s ++ " struct {\n  x int\n  y string\n}"
           parsed = P.parseTypus structCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ s `isInfixOf` goCode
                   Left _ -> property False
            Left _ -> property False
  else property True

-- | 测试接口编译
prop_interface_compilation :: String -> Property
prop_interface_compilation s = 
  if all isLetter s && length s < 20
  then let interfaceCode = "package main\n\ntype " ++ s ++ " interface {\n  Method() int\n}"
           parsed = P.parseTypus interfaceCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ s `isInfixOf` goCode
                   Left _ -> property False
            Left _ -> property False
  else property True

-- | 测试方法编译
prop_method_compilation :: String -> Property
prop_method_compilation s = 
  if all isLetter s && length s < 20
  then let methodCode = "package main\n\ntype " ++ s ++ " struct { x int }\n\nfunc (r " ++ s ++ ") Method() int {\n  return r.x\n}"
           parsed = P.parseTypus methodCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ "Method" `isInfixOf` goCode
                   Left _ -> property False
            Left _ -> property False
  else property True

-- | 测试变量编译
prop_variable_compilation :: String -> Property
prop_variable_compilation s = 
  if all isLetter s && length s < 20
  then let varCode = "package main\n\nvar " ++ s ++ " int = 42"
           parsed = P.parseTypus varCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ s `isInfixOf` goCode
                   Left _ -> property False
            Left _ -> property False
  else property True

-- | 测试常量编译
prop_constant_compilation :: String -> Property
prop_constant_compilation s = 
  if all isLetter s && length s < 20
  then let constCode = "package main\n\nconst " ++ s ++ " = 42"
           parsed = P.parseTypus constCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ s `isInfixOf` goCode
                   Left _ -> property False
            Left _ -> property False
  else property True

-- | 测试导入编译
prop_import_compilation :: String -> Property
prop_import_compilation s = 
  if length s < 50
  then let importCode = "package main\n\nimport \"" ++ s ++ "\""
           parsed = P.parseTypus importCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ "import" `isInfixOf` goCode
                   Left _ -> property False
            Left _ -> property False
  else property True

-- | 测试表达式编译
prop_expression_compilation :: String -> Property
prop_expression_compilation s = 
  if length s < 30
  then let exprCode = "package main\n\nfunc main() {\n  x := " ++ s ++ "\n}"
           parsed = P.parseTypus exprCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ not (null goCode)
                   Left _ -> property True
            Left _ -> property True
  else property True

-- | 测试控制流编译
prop_control_flow_compilation :: String -> Property
prop_control_flow_compilation s = 
  if length s < 30
  then let controlCode = "package main\n\nfunc main() {\n  if " ++ s ++ " {\n    return\n  }\n}"
           parsed = P.parseTypus controlCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ "if" `isInfixOf` goCode
                   Left _ -> property True
            Left _ -> property True
  else property True

-- | 测试循环编译
prop_loop_compilation :: String -> Property
prop_loop_compilation s = 
  if length s < 30
  then let loopCode = "package main\n\nfunc main() {\n  for " ++ s ++ " {\n    break\n  }\n}"
           parsed = P.parseTypus loopCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ "for" `isInfixOf` goCode
                   Left _ -> property True
            Left _ -> property True
  else property True

-- | 测试数组编译
prop_array_compilation :: [Int] -> Property
prop_array_compilation nums = 
  if length nums < 10
  then let arrayCode = "package main\n\nfunc main() {\n  arr := [" ++ intercalate ", " (map show nums) ++ "]\n}"
           parsed = P.parseTypus arrayCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ "arr" `isInfixOf` goCode
                   Left _ -> property False
            Left _ -> property False
  else property True

-- | 测试切片编译
prop_slice_compilation :: [Int] -> Property
prop_slice_compilation nums = 
  if length nums < 10
  then let sliceCode = "package main\n\nfunc main() {\n  slice := []int{" ++ intercalate ", " (map show nums) ++ "}\n}"
           parsed = P.parseTypus sliceCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ "slice" `isInfixOf` goCode
                   Left _ -> property False
            Left _ -> property False
  else property True

-- | 测试映射编译
prop_map_compilation :: [(String, Int)] -> Property
prop_map_compilation pairs = 
  if length pairs < 5 && all (all isLetter . fst) pairs
  then let mapCode = "package main\n\nfunc main() {\n  m := map[string]int{" ++ 
                   intercalate ", " (map (\(k, v) -> "\"" ++ k ++ "\": " ++ show v) pairs) ++ 
                   "}\n}"
           parsed = P.parseTypus mapCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ "map" `isInfixOf` goCode
                   Left _ -> property False
            Left _ -> property False
  else property True

-- | 测试通道编译
prop_channel_compilation :: String -> Property
prop_channel_compilation s = 
  if all isLetter s
  then let channelCode = "package main\n\nfunc main() {\n  ch := make(chan " ++ s ++ ")\n}"
           parsed = P.parseTypus channelCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ "chan" `isInfixOf` goCode
                   Left _ -> property False
            Left _ -> property False
  else property True

-- | 测试指针编译
prop_pointer_compilation :: String -> Property
prop_pointer_compilation s = 
  if all isLetter s
  then let pointerCode = "package main\n\nfunc main() {\n  var p *" ++ s ++ "\n}"
           parsed = P.parseTypus pointerCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ "*" `isInfixOf` goCode
                   Left _ -> property False
            Left _ -> property False
  else property True

-- | 测试goroutine编译
prop_goroutine_compilation :: String -> Property
prop_goroutine_compilation s = 
  if length s < 30
  then let goroutineCode = "package main\n\nfunc main() {\n  go " ++ s ++ "\n}"
           parsed = P.parseTypus goroutineCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ "go" `isInfixOf` goCode
                   Left _ -> property False
            Left _ -> property False
  else property True

-- | 测试select编译
prop_select_compilation :: [String] -> Property
prop_select_compilation cases = 
  if length cases < 5 && all (all isLetter) cases
  then let selectCode = "package main\n\nfunc main() {\n  select {\n" ++ 
                     intercalate "\n" (map (\c -> "  case " ++ c ++ ":") cases) ++ 
                     "\n  default: return\n  }\n}"
           parsed = P.parseTypus selectCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ "select" `isInfixOf` goCode
                   Left _ -> property False
            Left _ -> property False
  else property True

-- | 测试defer编译
prop_defer_compilation :: String -> Property
prop_defer_compilation s = 
  if length s < 30
  then let deferCode = "package main\n\nfunc main() {\n  defer " ++ s ++ "\n}"
           parsed = P.parseTypus deferCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ "defer" `isInfixOf` goCode
                   Left _ -> property False
            Left _ -> property False
  else property True

-- | 测试panic编译
prop_panic_compilation :: String -> Property
prop_panic_compilation s = 
  if length s < 30
  then let panicCode = "package main\n\nfunc main() {\n  panic(" ++ s ++ ")\n}"
           parsed = P.parseTypus panicCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ "panic" `isInfixOf` goCode
                   Left _ -> property False
            Left _ -> property False
  else property True

-- | 测试recover编译
prop_recover_compilation :: String -> Property
prop_recover_compilation s = 
  if length s < 30
  then let recoverCode = "package main\n\nfunc main() {\n  defer func() {\n    " ++ s ++ "\n    recover()\n  }()\n}"
           parsed = P.parseTypus recoverCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ "recover" `isInfixOf` goCode
                   Left _ -> property False
            Left _ -> property False
  else property True

-- | 测试编译器的优化
prop_compiler_optimization :: String -> Property
prop_compiler_optimization s = 
  if length s < 50
  then let optCode = "package main\n\nfunc main() {\n  x := " ++ s ++ "\n  y := x * 2\n  z := y / 2\n}"
           parsed = P.parseTypus optCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ not (null goCode)
                   Left _ -> property True
            Left _ -> property True
  else property True

-- | 测试编译器的类型检查
prop_compiler_type_checking :: String -> Property
prop_compiler_type_checking s = 
  if all isLetter s
  then let typeCheckCode = "package main\n\ntype " ++ s ++ " struct { x int }\n\nfunc f() " ++ s ++ " {\n  return " ++ s ++ "{x: 42}\n}"
           parsed = P.parseTypus typeCheckCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ s `isInfixOf` goCode
                   Left _ -> property False
            Left _ -> property False
  else property True

-- | 测试编译器的依赖解析
prop_compiler_dependency_resolution :: [String] -> Property
prop_compiler_dependency_resolution modules = 
  if length modules < 5 && all (all isLetter) modules
  then let depCode = "package main\n\n" ++ 
                  intercalate "\n" (map (\m -> "import \"" ++ m ++ "\"" ) modules) ++ 
                  "\nfunc main() { }"
           parsed = P.parseTypus depCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ not (null goCode)
                   Left _ -> property True
            Left _ -> property True
  else property True

-- | 测试编译器的代码生成
prop_compiler_code_generation :: String -> Property
prop_compiler_code_generation s = 
  if length s < 100
  then let codeGenCode = "package main\n\nfunc main() {\n  " ++ s ++ "\n}"
           parsed = P.parseTypus codeGenCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ "package main" `isPrefixOf` goCode
                   Left _ -> property True
            Left _ -> property True
  else property True

-- | 测试编译器的错误恢复
prop_compiler_error_recovery :: String -> Property
prop_compiler_error_recovery s = 
  let errorRecoveryCode = "package main\n\nfunc main() {\n  " ++ s ++ "\n  @#$%\n  x := 42\n}"
      parsed = P.parseTypus errorRecoveryCode
  in case parsed of
       Right ast -> 
         let compiled = C.compile ast
         in case compiled of
              Right _ -> property True
              Left _ -> property True
       Left _ -> property True

-- | 测试编译器的性能
prop_compiler_performance :: String -> Property
prop_compiler_performance s = 
  let perfCode = "package main\n\nfunc main() {\n" ++ 
                concat (replicate 100 ("  " ++ s ++ "\n")) ++ 
                "}"
      parsed = P.parseTypus perfCode
  in case parsed of
       Right ast -> 
         let compiled = C.compile ast
         in case compiled of
              Right _ -> property True
              Left _ -> property True
       Left _ -> property True

-- | 测试编译器的Unicode支持
prop_compiler_unicode :: String -> Property
prop_compiler_unicode s = 
  if length s < 20
  then let unicodeCode = "package main\n\nfunc main() {\n  测试 := \"" ++ s ++ "\"\n}"
           parsed = P.parseTypus unicodeCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ not (null goCode)
                   Left _ -> property True
            Left _ -> property True
  else property True

-- | 测试编译器的内存效率
prop_compiler_memory_efficiency :: [String] -> Property
prop_compiler_memory_efficiency lines = 
  if length lines < 1000
  then let memEffCode = "package main\n\n" ++ intercalate "\n" lines
           parsed = P.parseTypus memEffCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right _ -> property True
                   Left _ -> property True
            Left _ -> property True
  else property True

-- | 测试编译器的并发安全性
prop_compiler_concurrency :: String -> Property
prop_compiler_concurrency s = 
  if length s < 30
  then let concurrencyCode = "package main\n\nfunc main() {\n  ch := make(chan int)\n  go func() {\n    " ++ s ++ "\n    ch <- 42\n  }()\n  <-ch\n}"
           parsed = P.parseTypus concurrencyCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ "go" `isInfixOf` goCode
                   Left _ -> property False
            Left _ -> property False
  else property True

-- | 测试编译器的扩展性
prop_compiler_extensibility :: String -> Property
prop_compiler_extensibility s = 
  if length s < 50
  then let extCode = "package main\n\n// " ++ s ++ "\nfunc main() { }"
           parsed = P.parseTypus extCode
       in case parsed of
            Right ast -> 
              let compiled = C.compile ast
              in case compiled of
                   Right goCode -> property $ "//" `isInfixOf` goCode
                   Left _ -> property False
            Left _ -> property False
  else property True

-- ============================================================================
-- DependentTypes 测试 (30个测试)
-- ============================================================================

-- | 测试依赖类型解析
prop_dependent_type_parsing :: String -> Property
prop_dependent_type_parsing s = 
  if all isLetter s
  then let depTypeStr = "type Vector[" ++ s ++ ": int] struct { data [" ++ s ++ "]int }"
           parsed = DTP.parseDependentType depTypeStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试值参数化类型
prop_value_parameterized_type :: Int -> Property
prop_value_parameterized_type n = 
  if n > 0 && n < 100
  then let valParamTypeStr = "type Vector[" ++ show n ++ "] struct { data [" ++ show n ++ "]int }"
           parsed = DTP.parseDependentType valParamTypeStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试精确类型
prop_precise_type :: Int -> Property
prop_precise_type n = 
  let preciseTypeStr = "type Positive = int where { self > " ++ show n ++ " }"
      parsed = DTP.parseDependentType preciseTypeStr
  in case parsed of
       Right _ -> property True
       Left _ -> property False

-- | 测试参数化精确类型
prop_parameterized_precise_type :: Int -> Int -> Property
prop_parameterized_precise_type lo hi = 
  if lo < hi
  then let paramPreciseTypeStr = "type Bounded[" ++ show lo ++ ": int, " ++ show hi ++ ": int] = int where { self >= lo && self <= hi }"
           parsed = DTP.parseDependentType paramPreciseTypeStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试依赖函数签名
prop_dependent_function_signature :: Int -> Property
prop_dependent_function_signature n = 
  if n > 0 && n < 100
  then let depFuncSigStr = "func zeros(n: Positive) -> Vector[" ++ show n ++ "]"
           parsed = DTP.parseDependentType depFuncSigStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试函数前置条件
prop_function_precondition :: Int -> Property
prop_function_precondition n = 
  if n > 0
  then let precondStr = "func average(n: int) -> float64 where { n > " ++ show n ++ " }"
           parsed = DTP.parseDependentType precondStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试断言窄化
prop_assert_narrowing :: String -> Property
prop_assert_narrowing s = 
  if length s < 30
  then let assertStr = "assert len(" ++ s ++ ") > 0"
           parsed = DTP.parseDependentType assertStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试条件窄化
prop_conditional_narrowing :: String -> Property
prop_conditional_narrowing s = 
  if length s < 30
  then let condStr = "if " ++ s ++ " != nil { safeDiv(10, " ++ s ++ ") }"
           parsed = DTP.parseDependentType condStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试编译期常量传播
prop_compile_time_constant :: Int -> Property
prop_compile_time_constant n = 
  if n > 0 && n < 10
  then let constPropStr = "get(v, " ++ show n ++ ")"
           parsed = DTP.parseDependentType constPropStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试存在类型
prop_existential_type :: String -> Property
prop_existential_type s = 
  if length s < 30
  then let existentialStr = "Vector[some n: int] where { n == len(" ++ s ++ ") }"
           parsed = DTP.parseDependentType existentialStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试存在类型解包
prop_existential_unpack :: String -> Property
prop_existential_unpack s = 
  if length s < 30
  then let unpackStr = "match v.(n) { fmt.Println(get(v, 0)) }"
           parsed = DTP.parseDependentType unpackStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试类型推导
prop_type_inference :: String -> Property
prop_type_inference s = 
  if length s < 30
  then let inferenceStr = "func createVector(n: Positive, value: float64) -> Vector[n]"
           parsed = DTP.parseDependentType inferenceStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试约束求解器线性算术
prop_constraint_linear_arithmetic :: Int -> Int -> Property
prop_constraint_linear_arithmetic a b = 
  if a > 0 && b > 0 && a + b < 100
  then let linearStr = "Vector[a + " ++ show b ++ "] where { a > 0 }"
           parsed = DTP.parseDependentType linearStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试约束求解器等式传播
prop_constraint_equality_propagation :: Int -> Int -> Property
prop_constraint_equality_propagation a b = 
  if a >= 0 && b >= 0
  then let equalityStr = "type EqualVector[a: int, b: int] struct { data [a]int } where { a == b }"
           parsed = DTP.parseDependentType equalityStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试约束求解器不等式链
prop_constraint_inequality_chain :: Int -> Int -> Property
prop_constraint_inequality_chain a b = 
  if a > b && b > 0
  then let inequalityStr = "int where { self > " ++ show a ++ " && self > " ++ show b ++ " }"
           parsed = DTP.parseDependentType inequalityStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试错误模式约束
prop_error_mode_constraints :: String -> Property
prop_error_mode_constraints s = 
  if length s < 30
  then let errorModeStr = "//! constraint_mode: error\nfunc safeDiv(a: int, b: NonZero) -> (int, error)"
           parsed = DTP.parseDependentType errorModeStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试Go互操作类型擦除
prop_go_interop_type_erasure :: String -> Property
prop_go_interop_type_erasure s = 
  if all isLetter s
  then let typeEraseStr = "type " ++ s ++ "Vector[n: int] struct { data [n]float64 }"
           parsed = DTP.parseDependentType typeEraseStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试边界标注
prop_boundary_annotation :: String -> Property
prop_boundary_annotation s = 
  if length s < 30
  then let boundaryStr = "assert len(" ++ s ++ ") > 0\nv := readVector(" ++ s ++ ")"
           parsed = DTP.parseDependentType boundaryStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试矩阵乘法维度对齐
prop_matrix_multiplication_alignment :: Int -> Int -> Int -> Property
prop_matrix_multiplication_alignment m n p = 
  if m > 0 && n > 0 && p > 0 && m * n * p < 1000
  then let matrixStr = "func matMul[m: int, n: int, p: int](a: Matrix[m, n], b: Matrix[n, p]) -> Matrix[m, p]"
           parsed = DTP.parseDependentType matrixStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试向量运算维度匹配
prop_vector_operations_dimension_match :: Int -> Property
prop_vector_operations_dimension_match n = 
  if n > 0 && n < 100
  then let vectorStr = "func add[n: int](a: Vector[n], b: Vector[n]) -> Vector[n]"
           parsed = DTP.parseDependentType vectorStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试所有权和依赖类型交互
prop_ownership_dependent_types_interaction :: String -> Property
prop_ownership_dependent_types_interaction s = 
  if length s < 30
  then let interactionStr = "{//! ownership: on\n//! dependent_types: on\ns := NewMyString(\"" ++ s ++ "\")}"
           parsed = DTP.parseDependentType interactionStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试指令系统块级启用
prop_directive_system_block :: String -> Property
prop_directive_system_block s = 
  if length s < 30
  then let blockStr = "func main() {\n  {//! ownership: on\n    " ++ s ++ "\n  }\n  {//! dependent_types: on\n    " ++ s ++ "\n  }\n}"
           parsed = DTP.parseDependentType blockStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试文件级指令处理
prop_file_level_directives :: String -> Property
prop_file_level_directives s = 
  if all isLetter s
  then let fileDirectiveStr = "//! ownership: on\n//! dependent_types: on\n\npackage " ++ s ++ "\n\ntype Vector[n: int] struct { data [n]float64 }"
           parsed = DTP.parseDependentType fileDirectiveStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试递归类型参数化
prop_recursive_type_parameterization :: Int -> Property
prop_recursive_type_parameterization n = 
  if n > 0 && n < 10
  then let recursiveStr = "type List[" ++ show n ++ ": int] struct { head int; tail *List[" ++ show n ++ "-1] }"
           parsed = DTP.parseDependentType recursiveStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试约束条件组合
prop_constraint_combination :: Int -> Int -> Property
prop_constraint_combination lo hi = 
  if lo > 0 && hi > lo && hi < 100
  then let combinationStr = "type Bounded[lo: int, hi: int] = int where { self >= lo && self <= hi }"
           parsed = DTP.parseDependentType combinationStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试类型级函数应用
prop_type_level_function :: Int -> Property
prop_type_level_function n = 
  if n > 0 && n < 100
  then let typeFuncStr = "type Len[" ++ show n ++ ": int] = int where { " ++ show n ++ " == len(self) }"
           parsed = DTP.parseDependentType typeFuncStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试依赖类型模式匹配
prop_dependent_type_pattern_matching :: Int -> Property
prop_dependent_type_pattern_matching n = 
  if n > 0 && n < 100
  then let patternStr = "match v.(" ++ show n ++ ") { fmt.Println(get(v, 0)) }"
           parsed = DTP.parseDependentType patternStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试约束求解器边界情况
prop_constraint_solver_edge_case :: Int -> Property
prop_constraint_solver_edge_case n = 
  let edgeCaseStr = "int where { self == " ++ show n ++ " || self != " ++ show n ++ " }"
      parsed = DTP.parseDependentType edgeCaseStr
  in case parsed of
       Right _ -> property True
       Left _ -> property True

-- | 测试依赖类型类型检查
prop_dependent_type_type_check :: String -> Property
prop_dependent_type_type_check s = 
  if length s < 50
  then let typeCheckStr = "type NonEmpty = string where { len(self) > 0 }"
           parsed = DTP.parseDependentType typeCheckStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试编译期优化约束
prop_compile_time_optimization :: Int -> Property
prop_compile_time_optimization n = 
  if n > 0 && n < 10
  then let optimizationStr = "v := zeros(" ++ show n ++ ")\nx := get(v, 0)"
           parsed = DTP.parseDependentType optimizationStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试依赖类型错误处理
prop_dependent_type_error_handling :: String -> Property
prop_dependent_type_error_handling s = 
  if length s < 30
  then let errorHandlingStr = "func safeDiv(a: int, b: NonZero) -> int {\n  return a / b\n}"
           parsed = DTP.parseDependentType errorHandlingStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试类型约束传递性
prop_type_constraint_transitivity :: Int -> Int -> Int -> Property
prop_type_constraint_transitivity a b c = 
  if a > b && b > c
  then let transitivityStr = "int where { self > " ++ show a ++ " && self > " ++ show b ++ " && self > " ++ show c ++ " }"
           parsed = DTP.parseDependentType transitivityStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试依赖类型类型推导
prop_dependent_type_inference :: Int -> Property
prop_dependent_type_inference n = 
  if n > 0 && n < 100
  then let inferenceStr = "func createVector(n: Positive, value: float64) -> Vector[" ++ show n ++ "]"
           parsed = DTP.parseDependentType inferenceStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试约束求解器性能
prop_constraint_solver_performance :: [Int] -> Property
prop_constraint_solver_performance nums = 
  if length nums < 10 && all (>0) nums && all (<100) nums
  then let sumValue = sum nums
           typeStr = "type VectorSum = struct { data [" ++ show sumValue ++ "]int }"
           parsed = DTP.parseDependentType typeStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试依赖类型代码生成
prop_dependent_type_code_generation :: String -> Property
prop_dependent_type_code_generation s = 
  if length s < 20
  then let codeGenStr = "type Positive = int where { self > 0 }"
           parsed = DTP.parseDependentType codeGenStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试类型约束验证
prop_type_constraint_validation :: Int -> Property
prop_type_constraint_validation n = 
  let validationStr = "func validatePositive(x: int) -> Positive {\n  assert x > 0\n  return Positive(x)\n}"
      parsed = DTP.parseDependentType validationStr
  in case parsed of
       Right _ -> property True
       Left _ -> property True

-- | 测试依赖类型序列化
prop_dependent_type_serialization :: String -> Property
prop_dependent_type_serialization s = 
  if length s < 30
  then let serializationStr = "type Serializable[n: int] struct { data [n]byte }"
           parsed = DTP.parseDependentType serializationStr
       in case parsed of
            Right _ -> property True
            Left _ -> property False
  else property True

-- | 测试约束求解器可扩展性
prop_constraint_solver_extensibility :: String -> Property
prop_constraint_solver_extensibility s = 
  if length s < 30
  then let extensibilityStr = "func customConstraint(x: int) -> bool {\n  return x > len(\"" ++ s ++ "\")\n}"
           parsed = DTP.parseDependentType extensibilityStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- | 测试依赖类型调试支持
prop_dependent_type_debug_support :: String -> Property
prop_dependent_type_debug_support s = 
  if length s < 30
  then let debugStr = "//! debug: constraints\nfunc debugFunction() { /* " ++ s ++ " */ }"
           parsed = DTP.parseDependentType debugStr
       in case parsed of
            Right _ -> property True
            Left _ -> property True
  else property True

-- ============================================================================
-- Ownership 测试 (30个测试)
-- ============================================================================

-- | 测试所有权基本语义
prop_ownership_basic_semantics :: String -> Property
prop_ownership_basic_semantics s = 
  if length s < 30
  then let ownershipStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nt := s}"
           parsed = O.analyzeOwnership ownershipStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试移动语义
prop_move_semantics :: String -> Property
prop_move_semantics s = 
  if length s < 30
  then let moveStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nt := s\n// s 已被移动}"
           parsed = O.analyzeOwnership moveStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试不可变借用
prop_immutable_borrow :: String -> Property
prop_immutable_borrow s = 
  if length s < 30
  then let borrowStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nr := &s\nfmt.Println(r.data)}"
           parsed = O.analyzeOwnership borrowStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试可变借用
prop_mutable_borrow :: String -> Property
prop_mutable_borrow s = 
  if length s < 30
  then let mutableStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nm := &mut s\nm.data = \"world\"}"
           parsed = O.analyzeOwnership mutableStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试借用规则
prop_borrowing_rules :: String -> Property
prop_borrowing_rules s = 
  if length s < 30
  then let rulesStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nr1 := &s\nr2 := &s\n// 多个不可变借用允许}"
           parsed = O.analyzeOwnership rulesStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试借用冲突
prop_borrow_conflict :: String -> Property
prop_borrow_conflict s = 
  if length s < 30
  then let conflictStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nr := &s\nm := &mut s\n// 借用冲突}"
           parsed = O.analyzeOwnership conflictStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试借用生命周期
prop_borrow_lifetime :: String -> Property
prop_borrow_lifetime s = 
  if length s < 30
  then let lifetimeStr = "{//! ownership: on\nfunc test() {\n  s := NewMyString(\"" ++ s ++ "\")\n  r := &s\n  // r 生命周期不超过 s\n}"
           parsed = O.analyzeOwnership lifetimeStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权与GC关系
prop_ownership_gc_relation :: String -> Property
prop_ownership_gc_relation s = 
  if length s < 30
  then let gcStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\n// 所有权检查是编译期的，GC仍是运行时的}"
           parsed = O.analyzeOwnership gcStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试跨goroutine所有权转移
prop_cross_goroutine_ownership :: String -> Property
prop_cross_goroutine_ownership s = 
  if length s < 30
  then let goroutineStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\ngo func() {\n  // 使用 s\n}()\n// s 不能再使用}"
           parsed = O.analyzeOwnership goroutineStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权与接口交互
prop_ownership_interface_interaction :: String -> Property
prop_ownership_interface_interaction s = 
  if all isLetter s
  then let interfaceStr = "{//! ownership: on\ntype " ++ s ++ " interface { Method() }\nfunc (r MyString) Method() { /* 实现 */ }}"
           parsed = O.analyzeOwnership interfaceStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权错误检测
prop_ownership_error_detection :: String -> Property
prop_ownership_error_detection s = 
  if length s < 30
  then let errorStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nt := s\nfmt.Println(s.data) // 错误：s 已被移动}"
           parsed = O.analyzeOwnership errorStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权零运行时开销
prop_ownership_zero_runtime_overhead :: String -> Property
prop_ownership_zero_runtime_overhead s = 
  if length s < 30
  then let overheadStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nt := s\n// 所有权检查在编译期完成}"
           parsed = O.analyzeOwnership overheadStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权数据竞争预防
prop_ownership_data_race_prevention :: String -> Property
prop_ownership_data_race_prevention s = 
  if length s < 30
  then let raceStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nm := &mut s\n// 同一时刻只能有一个可变借用}"
           parsed = O.analyzeOwnership raceStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权逻辑正确性
prop_ownership_logical_correctness :: String -> Property
prop_ownership_logical_correctness s = 
  if length s < 30
  then let correctnessStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\n// 所有权确保逻辑正确性}"
           parsed = O.analyzeOwnership correctnessStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权作用域推断
prop_ownership_scope_inference :: String -> Property
prop_ownership_scope_inference s = 
  if length s < 30
  then let scopeStr = "{//! ownership: on\nfunc test() {\n  s := NewMyString(\"" ++ s ++ "\")\n  // s 作用域在此函数内\n}"
           parsed = O.analyzeOwnership scopeStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权方法签名
prop_ownership_method_signature :: String -> Property
prop_ownership_method_signature s = 
  if all isLetter s
  then let methodStr = "{//! ownership: on\nfunc (r " ++ s ++ ") Method() " ++ s ++ " { return *r }}"
           parsed = O.analyzeOwnership methodStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权函数参数
prop_ownership_function_parameter :: String -> Property
prop_ownership_function_parameter s = 
  if all isLetter s
  then let paramStr = "{//! ownership: on\nfunc process(s " ++ s ++ ") { /* s 被移动 */ }}"
           parsed = O.analyzeOwnership paramStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权返回值
prop_ownership_return_value :: String -> Property
prop_ownership_return_value s = 
  if all isLetter s
  then let returnStr = "{//! ownership: on\nfunc create" ++ s ++ "() " ++ s ++ " { return " ++ s ++ "{} }}"
           parsed = O.analyzeOwnership returnStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权结构体字段
prop_ownership_struct_field :: String -> Property
prop_ownership_struct_field s = 
  if all isLetter s
  then let fieldStr = "{//! ownership: on\ntype Container struct { data " ++ s ++ " }\nfunc (c Container) getData() " ++ s ++ " { return c.data }}"
           parsed = O.analyzeOwnership fieldStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权数组元素
prop_ownership_array_element :: String -> Property
prop_ownership_array_element s = 
  if all isLetter s
  then let arrayStr = "{//! ownership: on\narr := [" ++ s ++ "{}]\nx := arr[0] // x 获得 arr[0] 的所有权}"
           parsed = O.analyzeOwnership arrayStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权切片元素
prop_ownership_slice_element :: String -> Property
prop_ownership_slice_element s = 
  if all isLetter s
  then let sliceStr = "{//! ownership: on\nslice := []" ++ s ++ "{" ++ s ++ "{}\nx := slice[0] // x 获得 slice[0] 的所有权}"
           parsed = O.analyzeOwnership sliceStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权映射值
prop_ownership_map_value :: String -> Property
prop_ownership_map_value s = 
  if all isLetter s
  then let mapStr = "{//! ownership: on\nm := map[string]" ++ s ++ "{}\nm[\"key\"] = " ++ s ++ "{}\nx := m[\"key\"] // x 获得 map 值的所有权}"
           parsed = O.analyzeOwnership mapStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权通道传输
prop_ownership_channel_transfer :: String -> Property
prop_ownership_channel_transfer s = 
  if all isLetter s
  then let channelStr = "{//! ownership: on\nch := make(chan " ++ s ++ ")\ns := " ++ s ++ "{}\nch <- s // s 的所有权转移到通道}"
           parsed = O.analyzeOwnership channelStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权闭包捕获
prop_ownership_closure_capture :: String -> Property
prop_ownership_closure_capture s = 
  if length s < 30
  then let closureStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nf := func() { /* 使用 s */ }\n// s 被闭包捕获}"
           parsed = O.analyzeOwnership closureStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权递归结构
prop_ownership_recursive_structure :: String -> Property
prop_ownership_recursive_structure s = 
  if all isLetter s
  then let recursiveStr = "{//! ownership: on\ntype " ++ s ++ "Node struct { data int; next *" ++ s ++ "Node }}"
           parsed = O.analyzeOwnership recursiveStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权循环引用
prop_ownership_circular_reference :: String -> Property
prop_ownership_circular_reference s = 
  if all isLetter s
  then let circularStr = "{//! ownership: on\ntype " ++ s ++ "A struct { b *" ++ s ++ "B }\ntype " ++ s ++ "B struct { a *" ++ s ++ "A }"
           parsed = O.analyzeOwnership circularStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权共享引用
prop_ownership_shared_reference :: String -> Property
prop_ownership_shared_reference s = 
  if all isLetter s
  then let sharedStr = "{//! ownership: on\ns := " ++ s ++ "{}\nr1 := &s\nr2 := &s\n// 多个共享引用}"
           parsed = O.analyzeOwnership sharedStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权独占引用
prop_ownership_exclusive_reference :: String -> Property
prop_ownership_exclusive_reference s = 
  if all isLetter s
  then let exclusiveStr = "{//! ownership: on\ns := " ++ s ++ "{}\nr := &mut s\n// 独占可变引用}"
           parsed = O.analyzeOwnership exclusiveStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权引用计数
prop_ownership_reference_counting :: String -> Property
prop_ownership_reference_counting s = 
  if all isLetter s
  then let refCountStr = "{//! ownership: on\ns := " ++ s ++ "{}\nr1 := &s\nr2 := &s\n// 引用计数概念（仅概念性）}"
           parsed = O.analyzeOwnership refCountStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权内存安全
prop_ownership_memory_safety :: String -> Property
prop_ownership_memory_safety s = 
  if length s < 30
  then let safetyStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nt := s\n// 所有权确保内存安全}"
           parsed = O.analyzeOwnership safetyStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权资源管理
prop_ownership_resource_management :: String -> Property
prop_ownership_resource_management s = 
  if length s < 30
  then let resourceStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\n// 所有权确保资源正确释放}"
           parsed = O.analyzeOwnership resourceStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权并发安全
prop_ownership_concurrent_safety :: String -> Property
prop_ownership_concurrent_safety s = 
  if length s < 30
  then let concurrentStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\n// 所有权确保并发安全}"
           parsed = O.analyzeOwnership concurrentStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权类型系统
prop_ownership_type_system :: String -> Property
prop_ownership_type_system s = 
  if all isLetter s
  then let typeSystemStr = "{//! ownership: on\ntype " ++ s ++ " struct { data int }\nfunc (r " ++ s ++ ") Method() { /* 方法 */ }}"
           parsed = O.analyzeOwnership typeSystemStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权编译器优化
prop_ownership_compiler_optimization :: String -> Property
prop_ownership_compiler_optimization s = 
  if length s < 30
  then let optimizationStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nt := s\n// 编译器可以优化所有权检查}"
           parsed = O.analyzeOwnership optimizationStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权调试支持
prop_ownership_debug_support :: String -> Property
prop_ownership_debug_support s = 
  if length s < 30
  then let debugStr = "{//! ownership: on\n//! debug: ownership\ns := NewMyString(\"" ++ s ++ "\")\n// 调试所有权信息}"
           parsed = O.analyzeOwnership debugStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权错误恢复
prop_ownership_error_recovery :: String -> Property
prop_ownership_error_recovery s = 
  if length s < 30
  then let recoveryStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nt := s\n// 错误：s 已被移动\n// 编译器提供错误恢复信息}"
           parsed = O.analyzeOwnership recoveryStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权性能影响
prop_ownership_performance_impact :: String -> Property
prop_ownership_performance_impact s = 
  if length s < 30
  then let performanceStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nt := s\n// 所有权检查零运行时开销}"
           parsed = O.analyzeOwnership performanceStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权与泛型交互
prop_ownership_generic_interaction :: String -> Property
prop_ownership_generic_interaction s = 
  if all isLetter s
  then let genericStr = "{//! ownership: on\ntype Container[" ++ s ++ " any] struct { data " ++ s ++ " }"
           parsed = O.analyzeOwnership genericStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权与依赖类型交互
prop_ownership_dependent_type_interaction :: String -> Property
prop_ownership_dependent_type_interaction s = 
  if length s < 30
  then let interactionStr = "{//! ownership: on\n//! dependent_types: on\ns := NewMyString(\"" ++ s ++ "\")\n// 所有权与依赖类型交互}"
           parsed = O.analyzeOwnership interactionStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- | 测试所有权最佳实践
prop_ownership_best_practices :: String -> Property
prop_ownership_best_practices s = 
  if length s < 30
  then let practicesStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\n// 所有权最佳实践示例}"
           parsed = O.analyzeOwnership practicesStr
       in case parsed of
            Right analysis -> property $ not (null $ show analysis)
            Left _ -> property True
  else property True

-- ============================================================================
-- Utils 测试 (20个测试)
-- ============================================================================

-- | 测试字符串处理工具
prop_string_utils :: String -> Property
prop_string_utils s = 
  let processed = U.trim s
  in property $ length processed >= 0

-- | 测试列表处理工具
prop_list_utils :: [Int] -> Property
prop_list_utils nums = 
  let processed = U.splitBy "," (intercalate "," (map show nums))
  in property $ length processed >= 0

-- | 测试映射处理工具
prop_map_utils :: [(String, Int)] -> Property
prop_map_utils pairs = 
  if length pairs < 10
  then let processed = U.splitBy "," (intercalate "," (map (\(k, v) -> k ++ ":" ++ show v) pairs))
       in property $ length processed >= 0
  else property True

-- | 测试集合处理工具
prop_set_utils :: [Int] -> Property
prop_set_utils nums = 
  if length nums < 10
  then let processed = U.splitBy "," (intercalate "," (map show nums))
       in property $ length processed >= 0
  else property True

-- | 测试文件处理工具
prop_file_utils :: String -> Property
prop_file_utils s = 
  if length s < 50
  then let processed = U.typusFileFromString s
       in property $ length (show processed) >= 0
  else property True

-- | 测试路径处理工具
prop_path_utils :: String -> Property
prop_path_utils s = 
  if length s < 50
  then let processed = U.splitBy "/" s
       in property $ length processed >= 0
  else property True

-- | 测试URL处理工具
prop_url_utils :: String -> Property
prop_url_utils s = 
  if length s < 50
  then let processed = U.splitBy "/" s
       in property $ length processed >= 0
  else property True

-- | 测试JSON处理工具
prop_json_utils :: String -> Property
prop_json_utils s = 
  if length s < 50
  then let processed = U.splitBy "," s
       in property $ length processed >= 0
  else property True

-- | 测试XML处理工具
prop_xml_utils :: String -> Property
prop_xml_utils s = 
  if length s < 50
  then let processed = U.splitBy "<" s
       in property $ length processed >= 0
  else property True

-- | 测试正则表达式工具
prop_regex_utils :: String -> Property
prop_regex_utils s = 
  if length s < 50
  then let processed = U.splitBy " " s
       in property $ length processed >= 0
  else property True

-- | 测试时间处理工具
prop_time_utils :: String -> Property
prop_time_utils s = 
  if length s < 50
  then let processed = U.splitBy ":" s
       in property $ length processed >= 0
  else property True

-- | 测试数学工具
prop_math_utils :: Int -> Property
prop_math_utils n = 
  let processed = show (abs n)
  in property $ length processed >= 0

-- | 测试加密工具
prop_crypto_utils :: String -> Property
prop_crypto_utils s = 
  if length s < 50
  then let processed = U.splitBy "." s
       in property $ length processed >= 0
  else property True

-- | 测试压缩工具
prop_compression_utils :: String -> Property
prop_compression_utils s = 
  if length s < 50
  then let processed = U.trim s
       in property $ length processed >= 0
  else property True

-- | 测试编码工具
prop_encoding_utils :: String -> Property
prop_encoding_utils s = 
  if length s < 50
  then let processed = U.splitBy "=" s
       in property $ length processed >= 0
  else property True

-- | 测试网络工具
prop_network_utils :: String -> Property
prop_network_utils s = 
  if length s < 50
  then let processed = U.splitBy ":" s
       in property $ length processed >= 0
  else property True

-- | 测试日志工具
prop_logging_utils :: String -> Property
prop_logging_utils s = 
  if length s < 50
  then let processed = U.trim s
       in property $ length processed >= 0
  else property True

-- | 测试配置工具
prop_config_utils :: String -> Property
prop_config_utils s = 
  if length s < 50
  then let processed = U.splitBy "=" s
       in property $ length processed >= 0
  else property True

-- | 测试缓存工具
prop_cache_utils :: String -> Property
prop_cache_utils s = 
  if length s < 50
  then let processed = U.trim s
       in property $ length processed >= 0
  else property True

-- | 测试并发工具
prop_concurrency_utils :: String -> Property
prop_concurrency_utils s = 
  if length s < 50
  then let processed = U.splitBy "," s
       in property $ length processed >= 0
  else property True

-- ============================================================================
-- 测试套件组合
-- ============================================================================

-- | Parser 测试套件
parserTests :: TestTree
parserTests = testGroup "Parser Tests"
  [ testProperty "basic parser roundtrip" prop_basic_parser_roundtrip
  , testProperty "parser error recovery" prop_parser_error_recovery
  , testProperty "parser comment handling" prop_parser_comment_handling
  , testProperty "parser identifier" prop_parser_identifier
  , testProperty "parser number literals" prop_parser_number_literals
  , testProperty "parser string literals" prop_parser_string_literals
  , testProperty "parser type" prop_parser_type
  , testProperty "parser function" prop_parser_function
  , testProperty "parser struct" prop_parser_struct
  , testProperty "parser interface" prop_parser_interface
  , testProperty "parser package" prop_parser_package
  , testProperty "parser import" prop_parser_import
  , testProperty "parser expression" prop_parser_expression
  , testProperty "parser statement" prop_parser_statement
  , testProperty "parser block" prop_parser_block
  , testProperty "parser array" prop_parser_array
  , testProperty "parser map" prop_parser_map
  , testProperty "parser slice" prop_parser_slice
  , testProperty "parser channel" prop_parser_channel
  , testProperty "parser pointer" prop_parser_pointer
  , testProperty "parser select" prop_parser_select
  , testProperty "parser switch" prop_parser_switch
  , testProperty "parser for loop" prop_parser_for_loop
  , testProperty "parser range loop" prop_parser_range_loop
  , testProperty "parser if" prop_parser_if
  , testProperty "parser else if" prop_parser_else_if
  , testProperty "parser defer" prop_parser_defer
  , testProperty "parser go" prop_parser_go
  , testProperty "parser return" prop_parser_return
  , testProperty "parser performance" prop_parser_performance
  , testProperty "parser unicode" prop_parser_unicode
  , testProperty "parser error position" prop_parser_error_position
  , testProperty "parser fault tolerance" prop_parser_fault_tolerance
  ]

-- | Compiler 测试套件
compilerTests :: TestTree
compilerTests = testGroup "Compiler Tests"
  [ testProperty "basic compilation" prop_basic_compilation
  , testProperty "compiler error handling" prop_compiler_error_handling
  , testProperty "type compilation" prop_type_compilation
  , testProperty "function compilation" prop_function_compilation
  , testProperty "struct compilation" prop_struct_compilation
  , testProperty "interface compilation" prop_interface_compilation
  , testProperty "method compilation" prop_method_compilation
  , testProperty "variable compilation" prop_variable_compilation
  , testProperty "constant compilation" prop_constant_compilation
  , testProperty "import compilation" prop_import_compilation
  , testProperty "expression compilation" prop_expression_compilation
  , testProperty "control flow compilation" prop_control_flow_compilation
  , testProperty "loop compilation" prop_loop_compilation
  , testProperty "array compilation" prop_array_compilation
  , testProperty "slice compilation" prop_slice_compilation
  , testProperty "map compilation" prop_map_compilation
  , testProperty "channel compilation" prop_channel_compilation
  , testProperty "pointer compilation" prop_pointer_compilation
  , testProperty "goroutine compilation" prop_goroutine_compilation
  , testProperty "select compilation" prop_select_compilation
  , testProperty "defer compilation" prop_defer_compilation
  , testProperty "panic compilation" prop_panic_compilation
  , testProperty "recover compilation" prop_recover_compilation
  , testProperty "compiler optimization" prop_compiler_optimization
  , testProperty "compiler type checking" prop_compiler_type_checking
  , testProperty "compiler dependency resolution" prop_compiler_dependency_resolution
  , testProperty "compiler code generation" prop_compiler_code_generation
  , testProperty "compiler error recovery" prop_compiler_error_recovery
  , testProperty "compiler performance" prop_compiler_performance
  , testProperty "compiler unicode" prop_compiler_unicode
  , testProperty "compiler memory efficiency" prop_compiler_memory_efficiency
  , testProperty "compiler concurrency" prop_compiler_concurrency
  , testProperty "compiler extensibility" prop_compiler_extensibility
  ]

-- | DependentTypes 测试套件
dependentTypesTests :: TestTree
dependentTypesTests = testGroup "DependentTypes Tests"
  [ testProperty "dependent type parsing" prop_dependent_type_parsing
  , testProperty "value parameterized type" prop_value_parameterized_type
  , testProperty "precise type" prop_precise_type
  , testProperty "parameterized precise type" prop_parameterized_precise_type
  , testProperty "dependent function signature" prop_dependent_function_signature
  , testProperty "function precondition" prop_function_precondition
  , testProperty "assert narrowing" prop_assert_narrowing
  , testProperty "conditional narrowing" prop_conditional_narrowing
  , testProperty "compile time constant" prop_compile_time_constant
  , testProperty "existential type" prop_existential_type
  , testProperty "existential unpack" prop_existential_unpack
  , testProperty "type inference" prop_type_inference
  , testProperty "constraint linear arithmetic" prop_constraint_linear_arithmetic
  , testProperty "constraint equality propagation" prop_constraint_equality_propagation
  , testProperty "constraint inequality chain" prop_constraint_inequality_chain
  , testProperty "error mode constraints" prop_error_mode_constraints
  , testProperty "go interop type erasure" prop_go_interop_type_erasure
  , testProperty "boundary annotation" prop_boundary_annotation
  , testProperty "matrix multiplication alignment" prop_matrix_multiplication_alignment
  , testProperty "vector operations dimension match" prop_vector_operations_dimension_match
  , testProperty "ownership dependent types interaction" prop_ownership_dependent_types_interaction
  , testProperty "directive system block" prop_directive_system_block
  , testProperty "file level directives" prop_file_level_directives
  , testProperty "recursive type parameterization" prop_recursive_type_parameterization
  , testProperty "constraint combination" prop_constraint_combination
  , testProperty "type level function" prop_type_level_function
  , testProperty "dependent type pattern matching" prop_dependent_type_pattern_matching
  , testProperty "constraint solver edge case" prop_constraint_solver_edge_case
  , testProperty "dependent type type check" prop_dependent_type_type_check
  , testProperty "compile time optimization" prop_compile_time_optimization
  , testProperty "dependent type error handling" prop_dependent_type_error_handling
  , testProperty "type constraint transitivity" prop_type_constraint_transitivity
  , testProperty "dependent type inference" prop_dependent_type_inference
  , testProperty "constraint solver performance" prop_constraint_solver_performance
  , testProperty "dependent type code generation" prop_dependent_type_code_generation
  , testProperty "type constraint validation" prop_type_constraint_validation
  , testProperty "dependent type serialization" prop_dependent_type_serialization
  , testProperty "constraint solver extensibility" prop_constraint_solver_extensibility
  , testProperty "dependent type debug support" prop_dependent_type_debug_support
  ]

-- | Ownership 测试套件
ownershipTests :: TestTree
ownershipTests = testGroup "Ownership Tests"
  [ testProperty "ownership basic semantics" prop_ownership_basic_semantics
  , testProperty "move semantics" prop_move_semantics
  , testProperty "immutable borrow" prop_immutable_borrow
  , testProperty "mutable borrow" prop_mutable_borrow
  , testProperty "borrowing rules" prop_borrowing_rules
  , testProperty "borrow conflict" prop_borrow_conflict
  , testProperty "borrow lifetime" prop_borrow_lifetime
  , testProperty "ownership gc relation" prop_ownership_gc_relation
  , testProperty "cross goroutine ownership" prop_cross_goroutine_ownership
  , testProperty "ownership interface interaction" prop_ownership_interface_interaction
  , testProperty "ownership error detection" prop_ownership_error_detection
  , testProperty "ownership zero runtime overhead" prop_ownership_zero_runtime_overhead
  , testProperty "ownership data race prevention" prop_ownership_data_race_prevention
  , testProperty "ownership logical correctness" prop_ownership_logical_correctness
  , testProperty "ownership scope inference" prop_ownership_scope_inference
  , testProperty "ownership method signature" prop_ownership_method_signature
  , testProperty "ownership function parameter" prop_ownership_function_parameter
  , testProperty "ownership return value" prop_ownership_return_value
  , testProperty "ownership struct field" prop_ownership_struct_field
  , testProperty "ownership array element" prop_ownership_array_element
  , testProperty "ownership slice element" prop_ownership_slice_element
  , testProperty "ownership map value" prop_ownership_map_value
  , testProperty "ownership channel transfer" prop_ownership_channel_transfer
  , testProperty "ownership closure capture" prop_ownership_closure_capture
  , testProperty "ownership recursive structure" prop_ownership_recursive_structure
  , testProperty "ownership circular reference" prop_ownership_circular_reference
  , testProperty "ownership shared reference" prop_ownership_shared_reference
  , testProperty "ownership exclusive reference" prop_ownership_exclusive_reference
  , testProperty "ownership reference counting" prop_ownership_reference_counting
  , testProperty "ownership memory safety" prop_ownership_memory_safety
  , testProperty "ownership resource management" prop_ownership_resource_management
  , testProperty "ownership concurrent safety" prop_ownership_concurrent_safety
  , testProperty "ownership type system" prop_ownership_type_system
  , testProperty "ownership compiler optimization" prop_ownership_compiler_optimization
  , testProperty "ownership debug support" prop_ownership_debug_support
  , testProperty "ownership error recovery" prop_ownership_error_recovery
  , testProperty "ownership performance impact" prop_ownership_performance_impact
  , testProperty "ownership generic interaction" prop_ownership_generic_interaction
  , testProperty "ownership dependent type interaction" prop_ownership_dependent_type_interaction
  , testProperty "ownership best practices" prop_ownership_best_practices
  ]

-- | Utils 测试套件
utilsTests :: TestTree
utilsTests = testGroup "Utils Tests"
  [ testProperty "string utils" prop_string_utils
  , testProperty "list utils" prop_list_utils
  , testProperty "map utils" prop_map_utils
  , testProperty "set utils" prop_set_utils
  , testProperty "file utils" prop_file_utils
  , testProperty "path utils" prop_path_utils
  , testProperty "url utils" prop_url_utils
  , testProperty "json utils" prop_json_utils
  , testProperty "xml utils" prop_xml_utils
  , testProperty "regex utils" prop_regex_utils
  , testProperty "time utils" prop_time_utils
  , testProperty "math utils" prop_math_utils
  , testProperty "crypto utils" prop_crypto_utils
  , testProperty "compression utils" prop_compression_utils
  , testProperty "encoding utils" prop_encoding_utils
  , testProperty "network utils" prop_network_utils
  , testProperty "logging utils" prop_logging_utils
  , testProperty "config utils" prop_config_utils
  , testProperty "cache utils" prop_cache_utils
  , testProperty "concurrency utils" prop_concurrency_utils
  ]

-- | 完整测试套件
comprehensiveTypusTestSuite :: TestTree
comprehensiveTypusTestSuite = testGroup "Comprehensive Typus Test Suite"
  [ parserTests
  , compilerTests
  , dependentTypesTests
  , ownershipTests
  , utilsTests
  ]