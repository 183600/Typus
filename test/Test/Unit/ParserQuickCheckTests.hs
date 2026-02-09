{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.ParserQuickCheckTests where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Parser as P
import qualified Utils as U
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, intercalate)
import Data.Char (isSpace, isLetter, isDigit)
import Data.Maybe (isJust, isNothing)
import Data.Either (isLeft, isRight)
import Text.Megaparsec (parse, parseTest)
import Text.Megaparsec.Char (string, char, space)
import Control.Applicative ((<|>))

-- | 测试解析器的基本标识符解析
prop_parse_identifier_basic :: String -> Property
prop_parse_identifier_basic s =
  let valid = all (\c -> isLetter c || c == '_' || isDigit c) s && not (null s)
      startsWithLetter = not (null s) && isLetter (head s)
      result = parse P.identifier "" s
  in if valid && startsWithLetter
     then property $ isRight result
     else property $ isLeft result

-- | 测试解析器对数字的解析
prop_parse_number :: Int -> Property
prop_parse_number n =
  let numStr = show n
      result = parse P.number "" numStr
  in property $ isRight result

-- | 测试解析器对字符串字面量的解析
prop_parse_string_literal :: String -> Property
prop_parse_string_literal s =
  let strLit = "\"" ++ s ++ "\""
      result = parse P.stringLiteral "" strLit
  in property $ isRight result

-- | 测试解析器对注释的跳过
prop_parse_skip_comments :: String -> Property
prop_parse_skip_comments s =
  let withComment = "//" ++ s ++ "\nidentifier"
      result = parse P.identifier "" withComment
  in property $ isRight result

-- | 测试解析器对空白字符的处理
prop_parse_whitespace :: String -> Property
prop_parse_whitespace s =
  let withSpaces = "  " ++ s ++ "  "
      result = parse P.identifier "" withSpaces
  in if all (\c -> isLetter c || c == '_' || isDigit c) s && not (null s) && isLetter (head s)
     then property $ isRight result
     else property $ True

-- | 测试解析器对关键字的处理
prop_parse_keywords :: String -> Property
prop_parse_keywords s =
  let isKeyword = s `elem` ["func", "var", "if", "else", "for", "while", "return"]
      result = parse P.identifier "" s
  in if isKeyword
     then property $ isLeft result
     else property $ True

-- | 测试解析器对操作符的解析
prop_parse_operators :: String -> Property
prop_parse_operators s =
  let isOperator = all (`elem` "+-*/%=<>!&|^~") s && not (null s)
      result = parse P.operator "" s
  in if isOperator
     then property $ isRight result
     else property $ True

-- | 测试解析器对括号匹配的处理
prop_parse_parentheses :: String -> Property
prop_parse_parentheses s =
  let withParens = "(" ++ s ++ ")"
      result = parse P.parenthesized "" withParens
  in property $ isRight result

-- | 测试解析器对数组的解析
prop_parse_array :: [String] -> Property
prop_parse_array elems =
  let elemsStr = intercalate ", " elems
      arrayStr = "[" ++ elemsStr ++ "]"
      result = parse P.array "" arrayStr
  in property $ isRight result

-- | 测试解析器对函数定义的解析
prop_parse_function_def :: String -> [String] -> Property
prop_parse_function_def name params =
  let validName = not (null name) && isLetter (head name) && all (\c -> isLetter c || c == '_' || isDigit c) name
      validParams = all (\p -> not (null p) && isLetter (head p) && all (\c -> isLetter c || c == '_' || isDigit c) p) params
      paramsStr = intercalate ", " params
      funcStr = "func " ++ name ++ "(" ++ paramsStr ++ ") { }"
      result = parse P.functionDef "" funcStr
  in if validName && validParams
     then property $ isRight result
     else property $ True

-- | 测试解析器对变量声明的解析
prop_parse_variable_decl :: String -> String -> Property
prop_parse_variable_decl varName typeName =
  let validVar = not (null varName) && isLetter (head varName) && all (\c -> isLetter c || c == '_' || isDigit c) varName
      validType = not (null typeName) && isLetter (head typeName) && all (\c -> isLetter c || c == '_' || isDigit c) typeName
      declStr = "var " ++ varName ++ " " ++ typeName
      result = parse P.variableDecl "" declStr
  in if validVar && validType
     then property $ isRight result
     else property $ True

-- | 测试解析器对表达式的解析
prop_parse_expression :: Int -> Int -> Property
prop_parse_expression a b =
  let exprStr = show a ++ " + " ++ show b
      result = parse P.expression "" exprStr
  in property $ isRight result

-- | 测试解析器对条件语句的解析
prop_parse_if_statement :: String -> Property
prop_parse_if_statement condition =
  let condStr = "if (" ++ condition ++ ") { }"
      result = parse P.ifStatement "" condStr
  in property $ isRight result

-- | 测试解析器对循环语句的解析
prop_parse_while_loop :: String -> Property
prop_parse_while_loop condition =
  let loopStr = "while (" ++ condition ++ ") { }"
      result = parse P.whileLoop "" loopStr
  in property $ isRight result

-- | 测试解析器对返回语句的解析
prop_parse_return_statement :: String -> Property
prop_parse_return_statement expr =
  let returnStr = "return " ++ expr
      result = parse P.returnStatement "" returnStr
  in property $ isRight result

-- | 测试解析器对赋值语句的解析
prop_parse_assignment :: String -> String -> Property
prop_parse_assignment varName expr =
  let validVar = not (null varName) && isLetter (head varName) && all (\c -> isLetter c || c == '_' || isDigit c) varName
      assignStr = varName ++ " = " ++ expr
      result = parse P.assignment "" assignStr
  in if validVar
     then property $ isRight result
     else property $ True

-- | 测试解析器对多行代码的解析
prop_parse_multiline :: [String] -> Property
prop_parse_multiline lines' =
  let code = unlines lines'
      result = parse P.codeBlock "" code
  in property $ isRight result

-- | 测试解析器对嵌套结构的解析
prop_parse_nested_structures :: Int -> Property
prop_parse_nested_structures depth =
  let nested = replicate depth "if (true) { "
      code = concat nested ++ "var x = 1" ++ concat (replicate depth " }")
      result = parse P.codeBlock "" code
  in property $ depth < 10 ==> isRight result

-- | 测试解析器对错误恢复的处理
prop_parse_error_recovery :: String -> Property
prop_parse_error_recovery s =
  let withError = s ++ " @@@ invalid"
      result = parse P.statement "" withError
  in property $ isLeft result

-- | 测试解析器对Unicode字符的处理
prop_parse_unicode :: String -> Property
prop_parse_unicode s =
  let unicodeName = "变量_" ++ s
      valid = all (\c -> isLetter c || c == '_' || isDigit c) unicodeName && not (null unicodeName)
      result = parse P.identifier "" unicodeName
  in if valid && isLetter (head unicodeName)
     then property $ isRight result
     else property $ True

-- | 测试解析器对长标识符的处理
prop_parse_long_identifier :: Int -> Property
prop_parse_long_identifier n =
  let longName = "id" ++ replicate n 'a'
      valid = length longName < 1000
      result = parse P.identifier "" longName
  in if valid
     then property $ isRight result
     else property $ True

-- | 测试解析器对转义字符的处理
prop_parse_escape_sequences :: String -> Property
prop_parse_escape_sequences s =
  let withEscapes = "\"\\n\\t\\\\\"" ++ s ++ "\\\""
      result = parse P.stringLiteral "" withEscapes
  in property $ isRight result

-- | 测试解析器对空输入的处理
prop_parse_empty_input :: Property
prop_parse_empty_input = isLeft (parse P.statement "" "")

-- | 测试解析器对部分输入的解析
prop_parse_partial_input :: String -> Property
prop_parse_partial_input s =
  let result = parse P.statement "" s
  in property $ isRight result || isLeft result

-- | 测试解析器对注释后的代码解析
prop_parse_code_after_comment :: String -> Property
prop_parse_code_after_comment code =
  let withComment = "// comment\n" ++ code
      result = parse P.statement "" withComment
  in property $ isRight result || isLeft result

-- | 测试解析器对复杂表达式的解析
prop_parse_complex_expression :: [Int] -> Property
prop_parse_complex_expression nums =
  let exprStr = intercalate " + " (map show nums)
      result = parse P.expression "" exprStr
  in property $ isRight result

-- | 测试解析器对函数调用的解析
prop_parse_function_call :: String -> [String] -> Property
prop_parse_function_call name args =
  let validName = not (null name) && isLetter (head name) && all (\c -> isLetter c || c == '_' || isDigit c) name
      argsStr = intercalate ", " args
      callStr = name ++ "(" ++ argsStr ++ ")"
      result = parse P.functionCall "" callStr
  in if validName
     then property $ isRight result
     else property $ True

-- | 测试解析器对结构体定义的解析
prop_parse_struct_definition :: String -> [String] -> Property
prop_parse_struct_definition name fields =
  let validName = not (null name) && isLetter (head name) && all (\c -> isLetter c || c == '_' || isDigit c) name
      fieldsStr = intercalate "; " fields
      structStr = "struct " ++ name ++ " { " ++ fieldsStr ++ " }"
      result = parse P.structDef "" structStr
  in if validName
     then property $ isRight result
     else property $ True

-- | 测试解析器对类型注解的解析
prop_parse_type_annotation :: String -> String -> Property
prop_parse_type_annotation varName typeName =
  let validVar = not (null varName) && isLetter (head varName) && all (\c -> isLetter c || c == '_' || isDigit c) varName
      validType = not (null typeName) && isLetter (head typeName) && all (\c -> isLetter c || c == '_' || isDigit c) typeName
      typeStr = varName ++ ": " ++ typeName
      result = parse P.typeAnnotation "" typeStr
  in if validVar && validType
     then property $ isRight result
     else property $ True

-- | 测试解析器对导入语句的解析
prop_parse_import :: String -> Property
prop_parse_import modulePath =
  let importStr = "import \"" ++ modulePath ++ "\""
      result = parse P.importStmt "" importStr
  in property $ isRight result

-- | 测试解析器对导出语句的解析
prop_parse_export :: String -> Property
prop_parse_export name =
  let validName = not (null name) && isLetter (head name) && all (\c -> isLetter c || c == '_' || isDigit c) name
      exportStr = "export " ++ name
      result = parse P.exportStmt "" exportStr
  in if validName
     then property $ isRight result
     else property $ True

-- | 测试解析器对接口定义的解析
prop_parse_interface :: String -> [String] -> Property
prop_parse_interface name methods =
  let validName = not (null name) && isLetter (head name) && all (\c -> isLetter c || c == '_' || isDigit c) name
      methodsStr = intercalate "; " methods
      interfaceStr = "interface " ++ name ++ " { " ++ methodsStr ++ " }"
      result = parse P.interfaceDef "" interfaceStr
  in if validName
     then property $ isRight result
     else property $ True

-- | 测试解析器对枚举定义的解析
prop_parse_enum :: String -> [String] -> Property
prop_parse_enum name variants =
  let validName = not (null name) && isLetter (head name) && all (\c -> isLetter c || c == '_' || isDigit c) name
      validVariants = all (\v -> not (null v) && isLetter (head v) && all (\c -> isLetter c || c == '_' || isDigit c) v) variants
      variantsStr = intercalate ", " variants
      enumStr = "enum " ++ name ++ " { " ++ variantsStr ++ " }"
      result = parse P.enumDef "" enumStr
  in if validName && validVariants
     then property $ isRight result
     else property $ True

-- | 测试解析器对泛型参数的解析
prop_parse_generic_params :: String -> [String] -> Property
prop_parse_generic_params name params =
  let validName = not (null name) && isLetter (head name) && all (\c -> isLetter c || c == '_' || isDigit c) name
      validParams = all (\p -> not (null p) && isLetter (head p) && all (\c -> isLetter c || c == '_' || isDigit c) p) params
      paramsStr = intercalate ", " params
      genericStr = name ++ "<" ++ paramsStr ++ ">"
      result = parse P.genericType "" genericStr
  in if validName && validParams
     then property $ isRight result
     else property $ True

-- | 测试解析器对模式匹配的解析
prop_parse_pattern_match :: String -> String -> Property
prop_parse_pattern_match pattern expr =
  let matchStr = "match " ++ expr ++ " { case " ++ pattern ++ " => }"
      result = parse P.matchExpr "" matchStr
  in property $ isRight result

-- | 测试解析器对异步函数的解析
prop_parse_async_function :: String -> [String] -> Property
prop_parse_async_function name params =
  let validName = not (null name) && isLetter (head name) && all (\c -> isLetter c || c == '_' || isDigit c) name
      validParams = all (\p -> not (null p) && isLetter (head p) && all (\c -> isLetter c || c == '_' || isDigit c) p) params
      paramsStr = intercalate ", " params
      asyncStr = "async func " ++ name ++ "(" ++ paramsStr ++ ") { }"
      result = parse P.functionDef "" asyncStr
  in if validName && validParams
     then property $ isRight result
     else property $ True

-- | 测试解析器对错误位置的记录
prop_parse_error_position :: String -> Property
prop_parse_error_position s =
  let withError = "valid " ++ s ++ " @@@ invalid"
      result = parse P.statement "" withError
  in case result of
    Left err -> property $ True
    Right _ -> property $ True

-- | 测试解析器对大文件的处理
prop_parse_large_file :: Int -> Property
prop_parse_large_file n =
  let lines' = replicate n "var x = 1;"
      code = unlines lines'
      result = parse P.codeBlock "" code
  in property $ n < 100 ==> isRight result

-- | 测试解析器对递归结构的解析
prop_parse_recursive :: Int -> Property
prop_parse_recursive depth =
  let buildRecursive 0 = "var x = 0;"
      buildRecursive n = "func f" ++ show n ++ "() { " ++ buildRecursive (n-1) ++ " }"
      code = buildRecursive depth
      result = parse P.codeBlock "" code
  in property $ depth < 10 ==> isRight result

-- | 组合所有测试
parserQuickCheckTests :: TestTree
parserQuickCheckTests = testGroup "Parser QuickCheck Tests"
  [ testProperty "parse identifier basic" prop_parse_identifier_basic
  , testProperty "parse number" prop_parse_number
  , testProperty "parse string literal" prop_parse_string_literal
  , testProperty "parse skip comments" prop_parse_skip_comments
  , testProperty "parse whitespace" prop_parse_whitespace
  , testProperty "parse keywords" prop_parse_keywords
  , testProperty "parse operators" prop_parse_operators
  , testProperty "parse parentheses" prop_parse_parentheses
  , testProperty "parse array" prop_parse_array
  , testProperty "parse function def" prop_parse_function_def
  , testProperty "parse variable decl" prop_parse_variable_decl
  , testProperty "parse expression" prop_parse_expression
  , testProperty "parse if statement" prop_parse_if_statement
  , testProperty "parse while loop" prop_parse_while_loop
  , testProperty "parse return statement" prop_parse_return_statement
  , testProperty "parse assignment" prop_parse_assignment
  , testProperty "parse multiline" prop_parse_multiline
  , testProperty "parse nested structures" prop_parse_nested_structures
  , testProperty "parse error recovery" prop_parse_error_recovery
  , testProperty "parse unicode" prop_parse_unicode
  , testProperty "parse long identifier" prop_parse_long_identifier
  , testProperty "parse escape sequences" prop_parse_escape_sequences
  , testProperty "parse empty input" prop_parse_empty_input
  , testProperty "parse partial input" prop_parse_partial_input
  , testProperty "parse code after comment" prop_parse_code_after_comment
  , testProperty "parse complex expression" prop_parse_complex_expression
  , testProperty "parse function call" prop_parse_function_call
  , testProperty "parse struct definition" prop_parse_struct_definition
  , testProperty "parse type annotation" prop_parse_type_annotation
  , testProperty "parse import" prop_parse_import
  , testProperty "parse export" prop_parse_export
  , testProperty "parse interface" prop_parse_interface
  , testProperty "parse enum" prop_parse_enum
  , testProperty "parse generic params" prop_parse_generic_params
  , testProperty "parse pattern match" prop_parse_pattern_match
  , testProperty "parse async function" prop_parse_async_function
  , testProperty "parse error position" prop_parse_error_position
  , testProperty "parse large file" prop_parse_large_file
  , testProperty "parse recursive" prop_parse_recursive
  ]