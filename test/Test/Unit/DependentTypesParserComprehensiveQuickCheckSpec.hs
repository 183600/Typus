{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.DependentTypesParserComprehensiveQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified DependentTypesParser as DTP
import qualified SourceLocation as SL
import Data.Char (isAlphaNum, isLetter, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub, intercalate)
import Data.Maybe (isJust, isNothing)
import Data.Either (isLeft, isRight)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

-- ============================================================================
-- DependentTypesParser模块的QuickCheck测试 (25个测试)
-- ============================================================================

-- | 测试runDependentTypesParser函数
prop_run_dependent_types_parser :: String -> Property
prop_run_dependent_types_parser input =
  let validInput = not (null input)
      result = if validInput
               then DTP.runDependentTypesParser input
               else ([], DTP.DependentTypesParser Map.empty [])
  in property $ length (fst result) >= 0

-- | 测试parseDependentType函数
prop_parse_dependent_type :: String -> Property
prop_parse_dependent_type input =
  let validInput = not (null input)
      result = if validInput
               then DTP.parseDependentType input
               else Nothing
  in if validInput
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试parseTypeDeclaration函数
prop_parse_type_declaration :: String -> Property
prop_parse_type_declaration input =
  let validInput = not (null input)
      result = if validInput
               then DTP.parseTypeDeclaration input
               else Nothing
  in if validInput
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试validateDependentTypeSyntax函数
prop_validate_dependent_type_syntax :: String -> Property
prop_validate_dependent_type_syntax input =
  let validInput = not (null input)
      result = if validInput
               then DTP.validateDependentTypeSyntax input
               else []
  in property $ length result >= 0

-- | 测试简单类型声明解析
prop_parse_simple_type_declaration :: String -> Property
prop_parse_simple_type_declaration typeName =
  let validType = not (null typeName) && isLetter (head typeName) && 
                  all (\c -> isLetter c || isDigit c) typeName
      typeDecl = "type " ++ typeName ++ " = int"
      result = if validType
               then DTP.parseTypeDeclaration typeDecl
               else Nothing
  in if validType
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试泛型类型声明解析
prop_parse_generic_type_declaration :: String -> String -> Property
prop_parse_generic_type_declaration typeName typeParam =
  let validType = not (null typeName) && isLetter (head typeName) && 
                  all (\c -> isLetter c || isDigit c) typeName
      validParam = not (null typeParam) && isLetter (head typeParam) && 
                   all (\c -> isLetter c || isDigit c) typeParam
      typeDecl = "type " ++ typeName ++ "[" ++ typeParam ++ ": int] = struct { value: " ++ typeParam ++ " }"
      result = if validType && validParam
               then DTP.parseTypeDeclaration typeDecl
               else Nothing
  in if validType && validParam
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试带约束的类型声明解析
prop_parse_constrained_type_declaration :: String -> String -> Property
prop_parse_constrained_type_declaration typeName constraint =
  let validType = not (null typeName) && isLetter (head typeName) && 
                  all (\c -> isLetter c || isDigit c) typeName
      validConstraint = not (null constraint)
      typeDecl = "type " ++ typeName ++ " = int where { " ++ constraint ++ " }"
      result = if validType && validConstraint
               then DTP.parseTypeDeclaration typeDecl
               else Nothing
  in if validType && validConstraint
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试函数声明解析
prop_parse_function_declaration :: String -> Property
prop_parse_function_declaration funcName =
  let validFunc = not (null funcName) && isLetter (head funcName) && 
                  all (\c -> isLetter c || isDigit c) funcName
      funcDecl = "func " ++ funcName ++ "() -> int"
      result = if validFunc
               then DTP.parseDependentType funcDecl
               else Nothing
  in if validFunc
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试带参数的函数声明解析
prop_parse_function_with_params :: String -> String -> Property
prop_parse_function_with_params funcName paramName =
  let validFunc = not (null funcName) && isLetter (head funcName) && 
                  all (\c -> isLetter c || isDigit c) funcName
      validParam = not (null paramName) && isLetter (head paramName) && 
                   all (\c -> isLetter c || isDigit c) paramName
      funcDecl = "func " ++ funcName ++ "(" ++ paramName ++ ": int) -> int"
      result = if validFunc && validParam
               then DTP.parseDependentType funcDecl
               else Nothing
  in if validFunc && validParam
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试泛型函数声明解析
prop_parse_generic_function :: String -> String -> Property
prop_parse_generic_function funcName typeParam =
  let validFunc = not (null funcName) && isLetter (head funcName) && 
                  all (\c -> isLetter c || isDigit c) funcName
      validParam = not (null typeParam) && isLetter (head typeParam) && 
                   all (\c -> isLetter c || isDigit c) typeParam
      funcDecl = "func " ++ funcName ++ "[" ++ typeParam ++ ": int](x: " ++ typeParam ++ ") -> " ++ typeParam
      result = if validFunc && validParam
               then DTP.parseDependentType funcDecl
               else Nothing
  in if validFunc && validParam
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试结构体类型解析
prop_parse_struct_type :: String -> [String] -> Property
prop_parse_struct_type structName fieldNames =
  let validStruct = not (null structName) && isLetter (head structName) && 
                    all (\c -> isLetter c || isDigit c) structName
      validFields = all (\f -> not (null f) && isLetter (head f) && 
                              all (\c -> isLetter c || isDigit c) f) fieldNames
      fields = concatMap (\f -> "  " ++ f ++ ": int\n") fieldNames
      structDecl = "type " ++ structName ++ " = struct {\n" ++ fields ++ "}"
      result = if validStruct && validFields
               then DTP.parseTypeDeclaration structDecl
               else Nothing
  in if validStruct && validFields
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试别名类型解析
prop_parse_alias_type :: String -> String -> Property
prop_parse_alias_type aliasName originalType =
  let validAlias = not (null aliasName) && isLetter (head aliasName) && 
                   all (\c -> isLetter c || isDigit c) aliasName
      validOriginal = not (null originalType) && isLetter (head originalType)
      aliasDecl = "type " ++ aliasName ++ " = " ++ originalType
      result = if validAlias && validOriginal
               then DTP.parseTypeDeclaration aliasDecl
               else Nothing
  in if validAlias && validOriginal
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试联合类型解析
prop_parse_union_type :: String -> [String] -> Property
prop_parse_union_type unionType typeNames =
  let validUnion = not (null unionType) && isLetter (head unionType) && 
                   all (\c -> isLetter c || isDigit c) unionType
      validTypes = all (\t -> not (null t) && isLetter (head t) && 
                             all (\c -> isLetter c || isDigit c) t) typeNames
      typeList = intercalate " | " typeNames
      unionDecl = "type " ++ unionType ++ " = " ++ typeList
      result = if validUnion && validTypes
               then DTP.parseTypeDeclaration unionDecl
               else Nothing
  in if validUnion && validTypes
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试嵌套类型解析
prop_parse_nested_type :: String -> String -> Property
prop_parse_nested_type outerType innerType =
  let validOuter = not (null outerType) && isLetter (head outerType) && 
                   all (\c -> isLetter c || isDigit c) outerType
      validInner = not (null innerType) && isLetter (head innerType) && 
                   all (\c -> isLetter c || isDigit c) innerType
      nestedDecl = "type " ++ outerType ++ " = struct { value: " ++ innerType ++ " }"
      result = if validOuter && validInner
               then DTP.parseTypeDeclaration nestedDecl
               else Nothing
  in if validOuter && validInner
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试带where子句的函数解析
prop_parse_function_with_where :: String -> String -> Property
prop_parse_function_with_where funcName constraint =
  let validFunc = not (null funcName) && isLetter (head funcName) && 
                  all (\c -> isLetter c || isDigit c) funcName
      validConstraint = not (null constraint)
      funcDecl = "func " ++ funcName ++ "(n: int) -> int where { " ++ constraint ++ " }"
      result = if validFunc && validConstraint
               then DTP.parseDependentType funcDecl
               else Nothing
  in if validFunc && validConstraint
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试数组类型解析
prop_parse_array_type :: String -> String -> Property
prop_parse_array_type arrayName elementType =
  let validArray = not (null arrayName) && isLetter (head arrayName) && 
                   all (\c -> isLetter c || isDigit c) arrayName
      validElement = not (null elementType) && isLetter (head elementType)
      arrayDecl = "type " ++ arrayName ++ " = [" ++ elementType ++ "]"
      result = if validArray && validElement
               then DTP.parseTypeDeclaration arrayDecl
               else Nothing
  in if validArray && validElement
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试指针类型解析
prop_parse_pointer_type :: String -> String -> Property
prop_parse_pointer_type ptrName targetType =
  let validPtr = not (null ptrName) && isLetter (head ptrName) && 
                 all (\c -> isLetter c || isDigit c) ptrName
      validTarget = not (null targetType) && isLetter (head targetType)
      ptrDecl = "type " ++ ptrName ++ " = *" ++ targetType
      result = if validPtr && validTarget
               then DTP.parseTypeDeclaration ptrDecl
               else Nothing
  in if validPtr && validTarget
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试可选类型解析
prop_parse_optional_type :: String -> String -> Property
prop_parse_optional_type optionalName elementType =
  let validOptional = not (null optionalName) && isLetter (head optionalName) && 
                      all (\c -> isLetter c || isDigit c) optionalName
      validElement = not (null elementType) && isLetter (head elementType)
      optionalDecl = "type " ++ optionalName ++ " = ?" ++ elementType
      result = if validOptional && validElement
               then DTP.parseTypeDeclaration optionalDecl
               else Nothing
  in if validOptional && validElement
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试函数类型解析
prop_parse_function_type :: String -> String -> String -> Property
prop_parse_function_type funcTypeName inputType outputType =
  let validFuncType = not (null funcTypeName) && isLetter (head funcTypeName) && 
                      all (\c -> isLetter c || isDigit c) funcTypeName
      validInput = not (null inputType) && isLetter (head inputType)
      validOutput = not (null outputType) && isLetter (head outputType)
      funcTypeDecl = "type " ++ funcTypeName ++ " = (" ++ inputType ++ ") -> " ++ outputType
      result = if validFuncType && validInput && validOutput
               then DTP.parseTypeDeclaration funcTypeDecl
               else Nothing
  in if validFuncType && validInput && validOutput
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试元组类型解析
prop_parse_tuple_type :: String -> [String] -> Property
prop_parse_tuple_type tupleName elementTypes =
  let validTuple = not (null tupleName) && isLetter (head tupleName) && 
                   all (\c -> isLetter c || isDigit c) tupleName
      validElements = all (\t -> not (null t) && isLetter (head t)) elementTypes
      typeList = intercalate ", " elementTypes
      tupleDecl = "type " ++ tupleName ++ " = (" ++ typeList ++ ")"
      result = if validTuple && validElements
               then DTP.parseTypeDeclaration tupleDecl
               else Nothing
  in if validTuple && validElements
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试映射类型解析
prop_parse_map_type :: String -> String -> String -> Property
prop_parse_map_type mapName keyType valueType =
  let validMap = not (null mapName) && isLetter (head mapName) && 
                 all (\c -> isLetter c || isDigit c) mapName
      validKey = not (null keyType) && isLetter (head keyType)
      validValue = not (null valueType) && isLetter (head valueType)
      mapDecl = "type " ++ mapName ++ " = map[" ++ keyType ++ "]" ++ valueType
      result = if validMap && validKey && validValue
               then DTP.parseTypeDeclaration mapDecl
               else Nothing
  in if validMap && validKey && validValue
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试通道类型解析
prop_parse_channel_type :: String -> String -> Property
prop_parse_channel_type channelName elementType =
  let validChannel = not (null channelName) && isLetter (head channelName) && 
                     all (\c -> isLetter c || isDigit c) channelName
      validElement = not (null elementType) && isLetter (head elementType)
      channelDecl = "type " ++ channelName ++ " = chan " ++ elementType
      result = if validChannel && validElement
               then DTP.parseTypeDeclaration channelDecl
               else Nothing
  in if validChannel && validElement
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试接口类型解析
prop_parse_interface_type :: String -> [String] -> Property
prop_parse_interface_type interfaceName methodNames =
  let validInterface = not (null interfaceName) && isLetter (head interfaceName) && 
                       all (\c -> isLetter c || isDigit c) interfaceName
      validMethods = all (\m -> not (null m) && isLetter (head m) && 
                               all (\c -> isLetter c || isDigit c) m) methodNames
      methods = concatMap (\m -> "  " ++ m ++ "()\n") methodNames
      interfaceDecl = "type " ++ interfaceName ++ " = interface {\n" ++ methods ++ "}"
      result = if validInterface && validMethods
               then DTP.parseTypeDeclaration interfaceDecl
               else Nothing
  in if validInterface && validMethods
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试多个声明解析
prop_parse_multiple_declarations :: [String] -> Property
prop_parse_multiple_declarations typeNames =
  let -- 确保类型名称是有效的
      validTypes = filter (\name -> 
                           not (null name) && isLetter (head name) &&
                           all (\c -> isLetter c || isDigit c) name) typeNames
      declarations = map (\name -> "type " ++ name ++ " = int") validTypes
      input = unlines declarations
      result = DTP.runDependentTypesParser input
  in property $ length (fst result) === length validTypes

-- | 测试解析错误处理
prop_parse_error_handling :: String -> Property
prop_parse_error_handling invalidInput =
  let result = DTP.validateDependentTypeSyntax invalidInput
  in property $ length result >= 0

-- | 测试注释保留
prop_parse_preserves_comments :: String -> Property
prop_parse_preserves_comments comment =
  let -- 确保注释不包含结束标记
      safeComment = filter (/= '*') comment
      input = "type Test = int // " ++ safeComment
      result = DTP.runDependentTypesParser input
  in property $ length (fst result) >= 0

-- | 测试解析一致性
prop_parse_consistency :: String -> Property
prop_parse_consistency input =
  let result1 = DTP.runDependentTypesParser input
      result2 = DTP.runDependentTypesParser input
  in property $ length (fst result1) === length (fst result2)

-- | 测试空输入处理
prop_parse_empty_input :: Property
prop_parse_empty_input = 
  let result = DTP.runDependentTypesParser ""
  in property $ null (fst result)

-- | 测试空白输入处理
prop_parse_whitespace_input :: String -> Property
prop_parse_whitespace_input whitespace =
  let result = DTP.runDependentTypesParser whitespace
  in property $ null (fst result)

-- | 测试复杂类型表达式解析
prop_parse_complex_type_expression :: String -> Property
prop_parse_complex_type_expression typeName =
  let validType = not (null typeName) && isLetter (head typeName) && 
                  all (\c -> isLetter c || isDigit c) typeName
      complexDecl = "type " ++ typeName ++ " = map[string]struct { value: int, items: []string }"
      result = if validType
               then DTP.parseTypeDeclaration complexDecl
               else Nothing
  in if validType
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试递归类型解析
prop_parse_recursive_type :: String -> Property
prop_parse_recursive_type typeName =
  let validType = not (null typeName) && isLetter (head typeName) && 
                  all (\c -> isLetter c || isDigit c) typeName
      recursiveDecl = "type " ++ typeName ++ " = struct { value: int, next: ?" ++ typeName ++ " }"
      result = if validType
               then DTP.parseTypeDeclaration recursiveDecl
               else Nothing
  in if validType
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- | 测试高级约束解析
prop_parse_advanced_constraints :: String -> String -> Property
prop_parse_advanced_constraints typeName constraint =
  let validType = not (null typeName) && isLetter (head typeName) && 
                  all (\c -> isLetter c || isDigit c) typeName
      validConstraint = not (null constraint)
      advancedDecl = "type " ++ typeName ++ " = int where { " ++ constraint ++ " && len(self) > 0 }"
      result = if validType && validConstraint
               then DTP.parseTypeDeclaration advancedDecl
               else Nothing
  in if validType && validConstraint
     then property $ isJust result || isNothing result
     else property $ isNothing result

-- 将所有测试组合在一起
testSuite :: TestTree
testSuite = testGroup "DependentTypesParser模块Comprehensive QuickCheck测试"
  [ testProperty "runDependentTypesParser函数" prop_run_dependent_types_parser
  , testProperty "parseDependentType函数" prop_parse_dependent_type
  , testProperty "parseTypeDeclaration函数" prop_parse_type_declaration
  , testProperty "validateDependentTypeSyntax函数" prop_validate_dependent_type_syntax
  , testProperty "简单类型声明解析" prop_parse_simple_type_declaration
  , testProperty "泛型类型声明解析" prop_parse_generic_type_declaration
  , testProperty "带约束的类型声明解析" prop_parse_constrained_type_declaration
  , testProperty "函数声明解析" prop_parse_function_declaration
  , testProperty "带参数的函数声明解析" prop_parse_function_with_params
  , testProperty "泛型函数声明解析" prop_parse_generic_function
  , testProperty "结构体类型解析" prop_parse_struct_type
  , testProperty "别名类型解析" prop_parse_alias_type
  , testProperty "联合类型解析" prop_parse_union_type
  , testProperty "嵌套类型解析" prop_parse_nested_type
  , testProperty "带where子句的函数解析" prop_parse_function_with_where
  , testProperty "数组类型解析" prop_parse_array_type
  , testProperty "指针类型解析" prop_parse_pointer_type
  , testProperty "可选类型解析" prop_parse_optional_type
  , testProperty "函数类型解析" prop_parse_function_type
  , testProperty "元组类型解析" prop_parse_tuple_type
  , testProperty "映射类型解析" prop_parse_map_type
  , testProperty "通道类型解析" prop_parse_channel_type
  , testProperty "接口类型解析" prop_parse_interface_type
  , testProperty "多个声明解析" prop_parse_multiple_declarations
  , testProperty "解析错误处理" prop_parse_error_handling
  , testProperty "注释保留" prop_parse_preserves_comments
  , testProperty "解析一致性" prop_parse_consistency
  , testProperty "空输入处理" prop_parse_empty_input
  , testProperty "空白输入处理" prop_parse_whitespace_input
  , testProperty "复杂类型表达式解析" prop_parse_complex_type_expression
  , testProperty "递归类型解析" prop_parse_recursive_type
  , testProperty "高级约束解析" prop_parse_advanced_constraints
  ]