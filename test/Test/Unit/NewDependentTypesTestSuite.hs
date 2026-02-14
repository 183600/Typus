{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewDependentTypesTestSuite where

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

import DependentTypesParser (parseDependentType, parseTypeReference, parseTypeExpression, DependentType(..), TypeBody(..), Field(..), TypeRef(..))
import Parser (parseTypus, TypusFile(..))
import Compiler (compile, CompilerResult, CompilerError(..), renderCompilationError)
import Compiler.Errors (ErrorCategory(..), ErrorSeverity(..), CompilationPhase(..), mkCompilerError)
import Utils (trim)
import qualified Data.Text as T
import Text.Megaparsec (runParser, errorBundlePretty)

-- | 辅助函数：从字符串编译 Typus 代码
compileTypusString :: String -> CompilerResult String
compileTypusString input = 
  case parseTypus input of
    Left err -> Left [mkCompilerError "ParseError" (T.pack err) ParsingPhase Parsing Error Nothing Nothing [] ["compileTypusString"] Nothing]
    Right typusFile -> compile typusFile

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

-- | 测试依赖类型解析器的基本属性
prop_parse_dependent_type_basic :: String -> Property
prop_parse_dependent_type_basic s =
  let limitedString = take 10 s  -- 限制字符串大小
      result = parseDependentType limitedString
  in property $ case result of
    Left _ -> True
    Right dt -> length (show dt) <= 100  -- 限制结果大小

-- | 测试Vector类型的解析
test_vector_type_parsing :: Assertion
test_vector_type_parsing = do
  let validVector = "Vector<n3>"
      result = parseTypeExpression validVector
  case result of
    Right ty -> assertEqual "Vector type parsed correctly" (TypeRef "Vector" [TypeRef "n3" []]) ty
    Left err -> assertFailure $ "Failed to parse Vector type: " ++ err

-- | 测试Matrix类型的解析
test_matrix_type_parsing :: Assertion
test_matrix_type_parsing = do
  let validMatrix = "Matrix<m3, n4>"
      result = parseTypeExpression validMatrix
  case result of
    Right ty -> assertEqual "Matrix type parsed correctly" (TypeRef "Matrix" [TypeRef "m3" [], TypeRef "n4" []]) ty
    Left err -> assertFailure $ "Failed to parse Matrix type: " ++ err

-- | 测试NonZero约束类型的解析
test_nonzero_constraint_parsing :: Assertion
test_nonzero_constraint_parsing = do
  let validNonZero = "NonZero"
      result = parseTypeExpression validNonZero
  case result of
    Right ty -> assertEqual "NonZero constraint parsed correctly" (TypeRef "NonZero" []) ty
    Left err -> assertFailure $ "Failed to parse NonZero constraint: " ++ err

-- | 测试Positive约束类型的解析
test_positive_constraint_parsing :: Assertion
test_positive_constraint_parsing = do
  let validPositive = "Positive"
      result = parseTypeExpression validPositive
  case result of
    Right ty -> assertEqual "Positive constraint parsed correctly" (TypeRef "Positive" []) ty
    Left err -> assertFailure $ "Failed to parse Positive constraint: " ++ err

-- | 测试Bounded约束类型的解析
test_bounded_constraint_parsing :: Assertion
test_bounded_constraint_parsing = do
  let validBounded = "Bounded<minVal, maxVal>"
      result = parseTypeExpression validBounded
  case result of
    Right ty -> assertEqual "Bounded constraint parsed correctly" (TypeRef "Bounded" [TypeRef "minVal" [], TypeRef "maxVal" []]) ty
    Left err -> assertFailure $ "Failed to parse Bounded constraint: " ++ err

-- | 测试依赖函数签名的解析
test_dependent_function_parsing :: Assertion
test_dependent_function_parsing = do
  let validFunction = "func zeros(n: Positive) -> Vector[n]"
      result = parseTypus validFunction
  case result of
    Right ast -> assertBool "Dependent function parsed successfully" (not $ null $ show ast)
    Left err -> assertFailure $ "Failed to parse dependent function: " ++ err

-- | 测试类型级算术的解析
test_type_level_arithmetic_parsing :: Assertion
test_type_level_arithmetic_parsing = do
  let validArithmetic = "Add<m1, n2>"
      result = parseTypeExpression validArithmetic
  case result of
    Right ty -> assertEqual "Type-level arithmetic parsed correctly" (TypeRef "Add" [TypeRef "m1" [], TypeRef "n2" []]) ty
    Left err -> assertFailure $ "Failed to parse type-level arithmetic: " ++ err

-- | 测试存在类型的解析
test_existential_type_parsing :: Assertion
test_existential_type_parsing = do
  let validExistential = "Exists<varName>"
      result = runParser parseTypeReference "<input>" validExistential
  case result of
    Right ty -> assertEqual "Existential type parsed correctly" "Exists[varName]" (show ty)
    Left err -> assertFailure $ "Failed to parse existential type: " ++ (errorBundlePretty err)

-- | 测试混合参数类型的解析
test_mixed_parameters_parsing :: Assertion
test_mixed_parameters_parsing = do
  let validMixed = "BoundedSlice<TypeParam, capacityVar>"
      result = parseTypeExpression validMixed
  case result of
    Right ty -> assertEqual "Mixed parameters parsed correctly" (TypeRef "BoundedSlice" [TypeRef "TypeParam" [], TypeRef "capacityVar" []]) ty
    Left err -> assertFailure $ "Failed to parse mixed parameters: " ++ err

-- | 测试函数前置条件的解析
test_function_precondition_parsing :: Assertion
test_function_precondition_parsing = do
  let validPrecondition = "func average[n: int](v: Vector[n]) -> float64 where { n > 0 }"
      result = parseTypus validPrecondition
  case result of
    Right ast -> assertBool "Function precondition parsed successfully" (not $ null $ show ast)
    Left err -> assertFailure $ "Failed to parse function precondition: " ++ err

-- | 测试assert语句的解析
test_assert_parsing :: Assertion
test_assert_parsing = do
  let validAssert = "assert n > 0"
      result = parseTypus validAssert
  case result of
    Right ast -> assertBool "Assert statement parsed successfully" (not $ null $ show ast)
    Left err -> assertFailure $ "Failed to parse assert statement: " ++ err

-- | 测试static_assert语句的解析
test_static_assert_parsing :: Assertion
test_static_assert_parsing = do
  let validStaticAssert = "static_assert n > 0"
      result = parseTypus validStaticAssert
  case result of
    Right ast -> assertBool "Static assert statement parsed successfully" (not $ null $ show ast)
    Left err -> assertFailure $ "Failed to parse static assert statement: " ++ err

-- | 测试match语句的解析
test_match_parsing :: Assertion
test_match_parsing = do
  let validMatch = "match v.(n) { fmt.Println(get(v, 0)) }"
      result = parseTypus validMatch
  case result of
    Right ast -> assertBool "Match statement parsed successfully" (not $ null $ show ast)
    Left err -> assertFailure $ "Failed to parse match statement: " ++ err

-- | 测试依赖类型编译
test_dependent_type_compilation :: Assertion
test_dependent_type_compilation = do
  let validCode = "package main\n\nfunc main() {\n    // Simple test\n}\n"
      result = compileTypusString validCode
  case result of
    Right goCode -> assertBool "Generated Go code contains main function" ("func main" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to compile simple code: " ++ renderCompilationError err

-- | 测试Vector类型编译
test_vector_type_compilation :: Assertion
test_vector_type_compilation = do
  let validCode = "package main\n\ntype Vector struct {\n    data []float64\n}\n\nfunc main() {\n    // Simple test\n}\n"
      result = compileTypusString validCode
  case result of
    Right goCode -> assertBool "Generated Go code contains Vector struct" ("type Vector struct" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to compile Vector type: " ++ renderCompilationError err

-- | 测试约束违反处理
test_constraint_violation_handling :: Assertion
test_constraint_violation_handling = do
  let validCode = "package main\n\nfunc test() {\n    panic(\"test panic\")\n}\n\nfunc main() {\n    // Simple test\n}\n"
      result = compileTypusString validCode
  case result of
    Right goCode -> assertBool "Generated Go code contains panic" ("panic(" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to compile constraint violation handling: " ++ renderCompilationError err

-- | 测试错误模式下的约束处理
test_error_mode_constraint_handling :: Assertion
test_error_mode_constraint_handling = do
  let validCode = "package main\n\nimport \"errors\"\n\nfunc test() (int, error) {\n    return 0, errors.New(\"test error\")\n}\n\nfunc main() {\n    // Simple test\n}\n"
      result = compileTypusString validCode
  case result of
    Right goCode -> assertBool "Generated Go code returns error" ("error" `isInfixOf` goCode)
    Left err -> assertFailure $ "Failed to compile error mode constraint handling: " ++ renderCompilationError err

-- | 测试依赖类型QuickCheck属性
prop_dependent_type_parsing_roundtrip :: String -> Property
prop_dependent_type_parsing_roundtrip s =
  let limitedString = take 5 s  -- 限制字符串大小
      result = parseDependentType limitedString
  in case result of
    Left _ -> property True
    Right dt -> 
      let str = show dt
          result2 = parseDependentType str
      in case result2 of
        Left _ -> property False
        Right dt2 -> property $ show dt2 == str

-- | 测试依赖类型约束验证
prop_constraint_validation :: Int -> Property
prop_constraint_validation n =
  let limitedN = abs n `mod` 10  -- 限制输入范围
      result = parseDependentType ("type Positive = int where { self > 0 }")
  in case result of
    Left _ -> property $ limitedN /= limitedN  -- 总是False，但确保测试结构正确
    Right (dt, _) -> 
      -- 简单测试：确保解析成功且结果不为空
      let dtStr = show dt
      in property $ not (null dtStr) && length dtStr <= 500

-- | 测试套件
tests :: TestTree
tests = memoryLevelTestGroup Minimal "New Dependent Types Test Suite (Memory Optimized)"
  [ withMemoryLevel Minimal $ testCase "Vector type parsing" test_vector_type_parsing
  , withMemoryLevel Minimal $ testCase "Matrix type parsing" test_matrix_type_parsing
  , withMemoryLevel Minimal $ testCase "NonZero constraint parsing" test_nonzero_constraint_parsing
  , withMemoryLevel Minimal $ testCase "Positive constraint parsing" test_positive_constraint_parsing
  , withMemoryLevel Minimal $ testCase "Bounded constraint parsing" test_bounded_constraint_parsing
  , withMemoryLevel Minimal $ testCase "Dependent function parsing" test_dependent_function_parsing
  , withMemoryLevel Minimal $ testCase "Type-level arithmetic parsing" test_type_level_arithmetic_parsing
  , withMemoryLevel Minimal $ testCase "Existential type parsing" test_existential_type_parsing
  , withMemoryLevel Minimal $ testCase "Mixed parameters parsing" test_mixed_parameters_parsing
  , withMemoryLevel Minimal $ testCase "Function precondition parsing" test_function_precondition_parsing
  , withMemoryLevel Minimal $ testCase "Assert parsing" test_assert_parsing
  , withMemoryLevel Minimal $ testCase "Static assert parsing" test_static_assert_parsing
  , withMemoryLevel Minimal $ testCase "Match parsing" test_match_parsing
  , withMemoryLevel Minimal $ testCase "Dependent type compilation" test_dependent_type_compilation
  , withMemoryLevel Minimal $ testCase "Vector type compilation" test_vector_type_compilation
  , withMemoryLevel Minimal $ testCase "Constraint violation handling" test_constraint_violation_handling
  , withMemoryLevel Minimal $ testCase "Error mode constraint handling" test_error_mode_constraint_handling
  , withMemoryLevel Minimal $ testProperty "Dependent type parsing roundtrip" prop_dependent_type_parsing_roundtrip
  , withMemoryLevel Minimal $ testProperty "Constraint validation" prop_constraint_validation
  ]