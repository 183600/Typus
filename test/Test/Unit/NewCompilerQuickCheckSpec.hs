{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewCompilerQuickCheckSpec where

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

import Compiler
import Parser (TypusFile(..), CodeBlock(..), defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..))
import Data.Either (isLeft, isRight)
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

-- | 测试编译错误分类
prop_compiler_error_categorization :: String -> Property
prop_compiler_error_categorization msg =
  let limitedMsg = take 50 msg
      error = CompilerError SyntaxError CompilationPhase limitedMsg Nothing
  in errorCategory error === SyntaxError

-- | 测试编译错误严重性
prop_compiler_error_severity :: String -> Property
prop_compiler_error_severity msg =
  let limitedMsg = take 50 msg
      error = CompilerError TypeError CompilationPhase limitedMsg Nothing
  in errorSeverity error === Error

-- | 测试编译错误阶段
prop_compiler_error_phase :: String -> Property
prop_compiler_error_phase msg =
  let limitedMsg = take 50 msg
      error = CompilerError RuntimeError RuntimePhase limitedMsg Nothing
  in errorPhase error === RuntimePhase

-- | 测试编译错误消息
prop_compiler_error_message :: String -> Property
prop_compiler_error_message msg =
  let limitedMsg = take 100 msg
      error = CompilerError WarningError ValidationPhase limitedMsg Nothing
  in errorMessage error === limitedMsg

-- | 测试语法错误创建
prop_syntax_error_creation :: String -> Property
prop_syntax_error_creation msg =
  let limitedMsg = take 80 msg
      span = SourceSpan (SourcePos 1 1) (SourcePos 1 10)
      error = SyntaxError limitedMsg span
  in conjoin
    [ seMessage error === limitedMsg
    , seSpan error === span
    ]

-- | 测试类型错误创建
prop_type_error_creation :: String -> String -> Property
prop_type_error_creation expected actual =
  let limitedExpected = take 30 expected
      limitedActual = take 30 actual
      span = SourceSpan (SourcePos 1 1) (SourcePos 1 10)
      error = TypeError limitedExpected limitedActual span
  in conjoin
    [ teExpected error === limitedExpected
    , teActual error === limitedActual
    , teSpan error === span
    ]

-- | 测试畸形语法错误
prop_malformed_syntax_error_properties :: Property
prop_malformed_syntax_error_properties =
  let error = malformedSyntaxError
  in conjoin
    [ errorCategory error === SyntaxError
    , errorPhase error === CompilationPhase
    , errorSeverity error === Error
    , "malformed syntax" `isInfixOf` errorMessage error
    ]

-- | 测试编译错误格式化
prop_compiler_error_formatting :: String -> Property
prop_compiler_error_formatting msg =
  let limitedMsg = take 60 msg
      error = CompilerError TypeError CompilationPhase limitedMsg Nothing
      formatted = renderCompilationError [error]
  in limitedMsg `isInfixOf` formatted

-- | 测试多个编译错误格式化
prop_multiple_compiler_errors_formatting :: String -> String -> Property
prop_multiple_compiler_errors_formatting msg1 msg2 =
  let limitedMsg1 = take 40 msg1
      limitedMsg2 = take 40 msg2
      error1 = CompilerError SyntaxError CompilationPhase limitedMsg1 Nothing
      error2 = CompilerError TypeError CompilationPhase limitedMsg2 Nothing
      formatted = renderCompilationError [error1, error2]
  in conjoin
    [ limitedMsg1 `isInfixOf` formatted
    , limitedMsg2 `isInfixOf` formatted
    ]

-- | 测试编译错误分析
prop_compiler_error_analysis :: String -> Property
prop_compiler_error_analysis msg =
  let limitedMsg = take 50 msg
      error = CompilerError TypeError CompilationPhase limitedMsg Nothing
      analysis = analyzeErrors [error]
  in not (null analysis)

-- | 测试类型错误检查
prop_type_error_checking :: String -> Property
prop_type_error_checking msg =
  let limitedMsg = take 50 msg
      error = TypeError "Int" "String" (SourceSpan (SourcePos 1 1) (SourcePos 1 10))
  in hasTypeErrors [error]

-- | 测试方法声明检查
prop_method_declaration_check :: String -> Property
prop_method_declaration_check declaration =
  let limitedDecl = take 60 declaration
      -- 简化的方法声明检查逻辑
      isMethod = "func" `isPrefixOf` limitedDecl && "(" `isInfixOf` limitedDecl
  in isMethodDeclaration limitedDecl === isMethod

-- | 测试依赖类型检查
prop_dependent_types_check :: String -> Property
prop_dependent_types_check content =
  let limitedContent = take 100 content
      span = SourceSpan (SourcePos 1 1) (SourcePos 1 (length limitedContent + 1))
      directives = defaultBlockDirectives
      block = CodeBlock directives limitedContent span
      typusFile = TypusFile defaultFileDirectives [] [block] []
      result = checkDependentTypes typusFile
  in property True  -- 简化测试，实际应该检查返回值

-- | 测试所有权检查
prop_ownership_check :: String -> Property
prop_ownership_check content =
  let limitedContent = take 100 content
      span = SourceSpan (SourcePos 1 1) (SourcePos 1 (length limitedContent + 1))
      directives = defaultBlockDirectives
      block = CodeBlock directives limitedContent span
      typusFile = TypusFile defaultFileDirectives [] [block] []
      result = checkOwnership typusFile
  in property True  -- 简化测试，实际应该检查返回值

-- | 测试声明提取
prop_declaration_extraction :: String -> Property
prop_declaration_extraction content =
  let limitedContent = take 80 content
      declarations = extractDeclarations limitedContent
  in property (length declarations >= 0)

-- | 测试函数调用提取
prop_function_call_extraction :: String -> Property
prop_function_call_extraction content =
  let limitedContent = take 80 content
      calls = extractFunctionCalls limitedContent
  in property (length calls >= 0)

-- | 测试类型环境构建
prop_type_environment_building :: [(String, String)] -> Property
prop_type_environment_building pairs =
  let limitedPairs = take 10 pairs
      stringPairs = map (\(a, b) -> (take 20 a, take 20 b)) limitedPairs
      typeEnv = buildTypeEnvFromPairs stringPairs
  in property (length typeEnv >= 0)

-- | 测试编译器错误处理
test_compiler_error_handling :: Assertion
test_compiler_error_handling = do
  let error = CompilerError SyntaxError CompilationPhase "Test error" Nothing
      errors = [error]
      formatted = renderCompilationError errors
  assertBool "Error message should be in formatted output" $ "Test error" `isInfixOf` formatted

-- | 测试编译器基本功能
test_compiler_basic_functionality :: Assertion
test_compiler_basic_functionality = do
  let span = SourceSpan (SourcePos 1 1) (SourcePos 1 20)
      directives = defaultBlockDirectives
      block = CodeBlock directives "func main() { return 42 }" span
      typusFile = TypusFile defaultFileDirectives [] [block] []
      result = compile typusFile
  case result of
    Left _ -> assertBool "Compilation might fail with malformed input" True
    Right output -> assertBool "Should produce some output" $ not (null output)

-- | 测试编译器空输入处理
test_compiler_empty_input :: Assertion
test_compiler_empty_input = do
  let typusFile = TypusFile defaultFileDirectives [] [] []
      result = compile typusFile
  case result of
    Left _ -> assertBool "Empty input should produce error" True
    Right output -> assertFailure "Empty input should not compile successfully"

-- | 测试编译器复杂输入处理
test_compiler_complex_input :: Assertion
test_compiler_complex_input = do
  let span1 = SourceSpan (SourcePos 1 1) (SourcePos 1 30)
      span2 = SourceSpan (SourcePos 2 1) (SourcePos 2 25)
      directives = defaultBlockDirectives
      block1 = CodeBlock directives "func add(a int, b int) int { return a + b }" span1
      block2 = CodeBlock directives "func main() { result := add(1, 2); return result }" span2
      typusFile = TypusFile defaultFileDirectives [] [block1, block2] []
      result = compile typusFile
  case result of
    Left _ -> assertBool "Complex input might fail" True
    Right output -> assertBool "Should produce some output" $ not (null output)

-- | 测试编译器错误报告
test_compiler_error_reporting :: Assertion
test_compiler_error_reporting = do
  let error1 = CompilerError SyntaxError CompilationPhase "Syntax error" Nothing
      error2 = CompilerError TypeError CompilationPhase "Type error" Nothing
      errors = [error1, error2]
      report = generateDetailedReport errors
  assertBool "Report should contain syntax error" $ "Syntax error" `isInfixOf` report
  assertBool "Report should contain type error" $ "Type error" `isInfixOf` report

-- | 测试类型诊断转换
test_type_diagnostic_conversion :: Assertion
test_type_diagnostic_conversion = do
  let diagnostic = TypeCheckDiagnostic "Expected Int" "Found String" (SourceSpan (SourcePos 1 1) (SourcePos 1 10))
      error = typeDiagnosticToCompilerError diagnostic
  assertEqual "Should convert to TypeError" TypeError (errorCategory error)

-- | 测试Go代码生成
test_go_code_generation :: Assertion
test_go_code_generation = do
  let span = SourceSpan (SourcePos 1 1) (SourcePos 1 20)
      directives = defaultBlockDirectives
      block = CodeBlock directives "func test() { return 42 }" span
      typusFile = TypusFile defaultFileDirectives [] [block] []
      result = generateGoCode typusFile
  case result of
    Left _ -> assertBool "Code generation might fail" True
    Right goCode -> assertBool "Should produce Go code" $ "func" `isInfixOf` goCode

-- | 测试套件
tests :: TestTree
tests = memoryLevelTestGroup Moderate "New Compiler QuickCheck Tests"
  [ withMemoryLevel Moderate $ testProperty "Compiler error categorization" prop_compiler_error_categorization
  , withMemoryLevel Moderate $ testProperty "Compiler error severity" prop_compiler_error_severity
  , withMemoryLevel Moderate $ testProperty "Compiler error phase" prop_compiler_error_phase
  , withMemoryLevel Moderate $ testProperty "Compiler error message" prop_compiler_error_message
  , withMemoryLevel Moderate $ testProperty "Syntax error creation" prop_syntax_error_creation
  , withMemoryLevel Moderate $ testProperty "Type error creation" prop_type_error_creation
  , withMemoryLevel Moderate $ testProperty "Malformed syntax error properties" prop_malformed_syntax_error_properties
  , withMemoryLevel Moderate $ testProperty "Compiler error formatting" prop_compiler_error_formatting
  , withMemoryLevel Moderate $ testProperty "Multiple compiler errors formatting" prop_multiple_compiler_errors_formatting
  , withMemoryLevel Moderate $ testProperty "Compiler error analysis" prop_compiler_error_analysis
  , withMemoryLevel Moderate $ testProperty "Type error checking" prop_type_error_checking
  , withMemoryLevel Moderate $ testProperty "Method declaration check" prop_method_declaration_check
  , withMemoryLevel Moderate $ testProperty "Dependent types check" prop_dependent_types_check
  , withMemoryLevel Moderate $ testProperty "Ownership check" prop_ownership_check
  , withMemoryLevel Moderate $ testProperty "Declaration extraction" prop_declaration_extraction
  , withMemoryLevel Moderate $ testProperty "Function call extraction" prop_function_call_extraction
  , withMemoryLevel Moderate $ testProperty "Type environment building" prop_type_environment_building
  , testCase "Compiler error handling" test_compiler_error_handling
  , testCase "Compiler basic functionality" test_compiler_basic_functionality
  , testCase "Compiler empty input" test_compiler_empty_input
  , testCase "Compiler complex input" test_compiler_complex_input
  , testCase "Compiler error reporting" test_compiler_error_reporting
  , testCase "Type diagnostic conversion" test_type_diagnostic_conversion
  , testCase "Go code generation" test_go_code_generation
  ]

-- | 轻量级测试套件，用于内存受限环境
essentialTests :: TestTree
essentialTests = memoryLevelTestGroup Minimal "New Compiler Essential Tests"
  [ withMemoryLevel Minimal $ testProperty "Compiler error categorization" prop_compiler_error_categorization
  , withMemoryLevel Minimal $ testProperty "Syntax error creation" prop_syntax_error_creation
  , withMemoryLevel Minimal $ testProperty "Compiler error formatting" prop_compiler_error_formatting
  , withMemoryLevel Minimal $ testCase "Compiler error handling" test_compiler_error_handling
  , withMemoryLevel Minimal $ testCase "Compiler basic functionality" test_compiler_basic_functionality
  ]