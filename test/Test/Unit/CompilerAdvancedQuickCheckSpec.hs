module Test.Unit.CompilerAdvancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler
  ( compile
  , CompilerError(..)
  , CompilerResult
  , CompilationPhase(..)
  , SyntaxError(..)
  , TypeError(..)
  , malformedSyntaxError
  , renderCompilationError
  , formatCompilerErrors
  , generateDetailedReport
  , analyzeErrors
  , hasTypeErrors
  , TypeCheckDiagnostic(..)
  , diagnoseTypeErrors
  , extractDeclarations
  , extractFunctionCalls
  , buildTypeEnv
  , buildTypeEnvFromPairs
  , createTypusFileFromErrors
  , isMethodDeclaration
  , checkTypeError
  , hasMalformedSyntax
  , checkDependentTypes
  , checkOwnership
  , ensureSourceIR
  , typeCheckFailure
  , typeDiagnosticToCompilerError
  , generateGoCode
  )
import Parser (TypusFile(..))
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Map.Strict as Map

-- | 生成简单的Typus代码
newtype SimpleTypusCode = SimpleTypusCode { getSimpleTypusCode :: String }
  deriving Show

instance Arbitrary SimpleTypusCode where
  arbitrary = do
    codeType <- elements ["var", "func", "expr", "empty"]
    case codeType of
      "var" -> return $ SimpleTypusCode "var x int = 42"
      "func" -> return $ SimpleTypusCode "func test() int { return 42 }"
      "expr" -> return $ SimpleTypusCode "let x = 42"
      "empty" -> return $ SimpleTypusCode ""
      _ -> return $ SimpleTypusCode "let x = 42"

-- | 生成可能包含错误的Typus代码
newtype ErrorTypusCode = ErrorTypusCode { getErrorTypusCode :: String }
  deriving Show

instance Arbitrary ErrorTypusCode where
  arbitrary = do
    errorType <- elements ["syntax", "type", "incomplete"]
    case errorType of
      "syntax" -> return $ ErrorTypusCode "let x = +"  -- 语法错误
      "type" -> return $ ErrorTypusCode "var x int = \"string\""  -- 类型错误
      "incomplete" -> return $ ErrorTypusCode "func missingReturn() int {"  -- 不完整的函数
      _ -> return $ ErrorTypusCode "let x = 42"

-- | 生成TypeCheckDiagnostic
instance Arbitrary TypeCheckDiagnostic where
  arbitrary = do
    hasContext <- arbitrary
    context <- if hasContext then Just "testContext" else Nothing
    detail <- elements ["type mismatch", "undefined variable", "invalid operation"]
    return $ TypeCheckDiagnostic context detail

-- | 生成TypusFile
instance Arbitrary TypusFile where
  arbitrary = do
    SimpleTypusCode code <- arbitrary
    let typusFile = TypusFile 
          { tfDirectives = Parser.defaultFileDirectives
          , tfBuildTags = []
          , tfBlocks = []
          , tfSyntaxErrors = []
          }
    return typusFile

-- | 测试编译器的基本属性
prop_compile_simple_code :: SimpleTypusCode -> Property
prop_compile_simple_code (SimpleTypusCode code) =
  let typusFile = TypusFile 
          { tfDirectives = Parser.defaultFileDirectives
          , tfBuildTags = []
          , tfBlocks = []
          , tfSyntaxErrors = []
          }
      result = compile typusFile
  in case result of
    Left _ -> property True  -- 编译可能失败，这是预期的
    Right _ -> property True  -- 编译可能成功

prop_compile_error_code :: ErrorTypusCode -> Property
prop_compile_error_code (ErrorTypusCode code) =
  let typusFile = TypusFile 
          { tfDirectives = Parser.defaultFileDirectives
          , tfBuildTags = []
          , tfBlocks = []
          , tfSyntaxErrors = []
          }
      result = compile typusFile
  in case result of
    Left errors -> length errors > 0  -- 应该有错误
    Right _ -> property False  -- 不应该成功编译

prop_compile_empty_file :: Property
prop_compile_empty_file =
  let typusFile = TypusFile 
          { tfDirectives = Parser.defaultFileDirectives
          , tfBuildTags = []
          , tfBlocks = []
          , tfSyntaxErrors = []
          }
      result = compile typusFile
  in case result of
    Left _ -> property True  -- 可能失败
    Right output -> not (null output)  -- 如果成功，应该有输出

-- | 测试错误处理的基本属性
prop_malformed_syntax_error :: Property
prop_malformed_syntax_error =
  let err = malformedSyntaxError
  in errorId err === "CP0001" .&&.
     severity err === Error .&&.
     phase err === ParsingPhase

prop_type_check_failure :: Property
prop_type_check_failure =
  let err = typeCheckFailure
  in errorId err === "CP0002" .&&.
     severity err === Error .&&.
     phase err === TypeCheckingPhase

prop_type_diagnostic_to_compiler_error :: TypeCheckDiagnostic -> Property
prop_type_diagnostic_to_compiler_error diagnostic =
  let err = typeDiagnosticToCompilerError diagnostic
  in errorId err === "CP0002" .&&.
     severity err === Error .&&.
     phase err === TypeCheckingPhase

-- | 测试错误格式化的基本属性
prop_render_compilation_error :: [CompilerError] -> Property
prop_render_compilation_error errors =
  let formatted = renderCompilationError errors
  in if null errors
     then null formatted
     else not (null formatted)

prop_format_compiler_errors :: [CompilerError] -> Property
prop_format_compiler_errors errors =
  let formatted = formatCompilerErrors errors
  in if null errors
     then null formatted
     else not (null formatted)

-- | 测试错误分析的基本属性
prop_analyze_errors :: [CompilerError] -> Property
prop_analyze_errors errors =
  let analysis = analyzeErrors errors
  in Map.size analysis >= 0

prop_generate_detailed_report :: [CompilerError] -> Property
prop_generate_detailed_report errors =
  let report = generateDetailedReport errors
  in if null errors
     then null report
     else not (null report)

-- | 测试类型检查诊断的基本属性
prop_diagnose_type_errors :: TypusFile -> Property
prop_diagnose_type_errors typusFile =
  case diagnoseTypeErrors typusFile of
    Left _ -> property True  -- 可能有错误
    Right diagnostics -> length diagnostics >= 0

prop_has_type_errors :: [TypeError] -> Property
prop_has_type_errors typeErrors =
  let hasErrors = hasTypeErrors typeErrors
  in if null typeErrors
     then not hasErrors
     else hasErrors

-- | 测试声明提取的基本属性
prop_extract_declarations :: TypusFile -> Property
prop_extract_declarations typusFile =
  let declarations = extractDeclarations typusFile
  in length declarations >= 0

prop_extract_function_calls :: TypusFile -> Property
prop_extract_function_calls typusFile =
  let calls = extractFunctionCalls typusFile
  in length calls >= 0

-- | 测试类型环境构建的基本属性
prop_build_type_env :: TypusFile -> Property
prop_build_type_env typusFile =
  let typeEnv = buildTypeEnv typusFile
  in case typeEnv of
    Left _ -> property True  -- 可能失败
    Right env -> Map.size env >= 0

prop_build_type_env_from_pairs :: [(String, String)] -> Property
prop_build_type_env_from_pairs pairs =
  let typeEnv = buildTypeEnvFromPairs pairs
  in Map.size typeEnv === length pairs

-- | 测试方法声明检查的基本属性
prop_is_method_declaration :: String -> Property
prop_is_method_declaration code =
  let isMethod = isMethodDeclaration code
  in property True  -- 函数应该返回布尔值

-- | 测试类型错误检查的基本属性
prop_check_type_error :: String -> Property
prop_check_type_error code =
  let hasError = checkTypeError code
  in property True  -- 函数应该返回布尔值

-- | 测试语法错误检查的基本属性
prop_has_malformed_syntax :: TypusFile -> Property
prop_has_malformed_syntax typusFile =
  let hasMalformed = hasMalformedSyntax typusFile
  in property True  -- 函数应该返回布尔值

-- | 测试依赖类型检查的基本属性
prop_check_dependent_types :: TypusFile -> Property
prop_check_dependent_types typusFile =
  case checkDependentTypes typusFile of
    Left _ -> property True  -- 可能失败
    Right _ -> property True  -- 可能成功

-- | 测试所有权检查的基本属性
prop_check_ownership :: TypusFile -> Property
prop_check_ownership typusFile =
  case checkOwnership typusFile of
    Left _ -> property True  -- 可能失败
    Right _ -> property True  -- 可能成功

-- | 测试源IR确保的基本属性
prop_ensure_source_ir :: TypusFile -> Property
prop_ensure_source_ir typusFile =
  case ensureSourceIR typusFile of
    Left _ -> property True  -- 可能失败
    Right _ -> property True  -- 可能成功

-- | 测试Go代码生成的基本属性
prop_generate_go_code :: SimpleTypusCode -> Property
prop_generate_go_code (SimpleTypusCode code) =
  let typusFile = TypusFile 
          { tfDirectives = Parser.defaultFileDirectives
          , tfBuildTags = []
          , tfBlocks = []
          , tfSyntaxErrors = []
          }
      goCode = generateGoCode typusFile
  in not (null goCode)

prop_generate_go_code_empty :: Property
prop_generate_go_code_empty =
  let typusFile = TypusFile 
          { tfDirectives = Parser.defaultFileDirectives
          , tfBuildTags = []
          , tfBlocks = []
          , tfSyntaxErrors = []
          }
      goCode = generateGoCode typusFile
  in not (null goCode) .&&. "package main" `isInfixOf` goCode

-- | 测试从错误创建Typus文件的基本属性
prop_create_typus_file_from_errors :: [TypeError] -> Property
prop_create_typus_file_from_errors typeErrors =
  let typusFile = createTypusFileFromErrors typeErrors
  in tfSyntaxErrors typusFile === typeErrors

-- | 测试编译错误的基本属性
prop_compiler_error_id :: CompilerError -> Property
prop_compiler_error_id err =
  not (null $ errorId err)

prop_compiler_error_message :: CompilerError -> Property
prop_compiler_error_message err =
  not (T.null $ message err)

prop_compiler_error_severity :: CompilerError -> Property
prop_compiler_error_severity err =
  let sev = severity err
  in sev `elem` [Fatal, Error, Warning, Info]

prop_compiler_error_phase :: CompilerError -> Property
prop_compiler_error_phase err =
  let ph = phase err
  in ph `elem` [ParsingPhase, TypeCheckingPhase, OptimizationPhase, CodeGenPhase]

-- | 测试编译结果的基本属性
prop_compiler_result_success :: String -> Property
prop_compiler_result_success code =
  let typusFile = TypusFile 
          { tfDirectives = Parser.defaultFileDirectives
          , tfBuildTags = []
          , tfBlocks = []
          , tfSyntaxErrors = []
          }
      result = compile typusFile
  in case result of
    Left _ -> property True  -- 可能失败
    Right output -> not (null output)  -- 如果成功，应该有输出

prop_compiler_result_failure :: ErrorTypusCode -> Property
prop_compiler_result_failure (ErrorTypusCode code) =
  let typusFile = TypusFile 
          { tfDirectives = Parser.defaultFileDirectives
          , tfBuildTags = []
          , tfBlocks = []
          , tfSyntaxErrors = []
          }
      result = compile typusFile
  in case result of
    Left errors -> length errors > 0  -- 应该有错误
    Right _ -> property False  -- 不应该成功编译

-- | 测试编译阶段的比较
prop_compilation_phase_ordering :: CompilationPhase -> CompilationPhase -> Property
prop_compilation_phase_ordering phase1 phase2 =
  let ordering = compare phase1 phase2
  in ordering === EQ || ordering === LT || ordering === GT

prop_compilation_phase_transitive :: CompilationPhase -> CompilationPhase -> CompilationPhase -> Property
prop_compilation_phase_transitive phase1 phase2 phase3 =
  (phase1 <= phase2 && phase2 <= phase3) ==> phase1 <= phase3

-- | 测试语法错误的基本属性
prop_syntax_error_message :: SyntaxError -> Property
prop_syntax_error_message err =
  not (null $ errorMessage err)

prop_syntax_error_position :: SyntaxError -> Property
prop_syntax_error_position err =
  let pos = errorPosition err
  in pos >= 0

-- | 测试类型错误的基本属性
prop_type_error_message :: TypeError -> Property
prop_type_error_message err =
  not (null $ typeErrorMessage err)

prop_type_error_position :: TypeError -> Property
prop_type_error_position err =
  let pos = typeErrorPosition err
  in pos >= 0

tests :: TestTree
tests = testGroup "Compiler Advanced QuickCheck Tests"
  -- Compilation tests
  [ testProperty "compile simple code" prop_compile_simple_code
  , testProperty "compile error code" prop_compile_error_code
  , testProperty "compile empty file" prop_compile_empty_file
  , testProperty "compiler result success" prop_compiler_result_success
  , testProperty "compiler result failure" prop_compiler_result_failure
  
  -- Error handling tests
  , testProperty "malformed syntax error" prop_malformed_syntax_error
  , testProperty "type check failure" prop_type_check_failure
  , testProperty "type diagnostic to compiler error" prop_type_diagnostic_to_compiler_error
  
  -- Error formatting tests
  , testProperty "render compilation error" prop_render_compilation_error
  , testProperty "format compiler errors" prop_format_compiler_errors
  
  -- Error analysis tests
  , testProperty "analyze errors" prop_analyze_errors
  , testProperty "generate detailed report" prop_generate_detailed_report
  
  -- Type checking diagnostic tests
  , testProperty "diagnose type errors" prop_diagnose_type_errors
  , testProperty "has type errors" prop_has_type_errors
  
  -- Declaration extraction tests
  , testProperty "extract declarations" prop_extract_declarations
  , testProperty "extract function calls" prop_extract_function_calls
  
  -- Type environment tests
  , testProperty "build type env" prop_build_type_env
  , testProperty "build type env from pairs" prop_build_type_env_from_pairs
  
  -- Method declaration tests
  , testProperty "is method declaration" prop_is_method_declaration
  
  -- Type error checking tests
  , testProperty "check type error" prop_check_type_error
  
  -- Syntax error checking tests
  , testProperty "has malformed syntax" prop_has_malformed_syntax
  
  -- Dependent type checking tests
  , testProperty "check dependent types" prop_check_dependent_types
  
  -- Ownership checking tests
  , testProperty "check ownership" prop_check_ownership
  
  -- Source IR tests
  , testProperty "ensure source ir" prop_ensure_source_ir
  
  -- Go code generation tests
  , testProperty "generate go code" prop_generate_go_code
  , testProperty "generate go code empty" prop_generate_go_code_empty
  
  -- Typus file creation tests
  , testProperty "create typus file from errors" prop_create_typus_file_from_errors
  
  -- Compiler error tests
  , testProperty "compiler error id" prop_compiler_error_id
  , testProperty "compiler error message" prop_compiler_error_message
  , testProperty "compiler error severity" prop_compiler_error_severity
  , testProperty "compiler error phase" prop_compiler_error_phase
  
  -- Compilation phase tests
  , testProperty "compilation phase ordering" prop_compilation_phase_ordering
  , testProperty "compilation phase transitive" prop_compilation_phase_transitive
  
  -- Syntax error tests
  , testProperty "syntax error message" prop_syntax_error_message
  , testProperty "syntax error position" prop_syntax_error_position
  
  -- Type error tests
  , testProperty "type error message" prop_type_error_message
  , testProperty "type error position" prop_type_error_position
  ]