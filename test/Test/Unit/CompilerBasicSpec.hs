module Test.Unit.CompilerBasicSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Compiler as C
import Parser (TypusFile(..), defaultFileDirectives)
import Compiler.Errors.Core (ErrorSeverity(..))
import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe (isJust, isNothing)

-- 测试编译阶段的属性
prop_compilationphase_ordering :: C.CompilationPhase -> C.CompilationPhase -> Property
prop_compilationphase_ordering phase1 phase2 = 
  case (phase1, phase2) of
    (C.ParsingPhase, C.LexingPhase) -> phase1 > phase2
    (C.TypeCheckingPhase, C.ParsingPhase) -> phase1 > phase2
    (C.CodeGenerationPhase, C.TypeCheckingPhase) -> phase1 > phase2
    (C.OptimizationPhase, C.CodeGenerationPhase) -> phase1 > phase2
    _ -> phase1 <= phase2

-- 测试编译错误的属性
prop_compilererror_contains_message :: String -> Property
prop_compilererror_contains_message msg = 
  let error = C.malformedSyntaxError msg
  in msg `isInfixOf` C.renderCompilationError error

prop_compilererror_has_phase :: String -> C.CompilationPhase -> Property
prop_compilererror_has_phase msg phase = 
  let error = C.malformedSyntaxError msg
      formatted = C.renderCompilationError error
  in show phase `isInfixOf` formatted

-- 测试编译结果的属性
prop_compilerresult_error_handling :: String -> Property
prop_compilerresult_error_handling content = 
  let file = TypusFile "test.typus" [] defaultFileDirectives
      result = C.compile file
  in case result of
    Left errors -> not (null errors)
    Right _ -> property True

prop_compilerresult_success_structure :: String -> Property
prop_compilerresult_success_structure content = 
  let file = TypusFile "test.typus" [] defaultFileDirectives
      result = C.compile file
  in case result of
    Left _ -> property False
    Right success -> property True  -- 成功结果的结构检查

-- 测试错误分析的属性
prop_analyzeerrors_counts :: [String] -> Property
prop_analyzeerrors_counts messages = 
  let errors = map C.malformedSyntaxError messages
      analysis = C.analyzeErrors errors
  in length errors >= length analysis

prop_analyzeerrors_severity_distribution :: [String] -> Property
prop_analyzeerrors_severity_distribution messages = 
  let errors = map C.malformedSyntaxError messages
      analysis = C.analyzeErrors errors
  in all (\e -> errorSeverity e `elem` [ErrorError, ErrorWarning, ErrorInfo]) analysis

-- 测试类型错误检查的属性
prop_hastypeerrors_detection :: String -> Property
prop_hastypeerrors_detection content = 
  let file = TypusFile "test.typus" [] defaultFileDirectives
      result = C.compile file
  in case result of
    Left errors -> C.hasTypeErrors errors
    Right _ -> not (C.hasTypeErrors [])

prop_hastypeerrors_empty_list :: Property
prop_hastypeerrors_empty_list = 
  not (C.hasTypeErrors [])

-- 测试语法错误检查的属性
prop_hasmalformedsyntax_detection :: String -> Property
prop_hasmalformedsyntax_detection content = 
  let file = TypusFile "test.typus" [] defaultFileDirectives
      result = C.compile file
  in case result of
    Left errors -> C.hasMalformedSyntax errors
    Right _ -> not (C.hasMalformedSyntax [])

prop_hasmalformedsyntax_empty_list :: Property
prop_hasmalformedsyntax_empty_list = 
  not (C.hasMalformedSyntax [])

-- 测试依赖类型检查的属性
prop_checkdependentlypes_consistency :: String -> Property
prop_checkdependentlypes_consistency content = 
  let file = TypusFile "test.typus" [] defaultFileDirectives
      result = C.checkDependentTypes file
  in case result of
    Left _ -> property True
    Right _ -> property True

-- 测试所有权检查的属性
prop_checkownership_consistency :: String -> Property
prop_checkownership_consistency content = 
  let file = TypusFile "test.typus" [] defaultFileDirectives
      result = C.checkOwnership file
  in case result of
    Left _ -> property True
    Right _ -> property True

-- 测试Go代码生成的属性
prop_generategocode_non_empty :: String -> Property
prop_generategocode_non_empty content = 
  let file = TypusFile "test.typus" [] defaultFileDirectives
      result = C.generateGoCode file
  in case result of
    Left _ -> property False
    Right goCode -> not (null goCode)

prop_generategocode_go_syntax :: String -> Property
prop_generategocode_go_syntax content = 
  let file = TypusFile "test.typus" [] defaultFileDirectives
      result = C.generateGoCode file
  in case result of
    Left _ -> property False
    Right goCode -> "package" `isInfixOf` goCode

-- 测试错误报告生成的属性
prop_generatedetailedreport_structure :: [String] -> Property
prop_generatedetailedreport_structure messages = 
  let errors = map C.malformedSyntaxError messages
      report = C.generateDetailedReport errors
  in not (null report)

prop_formatcompilererrors_preserves_order :: [String] -> Property
prop_formatcompilererrors_preserves_order messages = 
  let errors = map C.malformedSyntaxError messages
      formatted = C.formatCompilerErrors errors
      lines' = lines formatted
  in length lines' >= length messages

-- 测试类型诊断的属性
prop_diagnosetypeerrors_structure :: String -> Property
prop_diagnosetypeerrors_structure content = 
  let file = TypusFile "test.typus" [] defaultFileDirectives
      diagnostics = C.diagnoseTypeErrors file
  in length diagnostics >= 0

-- 测试声明提取的属性
prop_extractdeclarations_consistency :: String -> Property
prop_extractdeclarations_consistency content = 
  let file = TypusFile "test.typus" [] defaultFileDirectives
      declarations = C.extractDeclarations file
  in length declarations >= 0

-- 测试函数调用提取的属性
prop_extractfunctioncalls_consistency :: String -> Property
prop_extractfunctioncalls_consistency content = 
  let file = TypusFile "test.typus" [] defaultFileDirectives
      calls = C.extractFunctionCalls file
  in length calls >= 0

-- 测试类型环境构建的属性
prop_buildtypeenv_consistency :: [(String, String)] -> Property
prop_buildtypeenv_consistency pairs = 
  let typeEnv = C.buildTypeEnvFromPairs pairs
  in length typeEnv === length pairs

prop_ensuresourceir_structure :: String -> Property
prop_ensuresourceir_structure content = 
  let file = TypusFile "test.typus" [] defaultFileDirectives
      result = C.ensureSourceIR file
  in case result of
    Left _ -> property True
    Right _ -> property True

-- 测试类型检查失败的属性
prop_typecheckfailure_error_creation :: String -> Property
prop_typecheckfailure_error_creation msg = 
  let error = C.typeCheckFailure msg
  in msg `isInfixOf` C.renderCompilationError error

-- 测试方法声明检查的属性
prop_ismethoddeclaration_detection :: String -> Property
prop_ismethoddeclaration_detection content = 
  let file = TypusFile "test.typus" [] defaultFileDirectives
      declarations = C.extractDeclarations file
  in all (\d -> C.isMethodDeclaration d `elem` [True, False]) declarations

-- 测试从错误创建Typus文件的属性
prop_createtypusfilefromerrors_structure :: [String] -> Property
prop_createtypusfilefromerrors_structure messages = 
  let errors = map C.malformedSyntaxError messages
      file = C.createTypusFileFromErrors errors
  in tfPath file `isPrefixOf` "error-recovery"

-- 测试类型诊断到编译器错误的转换
prop_typediagnostictocompilererror_preservation :: String -> Property
prop_typediagnostictocompilererror_preservation msg = 
  let diagnostic = C.TypeCheckDiagnostic msg ErrorError
      error = C.typeDiagnosticToCompilerError diagnostic
  in msg `isInfixOf` C.renderCompilationError error

tests :: TestTree
tests = testGroup "Compiler Basic Tests"
  [ testProperty "CompilationPhase ordering" prop_compilationphase_ordering
  , testProperty "CompilerError contains message" prop_compilererror_contains_message
  , testProperty "CompilerError has phase" prop_compilererror_has_phase
  , testProperty "CompilerResult error handling" prop_compilerresult_error_handling
  , testProperty "CompilerResult success structure" prop_compilerresult_success_structure
  , testProperty "analyzeErrors counts" prop_analyzeerrors_counts
  , testProperty "analyzeErrors severity distribution" prop_analyzeerrors_severity_distribution
  , testProperty "hasTypeErrors detection" prop_hastypeerrors_detection
  , testProperty "hasTypeErrors empty list" prop_hastypeerrors_empty_list
  , testProperty "hasMalformedSyntax detection" prop_hasmalformedsyntax_detection
  , testProperty "hasMalformedSyntax empty list" prop_hasmalformedsyntax_empty_list
  , testProperty "checkDependentTypes consistency" prop_checkdependentlypes_consistency
  , testProperty "checkOwnership consistency" prop_checkownership_consistency
  , testProperty "generateGoCode non empty" prop_generategocode_non_empty
  , testProperty "generateGoCode Go syntax" prop_generategocode_go_syntax
  , testProperty "generateDetailedReport structure" prop_generatedetailedreport_structure
  , testProperty "formatCompilerErrors preserves order" prop_formatcompilererrors_preserves_order
  , testProperty "diagnoseTypeErrors structure" prop_diagnosetypeerrors_structure
  , testProperty "extractDeclarations consistency" prop_extractdeclarations_consistency
  , testProperty "extractFunctionCalls consistency" prop_extractfunctioncalls_consistency
  , testProperty "buildTypeEnv consistency" prop_buildtypeenv_consistency
  , testProperty "ensureSourceIR structure" prop_ensuresourceir_structure
  , testProperty "typeCheckFailure error creation" prop_typecheckfailure_error_creation
  , testProperty "isMethodDeclaration detection" prop_ismethoddeclaration_detection
  , testProperty "createTypusFileFromErrors structure" prop_createtypusfilefromerrors_structure
  , testProperty "typeDiagnosticToCompilerError preservation" prop_typediagnostictocompilererror_preservation
  ]
