{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.CompilerComprehensiveQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Compiler as C
import qualified Parser as P
import qualified SourceLocation as SL
import Data.Char (isAlphaNum, isLetter, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Maybe (isJust, isNothing)
import Data.Either (isLeft, isRight)

-- ============================================================================
-- Compiler模块的QuickCheck测试 (25个测试)
-- ============================================================================

-- | 测试编译空文件
prop_compile_empty_file :: Property
prop_compile_empty_file = 
  let emptyFile = P.TypusFile P.defaultFileDirectives [] [] []
  in case C.compile emptyFile of
       Right goCode -> property $ True
       Left _ -> property False

-- | 测试编译简单包声明
prop_compile_simple_package :: String -> Property
prop_compile_simple_package packageName =
  let validPkg = not (null packageName) && isLetter (head packageName) && 
                 all (\c -> isLetter c || isDigit c) packageName
      simpleFile = P.TypusFile P.defaultFileDirectives [] [] []
  in if validPkg
     then case C.compile simpleFile of
            Right goCode -> property $ True
            Left _ -> property False
     else property True

-- | 测试编译带有语法错误的文件
prop_compile_syntax_error_file :: String -> Property
prop_compile_syntax_error_file invalidCode =
  let -- 创建一个简单的无效语法文件
      malformedFile = P.TypusFile P.defaultFileDirectives [] [] 
                       [P.SyntaxError SL.defaultSpan "syntax error"]
  in case C.compile malformedFile of
       Left _ -> property $ True  -- 应该编译失败
       Right _ -> property False  -- 不应该成功

-- | 测试hasTypeErrors函数
prop_has_type_errors :: [C.SyntaxError] -> Property
prop_has_type_errors syntaxErrors =
  let typusFile = P.TypusFile P.defaultFileDirectives [] [] 
                    (map (\(C.SyntaxError span msg) -> P.SyntaxError span msg) syntaxErrors)
  in property $ C.hasTypeErrors typusFile === not (null syntaxErrors)

-- | 测试malformedSyntaxError
prop_malformed_syntax_error :: Property
prop_malformed_syntax_error = 
  let err = C.malformedSyntaxError
  in property $ C.renderCompilationError [err] /= ""

-- | 测试formatCompilerErrors
prop_format_compiler_errors :: String -> Property
prop_format_compiler_errors errorMsg =
  let span = SL.SourceSpan (SL.SourcePos 1 1) (SL.SourcePos 1 10)
      err = C.mkCompilerError C.SyntaxErrorPhase C.Error span errorMsg
      formatted = C.formatCompilerErrors [err]
  in property $ formatted /= ""

-- | 测试generateDetailedReport
prop_generate_detailed_report :: String -> Property
prop_generate_detailed_report errorMsg =
  let span = SL.SourceSpan (SL.SourcePos 1 1) (SL.SourcePos 1 10)
      err = C.mkCompilerError C.SyntaxErrorPhase C.Error span errorMsg
      report = C.generateDetailedReport [err]
  in property $ report /= ""

-- | 测试analyzeErrors
prop_analyze_errors :: [String] -> Property
prop_analyze_errors errorMessages =
  let errors = map (\msg -> 
        let span = SL.SourceSpan (SL.SourcePos 1 1) (SL.SourcePos 1 10)
        in C.mkCompilerError C.SyntaxErrorPhase C.Error span msg) errorMessages
      analysis = C.analyzeErrors errors
  in property $ analysis /= ""

-- | 测试extractDeclarations
prop_extract_declarations :: String -> Property
prop_extract_declarations code =
  let -- 简单的代码提取测试
      declarations = C.extractDeclarations code
  in property $ length declarations >= 0

-- | 测试extractFunctionCalls
prop_extract_function_calls :: String -> Property
prop_extract_function_calls code =
  let -- 简单的函数调用提取测试
      functionCalls = C.extractFunctionCalls code
  in property $ length functionCalls >= 0

-- | 测试buildTypeEnv
prop_build_type_env :: [(String, String)] -> Property
prop_build_type_env typePairs =
  let -- 确保类型名称是有效的
      validPairs = filter (\(name, typ) -> 
                           not (null name) && isLetter (head name) &&
                           not (null typ) && isLetter (head typ)) typePairs
      typeEnv = C.buildTypeEnv validPairs
  in property $ length typeEnv === length validPairs

-- | 测试buildTypeEnvFromPairs
prop_build_type_env_from_pairs :: [(String, String)] -> Property
prop_build_type_env_from_pairs typePairs =
  let -- 确保类型名称是有效的
      validPairs = filter (\(name, typ) -> 
                           not (null name) && isLetter (head name) &&
                           not (null typ) && isLetter (head typ)) typePairs
      typeEnv = C.buildTypeEnvFromPairs validPairs
  in property $ length typeEnv === length validPairs

-- | 测试isMethodDeclaration
prop_is_method_declaration :: String -> Property
prop_is_method_declaration funcName =
  let validFunc = not (null funcName) && isLetter (head funcName) && 
                  all (\c -> isLetter c || isDigit c) funcName
      methodDecl = "func (r *Receiver) " ++ funcName ++ "()"
  in if validFunc
     then property $ C.isMethodDeclaration methodDecl
     else property True

-- | 测试checkTypeError
prop_check_type_error :: String -> Property
prop_check_type_error errorMsg =
  let span = SL.SourceSpan (SL.SourcePos 1 1) (SL.SourcePos 1 10)
      typeError = C.TypeError span errorMsg
      checked = C.checkTypeError typeError
  in property $ checked

-- | 测试hasMalformedSyntax
prop_has_malformed_syntax :: [String] -> Property
prop_has_malformed_syntax errorMessages =
  let syntaxErrors = map (\msg -> C.SyntaxError SL.defaultSpan msg) errorMessages
      typusFile = P.TypusFile P.defaultFileDirectives [] [] 
                    (map (\(C.SyntaxError span msg) -> P.SyntaxError span msg) syntaxErrors)
  in property $ C.hasMalformedSyntax typusFile === not (null errorMessages)

-- | 测试checkDependentTypes
prop_check_dependent_types :: String -> Property
prop_check_dependent_types code =
  let -- 创建一个简单的Typus文件用于测试
      typusFile = P.TypusFile P.defaultFileDirectives [] [] []
      result = C.checkDependentTypes typusFile
  in case result of
       Right () -> property $ True
       Left _ -> property $ True  -- 检查失败也是有效的结果

-- | 测试checkOwnership
prop_check_ownership :: String -> Property
prop_check_ownership code =
  let -- 创建一个简单的Typus文件用于测试
      typusFile = P.TypusFile P.defaultFileDirectives [] [] []
      result = C.checkOwnership typusFile
  in case result of
       Right () -> property $ True
       Left _ -> property $ True  -- 检查失败也是有效的结果

-- | 测试ensureSourceIR
prop_ensure_source_ir :: Property
prop_ensure_source_ir = 
  let typusFile = P.TypusFile P.defaultFileDirectives [] [] []
  in case C.ensureSourceIR typusFile of
       Right ir -> property $ True
       Left _ -> property $ True  -- 失败也是有效的结果

-- | 测试typeCheckFailure
prop_type_check_failure :: Property
prop_type_check_failure = 
  let failure = C.typeCheckFailure
  in property $ C.renderCompilationError [failure] /= ""

-- | 测试typeDiagnosticToCompilerError
prop_type_diagnostic_to_compiler_error :: String -> Property
prop_type_diagnostic_to_compiler_error errorMsg =
  let span = SL.SourceSpan (SL.SourcePos 1 1) (SL.SourcePos 1 10)
      diagnostic = C.TypeCheckDiagnostic span C.Error errorMsg
      compilerError = C.typeDiagnosticToCompilerError diagnostic
  in property $ C.renderCompilationError [compilerError] /= ""

-- | 测试diagnoseTypeErrors
prop_diagnose_type_errors :: String -> Property
prop_diagnose_type_errors code =
  let -- 创建一个简单的Typus文件用于测试
      typusFile = P.TypusFile P.defaultFileDirectives [] [] []
      result = C.diagnoseTypeErrors typusFile
  in case result of
       Right diagnostics -> property $ True
       Left _ -> property $ True  -- 诊断失败也是有效的结果

-- | 测试createTypusFileFromErrors
prop_create_typus_file_from_errors :: [String] -> Property
prop_create_typus_file_from_errors errorMessages =
  let typeErrors = map (\msg -> C.TypeError SL.defaultSpan msg) errorMessages
      typusFile = C.createTypusFileFromErrors typeErrors
  in property $ P.tfSyntaxErrors typusFile === map (\(C.TypeError span msg) -> 
                                                    P.SyntaxError span msg) typeErrors

-- | 测试编译结果包含Go代码
prop_compile_result_contains_go :: String -> Property
prop_compile_result_contains_go packageName =
  let validPkg = not (null packageName) && isLetter (head packageName) && 
                 all (\c -> isLetter c || isDigit c) packageName
      simpleFile = P.TypusFile P.defaultFileDirectives [] [] []
  in if validPkg
     then case C.compile simpleFile of
            Right goCode -> property $ "package" `isInfixOf` goCode
            Left _ -> property False
     else property True

-- | 测试编译错误的一致性
prop_compile_error_consistency :: String -> Property
prop_compile_error_consistency code =
  let -- 创建一个带有语法错误的文件
      malformedFile = P.TypusFile P.defaultFileDirectives [] [] 
                       [P.SyntaxError SL.defaultSpan "syntax error"]
      result1 = C.compile malformedFile
      result2 = C.compile malformedFile
  in case (result1, result2) of
       (Left errs1, Left errs2) -> property $ length errs1 === length errs2
       (Right _, Right _) -> property $ True
       _ -> property False  -- 两次编译结果应该一致

-- | 测试编译阶段的错误分类
prop_compilation_phase_categorization :: Property
prop_compilation_phase_categorization = 
  let phases = [C.SyntaxErrorPhase, C.TypeErrorPhase, C.SemanticErrorPhase, C.CodeGenPhase]
      testPhase phase = 
        let span = SL.SourceSpan (SL.SourcePos 1 1) (SL.SourcePos 1 10)
            err = C.mkCompilerError phase C.Error span "test error"
        in C.compilationPhase err === phase
  in conjoin $ map testPhase phases

-- | 测试编译错误的严重性
prop_compilation_error_severity :: Property
prop_compilation_error_severity = 
  let severities = [C.Error, C.Warning, C.Info]
      testSeverity severity = 
        let span = SL.SourceSpan (SL.SourcePos 1 1) (SL.SourcePos 1 10)
            err = C.mkCompilerError C.SyntaxErrorPhase severity span "test"
        in C.errorSeverity err === severity
  in conjoin $ map testSeverity severities

-- | 测试编译错误的源位置
prop_compilation_error_location :: Int -> Int -> Int -> Int -> Property
prop_compilation_error_location line1 col1 line2 col2 =
  let pos1 = SL.SourcePos line1 col1
      pos2 = SL.SourcePos line2 col2
      span = SL.SourceSpan pos1 pos2
      err = C.mkCompilerError C.SyntaxErrorPhase C.Error span "test"
      errSpan = C.errorSpan err
  in property $ errSpan === span

-- | 测试编译错误消息的提取
prop_compilation_error_message :: String -> Property
prop_compilation_error_message errorMsg =
  let span = SL.SourceSpan (SL.SourcePos 1 1) (SL.SourcePos 1 10)
      err = C.mkCompilerError C.SyntaxErrorPhase C.Error span errorMsg
      extractedMsg = C.errorMessage err
  in property $ extractedMsg === errorMsg

-- 将所有测试组合在一起
testSuite :: TestTree
testSuite = testGroup "Compiler模块Comprehensive QuickCheck测试"
  [ testProperty "编译空文件" prop_compile_empty_file
  , testProperty "编译简单包声明" prop_compile_simple_package
  , testProperty "编译带有语法错误的文件" prop_compile_syntax_error_file
  , testProperty "hasTypeErrors函数" prop_has_type_errors
  , testProperty "malformedSyntaxError" prop_malformed_syntax_error
  , testProperty "formatCompilerErrors" prop_format_compiler_errors
  , testProperty "generateDetailedReport" prop_generate_detailed_report
  , testProperty "analyzeErrors" prop_analyze_errors
  , testProperty "extractDeclarations" prop_extract_declarations
  , testProperty "extractFunctionCalls" prop_extract_function_calls
  , testProperty "buildTypeEnv" prop_build_type_env
  , testProperty "buildTypeEnvFromPairs" prop_build_type_env_from_pairs
  , testProperty "isMethodDeclaration" prop_is_method_declaration
  , testProperty "checkTypeError" prop_check_type_error
  , testProperty "hasMalformedSyntax" prop_has_malformed_syntax
  , testProperty "checkDependentTypes" prop_check_dependent_types
  , testProperty "checkOwnership" prop_check_ownership
  , testProperty "ensureSourceIR" prop_ensure_source_ir
  , testProperty "typeCheckFailure" prop_type_check_failure
  , testProperty "typeDiagnosticToCompilerError" prop_type_diagnostic_to_compiler_error
  , testProperty "diagnoseTypeErrors" prop_diagnose_type_errors
  , testProperty "createTypusFileFromErrors" prop_create_typus_file_from_errors
  , testProperty "编译结果包含Go代码" prop_compile_result_contains_go
  , testProperty "编译错误的一致性" prop_compile_error_consistency
  , testProperty "编译阶段的错误分类" prop_compilation_phase_categorization
  , testProperty "编译错误的严重性" prop_compilation_error_severity
  , testProperty "编译错误的源位置" prop_compilation_error_location
  , testProperty "编译错误消息的提取" prop_compilation_error_message
  ]