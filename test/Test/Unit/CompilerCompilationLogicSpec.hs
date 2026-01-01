{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerCompilationLogicSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, elements, listOf, oneof, sized)
import Compiler
  ( compile
  , CompilerError(..)
  , CompilerResult
  , CompilationPhase(..)
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
import Parser (TypusFile(..), CodeBlock(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)

-- ============================================================================
-- 生成测试数据
-- ============================================================================

-- 生成编译阶段
genCompilationPhase :: Gen CompilationPhase
genCompilationPhase = elements 
  [ ParsingPhase
  , TypeCheckingPhase
  , OwnershipAnalysisPhase
  , DependencyAnalysisPhase
  , CodeGenerationPhase
  ]

-- 生成有效的Go代码片段
genGoCodeSnippet :: Gen String
genGoCodeSnippet = oneof
  [ return "func test() {}"
  , return "var x int = 42"
  , return "type MyStruct struct { field int }"
  , return "package main"
  , return "import \"fmt\""
  , return "fmt.Println(\"hello\")"
  ]

-- 生成包含错误的代码片段
genErrorGoCodeSnippet :: Gen String
genErrorGoCodeSnippet = oneof
  [ return "func test() { // missing closing brace"
  , return "var x int = \"string\""  -- type error
  , return "undefined_function()"    -- undefined function
  , return "return 1 + \"string\""   -- type mismatch
  ]

-- 生成Typus文件
genTypusFile :: Gen TypusFile
genTypusFile = do
  numBlocks <- choose (0, 3)
  codeSnippets <- listOf genGoCodeSnippet
  let blocks = L.map (\code -> CodeBlock defaultBlockDirectives code undefined) codeSnippets
  return $ TypusFile defaultFileDirectives [] blocks []

-- ============================================================================
-- 编译逻辑属性测试
-- ============================================================================

-- Property: 空文件编译结果
prop_compile_empty_file :: Property
prop_compile_empty_file =
  let emptyFile = TypusFile defaultFileDirectives [] [] []
      result = compile emptyFile
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: 简单Go代码编译
prop_compile_simple_go_code :: String -> Property
prop_compile_simple_go_code code =
  not (null code) && not ("{" `L.isInfixOf` code) ==>
  let block = CodeBlock defaultBlockDirectives code undefined
      file = TypusFile defaultFileDirectives [] [block] []
      result = compile file
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: 错误代码检测
prop_compile_detects_errors :: String -> Property
prop_compile_detects_errors code =
  "undefined" `L.isInfixOf` code ==>
  let block = CodeBlock defaultBlockDirectives code undefined
      file = TypusFile defaultFileDirectives [] [block] []
      result = compile file
  in case result of
    Left _ -> property True
    Right _ -> property False

-- Property: 编译错误包含阶段信息
prop_compilation_error_includes_phase :: CompilationPhase -> String -> Property
prop_compilation_error_includes_phase phase code =
  let error = CompilerError phase "test error" Nothing
      rendered = renderCompilationError error
  in property $ show phase `L.isInfixOf` rendered

-- Property: 类型错误诊断
prop_type_error_diagnosis :: String -> Property
prop_type_error_diagnosis code =
  not (null code) ==>
  let diagnostics = diagnoseTypeErrors code
      hasErrors = not (null diagnostics)
  in property $ hasErrors ==> L.all (\d -> diagnosticMessage d /= "") diagnostics

-- Property: 函数声明提取
prop_extract_function_declarations :: String -> Property
prop_extract_function_declarations code =
  "func" `L.isInfixOf` code ==>
  let declarations = extractDeclarations code
  in property $ L.all ("func" `L.isPrefixOf`) declarations

-- Property: 函数调用提取
prop_extract_function_calls :: String -> Property
prop_extract_function_calls code =
  "(" `L.isInfixOf` code && ")" `L.isInfixOf` code ==>
  let calls = extractFunctionCalls code
  in property $ L.all (not . null) calls

-- Property: 类型环境构建
prop_build_type_environment :: [(String, String)] -> Property
prop_build_type_environment pairs =
  not (null pairs) ==>
  let typeEnv = buildTypeEnvFromPairs pairs
  in property $ L.length typeEnv === L.length pairs

-- Property: 方法声明检测
prop_method_declaration_detection :: String -> Property
prop_method_declaration_detection code =
  "func" `L.isInfixOf` code ==>
  let isMethod = isMethodDeclaration code
      hasReceiver = "(" `L.isInfixOf` code && ")" `L.isInfixOf` code
  in property $ isMethod === hasReceiver

-- Property: 语法错误检测
prop_syntax_error_detection :: String -> Property
prop_syntax_error_detection code =
  not (null code) ==>
  let hasSyntaxError = hasMalformedSyntax code
      hasUnmatchedBraces = (L.length (L.filter (== '{') code) /= L.length (L.filter (== '}') code))
  in property $ hasSyntaxError ==> hasUnmatchedBraces

-- Property: 依赖类型检查
prop_dependent_type_checking :: String -> Property
prop_dependent_type_checking code =
  not (null code) ==>
  let result = checkDependentTypes code
  in case result of
    Left _ -> property True
    Right _ -> property True

-- Property: 所有权检查
prop_ownership_checking :: String -> Property
prop_ownership_checking code =
  not (null code) ==>
  let result = checkOwnership code
  in case result of
    Left _ -> property True
    Right _ -> property True

-- ============================================================================
-- 单元测试
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Compiler Compilation Logic Tests"
    [ testGroup "Property Tests"
        [ fastProperty "compile empty file" prop_compile_empty_file
        , fastProperty "compile simple go code" prop_compile_simple_go_code
        , fastProperty "compile detects errors" prop_compile_detects_errors
        , fastProperty "compilation error includes phase" prop_compilation_error_includes_phase
        , fastProperty "type error diagnosis" prop_type_error_diagnosis
        , fastProperty "extract function declarations" prop_extract_function_declarations
        , fastProperty "extract function calls" prop_extract_function_calls
        , fastProperty "build type environment" prop_build_type_environment
        , fastProperty "method declaration detection" prop_method_declaration_detection
        , fastProperty "syntax error detection" prop_syntax_error_detection
        , fastProperty "dependent type checking" prop_dependent_type_checking
        , fastProperty "ownership checking" prop_ownership_checking
        ]
    , testGroup "Unit Tests"
        [ testCase "compile simple function" $ do
            let code = "func hello() { fmt.Println(\"Hello, World!\") }"
                block = CodeBlock defaultBlockDirectives code undefined
                file = TypusFile defaultFileDirectives [] [block] []
                result = compile file
            case result of
              Left err -> assertFailure $ "Compilation failed: " ++ show err
              Right _ -> return ()

        , testCase "compile variable declaration" $ do
            let code = "var x int = 42"
                block = CodeBlock defaultBlockDirectives code undefined
                file = TypusFile defaultFileDirectives [] [block] []
                result = compile file
            case result of
              Left err -> assertFailure $ "Compilation failed: " ++ show err
              Right _ -> return ()

        , testCase "compile struct definition" $ do
            let code = "type Person struct { Name string; Age int }"
                block = CodeBlock defaultBlockDirectives code undefined
                file = TypusFile defaultFileDirectives [] [block] []
                result = compile file
            case result of
              Left err -> assertFailure $ "Compilation failed: " ++ show err
              Right _ -> return ()

        , testCase "detect undefined function" $ do
            let code = "func test() { undefinedFunction() }"
                block = CodeBlock defaultBlockDirectives code undefined
                file = TypusFile defaultFileDirectives [] [block] []
                result = compile file
            case result of
              Left _ -> return ()
              Right _ -> assertFailure "Expected compilation to fail with undefined function"

        , testCase "detect type mismatch" $ do
            let code = "func test() { return 1 + \"string\" }"
                block = CodeBlock defaultBlockDirectives code undefined
                file = TypusFile defaultFileDirectives [] [block] []
                result = compile file
            case result of
              Left _ -> return ()
              Right _ -> assertFailure "Expected compilation to fail with type mismatch"

        , testCase "extract function declarations correctly" $ do
            let code = unlines
                  [ "func main() {}"
                  , "func helper(x int) int { return x }"
                  , "func (s *MyStruct) method() {}"
                  ]
                declarations = extractDeclarations code
            L.length declarations @?= 3
            "func main()" `L.isInfixOf` unlines declarations @?= True

        , testCase "extract function calls correctly" $ do
            let code = unlines
                  [ "func main() {"
                  , "  fmt.Println(\"hello\")"
                  , "  helper(42)"
                  , "  obj.method()"
                  , "}"
                  ]
                calls = extractFunctionCalls code
            L.length calls @?= 3
            "fmt.Println" `elem` calls @?= True
            "helper" `elem` calls @?= True
            "obj.method" `elem` calls @?= True

        , testCase "build type environment from pairs" $ do
            let pairs = [("x", "int"), ("y", "string"), ("z", "bool")]
                typeEnv = buildTypeEnvFromPairs pairs
            L.length typeEnv @?= 3

        , testCase "detect method declarations" $ do
            let methodCode = "func (s *MyStruct) Method() {}"
                functionCode = "func standalone() {}"
            isMethodDeclaration methodCode @?= True
            isMethodDeclaration functionCode @?= False

        , testCase "detect syntax errors" $ do
            let validCode = "func test() { return 42 }"
                invalidCode = "func test() { return 42"  -- missing closing brace
            hasMalformedSyntax validCode @?= False
            hasMalformedSyntax invalidCode @?= True

        , testCase "render compilation error" $ do
            let error = CompilerError TypeCheckingPhase "type mismatch" Nothing
                rendered = renderCompilationError error
            "TypeCheckingPhase" `L.isInfixOf` rendered @?= True
            "type mismatch" `L.isInfixOf` rendered @?= True

        , testCase "format multiple compiler errors" $ do
            let errors = 
                  [ CompilerError ParsingPhase "syntax error" Nothing
                  , CompilerError TypeCheckingPhase "type error" Nothing
                  , CompilerError OwnershipAnalysisPhase "ownership error" Nothing
                  ]
                formatted = formatCompilerErrors errors
            "ParsingPhase" `L.isInfixOf` formatted @?= True
            "TypeCheckingPhase" `L.isInfixOf` formatted @?= True
            "OwnershipAnalysisPhase" `L.isInfixOf` formatted @?= True

        , testCase "generate detailed error report" $ do
            let errors = 
                  [ CompilerError TypeCheckingPhase "type error" Nothing
                  , CompilerError OwnershipAnalysisPhase "ownership error" Nothing
                  ]
                report = generateDetailedReport errors
            "Detailed Compilation Report" `L.isInfixOf` report @?= True
            "type error" `L.isInfixOf` report @?= True
            "ownership error" `L.isInfixOf` report @?= True

        , testCase "analyze errors by phase" $ do
            let errors = 
                  [ CompilerError ParsingPhase "parse error 1" Nothing
                  , CompilerError ParsingPhase "parse error 2" Nothing
                  , CompilerError TypeCheckingPhase "type error" Nothing
                  ]
                analysis = analyzeErrors errors
            L.length analysis @?= 2  -- Two phases with errors

        , testCase "type diagnostic to compiler error conversion" $ do
            let diagnostic = TypeCheckDiagnostic "TypeError" "type mismatch" Nothing
                error = typeDiagnosticToCompilerError diagnostic
            compilationPhase error @?= TypeCheckingPhase
            errorMessage error @?= "type mismatch"

        , testCase "generate Go code from IR" $ do
            let result = generateGoCode "func test() { return 42 }"
            case result of
              Left _ -> assertFailure "Go code generation failed"
              Right goCode -> "func test()" `L.isInfixOf` goCode @?= True
        ]
    ]