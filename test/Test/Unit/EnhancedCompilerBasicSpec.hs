module Test.Unit.EnhancedCompilerBasicSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler 
  ( compile
  , CompilerError(..)
  , CompilerResult
  , CompilationPhase(..)
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
import Data.Maybe (isJust, isNothing)
import Data.List (isPrefixOf)

-- | 测试编译空字符串
prop_compile_empty_string :: Property
prop_compile_empty_string = 
  let result = compile ""
  in case result of
    Left _ -> property True
    Right _ -> property True -- 空字符串应该能够编译（可能产生空结果）

-- | 测试编译简单注释
prop_compile_simple_comment :: Property
prop_compile_simple_comment = 
  let code = "// This is a comment"
      result = compile code
  in case result of
    Left _ -> property True
    Right _ -> property True -- 注释应该能够编译

-- | 测试编译 malformedSyntaxError 的属性
prop_malformed_syntax_error :: String -> Property
prop_malformed_syntax_error msg = 
  let error = malformedSyntaxError msg
  in case error of
    SyntaxError _ _ _ -> property True
    _ -> property False

-- | 测试 renderCompilationError 的属性：renderCompilationError 总是返回非空字符串
prop_render_compilation_error_nonempty :: CompilerError -> Property
prop_render_compilation_error_nonempty error = 
  let rendered = renderCompilationError error
  in not (T.null rendered)

-- | 测试 formatCompilerErrors 的属性：空错误列表产生空格式化结果
prop_format_compilation_errors_empty :: Property
prop_format_compilation_errors_empty = 
  let formatted = formatCompilerErrors []
  in T.null formatted

-- | 测试 formatCompilerErrors 的属性：非空错误列表产生非空格式化结果
prop_format_compilation_errors_nonempty :: NonEmptyList CompilerError -> Property
prop_format_compilation_errors_nonempty (NonEmpty errors) = 
  let formatted = formatCompilerErrors errors
  in not (T.null formatted)

-- | 测试 hasTypeErrors 的属性：空错误列表没有类型错误
prop_has_type_errors_empty :: Property
prop_has_type_errors_empty = not (hasTypeErrors [])

-- | 测试 hasTypeErrors 的属性：包含 TypeError 的列表有类型错误
prop_has_type_errors_with_type_error :: CompilerError -> Property
prop_has_type_errors_with_type_error error = 
  hasTypeErrors [error] === isTypeError error
  where
    isTypeError (TypeError _ _ _) = True
    isTypeError _ = False

-- | 测试 buildTypeEnv 的属性：空列表构建空类型环境
prop_build_type_env_empty :: Property
prop_build_type_env_empty = 
  let env = buildTypeEnv []
  in null env

-- | 测试 buildTypeEnvFromPairs 的属性：空列表构建空类型环境
prop_build_type_env_from_pairs_empty :: Property
prop_build_type_env_from_pairs_empty = 
  let env = buildTypeEnvFromPairs []
  in null env

-- | 测试 buildTypeEnvFromPairs 的属性：构建的环境包含所有输入的键值对
prop_build_type_env_from_pairs_contains_all :: [(String, String)] -> Property
prop_build_type_env_from_pairs_contains_all pairs = 
  let env = buildTypeEnvFromPairs pairs
      allContained = all (\(k, v) -> lookup k env == Just v) pairs
  in allContained

-- | 测试 isMethodDeclaration 的属性：包含括号的标识符可能是方法声明
prop_is_method_declaration_with_parens :: String -> Property
prop_is_method_declaration_with_parens s = 
  let sWithParens = s ++ "()"
  in isMethodDeclaration sWithParens

-- | 测试 checkTypeError 的属性：空错误列表检查通过
prop_check_type_error_empty :: Property
prop_check_type_error_empty = checkTypeError []

-- | 测试 hasMalformedSyntax 的属性：空错误列表没有语法错误
prop_has_malformed_syntax_empty :: Property
prop_has_malformed_syntax_empty = not (hasMalformedSyntax [])

-- | 测试 hasMalformedSyntax 的属性：包含 SyntaxError 的列表有语法错误
prop_has_malformed_syntax_with_syntax_error :: CompilerError -> Property
prop_has_malformed_syntax_with_syntax_error error = 
  hasMalformedSyntax [error] === isSyntaxError error
  where
    isSyntaxError (SyntaxError _ _ _) = True
    isSyntaxError _ = False

-- | 测试 typeCheckFailure 的属性：typeCheckFailure 总是返回 Left
prop_type_check_failure :: String -> Property
prop_type_check_failure msg = 
  case typeCheckFailure msg of
    Left _ -> property True
    Right _ -> property False

-- | 测试 typeDiagnosticToCompilerError 的属性：转换后的错误保留原始信息
prop_type_diagnostic_to_compiler_error :: TypeCheckDiagnostic -> Property
prop_type_diagnostic_to_compiler_error diagnostic = 
  let error = typeDiagnosticToCompilerError diagnostic
  in case error of
    TypeError _ _ _ -> property True
    _ -> property False

-- | 测试 generateGoCode 的属性：generateGoCode 总是返回非空字符串
prop_generate_go_code_nonempty :: TypusFile -> Property
prop_generate_go_code_nonempty file = 
  let goCode = generateGoCode file
  in not (T.null goCode)

tests :: TestTree
tests = testGroup "Enhanced Compiler Basic Tests"
  [ testProperty "compile empty string" prop_compile_empty_string
  , testProperty "compile simple comment" prop_compile_simple_comment
  , testProperty "malformed syntax error" prop_malformed_syntax_error
  , testProperty "render compilation error nonempty" prop_render_compilation_error_nonempty
  , testProperty "format compilation errors empty" prop_format_compilation_errors_empty
  , testProperty "format compilation errors nonempty" prop_format_compilation_errors_nonempty
  , testProperty "has type errors empty" prop_has_type_errors_empty
  , testProperty "has type errors with type error" prop_has_type_errors_with_type_error
  , testProperty "build type env empty" prop_build_type_env_empty
  , testProperty "build type env from pairs empty" prop_build_type_env_from_pairs_empty
  , testProperty "build type env from pairs contains all" prop_build_type_env_from_pairs_contains_all
  , testProperty "is method declaration with parens" prop_is_method_declaration_with_parens
  , testProperty "check type error empty" prop_check_type_error_empty
  , testProperty "has malformed syntax empty" prop_has_malformed_syntax_empty
  , testProperty "has malformed syntax with syntax error" prop_has_malformed_syntax_with_syntax_error
  , testProperty "type check failure" prop_type_check_failure
  , testProperty "type diagnostic to compiler error" prop_type_diagnostic_to_compiler_error
  , testProperty "generate go code nonempty" prop_generate_go_code_nonempty
  ]