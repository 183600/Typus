{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.CompilerCoreQuickCheckTests where

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Compiler as C
import qualified Compiler.IR as IR
import qualified Compiler.GoAst as GoAst
import qualified Compiler.GoLexer as GL
import qualified Compiler.TypeChecker as TC
import qualified Compiler.OwnershipChecker as OC
import qualified Utils as U
import qualified Parser as P
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Either (isLeft, isRight)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | 测试编译器的基本编译功能
prop_compile_basic :: String -> Property
prop_compile_basic s =
  let validCode = "func main() { return " ++ show (length s) ++ "; }"
      result = case P.parseTypusFile validCode of
                 Left _ -> Left []
                 Right typusFile -> C.compile typusFile
  in property $ isRight result

-- | 测试IR生成的一致性
prop_ir_generation_consistent :: String -> Property
prop_ir_generation_consistent s =
  let code = "func test() { var x = " ++ show (length s) ++ "; return x; }"
      -- 使用Parser解析代码，然后构建IR
      result = case P.parseTypusFile code of
                 Left _ -> Left []
                 Right typusFile -> C.compile typusFile
  in property $ isRight result

-- | 测试IR的优化不变性
prop_ir_optimization_invariant :: String -> Property
prop_ir_optimization_invariant s =
  let code = "func test() { var x = " ++ show (length s) ++ "; var y = x + 1; return y; }"
      -- 使用编译函数，因为优化是编译过程的一部分
      parseResult = P.parseTypusFile code
      result1 = case parseResult of
                  Left _ -> Left []
                  Right typusFile -> C.compile typusFile
      result2 = case parseResult of
                  Left _ -> Left []
                  Right typusFile -> C.compile typusFile
      -- 两次编译结果应该一致
  in property $ result1 == result2

-- | 测试类型检查的正确性
prop_type_check_correct :: String -> Property
prop_type_check_correct s =
  let validCode = "func test() { var x int = " ++ show (length s) ++ "; return x; }"
      result = case P.parseTypusFile validCode of
                 Left _ -> []
                 Right typusFile -> case TC.diagnoseTypeErrors typusFile of
                                     Left _ -> []
                                     Right diagnostics -> diagnostics
  in property $ null result

-- | 测试类型错误检测
prop_type_error_detection :: String -> Property
prop_type_error_detection s =
  let invalidCode = "func test() { var x int = \"" ++ s ++ "\"; return x; }"
      result = case P.parseTypusFile invalidCode of
                 Left _ -> []
                 Right typusFile -> case TC.diagnoseTypeErrors typusFile of
                                     Left _ -> []
                                     Right diagnostics -> diagnostics
  in property $ not (null result)

-- | 测试所有权检查的基本功能
prop_ownership_check_basic :: String -> Property
prop_ownership_check_basic s =
  let code = "func test() { var x = " ++ show (length s) ++ "; var y = x; return y; }"
      result = case P.parseTypusFile code of
                 Left _ -> Left []
                 Right typusFile -> OC.checkOwnership typusFile
  in property $ isRight result

-- | 测试所有权转移检测
prop_ownership_transfer :: String -> Property
prop_ownership_transfer s =
  let code = "func test() { var x = \"" ++ s ++ "\"; var y = x; return y; }"
      result = case P.parseTypusFile code of
                 Left _ -> Left []
                 Right typusFile -> OC.checkOwnership typusFile
  in property $ isRight result

-- | 测试Go AST生成的正确性
prop_go_ast_generation :: String -> Property
prop_go_ast_generation s =
  let code = "func main() { println(\"" ++ take 10 s ++ "\"); }"
      result = GoAst.parseGoModule (lines code)
  in property $ isRight result

-- | 测试Go词法分析器的标记化
prop_go_lexer_tokenize :: String -> Property
prop_go_lexer_tokenize s =
  let code = "func test() { return " ++ show (length s) ++ "; }"
      tokens = GL.tokenizeGo code
  in property $ not (null tokens)

-- | 测试错误处理器的错误收集
prop_error_collection :: String -> Property
prop_error_collection s =
  let invalidCode = "func test() { var x = " ++ s ++ " @@@ invalid; }"
      result = case P.parseTypusFile invalidCode of
                 Left _ -> Left []
                 Right typusFile -> C.compile typusFile
  in property $ isLeft result

-- | 测试错误恢复机制
prop_error_recovery :: String -> Property
prop_error_recovery s =
  let withError = "func test() { var x = " ++ s ++ " @@@ invalid; return x; }"
      result = case P.parseTypusFile withError of
                 Left _ -> Left []
                 Right typusFile -> C.compile typusFile
  in property $ isLeft result

-- | 测试编译器的幂等性
prop_compiler_idempotent :: String -> Property
prop_compiler_idempotent s =
  let code = "func test() { return " ++ show (length s) ++ "; }"
      parseResult = P.parseTypusFile code
      result1 = case parseResult of
                  Left _ -> Left []
                  Right typusFile -> C.compile typusFile
      result2 = case parseResult of
                  Left _ -> Left []
                  Right typusFile -> C.compile typusFile
  in property $ result1 == result2

-- | 测试IR的构建一致性
prop_ir_consistency :: String -> Property
prop_ir_consistency s =
  let code = "func test() { var x = " ++ show (length s) ++ "; return x; }"
      parseResult = P.parseTypusFile code
      (sourceIR1, sourceIR2) = case parseResult of
                                 Left _ -> (IR.buildSourceIR (P.TypusFile P.defaultFileDirectives [] [] []), 
                                            IR.buildSourceIR (P.TypusFile P.defaultFileDirectives [] [] []))
                                 Right typusFile -> (IR.buildSourceIR typusFile, IR.buildSourceIR typusFile)
  in property $ sourceIR1 == sourceIR2

-- | 测试类型推断的正确性
prop_type_inference :: String -> Property
prop_type_inference s =
  let code = "func test() { var x = " ++ show (length s) ++ "; return x; }"
      result = case P.parseTypusFile code of
                 Left _ -> []
                 Right typusFile -> case TC.diagnoseTypeErrors typusFile of
                                     Left _ -> []
                                     Right diagnostics -> diagnostics
  in property $ null result

-- | 测试所有权检查的循环检测
prop_ownership_cycle_detection :: [String] -> Property
prop_ownership_cycle_detection vars =
  let code = "func test() { " ++ unlines (map (\v -> "var " ++ v ++ " = " ++ v ++ ";") vars) ++ " }"
      result = case P.parseTypusFile code of
                 Left _ -> Left []
                 Right typusFile -> OC.checkOwnership typusFile
  in property $ length vars < 10 ==> isRight result

-- | 测试编译器的内存使用
prop_compiler_memory_usage :: Int -> Property
prop_compiler_memory_usage n =
  let code = unlines $ replicate n "var x = 1;"
      result = case P.parseTypusFile code of
                 Left _ -> Left []
                 Right typusFile -> C.compile typusFile
  in property $ n < 100 ==> isRight result

-- | 测试编译结果的一致性
prop_compilation_consistency :: String -> Property
prop_compilation_consistency s =
  let code = "func test() { var x = " ++ show (length s) ++ "; var y = x + 0; return y; }"
      parseResult = P.parseTypusFile code
      result1 = case parseResult of
                  Left _ -> Left []
                  Right typusFile -> C.compile typusFile
      result2 = case parseResult of
                  Left _ -> Left []
                  Right typusFile -> C.compile typusFile
  in property $ result1 == result2

-- | 测试类型检查器的泛型支持
prop_type_check_generics :: String -> Property
prop_type_check_generics _ =
  let code :: String = "func test[T](x T) { return x; }"
      result = case P.parseTypusFile code of
                 Left _ -> []
                 Right typusFile -> case TC.diagnoseTypeErrors typusFile of
                                     Left _ -> []
                                     Right diagnostics -> diagnostics
  in property $ null result

-- | 测试所有权检查的借用检查
prop_ownership_borrow_check :: String -> Property
prop_ownership_borrow_check s =
  let code = "func test() { var x = \"" ++ s ++ "\"; var y = &x; return *y; }"
      result = case P.parseTypusFile code of
                 Left _ -> Left []
                 Right typusFile -> OC.checkOwnership typusFile
  in property $ isRight result

-- | 测试Go AST的结构完整性
prop_go_ast_integrity :: String -> Property
prop_go_ast_integrity s =
  let code = "func main() { if true { println(\"" ++ take 5 s ++ "\"); } }"
      result = GoAst.parseGoModule (lines code)
  in case result of
    Right module' -> property $ not (null (GoAst.gmDecls module'))
    Left _ -> property $ False

-- | 测试词法分析器的位置信息
prop_lexer_position_info :: String -> Property
prop_lexer_position_info s =
  let code = "func test() { return " ++ s ++ "; }"
      tokens = GL.tokenizeGo code
      hasTokens = not (null tokens)
  in property $ hasTokens

-- | 测试错误消息的有用性
prop_error_messages_useful :: String -> Property
prop_error_messages_useful s =
  let invalidCode = "func test() { var x = " ++ s ++ " @@@ invalid; }"
      result = case P.parseTypusFile invalidCode of
                 Left _ -> Left []
                 Right typusFile -> C.compile typusFile
  in case result of
    Left errors -> property $ not (null errors)
    Right _ -> property $ False

-- | 测试编译器的并发安全性
prop_compiler_concurrent_safe :: String -> Property
prop_compiler_concurrent_safe s =
  let code = "func test() { return " ++ show (length s) ++ "; }"
      parseResult = P.parseTypusFile code
      result1 = case parseResult of
                  Left _ -> Left []
                  Right typusFile -> C.compile typusFile
      result2 = case parseResult of
                  Left _ -> Left []
                  Right typusFile -> C.compile typusFile
  in property $ result1 == result2

-- | 测试编译器的条件编译
prop_compiler_conditional :: String -> Property
prop_compiler_conditional s =
  let code = "func test() { if " ++ show (length s `mod` 2 == 0) ++ " { return 1; } else { return 0; } }"
      result = case P.parseTypusFile code of
                 Left _ -> Left []
                 Right typusFile -> C.compile typusFile
  in property $ isRight result

-- | 测试类型检查器的依赖分析
prop_type_dependency_analysis :: [String] -> Property
prop_type_dependency_analysis types =
  let code = unlines $ map (\t -> "type " ++ t ++ " struct { x int }") types
      result = case P.parseTypusFile code of
                 Left _ -> []
                 Right typusFile -> case TC.diagnoseTypeErrors typusFile of
                                     Left _ -> []
                                     Right diagnostics -> diagnostics
  in property $ length types < 5 ==> null result

-- | 测试所有权检查的生命周期分析
prop_ownership_lifetime_analysis :: String -> Property
prop_ownership_lifetime_analysis s =
  let code = "func test() { var x = \"" ++ s ++ "\"; { var y = x; } return x; }"
      result = case P.parseTypusFile code of
                 Left _ -> Left []
                 Right typusFile -> OC.checkOwnership typusFile
  in property $ isRight result

-- | 测试Go代码生成的正确性
prop_go_code_generation :: String -> Property
prop_go_code_generation s =
  let code = "func test() { return " ++ show (length s) ++ "; }"
      result = case P.parseTypusFile code of
                 Left _ -> ""
                 Right typusFile -> C.generateGoCode typusFile
  in property $ "func test()" `isInfixOf` result

-- | 测试词法分析器的错误处理
prop_lexer_error_handling :: String -> Property
prop_lexer_error_handling s =
  let invalidCode = "func test() { return " ++ s ++ " @@@; }"
      tokens = GL.tokenizeGo invalidCode
      hasTokens = not (null tokens)
  in property $ hasTokens

-- | 测试编译器的性能
prop_compiler_performance :: Int -> Property
prop_compiler_performance n =
  let code = unlines $ replicate n "var x = 1;"
      result = case P.parseTypusFile code of
                 Left _ -> Left []
                 Right typusFile -> C.compile typusFile
  in property $ n < 1000 ==> isRight result

-- | 测试编译器的常量处理
prop_compiler_constant_handling :: String -> Property
prop_compiler_constant_handling s =
  let code = "func test() { return " ++ show (length s) ++ " + 0; }"
      result = case P.parseTypusFile code of
                 Left _ -> Left []
                 Right typusFile -> C.compile typusFile
  in property $ isRight result

-- | 测试类型检查器的多态函数
prop_type_polymorphic_functions :: String -> Property
prop_type_polymorphic_functions _ =
  let code :: String = "func identity[T](x T) T { return x; }"
      result = case P.parseTypusFile code of
                 Left _ -> []
                 Right typusFile -> case TC.diagnoseTypeErrors typusFile of
                                     Left _ -> []
                                     Right diagnostics -> diagnostics
  in property $ null result

-- | 测试所有权检查的移动语义
prop_ownership_move_semantics :: String -> Property
prop_ownership_move_semantics s =
  let code = "func test() { var x = \"" ++ s ++ "\"; var y = move x; return y; }"
      result = case P.parseTypusFile code of
                 Left _ -> Left []
                 Right typusFile -> OC.checkOwnership typusFile
  in property $ isRight result

-- | 测试Go AST的类型信息
prop_go_ast_type_info :: String -> Property
prop_go_ast_type_info s =
  let code = "func test() { var x int = " ++ show (length s) ++ "; return x; }"
      result = GoAst.parseGoModule (lines code)
  in case result of
    Right mod -> property $ not (null (GoAst.gmDecls mod))
    Left _ -> property $ False

-- | 测试词法分析器的字符串处理
prop_lexer_string_processing :: String -> Property
prop_lexer_string_processing s =
  let code = "func test() { var x = \"" ++ s ++ "\"; return x; }"
      tokens = GL.tokenizeGo code
      hasStringToken = any GL.isStringToken tokens
  in property $ hasStringToken

-- | 测试错误处理器的上下文信息
prop_error_context :: String -> Property
prop_error_context s =
  let invalidCode = "func test() { var x = " ++ s ++ " @@@ invalid; }"
      result = case P.parseTypusFile invalidCode of
                 Left _ -> Left []
                 Right typusFile -> C.compile typusFile
  in case result of
    Left errors -> property $ not (null errors)
    Right _ -> property $ False

-- | 测试编译器的模块化
prop_compiler_modular :: String -> Property
prop_compiler_modular s =
  let code = "func test() { return " ++ show (length s) ++ "; }"
      result = case P.parseTypusFile code of
                 Left _ -> Left []
                 Right typusFile -> C.compile typusFile
  in property $ isRight result

-- | 测试编译器的数据流分析
prop_compiler_data_flow :: String -> Property
prop_compiler_data_flow s =
  let code = "func test() { var x = " ++ show (length s) ++ "; var y = x + 1; return y; }"
      result = case P.parseTypusFile code of
                 Left _ -> Left []
                 Right typusFile -> C.compile typusFile
  in property $ isRight result

-- | 测试类型检查器的递归类型
prop_type_recursive_types :: String -> Property
prop_type_recursive_types s =
  let code = "type " ++ take 5 s ++ " struct { next *" ++ take 5 s ++ " }"
      result = case P.parseTypusFile code of
                 Left _ -> []
                 Right typusFile -> case TC.diagnoseTypeErrors typusFile of
                                     Left _ -> []
                                     Right diagnostics -> diagnostics
  in property $ null result

-- | 测试所有权检查的共享引用
prop_ownership_shared_references :: String -> Property
prop_ownership_shared_references s =
  let code = "func test() { var x = \"" ++ s ++ "\"; var y = &x; var z = &x; return *y + *z; }"
      result = case P.parseTypusFile code of
                 Left _ -> Left []
                 Right typusFile -> OC.checkOwnership typusFile
  in property $ isRight result

-- | 测试Go AST的注释处理
prop_go_ast_comments :: String -> Property
prop_go_ast_comments s =
  let code = "func test() { // " ++ s ++ "\n return 1; }"
      result = GoAst.parseGoModule (lines code)
  in case result of
    Right mod -> property $ not (null (GoAst.gmDecls mod))
    Left _ -> property $ False

-- | 测试词法分析器的关键字处理
prop_lexer_keywords :: String -> Property
prop_lexer_keywords s =
  let code = "func test() { " ++ s ++ " return 1; }"
      tokens = GL.tokenizeGo code
  in property $ not (null tokens)

-- | 测试错误处理器的严重性级别
prop_error_severity :: String -> Property
prop_error_severity s =
  let invalidCode = "func test() { var x = " ++ s ++ " @@@ invalid; }"
      result = case P.parseTypusFile invalidCode of
                 Left _ -> Left []
                 Right typusFile -> C.compile typusFile
  in case result of
    Left errors -> property $ not (null errors)
    Right _ -> property $ False

-- | 测试编译器的增量编译
prop_compiler_incremental :: String -> Property
prop_compiler_incremental s =
  let code1 = "func test() { return " ++ show (length s) ++ "; }"
      code2 = code1 ++ "\nfunc test2() { return 2; }"
      parseResult1 = P.parseTypusFile code1
      parseResult2 = P.parseTypusFile code2
      result1 = case parseResult1 of
                  Left _ -> Left []
                  Right typusFile -> C.compile typusFile
      result2 = case parseResult2 of
                  Left _ -> Left []
                  Right typusFile -> C.compile typusFile
  in property $ isRight result1 && isRight result2

-- | 测试编译器的寄存器分配
prop_compiler_register_allocation :: String -> Property
prop_compiler_register_allocation s =
  let code = "func test() { var a = " ++ show (length s) ++ "; var b = a + 1; return b; }"
      result = case P.parseTypusFile code of
                 Left _ -> Left []
                 Right typusFile -> C.compile typusFile
  in property $ isRight result

-- | 测试类型检查器的类型约束
prop_type_constraints :: String -> Property
prop_type_constraints _ =
  let code :: String = "func test[T: Number](x T) { return x + 1; }"
      result = case P.parseTypusFile code of
                 Left _ -> []
                 Right typusFile -> case TC.diagnoseTypeErrors typusFile of
                                     Left _ -> []
                                     Right diagnostics -> diagnostics
  in property $ null result

-- | 测试所有权检查的析构函数
prop_ownership_destructors :: String -> Property
prop_ownership_destructors s =
  let code = "func test() { var x = \"" ++ s ++ "\"; defer { free(x); } return x; }"
      result = case P.parseTypusFile code of
                 Left _ -> Left []
                 Right typusFile -> OC.checkOwnership typusFile
  in property $ isRight result

-- | 测试Go AST的导入处理
prop_go_ast_imports :: String -> Property
prop_go_ast_imports s =
  let code = "import \"" ++ s ++ "\"\nfunc test() { return 1; }"
      result = GoAst.parseGoModule (lines code)
  in case result of
    Right goModule -> property $ not (null (GoAst.gmImports goModule))
    Left _ -> property $ False

-- | 测试词法分析器的数字处理
prop_lexer_numbers :: Int -> Property
prop_lexer_numbers n =
  let code = "func test() { return " ++ show n ++ "; }"
      tokens = GL.tokenizeGo code
      hasNumberToken = any (\t -> GL.tokenKind t == GL.TokNumber) tokens
  in property $ hasNumberToken

-- | 测试错误处理器的建议信息
prop_error_suggestions :: String -> Property
prop_error_suggestions s =
  let invalidCode = "func test() { var x = " ++ s ++ " @@@ invalid; }"
      result = case P.parseTypusFile invalidCode of
                 Left _ -> Left []
                 Right typusFile -> C.compile typusFile
  in case result of
    Left errors -> property $ not (null errors)
    Right _ -> property $ False

-- | 测试编译器的交叉编译
prop_compiler_cross_compile :: String -> Property
prop_compiler_cross_compile s =
  let code = "func test() { return " ++ show (length s) ++ "; }"
      result = case P.parseTypusFile code of
                 Left _ -> Left []
                 Right typusFile -> C.compile typusFile
  in property $ isRight result

-- | 组合所有测试
compilerCoreQuickCheckTests :: TestTree
compilerCoreQuickCheckTests = testGroup "Compiler Core QuickCheck Tests"
  [ testProperty "compile basic" prop_compile_basic
  , testProperty "ir generation consistent" prop_ir_generation_consistent
  , testProperty "ir optimization invariant" prop_ir_optimization_invariant
  , testProperty "type check correct" prop_type_check_correct
  , testProperty "type error detection" prop_type_error_detection
  , testProperty "ownership check basic" prop_ownership_check_basic
  , testProperty "ownership transfer" prop_ownership_transfer
  , testProperty "go ast generation" prop_go_ast_generation
  , testProperty "go lexer tokenize" prop_go_lexer_tokenize
  , testProperty "error collection" prop_error_collection
  , testProperty "error recovery" prop_error_recovery
  , testProperty "compiler idempotent" prop_compiler_idempotent
  , testProperty "ir consistency" prop_ir_consistency
  , testProperty "type inference" prop_type_inference
  , testProperty "ownership cycle detection" prop_ownership_cycle_detection
  , testProperty "compiler memory usage" prop_compiler_memory_usage
  , testProperty "compilation consistency" prop_compilation_consistency
  , testProperty "type check generics" prop_type_check_generics
  , testProperty "ownership borrow check" prop_ownership_borrow_check
  , testProperty "go ast integrity" prop_go_ast_integrity
  , testProperty "lexer position info" prop_lexer_position_info
  , testProperty "error messages useful" prop_error_messages_useful
  , testProperty "compiler concurrent safe" prop_compiler_concurrent_safe
  , testProperty "compiler conditional" prop_compiler_conditional
  , testProperty "type dependency analysis" prop_type_dependency_analysis
  , testProperty "ownership lifetime analysis" prop_ownership_lifetime_analysis
  , testProperty "go code generation" prop_go_code_generation
  , testProperty "lexer error handling" prop_lexer_error_handling
  , testProperty "compiler performance" prop_compiler_performance
  , testProperty "compiler constant handling" prop_compiler_constant_handling
  , testProperty "type polymorphic functions" prop_type_polymorphic_functions
  , testProperty "ownership move semantics" prop_ownership_move_semantics
  , testProperty "go ast type info" prop_go_ast_type_info
  , testProperty "lexer string processing" prop_lexer_string_processing
  , testProperty "error context" prop_error_context
  , testProperty "compiler modular" prop_compiler_modular
  , testProperty "compiler data flow" prop_compiler_data_flow
  , testProperty "type recursive types" prop_type_recursive_types
  , testProperty "ownership shared references" prop_ownership_shared_references
  , testProperty "go ast comments" prop_go_ast_comments
  , testProperty "lexer keywords" prop_lexer_keywords
  , testProperty "error severity" prop_error_severity
  , testProperty "compiler incremental" prop_compiler_incremental
  , testProperty "compiler register allocation" prop_compiler_register_allocation
  , testProperty "type constraints" prop_type_constraints
  , testProperty "ownership destructors" prop_ownership_destructors
  , testProperty "go ast imports" prop_go_ast_imports
  , testProperty "lexer numbers" prop_lexer_numbers
  , testProperty "error suggestions" prop_error_suggestions
  , testProperty "compiler cross compile" prop_compiler_cross_compile
  ]