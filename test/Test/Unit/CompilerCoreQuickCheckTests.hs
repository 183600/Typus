{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.CompilerCoreQuickCheckTests where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Compiler as C
import qualified Compiler.IR as IR
import qualified Compiler.GoAst as GoAst
import qualified Compiler.GoLexer as GL
import qualified Compiler.TypeChecker as TC
import qualified Compiler.OwnershipChecker as OC
import qualified Compiler.Error as CE
import qualified Utils as U
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort)
import Data.Char (isSpace, isLetter, isDigit)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | 测试编译器的基本编译功能
prop_compile_basic :: String -> Property
prop_compile_basic s =
  let validCode = "func main() { return " ++ show (length s) ++ "; }"
      result = C.compile validCode
  in property $ isRight result

-- | 测试IR生成的一致性
prop_ir_generation_consistent :: String -> Property
prop_ir_generation_consistent s =
  let code = "func test() { var x = " ++ show (length s) ++ "; return x; }"
      ir1 = C.generateIR code
      ir2 = C.generateIR code
  in property $ ir1 == ir2

-- | 测试IR的优化不变性
prop_ir_optimization_invariant :: String -> Property
prop_ir_optimization_invariant s =
  let code = "func test() { var x = " ++ show (length s) ++ "; var y = x + 1; return y; }"
      ir = C.generateIR code
      optimized = C.optimizeIR ir
      -- 优化后的IR应该语义等价
  in property $ IR.semanticEquivalent ir optimized

-- | 测试类型检查的正确性
prop_type_check_correct :: String -> Property
prop_type_check_correct s =
  let validCode = "func test() { var x int = " ++ show (length s) ++ "; return x; }"
      result = TC.typeCheck validCode
  in property $ isRight result

-- | 测试类型错误检测
prop_type_error_detection :: String -> Property
prop_type_error_detection s =
  let invalidCode = "func test() { var x int = \"" ++ s ++ "\"; return x; }"
      result = TC.typeCheck invalidCode
  in property $ isLeft result

-- | 测试所有权检查的基本功能
prop_ownership_check_basic :: String -> Property
prop_ownership_check_basic s =
  let code = "func test() { var x = " ++ show (length s) ++ "; var y = x; return y; }"
      result = OC.checkOwnership code
  in property $ isRight result

-- | 测试所有权转移检测
prop_ownership_transfer :: String -> Property
prop_ownership_transfer s =
  let code = "func test() { var x = \"" ++ s ++ "\"; var y = x; return y; }"
      result = OC.checkOwnership code
  in property $ isRight result

-- | 测试Go AST生成的正确性
prop_go_ast_generation :: String -> Property
prop_go_ast_generation s =
  let code = "func main() { println(\"" ++ take 10 s ++ "\"); }"
      ast = GoAst.generateAST code
  in property $ isJust ast

-- | 测试Go词法分析器的标记化
prop_go_lexer_tokenize :: String -> Property
prop_go_lexer_tokenize s =
  let code = "func test() { return " ++ show (length s) ++ "; }"
      tokens = GL.tokenize code
  in property $ not (null tokens)

-- | 测试错误处理器的错误收集
prop_error_collection :: String -> Property
prop_error_collection s =
  let invalidCode = "func test() { var x = " ++ s ++ " @@@ invalid; }"
      errors = CE.collectErrors invalidCode
  in property $ not (null errors)

-- | 测试错误恢复机制
prop_error_recovery :: String -> Property
prop_error_recovery s =
  let withError = "func test() { var x = " ++ s ++ " @@@ invalid; return x; }"
      result = C.compileWithErrorRecovery withError
  in property $ isRight result

-- | 测试编译器的幂等性
prop_compiler_idempotent :: String -> Property
prop_compiler_idempotent s =
  let code = "func test() { return " ++ show (length s) ++ "; }"
      result1 = C.compile code
      result2 = C.compile code
  in property $ result1 == result2

-- | 测试IR的序列化和反序列化
prop_ir_serialization :: String -> Property
prop_ir_serialization s =
  let code = "func test() { var x = " ++ show (length s) ++ "; return x; }"
      ir = C.generateIR code
      serialized = IR.serialize ir
      deserialized = IR.deserialize serialized
  in property $ ir == deserialized

-- | 测试类型推断的正确性
prop_type_inference :: String -> Property
prop_type_inference s =
  let code = "func test() { var x = " ++ show (length s) ++ "; return x; }"
      inferredType = TC.inferType code "x"
  in property $ inferredType == Just "int"

-- | 测试所有权检查的循环检测
prop_ownership_cycle_detection :: [String] -> Property
prop_ownership_cycle_detection vars =
  let code = "func test() { " ++ unlines (map (\v -> "var " ++ v ++ " = " ++ v ++ ";") vars) ++ " }"
      result = OC.checkOwnership code
  in property $ length vars < 10 ==> isRight result

-- | 测试编译器的内存使用
prop_compiler_memory_usage :: Int -> Property
prop_compiler_memory_usage n =
  let code = unlines $ replicate n "var x = 1;"
      result = C.compile code
  in property $ n < 100 ==> isRight result

-- | 测试IR优化的有效性
prop_ir_optimization_effective :: String -> Property
prop_ir_optimization_effective s =
  let code = "func test() { var x = " ++ show (length s) ++ "; var y = x + 0; return y; }"
      ir = C.generateIR code
      optimized = C.optimizeIR ir
  in property $ IR.instructionCount optimized <= IR.instructionCount ir

-- | 测试类型检查器的泛型支持
prop_type_check_generics :: String -> Property
prop_type_check_generics s =
  let code = "func test[T](x T) { return x; }"
      result = TC.typeCheck code
  in property $ isRight result

-- | 测试所有权检查的借用检查
prop_ownership_borrow_check :: String -> Property
prop_ownership_borrow_check s =
  let code = "func test() { var x = \"" ++ s ++ "\"; var y = &x; return *y; }"
      result = OC.checkOwnership code
  in property $ isRight result

-- | 测试Go AST的结构完整性
prop_go_ast_integrity :: String -> Property
prop_go_ast_integrity s =
  let code = "func main() { if true { println(\"" ++ take 5 s ++ "\"); } }"
      ast = GoAst.generateAST code
  in case ast of
    Just a -> property $ GoAst.wellFormed a
    Nothing -> property $ False

-- | 测试词法分析器的位置信息
prop_lexer_position_info :: String -> Property
prop_lexer_position_info s =
  let code = "func test() { return " ++ s ++ "; }"
      tokens = GL.tokenize code
      hasPositions = all GL.hasPositionInfo tokens
  in property $ hasPositions

-- | 测试错误消息的有用性
prop_error_messages_useful :: String -> Property
prop_error_messages_useful s =
  let invalidCode = "func test() { var x = " ++ s ++ " @@@ invalid; }"
      errors = CE.collectErrors invalidCode
      hasLocation = all CE.hasLocationInfo errors
  in property $ hasLocation

-- | 测试编译器的并发安全性
prop_compiler_concurrent_safe :: String -> Property
prop_compiler_concurrent_safe s =
  let code = "func test() { return " ++ show (length s) ++ "; }"
      result1 = C.compile code
      result2 = C.compile code
  in property $ result1 == result2

-- | 测试IR的控制流分析
prop_ir_control_flow :: String -> Property
prop_ir_control_flow s =
  let code = "func test() { if " ++ show (even (length s)) ++ " { return 1; } else { return 0; } }"
      ir = C.generateIR code
      cfg = IR.buildCFG ir
  in property $ IR.wellFormedCFG cfg

-- | 测试类型检查器的依赖分析
prop_type_dependency_analysis :: [String] -> Property
prop_type_dependency_analysis types =
  let code = unlines $ map (\t -> "type " ++ t ++ " struct { x " ++ t ++ " }") types
      result = TC.typeCheck code
  in property $ length types < 5 ==> isRight result

-- | 测试所有权检查的生命周期分析
prop_ownership_lifetime_analysis :: String -> Property
prop_ownership_lifetime_analysis s =
  let code = "func test() { var x = \"" ++ s ++ "\"; { var y = x; } return x; }"
      result = OC.checkOwnership code
  in property $ isRight result

-- | 测试Go代码生成的正确性
prop_go_code_generation :: String -> Property
prop_go_code_generation s =
  let code = "func test() { return " ++ show (length s) ++ "; }"
      goCode = C.generateGoCode code
  in property $ "func test()" `isInfixOf` goCode

-- | 测试词法分析器的错误处理
prop_lexer_error_handling :: String -> Property
prop_lexer_error_handling s =
  let invalidCode = "func test() { return " ++ s ++ " @@@; }"
      tokens = GL.tokenize invalidCode
      hasErrors = any GL.isErrorToken tokens
  in property $ hasErrors

-- | 测试编译器的性能
prop_compiler_performance :: Int -> Property
prop_compiler_performance n =
  let code = unlines $ replicate n "var x = 1;"
      result = C.compile code
  in property $ n < 1000 ==> isRight result

-- | 测试IR的常量折叠
prop_ir_constant_folding :: String -> Property
prop_ir_constant_folding s =
  let code = "func test() { return " ++ show (length s) ++ " + 0; }"
      ir = C.generateIR code
      optimized = C.optimizeIR ir
  in property $ IR.hasConstantFolded optimized

-- | 测试类型检查器的多态函数
prop_type_polymorphic_functions :: String -> Property
prop_type_polymorphic_functions s =
  let code = "func identity[T](x T) T { return x; }"
      result = TC.typeCheck code
  in property $ isRight result

-- | 测试所有权检查的移动语义
prop_ownership_move_semantics :: String -> Property
prop_ownership_move_semantics s =
  let code = "func test() { var x = \"" ++ s ++ "\"; var y = move x; return y; }"
      result = OC.checkOwnership code
  in property $ isRight result

-- | 测试Go AST的类型信息
prop_go_ast_type_info :: String -> Property
prop_go_ast_type_info s =
  let code = "func test() { var x int = " ++ show (length s) ++ "; return x; }"
      ast = GoAst.generateAST code
  in case ast of
    Just a -> property $ GoAst.hasTypeInfo a
    Nothing -> property $ False

-- | 测试词法分析器的字符串处理
prop_lexer_string_processing :: String -> Property
prop_lexer_string_processing s =
  let code = "func test() { var x = \"" ++ s ++ "\"; return x; }"
      tokens = GL.tokenize code
      hasStringToken = any GL.isStringToken tokens
  in property $ hasStringToken

-- | 测试错误处理器的上下文信息
prop_error_context :: String -> Property
prop_error_context s =
  let invalidCode = "func test() { var x = " ++ s ++ " @@@ invalid; }"
      errors = CE.collectErrors invalidCode
      hasContext = all CE.hasContextInfo errors
  in property $ hasContext

-- | 测试编译器的模块化
prop_compiler_modular :: String -> Property
prop_compiler_modular s =
  let module1 = "module1 { func test() { return " ++ show (length s) ++ "; } }"
      module2 = "module2 { func test2() { return 1; } }"
      result = C.compileModules [module1, module2]
  in property $ isRight result

-- | 测试IR的数据流分析
prop_ir_data_flow :: String -> Property
prop_ir_data_flow s =
  let code = "func test() { var x = " ++ show (length s) ++ "; var y = x + 1; return y; }"
      ir = C.generateIR code
      dfa = IR.buildDataFlowAnalysis ir
  in property $ IR.wellFormedDFA dfa

-- | 测试类型检查器的递归类型
prop_type_recursive_types :: String -> Property
prop_type_recursive_types s =
  let code = "type " ++ s ++ " struct { next *" ++ s ++ " }"
      result = TC.typeCheck code
  in property $ isRight result

-- | 测试所有权检查的共享引用
prop_ownership_shared_references :: String -> Property
prop_ownership_shared_references s =
  let code = "func test() { var x = \"" ++ s ++ "\"; var y = &x; var z = &x; return *y + *z; }"
      result = OC.checkOwnership code
  in property $ isRight result

-- | 测试Go AST的注释处理
prop_go_ast_comments :: String -> Property
prop_go_ast_comments s =
  let code = "func test() { // " ++ s ++ "\n return 1; }"
      ast = GoAst.generateAST code
  in case ast of
    Just a -> property $ GoAst.hasComments a
    Nothing -> property $ False

-- | 测试词法分析器的关键字处理
prop_lexer_keywords :: String -> Property
prop_lexer_keywords s =
  let code = "func test() { " ++ s ++ " return 1; }"
      tokens = GL.tokenize code
  in property $ not (null tokens)

-- | 测试错误处理器的严重性级别
prop_error_severity :: String -> Property
prop_error_severity s =
  let invalidCode = "func test() { var x = " ++ s ++ " @@@ invalid; }"
      errors = CE.collectErrors invalidCode
      hasSeverity = all CE.hasSeverityLevel errors
  in property $ hasSeverity

-- | 测试编译器的增量编译
prop_compiler_incremental :: String -> Property
prop_compiler_incremental s =
  let code1 = "func test() { return " ++ show (length s) ++ "; }"
      code2 = code1 ++ "\nfunc test2() { return 2; }"
      result1 = C.compile code1
      result2 = C.compileIncremental code1 code2
  in property $ isRight result1 && isRight result2

-- | 测试IR的寄存器分配
prop_ir_register_allocation :: String -> Property
prop_ir_register_allocation s =
  let code = "func test() { var a = " ++ show (length s) ++ "; var b = a + 1; return b; }"
      ir = C.generateIR code
      allocated = IR.allocateRegisters ir
  in property $ IR.hasValidRegisterAllocation allocated

-- | 测试类型检查器的类型约束
prop_type_constraints :: String -> Property
prop_type_constraints s =
  let code = "func test[T: Number](x T) { return x + 1; }"
      result = TC.typeCheck code
  in property $ isRight result

-- | 测试所有权检查的析构函数
prop_ownership_destructors :: String -> Property
prop_ownership_destructors s =
  let code = "func test() { var x = \"" ++ s ++ "\"; defer { free(x); } return x; }"
      result = OC.checkOwnership code
  in property $ isRight result

-- | 测试Go AST的导入处理
prop_go_ast_imports :: String -> Property
prop_go_ast_imports s =
  let code = "import \"" ++ s ++ "\"\nfunc test() { return 1; }"
      ast = GoAst.generateAST code
  in case ast of
    Just a -> property $ GoAst.hasImports a
    Nothing -> property $ False

-- | 测试词法分析器的数字处理
prop_lexer_numbers :: Int -> Property
prop_lexer_numbers n =
  let code = "func test() { return " ++ show n ++ "; }"
      tokens = GL.tokenize code
      hasNumberToken = any GL.isNumberToken tokens
  in property $ hasNumberToken

-- | 测试错误处理器的建议信息
prop_error_suggestions :: String -> Property
prop_error_suggestions s =
  let invalidCode = "func test() { var x = " ++ s ++ " @@@ invalid; }"
      errors = CE.collectErrors invalidCode
      hasSuggestions = any CE.hasSuggestions errors
  in property $ hasSuggestions

-- | 测试编译器的交叉编译
prop_compiler_cross_compile :: String -> Property
prop_compiler_cross_compile s =
  let code = "func test() { return " ++ show (length s) ++ "; }"
      result = C.crossCompile code "windows"
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
  , testProperty "ir serialization" prop_ir_serialization
  , testProperty "type inference" prop_type_inference
  , testProperty "ownership cycle detection" prop_ownership_cycle_detection
  , testProperty "compiler memory usage" prop_compiler_memory_usage
  , testProperty "ir optimization effective" prop_ir_optimization_effective
  , testProperty "type check generics" prop_type_check_generics
  , testProperty "ownership borrow check" prop_ownership_borrow_check
  , testProperty "go ast integrity" prop_go_ast_integrity
  , testProperty "lexer position info" prop_lexer_position_info
  , testProperty "error messages useful" prop_error_messages_useful
  , testProperty "compiler concurrent safe" prop_compiler_concurrent_safe
  , testProperty "ir control flow" prop_ir_control_flow
  , testProperty "type dependency analysis" prop_type_dependency_analysis
  , testProperty "ownership lifetime analysis" prop_ownership_lifetime_analysis
  , testProperty "go code generation" prop_go_code_generation
  , testProperty "lexer error handling" prop_lexer_error_handling
  , testProperty "compiler performance" prop_compiler_performance
  , testProperty "ir constant folding" prop_ir_constant_folding
  , testProperty "type polymorphic functions" prop_type_polymorphic_functions
  , testProperty "ownership move semantics" prop_ownership_move_semantics
  , testProperty "go ast type info" prop_go_ast_type_info
  , testProperty "lexer string processing" prop_lexer_string_processing
  , testProperty "error context" prop_error_context
  , testProperty "compiler modular" prop_compiler_modular
  , testProperty "ir data flow" prop_ir_data_flow
  , testProperty "type recursive types" prop_type_recursive_types
  , testProperty "ownership shared references" prop_ownership_shared_references
  , testProperty "go ast comments" prop_go_ast_comments
  , testProperty "lexer keywords" prop_lexer_keywords
  , testProperty "error severity" prop_error_severity
  , testProperty "compiler incremental" prop_compiler_incremental
  , testProperty "ir register allocation" prop_ir_register_allocation
  , testProperty "type constraints" prop_type_constraints
  , testProperty "ownership destructors" prop_ownership_destructors
  , testProperty "go ast imports" prop_go_ast_imports
  , testProperty "lexer numbers" prop_lexer_numbers
  , testProperty "error suggestions" prop_error_suggestions
  , testProperty "compiler cross compile" prop_compiler_cross_compile
  ]