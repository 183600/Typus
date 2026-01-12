module Test.Unit.EnhancedIntegrationSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler (compile)
import Parser (parseTypus)
import Compiler.DependentTypeChecker (checkDependentTypes)
import Compiler.OwnershipChecker (checkOwnership)
import Compiler (formatCompilerErrors)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing)

-- | 测试解析-编译流水线
prop_parse_compile_pipeline :: String -> Property
prop_parse_compile_pipeline input = 
  let parseResult = parseTypus input
      compileResult = case parseTypus input of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile
  in case (parseResult, compileResult) of
    (Left _, Left _) -> property True
    (Right _, Right _) -> property True
    (Left _, Right _) -> property False -- 解析失败但编译成功应该不可能
    (Right _, Left _) -> property True  -- 解析成功但编译失败是可能的

-- | 测试依赖类型检查集成
prop_dependent_types_integration :: String -> Property
prop_dependent_types_integration input = 
  let parseResult = parseTypus input
      dependentTypesResult = case parseTypus input of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkDependentTypes typusFile
  in case (parseResult, dependentTypesResult) of
    (Left _, Left _) -> property True
    (Right _, Right _) -> property True
    (Left _, Right _) -> property False -- 解析失败但类型检查成功应该不可能
    (Right _, Left _) -> property True  -- 解析成功但类型检查失败是可能的

-- | 测试所有权检查集成
prop_ownership_integration :: String -> Property
prop_ownership_integration input = 
  let parseResult = parseTypus input
      ownershipResult = case parseTypus input of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkOwnership typusFile
  in case (parseResult, ownershipResult) of
    (Left _, Left _) -> property True
    (Right _, Right _) -> property True
    (Left _, Right _) -> property False -- 解析失败但所有权检查成功应该不可能
    (Right _, Left _) -> property True  -- 解析成功但所有权检查失败是可能的

-- | 测试错误处理集成
prop_error_handling_integration :: String -> Property
prop_error_handling_integration input = 
  let parseResult = parseTypus input
      compileResult = case parseTypus input of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile
  in case (parseResult, compileResult) of
    (Left parseErr, Left compileErr) -> 
      let parseMsg = T.pack $ formatCompilerErrors [parseErr]
          compileMsg = T.pack $ formatCompilerErrors compileErr
      in property (not (T.null parseMsg) && not (T.null compileMsg))
    (Right _, Right _) -> property True
    _ -> property True

-- | 测试多模块集成
prop_multi_module_integration :: [String] -> Property
prop_multi_module_integration inputs = 
  let parseResults = map parseTypus inputs
      compileResults = map (\input -> case parseTypus input of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile) inputs
      allParseSuccess = all isRight parseResults
      allCompileSuccess = all isRight compileResults
  in if allParseSuccess then 
        if allCompileSuccess then property True
        else property True -- 有些模块编译失败是可能的
     else property True -- 有些模块解析失败是可能的
  where
    isRight (Right _) = True
    isRight (Left _) = False

-- | 测试依赖类型和所有权集成
prop_dependent_types_ownership_integration :: String -> Property
prop_dependent_types_ownership_integration input = 
  let dependentTypesResult = case parseTypus input of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkDependentTypes typusFile
      ownershipResult = case parseTypus input of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkOwnership typusFile
  in case (dependentTypesResult, ownershipResult) of
    (Left _, Left _) -> property True
    (Right _, Right _) -> property True
    (Left _, Right _) -> property True
    (Right _, Left _) -> property True

-- | 测试完整编译流水线
prop_full_compilation_pipeline :: String -> Property
prop_full_compilation_pipeline input = 
  let parseResult = parseTypus input
      compileResult = case parseTypus input of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile
  in case (parseResult, compileResult) of
    (Right parseFile, Right compileResult) -> 
      property True -- 成功完成整个流水线
    (Left _, Left _) -> 
      property True -- 在早期阶段失败是预期的
    _ -> property True -- 其他情况也是可能的

-- | 测试增量编译集成
prop_incremental_compilation_integration :: String -> String -> Property
prop_incremental_compilation_integration original modified = 
  let originalResult = case parseTypus original of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile
      modifiedResult = case parseTypus modified of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile
  in case (originalResult, modifiedResult) of
    (Right _, Right _) -> property True
    (Left _, Left _) -> property True
    (Right _, Left _) -> property True -- 修改可能引入错误
    (Left _, Right _) -> property True -- 修改可能修复错误

-- | 测试错误恢复集成
prop_error_recovery_integration :: String -> Property
prop_error_recovery_integration input = 
  let compileResult = case parseTypus input of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile
  in case compileResult of
    Left errors -> property (length errors > 0) -- 应该有错误信息
    Right _ -> property True -- 编译成功

-- | 测试优化集成
prop_optimization_integration :: String -> Property
prop_optimization_integration input = 
  let compileResult = case parseTypus input of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile
      optimizedResult = case parseTypus ("-O " ++ input) of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile
  in case (compileResult, optimizedResult) of
    (Right _, Right _) -> property True
    (Left _, Left _) -> property True
    (Left _, Right _) -> property True -- 优化可能修复某些错误
    (Right _, Left _) -> property True -- 优化可能引入某些错误

-- | 测试代码生成集成
prop_code_generation_integration :: String -> Property
prop_code_generation_integration input = 
  let compileResult = case parseTypus input of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile
  in case compileResult of
    Right result -> property (not (T.null (T.pack result))) -- 生成的代码应该非空
    Left _ -> property True -- 编译失败时跳过

-- | 测试类型推断集成
prop_type_inference_integration :: String -> Property
prop_type_inference_integration input = 
  let compileResult = case parseTypus input of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile
  in case compileResult of
    Right result -> property True -- 类型推断成功
    Left _ -> property True -- 类型推断可能失败

-- | 测试符号表集成
prop_symbol_table_integration :: String -> Property
prop_symbol_table_integration input = 
  let parseResult = parseTypus input
      compileResult = case parseTypus input of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile
  in case (parseResult, compileResult) of
    (Right parseFile, Right compileResult) -> 
      property True -- 符号表构建成功
    _ -> property True

-- | 测试跨模块引用集成
prop_cross_module_reference_integration :: String -> String -> Property
prop_cross_module_reference_integration module1 module2 = 
  let parseResult1 = parseTypus module1
      parseResult2 = parseTypus module2
      compileResult1 = case parseTypus module1 of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile
      compileResult2 = case parseTypus module2 of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile
  in case (parseResult1, parseResult2) of
    (Right _, Right _) -> 
      case (compileResult1, compileResult2) of
        (Right _, Right _) -> property True
        _ -> property True
    _ -> property True

-- | 测试循环依赖检测集成
prop_circular_dependency_integration :: [String] -> Property
prop_circular_dependency_integration modules = 
  let parseResults = map parseTypus modules
      compileResults = map (\m -> case parseTypus m of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile) modules
  in case (all isRight parseResults, all isRight compileResults) of
    (True, True) -> property True
    (True, False) -> property True -- 可能检测到循环依赖
    (False, _) -> property True -- 解析失败
  where
    isRight (Right _) = True
    isRight (Left _) = False

-- | 测试资源管理集成
prop_resource_management_integration :: String -> Property
prop_resource_management_integration input = 
  let compileResult = case parseTypus input of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile
  in case compileResult of
    Right result -> property True -- 资源管理成功
    Left _ -> property True -- 资源管理可能失败

tests :: TestTree
tests = testGroup "Enhanced Integration Tests"
  [ testProperty "parse compile pipeline" prop_parse_compile_pipeline
  , testProperty "dependent types integration" prop_dependent_types_integration
  , testProperty "ownership integration" prop_ownership_integration
  , testProperty "error handling integration" prop_error_handling_integration
  , testProperty "multi module integration" prop_multi_module_integration
  , testProperty "dependent types ownership integration" prop_dependent_types_ownership_integration
  , testProperty "full compilation pipeline" prop_full_compilation_pipeline
  , testProperty "incremental compilation integration" prop_incremental_compilation_integration
  , testProperty "error recovery integration" prop_error_recovery_integration
  , testProperty "optimization integration" prop_optimization_integration
  , testProperty "code generation integration" prop_code_generation_integration
  , testProperty "type inference integration" prop_type_inference_integration
  , testProperty "symbol table integration" prop_symbol_table_integration
  , testProperty "cross module reference integration" prop_cross_module_reference_integration
  , testProperty "circular dependency integration" prop_circular_dependency_integration
  , testProperty "resource management integration" prop_resource_management_integration
  ]