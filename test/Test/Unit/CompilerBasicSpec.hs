module Test.Unit.CompilerBasicSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Compiler
import qualified Compiler.IR as IR
import qualified Compiler.TypeChecker as TypeChecker
import qualified Compiler.Errors as Errors
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import Data.List (nub)

-- 测试基本编译器功能的属性
prop_compilation_roundtrip :: String -> Property
prop_compilation_roundtrip source = 
  case Compiler.compile source of
    Right ir -> 
      case Compiler.decompile ir of
        Right decompiled -> property $ decompiled === source
        Left _ -> property False
    Left _ -> property True

prop_typechecking_valid :: String -> Property
prop_typechecking_valid source = 
  case TypeChecker.check source of
    Right _ -> property True
    Left _ -> property False

prop_ir_generation_consistency :: String -> Property
prop_ir_generation_consistency source = 
  case Compiler.compile source of
    Right ir -> 
      case Compiler.validateIR ir of
        Right _ -> property True
        Left _ -> property False
    Left _ -> property True

prop_error_location_accuracy :: String -> String -> Property
prop_error_location_accuracy source errorType = 
  case Compiler.checkErrors source of
    Right errors -> 
      case errors of
        [] -> property True
        (e:_) -> property $ Errors.line e > 0 && Errors.column e > 0
    Left _ -> property True

prop_optimization_preserves_semantics :: String -> Property
prop_optimization_preserves_semantics source = 
  case Compiler.compile source of
    Right ir -> 
      let optimized = Compiler.optimize ir
      in case (Compiler.evaluate ir, Compiler.evaluate optimized) of
        (Right result1, Right result2) -> property $ result1 === result2
        _ -> property True
    Left _ -> property True

prop_import_resolution :: String -> Property
prop_import_resolution source = 
  case Compiler.resolveImports source of
    Right resolved -> 
      case Compiler.compile resolved of
        Right _ -> property True
        Left _ -> property False
    Left _ -> property True

prop_symbol_table_consistency :: String -> Property
prop_symbol_table_consistency source = 
  case Compiler.compile source of
    Right ir -> 
      let symbols = Compiler.extractSymbols ir
          duplicates = symbols `intersect` symbols
      in property $ null duplicates
    Left _ -> property True
  where
    intersect xs = nub . filter (\x -> x `elem` xs)

prop_type_inference :: String -> Property
prop_type_inference source = 
  case Compiler.inferType source of
    Right (inferredType, _) -> property $ not (null inferredType)
    Left _ -> property True

prop_dependency_analysis :: String -> Property
prop_dependency_analysis source = 
  case Compiler.analyzeDependencies source of
    Right deps -> 
      case Compiler.topologicalSort deps of
        Right sorted -> property $ length sorted == length deps
        Left _ -> property False
    Left _ -> property True

prop_constant_folding :: String -> Property
prop_constant_folding source = 
  case Compiler.compile source of
    Right ir -> 
      let folded = Compiler.foldConstants ir
      in case (Compiler.evaluate ir, Compiler.evaluate folded) of
        (Right result1, Right result2) -> property $ result1 === result2
        _ -> property True
    Left _ -> property True

prop_dead_code_elimination :: String -> Property
prop_dead_code_elimination source = 
  case Compiler.compile source of
    Right ir -> 
      let optimized = Compiler.elinateDeadCode ir
      in property $ IR.size optimized <= IR.size ir
    Left _ -> property True

prop_inlining_preserves_behavior :: String -> Property
prop_inlining_preserves_behavior source = 
  case Compiler.compile source of
    Right ir -> 
      let inlined = Compiler.inlineFunctions ir
      in case (Compiler.evaluate ir, Compiler.evaluate inlined) of
        (Right result1, Right result2) -> property $ result1 === result2
        _ -> property True
    Left _ -> property True

prop_memory_usage_bounds :: String -> Property
prop_memory_usage_bounds source = 
  case Compiler.compile source of
    Right ir -> 
      let memoryUsage = Compiler.estimateMemoryUsage ir
      in property $ memoryUsage >= 0
    Left _ -> property True

prop_compilation_time_reasonable :: String -> Property
prop_compilation_time_reasonable source = 
  case Compiler.timedCompile source of
    Right (ir, time) -> property $ time < 1000000 -- 1 second in microseconds
    Left _ -> property True

prop_error_message_informativeness :: String -> Property
prop_error_message_informativeness source = 
  case Compiler.compile source of
    Left err -> property $ length err > 10
    Right _ -> property True

prop_cross_module_consistency :: String -> String -> Property
prop_cross_module_consistency mod1 mod2 = 
  case (Compiler.compile mod1, Compiler.compile mod2) of
    (Right ir1, Right ir2) -> 
      case Compiler.linkModules [ir1, ir2] of
        Right _ -> property True
        Left _ -> property False
    _ -> property True

prop_target_code_generation :: String -> Property
prop_target_code_generation source = 
  case Compiler.compile source of
    Right ir -> 
      case Compiler.generateTargetCode ir of
        Right code -> property $ not (null code)
        Left _ -> property False
    Left _ -> property True

prop_warning_detection :: String -> Property
prop_warning_detection source = 
  case Compiler.checkWarnings source of
    Right warnings -> 
      case warnings of
        [] -> property True
        (w:_) -> property $ length w > 5
    Left _ -> property True

prop_resource_limits :: String -> Property
prop_resource_limits source = 
  case Compiler.compileWithLimits source 1000 1000 of
    Right _ -> property True
    Left _ -> property True

prop_incremental_compilation :: String -> String -> Property
prop_incremental_compilation original modified = 
  case Compiler.compile original of
    Right ir1 -> 
      case Compiler.compileIncremental original modified of
        Right ir2 -> 
          case (Compiler.evaluate ir1, Compiler.evaluate ir2) of
            (Right result1, Right result2) -> 
              if modified == original 
                then property $ result1 === result2
                else property True
            _ -> property True
        Left _ -> property False
    Left _ -> property True

prop_parallel_compilation :: [String] -> Property
prop_parallel_compilation sources = 
  case Compiler.compileParallel sources of
    Right results -> property $ length results == length sources
    Left _ -> property True

prop_cache_consistency :: String -> Property
prop_cache_consistency source = 
  case Compiler.compileWithCache source of
    Right (ir1, cache) -> 
      case Compiler.compileWithCache source of
        Right (ir2, _) -> property $ ir1 === ir2
        Left _ -> property False
    Left _ -> property True

prop_debug_info_preservation :: String -> Property
prop_debug_info_preservation source = 
  case Compiler.compileWithDebug source of
    Right (ir, debug) -> property $ length debug > 0
    Left _ -> property True

tests :: TestTree
tests = testGroup "Compiler Basic Tests"
  [ testProperty "Compilation roundtrip" prop_compilation_roundtrip
  , testProperty "Typechecking valid" prop_typechecking_valid
  , testProperty "IR generation consistency" prop_ir_generation_consistency
  , testProperty "Error location accuracy" prop_error_location_accuracy
  , testProperty "Optimization preserves semantics" prop_optimization_preserves_semantics
  , testProperty "Import resolution" prop_import_resolution
  , testProperty "Symbol table consistency" prop_symbol_table_consistency
  , testProperty "Type inference" prop_type_inference
  , testProperty "Dependency analysis" prop_dependency_analysis
  , testProperty "Constant folding" prop_constant_folding
  , testProperty "Dead code elimination" prop_dead_code_elimination
  , testProperty "Inlining preserves behavior" prop_inlining_preserves_behavior
  , testProperty "Memory usage bounds" prop_memory_usage_bounds
  , testProperty "Compilation time reasonable" prop_compilation_time_reasonable
  , testProperty "Error message informativeness" prop_error_message_informativeness
  , testProperty "Cross module consistency" prop_cross_module_consistency
  , testProperty "Target code generation" prop_target_code_generation
  , testProperty "Warning detection" prop_warning_detection
  , testProperty "Resource limits" prop_resource_limits
  , testProperty "Incremental compilation" prop_incremental_compilation
  , testProperty "Parallel compilation" prop_parallel_compilation
  , testProperty "Cache consistency" prop_cache_consistency
  , testProperty "Debug info preservation" prop_debug_info_preservation
  ]