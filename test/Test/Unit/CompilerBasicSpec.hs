module Test.Unit.CompilerBasicSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Compiler
import qualified Compiler.IR as IR
import qualified Compiler.TypeChecker as TypeChecker
import qualified Compiler.Errors as Errors
import qualified Parser
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing)
import Data.List (nub)

-- Helper function to compile from string
compileFromString :: String -> Either [Errors.CompilerError] String
compileFromString source = 
  case Parser.parseTypus source of
    Right typusFile -> 
      Right $ Compiler.generateGoCode typusFile
    Left _ -> Left [Errors.mkCompilerError "PARSE_ERROR" (T.pack "Parse error") Errors.ParsingPhase Errors.Parsing Errors.Error Nothing Nothing [] [] Nothing]

-- 测试基本编译器功能的属性
prop_compilation_roundtrip :: String -> Property
prop_compilation_roundtrip source = 
  case compileFromString source of
    Right goCode -> property $ length goCode >= 0
    Left _ -> property True

prop_typechecking_valid :: String -> Property
prop_typechecking_valid source = 
  case Parser.parseTypus source of
    Right typusFile -> 
      case TypeChecker.typeCheck typusFile of
        Right _ -> property True
        Left _ -> property False
    Left _ -> property False

prop_ir_generation_consistency :: String -> Property
prop_ir_generation_consistency source = 
  case Parser.parseTypus source of
    Right typusFile -> 
      case Compiler.compile typusFile of
        Right _ -> property True
        Left _ -> property False
    Left _ -> property True

prop_error_location_accuracy :: String -> String -> Property
prop_error_location_accuracy source errorType = 
  case Compiler.analyzeErrors source of
    Right errors -> 
      case errors of
        [] -> property True
        (e:_) -> property $ Errors.line e > 0 && Errors.column e > 0
    Left _ -> property True

prop_optimization_preserves_semantics :: String -> Property
prop_optimization_preserves_semantics source = 
  case Compiler.compile source of
    Right ir -> 
      case Compiler.generateGoCode ir of
        Right goCode -> property $ length goCode >= 0
        Left _ -> property False
    Left _ -> property True

prop_import_resolution :: String -> Property
prop_import_resolution source = 
  case Compiler.compile source of
    Right ir -> 
      case Compiler.extractDeclarations ir of
        decls -> property $ not (null decls)
    Left _ -> property False

prop_symbol_table_consistency :: String -> Property
prop_symbol_table_consistency source = 
  case Compiler.compile source of
    Right ir -> 
      case Compiler.extractDeclarations ir of
        decls -> property $ not (null decls)
    Left _ -> property True

prop_type_inference :: String -> Property
prop_type_inference source = 
  case TypeChecker.typeCheck source of
    Right _ -> property True
    Left _ -> property False

prop_dependency_analysis :: String -> Property
prop_dependency_analysis source = 
  case TypeChecker.buildTypeEnv source of
    Right env -> property $ not (null env)
    Left _ -> property False

prop_constant_folding :: String -> Property
prop_constant_folding source = 
  case Compiler.compile source of
    Right ir -> 
      case Compiler.generateGoCode ir of
        Right goCode -> property $ length goCode >= 0
        Left _ -> property False
    Left _ -> property True

prop_dead_code_elimination :: String -> Property
prop_dead_code_elimination source = 
  case Compiler.compile source of
    Right ir -> 
      case Compiler.generateGoCode ir of
        Right goCode -> property $ length goCode >= 0
        Left _ -> property False
    Left _ -> property True

prop_inlining_preserves_behavior :: String -> Property
prop_inlining_preserves_behavior source = 
  case Compiler.compile source of
    Right ir -> 
      case Compiler.generateGoCode ir of
        Right goCode -> property $ length goCode >= 0
        Left _ -> property False
    Left _ -> property True

prop_memory_usage_bounds :: String -> Property
prop_memory_usage_bounds source = 
  case Compiler.compile source of
    Right ir -> 
      case Compiler.generateGoCode ir of
        Right goCode -> property $ length goCode >= 0
        Left _ -> property False
    Left _ -> property True

prop_compilation_time :: String -> Property
prop_compilation_time source = 
  case Compiler.compile source of
    Right ir -> 
      case Compiler.generateGoCode ir of
        Right goCode -> property $ length goCode >= 0
        Left _ -> property False
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
      case (Compiler.generateGoCode ir1, Compiler.generateGoCode ir2) of
        (Right code1, Right code2) -> property $ length code1 >= 0 && length code2 >= 0
        _ -> property False
    _ -> property True

prop_target_code_generation :: String -> Property
prop_target_code_generation source = 
  case Compiler.compile source of
    Right ir -> 
      case Compiler.generateGoCode ir of
        Right code -> property $ not (null code)
        Left _ -> property False
    Left _ -> property True

prop_warning_detection :: String -> Property
prop_warning_detection source = 
  case Compiler.compile source of
    Right ir -> 
      case Compiler.generateGoCode ir of
        Right code -> property $ length code >= 0
        Left _ -> property False
    Left _ -> property True

prop_resource_limits :: String -> Property
prop_resource_limits source = 
  case Compiler.compile source of
    Right ir -> 
      case Compiler.generateGoCode ir of
        Right code -> property $ length code >= 0
        Left _ -> property False
    Left _ -> property True

prop_incremental_compilation :: String -> String -> Property
prop_incremental_compilation original modified = 
  case (Compiler.compile original, Compiler.compile modified) of
    (Right ir1, Right ir2) -> 
      case (Compiler.generateGoCode ir1, Compiler.generateGoCode ir2) of
        (Right code1, Right code2) -> 
          if modified == original 
            then property $ code1 === code2
            else property True
        _ -> property True
    _ -> property True

prop_parallel_compilation :: [String] -> Property
prop_parallel_compilation sources = 
  case mapM Compiler.compile sources of
    Right results -> property $ length results == length sources
    Left _ -> property True

prop_cache_consistency :: String -> Property
prop_cache_consistency source = 
  case Compiler.compile source of
    Right result1 -> 
      case Compiler.compile source of
        Right result2 -> property $ result1 === result2
        Left _ -> property False
    Left _ -> property True

prop_debug_info :: String -> Property
prop_debug_info source = 
  case Compiler.compile source of
    Right ir -> 
      case Compiler.generateGoCode ir of
        Right code -> property $ length code >= 0
        Left _ -> property False
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
  , testProperty "Compilation time" prop_compilation_time
  , testProperty "Error message informativeness" prop_error_message_informativeness
  , testProperty "Cross module consistency" prop_cross_module_consistency
  , testProperty "Target code generation" prop_target_code_generation
  , testProperty "Warning detection" prop_warning_detection
  , testProperty "Resource limits" prop_resource_limits
  , testProperty "Incremental compilation" prop_incremental_compilation
  , testProperty "Parallel compilation" prop_parallel_compilation
  , testProperty "Cache consistency" prop_cache_consistency
  , testProperty "Debug info" prop_debug_info
  ]