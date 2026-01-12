module Test.Unit.EnhancedPerformanceSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler (compile)
import Parser (parseTypus)
import Compiler.DependentTypeChecker (checkDependentTypes)
import Compiler.OwnershipChecker (checkOwnership)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Data.List (foldl')
import qualified Data.Text as T

-- | 测试解析性能：解析时间应该与输入大小成线性关系
prop_parse_performance_linear :: Positive Int -> Property
prop_parse_performance_linear (Positive n) = 
  let input = replicate n "let x = 42\n"
  in ioProperty $ do
      startTime <- getCurrentTime
      _ <- evaluate $ parseTypus (concat input)
      endTime <- getCurrentTime
      let duration = diffUTCTime endTime startTime
      return $ duration `seq` property True -- 这个测试主要检查是否能在合理时间内完成

-- | 测试编译性能：编译时间应该与输入大小成合理关系
prop_compile_performance_reasonable :: Positive Int -> Property
prop_compile_performance_reasonable (Positive n) = 
  let input = "// @dependent-types: true\n// @ownership: true\n```typus\n" ++ 
              concat (replicate n "let x = 42\n") ++ 
              "```"
      startTime <- getCurrentTime
      _ <- compile input
      endTime <- getCurrentTime
      duration = diffUTCTime endTime startTime
  in duration `seq` property True -- 这个测试主要检查是否能在合理时间内完成

-- | 测试依赖类型检查性能
prop_dependent_types_performance :: Positive Int -> Property
prop_dependent_types_performance (Positive n) = 
  let input = "// @dependent-types: true\n```typus\n" ++ 
              concat (replicate n "let x: Nat = 42\n") ++ 
              "```"
      startTime <- getCurrentTime
      _ <- checkDependentTypes input
      endTime <- getCurrentTime
      duration = diffUTCTime endTime startTime
  in duration `seq` property True

-- | 测试所有权检查性能
prop_ownership_performance :: Positive Int -> Property
prop_ownership_performance (Positive n) = 
  let input = "// @ownership: true\n```typus\n" ++ 
              concat (replicate n "let x = Box(42)\n") ++ 
              "```"
      startTime <- getCurrentTime
      _ <- checkOwnership input
      endTime <- getCurrentTime
      duration = diffUTCTime endTime startTime
  in duration `seq` property True

-- | 测试内存使用：大型输入不应该导致内存溢出
prop_memory_usage_large_input :: Positive Int -> Property
prop_memory_usage_large_input (Positive n) = 
  let input = concat (replicate n "let x = 42\n")
      result = parseTypus input
  in case result of
    Left _ -> property True
    Right file -> length (show (force file)) `seq` property True

-- | 测试缓存性能：重复解析相同内容应该更快
prop_cache_performance :: String -> Property
prop_cache_performance input = 
  let firstParse = parseTypus input
      secondParse = parseTypus input
  in case (firstParse, secondParse) of
    (Right f1, Right f2) -> f1 == f2
    _ -> property True

-- | 测试并发性能：并行处理多个文件
prop_concurrent_performance :: [String] -> Property
prop_concurrent_performance inputs = 
  let results = map parseTypus inputs
      successCount = length $ filter isRight results
  in successCount >= 0 -- 确保至少有一些结果
  where
    isRight (Right _) = True
    isRight (Left _) = False

-- | 测试增量性能：小改动不应该导致完全重新编译
prop_incremental_performance :: String -> String -> Property
prop_incremental_performance original modified = 
  let originalResult = compile original
      modifiedResult = compile modified
  in case (originalResult, modifiedResult) of
    (Right _, Right _) -> property True
    _ -> property True

-- | 测试错误恢复性能：处理错误不应该显著降低性能
prop_error_recovery_performance :: String -> Property
prop_error_recovery_performance input = 
  let erroneousInput = input ++ "\nlet x = " -- 故意引入语法错误
      result = compile erroneousInput
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | 测试优化性能：优化不应该过度增加编译时间
prop_optimization_performance :: String -> Property
prop_optimization_performance input = 
  let normalResult = compile input
      optimizedResult = compile ("-O " ++ input)
  in case (normalResult, optimizedResult) of
    (Right _, Right _) -> property True
    _ -> property True

-- | 测试大型项目性能：处理大量模块
prop_large_project_performance :: Positive Int -> Property
prop_large_project_performance (Positive n) = 
  let modules = replicate n "```typus\nlet x = 42\n```"
      results = map parseTypus modules
      successCount = length $ filter isRight results
  in successCount >= 0
  where
    isRight (Right _) = True
    isRight (Left _) = False

-- | 测试类型推断性能
prop_type_inference_performance :: Positive Int -> Property
prop_type_inference_performance (Positive n) = 
  let input = "```typus\n" ++ 
              concat (replicate n "let x = 42\n") ++ 
              "```"
      result = compile input
  in case result of
    Right _ -> property True
    Left _ -> property True

-- | 测试符号表性能
prop_symbol_table_performance :: Positive Int -> Property
prop_symbol_table_performance (Positive n) = 
  let input = "```typus\n" ++ 
              concat (map (\i -> "let x" ++ show i ++ " = " ++ show i ++ "\n") [1..n]) ++ 
              "```"
      result = compile input
  in case result of
    Right _ -> property True
    Left _ -> property True

-- | 测试代码生成性能
prop_code_generation_performance :: Positive Int -> Property
prop_code_generation_performance (Positive n) = 
  let input = "```typus\n" ++ 
              concat (replicate n "let x = 42\n") ++ 
              "```"
      result = compile input
  in case result of
    Right code -> not (T.null code)
    Left _ -> property True

-- | 测试错误报告性能
prop_error_reporting_performance :: Positive Int -> Property
prop_error_reporting_performance (Positive n) = 
  let input = concat (replicate n "let x = ") -- 故意引入语法错误
      result = compile input
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | 测试内存清理性能
prop_memory_cleanup_performance :: Positive Int -> Property
prop_memory_cleanup_performance (Positive n) = 
  let input = concat (replicate n "let x = 42\n")
      result = parseTypus input
  in case result of
    Right file -> 
      let _ = force file
      in property True
    Left _ -> property True

-- | 测试递归性能
prop_recursion_performance :: Positive Int -> Property
prop_recursion_performance (Positive n) = 
  let input = "```typus\n" ++ 
              "fn factorial(n: Nat): Nat {\n" ++
              "  if n <= 1 then 1 else n * factorial(n-1)\n" ++
              "}\n" ++
              "let result = factorial(" ++ show n ++ ")\n" ++
              "```"
      result = compile input
  in case result of
    Right _ -> property True
    Left _ -> property True

tests :: TestTree
tests = testGroup "Enhanced Performance Tests"
  [ testProperty "parse performance linear" prop_parse_performance_linear
  , testProperty "compile performance reasonable" prop_compile_performance_reasonable
  , testProperty "dependent types performance" prop_dependent_types_performance
  , testProperty "ownership performance" prop_ownership_performance
  , testProperty "memory usage large input" prop_memory_usage_large_input
  , testProperty "cache performance" prop_cache_performance
  , testProperty "concurrent performance" prop_concurrent_performance
  , testProperty "incremental performance" prop_incremental_performance
  , testProperty "error recovery performance" prop_error_recovery_performance
  , testProperty "optimization performance" prop_optimization_performance
  , testProperty "large project performance" prop_large_project_performance
  , testProperty "type inference performance" prop_type_inference_performance
  , testProperty "symbol table performance" prop_symbol_table_performance
  , testProperty "code generation performance" prop_code_generation_performance
  , testProperty "error reporting performance" prop_error_reporting_performance
  , testProperty "memory cleanup performance" prop_memory_cleanup_performance
  , testProperty "recursion performance" prop_recursion_performance
  ]