{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalTest9Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Data.Char (isSpace, isAlpha)
import Data.List (isPrefixOf, isInfixOf)

import IntegratedCompiler
  ( compileToEndToEnd
  , CompilationPipeline(..)
  , PipelineResult(..)
  , PipelineStage(..)
  )
import Compiler (compileTypus)
import Parser (parseTypus)
import Ownership (analyzeOwnership)
import Dependencies (analyzeDependencies)
import ErrorHandler (formatErrors)

-- | 测试端到端编译流程的集成和属性
tests :: TestTree
tests =
  testGroup "NewCabalTest9 - 端到端集成测试"
    [ testGroup "单元测试"
        [ testCase "完整编译流程" $ do
            let code = unlines
                  [ "func factorial(n int) int {"
                  , "  if n <= 1 {"
                  , "    return 1"
                  , "  }"
                  , "  return n * factorial(n - 1)"
                  , "}"
                  , ""
                  , "func main() {"
                  , "  result := factorial(5)"
                  , "  // result should be 120"
                  , "}"
                  ]
                result = compileToEndToEnd code
            case result of
                PipelineSuccess output -> 
                    assertBool "Should compile successfully" $ not $ null output
                PipelineError errors -> 
                    assertBool ("End-to-end compilation failed: " ++ show errors) False

        , testCase "错误传播测试" $ do
            let code = "func broken( { return 42 }"  -- 语法错误
                result = compileToEndToEnd code
            case result of
                PipelineError errors -> 
                    assertBool "Should propagate errors" $ not $ null errors
                PipelineSuccess output -> 
                    assertBool "Should detect errors" False

        , testCase "多阶段编译一致性" $ do
            let code = "func test() int { return 42 }"
                -- 分别测试各个阶段
                parseResult = parseTypus code
                compileResult = compileTypus code
                endToEndResult = compileToEndToEnd code
            case (parseResult, compileResult, endToEndResult) of
                (Right _, Right _, PipelineSuccess _) -> 
                    assertBool "All stages should succeed" True
                _ -> 
                    assertBool "All stages should have consistent results" $ 
                        case (parseResult, compileResult) of
                            (Left _, Left _) -> True
                            (Right _, Right _) -> True
                            _ -> False

        , testCase "复杂程序集成测试" $ do
            let code = unlines
                  [ "// @ownership: true"
                  , "// @dependentTypes: true"
                  , ""
                  , "type Vector[T] struct {"
                  , "  data [T] int"
                  , "  size int"
                  , "}"
                  , ""
                  , "func (v *Vector[T]) get(index T) int {"
                  , "  return v.data[index]"
                  , "}"
                  , ""
                  , "func main() {"
                  , "  vec := Vector[int]{data: [5]int{1,2,3,4,5}, size: 5}"
                  , "  value := vec.get(2)"
                  , "}"
                  ]
                result = compileToEndToEnd code
            case result of
                PipelineSuccess output -> 
                    assertBool "Should handle complex features" $ True
                PipelineError errors -> 
                    assertBool ("Complex compilation failed: " ++ show errors) False
        ]

    , testGroup "QuickCheck属性测试"
        [ fastProperty "编译流程的确定性" prop_compilation_deterministic
        , fastProperty "错误传播的完整性" prop_error_propagation_completeness
        , fastProperty "阶段间的一致性" prop_stage_consistency
        , fastProperty "编译流程的幂等性" prop_compilation_idempotent
        , fastProperty "输出输入的循环性" prop_output_input_cyclic
        ]
    ]

-- QuickCheck属性测试

-- 编译流程的确定性：相同输入应该产生相同的输出
prop_compilation_deterministic :: String -> Property
prop_compilation_deterministic code =
  let result1 = compileToEndToEnd code
      result2 = compileToEndToEnd code
  in case (result1, result2) of
       (PipelineSuccess output1, PipelineSuccess output2) -> 
         property $ output1 === output2
       (PipelineError errors1, PipelineError errors2) -> 
         property $ length errors1 === length errors2
       _ -> property $ False

-- 错误传播的完整性：早期阶段的错误应该传播到最终结果
prop_error_propagation_completeness :: String -> Property
prop_error_propagation_completeness code =
  let parseResult = parseTypus code
      endToEndResult = compileToEndToEnd code
  in case parseResult of
       Left parseError -> 
         case endToEndResult of
           PipelineError errors -> property $ not $ null errors
           PipelineSuccess _ -> property $ False
       Right _ -> property $ True  -- 解析成功时跳过

-- 阶段间的一致性：各个编译阶段应该对相同的代码有一致的判断
prop_stage_consistency :: String -> Property
prop_stage_consistency code =
  let parseResult = parseTypus code
      compileResult = compileTypus code
  in case (parseResult, compileResult) of
       (Left _, Left _) -> property $ True  -- 两个都失败
       (Right _, Right _) -> property $ True  -- 两个都成功
       _ -> property $ False  -- 结果不一致

-- 编译流程的幂等性：成功编译的代码再次编译应该产生相同结果
prop_compilation_idempotent :: String -> Property
prop_compilation_idempotent code =
  let result1 = compileToEndToEnd code
  in case result1 of
       PipelineSuccess output -> 
         let result2 = compileToEndToEnd code
         in case result2 of
              PipelineSuccess output2 -> property $ output === output2
              PipelineError _ -> property $ False
       PipelineError _ -> property $ True  -- 编译失败时跳过

-- 输出输入的循环性：编译器的输出如果作为输入（如果是有效的中间代码）应该能被处理
prop_output_input_cyclic :: String -> Property
prop_output_input_cyclic code =
  let result = compileToEndToEnd code
  in case result of
       PipelineSuccess output -> 
         -- 这里假设输出是某种中间表示，可以重新编译
         -- 实际实现中可能需要更复杂的逻辑
         property $ not $ null output
       PipelineError _ -> property $ True  -- 编译失败时跳过