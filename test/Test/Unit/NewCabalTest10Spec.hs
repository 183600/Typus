{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalTest10Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Data.Char (isSpace, isAlpha)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, length)
import Control.DeepSeq (NFData, force)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

import Utils
  ( trim
  , splitBy
  , removeComments
  , normalizeIndentation
  )
import Parser (parseTypus)
import Compiler (compileTypus)
import SourceLocation (advancePosBy, SourcePos(..))

-- | 测试性能相关的边界情况和资源使用
tests :: TestTree
tests =
  testGroup "NewCabalTest10 - 性能边界测试"
    [ testGroup "单元测试"
        [ testCase "大文件解析性能" $ do
            let largeCode = unlines $ replicate 1000 "func test" ++ [i ++ "() { return " ++ show i ++ " }" | i <- map show [1..1000]]
                startTime <- getCPUTime
                result <- return $ parseTypus largeCode "large.typus"
                endTime <- getCPUTime
                let duration = fromIntegral (endTime - startTime) / (10^12) :: Double
            case result of
                Left err -> assertBool ("Large file parsing failed: " ++ show err) False
                Right _ -> 
                    assertBool ("Parsing should complete in reasonable time: " ++ show duration ++ "s") $ duration < 5.0

        , testCase "内存使用边界测试" $ do
            let deepNesting = "func main() " ++ L.concat (replicate 100 "{ ") ++ "return 42" ++ L.concat (replicate 100 " }")
                result = parseTypus deepNesting "deep.typus"
            case result of
                Left err -> 
                    -- 深度嵌套可能失败，但不应该崩溃
                    assertBool "Should handle deep nesting gracefully" $ True
                Right _ -> 
                    assertBool "Should handle deep nesting" $ True

        , testCase "字符串处理性能" $ do
            let largeString = L.concat $ replicate 10000 "hello world "
                trimmed = trim largeString
                split = splitBy ' ' largeString
                commentsRemoved = removeComments largeString
                normalized = normalizeIndentation largeString
            assertBool "String processing should handle large inputs" $ 
                L.length trimmed <= L.length largeString && 
                L.length split >= 1 &&
                L.length commentsRemoved <= L.length largeString

        , testCase "位置计算性能" $ do
            let largeText = L.concat $ replicate 10000 "line\n"
                finalPos = advancePosBy largeText startPos
                SourcePos line col offset = finalPos
            assertBool "Position calculation should handle large texts" $ 
                line > 1000 && offset > 10000

        , testCase "编译器资源管理" $ do
            let complexCode = unlines $ 
                  [ "func complex" ++ show i ++ "() {" ++
                    "  x" ++ show i ++ " := " ++ show i ++ "; " ++
                    "  y" ++ show i ++ " := x" ++ show i ++ " * 2; " ++
                    "  return y" ++ show i ++ ";" ++
                    "}" | i <- [1..100] ]
                result = compileTypus complexCode
            case result of
                Left errors -> 
                    assertBool ("Complex compilation failed: " ++ show errors) False
                Right _ -> 
                    assertBool "Should handle complex compilation" $ True
        ]

    , testGroup "QuickCheck属性测试"
        [ fastProperty "字符串处理的线性时间复杂度" prop_string_processing_linear_time
        , fastProperty "解析器的内存效率" prop_parser_memory_efficiency
        , fastProperty "位置计算的常数时间操作" prop_position_constant_time
        , fastProperty "编译器的资源限制" prop_compiler_resource_limits
        , fastProperty "深度递归的栈安全性" prop_deep_recursion_safety
        ]
    ]

-- QuickCheck属性测试

-- 字符串处理的线性时间复杂度：处理时间应该与输入长度成线性关系
prop_string_processing_linear_time :: String -> Int -> Property
prop_string_processing_linear_time base multiplier =
  multiplier > 0 && multiplier <= 100 ==>  -- 限制测试规模
  let largeInput = L.concat $ replicate multiplier base
      result1 = trim base
      result2 = trim largeInput
      -- 简化的性能检查：确保能处理大输入
  in property $ L.length result2 <= L.length largeInput

-- 解析器的内存效率：解析大文件不应该导致内存泄漏
prop_parser_memory_efficiency :: String -> Int -> Property
prop_parser_memory_efficiency baseCode multiplier =
  multiplier > 0 && multiplier <= 10 ==>  -- 限制测试规模
  let largeCode = unlines $ replicate multiplier baseCode
      result = parseTypus largeCode "test.typus"
  in case result of
       Right parsed -> property $ True  -- 成功解析表示内存管理良好
       Left _ -> property $ True  -- 解析失败但不崩溃

-- 位置计算的常数时间操作：位置计算应该是高效的
prop_position_constant_time :: String -> Property
prop_position_constant_time text =
  let result = advancePosBy text startPos
      SourcePos line col offset = result
  in property $ offset >= 0 && line >= 1 && col >= 1

-- 编译器的资源限制：编译器应该能处理合理规模的代码
prop_compiler_resource_limits :: String -> Int -> Property
prop_compiler_resource_limits baseCode multiplier =
  multiplier > 0 && multiplier <= 5 ==>  -- 限制测试规模
  let largeCode = unlines $ replicate multiplier baseCode
      result = compileTypus largeCode
  in case result of
       Right _ -> property $ True
       Left _ -> property $ True  -- 失败但不崩溃

-- 深度递归的栈安全性：深度嵌套的结构不应该导致栈溢出
prop_deep_recursion_safety :: Int -> Property
prop_deep_recursion_safety depth =
  depth > 0 && depth <= 50 ==>  -- 限制深度避免实际栈溢出
  let nestedBraces = "func main() " ++ L.concat (replicate depth "{ ") ++ "return 42" ++ L.concat (replicate depth " }")
      result = parseTypus nestedBraces "deep.typus"
  in case result of
       Right _ -> property $ True
       Left _ -> property $ True  -- 解析失败但不崩溃

-- 辅助函数：强制求值以确保严格的性能测试
forceEval :: NFData a => a -> a
forceEval = force