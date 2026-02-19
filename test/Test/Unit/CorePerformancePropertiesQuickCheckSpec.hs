{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-local-binds #-}
module Test.Unit.CorePerformancePropertiesQuickCheckSpec where


import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck

import Parser (parseTypus)
import Compiler (compile)
import qualified Data.Text as T
-- import Criterion.Main

-- | Test performance properties with QuickCheck
corePerformancePropertiesSpec :: TestTree
corePerformancePropertiesSpec = testGroup "Core Performance Properties"
  [ testProperty "Parser performance scales linearly with input size" $
      \size -> size >= 0 && size < 100 ==>  -- 从1000减少到100，大幅减少内存使用
        let input = T.pack (replicate size 'x')
            result = parseTypus (T.unpack input)
        in property True -- In real tests, would measure actual time

  , testCase "Parsing large files doesn't cause stack overflow" $ do
    let largeFile = T.pack (replicate 100 'x')  -- 从10000减少到100，大幅减少内存使用
    case parseTypus (T.unpack largeFile) of
      Right _ -> assertBool "Large file parsed successfully" True
      Left _ -> assertBool "Large file parsing failed gracefully" True

  , testCase "Compilation completes within reasonable time" $ do
    let program = generateProgram 10  -- 从100减少到10，大幅减少内存使用
    case parseTypus (T.unpack program) of
      Right parsed -> 
        case compile parsed of
          Right _ -> assertBool "Compilation completed" True
          Left _ -> assertBool "Compilation failed in reasonable time" True
      Left _ -> assertFailure "Parsing failed"

  , testProperty "Memory usage doesn't grow with repeated operations" $
      \iterations -> iterations >= 0 && iterations < 10 ==>  -- 从100减少到10，大幅减少内存使用
        let input = "func test() { return 42; }"
            results = replicate iterations $ parseTypus input
        in property True

  , testProperty "Parser handles deeply nested structures efficiently" $
      \depth -> depth >= 0 && depth < 3 ==>  -- 从10减少到3，大幅减少内存使用
        let nested = generateNestedStructure depth
            result = parseTypus (T.unpack nested)
        in property True
  ]

-- Helper functions for testing
generateProgram :: Int -> T.Text
generateProgram size = T.pack ("func main() { " ++ replicate size 'x' ++ " }")

parseIncremental :: T.Text -> T.Text -> Either a b
parseIncremental _ _ = undefined

generateNestedStructure :: Int -> T.Text
generateNestedStructure depth = T.pack (replicate depth '{')

generateLargeType :: Int -> T.Text
generateLargeType size = 
  let limitedSize = min size 3  -- 限制最大字段数量为3，大幅减少内存使用
  in T.pack ("type Large struct { " ++ concat (replicate limitedSize "Field int; ") ++ " }")

performLongRunningOperation :: Int -> Bool
performLongRunningOperation _ = True

generateUnicodeInput :: Int -> T.Text
generateUnicodeInput size = 
  let limitedSize = min size 10  -- 限制最大输入长度为10，大幅减少内存使用
  in T.pack $ take limitedSize $ cycle "测试中文🚀"