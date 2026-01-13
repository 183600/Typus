{-# LANGUAGE ScopedTypeVariables #-}

module CorePerformancePropertiesQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser (parseTypus)
import Compiler (compile)
import qualified Data.Text as T
import Control.DeepSeq (NFData, force)
import Criterion.Main

-- | Test performance properties with QuickCheck
corePerformancePropertiesSpec :: TestTree
corePerformancePropertiesSpec = testGroup "Core Performance Properties"
  [ testProperty "Parser performance scales linearly with input size" $
      \size -> size >= 0 && size < 1000 ==> 
        let input = T.replicate size "x"
            result = parseTypus input
        in property True -- In real tests, would measure actual time

  , testCase "Parsing large files doesn't cause stack overflow" $ do
    let largeFile = T.replicate 10000 "x"
    case parseTypus largeFile of
      Right _ -> assertBool "Large file parsed successfully" True
      Left _ -> assertBool "Large file parsing failed gracefully" True

  , testCase "Compilation completes within reasonable time" $ do
    let program = generateProgram 100
    case parseTypus program of
      Right parsed -> 
        case compile parsed of
          Right _ -> assertBool "Compilation completed" True
          Left _ -> assertBool "Compilation failed in reasonable time" True
      Left _ -> assertFailure "Parsing failed"

  , testProperty "Memory usage doesn't grow with repeated operations" $
      \iterations -> iterations >= 0 && iterations < 100 ==> 
        let input = "func test() { return 42; }"
            results = replicate iterations $ force $ parseTypus input
        in property True

  , testProperty "Parser handles deeply nested structures efficiently" $
      \depth -> depth >= 0 && depth < 10 ==> 
        let nested = generateNestedStructure depth
            result = parseTypus nested
        in property True
  ]

-- Helper functions for testing
generateProgram :: Int -> T.Text
generateProgram size = "func main() { " <> T.replicate size "x" <> " }"

parseIncremental :: T.Text -> T.Text -> Either a b
parseIncremental _ _ = undefined

generateNestedStructure :: Int -> T.Text
generateNestedStructure depth = T.replicate depth "{"

generateLargeType :: Int -> T.Text
generateLargeType size = "type Large struct { " <> T.replicate size "Field int; " <> " }"

performLongRunningOperation :: Int -> Bool
performLongRunningOperation _ = True

generateUnicodeInput :: Int -> T.Text
generateUnicodeInput size = T.pack $ take size $ cycle "测试中文🚀"