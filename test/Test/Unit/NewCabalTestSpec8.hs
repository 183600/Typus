{-# LANGUAGE CPP #-}
module Test.Unit.NewCabalTestSpec8 (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import GoToolchain (generateGoCode, GoCodeConfig(..))
import Compiler.IR (IRModule(..), IRFunction(..))
import Parser (parseTypus)
import Utils (trim)
import qualified Data.List as L
import Data.List (isInfixOf)

-- | 测试用例8: Go工具链代码生成测试
tests :: TestTree
tests = 
  testGroup "New Cabal Test 8 - Go Toolchain Code Generation"
    [ testCase "Go code generation produces valid package declaration" $ do
        let config = GoCodeConfig { packageName = "main" }
            irModule = IRModule { irFunctions = [] }
            goCode = generateGoCode config irModule
        "package main" `L.isInfixOf` goCode @?= True

    , testCase "Go code generation handles function declarations" $ do
        let config = GoCodeConfig { packageName = "main" }
            func = IRFunction { functionName = "test", functionBody = "return 42" }
            irModule = IRModule { irFunctions = [func] }
            goCode = generateGoCode config irModule
        "func test()" `L.isInfixOf` goCode @?= True

    , testCase "Go code generation includes imports when needed" $ do
        let config = GoCodeConfig { packageName = "main" }
            func = IRFunction { functionName = "printHello", functionBody = "fmt.Println(\"hello\")" }
            irModule = IRModule { irFunctions = [func] }
            goCode = generateGoCode config irModule
        "import" `L.isInfixOf` goCode @?= True

    , testCase "Go code generation preserves function bodies" $ do
        let config = GoCodeConfig { packageName = "main" }
            func = IRFunction { functionName = "calculate", functionBody = "return 2 + 2" }
            irModule = IRModule { irFunctions = [func] }
            goCode = generateGoCode config irModule
        "return 2 + 2" `L.isInfixOf` goCode @?= True

    -- QuickCheck properties
    , fastProperty "code generation is deterministic" prop_code_generation_deterministic
    , fastProperty "generated code contains package declaration" prop_generated_code_contains_package
    , fastProperty "code generation preserves function count" prop_code_generation_preserves_function_count
    ]

-- QuickCheck properties

-- Property: code generation is deterministic for the same input
prop_code_generation_deterministic :: String -> Property
prop_code_generation_deterministic packageName =
  let config = GoCodeConfig { packageName = packageName }
      irModule = IRModule { irFunctions = [] }
      result1 = generateGoCode config irModule
      result2 = generateGoCode config irModule
  in property $ result1 === result2

-- Property: generated code contains package declaration
prop_generated_code_contains_package :: String -> Property
prop_generated_code_contains_package packageName =
  not (null packageName) ==> 
  let config = GoCodeConfig { packageName = packageName }
      irModule = IRModule { irFunctions = [] }
      goCode = generateGoCode config irModule
  in property $ ("package " ++ packageName) `L.isInfixOf` goCode

-- Property: code generation preserves function count
prop_code_generation_preserves_function_count :: String -> Property
prop_code_generation_preserves_function_count funcName =
  not (null funcName) ==> 
  let func = IRFunction { functionName = funcName, functionBody = "" }
      irModule = IRModule { irFunctions = [func] }
      config = GoCodeConfig { packageName = "main" }
      goCode = generateGoCode config irModule
  in property $ funcName `L.isInfixOf` goCode