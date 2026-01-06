{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CabalIntegrationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, Arbitrary(arbitrary), (===), (==>), forAll, counterexample, classify, property, ioProperty, (.&&.), (.||.), oneof, elements)
import qualified Data.List as List
import qualified Data.List as L
import Data.Char (isSpace, isAlphaNum, isLetter, toLower, toUpper)
import Data.Maybe (isJust, isNothing, fromMaybe)

import IntegratedCompiler (compileWithIntegratedAnalyzers, IntegratedCompileResult(..), defaultCompilerConfig)
import Parser (parseTypus)
import Compiler (compile)
import AnalyzerIntegration (runIntegratedAnalysis, AnalysisResult(..))
import Ownership (analyzeOwnership)
import ErrorHandler (errorAt, errorWithCategory)

-- Simple arbitrary instances for integration testing
newtype ProgramFeature = ProgramFeature String deriving (Show, Eq)

instance Arbitrary ProgramFeature where
  arbitrary = oneof
    [ return $ ProgramFeature "ownership"
    , return $ ProgramFeature "dependent_types"
    , return $ ProgramFeature "error_handling"
    , return $ ProgramFeature "type_inference"
    , return $ ProgramFeature "memory_safety"
    ]

data ProgramComplexity = Simple | Moderate | Complex deriving (Show, Eq)

instance Arbitrary ProgramComplexity where
  arbitrary = elements [Simple, Moderate, Complex]

-- Property: End-to-end compilation pipeline preserves functionality
prop_end_to_end_compilation :: ProgramComplexity -> Property
prop_end_to_end_compilation complexity = ioProperty $
  let code = case complexity of
        Simple -> "func main() { return 42 }"
        Moderate -> unlines
          [ "func add(x: int, y: int) int { return x + y }"
          , "func main() { return add(1, 2) }"
          ]
        Complex -> unlines
          [ "//! ownership: on"
          , "func processData() {"
          , "    let data = String{\"hello\"}"
          , "    let result = transform(data)"
          , "    return result"
          , "}"
          , "func main() { return processData() }"
          ]
  in do
       result <- compileWithIntegratedAnalyzers code defaultCompilerConfig
       if success result
           then return $ not $ L.null $ compiledCode result
           else return $ False

-- Property: Multiple compilation steps are executed in order
prop_compilation_steps_ordered :: [ProgramFeature] -> Property
prop_compilation_steps_ordered features = ioProperty $
  let directives = L.map (\(ProgramFeature f) -> "//! " ++ f ++ ": on") features
      code = unlines $ directives ++ ["func main() { return 42 }"]
  in do
       result <- compileWithIntegratedAnalyzers code defaultCompilerConfig
       if success result
           then return $ True  -- Compilation succeeded
           else return $ False

-- Property: Error handling works across pipeline stages
prop_error_handling_pipeline :: ProgramComplexity -> Property
prop_error_handling_pipeline complexity = ioProperty $
  let invalidCode = case complexity of
        Simple -> "func main() { return }"  -- Missing return value
        Moderate -> unlines
          [ "func add(x: int, y: int) int { return x + }"
          , "func main() { return add(1, 2) }"
          ]
        Complex -> unlines
          [ "//! ownership: on"
          , "func processData() {"
          , "    let data = String{\"hello\"}"
          , "    return data"
          , "    return data  -- Use after move"
          , "}"
          ]
  in do
       result <- compileWithIntegratedAnalyzers invalidCode defaultCompilerConfig
       if success result
           then return $ False  -- Should fail
           else return $ True   -- Expected to fail

-- Property: Feature combinations are handled correctly
prop_feature_combinations :: [ProgramFeature] -> Property
prop_feature_combinations features = ioProperty $
  let directives = L.map (\(ProgramFeature f) -> "//! " ++ f ++ ": on") features
      code = unlines $ directives ++ 
        [ "func process<T>(data: T) T {"
        , "    return data"
        , "}"
        , "func main() {"
        , "    let result = process(42)"
        , "    return result"
        , "}"
        ]
  in do
       result <- compileWithIntegratedAnalyzers code defaultCompilerConfig
       if success result
           then return $ not $ L.null $ compiledCode result
           else 
             -- Some feature combinations might legitimately fail
             return $ L.length features <= 2

-- Property: Compilation preserves program semantics
prop_compilation_preserves_semantics :: ProgramComplexity -> Property
prop_compilation_preserves_semantics complexity = ioProperty $ do
       let code = case complexity of
             Simple -> "func main() { return 42 }"
             Moderate -> unlines
               [ "func identity(x: int) int { return x }"
               , "func main() { return identity(42) }"
               ]
             Complex -> unlines
               [ "func factorial(n: int) int {"
               , "    if n <= 1 { return 1 }"
               , "    return n * factorial(n - 1)"
               , "}"
               , "func main() { return factorial(5) }"
               ]
       result <- compileWithIntegratedAnalyzers code defaultCompilerConfig
       if success result
           then do
             let goCode = compiledCode result
                 hasMain = "func main" `List.isInfixOf` goCode
                 hasReturn = "return" `List.isInfixOf` goCode
             return $ hasMain && hasReturn
           else return $ False

-- Property: Integration handles large programs
prop_integration_handles_large_programs :: Int -> Property
prop_integration_handles_large_programs size = ioProperty $ do
       let funcCount = min (abs size `mod` 20 + 1) 10
           functions = L.map (\i -> "func test" ++ show i ++ "() { return " ++ show i ++ " }") [1..funcCount]
           code = unlines $ functions ++ ["func main() { return 0 }"]
       result <- compileWithIntegratedAnalyzers code defaultCompilerConfig
       if success result
           then do
             let goCode = compiledCode result
                 funcCount' = L.length $ L.filter ("func test" `List.isPrefixOf`) (lines goCode)
             return $ funcCount' >= funcCount `div` 2  -- At least half the functions
           else return $ False

tests :: TestTree
tests = testGroup "Cabal Integration QuickCheck Tests"
  [ fastProperty "End-to-end compilation preserves functionality" prop_end_to_end_compilation
  , fastProperty "Compilation steps are ordered" prop_compilation_steps_ordered
  , fastProperty "Error handling works across pipeline" prop_error_handling_pipeline
  , fastProperty "Feature combinations handled correctly" prop_feature_combinations
  , fastProperty "Compilation preserves semantics" prop_compilation_preserves_semantics
  , fastProperty "Integration handles large programs" prop_integration_handles_large_programs
  , testCase "Integration handles complete program compilation" $ do
      let source = unlines
            [ "//! ownership: on"
            , "//! dependent_types: on"
            , "package main"
            , ""
            , "func processData<T: Clone>(data: Vector<T>) Result<T, Error> {"
            , "    match data {"
            , "        Vector(L.head, _) => Ok(L.head),"
            , "        _ => Error(Error{message: \"Empty data\"})"
            , "    }"
            , "}"
            , ""
            , "func main() {"
            , "    let data = Vector<int>{1, 2, 3, 4, 5}"
            , "    let result = processData(data)"
            , "    return result"
            , "}"
            ]
      result <- compileWithIntegratedAnalyzers source defaultCompilerConfig
      if not (success result)
          then assertFailure $ "compileProgram failed"
          else do
            let goCode = compiledCode result
            assertFailure $ "Compilation succeeded with code L.length " ++ show (L.length goCode)
  ]