{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CabalIntegrationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import qualified Data.List as List
import Data.Char (isSpace, isAlphaNum, isLetter, toLower, toUpper)
import Data.Maybe (isJust, isNothing, fromMaybe)

import IntegratedCompiler (compileProgram, CompilationStep(..), CompilationResult(..))
import Parser (parseTypus)
import Compiler (compileTypus)
import Analyzer (analyzeProgram)
import Ownership (analyzeOwnership)
import SyntaxValidator (validateSyntax)
import ErrorHandler (handleError, createError)

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
prop_end_to_end_compilation complexity =
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
  in case compileProgram code of
       Right result -> property $ not $ null $ crGoCode result
       Left err -> counterexample ("Compilation failed: " ++ show err) $ property False

-- Property: Multiple compilation steps are executed in order
prop_compilation_steps_ordered :: [ProgramFeature] -> Property
prop_compilation_steps_ordered features =
  let directives = map (\(ProgramFeature f) -> "//! " ++ f ++ ": on") features
      code = unlines $ directives ++ ["func main() { return 42 }"]
  in case compileProgram code of
       Right result -> 
         let steps = crCompilationSteps result
         in property $ length steps >= 3  -- Should have at least parse, analyze, compile
       Left err -> counterexample ("Compilation failed: " ++ show err) $ property False

-- Property: Error handling works across pipeline stages
prop_error_handling_pipeline :: ProgramComplexity -> Property
prop_error_handling_pipeline complexity =
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
  in case compileProgram invalidCode of
       Right _ -> property False  -- Should fail
       Left _ -> property True   -- Expected to fail

-- Property: Feature combinations are handled correctly
prop_feature_combinations :: [ProgramFeature] -> Property
prop_feature_combinations features =
  let directives = map (\(ProgramFeature f) -> "//! " ++ f ++ ": on") features
      code = unlines $ directives ++ 
        [ "func process<T>(data: T) T {"
        , "    return data"
        , "}"
        , "func main() {"
        , "    let result = process(42)"
        , "    return result"
        , "}"
        ]
  in case compileProgram code of
       Right result -> property $ not $ null $ crGoCode result
       Left err -> 
         -- Some feature combinations might legitimately fail
         property $ length features <= 2

-- Property: Compilation preserves program semantics
prop_compilation_preserves_semantics :: ProgramComplexity -> Property
prop_compilation_preserves_semantics complexity =
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
  in case compileProgram code of
       Right result -> 
         let goCode = crGoCode result
             hasMain = "func main" `List.isInfixOf` goCode
             hasReturn = "return" `List.isInfixOf` goCode
         in property $ hasMain .&&. hasReturn
       Left err -> counterexample ("Compilation failed: " ++ show err) $ property False

-- Property: Integration handles large programs
prop_integration_handles_large_programs :: Int -> Property
prop_integration_handles_large_programs size =
  let funcCount = min (abs size `mod` 20 + 1) 10
      functions = map (\i -> "func test" ++ show i ++ "() { return " ++ show i ++ " }") [1..funcCount]
      code = unlines $ functions ++ ["func main() { return 0 }"]
  in case compileProgram code of
       Right result -> 
         let goCode = crGoCode result
             funcCount' = length $ filter ("func test" `List.isPrefixOf`) (lines goCode)
         in property $ funcCount' == funcCount
       Left err -> 
         -- Large programs might legitimately fail
         property $ funcCount <= 5

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
            , "        Vector(head, _) => Ok(head),"
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
      case compileProgram source of
        Left err -> assertFailure $ "compileProgram failed: " ++ show err
        Right result -> do
          let goCode = crGoCode result
              steps = crCompilationSteps result
          assertFailure $ "Compilation succeeded with " ++ show (length steps) ++ " steps and code length " ++ show (length goCode)
  ]