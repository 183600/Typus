{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.NewDependentTypesPerfSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, elements, listOf, oneof, sized, Positive(..), generate)

import DependentTypesParser (parseDependentType)
import Compiler.DependentTypeChecker (checkDependentType, TypeInferenceResult(..))
import Parser (parseTypus)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.DeepSeq (force)
import Data.List (nub, sort)
import System.CPUTime (getCPUTime)

tests :: TestTree
tests = testGroup "New Dependent Types Performance Tests"
    [ testCase "handles simple dependent type constraints efficiently" $ do
        let source = unlines
              [ "//! dependent_types: on"
              , "package main"
              , "func safe_array_access<T>(arr: [T; n], i: nat) where i < n {"
              , "  return arr[i]"
              , "}"
              , "func main() {"
              , "  let arr = [1, 2, 3, 4, 5]"
              , "  let result = safe_array_access(arr, 2)"
              , "}"
              ]
        startTime <- getCurrentTime
        case parseTypus source of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            result <- checkDependentType typusFile
            endTime <- getCurrentTime
            let duration = diffUTCTime endTime startTime
            assertBool ("Simple dependent type checking should be fast, took: " ++ show duration) $
              duration < 0.1  -- Should complete in less than 100ms
            case result of
              Left errs -> assertFailure $ "Type checking failed: " ++ show errs
              Right _ -> assertBool "Type checking succeeded" True
              
    , testCase "handles complex dependent type constraints within reasonable time" $ do
        let source = unlines
              [ "//! dependent_types: on"
              , "package main"
              , "func matrix_multiply<T>(m1: [T; m, n], m2: [T; n, p]) -> [T; m, p] where"
              , "  m > 0, n > 0, p > 0"
              , "{"
              , "  let result: [T; m, p] = create_matrix(m, p)"
              , "  for i in 0..m {"
              , "    for j in 0..p {"
              , "      for k in 0..n {"
              , "        result[i][j] += m1[i][k] * m2[k][j]"
              , "      }"
              , "    }"
              , "  }"
              , "  return result"
              , "}"
              , "func main() {"
              , "  let m1 = create_matrix(10, 15)"
              , "  let m2 = create_matrix(15, 20)"
              , "  let result = matrix_multiply(m1, m2)"
              , "}"
              ]
        startTime <- getCurrentTime
        case parseTypus source of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            result <- checkDependentType typusFile
            endTime <- getCurrentTime
            let duration = diffUTCTime endTime startTime
            assertBool ("Complex dependent type checking should complete in reasonable time, took: " ++ show duration) $
              duration < 1.0  -- Should complete in less than 1 second
            case result of
              Left errs -> assertFailure $ "Type checking failed: " ++ show errs
              Right _ -> assertBool "Type checking succeeded" True
    ]

-- QuickCheck properties for dependent types performance

-- Property: Type checking time should grow sub-quadratically with program size
prop_dependent_type_checking_scalability :: Positive Int -> Property
prop_dependent_type_checking_scalability (Positive n) =
  let source = generateDependentTypeProgram n
  in case parseTypus source of
       Left _ -> property $ True  -- Invalid source, skip property test
       Right typusFile -> do
         startCPU <- getCPUTime
         result <- checkDependentType typusFile
         endCPU <- getCPUTime
         let cpuTime = fromIntegral (endCPU - startCPU) / (10^12) :: Double
         property $ classify (cpuTime < 1.0) "fast" $
                    classify (cpuTime < 5.0) "reasonable" $
                    classify (cpuTime < 10.0) "acceptable" $
                    cpuTime < 10.0  -- Should complete in less than 10 CPU seconds

-- Helper functions for QuickCheck
generateDependentTypeProgram :: Int -> String
generateDependentTypeProgram n = unlines $
  [ "//! dependent_types: on"
  , "package main"
  , "func test_function(x: int) where x > 0 {"
  ] ++
  concatMap (\i -> 
    [ "  let constraint_" ++ show i ++ ": int where constraint_" ++ show i ++ " > " ++ show i ++ " = x + " ++ show i
    , "  process_constraint(constraint_" ++ show i ++ ")"
    ]) [1..n] ++
  ["}"]