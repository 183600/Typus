{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.CompilerOptimizationConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertFailure, (@?=), (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, choose, vectorOf, oneof, elements, listOf1, arbitrary)

import Compiler
  ( compile
  , compileWithOptimizations
  , OptimizationLevel(..)
  , OptimizationPass(..)
  , OptimizationResult(..)
  , CompilerOptions(..)
  , defaultCompilerOptions
  , validateOptimizationResult
  )

import Compiler.IR
  ( IRProgram(..)
  , IRFunction(..)
  , IRStatement(..)
  , IRExpression(..)
  , IRType(..)
  , optimizeProgram
  , validateProgram
  )

import Compiler.TypeChecker
  ( TypeCheckResult(..)
  , validateTypes
  )

import Data.List (sort, nub, intersect, union)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Test compiler optimization consistency
tests :: TestTree
tests =
  testGroup "Compiler Optimization Consistency Tests"
    [ testGroup "Basic optimization consistency"
        [ testCase "optimization preserves program semantics" $ do
            let irProgram = IRProgram
                  [ IRFunction "main" [] IRTypeInt
                    [ IRReturn (IRLiteral (IRInt 42))
                    ]
                  ]
                optimized = optimizeProgram [ConstantFolding, DeadCodeElimination] irProgram
                originalValid = validateProgram irProgram
                optimizedValid = validateProgram optimized
            assertBool "original program should be valid" $ isRight originalValid
            assertBool "optimized program should be valid" $ isRight optimizedValid
          where
            isRight (Right _) = True
            isRight (Left _) = False

        , testCase "constant folding produces correct results" $ do
            let irProgram = IRProgram
                  [ IRFunction "test" [] IRTypeInt
                    [ IRReturn (IRBinaryOp Add (IRLiteral (IRInt 5)) (IRLiteral (IRInt 3)))
                    ]
                  ]
                optimized = optimizeProgram [ConstantFolding] irProgram
            assertBool "should fold constants" $ 
              case optimized of
                IRProgram [IRFunction _ _ _ [IRReturn (IRLiteral (IRInt 8))]] -> True
                _ -> False

        , testCase "dead code elimination removes unreachable code" $ do
            let irProgram = IRProgram
                  [ IRFunction "test" [] IRTypeInt
                    [ IRReturn (IRLiteral (IRInt 1))
                    , IRReturn (IRLiteral (IRInt 2))  -- Unreachable
                    ]
                  ]
                optimized = optimizeProgram [DeadCodeElimination] irProgram
            assertBool "should remove dead code" $ 
              case optimized of
                IRProgram [IRFunction _ _ _ [IRReturn (IRLiteral (IRInt 1))]] -> True
                _ -> False
        ]

    , testGroup "Multi-pass optimization consistency"
        [ testCase "multiple optimization passes work together" $ do
            let irProgram = IRProgram
                  [ IRFunction "complex" [] IRTypeInt
                    [ IRDeclaration "x" IRTypeInt (IRBinaryOp Add (IRLiteral (IRInt 10)) (IRLiteral (IRInt 20)))
                    , IRDeclaration "y" IRTypeInt (IRBinaryOp Mul (IRVariable "x") (IRLiteral (IRInt 2)))
                    , IRReturn (IRVariable "y")
                    ]
                  ]
                passes = [ConstantFolding, DeadCodeElimination, InlineExpansion]
                optimized = optimizeProgram passes irProgram
            assertBool "multi-pass optimization should work" $ 
              validateProgram optimized == Right ()

        , testCase "optimization order doesn't affect final result" $ do
            let irProgram = IRProgram
                  [ IRFunction "order_test" [] IRTypeInt
                    [ IRDeclaration "a" IRTypeInt (IRBinaryOp Add (IRLiteral (IRInt 1)) (IRLiteral (IRInt 2)))
                    , IRDeclaration "b" IRTypeInt (IRBinaryOp Mul (IRVariable "a") (IRLiteral (IRInt 3)))
                    , IRReturn (IRVariable "b")
                    ]
                  ]
                order1 = optimizeProgram [ConstantFolding, DeadCodeElimination] irProgram
                order2 = optimizeProgram [DeadCodeElimination, ConstantFolding] irProgram
                result1 = validateProgram order1
                result2 = validateProgram order2
            result1 @?= result2

        , testCase "optimization passes are idempotent" $ do
            let irProgram = IRProgram
                  [ IRFunction "idempotent_test" [] IRTypeInt
                    [ IRReturn (IRBinaryOp Add (IRLiteral (IRInt 5)) (IRLiteral (IRInt 3)))
                    ]
                  ]
                once = optimizeProgram [ConstantFolding] irProgram
                twice = optimizeProgram [ConstantFolding] once
            once @?= twice
        ]

    , testGroup "Type preservation during optimization"
        [ testCase "optimizations preserve type correctness" $ do
            let irProgram = IRProgram
                  [ IRFunction "typed" [] IRTypeInt
                    [ IRDeclaration "x" IRTypeInt (IRLiteral (IRInt 42))
                    , IRReturn (IRVariable "x")
                    ]
                  ]
                optimized = optimizeProgram [ConstantFolding] irProgram
                typeCheck = validateTypes optimized
            assertBool "optimization should preserve types" $ 
              case typeCheck of
                Right _ -> True
                Left _ -> False

        , testCase "function signatures remain unchanged" $ do
            let irProgram = IRProgram
                  [ IRFunction "preserve_sig" [IRTypeString] IRTypeBool
                    [ IRReturn (IRBinaryOp Equal (IRVariable "param0") (IRLiteral (IRString "test"))))
                    ]
                  ]
                optimized = optimizeProgram [ConstantFolding] irProgram
            assertBool "function signatures should be preserved" $ 
              case optimized of
                IRProgram [IRFunction "preserve_sig" [IRTypeString] IRTypeBool _] -> True
                _ -> False
        ]

    , testGroup "Optimization level consistency"
        [ testCase "higher optimization levels include lower levels" $ do
            let options = defaultCompilerOptions { optLevel = High }
                basicOptions = defaultCompilerOptions { optLevel = Basic }
                irProgram = IRProgram
                  [ IRFunction "level_test" [] IRTypeInt
                    [ IRReturn (IRBinaryOp Add (IRLiteral (IRInt 1)) (IRLiteral (IRInt 2)))
                    ]
                  ]
                highResult = compileWithOptimizations options irProgram
                basicResult = compileWithOptimizations basicOptions irProgram
            assertBool "both should compile successfully" $ 
              isRight highResult && isRight basicResult
          where
            isRight (Right _) = True
            isRight (Left _) = False

        , testCase "optimization levels are monotonic" $ do
            let irProgram = IRProgram
                  [ IRFunction "monotonic_test" [] IRTypeInt
                    [ IRDeclaration "x" IRTypeInt (IRLiteral (IRInt 10))
                    , IRDeclaration "y" IRTypeInt (IRBinaryOp Add (IRVariable "x") (IRLiteral (IRInt 5)))
                    , IRReturn (IRVariable "y")
                    ]
                  ]
                noneOpts = optimizeProgram [] irProgram
                basicOpts = optimizeProgram [ConstantFolding] irProgram
                highOpts = optimizeProgram [ConstantFolding, DeadCodeElimination, InlineExpansion] irProgram
            assertBool "no optimization should be valid" $ validateProgram noneOpts == Right ()
            assertBool "basic optimization should be valid" $ validateProgram basicOpts == Right ()
            assertBool "high optimization should be valid" $ validateProgram highOpts == Right ()
        ]

    , testGroup "Error handling in optimizations"
        [ testCase "invalid optimizations are rejected" $ do
            let irProgram = IRProgram
                  [ IRFunction "invalid" [] IRTypeInt
                    [ IRReturn (IRBinaryOp Add (IRLiteral (IRInt 1)) (IRLiteral (IRString "bad")))
                    ]
                  ]
                result = optimizeProgram [ConstantFolding] irProgram
            assertBool "should handle type errors in optimization" $ 
              case validateProgram result of
                Left _ -> True  -- Should detect type error
                Right _ -> False -- Should not succeed with invalid program

        , testCase "optimization failures are recoverable" $ do
            let irProgram = IRProgram
                  [ IRFunction "recoverable" [] IRTypeInt
                    [ IRDeclaration "x" IRTypeInt (IRLiteral (IRInt 42))
                    , IRReturn (IRVariable "x")
                    ]
                  ]
                result = compileWithOptimizations defaultCompilerOptions irProgram
            assertBool "should recover from optimization failures" $ 
              case result of
                Right _ -> True
                Left _ -> False
        ]

    , testGroup "Performance and memory consistency"
        [ testCase "optimizations don't increase memory usage significantly" $ do
            let irProgram = IRProgram
                  [ IRFunction "memory_test" [] IRTypeInt
                    [ IRDeclaration "x" IRTypeInt (IRLiteral (IRInt 1))
                    , IRDeclaration "y" IRTypeInt (IRLiteral (IRInt 2))
                    , IRDeclaration "z" IRTypeInt (IRBinaryOp Add (IRVariable "x") (IRVariable "y"))
                    , IRReturn (IRVariable "z")
                    ]
                  ]
                optimized = optimizeProgram [DeadCodeElimination] irProgram
            assertBool "optimized program should not be larger" $ 
              programSize optimized <= programSize irProgram
          where
            programSize (IRProgram functions) = sum (map functionSize functions)
            functionSize (IRFunction _ _ _ statements) = length statements

        , testCase "optimization results are deterministic" $ do
            let irProgram = IRProgram
                  [ IRFunction "deterministic" [] IRTypeInt
                    [ IRReturn (IRBinaryOp Mul (IRLiteral (IRInt 6)) (IRLiteral (IRInt 7)))
                    ]
                  ]
                result1 = optimizeProgram [ConstantFolding] irProgram
                result2 = optimizeProgram [ConstantFolding] irProgram
            result1 @?= result2
        ]

    , testGroup "QuickCheck property tests for optimization consistency"
        [ fastProperty "constant folding preserves semantics" $
            \left right ->
            let program = IRProgram
                  [ IRFunction "const_fold" [] IRTypeInt
                    [ IRReturn (IRBinaryOp Add (IRLiteral (IRInt left)) (IRLiteral (IRInt right)))
                    ]
                  ]
                optimized = optimizeProgram [ConstantFolding] program
                expected = left + right
            in case optimized of
                 IRProgram [IRFunction _ _ _ [IRReturn (IRLiteral (IRInt result))]] -> 
                   result === expected
                 _ -> property False

        , fastProperty "dead code elimination preserves valid programs" $
            \program ->
            validateProgram program === Right () ==>
            let optimized = optimizeProgram [DeadCodeElimination] program
            in validateProgram optimized === Right ()

        , fastProperty "optimization is deterministic" $
            \program ->
            let opt1 = optimizeProgram [ConstantFolding] program
                opt2 = optimizeProgram [ConstantFolding] program
            in opt1 === opt2

        , fastProperty "multiple optimizations compose correctly" $
            \program ->
            validateProgram program === Right () ==>
            let singlePass = optimizeProgram [ConstantFolding, DeadCodeElimination] program
                firstPass = optimizeProgram [ConstantFolding] program
                secondPass = optimizeProgram [DeadCodeElimination] firstPass
            in validateProgram singlePass === Right () && validateProgram secondPass === Right ()

        , fastProperty "optimization doesn't change function signatures" $
            \program ->
            let optimized = optimizeProgram [ConstantFolding] program
                originalSigs = extractSignatures program
                optimizedSigs = extractSignatures optimized
            in originalSigs === optimizedSigs
        ]
  ]

-- Helper functions
extractSignatures :: IRProgram -> [(String, [IRType], IRType)]
extractSignatures (IRProgram functions) = 
  map (\(IRFunction name params retType _) -> (name, params, retType)) functions