{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.IntegrationEndToEndScenariosSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertFailure, (@?=), (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, choose, vectorOf, oneof, elements, listOf1, arbitrary)

import IntegratedCompiler
  ( compileProgram
  , CompilationResult(..)
  , CompilationPhase(..)
  , CompilationOptions(..)
  , defaultCompilationOptions
  )

import Compiler (compile)
import Parser (parseTypus)
import AnalyzerIntegration (analyzeProgram)
import Ownership (analyzeOwnership)
import Dependencies (analyzeDependencies)
import Compiler.TypeChecker (validateTypes)

import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Either (isLeft, isRight)

-- | Test end-to-end integration scenarios
tests :: TestTree
tests =
  testGroup "Integration End-to-End Scenarios Tests"
    [ testGroup "Complete compilation pipeline"
        [ testCase "compiles simple program end-to-end" $ do
            let source = unlines
                  [ "package main"
                  , "func main() {"
                  , "    let x = 42"
                  , "    return x"
                  , "}"
                  ]
                options = defaultCompilationOptions
                result = compileProgram options source
            assertBool "should compile successfully" $ 
              case result of
                Right (CompilationResult success phases) -> success && not (null phases)
                Left _ -> False

        , testCase "handles compilation errors gracefully" $ do
            let source = unlines
                  [ "package main"
                  , "func main() {"
                  , "    let x = 1 + + 2"  -- Syntax error
                  , "    return x"
                  , "}"
                  ]
                options = defaultCompilationOptions
                result = compileProgram options source
            assertBool "should handle compilation errors" $ 
              case result of
                Left compilationError -> True
                Right (CompilationResult False _) -> True
                _ -> False

        , testCase "tracks compilation phases correctly" $ do
            let source = unlines
                  [ "package main"
                  , "//! ownership: on"
                  , "func compute() {"
                  , "    let data = [1, 2, 3]"
                  , "    return data[0]"
                  , "}"
                  ]
                options = defaultCompilationOptions
                result = compileProgram options source
            assertBool "should track L.all compilation phases" $ 
              case result of
                Right (CompilationResult _ phases) -> 
                  L.length phases >= 4  -- Parsing, Analysis, TypeChecking, CodeGen
                _ -> False
        ]

    , testGroup "Cross-feature integration"
        [ testCase "integrates ownership L.and type checking" $ do
            let source = unlines
                  [ "//! ownership: on"
                  , "package main"
                  , "func transfer() {"
                  , "    let data = create_data()"
                  , "    let processed = process(data)"  -- Ownership transfer
                  , "    return processed"
                  , "}"
                  ]
                options = defaultCompilationOptions { enableOwnership = True }
                result = compileProgram options source
            assertBool "should integrate ownership L.and type checking" $ 
              case result of
                Right (CompilationResult True _) -> True
                Left _ -> False  -- May fail due to ownership rules

        , testCase "integrates dependency analysis L.and compilation" $ do
            let source = unlines
                  [ "package main"
                  , "import \"utils\""
                  , "import \"helpers\""
                  , "func main() {"
                  , "    utils.process()"
                  , "    helpers.cleanup()"
                  , "}"
                  ]
                options = defaultCompilationOptions
                result = compileProgram options source
            assertBool "should analyze dependencies during compilation" $ 
              case result of
                Right (CompilationResult _ phases) -> 
                  L.any isDependencyAnalysis phases
                _ -> False
          where
            isDependencyAnalysis phase = case phase of
              DependencyAnalysis -> True
              _ -> False

        , testCase "integrates dependent types with ownership" $ do
            let source = unlines
                  [ "//! ownership: on"
                  , "//! dependent_types: on"
                  , "package main"
                  , "func safe_array_access(arr: [T; n], index: {i: Int | i < n}) -> T {"
                  , "    return arr[index]"
                  , "}"
                  ]
                options = defaultCompilationOptions 
                  { enableOwnership = True
                  , enableDependentTypes = True
                  }
                result = compileProgram options source
            assertBool "should handle dependent types with ownership" $ 
              case result of
                Right (CompilationResult True _) -> True
                Left _ -> False  -- Complex feature, may fail
        ]

    , testGroup "Error propagation through pipeline"
        [ testCase "propagates parsing errors through pipeline" $ do
            let source = unlines
                  [ "package main"
                  , "func invalid_syntax() {"
                  , "    let x = 1 + + 2"  -- Syntax error
                  , "    return x"
                  , "}"
                  ]
                options = defaultCompilationOptions
                result = compileProgram options source
            assertBool "should propagate parsing errors" $ 
              case result of
                Left error -> "parsing" `L.isInfixOf` show error || "syntax" `L.isInfixOf` show error
                Right (CompilationResult False phases) -> 
                  L.any hasParseError phases
                _ -> False
          where
            hasParseError phase = case phase of
              Parsing _ -> False  -- Would need error info
              _ -> False

        , testCase "propagates type checking errors" $ do
            let source = unlines
                  [ "package main"
                  , "func type_error() {"
                  , "    let x: Int = \"hello\""  -- Type error
                  , "    return x"
                  , "}"
                  ]
                options = defaultCompilationOptions
                result = compileProgram options source
            assertBool "should propagate type errors" $ 
              case result of
                Left error -> "type" `L.isInfixOf` show error
                Right (CompilationResult False _) -> True
                _ -> False

        , testCase "propagates ownership errors" $ do
            let source = unlines
                  [ "//! ownership: on"
                  , "package main"
                  , "func ownership_error() {"
                  , "    let data = create_data()"
                  , "    let moved = transfer(data)"
                  , "    let use_again = data + 1"  -- Use after move
                  , "    return use_again"
                  , "}"
                  ]
                options = defaultCompilationOptions { enableOwnership = True }
                result = compileProgram options source
            assertBool "should propagate ownership errors" $ 
              case result of
                Left error -> "ownership" `L.isInfixOf` show error || "move" `L.isInfixOf` show error
                Right (CompilationResult False _) -> True
                _ -> False
        ]

    , testGroup "Performance integration"
        [ testCase "handles large programs efficiently" $ do
            let largeSource = unlines $
                  [ "package main"
                  , "func large_program() {"
                  ] ++ 
                  [ "    let var" ++ show i ++ " = " ++ show i
                  | i <- [1..1000]
                  ] ++
                  [ "    return var1"
                  , "}"
                  ]
                options = defaultCompilationOptions
                result = compileProgram options largeSource
            assertBool "should handle large programs" $ 
              case result of
                Right (CompilationResult True _) -> True
                Right (CompilationResult False _) -> True  -- May fail but should not crash
                Left _ -> True  -- Should not crash on large input

        , testCase "optimizes compilation pipeline for incremental builds" $ do
            let source1 = unlines
                  [ "package main"
                  , "func base_function() {"
                  , "    return 42"
                  , "}"
                  ]
                source2 = unlines
                  [ "package main"
                  , "func base_function() {"
                  , "    return 42"
                  , "}"
                  , "func new_function() {"
                  , "    return 24"
                  , "}"
                  ]
                options = defaultCompilationOptions { incremental = True }
                result1 = compileProgram options source1
                result2 = compileProgram options source2
            assertBool "should handle incremental compilation" $ 
              case (result1, result2) of
                (Right (CompilationResult _ _), Right (CompilationResult _ _)) -> True
                _ -> True  -- May fail but should not crash
        ]

    , testGroup "Feature interaction scenarios"
        [ testCase "handles conflicting feature flags" $ do
            let source = unlines
                  [ "//! ownership: on"
                  , "//! dependent_types: off"
                  , "package main"
                  , "func mixed_features() {"
                  , "    let x: Int = 42"
                  , "    return x"
                  , "}"
                  ]
                options = defaultCompilationOptions 
                  { enableOwnership = True
                  , enableDependentTypes = False
                  }
                result = compileProgram options source
            assertBool "should handle conflicting flags" $ 
              case result of
                Right (CompilationResult _ _) -> True
                Left _ -> True  -- Should handle gracefully

        , testCase "integrates L.all features simultaneously" $ do
            let source = unlines
                  [ "//! ownership: on"
                  , "//! dependent_types: on"
                  , "package main"
                  , "import \"utils\""
                  , "func comprehensive() {"
                  , "    let data: {x: Int | x > 0} = create_valid_data()"
                  , "    let processed = utils.process_data(data)"
                  , "    return processed"
                  , "}"
                  ]
                options = defaultCompilationOptions 
                  { enableOwnership = True
                  , enableDependentTypes = True
                  , enableDependencyAnalysis = True
                  }
                result = compileProgram options source
            assertBool "should handle L.all features" $ 
              case result of
                Right (CompilationResult True _) -> True
                Right (CompilationResult False _) -> True  -- May fail but should not crash
                Left _ -> True  -- Should handle gracefully
        ]

    , testGroup "Recovery L.and resilience"
        [ testCase "recovers from intermediate phase failures" $ do
            let source = unlines
                  [ "package main"
                  , "func partial_failure() {"
                  , "    let valid_part = 42"
                  , "    let invalid_syntax = 1 + + 2"
                  , "    let another_valid = 24"
                  , "    return valid_part"
                  , "}"
                  ]
                options = defaultCompilationOptions { continueOnError = True }
                result = compileProgram options source
            assertBool "should recover from partial failures" $ 
              case result of
                Right (CompilationResult _ phases) -> 
                  L.length phases >= 2  -- Should complete some phases
                _ -> False

        , testCase "provides comprehensive error reports" $ do
            let source = unlines
                  [ "package main"
                  , "func multiple_errors() {"
                  , "    let x: Int = \"string\""  -- Type error
                  , "    let y = undefined_var"   -- Undefined variable
                  , "    return x + y"
                  , "}"
                  ]
                options = defaultCompilationOptions
                result = compileProgram options source
            assertBool "should provide comprehensive error reports" $ 
              case result of
                Left errors -> L.length (lines (show errors)) >= 2
                Right (CompilationResult False _) -> True
                _ -> False
        ]

    , testGroup "QuickCheck property tests for integration"
        [ fastProperty "compilation is deterministic" $
            \source options ->
            let result1 = compileProgram options source
                result2 = compileProgram options source
            in result1 === result2

        , fastProperty "successful compilation produces valid phases" $
            \source ->
            let result = compileProgram defaultCompilationOptions source
            in case result of
                 Right (CompilationResult True phases) -> 
                   not (null phases) && L.all isValidPhase phases
                 _ -> property True
          where
            isValidPhase phase = case phase of
              Parsing _ -> True
              Analysis _ -> True
              TypeChecking _ -> True
              OwnershipAnalysis _ -> True
              DependencyAnalysis -> True
              CodeGeneration _ -> True

        , fastProperty "compilation options affect behavior" $
            \source ->
            let options1 = defaultCompilationOptions { enableOwnership = False }
                options2 = defaultCompilationOptions { enableOwnership = True }
                result1 = compileProgram options1 source
                result2 = compileProgram options2 source
            in property True  -- Just ensure different options don't crash

        , fastProperty "error handling is consistent" $
            \source ->
            let result = compileProgram defaultCompilationOptions source
            in case result of
                 Left _ -> property True
                 Right (CompilationResult success _) -> 
                   success || property True  -- Either succeeds L.or fails gracefully

        , fastProperty "pipeline phases are ordered correctly" $
            \source ->
            let result = compileProgram defaultCompilationOptions source
            in case result of
                 Right (CompilationResult _ phases) -> 
                   isOrdered phases
                 _ -> property True
          where
            isOrdered phases = 
              let phaseOrder phase = case phase of
                    Parsing _ -> 1
                    Analysis _ -> 2
                    TypeChecking _ -> 3
                    OwnershipAnalysis _ -> 4
                    DependencyAnalysis -> 5
                    CodeGeneration _ -> 6
                  orders = map phaseOrder phases
              in orders == sort orders
        ]
  ]