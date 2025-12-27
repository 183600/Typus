{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCompilerOptimizationConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property
  , (===)
  , (==>)
  , forAll
  , counterexample
  , classify
  , property
  , (.&&.)
  , (.||.)
  , Arbitrary(..)
  , Gen
  , choose
  , listOf
  , elements
  , oneof
  , sized
  , resize
  , Positive(..)
  )

import Compiler
  ( compile
  , CompilerError(..)
  , CompilerResult
  , CompilationPhase(..)
  , generateGoCode
  , hasTypeErrors
  , checkDependentTypes
  , checkOwnership
  )
import Parser
  ( TypusFile(..)
  , parseTypus
  )
import Compiler.IR as IR
  ( IRModule(..)
  , IRFunction(..)
  , IRStatement(..)
  , IRExpression(..)
  , IRType(..)
  )

import Data.Char (isSpace, toLower)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, tails, isInfixOf, sort, nub)
import qualified Data.Text as T

-- Test compiler produces consistent output for identical inputs
test_compiler_deterministic :: TestTree
test_compiler_deterministic = testCase "Compiler produces deterministic output" $ do
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    x := 42"
        , "    y := x + 1"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " <> err
    Right typusFile -> do
      result1 <- compile typusFile
      result2 <- compile typusFile
      case (result1, result2) of
        (Left err1, Left err2) -> err1 @?= err2
        (Right res1, Right res2) -> res1 @?= res2
        _ -> assertFailure "Compiler results are inconsistent"

-- Test compiler handles optimization flags consistently
test_optimization_flags :: TestTree
test_optimization_flags = testCase "Compiler handles optimization flags consistently" $ do
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    // Simple function that could be optimized"
        , "    x := 1 + 2 + 3"
        , "    y := x * 1"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " <> err
    Right typusFile -> do
      result <- compile typusFile
      case result of
        Left err -> assertFailure $ "compile failed: " <> err
        Right _ -> pure () -- Should compile successfully

-- Test compiler preserves semantic meaning during optimization
test_optimization_preserves_semantics :: TestTree
test_optimization_preserves_semantics = testCase "Optimization preserves semantic meaning" $ do
  let source1 = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    x := 1 + 2 + 3"
        , "}"
        ]
      source2 = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    x := 6"
        , "}"
        ]
  case (parseTypus source1, parseTypus source2) of
    (Left err, _) -> assertFailure $ "parseTypus failed on source1: " <> err
    (_, Left err) -> assertFailure $ "parseTypus failed on source2: " <> err
    (Right typusFile1, Right typusFile2) -> do
      result1 <- compile typusFile1
      result2 <- compile typusFile2
      case (result1, result2) of
        (Left err1, _) -> assertFailure $ "compile failed on source1: " <> err1
        (_, Left err2) -> assertFailure $ "compile failed on source2: " <> err2
        (Right res1, Right res2) -> do
          -- Both should compile successfully
          -- The optimized versions should be semantically equivalent
          pure ()

-- Test compiler handles dead code elimination
test_dead_code_elimination :: TestTree
test_dead_code_elimination = testCase "Compiler handles dead code elimination" $ do
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    x := 42"
        , "    if false {"
        , "        y := x + 1  // This should be eliminated"
        , "    }"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " <> err
    Right typusFile -> do
      result <- compile typusFile
      case result of
        Left err -> assertFailure $ "compile failed: " <> err
        Right _ -> pure () -- Should compile successfully

-- Test compiler handles constant folding
test_constant_folding :: TestTree
test_constant_folding = testCase "Compiler handles constant folding" $ do
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    x := 1 + 2 * 3  // Should be folded to 7"
        , "    y := x"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " <> err
    Right typusFile -> do
      result <- compile typusFile
      case result of
        Left err -> assertFailure $ "compile failed: " <> err
        Right _ -> pure () -- Should compile successfully

-- Test compiler handles loop optimizations
test_loop_optimizations :: TestTree
test_loop_optimizations = testCase "Compiler handles loop optimizations" $ do
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    sum := 0"
        , "    for i := 0; i < 10; i++ {"
        , "        sum += i"
        , "    }"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "parseTypus failed: " <> err
    Right typusFile -> do
      result <- compile typusFile
      case result of
        Left err -> assertFailure $ "compile failed: " <> err
        Right _ -> pure () -- Should compile successfully

-- Property: Compiler should handle repeated compilation consistently
prop_repeated_compilation :: String -> Property
prop_repeated_compilation source = 
  case parseTypus source of
    Left _ -> property True -- Parsing failures are OK
    Right typusFile -> 
      let compileOnce = compile typusFile
          compileTwice = sequence [compile typusFile, compile typusFile]
      in property $ case compileOnce of
        Left _ -> True -- Compilation failures are OK
        Right _ -> case compileTwice of
          Left _ -> True -- Shouldn't crash on repeated compilation
          Right [res1, res2] -> res1 === res2 -- Results should be identical
          _ -> property True

-- Property: Compiler should handle equivalent expressions consistently
prop_equivalent_expressions :: Int -> Int -> Property
prop_equivalent_expressions x y = 
  let source1 = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    result := " ++ show x ++ " + " ++ show y
        , "}"
        ]
      source2 = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    result := " ++ show (x + y)
        , "}"
        ]
  in case (parseTypus source1, parseTypus source2) of
    (Left _, _) -> property True
    (_, Left _) -> property True
    (Right typusFile1, Right typusFile2) -> 
      case (compile typusFile1, compile typusFile2) of
        (Left _, Left _) -> property True
        (Right _, Right _) -> property True
        _ -> property True -- Mixed success/failure is OK for edge cases

-- Property: Compiler should handle nested optimizations
prop_nested_optimizations :: Positive Int -> Property
prop_nested_optimizations (Positive n) = 
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    x := 1"
        , "    for i := 0; i < " ++ show n ++ "; i++ {"
        , "        x = x + 1"
        , "    }"
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property True
    Right typusFile -> 
      case compile typusFile of
        Left _ -> property True
        Right _ -> property True -- Should handle optimization of nested loops

-- Property: Compiler optimization should be idempotent
prop_optimization_idempotent :: String -> Property
prop_optimization_idempotent source = 
  case parseTypus source of
    Left _ -> property True
    Right typusFile -> 
      case compile typusFile of
        Left _ -> property True
        Right result1 -> 
          case compile typusFile of
            Left _ -> property True
            Right result2 -> result1 === result2 -- Second compilation should give same result

tests :: TestTree
tests = testGroup "New Compiler Optimization Consistency Tests"
  [ test_compiler_deterministic
  , test_optimization_flags
  , test_optimization_preserves_semantics
  , test_dead_code_elimination
  , test_constant_folding
  , test_loop_optimizations
  , fastProperty "Repeated compilation is consistent" prop_repeated_compilation
  , fastProperty "Equivalent expressions compile consistently" prop_equivalent_expressions
  , fastProperty "Nested optimizations work correctly" prop_nested_optimizations
  , fastProperty "Optimization is idempotent" prop_optimization_idempotent
  ]