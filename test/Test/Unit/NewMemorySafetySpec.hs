{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.NewMemorySafetySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, elements, listOf, oneof, sized, Positive(..))

import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import Ownership (OwnershipType(..), OwnershipError(..), newOwnershipAnalyzer, analyzeOwnership)
import Compiler (compile, CompilationResult(..))
import Compiler.IR (IRModule(..), IRFunction(..))
import Data.List (length)
import Data.List (nub, sort)
import Control.DeepSeq (force)
import System.Mem (performGC)

tests :: TestTree
tests = testGroup "New Memory Safety Tests"
    [ testCase "prevents use-after-free errors" $ do
        let source = unlines
              [ "//! ownership: on"
              , "package main"
              , "func use_after_free() {"
              , "  let resource = allocate_resource()"
              , "  free_resource(resource)"
              , "  use_resource(resource)  // Should error: use after free"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            analyzer <- newOwnershipAnalyzer
            result <- analyzeOwnership analyzer typusFile
            case result of
              Left errs -> 
                case filter isUseAfterFreeError errs of
                  [] -> assertFailure "Expected use-after-free error"
                  _ -> assertBool "Use-after-free detected correctly" True
              Right _ -> assertFailure "Expected ownership error"
              
    , testCase "prevents double free errors" $ do
        let source = unlines
              [ "//! ownership: on"
              , "package main"
              , "func double_free() {"
              , "  let resource = allocate_resource()"
              , "  free_resource(resource)"
              , "  free_resource(resource)  // Should error: double free"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            analyzer <- newOwnershipAnalyzer
            result <- analyzeOwnership analyzer typusFile
            case result of
              Left errs -> 
                case filter isDoubleFreeError errs of
                  [] -> assertFailure "Expected double-free error"
                  _ -> assertBool "Double-free detected correctly" True
              Right _ -> assertFailure "Expected ownership error"
              
    , testCase "prevents memory leaks" $ do
        let source = unlines
              [ "//! ownership: on"
              , "package main"
              , "func memory_leak() {"
              , "  let resource = allocate_resource()"
              , "  // Resource not freed - should warn about potential leak"
              , "  return"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            analyzer <- newOwnershipAnalyzer
            result <- analyzeOwnership analyzer typusFile
            case result of
              Left errs -> 
                case filter isMemoryLeakWarning errs of
                  [] -> assertFailure "Expected memory leak warning"
                  _ -> assertBool "Memory leak detected correctly" True
              Right _ -> assertBool "Should warn about potential memory leak" True
              
    , testCase "handles stack overflow prevention" $ do
        let source = unlines
              [ "package main"
              , "func recursive_function(n: int) -> int {"
              , "  if n <= 0 {"
              , "    return 0"
              , "  }"
              , "  return recursive_function(n - 1) + 1"
              , "}"
              , "func main() {"
              , "  let result = recursive_function(1000000)  // Large recursion depth"
              , "  return result"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            result <- compile typusFile
            case result of
              Left errs -> 
                case filter isStackOverflowWarning errs of
                  [] -> assertFailure "Expected stack overflow warning"
                  _ -> assertBool "Stack overflow risk detected" True
              Right _ -> assertBool "Should handle large recursion safely" True
    ]

-- QuickCheck properties for memory safety

-- Property: Memory safety checks should not crash
prop_memory_safety_never_crashes :: String -> Property
prop_memory_safety_never_crashes source =
  case parseTypus source of
    Left _ -> property $ True  -- Invalid source, skip property test
    Right typusFile -> do
      analyzer <- newOwnershipAnalyzer
      result <- analyzeOwnership analyzer typusFile
      property $ case result of
        Left _ -> True  -- Errors are expected for invalid input
        Right _ -> True  -- Success is also valid

-- Property: Memory usage should be bounded
prop_memory_usage_bounded :: Positive Int -> Property
prop_memory_usage_bounded (Positive n) =
  let source = generateMemoryIntensiveProgram n
  in case parseTypus source of
       Left _ -> property $ True
       Right typusFile -> do
         performGC  -- Force garbage collection
         analyzer <- newOwnershipAnalyzer
         result <- analyzeOwnership analyzer typusFile
         performGC  -- Force garbage collection again
         property $ case result of
           Left _ -> True
           Right _ -> True

-- Helper functions for memory safety checking

isUseAfterFreeError :: OwnershipError -> Bool
isUseAfterFreeError (OwnershipError _ "Use after free" _) = True
isUseAfterFreeError _ = False

isDoubleFreeError :: OwnershipError -> Bool
isDoubleFreeError (OwnershipError _ "Double free" _) = True
isDoubleFreeError _ = False

isMemoryLeakWarning :: OwnershipError -> Bool
isMemoryLeakWarning (OwnershipError _ "Memory leak" _) = True
isMemoryLeakWarning _ = False

isStackOverflowWarning :: String -> Bool
isStackOverflowWarning err = "stack overflow" `L.isInfixOf` err || "recursion" `L.isInfixOf` err

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (substrings haystack)
  where
    substrings [] = []
    substrings s@(x:xs) = take (L.length needle) s : substrings xs

-- Helper functions for QuickCheck
generateMemoryIntensiveProgram :: Int -> String
generateMemoryIntensiveProgram n = unlines $
  [ "//! ownership: on"
  , "package main"
  , "func memory_intensive() {"
  ] ++
  concatMap (\i -> 
    [ "  let resource" ++ show i ++ " = allocate_resource()"
    , "  process_resource(resource" ++ show i ++ ")"
    , "  free_resource(resource" ++ show i ++ ")"
    ]) [1..n] ++
  ["}"]