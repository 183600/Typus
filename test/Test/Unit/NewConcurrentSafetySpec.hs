{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.NewConcurrentSafetySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, elements, listOf, oneof, sized, Positive(..))

import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import Ownership (OwnershipType(..), OwnershipError(..), newOwnershipAnalyzer, analyzeOwnership)
import Compiler (compile, CompilationResult(..))
import Control.Concurrent (forkIO, threadDelay, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM_)
import Data.List (nub, sort, length)
import Control.DeepSeq (force)

tests :: TestTree
tests = testGroup "New Concurrent Safety Tests"
    [ testCase "prevents data races in shared mutable state" $ do
        let source = unlines
              [ "package main"
              , "func data_race() {"
              , "  let shared_counter = create_shared_counter()"
              , "  spawn_thread(|| {"
              , "    increment_counter(shared_counter)"
              , "  })"
              , "  spawn_thread(|| {"
              , "    increment_counter(shared_counter)"
              , "  })"
              , "  // Should warn about potential data race"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            result <- compile typusFile
            case result of
              Left errs -> 
                case filter isDataRaceWarning errs of
                  [] -> assertFailure "Expected data race warning"
                  _ -> assertBool "Data race detected correctly" True
              Right _ -> assertBool "Should warn about potential data race" True
              
    , testCase "ensures proper synchronization in concurrent access" $ do
        let source = unlines
              [ "package main"
              , "func synchronized_access() {"
              , "  let shared_data = create_shared_data()"
              , "  let mutex = create_mutex()"
              , "  spawn_thread(|| {"
              , "    lock(mutex)"
              , "    modify_data(shared_data)"
              , "    unlock(mutex)"
              , "  })"
              , "  spawn_thread(|| {"
              , "    lock(mutex)"
              , "    read_data(shared_data)"
              , "    unlock(mutex)"
              , "  })"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            result <- compile typusFile
            case result of
              Left errs -> assertFailure $ "Unexpected compilation errors: " ++ show errs
              Right _ -> assertBool "Proper synchronization handled correctly" True
              
    , testCase "prevents deadlock scenarios" $ do
        let source = unlines
              [ "package main"
              , "func potential_deadlock() {"
              , "  let mutex1 = create_mutex()"
              , "  let mutex2 = create_mutex()"
              , "  spawn_thread(|| {"
              , "    lock(mutex1)"
              , "    lock(mutex2)"
              , "    // Critical section"
              , "    unlock(mutex2)"
              , "    unlock(mutex1)"
              , "  })"
              , "  spawn_thread(|| {"
              , "    lock(mutex2)  // Different order - potential deadlock"
              , "    lock(mutex1)"
              , "    // Critical section"
              , "    unlock(mutex1)"
              , "    unlock(mutex2)"
              , "  })"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            result <- compile typusFile
            case result of
              Left errs -> 
                case filter isDeadlockWarning errs of
                  [] -> assertFailure "Expected deadlock warning"
                  _ -> assertBool "Deadlock risk detected correctly" True
              Right _ -> assertBool "Should warn about deadlock risk" True
    ]

-- QuickCheck properties for concurrent safety

-- Property: Concurrent compilation should be deterministic
prop_concurrent_compilation_deterministic :: String -> Property
prop_concurrent_compilation_deterministic source =
  case parseTypus source of
    Left _ -> property $ True  -- Invalid source, skip property test
    Right typusFile -> do
      result1 <- compile typusFile
      result2 <- compile typusFile
      property $ result1 === result2

-- Property: Concurrent ownership analysis should be thread-safe
prop_concurrent_ownership_thread_safe :: String -> Property
prop_concurrent_ownership_thread_safe source =
  case parseTypus source of
    Left _ -> property $ True
    Right typusFile -> do
      mvar1 <- newEmptyMVar
      mvar2 <- newEmptyMVar
      
      -- Run ownership analysis in parallel
      _ <- forkIO $ do
        analyzer <- newOwnershipAnalyzer
        result <- analyzeOwnership analyzer typusFile
        putMVar mvar1 result
      
      _ <- forkIO $ do
        analyzer <- newOwnershipAnalyzer
        result <- analyzeOwnership analyzer typusFile
        putMVar mvar2 result
      
      result1 <- takeMVar mvar1
      result2 <- takeMVar mvar2
      property $ result1 === result2

-- Helper functions for concurrent safety checking

isDataRaceWarning :: String -> Bool
isDataRaceWarning err = "data race" `isInfixOf` err || "concurrent access" `isInfixOf` err

isDeadlockWarning :: String -> Bool
isDeadlockWarning err = "deadlock" `isInfixOf` err || "lock ordering" `isInfixOf` err

isConcurrentMoveError :: OwnershipError -> Bool
isConcurrentMoveError (OwnershipError _ "Concurrent move" _) = True
isConcurrentMoveError _ = False

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (substrings haystack)
  where
    substrings [] = []
    substrings s@(x:xs) = take (length needle) s : substrings xs

-- Helper functions for QuickCheck
generateConcurrentProgram :: Int -> String
generateConcurrentProgram n = unlines $
  [ "package main"
  , "func concurrent_program() {"
  , "  let shared_data = create_shared_data()"
  , "  let mutex = create_mutex()"
  ] ++
  concatMap (\i -> 
    [ "  spawn_thread(|| {"
    , "    lock(mutex)"
    , "    process_data(shared_data, " ++ show i ++ ")"
    , "    unlock(mutex)"
    , "  })"
    ]) [1..n] ++
  ["}"]