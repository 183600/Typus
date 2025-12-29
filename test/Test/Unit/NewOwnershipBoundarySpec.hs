{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.NewOwnershipBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, elements, listOf, oneof, sized, Positive(..))

import Ownership (OwnershipType(..), OwnershipError(..), newOwnershipAnalyzer, analyzeOwnership)
import Ownership.Common.Types (OwnershipTransfer(..))
import Parser (parseTypus)
import Data.List (nub)
import Control.DeepSeq (force)

tests :: TestTree
tests = testGroup "New Ownership Boundary Tests"
    [ testCase "handles empty ownership scope" $ do
        let source = unlines
              [ "//! ownership: on"
              , "package main"
              , "func main() {"
              , "  // No variables declared"
              , "  return"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            analyzer <- newOwnershipAnalyzer
            result <- analyzeOwnership analyzer typusFile
            case result of
              Left errs -> assertFailure $ "Ownership analysis failed: " ++ show errs
              Right _ -> assertBool "Empty scope handled correctly" True
              
    , testCase "detects double move error" $ do
        let source = unlines
              [ "//! ownership: on"
              , "package main"
              , "func main() {"
              , "  let x = create_resource()"
              , "  use_resource(x)"
              , "  use_resource(x)  // Should error: x already moved"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            analyzer <- newOwnershipAnalyzer
            result <- analyzeOwnership analyzer typusFile
            case result of
              Left errs -> 
                case filter isDoubleMoveError errs of
                  [] -> assertFailure "Expected double move error"
                  _ -> assertBool "Double move detected correctly" True
              Right _ -> assertFailure "Expected ownership error"
              
    , testCase "handles nested ownership scopes" $ do
        let source = unlines
              [ "//! ownership: on"
              , "package main"
              , "func main() {"
              , "  let x = create_resource()"
              , "  {"
              , "    let y = x  // Move x to inner scope"
              , "    use_resource(y)"
              , "  }"
              , "  // x should not be accessible here"
              , "  use_resource(x)  // Should error"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            analyzer <- newOwnershipAnalyzer
            result <- analyzeOwnership analyzer typusFile
            case result of
              Left errs -> 
                case filter isUseAfterMoveError errs of
                  [] -> assertFailure "Expected use after move error"
                  _ -> assertBool "Use after move in nested scope detected" True
              Right _ -> assertFailure "Expected ownership error"
    ]

-- Helper functions for error detection
isDoubleMoveError :: OwnershipError -> Bool
isDoubleMoveError (OwnershipError _ "Double move" _) = True
isDoubleMoveError _ = False

isUseAfterMoveError :: OwnershipError -> Bool
isUseAfterMoveError (OwnershipError _ "Use after move" _) = True
isUseAfterMoveError _ = False

-- QuickCheck properties for ownership analysis

-- Property: Ownership analysis should be deterministic
prop_ownership_analysis_deterministic :: String -> Property
prop_ownership_analysis_deterministic source =
  case parseTypus source of
    Left _ -> property $ True  -- Invalid source, skip property test
    Right typusFile -> do
      analyzer1 <- newOwnershipAnalyzer
      analyzer2 <- newOwnershipAnalyzer
      result1 <- analyzeOwnership analyzer1 typusFile
      result2 <- analyzeOwnership analyzer2 typusFile
      property $ result1 === result2

-- Helper functions for QuickCheck
generateLargeProgram :: Int -> String
generateLargeProgram n = unlines $
  [ "//! ownership: on"
  , "package main"
  , "func main() {"
  ] ++
  concatMap (\i -> 
    [ "  let x" ++ show i ++ " = create_resource()"
    , "  use_resource(x" ++ show i ++ ")"
    ]) [1..n] ++
  ["}"]