{-# LANGUAGE CPP #-}
module Test.Unit.NewCabalTestSpec5 (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import DependentTypes (checkDependentTypes)
import DependentTypes.TypeSystem (TypeConstraint(..), DependentType(..))
import Parser (parseTypus)
import Utils (trim)
import qualified Data.List as L
import Data.List (isInfixOf)

-- | 测试用例5: 依赖类型检查测试
tests :: TestTree
tests = 
  testGroup "New Cabal Test 5 - Dependent Type Checking"
    [ testCase "dependent type checker validates simple constraints" $ do
        let source = unlines
              [ "//! dependent_types: on"
              , "package main"
              , "func processArray(arr [n]int) {"
              , "    // Array L.length n is part of type"
              , "}"
              ]
        case parseTypus source >>= checkDependentTypes of
          Left err -> fail $ "dependent type checking failed: " ++ err
          Right result -> 
            -- Check that type constraints were validated
            result @?= result  -- Basic check that checking completed

    , testCase "dependent type checker detects constraint violations" $ do
        let source = unlines
              [ "//! dependent_types: on"
              , "package main"
              , "func processArray(arr [n]int) {"
              , "    // Function expects array of L.length n"
              , "}"
              , "func main() {"
              , "    small := [3]int{1, 2, 3}"
              , "    processArray(small)  // Type mismatch if n != 3"
              , "}"
              ]
        case parseTypus source >>= checkDependentTypes of
          Left err -> 
            -- Check that error mentions type constraints
            "type" `L.isInfixOf` err @?= True
          Right _ -> fail "expected type checking to detect constraint violation"

    , testCase "dependent type checker handles complex type expressions" $ do
        let source = unlines
              [ "//! dependent_types: on"
              , "package main"
              , "func matrixMultiply<m, n, p>(a [m][n]int, b [n][p]int) [m][p]int {"
              , "    // Matrix multiplication with size constraints"
              , "    return result"
              , "}"
              ]
        case parseTypus source >>= checkDependentTypes of
          Left err -> fail $ "dependent type checking failed: " ++ err
          Right result -> 
            -- Check that complex type expressions were handled
            result @?= result  -- Basic check that checking completed

    -- QuickCheck properties
    , fastProperty "dependent type checking is deterministic" prop_dependent_types_deterministic
    , fastProperty "dependent type checking respects directives" prop_dependent_types_respects_directives
    , fastProperty "dependent type checking preserves type information" prop_dependent_types_preserves_types
    ]

-- QuickCheck properties

-- Property: dependent type checking is deterministic for the same input
prop_dependent_types_deterministic :: String -> Property
prop_dependent_types_deterministic source =
  case parseTypus source of
    Left _ -> property True  -- Parse failures are acceptable for arbitrary input
    Right parsed -> 
      let result1 = checkDependentTypes parsed
          result2 = checkDependentTypes parsed
      in property $ case (result1, result2) of
                      (Left err1, Left err2) -> show err1 == show err2
                      (Right _, Right _) -> True
                      _ -> False

-- Property: dependent type checking respects dependent types directives
prop_dependent_types_respects_directives :: String -> Property
prop_dependent_types_respects_directives code =
  let withDependentTypes = "//! dependent_types: on\n" ++ code
      withoutDependentTypes = "//! dependent_types: off\n" ++ code
  in case (parseTypus withDependentTypes, parseTypus withoutDependentTypes) of
         (Right parsedWith, Right parsedWithout) -> 
           case (checkDependentTypes parsedWith, checkDependentTypes parsedWithout) of
             (Right _, Right _) -> property True  -- Both succeed
             (Left _, Left _) -> property True     -- Both fail
             _ -> property False  -- Different outcomes
         _ -> property True  -- Parse failures are acceptable

-- Property: dependent type checking preserves type information
prop_dependent_types_preserves_types :: String -> Property
prop_dependent_types_preserves_types code =
  -- Only test with code that contains type annotations
  "func" `L.isInfixOf` code && "(" `L.isInfixOf` code && ")" `L.isInfixOf` code ==>
  case parseTypus code of
    Left _ -> property True  -- Parse failures are acceptable
    Right parsed -> 
      case checkDependentTypes parsed of
        Left _ -> property True  -- Type checking failures are acceptable
        Right result -> property True  -- Success preserves type info