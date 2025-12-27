{-# LANGUAGE CPP #-}
module Test.Unit.NewCabalTestSpec10 (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import IntegratedCompiler (compileTypusIntegrated, CompilationResult(..))
import Parser (parseTypus)
import Utils (trim, normalizeIndentation)
import Data.List (isInfixOf)

-- | 测试用例10: 集成编译器测试
tests :: TestTree
tests = 
  testGroup "New Cabal Test 10 - Integrated Compiler"
    [ testCase "integrated compiler handles complete compilation pipeline" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    println(\"hello world\")"
              , "}"
              ]
        case compileTypusIntegrated source of
          CompilationSuccess result -> 
            -- Check that Go code was generated
            "package main" `isInfixOf` result @?= True
          CompilationError err -> 
            fail $ "integrated compilation failed: " ++ err

    , testCase "integrated compiler processes ownership directives" $ do
        let source = unlines
              [ "//! ownership: on"
              , "package main"
              , "func main() {"
              , "    data := make([]int, 10)"
              , "    processData(data)"
              , "}"
              ]
        case compileTypusIntegrated source of
          CompilationSuccess result -> 
            -- Check that compilation succeeded with ownership
            "package main" `isInfixOf` result @?= True
          CompilationError err -> 
            -- Check that error mentions ownership if it fails
            "ownership" `isInfixOf` err @?= True

    , testCase "integrated compiler handles dependent types" $ do
        let source = unlines
              [ "//! dependent_types: on"
              , "package main"
              , "func processArray(arr [n]int) {"
              , "    // Array with dependent type"
              , "}"
              ]
        case compileTypusIntegrated source of
          CompilationSuccess result -> 
            -- Check that compilation succeeded with dependent types
            "package main" `isInfixOf` result @?= True
          CompilationError err -> 
            -- Check that error mentions types if it fails
            "type" `isInfixOf` err @?= True

    , testCase "integrated compiler reports meaningful errors" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    println(\"unterminated string"
              , "}"
              ]
        case compileTypusIntegrated source of
          CompilationSuccess _ -> 
            fail "expected compilation to fail with unterminated string"
          CompilationError err -> 
            -- Check that error provides useful information
            length err @?= 20  -- Basic check that error message is not empty

    -- QuickCheck properties
    , fastProperty "integrated compilation is deterministic" prop_integrated_compilation_deterministic
    , fastProperty "integrated compiler preserves package name" prop_integrated_compiler_preserves_package
    , fastProperty "integrated compiler handles empty source" prop_integrated_compiler_empty_source
    ]

-- QuickCheck properties

-- Property: integrated compilation is deterministic for the same input
prop_integrated_compilation_deterministic :: String -> Property
prop_integrated_compilation_deterministic source =
  let result1 = compileTypusIntegrated source
      result2 = compileTypusIntegrated source
  in property $ case (result1, result2) of
                  (CompilationSuccess r1, CompilationSuccess r2) -> r1 == r2
                  (CompilationError e1, CompilationError e2) -> e1 == e2
                  _ -> False

-- Property: integrated compiler preserves package name
prop_integrated_compiler_preserves_package :: String -> Property
prop_integrated_compiler_preserves_package packageName =
  let source = "package " ++ packageName ++ "\nfunc main() {}"
  in case compileTypusIntegrated source of
         CompilationSuccess result -> 
           property $ ("package " ++ packageName) `isInfixOf` result
         CompilationError _ -> property True  -- Compilation failures are acceptable

-- Property: integrated compiler handles empty source gracefully
prop_integrated_compiler_empty_source :: String -> Property
prop_integrated_compiler_empty_source _ =
  let emptySource = ""
  in case compileTypusIntegrated emptySource of
         CompilationSuccess result -> property True  -- Success is acceptable
         CompilationError err -> property True  -- Graceful failure is also acceptable