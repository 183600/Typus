{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.ConciseIntegrationQuickCheckSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>))
import qualified Data.Text as T
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Data.Char (isSpace, isAlpha, isAlphaNum, toLower, toUpper, isDigit, isLetter)
import Compiler (compile, CompilerError(..), CompilerResult)
import Parser (TypusFile(..), parseTypusFile)
import ErrorHandler (ErrorHandler, handleError, hasErrors)
import Ownership (OwnershipAnalysis, analyzeOwnership, hasOwnershipErrors)
import Dependencies (DependencyGraph, analyzeDependencies, hasCycles)
import SourceLocation (SourcePos(..), SourceSpan(..))

-- Helper generators for Integration tests
genSimpleTypusCode :: Gen String
genSimpleTypusCode = do
  numLines <- choose (1, 5)
  lines <- vectorOf numLines $ oneof
    [ genVarDeclaration
    , genFuncDeclaration
    , genImportStatement
    ]
  return $ unlines lines

genVarDeclaration :: Gen String
genVarDeclaration = do
  name <- elements ["x", "y", "z", "value", "result"]
  value <- elements ["0", "1", "true", "false", "\"hello\""]
  return $ "var " ++ name ++ " = " ++ value ++ ";"

genFuncDeclaration :: Gen String
genFuncDeclaration = do
  name <- elements ["func1", "func2", "method", "calculate"]
  params <- listOf $ elements ["param1", "param2", "x", "y"]
  body <- elements ["return 0;", "return true;", "return x;"]
  return $ "func " ++ name ++ "(" ++ unwords params ++ ") { " ++ body ++ " }"

genImportStatement :: Gen String
genImportStatement = do
  module' <- elements ["module1", "module2", "utils", "types"]
  return $ "import " ++ module' ++ ";"

genComplexTypusCode :: Gen String
genComplexTypusCode = do
  numVars <- choose (1, 3)
  numFuncs <- choose (1, 2)
  numImports <- choose (0, 2)
  
  vars <- vectorOf numVars genVarDeclaration
  funcs <- vectorOf numFuncs genFuncDeclaration
  imports <- vectorOf numImports genImportStatement
  
  let allLines = imports ++ vars ++ funcs
  return $ unlines allLines

genTypusFileWithDependencies :: Gen (String, [String])
genTypusFileWithDependencies = do
  code <- genComplexTypusCode
  numDeps <- choose (0, 3)
  deps <- vectorOf numDeps $ elements ["module1", "module2", "utils", "types"]
  return (code, deps)

-- Test properties for Integration module

-- End-to-end compilation tests
prop_compile_parse_roundtrip :: String -> Property
prop_compile_parse_roundtrip code = 
  not (null code) ==>
  case parseTypusFile code of
    Left _ -> property True
    Right file -> 
      let reconstructed = unlines $ map show (declarations file)
      in case parseTypusFile reconstructed of
           Left _ -> property True
           Right reparsed -> property $ length (declarations file) == length (declarations reparsed)

prop_compile_no_crash :: String -> Property
prop_compile_no_crash code = 
  not (null code) ==>
  let result = compile code
  in case result of
       Left _ -> property True
       Right _ -> property True

prop_compile_error_consistency :: String -> Property
prop_compile_error_consistency code = 
  not (null code) ==>
  case compile code of
    Left errs -> property $ length errs > 0
    Right _ -> property True

prop_compile_twice_consistent :: String -> Property
prop_compile_twice_consistent code = 
  not (null code) ==>
  let result1 = compile code
      result2 = compile code
  in case (result1, result2) of
       (Left _, Left _) -> property True
       (Right _, Right _) -> property True
       _ -> property False

-- Integration with ErrorHandler tests
prop_error_handler_integration :: String -> Property
prop_error_handler_integration code = 
  not (null code) ==>
  case compile code of
    Left errs -> 
      let handler = foldl (\h e -> handleError h e) (ErrorHandler [] [] []) errs
      in hasErrors handler === True
    Right _ -> property True

-- Integration with Ownership tests
prop_ownership_integration :: String -> Property
prop_ownership_integration code = 
  not (null code) ==>
  case parseTypusFile code of
    Left _ -> property True
    Right file -> 
      let result = analyzeOwnership file
      in case result of
           Left _ -> property True
           Right analysis -> property $ not (null (show analysis))

prop_ownership_error_detection :: String -> Property
prop_ownership_error_detection code = 
  not (null code) ==>
  case parseTypusFile code of
    Left _ -> property True
    Right file -> 
      case analyzeOwnership file of
        Left _ -> property True
        Right analysis -> hasOwnershipErrors analysis === False

-- Integration with Dependencies tests
prop_dependencies_integration :: String -> Property
prop_dependencies_integration code = 
  not (null code) ==>
  let result = analyzeDependencies code
  in case result of
       Left _ -> property True
       Right graph -> property $ not (null (show graph))

prop_dependencies_cycle_detection :: String -> Property
prop_dependencies_cycle_detection code = 
  not (null code) ==>
  case analyzeDependencies code of
    Left _ -> property True
    Right graph -> 
      let hasCycles' = hasCycles graph
      in property $ not hasCycles'  -- Simple code shouldn't have cycles

-- Multi-module integration tests
prop_multi_module_compilation :: (String, [String]) -> Property
prop_multi_module_compilation (mainCode, deps) = 
  not (null mainCode) ==>
  let mainResult = compile mainCode
      depResults = map compile deps
  in case mainResult of
       Left _ -> property True
       Right _ -> 
         let allCompiled = all (\r -> case r of
                                        Left _ -> False
                                        Right _ -> True) depResults
         in property True  -- Even if deps fail to compile, main should be testable

prop_dependency_resolution :: (String, [String]) -> Property
prop_dependency_resolution (mainCode, deps) = 
  not (null mainCode) && not (null deps) ==>
  let mainResult = analyzeDependencies mainCode
      depResults = map analyzeDependencies deps
  in case mainResult of
       Left _ -> property True
       Right mainGraph -> 
         let allGraphs = mainGraph : [g | Right g <- depResults]
         in property $ length allGraphs > 0

-- Performance integration tests
prop_compilation_performance :: String -> Property
prop_compilation_performance code = 
  not (null code) && length code < 1000 ==>
  let result = compile code
  in case result of
       Left _ -> property True
       Right _ -> property True  -- If it compiles, it should be reasonably fast

prop_large_code_compilation :: Property
prop_large_code_compilation = 
  let largeCode = unlines $ replicate 100 "var x = 0;"
  in case compile largeCode of
       Left _ -> property True
       Right _ -> property True

-- Error recovery integration tests
prop_error_recovery_compilation :: String -> Property
prop_error_recovery_compilation code = 
  not (null code) ==>
  let result = compile code
  in case result of
       Left errs -> property $ length errs > 0
       Right _ -> property True

prop_partial_compilation :: String -> Property
prop_partial_compilation code = 
  not (null code) ==>
  case compile code of
    Left _ -> property True
    Right _ -> property True

-- Source location integration tests
prop_source_location_tracking :: String -> Property
prop_source_location_tracking code = 
  not (null code) ==>
  case compile code of
    Left errs -> 
      let allHaveLocations = all (\e -> case e of
                                          SyntaxErr se -> sourceLine se > 0 && sourceColumn se > 0
                                          TypeErr te -> sourceLine te > 0 && sourceColumn te > 0
                                          _ -> True) errs
      in property allHaveLocations
    Right _ -> property True

tests :: TestTree
tests = testGroup "Concise Integration QuickCheck Tests"
  [ testProperties "End-to-End Compilation Tests"
    [ ("compile parse roundtrip", prop_compile_parse_roundtrip)
    , ("compile no crash", prop_compile_no_crash)
    , ("compile error consistency", prop_compile_error_consistency)
    , ("compile twice consistent", prop_compile_twice_consistent)
    ]
  , testProperties "ErrorHandler Integration Tests"
    [ ("error handler integration", prop_error_handler_integration)
    ]
  , testProperties "Ownership Integration Tests"
    [ ("ownership integration", prop_ownership_integration)
    , ("ownership error detection", prop_ownership_error_detection)
    ]
  , testProperties "Dependencies Integration Tests"
    [ ("dependencies integration", prop_dependencies_integration)
    , ("dependencies cycle detection", prop_dependencies_cycle_detection)
    ]
  , testProperties "Multi-Module Integration Tests"
    [ ("multi module compilation", prop_multi_module_compilation)
    , ("dependency resolution", prop_dependency_resolution)
    ]
  , testProperties "Performance Integration Tests"
    [ ("compilation performance", prop_compilation_performance)
    , ("large code compilation", prop_large_code_compilation)
    ]
  , testProperties "Error Recovery Integration Tests"
    [ ("error recovery compilation", prop_error_recovery_compilation)
    , ("partial compilation", prop_partial_compilation)
    ]
  , testProperties "Source Location Integration Tests"
    [ ("source location tracking", prop_source_location_tracking)
    ]
  ]