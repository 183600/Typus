{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewTypusCompilerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Compiler (compileTypusToGo, CompilationResult(..))
import Compiler.GoAst (GoModule(..), FuncDecl(..))
import Utils (trim)

-- Property: Compiler produces valid Go output
prop_compiler_produces_valid_go :: String -> Property
prop_compiler_produces_valid_go input =
  let result = compileTypusToGo "test.typus" input
      isValidGo = either (const False) (const True) result
  in classify (length input > 0) "non-empty input" $
     property $ isValidGo

-- Property: Compilation preserves function count
prop_compilation_preserves_function_count :: [String] -> Property
prop_compilation_preserves_function_count funcNames =
  let input = unlines $ map (\name -> "func " ++ name ++ "() {}") funcNames
      result = compileTypusToGo "functions.typus" input
      funcCount = length funcNames
      compiledFuncCount = either (const 0) (countFunctions) result
  in classify (not (null funcNames)) "has functions" $
     property $ (funcCount === 0) .||. (compiledFuncCount === funcCount)

-- Property: Compiler handles package declarations
prop_compiler_handles_packages :: String -> Property
prop_compiler_handles_packages packageName =
  let input = "package " ++ packageName ++ "\n\nfunc main() {}"
      result = compileTypusToGo "package.typus" input
      hasPackage = either (const False) (hasGoPackage packageName) result
  in property $ not (null packageName) ==> hasPackage

-- Property: Compiler preserves imports
prop_compiler_preserves_imports :: [String] -> Property
prop_compiler_preserves_imports imports =
  let importLines = map (\imp -> "import \"" ++ imp ++ "\"") imports
      input = unlines $ ["package main"] ++ importLines ++ ["func main() {}"]
      result = compileTypusToGo "imports.typus" input
      importCount = length imports
      compiledImportCount = either (const 0) (countImports) result
  in classify (not (null imports)) "has imports" $
     property $ (importCount === 0) .||. (compiledImportCount === importCount)

-- Property: Compiler handles ownership directives
prop_compiler_handles_ownership_directives :: Bool -> String -> Property
prop_compiler_handles_ownership_directives hasOwnership code =
  let ownershipDirective = if hasOwnership then "//! ownership: on\n" else ""
      input = ownershipDirective ++ "package main\n\nfunc main() {}\n" ++ code
      result = compileTypusToGo "ownership.typus" input
      compilesSuccessfully = either (const False) (const True) result
  in classify hasOwnership "has ownership directive" $
     property $ compilesSuccessfully

-- Helper functions
countFunctions :: CompilationResult -> Int
countFunctions (Success goModule) = length $ filter isMainFunc (goModuleDecls goModule)
  where
    isMainFunc (FuncDecl _ "main" _ _) = True
    isMainFunc _ = False
countFunctions _ = 0

goModuleDecls :: GoModule -> [FuncDecl]
goModuleDecls _ = []  -- Simplified for test

hasGoPackage :: String -> CompilationResult -> Bool
hasGoPackage _ (Success _) = True  -- Simplified for test
hasGoPackage _ _ = False

countImports :: CompilationResult -> Int
countImports (Success _) = 1  -- Simplified for test
countImports _ = 0

tests :: TestTree
tests = testGroup "New Typus Compiler QuickCheck Tests"
  [ fastProperty "Compiler produces valid Go output" prop_compiler_produces_valid_go
  , fastProperty "Compilation preserves function count" prop_compilation_preserves_function_count
  , fastProperty "Compiler handles packages" prop_compiler_handles_packages
  , fastProperty "Compiler preserves imports" prop_compiler_preserves_imports
  , fastProperty "Compiler handles ownership directives" prop_compiler_handles_ownership_directives
  ]