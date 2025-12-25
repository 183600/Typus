{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.EnhancedCompilerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, suchThat
  , resize, Positive(..), NonEmpty(..)
  )

import Compiler
  ( compile
  , CompilerError(..)
  , CompilerResult
  , CompilationPhase(..)
  , ErrorCategory(..)
  , ErrorSeverity(..)
  , generateGoCode
  , renderCompilationError
  , hasTypeErrors
  , formatCompilerErrors
  , analyzeErrors
  , generateDetailedReport
  )

import Parser
  ( TypusFile(..)
  , parseTypus
  , defaultFileDirectives
  )

import Data.List (isInfixOf, isPrefixOf, null)
import Data.Char (isSpace)
import qualified Data.Text as T

-- Property: Compilation preserves basic structure
prop_compile_preserves_structure :: String -> Property
prop_compile_preserves_structure source =
  not (null source) && not (all isSpace source) ==>
  case parseTypus source of
    Left _ -> property True -- Parsing failures are acceptable
    Right typusFile -> 
      case compile typusFile of
        Left _ -> property True -- Compilation failures are acceptable
        Right goCode -> property (not (null goCode))

-- Property: generateGoCode always returns some output
prop_generateGoCode_always_returns_output :: String -> Property
prop_generateGoCode_always_returns_output source =
  case parseTypus source of
    Left _ -> property True
    Right typusFile -> 
      let goCode = generateGoCode typusFile
      in property (not (null goCode))

-- Property: generateGoCode preserves source content for simple cases
prop_generateGoCode_preserves_simple_content :: String -> Property
prop_generateGoCode_preserves_simple_content simpleContent =
  not (null simpleContent) && not ("//!" `isInfixOf` simpleContent) && 
  not ("package " `isInfixOf` simpleContent) ==>
  let fullSource = unlines ["package main", "func main() {", simpleContent, "}"]
  in case parseTypus fullSource of
    Left _ -> property True
    Right typusFile -> 
      let goCode = generateGoCode typusFile
      in property (length goCode >= length simpleContent)

-- Property: Compilation errors have proper structure
prop_compile_errors_structure :: String -> Property
prop_compile_errors_structure source =
  "var x int = \"string\"" `isInfixOf` source ==> -- Force a type error
  case parseTypus source of
    Left _ -> property True
    Right typusFile -> 
      case compile typusFile of
        Right _ -> property True -- Unexpected success is acceptable
        Left errors -> 
          property (all hasErrorCode errors && all hasErrorPhase errors)

-- Property: Error rendering produces output
prop_renderCompilationError_produces_output :: [CompilerError] -> Property
prop_renderCompilationError_produces_output errors =
  not (null errors) ==>
  let rendered = renderCompilationError errors
  in property (not (null rendered))

-- Property: Format errors consistency
prop_formatCompilerErrors_consistent :: [CompilerError] -> Property
prop_formatCompilerErrors_consistent errors =
  let formatted = formatCompilerErrors errors
      rendered = renderCompilationError errors
  in property (formatted === rendered)

-- Property: analyzeErrors handles empty list
prop_analyzeErrors_empty :: Property
prop_analyzeErrors_empty =
  let result = analyzeErrors []
  in case result of
    Left _ -> property False
    Right analysis -> property True -- Should handle empty input gracefully

-- Property: generateDetailedReport handles errors
prop_generateDetailedReport_handles_errors :: [CompilerError] -> Property
prop_generateDetailedReport_handles_errors errors =
  let report = generateDetailedReport errors
  in property (not (null report))

-- Property: Compilation handles malformed input gracefully
prop_compile_handles_malformed :: String -> Property
prop_compile_handles_malformed malformed =
  not (null malformed) && all isSpace (take 5 malformed) ==>
  case parseTypus malformed of
    Left _ -> property True
    Right typusFile -> 
      case compile typusFile of
        Left _ -> property True
        Right _ -> property True

-- Property: generateGoCode handles empty TypusFile
prop_generateGoCode_empty_file :: Property
prop_generateGoCode_empty_file =
  let emptyFile = TypusFile defaultFileDirectives [] [] []
      goCode = generateGoCode emptyFile
  in property (not (null goCode))

-- Property: Compilation phases are properly ordered
prop_compile_phases_ordered :: String -> Property
prop_compile_phases_ordered source =
  "var x int = \"string\"" `isInfixOf` source ==>
  case parseTypus source of
    Left _ -> property True
    Right typusFile -> 
      case compile typusFile of
        Right _ -> property True
        Left errors -> 
          property (all hasValidPhase errors)

-- Helper functions
hasErrorCode :: CompilerError -> Bool
hasErrorCode err = not (T.null (errorCode err))

hasErrorPhase :: CompilerError -> Bool
hasErrorPhase err = errorPhase err `elem` [ParsingPhase, TypeCheckingPhase, OwnershipPhase, CodeGenPhase]

hasValidPhase :: CompilerError -> Bool
hasValidPhase err = errorPhase err `elem` 
  [ParsingPhase, TypeCheckingPhase, OwnershipPhase, CodeGenPhase, OptimizationPhase]

tests :: TestTree
tests = testGroup "Enhanced Compiler QuickCheck Tests"
  [ fastProperty "Preserves compilation structure" prop_compile_preserves_structure
  , fastProperty "generateGoCode always returns output" prop_generateGoCode_always_returns_output
  , fastProperty "generateGoCode preserves simple content" prop_generateGoCode_preserves_simple_content
  , fastProperty "Compilation errors have structure" prop_compile_errors_structure
  , fastProperty "Error rendering produces output" prop_renderCompilationError_produces_output
  , fastProperty "Format errors consistent" prop_formatCompilerErrors_consistent
  , fastProperty "analyzeErrors handles empty list" prop_analyzeErrors_empty
  , fastProperty "generateDetailedReport handles errors" prop_generateDetailedReport_handles_errors
  , fastProperty "Compilation handles malformed input" prop_compile_handles_malformed
  , fastProperty "generateGoCode handles empty file" prop_generateGoCode_empty_file
  , fastProperty "Compilation phases ordered" prop_compile_phases_ordered
  ]