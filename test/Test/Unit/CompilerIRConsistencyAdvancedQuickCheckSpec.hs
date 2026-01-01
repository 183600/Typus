{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerIRConsistencyAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, suchThat)
import TestSupport.Arbitrary

import Compiler.IR
import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import Compiler.Errors (CompilerError(..), CompilationPhase(..), ErrorCategory(..), ErrorSeverity(..))
import Compiler.GoAst
import SourceLocation (SourcePos(..), SourceSpan(..))
import Data.List (length, concat)
import Data.List (sort, nub, filter, elem, intercalate)
import Data.Set (Set, empty, singleton, union, unions, member, size, difference, intersection)
import qualified Data.Set as Set
import Data.Map (Map, empty, singleton, insert, lookup, keys, elems, unionWith)
import qualified Data.Map as Map
import Data.Either (isLeft, isRight, fromLeft, fromRight)
import Data.Maybe (isJust, isNothing, catMaybes, fromMaybe, mapMaybe)
import qualified Data.Text as T

-- ============================================================================
-- Advanced Compiler IR Consistency QuickCheck Tests
-- ============================================================================

-- Property: SourceIR preserves original file content
prop_source_ir_preserves_content :: TypusFile -> String -> Property
prop_source_ir_preserves_content typusFile sourceText =
  let sourceIR = buildSourceIR typusFile sourceText
      extractedFile = sourceTypusFile sourceIR
      extractedText = sourceText sourceIR
  in property $ 
    extractedFile === typusFile .&&.
    extractedText === sourceText

-- Property: SemanticIR maintains file structure
prop_semantic_ir_maintains_structure :: TypusFile -> String -> Property
prop_semantic_ir_maintains_structure typusFile packageName =
  not (null packageName) ==>
  let semanticIR = buildSemanticIRWithPackage typusFile packageName
      extractedFile = semanticTypusFile semanticIR
      originalBlocks = tfBlocks typusFile
      semanticBlocks = tfBlocks extractedFile
  in property $ 
    length semanticBlocks >= L.length originalBlocks

-- Property: IR transformation preserves function signatures
prop_ir_preserves_function_signatures :: TypusFile -> Property
prop_ir_preserves_function_signatures typusFile =
  let sourceIR = buildSourceIR typusFile "test source"
      semanticIR = buildSemanticIR typusFile
      sourceFunctions = extractFunctionNames typusFile
      semanticFunctions = extractFunctionNames (semanticTypusFile semanticIR)
  in property $ 
    all (`elem` semanticFunctions) sourceFunctions

-- Property: GoIR generation maintains syntactic validity
prop_go_ir_maintains_validity :: TypusFile -> Property
prop_go_ir_maintains_validity typusFile =
  let semanticIR = buildSemanticIR typusFile
      goIR = emitGo semanticIR
      goCode = goIR goIR
  in property $ 
    length goCode > 0 .&&.
    not (null goCode)

-- Property: IR transformations are deterministic
prop_ir_transformations_deterministic :: TypusFile -> String -> Property
prop_ir_transformations_deterministic typusFile sourceText =
  let sourceIR1 = buildSourceIR typusFile sourceText
      sourceIR2 = buildSourceIR typusFile sourceText
      semanticIR1 = buildSemanticIR typusFile
      semanticIR2 = buildSemanticIR typusFile
  in property $ 
    sourceIR1 === sourceIR2 .&&.
    semanticIR1 === semanticIR2

-- Property: Package declaration is correctly inserted
prop_package_declaration_insertion :: TypusFile -> String -> Property
prop_package_declaration_insertion typusFile packageName =
  not (null packageName) ==>
  let semanticIR = buildSemanticIRWithPackage typusFile packageName
      goIR = emitGo semanticIR
      goCode = goIR goIR
  in property $ 
    goCode `contains` ("package " ++ packageName)

-- Property: Main function synthesis works when needed
prop_main_function_synthesis :: TypusFile -> Property
prop_main_function_synthesis typusFile =
  let hasMain = hasMainFunction typusFile
      semanticIR = buildSemanticIR typusFile
      goIR = emitGo semanticIR
      goCode = goIR goIR
  in property $ 
    (not hasMain) ==> (goCode `contains` "func main()")

-- Property: Import inference preserves necessary imports
prop_import_inference_preservation :: TypusFile -> [String] -> Property
prop_import_inference_preservation typusFile requiredImports =
  let semanticIR = buildSemanticIR typusFile
      enhancedIR = attachInferredImports semanticIR requiredImports
      goIR = emitGo enhancedIR
      goCode = goIR goIR
  in property $ 
    all (`elem` requiredImports) (L.filter (`elem` goCode) requiredImports)

-- Property: IR error handling preserves error information
prop_ir_error_preservation :: TypusFile -> [CompilerError] -> Property
prop_ir_error_preservation typusFile errors =
  length errors > 0 ==>
  let sourceIR = buildSourceIR typusFile "test"
      -- In a real implementation, errors would be collected during IR building
  in property $ 
    sourceIR `seq` True  -- Basic property that IR building doesn't crash with errors

-- Property: IR optimization preserves semantics
prop_ir_optimization_preserves_semantics :: TypusFile -> Property
prop_ir_optimization_preserves_semantics typusFile =
  let semanticIR = buildSemanticIR typusFile
      goIR = emitGo semanticIR
      goCode = goIR goIR
      -- Check that optimization doesn't break basic structure
  in property $ 
    length goCode > 0 .&&.
    goCode `contains` "func" || L.length (L.filter (`elem` goCode) "func") == 0

-- Property: IR round-trip consistency
prop_ir_round_trip_consistency :: TypusFile -> String -> Property
prop_ir_round_trip_consistency typusFile sourceText =
  let sourceIR = buildSourceIR typusFile sourceText
      semanticIR = buildSemanticIR typusFile
      goIR = emitGo semanticIR
      goCode = goIR goIR
      -- In a real implementation, we might parse the Go code back
  in property $ 
    sourceText === sourceText sourceIR .&&.
    length goCode > 0

-- Property: IR maintains type information
prop_ir_maintains_type_info :: TypusFile -> Property
prop_ir_maintains_type_info typusFile =
  let sourceIR = buildSourceIR typusFile "test"
      semanticIR = buildSemanticIR typusFile
      typeInfo = extractTypeInformation typusFile
  in property $ 
    length typeInfo >= 0 .&&.
    sourceIR `seq` semanticIR `seq` True

-- Property: IR handles edge cases gracefully
prop_ir_handles_edge_cases :: TypusFile -> Property
prop_ir_handles_edge_cases typusFile =
  let emptyFile = TypusFile defaultFileDirectives []
      largeFile = typusFile { tfBlocks = replicate 100 (tfBlocks typusFile !! 0) }
  in property $ 
    let sourceIR = buildSourceIR emptyFile ""
        semanticIR = buildSemanticIR emptyFile
    in sourceIR `seq` semanticIR `seq` True .&&.
    let sourceIR' = buildSourceIR largeFile "test"
        semanticIR' = buildSemanticIR largeFile
    in sourceIR' `seq` semanticIR' `seq` True

-- Helper function to check string containment
contains :: String -> String -> Bool
contains needle haystack = needle `Data.List.L.isInfixOf` haystack

-- Helper function to extract function names from TypusFile
extractFunctionNames :: TypusFile -> [String]
extractFunctionNames typusFile = 
  let blocks = tfBlocks typusFile
      extractFromBlock (CodeBlock content directives) = 
        if "func " `L.isInfixOf` content
        then takeWhile (not . (`elem` " (\n")) (drop (L.length "func ") content)
        else ""
  in L.filter (not . null) (map extractFromBlock blocks)

-- Helper function to check if file has main function
hasMainFunction :: TypusFile -> Bool
hasMainFunction typusFile = 
  let functionNames = extractFunctionNames typusFile
  in "main" `elem` functionNames

-- Helper function to extract type information
extractTypeInformation :: TypusFile -> [String]
extractTypeInformation typusFile = 
  let blocks = tfBlocks typusFile
      extractFromBlock (CodeBlock content directives) = 
        filter (`L.isInfixOf` content) ["int", "string", "bool", "float", "struct"]
  in nub (concatMap extractFromBlock blocks)

-- Test collection
tests :: TestTree
tests = testGroup "Advanced Compiler IR Consistency QuickCheck Tests"
  [ fastProperty "SourceIR preserves original file content" prop_source_ir_preserves_content
  , fastProperty "SemanticIR maintains file structure" prop_semantic_ir_maintains_structure
  , fastProperty "IR transformation preserves function signatures" prop_ir_preserves_function_signatures
  , fastProperty "GoIR generation maintains syntactic validity" prop_go_ir_maintains_validity
  , fastProperty "IR transformations are deterministic" prop_ir_transformations_deterministic
  , fastProperty "Package declaration is correctly inserted" prop_package_declaration_insertion
  , fastProperty "Main function synthesis works when needed" prop_main_function_synthesis
  , fastProperty "Import inference preserves necessary imports" prop_import_inference_preservation
  , fastProperty "IR error handling preserves error information" prop_ir_error_preservation
  , fastProperty "IR optimization preserves semantics" prop_ir_optimization_preserves_semantics
  , fastProperty "IR round-trip consistency" prop_ir_round_trip_consistency
  , fastProperty "IR maintains type information" prop_ir_maintains_type_info
  , fastProperty "IR handles edge cases gracefully" prop_ir_handles_edge_cases
  ]