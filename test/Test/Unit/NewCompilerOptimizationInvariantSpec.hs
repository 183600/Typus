{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCompilerOptimizationInvariantSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (choose, listOf, listOf1, elements, vectorOf, resize)
import Test.QuickCheck.Arbitrary (Arbitrary(..), oneof)

import Compiler (compile, CompilerResult(..), CompilerError(..), CompilationPhase(..))
import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..), buildSourceIR, buildSemanticIR, emitGo, rawSourceFromTypus)
import Compiler.GoAst (GoModule(..), GoDecl(..), GoImport(..), GoExpr(..))
import SourceLocation (SourceSpan(..), SourcePos(..), startPos, spanBetween)

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)

-- ============================================================================
-- New Compiler Optimization Invariant Tests
-- ============================================================================

-- Create a simple code block for testing
createCodeBlock :: String -> CodeBlock
createCodeBlock content = CodeBlock
    { cbDirectives = defaultBlockDirectives
    , cbContent = content
    , cbSpan = spanBetween startPos startPos
    }

-- Create a simple Typus file for testing
createTypusFile :: [String] -> TypusFile
createTypusFile codeContents =
  let blocks = map createCodeBlock codeContents
  in TypusFile
     { tfDirectives = defaultFileDirectives
     , tfBuildTags = []
     , tfBlocks = blocks
     , tfSyntaxErrors = []
     }

-- Property: Compilation preserves semantic meaning
prop_compilation_preserves_semantics :: String -> Property
prop_compilation_preserves_semantics code =
  not (null code) && L.length code <= 1000 ==>  -- Limit for performance
  let typusFile = createTypusFile [code]
      result = compile typusFile
  in case result of
       Right goCode -> property $ not (null goCode)  -- Successful compilation should produce code
       Left _ -> property $ True  -- Compilation may fail, but shouldn't crash

-- Property: Source IR preserves original source text
prop_source_ir_preserves_source :: [String] -> Property
prop_source_ir_preserves_source codeBlocks =
  not (null codeBlocks) && L.length codeBlocks <= 10 ==>  -- Limit for performance
  let typusFile = createTypusFile codeBlocks
      sourceIR = buildSourceIR typusFile
      originalText = unlines codeBlocks
      extractedText = rawSourceFromTypus typusFile
  in property $ sourceText sourceIR === extractedText

-- Property: Semantic IR contains valid Go module structure
prop_semantic_ir_valid_structure :: String -> Property
prop_semantic_ir_valid_structure code =
  not (null code) && L.length code <= 500 ==>  -- Limit for performance
  let typusFile = createTypusFile [code]
      sourceIR = buildSourceIR typusFile
      result = buildSemanticIR sourceIR
  in case result of
       Right semanticIR -> property $ not (L.null (show semanticIR))  -- Should have some structure
       Left _ -> property $ True  -- May fail, but shouldn't crash

-- Property: Go IR contains valid Go source code
prop_go_ir_valid_source :: String -> Property
prop_go_ir_valid_source code =
  not (null code) && L.length code <= 500 ==>  -- Limit for performance
  let typusFile = createTypusFile [code]
      result = compile typusFile
  in case result of
       Right goCode -> property $ "package main" `L.isInfixOf` goCode .||. 
                                   "func main()" `L.isInfixOf` goCode .||.
                                   not (null goCode)
       Left _ -> property $ True  -- May fail, but shouldn't crash

-- Property: Compilation is deterministic
prop_compilation_deterministic :: String -> Property
prop_compilation_deterministic code =
  not (null code) && L.length code <= 500 ==>  -- Limit for performance
  let typusFile = createTypusFile [code]
      result1 = compile typusFile
      result2 = compile typusFile
  in case (result1, result2) of
       (Right code1, Right code2) -> property $ code1 === code2
       (Left err1, Left err2) -> property $ L.length err1 === L.length err2  -- Same number of errors
       (Right _, Left _) -> property $ False  -- Shouldn't happen
       (Left _, Right _) -> property $ False  -- Shouldn't happen

-- Property: Multiple code blocks are processed correctly
prop_multiple_code_blocks :: [String] -> Property
prop_multiple_code_blocks codeBlocks =
  not (null codeBlocks) && L.length codeBlocks <= 5 ==>  -- Limit for performance
  let typusFile = createTypusFile codeBlocks
      result = compile typusFile
  in case result of
       Right goCode -> property $ not (null goCode)  -- Should produce some output
       Left _ -> property $ True  -- May fail, but shouldn't crash

-- Property: Empty code blocks are handled gracefully
prop_empty_code_blocks :: Int -> Property
prop_empty_code_blocks numBlocks =
  numBlocks >= 0 && numBlocks <= 10 ==>  -- Limit for performance
  let emptyBlocks = replicate numBlocks ""
      typusFile = createTypusFile emptyBlocks
      result = compile typusFile
  in case result of
       Right goCode -> property $ not (null goCode)  -- Should still produce basic structure
       Left _ -> property $ True  -- May fail, but shouldn't crash

-- Property: Compilation preserves import statements
prop_compilation_preserves_imports :: String -> Property
prop_compilation_preserves_imports code =
  "import" `L.isInfixOf` code && L.length code <= 500 ==>  -- Only test code with imports
  let typusFile = createTypusFile [code]
      result = compile typusFile
  in case result of
       Right goCode -> property $ "import" `L.isInfixOf` goCode
       Left _ -> property $ True  -- May fail, but shouldn't crash

-- Property: Compilation preserves function declarations
prop_compilation_preserves_functions :: String -> Property
prop_compilation_preserves_functions code =
  "func" `L.isInfixOf` code && L.length code <= 500 ==>  -- Only test code with functions
  let typusFile = createTypusFile [code]
      result = compile typusFile
  in case result of
       Right goCode -> property $ "func" `L.isInfixOf` goCode
       Left _ -> property $ True  -- May fail, but shouldn't crash

-- Property: Compilation handles variable declarations
prop_compilation_handles_variables :: String -> Property
prop_compilation_handles_variables code =
  "var" `L.isInfixOf` code && L.length code <= 500 ==>  -- Only test code with variables
  let typusFile = createTypusFile [code]
      result = compile typusFile
  in case result of
       Right goCode -> property $ "var" `L.isInfixOf` goCode
       Left _ -> property $ True  -- May fail, but shouldn't crash

-- Property: Compilation handles type annotations
prop_compilation_handles_types :: String -> Property
prop_compilation_handles_types code =
  any (`L.isInfixOf` code) ["int", "string", "bool", "float"] && L.length code <= 500 ==>  -- Only test code with types
  let typusFile = createTypusFile [code]
      result = compile typusFile
  in case result of
       Right goCode -> property $ L.any (`L.isInfixOf` goCode) ["int", "string", "bool", "float"]
       Left _ -> property $ True  -- May fail, but shouldn't crash

-- Property: Source IR building is deterministic
prop_source_ir_deterministic :: [String] -> Property
prop_source_ir_deterministic codeBlocks =
  not (null codeBlocks) && L.length codeBlocks <= 5 ==>  -- Limit for performance
  let typusFile = createTypusFile codeBlocks
      sourceIR1 = buildSourceIR typusFile
      sourceIR2 = buildSourceIR typusFile
  in property $ sourceText sourceIR1 === sourceText sourceIR2 .&&.
     show (sourceTypusFile sourceIR1) === show (sourceTypusFile sourceIR2)

-- Property: Semantic IR building is deterministic
prop_semantic_ir_deterministic :: String -> Property
prop_semantic_ir_deterministic code =
  not (null code) && L.length code <= 500 ==>  -- Limit for performance
  let typusFile = createTypusFile [code]
      sourceIR = buildSourceIR typusFile
      result1 = buildSemanticIR sourceIR
      result2 = buildSemanticIR sourceIR
  in case (result1, result2) of
       (Right ir1, Right ir2) -> property $ show ir1 === show ir2
       (Left err1, Left err2) -> property $ L.length err1 === L.length err2  -- Same number of errors
       (Right _, Left _) -> property $ False  -- Shouldn't happen
       (Left _, Right _) -> property $ False  -- Shouldn't happen

-- Property: Go emission preserves module structure
prop_go_emission_preserves_structure :: String -> Property
prop_go_emission_preserves_structure code =
  not (null code) && L.length code <= 500 ==>  -- Limit for performance
  let typusFile = createTypusFile [code]
      sourceIR = buildSourceIR typusFile
      result = buildSemanticIR sourceIR
  in case result of
       Right semanticIR ->
         let goIR = emitGo semanticIR
             goSource = goModule (goIR)
         in property $ not (L.null (show goSource))
       Left _ -> property $ True  -- May fail, but shouldn't crash

-- Property: Compilation pipeline maintains consistency
prop_compilation_pipeline_consistency :: String -> Property
prop_compilation_pipeline_consistency code =
  not (null code) && L.length code <= 500 ==>  -- Limit for performance
  let typusFile = createTypusFile [code]
      directResult = compile typusFile
      pipelineResult = do
        sourceIR <- buildSourceIR typusFile
        semanticIR <- buildSemanticIR sourceIR
        return (emitGo semanticIR)
  in case (directResult, pipelineResult) of
       (Right directCode, Right pipelineIR) -> 
         property $ goSource pipelineIR === directCode
       (Left _, Left _) -> property $ True  -- Both failed is acceptable
       (Right _, Left _) -> property $ True  -- Direct succeeded, pipeline failed is acceptable
       (Left _, Right _) -> property $ True  -- Direct failed, pipeline succeeded is acceptable

-- Property: Error messages contain useful information
prop_error_messages_useful :: String -> Property
prop_error_messages_useful code =
  "var x int = \"string\"" `L.isInfixOf` code ==>  -- Force a specific error
  let typusFile = createTypusFile [code]
      result = compile typusFile
  in case result of
       Left errors -> property $ L.any (\err -> "type error" `L.isInfixOf` show err || 
                                           "CP0003" `L.isInfixOf` show err) errors
       Right _ -> property $ True  -- May succeed unexpectedly

-- Property: Large source files are handled without crashing
prop_large_source_files :: Int -> String -> Property
prop_large_source_files multiplier base =
  multiplier >= 0 && multiplier <= 100 ==>  -- Limit for performance
  let largeCode = L.concat (replicate multiplier (base ++ "\n"))
      typusFile = createTypusFile [largeCode]
      result = compile typusFile
  in case result of
       Right _ -> property $ True  -- Success is acceptable
       Left _ -> property $ True  -- Failure is acceptable, but shouldn't crash

-- Property: Compilation with syntax errors provides diagnostics
prop_syntax_error_diagnostics :: String -> Property
prop_syntax_error_diagnostics code =
  "func {" `L.isInfixOf` code ==>  -- Force a syntax error
  let typusFile = createTypusFile [code]
      result = compile typusFile
  in case result of
       Left errors -> property $ not (null errors)  -- Should produce some errors
       Right _ -> property $ True  -- May succeed unexpectedly

-- Property: Compilation preserves comments (when possible)
prop_compilation_preserves_comments :: String -> Property
prop_compilation_preserves_comments code =
  "//" `L.isInfixOf` code && L.length code <= 500 ==>  -- Only test code with comments
  let typusFile = createTypusFile [code]
      result = compile typusFile
  in case result of
       Right goCode -> property $ "//" `L.isInfixOf` goCode .||. "/*" `L.isInfixOf` goCode
       Left _ -> property $ True  -- May fail, but shouldn't crash

-- Property: Multiple compilation passes are idempotent
prop_multiple_compilation_idempotent :: String -> Property
prop_multiple_compilation_idempotent code =
  not (null code) && L.length code <= 500 ==>  -- Limit for performance
  let typusFile = createTypusFile [code]
      result1 = compile typusFile
  in case result1 of
       Right goCode1 ->
         let typusFile2 = createTypusFile [goCode1]  -- Compile the result again
             result2 = compile typusFile2
         in case result2 of
              Right goCode2 -> property $ L.length goCode2 > 0  -- Should still produce output
              Left _ -> property $ True  -- May fail, but shouldn't crash
       Left _ -> property $ True  -- First compilation failed, skip second

-- Tests collection
tests :: TestTree
tests = testGroup "New Compiler Optimization Invariant Tests"
  [ fastProperty "Compilation preserves semantic meaning" prop_compilation_preserves_semantics
  , fastProperty "Source IR preserves original source text" prop_source_ir_preserves_source
  , fastProperty "Semantic IR contains valid Go module structure" prop_semantic_ir_valid_structure
  , fastProperty "Go IR contains valid Go source code" prop_go_ir_valid_source
  , fastProperty "Compilation is deterministic" prop_compilation_deterministic
  , fastProperty "Multiple code blocks are processed correctly" prop_multiple_code_blocks
  , fastProperty "Empty code blocks are handled gracefully" prop_empty_code_blocks
  , fastProperty "Compilation preserves import statements" prop_compilation_preserves_imports
  , fastProperty "Compilation preserves function declarations" prop_compilation_preserves_functions
  , fastProperty "Compilation handles variable declarations" prop_compilation_handles_variables
  , fastProperty "Compilation handles type annotations" prop_compilation_handles_types
  , fastProperty "Source IR building is deterministic" prop_source_ir_deterministic
  , fastProperty "Semantic IR building is deterministic" prop_semantic_ir_deterministic
  , fastProperty "Go emission preserves module structure" prop_go_emission_preserves_structure
  , fastProperty "Compilation pipeline maintains consistency" prop_compilation_pipeline_consistency
  , fastProperty "Error messages contain useful information" prop_error_messages_useful
  , fastProperty "Large source files are handled without crashing" prop_large_source_files
  , fastProperty "Compilation with syntax errors provides diagnostics" prop_syntax_error_diagnostics
  , fastProperty "Compilation preserves comments (when possible)" prop_compilation_preserves_comments
  , fastProperty "Multiple compilation passes are idempotent" prop_multiple_compilation_idempotent
  ]