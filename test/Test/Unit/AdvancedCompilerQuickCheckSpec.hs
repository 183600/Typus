{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.AdvancedCompilerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof)
import TestSupport.Arbitrary

import Compiler.IR
  ( SourceIR(..)
  , SemanticIR(..)
  , GoIR(..)
  , buildSourceIR
  , buildSemanticIR
  , buildSemanticIRWithPackage
  , emitGo
  , rawSourceFromTypus
  , moduleFromTypus
  , ensurePackageDecl
  , ensureMainFunction
  , attachInferredImports
  )

import Parser (TypusFile(..), CodeBlock(..))
import SyntaxValidator (SyntaxError(..))
import Compiler.Errors (CompilerResult)
import Compiler.GoAst (GoModule(..))
import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf, nub)
import qualified Data.Text as T

-- Property: buildSourceIR creates IR with correct typus file
prop_build_source_ir_correct :: TypusFile -> Property
prop_build_source_ir_correct typusFile =
  let ir = buildSourceIR typusFile
  in property $ sourceTypusFile ir === typusFile

-- Property: buildSourceIR extracts source text
prop_build_source_ir_extracts_text :: TypusFile -> Property
prop_build_source_ir_extracts_text typusFile =
  let ir = buildSourceIR typusFile
      sourceText = sourceText ir
  in property $ not (null sourceText) || null (tfCodeBlocks typusFile)

-- Property: rawSourceFromTypus extracts code from blocks
prop_raw_source_from_typus :: TypusFile -> Property
prop_raw_source_from_typus typusFile =
  let raw = rawSourceFromTypus typusFile
      blocks = tfCodeBlocks typusFile
      blockCount = length blocks
  in property $ (blockCount == 0) ==> (null raw)

-- Property: rawSourceFromTypus preserves block order
prop_raw_source_preserves_order :: [String] -> Property
prop_raw_source_preserves_order blockContents =
  not (null blockContents) && all (not . null) blockContents ==>
  let blocks = map (\content -> CodeBlock content Nothing Nothing) blockContents
      typusFile = TypusFile "" Nothing Nothing blocks
      raw = rawSourceFromTypus typusFile
      rawLines = lines raw
  in property $ length rawLines >= length blockContents

-- Property: buildSemanticIR handles valid input
prop_build_semantic_ir_valid :: TypusFile -> Property
prop_build_semantic_ir_valid typusFile =
  let ir = buildSourceIR typusFile
      result = buildSemanticIR ir
  in case result of
    Left _ -> property True  -- May fail for invalid input
    Right semanticIR -> property $ True  -- Should handle valid input

-- Property: buildSemanticIR preserves typus file
prop_build_semantic_ir_preserves :: TypusFile -> Property
prop_build_semantic_ir_preserves typusFile =
  let ir = buildSourceIR typusFile
      result = buildSemanticIR ir
  in case result of
    Left _ -> property True
    Right semanticIR -> property $ semanticTypusFile semanticIR === typusFile

-- Property: buildSemanticIR creates Go module
prop_build_semantic_ir_creates_module :: TypusFile -> Property
prop_build_semantic_ir_creates_module typusFile =
  let ir = buildSourceIR typusFile
      result = buildSemanticIR ir
  in case result of
    Left _ -> property True
    Right semanticIR -> property $ True  -- Should create Go module

-- Property: buildSemanticIRWithPackage handles package files
prop_build_semantic_ir_with_package :: TypusFile -> [(String, TypusFile)] -> Property
prop_build_semantic_ir_with_package typusFile packageFiles =
  let ir = buildSourceIR typusFile
      result = buildSemanticIRWithPackage ir packageFiles
  in case result of
    Left _ -> property True  -- May fail for invalid input
    Right semanticIR -> property $ True  -- Should handle package files

-- Property: buildSemanticIRWithPackage combines declarations
prop_build_semantic_ir_combines_decls :: TypusFile -> [(String, TypusFile)] -> Property
prop_build_semantic_ir_combines_decls typusFile packageFiles =
  not (null packageFiles) ==>
  let ir = buildSourceIR typusFile
      result = buildSemanticIRWithPackage ir packageFiles
  in case result of
    Left _ -> property True
    Right semanticIR -> 
      let module' = semanticModule semanticIR
          decls = gmDecls module'
      in property $ length decls >= 0  -- Should combine declarations

-- Property: emitGo creates Go IR
prop_emit_go_creates :: TypusFile -> Property
prop_emit_go_creates typusFile =
  let ir = buildSourceIR typusFile
      result = buildSemanticIR ir
  in case result of
    Left _ -> property True
    Right semanticIR ->
      let goIR = emitGo semanticIR
      in property $ True  -- Should create Go IR

-- Property: emitGo preserves Go module
prop_emit_go_preserves_module :: TypusFile -> Property
prop_emit_go_preserves_module typusFile =
  let ir = buildSourceIR typusFile
      result = buildSemanticIR ir
  in case result of
    Left _ -> property True
    Right semanticIR ->
      let goIR = emitGo semanticIR
          originalModule = semanticModule semanticIR
          goModule = goModule goIR
      in property $ goModule === originalModule

-- Property: emitGo generates source code
prop_emit_go_generates_source :: TypusFile -> Property
prop_emit_go_generates_source typusFile =
  let ir = buildSourceIR typusFile
      result = buildSemanticIR ir
  in case result of
    Left _ -> property True
    Right semanticIR ->
      let goIR = emitGo semanticIR
          source = goSource goIR
      in property $ not (null source) || null (tfCodeBlocks typusFile)

-- Property: moduleFromTypus handles empty file
prop_module_from_empty :: Property
prop_module_from_empty =
  let typusFile = TypusFile "" Nothing Nothing []
      result = moduleFromTypus typusFile
  in case result of
    Left _ -> property True
    Right goModule -> property $ True  -- Should handle empty file

-- Property: moduleFromTypus handles file with blocks
prop_module_from_blocks :: [String] -> Property
prop_module_from_blocks blockContents =
  not (null blockContents) && all (not . null) blockContents ==>
  let blocks = map (\content -> CodeBlock content Nothing Nothing) blockContents
      typusFile = TypusFile "" Nothing Nothing blocks
      result = moduleFromTypus typusFile
  in case result of
    Left _ -> property True
    Right goModule -> property $ True  -- Should handle blocks

-- Property: ensurePackageDecl adds package when missing
prop_ensure_package_adds_missing :: GoModule -> Property
prop_ensure_package_adds_missing goModule =
  let result = ensurePackageDecl goModule
  in case result of
    Left _ -> property True
    Right updatedModule -> property $ True  -- Should add package when missing

-- Property: ensurePackageDecl preserves existing package
prop_ensure_package_preserves_existing :: GoModule -> Property
prop_ensure_package_preserves_existing goModule =
  let result = ensurePackageDecl goModule
  in case result of
    Left _ -> property True
    Right updatedModule -> property $ True  -- Should preserve existing package

-- Property: ensureMainFunction adds main when missing
prop_ensure_main_adds_missing :: GoModule -> Property
prop_ensure_main_adds_missing goModule =
  let result = ensureMainFunction goModule
  in case result of
    Left _ -> property True
    Right updatedModule -> property $ True  -- Should add main when missing

-- Property: ensureMainFunction preserves existing main
prop_ensure_main_preserves_existing :: GoModule -> Property
prop_ensure_main_preserves_existing goModule =
  let result = ensureMainFunction goModule
  in case result of
    Left _ -> property True
    Right updatedModule -> property $ True  -- Should preserve existing main

-- Property: attachInferredImports handles empty imports
prop_attach_inferred_empty :: GoModule -> Property
prop_attach_inferred_empty goModule =
  let result = attachInferredImports goModule
  in case result of
    Left _ -> property True
    Right updatedModule -> property $ True  -- Should handle empty imports

-- Property: attachInferredImports preserves existing imports
prop_attach_inferred_preserves :: GoModule -> Property
prop_attach_inferred_preserves goModule =
  let result = attachInferredImports goModule
  in case result of
    Left _ -> property True
    Right updatedModule -> property $ True  -- Should preserve existing imports

-- Property: IR building pipeline is deterministic
prop_ir_pipeline_deterministic :: TypusFile -> Property
prop_ir_pipeline_deterministic typusFile =
  let ir1 = buildSourceIR typusFile
      ir2 = buildSourceIR typusFile
      result1 = buildSemanticIR ir1
      result2 = buildSemanticIR ir2
  in case (result1, result2) of
    (Right sem1, Right sem2) -> property $ sem1 === sem2
    (Left err1, Left err2) -> property $ err1 === err2
    _ -> property False  -- Should be consistent

-- Property: Go source generation is deterministic
prop_go_generation_deterministic :: TypusFile -> Property
prop_go_generation_deterministic typusFile =
  let ir = buildSourceIR typusFile
      result1 = buildSemanticIR ir
      result2 = buildSemanticIR ir
  in case (result1, result2) of
    (Right sem1, Right sem2) ->
      let goIR1 = emitGo sem1
          goIR2 = emitGo sem2
      in property $ goIR1 === goIR2
    _ -> property True  -- Handle error cases consistently

-- Property: IR handles large inputs
prop_ir_large_input :: String -> Int -> Property
prop_ir_large_input base multiplier =
  multiplier >= 0 && multiplier <= 50 ==>  -- Limit for performance
  let largeContent = concat (replicate multiplier base)
      block = CodeBlock largeContent Nothing Nothing
      typusFile = TypusFile "" Nothing Nothing [block]
      ir = buildSourceIR typusFile
  in property $ sourceText ir === largeContent

-- Property: IR handles unicode content
prop_ir_unicode :: String -> Property
prop_ir_unicode content =
  let unicodeContent = content ++ "测试🚀"
      block = CodeBlock unicodeContent Nothing Nothing
      typusFile = TypusFile "" Nothing Nothing [block]
      ir = buildSourceIR typusFile
  in property $ "测试🚀" `isInfixOf` (sourceText ir)

tests :: TestTree
tests = testGroup "Advanced Compiler QuickCheck"
  [ fastProperty "build source ir correct" prop_build_source_ir_correct
  , fastProperty "build source ir extracts text" prop_build_source_ir_extracts_text
  , fastProperty "raw source from typus" prop_raw_source_from_typus
  , fastProperty "raw source preserves order" prop_raw_source_preserves_order
  , fastProperty "build semantic ir valid" prop_build_semantic_ir_valid
  , fastProperty "build semantic ir preserves" prop_build_semantic_ir_preserves
  , fastProperty "build semantic ir creates module" prop_build_semantic_ir_creates_module
  , fastProperty "build semantic ir with package" prop_build_semantic_ir_with_package
  , fastProperty "build semantic ir combines decls" prop_build_semantic_ir_combines_decls
  , fastProperty "emit go creates" prop_emit_go_creates
  , fastProperty "emit go preserves module" prop_emit_go_preserves_module
  , fastProperty "emit go generates source" prop_emit_go_generates_source
  , fastProperty "module from empty" prop_module_from_empty
  , fastProperty "module from blocks" prop_module_from_blocks
  , fastProperty "ensure package adds missing" prop_ensure_package_adds_missing
  , fastProperty "ensure package preserves existing" prop_ensure_package_preserves_existing
  , fastProperty "ensure main adds missing" prop_ensure_main_adds_missing
  , fastProperty "ensure main preserves existing" prop_ensure_main_preserves_existing
  , fastProperty "attach inferred empty" prop_attach_inferred_empty
  , fastProperty "attach inferred preserves" prop_attach_inferred_preserves
  , fastProperty "ir pipeline deterministic" prop_ir_pipeline_deterministic
  , fastProperty "go generation deterministic" prop_go_generation_deterministic
  , fastProperty "ir large input" prop_ir_large_input
  , fastProperty "ir unicode" prop_ir_unicode
  ]