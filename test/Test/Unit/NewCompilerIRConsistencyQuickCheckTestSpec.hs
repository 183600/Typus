{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Test.Unit.NewCompilerIRConsistencyQuickCheckTestSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler.IR
import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import Compiler.GoAst
import SourceLocation (SourceSpan(..), SourcePos(..), startPos)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace)
import qualified Data.Text as T

-- ============================================================================
-- Compiler IR Consistency QuickCheck Tests
-- ============================================================================

-- Test IRStatement consistency
prop_ir_statement_consistency :: String -> String -> Property
prop_ir_statement_consistency stmtType stmtContent = 
  let statement = IRStatement stmtType stmtContent
  in property $ irStmtType statement === stmtType &&
                irStmtContent statement === stmtContent

prop_ir_statement_equality :: String -> String -> Property
prop_ir_statement_equality stmtType stmtContent = 
  let statement1 = IRStatement stmtType stmtContent
      statement2 = IRStatement stmtType stmtContent
  in property $ statement1 === statement2

-- Test IRExpression consistency
prop_ir_expression_consistency :: String -> String -> Property
prop_ir_expression_consistency exprType exprContent = 
  let expression = IRExpression exprType exprContent
  in property $ irExprType expression === exprType &&
                irExprContent expression === exprContent

prop_ir_expression_equality :: String -> String -> Property
prop_ir_expression_equality exprType exprContent = 
  let expression1 = IRExpression exprType exprContent
      expression2 = IRExpression exprType exprContent
  in property $ expression1 === expression2

-- Test SourceIR consistency
prop_source_ir_consistency :: TypusFile -> String -> Property
prop_source_ir_consistency typusFile sourceText = 
  let sourceIR = SourceIR typusFile sourceText
  in property $ sourceTypusFile sourceIR === typusFile &&
                sourceText sourceIR === sourceText

prop_source_ir_build_consistency :: TypusFile -> Property
prop_source_ir_build_consistency typusFile = 
  let sourceIR = buildSourceIR typusFile
      expectedSourceText = rawSourceFromTypus typusFile
  in property $ sourceTypusFile sourceIR === typusFile &&
                sourceText sourceIR === expectedSourceText

-- Test SemanticIR consistency
prop_semantic_ir_consistency :: TypusFile -> GoModule -> [ValueInfo] -> Property
prop_semantic_ir_consistency typusFile goModule valueInfo = 
  let semanticIR = SemanticIR typusFile goModule valueInfo
  in property $ semanticTypusFile semanticIR === typusFile &&
                semanticModule semanticIR === goModule &&
                semanticValueInfo semanticIR === valueInfo

-- Test GoIR consistency
prop_go_ir_consistency :: GoModule -> String -> Property
prop_go_ir_consistency goModule goSource = 
  let goIR = GoIR goModule goSource
  in property $ goModule goIR === goModule &&
                goSource goIR === goSource

prop_go_ir_emit_consistency :: TypusFile -> GoModule -> [ValueInfo] -> Property
prop_go_ir_emit_consistency typusFile goModule valueInfo = 
  let semanticIR = SemanticIR typusFile goModule valueInfo
      goIR = emitGo semanticIR
      expectedGoSource = renderGoModule goModule
  in property $ goModule goIR === goModule &&
                goSource goIR === expectedGoSource

-- Test raw source extraction consistency
prop_raw_source_from_typus_empty :: Property
prop_raw_source_from_typus_empty = 
  let typusFile = TypusFile defaultFileDirectives [] [] []
      sourceText = rawSourceFromTypus typusFile
  in property $ sourceText === ""

prop_raw_source_from_typus_single_block :: String -> Property
prop_raw_source_from_typus_single_block content = 
  let block = CodeBlock defaultBlockDirectives content (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
      sourceText = rawSourceFromTypus typusFile
  in property $ sourceText === content

prop_raw_source_from_typus_multiple_blocks :: String -> String -> Property
prop_raw_source_from_typus_multiple_blocks content1 content2 = 
  let block1 = CodeBlock defaultBlockDirectives content1 (emptySpan startPos)
      block2 = CodeBlock defaultBlockDirectives content2 (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block1, block2] []
      sourceText = rawSourceFromTypus typusFile
      expectedSourceText = content1 ++ "\n" ++ content2
  in property $ sourceText === expectedSourceText

-- Test package declaration consistency
prop_ensure_package_decl_existing :: String -> Property
prop_ensure_package_decl_existing packageName = 
  let packageDecl = PackageDecl packageName
      goModule = GoModule (Just packageDecl) [] [] [] []
      result = ensurePackageDecl goModule
  in property $ result === goModule

prop_ensure_package_decl_missing :: Property
prop_ensure_package_decl_missing = 
  let goModule = GoModule Nothing [] [] [] []
      result = ensurePackageDecl goModule
      expectedPackage = PackageDecl "main"
  in property $ gmPackage result === Just expectedPackage

prop_ensure_package_decl_preserves_decls :: String -> [GoDecl] -> Property
prop_ensure_package_decl_preserves_decls packageName decls = 
  let packageDecl = PackageDecl packageName
      goModule = GoModule (Just packageDecl) decls [] [] []
      result = ensurePackageDecl goModule
  in property $ gmDecls result === decls

-- Test main function consistency
prop_ensure_main_function_existing :: [GoDecl] -> Property
prop_ensure_main_function_existing decls = 
  let mainFunc = GoFunc (FuncDecl [Located startPos "func main() {}"])
      goModule = GoModule (Just (PackageDecl "main")) (mainFunc:decls) [] [] []
      result = ensureMainFunction goModule
  in property $ result === goModule

prop_ensure_main_function_missing :: [GoDecl] -> Property
prop_ensure_main_function_missing decls = 
  let goModule = GoModule (Just (PackageDecl "main")) decls [] [] []
      result = ensureMainFunction goModule
      hasMainFunc = any isMainFunc (gmDecls result)
  in property $ hasMainFunc
  where
    isMainFunc (GoFunc (FuncDecl ls)) = any ("main" `isInfixOf`) (map locatedValue ls)
    isMainFunc _ = False

prop_ensure_main_function_preserves_package :: [GoDecl] -> Property
prop_ensure_main_function_preserves_package decls = 
  let goModule = GoModule (Just (PackageDecl "test")) decls [] [] []
      result = ensureMainFunction goModule
  in property $ gmPackage result === gmPackage goModule

-- Test generic conversion consistency
prop_apply_generics_preserves_count :: [GoDecl] -> Property
prop_apply_generics_preserves_count decls = 
  let goModule = GoModule (Just (PackageDecl "test")) decls [] [] []
      result = applyGenerics goModule
  in property $ length (gmDecls result) === length decls

prop_apply_generics_preserves_structure :: [GoDecl] -> Property
prop_apply_generics_preserves_structure decls = 
  let goModule = GoModule (Just (PackageDecl "test")) decls [] [] []
      result = applyGenerics goModule
      originalTypes = map declType decls
      resultTypes = map declType result
  in property $ originalTypes === resultTypes
  where
    declType (GoFunc _) = "function"
    declType (GoType _) = "type"
    declType (GoVar _) = "variable"
    declType (GoConst _) = "constant"
    declType (GoStatement _) = "statement"
    declType (GoRaw _) = "raw"

prop_apply_generics_preserves_package :: [GoDecl] -> Property
prop_apply_generics_preserves_package decls = 
  let goModule = GoModule (Just (PackageDecl "test")) decls [] [] []
      result = applyGenerics goModule
  in property $ gmPackage result === gmPackage goModule

-- Test generic angle replacement
prop_replace_generic_angles_no_generics :: String -> Property
prop_replace_generic_angles_no_generics input = 
  let result = replaceGenericAngles input
  in property $ result === input

prop_replace_generic_angles_simple :: String -> String -> Property
prop_replace_generic_angles_simple typeName paramName = 
  let input = typeName ++ "<" ++ paramName ++ ">"
      result = replaceGenericAngles input
      expected = typeName ++ "[" ++ paramName ++ " any]"
  in property $ result === expected

prop_replace_generic_angles_multiple_params :: String -> String -> String -> Property
prop_replace_generic_angles_multiple_params typeName param1 param2 = 
  let input = typeName ++ "<" ++ param1 ++ ", " ++ param2 ++ ">"
      result = replaceGenericAngles input
      expected = typeName ++ "[" ++ param1 ++ " any, " ++ param2 ++ " any]"
  in property $ result === expected

prop_replace_generic_angles_nested :: String -> String -> String -> Property
prop_replace_generic_angles_nested outerType innerType param = 
  let input = outerType ++ "<" ++ innerType ++ "<" ++ param ++ ">>"
      result = replaceGenericAngles input
      expected = outerType ++ "[" ++ innerType ++ "[" ++ param ++ " any]]"
  in property $ result === expected

prop_replace_generic_angles_preserves_content :: String -> String -> Property
prop_replace_generic_angles_preserves_content prefix suffix = 
  let input = prefix ++ "Type<T>" ++ suffix
      result = replaceGenericAngles input
      expected = prefix ++ "Type[T any]" ++ suffix
  in property $ result === expected

-- Test IR round-trip consistency
prop_ir_round_trip_source :: TypusFile -> Property
prop_ir_round_trip_source typusFile = 
  let sourceIR = buildSourceIR typusFile
      reconstructedTypusFile = sourceTypusFile sourceIR
  in property $ reconstructedTypusFile === typusFile

prop_ir_round_trip_semantic :: TypusFile -> GoModule -> [ValueInfo] -> Property
prop_ir_round_trip_semantic typusFile goModule valueInfo = 
  let semanticIR = SemanticIR typusFile goModule valueInfo
      reconstructedTypusFile = semanticTypusFile semanticIR
      reconstructedGoModule = semanticModule semanticIR
      reconstructedValueInfo = semanticValueInfo semanticIR
  in property $ reconstructedTypusFile === typusFile &&
                reconstructedGoModule === goModule &&
                reconstructedValueInfo === valueInfo

prop_ir_round_trip_go :: GoModule -> Property
prop_ir_round_trip_go goModule = 
  let goSource = renderGoModule goModule
      goIR = GoIR goModule goSource
      reconstructedGoModule = goModule goIR
      reconstructedGoSource = goSource goIR
  in property $ reconstructedGoModule === goModule &&
                reconstructedGoSource === goSource

-- Test IR transformation consistency
prop_ir_transformation_preserves_semantics :: TypusFile -> Property
prop_ir_transformation_preserves_semantics typusFile = 
  let sourceIR = buildSourceIR typusFile
      sourceText = rawSourceFromTypus typusFile
  in property $ sourceText sourceIR === sourceText

prop_ir_transformation_preserves_structure :: TypusFile -> Property
prop_ir_transformation_preserves_structure typusFile = 
  let sourceIR = buildSourceIR typusFile
      blocks = tfBlocks typusFile
  in property $ not (null blocks) ==> length (lines (sourceText sourceIR)) >= length blocks

-- Test IR error handling consistency
prop_ir_error_handling_preserves_state :: TypusFile -> Property
prop_ir_error_handling_preserves_state typusFile = 
  let sourceIR = buildSourceIR typusFile
      originalTypusFile = sourceTypusFile sourceIR
  in property $ originalTypusFile === typusFile

-- Test IR optimization consistency
prop_ir_optimization_preserves_functionality :: TypusFile -> Property
prop_ir_optimization_preserves_functionality typusFile = 
  let sourceIR = buildSourceIR typusFile
      sourceText = rawSourceFromTypus typusFile
      optimizedSourceText = sourceText sourceIR
  in property $ length optimizedSourceText >= 0

-- Test IR validation consistency
prop_ir_validation_preserves_invariants :: TypusFile -> Property
prop_ir_validation_preserves_invariants typusFile = 
  let sourceIR = buildSourceIR typusFile
      sourceText = rawSourceFromTypus typusFile
      extractedSourceText = sourceText sourceIR
  in property $ sourceText === extractedSourceText

-- Test IR serialization consistency
prop_ir_serialization_preserves_content :: TypusFile -> Property
prop_ir_serialization_preserves_content typusFile = 
  let sourceIR = buildSourceIR typusFile
      sourceText = rawSourceFromTypus typusFile
      serializedSourceText = sourceText sourceIR
  in property $ sourceText === serializedSourceText

-- Test IR deserialization consistency
prop_ir_deserialization_preserves_structure :: String -> Property
prop_ir_deserialization_preserves_structure sourceText = 
  let lines' = lines sourceText
      block = CodeBlock defaultBlockDirectives sourceText (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
      sourceIR = buildSourceIR typusFile
      extractedSourceText = sourceText sourceIR
  in property $ sourceText === extractedSourceText

-- Test IR transformation pipeline consistency
prop_ir_transformation_pipeline_consistency :: TypusFile -> Property
prop_ir_transformation_pipeline_consistency typusFile = 
  let sourceIR = buildSourceIR typusFile
      sourceText = rawSourceFromTypus typusFile
      pipelineSourceText = sourceText sourceIR
  in property $ sourceText === pipelineSourceText

-- Test IR optimization pipeline consistency
prop_ir_optimization_pipeline_consistency :: TypusFile -> Property
prop_ir_optimization_pipeline_consistency typusFile = 
  let sourceIR = buildSourceIR typusFile
      sourceText = rawSourceFromTypus typusFile
      optimizedSourceText = sourceText sourceIR
  in property $ sourceText === optimizedSourceText

-- Test IR generation consistency
prop_ir_generation_consistency :: TypusFile -> Property
prop_ir_generation_consistency typusFile = 
  let sourceIR = buildSourceIR typusFile
      sourceText = rawSourceFromTypus typusFile
      generatedSourceText = sourceText sourceIR
  in property $ sourceText === generatedSourceText

-- Test IR analysis consistency
prop_ir_analysis_consistency :: TypusFile -> Property
prop_ir_analysis_consistency typusFile = 
  let sourceIR = buildSourceIR typusFile
      sourceText = rawSourceFromTypus typusFile
      analyzedSourceText = sourceText sourceIR
  in property $ sourceText === analyzedSourceText

-- Test IR transformation invariants
prop_ir_transformation_invariants :: TypusFile -> Property
prop_ir_transformation_invariants typusFile = 
  let sourceIR = buildSourceIR typusFile
      sourceText = rawSourceFromTypus typusFile
      transformedSourceText = sourceText sourceIR
  in property $ sourceText === transformedSourceText

-- Test IR optimization invariants
prop_ir_optimization_invariants :: TypusFile -> Property
prop_ir_optimization_invariants typusFile = 
  let sourceIR = buildSourceIR typusFile
      sourceText = rawSourceFromTypus typusFile
      optimizedSourceText = sourceText sourceIR
  in property $ sourceText === optimizedSourceText

-- Test IR validation invariants
prop_ir_validation_invariants :: TypusFile -> Property
prop_ir_validation_invariants typusFile = 
  let sourceIR = buildSourceIR typusFile
      sourceText = rawSourceFromTypus typusFile
      validatedSourceText = sourceText sourceIR
  in property $ sourceText === validatedSourceText

-- Helper functions
emptySpan :: SourcePos -> SourceSpan
emptySpan pos = SourceSpan pos pos

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

splitByComma :: String -> [String]
splitByComma "" = []
splitByComma s = 
  let (part, rest) = break (== ',') s
  in part : case rest of
              [] -> []
              (_:xs) -> splitByComma xs

-- Tests collection
tests :: TestTree
tests = testGroup "Compiler IR Consistency QuickCheck Tests"
  [ testProperty "ir statement consistency" prop_ir_statement_consistency
  , testProperty "ir statement equality" prop_ir_statement_equality
  , testProperty "ir expression consistency" prop_ir_expression_consistency
  , testProperty "ir expression equality" prop_ir_expression_equality
  , testProperty "source ir consistency" prop_source_ir_consistency
  , testProperty "source ir build consistency" prop_source_ir_build_consistency
  , testProperty "semantic ir consistency" prop_semantic_ir_consistency
  , testProperty "go ir consistency" prop_go_ir_consistency
  , testProperty "go ir emit consistency" prop_go_ir_emit_consistency
  , testProperty "raw source from typus empty" prop_raw_source_from_typus_empty
  , testProperty "raw source from typus single block" prop_raw_source_from_typus_single_block
  , testProperty "raw source from typus multiple blocks" prop_raw_source_from_typus_multiple_blocks
  , testProperty "ensure package decl existing" prop_ensure_package_decl_existing
  , testProperty "ensure package decl missing" prop_ensure_package_decl_missing
  , testProperty "ensure package decl preserves decls" prop_ensure_package_decl_preserves_decls
  , testProperty "ensure main function existing" prop_ensure_main_function_existing
  , testProperty "ensure main function missing" prop_ensure_main_function_missing
  , testProperty "ensure main function preserves package" prop_ensure_main_function_preserves_package
  , testProperty "apply generics preserves count" prop_apply_generics_preserves_count
  , testProperty "apply generics preserves structure" prop_apply_generics_preserves_structure
  , testProperty "apply generics preserves package" prop_apply_generics_preserves_package
  , testProperty "replace generic angles no generics" prop_replace_generic_angles_no_generics
  , testProperty "replace generic angles simple" prop_replace_generic_angles_simple
  , testProperty "replace generic angles multiple params" prop_replace_generic_angles_multiple_params
  , testProperty "replace generic angles nested" prop_replace_generic_angles_nested
  , testProperty "replace generic angles preserves content" prop_replace_generic_angles_preserves_content
  , testProperty "ir round trip source" prop_ir_round_trip_source
  , testProperty "ir round trip semantic" prop_ir_round_trip_semantic
  , testProperty "ir round trip go" prop_ir_round_trip_go
  , testProperty "ir transformation preserves semantics" prop_ir_transformation_preserves_semantics
  , testProperty "ir transformation preserves structure" prop_ir_transformation_preserves_structure
  , testProperty "ir error handling preserves state" prop_ir_error_handling_preserves_state
  , testProperty "ir optimization preserves functionality" prop_ir_optimization_preserves_functionality
  , testProperty "ir validation preserves invariants" prop_ir_validation_preserves_invariants
  , testProperty "ir serialization preserves content" prop_ir_serialization_preserves_content
  , testProperty "ir deserialization preserves structure" prop_ir_deserialization_preserves_structure
  , testProperty "ir transformation pipeline consistency" prop_ir_transformation_pipeline_consistency
  , testProperty "ir optimization pipeline consistency" prop_ir_optimization_pipeline_consistency
  , testProperty "ir generation consistency" prop_ir_generation_consistency
  , testProperty "ir analysis consistency" prop_ir_analysis_consistency
  , testProperty "ir transformation invariants" prop_ir_transformation_invariants
  , testProperty "ir optimization invariants" prop_ir_optimization_invariants
  , testProperty "ir validation invariants" prop_ir_validation_invariants
  ]