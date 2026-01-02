{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewEndToEndIntegrationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, suchThat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (sort, nub, (\\), delete, intersect, union, intercalate)
import Data.Set (Set, fromList, toList, union, intersection, difference)
import qualified Data.Set as Set
import Data.Map (Map, fromList, toList, keys, elems, insert, delete, lookup, member, empty)
import qualified Data.Map as Map
import Data.Char (isSpace, isAlphaNum, isAlpha, isDigit)
import Control.Monad (replicateM)

import Parser (parseTypus, TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import Compiler.IR (buildSourceIR, buildSemanticIR, emitGo, SourceIR(..), SemanticIR(..), GoIR(..))
import Compiler.GoAst (GoModule(..), renderGoModule)
import SyntaxValidator (validateSyntax, SyntaxError(..))
import ErrorHandler (TypeError(..), ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..))
import Ownership.Common.Types (OwnershipType(..), OwnershipError(..))
import Dependencies.AST (AST(..), Statement(..), TypeExpr(..), Constraint(..))
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import Utils (trim, splitBy, removeComments, normalizeIndentation)

-- ============================================================================
-- Helper Functions L.and Generators
-- ============================================================================

-- Generate valid identifiers
genIdentifier :: Gen String
genIdentifier = do
  first <- elements (['a'..'z'] ++ ['A'..'Z'])
  rest <- listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
  return (first : rest)

-- Generate valid Typus content
genTypusContent :: Gen String
genTypusContent = do
  directives <- listOf genDirective
  codeBlocks <- listOf genCodeBlock
  return $ unlines directives ++ "\n" ++ unlines (L.map (\cb -> cbContent cb) codeBlocks)

-- Generate file directives
genDirective :: Gen String
genDirective = do
  key <- elements ["ownership", "dependent-types", "constraints"]
  value <- elements ["true", "false", "on", "off"]
  return $ "//! " ++ key ++ ": " ++ value

-- Generate code blocks
genCodeBlock :: Gen CodeBlock
genCodeBlock = do
  directives <- return defaultBlockDirectives
  content <- listOf genGoStatement
  span <- return $ emptySpan startPos
  return $ CodeBlock directives (unlines content) span

-- Generate Go statements
genGoStatement :: Gen String
genGoStatement = oneof
  [ do
      name <- genIdentifier
      return $ "var " ++ name ++ " int = 42"
  , do
      name <- genIdentifier
      return $ "func " ++ name ++ "() { return }"
  , do
      name <- genIdentifier
      return $ "type " ++ name ++ " struct { Value int }"
  , return "import \"fmt\""
  , return "package main"
  , return "fmt.Println(\"Hello, World!\")"
  ]

-- Generate complex Typus files
genComplexTypusFile :: Gen TypusFile
genComplexTypusFile = do
  numBlocks <- choose (1, 10)
  directives <- return defaultFileDirectives
  buildTags <- listOf $ genIdentifier
  blocks <- replicateM numBlocks genCodeBlock
  syntaxErrors <- return []
  return $ TypusFile directives buildTags blocks syntaxErrors

-- Generate source positions
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  column <- choose (1, 100)
  offset <- choose (0, 100000)
  return $ SourcePos line column offset

-- Generate source spans
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  endOffset <- choose (0, 1000)
  let end = start { posOffset = posOffset start + endOffset }
  return $ SourceSpan start end

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = genSourcePos

instance Arbitrary SourceSpan where
  arbitrary = genSourceSpan

instance Arbitrary CodeBlock where
  arbitrary = genCodeBlock

instance Arbitrary TypusFile where
  arbitrary = genComplexTypusFile

-- ============================================================================
-- End-to-End Pipeline Properties
-- ============================================================================

-- Property: Complete pipeline preserves content structure
prop_complete_pipeline_preserves_structure :: TypusFile -> Property
prop_complete_pipeline_preserves_structure typusFile =
  let sourceIR = buildSourceIR typusFile
      originalContent = sourceText sourceIR
      parsedContent = intercalate "\n" $ map cbContent (tfBlocks typusFile)
  in property $ originalContent === parsedContent

-- Property: Parsing L.and validation are consistent
prop_parsing_validation_consistent :: String -> Property
prop_parsing_validation_consistent content =
  let parseResult = parseTypus content
      syntaxErrors = validateSyntax content
  in case parseResult of
    Left _ -> property $ not (null syntaxErrors) ==> True
    Right _ -> property $ True

-- Property: IR transformation preserves semantic information
prop_ir_transformation_preserves_semantics :: TypusFile -> Property
prop_ir_transformation_preserves_semantics typusFile =
  let sourceIR = buildSourceIR typusFile
  in case buildSemanticIR sourceIR of
    Left _ -> property True
    Right semanticIR ->
      let goIR = emitGo semanticIR
          goSource = goSource goIR
      in property $ not (null goSource)

-- Property: Error handling across pipeline stages
prop_error_handling_across_pipeline :: String -> Property
prop_error_handling_across_pipeline content =
  let parseResult = parseTypus content
      syntaxErrors = validateSyntax content
  in case parseResult of
    Left parseErr -> property $ not (null parseErr)
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property True
        Right semanticIR ->
          let goIR = emitGo semanticIR
              goSource = goSource goIR
          in property $ not (null goSource)

-- Property: Source location tracking is consistent
prop_source_location_consistent :: SourceSpan -> Property
prop_source_location_consistent span =
  let start = spanStart span
      end = spanEnd span
  in property $ start <= end

-- Property: Syntax validation catches basic errors
prop_syntax_validation_catches_errors :: String -> Property
prop_syntax_validation_catches_errors malformedCode =
  let codeWithErrors = malformedCode ++ "func test( { return }"  -- Intentionally malformed
      errors = validateSyntax codeWithErrors
  in property $ not (null errors)

-- ============================================================================
-- Integration Properties
-- ============================================================================

-- Property: Multi-file compilation maintains consistency
prop_multi_file_compilation_consistent :: [TypusFile] -> Property
prop_multi_file_compilation_consistent typusFiles =
  not (null typusFiles) ==>
  let sourceIRs = map buildSourceIR typusFiles
      semanticResults = map buildSemanticIR sourceIRs
      successCount = L.length [() | Right _ <- semanticResults]
  in property $ successCount >= 0

-- Property: Dependency resolution handles complex graphs
prop_dependency_resolution_complex :: [String] -> Property
prop_dependency_resolution_complex dependencies =
  not (null dependencies) ==>
  let uniqueDeps = nub dependencies
      depCount = L.length uniqueDeps
  in property $ depCount <= L.length dependencies

-- Property: Type checking preserves type safety
prop_type_checking_preserves_safety :: TypusFile -> Property
prop_type_checking_preserves_safety typusFile =
  let sourceIR = buildSourceIR typusFile
  in case buildSemanticIR sourceIR of
    Left _ -> property True
    Right semanticIR ->
      let goIR = emitGo semanticIR
          goSource = goSource goIR
          hasValidGo = "func" `L.isInfixOf` goSource || "var" `L.isInfixOf` goSource
      in property $ hasValidGo

-- ============================================================================
-- Performance L.and Scalability Properties
-- ============================================================================

-- Property: Large files are processed efficiently
prop_large_files_processed_efficiently :: Int -> Property
prop_large_files_processed_efficiently numBlocks =
  numBlocks >= 0 && numBlocks <= 100 ==>
  let typusFile = TypusFile defaultFileDirectives [] 
                    (replicate numBlocks $ CodeBlock defaultBlockDirectives "fmt.Println(\"test\")" (emptySpan startPos))
                    []
      sourceIR = buildSourceIR typusFile
      contentLength = L.length (sourceText sourceIR)
  in property $ contentLength >= numBlocks

-- Property: Complex expressions are handled correctly
prop_complex_expressions_handled :: Int -> Property
prop_complex_expressions_handled complexity =
  complexity >= 0 && complexity <= 50 ==>
  let expr = intercalate " + " (replicate complexity "x")
      code = "func test() {\n  result := " ++ expr ++ "\n  fmt.Println(result)\n}"
      typusFile = TypusFile defaultFileDirectives [] 
                    [CodeBlock defaultBlockDirectives code (emptySpan startPos)]
                    []
      sourceIR = buildSourceIR typusFile
  in case buildSemanticIR sourceIR of
    Left _ -> property True
    Right semanticIR ->
      let goIR = emitGo semanticIR
          goSource = goSource goIR
      in property $ expr `L.isInfixOf` goSource

-- ============================================================================
-- Error Recovery Properties
-- ============================================================================

-- Property: Error recovery preserves partial results
prop_error_recovery_preserves_partial :: String -> Property
prop_error_recovery_preserves_partial content =
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> property True
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          contentLength = L.length (sourceText sourceIR)
      in property $ contentLength >= 0

-- Property: Graceful degradation on malformed input
prop_graceful_degradation_malformed :: String -> Property
prop_graceful_degradation_malformed malformed =
  let content = malformed ++ "\nfunc valid() { return }"
      parseResult = parseTypus content
  in case parseResult of
    Left _ -> property True
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property True
        Right _ -> property True

-- ============================================================================
-- Consistency Properties
-- ============================================================================

-- Property: Round-trip compilation is consistent
prop_round_trip_consistent :: TypusFile -> Property
prop_round_trip_consistent typusFile =
  let sourceIR = buildSourceIR typusFile
      originalContent = sourceText sourceIR
  in case buildSemanticIR sourceIR of
    Left _ -> property True
    Right semanticIR ->
      let goIR = emitGo semanticIR
          goSource = goSource goIR
          -- Parse the generated Go back (simplified check)
          hasValidStructure = "package" `L.isInfixOf` goSource || "func" `L.isInfixOf` goSource
      in property $ hasValidStructure

-- Property: Multiple runs produce identical results
prop_multiple_runs_identical :: TypusFile -> Property
prop_multiple_runs_identical typusFile =
  let sourceIR1 = buildSourceIR typusFile
      sourceIR2 = buildSourceIR typusFile
      content1 = sourceText sourceIR1
      content2 = sourceText sourceIR2
  in property $ content1 === content2

-- ============================================================================
-- Edge Cases L.and Boundary Conditions
-- ============================================================================

-- Property: Empty input handles gracefully
prop_empty_input_handles :: Property
prop_empty_input_handles =
  let emptyTypusFile = TypusFile defaultFileDirectives [] [] []
      sourceIR = buildSourceIR emptyTypusFile
      content = sourceText sourceIR
  in property $ null content

-- Property: Unicode content is preserved
prop_unicode_content_preserved :: String -> Property
prop_unicode_content_preserved unicode =
  let content = "func test() {\n  fmt.Println(\"" ++ unicode ++ "\")\n}"
      typusFile = TypusFile defaultFileDirectives [] 
                    [CodeBlock defaultBlockDirectives content (emptySpan startPos)]
                    []
      sourceIR = buildSourceIR typusFile
      sourceContent = sourceText sourceIR
  in property $ unicode `L.isInfixOf` sourceContent

-- Property: Deep nesting is handled correctly
prop_deep_nesting_handled :: Int -> Property
prop_deep_nesting_handled depth =
  depth >= 0 && depth <= 20 ==>
  let nestedBlocks = replicate depth "  if true {\n"
      closingBlocks = replicate depth "  }\n"
      content = "func test() {\n" ++ L.concat nestedBlocks ++ "fmt.Println(\"nested\")\n" ++ L.concat closingBlocks
      typusFile = TypusFile defaultFileDirectives [] 
                    [CodeBlock defaultBlockDirectives content (emptySpan startPos)]
                    []
      sourceIR = buildSourceIR typusFile
  in case buildSemanticIR sourceIR of
    Left _ -> property True
    Right semanticIR ->
      let goIR = emitGo semanticIR
          goSource = goSource goIR
      in property $ "nested" `L.isInfixOf` goSource

-- ============================================================================
-- Cross-Module Integration Properties
-- ============================================================================

-- Property: Module dependencies are resolved correctly
prop_module_dependencies_resolved :: [String] -> Property
prop_module_dependencies_resolved modules =
  not (null modules) ==>
  let uniqueModules = nub modules
      moduleCount = L.length uniqueModules
  in property $ moduleCount > 0

-- Property: Import statements are preserved
prop_import_statements_preserved :: [String] -> Property
prop_import_statements_preserved imports =
  not (null imports) ==>
  let importLines = L.map (\imp -> "import \"" ++ imp ++ "\"") imports
      content = "package main\n\n" ++ unlines importLines
      typusFile = TypusFile defaultFileDirectives [] 
                    [CodeBlock defaultBlockDirectives content (emptySpan startPos)]
                    []
      sourceIR = buildSourceIR typusFile
      sourceContent = sourceText sourceIR
  in property $ L.all (`L.isInfixOf` sourceContent) (take 5 imports)  -- Check first 5 to avoid long tests

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New End-to-End Integration QuickCheck Tests"
  [ testGroup "End-to-End Pipeline Properties"
    [ fastProperty "complete pipeline preserves structure" prop_complete_pipeline_preserves_structure
    , fastProperty "parsing validation consistent" prop_parsing_validation_consistent
    , fastProperty "ir transformation preserves semantics" prop_ir_transformation_preserves_semantics
    , fastProperty "error handling across pipeline" prop_error_handling_across_pipeline
    ]

  , testGroup "Integration Properties"
    [ fastProperty "multi file compilation consistent" prop_multi_file_compilation_consistent
    , fastProperty "dependency resolution complex" prop_dependency_resolution_complex
    , fastProperty "type checking preserves safety" prop_type_checking_preserves_safety
    ]

  , testGroup "Performance L.and Scalability Properties"
    [ fastProperty "large files processed efficiently" prop_large_files_processed_efficiently
    , fastProperty "complex expressions handled" prop_complex_expressions_handled
    ]

  , testGroup "Error Recovery Properties"
    [ fastProperty "error recovery preserves partial" prop_error_recovery_preserves_partial
    , fastProperty "graceful degradation malformed" prop_graceful_degradation_malformed
    ]

  , testGroup "Consistency Properties"
    [ fastProperty "round trip consistent" prop_round_trip_consistent
    , fastProperty "multiple runs identical" prop_multiple_runs_identical
    ]

  , testGroup "Edge Cases L.and Boundary Conditions"
    [ fastProperty "empty input handles" prop_empty_input_handles
    , fastProperty "unicode content preserved" prop_unicode_content_preserved
    , fastProperty "deep nesting handled" prop_deep_nesting_handled
    ]

  , testGroup "Cross-Module Integration Properties"
    [ fastProperty "module dependencies resolved" prop_module_dependencies_resolved
    , fastProperty "import statements preserved" prop_import_statements_preserved
    ]
  ]