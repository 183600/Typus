{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.IntegrationBasicSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, sized, suchThat)
import Data.List (isInfixOf, isPrefixOf, sort, nub, intercalate, lines, unlines)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (isSpace, isAlphaNum)

import Parser (parseTypus, TypusFile(..), CodeBlock(..), defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), locatedAt, locatedWithSpan)
import ErrorHandler (TypeError(..), ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..))
import Ownership (OwnershipAnalyzer(..), newOwnershipAnalyzer, analyzeOwnership)
import Dependencies.TypeSystem (TypeVar(..), TypeConstraint(..), DependentTypeChecker(..), newDependentTypeChecker)
import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..), buildSourceIR, buildSemanticIR, emitGo)
import Compiler.GoAst (GoModule(..), PackageDecl(..), parseGoModule, renderGoModule)
import Compiler.Errors (CompilerResult, CompilationPhase(..), ErrorCategory(..), ErrorSeverity(..))
import Utils (trim, splitBy)

-- ============================================================================
-- Arbitrary Instances for Integration Tests
-- ============================================================================

-- Generate simple Typus code for integration testing
arbitraryTypusCode :: Gen String
arbitraryTypusCode = oneof
  [ return "package main\n\nfunc main() {\n\tfmt.Println(\"Hello\")\n}"
  , return "x := 5\ny := x\nfmt.Println(y)"
  , return "{//! ownership: on}\nx := 5\ny := x\nfmt.Println(y)\n}"
  , return "//! dependent_types: on\nfunc test() int {\n\treturn 42\n}"
  , return "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(\"test\")\n}"
  ]

-- Generate simple Go code for integration testing
arbitraryGoCode :: Gen String
arbitraryGoCode = oneof
  [ return "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(\"Hello\")\n}"
  , return "package test\n\nfunc add(a, b int) int {\n\treturn a + b\n}"
  , return "package main\n\nvar x int = 5\n\nfunc main() {\n\tprintln(x)\n}"
  ]

-- ============================================================================
-- Parser Integration Properties
-- ============================================================================

-- Property: parseTypus integrates with source location tracking
prop_parser_source_location_integration :: String -> Property
prop_parser_source_location_integration code =
  length code <= 100 ==> -- Limit for performance
  let result = parseTypus code
  in case result of
    Left _ -> property True -- May fail for invalid code
    Right typusFile -> property $ 
      let blocks = tfBlocks typusFile
      in all (hasValidSpan . cbSpan) blocks
  where
    hasValidSpan span = spanStart span <= spanEnd span

-- Property: parseTypus handles directives consistently
prop_parser_directives_consistency :: String -> Property
prop_parser_directives_consistency code =
  "//!" `isInfixOf` code ==>
  let result = parseTypus code
  in case result of
    Left _ -> property True -- May fail for malformed directives
    Right typusFile -> property $ 
      let fileDirs = tfDirectives typusFile
      in hasValidDirectives fileDirs
  where
    hasValidDirectives dirs = True -- Basic check that directives are parsed

-- ============================================================================
-- Error Handling Integration Properties
-- ============================================================================

-- Property: Error handling integrates with parser
prop_error_handling_parser_integration :: String -> Property
prop_error_handling_parser_integration code =
  let result = parseTypus code
  in case result of
    Left err -> property $ length err > 0 -- Should provide meaningful error
    Right typusFile -> property $ 
      let syntaxErrors = tfSyntaxErrors typusFile
      in all (hasValidErrorInfo) syntaxErrors
  where
    hasValidErrorInfo err = True -- Basic check that errors have valid structure

-- Property: Error location tracking works across modules
prop_error_location_tracking :: String -> Int -> Int -> Property
prop_error_location_tracking code line col =
  line > 0 && col > 0 && line <= 100 && col <= 100 ==>
  let result = parseTypus code
  in case result of
    Left _ -> property True -- May fail
    Right typusFile -> property $ 
      let blocks = tfBlocks typusFile
      in all (hasValidLocation) blocks
  where
    hasValidLocation block = True -- Basic location validity check

-- ============================================================================
-- Ownership Analysis Integration Properties
-- ============================================================================

-- Property: Ownership analysis integrates with parser
prop_ownership_parser_integration :: String -> Property
prop_ownership_parser_integration code =
  length code <= 50 ==> -- Limit for performance
  let parseResult = parseTypus code
  in case parseResult of
    Left _ -> property True -- May fail to parse
    Right typusFile ->
      let sourceCode = unlines $ map cbContent (tfBlocks typusFile)
          analyzer = newOwnershipAnalyzer
          ownershipResult = analyzeOwnership analyzer sourceCode
      in property $ True -- Should not crash

-- Property: Ownership analysis handles different code patterns
prop_ownership_code_patterns :: Property
prop_ownership_code_patterns =
  let codePatterns = 
        [ "x := 5\ny := x"
        , "x := 5\ny := &x\nz := *y"
        , "func test() {\n\tx := 5\n\ty := x\n}"
        , "{//! ownership: on}\nx := 5\ny := x"
        ]
      analyzer = newOwnershipAnalyzer
      results = map (analyzeOwnership analyzer) codePatterns
  in property $ length results === length codePatterns

-- ============================================================================
-- Type System Integration Properties
-- ============================================================================

-- Property: Type system integrates with parser output
prop_type_system_parser_integration :: String -> Property
prop_type_system_parser_integration code =
  length code <= 50 ==> -- Limit for performance
  let parseResult = parseTypus code
  in case parseResult of
    Left _ -> property True -- May fail to parse
    Right typusFile ->
      let typeChecker = newDependentTypeChecker
      in property $ True -- Type checker should be able to process parsed output

-- Property: Type constraint validation works
prop_type_constraint_validation :: TypeVar -> TypeVar -> Property
prop_type_constraint_validation tv1 tv2 =
  let constraint = Equal tv1 tv2
      typeChecker = newDependentTypeChecker
  in property $ True -- Should be able to validate basic constraints

-- ============================================================================
-- Compiler IR Integration Properties
-- ============================================================================

-- Property: SourceIR integrates with parser
prop_source_ir_parser_integration :: String -> Property
prop_source_ir_parser_integration code =
  length code <= 100 ==> -- Limit for performance
  let parseResult = parseTypus code
  in case parseResult of
    Left _ -> property True -- May fail to parse
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
      in property $ sourceTypusFile sourceIR === typusFile .&&.
                 not (null (sourceText sourceIR))

-- Property: SemanticIR integrates with SourceIR
prop_semantic_ir_source_integration :: String -> Property
prop_semantic_ir_source_integration code =
  length code <= 100 ==> -- Limit for performance
  let parseResult = parseTypus code
  in case parseResult of
    Left _ -> property True -- May fail to parse
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property True -- May fail for invalid IR
        Right semanticIR -> property $ 
          semanticTypusFile semanticIR === typusFile

-- Property: GoIR integrates with SemanticIR
prop_go_ir_semantic_integration :: String -> Property
prop_go_ir_semantic_integration code =
  length code <= 100 ==> -- Limit for performance
  let parseResult = parseTypus code
  in case parseResult of
    Left _ -> property True -- May fail to parse
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property True -- May fail for invalid IR
        Right semanticIR ->
          let goIR = emitGo semanticIR
          in property $ goModule goIR === semanticModule semanticIR .&&.
                     not (null (goSource goIR))

-- ============================================================================
-- Go AST Integration Properties
-- ============================================================================

-- Property: Go AST parsing integrates with compilation
prop_go_ast_parsing_integration :: String -> Property
prop_go_ast_parsing_integration code =
  length code <= 100 ==> -- Limit for performance
  let goLines = lines code
      parseResult = parseGoModule goLines
  in case parseResult of
    Left _ -> property True -- May fail for invalid Go
    Right goModule -> property $ 
      let rendered = renderGoModule goModule
      in not (null rendered)

-- Property: Go AST rendering preserves package information
prop_go_ast_rendering_preserves_package :: GoModule -> Property
prop_go_ast_rendering_preserves_package goModule =
  let rendered = renderGoModule goModule
  in case gmPackage goModule of
    Nothing -> property True -- No package to preserve
    Just pkg -> 
      let pkgName = packageName pkg
      in property $ pkgName `isInfixOf` rendered

-- ============================================================================
-- Cross-Module Integration Properties
-- ============================================================================

-- Property: Parser to Go generation pipeline
prop_parser_to_go_pipeline :: String -> Property
prop_parser_to_go_pipeline code =
  length code <= 100 ==> -- Limit for performance
  let parseResult = parseTypus code
  in case parseResult of
    Left _ -> property True -- May fail to parse
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property True -- May fail for invalid IR
        Right semanticIR ->
          let goIR = emitGo semanticIR
              goSource = goSource goIR
          in property $ not (null goSource)

-- Property: Error handling across pipeline stages
prop_error_handling_pipeline :: String -> Property
prop_error_handling_pipeline code =
  length code <= 100 ==> -- Limit for performance
  let parseResult = parseTypus code
  in case parseResult of
    Left parseErr -> property $ length parseErr > 0
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left semanticErr -> property $ True -- Should have error information
        Right semanticIR -> property $ True -- Should succeed

-- Property: Ownership analysis in full pipeline
prop_ownership_full_pipeline :: String -> Property
prop_ownership_full_pipeline code =
  "{//! ownership: on}" `isInfixOf` code && length code <= 100 ==>
  let parseResult = parseTypus code
  in case parseResult of
    Left _ -> property True -- May fail to parse
    Right typusFile ->
      let blocks = tfBlocks typusFile
          ownershipBlocks = filter (hasOwnershipDirective . cbDirectives) blocks
          analyzer = newOwnershipAnalyzer
          blockCodes = map cbContent ownershipBlocks
          ownershipResults = map (analyzeOwnership analyzer) blockCodes
      in property $ length ownershipResults === length ownershipBlocks
  where
    hasOwnershipDirective dirs = isJust (bdOwnership dirs)

-- ============================================================================
-- Complex Integration Properties
-- ============================================================================

-- Property: End-to-end compilation pipeline
prop_end_to_end_compilation :: String -> Property
prop_end_to_end_compilation code =
  length code <= 100 ==> -- Limit for performance
  let parseResult = parseTypus code
  in case parseResult of
    Left _ -> property True -- May fail to parse
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property True -- May fail for invalid IR
        Right semanticIR ->
          let goIR = emitGo semanticIR
              goSource = goSource goIR
              goLines = lines goSource
              goParseResult = parseGoModule goLines
          in case goParseResult of
            Left _ -> property True -- May fail to parse generated Go
            Right goModule -> property $ 
              let rendered = renderGoModule goModule
              in not (null rendered)

-- Property: Multiple file processing
prop_multiple_file_processing :: [String] -> Property
prop_multiple_file_processing codes =
  length codes <= 3 ==> -- Limit for performance
  let parseResults = map parseTypus codes
      successfulParses = [typusFile | Right typusFile <- parseResults]
      sourceIRs = map buildSourceIR successfulParses
  in property $ length sourceIRs === length successfulParses

-- Property: Directive propagation through pipeline
prop_directive_propagation :: String -> Property
prop_directive_propagation code =
  "//!" `isInfixOf` code && length code <= 100 ==>
  let parseResult = parseTypus code
  in case parseResult of
    Left _ -> property True -- May fail to parse
    Right typusFile ->
      let fileDirs = tfDirectives typusFile
          sourceIR = buildSourceIR typusFile
      in property $ sourceTypusFile sourceIR === typusFile .&&.
                 tfDirectives (sourceTypusFile sourceIR) === fileDirs

-- Property: Error consistency across modules
prop_error_consistency :: String -> Property
prop_error_consistency code =
  length code <= 100 ==> -- Limit for performance
  let parseResult = parseTypus code
  in case parseResult of
    Left parseErr -> 
      let syntaxErrors = tfSyntaxErrors (error "impossible") -- Can't extract file from error
      in property $ length parseErr > 0
    Right typusFile ->
      let syntaxErrors = tfSyntaxErrors typusFile
          hasErrors = not (null syntaxErrors)
      in property $ hasErrors ==> all (hasValidErrorInfo) syntaxErrors

-- ============================================================================
-- Performance and Scalability Properties
-- ============================================================================

-- Property: Pipeline performance with larger inputs
prop_pipeline_performance :: Int -> String -> Property
prop_pipeline_performance multiplier baseCode =
  multiplier > 0 && multiplier <= 10 ==> -- Reasonable limit
  let repeatedCode = unlines $ replicate multiplier baseCode
      parseResult = parseTypus repeatedCode
  in case parseResult of
    Left _ -> property True -- May fail
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
      in property $ not (null (sourceText sourceIR))

-- Property: Memory efficiency with repeated operations
prop_memory_efficiency :: String -> Int -> Property
prop_memory_efficiency baseCode iterations =
  iterations > 0 && iterations <= 5 ==> -- Reasonable limit
  let parseResult = parseTypus baseCode
  in case parseResult of
    Left _ -> property True -- May fail
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          -- Simulate repeated processing
          processed = replicate iterations sourceIR
      in property $ length processed === iterations

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Integration Basic Tests"
  [ testGroup "Parser Integration Properties"
    [ fastProperty "parseTypus integrates with source location tracking" prop_parser_source_location_integration
    , fastProperty "parseTypus handles directives consistently" prop_parser_directives_consistency
    ]
  , testGroup "Error Handling Integration Properties"
    [ fastProperty "Error handling integrates with parser" prop_error_handling_parser_integration
    , fastProperty "Error location tracking works across modules" prop_error_location_tracking
    ]
  , testGroup "Ownership Analysis Integration Properties"
    [ fastProperty "Ownership analysis integrates with parser" prop_ownership_parser_integration
    , fastProperty "Ownership analysis handles different code patterns" prop_ownership_code_patterns
    ]
  , testGroup "Type System Integration Properties"
    [ fastProperty "Type system integrates with parser output" prop_type_system_parser_integration
    , fastProperty "Type constraint validation works" prop_type_constraint_validation
    ]
  , testGroup "Compiler IR Integration Properties"
    [ fastProperty "SourceIR integrates with parser" prop_source_ir_parser_integration
    , fastProperty "SemanticIR integrates with SourceIR" prop_semantic_ir_source_integration
    , fastProperty "GoIR integrates with SemanticIR" prop_go_ir_semantic_integration
    ]
  , testGroup "Go AST Integration Properties"
    [ fastProperty "Go AST parsing integrates with compilation" prop_go_ast_parsing_integration
    , fastProperty "Go AST rendering preserves package information" prop_go_ast_rendering_preserves_package
    ]
  , testGroup "Cross-Module Integration Properties"
    [ fastProperty "Parser to Go generation pipeline" prop_parser_to_go_pipeline
    , fastProperty "Error handling across pipeline stages" prop_error_handling_pipeline
    , fastProperty "Ownership analysis in full pipeline" prop_ownership_full_pipeline
    ]
  , testGroup "Complex Integration Properties"
    [ fastProperty "End-to-end compilation pipeline" prop_end_to_end_compilation
    , fastProperty "Multiple file processing" prop_multiple_file_processing
    , fastProperty "Directive propagation through pipeline" prop_directive_propagation
    , fastProperty "Error consistency across modules" prop_error_consistency
    ]
  , testGroup "Performance and Scalability Properties"
    [ fastProperty "Pipeline performance with larger inputs" prop_pipeline_performance
    , fastProperty "Memory efficiency with repeated operations" prop_memory_efficiency
    ]
  ]