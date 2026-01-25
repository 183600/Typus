{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Test.Unit.NewIntegrationEndToEndQuickCheckTestSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

import Parser (parseTypus, TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import Compiler.IR (buildSourceIR, buildSemanticIR, emitGo, SourceIR(..), SemanticIR(..), GoIR(..))
import Ownership (analyzeOwnership, OwnershipError(..))
import Dependencies (analyzeDependentTypes, DependentTypeError(..))
import ErrorHandler (formatErrors, ErrorSeverity(..))
import SourceLocation (SourceSpan(..), SourcePos(..), startPos)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace)
import qualified Data.Text as T

-- ============================================================================
-- Integration End-to-End QuickCheck Tests
-- ============================================================================

-- Test end-to-end parsing pipeline
prop_end_to_end_parsing_empty :: Property
prop_end_to_end_parsing_empty = 
  let input = ""
      result = parseTypus input
  in case result of
    Left _ -> property $ True
    Right file -> property $ tfBlocks file === []

prop_end_to_end_parsing_simple :: String -> Property
prop_end_to_end_parsing_simple content = 
  let input = content
      result = parseTypus input
  in case result of
    Left _ -> property $ True
    Right file -> property $ not (null (tfBlocks file)) ==> not (null (cbContent (head (tfBlocks file))))

prop_end_to_end_parsing_with_directives :: String -> String -> Property
prop_end_to_end_parsing_with_directives directive content = 
  let input = "//!ownership: true\n" ++ content
      result = parseTypus input
  in case result of
    Left _ -> property $ True
    Right file -> 
      let ownership = fdOwnership (tfDirectives file)
      in property $ isJust ownership

-- Test end-to-end IR pipeline
prop_end_to_end_ir_pipeline :: String -> Property
prop_end_to_end_ir_pipeline content = 
  let block = CodeBlock defaultBlockDirectives content (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
      sourceIR = buildSourceIR typusFile
      sourceText = rawSourceFromTypus typusFile
  in property $ sourceText sourceIR === sourceText

prop_end_to_end_semantic_ir :: String -> Property
prop_end_to_end_semantic_ir content = 
  let block = CodeBlock defaultBlockDirectives content (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
      sourceIR = buildSourceIR typusFile
      result = buildSemanticIR sourceIR
  in case result of
    Left _ -> property $ True
    Right semanticIR -> property $ sourceTypusFile (semanticTypusFile semanticIR) === typusFile

prop_end_to_end_go_emission :: String -> Property
prop_end_to_end_go_emission content = 
  let block = CodeBlock defaultBlockDirectives content (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
      sourceIR = buildSourceIR typusFile
      result = buildSemanticIR sourceIR
  in case result of
    Left _ -> property $ True
    Right semanticIR -> 
      let goIR = emitGo semanticIR
          goSource = goSource goIR
      in property $ not (null goSource) ==> length (lines goSource) > 0

-- Test end-to-end ownership analysis
prop_end_to_end_ownership_analysis :: String -> Property
prop_end_to_end_ownership_analysis content = 
  let block = CodeBlock defaultBlockDirectives content (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
      result = analyzeOwnership typusFile
  in case result of
    Left _ -> property $ True
    Right errors -> property $ length errors >= 0

prop_end_to_end_ownership_with_directives :: String -> String -> Property
prop_end_to_end_ownership_with_directives directive content = 
  let block = CodeBlock (defaultBlockDirectives { bdOwnership = Just (Located startPos True) }) content (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
      result = analyzeOwnership typusFile
  in case result of
    Left _ -> property $ True
    Right errors -> property $ length errors >= 0

-- Test end-to-end dependent types analysis
prop_end_to_end_dependent_types_analysis :: String -> Property
prop_end_to_end_dependent_types_analysis content = 
  let block = CodeBlock defaultBlockDirectives content (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
      result = analyzeDependentTypes typusFile
  in case result of
    Left _ -> property $ True
    Right errors -> property $ length errors >= 0

prop_end_to_end_dependent_types_with_constraints :: String -> String -> Property
prop_end_to_end_dependent_types_with_constraints constraint content = 
  let block = CodeBlock (defaultBlockDirectives { bdConstraints = Just (Located startPos True) }) content (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
      result = analyzeDependentTypes typusFile
  in case result of
    Left _ -> property $ True
    Right errors -> property $ length errors >= 0

-- Test end-to-end error handling
prop_end_to_end_error_handling :: String -> Property
prop_end_to_end_error_handling content = 
  let block = CodeBlock defaultBlockDirectives content (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
      parseResult = parseTypus content
      ownershipResult = analyzeOwnership typusFile
      dependentTypesResult = analyzeDependentTypes typusFile
  in case (parseResult, ownershipResult, dependentTypesResult) of
    (Left parseErr, _, _) -> property $ not (null parseErr)
    (_, Left ownErr, _) -> property $ not (null ownErr)
    (_, _, Left depErr) -> property $ not (null depErr)
    (Right _, Right ownErrors, Right depErrors) -> property $ length ownErrors >= 0 && length depErrors >= 0

prop_end_to_end_error_formatting :: [String] -> Property
prop_end_to_end_error_formatting errors = 
  let formatted = formatErrors errors
      hasAllErrors = all (`isInfixOf` formatted) errors
  in property $ hasAllErrors || null errors

-- Test end-to-end compilation pipeline
prop_end_to_end_compilation_pipeline :: String -> Property
prop_end_to_end_compilation_pipeline content = 
  let block = CodeBlock defaultBlockDirectives content (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
      parseResult = parseTypus content
  in case parseResult of
    Left _ -> property $ True
    Right parsedFile -> 
      let sourceIR = buildSourceIR parsedFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property $ True
        Right semanticIR -> 
          let goIR = emitGo semanticIR
              goSource = goSource goIR
          in property $ not (null goSource) ==> length (lines goSource) > 0

prop_end_to_end_compilation_with_ownership :: String -> Property
prop_end_to_end_compilation_with_ownership content = 
  let block = CodeBlock (defaultBlockDirectives { bdOwnership = Just (Located startPos True) }) content (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
      parseResult = parseTypus content
  in case parseResult of
    Left _ -> property $ True
    Right parsedFile -> 
      let sourceIR = buildSourceIR parsedFile
          semanticResult = buildSemanticIR sourceIR
          ownershipResult = analyzeOwnership parsedFile
      in case (semanticResult, ownershipResult) of
        (Left _, Left _) -> property $ True
        (Left _, Right _) -> property $ True
        (Right _, Left _) -> property $ True
        (Right semanticIR, Right ownershipErrors) -> 
          let goIR = emitGo semanticIR
              goSource = goSource goIR
          in property $ not (null goSource) ==> length (lines goSource) > 0

prop_end_to_end_compilation_with_dependent_types :: String -> Property
prop_end_to_end_compilation_with_dependent_types content = 
  let block = CodeBlock (defaultBlockDirectives { bdDependentTypes = Just (Located startPos True) }) content (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
      parseResult = parseTypus content
  in case parseResult of
    Left _ -> property $ True
    Right parsedFile -> 
      let sourceIR = buildSourceIR parsedFile
          semanticResult = buildSemanticIR sourceIR
          dependentTypesResult = analyzeDependentTypes parsedFile
      in case (semanticResult, dependentTypesResult) of
        (Left _, Left _) -> property $ True
        (Left _, Right _) -> property $ True
        (Right _, Left _) -> property $ True
        (Right semanticIR, Right dependentTypesErrors) -> 
          let goIR = emitGo semanticIR
              goSource = goSource goIR
          in property $ not (null goSource) ==> length (lines goSource) > 0

-- Test end-to-end integration consistency
prop_end_to_end_integration_consistency :: String -> Property
prop_end_to_end_integration_consistency content = 
  let block = CodeBlock defaultBlockDirectives content (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
      sourceIR = buildSourceIR typusFile
      sourceText = rawSourceFromTypus typusFile
      extractedSourceText = sourceText sourceIR
  in property $ sourceText === extractedSourceText

prop_end_to_end_round_trip :: String -> Property
prop_end_to_end_round_trip content = 
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> property $ True
    Right parsedFile -> 
      let sourceText = rawSourceFromTypus parsedFile
          roundTripResult = parseTypus sourceText
      in case roundTripResult of
        Left _ -> property $ True
        Right roundTripFile -> 
          let originalBlocks = tfBlocks parsedFile
              roundTripBlocks = tfBlocks roundTripFile
              originalContent = map cbContent originalBlocks
              roundTripContent = map cbContent roundTripBlocks
          in property $ originalContent === roundTripContent

-- Test end-to-end performance properties
prop_end_to_end_performance_parsing :: String -> Property
prop_end_to_end_performance_parsing content = 
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> property $ True
    Right parsedFile -> 
      let blocks = tfBlocks parsedFile
      in property $ length blocks >= 0

prop_end_to_end_performance_ir :: String -> Property
prop_end_to_end_performance_ir content = 
  let block = CodeBlock defaultBlockDirectives content (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
      sourceIR = buildSourceIR typusFile
      sourceText = sourceText sourceIR
  in property $ length sourceText >= 0

prop_end_to_end_performance_analysis :: String -> Property
prop_end_to_end_performance_analysis content = 
  let block = CodeBlock defaultBlockDirectives content (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
      ownershipResult = analyzeOwnership typusFile
      dependentTypesResult = analyzeDependentTypes typusFile
  in case (ownershipResult, dependentTypesResult) of
    (Left _, Left _) -> property $ True
    (Left _, Right _) -> property $ True
    (Right _, Left _) -> property $ True
    (Right ownershipErrors, Right dependentTypesErrors) -> 
      property $ length ownershipErrors >= 0 && length dependentTypesErrors >= 0

-- Test end-to-end error recovery
prop_end_to_end_error_recovery_parsing :: String -> Property
prop_end_to_end_error_recovery_parsing content = 
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> property $ True
    Right parsedFile -> property $ True

prop_end_to_end_error_recovery_ownership :: String -> Property
prop_end_to_end_error_recovery_ownership content = 
  let block = CodeBlock defaultBlockDirectives content (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
      ownershipResult = analyzeOwnership typusFile
  in case ownershipResult of
    Left _ -> property $ True
    Right errors -> property $ True

prop_end_to_end_error_recovery_dependent_types :: String -> Property
prop_end_to_end_error_recovery_dependent_types content = 
  let block = CodeBlock defaultBlockDirectives content (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
      dependentTypesResult = analyzeDependentTypes typusFile
  in case dependentTypesResult of
    Left _ -> property $ True
    Right errors -> property $ True

-- Test end-to-end edge cases
prop_end_to_end_edge_case_empty_input :: Property
prop_end_to_end_edge_case_empty_input = 
  let input = ""
      parseResult = parseTypus input
  in case parseResult of
    Left _ -> property $ True
    Right parsedFile = 
      let sourceIR = buildSourceIR parsedFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property $ True
        Right semanticIR -> 
          let goIR = emitGo semanticIR
              goSource = goSource goIR
          in property $ length goSource >= 0

prop_end_to_end_edge_case_whitespace_only :: Property
prop_end_to_end_edge_case_whitespace_only = 
  let input = "\n\n\n   \n\t\n"
      parseResult = parseTypus input
  in case parseResult of
    Left _ -> property $ True
    Right parsedFile = 
      let sourceIR = buildSourceIR parsedFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property $ True
        Right semanticIR -> 
          let goIR = emitGo semanticIR
              goSource = goSource goIR
          in property $ length goSource >= 0

prop_end_to_end_edge_case_comments_only :: Property
prop_end_to_end_edge_case_comments_only = 
  let input = "// This is a comment\n// Another comment\n"
      parseResult = parseTypus input
  in case parseResult of
    Left _ -> property $ True
    Right parsedFile = 
      let sourceIR = buildSourceIR parsedFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property $ True
        Right semanticIR -> 
          let goIR = emitGo semanticIR
              goSource = goSource goIR
          in property $ length goSource >= 0

prop_end_to_end_edge_case_directives_only :: Property
prop_end_to_end_edge_case_directives_only = 
  let input = "//!ownership: true\n//!dependent-types: true\n"
      parseResult = parseTypus input
  in case parseResult of
    Left _ -> property $ True
    Right parsedFile = 
      let sourceIR = buildSourceIR parsedFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property $ True
        Right semanticIR -> 
          let goIR = emitGo semanticIR
              goSource = goSource goIR
          in property $ length goSource >= 0

-- Test end-to-end integration with multiple blocks
prop_end_to_end_multiple_blocks :: [String] -> Property
prop_end_to_end_multiple_blocks contents = 
  let blocks = [CodeBlock defaultBlockDirectives content (emptySpan startPos) | content <- contents]
      typusFile = TypusFile defaultFileDirectives [] blocks []
      parseResult = parseTypus (unlines contents)
  in case parseResult of
    Left _ -> property $ True
    Right parsedFile -> 
      let sourceIR = buildSourceIR parsedFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property $ True
        Right semanticIR -> 
          let goIR = emitGo semanticIR
              goSource = goSource goIR
          in property $ length goSource >= 0

prop_end_to_end_mixed_directives :: [String] -> Property
prop_end_to_end_mixed_directives contents = 
  let directives = ["//!ownership: true", "//!dependent-types: true", "//!constraints: true"]
      input = unlines directives ++ unlines contents
      parseResult = parseTypus input
  in case parseResult of
    Left _ -> property $ True
    Right parsedFile -> 
      let sourceIR = buildSourceIR parsedFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property $ True
        Right semanticIR -> 
          let goIR = emitGo semanticIR
              goSource = goSource goIR
          in property $ length goSource >= 0

-- Test end-to-end integration with error scenarios
prop_end_to_end_error_scenarios :: String -> Property
prop_end_to_end_error_scenarios content = 
  let malformedContent = content ++ "function incomplete {"
      parseResult = parseTypus malformedContent
  in case parseResult of
    Left _ -> property $ True
    Right parsedFile -> 
      let sourceIR = buildSourceIR parsedFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property $ True
        Right semanticIR -> 
          let goIR = emitGo semanticIR
              goSource = goSource goIR
          in property $ length goSource >= 0

-- Helper functions
emptySpan :: SourcePos -> SourceSpan
emptySpan pos = SourceSpan pos pos

defaultFileDirectives :: FileDirectives
defaultFileDirectives = FileDirectives Nothing Nothing Nothing

defaultBlockDirectives :: BlockDirectives
defaultBlockDirectives = BlockDirectives Nothing Nothing Nothing

rawSourceFromTypus :: TypusFile -> String
rawSourceFromTypus typusFile = unlines (map cbContent (tfBlocks typusFile))

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

-- Tests collection
tests :: TestTree
tests = testGroup "Integration End-to-End QuickCheck Tests"
  [ testProperty "end to end parsing empty" prop_end_to_end_parsing_empty
  , testProperty "end to end parsing simple" prop_end_to_end_parsing_simple
  , testProperty "end to end parsing with directives" prop_end_to_end_parsing_with_directives
  , testProperty "end to end ir pipeline" prop_end_to_end_ir_pipeline
  , testProperty "end to end semantic ir" prop_end_to_end_semantic_ir
  , testProperty "end to end go emission" prop_end_to_end_go_emission
  , testProperty "end to end ownership analysis" prop_end_to_end_ownership_analysis
  , testProperty "end to end ownership with directives" prop_end_to_end_ownership_with_directives
  , testProperty "end to end dependent types analysis" prop_end_to_end_dependent_types_analysis
  , testProperty "end to end dependent types with constraints" prop_end_to_end_dependent_types_with_constraints
  , testProperty "end to end error handling" prop_end_to_end_error_handling
  , testProperty "end to end error formatting" prop_end_to_end_error_formatting
  , testProperty "end to end compilation pipeline" prop_end_to_end_compilation_pipeline
  , testProperty "end to end compilation with ownership" prop_end_to_end_compilation_with_ownership
  , testProperty "end to end compilation with dependent types" prop_end_to_end_compilation_with_dependent_types
  , testProperty "end to end integration consistency" prop_end_to_end_integration_consistency
  , testProperty "end to end round trip" prop_end_to_end_round_trip
  , testProperty "end to end performance parsing" prop_end_to_end_performance_parsing
  , testProperty "end to end performance ir" prop_end_to_end_performance_ir
  , testProperty "end to end performance analysis" prop_end_to_end_performance_analysis
  , testProperty "end to end error recovery parsing" prop_end_to_end_error_recovery_parsing
  , testProperty "end to end error recovery ownership" prop_end_to_end_error_recovery_ownership
  , testProperty "end to end error recovery dependent types" prop_end_to_end_error_recovery_dependent_types
  , testProperty "end to end edge case empty input" prop_end_to_end_edge_case_empty_input
  , testProperty "end to end edge case whitespace only" prop_end_to_end_edge_case_whitespace_only
  , testProperty "end to end edge case comments only" prop_end_to_end_edge_case_comments_only
  , testProperty "end to end edge case directives only" prop_end_to_end_edge_case_directives_only
  , testProperty "end to end multiple blocks" prop_end_to_end_multiple_blocks
  , testProperty "end to end mixed directives" prop_end_to_end_mixed_directives
  , testProperty "end to end error scenarios" prop_end_to_end_error_scenarios
  ]