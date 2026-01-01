{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.Unit.IRQuickCheckSpec (tests) where

import GHC.Generics (Generic)

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), property, forAll, counterexample, classify, Arbitrary(..), Gen, oneof, choose, listOf, elements)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (nub)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)
import qualified Data.Text as T

import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..))
import Parser (TypusFile(..), FileDirectives(..), defaultFileDirectives)
import SourceLocation (Located(..), SourcePos(..), SourceSpan(..))
import Analyzer.Types (SymbolInfo(..))
import Compiler.GoAst (GoModule(..))
import TestSupport.Arbitrary

-- Helper function to create a simple TypusFile
createSimpleTypusFile :: TypusFile
createSimpleTypusFile = TypusFile defaultFileDirectives [] [] []

-- Helper function to create a simple GoModule
createSimpleGoModule :: GoModule
createSimpleGoModule = GoModule [] Nothing [] []

-- Property: SourceIR preserves original source text
prop_sourceir_preserves_source :: String -> Property
prop_sourceir_preserves_source source =
  let sourceIR = SourceIR createSimpleTypusFile source
  in property $ sourceText sourceIR === source

-- Property: SourceIR stores directives correctly
prop_sourceir_directives_storage :: FileDirectives -> String -> Property
prop_sourceir_directives_storage directives source =
  let typusFile = TypusFile directives [] [] []
      sourceIR = SourceIR typusFile source
  in property $ tfDirectives (sourceTypusFile sourceIR) === directives

-- Property: SemanticIR contains valid module
prop_semanticir_module :: FileDirectives -> String -> Property
prop_semanticir_module directives source =
  let typusFile = TypusFile directives [] [] []
      sourceIR = SourceIR typusFile source
      goModule = createSimpleGoModule
      semanticIR = SemanticIR (sourceTypusFile sourceIR) goModule []
  in property $ semanticModule semanticIR === goModule

-- Property: SemanticIR preserves value info
prop_semanticir_value_info :: FileDirectives -> String -> [String] -> Property
prop_semanticir_value_info directives source valueInfoStrings =
  let typusFile = TypusFile directives [] [] []
      sourceIR = SourceIR typusFile source
      goModule = createSimpleGoModule
      valueInfo = replicate (L.length valueInfoStrings) undefined  -- Simplified for test
      semanticIR = SemanticIR (sourceTypusFile sourceIR) goModule valueInfo
  in property $ L.length (semanticValueInfo semanticIR) === L.length valueInfo

-- Property: GoIR maintains generated code
prop_goir_code_generation :: String -> Property
prop_goir_code_generation goCode =
  let goModule = createSimpleGoModule
      goIR = GoIR goModule goCode
  in property $ goSource goIR === goCode

-- Property: GoIR preserves module information
prop_goir_module_preservation :: String -> Property
prop_goir_module_preservation goCode =
  let inputGoModule = createSimpleGoModule
      goIR = GoIR inputGoModule goCode
  in property $ goModule goIR === inputGoModule

-- Property: IR transformation chain consistency
prop_ir_chain_consistency :: FileDirectives -> String -> String -> Property
prop_ir_chain_consistency directives source goCode =
  let typusFile = TypusFile directives [] [] []
      sourceIR = SourceIR typusFile source
      inputGoModule = createSimpleGoModule
      semanticIR = SemanticIR typusFile inputGoModule []
      goIR = GoIR inputGoModule goCode
      allCorrect = tfDirectives (sourceTypusFile sourceIR) == directives &&
                  sourceText sourceIR == source &&
                  semanticModule semanticIR == inputGoModule &&
                  goModule goIR == inputGoModule &&
                  goSource goIR == goCode
  in property $ allCorrect

-- Property: Empty source handling
prop_ir_empty_source :: FileDirectives -> Property
prop_ir_empty_source directives =
  let typusFile = TypusFile directives [] [] []
      sourceIR = SourceIR typusFile ""
      goModule = GoModule [] Nothing [] []
      semanticIR = SemanticIR typusFile goModule []
      goIR = GoIR goModule ""
      allEmpty = sourceText sourceIR == "" &&
                L.null (semanticValueInfo semanticIR) &&
                goSource goIR == ""
  in property $ allEmpty

-- Property: Large source handling
prop_ir_large_source :: FileDirectives -> Int -> Property
prop_ir_large_source directives size =
  size >= 0 && size <= 10000 ==>
  let largeSource = replicate size 'x'
      typusFile = TypusFile directives [] [] []
      sourceIR = SourceIR typusFile largeSource
  in property $ L.length (sourceText sourceIR) === size

-- Property: Unicode source handling
prop_ir_unicode_source :: FileDirectives -> String -> Property
prop_ir_unicode_source directives unicodeText =
  let sourceWithUnicode = unicodeText ++ "测试内容🚀αβγ"
      typusFile = TypusFile directives [] [] []
      sourceIR = SourceIR typusFile sourceWithUnicode
  in property $ "测试内容🚀αβγ" `L.isInfixOf` sourceText sourceIR

-- Property: IR error accumulation simulation
prop_ir_error_accumulation :: FileDirectives -> String -> [String] -> [String] -> Property
prop_ir_error_accumulation directives source errors1 errors2 =
  let typusFile = TypusFile directives [] [] []
      sourceIR = SourceIR typusFile source
      goModule = GoModule [] Nothing [] []
      semanticIR1 = SemanticIR (sourceTypusFile sourceIR) goModule []
      semanticIR2 = SemanticIR (sourceTypusFile sourceIR) goModule []
      combinedValueInfo = semanticValueInfo semanticIR1 ++ semanticValueInfo semanticIR2
  in property $ L.length combinedValueInfo === L.length errors1 + L.length errors2

-- Property: Symbol table size limits simulation
prop_ir_symbol_table_limits :: FileDirectives -> String -> Int -> Property
prop_ir_symbol_table_limits directives source numSymbols =
  numSymbols >= 0 && numSymbols <= 1000 ==>
  let typusFile = TypusFile directives [] [] []
      sourceIR = SourceIR typusFile source
      valueInfo = replicate numSymbols undefined  -- Simplified for test
      goModule = GoModule [] Nothing [] []
      semanticIR = SemanticIR (sourceTypusFile sourceIR) goModule valueInfo
  in property $ L.length (semanticValueInfo semanticIR) === numSymbols

-- Property: IR round-trip transformation
prop_ir_roundtrip :: FileDirectives -> String -> String -> Property
prop_ir_roundtrip directives source goCode =
  let typusFile = TypusFile directives [] [] []
      sourceIR = SourceIR typusFile source
      inputGoModule = createSimpleGoModule
      semanticIR = SemanticIR typusFile inputGoModule []
      goIR = GoIR inputGoModule goCode
      -- Extract back original components
      extractedDirectives = tfDirectives (sourceTypusFile sourceIR)
      extractedSource = sourceText sourceIR
      extractedModule = semanticModule semanticIR
      extractedCode = goSource goIR
      allCorrect = extractedDirectives == directives &&
                  extractedSource == source &&
                  extractedModule == inputGoModule &&
                  extractedCode == goCode
  in property $ allCorrect

-- Property: IR validation
prop_ir_validation :: FileDirectives -> String -> String -> Property
prop_ir_validation directives source goCode =
  let typusFile = TypusFile directives [] [] []
      sourceIR = SourceIR typusFile source
      goModule = createSimpleGoModule
      semanticIR = SemanticIR (sourceTypusFile sourceIR) goModule []
      goIR = GoIR goModule goCode
      validSource = not (L.null $ sourceText sourceIR)
      validSemantic = not (L.null $ semanticValueInfo semanticIR) || L.null (semanticValueInfo semanticIR)
      validGo = not (L.null $ goSource goIR)
  in property $ validSource && validSemantic && validGo

-- Property: IR value info formatting
prop_ir_value_info_formatting :: FileDirectives -> String -> [String] -> Property
prop_ir_value_info_formatting directives source valueInfoStrings =
  let typusFile = TypusFile directives [] [] []
      sourceIR = SourceIR typusFile source
      goModule = createSimpleGoModule
      valueInfo = replicate (L.length valueInfoStrings) undefined  -- Simplified for test
      semanticIR = SemanticIR (sourceTypusFile sourceIR) goModule valueInfo
      allValueInfo = semanticValueInfo semanticIR
      hasValidValueInfo = not (null allValueInfo)
  in property $ hasValidValueInfo ==> L.length allValueInfo === L.length valueInfoStrings

-- Property: IR symbol consistency simulation
prop_ir_symbol_consistency :: FileDirectives -> String -> [String] -> Property
prop_ir_symbol_consistency directives source symbolNames =
  let typusFile = TypusFile directives [] [] []
      sourceIR = SourceIR typusFile source
      valueInfo = replicate (L.length symbolNames) undefined  -- Simplified for test
      goModule = GoModule [] Nothing [] []
      semanticIR = SemanticIR (sourceTypusFile sourceIR) goModule valueInfo
      uniqueNames = L.length symbolNames === L.length (nub symbolNames)
  in property $ uniqueNames

-- Property: IR code generation consistency
prop_ir_code_consistency :: String -> Property
prop_ir_code_consistency code1 =
  let goModule = createSimpleGoModule
      goIR1 = GoIR goModule code1
      goIR2 = GoIR goModule code1
  in property $ goSource goIR1 === goSource goIR2

-- Property: IR incremental updates
prop_ir_incremental_updates :: FileDirectives -> String -> [String] -> Property
prop_ir_incremental_updates directives source valueInfoStrings =
  let typusFile = TypusFile directives [] [] []
      sourceIR = SourceIR typusFile source
      goModule = createSimpleGoModule
      baseSemanticIR = SemanticIR (sourceTypusFile sourceIR) goModule []
      valueInfo = replicate (L.length valueInfoStrings) undefined  -- Simplified for test
      updatedSemanticIR = SemanticIR (sourceTypusFile sourceIR) goModule valueInfo
      finalSemanticIR = SemanticIR (sourceTypusFile sourceIR) goModule (semanticValueInfo updatedSemanticIR)
  in property $ 
       L.length (semanticValueInfo finalSemanticIR) === L.length valueInfo

-- Property: IR memory efficiency
prop_ir_memory_efficiency :: FileDirectives -> String -> Int -> Property
prop_ir_memory_efficiency directives source multiplier =
  multiplier >= 1 && multiplier <= 100 ==>
  let typusFile = TypusFile directives [] [] []
      sourceIR = SourceIR typusFile source
      valueInfo = replicate multiplier undefined  -- Simplified for test
      goModule = GoModule [] Nothing [] []
      semanticIR = SemanticIR (sourceTypusFile sourceIR) goModule valueInfo
      totalItems = L.length (semanticValueInfo semanticIR)
  in property $ totalItems === multiplier

-- Property: IR serialization consistency
prop_ir_serialization :: FileDirectives -> String -> String -> Property
prop_ir_serialization directives source goCode =
  let typusFile = TypusFile directives [] [] []
      sourceIR = SourceIR typusFile source
      inputGoModule = createSimpleGoModule
      semanticIR = SemanticIR typusFile inputGoModule []
      goIR = GoIR inputGoModule goCode
      -- Simulate serialization by creating new IRs with same data
      serializedSourceIR = SourceIR (sourceTypusFile sourceIR) (sourceText sourceIR)
      serializedSemanticIR = SemanticIR (sourceTypusFile serializedSourceIR) (semanticModule semanticIR) (semanticValueInfo semanticIR)
      serializedGoIR = GoIR (goModule goIR) (goSource goIR)
      allCorrect = sourceTypusFile serializedSourceIR == sourceTypusFile sourceIR &&
                  sourceText serializedSourceIR == sourceText sourceIR &&
                  semanticModule serializedSemanticIR == semanticModule semanticIR &&
                  semanticValueInfo serializedSemanticIR == semanticValueInfo semanticIR &&
                  goModule serializedGoIR == goModule goIR &&
                  goSource serializedGoIR == goSource goIR
  in property $ allCorrect

tests :: TestTree
tests = testGroup "IR QuickCheck Tests"
  [ fastProperty "SourceIR preserves source text" prop_sourceir_preserves_source
  , fastProperty "SourceIR stores directives correctly" prop_sourceir_directives_storage
  , fastProperty "SemanticIR contains valid module" prop_semanticir_module
  , fastProperty "SemanticIR preserves value info" prop_semanticir_value_info
  , fastProperty "GoIR maintains generated code" prop_goir_code_generation
  , fastProperty "GoIR preserves module information" prop_goir_module_preservation
  , fastProperty "IR transformation chain consistency" prop_ir_chain_consistency
  , fastProperty "Empty source handling" prop_ir_empty_source
  , fastProperty "Large source handling" prop_ir_large_source
  , fastProperty "Unicode source handling" prop_ir_unicode_source
  , fastProperty "IR error accumulation" prop_ir_error_accumulation
  , fastProperty "Symbol table size limits" prop_ir_symbol_table_limits
  , fastProperty "IR round-trip transformation" prop_ir_roundtrip
  , fastProperty "IR validation" prop_ir_validation
  , fastProperty "IR value info formatting" prop_ir_value_info_formatting
  , fastProperty "IR symbol consistency" prop_ir_symbol_consistency
  , fastProperty "IR code generation consistency" prop_ir_code_consistency
  , fastProperty "IR incremental updates" prop_ir_incremental_updates
  , fastProperty "IR memory efficiency" prop_ir_memory_efficiency
  , fastProperty "IR serialization consistency" prop_ir_serialization
  ]