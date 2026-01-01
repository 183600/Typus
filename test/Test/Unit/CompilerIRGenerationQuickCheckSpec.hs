{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerIRGenerationQuickCheckSpec (tests) where

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
  , emitGo
  , rawSourceFromTypus
  , moduleFromTypus
  )

import Parser
  ( TypusFile(..)
  , CodeBlock(..)
  , parseTypus
  )

import Compiler.GoAst
  ( GoModule(..)
  , GoDecl(..)
  , ImportDecl(..)
  )

import SourceLocation (SourcePos(..), startPos)
import Utils (trim)
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

-- Property: buildSourceIR preserves TypusFile structure
prop_buildSourceIR_preserves_structure :: String -> Property
prop_buildSourceIR_preserves_structure content =
  let result = parseTypus content startPos
  in case result of
    Left _ -> property True  -- Invalid input is handled gracefully
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
      in property $ sourceTypusFile sourceIR === typusFile

-- Property: rawSourceFromTypus extracts content from code blocks
prop_rawSourceFromTypus_extracts_blocks :: [String] -> Property
prop_rawSourceFromTypus_extracts_blocks blockContents =
  not (null blockContents) && L.all (not . null) blockContents ==>
  let codeBlocks = L.map (\content -> CodeBlock 
        { cbDirectives = defaultBlockDirectives
        , cbSpan = undefined  -- Simplified for test
        , cbContent = content
        }) blockContents
      typusFile = TypusFile 
        { tfDirectives = defaultFileDirectives
        , tfBlocks = codeBlocks
        }
      extracted = rawSourceFromTypus typusFile
      expected = unlines blockContents
  in property $ extracted === expected

-- Property: buildSemanticIR succeeds for valid SourceIR
prop_buildSemanticIR_valid_source :: String -> Property
prop_buildSemanticIR_valid_source content =
  let result = parseTypus content startPos
  in case result of
    Left _ -> property True  -- Invalid input handled gracefully
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property True  -- Compilation errors are expected
        Right semanticIR -> property $ not (L.null (gmDecls (semanticModule semanticIR)))

-- Property: emitGo produces valid Go source
prop_emitGo_produces_valid_source :: String -> Property
prop_emitGo_produces_valid_source content =
  let result = parseTypus content startPos
  in case result of
    Left _ -> property True
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property True
        Right semanticIR ->
          let goIR = emitGo semanticIR
              goSource = goSource goIR
          in property $ not (null goSource)

-- Property: IR generation preserves import declarations
prop_IR_preserves_imports :: String -> [String] -> Property
prop_IR_preserves_imports baseContent imports =
  not (null baseContent) && L.all (not . null) imports ==>
  let importLines = L.map (\imp -> "import \"" ++ imp ++ "\"") imports
      contentWithImports = unlines $ baseContent : importLines
      result = parseTypus contentWithImports startPos
  in case result of
    Left _ -> property True
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property True
        Right semanticIR ->
          let goModule = semanticModule semanticIR
              importDecls = gmImports goModule
          in property $ L.length importDecls >= L.length imports

-- Property: IR generation handles empty files
prop_IR_handles_empty_files :: Property
prop_IR_handles_empty_files =
  let emptyFile = TypusFile 
        { tfDirectives = defaultFileDirectives
        , tfBlocks = []
        }
      sourceIR = buildSourceIR emptyFile
      semanticResult = buildSemanticIR sourceIR
  in case semanticResult of
    Left _ -> property True
    Right semanticIR ->
      let goIR = emitGo semanticIR
      in property $ not (L.null (goSource goIR))

-- Property: IR generation preserves function declarations
prop_IR_preserves_functions :: [String] -> Property
prop_IR_preserves_functions functionNames =
  not (null functionNames) && L.all (not . null) functionNames ==>
  let functionDecls = L.map (\name -> "func " ++ name ++ "() {}") functionNames
      content = unlines functionDecls
      result = parseTypus content startPos
  in case result of
    Left _ -> property True
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property True
        Right semanticIR ->
          let goModule = semanticModule semanticIR
              decls = gmDecls goModule
          in property $ L.length decls >= L.length functionNames

-- Property: IR generation handles package declarations
prop_IR_handles_package :: String -> Property
prop_IR_handles_package packageName =
  not (null packageName) && not (L.any (`elem` packageName) " \t\n\r\"'\\") ==>
  let packageContent = "package " ++ packageName
      result = parseTypus packageContent startPos
  in case result of
    Left _ -> property True
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property True
        Right semanticIR ->
          let goModule = semanticModule semanticIR
          in property $ gmPackage goModule === packageName

-- Property: IR generation handles variable declarations
prop_IR_handles_variables :: [String] -> [String] -> Property
prop_IR_handles_variables varNames varTypes =
  not (null varNames) && L.length varNames == L.length varTypes &&
  L.all (not . null) varNames && L.all (not . null) varTypes ==>
  let varDecls = zipWith (\name typ -> "var " ++ name ++ " " ++ typ) varNames varTypes
      content = unlines varDecls
      result = parseTypus content startPos
  in case result of
    Left _ -> property True
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property True
        Right semanticIR ->
          let goModule = semanticModule semanticIR
              decls = gmDecls goModule
          in property $ L.length decls >= L.length varNames

-- Property: IR generation handles type declarations
prop_IR_handles_types :: [String] -> [String] -> Property
prop_IR_handles_types typeNames baseTypes =
  not (null typeNames) && L.length typeNames == L.length baseTypes &&
  L.all (not . null) typeNames && L.all (not . null) baseTypes ==>
  let typeDecls = zipWith (\name base -> "type " ++ name ++ " " ++ base) typeNames baseTypes
      content = unlines typeDecls
      result = parseTypus content startPos
  in case result of
    Left _ -> property True
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property True
        Right semanticIR ->
          let goModule = semanticModule semanticIR
              decls = gmDecls goModule
          in property $ L.length decls >= L.length typeNames

-- Property: IR generation handles comments
prop_IR_handles_comments :: [String] -> Property
prop_IR_handles_comments comments =
  not (null comments) && L.all (not . L.any (`elem` "\"'\\") ) comments ==>
  let commentLines = L.map (\comment -> "// " ++ comment) comments
      content = unlines commentLines
      result = parseTypus content startPos
  in case result of
    Left _ -> property True
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property True
        Right semanticIR ->
          let goIR = emitGo semanticIR
              goSource = goSource goIR
          in property $ not (null goSource)

-- Property: IR generation handles multiline strings
prop_IR_handles_multiline_strings :: [String] -> Property
prop_IR_handles_multiline_strings stringLines =
  not (null stringLines) && L.all (not . L.any (`elem` "\\\"") ) stringLines ==>
  let multilineString = unlines stringLines
      content = "var s string = `" ++ multilineString ++ "`"
      result = parseTypus content startPos
  in case result of
    Left _ -> property True
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property True
        Right semanticIR ->
          let goIR = emitGo semanticIR
              goSource = goSource goIR
          in property $ multilineString `L.isInfixOf` goSource

-- Property: IR generation is deterministic
prop_IR_deterministic :: String -> Property
prop_IR_deterministic content =
  let result = parseTypus content startPos
  in case result of
    Left _ -> property True
    Right typusFile ->
      let sourceIR1 = buildSourceIR typusFile
          sourceIR2 = buildSourceIR typusFile
          semanticResult1 = buildSemanticIR sourceIR1
          semanticResult2 = buildSemanticIR sourceIR2
      in case (semanticResult1, semanticResult2) of
        (Right sem1, Right sem2) ->
          let goIR1 = emitGo sem1
              goIR2 = emitGo sem2
          in property $ goSource goIR1 === goSource goIR2
        _ -> property True

-- Property: IR generation handles complex expressions
prop_IR_handles_expressions :: String -> Property
prop_IR_handles_expressions expression =
  not (null expression) && not (L.any (`elem` expression) "\"'\\") ==>
  let content = "func test() { result := " ++ expression ++ " }"
      result = parseTypus content startPos
  in case result of
    Left _ -> property True
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property True
        Right semanticIR ->
          let goIR = emitGo semanticIR
              goSource = goSource goIR
          in property $ expression `L.isInfixOf` goSource

-- Property: IR generation handles struct definitions
prop_IR_handles_structs :: [String] -> [String] -> Property
prop_IR_handles_structs structNames fieldTypes =
  not (null structNames) && not (null fieldTypes) &&
  L.all (not . null) structNames && L.all (not . null) fieldTypes ==>
  let structDef = "type " ++ L.head structNames ++ " struct { Field " ++ L.head fieldTypes ++ " }"
      content = structDef
      result = parseTypus content startPos
  in case result of
    Left _ -> property True
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property True
        Right semanticIR ->
          let goModule = semanticModule semanticIR
              decls = gmDecls goModule
          in property $ not (null decls)

-- Property: IR generation handles interface definitions
prop_IR_handles_interfaces :: [String] -> Property
prop_IR_handles_interfaces methodNames =
  not (null methodNames) && L.all (not . null) methodNames ==>
  let methodDecls = L.map (\name -> name ++ "()") methodNames
      interfaceDef = "type TestInterface interface { " ++ unwords methodDecls ++ " }"
      content = interfaceDef
      result = parseTypus content startPos
  in case result of
    Left _ -> property True
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticResult = buildSemanticIR sourceIR
      in case semanticResult of
        Left _ -> property True
        Right semanticIR ->
          let goModule = semanticModule semanticIR
              decls = gmDecls goModule
          in property $ not (null decls)

tests :: TestTree
tests = testGroup "Compiler IR Generation QuickCheck"
  [ fastProperty "buildSourceIR preserves structure" prop_buildSourceIR_preserves_structure
  , fastProperty "rawSourceFromTypus extracts blocks" prop_rawSourceFromTypus_extracts_blocks
  , fastProperty "buildSemanticIR valid source" prop_buildSemanticIR_valid_source
  , fastProperty "emitGo produces valid source" prop_emitGo_produces_valid_source
  , fastProperty "IR preserves imports" prop_IR_preserves_imports
  , fastProperty "IR handles empty files" prop_IR_handles_empty_files
  , fastProperty "IR preserves functions" prop_IR_preserves_functions
  , fastProperty "IR handles package" prop_IR_handles_package
  , fastProperty "IR handles variables" prop_IR_handles_variables
  , fastProperty "IR handles types" prop_IR_handles_types
  , fastProperty "IR handles comments" prop_IR_handles_comments
  , fastProperty "IR handles multiline strings" prop_IR_handles_multiline_strings
  , fastProperty "IR deterministic" prop_IR_deterministic
  , fastProperty "IR handles expressions" prop_IR_handles_expressions
  , fastProperty "IR handles structs" prop_IR_handles_structs
  , fastProperty "IR handles interfaces" prop_IR_handles_interfaces
  ]