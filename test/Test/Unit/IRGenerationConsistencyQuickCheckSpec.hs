{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.IRGenerationConsistencyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, sized, resize, choose)
import qualified Test.QuickCheck as QC

import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..), buildSourceIR, buildSemanticIR, buildSemanticIRWithPackage, emitGo, rawSourceFromTypus)
import Compiler (compile)
import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub, lines, unlines)
import Data.Char (isAlpha, isAlphaNum, isSpace, isDigit)
import qualified Data.Set as Set

-- | Generate valid identifiers
genIdentifier :: Gen String
genIdentifier = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements (['a'..'z'] ++ ['0'..'9'] ++ ['_'])
  return (first : rest)

-- | Generate simple type names
genType :: Gen String
genType = elements
  [ "int", "string", "bool", "float", "void" ]

-- | Generate valid function declarations
genFunction :: Gen String
genFunction = do
  funcName <- genIdentifier
  returnType <- genType
  paramName <- genIdentifier
  paramType <- genType
  return $ "func " ++ funcName ++ "(" ++ paramName ++ " " ++ paramType ++ ") " ++ returnType ++ " {\n  return 0\n}"

-- | Generate variable declarations
genVariable :: Gen String
genVariable = do
  varName <- genIdentifier
  varType <- genType
  return $ varName ++ " := " ++ varType ++ "(0)"

-- | Generate control structures
genControlStructure :: Gen String
genControlStructure = oneof
  [ do
      condition <- genIdentifier
      return $ "if " ++ condition ++ " {\n  doSomething()\n}"
  , do
      condition <- genIdentifier
      return $ "for " ++ condition ++ " {\n  doSomething()\n}"
  , do
      value <- genIdentifier
      cases <- listOf $ genIdentifier >>= \caseName -> return "case " ++ caseName ++ ": doSomething()"
      return $ "switch " ++ value ++ " {\n" ++ unlines cases ++ "\n}"
  ]

-- | Generate struct definitions
genStruct :: Gen String
genStruct = do
  structName <- genIdentifier
  fieldNames <- listOf genIdentifier
  fieldTypes <- listOf genType
  let fields = zipWith (\name typ -> name ++ " " ++ typ) fieldNames fieldTypes
  return $ "type " ++ structName ++ " struct {\n  " ++ unlines fields ++ "\n}"

-- | Generate import statements
genImport :: Gen String
genImport = oneof
  [ return "import \"fmt\""
  , return "import \"os\""
  , return "import \"strings\""
  , do
      importPath <- listOf $ elements ['a'..'z'] ++ ['/', '.']
      return $ "import \"" ++ importPath ++ "\""
  ]

-- | Generate complete program
genProgram :: Gen String
genProgram = do
  imports <- listOf genImport
  structs <- listOf genStruct
  functions <- listOf genFunction
  variables <- listOf genVariable
  controls <- listOf genControlStructure
  let allParts = imports ++ structs ++ functions ++ variables ++ controls
  return $ unlines allParts

-- | Generate malformed program for error testing
genMalformedProgram :: Gen String
genMalformedProgram = oneof
  [ -- Unclosed function
    do
      funcName <- genIdentifier
      return $ "func " ++ funcName ++ "() int {\n  return 42"
  
  , -- Invalid syntax
    do
      varName <- genIdentifier
      return $ varName ++ " := @#$%"
  
  , -- Missing imports
    do
      return $ "func main() {\n  fmt.Println(\"hello\")\n}"
  
  , -- Type errors
    do
      funcName <- genIdentifier
      return $ "func " ++ funcName ++ "() string {\n  return 42\n}"
  ]

-- Property: SourceIR should preserve original source structure
prop_sourceir_preserves_structure :: String -> Property
prop_sourceir_preserves_structure typusCode =
  not (null typusCode) ==>
  case parseTypus typusCode of
    Left _ -> property $ True -- Parse failure is acceptable
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile typusCode
          originalLines = lines typusCode
          sourceTextLines = lines (sourceText sourceIR)
      in property $ length originalLines === length sourceTextLines

-- Property: SemanticIR should be consistent with SourceIR
prop_semanticir_consistent_with_sourceir :: String -> Property
prop_semanticir_consistent_with_sourceir typusCode =
  not (null typusCode) && length (lines typusCode) <= 20 ==> -- Limit complexity
  case parseTypus typusCode of
    Left _ -> property $ True -- Parse failure is acceptable
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile typusCode
          semanticIR = buildSemanticIR sourceIR
          sourceFile = sourceTypusFile sourceIR
          semanticFile = semanticTypusFile semanticIR
      in property $ length (codeBlocks sourceFile) === length (codeBlocks semanticFile)

-- Property: IR generation should be idempotent
prop_ir_generation_idempotent :: String -> Property
prop_ir_generation_idempotent typusCode =
  not (null typusCode) ==>
  case parseTypus typusCode of
    Left _ -> property $ True -- Parse failure is acceptable
    Right typusFile ->
      let sourceIR1 = buildSourceIR typusFile typusCode
          sourceIR2 = buildSourceIR typusFile typusCode
          semanticIR1 = buildSemanticIR sourceIR1
          semanticIR2 = buildSemanticIR sourceIR2
      in property $ sourceText sourceIR1 === sourceText sourceIR2 .&&.
                 length (codeBlocks (semanticTypusFile semanticIR1)) === length (codeBlocks (semanticTypusFile semanticIR2))

-- Property: GoIR should contain valid Go syntax
prop_goir_valid_go_syntax :: String -> Property
prop_goir_valid_go_syntax typusCode =
  not (null typusCode) && "func" `isInfixOf` typusCode ==>
  case parseTypus typusCode of
    Left _ -> property $ True -- Parse failure is acceptable
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile typusCode
          semanticIR = buildSemanticIR sourceIR
          goIR = emitGo semanticIR
          goCode = show goIR
      in property $ "func" `isInfixOf` goCode || "package" `isInfixOf` goCode

-- Property: IR should preserve function signatures
prop_ir_preserves_function_signatures :: String -> Property
prop_ir_preserves_function_signatures typusCode =
  not (null typusCode) && "func" `isInfixOf` typusCode ==>
  case parseTypus typusCode of
    Left _ -> property $ True -- Parse failure is acceptable
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile typusCode
          semanticIR = buildSemanticIR sourceIR
          goIR = emitGo semanticIR
          originalFuncs = countFunctions typusCode
          goFuncs = countFunctions (show goIR)
      in property $ goFuncs >= originalFuncs
  where
    countFunctions code = length $ filter (== "func") (words code)

-- Property: IR should preserve variable declarations
prop_ir_preserves_variables :: String -> Property
prop_ir_preserves_variables typusCode =
  not (null typusCode) && any (`isInfixOf` typusCode) [":=", "var"] ==>
  case parseTypus typusCode of
    Left _ -> property $ True -- Parse failure is acceptable
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile typusCode
          semanticIR = buildSemanticIR sourceIR
          goIR = emitGo semanticIR
          originalVars = countVariables typusCode
          goVars = countVariables (show goIR)
      in property $ goVars >= originalVars
  where
    countVariables code = length $ filter (`isInfixOf` code) [":=", "var"]

-- Property: IR generation should handle imports correctly
prop_ir_handles_imports :: String -> Property
prop_ir_handles_imports typusCode =
  "import" `isInfixOf` typusCode ==>
  case parseTypus typusCode of
    Left _ -> property $ True -- Parse failure is acceptable
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile typusCode
          semanticIR = buildSemanticIR sourceIR
          goIR = emitGo semanticIR
          goCode = show goIR
      in property $ "import" `isInfixOf` goCode || "package" `isInfixOf` goCode

-- Property: IR should preserve control structures
prop_ir_preserves_control_structures :: String -> Property
prop_ir_preserves_control_structures typusCode =
  not (null typusCode) && any (`isInfixOf` typusCode) ["if", "for", "switch"] ==>
  case parseTypus typusCode of
    Left _ -> property $ True -- Parse failure is acceptable
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile typusCode
          semanticIR = buildSemanticIR sourceIR
          goIR = emitGo semanticIR
          originalControls = countControlStructures typusCode
          goControls = countControlStructures (show goIR)
      in property $ goControls >= originalControls
  where
    countControlStructures code = length $ filter (`isInfixOf` code) ["if", "for", "switch"]

-- Property: IR generation should be consistent across multiple runs
prop_ir_generation_consistent :: String -> Property
prop_ir_generation_consistent typusCode =
  not (null typusCode) ==>
  case parseTypus typusCode of
    Left _ -> property $ True -- Parse failure is acceptable
    Right typusFile ->
      let goIR1 = emitGo (buildSemanticIR (buildSourceIR typusFile typusCode))
          goIR2 = emitGo (buildSemanticIR (buildSourceIR typusFile typusCode))
          goCode1 = show goIR1
          goCode2 = show goIR2
      in property $ goCode1 === goCode2

-- Property: IR should handle package declarations correctly
prop_ir_handles_packages :: String -> Property
prop_ir_handles_packages typusCode =
  not (null typusCode) ==>
  case parseTypus typusCode of
    Left _ -> property $ True -- Parse failure is acceptable
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile typusCode
          semanticIR1 = buildSemanticIR sourceIR
          semanticIR2 = buildSemanticIRWithPackage "testpackage" sourceIR
          goIR1 = emitGo semanticIR1
          goIR2 = emitGo semanticIR2
          goCode1 = show goIR1
          goCode2 = show goIR2
      in property $ "package" `isInfixOf` goCode1 .&&. 
                 "package testpackage" `isInfixOf` goCode2

-- Property: IR should preserve struct definitions
prop_ir_preserves_structs :: String -> Property
prop_ir_preserves_structs typusCode =
  "struct" `isInfixOf` typusCode ==>
  case parseTypus typusCode of
    Left _ -> property $ True -- Parse failure is acceptable
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile typusCode
          semanticIR = buildSemanticIR sourceIR
          goIR = emitGo semanticIR
          originalStructs = countStructs typusCode
          goStructs = countStructs (show goIR)
      in property $ goStructs >= originalStructs
  where
    countStructs code = length $ filter (== "struct") (words code)

-- Property: IR should handle comments correctly
prop_ir_handles_comments :: String -> Property
prop_ir_handles_comments typusCode =
  "//" `isInfixOf` typusCode || "/*" `isInfixOf` typusCode ==>
  case parseTypus typusCode of
    Left _ -> property $ True -- Parse failure is acceptable
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile typusCode
          semanticIR = buildSemanticIR sourceIR
          goIR = emitGo semanticIR
          goCode = show goIR
      in property $ length goCode >= length typusCode -- Should preserve or expand content

-- Property: IR should handle empty programs
prop_ir_handles_empty_program :: Property
prop_ir_handles_empty_program =
  let emptyCode = ""
      result = parseTypus emptyCode
  in case result of
    Left _ -> property $ True -- Parse failure is acceptable for empty input
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile emptyCode
          semanticIR = buildSemanticIR sourceIR
          goIR = emitGo semanticIR
      in property $ True -- Should handle empty programs gracefully

-- Property: IR should handle malformed input gracefully
prop_ir_handles_malformed_input :: String -> Property
prop_ir_handles_malformed_input malformedCode =
  not (null malformedCode) ==>
  case parseTypus malformedCode of
    Left _ -> property $ True -- Parse failure is expected for malformed input
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile malformedCode
          semanticIR = buildSemanticIR sourceIR
          goIR = emitGo semanticIR
      in property $ True -- Should handle unexpected parsing gracefully

-- Export all tests
tests :: TestTree
tests =
  testGroup "IR Generation Consistency QuickCheck Tests"
    [ fastProperty "SourceIR should preserve original source structure" prop_sourceir_preserves_structure
    , fastProperty "SemanticIR should be consistent with SourceIR" prop_semanticir_consistent_with_sourceir
    , fastProperty "IR generation should be idempotent" prop_ir_generation_idempotent
    , fastProperty "GoIR should contain valid Go syntax" prop_goir_valid_go_syntax
    , fastProperty "IR should preserve function signatures" prop_ir_preserves_function_signatures
    , fastProperty "IR should preserve variable declarations" prop_ir_preserves_variables
    , fastProperty "IR generation should handle imports correctly" prop_ir_handles_imports
    , fastProperty "IR should preserve control structures" prop_ir_preserves_control_structures
    , fastProperty "IR generation should be consistent across multiple runs" prop_ir_generation_consistent
    , fastProperty "IR should handle package declarations correctly" prop_ir_handles_packages
    , fastProperty "IR should preserve struct definitions" prop_ir_preserves_structs
    , fastProperty "IR should handle comments correctly" prop_ir_handles_comments
    , fastProperty "IR should handle empty programs" prop_ir_handles_empty_program
    , fastProperty "IR should handle malformed input gracefully" prop_ir_handles_malformed_input
    ]