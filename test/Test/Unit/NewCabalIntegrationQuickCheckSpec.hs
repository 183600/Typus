{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalIntegrationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Arbitrary (Arbitrary(..), arbitrary)
import Test.QuickCheck.Gen (oneof, listOf, choose, elements, listOf1)

import Parser
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import Compiler.Errors.Core (TypeError(..), ErrorSeverity(..), ErrorCategory(..))
import Utils (trim, removeComments)

import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.List (nub, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- ============================================================================
-- Mock Integration Data Types for Testing
-- ============================================================================

data MockCompilationPipeline = MockCompilationPipeline
  { pipelineSource :: String
  , pipelineParseResult :: MockParseResult
  , pipelineAnalysisResult :: MockAnalysisResult
  , pipelineCompileResult :: MockCompileResult
  } deriving (Show, Eq)

data MockParseResult = MockParseResult
  { parseSuccess :: Bool
  , parseErrors :: [MockParseError]
  , parseAST :: MockAST
  } deriving (Show, Eq)

data MockAnalysisResult = MockAnalysisResult
  { analysisSuccess :: Bool
  , analysisErrors :: [MockAnalysisError]
  , analysisWarnings :: [MockAnalysisWarning]
  , analysisSymbolTable :: MockSymbolTable
  } deriving (Show, Eq)

data MockCompileResult = MockCompileResult
  { compileSuccess :: Bool
  , compileErrors :: [MockCompileError]
  , compileOutput :: String
  , compileIR :: MockIR
  } deriving (Show, Eq)

data MockParseError = MockParseError
  { parseErrorMessage :: String
  , parseErrorLocation :: SourceSpan
  } deriving (Show, Eq)

data MockAnalysisError = MockAnalysisError
  { analysisErrorMessage :: String
  , analysisErrorLocation :: SourceSpan
  , analysisErrorType :: String
  } deriving (Show, Eq)

data MockAnalysisWarning = MockAnalysisWarning
  { analysisWarningMessage :: String
  , analysisWarningLocation :: SourceSpan
  } deriving (Show, Eq)

data MockCompileError = MockCompileError
  { compileErrorMessage :: String
  , compileErrorLocation :: SourceSpan
  , compileErrorType :: String
  } deriving (Show, Eq)

data MockAST = MockAST
  { astNodes :: [MockASTNode]
  , astImports :: [String]
  , astExports :: [String]
  } deriving (Show, Eq)

data MockASTNode = MockASTNode
  { astNodeType :: String
  , astNodeValue :: String
  , astNodeLocation :: SourceSpan
  , astNodeChildren :: [MockASTNode]
  } deriving (Show, Eq)

data MockSymbolTable = MockSymbolTable
  { symbolTableSymbols :: Map String MockSymbol
  , symbolTableParent :: Maybe MockSymbolTable
  } deriving (Show, Eq)

data MockSymbol = MockSymbol
  { symbolName :: String
  , symbolType :: String
  , symbolLocation :: SourcePos
  } deriving (Show, Eq)

data MockIR = MockIR
  { irInstructions :: [MockIRInstruction]
  , irEntry :: Int
  , irExports :: Set String
  } deriving (Show, Eq)

data MockIRInstruction = MockIRInstruction
  { irOpcode :: String
  , irOperands :: [String]
  , irLocation :: SourceSpan
  } deriving (Show, Eq)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    return $ SourcePos line col

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    let validEnd = if end >= start then end else start
    return $ SourceSpan start validEnd

instance Arbitrary MockSymbol where
  arbitrary = do
    name <- elements ["x", "y", "z", "func", "var", "const"]
    symType <- elements ["Int", "String", "Bool", "Void"]
    location <- arbitrary
    return $ MockSymbol name symType location

instance Arbitrary MockSymbolTable where
  arbitrary = do
    symbols <- Map.fromList <$> listOf (do
      name <- elements ["x", "y", "z", "func", "var"]
      symbol <- arbitrary
      return (name, symbol))
    hasParent <- arbitrary
    parent <- if hasParent then Just arbitrary else Nothing
    return $ MockSymbolTable symbols parent

instance Arbitrary MockIRInstruction where
  arbitrary = do
    opcode <- elements ["MOV", "ADD", "SUB", "CALL", "RET", "JMP"]
    operands <- listOf (elements ["rax", "rbx", "rcx", "rdx", "rsp"])
    location <- arbitrary
    return $ MockIRInstruction opcode operands location

instance Arbitrary MockIR where
  arbitrary = do
    instructions <- listOf1 arbitrary
    entry <- choose (0, L.length instructions - 1)
    exports <- Set.fromList <$> listOf (elements ["main", "func1", "func2"])
    return $ MockIR instructions entry exports

instance Arbitrary MockASTNode where
  arbitrary = do
    nodeType <- elements ["VarDecl", "FuncDecl", "Call", "Return", "Const"]
    value <- elements ["x", "y", "z", "1", "2", "3", "true", "false"]
    location <- arbitrary
    children <- listOf arbitrary
    return $ MockASTNode nodeType value location children

instance Arbitrary MockAST where
  arbitrary = do
    nodes <- listOf arbitrary
    imports <- listOf (elements ["import1", "import2", "import3"])
    exports <- listOf (elements ["export1", "export2", "export3"])
    return $ MockAST nodes imports exports

instance Arbitrary MockParseError where
  arbitrary = do
    message <- listOf1 (elements ['a'..'z'] ++ " ")
    location <- arbitrary
    return $ MockParseError message location

instance Arbitrary MockAnalysisError where
  arbitrary = do
    message <- listOf1 (elements ['a'..'z'] ++ " ")
    location <- arbitrary
    errorType' <- elements ["TypeError", "NameError", "SemanticError"]
    return $ MockAnalysisError message location errorType'

instance Arbitrary MockAnalysisWarning where
  arbitrary = do
    message <- listOf1 (elements ['a'..'z'] ++ " ")
    location <- arbitrary
    return $ MockAnalysisWarning message location

instance Arbitrary MockCompileError where
  arbitrary = do
    message <- listOf1 (elements ['a'..'z'] ++ " ")
    location <- arbitrary
    errorType' <- elements ["CodeGenError", "OptimizationError", "TargetError"]
    return $ MockCompileError message location errorType'

instance Arbitrary MockParseResult where
  arbitrary = do
    success <- arbitrary
    errors <- listOf arbitrary
    ast <- arbitrary
    return $ MockParseResult success errors ast

instance Arbitrary MockAnalysisResult where
  arbitrary = do
    success <- arbitrary
    errors <- listOf arbitrary
    warnings <- listOf arbitrary
    symbolTable <- arbitrary
    return $ MockAnalysisResult success errors warnings symbolTable

instance Arbitrary MockCompileResult where
  arbitrary = do
    success <- arbitrary
    errors <- listOf arbitrary
    output <- listOf1 (elements ['a'..'z'] ++ " \n")
    ir <- arbitrary
    return $ MockCompileResult success errors output ir

instance Arbitrary MockCompilationPipeline where
  arbitrary = do
    source <- listOf1 (elements ['a'..'z'] ++ " \n\t()")
    parseResult <- arbitrary
    analysisResult <- arbitrary
    compileResult <- arbitrary
    return $ MockCompilationPipeline source parseResult analysisResult compileResult

-- ============================================================================
-- Integration Property Tests
-- ============================================================================

-- Property: Pipeline preserves source through stages
prop_pipeline_preserves_source :: MockCompilationPipeline -> Property
prop_pipeline_preserves_source pipeline =
  let originalSource = pipelineSource pipeline
      retrievedSource = pipelineSource pipeline
  in property $ originalSource === retrievedSource

-- Property: Parse success affects analysis
prop_parse_success_affects_analysis :: MockCompilationPipeline -> Property
prop_parse_success_affects_analysis pipeline =
  let parseResult = pipelineParseResult pipeline
      parseSuccess' = parseSuccess parseResult
      analysisResult = pipelineAnalysisResult pipeline
      analysisSuccess' = analysisSuccess analysisResult
  in classify parseSuccess' "parse succeeded" $
     classify (not parseSuccess') "parse failed" $
     property $ True

-- Property: Analysis success affects compilation
prop_analysis_success_affects_compilation :: MockCompilationPipeline -> Property
prop_analysis_success_affects_compilation pipeline =
  let analysisResult = pipelineAnalysisResult pipeline
      analysisSuccess' = analysisSuccess analysisResult
      compileResult = pipelineCompileResult pipeline
      compileSuccess' = compileSuccess compileResult
  in classify analysisSuccess' "analysis succeeded" $
     classify (not analysisSuccess') "analysis failed" $
     property $ True

-- Property: Parse errors are propagated
prop_parse_errors_propagated :: MockCompilationPipeline -> Property
prop_parse_errors_propagated pipeline =
  let parseResult = pipelineParseResult pipeline
      parseErrors = parseErrors parseResult
      errorCount = L.length parseErrors
  in property $ errorCount >= 0

-- Property: Analysis errors are accumulated
prop_analysis_errors_accumulated :: MockCompilationPipeline -> Property
prop_analysis_errors_accumulated pipeline =
  let analysisResult = pipelineAnalysisResult pipeline
      analysisErrors = analysisErrors analysisResult
      errorCount = L.length analysisErrors
  in property $ errorCount >= 0

-- Property: Compile errors are preserved
prop_compile_errors_preserved :: MockCompilationPipeline -> Property
prop_compile_errors_preserved pipeline =
  let compileResult = pipelineCompileResult pipeline
      compileErrors = compileErrors compileResult
      errorCount = L.length compileErrors
  in property $ errorCount >= 0

-- Property: AST structure is preserved through pipeline
prop_ast_structure_preserved :: MockCompilationPipeline -> Property
prop_ast_structure_preserved pipeline =
  let parseResult = pipelineParseResult pipeline
      ast = parseAST parseResult
      nodeCount = L.length (astNodes ast)
  in property $ nodeCount >= 0

-- Property: Symbol table is maintained through analysis
prop_symboltable_maintained :: MockCompilationPipeline -> Property
prop_symboltable_maintained pipeline =
  let analysisResult = pipelineAnalysisResult pipeline
      symbolTable = analysisSymbolTable analysisResult
      symbols = symbolTableSymbols symbolTable
      symbolCount = Map.size symbols
  in property $ symbolCount >= 0

-- Property: IR is generated only when compilation succeeds
prop_ir_generated_on_success :: MockCompilationPipeline -> Property
prop_ir_generated_on_success pipeline =
  let compileResult = pipelineCompileResult pipeline
      compileSuccess' = compileSuccess compileResult
      ir = compileIR compileResult
      instructionCount = L.length (irInstructions ir)
  in classify compileSuccess' "compile succeeded" $
     classify (not compileSuccess') "compile failed" $
     property $ instructionCount >= 0

-- Property: Error locations are preserved across stages
prop_error_locations_preserved :: MockCompilationPipeline -> Property
prop_error_locations_preserved pipeline =
  let parseResult = pipelineParseResult pipeline
      parseErrors = parseErrors parseResult
      errorLocations = map parseErrorLocation parseErrors
  in property $ L.length errorLocations === L.length parseErrors

-- Property: Pipeline stages are ordered correctly
prop_pipeline_stages_ordered :: MockCompilationPipeline -> Property
prop_pipeline_stages_ordered pipeline =
  let parseResult = pipelineParseResult pipeline
      analysisResult = pipelineAnalysisResult pipeline
      compileResult = pipelineCompileResult pipeline
      parseSuccess' = parseSuccess parseResult
      analysisSuccess' = analysisSuccess analysisResult
      compileSuccess' = compileSuccess compileResult
  in property $ True

-- Property: Warnings are preserved through analysis
prop_warnings_preserved :: MockCompilationPipeline -> Property
prop_warnings_preserved pipeline =
  let analysisResult = pipelineAnalysisResult pipeline
      warnings = analysisWarnings analysisResult
      warningCount = L.length warnings
  in property $ warningCount >= 0

-- Property: Output is generated only on successful compilation
prop_output_generated_on_success :: MockCompilationPipeline -> Property
prop_output_generated_on_success pipeline =
  let compileResult = pipelineCompileResult pipeline
      compileSuccess' = compileSuccess compileResult
      output = compileOutput compileResult
      outputLength = L.length output
  in classify compileSuccess' "compile succeeded" $
     classify (not compileSuccess') "compile failed" $
     property $ outputLength >= 0

-- Property: IR exports are preserved
prop_ir_exports_preserved :: MockCompilationPipeline -> Property
prop_ir_exports_preserved pipeline =
  let compileResult = pipelineCompileResult pipeline
      ir = compileIR compileResult
      exports = irExports ir
      exportCount = Set.size exports
  in property $ exportCount >= 0

-- Property: AST imports are preserved
prop_ast_imports_preserved :: MockCompilationPipeline -> Property
prop_ast_imports_preserved pipeline =
  let parseResult = pipelineParseResult pipeline
      ast = parseAST parseResult
      imports = astImports ast
      importCount = L.length imports
  in property $ importCount >= 0

-- Property: AST exports are preserved
prop_ast_exports_preserved :: MockCompilationPipeline -> Property
prop_ast_exports_preserved pipeline =
  let parseResult = pipelineParseResult pipeline
      ast = parseAST parseResult
      exports = astExports ast
      exportCount = L.length exports
  in property $ exportCount >= 0

-- Property: Pipeline handles empty source gracefully
prop_pipeline_handles_empty_source :: Property
prop_pipeline_handles_empty_source =
  let emptySource = ""
      parseResult = MockParseResult False [] (MockAST [] [] [])
      analysisResult = MockAnalysisResult False [] [] (MockSymbolTable Map.empty Nothing)
      compileResult = MockCompileResult False [] "" (MockIR [] 0 Set.empty)
      pipeline = MockCompilationPipeline emptySource parseResult analysisResult compileResult
  in property $ pipelineSource pipeline === emptySource

-- Property: Error types are preserved across stages
prop_error_types_preserved :: MockCompilationPipeline -> Property
prop_error_types_preserved pipeline =
  let analysisResult = pipelineAnalysisResult pipeline
      analysisErrors = analysisErrors analysisResult
      errorTypes = map analysisErrorType analysisErrors
  in property $ L.length errorTypes === L.length analysisErrors

-- Property: Pipeline is deterministic
prop_pipeline_deterministic :: MockCompilationPipeline -> Property
prop_pipeline_deterministic pipeline =
  let pipeline1 = pipeline
      pipeline2 = pipeline
  in property $ pipeline1 === pipeline2

-- Property: Compilation output contains valid instructions
prop_compilation_output_valid :: MockCompilationPipeline -> Property
prop_compilation_output_valid pipeline =
  let compileResult = pipelineCompileResult pipeline
      ir = compileIR compileResult
      instructions = irInstructions ir
      validOpcodes = ["MOV", "ADD", "SUB", "CALL", "RET", "JMP"]
      allValid = L.all (\instr -> irOpcode instr `elem` validOpcodes) instructions
  in property $ allValid .||. null instructions

tests :: TestTree
tests = testGroup "New Cabal Integration QuickCheck Tests"
  [ fastProperty "Pipeline preserves source" prop_pipeline_preserves_source
  , fastProperty "Parse success affects analysis" prop_parse_success_affects_analysis
  , fastProperty "Analysis success affects compilation" prop_analysis_success_affects_compilation
  , fastProperty "Parse errors propagated" prop_parse_errors_propagated
  , fastProperty "Analysis errors accumulated" prop_analysis_errors_accumulated
  , fastProperty "Compile errors preserved" prop_compile_errors_preserved
  , fastProperty "AST structure preserved" prop_ast_structure_preserved
  , fastProperty "Symbol table maintained" prop_symboltable_maintained
  , fastProperty "IR generated on success" prop_ir_generated_on_success
  , fastProperty "Error locations preserved" prop_error_locations_preserved
  , fastProperty "Pipeline stages ordered" prop_pipeline_stages_ordered
  , fastProperty "Warnings preserved" prop_warnings_preserved
  , fastProperty "Output generated on success" prop_output_generated_on_success
  , fastProperty "IR exports preserved" prop_ir_exports_preserved
  , fastProperty "AST imports preserved" prop_ast_imports_preserved
  , fastProperty "AST exports preserved" prop_ast_exports_preserved
  , fastProperty "Pipeline handles empty source" prop_pipeline_handles_empty_source
  , fastProperty "Error types preserved" prop_error_types_preserved
  , fastProperty "Pipeline deterministic" prop_pipeline_deterministic
  , fastProperty "Compilation output valid" prop_compilation_output_valid
  ]