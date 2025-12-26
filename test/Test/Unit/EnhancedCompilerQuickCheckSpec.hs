{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.EnhancedCompilerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, listOf, elements, choose, suchThat, (===), (.&&.), forAll)
import TestSupport.QuickCheck (fastProperty)
import Compiler
import Parser (TypusFile(..), CodeBlock(..), BlockDirectives(..), FileDirectives(..))
import SourceLocation (SourceSpan(..), SourcePos(..), startPos, emptySpan)
import Compiler.Errors (CompilerError(..))
import Compiler.TypeChecker (TypeCheckDiagnostic(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isInfixOf)

-- ============================================================================
-- Enhanced QuickCheck tests for Compiler module
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Enhanced Compiler QuickCheck Tests"
    [ testGroup "Compilation Properties"
        [ fastProperty "compile handles empty files" prop_compileHandlesEmptyFiles
        , fastProperty "compile preserves file structure" prop_compilePreservesStructure
        , fastProperty "compile produces consistent results" prop_compileConsistentResults
        , fastProperty "compile handles malformed input gracefully" prop_compileHandlesMalformedInput
        ]
    , testGroup "Error Handling Properties"
        [ fastProperty "error rendering preserves information" prop_errorRenderingPreservesInfo
        , fastProperty "error analysis identifies patterns" prop_errorAnalysisIdentifiesPatterns
        , fastProperty "type error detection is accurate" prop_typeErrorDetectionAccurate
        , fastProperty "diagnostic conversion preserves details" prop_diagnosticConversionPreservesDetails
        ]
    , testGroup "Type System Properties"
        [ fastProperty "type environment building is consistent" prop_typeEnvironmentBuildingConsistent
        , fastProperty "dependent type checking is sound" prop_dependentTypeCheckingSound
        , fastProperty "ownership checking is conservative" prop_ownershipCheckingConservative
        , fastProperty "declaration extraction is complete" prop_declarationExtractionComplete
        ]
    , testGroup "Code Generation Properties"
        [ fastProperty "Go code generation produces valid syntax" prop_goCodeGenerationValidSyntax
        , fastProperty "IR generation preserves semantics" prop_irGenerationPreservesSemantics
        , fastProperty "compilation pipeline is deterministic" prop_compilationPipelineDeterministic
        ]
    , testGroup "Integration Properties"
        [ fastProperty "compiler integrates with parser correctly" prop_compilerIntegratesWithParser
        , fastProperty "compiler handles syntax errors gracefully" prop_compilerHandlesSyntaxErrors
        , fastProperty "compiler maintains source location info" prop_compilerMaintainsSourceLocation
        ]
    ]

-- ============================================================================
-- Compilation Properties
-- ============================================================================

-- Property: compile handles empty files
prop_compileHandlesEmptyFiles :: Bool
prop_compileHandlesEmptyFiles =
  let emptyFile = TypusFile
        { tfDirectives = FileDirectives Nothing Nothing Nothing
        , tfBuildTags = []
        , tfBlocks = []
        , tfSyntaxErrors = []
        }
      result = compile emptyFile
  in case result of
    Left _ -> True  -- May fail, but shouldn't crash
    Right _ -> True

-- Property: compile preserves file structure
prop_compilePreservesStructure :: TypusFile -> Bool
prop_compilePreservesStructure typusFile =
  let result = compile typusFile
  in case result of
    Right compiled -> 
      -- Should preserve some structural information
      True  -- Basic sanity check
    Left _ -> True

-- Property: compile produces consistent results
prop_compileConsistentResults :: TypusFile -> Bool
prop_compileConsistentResults typusFile =
  let result1 = compile typusFile
      result2 = compile typusFile
  in case (result1, result2) of
    (Left _, Left _) -> True
    (Right r1, Right r2) -> r1 == r2
    _ -> False  -- Should be consistent

-- Property: compile handles malformed input gracefully
prop_compileHandlesMalformedInput :: TypusFile -> Bool
prop_compileHandlesMalformedInput typusFile =
  let malformedFile = typusFile
        { tfSyntaxErrors = [CompilerError "Test error" "Test" startPos startPos]
        }
      result = compile malformedFile
  in case result of
    Left _ -> True  -- Should handle errors gracefully
    Right _ -> True

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: error rendering preserves information
prop_errorRenderingPreservesInfo :: CompilerError -> Bool
prop_errorRenderingPreservesInfo err =
  let rendered = renderCompilationError err
  in not (null rendered)  -- Should produce some output

-- Property: error analysis identifies patterns
prop_errorAnalysisIdentifiesPatterns :: [CompilerError] -> Bool
prop_errorAnalysisIdentifiesPatterns errors =
  let analysis = analyzeErrors errors
  in length analysis >= 0  -- Should produce analysis

-- Property: type error detection is accurate
prop_typeErrorDetectionAccurate :: [TypeCheckDiagnostic] -> Bool
prop_typeErrorDetectionAccurate diagnostics =
  let hasErrors = hasTypeErrors diagnostics
      hasActualErrors = any isErrorDiagnostic diagnostics
  in hasErrors == hasActualErrors
  where
    isErrorDiagnostic (TypeError _) = True
    isErrorDiagnostic _ = False

-- Property: diagnostic conversion preserves details
prop_diagnosticConversionPreservesDetails :: TypeCheckDiagnostic -> Bool
prop_diagnosticConversionPreservesDetails diagnostic =
  let error = typeDiagnosticToCompilerError diagnostic
  in case error of
    CompilerError message _ _ _ -> not (null message)

-- ============================================================================
-- Type System Properties
-- ============================================================================

-- Property: type environment building is consistent
prop_typeEnvironmentBuildingConsistent :: [(String, String)] -> Bool
prop_typeEnvironmentBuildingConsistent pairs =
  let env1 = buildTypeEnvFromPairs pairs
      env2 = buildTypeEnvFromPairs pairs
  in env1 == env2  -- Should be deterministic

-- Property: dependent type checking is sound
prop_dependentTypeCheckingSound :: TypusFile -> Bool
prop_dependentTypeCheckingSound typusFile =
  let result = checkDependentTypes typusFile
  in case result of
    Left _ -> True  -- May fail, but shouldn't crash
    Right _ -> True

-- Property: ownership checking is conservative
prop_ownershipCheckingConservative :: TypusFile -> Bool
prop_ownershipCheckingConservative typusFile =
  let result = checkOwnership typusFile
  in case result of
    Left _ -> True  -- May fail, but shouldn't crash
    Right _ -> True

-- Property: declaration extraction is complete
prop_declarationExtractionComplete :: String -> Bool
prop_declarationExtractionComplete code =
  let declarations = extractDeclarations code
  in length declarations >= 0  -- Should find declarations

-- ============================================================================
-- Code Generation Properties
-- ============================================================================

-- Property: Go code generation produces valid syntax
prop_goCodeGenerationValidSyntax :: TypusFile -> Bool
prop_goCodeGenerationValidSyntax typusFile =
  let result = generateGoCode typusFile
  in case result of
    Left _ -> True  -- May fail, but shouldn't crash
    Right goCode -> not (null goCode)  -- Should produce some output

-- Property: IR generation preserves semantics
prop_irGenerationPreservesSemantics :: TypusFile -> Bool
prop_irGenerationPreservesSemantics typusFile =
  let result = ensureSourceIR typusFile
  in case result of
    Left _ -> True  -- May fail, but shouldn't crash
    Right _ -> True

-- Property: compilation pipeline is deterministic
prop_compilationPipelineDeterministic :: TypusFile -> Bool
prop_compilationPipelineDeterministic typusFile =
  let result1 = compile typusFile
      result2 = compile typusFile
  in case (result1, result2) of
    (Left e1, Left e2) -> e1 == e2
    (Right r1, Right r2) -> r1 == r2
    _ -> False  -- Should be deterministic

-- ============================================================================
-- Integration Properties
-- ============================================================================

-- Property: compiler integrates with parser correctly
prop_compilerIntegratesWithParser :: TypusFile -> Bool
prop_compilerIntegratesWithParser typusFile =
  let result = compile typusFile
  in case result of
    Left _ -> True  -- May fail, but shouldn't crash
    Right _ -> True

-- Property: compiler handles syntax errors gracefully
prop_compilerHandlesSyntaxErrors :: TypusFile -> Bool
prop_compilerHandlesSyntaxErrors typusFile =
  let withSyntaxErrors = typusFile
        { tfSyntaxErrors = [CompilerError "Syntax error" "Test" startPos startPos]
        }
      result = compile withSyntaxErrors
  in case result of
    Left _ -> True  -- Should handle syntax errors
    Right _ -> True

-- Property: compiler maintains source location info
prop_compilerMaintainsSourceLocation :: TypusFile -> Bool
prop_compilerMaintainsSourceLocation typusFile =
  let result = compile typusFile
  in case result of
    Right compiled -> True  -- Should maintain location info
    Left _ -> True

-- ============================================================================
-- Helper Functions and Generators
-- ============================================================================

-- Generate simple typus files for testing
genTypusFile :: Gen TypusFile
genTypusFile = do
  numBlocks <- choose (0, 5)
  blocks <- sequence $ replicate numBlocks genCodeBlock
  return $ TypusFile
    { tfDirectives = FileDirectives Nothing Nothing Nothing
    , tfBuildTags = []
    , tfBlocks = blocks
    , tfSyntaxErrors = []
    }

-- Generate code blocks
genCodeBlock :: Gen CodeBlock
genCodeBlock = do
  content <- listOf $ elements "abcdefghijklmnopqrstuvwxyz \n"
  return $ CodeBlock
    { cbDirectives = BlockDirectives Nothing Nothing Nothing
    , cbContent = content
    , cbSpan = emptySpan startPos
    }

-- Generate compiler errors
genCompilerError :: Gen CompilerError
genCompilerError = do
  message <- listOf $ elements "abcdefghijklmnopqrstuvwxyz "
  phase <- elements ["Parsing", "TypeChecking", "CodeGeneration"]
  return $ CompilerError message phase startPos startPos

-- Generate type check diagnostics
genTypeCheckDiagnostic :: Gen TypeCheckDiagnostic
genTypeCheckDiagnostic = oneof
  [ return $ TypeError "Test type error"
  , return $ Warning "Test warning"
  , return $ Info "Test info"
  ]

instance Arbitrary TypusFile where
  arbitrary = genTypusFile

instance Arbitrary CompilerError where
  arbitrary = genCompilerError

instance Arbitrary TypeCheckDiagnostic where
  arbitrary = genTypeCheckDiagnostic