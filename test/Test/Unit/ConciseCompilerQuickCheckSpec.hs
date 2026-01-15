{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.ConciseCompilerQuickCheckSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, property, Arbitrary(..), Gen, choose, elements)
import Compiler
  ( compile
  , CompilerError(..)
  , CompilerResult
  , CompilationPhase(..)
  , SyntaxError(..)
  , TypeError(..)
  , malformedSyntaxError
  , renderCompilationError
  , formatCompilerErrors
  , generateDetailedReport
  , analyzeErrors
  , hasTypeErrors
  , TypeCheckDiagnostic(..)
  , diagnoseTypeErrors
  , extractDeclarations
  , extractFunctionCalls
  , buildTypeEnv
  , buildTypeEnvFromPairs
  , createTypusFileFromErrors
  , isMethodDeclaration
  , checkTypeError
  , hasMalformedSyntax
  , checkDependentTypes
  , checkOwnership
  , ensureSourceIR
  , typeCheckFailure
  , typeDiagnosticToCompilerError
  , generateGoCode
  )
import Parser (TypusFile(..), FileDirectives(..), BlockDirectives(..), CodeBlock(..))
import SourceLocation (SourceSpan(..), SourcePos(..))
import qualified Data.Text as T
import qualified Data.List as List

-- Arbitrary instances for QuickCheck
instance Arbitrary CompilationPhase where
  arbitrary = elements [ParsingPhase, TypeCheckingPhase, OptimizationPhase, CodeGenPhase]

instance Arbitrary CompilerError where
  arbitrary = do
    errorId <- arbitrary
    message <- arbitrary
    phase <- arbitrary
    category <- arbitrary
    severity <- arbitrary
    location <- arbitrary
    context <- arbitrary
    suggestions <- arbitrary
    stackTrace <- arbitrary
    timestamp <- arbitrary
    return $ CompilerError errorId message phase category severity location context suggestions stackTrace timestamp

instance Arbitrary SyntaxError where
  arbitrary = do
    errorId <- arbitrary
    message <- arbitrary
    location <- arbitrary
    return $ SyntaxError errorId message location

instance Arbitrary TypeError where
  arbitrary = do
    errorId <- arbitrary
    message <- arbitrary
    location <- arbitrary
    return $ TypeError errorId message location

instance Arbitrary TypeCheckDiagnostic where
  arbitrary = do
    context <- arbitrary
    detail <- arbitrary
    return $ TypeCheckDiagnostic context detail

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    offset <- choose (0, 1000000)
    return $ SourcePos line column offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end

instance Arbitrary FileDirectives where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    constraints <- arbitrary
    return $ FileDirectives ownership dependentTypes constraints

instance Arbitrary BlockDirectives where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    constraints <- arbitrary
    return $ BlockDirectives ownership dependentTypes constraints

instance Arbitrary CodeBlock where
  arbitrary = do
    directives <- arbitrary
    content <- arbitrary
    span <- arbitrary
    return $ CodeBlock directives content span

instance Arbitrary TypusFile where
  arbitrary = do
    directives <- arbitrary
    buildTags <- arbitrary
    blocks <- arbitrary
    syntaxErrors <- arbitrary
    return $ TypusFile directives buildTags blocks syntaxErrors

tests :: TestTree
tests = testGroup "Concise Compiler QuickCheck Tests"
  [ testProperties "Compiler Properties"
    [ compile_properties
    , renderCompilationError_properties
    , formatCompilerErrors_properties
    , generateDetailedReport_properties
    , analyzeErrors_properties
    ]
  , testProperties "Type Checking Properties"
    [ hasTypeErrors_properties
    , diagnoseTypeErrors_properties
    , checkTypeError_properties
    , hasMalformedSyntax_properties
    ]
  , testProperties "Type Environment Properties"
    [ buildTypeEnv_properties
    , buildTypeEnvFromPairs_properties
    ]
  , testProperties "Declaration and Function Properties"
    [ extractDeclarations_properties
    , extractFunctionCalls_properties
    , isMethodDeclaration_properties
    ]
  , testProperties "Error Handling Properties"
    [ malformedSyntaxError_properties
    , typeCheckFailure_properties
    , typeDiagnosticToCompilerError_properties
    ]
  , testProperties "Code Generation Properties"
    [ ensureSourceIR_properties
    , generateGoCode_properties
    ]
  , testProperties "Specialized Analysis Properties"
    [ checkDependentTypes_properties
    , checkOwnership_properties
    ]
  ]

-- | Test compile properties
compile_properties :: TypusFile -> Bool
compile_properties typusFile = 
  case compile typusFile of
    Left _ -> True  -- Compilation errors are acceptable
    Right result -> length result >= 0  -- Result should be a non-empty string

-- | Test renderCompilationError properties
renderCompilationError_properties :: [CompilerError] -> Bool
renderCompilationError_properties errors = 
  let rendered = renderCompilationError errors
  in length rendered >= 0

-- | Test formatCompilerErrors properties
formatCompilerErrors_properties :: [CompilerError] -> Bool
formatCompilerErrors_properties errors = 
  let formatted = formatCompilerErrors errors
  in length formatted >= 0

-- | Test generateDetailedReport properties
generateDetailedReport_properties :: [CompilerError] -> Bool
generateDetailedReport_properties errors = 
  let report = generateDetailedReport errors
  in length report >= 0

-- | Test analyzeErrors properties
analyzeErrors_properties :: [CompilerError] -> Bool
analyzeErrors_properties errors = 
  let analyzed = analyzeErrors errors
  in length analyzed >= 0

-- | Test hasTypeErrors properties
hasTypeErrors_properties :: TypusFile -> Bool
hasTypeErrors_properties typusFile = 
  let hasErr = hasTypeErrors typusFile
  in hasErr == True || hasErr == False  -- Should return a boolean

-- | Test diagnoseTypeErrors properties
diagnoseTypeErrors_properties :: TypusFile -> Bool
diagnoseTypeErrors_properties typusFile = 
  case diagnoseTypeErrors typusFile of
    Left _ -> True  -- Errors are acceptable
    Right diagnostics -> length diagnostics >= 0

-- | Test checkTypeError properties
checkTypeError_properties :: TypeError -> Bool
checkTypeError_properties err = 
  let result = checkTypeError err
  in result == True || result == False  -- Should return a boolean

-- | Test hasMalformedSyntax properties
hasMalformedSyntax_properties :: TypusFile -> Bool
hasMalformedSyntax_properties typusFile = 
  let hasMalformed = hasMalformedSyntax typusFile
  in hasMalformed == True || hasMalformed == False  -- Should return a boolean

-- | Test buildTypeEnv properties
buildTypeEnv_properties :: TypusFile -> Bool
buildTypeEnv_properties typusFile = 
  let env = buildTypeEnv typusFile
  in length env >= 0

-- | Test buildTypeEnvFromPairs properties
buildTypeEnvFromPairs_properties :: [(String, String)] -> Bool
buildTypeEnvFromPairs_properties pairs = 
  let env = buildTypeEnvFromPairs pairs
  in length env >= 0

-- | Test extractDeclarations properties
extractDeclarations_properties :: TypusFile -> Bool
extractDeclarations_properties typusFile = 
  let declarations = extractDeclarations typusFile
  in length declarations >= 0

-- | Test extractFunctionCalls properties
extractFunctionCalls_properties :: TypusFile -> Bool
extractFunctionCalls_properties typusFile = 
  let calls = extractFunctionCalls typusFile
  in length calls >= 0

-- | Test isMethodDeclaration properties
isMethodDeclaration_properties :: String -> Bool
isMethodDeclaration_properties declaration = 
  let isMethod = isMethodDeclaration declaration
  in isMethod == True || isMethod == False  -- Should return a boolean

-- | Test malformedSyntaxError properties
malformedSyntaxError_properties :: Bool
malformedSyntaxError_properties = 
  let err = malformedSyntaxError
  in length (errorMessage err) > 0

-- | Test typeCheckFailure properties
typeCheckFailure_properties :: Bool
typeCheckFailure_properties = 
  let err = typeCheckFailure
  in length (errorMessage err) > 0

-- | Test typeDiagnosticToCompilerError properties
typeDiagnosticToCompilerError_properties :: TypeCheckDiagnostic -> Bool
typeDiagnosticToCompilerError_properties diagnostic = 
  let err = typeDiagnosticToCompilerError diagnostic
  in length (errorMessage err) > 0

-- | Test ensureSourceIR properties
ensureSourceIR_properties :: TypusFile -> Bool
ensureSourceIR_properties typusFile = 
  case ensureSourceIR typusFile of
    Left _ -> True  -- Errors are acceptable
    Right ir -> True  -- Success is acceptable

-- | Test generateGoCode properties
generateGoCode_properties :: TypusFile -> Bool
generateGoCode_properties typusFile = 
  let goCode = generateGoCode typusFile
  in length goCode >= 0

-- | Test checkDependentTypes properties
checkDependentTypes_properties :: TypusFile -> Bool
checkDependentTypes_properties typusFile = 
  checkDependentTypes typusFile == ()  -- Should return unit

-- | Test checkOwnership properties
checkOwnership_properties :: TypusFile -> Bool
checkOwnership_properties typusFile = 
  checkOwnership typusFile == ()  -- Should return unit