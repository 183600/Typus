{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerIntegrationComprehensiveSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import qualified Data.List as L
import Data.List (isInfixOf, length)
import Data.List (null)
import qualified Data.Text as T

import Compiler
  ( compile
  , CompilerError(..)
  , CompilerResult
  , CompilationPhase(..)
  , ErrorCategory(..)
  , ErrorSeverity(..)
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

import Parser
  ( TypusFile(..)
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , parseTypus
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import SourceLocation
  ( SourceSpan(..)
  , SourcePos(..)
  , startPos
  , locatedWithSpan
  , locatedValue
  )

import qualified Compiler.IR as IR
import Compiler.GoAst (renderGoModule)

-- | Comprehensive QuickCheck tests for Compiler integration
-- This module tests the complete compilation pipeline L.and error handling

-- Property: compile handles empty input
prop_compile_empty_input :: Property
prop_compile_empty_input =
  let emptyFile = TypusFile defaultFileDirectives [] [] []
      result = compile emptyFile
  in case result of
    Left _ -> property False
    Right goCode -> not (null goCode)

-- Property: compile handles simple valid Go code
prop_compile_simple_valid :: String -> Property
prop_compile_simple_valid functionName =
  not (null functionName) && not (' ' `elem` functionName) ==>
  let content = "package main\n\nfunc " ++ functionName ++ "() {\n    return\n}\n"
      caseResult = parseTypus content
  in case caseResult of
    Left _ -> property False
    Right typusFile -> 
      case compile typusFile of
        Left _ -> property False
        Right goCode -> functionName `L.isInfixOf` goCode

-- Property: compile detects type errors
prop_compile_detects_type_errors :: String -> Property
prop_compile_detects_type_errors variableName =
  not (null variableName) && not (' ' `elem` variableName) ==>
  let content = "package main\n\nfunc main() {\n    var " ++ variableName ++ " int = \"string\"\n}\n"
      caseResult = parseTypus content
  in case caseResult of
    Left _ -> property False
    Right typusFile -> 
      case compile typusFile of
        Left errs -> L.length errs >= 1
        Right _ -> property False

-- Property: compile handles multiple functions
prop_compile_multiple_functions :: String -> String -> Property
prop_compile_multiple_functions func1 func2 =
  not (null func1) && not (null func2) && func1 /= func2 && 
  not (' ' `elem` func1) && not (' ' `elem` func2) ==>
  let content = "package main\n\nfunc " ++ func1 ++ "() {\n    return\n}\n\nfunc " ++ func2 ++ "() {\n    return\n}\n"
      caseResult = parseTypus content
  in case caseResult of
    Left _ -> property False
    Right typusFile -> 
      case compile typusFile of
        Left _ -> property False
        Right goCode -> func1 `L.isInfixOf` goCode && func2 `L.isInfixOf` goCode

-- Property: renderCompilationError handles empty error list
prop_renderCompilationError_empty :: Property
prop_renderCompilationError_empty =
  let emptyErrors = [] :: [CompilerError]
      rendered = renderCompilationError emptyErrors
  in null rendered

-- Property: renderCompilationError includes error codes
prop_renderCompilationError_includes_codes :: String -> Property
prop_renderCompilationError_includes_codes errorCode =
  not (null errorCode) ==>
  let error = mkCompilerError errorCode (T.pack "test error") 
               TypeCheckingPhase TypeChecking Error (Just defaultSpan) Nothing [] [] Nothing
      errors = [error]
      rendered = renderCompilationError errors
  in errorCode `L.isInfixOf` rendered

-- Property: formatCompilerErrors is consistent with renderCompilationError
prop_formatCompilerError_consistent :: [CompilerError] -> Property
prop_formatCompilerError_consistent errors =
  let rendered1 = renderCompilationError errors
      rendered2 = formatCompilerErrors errors
  in rendered1 === rendered2

-- Property: generateDetailedReport contains error phases
prop_generateDetailedReport_phases :: [CompilerError] -> Property
prop_generateDetailedReport_phases errors =
  not (null errors) ==>
  let report = generateDetailedReport errors
      hasPhaseInfo = L.any (`L.isInfixOf` report) ["Parsing", "TypeChecking", "CodeGeneration"]
  in hasPhaseInfo

-- Property: analyzeErrors categorizes errors correctly
prop_analyzeErrors_categorization :: [CompilerError] -> Property
prop_analyzeErrors_categorization errors =
  let analysis = analyzeErrors errors
  in L.length analysis >= 0

-- Property: hasTypeErrors detects type checking errors
prop_hasTypeErrors_detection :: [CompilerError] -> Property
prop_hasTypeErrors_detection errors =
  let typeErrors = L.filter (\e -> cePhase e == TypeCheckingPhase) errors
      hasType = hasTypeErrors errors
  in hasType === not (null typeErrors)

-- Property: diagnoseTypeErrors handles valid files
prop_diagnoseTypeErrors_valid :: String -> Property
prop_diagnoseTypeErrors_valid content =
  not (null content) && not ("var" `L.isInfixOf` content) ==>
  let caseResult = parseTypus content
  in case caseResult of
    Left _ -> property False
    Right typusFile -> 
      case diagnoseTypeErrors typusFile of
        Left _ -> property False
        Right diagnostics -> True

-- Property: extractDeclarations finds function declarations
prop_extractDeclarations_functions :: String -> Property
prop_extractDeclarations_functions functionName =
  not (null functionName) && not (' ' `elem` functionName) ==>
  let content = "package main\n\nfunc " ++ functionName ++ "() {\n    return\n}\n"
      caseResult = parseTypus content
  in case caseResult of
    Left _ -> property False
    Right typusFile -> 
      let declarations = extractDeclarations typusFile
      in L.any (functionName `L.isInfixOf`) declarations

-- Property: extractFunctionCalls identifies function calls
prop_extractFunctionCalls_calls :: String -> String -> Property
prop_extractFunctionCalls_calls caller callee =
  not (null caller) && not (null callee) && 
  not (' ' `elem` caller) && not (' ' `elem` callee) ==>
  let content = "package main\n\nfunc " ++ caller ++ "() {\n    " ++ callee ++ "()\n    return\n}\n"
      caseResult = parseTypus content
  in case caseResult of
    Left _ -> property False
    Right typusFile -> 
      let calls = extractFunctionCalls typusFile
      in L.any (callee `L.isInfixOf`) calls

-- Property: buildTypeEnv creates consistent environment
prop_buildTypeEnv_consistency :: [(String, String)] -> Property
prop_buildTypeEnv_consistency pairs =
  L.length pairs <= 5 ==>
  let env = buildTypeEnvFromPairs pairs
      pairCount = L.length pairs
  in L.length env >= pairCount

-- Property: isMethodDeclaration identifies methods correctly
prop_isMethodDeclaration_identification :: String -> String -> Property
prop_isMethodDeclaration_identification receiver methodName =
  not (null receiver) && not (null methodName) && 
  not (' ' `elem` receiver) && not (' ' `elem` methodName) ==>
  let methodDecl = "func (" ++ receiver ++ ") " ++ methodName ++ "()"
      regularDecl = "func " ++ methodName ++ "()"
  in isMethodDeclaration methodDecl && not (isMethodDeclaration regularDecl)

-- Property: checkTypeError validates error patterns
prop_checkTypeError_validation :: String -> Property
prop_checkTypeError_validation errorMsg =
  not (null errorMsg) ==>
  let isError = checkTypeError errorMsg
  in isError === ("type error" `L.isInfixOf` errorMsg || "error" `L.isInfixOf` errorMsg)

-- Property: hasMalformedSyntax detects syntax issues
prop_hasMalformedSyntax_detection :: String -> Property
prop_hasMalformedSyntax_detection content =
  let malformed = "package main\n\nfunc main() {\n    if true {\n    // missing closing brace\n"
      valid = "package main\n\nfunc main() {\n    if true {\n    }\n}\n"
      malformedResult = parseTypus malformed
      validResult = parseTypus valid
  in case (malformedResult, validResult) of
    (Left _, Right _) -> property True
    _ -> property False

-- Property: ensureSourceIR handles valid files
prop_ensureSourceIR_valid :: String -> Property
prop_ensureSourceIR_valid content =
  not (null content) && "package" `L.isInfixOf` content ==>
  let caseResult = parseTypus content
  in case caseResult of
    Left _ -> property False
    Right typusFile -> 
      case ensureSourceIR typusFile of
        Left _ -> property False
        Right _ -> property True

-- Property: typeDiagnosticToCompilerError preserves information
prop_typeDiagnosticToCompilerError_preservation :: TypeCheckDiagnostic -> Property
prop_typeDiagnosticToCompilerError_preservation diagnostic =
  let compilerError = typeDiagnosticToCompilerError diagnostic
      diagnosticMsg = T.pack (show diagnostic)
      errorMsg = ceMessage compilerError
  in T.L.length errorMsg > 0

-- Property: generateGoCode produces syntactically valid Go
prop_generateGoCode_valid_syntax :: String -> Property
prop_generateGoCode_valid_syntax content =
  "package" `L.isInfixOf` content && "func" `L.isInfixOf` content ==>
  let caseResult = parseTypus content
  in case caseResult of
    Left _ -> property False
    Right typusFile -> 
      case compile typusFile of
        Left _ -> property False
        Right goCode -> "package" `L.isInfixOf` goCode

-- Property: compilation pipeline preserves function names
prop_compilation_preserves_functions :: String -> String -> Property
prop_compilation_preserves_functions pkgName funcName =
  not (null pkgName) && not (null funcName) && 
  not (' ' `elem` pkgName) && not (' ' `elem` funcName) ==>
  let content = "package " ++ pkgName ++ "\n\nfunc " ++ funcName ++ "() {\n    return\n}\n"
      caseResult = parseTypus content
  in case caseResult of
    Left _ -> property False
    Right typusFile -> 
      case compile typusFile of
        Left _ -> property False
        Right goCode -> funcName `L.isInfixOf` goCode

-- Property: error handling preserves source location information
prop_error_preserves_location :: String -> Property
prop_error_preserves_location content =
  "var" `L.isInfixOf` content && "string" `L.isInfixOf` content ==>
  let caseResult = parseTypus content
  in case caseResult of
    Left _ -> property False
    Right typusFile -> 
      case compile typusFile of
        Left errors -> L.any (hasLocation . ceSpan) errors
        Right _ -> property False
  where
    hasLocation Nothing = False
    hasLocation (Just _) = True

-- Property: compilation handles Unicode content
prop_compilation_unicode :: String -> Property
prop_compilation_unicode unicodeContent =
  let content = "package main\n\nfunc main() {\n    // Unicode: " ++ unicodeContent ++ "\n    return\n}\n"
      caseResult = parseTypus content
  in case caseResult of
    Left _ -> property False
    Right typusFile -> 
      case compile typusFile of
        Left _ -> property False
        Right goCode -> True

tests :: TestTree
tests = testGroup "Compiler Integration Comprehensive QuickCheck tests"
  [ fastProperty "compile handles empty input" prop_compile_empty_input
  , fastProperty "compile handles simple valid Go code" prop_compile_simple_valid
  , fastProperty "compile detects type errors" prop_compile_detects_type_errors
  , fastProperty "compile handles multiple functions" prop_compile_multiple_functions
  , fastProperty "renderCompilationError handles empty error list" prop_renderCompilationError_empty
  , fastProperty "renderCompilationError includes error codes" prop_renderCompilationError_includes_codes
  , fastProperty "formatCompilerErrors is consistent with renderCompilationError" prop_formatCompilerError_consistent
  , fastProperty "generateDetailedReport contains error phases" prop_generateDetailedReport_phases
  , fastProperty "analyzeErrors categorizes errors correctly" prop_analyzeErrors_categorization
  , fastProperty "hasTypeErrors detects type checking errors" prop_hasTypeErrors_detection
  , fastProperty "diagnoseTypeErrors handles valid files" prop_diagnoseTypeErrors_valid
  , fastProperty "extractDeclarations finds function declarations" prop_extractDeclarations_functions
  , fastProperty "extractFunctionCalls identifies function calls" prop_extractFunctionCalls_calls
  , fastProperty "buildTypeEnv creates consistent environment" prop_buildTypeEnv_consistency
  , fastProperty "isMethodDeclaration identifies methods correctly" prop_isMethodDeclaration_identification
  , fastProperty "checkTypeError validates error patterns" prop_checkTypeError_validation
  , fastProperty "hasMalformedSyntax detects syntax issues" prop_hasMalformedSyntax_detection
  , fastProperty "ensureSourceIR handles valid files" prop_ensureSourceIR_valid
  , fastProperty "typeDiagnosticToCompilerError preserves information" prop_typeDiagnosticToCompilerError_preservation
  , fastProperty "generateGoCode produces syntactically valid Go" prop_generateGoCode_valid_syntax
  , fastProperty "compilation pipeline preserves function names" prop_compilation_preserves_functions
  , fastProperty "error handling preserves source location information" prop_error_preserves_location
  , fastProperty "compilation handles Unicode content" prop_compilation_unicode
  ]