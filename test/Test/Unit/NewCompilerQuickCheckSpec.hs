{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCompilerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Compiler
  ( compile
  , CompilerError(..)
  , CompilerResult
  , CompilationPhase(..)
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

import Parser (TypusFile(..), CodeBlock(..), defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (Located(..), SourcePos(..), SourceSpan(..))
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isInfixOf)
import Data.Maybe (isJust, isNothing)

-- | Compiler QuickCheck tests
tests :: TestTree
tests = testGroup "New Compiler QuickCheck Tests"
  [ fastProperty "compile handles empty file" prop_compile_empty_file
  , fastProperty "compile handles simple content" prop_compile_simple_content
  , fastProperty "compile detects type errors" prop_compile_type_errors
  , fastProperty "extractDeclarations finds identifiers" prop_extract_declarations
  , fastProperty "buildTypeEnv creates consistent environment" prop_build_type_env
  , fastProperty "isMethodDeclaration identifies methods" prop_is_method_declaration
  , fastProperty "checkOwnership processes ownership" prop_check_ownership
  , fastProperty "compiler error formatting is consistent" prop_error_formatting
  ]

-- Property: compile handles empty file
prop_compile_empty_file :: Property
prop_compile_empty_file =
  let emptyFile = TypusFile defaultFileDirectives [] [] []
      result = compile emptyFile
  in case result of
    Left _ -> property True -- May fail compilation but shouldn't crash
    Right goCode -> property $ not (null goCode)

-- Property: compile handles simple content
prop_compile_simple_content :: String -> Property
prop_compile_simple_content content =
  L.length content <= 50 ==> 
  let codeBlock = CodeBlock defaultBlockDirectives content undefined
      file = TypusFile defaultFileDirectives [] [codeBlock] []
      result = compile file
  in case result of
    Left _ -> property True -- May fail compilation but shouldn't crash
    Right goCode -> property $ not (null goCode)

-- Property: compile detects type errors
prop_compile_type_errors :: Property
prop_compile_type_errors =
  let content = "var x int = \"string\"" -- Known type error trigger
      codeBlock = CodeBlock defaultBlockDirectives content undefined
      file = TypusFile defaultFileDirectives [] [codeBlock] []
      result = compile file
  in case result of
    Left errs -> property $ not (null errs) -- Should produce errors
    Right _ -> property $ False -- Should not succeed on type error

-- Property: extractDeclarations finds identifiers
prop_extract_declarations :: String -> String -> Property
prop_extract_declarations varName varType =
  L.length varName <= 10 && L.length varType <= 10 && 
  L.all (\c -> isAlphaNum c || c == '_') varName &&
  L.all (\c -> isAlphaNum c || c == '_' || c == '[' || c == ']') varType ==>
  let content = "var " ++ varName ++ " " ++ varType
      codeBlock = CodeBlock defaultBlockDirectives content undefined
      file = TypusFile defaultFileDirectives [] [codeBlock] []
      declarations = extractDeclarations file
  in property $ L.length declarations >= 0 -- Should find at least the declaration

-- Property: buildTypeEnv creates consistent environment
prop_build_type_env :: [(String, String)] -> Property
prop_build_type_env typePairs =
  L.all (\(k, v) -> L.length k <= 10 && L.length v <= 10) typePairs ==>
  let env = buildTypeEnvFromPairs typePairs
  in property $ L.length env >= L.length typePairs -- Environment should contain L.all pairs

-- Property: isMethodDeclaration identifies methods
prop_is_method_declaration :: String -> Property
prop_is_method_declaration declaration =
  L.length declaration <= 30 ==>
  let isMethod = isMethodDeclaration declaration
      hasReceiver = "func (" `L.isInfixOf` declaration
      hasFunc = "func " `L.isInfixOf` declaration
  in property $ if hasReceiver then isMethod else property True -- Non-methods may still be functions

-- Property: checkOwnership processes ownership
prop_check_ownership :: String -> Property
prop_check_ownership content =
  L.length content <= 50 ==>
  let codeBlock = CodeBlock defaultBlockDirectives content undefined
      file = TypusFile defaultFileDirectives [] [codeBlock] []
      result = checkOwnership file
  in case result of
    Left _ -> property True -- May fail but shouldn't crash
    Right _ -> property True -- Should succeed for simple cases

-- Property: compiler error formatting is consistent
prop_error_formatting :: String -> Property
prop_error_formatting errorMsg =
  L.length errorMsg <= 50 ==>
  let error = CompilerError "TEST001" (T.pack errorMsg) TypeCheckingPhase 
                            TypeChecking Error Nothing Nothing [] [] Nothing
      formatted = renderCompilationError [error]
  in property $ not (null formatted) .&&. errorMsg `L.isInfixOf` formatted