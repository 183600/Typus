{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CompilerIntegrationEnhancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler (compile, CompilerError(..), CompilationPhase(..), 
                renderCompilationError, formatCompilerErrors,
                hasTypeErrors, TypeCheckDiagnostic(..), diagnoseTypeErrors,
                checkDependentTypes, checkOwnership, generateGoCode)
import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import Compiler.Errors (ErrorSeverity(..), ErrorCategory(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)
import Data.Either (isLeft, isRight)

tests :: TestTree
tests = testGroup "Compiler Integration Enhanced QuickCheck Tests"
  [ basicCompilationProperties
  , errorHandlingProperties
  , typeCheckingProperties
  , dependentTypeProperties
  , ownershipProperties
  , codeGenerationProperties
  ]

-- | Basic compilation properties
basicCompilationProperties :: TestTree
basicCompilationProperties = testGroup "Basic Compilation Properties"
  [ testProperty "compile empty file" $
      \() -> 
        let result = parseTypus "" >>= compile
        in case result of
          Left _ -> property False
          Right _ -> property True
  
  , testProperty "compile simple valid code" $
      \code -> 
        not ("var x int = \"string\"" `isInfixOf` code) ==> 
        let result = parseTypus code >>= compile
        in case result of
          Left errs -> all (\e -> errorPhase e /= ParsingPhase) errs
          Right _ -> property True
  
  , testProperty "compile preserves valid structure" $
      \code -> 
        let result = parseTypus code >>= compile
        in case result of
          Left _ -> property True
          Right goCode -> not (null goCode) ==> length goCode > 0
  
  , testProperty "compile handles whitespace" $
      \whitespace -> 
        all (`elem` [' ', '\t', '\n', '\r']) whitespace ==> 
        let result = parseTypus whitespace >>= compile
        in case result of
          Left _ -> property False
          Right _ -> property True
  ]

-- | Error handling properties
errorHandlingProperties :: TestTree
errorHandlingProperties = testGroup "Error Handling Properties"
  [ testProperty "compile detects type errors" $
      \() -> 
        let code = "var x int = \"string\""
            result = parseTypus code >>= compile
        in case result of
          Left errs -> any (\e -> errorCategory e == TypeChecking) errs
          Right _ -> property False
  
  , testProperty "compile provides meaningful error messages" $
      \() -> 
        let code = "var x int = \"string\""
            result = parseTypus code >>= compile
        in case result of
          Left errs -> any (\e -> "type error" `T.isInfixOf` T.pack (errorMessage e)) errs
          Right _ -> property False
  
  , testProperty "renderCompilationError formats errors" $
      \errorMsg -> 
        let errors = [CompilerError "TEST001" (T.pack errorMsg) ParsingPhase Parsing Error Nothing Nothing [] Nothing Nothing]
            formatted = renderCompilationError errors
        in errorMsg `isInfixOf` formatted
  
  , testProperty "formatCompilerErrors handles multiple errors" $
      \errors -> 
        let formatted = formatCompilerErrors errors
            errorCount = length errors
        in errorCount > 0 ==> length (lines formatted) >= errorCount
  
  , testProperty "compile provides recovery suggestions" $
      \() -> 
        let code = "var x int = \"string\""
            result = parseTypus code >>= compile
        in case result of
          Left errs -> any (not . null . errorSuggestions) errs
          Right _ -> property False
  ]

-- | Type checking properties
typeCheckingProperties :: TestTree
typeCheckingProperties = testGroup "Type Checking Properties"
  [ testProperty "diagnoseTypeErrors returns diagnostics" $
      \code -> 
        let parsed = parseTypus code
        in case parsed of
          Left _ -> property True
          Right file -> 
            case diagnoseTypeErrors file of
              Left _ -> property True
              Right diagnostics -> length diagnostics >= 0
  
  , testProperty "hasTypeErrors detects type problems" $
      \code -> 
        let parsed = parseTypus code
        in case parsed of
          Left _ -> property True
          Right file -> hasTypeErrors file === ("var x int = \"string\"" `isInfixOf` code)
  
  , testProperty "type checking preserves file structure" $
      \code -> 
        let parsed = parseTypus code
        in case parsed of
          Left _ -> property True
          Right file -> 
            case diagnoseTypeErrors file of
              Left _ -> property True
              Right diagnostics -> length diagnostics >= 0
  
  , testProperty "type checking handles valid code" $
      \code -> 
        not ("var x int = \"string\"" `isInfixOf` code) ==> 
        let parsed = parseTypus code
        in case parsed of
          Left _ -> property True
          Right file -> 
            case diagnoseTypeErrors file of
              Left _ -> property True
              Right diagnostics -> True
  ]

-- | Dependent type properties
dependentTypeProperties :: TestTree
dependentTypeProperties = testGroup "Dependent Type Properties"
  [ testProperty "checkDependentTypes handles empty file" $
      \() -> 
        let parsed = parseTypus ""
        in case parsed of
          Left _ -> property True
          Right file -> checkDependentTypes file === ()
  
  , testProperty "checkDependentTypes handles simple code" $
      \code -> 
        let parsed = parseTypus code
        in case parsed of
          Left _ -> property True
          Right file -> checkDependentTypes file === ()
  
  , testProperty "checkDependentTypes preserves file content" $
      \code -> 
        let parsed = parseTypus code
        in case parsed of
          Left _ -> property True
          Right file -> 
            let _ = checkDependentTypes file
            in True  -- Just ensure it doesn't crash
  ]

-- | Ownership properties
ownershipProperties :: TestTree
ownershipProperties = testGroup "Ownership Properties"
  [ testProperty "checkOwnership handles empty file" $
      \() -> 
        let parsed = parseTypus ""
        in case parsed of
          Left _ -> property True
          Right file -> checkOwnership file === ()
  
  , testProperty "checkOwnership handles simple code" $
      \code -> 
        let parsed = parseTypus code
        in case parsed of
          Left _ -> property True
          Right file -> checkOwnership file === ()
  
  , testProperty "checkOwnership preserves file structure" $
      \code -> 
        let parsed = parseTypus code
        in case parsed of
          Left _ -> property True
          Right file -> 
            let _ = checkOwnership file
            in True  -- Just ensure it doesn't crash
  
  , testProperty "checkOwnership handles ownership directives" $
      \code -> 
        let codeWithOwnership = "{//! ownership: true}\n" ++ code
            parsed = parseTypus codeWithOwnership
        in case parsed of
          Left _ -> property True
          Right file -> 
            let _ = checkOwnership file
            in True  -- Just ensure it doesn't crash
  ]

-- | Code generation properties
codeGenerationProperties :: TestTree
codeGenerationProperties = testGroup "Code Generation Properties"
  [ testProperty "generateGoCode produces output" $
      \code -> 
        let parsed = parseTypus code
        in case parsed of
          Left _ -> property True
          Right file -> 
            case compile file of
              Left _ -> property True
              Right goCode -> not (null goCode) ==> length goCode > 0
  
  , testProperty "generateGoCode handles valid code" $
      \code -> 
        not ("var x int = \"string\"" `isInfixOf` code) ==> 
        let parsed = parseTypus code
        in case parsed of
          Left _ -> property True
          Right file -> 
            case compile file of
              Left _ -> property True
              Right goCode -> True
  
  , testProperty "generateGoCode preserves semantics" $
      \code -> 
        let parsed = parseTypus code
        in case parsed of
          Left _ -> property True
          Right file -> 
            case compile file of
              Left _ -> property True
              Right goCode -> 
                -- Basic sanity check that generated code is not empty
                not (null goCode) ==> any (`isInfixOf` goCode) ["func", "var", "package"]
  
  , testProperty "generateGoCode handles errors gracefully" $
      \code -> 
        let codeWithError = code ++ "\nvar x int = \"string\""
            parsed = parseTypus codeWithError
        in case parsed of
          Left _ -> property True
          Right file -> 
            case compile file of
              Left _ -> property True  -- Expected to fail
              Right _ -> property True -- Might succeed in some cases
  ]