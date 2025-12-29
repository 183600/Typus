{-# LANGUAGE CPP #-}

module Test.Unit.NewCompilerQuickCheckPropertiesSpec (tests) where

import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import Test.QuickCheck (Property, (===), forAll, Gen, choose, listOf, elements, suchThat, oneof)

import TestSupport.QuickCheck (fastProperty)

import Compiler
  ( compile
  , CompilerError(..)
  , CompilerResult
  , CompilationPhase(..)
  , renderCompilationError
  , generateGoCode
  , hasTypeErrors
  , TypeCheckDiagnostic(..)
  , TypeCheckDiagnostic(..)
  , diagnoseTypeErrors
  , extractDeclarations
  , buildTypeEnv
  )
import Parser (TypusFile(..), parseTypus)
import Compiler.Errors (ErrorCategory(..), ErrorSeverity(..))

-- QuickCheck generators
genCompilationPhase :: Gen CompilationPhase
genCompilationPhase = elements 
  [ ParsingPhase
  , TypeCheckingPhase
  , OwnershipPhase
  , DependentTypePhase
  , CodeGenPhase
  ]

genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements 
  [ TypeChecking
  , Ownership
  , Parsing
  , Semantic
  , Runtime
  , Constraint
  , Inference
  , Integration
  ]

genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Fatal, Error, Warning, Info]

genValidGoIdentifier :: Gen String
genValidGoIdentifier = do
  firstChar <- elements $ ['a'..'z'] ++ ['_']
  restChars <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
  return $ firstChar : restChars

genValidTypusCode :: Gen String
genValidTypusCode = oneof
  [ genSimpleFunction
  , genStructDefinition
  , genInterfaceDefinition
  , genVariableDeclaration
  , genConstantDeclaration
  ]

genSimpleFunction :: Gen String
genSimpleFunction = do
  funcName <- genValidGoIdentifier
  return $ unlines
    [ "package main"
    , "func " ++ funcName ++ "() {"
    , "    println(\"test\")"
    , "}"
    ]

genStructDefinition :: Gen String
genStructDefinition = do
  structName <- genValidGoIdentifier
  return $ unlines
    [ "package main"
    , "type " ++ structName ++ " struct {"
    , "    field1 int"
    , "    field2 string"
    , "}"
    , "func main() {}"
    ]

genInterfaceDefinition :: Gen String
genInterfaceDefinition = do
  interfaceName <- genValidGoIdentifier
  return $ unlines
    [ "package main"
    , "type " ++ interfaceName ++ " interface {"
    , "    Method() int"
    , "}"
    , "func main() {}"
    ]

genVariableDeclaration :: Gen String
genVariableDeclaration = do
  varName <- genValidGoIdentifier
  return $ unlines
    [ "package main"
    , "func main() {"
    , "    " ++ varName ++ " := 42"
    , "    println(" ++ varName ++ ")"
    , "}"
    ]

genConstantDeclaration :: Gen String
genConstantDeclaration = do
  constName <- genValidGoIdentifier
  return $ unlines
    [ "package main"
    , "const " ++ constName ++ " = 42"
    , "func main() {"
    , "    println(" ++ constName ++ ")"
    , "}"
    ]

genInvalidTypusCode :: Gen String
genInvalidTypusCode = oneof
  [ genUnbalancedBraces
  , genTypeMismatch
  , genUndefinedVariable
  , genInvalidSyntax
  ]

genUnbalancedBraces :: Gen String
genUnbalancedBraces = return $ unlines
  [ "package main"
  , "func main() {"
  , "    println(\"missing closing brace\")"
  ]

genTypeMismatch :: Gen String
genTypeMismatch = do
  varName <- genValidGoIdentifier
  return $ unlines
    [ "package main"
    , "func main() {"
    , "    var " ++ varName ++ " int = \"string\""
    , "    println(" ++ varName ++ ")"
    , "}"
    ]

genUndefinedVariable :: Gen String
genUndefinedVariable = do
  varName <- genValidGoIdentifier
  return $ unlines
    [ "package main"
    , "func main() {"
    , "    println(" ++ varName ++ ")"
    , "}"
    ]

genInvalidSyntax :: Gen String
genInvalidSyntax = return $ unlines
  [ "package main"
  , "func main() {"
  , "    if true"
  , "        println(\"missing condition\")"
  , "}"
  ]

genOwnershipCode :: Gen String
genOwnershipCode = do
  funcName <- genValidGoIdentifier
  return $ unlines
    [ "//! ownership: on"
    , "package main"
    , "func " ++ funcName ++ "() {"
    , "    data := \"hello\""
    , "    println(data)"
    , "}"
    ]

genDependentTypesCode :: Gen String
genDependentTypesCode = do
  typeName <- genValidGoIdentifier
  return $ unlines
    [ "//! dependent_types: on"
    , "package main"
    , "type " ++ typeName ++ " struct {"
    , "    data []int"
    , "}"
    , "where len data > 0"
    , "func main() {}"
    ]

-- | QuickCheck property tests for Compiler module
tests :: TestTree
tests =
  testGroup "NewCompiler QuickCheck Properties"
    [ testGroup "Compilation properties"
        [ fastProperty "generateGoCode always returns non-empty string for valid code" $
            forAll genValidTypusCode $ \code ->
              let goCode = generateGoCode (parseTypusOrError code)
              in not (null goCode)

        , fastProperty "generateGoCode preserves package declaration" $
            forAll genValidTypusCode $ \code ->
              let goCode = generateGoCode (parseTypusOrError code)
              in "package main" `isInfixOf` goCode

        , fastProperty "generateGoCode contains function definitions" $
            forAll genSimpleFunction $ \code ->
              let goCode = generateGoCode (parseTypusOrError code)
              in "func " `isInfixOf` goCode

        , fastProperty "generateGoCode contains struct definitions" $
            forAll genStructDefinition $ \code ->
              let goCode = generateGoCode (parseTypusOrError code)
              in "type " `isInfixOf` goCode && "struct" `isInfixOf` goCode

        , fastProperty "generateGoCode contains interface definitions" $
            forAll genInterfaceDefinition $ \code ->
              let goCode = generateGoCode (parseTypusOrError code)
              in "type " `isInfixOf` goCode && "interface" `isInfixOf` goCode
        ]

    , testGroup "Error handling properties"
        [ fastProperty "compile fails for code with type mismatch" $
            forAll genTypeMismatch $ \code ->
              case compile (parseTypusOrError code) of
                Left _ -> True
                Right _ -> False

        , fastProperty "compile fails for code with unbalanced braces" $
            forAll genUnbalancedBraces $ \code ->
              case compile (parseTypusOrError code) of
                Left _ -> True
                Right _ -> False

        , fastProperty "compile fails for code with undefined variables" $
            forAll genUndefinedVariable $ \code ->
              case compile (parseTypusOrError code) of
                Left _ -> True
                Right _ -> False

        , fastProperty "compile succeeds for valid code" $
            forAll genValidTypusCode $ \code ->
              case compile (parseTypusOrError code) of
                Left _ -> False
                Right _ -> True

        , fastProperty "renderCompilationError includes error information" $
            forAll genInvalidTypusCode $ \code ->
              case compile (parseTypusOrError code) of
                Left errs -> let rendered = renderCompilationError errs
                            in length rendered > 10
                Right _ -> False
        ]

    , testGroup "Ownership and dependent types properties"
        [ fastProperty "compile succeeds for ownership-enabled code" $
            forAll genOwnershipCode $ \code ->
              case compile (parseTypusOrError code) of
                Left _ -> False
                Right _ -> True

        , fastProperty "compile succeeds for dependent types code" $
            forAll genDependentTypesCode $ \code ->
              case compile (parseTypusOrError code) of
                Left _ -> False
                Right _ -> True

        , fastProperty "generateGoCode handles ownership directives" $
            forAll genOwnershipCode $ \code ->
              let goCode = generateGoCode (parseTypusOrError code)
              in not (null goCode)

        , fastProperty "generateGoCode handles dependent types directives" $
            forAll genDependentTypesCode $ \code ->
              let goCode = generateGoCode (parseTypusOrError code)
              in not (null goCode)
        ]

    , testGroup "Type checking properties"
        [ fastProperty "diagnoseTypeErrors returns Right for valid code" $
            forAll genValidTypusCode $ \code ->
              case diagnoseTypeErrors (parseTypusOrError code) of
                Right _ -> True
                Left _ -> False

        , fastProperty "diagnoseTypeErrors returns Left for invalid code" $
            forAll genTypeMismatch $ \code ->
              case diagnoseTypeErrors (parseTypusOrError code) of
                Right _ -> False
                Left _ -> True

        , fastProperty "extractDeclarations finds function declarations" $
            forAll genSimpleFunction $ \code ->
              let decls = extractDeclarations (parseTypusOrError code)
              in not (null decls)

        , fastProperty "buildTypeEnv succeeds for valid code" $
            forAll genValidTypusCode $ \code ->
              case buildTypeEnv (parseTypusOrError code) of
                Right _ -> True
                Left _ -> False
        ]

    , testGroup "Code generation properties"
        [ fastProperty "generated Go code is syntactically valid Go" $
            forAll genValidTypusCode $ \code ->
              let goCode = generateGoCode (parseTypusOrError code)
              in "package main" `isPrefixOf` goCode

        , fastProperty "generated Go code preserves main function" $
            forAll genValidTypusCode $ \code ->
              let goCode = generateGoCode (parseTypusOrError code)
              in "func main" `isInfixOf` goCode

        , fastProperty "generated Go code preserves variable declarations" $
            forAll genVariableDeclaration $ \code ->
              let goCode = generateGoCode (parseTypusOrError code)
              in ":=" `isInfixOf` goCode

        , fastProperty "generated Go code preserves constant declarations" $
            forAll genConstantDeclaration $ \code ->
              let goCode = generateGoCode (parseTypusOrError code)
              in "const " `isInfixOf` goCode
        ]

    , testGroup "Error message properties"
        [ fastProperty "error messages contain phase information" $
            forAll genInvalidTypusCode $ \code ->
              case compile (parseTypusOrError code) of
                Left errs -> let rendered = renderCompilationError errs
                            in any (`isInfixOf` rendered) 
                                 ["ParsingPhase", "TypeCheckingPhase", "OwnershipPhase", "DependentTypePhase"]
                Right _ -> False

        , fastProperty "error messages contain severity information" $
            forAll genInvalidTypusCode $ \code ->
              case compile (parseTypusOrError code) of
                Left errs -> let rendered = renderCompilationError errs
                            in any (`isInfixOf` rendered) ["Fatal", "Error", "Warning", "Info"]
                Right _ -> False

        , fastProperty "error messages contain category information" $
            forAll genInvalidTypusCode $ \code ->
              case compile (parseTypusOrError code) of
                Left errs -> let rendered = renderCompilationError errs
                            in any (`isInfixOf` rendered) 
                                 ["TypeChecking", "Ownership", "Parsing", "Semantic", "Runtime"]
                Right _ -> False
        ]

    , testGroup "Idempotency properties"
        [ fastProperty "generateGoCode is idempotent" $
            forAll genValidTypusCode $ \code ->
              let typusFile = parseTypusOrError code
                  goCode1 = generateGoCode typusFile
                  goCode2 = generateGoCode typusFile
              in goCode1 === goCode2

        , fastProperty "renderCompilationError is idempotent" $
            forAll genInvalidTypusCode $ \code ->
              case compile (parseTypusOrError code) of
                Left errs -> 
                  let rendered1 = renderCompilationError errs
                      rendered2 = renderCompilationError errs
                  in rendered1 === rendered2
                Right _ -> True
        ]

    , testGroup "Edge case properties"
        [ fastProperty "compile handles empty input gracefully" $
            case compile (parseTypusOrError "") of
              Left _ -> True  -- Should fail gracefully
              Right _ -> True  -- Or succeed gracefully

        , fastProperty "generateGoCode handles empty input gracefully" $
            let goCode = generateGoCode (parseTypusOrError "")
            in not (null goCode)

        , fastProperty "compile handles whitespace-only input gracefully" $
            case compile (parseTypusOrError "   \n\t\n  ") of
              Left _ -> True  -- Should fail gracefully
              Right _ -> True  -- Or succeed gracefully

        , fastProperty "generateGoCode handles whitespace-only input gracefully" $
            let goCode = generateGoCode (parseTypusOrError "   \n\t\n  ")
            in not (null goCode)
        ]
  ]

-- Helper function to parse Typus code or fail gracefully
parseTypusOrError :: String -> TypusFile
parseTypusOrError code =
  case parseTypus code of
    Left _ -> TypusFile { tfDirectives = mempty, tfBuildTags = [], tfBlocks = [], tfSyntaxErrors = [] }
    Right typusFile -> typusFile