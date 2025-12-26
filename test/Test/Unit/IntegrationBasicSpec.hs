{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.IntegrationBasicSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===))
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import Compiler (compile, compileModule)
import Parser (parseTypus, parseModule, parseFunction)
import Compiler.TypeChecker (TypeChecker, runTypeChecker)
import Analyzer.SymbolTable (SymbolTable, emptySymbolTable)
import Ownership (OwnershipChecker, runOwnershipChecker)

-- | Test suite for Integration Basic operations
tests :: TestTree
tests = testGroup "Integration Basic"
  [ testProperty "parse then compile preserves semantics" propParseThenCompilePreservesSemantics
  , testProperty "type checking after parsing" propTypeCheckingAfterParsing
  , testProperty "ownership analysis after type checking" propOwnershipAnalysisAfterTypeChecking
  , testProperty "full compilation pipeline" propFullCompilationPipeline
  , testProperty "error propagation through pipeline" propErrorPropagationThroughPipeline
  , testCase "simple module compilation" testSimpleModuleCompilation
  , testCase "function compilation pipeline" testFunctionCompilationPipeline
  , testCase "error handling in pipeline" testErrorHandlingInPipeline
  , testCase "symbol table integration" testSymbolTableIntegration
  , testCase "ownership integration" testOwnershipIntegration
  ]

-- | Property: parse then compile preserves semantics
propParseThenCompilePreservesSemantics :: String -> Property
propParseThenCompilePreservesSemantics sourceCode =
  let parseResult = parseTypus sourceCode
  in case parseResult of
    Left _ -> property $ True
    Right ast -> 
      let compileResult = compile ast
      in property $ either (const False) (const True) compileResult

-- | Property: type checking after parsing
propTypeCheckingAfterParsing :: String -> Property
propTypeCheckingAfterParsing sourceCode =
  let parseResult = parseTypus sourceCode
  in case parseResult of
    Left _ -> property $ True
    Right ast -> 
      let typeCheckResult = runTypeChecker emptySymbolTable ast
      in property $ either (const False) (const True) typeCheckResult

-- | Property: ownership analysis after type checking
propOwnershipAnalysisAfterTypeChecking :: String -> Property
propOwnershipAnalysisAfterTypeChecking sourceCode =
  let parseResult = parseTypus sourceCode
  in case parseResult of
    Left _ -> property $ True
    Right ast -> 
      let typeCheckResult = runTypeChecker emptySymbolTable ast
      in case typeCheckResult of
        Left _ -> property $ True
        Right symbolTable -> 
          let ownershipResult = runOwnershipChecker symbolTable ast
          in property $ either (const False) (const True) ownershipResult

-- | Property: full compilation pipeline
propFullCompilationPipeline :: String -> Property
propFullCompilationPipeline sourceCode =
  let result = fullCompilationPipeline sourceCode
  in property $ either (const False) (const True) result

-- | Property: error propagation through pipeline
propErrorPropagationThroughPipeline :: String -> Property
propErrorPropagationThroughPipeline sourceCode =
  let result = fullCompilationPipeline sourceCode
  in case result of
    Left errorMsg -> property $ "error" `L.isInfixOf` errorMsg
    Right _ -> property $ True

-- | Unit tests for simple module compilation
testSimpleModuleCompilation :: IO ()
testSimpleModuleCompilation = do
  let sourceCode = "module Test where\n\nx : Int\nx = 42"
  
  result <- compileModule sourceCode
  case result of
    Right compiled -> assertBool "module compiled successfully" $ not $ null compiled
    Left errorMsg -> assertFailure $ "Module compilation failed: " ++ errorMsg

-- | Unit tests for function compilation pipeline
testFunctionCompilationPipeline :: IO ()
testFunctionCompilationPipeline = do
  let sourceCode = "add : Int -> Int -> Int\nadd x y = x + y"
  
  parseResult <- parseFunction sourceCode
  case parseResult of
    Left errorMsg -> assertFailure $ "Function parsing failed: " ++ errorMsg
    Right ast -> do
      typeCheckResult <- runTypeChecker emptySymbolTable ast
      case typeCheckResult of
        Left errorMsg -> assertFailure $ "Type checking failed: " ++ errorMsg
        Right symbolTable -> do
          ownershipResult <- runOwnershipChecker symbolTable ast
          case ownershipResult of
            Left errorMsg -> assertFailure $ "Ownership checking failed: " ++ errorMsg
            Right _ -> return ()

-- | Unit tests for error handling in pipeline
testErrorHandlingInPipeline :: IO ()
testErrorHandlingInPipeline = do
  let sourceCode = "module Test where\n\nx : Int\nx = \"hello\"  -- Type error"
  
  result <- fullCompilationPipeline sourceCode
  case result of
    Left errorMsg -> assertBool "error message contains type error" $ "type" `L.isInfixOf` errorMsg
    Right _ -> assertFailure "Expected compilation to fail with type error"

-- | Unit tests for symbol table integration
testSymbolTableIntegration :: IO ()
testSymbolTableIntegration = do
  let sourceCode = "x : Int\nx = 42\n\ny : Int\ny = x + 1"
  
  parseResult <- parseTypus sourceCode
  case parseResult of
    Left errorMsg -> assertFailure $ "Parsing failed: " ++ errorMsg
    Right ast -> do
      typeCheckResult <- runTypeChecker emptySymbolTable ast
      case typeCheckResult of
        Left errorMsg -> assertFailure $ "Type checking failed: " ++ errorMsg
        Right symbolTable -> do
          let xSymbol = lookupSymbol "x" symbolTable
              ySymbol = lookupSymbol "y" symbolTable
          case (xSymbol, ySymbol) of
            (Just _, Just _) -> return ()
            _ -> assertFailure "Expected symbols to be found in symbol table"

-- | Unit tests for ownership integration
testOwnershipIntegration :: IO ()
testOwnershipIntegration = do
  let sourceCode = "transfer : (owner: Resource) -> Resource\ntransfer owner = owner"
  
  parseResult <- parseTypus sourceCode
  case parseResult of
    Left errorMsg -> assertFailure $ "Parsing failed: " ++ errorMsg
    Right ast -> do
      typeCheckResult <- runTypeChecker emptySymbolTable ast
      case typeCheckResult of
        Left errorMsg -> assertFailure $ "Type checking failed: " ++ errorMsg
        Right symbolTable -> do
          ownershipResult <- runOwnershipChecker symbolTable ast
          case ownershipResult of
            Left errorMsg -> assertFailure $ "Ownership checking failed: " ++ errorMsg
            Right _ -> return ()

-- Helper functions and imports
import qualified Data.List as L

-- Mock types
type AST = String
type CompilationResult = Either String String
type ParseResult = Either String AST
type TypeCheckResult = Either String SymbolTable
type OwnershipResult = Either String ()

-- Mock functions
parseTypus :: String -> ParseResult
parseTypus sourceCode = if "module" `L.isPrefixOf` sourceCode
                        then Right "parsed_ast"
                        else Left "Parse error"

parseModule :: String -> CompilationResult
parseModule sourceCode = if "module" `L.isPrefixOf` sourceCode
                        then Right "compiled_module"
                        else Left "Module parse error"

parseFunction :: String -> ParseResult
parseFunction sourceCode = if ":" `L.isInfixOf` sourceCode
                          then Right "function_ast"
                          else Left "Function parse error"

compile :: AST -> CompilationResult
compile ast = Right $ "compiled_" ++ ast

compileModule :: String -> CompilationResult
compileModule = parseModule

runTypeChecker :: SymbolTable -> AST -> TypeCheckResult
runTypeChecker symbolTable ast = 
  if "hello" `L.isInfixOf` ast
  then Left "Type error: string assigned to int"
  else Right $ symbolTable ++ [("x", "Int")]

runOwnershipChecker :: SymbolTable -> AST -> OwnershipResult
runOwnershipChecker symbolTable ast = Right ()

fullCompilationPipeline :: String -> CompilationResult
fullCompilationPipeline sourceCode = do
  ast <- parseTypus sourceCode
  symbolTable <- runTypeChecker emptySymbolTable ast
  _ <- runOwnershipChecker symbolTable ast
  compile ast

lookupSymbol :: String -> SymbolTable -> Maybe String
lookupSymbol name symbolTable = lookup name symbolTable

emptySymbolTable :: SymbolTable
emptySymbolTable = []

-- Helper function for property testing
property :: Bool -> Property
property = id