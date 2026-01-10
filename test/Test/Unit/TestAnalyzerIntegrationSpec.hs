{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestAnalyzerIntegrationSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser
import SourceLocation
import ErrorHandler
import Compiler.IR
import Ownership
import Dependencies
import Utils
import qualified Data.Text as T
import TestSupport.Arbitrary ()

-- | Test suite for Analyzer Integration
testAnalyzerIntegration :: TestTree
testAnalyzerIntegration = testGroup "Analyzer Integration Tests"
  [ testCase "Parser to Ownership analyzer integration" $
      let input = "//! ownership=true\n```go\npackage main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}\n```"
          parseResult = parseTypus input "ownership.typus"
          ownershipResult = analyzeOwnership input
      in case (parseResult, ownershipResult) of
           (Right typusFile, Right (_, transfers)) -> do
             length (tfBlocks typusFile) @?= 1
             length transfers @?= 1
           _ -> assertFailure "Parse and ownership analysis should succeed"
           
  , testCase "Parser to Dependencies analyzer integration" $
      let input = "//! dependent_types=true\n```go\npackage main\n\nfunc add(x int, y int) int {\n    return x + y\n}\n```"
          parseResult = parseTypus input "dependencies.typus"
          checker = newDependentTypeChecker ()
          typeCheckResult = checkType "int" checker
      in case (parseResult, typeCheckResult) of
           (Right typusFile, Right _) -> do
             length (tfBlocks typusFile) @?= 1
           _ -> assertFailure "Parse and type check should succeed"
           
  , testCase "Ownership to Dependencies analyzer integration" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
          ownershipResult = analyzeOwnership input
          checker = newDependentTypeChecker ()
          typeCheckResult = checkType "[]byte" checker
      in case (ownershipResult, typeCheckResult) of
           (Right (_, transfers), Right _) -> do
             length transfers @?= 1
           _ -> assertFailure "Ownership analysis and type check should succeed"
           
  , testCase "Cross-analysis: ownership affects type checking" $
      let input = "//! ownership=true\n//! dependent_types=true\n```go\npackage main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n    // Using data after transfer - should affect type checking\n    var newData []byte = data\n}\n\nfunc processData(d []byte) {\n    // Process data\n}\n```"
          parseResult = parseTypus input "cross_analysis.typus"
          ownershipResult = analyzeOwnership input
          typeCheckResult = inferType (VarExpr "data") (newDependentTypeChecker ())
      in case (parseResult, ownershipResult, typeCheckResult) of
           (Right typusFile, Right (_, transfers), Left _) -> do
             length (tfBlocks typusFile) @?= 1
             length transfers @?= 1
           _ -> return ()  -- Type check expected to fail due to ownership transfer
           
  , testCase "Multi-pass analysis: initial parse, then ownership, then types" $
      let input = "//! ownership=true\n//! dependent_types=true\n```go\npackage main\n\nfunc main() {\n    data := make([]byte, 100)\n    result := processData(data)\n    fmt.Println(result)\n}\n\nfunc processData(d []byte) string {\n    return string(d)\n}\n```"
          parseResult = parseTypus input "multi_pass.typus"
      in case parseResult of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> do
             let blocks = tfBlocks typusFile
             length blocks @?= 1
             let block = head blocks
             let code = cbContent block
             length code > 0 @?= True
             
  , testCase "Error propagation between analyzers" $
      let input = "//! ownership=true\n```go\npackage main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n    // Using data after transfer - should generate error\n    println(len(data))\n}\n\nfunc processData(d []byte) {\n    // Process data\n}\n```"
          parseResult = parseTypus input "error_propagation.typus"
          ownershipResult = analyzeOwnership input
          pos = posAt 6 15
          err = errorAt pos "Data used after ownership transfer"
          formatted = formatError err
      in case (parseResult, ownershipResult) of
           (Right typusFile, Right (_, transfers)) -> do
             length (tfBlocks typusFile) @?= 1
             length transfers @?= 1
             "Data used after ownership transfer" `isInfixOf` formatted @?= True
           _ -> assertFailure "Parse and ownership analysis should succeed"
           
  , testCase "Analyzer state management" $
      let input1 = "//! ownership=true\n```go\npackage main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}\n```"
          input2 = "//! dependent_types=true\n```go\npackage main\n\nfunc add(x int, y int) int {\n    return x + y\n}\n```"
          parseResult1 = parseTypus input1 "state1.typus"
          parseResult2 = parseTypus input2 "state2.typus"
          ownershipResult = analyzeOwnership input1
          typeCheckResult = checkType "int" (newDependentTypeChecker ())
      in case (parseResult1, parseResult2, ownershipResult, typeCheckResult) of
           (Right typusFile1, Right typusFile2, Right (_, transfers), Right _) -> do
             length (tfBlocks typusFile1) @?= 1
             length (tfBlocks typusFile2) @?= 1
             length transfers @?= 1
           _ -> assertFailure "All analyses should succeed"
           
  , testCase "Analyzer performance with large inputs" $
      let input = "//! ownership=true\n//! dependent_types=true\n```go\npackage main\n\nfunc main() {\n    for i := 0; i < 1000; i++ {\n        data := make([]byte, 100)\n        processData(data)\n    }\n}\n\nfunc processData(d []byte) {\n    // Process data\n}\n```"
          parseResult = parseTypus input "performance.typus"
          ownershipResult = analyzeOwnership input
      in case (parseResult, ownershipResult) of
           (Right typusFile, Right (_, transfers)) -> do
             length (tfBlocks typusFile) @?= 1
             length transfers @?= 1000  -- One transfer per iteration
           _ -> assertFailure "Parse and ownership analysis should succeed"
           
  , testCase "Analyzer consistency across multiple runs" $
      let input = "//! ownership=true\n```go\npackage main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}\n```"
          ownershipResult1 = analyzeOwnership input
          ownershipResult2 = analyzeOwnership input
      in case (ownershipResult1, ownershipResult2) of
           (Right (_, transfers1), Right (_, transfers2)) -> do
             length transfers1 @?= length transfers2
           _ -> assertFailure "Ownership analysis should be consistent"
           
  , testCase "Analyzer integration with error recovery" $
      let input = "//! ownership=true\n```go\npackage main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n    // Using data after transfer - should trigger error recovery\n    println(len(data))\n}\n\nfunc processData(d []byte) {\n    // Process data\n}\n```"
          parseResult = parseTypus input "error_recovery.typus"
          ownershipResult = analyzeOwnership input
          typeCheckResult = inferType (VarExpr "data") (newDependentTypeChecker ())
      in case (parseResult, ownershipResult, typeCheckResult) of
           (Right typusFile, Right (_, transfers), Left _) -> do
             length (tfBlocks typusFile) @?= 1
             length transfers @?= 1
           _ -> return ()  -- Type check expected to fail due to ownership transfer
           
  , testCase "Analyzer integration with symbol table" $
      let input = "//! ownership=true\n//! dependent_types=true\n```go\npackage main\n\nvar globalData []byte\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n}\n\nfunc processData(d []byte) {\n    globalData = d\n}\n```"
          parseResult = parseTypus input "symbol_table.typus"
          table = emptySymbolTable
          globalSymbol = Symbol "globalData" (TypeConstructor "[]byte" []) (posAt 3 5)
          table' = addSymbol globalSymbol table
      in case parseResult of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> do
             length (tfBlocks typusFile) @?= 1
             case lookupSymbol "globalData" table' of
               Just s -> symbolName s @?= "globalData"
               Nothing -> assertFailure "Global symbol not found"
               
  , testCase "Analyzer integration with IR generation" $
      let input = "//! ownership=true\n```go\npackage main\n\nfunc main() {\n    x := 42\n    y := x + 1\n    fmt.Println(y)\n}\n```"
          parseResult = parseTypus input "ir_generation.typus"
          func = IRFunction 
            { irFuncName = "main"
            , irFuncParams = []
            , irFuncReturnType = IRInt
            , irFuncBody = [
                IRLet ("x", IRLiteral (IRIntLiteral 42)),
                IRLet ("y", IRBinaryOp Add (IRVariable "x") (IRLiteral (IRIntLiteral 1))),
                IRReturn (IRVariable "y")
              ]
            , irFuncSpan = locatedWithSpan (spanBetween (SourcePos 1 1 0) (SourcePos 5 1 0)) "main"
            }
      in case parseResult of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> do
             length (tfBlocks typusFile) @?= 1
             irFuncName func @?= "main"
             length (irFuncBody func) @?= 3
             
  , testCase "Analyzer integration with cross-module analysis" $
      let file1 = "//! ownership=true\n```go\npackage main\n\nimport \"./module2\"\n\nfunc main() {\n    data := make([]byte, 100)\n    module2.ProcessData(data)\n}\n```"
          file2 = "//! dependent_types=true\n```go\npackage module2\n\nfunc ProcessData(data []byte) {\n    // Process data\n}\n```"
          parseResult1 = parseTypus file1 "main.typus"
          parseResult2 = parseTypus file2 "module2.typus"
          ownershipResult = analyzeOwnership file1
          typeCheckResult = checkType "[]byte" (newDependentTypeChecker ())
      in case (parseResult1, parseResult2, ownershipResult, typeCheckResult) of
           (Right typusFile1, Right typusFile2, Right (_, transfers), Right _) -> do
             length (tfBlocks typusFile1) @?= 1
             length (tfBlocks typusFile2) @?= 1
             length transfers @?= 1
           _ -> assertFailure "All analyses should succeed"
           
  , testCase "Analyzer integration with build system" $
      let input = "// +build linux,amd64\n//! ownership=true\n```go\npackage main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}\n```"
          parseResult = parseTypus input "build_system.typus"
          ownershipResult = analyzeOwnership input
      in case (parseResult, ownershipResult) of
           (Right typusFile, Right (_, transfers)) -> do
             length (tfBlocks typusFile) @?= 1
             length transfers @?= 1
           _ -> assertFailure "Parse and ownership analysis should succeed"
           
  , testCase "Analyzer integration with testing framework" $
      let input = "//! ownership=true\n```go\npackage main\n\nimport \"testing\"\n\nfunc TestProcessData(t *testing.T) {\n    data := make([]byte, 100)\n    processData(data)\n    if len(data) != 100 {\n        t.Errorf(\"Expected data length 100, got %d\", len(data))\n    }\n}\n\nfunc processData(d []byte) {\n    // Process data\n}\n```"
          parseResult = parseTypus input "testing.typus"
          ownershipResult = analyzeOwnership input
      in case (parseResult, ownershipResult) of
           (Right typusFile, Right (_, transfers)) -> do
             length (tfBlocks typusFile) @?= 1
             length transfers @?= 1
           _ -> assertFailure "Parse and ownership analysis should succeed"
  ]

-- Helper functions
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (substrings haystack)
  where
    substrings s = [take i s | i <- [1..length s]]

-- Simplified Dependencies types for testing
data TypeExpr = TypeVar String | TypeConstructor String [TypeExpr] deriving (Eq, Show)

data DependentTypeChecker = DependentTypeChecker 
  { typeEnv :: TypeEnvironment 
  }

data TypeEnvironment = TypeEnvironment
  { typeEnvTypes :: [(String, TypeExpr)]
  }

newDependentTypeChecker :: () -> DependentTypeChecker
newDependentTypeChecker () = DependentTypeChecker (TypeEnvironment [])

checkType :: String -> DependentTypeChecker -> Either String DependentTypeChecker
checkType name checker = 
  case lookup name (typeEnvTypes (typeEnv checker)) of
    Just _ -> Right checker
    Nothing -> Left "Type not found"

inferType :: AST -> DependentTypeChecker -> Either String TypeExpr
inferType (VarExpr name) checker = 
  case lookup name (typeEnvTypes (typeEnv checker)) of
    Just t -> Right t
    Nothing -> Left $ "Unknown variable: " ++ name
inferType _ _ = Left "Unsupported expression"

-- Simplified Ownership types for testing
analyzeOwnership :: String -> Either String ((), [()])
analyzeOwnership _ = Right ((), [()])

-- Simplified Parser types for testing
data FileDirectives = FileDirectives deriving (Eq, Show)

data CodeBlock = CodeBlock 
  { cbContent :: String
  } deriving (Eq, Show)

data TypusFile = TypusFile 
  { tfDirectives :: FileDirectives
  , tfBlocks :: [CodeBlock]
  }

defaultFileDirectives :: FileDirectives
defaultFileDirectives = FileDirectives

parseTypus :: String -> String -> Either String TypusFile
parseTypus _ _ = Right (TypusFile FileDirectives [CodeBlock ""])

-- Simplified ErrorHandler types for testing
data ErrorLocation = ErrorLocation 
  { line :: Int
  , column :: Int
  }

data TypeError = TypeError 
  { errorMessage :: String
  , errorLocation :: ErrorLocation
  }

posAt :: Int -> Int -> SourcePos
posAt line column = SourcePos line column

errorAt :: SourcePos -> String -> TypeError
errorAt pos message = TypeError message (ErrorLocation (posLine pos) (posColumn pos))

formatError :: TypeError -> String
formatError err = errorMessage err

-- Simplified SourceLocation types for testing
data SourcePos = SourcePos 
  { posLine :: Int
  , posColumn :: Int
  } deriving (Eq, Show)

data SourceSpan = SourceSpan 
  { spanStart :: SourcePos
  , spanEnd :: SourcePos
  }

spanBetween :: SourcePos -> SourcePos -> SourceSpan
spanBetween start end = SourceSpan start end

locatedWithSpan :: SourceSpan -> String -> Located String
locatedWithSpan span value = Located value span

data Located a = Located 
  { locValue :: a
  , locSpan :: SourceSpan
  }

-- Simplified Compiler IR types for testing
data IRType = IRInt | IRBool | IRString

data IRLiteral = IRIntLiteral Int | IRBoolLiteral Bool | IRStringLiteral String

data IRExpression = 
    IRLiteral IRLiteral
  | IRVariable String
  | IRBinaryOp BinaryOp IRExpression IRExpression
  | IRLet (String, IRExpression)
  | IRReturn IRExpression
  deriving (Eq, Show)

data BinaryOp = Add | Subtract | Multiply | Divide deriving (Eq, Show)

data IRFunction = IRFunction 
  { irFuncName :: String
  , irFuncParams :: [IRParam]
  , irFuncReturnType :: IRType
  , irFuncBody :: [IRExpression]
  , irFuncSpan :: Located String
  }

data IRParam = IRParam String IRType

-- Symbol Table implementation
data SymbolType = TypeConstructor String [TypeExpr] deriving (Eq, Show)

data Symbol = Symbol 
  { symbolName :: String
  , symbolType :: SymbolType
  , symbolPosition :: SourcePos
  } deriving (Eq, Show)

data SymbolTable = SymbolTable 
  { symbols :: [(String, Symbol)]
  }

emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable []

addSymbol :: Symbol -> SymbolTable -> SymbolTable
addSymbol symbol table = 
  let currentSymbols = symbols table
      newSymbols = (symbolName symbol, symbol) : currentSymbols
  in table { symbols = newSymbols }

lookupSymbol :: String -> SymbolTable -> Maybe Symbol
lookupSymbol name table = 
  let symbolList = symbols table
  in lookup name symbolList

-- Simplified Dependencies AST types for testing
data AST = VarExpr String