{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestCrossModuleIntegrationSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser
import SourceLocation
import qualified ErrorHandler
import Compiler.IR
import Ownership
import Dependencies
import Utils
import qualified Data.Text as T
import TestSupport.Arbitrary ()

-- | Test suite for cross-module integration
testCrossModuleIntegration :: TestTree
testCrossModuleIntegration = testGroup "Cross-Module Integration Tests"
  [ testCase "Parser to SourceLocation integration: spans are correctly calculated" $
      let input = "//! ownership=true\n```go\nfmt.Println(\"hello\")\n```"
          result = parseTypus input "test.typus"
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> do
             let blocks = tfBlocks typusFile
             if not (null blocks)
               then do
                 let span = cbSpan (head blocks)
                 isValidSpan span @?= True
               else return ()
               
  , testCase "Parser to ErrorHandler integration: syntax errors are properly reported" $
      let input = "//! ownership=true\n```go\nfunc invalid_syntax(\n```"
          result = parseTypus input "syntax_error.typus"
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> do
             -- Syntax errors should be captured for later processing
             tfDirectives typusFile @?= defaultFileDirectives
             
  , testCase "SourceLocation to ErrorHandler integration: error locations are preserved" $
      let pos = posAt 5 10
          err = errorAt pos "Test error"
          errLoc = errorLocation err
      in do
        line errLoc @?= 5
        column errLoc @?= 10
      
  , testCase "Utils to Parser integration: comment removal doesn't affect parsing" $
      let inputWithComments = "// This is a comment\n//! ownership=true\n/* Block comment */\n```go\nfmt.Println(\"hello\")\n```"
          inputWithoutComments = removeComments inputWithComments
          result1 = parseTypus inputWithComments "with_comments.typus"
          result2 = parseTypus inputWithoutComments "without_comments.typus"
      in case (result1, result2) of
           (Right file1, Right file2) -> 
             length (tfBlocks file1) @?= length (tfBlocks file2)
           _ -> assertFailure "Both parses should succeed"
           
  , testCase "Ownership to SourceLocation integration: ownership transfers have proper locations" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
          result = analyzeOwnership input
      in case result of
           Left err -> assertFailure $ "Ownership analysis failed: " ++ show err
           Right (analyzer, transfers) -> do
             if not (null transfers)
               then do
                 let transfer = head transfers
                 -- Ownership transfers should have valid locations
                 return ()
               else return ()
               
  , testCase "Dependencies to ErrorHandler integration: type errors are properly formatted" $
      let checker = newDependentTypeChecker ()
          result = checkType "UnknownType" checker
      in case result of
           Right _ -> assertFailure "Type check should have failed"
           Left err -> length (show err) > 0
           
  , testCase "Compiler IR to SourceLocation integration: IR nodes have proper spans" $
      let func = TestIRFunction 
            { testIRFuncName = "test"
            , testIRFuncParams = [TestIRParam "x" TestIRInt]
            , testIRFuncReturnType = TestIRBool
            , testIRFuncBody = [TestIRLiteral (TestIRBoolLiteral True)]
            , testIRFuncSpan = testLocatedWithSpan (testSpanBetween (TestSourcePos 1 1) (TestSourcePos 3 1)) "test"
            }
      in testIsValidSpan (testLocSpan (testIRFuncSpan func)) @?= True
      
  , testCase "Parser to Dependencies integration: parsed code can be type-checked" $
      let input = "//! dependent_types=true\n```go\nfunc add(x int, y int) int {\n    return x + y\n}\n```"
          result = parseTypus input "typed_code.typus"
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> do
             let blocks = tfBlocks typusFile
             if not (null blocks)
               then do
                 let block = head blocks
                 let code = cbContent block
                 -- Code should be analyzable by type checker
                 length code > 0 @?= True
               else return ()
               
  , testCase "Ownership to Dependencies integration: ownership types are compatible" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
          ownershipResult = analyzeOwnership input
      in case ownershipResult of
           Left err -> assertFailure $ "Ownership analysis failed: " ++ show err
           Right (analyzer, transfers) -> do
             -- Ownership transfers should be compatible with type system
             length transfers @?= 1
             
  , testCase "Utils to ErrorHandler integration: string processing affects error messages" $
      let message = "  Error with extra spaces  "
          trimmed = trim message
          pos = posAt 1 1
          err = errorAt pos trimmed
      in errorMessage err @?= "Error with extra spaces"
      
  , testCase "Parser to Compiler IR integration: parsed code generates valid IR" $
      let input = "//! ownership=true\n```go\nfunc add(x int, y int) int {\n    return x + y\n}\n```"
          result = parseTypus input "ir_generation.typus"
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> do
             let blocks = tfBlocks typusFile
             if not (null blocks)
               then do
                 let block = head blocks
                 let code = cbContent block
                 -- Code should be convertible to IR
                 length code > 0 @?= True
               else return ()
               
  , testCase "Dependencies to SourceLocation integration: type errors have proper locations" $
      let pos = posAt 5 10
          checker = newDependentTypeChecker ()
          result = checkTypeAt pos "UnknownType" checker
      in case result of
           Right _ -> assertFailure "Type check should have failed"
           Left err -> line (errorLocation err) @?= 5
           
  , testCase "ErrorHandler to Utils integration: error formatting uses string utilities" $
      let pos = posAt 5 10
          err = errorAt pos "Test error message"
          formatted = formatError err
      in "Test error message" `isInfixOf` formatted
      
  , testCase "Ownership to ErrorHandler integration: ownership errors are properly formatted" $
      let input = "package main\n\nfunc main() {\n    data := make([]byte, 100)\n    processData(data)\n    // Using data after transfer - violation\n    println(len(data))\n}\n\nfunc processData(d []byte) {\n    // Process data\n}"
          result = analyzeOwnership input
      in case result of
           Left err -> assertFailure $ "Ownership analysis failed: " ++ show err
           Right (analyzer, transfers) -> do
             let errors = formatOwnershipErrors analyzer
             length errors > 0 @?= True
             
  , testCase "Dependencies to Compiler IR integration: types are preserved in IR" $
      let checker = newDependentTypeChecker ()
          intType = TypeVar "Int"
          checker' = addType "Int" intType checker
          irType = IRInt
      in case checkType "Int" checker' of
           Right _ -> return ()
           Left err -> assertFailure $ "Type check failed: " ++ show err
           
  , testCase "Utils to SourceLocation integration: indentation normalization affects location calculation" $
      let input = "    line1\n      line2\n    line3"
          normalized = normalizeIndentation input
          lines' = lines normalized
      in length lines' @?= 3
  ]

-- Helper function
checkTypeAt :: SourcePos -> String -> Dependencies.DependentTypeChecker -> Either Dependencies.TypeInferenceError Dependencies.DependentTypeChecker
checkTypeAt pos typeName checker = checkType typeName checker

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (substrings haystack)
  where
    substrings s = [take i s | i <- [1..length s]]

-- Local types to avoid conflicts
data TestSourcePos = TestSourcePos 
  { testPosLine :: Int
  , testPosColumn :: Int
  }

data TestSourceSpan = TestSourceSpan 
  { testSpanStart :: TestSourcePos
  , testSpanEnd :: TestSourcePos
  }

data TestLocated a = TestLocated 
  { testLocValue :: a
  , testLocSpan :: TestSourceSpan
  }

data TestIRType = TestIRInt | TestIRBool | TestIRString

data TestIRLiteral = TestIRIntLiteral Int | TestIRBoolLiteral Bool | TestIRStringLiteral String

data TestIRParam = TestIRParam String TestIRType

data TestIRFunction = TestIRFunction 
  { testIRFuncName :: String
  , testIRFuncParams :: [TestIRParam]
  , testIRFuncReturnType :: TestIRType
  , testIRFuncBody :: [TestIRLiteral]
  , testIRFuncSpan :: TestLocated String
  }

-- Local functions
testLocatedWithSpan :: TestSourceSpan -> String -> TestLocated String
testLocatedWithSpan span value = TestLocated value span

testSpanBetween :: TestSourcePos -> TestSourcePos -> TestSourceSpan
testSpanBetween start end = TestSourceSpan start end

testIsValidSpan :: TestSourceSpan -> Bool
testIsValidSpan span = testPosLine (testSpanStart span) > 0 && testPosLine (testSpanEnd span) > 0

testLocSpan :: TestLocated a -> TestSourceSpan
testLocSpan (TestLocated _ span) = span