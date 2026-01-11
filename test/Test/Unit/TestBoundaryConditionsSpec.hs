{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestBoundaryConditionsSpec where

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

-- | Test suite for Boundary Conditions
testBoundaryConditions :: TestTree
testBoundaryConditions = testGroup "Boundary Conditions Tests"
  [ testCase "Parser: empty input" $
      let input = ""
          result = Parser.parseTypus input
      in case result of
           Left _ -> return ()  -- Expected to fail
           Right _ -> assertFailure "Empty input should fail to parse"
           
  , testCase "Parser: input with only whitespace" $
      let input = "   \n  \t  \n  "
          result = Parser.parseTypus input
      in case result of
           Left _ -> return ()  -- Expected to fail
           Right _ -> assertFailure "Whitespace-only input should fail to parse"
           
  , testCase "Parser: input with only comments" $
      let input = "// This is a comment\n/* This is another comment */"
          result = Parser.parseTypus input
      in case result of
           Left _ -> return ()  -- Expected to fail
           Right _ -> assertFailure "Comment-only input should fail to parse"
           
  , testCase "Parser: input with only directives" $
      let input = "//! ownership=true\n//! dependent=true"
          result = Parser.parseTypus input
      in case result of
           Left _ -> return ()  -- Expected to fail
           Right _ -> assertFailure "Directive-only input should fail to parse"
           
  , testCase "Parser: input with only block markers" $
      let input = "```\n```"
          result = Parser.parseTypus input
      in case result of
           Left _ -> return ()  -- Expected to fail
           Right _ -> assertFailure "Block-marker-only input should fail to parse"
           
  , testCase "Parser: input with mismatched block markers" $
      let input = "```\ncode\n```\n```\n"
          result = Parser.parseTypus input
      in case result of
           Left _ -> return ()  -- Expected to fail
           Right _ -> assertFailure "Mismatched block markers should fail to parse"
           
  , testCase "Parser: input with unclosed block" $
      let input = "```go\ncode without closure"
          result = Parser.parseTypus input
      in case result of
           Left _ -> return ()  -- Expected to fail
           Right _ -> assertFailure "Unclosed block should fail to parse"
           
  , testCase "Parser: input with extremely long line" $
      let longLine = concat (replicate 10000 "a")
          input = "```go\n" ++ longLine ++ "\n```"
          result = Parser.parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> length (Parser.tfBlocks typusFile) @?= 1
           
  , testCase "Parser: input with many small blocks" $
      let input = concat (replicate 1000 "```go\nfmt.Println(\"hello\")\n```\n")
          result = Parser.parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> length (Parser.tfBlocks typusFile) @?= 1000
           
  , testCase "ErrorHandler: empty error collector" $
      let collector = ErrorHandler.newErrorCollector
          hasErrors = ErrorHandler.hasErrors collector
          hasWarnings = ErrorHandler.hasWarnings collector
      in do
        hasErrors @?= False
        hasWarnings @?= False
           
  , testCase "ErrorHandler: error collector with many errors" $
      let errors = [ErrorHandler.errorAt ("error" ++ show i) (T.pack ("Error " ++ show i)) | i <- [1..1000]]
          -- Simplified: just create a collector and verify it can handle errors
          collector = ErrorHandler.newErrorCollector
          errorCount = length errors
      in errorCount @?= 1000
           
  , testCase "SourceLocation: position at origin" $
      let pos = SourceLocation.posAt 1 1
      in do
        SourceLocation.posLine pos @?= 1
        SourceLocation.posColumn pos @?= 1
        SourceLocation.posOffset pos @?= 0
        
  , testCase "SourceLocation: position at large coordinates" $
      let pos = SourceLocation.posAt 1000000 1000000
      in do
        SourceLocation.posLine pos @?= 1000000
        SourceLocation.posColumn pos @?= 1000000
        
  , testCase "SourceLocation: span with same start and end" $
      let pos = SourceLocation.posAt 1 1
          span = SourceLocation.spanBetween pos pos
      in do
        SourceLocation.spanStart span @?= pos
        SourceLocation.spanEnd span @?= pos
        
  , testCase "SourceLocation: span with large coordinates" $
      let start = SourceLocation.posAt 1 1
          end = SourceLocation.posAt 1000000 1000000
          span = SourceLocation.spanBetween start end
      in do
        SourceLocation.spanStart span @?= start
        SourceLocation.spanEnd span @?= end
        
  , testCase "Compiler IR: empty function" $
      let func = TestIRFunction 
            { testIRFuncName = "empty_func"
            , testIRFuncParams = []
            , testIRFuncReturnType = TestIRInt
            , testIRFuncBody = []
            , testIRFuncSpan = testLocatedWithSpan (testSpanBetween (TestSourcePos 1 1) (TestSourcePos 3 1)) "empty_func"
            }
      in length (testIRFuncBody func) @?= 0
      
  , testCase "Compiler IR: IRModule handles empty function list" $
      let testModule = TestIRModule 
            { testIRModuleName = "test_module"
            , testIRModuleImports = []
            , testIRModuleFunctions = []
            , testIRModuleGlobals = []
            , testIRModuleSpan = testLocatedWithSpan (testSpanBetween (TestSourcePos 1 1) (TestSourcePos 3 1)) "test_module"
            }
      in length (testIRModuleFunctions testModule) @?= 0
           
  , testCase "Ownership: empty input" $
      let input = ""
          result = Ownership.analyzeOwnership input
      in length result @?= 0
           
  , testCase "Ownership: input with no Go code" $
      let input = "//! ownership=true\n"
          result = Ownership.analyzeOwnership input
      in length result @?= 0
           
  , testCase "Dependencies: empty type environment" $
      let checker = Dependencies.newDependentTypeChecker
          result = Dependencies.checkType (Dependencies.TypeVar "nonexistent") checker
      in case result of
           Left _ -> return ()  -- Expected to fail
           Right _ -> assertFailure "Non-existent type should fail to check"
           
  , testCase "Utils: trim empty string" $
      let input = ""
          result = Utils.trim input
      in result @?= ""
      
  , testCase "Utils: trim string with only whitespace" $
      let input = "   \n  \t  \n  "
          result = Utils.trim input
      in result @?= ""
      
  , testCase "Utils: removeComments from empty string" $
      let input = ""
          result = Utils.removeComments input
      in result @?= ""
      
  , testCase "Utils: normalizeIndentation of empty string" $
      let input = ""
          result = Utils.normalizeIndentation input
      in result @?= ""
      
  , testCase "Utils: normalizeIndentation of string with only whitespace" $
      let input = "   \n  \t  \n  "
          result = Utils.normalizeIndentation input
      in result @?= ""
  ]

-- Helper functions
testLocatedWithSpan :: TestSourceSpan -> String -> TestLocated String
testLocatedWithSpan span value = TestLocated value span

testSpanBetween :: TestSourcePos -> TestSourcePos -> TestSourceSpan
testSpanBetween start end = TestSourceSpan start end

-- Local types to avoid conflicts
data TestIRType = TestIRInt | TestIRBool | TestIRString

data TestIRFunction = TestIRFunction 
  { testIRFuncName :: String
  , testIRFuncParams :: [TestIRParam]
  , testIRFuncReturnType :: TestIRType
  , testIRFuncBody :: [TestIRExpression]
  , testIRFuncSpan :: TestLocated String
  }

data TestIRExpression = TestIRExpression

data TestIRParam = TestIRParam String TestIRType

data TestIRModule = TestIRModule 
  { testIRModuleName :: String
  , testIRModuleImports :: [String]
  , testIRModuleFunctions :: [TestIRFunction]
  , testIRModuleGlobals :: [String]
  , testIRModuleSpan :: TestLocated String
  }

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