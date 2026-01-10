{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestBoundaryConditionsSpec where

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
import Data.Char (chr, ord)

-- | Test suite for boundary conditions
testBoundaryConditions :: TestTree
testBoundaryConditions = testGroup "Boundary Conditions Tests"
  [ testCase "Utils: trim handles empty string" $
      trim "" @?= ""
      
  , testCase "Utils: trim handles whitespace-only string" $
      trim "   \t\n   " @?= ""
      
  , testCase "Utils: splitBy handles empty string" $
      splitBy ',' "" @?= [""]
      
  , testCase "Utils: splitBy handles string with only delimiters" $
      splitBy ',' ",,," @?= ["", "", "", ""]
      
  , testCase "Utils: removeComments handles empty string" $
      removeComments "" @?= ""
      
  , testCase "Utils: removeComments handles string with only comments" $
      removeComments "// line comment\n/* block comment */" @?= "\n"
      
  , testCase "Utils: normalizeIndentation handles empty string" $
      normalizeIndentation "" @?= ""
      
  , testCase "Utils: normalizeIndentation handles string with only whitespace" $
      normalizeIndentation "   \n\t   \n   " @?= "\n\n"
      
  , testCase "Utils: breakOn handles empty pattern" $
      breakOn "" "hello" @?= ("", "hello")
      
  , testCase "Utils: breakOn handles pattern not found" $
      breakOn "xyz" "hello" @?= ("hello", "")
      
  , testCase "Utils: safeProcessString handles empty string" $
      case safeProcessString "" of
        Left _ -> return ()
        Right result -> result @?= ""
        
  , testCase "Utils: safeProcessString handles string with only control characters" $
      let controlString = [chr 0, chr 1, chr 2]
      in case safeProcessString controlString of
        Left _ -> return ()
        Right result -> assertFailure "Should have failed"
        
  , testCase "SourceLocation: posAt handles minimum values" $
      let pos = posAt 1 1
      in posLine pos @?= 1 && posColumn pos @?= 1
      
  , testCase "SourceLocation: posAt handles large values" $
      let pos = posAt 1000000 1000000
      in posLine pos @?= 1000000 && posColumn pos @?= 1000000
      
  , testCase "SourceLocation: spanBetween handles same position" $
      let pos = posAt 5 10
          span = spanBetween pos pos
      in spanStart span @?= pos && spanEnd span @?= pos
      
  , testCase "SourceLocation: mergeSpans handles identical spans" $
      let pos = posAt 5 10
          span = spanBetween pos pos
          merged = mergeSpans span span
      in merged @?= span
      
  , testCase "SourceLocation: advancePosBy handles empty string" $
      let pos = posAt 1 1
          advanced = advancePosBy "" pos
      in advanced @?= pos
      
  , testCase "SourceLocation: advancePosBy handles string with only newlines" $
      let pos = posAt 1 1
          advanced = advancePosBy "\n\n\n" pos
      in posLine advanced @?= 4 && posColumn advanced @?= 1
      
  , testCase "SourceLocation: advancePosBy handles string with only tabs" $
      let pos = posAt 1 1
          advanced = advancePosBy "\t\t\t" pos
      in posLine advanced @?= 1 && posColumn advanced @?= 25  -- 3 tabs, each aligning to 8-column boundary
      
  , testCase "Parser: parseTypus handles empty input" $
      let result = parseTypus "" "empty.typus"
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> tfBlocks typusFile @?= []
           
  , testCase "Parser: parseTypus handles extremely long input" $
      let longInput = replicate 100000 'a'
          result = parseTypus longInput "long.typus"
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> tfDirectives typusFile @?= defaultFileDirectives
           
  , testCase "Parser: parseTypus handles input with only newlines" $
      let input = "\n\n\n\n\n"
          result = parseTypus input "newlines.typus"
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> tfBlocks typusFile @?= []
           
  , testCase "Parser: parseTypus handles input with Unicode characters" $
      let input = "//! ownership=true\n```go\nfmt.Println(\"你好, 世界!\")\n```"
          result = parseTypus input "unicode.typus"
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> length (tfBlocks typusFile) @?= 1
           
  , testCase "ErrorHandler: errorAt handles minimum position values" $
      let pos = posAt 1 1
          err = errorAt pos "Test error"
          errLoc = errorLocation err
      in line errLoc @?= 1 && column errLoc @?= 1
      
  , testCase "ErrorHandler: errorAt handles maximum position values" $
      let pos = posAt maxBound maxBound
          err = errorAt pos "Test error"
          errLoc = errorLocation err
      in line errLoc @?= maxBound && column errLoc @?= maxBound
      
  , testCase "ErrorHandler: formatError handles empty error message" $
      let pos = posAt 1 1
          err = errorAt pos ""
          formatted = formatError err
      in length formatted > 0
      
  , testCase "ErrorHandler: formatError handles extremely long error message" $
      let longMessage = replicate 10000 'a'
          pos = posAt 1 1
          err = errorAt pos longMessage
          formatted = formatError err
      in length formatted > length longMessage
      
  , testCase "Compiler IR: IRFunction handles empty parameter list" $
      let func = IRFunction 
            { irFuncName = "test"
            , irFuncParams = []
            , irFuncReturnType = IRInt
            , irFuncBody = [IRReturn (IRLiteral (IRIntLiteral 42))]
            , irFuncSpan = locatedWithSpan (spanBetween (SourcePos 1 1 0) (SourcePos 3 1 0)) "test"
            }
      in length (irFuncParams func) @?= 0
      
  , testCase "Compiler IR: IRFunction handles empty body" $
      let func = IRFunction 
            { irFuncName = "test"
            , irFuncParams = [IRParam "x" IRInt]
            , irFuncReturnType = IRInt
            , irFuncBody = []
            , irFuncSpan = locatedWithSpan (spanBetween (SourcePos 1 1 0) (SourcePos 3 1 0)) "test"
            }
      in length (irFuncBody func) @?= 0
      
  , testCase "Compiler IR: IRModule handles empty function list" $
      let module = IRModule 
            { irModuleName = "test_module"
            , irModuleImports = []
            , irModuleFunctions = []
            , irModuleGlobals = []
            , irModuleSpan = locatedWithSpan (spanBetween (SourcePos 1 1 0) (SourcePos 3 1 0)) "test_module"
            }
      in length (irModuleFunctions module) @?= 0
      
  , testCase "Ownership: analyzeOwnership handles empty input" $
      let result = analyzeOwnership ""
      in case result of
           Left err -> assertFailure $ "Ownership analysis failed: " ++ show err
           Right (analyzer, transfers) -> length transfers @?= 0
           
  , testCase "Ownership: analyzeOwnership handles input with only whitespace" $
      let input = "   \n\t   \n   "
          result = analyzeOwnership input
      in case result of
           Left err -> assertFailure $ "Ownership analysis failed: " ++ show err
           Right (analyzer, transfers) -> length transfers @?= 0
           
  , testCase "Dependencies: newDependentTypeChecker creates empty environment" $
      let checker = newDependentTypeChecker ()
          env = initialTypeEnvironment
      in null (typeEnvTypes env) @?= True
      
  , testCase "Dependencies: checkType handles empty type name" $
      let checker = newDependentTypeChecker ()
          result = checkType "" checker
      in case result of
           Right _ -> assertFailure "Type check should have failed"
           Left _ -> return ()
           
  , testCase "Dependencies: solveConstraints handles empty constraint list" $
      let checker = newDependentTypeChecker ()
          result = solveConstraints checker
      in case result of
           Right solved -> length (typeSubstitution (typeEnv solved)) @?= 0
           Left err -> assertFailure $ "Constraint solving failed: " ++ show err
           
  , testCase "Dependencies: inferType handles empty variable environment" $
      let checker = newDependentTypeChecker ()
          expr = VarExpr "nonexistent"
          result = inferType expr checker
      in case result of
           Right _ -> assertFailure "Type inference should have failed"
           Left _ -> return ()
  ]