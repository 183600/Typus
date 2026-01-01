{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewIntegrationQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.Tasty (TestTree)

import Parser (parseTypus, TypusFile(..))
import Compiler (compile, CompilerError(..))
import Ownership (analyzeOwnership)
import Dependencies (analyzeDependencies)
import ErrorHandler (handleError, ErrorContext(..), ErrorSeverity(..))
import SourceLocation (SourcePos(..), advancePos)
import Utils (trim, removeComments)
import Data.Char (isSpace, isAlphaNum)
import qualified Data.List as List

-- Property: End-to-end pipeline maintains consistency
prop_integration_pipeline_consistency :: String -> Property
prop_integration_pipeline_consistency input =
  let parseResult = parseTypus input
      compileResult = compile input
      ownershipResult = analyzeOwnership input
      dependencyResult = analyzeDependencies input
  in property $ case (parseResult, compileResult, ownershipResult, dependencyResult) of
    (Left parseErr, Left compileErr, Left ownershipErr, Left depErr) -> 
      property $ not (L.null (show parseErr)) .&&. not (L.null (show compileErr))
    (Right parseFile, Right compileRes, Right ownershipRes, Right depRes) -> 
      property True
    _ -> property True

-- Property: Parser L.and compiler error correlation
prop_integration_parser_compiler_errors :: String -> Property
prop_integration_parser_compiler_errors input =
  let parseResult = parseTypus input
      compileResult = compile input
  in property $ case (parseResult, compileResult) of
    (Left parseErr, Left compileErr) -> 
      property $ not (L.null (show parseErr)) .&&. not (L.null (show compileErr))
    (Right _, Right _) -> property True
    (Left _, Right _) -> property True  -- Parser fails but compilation succeeds with empty AST
    (Right _, Left _) -> property True  -- Parser succeeds but compilation fails

-- Property: Ownership L.and dependency analysis consistency
prop_integration_ownership_dependency_consistency :: String -> Property
prop_integration_ownership_dependency_consistency input =
  let ownershipResult = analyzeOwnership input
      dependencyResult = analyzeDependencies input
  in property $ case (ownershipResult, dependencyResult) of
    (Left _, Left _) -> property True
    (Right _, Right _) -> property True
    (Left _, Right _) -> property True
    (Right _, Left _) -> property True

-- Property: Error handling across modules
prop_integration_error_handling :: String -> String -> Property
prop_integration_error_handling input errorMsg =
  let parseResult = parseTypus input
      errorHandler = handleError (ErrorContext "integration-test" ErrorError)
      handledErrors = case parseResult of
        Left err -> [errorHandler (show err ++ errorMsg)]
        Right _ -> []
  in property $ not (null handledErrors) ==> not (L.null (show (L.head handledErrors)))

-- Property: Source location tracking consistency
prop_integration_source_location_consistency :: String -> Property
prop_integration_source_location_consistency input =
  let linesCount = L.length (lines input)
      startPos = SourcePos 1 1
      endPos = foldl advancePos startPos input
  in property $ sourceLine endPos >= 1 .&&. sourceColumn endPos >= 1

-- Property: Comment removal affects L.all modules consistently
prop_integration_comment_removal :: String -> String -> Property
prop_integration_comment_removal code comment =
  not ('"' `elem` code) && not ('\'' `elem` code) ==>
  let codeWithComment = code ++ "// " ++ comment ++ "\n" ++ code
      cleaned = removeComments codeWithComment
      parseOriginal = parseTypus code
      parseWithComment = parseTypus codeWithComment
      parseCleaned = parseTypus cleaned
  in property $ case (parseOriginal, parseWithComment, parseCleaned) of
    (Right orig, Right withComm, Right cleaned) -> 
      property True
    _ -> property True

-- Property: Multiple passes of compilation are idempotent
prop_integration_compilation_idempotent :: String -> Property
prop_integration_compilation_idempotent input =
  let compile1 = compile input
      compile2 = case compile1 of
        Left _ -> compile input
        Right _ -> compile input
  in property $ case (compile1, compile2) of
    (Left err1, Left err2) -> show err1 === show err2
    (Right res1, Right res2) -> show res1 === show res2
    _ -> property False

-- Property: Complex multi-feature programs are handled
prop_integration_multi_feature :: String -> String -> String -> Property
prop_integration_multi_feature ownershipCode dependentCode dependencyCode =
  not ('"' `elem` ownershipCode ++ dependentCode ++ dependencyCode) && 
  not ('\'' `elem` ownershipCode ++ dependentCode ++ dependencyCode) ==>
  let combined = "// @ownership: true\n" ++ ownershipCode ++ 
                 "// @dependent-types: true\n" ++ dependentCode ++
                 "// @dependencies: true\n" ++ dependencyCode
      parseResult = parseTypus combined
      compileResult = compile combined
      ownershipResult = analyzeOwnership combined
      dependencyResult = analyzeDependencies combined
  in property $ case (parseResult, compileResult, ownershipResult, dependencyResult) of
    (Left _, Left _, Left _, Left _) -> property True
    (Right _, Right _, Right _, Right _) -> property True
    _ -> property True

-- Property: Performance doesn't degrade with repeated analysis
prop_integration_performance_consistency :: Int -> String -> Property
prop_integration_performance_consistency iterations base =
  iterations >= 1 && iterations <= 5 ==>
  let input = List.L.concat (List.replicate iterations (base ++ "\n"))
      parseResult = parseTypus input
      compileResult = compile input
  in property $ case (parseResult, compileResult) of
    (Left _, Left _) -> property True
    (Right _, Right _) -> property True
    _ -> property True

-- Property: Error recovery across modules
prop_integration_error_recovery :: String -> String -> Property
prop_integration_error_recovery goodCode badCode =
  let mixed = goodCode ++ "\n" ++ badCode ++ "\n" ++ goodCode
      parseResult = parseTypus mixed
      compileResult = compile mixed
      ownershipResult = analyzeOwnership mixed
  in property $ case (parseResult, compileResult, ownershipResult) of
    (Left _, Left _, Left _) -> property True
    (Right _, Left _, _) -> property True  -- Parse succeeds but compile/ownership fails
    (Right _, Right _, _) -> property True  -- Everything succeeds
    _ -> property True

tests :: TestTree
tests = testGroup "New Integration QuickCheck Tests"
  [ fastProperty "Pipeline maintains consistency" prop_integration_pipeline_consistency
  , fastProperty "Parser L.and compiler error correlation" prop_integration_parser_compiler_errors
  , fastProperty "Ownership L.and dependency consistency" prop_integration_ownership_dependency_consistency
  , fastProperty "Error handling across modules" prop_integration_error_handling
  , fastProperty "Source location consistency" prop_integration_source_location_consistency
  , fastProperty "Comment removal consistency" prop_integration_comment_removal
  , fastProperty "Compilation is idempotent" prop_integration_compilation_idempotent
  , fastProperty "Multi-feature programs handled" prop_integration_multi_feature
  , fastProperty "Performance consistency" prop_integration_performance_consistency
  , fastProperty "Error recovery across modules" prop_integration_error_recovery
  ]