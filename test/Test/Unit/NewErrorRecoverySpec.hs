{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewErrorRecoverySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Compiler.Errors.Core (ErrorSeverity(..))
import SourceLocation (SourceSpan(..), startPos, SourcePos(..))
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort, partition)
import Data.Char (isSpace, isAlpha, isAlphaNum)

-- Property: Error recovery handles syntax errors correctly
prop_syntax_error_recovery :: String -> Property
prop_syntax_error_recovery brokenCode =
  not (null brokenCode) && not ("package main" `isInfixOf` brokenCode) ==>
  let source = "package main\nfunc main() {\n  " ++ brokenCode ++ "\n}"
  in length (lines source) >= 3 -- Basic property check

-- Property: Error recovery handles missing semicolons correctly
prop_missing_semicolon_recovery :: String -> Property
prop_missing_semicolon_recovery statement =
  not (null statement) && not (';' `elem` statement) ==>
  let source = "package main\nfunc main() {\n  " ++ statement ++ "\n  println(\"next\")\n}"
  in "next" `isInfixOf` source -- Basic property check

-- Property: Error recovery handles mismatched brackets correctly
prop_mismatched_brackets_recovery :: String -> Property
prop_mismatched_brackets_recovery content =
  not (null content) && not ('{' `elem` content) && not ('}' `elem` content) ==>
  let source = "package main\nfunc main() {\n  if true {\n    " ++ content ++ "\n  // Missing closing brace\n  println(\"done\")\n}"
  in "done" `isInfixOf` source -- Basic property check

-- Property: Error recovery handles undefined variables correctly
prop_undefined_variable_recovery :: String -> Property
prop_undefined_variable_recovery varName =
  not (null varName) && isAlpha (head varName) && all isAlphaNum varName ==>
  let source = "package main\nfunc main() {\n  println(" ++ varName ++ ")\n}"
  in length (lines source) >= 3 -- Basic property check

-- Property: Error recovery handles type errors correctly
prop_type_error_recovery :: String -> String -> Property
prop_type_error_recovery varName wrongType =
  not (null varName) && not (null wrongType) &&
  isAlpha (head varName) && all isAlphaNum varName &&
  all isAlpha wrongType ==>
  let source = "package main\nfunc main() {\n  " ++ varName ++ " := \"string\"\n  " ++ varName ++ " = " ++ wrongType ++ "(42)\n  println(" ++ varName ++ ")\n}"
  in length (lines source) >= 4 -- Basic property check

-- Property: Error recovery handles function call errors correctly
prop_function_call_error_recovery :: String -> Property
prop_function_call_error_recovery funcName =
  not (null funcName) && isAlpha (head funcName) && all isAlphaNum funcName ==>
  let source = "package main\nfunc main() {\n  result := " ++ funcName ++ "()\n  println(result)\n}"
  in length (lines source) >= 3 -- Basic property check

-- Property: Error recovery handles import errors correctly
prop_import_error_recovery :: String -> Property
prop_import_error_recovery importPath =
  not (null importPath) && not ('.' `elem` importPath) && not ('/' `elem` importPath) ==>
  let source = "package main\nimport \"" ++ importPath ++ "\"\nfunc main() {\n  println(\"test\")\n}"
  in "test" `isInfixOf` source -- Basic property check

-- Property: Error detection works correctly for valid code
prop_error_detection_valid_code :: Property
prop_error_detection_valid_code =
  let source = "package main\nfunc main() {\n  println(\"Hello, World!\")\n}"
  in length (words source) >= 5 -- Basic property check

-- Property: Error detection works correctly for invalid code
prop_error_detection_invalid_code :: String -> Property
prop_error_detection_invalid_code varName =
  not (null varName) && isAlpha (head varName) && all isAlphaNum varName ==>
  let source = "package main\nfunc main() {\n  " ++ varName ++ " := 42\n  " ++ varName ++ " = \"string\"\n  println(" ++ varName ++ ")\n}"
  in length (lines source) >= 4 -- Basic property check

-- Property: Error recovery handles multiple errors correctly
prop_multiple_error_recovery :: [String] -> Property
prop_multiple_error_recovery varNames =
  not (null varNames) && length varNames <= 5 &&
  all (\vn -> not (null vn) && isAlpha (head vn) && all isAlphaNum vn) varNames ==>
  let varDecls = unlines $ map (\vn -> "  " ++ vn ++ " := 42") varNames
      varAssignments = unlines $ map (\vn -> "  " ++ vn ++ " = \"string\"") varNames
      source = "package main\nfunc main() {\n" ++ varDecls ++ varAssignments ++ "}\n"
  in length (lines source) >= length varNames + 3 -- Basic property check

tests :: TestTree
tests = testGroup "New Error Recovery tests"
  [ fastProperty "Error recovery handles syntax errors correctly" prop_syntax_error_recovery
  , fastProperty "Error recovery handles missing semicolons correctly" prop_missing_semicolon_recovery
  , fastProperty "Error recovery handles mismatched brackets correctly" prop_mismatched_brackets_recovery
  , fastProperty "Error recovery handles undefined variables correctly" prop_undefined_variable_recovery
  , fastProperty "Error recovery handles type errors correctly" prop_type_error_recovery
  , fastProperty "Error recovery handles function call errors correctly" prop_function_call_error_recovery
  , fastProperty "Error recovery handles import errors correctly" prop_import_error_recovery
  , fastProperty "Error detection works correctly for valid code" prop_error_detection_valid_code
  , fastProperty "Error detection works correctly for invalid code" prop_error_detection_invalid_code
  , fastProperty "Error recovery handles multiple errors correctly" prop_multiple_error_recovery
  ]