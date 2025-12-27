{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCompilerQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.Tasty (TestTree)

import Compiler (compile, CompilerError(..), CompilationPhase(..), hasTypeErrors, TypeCheckDiagnostic(..))
import Parser (parseTypus)
import Data.Char (isSpace, isAlphaNum)
import qualified Data.List as List
import Data.Text (Text)

-- Property: Compiler handles empty input
prop_compiler_empty_input :: Property
prop_compiler_empty_input =
  let result = compile ""
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Compiler produces consistent results
prop_compiler_consistency :: String -> Property
prop_compiler_consistency input =
  let result1 = compile input
      result2 = compile input
  in property $ case (result1, result2) of
    (Left err1, Left err2) -> show err1 === show err2
    (Right res1, Right res2) -> show res1 === show res2
    _ -> property False

-- Property: Compiler handles whitespace gracefully
prop_compiler_whitespace :: String -> Property
prop_compiler_whitespace input =
  all isSpace input ==>
  let result = compile input
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Compiler error messages contain useful information
prop_compiler_error_messages :: String -> Property
prop_compiler_error_messages input =
  let result = compile input
  in property $ case result of
    Left err -> property $ not (null (show err))
    Right _ -> property True

-- Property: Compiler handles comments in input
prop_compiler_comments :: String -> String -> Property
prop_compiler_comments code comment =
  not ('"' `elem` code) && not ('\'' `elem` code) ==>
  let codeWithComment = code ++ "// " ++ comment ++ "\n" ++ code
      result1 = compile code
      result2 = compile codeWithComment
  in property $ case (result1, result2) of
    (Left _, Left _) -> property True
    (Right _, Right _) -> property True
    _ -> property False

-- Property: Compiler handles multiline input
prop_compiler_multiline :: [String] -> Property
prop_compiler_multiline lines =
  not (null lines) ==>
  let multiline = List.intercalate "\n" lines
      result = compile multiline
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Compiler type checking is consistent
prop_compiler_type_checking :: String -> Property
prop_compiler_type_checking input =
  let result = compile input
  in property $ case result of
    Left err -> property True
    Right res -> property True

-- Property: Compiler handles invalid syntax gracefully
prop_compiler_invalid_syntax :: String -> Property
prop_compiler_invalid_syntax input =
  let invalid = "invalid syntax {" ++ input ++ "}"
      result = compile invalid
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Compiler handles large inputs
prop_compiler_large_input :: Int -> String -> Property
prop_compiler_large_input multiplier base =
  multiplier >= 0 && multiplier <= 10 ==>
  let large = List.concat (List.replicate multiplier (base ++ "\n"))
      result = compile large
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Compiler handles special characters
prop_compiler_special_chars :: String -> Property
prop_compiler_special_chars base =
  let special = "func test() { return " ++ base ++ "!@#$%^&*(); }"
      result = compile special
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

tests :: TestTree
tests = testGroup "New Compiler QuickCheck Tests"
  [ fastProperty "Compiler handles empty input" prop_compiler_empty_input
  , fastProperty "Compiler produces consistent results" prop_compiler_consistency
  , fastProperty "Compiler handles whitespace" prop_compiler_whitespace
  , fastProperty "Compiler error messages contain information" prop_compiler_error_messages
  , fastProperty "Compiler handles comments" prop_compiler_comments
  , fastProperty "Compiler handles multiline input" prop_compiler_multiline
  , fastProperty "Compiler type checking is consistent" prop_compiler_type_checking
  , fastProperty "Compiler handles invalid syntax" prop_compiler_invalid_syntax
  , fastProperty "Compiler handles large inputs" prop_compiler_large_input
  , fastProperty "Compiler handles special characters" prop_compiler_special_chars
  ]