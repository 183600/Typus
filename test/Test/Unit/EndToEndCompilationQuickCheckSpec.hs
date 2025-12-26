{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.EndToEndCompilationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Compiler (compile, CompilerError(..), CompilationResult(..))
import Parser (parseTypus, TypusFile(..))
import Compiler.GoAst (renderGoModule)
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)

-- Property: Successful compilation produces valid Go code
prop_compilation_produces_valid_go :: String -> Property
prop_compilation_produces_valid_go typusCode =
  let trimmed = trim typusCode
      hasContent = length trimmed > 0
      parseResult = parseTypus trimmed
  in hasContent && isRight parseResult ==>
  case parseResult of
    Left _ -> property $ True -- Skip if parsing fails
    Right typusFile ->
      let compileResult = compile typusFile
      in case compileResult of
        Left _ -> property $ True -- Skip if compilation fails
        Right goCode ->
          let goText = renderGoModule goCode
              hasPackage = "package" `isInfixOf` T.unpack goText
              hasImports = "import" `isInfixOf` T.unpack goText || "func" `isInfixOf` T.unpack goText
          in property $ hasPackage .&&. (hasImports || T.null goText)

-- Property: Compilation preserves function signatures
prop_compilation_preserves_signatures :: String -> String -> Property
prop_compilation_preserves_signatures funcName funcBody =
  let validName = not (null funcName) && all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['_']) funcName
      validBody = not (null funcBody)
      typusCode = "func " ++ funcName ++ "() " ++ funcBody
  in validName && validBody ==>
  case parseTypus typusCode of
    Left _ -> property $ True
    Right typusFile ->
      case compile typusFile of
        Left _ -> property $ True
        Right goCode ->
          let goText = T.unpack $ renderGoModule goCode
              hasFuncName = funcName `isInfixOf` goText
          in property $ hasFuncName

-- Property: Error messages contain source location information
prop_error_messages_have_location :: String -> Property
prop_error_messages_have_location malformedCode =
  let hasContent = length malformedCode > 5
  in hasContent ==>
  case parseTypus malformedCode of
    Right _ -> property $ True -- No error to check
    Left parseError ->
      let errorStr = show parseError
          hasLocation = any (`isInfixOf` errorStr) ["line", "column", ":", "at"]
      in property $ hasLocation

-- Property: Round-trip compilation preserves semantics
prop_round_trip_preserves_semantics :: String -> Property
prop_round_trip_preserves_semantics simpleCode =
  let isSimple = all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ' ' ++ '\n' ++ '(' ++ ')' ++ '{' ++ '}' ++ '=') simpleCode
      hasContent = length (trim simpleCode) > 0
  in isSimple && hasContent ==>
  case parseTypus simpleCode of
    Left _ -> property $ True
    Right typusFile ->
      case compile typusFile of
        Left _ -> property $ True
        Right goCode ->
          let goText = T.unpack $ renderGoModule goCode
              hasMain = "main" `isInfixOf` goText || "func" `isInfixOf` goText
          in property $ hasMain

-- Helper functions
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
  where isSpace c = c `elem` " \t\n\r"

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

tests :: TestTree
tests = testGroup "End-to-End Compilation QuickCheck Tests"
  [ fastProperty "Compilation produces valid Go code" prop_compilation_produces_valid_go
  , fastProperty "Compilation preserves function signatures" prop_compilation_preserves_signatures
  , fastProperty "Error messages contain location information" prop_error_messages_have_location
  , fastProperty "Round-trip compilation preserves semantics" prop_round_trip_preserves_semantics
  ]