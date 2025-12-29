{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.NewParserRecoverySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, elements, listOf, oneof, sized)

import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import Text.Megaparsec (errorBundlePretty)
import Data.List (isInfixOf, isPrefixOf)

tests :: TestTree
tests = testGroup "New Parser Recovery Tests"
    [ testCase "recovers from syntax errors in function declarations" $ do
        let source = unlines
              [ "package main"
              , "func valid_function() {"
              , "  return 42"
              , "}"
              , "func invalid_function( {  // Missing parameter name"
              , "  return 24"
              , "}"
              , "func another_valid_function() {"
              , "  return 84"
              , "}"
              ]
        case parseTypus source of
          Left err -> do
            let errMsg = errorBundlePretty err
            assertBool "Should report syntax error" $ "syntax error" `isInfixOf` errMsg
            assertBool "Should report location of error" $ "line 5" `isInfixOf` errMsg
            -- Parser should attempt to continue and find other valid constructs
            assertBool "Should mention recovery attempt" $ 
              "recovery" `isInfixOf` errMsg || "continuing" `isInfixOf` errMsg
          Right _ -> assertFailure "Expected parsing error"
              
    , testCase "recovers from missing braces" $ do
        let source = unlines
              [ "package main"
              , "func missing_brace() {"
              , "  if true {"
              , "    return 42"
              , "  // Missing closing brace for if"
              , "  return 24"
              , "}"
              , "func next_function() {"
              , "  return 84"
              , "}"
              ]
        case parseTypus source of
          Left err -> do
            let errMsg = errorBundlePretty err
            assertBool "Should report missing brace" $ 
              "brace" `isInfixOf` errMsg || "}" `isInfixOf` errMsg
            assertBool "Should attempt to continue parsing" $ 
              "recovery" `isInfixOf` errMsg || "continuing" `isInfixOf` errMsg
          Right _ -> assertFailure "Expected parsing error"
              
    , testCase "recovers from invalid type annotations" $ do
        let source = unlines
              [ "package main"
              , "func valid_func(x: int) -> int {"
              , "  return x + 1"
              , "}"
              , "func invalid_func(y: InvalidType@@) -> StrangeType!! {"
              , "  return y"
              , "}"
              , "func another_valid_func(z: string) -> string {"
              , "  return z + \"suffix\""
              , "}"
              ]
        case parseTypus source of
          Left err -> do
            let errMsg = errorBundlePretty err
            assertBool "Should report invalid type" $ 
              "type" `isInfixOf` errMsg || "InvalidType" `isInfixOf` errMsg
            assertBool "Should continue to parse next function" $ 
              "another_valid_func" `isInfixOf` errMsg || "recovery" `isInfixOf` errMsg
          Right _ -> assertFailure "Expected parsing error"
    ]

-- QuickCheck properties for parser error recovery

-- Property: Parser should not crash on any input string
prop_parser_never_crashes :: String -> Property
prop_parser_never_crashes source =
  let result = parseTypus source
  in property $ case result of
       Left _ -> True  -- Error is expected for invalid input
       Right _ -> True  -- Success is also valid

-- Property: Error messages should contain line numbers for syntax errors
prop_error_messages_include_line_numbers :: String -> Property
prop_error_messages_include_line_numbers source =
  let result = parseTypus source
  in case result of
       Left err -> 
         let errMsg = errorBundlePretty err
         in property $ "line" `isInfixOf` errMsg || "Line" `isInfixOf` errMsg
       Right _ -> property $ True  -- No error, no line number needed

-- Helper functions for QuickCheck
generateValidTypusCode :: Gen String
generateValidTypusCode = do
  n <- elements [1..5]
  functions <- listOf $ generateValidFunction
  return $ unlines $ ["package main", ""] ++ functions

generateValidFunction :: Gen String
generateValidFunction = do
  name <- elements ["func1", "func2", "test", "calculate", "process"]
  return $ unlines
    [ "func " ++ name ++ "() -> int {"
    , "  return 42"
    , "}"
    ]

generateInvalidSyntax :: Gen String
generateInvalidSyntax = oneof
  [ return "func invalid( {  // Missing parameter name"
  , return "let x = ;  // Missing value"
  , return "if condition {  // Missing closing brace"
  , return "return 42 +  // Incomplete expression"
  , return "invalid_keyword xyz {"
  ]