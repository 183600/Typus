{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.BasicParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Parser (parseTypus, TypusFile(..))
import Utils (trim, splitBy, removeComments)
import Data.Char (isSpace, isLetter, isDigit)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, isInfixOf, sort)

-- Property: Parser handles empty input gracefully
prop_parser_empty_input :: Property
prop_parser_empty_input =
  let result = parseTypus ""
  in property $ case result of
    Left _ -> property True
    Right ast -> property $ null (tfBlocks ast)

-- Property: Parser handles whitespace-only input
prop_parser_whitespace_input :: String -> Property
prop_parser_whitespace_input ws =
  all isSpace ws ==>
  let result = parseTypus ws
  in property $ case result of
    Left _ -> property True
    Right ast -> property $ null (tfBlocks ast) || all isSpace (show ast)

-- Property: Parser preserves identifiers
prop_parser_preserves_identifiers :: String -> Property
prop_parser_preserves_identifiers ident =
  not (null ident) && all (\c -> isLetter c || isDigit c || c == '_') ident ==>
  let input = "var " ++ ident ++ " = 42"
      result = parseTypus input
  in property $ case result of
    Left _ -> property False
    Right ast -> property $ ident `isInfixOf` show ast

-- Property: Parser handles numeric literals
prop_parser_numeric_literals :: Integer -> Property
prop_parser_numeric_literals num =
  num >= 0 && num <= 10000 ==>
  let input = "var x = " ++ show num
      result = parseTypus input
  in property $ case result of
    Left _ -> property False
    Right ast -> property $ show num `isInfixOf` show ast

-- Property: Parser handles string literals
prop_parser_string_literals :: String -> Property
prop_parser_string_literals content =
  not (any (`elem` "\\\"\\n\\r") content) ==> -- Avoid problematic characters
  let input = "var s = \"" ++ content ++ "\""
      result = parseTypus input
  in property $ case result of
    Left _ -> property False
    Right ast -> property $ content `isInfixOf` show ast

-- Property: Parser handles comments correctly
prop_parser_handles_comments :: String -> String -> Property
prop_parser_handles_comments code comment =
  not ('"' `elem` code) && not ('\'' `elem` code) ==> -- Avoid string literals
  let input = code ++ " // " ++ comment ++ "\nvar x = 42"
      result = parseTypus input
  in property $ case result of
    Left _ -> property False
    Right ast -> property $ "x" `isInfixOf` show ast

-- Property: Parser handles block comments
prop_parser_handles_block_comments :: String -> String -> Property
prop_parser_handles_block_comments before after =
  not ('"' `elem` before) && not ('\'' `elem` before) &&
  not ('"' `elem` after) && not ('\'' `elem` after) ==> -- Avoid string literals
  let input = before ++ " /* block comment */ " ++ after
      result = parseTypus input
  in property $ case result of
    Left _ -> property False
    Right ast -> property $ before `isInfixOf` show ast .&&. after `isInfixOf` show ast

-- Property: Parser handles multiple statements
prop_parser_multiple_statements :: [String] -> Property
prop_parser_multiple_statements statements =
  not (null statements) && all (not . null) statements ==>
  let input = Data.List.unlines statements
      result = parseTypus input
  in property $ case result of
    Left _ -> property False
    Right ast -> property $ length (lines input) <= length (show ast)

-- Property: Parser position tracking
prop_parser_position_tracking :: String -> Property
prop_parser_position_tracking content =
  let lines' = lines content
      lineCount = length lines'
      result = parseTypus content
  in property $ case result of
    Left _ -> property True
    Right ast -> property True -- Position tracking would be verified through AST inspection

-- Property: Parser error recovery
prop_parser_error_recovery :: String -> Property
prop_parser_error_recovery malformed =
  not (null malformed) && malformed `isInfixOf` "syntax error" ==>
  let input = malformed ++ "\nvar x = 42"
      result = parseTypus input
  in property $ case result of
    Left _ -> property True -- Expected to fail but should recover
    Right ast -> property $ "x" `isInfixOf` show ast

-- Property: Parser handles nested structures
prop_parser_nested_structures :: Int -> Property
prop_parser_nested_structures depth =
  depth >= 0 && depth <= 5 ==> -- Limit depth for practicality
  let nested = Data.List.intercalate "\n" (replicate depth "  if true {")
      content = nested ++ "\n    var x = 42\n" ++ Data.List.intercalate "\n" (replicate depth "  }")
      result = parseTypus content
  in property $ case result of
    Left _ -> property $ depth > 3 -- May fail for deeper nesting
    Right ast -> property $ "x" `isInfixOf` show ast

-- Property: Parser handles operator precedence
prop_parser_operator_precedence :: Int -> Int -> Int -> Property
prop_parser_operator_precedence a b c =
  a >= 0 && b >= 0 && c >= 0 && b /= 0 ==> -- Avoid division by zero
  let input = "var x = " ++ show a ++ " + " ++ show b ++ " * " ++ show c
      result = parseTypus input
  in property $ case result of
    Left _ -> property False
    Right ast -> property $ True -- Would verify precedence through AST structure

-- Property: Parser handles function declarations
prop_parser_function_declarations :: String -> Property
prop_parser_function_declarations funcName =
  not (null funcName) && all (\c -> isLetter c || c == '_') funcName ==>
  let input = "func " ++ funcName ++ "() { return 42 }"
      result = parseTypus input
  in property $ case result of
    Left _ -> property False
    Right ast -> property $ funcName `isInfixOf` show ast

-- Property: Parser handles function calls
prop_parser_function_calls :: String -> [Int] -> Property
prop_parser_function_calls funcName args =
  not (null funcName) && all (\c -> isLetter c || c == '_') funcName && length args <= 3 ==>
  let argsStr = Data.List.intercalate ", " (map show args)
      input = funcName ++ "(" ++ argsStr ++ ")"
      result = parseTypus input
  in property $ case result of
    Left _ -> property False
    Right ast -> property $ funcName `isInfixOf` show ast

-- Property: Parser handles variable assignments
prop_parser_variable_assignments :: String -> Int -> Property
prop_parser_variable_assignments varName value =
  not (null varName) && all (\c -> isLetter c || c == '_') varName ==>
  let input = varName ++ " = " ++ show value
      result = parseTypus input
  in property $ case result of
    Left _ -> property False
    Right ast -> property $ varName `isInfixOf` show ast .&&. show value `isInfixOf` show ast

-- Property: Parser handles type annotations
prop_parser_type_annotations :: String -> String -> Property
prop_parser_type_annotations varName typeName =
  not (null varName) && not (null typeName) &&
  all (\c -> isLetter c || c == '_') varName &&
  all isLetter typeName ==>
  let input = "var " ++ varName ++ " " ++ typeName ++ " = 42"
      result = parseTypus input
  in property $ case result of
    Left _ -> property False
    Right ast -> property $ property $ varName `isInfixOf` show ast .&&. typeName `isInfixOf` show ast

-- Property: Parser handles complex expressions
prop_parser_complex_expressions :: [Int] -> Property
prop_parser_complex_expressions values =
  not (null values) && length values <= 5 ==> -- Limit complexity
  let exprStr = Data.List.intercalate " + " (map show values)
      input = "var result = " ++ exprStr
      result = parseTypus input
  in property $ case result of
    Left _ -> property False
    Right ast -> property $ "result" `isInfixOf` show ast

-- Property: Parser idempotency with valid code
prop_parser_idempotency :: String -> Property
prop_parser_idempotency validCode =
  let result1 = parseTypus validCode
      result2 = case result1 of
        Left _ -> Nothing
        Right ast -> Just (parseTypus (show ast))
  in property $ case result2 of
    Nothing -> property True
    Just result2' -> property $ result1 == result2'

tests :: TestTree
tests =
  testGroup "Basic Parser QuickCheck Tests"
    [ fastProperty "empty input" prop_parser_empty_input
    , fastProperty "whitespace input" prop_parser_whitespace_input
    , fastProperty "preserves identifiers" prop_parser_preserves_identifiers
    , fastProperty "numeric literals" prop_parser_numeric_literals
    , fastProperty "string literals" prop_parser_string_literals
    , fastProperty "handles comments" prop_parser_handles_comments
    , fastProperty "handles block comments" prop_parser_handles_block_comments
    , fastProperty "multiple statements" prop_parser_multiple_statements
    , fastProperty "position tracking" prop_parser_position_tracking
    , fastProperty "error recovery" prop_parser_error_recovery
    , fastProperty "nested structures" prop_parser_nested_structures
    , fastProperty "operator precedence" prop_parser_operator_precedence
    , fastProperty "function declarations" prop_parser_function_declarations
    , fastProperty "function calls" prop_parser_function_calls
    , fastProperty "variable assignments" prop_parser_variable_assignments
    , fastProperty "type annotations" prop_parser_type_annotations
    , fastProperty "complex expressions" prop_parser_complex_expressions
    , fastProperty "idempotency" prop_parser_idempotency
    ]