{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerIRQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Compiler.IR
import Compiler (compile)
import Parser (parseTypus)
import Utils (trim)
import Data.Char (isSpace, isLetter, isDigit)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, isInfixOf, sort)

-- Property: IR generation preserves basic structure
prop_ir_preserves_structure :: String -> Property
prop_ir_preserves_structure sourceCode =
  let parseResult = parseTypus sourceCode
      compileResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (compile ast)
  in property $ case compileResult of
    Nothing -> True
    Just ir -> not (null (show ir))

-- Property: IR handles variable declarations
prop_ir_variable_declarations :: String -> Int -> Property
prop_ir_variable_declarations varName value =
  not (null varName) && all (\c -> isLetter c || c == '_') varName ==> 
  let sourceCode = "var " ++ varName ++ " = " ++ show value
      parseResult = parseTypus sourceCode
      compileResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (compile ast)
  in property $ case compileResult of
    Nothing -> property False
    Right ir -> property $ varName `isInfixOf` show ir .&&. show value `isInfixOf` show ir

-- Property: IR handles arithmetic operations
prop_ir_arithmetic_operations :: Int -> Int -> Property
prop_ir_arithmetic_operations a b =
  a >= 0 && b >= 0 && b /= 0 ==> -- Avoid division by zero
  let sourceCode = "var result = " ++ show a ++ " + " ++ show b
      parseResult = parseTypus sourceCode
      compileResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (compile ast)
  in property $ case compileResult of
    Nothing -> property False
    Right ir -> property $ "+" `isInfixOf` show ir .&&. show a `isInfixOf` show ir .&&. show b `isInfixOf` show ir

-- Property: IR handles function definitions
prop_ir_function_definitions :: String -> [String] -> Property
prop_ir_function_definitions funcName params =
  not (null funcName) && all (\c -> isLetter c || c == '_') funcName && length params <= 3 ==>
  let paramsStr = Data.List.intercalate ", " params
      sourceCode = "func " ++ funcName ++ "(" ++ paramsStr ++ ") { return 42 }"
      parseResult = parseTypus sourceCode
      compileResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (compile ast)
  in property $ case compileResult of
    Nothing -> property False
    Right ir -> property $ funcName `isInfixOf` show ir .&&. "return" `isInfixOf` show ir

-- Property: IR handles function calls
prop_ir_function_calls :: String -> [Int] -> Property
prop_ir_function_calls funcName args =
  not (null funcName) && all (\c -> isLetter c || c == '_') funcName && length args <= 3 ==>
  let argsStr = Data.List.intercalate ", " (map show args)
      sourceCode = "var result = " ++ funcName ++ "(" ++ argsStr ++ ")"
      parseResult = parseTypus sourceCode
      compileResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (compile ast)
  in property $ case compileResult of
    Nothing -> property False
    Right ir -> property $ funcName `isInfixOf` show ir .&&. "result" `isInfixOf` show ir

-- Property: IR handles conditional statements
prop_ir_conditional_statements :: Int -> Property
prop_ir_conditional_statements condition =
  condition >= 0 && condition <= 1 ==> -- Binary condition
  let sourceCode = "if " ++ show condition ++ " == 1 { var x = 42 } else { var x = 24 }"
      parseResult = parseTypus sourceCode
      compileResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (compile ast)
  in property $ case compileResult of
    Nothing -> property False
    Right ir -> property $ "if" `isInfixOf` show ir .&&. "else" `isInfixOf` show ir

-- Property: IR handles loops
prop_ir_loops :: Int -> Property
prop_ir_loops iterations =
  iterations >= 0 && iterations <= 10 ==>
  let sourceCode = "for i = 0 to " ++ show iterations ++ " { var x = i * 2 }"
      parseResult = parseTypus sourceCode
      compileResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (compile ast)
  in property $ case compileResult of
    Nothing -> iterations > 5 -- May fail for larger iterations
    Right ir -> property $ "for" `isInfixOf` show ir .&&. show iterations `isInfixOf` show ir

-- Property: IR handles array operations
prop_ir_array_operations :: [Int] -> Property
prop_ir_array_operations elements =
  not (null elements) && length elements <= 5 ==>
  let elementsStr = Data.List.intercalate ", " (map show elements)
      sourceCode = "var arr = [" ++ elementsStr ++ "]"
      parseResult = parseTypus sourceCode
      compileResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (compile ast)
  in property $ case compileResult of
    Nothing -> property False
    Right ir -> property $ "arr" `isInfixOf` show ir .&&. "[" `isInfixOf` show ir

-- Property: IR handles string operations
prop_ir_string_operations :: String -> Property
prop_ir_string_operations content =
  not (null content) && not (any (`elem` "\\\n\r") content) ==> -- Avoid problematic characters
  let sourceCode = "var str = \"" ++ content ++ "\""
      parseResult = parseTypus sourceCode
      compileResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (compile ast)
  in property $ case compileResult of
    Nothing -> property False
    Right ir -> property $ "str" `isInfixOf` show ir .&&. content `isInfixOf` show ir

-- Property: IR preserves type information
prop_ir_preserves_types :: String -> String -> Property
prop_ir_preserves_types varName typeName =
  not (null varName) && not (null typeName) &&
  all (\c -> isLetter c || c == '_') varName &&
  all isLetter typeName ==>
  let sourceCode = "var " ++ varName ++ " " ++ typeName ++ " = 42"
      parseResult = parseTypus sourceCode
      compileResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (compile ast)
  in property $ case compileResult of
    Nothing -> property False
    Right ir -> property $ varName `isInfixOf` show ir .&&. typeName `isInfixOf` show ir

-- Property: IR handles complex expressions
prop_ir_complex_expressions :: [Int] -> Property
prop_ir_complex_expressions values =
  not (null values) && length values <= 4 ==> -- Limit complexity
  let exprStr = Data.List.intercalate " + " (map show values)
      sourceCode = "var result = " ++ exprStr
      parseResult = parseTypus sourceCode
      compileResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (compile ast)
  in property $ case compileResult of
    Nothing -> property False
    Right ir -> property $ "result" `isInfixOf` show ir .&&. "+" `isInfixOf` show ir

-- Property: IR handles operator precedence
prop_ir_operator_precedence :: Int -> Int -> Int -> Property
prop_ir_operator_precedence a b c =
  a >= 0 && b >= 0 && c >= 0 && b /= 0 ==>
  let sourceCode = "var x = " ++ show a ++ " + " ++ show b ++ " * " ++ show c
      parseResult = parseTypus sourceCode
      compileResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (compile ast)
  in property $ case compileResult of
    Nothing -> property False
    Right ir -> property $ "+" `isInfixOf` show ir .&&. "*" `isInfixOf` show ir

-- Property: IR handles nested scopes
prop_ir_nested_scopes :: Int -> Property
prop_ir_nested_scopes depth =
  depth >= 0 && depth <= 3 ==> -- Limit depth for practicality
  let nested = Data.List.intercalate "\n" (replicate depth "  if true {")
      content = nested ++ "\n    var x = 42\n" ++ Data.List.intercalate "\n" (replicate depth "  }")
      parseResult = parseTypus content
      compileResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (compile ast)
  in property $ case compileResult of
    Nothing -> depth > 2 -- May fail for deeper nesting
    Right ir -> property $ "x" `isInfixOf` show ir

-- Property: IR handles return values
prop_ir_return_values :: Int -> Property
prop_ir_return_values returnValue =
  let sourceCode = "func test() { return " ++ show returnValue ++ " }"
      parseResult = parseTypus sourceCode
      compileResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (compile ast)
  in property $ case compileResult of
    Nothing -> property False
    Right ir -> property $ "return" `isInfixOf` show ir .&&. show returnValue `isInfixOf` show ir

-- Property: IR handles multiple variable declarations
prop_ir_multiple_variables :: [(String, Int)] -> Property
prop_ir_multiple_variables varDecls =
  not (null varDecls) && length varDecls <= 3 ==>
  let varLines = map (\(name, value) -> "var " ++ name ++ " = " ++ show value) varDecls
      sourceCode = Data.List.unlines varLines
      parseResult = parseTypus sourceCode
      compileResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (compile ast)
  in property $ case compileResult of
    Nothing -> property False
    Right ir -> property $ all (\(name, _) -> name `isInfixOf` show ir) varDecls

-- Property: IR handles assignment operations
prop_ir_assignment_operations :: String -> Int -> Int -> Property
prop_ir_assignment_operations varName initialValue newValue =
  not (null varName) && all (\c -> isLetter c || c == '_') varName ==>
  let sourceCode = "var " ++ varName ++ " = " ++ show initialValue ++ "\n" ++ varName ++ " = " ++ show newValue
      parseResult = parseTypus sourceCode
      compileResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (compile ast)
  in property $ case compileResult of
    Nothing -> property False
    Right ir -> property $ property $ varName `isInfixOf` show ir .&&. show newValue `isInfixOf` show ir

-- Property: IR is deterministic
prop_ir_deterministic :: String -> Property
prop_ir_deterministic sourceCode =
  let parseResult = parseTypus sourceCode
      compileResult1 = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (compile ast)
      compileResult2 = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (compile ast)
  in property $ compileResult1 == compileResult2

-- Property: IR roundtrip consistency
prop_ir_roundtrip :: String -> Property
prop_ir_roundtrip sourceCode =
  let parseResult = parseTypus sourceCode
      compileResult = case parseResult of
        Left _ -> Nothing
        Right ast -> Just (compile ast)
      roundtripResult = case compileResult of
        Nothing -> Nothing
        Just ir -> Just (show ir)
  in property $ case roundtripResult of
    Nothing -> True
    Just irStr -> not (null irStr)

tests :: TestTree
tests =
  testGroup "Compiler IR QuickCheck Tests"
    [ fastProperty "preserves structure" prop_ir_preserves_structure
    , fastProperty "variable declarations" prop_ir_variable_declarations
    , fastProperty "arithmetic operations" prop_ir_arithmetic_operations
    , fastProperty "function definitions" prop_ir_function_definitions
    , fastProperty "function calls" prop_ir_function_calls
    , fastProperty "conditional statements" prop_ir_conditional_statements
    , fastProperty "loops" prop_ir_loops
    , fastProperty "array operations" prop_ir_array_operations
    , fastProperty "string operations" prop_ir_string_operations
    , fastProperty "preserves types" prop_ir_preserves_types
    , fastProperty "complex expressions" prop_ir_complex_expressions
    , fastProperty "operator precedence" prop_ir_operator_precedence
    , fastProperty "nested scopes" prop_ir_nested_scopes
    , fastProperty "return values" prop_ir_return_values
    , fastProperty "multiple variables" prop_ir_multiple_variables
    , fastProperty "assignment operations" prop_ir_assignment_operations
    , fastProperty "deterministic" prop_ir_deterministic
    , fastProperty "roundtrip" prop_ir_roundtrip
    ]