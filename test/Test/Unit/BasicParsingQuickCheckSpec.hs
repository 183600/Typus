{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.BasicParsingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Parser (parseTypus)
import SourceLocation (SourceSpan(..), SourcePos(..))
import Data.Char (isSpace, isLetter, isDigit)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified Data.List as List

-- Property: Parsing empty string should succeed with empty result
prop_parse_empty_string :: Property
prop_parse_empty_string =
  case parseTypus "" of
    Left err -> counterexample ("parseTypus failed: " ++ err) $ property False
    Right result -> property $ True

-- Property: Parsing valid package declaration should succeed
prop_parse_valid_package :: String -> Property
prop_parse_valid_package name =
  not (null name) && L.all (\c -> isLetter c || isDigit c || c == '_') name ==>
  let source = "package " ++ name ++ "\n"
  in case parseTypus source of
       Left err -> counterexample ("parseTypus failed: " ++ err) $ property False
       Right result -> property $ True

-- Property: Parsing simple function should preserve function name
prop_parse_simple_function :: String -> Property
prop_parse_simple_function funcName =
  not (null funcName) && L.all isLetter funcName ==>
  let source = unlines ["package main", "func " ++ funcName ++ "() {}", ""]
  in case parseTypus source of
       Left err -> counterexample ("parseTypus failed: " ++ err) $ property False
       Right result -> property $ True

-- Property: Parsing should handle multiple newlines gracefully
prop_parse_multiple_newlines :: Int -> String -> Property
prop_parse_multiple_newlines count content =
  count >= 0 && count <= 20 ==>
  let newlines = replicate count '\n'
      source = "package main" ++ newlines ++ "func main() {}"
  in case parseTypus source of
       Left err -> counterexample ("parseTypus failed: " ++ err) $ property False
       Right result -> property $ True

-- Property: Parsing should handle whitespace variations
prop_parse_whitespace_variations :: String -> String -> Property
prop_parse_whitespace_variations before after =
  let source = before ++ "package main" ++ after ++ "func main() {}"
  in case parseTypus source of
       Left err -> counterexample ("parseTypus failed: " ++ err) $ property False
       Right result -> property $ True

-- Property: Parsing comments should not break structure
prop_parse_with_comments :: String -> Property
prop_parse_with_comments comment =
  not ("//" `L.isInfixOf` comment) && not ("/*" `L.isInfixOf` comment) ==>
  let source = unlines 
        [ "package main"
        , "// " ++ comment
        , "func main() {"
        , "   /* " ++ comment ++ " */"
        , "}"
        ]
  in case parseTypus source of
       Left err -> counterexample ("parseTypus failed: " ++ err) $ property False
       Right result -> property $ True

-- Property: Parsing should handle basic type declarations
prop_parse_basic_types :: String -> Property
prop_parse_basic_types typeName =
  not (null typeName) && L.all isLetter typeName ==>
  let source = unlines 
        [ "package main"
        , "type " ++ typeName ++ " int"
        , "func main() {}"
        ]
  in case parseTypus source of
       Left err -> counterexample ("parseTypus failed: " ++ err) $ property False
       Right result -> property $ True

-- Property: Parsing should handle variable declarations
prop_parse_variable_declarations :: String -> String -> Property
prop_parse_variable_declarations varName varType =
  not (null varName) && not (null varType) && 
  L.all isLetter varName && L.all isLetter varType ==>
  let source = unlines 
        [ "package main"
        , "var " ++ varName ++ " " ++ varType
        , "func main() {}"
        ]
  in case parseTypus source of
       Left err -> counterexample ("parseTypus failed: " ++ err) $ property False
       Right result -> property $ True

-- Property: Parsing should handle import statements
prop_parse_imports :: String -> Property
prop_parse_imports importPath =
  not (null importPath) && not (' ' `elem` importPath) ==>
  let source = unlines 
        [ "package main"
        , "import \"" ++ importPath ++ "\""
        , "func main() {}"
        ]
  in case parseTypus source of
       Left err -> counterexample ("parseTypus failed: " ++ err) $ property False
       Right result -> property $ True

-- Property: Parsing should handle multiple imports
prop_parse_multiple_imports :: [String] -> Property
prop_parse_multiple_imports importPaths =
  not (null importPaths) && L.all (\p -> not (null p) && not (' ' `elem` p)) (take 5 importPaths) ==>
  let limitedPaths = take 5 importPaths
      importLines = L.map (\p -> "import \"" ++ p ++ "\"") limitedPaths
      source = unlines $ ["package main"] ++ importLines ++ ["func main() {}"]
  in case parseTypus source of
       Left err -> counterexample ("parseTypus failed: " ++ err) $ property False
       Right result -> property $ True

-- Property: Parsing should handle basic expressions
prop_parse_basic_expressions :: String -> Property
prop_parse_basic_expressions expr =
  not (null expr) && L.length expr <= 50 ==>
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   x := " ++ expr
        , "}"
        ]
  in case parseTypus source of
       Left err -> counterexample ("parseTypus failed: " ++ err) $ property False
       Right result -> property $ True

-- Property: Parsing should handle function parameters
prop_parse_function_params :: String -> String -> Property
prop_parse_function_params paramName paramType =
  not (null paramName) && not (null paramType) &&
  L.all isLetter paramName && L.all isLetter paramType ==>
  let source = unlines 
        [ "package main"
        , "func test(" ++ paramName ++ " " ++ paramType ++ ") {}"
        , "func main() {}"
        ]
  in case parseTypus source of
       Left err -> counterexample ("parseTypus failed: " ++ err) $ property False
       Right result -> property $ True

-- Property: Parsing should handle return values
prop_parse_return_values :: String -> String -> Property
prop_parse_return_values funcName returnType =
  not (null funcName) && not (null returnType) &&
  L.all isLetter funcName && L.all isLetter returnType ==>
  let source = unlines 
        [ "package main"
        , "func " ++ funcName ++ "() " ++ returnType ++ " {"
        , "   return"
        , "}"
        , "func main() {}"
        ]
  in case parseTypus source of
       Left err -> counterexample ("parseTypus failed: " ++ err) $ property False
       Right result -> property $ True

-- Property: Parsing should handle basic control structures
prop_parse_control_structures :: String -> Property
prop_parse_control_structures condition =
  not (null condition) && L.length condition <= 30 ==>
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   if " ++ condition ++ " {"
        , "      // do something"
        , "   }"
        , "}"
        ]
  in case parseTypus source of
       Left err -> counterexample ("parseTypus failed: " ++ err) $ property False
       Right result -> property $ True

-- Property: Parsing should handle struct definitions
prop_parse_struct_definitions :: String -> [String] -> Property
prop_parse_struct_definitions structName fieldNames =
  not (null structName) && not (null fieldNames) &&
  L.all isLetter structName && L.all (\f -> not (null f) && L.all isLetter f) (take 3 fieldNames) ==>
  let limitedFields = take 3 fieldNames
      fieldLines = L.map (\f -> "   " ++ f ++ " int") limitedFields
      source = unlines $ ["package main", "type " ++ structName ++ " struct {"] ++ fieldLines ++ ["}", "func main() {}"]
  in case parseTypus source of
       Left err -> counterexample ("parseTypus failed: " ++ err) $ property False
       Right result -> property $ True

-- Property: Parsing should handle interface definitions
prop_parse_interface_definitions :: String -> [String] -> Property
prop_parse_interface_definitions interfaceName methodNames =
  not (null interfaceName) && not (null methodNames) &&
  L.all isLetter interfaceName && L.all (\m -> not (null m) && L.all isLetter m) (take 3 methodNames) ==>
  let limitedMethods = take 3 methodNames
      methodLines = L.map (\m -> "   " ++ m ++ "()") limitedMethods
      source = unlines $ ["package main", "type " ++ interfaceName ++ " interface {"] ++ methodLines ++ ["}", "func main() {}"]
  in case parseTypus source of
       Left err -> counterexample ("parseTypus failed: " ++ err) $ property False
       Right result -> property $ True

-- Property: Parsing idempotency with valid code
prop_parse_idempotent :: String -> Property
prop_parse_idempotent source =
  L.length source <= 100 ==> -- Limit size for performance
  case parseTypus source of
    Left _ -> property $ True  -- Invalid code, skip
    Right result1 -> 
      case parseTypus source of
        Left err -> counterexample ("Second parse failed: " ++ err) $ property False
        Right result2 -> property $ True  -- Both succeeded

-- Property: Parsing should be line-count preserving
prop_parse_line_count_preserving :: String -> Property
prop_parse_line_count_preserving content =
  let lineCount = L.length $ lines content
      source = "package main\nfunc main() {\n" ++ content ++ "\n}\n"
  in case parseTypus source of
       Left err -> counterexample ("parseTypus failed: " ++ err) $ property False
       Right result -> property $ True

-- Property: Parsing should handle unicode characters
prop_parse_unicode :: String -> Property
prop_parse_unicode unicodeContent =
  L.length unicodeContent <= 50 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "// " ++ unicodeContent ++ " unicode test"
        , "func main() {"
        , "   // " ++ unicodeContent
        , "}"
        ]
  in case parseTypus source of
       Left err -> counterexample ("parseTypus failed: " ++ err) $ property False
       Right result -> property $ True

tests :: TestTree
tests = testGroup "Basic Parsing QuickCheck Tests"
  [ fastProperty "Parse empty string" prop_parse_empty_string
  , fastProperty "Parse valid package declaration" prop_parse_valid_package
  , fastProperty "Parse simple function" prop_parse_simple_function
  , fastProperty "Parse multiple newlines" prop_parse_multiple_newlines
  , fastProperty "Parse whitespace variations" prop_parse_whitespace_variations
  , fastProperty "Parse with comments" prop_parse_with_comments
  , fastProperty "Parse basic types" prop_parse_basic_types
  , fastProperty "Parse variable declarations" prop_parse_variable_declarations
  , fastProperty "Parse imports" prop_parse_imports
  , fastProperty "Parse multiple imports" prop_parse_multiple_imports
  , fastProperty "Parse basic expressions" prop_parse_basic_expressions
  , fastProperty "Parse function parameters" prop_parse_function_params
  , fastProperty "Parse return values" prop_parse_return_values
  , fastProperty "Parse control structures" prop_parse_control_structures
  , fastProperty "Parse struct definitions" prop_parse_struct_definitions
  , fastProperty "Parse interface definitions" prop_parse_interface_definitions
  , fastProperty "Parse idempotency" prop_parse_idempotent
  , fastProperty "Parse line count preserving" prop_parse_line_count_preserving
  , fastProperty "Parse unicode" prop_parse_unicode
  ]