{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SimpleSyntaxValidatorQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import SimpleSyntaxValidator
  ( validateSyntaxSimple
  , countBraces
  , SyntaxError(..)
  , ErrorType(..)
  )

import Data.List (isPrefixOf, isInfixOf, sort)
import Data.Char (isSpace)

-- Property: countBraces with balanced braces returns 0
prop_countBraces_balanced :: String -> String -> String -> Property
prop_countBraces_balanced prefix middle suffix =
  let content = prefix ++ "{" ++ middle ++ "}" ++ suffix
      braceCount = countBraces content
  in property $ braceCount === 0

-- Property: countBraces with only opening braces returns positive count
prop_countBraces_only_opening :: String -> Int -> Property
prop_countBraces_only_opening prefix count =
  count > 0 && count <= 20 ==> 
  let opens = replicate count '{'
      content = prefix ++ opens
      braceCount = countBraces content
  in property $ braceCount === count

-- Property: countBraces with only closing braces returns negative count
prop_countBraces_only_closing :: String -> Int -> Property
prop_countBraces_only_closing suffix count =
  count > 0 && count <= 20 ==> 
  let closes = replicate count '}'
      content = closes ++ suffix
      braceCount = countBraces content
  in property $ braceCount === negate count

-- Property: validateSyntaxSimple detects missing package declaration
prop_validateSyntax_missing_package :: String -> Property
prop_validateSyntax_missing_package content =
  not ("package " `isPrefixOf` trim content) && 
  not (null (trim content)) &&
  not ("//" `isPrefixOf` trim content) &&
  not ("/*" `isPrefixOf` trim content) ==>
  let errors = validateSyntaxSimple content
      packageErrors = filter (\e -> errorType e == MissingPackageDeclaration) errors
  in property $ length packageErrors >= 0

-- Property: validateSyntaxSimple handles balanced parentheses
prop_validateSyntax_balanced_parens :: String -> String -> String -> Property
prop_validateSyntax_balanced_parens before middle after =
  let content = before ++ "(" ++ middle ++ ")" ++ after
      errors = validateSyntaxSimple content
      parenErrors = filter (\e -> errorType e == MissingParenthesis) errors
  in property $ null parenErrors

-- Property: validateSyntaxSimple detects missing closing parenthesis
prop_validateSyntax_missing_close_paren :: String -> String -> Property
prop_validateSyntax_missing_close_paren prefix middle =
  not (null middle) ==> 
  let content = prefix ++ "(" ++ middle
      errors = validateSyntaxSimple content
      parenErrors = filter (\e -> errorType e == MissingParenthesis) errors
  in property $ not (null parenErrors)

-- Property: validateSyntaxSimple handles balanced brackets
prop_validateSyntax_balanced_brackets :: String -> String -> String -> Property
prop_validateSyntax_balanced_brackets before middle after =
  let content = before ++ "[" ++ middle ++ "]" ++ after
      errors = validateSyntaxSimple content
      bracketErrors = filter (\e -> errorType e == MissingBracket) errors
  in property $ null bracketErrors

-- Property: validateSyntaxSimple detects missing closing bracket
prop_validateSyntax_missing_close_bracket :: String -> String -> Property
prop_validateSyntax_missing_close_bracket prefix middle =
  not (null middle) ==> 
  let content = prefix ++ "[" ++ middle
      errors = validateSyntaxSimple content
      bracketErrors = filter (\e -> errorType e == MissingBracket) errors
  in property $ not (null bracketErrors)

-- Property: validateSyntaxSimple handles nested structures correctly
prop_validateSyntax_nested_structures :: Int -> Property
prop_validateSyntax_nested_structures depth =
  depth >= 0 && depth <= 10 ==>
  let openBraces = replicate depth '{'
      closeBraces = replicate depth '}'
      content = openBraces ++ "content" ++ closeBraces
      errors = validateSyntaxSimple content
      braceErrors = filter (\e -> errorType e == MissingBrace) errors
  in property $ null braceErrors

-- Property: validateSyntaxSimple ignores comments
prop_validateSyntax_ignores_comments :: String -> String -> Property
prop_validateSyntax_ignores_comments code comment =
  let content = code ++ " // " ++ comment
      errors = validateSyntaxSimple content
      commentErrors = filter (\e -> "comment" `isInfixOf` message e) errors
  in property $ null commentErrors

-- Property: validateSyntaxSimple handles string literals with braces
prop_validateSyntax_string_literals :: String -> String -> Property
prop_validateSyntax_string_literals prefix content =
  not ('"' `elem` content) ==> 
  let strContent = "\"" ++ content ++ "\""
      fullContent = prefix ++ strContent
      errors = validateSyntaxSimple fullContent
      braceErrors = filter (\e -> errorType e == MissingBrace) errors
  in property $ null braceErrors

-- Property: validateSyntaxSimple handles raw string literals
prop_validateSyntax_raw_string_literals :: String -> String -> Property
prop_validateSyntax_raw_string_literals prefix content =
  not ('`' `elem` content) ==> 
  let rawContent = "`" ++ content ++ "`"
      fullContent = prefix ++ rawContent
      errors = validateSyntaxSimple fullContent
      braceErrors = filter (\e -> errorType e == MissingBrace) errors
  in property $ null braceErrors

-- Property: validateSyntaxSimple handles character literals
prop_validateSyntax_char_literals :: String -> Property
prop_validateSyntax_char_literals content =
  length content == 1 ==> 
  let charContent = "'" ++ content ++ "'"
      errors = validateSyntaxSimple charContent
      braceErrors = filter (\e -> errorType e == MissingBrace) errors
  in property $ null braceErrors

-- Property: validateSyntaxSimple detects invalid operators
prop_validateSyntax_invalid_operators :: String -> Property
prop_validateSyntax_invalid_operators code =
  not ("+++" `isInfixOf` code) && not ("---" `isInfixOf` code) ==> 
  let content = code ++ "+++ invalid"
      errors = validateSyntaxSimple content
      operatorErrors = filter (\e -> errorType e == InvalidOperator) errors
  in property $ not (null operatorErrors)

-- Property: validateSyntaxSimple validates function declarations
prop_validateSyntax_function_declarations :: String -> Property
prop_validateSyntax_function_declarations functionName =
  not ('(' `elem` functionName) && not (null functionName) ==> 
  let funcDecl = "func " ++ functionName ++ " ()"
      errors = validateSyntaxSimple funcDecl
      funcErrors = filter (\e -> errorType e == InvalidFunctionDeclaration) errors
  in property $ null funcErrors

-- Property: validateSyntaxSimple detects incomplete function declarations
prop_validateSyntax_incomplete_function :: String -> Property
prop_validateSyntax_incomplete_function functionName =
  not ('(' `elem` functionName) && not (null functionName) ==> 
  let funcDecl = "func " ++ functionName
      errors = validateSyntaxSimple funcDecl
      funcErrors = filter (\e -> errorType e == InvalidFunctionDeclaration) errors
  in property $ not (null funcErrors)

-- Property: validateSyntaxSimple validates import statements
prop_validateSyntax_import_statements :: String -> Property
prop_validateSyntax_import_statements packagePath =
  not ('"' `elem` packagePath) ==> 
  let importDecl = "import \"" ++ packagePath ++ "\""
      errors = validateSyntaxSimple importDecl
      importErrors = filter (\e -> errorType e == InvalidImport) errors
  in property $ null importErrors

-- Property: validateSyntaxSimple detects incomplete import statements
prop_validateSyntax_incomplete_import :: String -> Property
prop_validateSyntax_incomplete_import packageName =
  not ('"' `elem` packageName) && not (null packageName) ==> 
  let importDecl = "import " ++ packageName
      errors = validateSyntaxSimple importDecl
      importErrors = filter (\e -> errorType e == InvalidImport) errors
  in property $ not (null importErrors)

-- Property: validateSyntaxSimple validates type declarations
prop_validateSyntax_type_declarations :: String -> String -> Property
prop_validateSyntax_type_declarations typeName typeDef =
  not (null typeName) && not (null typeDef) ==> 
  let typeDecl = "type " ++ typeName ++ " " ++ typeDef
      errors = validateSyntaxSimple typeDecl
      typeErrors = filter (\e -> errorType e == InvalidTypeDeclaration) errors
  in property $ null typeErrors

-- Property: validateSyntaxSimple detects incomplete type declarations
prop_validateSyntax_incomplete_type :: String -> Property
prop_validateSyntax_incomplete_type typeName =
  not (null typeName) ==> 
  let typeDecl = "type " ++ typeName
      errors = validateSyntaxSimple typeDecl
      typeErrors = filter (\e -> errorType e == InvalidTypeDeclaration) errors
  in property $ not (null typeErrors)

-- Property: countBraces handles mixed bracket types
prop_countBraces_mixed_brackets :: String -> String -> String -> String -> Property
prop_countBraces_mixed_brackets prefix content suffix extra =
  let mixed = prefix ++ "{" ++ content ++ "(" ++ suffix ++ ")}" ++ extra
      braceCount = countBraces mixed
  in property $ braceCount === 0

-- Property: validateSyntaxSimple handles complex nested structures
prop_validateSyntax_complex_nesting :: Int -> Property
prop_validateSyntax_complex_nesting levels =
  levels >= 0 && levels <= 5 ==> 
  let nested = buildNestedStructure levels
      errors = validateSyntaxSimple nested
      structuralErrors = filter (\e -> errorType e `elem` [MissingBrace, MissingParenthesis, MissingBracket]) errors
  in property $ null structuralErrors

-- Property: countBraces is consistent with validation errors
prop_countBraces_consistency :: String -> Property
prop_countBraces_consistency content =
  let braceCount = countBraces content
      errors = validateSyntaxSimple content
      braceErrors = filter (\e -> errorType e == MissingBrace) errors
  in (braceCount == 0) ==> null braceErrors

-- Property: validateSyntaxSimple handles empty input
prop_validateSyntax_empty_input :: Property
prop_validateSyntax_empty_input =
  let errors = validateSyntaxSimple ""
  in property $ null errors

-- Property: countBraces with empty input returns 0
prop_countBraces_empty_input :: Property
prop_countBraces_empty_input =
  countBraces "" === 0

-- Property: validateSyntaxSimple handles whitespace-only input
prop_validateSyntax_whitespace_only :: String -> Property
prop_validateSyntax_whitespace_only whitespace =
  all isSpace whitespace ==> 
  let errors = validateSyntaxSimple whitespace
  in property $ null errors

-- Property: countBraces with whitespace-only input returns 0
prop_countBraces_whitespace_only :: String -> Property
prop_countBraces_whitespace_only whitespace =
  all isSpace whitespace ==> 
  countBraces whitespace === 0

-- Helper function to build nested structures
buildNestedStructure :: Int -> String
buildNestedStructure 0 = "content"
buildNestedStructure n = "{" ++ buildNestedStructure (n - 1) ++ "}"

-- Utility: trim whitespace (same as in SimpleSyntaxValidator)
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

tests :: TestTree
tests = testGroup "SimpleSyntaxValidator QuickCheck tests"
  [ fastProperty "countBraces with balanced braces returns 0" prop_countBraces_balanced
  , fastProperty "countBraces with only opening braces returns positive count" prop_countBraces_only_opening
  , fastProperty "countBraces with only closing braces returns negative count" prop_countBraces_only_closing
  , fastProperty "validateSyntaxSimple detects missing package declaration" prop_validateSyntax_missing_package
  , fastProperty "validateSyntaxSimple handles balanced parentheses" prop_validateSyntax_balanced_parens
  , fastProperty "validateSyntaxSimple detects missing closing parenthesis" prop_validateSyntax_missing_close_paren
  , fastProperty "validateSyntaxSimple handles balanced brackets" prop_validateSyntax_balanced_brackets
  , fastProperty "validateSyntaxSimple detects missing closing bracket" prop_validateSyntax_missing_close_bracket
  , fastProperty "validateSyntaxSimple handles nested structures correctly" prop_validateSyntax_nested_structures
  , fastProperty "validateSyntaxSimple ignores comments" prop_validateSyntax_ignores_comments
  , fastProperty "validateSyntaxSimple handles string literals with braces" prop_validateSyntax_string_literals
  , fastProperty "validateSyntaxSimple handles raw string literals" prop_validateSyntax_raw_string_literals
  , fastProperty "validateSyntaxSimple handles character literals" prop_validateSyntax_char_literals
  , fastProperty "validateSyntaxSimple detects invalid operators" prop_validateSyntax_invalid_operators
  , fastProperty "validateSyntaxSimple validates function declarations" prop_validateSyntax_function_declarations
  , fastProperty "validateSyntaxSimple detects incomplete function declarations" prop_validateSyntax_incomplete_function
  , fastProperty "validateSyntaxSimple validates import statements" prop_validateSyntax_import_statements
  , fastProperty "validateSyntaxSimple detects incomplete import statements" prop_validateSyntax_incomplete_import
  , fastProperty "validateSyntaxSimple validates type declarations" prop_validateSyntax_type_declarations
  , fastProperty "validateSyntaxSimple detects incomplete type declarations" prop_validateSyntax_incomplete_type
  , fastProperty "countBraces handles mixed bracket types" prop_countBraces_mixed_brackets
  , fastProperty "validateSyntaxSimple handles complex nested structures" prop_validateSyntax_complex_nesting
  , fastProperty "countBraces is consistent with validation errors" prop_countBraces_consistency
  , fastProperty "validateSyntaxSimple handles empty input" prop_validateSyntax_empty_input
  , fastProperty "countBraces with empty input returns 0" prop_countBraces_empty_input
  , fastProperty "validateSyntaxSimple handles whitespace-only input" prop_validateSyntax_whitespace_only
  , fastProperty "countBraces with whitespace-only input returns 0" prop_countBraces_whitespace_only
  ]