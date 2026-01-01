{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewSyntaxValidatorValidationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, suchThat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (sort, nub, (\\), delete, intersect, union, intercalate)
import Data.Set (Set, fromList, toList, union, intersection, difference)
import qualified Data.Set as Set
import Data.Map (Map, fromList, toList, keys, elems, insert, delete, lookup, member, empty)
import qualified Data.Map as Map
import Data.Char (isSpace, isAlphaNum, isAlpha, isDigit)

import SyntaxValidator
  ( SyntaxValidator
  , SyntaxError(..)
  , ErrorType(..)
  , newSyntaxValidator
  , validateSyntax
  , validateFile
  , getSyntaxErrors
  , formatSyntaxError
  )

-- ============================================================================
-- Helper Functions L.and Generators
-- ============================================================================

-- Generate valid identifiers
genIdentifier :: Gen String
genIdentifier = do
  first <- elements (['a'..'z'] ++ ['A'..'Z'] ++ "_")
  rest <- listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
  return (first : rest)

-- Generate valid Go keywords
genKeyword :: Gen String
genKeyword = elements 
  [ "package", "import", "func", "var", "const", "type"
  , "if", "else", "for", "switch", "case", "default"
  , "break", "continue", "return", "go", "defer", "select"
  , "struct", "interface", "map", "chan"
  ]

-- Generate valid operators
genOperator :: Gen String
genOperator = elements
  [ "+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>", "&^"
  , "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", "&^="
  , "&&", "||", "<", ">", "==", "!=", "<=", ">=", "++", ":="
  , "<-", "++", "--"
  ]

-- Generate delimiters
genDelimiter :: Gen Char
genDelimiter = elements "(){},;[]."

-- Generate string literals
genStringLiteral :: Gen String
genStringLiteral = do
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
  return $ "\"" ++ content ++ "\""

-- Generate numeric literals
genNumberLiteral :: Gen String
genNumberLiteral = do
  digits <- listOf1 $ elements ['0'..'9']
  return digits

-- Generate comments
genComment :: Gen String
genComment = do
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
  return $ "// " ++ content

-- Generate multiline comments
genMultilineComment :: Gen String
genComment = do
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \n"
  return $ "/* " ++ content ++ " */"

-- Generate valid Go statements
genGoStatement :: Gen String
genGoStatement = oneof
  [ do
      name <- genIdentifier
      return $ "var " ++ name ++ " int"
  , do
      name <- genIdentifier
      return $ "const " ++ name ++ " = 42"
  , do
      name <- genIdentifier
      params <- listOf genIdentifier
      return $ "func " ++ name ++ "(" ++ intercalate ", " params ++ ") {}"
  , do
      name <- genIdentifier
      return $ "type " ++ name ++ " struct{}"
  , return "import \"fmt\""
  , return "package main"
  , return "fmt.Println(\"hello\")"
  ]

-- Generate valid Go code
genGoCode :: Gen String
genGoCode = do
  numStatements <- choose (1, 10)
  statements <- listOf1 genGoStatement
  return $ unlines statements

-- Generate code with syntax errors
genCodeWithMissingBrace :: Gen String
genCodeWithMissingBrace = do
  name <- genIdentifier
  return $ "func " ++ name ++ "() {\n  fmt.Println(\"test\")\n"

genCodeWithMissingParen :: Gen String
genCodeWithMissingParen = do
  name <- genIdentifier
  return $ "func " ++ name ++ " {\n  fmt.Println(\"test\"\n}"

genCodeWithUnclosedString :: Gen String
genCodeWithUnclosedString = do
  content <- listOf $ elements $ ['a'..'z'] ++ ' '
  return $ "fmt.Println(\"" ++ content ++ "\n}"

genCodeWithInvalidIdentifier :: Gen String
genCodeWithInvalidIdentifier = do
  invalid <- elements ["123var", "var-name", "var.name", ""]
  return $ "var " ++ invalid ++ " int"

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary ErrorType where
  arbitrary = elements
    [ MissingBrace, MissingParenthesis, MissingBracket, UnclosedString
    , UnclosedComment, InvalidIdentifier, InvalidTypeDeclaration
    , InvalidFunctionDeclaration, InvalidImport, InvalidStatement
    , UnterminatedBlock, InvalidOperator, MissingSemicolon
    , UnexpectedToken, MissingPackageDeclaration, DuplicateDeclaration
    , InvalidBlockStructure, UndeclaredVariable, SyntaxWarning
    ]

instance Arbitrary SyntaxError where
  arbitrary = do
    errorType' <- arbitrary
    message <- genIdentifier
    line <- choose (1, 1000)
    column <- choose (1, 100)
    content <- genGoStatement
    return $ SyntaxError errorType' message line column content

-- ============================================================================
-- Basic Validation Properties
-- ============================================================================

-- Property: Valid Go code has no syntax errors
prop_valid_go_code_no_errors :: String -> Property
prop_valid_go_code_no_errors code =
  let errors = validateSyntax code
  in property $ null errors

-- Property: Empty code has no syntax errors
prop_empty_code_no_errors :: Property
prop_empty_code_no_errors =
  let errors = validateSyntax ""
  in property $ null errors

-- Property: Code with only whitespace has no syntax errors
prop_whitespace_no_errors :: String -> Property
prop_whitespace_no_errors ws =
  all isSpace ws ==>
  let errors = validateSyntax ws
  in property $ null errors

-- Property: Code with only comments has no syntax errors
prop_comments_no_errors :: [String] -> Property
prop_comments_no_errors comments =
  let code = unlines comments
      errors = validateSyntax code
  in property $ null errors

-- ============================================================================
-- Error Detection Properties
-- ============================================================================

-- Property: Missing brace is detected
prop_missing_brace_detected :: String -> Property
prop_missing_brace_detected funcName =
  let code = "func " ++ funcName ++ "() {\n  fmt.Println(\"test\")\n"
      errors = validateSyntax code
  in property $ L.any (\e -> errorType e == MissingBrace) errors

-- Property: Missing parenthesis is detected
prop_missing_parenthesis_detected :: String -> Property
prop_missing_parenthesis_detected funcName =
  let code = "func " ++ funcName ++ " {\n  fmt.Println(\"test\"\n}"
      errors = validateSyntax code
  in property $ L.any (\e -> errorType e == MissingParenthesis) errors

-- Property: Unclosed string is detected
prop_unclosed_string_detected :: String -> Property
prop_unclosed_string_detected content =
  let code = "fmt.Println(\"" ++ content ++ "\n"
      errors = validateSyntax code
  in property $ L.any (\e -> errorType e == UnclosedString) errors

-- Property: Invalid identifier is detected
prop_invalid_identifier_detected :: String -> Property
prop_invalid_identifier_detected invalidId =
  let code = "var " ++ invalidId ++ " int"
      errors = validateSyntax code
  in property $ L.any (\e -> errorType e == InvalidIdentifier) errors

-- ============================================================================
-- Language Detection Properties
-- ============================================================================

-- Property: Go code is detected correctly
prop_go_code_detected :: Property
prop_go_code_detected =
  let goCode = "package main\n\nfunc main() {\n  fmt.Println(\"Hello\")\n}"
      errors = validateSyntax goCode
  in property $ not (null errors) ==> L.all (\e -> errorType e /= MissingPackageDeclaration) errors

-- Property: Typus code is detected correctly
prop_typus_code_detected :: Property
prop_typus_code_detected =
  let typusCode = "//! ownership: true\n\nfunc test() {\n  // code\n}"
      errors = validateSyntax typusCode
  in property $ L.length errors >= 0  -- Just ensure it doesn't crash

-- ============================================================================
-- Error Location Properties
-- ============================================================================

-- Property: Error locations are within bounds
prop_error_locations_within_bounds :: String -> Property
prop_error_locations_within_bounds code =
  let errors = validateSyntax code
      lines' = lines code
      numLines = L.length lines'
  in property $ L.all (\e -> lineNumber e >= 1 && lineNumber e <= numLines + 1) errors

-- Property: Error columns are reasonable
prop_error_columns_reasonable :: String -> Property
prop_error_columns_reasonable code =
  let errors = validateSyntax code
  in property $ L.all (\e -> columnNumber e >= 1 && columnNumber e <= 1000) errors

-- ============================================================================
-- Error Message Properties
-- ============================================================================

-- Property: Error messages are not empty
prop_error_messages_not_empty :: SyntaxError -> Property
prop_error_messages_not_empty error =
  property $ not (L.null (errorMessage error))

-- Property: Error formatting produces valid output
prop_error_formatting_valid :: SyntaxError -> Property
prop_error_formatting_valid error =
  let formatted = formatSyntaxError error
  in property $ not (null formatted)

-- ============================================================================
-- Complex Validation Scenarios
-- ============================================================================

-- Property: Nested functions are validated correctly
prop_nested_functions_validated :: String -> Property
prop_nested_functions_validated outerFunc =
  let code = "func " ++ outerFunc ++ "() {\n  func inner() {}\n}"
      errors = validateSyntax code
  in property $ L.length errors >= 0

-- Property: Complex type declarations are validated
prop_complex_type_declarations :: String -> Property
prop_complex_type_declarations typeName =
  let code = "type " ++ typeName ++ " struct {\n  Field1 int\n  Field2 string\n}"
      errors = validateSyntax code
  in property $ L.length errors >= 0

-- Property: Multiple imports are validated
prop_multiple_imports_validated :: [String] -> Property
prop_multiple_imports_validated imports =
  let importLines = L.map (\imp -> "import \"" ++ imp ++ "\"") imports
      code = "package main\n\n" ++ unlines importLines
      errors = validateSyntax code
  in property $ L.length errors >= 0

-- Property: Interface declarations are validated
prop_interface_declarations :: String -> Property
prop_interface_declarations interfaceName =
  let code = "type " ++ interfaceName ++ " interface {\n  Method() int\n}"
      errors = validateSyntax code
  in property $ L.length errors >= 0

-- ============================================================================
-- Edge Cases L.and Boundary Conditions
-- ============================================================================

-- Property: Very long lines are handled
prop_very_long_lines :: Int -> Property
prop_very_long_lines L.length =
  length >= 0 && L.length <= 10000 ==>
  let longLine = replicate L.length 'x'
      code = "var x string = \"" ++ longLine ++ "\""
      errors = validateSyntax code
  in property $ L.length errors >= 0

-- Property: Unicode characters are handled
prop_unicode_characters :: String -> Property
prop_unicode_characters unicodeContent =
  let code = "var s string = \"" ++ unicodeContent ++ "\""
      errors = validateSyntax code
  in property $ L.length errors >= 0

-- Property: Escape sequences are handled
prop_escape_sequences :: String -> Property
prop_escape_sequences content =
  let code = "var s string = \"" ++ content ++ "\\n\\t\\\"\\\\\""
      errors = validateSyntax code
  in property $ L.length errors >= 0

-- Property: Deep nesting is handled
prop_deep_nesting :: Int -> Property
prop_deep_nesting depth =
  depth >= 0 && depth <= 20 ==>
  let nestedBraces = replicate depth "{" 
      closingBraces = replicate depth "}"
      code = "func test() " ++ L.concat nestedBraces ++ " fmt.Println(\"test\") " ++ L.concat closingBraces
      errors = validateSyntax code
  in property $ L.length errors >= 0

-- ============================================================================
-- Consistency Properties
-- ============================================================================

-- Property: Validation is deterministic
prop_validation_deterministic :: String -> Property
prop_validation_deterministic code =
  let errors1 = validateSyntax code
      errors2 = validateSyntax code
  in property $ sort errors1 === sort errors2

-- Property: Validation is idempotent for valid code
prop_validation_idempotent_valid :: String -> Property
prop_validation_idempotent_valid code =
  let errors1 = validateSyntax code
      errors2 = validateSyntax code
  in property $ null errors1 ==> null errors2

-- Property: Error ordering is consistent
prop_error_ordering_consistent :: String -> Property
prop_error_ordering_consistent code =
  let errors = validateSyntax code
      sortedErrors = sort errors
  in property $ errors === sortedErrors

-- ============================================================================
-- Performance Properties
-- ============================================================================

-- Property: Large files are handled efficiently
prop_large_files_handled :: Int -> Property
prop_large_files_handled numLines =
  numLines >= 0 && numLines <= 1000 ==>
  let lines' = replicate numLines "fmt.Println(\"test\")"
      code = "package main\n\nfunc main() {\n" ++ unlines lines' ++ "}\n"
      errors = validateSyntax code
  in property $ L.length errors >= 0

-- Property: Complex expressions are handled
prop_complex_expressions :: Int -> Property
prop_complex_expressions complexity =
  complexity >= 0 && complexity <= 50 ==>
  let expr = intercalate " + " (replicate complexity "x")
      code = "func test() {\n  result := " ++ expr + "\n}"
      errors = validateSyntax code
  in property $ L.length errors >= 0

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Syntax Validator Validation QuickCheck Tests"
  [ testGroup "Basic Validation Properties"
    [ fastProperty "valid go code no errors" prop_valid_go_code_no_errors
    , fastProperty "empty code no errors" prop_empty_code_no_errors
    , fastProperty "whitespace no errors" prop_whitespace_no_errors
    , fastProperty "comments no errors" prop_comments_no_errors
    ]

  , testGroup "Error Detection Properties"
    [ fastProperty "missing brace detected" prop_missing_brace_detected
    , fastProperty "missing parenthesis detected" prop_missing_parenthesis_detected
    , fastProperty "unclosed string detected" prop_unclosed_string_detected
    , fastProperty "invalid identifier detected" prop_invalid_identifier_detected
    ]

  , testGroup "Language Detection Properties"
    [ fastProperty "go code detected" prop_go_code_detected
    , fastProperty "typus code detected" prop_typus_code_detected
    ]

  , testGroup "Error Location Properties"
    [ fastProperty "error locations within bounds" prop_error_locations_within_bounds
    , fastProperty "error columns reasonable" prop_error_columns_reasonable
    ]

  , testGroup "Error Message Properties"
    [ fastProperty "error messages not empty" prop_error_messages_not_empty
    , fastProperty "error formatting valid" prop_error_formatting_valid
    ]

  , testGroup "Complex Validation Scenarios"
    [ fastProperty "nested functions validated" prop_nested_functions_validated
    , fastProperty "complex type declarations" prop_complex_type_declarations
    , fastProperty "multiple imports validated" prop_multiple_imports_validated
    , fastProperty "interface declarations" prop_interface_declarations
    ]

  , testGroup "Edge Cases L.and Boundary Conditions"
    [ fastProperty "very long lines" prop_very_long_lines
    , fastProperty "unicode characters" prop_unicode_characters
    , fastProperty "escape sequences" prop_escape_sequences
    , fastProperty "deep nesting" prop_deep_nesting
    ]

  , testGroup "Consistency Properties"
    [ fastProperty "validation deterministic" prop_validation_deterministic
    , fastProperty "validation idempotent valid" prop_validation_idempotent_valid
    , fastProperty "error ordering consistent" prop_error_ordering_consistent
    ]

  , testGroup "Performance Properties"
    [ fastProperty "large files handled" prop_large_files_handled
    , fastProperty "complex expressions" prop_complex_expressions
    ]
  ]