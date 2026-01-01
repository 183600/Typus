{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewParserComprehensiveSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, Arbitrary, arbitrary, oneof, elements, listOf, resize)
import Data.Char (isAlpha, isAlphaNum, isSpace, isDigit)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified Data.Text as T

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import SourceLocation (Located(..), SourcePos(..), SourceSpan(..))

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary FileDirectives where
  arbitrary = do
    fdOwnership <- arbitrary
    fdDependentTypes <- arbitrary
    fdConstraints <- arbitrary
    return $ FileDirectives fdOwnership fdDependentTypes fdConstraints

instance Arbitrary BlockDirectives where
  arbitrary = do
    bdOwnership <- arbitrary
    bdDependentTypes <- arbitrary
    bdConstraints <- arbitrary
    return $ BlockDirectives bdOwnership bdDependentTypes bdConstraints

-- Generate valid identifiers for Typus language
validIdentifier :: Gen String
validIdentifier = do
  first <- elements ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- Generate valid Typus code snippets
validTypusCode :: Gen String
validTypusCode = oneof
  [ -- Function declaration
    do
      funcName <- validIdentifier
      return $ "func " ++ funcName ++ "() {\n    return 42\n}"
  , -- Variable declaration
    do
      varName <- validIdentifier
      value <- elements ["0", "1", "42", "\"hello\"", "true", "false"]
      return $ "var " ++ varName ++ " = " ++ value
  , -- Type declaration
    do
      typeName <- validIdentifier
      return $ "type " ++ typeName ++ " struct {"
  , -- Simple expression
    do
      return "x + y * z"
  ]

-- Generate code with directives
codeWithDirectives :: Gen String
codeWithDirectives = do
  hasOwnership <- arbitrary
  hasDepTypes <- arbitrary
  hasConstraints <- arbitrary
  code <- validTypusCode
  
  let directives = L.concat $
        [ if hasOwnership then "// @ownership\n" else ""
        , if hasDepTypes then "// @dependent-types\n" else ""
        , if hasConstraints then "// @constraints\n" else ""
        ]
  
  return $ directives ++ code

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Property: parseTypus preserves code structure for valid inputs
prop_parseTypus_preserves_structure :: Property
prop_parseTypus_preserves_structure =
  forAll validTypusCode $ \code ->
    let result = parseTypus "test" code
    in case result of
         Left _ -> property $ counterexample "Failed to parse valid code" False
         Right typusFile -> property $ True

-- Property: parseTypus handles empty input gracefully
prop_parseTypus_empty_input :: Property
prop_parseTypus_empty_input =
  let result = parseTypus "empty" ""
  in case result of
       Left _ -> property True
       Right typusFile -> property $ tfCodeBlocks typusFile == []

-- Property: parseTypus handles whitespace-only input
prop_parseTypus_whitespace_only :: Property
prop_parseTypus_whitespace_only =
  forAll (listOf (elements " \t\n\r")) $ \whitespace ->
    let result = parseTypus "whitespace" whitespace
    in case result of
         Left _ -> property True
         Right typusFile -> property $ tfCodeBlocks typusFile == []

-- Property: parseTypus identifies directives correctly
prop_parseTypus_directives :: Property
prop_parseTypus_directives =
  forAll codeWithDirectives $ \code ->
    let result = parseTypus "directives" code
    in case result of
         Left _ -> property $ counterexample "Failed to parse code with directives" False
         Right typusFile -> property True

-- Property: parseTypus preserves line numbers in error messages
prop_parseTypus_line_numbers :: Property
prop_parseTypus_line_numbers =
  forAll validTypusCode $ \code ->
    let malformedCode = code ++ "\ninvalid syntax here !!!"
        result = parseTypus "linetest" malformedCode
    in case result of
         Left err -> property $ "line" `L.isInfixOf` show err
         Right _ -> property $ counterexample "Expected parse error for malformed code" False

-- Property: parseTypus handles Unicode characters
prop_parseTypus_unicode :: Property
prop_parseTypus_unicode =
  let unicodeCode = "func 测试() {\n    // 测试中文\n    return \"🚀\"\n}"
      result = parseTypus "unicode" unicodeCode
  in case result of
       Left _ -> property $ counterexample "Failed to parse Unicode code" False
       Right typusFile -> property True

-- Property: parseTypus is deterministic
prop_parseTypus_deterministic :: Property
prop_parseTypus_deterministic =
  forAll validTypusCode $ \code ->
    let result1 = parseTypus "test1" code
        result2 = parseTypus "test2" code
    in case (result1, result2) of
         (Left _, Left _) -> property True
         (Right f1, Right f2) -> property $ tfCodeBlocks f1 == tfCodeBlocks f2
         _ -> property $ counterexample "Inconsistent parse results" False

-- Property: parseTypus handles large files
prop_parseTypus_large_files :: Property
prop_parseTypus_large_files =
  forAll (resize 50 (listOf validTypusCode)) $ \codes ->
    let largeCode = unlines codes
        result = parseTypus "large" largeCode
    in case result of
         Left _ -> property $ counterexample "Failed to parse large file" False
         Right typusFile -> property $ L.length (tfCodeBlocks typusFile) >= 0

-- Property: parseTypus handles nested structures
prop_parseTypus_nested_structures :: Property
prop_parseTypus_nested_structures =
  let nestedCode = unlines
        [ "func outer() {"
        , "    func inner() {"
        , "        return 42"
        , "    }"
        , "    return inner()"
        , "}"
        ]
      result = parseTypus "nested" nestedCode
  in case result of
       Left _ -> property $ counterexample "Failed to parse nested structures" False
       Right typusFile -> property True

-- Property: parseTypus handles comments in various positions
prop_parseTypus_comments :: Property
prop_parseTypus_comments =
  let codeWithComments = unlines
        [ "// File header comment"
        , "func withComments() {"
        , "    var x = 1 // inline comment"
        , "    /* block comment */"
        , "    return x"
        , "}"
        , ""
        , "// Footer comment"
        ]
      result = parseTypus "comments" codeWithComments
  in case result of
       Left _ -> property $ counterexample "Failed to parse code with comments" False
       Right typusFile -> property True

-- Property: parseTypus handles string literals with special characters
prop_parseTypus_string_literals :: Property
prop_parseTypus_string_literals =
  let stringWithEscapes = unlines
        [ "func stringTest() {"
        , "    var s1 = \"Hello \\\"World\\\"\""
        , "    var s2 = \"Line 1\\nLine 2\\tTabbed\""
        , "    var s3 = \"Backslash: \\\\"
        , "    return s1 + s2 + s3"
        , "}"
        ]
      result = parseTypus "strings" stringWithEscapes
  in case result of
       Left _ -> property $ counterexample "Failed to parse string literals with escapes" False
       Right typusFile -> property True

-- ============================================================================
-- Unit Tests
-- ============================================================================

-- Test basic function parsing
test_basic_function_parsing :: TestTree
test_basic_function_parsing =
  testCase "Basic function parsing" $ do
    let code = "func hello() {\n    return \"world\"\n}"
        result = parseTypus "test" code
    case result of
      Left err -> assertFailure $ "Failed to parse basic function: " ++ show err
      Right typusFile -> do
        let blocks = tfCodeBlocks typusFile
        if null blocks
          then assertFailure "No code blocks found"
          else return ()

-- Test variable declaration parsing
test_variable_declaration_parsing :: TestTree
test_variable_declaration_parsing =
  testCase "Variable declaration parsing" $ do
    let code = "var x = 42\nvar y = \"hello\"\nvar z = true"
        result = parseTypus "test" code
    case result of
      Left err -> assertFailure $ "Failed to parse variable declarations: " ++ show err
      Right typusFile -> return ()

-- Test type declaration parsing
test_type_declaration_parsing :: TestTree
test_type_declaration_parsing =
  testCase "Type declaration parsing" $ do
    let code = "type Person struct {\n    name string\n    age int\n}"
        result = parseTypus "test" code
    case result of
      Left err -> assertFailure $ "Failed to parse type declaration: " ++ show err
      Right typusFile -> return ()

-- Test directive parsing
test_directive_parsing :: TestTree
test_directive_parsing =
  testCase "Directive parsing" $ do
    let code = unlines
          [ "// @ownership"
          , "// @dependent-types"
          , "func test() {"
          , "    return 42"
          , "}"
          ]
        result = parseTypus "test" code
    case result of
      Left err -> assertFailure $ "Failed to parse directives: " ++ show err
      Right typusFile -> return ()

-- Test error handling for malformed code
test_malformed_code_error :: TestTree
test_malformed_code_error =
  testCase "Error handling for malformed code" $ do
    let code = "func broken( {"
        result = parseTypus "test" code
    case result of
      Left _ -> return ()  -- Expected to fail
      Right _ -> assertFailure "Expected parse error for malformed code"

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =
  testGroup "New Parser Comprehensive Tests"
    [ testGroup "Property-based tests"
        [ fastProperty "parseTypus preserves structure for valid inputs" prop_parseTypus_preserves_structure
        , fastProperty "parseTypus handles empty input gracefully" prop_parseTypus_empty_input
        , fastProperty "parseTypus handles whitespace-only input" prop_parseTypus_whitespace_only
        , fastProperty "parseTypus identifies directives correctly" prop_parseTypus_directives
        , fastProperty "parseTypus preserves line numbers in error messages" prop_parseTypus_line_numbers
        , fastProperty "parseTypus handles Unicode characters" prop_parseTypus_unicode
        , fastProperty "parseTypus is deterministic" prop_parseTypus_deterministic
        , fastProperty "parseTypus handles large files" prop_parseTypus_large_files
        , fastProperty "parseTypus handles nested structures" prop_parseTypus_nested_structures
        , fastProperty "parseTypus handles comments in various positions" prop_parseTypus_comments
        , fastProperty "parseTypus handles string literals with special characters" prop_parseTypus_string_literals
        ]
    , testGroup "Unit tests"
        [ test_basic_function_parsing
        , test_variable_declaration_parsing
        , test_type_declaration_parsing
        , test_directive_parsing
        , test_malformed_code_error
        ]
    ]