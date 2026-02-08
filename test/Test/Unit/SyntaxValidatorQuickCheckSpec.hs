{-# LANGUAGE DeriveGeneric #-}
module Test.Unit.SyntaxValidatorQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import GHC.Generics (Generic)
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Set as Set

import SyntaxValidator
import SourceLocation (SourcePos(..))

-- Test data generators
generateErrorType :: Int -> ErrorType
generateErrorType n = case n `mod` 16 of
  0 -> MissingBrace
  1 -> MissingParenthesis
  2 -> MissingBracket
  3 -> UnclosedString
  4 -> UnclosedComment
  5 -> InvalidIdentifier
  6 -> InvalidTypeDeclaration
  7 -> InvalidFunctionDeclaration
  8 -> InvalidImport
  9 -> InvalidStatement
  10 -> UnterminatedBlock
  11 -> InvalidOperator
  12 -> MissingSemicolon
  13 -> UnexpectedToken
  14 -> MissingPackageDeclaration
  15 -> DuplicateDeclaration

generateSyntaxError :: Int -> SyntaxError
generateSyntaxError n = SyntaxError
    { errorType = generateErrorType n
    , errorMessage = "Test error " ++ show n
    , lineNumber = n `mod` 100 + 1
    , columnNumber = n `mod` 80 + 1
    , lineContent = "Line content " ++ show n
    }

generateLanguage :: Int -> Language
generateLanguage n = case n `mod` 4 of
  0 -> Go
  1 -> Typus
  2 -> GoAndTypus
  3 -> Unknown

generateToken :: Int -> Token
generateToken n = case n `mod` 9 of
  0 -> TString "test" (n `mod` 100 + 1) (n `mod` 80 + 1)
  1 -> TComment "// test comment" (n `mod` 100 + 1) (n `mod` 80 + 1)
  2 -> TIdentifier "testVar" (n `mod` 100 + 1) (n `mod` 80 + 1)
  3 -> TKeyword "func" (n `mod` 100 + 1) (n `mod` 80 + 1)
  4 -> TOperator "+" (n `mod` 100 + 1) (n `mod` 80 + 1)
  5 -> TDelimiter '{' (n `mod` 100 + 1) (n `mod` 80 + 1)
  6 -> TNumber "42" (n `mod` 100 + 1) (n `mod` 80 + 1)
  7 -> TWhitespace (n `mod` 100 + 1) (n `mod` 80 + 1)
  8 -> TNewline (n `mod` 100 + 1)

generateScope :: Int -> Scope
generateScope n = Scope
    { scopeName = "scope" ++ show n
    , scopeVariables = Set.fromList ["var" ++ show i | i <- [1..n `mod` 5]]
    , scopeFunctions = Set.fromList ["func" ++ show i | i <- [1..n `mod` 3]]
    , parentScope = if n `mod` 2 == 0 then Nothing else Just $ generateScope (n - 1)
    }

generateGoCode :: Int -> String
generateGoCode n = case n `mod` 6 of
  0 -> "package main\n\nfunc main() {\n    fmt.Println(\"Hello\")\n}"
  1 -> "package main\n\nimport \"fmt\"\n\nfunc add(a, b int) int {\n    return a + b\n}"
  2 -> "package main\n\nvar x int = 42\n\nfunc main() {\n    println(x)\n}"
  3 -> "package main\n\ntype Person struct {\n    Name string\n    Age  int\n}"
  4 -> "package main\n\nimport (\n    \"fmt\"\n    \"os\"\n)\n\nfunc main() {\n    fmt.Println(os.Args)\n}"
  5 -> "package main\n\nconst Pi = 3.14159\n\nfunc main() {\n    fmt.Printf(\"Pi = %f\\n\", Pi)\n}"

generateTypusCode :: Int -> String
generateTypusCode n = case n `mod` 4 of
  0 -> "//! ownership: true\n\nlet x = 42\n"
  1 -> "{//! dependent_types: true}\nfunc test() {\n    return 42\n}"
  2 -> "//! constraints: true\n\nlet y = x + 1\n"
  3 -> "//! file_directive\n\nfunc main() {\n    return\n}"

generateInvalidCode :: Int -> String
generateInvalidCode n = case n `mod` 6 of
  0 -> "func missingBrace() {\n    return 42"  -- Missing closing brace
  1 -> "func invalidSyntax(\n    return 42\n}"  -- Missing closing parenthesis
  2 -> "let x ="  -- Incomplete let declaration
  3 -> "func test() {\n    if x > 0\n        return x\n}"  -- Missing brace after if
  4 -> "package main\n\nfunc duplicate() {}\nfunc duplicate() {}"  -- Duplicate function
  5 -> "unclosed string \"test"  -- Unclosed string

-- QuickCheck properties
prop_syntax_error_creation :: Property
prop_syntax_error_creation =
  forAll arbitrary $ \n ->
    let error = generateSyntaxError n
        expectedType = generateErrorType n
        expectedLine = n `mod` 100 + 1
        expectedCol = n `mod` 80 + 1
    in property $
      errorType error == expectedType &&
      errorMessage error == "Test error " ++ show n &&
      lineNumber error == expectedLine &&
      columnNumber error == expectedCol &&
      lineContent error == "Line content " ++ show n

prop_syntax_error_ordering :: Property
prop_syntax_error_ordering =
  forAll arbitrary $ \n1 ->
  forAll arbitrary $ \n2 ->
    let error1 = generateSyntaxError n1
        error2 = generateSyntaxError n2
        ordering = compare error1 error2
    in property $ 
      (ordering == EQ) == (error1 == error2)

prop_new_syntax_validator :: Property
prop_new_syntax_validator =
  let validator = newSyntaxValidator
  in property $
    null (validatorErrors validator) &&
    scopeName (currentScope validator) == "global" &&
    null (scopeStack validator) &&
    null (braceStack validator) &&
    language validator == Unknown &&
    null (tokens validator) &&
    not (hasPackageDecl validator) &&
    not (hasMainFunc validator)

prop_detect_language_go :: Property
prop_detect_language_go =
  forAll arbitrary $ \n ->
    let code = generateGoCode n
        detectedLang = detectLanguage code
    in property $ detectedLang == Go

prop_detect_language_typus :: Property
prop_detect_language_typus =
  forAll arbitrary $ \n ->
    let code = generateTypusCode n
        detectedLang = detectLanguage code
    in property $ detectedLang == Typus

prop_detect_language_go_and_typus :: Property
prop_detect_language_go_and_typus =
  let code = "//! ownership: true\n\npackage main\n\nfunc main() {}"
      detectedLang = detectLanguage code
  in property $ detectedLang == GoAndTypus

prop_detect_language_unknown :: Property
prop_detect_language_unknown =
  let code = "Just some random text without Go or Typus markers"
      detectedLang = detectLanguage code
  in property $ detectedLang == Unknown

prop_validate_syntax_valid_go :: Property
prop_validate_syntax_valid_go =
  forAll arbitrary $ \n ->
    let code = generateGoCode n
        errors = validateSyntax code
    in property $ null errors  -- Valid Go code should have no errors

prop_validate_syntax_invalid_code :: Property
prop_validate_syntax_invalid_code =
  forAll arbitrary $ \n ->
    let code = generateInvalidCode n
        errors = validateSyntax code
    in property $ not (null errors)  -- Invalid code should have errors

prop_validate_syntax_empty :: Property
prop_validate_syntax_empty =
  let code = ""
      errors = validateSyntax code
  in property $ null errors  -- Empty code should have no errors

prop_validate_syntax_whitespace :: Property
prop_validate_syntax_whitespace =
  let code = "   \n  \n   \n"
      errors = validateSyntax code
  in property $ null errors  -- Whitespace only should have no errors

prop_tokenize_simple :: Property
prop_tokenize_simple =
  let code = "func main() {}"
      tokens = tokenize code
  in property $ length tokens >= 3  -- func, main, (), {}

prop_tokenize_with_comments :: Property
prop_tokenize_with_comments =
  let code = "// This is a comment\nfunc main() {}"
      tokens = tokenize code
      hasComment = any isCommentToken tokens
  in property $ hasComment
  where
    isCommentToken (TComment _ _ _) = True
    isCommentToken _ = False

prop_tokenize_with_strings :: Property
prop_tokenize_with_strings =
  let code = "func main() {\n    fmt.Println(\"Hello, world!\")\n}"
      tokens = tokenize code
      hasString = any isStringToken tokens
  in property $ hasString
  where
    isStringToken (TString _ _ _) = True
    isStringToken _ = False

prop_format_syntax_error :: Property
prop_format_syntax_error =
  forAll arbitrary $ \n ->
    let error = generateSyntaxError n
        formatted = formatSyntaxError error
    in property $
      show (errorType error) `isInfixOf` formatted &&
      errorMessage error `isInfixOf` formatted &&
      "Line " ++ show (lineNumber error) `isInfixOf` formatted &&
      ":" ++ show (columnNumber error) `isInfixOf` formatted

prop_get_syntax_errors :: Property
prop_get_syntax_errors =
  forAll arbitrary $ \n ->
    let errors = take (n `mod` 5) [generateSyntaxError i | i <- [1..10]]
        validator = newSyntaxValidator { validatorErrors = reverse errors }
        retrievedErrors = getSyntaxErrors validator
    in property $ retrievedErrors == errors

prop_validate_file :: Property
prop_validate_file =
  forAll arbitrary $ \n ->
    let code = generateGoCode n
        errors = validateFile code
    in property $ null errors  -- Valid Go code should have no errors

prop_scope_creation :: Property
prop_scope_creation =
  forAll arbitrary $ \n ->
    let scope = generateScope n
    in property $
      scopeName scope == "scope" ++ show n &&
      not (null $ scopeVariables scope) &&
      not (null $ scopeFunctions scope)

prop_token_creation :: Property
prop_token_creation =
  forAll arbitrary $ \n ->
    let token = generateToken n
    in property $ True  -- Just test that token creation doesn't crash

prop_language_creation :: Property
prop_language_creation =
  forAll arbitrary $ \n ->
    let lang = generateLanguage n
    in property $ True  -- Just test that language creation doesn't crash

-- Test suite
testSuite :: TestTree
testSuite = testGroup "SyntaxValidator QuickCheck Tests"
  [ testProperty "syntax error creation" prop_syntax_error_creation
  , testProperty "syntax error ordering" prop_syntax_error_ordering
  , testProperty "new syntax validator" prop_new_syntax_validator
  , testProperty "detect language go" prop_detect_language_go
  , testProperty "detect language typus" prop_detect_language_typus
  , testProperty "detect language go and typus" prop_detect_language_go_and_typus
  , testProperty "detect language unknown" prop_detect_language_unknown
  , testProperty "validate syntax valid go" prop_validate_syntax_valid_go
  , testProperty "validate syntax invalid code" prop_validate_syntax_invalid_code
  , testProperty "validate syntax empty" prop_validate_syntax_empty
  , testProperty "validate syntax whitespace" prop_validate_syntax_whitespace
  , testProperty "tokenize simple" prop_tokenize_simple
  , testProperty "tokenize with comments" prop_tokenize_with_comments
  , testProperty "tokenize with strings" prop_tokenize_with_strings
  , testProperty "format syntax error" prop_format_syntax_error
  , testProperty "get syntax errors" prop_get_syntax_errors
  , testProperty "validate file" prop_validate_file
  , testProperty "scope creation" prop_scope_creation
  , testProperty "token creation" prop_token_creation
  , testProperty "language creation" prop_language_creation
  ]

-- Unit tests for specific edge cases
unitTests :: TestTree
unitTests = testGroup "SyntaxValidator Unit Tests"
  [ testCase "missing package declaration" $ do
      let code = "func main() {}"
          errors = validateSyntax code
          hasPackageError = any (\e -> errorType e == MissingPackageDeclaration) errors
      assertBool "Should detect missing package declaration" hasPackageError

  , testCase "missing closing brace" $ do
      let code = "func test() {\n    return 42"
          errors = validateSyntax code
          hasBraceError = any (\e -> errorType e == MissingBrace) errors
      assertBool "Should detect missing closing brace" hasBraceError

  , testCase "incomplete let declaration" $ do
      let code = "let x ="
          errors = validateSyntax code
          hasStatementError = any (\e -> errorType e == InvalidStatement) errors
      assertBool "Should detect incomplete let declaration" hasStatementError

  , testCase "duplicate function declaration" $ do
      let code = "package main\n\nfunc duplicate() {}\nfunc duplicate() {}"
          errors = validateSyntax code
          hasDuplicateError = any (\e -> errorType e == DuplicateDeclaration) errors
      assertBool "Should detect duplicate function declaration" hasDuplicateError

  , testCase "invalid import" $ do
      let code = "package main\n\nimport"
          errors = validateSyntax code
          hasImportError = any (\e -> errorType e == InvalidImport) errors
      assertBool "Should detect invalid import" hasImportError

  , testCase "typus directive validation" $ do
      let validDirective = "//! ownership: true"
          invalidDirective = "//! invalid directive format"
          validErrors = validateSyntax validDirective
          invalidErrors = validateSyntax invalidDirective
      assertBool "Valid directive should not produce errors" $ null validErrors
      assertBool "Invalid directive should produce errors" $ not (null invalidErrors)

  , testCase "format syntax error with context" $ do
      let error = SyntaxError MissingBrace "Missing closing brace" 10 5 "func test() {"
          formatted = formatSyntaxError error
      assertBool "Should include line number" $ "Line 10:5" `isInfixOf` formatted
      assertBool "Should include error type" $ "MissingBrace" `isInfixOf` formatted
      assertBool "Should include error message" $ "Missing closing brace" `isInfixOf` formatted
      assertBool "Should include line content" $ "func test() {" `isInfixOf` formatted
  ]

-- Combined test suite
tests :: TestTree
tests = testGroup "SyntaxValidator Tests"
  [ testSuite
  , unitTests
  ]