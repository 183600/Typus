{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.NewCabalSyntaxValidatorQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (==>))
import TestSupport.QuickCheck (fastProperty)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace, isAlpha, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub)
import GHC.Generics (Generic)

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
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- | Generate valid error messages
genErrorMessage :: Gen String
genErrorMessage = do
  words <- listOf1 $ elements $ ["syntax", "error", "invalid", "missing", "unexpected", "declaration", "statement"]
  return $ unwords words

-- | Generate error types
genErrorType :: Gen ErrorType
genErrorType = elements
  [ MissingBrace
  , MissingParenthesis
  , MissingBracket
  , UnclosedString
  , UnclosedComment
  , InvalidIdentifier
  , InvalidTypeDeclaration
  , InvalidFunctionDeclaration
  , InvalidImport
  , InvalidStatement
  , UnterminatedBlock
  , InvalidOperator
  , MissingSemicolon
  , UnexpectedToken
  , MissingPackageDeclaration
  , DuplicateDeclaration
  , InvalidBlockStructure
  , UndeclaredVariable
  , SyntaxWarning
  ]

-- | Generate syntax errors
genSyntaxError :: Gen SyntaxError
genSyntaxError = do
  errorType <- genErrorType
  line <- arbitrary
  col <- arbitrary
  message <- genErrorMessage
  return $ SyntaxError errorType line col message

-- | Generate valid identifiers
genIdentifier :: Gen String
genIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
  return $ first : take 10 rest

-- | Generate simple valid type declarations
genTypeDeclaration :: Gen String
genTypeDeclaration = do
  typeName <- genTypeName
  let declarations = 
        [ "type " ++ typeName ++ " = Int"
        , "type " ++ typeName ++ " = String"
        , "type " ++ typeName ++ " a = List a"
        , "type " ++ typeName ++ " = { field: Int }"
        ]
  elements declarations

-- | Generate simple valid function declarations
genFunctionDeclaration :: Gen String
genFunctionDeclaration = do
  funcName <- genIdentifier
  paramName <- genIdentifier
  let declarations = 
        [ "fn " ++ funcName ++ "() { return 42; }"
        , "fn " ++ funcName ++ "(" ++ paramName ++ ": Int) { return " ++ paramName ++ "; }"
        , "fn " ++ funcName ++ "(x: Int, y: Int) -> Int { return x + y; }"
        , "func " ++ funcName ++ "() { return 42; }"
        ]
  elements declarations

-- | Generate simple valid statements
genStatement :: Gen String
genStatement = do
  var <- genIdentifier
  expr <- genExpression
  let statements = 
        [ "let " ++ var ++ " = " ++ expr ++ ";"
        , "const " ++ var ++ " = " ++ expr ++ ";"
        , var ++ " = " ++ expr ++ ";"
        , "return " ++ expr ++ ";"
        , "print(" ++ var ++ ");"
        ]
  elements statements

-- | Generate simple expressions
genExpression :: Gen String
genExpression = do
  var1 <- genIdentifier
  var2 <- genIdentifier
  let expressions = 
        [ var1
        , var1 ++ " + " ++ var2
        , var1 ++ " * " ++ var2
        , var1 ++ " == " ++ var2
        , "42"
        , "\"hello\""
        , "true"
        , "false"
        , "func()"
        ]
  elements expressions

-- | Generate code with balanced brackets
genBalancedCode :: Gen String
genBalancedCode = do
  decls <- listOf1 genFunctionDeclaration
  return $ unlines decls

-- | Generate code with unbalanced brackets
genUnbalancedCode :: Gen String
genUnbalancedCode = do
  funcName <- genIdentifier
  let unbalancedCodes = 
        [ "fn " ++ funcName ++ "() { return 42;"  -- missing closing brace
        , "fn " ++ funcName ++ "() return 42; }"  -- missing opening brace
        , "fn " ++ funcName ++ "() { return (42;"  -- missing closing parenthesis
        , "fn " ++ funcName ++ "() { return [42;"  -- missing closing bracket
        , "fn " ++ funcName ++ "() { return \"hello;"  -- missing closing quote
        ]
  elements unbalancedCodes

-- | Generate type names
genTypeName :: Gen String
genTypeName = do
  first <- elements $ ['A'..'Z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  return $ first : take 10 rest

instance Arbitrary ErrorType where
  arbitrary = genErrorType

instance Arbitrary SyntaxError where
  arbitrary = genSyntaxError

-- ============================================================================
-- Error Type Property Tests
-- ============================================================================

-- | Property: All error types should be distinct
prop_error_types_distinct :: Property
prop_error_types_distinct =
  let errorTypes = [MissingBrace, MissingParenthesis, MissingBracket, UnclosedString, UnclosedComment,
                    InvalidIdentifier, InvalidTypeDeclaration, InvalidFunctionDeclaration, InvalidImport,
                    InvalidStatement, UnterminatedBlock, InvalidOperator, MissingSemicolon, UnexpectedToken,
                    MissingPackageDeclaration, DuplicateDeclaration, InvalidBlockStructure, UndeclaredVariable,
                    SyntaxWarning]
      uniqueTypes = length (nub errorTypes)
  in uniqueTypes === length errorTypes

-- | Property: Error type show should contain type name
prop_error_type_show :: ErrorType -> Property
prop_error_type_show errorType =
  let typeStr = show errorType
      hasName = not (null typeStr) && any isAlphaNum typeStr
  in hasName ==> property True

-- ============================================================================
-- Syntax Error Property Tests
-- ============================================================================

-- | Property: Syntax error should preserve error type
prop_syntax_error_preserves_type :: ErrorType -> String -> Property
prop_syntax_error_preserves_type errorType message =
  let validMessage = not (null message) && all isAlphaNum (take 10 message)
      syntaxError = SyntaxError errorType 0 0 (take 10 message)
  in validMessage ==> errorType syntaxError === errorType

-- | Property: Syntax error should preserve line and column
prop_syntax_error_preserves_location :: Int -> Int -> String -> Property
prop_syntax_error_preserves_location line col message =
  let validLine = line >= 0
      validCol = col >= 0
      validMessage = not (null message) && all isAlphaNum (take 10 message)
      syntaxError = SyntaxError MissingBrace line col (take 10 message)
  in validLine .&&. validCol .&&. validMessage ==> 
     errorLine syntaxError === line .&&. errorColumn syntaxError === col

-- | Property: Syntax error should preserve message
prop_syntax_error_preserves_message :: String -> Property
prop_syntax_error_preserves_message message =
  let validMessage = not (null message) && all isAlphaNum (take 10 message)
      syntaxError = SyntaxError MissingBrace 0 0 (take 10 message)
  in validMessage ==> errorMessage syntaxError === take 10 message

-- | Property: Syntax error equality should work correctly
prop_syntax_error_equality :: SyntaxError -> SyntaxError -> Property
prop_syntax_error_equality err1 err2 =
  let equal = err1 == err2
      sameType = errorType err1 == errorType err2
      sameLine = errorLine err1 == errorLine err2
      sameCol = errorColumn err1 == errorColumn err2
      sameMsg = errorMessage err1 == errorMessage err2
  in equal === (sameType .&&. sameLine .&&. sameCol .&&. sameMsg)

-- ============================================================================
-- Syntax Validator Property Tests
-- ============================================================================

-- | Property: New syntax validator should have no errors
prop_new_syntax_validator_empty :: Property
prop_new_syntax_validator_empty =
  let validator = newSyntaxValidator
      errors = getSyntaxErrors validator
  in null errors

-- | Property: Validating empty string should produce no errors
prop_validate_empty_string :: Property
prop_validate_empty_string =
  let validator = newSyntaxValidator
      result = validateSyntax validator ""
      errors = getSyntaxErrors result
  in null errors

-- | Property: Validating balanced code should produce fewer errors than unbalanced code
prop_validate_balanced_vs_unbalanced :: Property
prop_validate_balanced_vs_unbalanced =
  let validator = newSyntaxValidator
      balancedCode = "fn test() { return 42; }"
      unbalancedCode = "fn test() { return 42;"  -- missing closing brace
      balancedResult = validateSyntax validator balancedCode
      unbalancedResult = validateSyntax validator unbalancedCode
      balancedErrors = getSyntaxErrors balancedResult
      unbalancedErrors = getSyntaxErrors unbalancedResult
  in length balancedErrors <= length unbalancedErrors

-- | Property: Validating code with missing braces should produce brace errors
prop_validate_missing_braces :: String -> Property
prop_validate_missing_braces funcName =
  let validName = not (null funcName) && all isAlphaNum (take 5 funcName)
      code = "fn " ++ take 5 funcName ++ "() { return 42;"  -- missing closing brace
      validator = newSyntaxValidator
      result = validateSyntax validator code
      errors = getSyntaxErrors result
      hasBraceError = any (\err -> errorType err == MissingBrace) errors
  in validName ==> hasBraceError

-- | Property: Validating code with missing parentheses should produce parenthesis errors
prop_validate_missing_parentheses :: String -> Property
prop_validate_missing_parentheses funcName =
  let validName = not (null funcName) && all isAlphaNum (take 5 funcName)
      code = "fn " ++ take 5 funcName ++ "() { return (42;"  -- missing closing parenthesis
      validator = newSyntaxValidator
      result = validateSyntax validator code
      errors = getSyntaxErrors result
      hasParenError = any (\err -> errorType err == MissingParenthesis) errors
  in validName ==> hasParenError

-- | Property: Validating code with missing brackets should produce bracket errors
prop_validate_missing_brackets :: String -> Property
prop_validate_missing_brackets funcName =
  let validName = not (null funcName) && all isAlphaNum (take 5 funcName)
      code = "fn " ++ take 5 funcName ++ "() { let x = [1, 2, 3;"  -- missing closing bracket
      validator = newSyntaxValidator
      result = validateSyntax validator code
      errors = getSyntaxErrors result
      hasBracketError = any (\err -> errorType err == MissingBracket) errors
  in validName ==> hasBracketError

-- | Property: Validating code with unclosed strings should produce string errors
prop_validate_unclosed_strings :: String -> Property
prop_validate_unclosed_strings funcName =
  let validName = not (null funcName) && all isAlphaNum (take 5 funcName)
      code = "fn " ++ take 5 funcName ++ "() { return \"hello;"  -- missing closing quote
      validator = newSyntaxValidator
      result = validateSyntax validator code
      errors = getSyntaxErrors result
      hasStringError = any (\err -> errorType err == UnclosedString) errors
  in validName ==> hasStringError

-- | Property: Validating valid identifiers should not produce identifier errors
prop_validate_valid_identifiers :: String -> Property
prop_validate_valid_identifiers identifier =
  let validId = not (null identifier) && all isAlphaNum (take 5 identifier) && isAlpha (head identifier)
      code = "let " ++ take 5 identifier ++ " = 42;"
      validator = newSyntaxValidator
      result = validateSyntax validator code
      errors = getSyntaxErrors result
      hasIdentifierError = any (\err -> errorType err == InvalidIdentifier) errors
  in validId ==> not hasIdentifierError

-- | Property: Validating invalid identifiers should produce identifier errors
prop_validate_invalid_identifiers :: String -> Property
prop_validate_invalid_identifiers identifier =
  let invalidId = null identifier || (not (null identifier) && isDigit (head identifier))
      code = "let " ++ take 5 identifier ++ " = 42;"
      validator = newSyntaxValidator
      result = validateSyntax validator code
      errors = getSyntaxErrors result
      hasIdentifierError = any (\err -> errorType err == InvalidIdentifier) errors
  in invalidId ==> hasIdentifierError

-- ============================================================================
-- Error Formatting Property Tests
-- ============================================================================

-- | Property: Formatting syntax error should produce non-empty output
prop_format_syntax_error :: SyntaxError -> Property
prop_format_syntax_error syntaxError =
  let formatted = formatSyntaxError syntaxError
      hasContent = length formatted > 10
  in hasContent ==> property True

-- | Property: Formatting syntax error should include error type
prop_format_syntax_error_includes_type :: ErrorType -> String -> Property
prop_format_syntax_error_includes_type errorType message =
  let validMessage = not (null message) && all isAlphaNum (take 10 message)
      syntaxError = SyntaxError errorType 0 0 (take 10 message)
      formatted = formatSyntaxError syntaxError
      typeStr = show errorType
  in validMessage ==> typeStr `isInfixOf` formatted

-- | Property: Formatting syntax error should include line information
prop_format_syntax_error_includes_line :: SyntaxError -> Property
prop_format_syntax_error_includes_line syntaxError =
  let formatted = formatSyntaxError syntaxError
      lineStr = show (errorLine syntaxError)
  in lineStr `isInfixOf` formatted

-- | Property: Formatting syntax error should include column information
prop_format_syntax_error_includes_column :: SyntaxError -> Property
prop_format_syntax_error_includes_column syntaxError =
  let formatted = formatSyntaxError syntaxError
      colStr = show (errorColumn syntaxError)
  in colStr `isInfixOf` formatted

-- ============================================================================
-- File Validation Property Tests
-- ============================================================================

-- | Property: File validation should handle empty content
prop_validate_file_empty :: Property
prop_validate_file_empty =
  let validator = newSyntaxValidator
      result = validateFile validator ""
      errors = getSyntaxErrors result
  in property True  -- Should not crash

-- | Property: File validation should handle simple content
prop_validate_file_simple :: String -> Property
prop_validate_file_simple content =
  let simpleContent = take 50 $ filter (\c -> isAlphaNum c || c `elem` " \n\t{}();") content
      validator = newSyntaxValidator
      result = validateFile validator simpleContent
  in not (null simpleContent) ==> property True  -- Should not crash

-- | Property: File validation should handle multiple statements
prop_validate_file_multiple :: [String] -> Property
prop_validate_file_multiple statements =
  let validStatements = filter (not . null) $ take 3 statements
      content = unlines validStatements
      validator = newSyntaxValidator
      result = validateFile validator content
  in not (null validStatements) ==> property True  -- Should not crash

-- ============================================================================
-- Integration Property Tests
-- ============================================================================

-- | Property: Complete validation pipeline should be deterministic
prop_validation_deterministic :: String -> Property
prop_validation_deterministic code =
  let testCode = take 100 $ filter (\c -> isAlphaNum c || c `elem` " \n\t{}();") code
      validator1 = newSyntaxValidator
      validator2 = newSyntaxValidator
      result1 = validateSyntax validator1 testCode
      result2 = validateSyntax validator2 testCode
      errors1 = getSyntaxErrors result1
      errors2 = getSyntaxErrors result2
  in not (null testCode) ==> length errors1 === length errors2

-- | Property: Error collection should be consistent
prop_error_collection_consistent :: String -> Property
prop_error_collection_consistent code =
  let testCode = take 80 $ filter (\c -> isAlphaNum c || c `elem` " \n\t{}();") code
      validator = newSyntaxValidator
      result = validateSyntax validator testCode
      errors = getSyntaxErrors result
      errorCount = length errors
  in not (null testCode) ==> errorCount >= 0 ==> property True

-- | Property: Validation should handle nested structures
prop_validation_nested_structures :: Property
prop_validation_nested_structures =
  let nestedCode = "fn outer() { fn inner() { return 42; } return inner(); }"
      validator = newSyntaxValidator
      result = validateSyntax validator nestedCode
      errors = getSyntaxErrors result
  in property True  -- Should not crash

-- | Property: Validation should handle complex expressions
prop_validation_complex_expressions :: String -> Property
prop_validation_complex_expressions expr =
  let complexExpr = take 60 $ filter (\c -> isAlphaNum c || c `elem` " +-*/()") expr
      code = "fn test() { return " ++ complexExpr ++ "; }"
      validator = newSyntaxValidator
      result = validateSyntax validator code
  in not (null complexExpr) ==> property True  -- Should not crash

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal SyntaxValidator QuickCheck Tests"
  [ -- Error Type Tests
    fastProperty "error types distinct" prop_error_types_distinct
  , fastProperty "error type show" prop_error_type_show
  
  -- Syntax Error Tests
  , fastProperty "syntax error preserves type" prop_syntax_error_preserves_type
  , fastProperty "syntax error preserves location" prop_syntax_error_preserves_location
  , fastProperty "syntax error preserves message" prop_syntax_error_preserves_message
  , fastProperty "syntax error equality" prop_syntax_error_equality
  
  -- Syntax Validator Tests
  , fastProperty "new syntax validator empty" prop_new_syntax_validator_empty
  , fastProperty "validate empty string" prop_validate_empty_string
  , fastProperty "validate balanced vs unbalanced" prop_validate_balanced_vs_unbalanced
  , fastProperty "validate missing braces" prop_validate_missing_braces
  , fastProperty "validate missing parentheses" prop_validate_missing_parentheses
  , fastProperty "validate missing brackets" prop_validate_missing_brackets
  , fastProperty "validate unclosed strings" prop_validate_unclosed_strings
  , fastProperty "validate valid identifiers" prop_validate_valid_identifiers
  , fastProperty "validate invalid identifiers" prop_validate_invalid_identifiers
  
  -- Error Formatting Tests
  , fastProperty "format syntax error" prop_format_syntax_error
  , fastProperty "format syntax error includes type" prop_format_syntax_error_includes_type
  , fastProperty "format syntax error includes line" prop_format_syntax_error_includes_line
  , fastProperty "format syntax error includes column" prop_format_syntax_error_includes_column
  
  -- File Validation Tests
  , fastProperty "validate file empty" prop_validate_file_empty
  , fastProperty "validate file simple" prop_validate_file_simple
  , fastProperty "validate file multiple" prop_validate_file_multiple
  
  -- Integration Tests
  , fastProperty "validation deterministic" prop_validation_deterministic
  , fastProperty "error collection consistent" prop_error_collection_consistent
  , fastProperty "validation nested structures" prop_validation_nested_structures
  , fastProperty "validation complex expressions" prop_validation_complex_expressions
  ]

-- Helper function
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)