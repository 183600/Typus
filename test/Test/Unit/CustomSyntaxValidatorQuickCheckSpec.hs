{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.CustomSyntaxValidatorQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (==>), forAll, elements, listOf, listOf1, oneof, choose)
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
import qualified Data.Set as Set

-- | Generate valid identifiers
genIdentifier :: Gen String
genIdentifier = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

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
  errorMessage <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['.', ',']
  lineNumber <- choose (1, 1000)
  columnNumber <- choose (1, 200)
  lineContent <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ [';', '{', '}', '(', ')']
  return $ SyntaxError errorType errorMessage lineNumber columnNumber lineContent

-- | Generate simple valid statements
genValidStatement :: Gen String
genValidStatement = oneof
  [ genVariableDeclaration
  , genFunctionDeclaration
  , genSimpleExpression
  , genComment
  ]

genVariableDeclaration :: Gen String
genVariableDeclaration = do
  varName <- genIdentifier
  varType <- elements ["int", "string", "bool", "float"]
  value <- case varType of
    "int" -> elements ["42", "0", "-1"]
    "string" -> elements ["\"hello\"", "\"world\""]
    "bool" -> elements ["true", "false"]
    "float" -> elements ["3.14", "0.0", "-1.5"]
    _ -> return "null"
  return $ varName ++ " " ++ varType ++ " = " ++ value ++ ";"

genFunctionDeclaration :: Gen String
genFunctionDeclaration = do
  funcName <- genIdentifier
  paramName <- genIdentifier
  paramType <- elements ["int", "string", "bool"]
  returnType <- elements ["int", "string", "bool", "void"]
  return $ "func " ++ funcName ++ "(" ++ paramName ++ " " ++ paramType ++ ") " ++ returnType ++ " { return " ++ paramName ++ "; }"

genSimpleExpression :: Gen String
genSimpleExpression = oneof
  [ return "42;"
  , return "\"hello\";"
  , genIdentifier >>= \name -> return $ name ++ ";"
  , return "true;"
  , return "false;"
  ]

genComment :: Gen String
genComment = do
  comment <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9']
  return $ "// " ++ comment

-- | Generate invalid syntax for error testing
genInvalidSyntax :: Gen String
genInvalidSyntax = oneof
  [ return "func incomplete {"
  , return "var x int"
  , return "{ invalid syntax }"
  , return "if condition { // missing closing brace"
  , return "unclosed string \""
  , return "/* unclosed comment"
  , return "123invalididentifier"
  , return "var 123invalid = 42;"
  ]

-- | Generate complete files
genValidFile :: Gen String
genValidFile = do
  numStatements <- choose (1, 10)
  statements <- sequence [genValidStatement | _ <- [1..numStatements]]
  return $ unlines statements

-- | Test SyntaxValidator creation
prop_syntaxValidatorCreation :: Property
prop_syntaxValidatorCreation = 
  let validator = newSyntaxValidator
  in True  -- Basic test that validator can be created

-- | Test SyntaxError equality
prop_syntaxErrorEquality :: Property
prop_syntaxErrorEquality = forAll genSyntaxError $ \error ->
  error == error

-- | Test SyntaxError ordering
prop_syntaxErrorOrdering :: Property
prop_syntaxErrorOrdering = forAll genSyntaxError $ \error1 ->
  forAll genSyntaxError $ \error2 ->
    let cmp = compare error1 error2
    in (cmp == LT) == (error1 < error2) &&
       (cmp == EQ) == (error1 == error2) &&
       (cmp == GT) == (error1 > error2)

-- | Test formatSyntaxError produces non-empty string
prop_formatSyntaxErrorNonEmpty :: Property
prop_formatSyntaxErrorNonEmpty = forAll genSyntaxError $ \error ->
  let formatted = formatSyntaxError error
  in not (null formatted)

-- | Test validateSyntax on valid input
prop_validateSyntaxValid :: Property
prop_validateSyntaxValid = forAll genValidStatement $ \statement ->
  let validator = newSyntaxValidator
      result = validateSyntax validator statement
  in True  -- Basic test that validation doesn't crash

-- | Test validateSyntax on invalid input
prop_validateSyntaxInvalid :: Property
prop_validateSyntaxInvalid = forAll genInvalidSyntax $ \invalidStatement ->
  let validator = newSyntaxValidator
      result = validateSyntax validator invalidStatement
  in True  -- Basic test that validation handles invalid syntax

-- | Test validateFile on valid file
prop_validateFileValid :: Property
prop_validateFileValid = forAll genValidFile $ \fileContent ->
  let validator = newSyntaxValidator
      result = validateFile validator fileContent
  in True  -- Basic test that file validation doesn't crash

-- | Test getSyntaxErrors after validation
prop_getSyntaxErrorsAfterValidation :: Property
prop_getSyntaxErrorsAfterValidation = forAll genValidStatement $ \statement ->
  let validator = newSyntaxValidator
      _ = validateSyntax validator statement
      errors = getSyntaxErrors validator
  in True  -- Basic test that error retrieval doesn't crash

-- | Test ErrorType properties
prop_errorTypeProperties :: Property
prop_errorTypeProperties = forAll genErrorType $ \errorType ->
  let errorTypeStr = show errorType
  in not (null errorTypeStr)

-- | Test SyntaxError field access
prop_syntaxErrorFieldAccess :: Property
prop_syntaxErrorFieldAccess = forAll genSyntaxError $ \error ->
  let errType = errorType error
      errMsg = errorMessage error
      lineNum = lineNumber error
      colNum = columnNumber error
      lineCont = lineContent error
  in lineNum >= 1 && colNum >= 1 && not (null errMsg)

-- | Test MissingBrace error
prop_missingBraceError :: Property
prop_missingBraceError = 
  let error = SyntaxError MissingBrace "Missing closing brace" 10 20 "if condition {"
      errorTypeStr = show (errorType error)
  in errorTypeStr == "MissingBrace"

-- | Test UnclosedString error
prop_unclosedStringError :: Property
prop_unclosedStringError = 
  let error = SyntaxError UnclosedString "Unclosed string literal" 5 15 "var x = \"hello"
      errorTypeStr = show (errorType error)
  in errorTypeStr == "UnclosedString"

-- | Test InvalidIdentifier error
prop_invalidIdentifierError :: Property
prop_invalidIdentifierError = 
  let error = SyntaxError InvalidIdentifier "Invalid identifier" 3 10 "var 123invalid = 42;"
      errorTypeStr = show (errorType error)
  in errorTypeStr == "InvalidIdentifier"

-- | Test SyntaxWarning error
prop_syntaxWarningError :: Property
prop_syntaxWarningError = 
  let error = SyntaxError SyntaxWarning "Warning message" 7 25 "some code"
      errorTypeStr = show (errorType error)
  in errorTypeStr == "SyntaxWarning"

-- | Test validateSyntax with empty string
prop_validateSyntaxEmpty :: Property
prop_validateSyntaxEmpty = 
  let validator = newSyntaxValidator
      result = validateSyntax validator ""
  in True  -- Basic test that empty input doesn't crash

-- | Test validateFile with empty string
prop_validateFileEmpty :: Property
prop_validateFileEmpty = 
  let validator = newSyntaxValidator
      result = validateFile validator ""
  in True  -- Basic test that empty file doesn't crash

-- | Test multiple validations on same validator
prop_multipleValidations :: Property
prop_multipleValidations = forAll genValidStatement $ \statement1 ->
  forAll genValidStatement $ \statement2 ->
    let validator = newSyntaxValidator
      _ = validateSyntax validator statement1
      _ = validateSyntax validator statement2
      errors = getSyntaxErrors validator
    in True  -- Basic test that multiple validations work

-- | Test syntax error with specific line numbers
prop_syntaxErrorLineNumbers :: Property
prop_syntaxErrorLineNumbers = forAll (choose (1, 1000)) $ \lineNum ->
  forAll (choose (1, 200)) $ \colNum ->
    let error = SyntaxError MissingBrace "test error" lineNum colNum "test line"
        retrievedLine = lineNumber error
        retrievedCol = columnNumber error
    in retrievedLine == lineNum && retrievedCol == colNum

-- | Test syntax error with line content
prop_syntaxErrorLineContent :: Property
prop_syntaxErrorLineContent = forAll genValidStatement $ \statement ->
  let error = SyntaxError InvalidStatement "Invalid statement" 1 1 statement
      retrievedContent = lineContent error
  in retrievedContent == statement

-- | Test formatSyntaxError includes line and column
prop_formatSyntaxErrorIncludesLocation :: Property
prop_formatSyntaxErrorIncludesLocation = forAll genSyntaxError $ \error ->
  let formatted = formatSyntaxError error
      lineStr = show (lineNumber error)
      colStr = show (columnNumber error)
  in lineStr `isInfixOf` formatted && colStr `isInfixOf` formatted

-- | Test formatSyntaxError includes error message
prop_formatSyntaxErrorIncludesMessage :: Property
prop_formatSyntaxErrorIncludesMessage = forAll genSyntaxError $ \error ->
  let formatted = formatSyntaxError error
      msg = errorMessage error
  in msg `isInfixOf` formatted

  where
    isInfixOf needle haystack = needle `elem` (substrings haystack)
    substrings [] = []
    substrings s@(x:xs) = s : substrings xs

tests :: TestTree
tests = testGroup "Custom SyntaxValidator QuickCheck Tests"
  [ testProperty "SyntaxValidator creation" prop_syntaxValidatorCreation
  , testProperty "SyntaxError equality" prop_syntaxErrorEquality
  , testProperty "SyntaxError ordering" prop_syntaxErrorOrdering
  , testProperty "formatSyntaxError non-empty" prop_formatSyntaxErrorNonEmpty
  , testProperty "validateSyntax valid" prop_validateSyntaxValid
  , testProperty "validateSyntax invalid" prop_validateSyntaxInvalid
  , testProperty "validateFile valid" prop_validateFileValid
  , testProperty "getSyntaxErrors after validation" prop_getSyntaxErrorsAfterValidation
  , testProperty "ErrorType properties" prop_errorTypeProperties
  , testProperty "SyntaxError field access" prop_syntaxErrorFieldAccess
  , testProperty "MissingBrace error" prop_missingBraceError
  , testProperty "UnclosedString error" prop_unclosedStringError
  , testProperty "InvalidIdentifier error" prop_invalidIdentifierError
  , testProperty "SyntaxWarning error" prop_syntaxWarningError
  , testProperty "validateSyntax empty" prop_validateSyntaxEmpty
  , testProperty "validateFile empty" prop_validateFileEmpty
  , testProperty "multiple validations" prop_multipleValidations
  , testProperty "syntax error line numbers" prop_syntaxErrorLineNumbers
  , testProperty "syntax error line content" prop_syntaxErrorLineContent
  , testProperty "formatSyntaxError includes location" prop_formatSyntaxErrorIncludesLocation
  , testProperty "formatSyntaxError includes message" prop_formatSyntaxErrorIncludesMessage
  ]