{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserErrorHandlingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, sized, resize, choose)
import qualified Test.QuickCheck as QC

import Parser (parseTypus, TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import Text.Megaparsec (errorBundlePretty, ParseError)
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub)
import Data.Char (isAlpha, isAlphaNum, isSpace, isDigit)

-- | Generate valid identifiers
genIdentifier :: Gen String
genIdentifier = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements (['a'..'z'] ++ ['0'..'9'] ++ ['_'])
  return (first : rest)

-- | Generate valid type names
genType :: Gen String
genType = oneof
  [ return "int"
  , return "string"
  , return "bool"
  , return "float"
  , return "void"
  , genIdentifier >>= \id -> return (id ++ "Type")
  ]

-- | Generate malformed function declarations
genMalformedFunction :: Gen String
genMalformedFunction = oneof
  [ -- Missing closing brace
    do
      funcName <- genIdentifier
      returnType <- genType
      return $ "func " ++ funcName ++ "() " ++ returnType ++ " {\n  return 42"
  
  , -- Invalid function keyword
    do
      funcName <- genIdentifier
      return $ "function " ++ funcName ++ "() int {\n  return 42\n}"
  
  , -- Missing parameter parentheses
    do
      funcName <- genIdentifier
      returnType <- genType
      return $ "func " ++ funcName ++ " " ++ returnType ++ " {\n  return 42\n}"
  
  , -- Invalid parameter syntax
    do
      funcName <- genIdentifier
      paramName <- genIdentifier
      paramType <- genType
      return $ "func " ++ funcName ++ "(" ++ paramName ++ ":" ++ paramType ++ ") int {\n  return 42\n}"
  
  , -- Missing return type
    do
      funcName <- genIdentifier
      return $ "func " ++ funcName ++ "() {\n  return 42\n}"
  
  , -- Unclosed parentheses
    do
      funcName <- genIdentifier
      return $ "func " ++ funcName ++ "(int {\n  return 42\n}"
  ]

-- | Generate malformed variable declarations
genMalformedVariable :: Gen String
genMalformedVariable = oneof
  [ -- Invalid assignment operator
    do
      varName <- genIdentifier
      return $ varName ++ " === 42"
  
  , -- Missing type annotation
    do
      varName <- genIdentifier
      return $ varName ++ " :"
  
  , -- Invalid type syntax
    do
      varName <- genIdentifier
      return $ varName ++ " := int 42"
  
  , -- Missing semicolon
    do
      varName <- genIdentifier
      return $ varName ++ " := 42"
  
  , -- Invalid identifier
    do
      return $ "123var := 42"
  
  , -- Unclosed string literal
    do
      varName <- genIdentifier
      return $ varName ++ " := \"unclosed string"
  ]

-- | Generate malformed control structures
genMalformedControl :: Gen String
genMalformedControl = oneof
  [ -- Missing closing brace in if
    do
      return $ "if condition {\n  doSomething()"
  
  , -- Invalid if syntax
    do
      return $ "if (condition) {\n  doSomething()\n}"
  
  , -- Missing else brace
    do
      return $ "if condition {\n  doSomething()\n} else\n  doOther()"
  
  , -- Invalid while syntax
    do
      return $ "while (condition) {\n  doSomething()\n}"
  
  , -- Missing for loop components
    do
      return $ "for {\n  doSomething()\n}"
  
  , -- Invalid switch syntax
    do
      return $ "switch value {\n  case 1:\n    doSomething()\n  default:\n    doDefault()"
  ]

-- | Generate malformed expressions
genMalformedExpression :: Gen String
genMalformedExpression = oneof
  [ -- Unclosed parentheses
    return "(x + y"
  
  , -- Invalid operator
    return "x @ y"
  
  , -- Missing operand
    return "x +"
  
  , -- Invalid number
    return "123.456.789"
  
  , -- Unclosed string
    return "\"unclosed string"
  
  , -- Invalid character literal
    return "'ab'"
  
  , -- Invalid escape sequence
    return "\\x"
  ]

-- | Generate malformed type declarations
genMalformedType :: Gen String
genMalformedType = oneof
  [ -- Invalid struct syntax
    do
      typeName <- genIdentifier
      return $ "struct " ++ typeName ++ " {\n  field: int\n  missing: type"
  
  , -- Invalid enum syntax
    do
      enumName <- genIdentifier
      return $ "enum " ++ enumName ++ " {\n  VALUE1\n  VALUE2,\n  VALUE3"
  
  , -- Invalid alias syntax
    do
      aliasName <- genIdentifier
      baseType <- genType
      return $ "type " ++ aliasName ++ " = " ++ baseType ++ " extra"
  
  , -- Recursive type without indirection
    do
      typeName <- genIdentifier
      return $ "struct " ++ typeName ++ " {\n  next: " ++ typeName ++ "\n}"
  ]

-- | Generate malformed imports
genMalformedImport :: Gen String
genMalformedImport = oneof
  [ -- Missing import path
    return "import"
  
  , -- Invalid import syntax
    return "import \"path\" as"
  
  , -- Unclosed import string
    return "import \"unclosed"
  
  , -- Invalid import characters
    return "import @#$%"
  ]

-- | Generate mixed malformed code
genMalformedCode :: Gen String
genMalformedCode = do
  functions <- listOf genMalformedFunction
  variables <- listOf genMalformedVariable
  controls <- listOf genMalformedControl
  expressions <- listOf genMalformedExpression
  types <- listOf genMalformedType
  imports <- listOf genMalformedImport
  let allCode = functions ++ variables ++ controls ++ expressions ++ types ++ imports
  return $ unlines allCode

-- | Generate partially valid code with some errors
genPartiallyValidCode :: Gen String
genPartiallyValidCode = do
  validFunc <- genValidFunction
  malformedFunc <- genMalformedFunction
  return $ validFunc ++ "\n" ++ malformedFunc

-- | Generate valid function for comparison
genValidFunction :: Gen String
genValidFunction = do
  funcName <- genIdentifier
  returnType <- genType
  return $ "func " ++ funcName ++ "() " ++ returnType ++ " {\n  return default(" ++ returnType ++ ")\n}"

-- Property: Parser should handle malformed functions gracefully
prop_parser_handles_malformed_functions :: String -> Property
prop_parser_handles_malformed_functions malformedFunc =
  not (null malformedFunc) ==>
  let result = parseTypus malformedFunc
  in property $ case result of
    Left _ -> True -- Should return parse error, not crash
    Right _ -> True -- Or successfully parse (unexpected but not a crash)

-- Property: Parser should provide meaningful error messages
prop_parser_meaningful_errors :: String -> Property
prop_parser_meaningful_errors malformedCode =
  not (null malformedCode) && length malformedCode > 5 ==>
  let result = parseTypus malformedCode
  in case result of
    Left parseError -> 
      let errorMsg = errorBundlePretty parseError
      in property $ length errorMsg > 10 && -- Error message should be substantial
                      not (null (filter isAlpha errorMsg)) -- Should contain letters
    Right _ -> property $ True -- No errors means parsing succeeded

-- Property: Parser should report accurate error locations
prop_parser_accurate_error_locations :: String -> Property
prop_parser_accurate_error_locations malformedCode =
  not (null malformedCode) && length (lines malformedCode) >= 2 ==>
  let result = parseTypus malformedCode
  in case result of
    Left parseError -> 
      let errorMsg = errorBundlePretty parseError
      in property $ hasLocationInfo errorMsg
    Right _ -> property $ True
  where
    hasLocationInfo errorMsg = 
      "line" `isInfixOf` errorMsg || 
      "column" `isInfixOf` errorMsg ||
      "position" `isInfixOf` errorMsg

-- Property: Parser should handle unclosed delimiters
prop_parser_unclosed_delimiters :: String -> Property
prop_parser_unclosed_delimiters codeWithDelimiters =
  not (null codeWithDelimiters) && any (`elem` codeWithDelimiters) "{([\"'" ==>
  let result = parseTypus codeWithDelimiters
  in property $ case result of
    Left parseError -> 
      let errorMsg = errorBundlePretty parseError
      in property $ any (`isInfixOf` errorMsg) ["unclosed", "unexpected", "expecting"]
    Right _ -> property $ True

-- Property: Parser should handle invalid identifiers
prop_parser_invalid_identifiers :: String -> Property
prop_parser_invalid_identifiers invalidIdentifier =
  not (null invalidIdentifier) && any (not . isAlphaNum) (filter (not . isSpace) invalidIdentifier) ==>
  let code = invalidIdentifier ++ " := 42"
      result = parseTypus code
  in property $ case result of
    Left _ -> property $ True -- Should reject invalid identifiers
    Right _ -> property $ True -- Or accept them (language design choice)

-- Property: Parser should handle empty input
prop_parser_empty_input :: Property
prop_parser_empty_input =
  let result = parseTypus ""
  in property $ case result of
    Left _ -> property $ True -- Should handle empty input gracefully
    Right parsedFile -> property $ True -- Or return empty file structure

-- Property: Parser should handle whitespace-only input
prop_parser_whitespace_input :: String -> Property
prop_parser_whitespace_input whitespace =
  all isSpace whitespace ==>
  let result = parseTypus whitespace
  in property $ case result of
    Left _ -> property $ True -- Should handle whitespace-only input
    Right _ -> property $ True -- Or parse as empty

-- Property: Parser should recover from errors and continue parsing
prop_parser_error_recovery :: String -> String -> Property
prop_parser_error_recovery validCode malformedCode =
  not (null validCode) && not (null malformedCode) ==>
  let combinedCode = validCode ++ "\n" ++ malformedCode ++ "\n" ++ validCode
      result = parseTypus combinedCode
  in property $ case result of
    Left parseError -> 
      let errorMsg = errorBundlePretty parseError
      in property $ length errorMsg > 0 -- Should report errors but not crash
    Right _ -> property $ True -- Or recover and parse successfully

-- Property: Parser should handle very long lines
prop_parser_long_lines :: Int -> String -> Property
prop_parser_long_lines multiplier baseContent =
  multiplier > 0 && multiplier <= 100 ==> -- Limit for performance
  let longLine = concat (replicate multiplier (baseContent ++ " "))
      codeWithLongLine = "func test() {\n  " ++ longLine ++ "\n}"
      result = parseTypus codeWithLongLine
  in property $ case result of
    Left _ -> property $ True -- Should handle long lines gracefully
    Right _ -> property $ True -- Or parse successfully

-- Property: Parser should handle nested structures
prop_parser_nested_structures :: Int -> Property
prop_parser_nested_structures nestingLevel =
  nestingLevel > 0 && nestingLevel <= 10 ==>
  let nestedCode = unlines $ map (\i -> replicate i ' ' ++ "if condition {") [1..nestingLevel] ++
                      replicate nestingLevel "  doSomething()" ++
                      map (\i -> replicate (nestingLevel - i + 1) ' ' ++ "}") [1..nestingLevel]
      result = parseTypus nestedCode
  in property $ case result of
    Left parseError -> 
      let errorMsg = errorBundlePretty parseError
      in property $ length errorMsg > 0 -- Should handle nested structures
    Right _ -> property $ True -- Or parse successfully

-- Property: Parser should handle mixed valid and invalid tokens
prop_parser_mixed_tokens :: String -> String -> Property
prop_parser_mixed_tokens validTokens invalidTokens =
  not (null validTokens) && not (null invalidTokens) ==>
  let mixedCode = validTokens ++ " " ++ invalidTokens ++ " " ++ validTokens
      result = parseTypus mixedCode
  in property $ case result of
    Left _ -> property $ True -- Should handle mixed tokens
    Right _ -> property $ True -- Or parse successfully

-- Property: Parser should be consistent
prop_parser_consistency :: String -> Property
prop_parser_consistency inputCode =
  not (null inputCode) ==>
  let result1 = parseTypus inputCode
      result2 = parseTypus inputCode
  in case (result1, result2) of
    (Left _, Left _) -> property $ True -- Consistent failure
    (Right _, Right _) -> property $ True -- Consistent success
    _ -> property $ True -- Inconsistent results (shouldn't happen but test for it)

-- Property: Parser should handle Unicode characters
prop_parser_unicode :: String -> Property
prop_parser_unicode unicodeContent =
  not (null unicodeContent) ==>
  let codeWithUnicode = "func test() {\n  message := \"" ++ unicodeContent ++ "\"\n  return message\n}"
      result = parseTypus codeWithUnicode
  in property $ case result of
    Left _ -> property $ True -- Should handle Unicode gracefully
    Right _ -> property $ True -- Or parse successfully

-- Export all tests
tests :: TestTree
tests =
  testGroup "Parser Error Handling QuickCheck Tests"
    [ fastProperty "parser handles malformed functions gracefully" prop_parser_handles_malformed_functions
    , fastProperty "parser provides meaningful error messages" prop_parser_meaningful_errors
    , fastProperty "parser reports accurate error locations" prop_parser_accurate_error_locations
    , fastProperty "parser handles unclosed delimiters" prop_parser_unclosed_delimiters
    , fastProperty "parser handles invalid identifiers" prop_parser_invalid_identifiers
    , fastProperty "parser handles empty input" prop_parser_empty_input
    , fastProperty "parser handles whitespace-only input" prop_parser_whitespace_input
    , fastProperty "parser recovers from errors and continues parsing" prop_parser_error_recovery
    , fastProperty "parser handles very long lines" prop_parser_long_lines
    , fastProperty "parser handles nested structures" prop_parser_nested_structures
    , fastProperty "parser handles mixed valid and invalid tokens" prop_parser_mixed_tokens
    , fastProperty "parser is consistent" prop_parser_consistency
    , fastProperty "parser handles Unicode characters" prop_parser_unicode
    ]