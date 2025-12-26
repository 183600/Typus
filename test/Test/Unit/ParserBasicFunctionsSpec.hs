{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.ParserBasicFunctionsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===))
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import qualified Data.Text as T
import qualified Data.Char as C
import Text.Megaparsec (Parsec, parse, many, single, satisfy, anySingle, eof)
import Text.Megaparsec.Char (space, char, string, digitChar, letterChar)
import Compiler.GoLexer (Token(..), tokenize)
import Compiler.GoParsing (parseExpression, parseStatement, parseDeclaration)
import Parser (parseTypus, parseModule, parseFunction)

-- | Test suite for Parser basic functions
tests :: TestTree
tests = testGroup "Parser Basic Functions"
  [ testProperty "tokenize preserves input length" propTokenizePreservesLength
  , testProperty "tokenize handles whitespace correctly" propTokenizeHandlesWhitespace
  , testProperty "parse module name extraction" propParseModuleExtraction
  , testProperty "parse function signature" propParseFunctionSignature
  , testProperty "expression parsing associativity" propExpressionParsingAssociativity
  , testCase "basic tokenization" testBasicTokenization
  , testCase "identifier parsing" testIdentifierParsing
  , testCase "number literal parsing" testNumberLiteralParsing
  , testCase "string literal parsing" testStringLiteralParsing
  , testCase "error recovery in parsing" testErrorRecovery
  ]

-- | Property: tokenize preserves input length when ignoring whitespace
propTokenizePreservesLength :: String -> Property
propTokenizePreservesLength input =
  let tokens = tokenize input
      totalTokenLength = sum $ map tokenLength tokens
      inputLength = length $ filter (not . C.isSpace) input
  in property $ totalTokenLength == inputLength
  where
    tokenLength (TokenIdentifier _) = 1
    tokenLength (TokenNumber _) = 1
    tokenLength (TokenString _) = 1
    tokenLength (TokenOperator _) = 1
    tokenLength (TokenKeyword _) = 1
    tokenLength (TokenDelimiter _) = 1
    tokenLength TokenEOF = 0

-- | Property: tokenize handles whitespace correctly
propTokenizeHandlesWhitespace :: String -> Property
propTokenizeHandlesWhitespace input =
  let tokens = tokenize input
      hasWhitespaceTokens = any isWhitespaceToken tokens
  in property $ not hasWhitespaceTokens
  where
    isWhitespaceToken (TokenIdentifier s) = all C.isSpace s
    isWhitespaceToken _ = False

-- | Property: parse module name extraction
propParseModuleExtraction :: String -> Property
propParseModuleExtraction moduleName =
  let input = "module " ++ moduleName ++ " where"
      result = parse parseModule "" input
  in case result of
    Left _ -> property $ False
    Right parsed -> property $ extractModuleName parsed == moduleName

-- | Property: parse function signature
propParseFunctionSignature :: String -> String -> Property
propParseFunctionSignature funcName paramType =
  let input = funcName ++ " :: " ++ paramType
      result = parse parseFunction "" input
  in case result of
    Left _ -> property $ False
    Right parsed -> property $ extractFunctionName parsed == funcName

-- | Property: expression parsing associativity
propExpressionParsingAssociativity :: Int -> Int -> Int -> Property
propExpressionParsingAssociativity a b c =
  let input = show a ++ " + " ++ show b ++ " * " ++ show c
      result = parse parseExpression "" input
  in case result of
    Left _ -> property $ False
    Right parsed -> property $ evaluateExpression parsed == a + (b * c)

-- | Unit tests for basic tokenization
testBasicTokenization :: IO ()
testBasicTokenization = do
  let input = "let x = 42"
      tokens = tokenize input
  assertBool "identifier token" $ any (isTokenIdentifier "let") tokens
  assertBool "identifier token x" $ any (isTokenIdentifier "x") tokens
  assertBool "number token" $ any isTokenNumber tokens
  assertBool "operator token" $ any (isTokenOperator "=") tokens

-- | Unit tests for identifier parsing
testIdentifierParsing :: IO ()
testIdentifierParsing = do
  let validIdentifiers = ["x", "myVar", "foo_bar", "test123"]
      invalidIdentifiers = ["123abc", "foo-bar", "with space"]
  mapM_ (\ident -> do
    let result = parse identifierParser "" ident
    assertBool ("valid identifier: " ++ ident) $ either (const False) (const True) result
    ) validIdentifiers
  mapM_ (\ident -> do
    let result = parse identifierParser "" ident
    assertBool ("invalid identifier: " ++ ident) $ either (const True) (const False) result
    ) invalidIdentifiers

-- | Unit tests for number literal parsing
testNumberLiteralParsing :: IO ()
testNumberLiteralParsing = do
  let numbers = ["42", "3.14", "-10", "0"]
  mapM_ (\num -> do
    let result = parse numberParser "" num
    assertBool ("valid number: " ++ num) $ either (const False) (const True) result
    ) numbers

-- | Unit tests for string literal parsing
testStringLiteralParsing :: IO ()
testStringLiteralParsing = do
  let strings = ["\"hello\"", "\"world\"", "\"\""]
      invalidStrings = ["unclosed", "\"missing end"]
  mapM_ (\str -> do
    let result = parse stringParser "" str
    assertBool ("valid string: " ++ str) $ either (const False) (const True) result
    ) strings
  mapM_ (\str -> do
    let result = parse stringParser "" str
    assertBool ("invalid string: " ++ str) $ either (const True) (const False) result
    ) invalidStrings

-- | Unit tests for error recovery in parsing
testErrorRecovery :: IO ()
testErrorRecovery = do
  let input = "let x = 42; y = ; z = 10"
      result = parse parseModule "" input
  assertBool "parsing recovers from error" $ either (const False) (const True) result

-- Helper functions
isTokenIdentifier :: String -> Token -> Bool
isTokenIdentifier name (TokenIdentifier n) = n == name
isTokenIdentifier _ _ = False

isTokenNumber :: Token -> Bool
isTokenNumber (TokenNumber _) = True
isTokenNumber _ = False

isTokenOperator :: String -> Token -> Bool
isTokenOperator op (TokenOperator o) = o == op
isTokenOperator _ _ = False

-- Mock parsers (these would be implemented in the actual Parser module)
identifierParser :: Parsec Void String String
identifierParser = many letterChar

numberParser :: Parsec Void String Int
numberParser = read <$> many digitChar

stringParser :: Parsec Void String String
stringParser = do
  char '"'
  content <- many (satisfy (/= '"'))
  char '"'
  return content

-- Mock extraction functions
extractModuleName :: a -> String
extractModuleName _ = "TestModule"

extractFunctionName :: a -> String
extractFunctionName _ = "testFunction"

evaluateExpression :: a -> Int
evaluateExpression _ = 42

-- Helper function for property testing
property :: Bool -> Property
property = property' where
  property' :: Bool -> Property
  property' = id

-- Import for Megaparsec
import Text.Megaparsec (Parsec, Void)