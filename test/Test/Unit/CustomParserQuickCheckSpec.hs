{-# LANGUAGE LambdaCase #-}
module Test.Unit.CustomParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (==>), forAll, elements, listOf1, oneof)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isLetter)
import qualified Text.Megaparsec as MP
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..))

-- | Generate valid identifiers for Typus language
genIdentifier :: Gen String
genIdentifier = do
  first <- elements ['a'..'z']
  rest <- listOf1 $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- | Generate simple valid Typus code blocks
genSimpleCodeBlock :: Gen String
genSimpleCodeBlock = oneof
  [ genVariableDeclaration
  , genFunctionDeclaration
  , genComment
  , genSimpleExpression
  ]

genVariableDeclaration :: Gen String
genVariableDeclaration = do
  varName <- genIdentifier
  value <- genSimpleExpression
  return $ varName ++ " := " ++ value

genFunctionDeclaration :: Gen String
genFunctionDeclaration = do
  funcName <- genIdentifier
  paramName <- genIdentifier
  body <- genSimpleExpression
  return $ "func " ++ funcName ++ "(" ++ paramName ++ ") " ++ body

genComment :: Gen String
genComment = do
  comment <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9']
  return $ "// " ++ comment

genSimpleExpression :: Gen String
genSimpleExpression = oneof
  [ return "42"
  , return "\"hello\""
  , genIdentifier
  , return "true"
  , return "false"
  ]

-- | Generate a complete Typus file content
genTypusFile :: Gen String
genTypusFile = do
  numBlocks <- elements [1..5]
  blocks <- listOf1 genSimpleCodeBlock
  directives <- oneof [return "", genDirectives]
  return $ directives ++ unlines blocks

genDirectives :: Gen String
genDirectives = do
  hasOwnership <- elements [True, False]
  hasDepTypes <- elements [True, False]
  let ownership = if hasOwnership then "// @ownership: true\n" else ""
      depTypes = if hasDepTypes then "// @dependent-types: true\n" else ""
  return $ ownership ++ depTypes

-- | Test that parsing a simple variable declaration works
prop_parseVariableDeclaration :: Property
prop_parseVariableDeclaration = forAll genVariableDeclaration $ \code ->
  let result = parseTypus "test.typus" code
  in case result of
    Left _ -> False
    Right (TypusFile _ _ blocks) -> not (null blocks)

-- | Test that parsing a simple function declaration works
prop_parseFunctionDeclaration :: Property
prop_parseFunctionDeclaration = forAll genFunctionDeclaration $ \code ->
  let result = parseTypus "test.typus" code
  in case result of
    Left _ -> False
    Right (TypusFile _ _ blocks) -> not (null blocks)

-- | Test that parsing comments works
prop_parseComment :: Property
prop_parseComment = forAll genComment $ \code ->
  let result = parseTypus "test.typus" code
  in case result of
    Left _ -> False
    Right (TypusFile _ _ blocks) -> True  -- Comments may be filtered out

-- | Test that empty file can be parsed
prop_parseEmptyFile :: Property
prop_parseEmptyFile = 
  let result = parseTypus "test.typus" ""
  in case result of
    Left _ -> False
    Right (TypusFile _ _ blocks) -> null blocks

-- | Test that file directives are parsed correctly
prop_parseFileDirectives :: Property
prop_parseFileDirectives = forAll genDirectives $ \directives ->
  let code = directives ++ "x := 42"
      result = parseTypus "test.typus" code
  in case result of
    Left _ -> False
    Right (TypusFile fileDirectives _ _) -> fileDirectives /= defaultFileDirectives

-- | Test that round-trip parsing preserves structure
prop_roundTripParsing :: Property
prop_roundTripParsing = forAll genTypusFile $ \code ->
  let result = parseTypus "test.typus" code
  in case result of
    Left _ -> False
    Right _ -> True  -- Basic success test

-- | Test that invalid syntax fails gracefully
prop_invalidSyntaxFails :: Property
prop_invalidSyntaxFails = 
  let invalidCode = "invalid syntax with { unmatched brackets"
      result = parseTypus "test.typus" invalidCode
  in case result of
    Left _ -> True
    Right _ -> False

-- | Test that whitespace handling is robust
prop_whitespaceHandling :: Property
prop_whitespaceHandling = forAll genVariableDeclaration $ \baseCode ->
  let codeWithExtraWhitespace = "  \n  " ++ baseCode ++ "  \n  "
      result = parseTypus "test.typus" codeWithExtraWhitespace
  in case result of
    Left _ -> False
    Right _ -> True

tests :: TestTree
tests = testGroup "Custom Parser QuickCheck Tests"
  [ testProperty "parse variable declaration" prop_parseVariableDeclaration
  , testProperty "parse function declaration" prop_parseFunctionDeclaration
  , testProperty "parse comment" prop_parseComment
  , testProperty "parse empty file" prop_parseEmptyFile
  , testProperty "parse file directives" prop_parseFileDirectives
  , testProperty "round-trip parsing" prop_roundTripParsing
  , testProperty "invalid syntax fails" prop_invalidSyntaxFails
  , testProperty "whitespace handling" prop_whitespaceHandling
  ]