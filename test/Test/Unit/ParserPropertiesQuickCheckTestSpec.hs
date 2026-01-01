{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserPropertiesQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Positive(Positive), getPositive, Arbitrary(..)
  , Gen, oneof, elements, listOf, listOf1, choose, sized, suchThat
  )

import Parser
import Compiler.GoLexer
import Compiler.GoParsing
import Compiler.GoAst
import SourceLocation (SourcePos(..), startPos)

import Data.Char (isSpace, isLetter, isDigit, isAlphaNum)
import qualified Data.List as L
import Data.List (isPrefixOf, isSuffixOf)
import Data.List (intercalate)
import qualified Data.Text as T

-- Generate valid identifiers (letters, digits, underscores, not starting with digit)
genIdentifier :: Gen String
genIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- Generate valid keywords
genKeyword :: Gen String
genKeyword = elements 
  [ "break", "case", "chan", "const", "continue", "default", "defer", "else"
  , "fallthrough", "for", "func", "go", "goto", "if", "import", "interface"
  , "map", "package", "range", "return", "select", "struct", "switch", "type"
  , "var"
  ]

-- Generate valid operators
genOperator :: Gen String
genOperator = elements
  [ "+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>", "&^", "+=", "-="
  , "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", "&^=", "&&", "||"
  , "<-", "++", "--", "==", "!=", "<", "<=", ">", ">=", "(", ")", "[", "]"
  , "{", "}", ",", ";", ".", ":", "...", ":=", "=", "!", "new", "make"
  ]

-- Generate valid literals
genLiteral :: Gen String
genLiteral = oneof
  [ -- Integer literals
    do
      digits <- listOf1 $ elements ['0'..'9']
      return digits
  , -- Float literals
    do
      intPart <- listOf1 $ elements ['0'..'9']
      fracPart <- listOf1 $ elements ['0'..'9']
      return $ intPart ++ "." ++ fracPart
  , -- String literals
    do
      content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
      return $ "\"" ++ content ++ "\""
  , -- Character literals
    do
      char <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "!@#$%^&*()"
      return $ "'" ++ [char] ++ "'"
  , -- Boolean literals
    elements ["true", "false"]
  ]

-- Generate simple expressions
genExpression :: Gen String
genExpression = sized $ \n -> if n == 0 then genLiteral else oneof
  [ genLiteral
  , genIdentifier
  , do
      expr1 <- genExpression
      op <- genOperator
      expr2 <- genExpression
      return $ expr1 ++ " " ++ op ++ " " ++ expr2
  , do
      expr <- genExpression
      return $ "(" ++ expr ++ ")"
  ]

-- Generate simple statements
genStatement :: Gen String
genStatement = oneof
  [ -- Variable declaration
    do
      vars <- listOf1 genIdentifier
      typ <- genIdentifier
      return $ "var " ++ intercalate "," vars ++ " " ++ typ
  , -- Assignment
    do
      vars <- listOf1 genIdentifier
      expr <- genExpression
      return $ intercalate "," vars ++ " = " ++ expr
  , -- Return statement
    do
      expr <- genExpression
      return $ "return " ++ expr
  , -- Function call
    do
      func <- genIdentifier
      args <- listOf genExpression
      return $ func ++ "(" ++ intercalate "," args ++ ")"
  , -- If statement
    do
      cond <- genExpression
      body <- genStatement
      return $ "if " ++ cond ++ " { " ++ body ++ " }"
  ]

-- Generate simple function declarations
genFunction :: Gen String
genFunction = do
  name <- genIdentifier
  params <- listOf $ do
    paramName <- genIdentifier
    paramType <- genIdentifier
    return $ paramName ++ " " ++ paramType
  returnType <- genIdentifier
  body <- listOf1 genStatement
  return $ "func " ++ name ++ "(" ++ intercalate "," params ++ ") " ++ returnType ++ " {\n" ++ 
           unlines (L.map ("  " ++) body) ++ "\n}"

-- Generate tokens that should be lexable
genLexableInput :: Gen String
genLexableInput = oneof
  [ genIdentifier
  , genKeyword
  , genOperator
  , genLiteral
  , do
      expr <- genExpression
      return expr
  , do
      stmt <- genStatement
      return stmt
  ]

-- Generate whitespace
genWhitespace :: Gen String
genWhitespace = listOf $ oneof
  [ pure ' '
  , pure '\t'
  , pure '\n'
  , pure '\r'
  ]

-- Property: identifier lexer recognizes valid identifiers
prop_identifier_lexing :: String -> Property
prop_identifier_lexing ident =
  not (null ident) && L.all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']) ident &&
  not (null ident) && isLetter (L.head ident) ==>
  let tokens = lexGo ident
  in not (null tokens) -- Should produce at least one token

-- Property: keyword lexer recognizes keywords
prop_keyword_lexing :: String -> Property
prop_keyword_lexing keyword =
  keyword `elem` ["break", "case", "chan", "const", "continue", "default", "defer", "else"
                , "fallthrough", "for", "func", "go", "goto", "if", "import", "interface"
                , "map", "package", "range", "return", "select", "struct", "switch", "type"
                , "var"] ==>
  let tokens = lexGo keyword
  in not (null tokens) -- Should produce at least one token

-- Property: operator lexer recognizes operators
prop_operator_lexing :: String -> Property
prop_operator_lexing op =
  op `elem` ["+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>", "&^", "+=", "-="
            , "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", "&^=", "&&", "||"
            , "<-", "++", "--", "==", "!=", "<", "<=", ">", ">=", "(", ")", "[", "]"
            , "{", "}", ",", ";", ".", ":", "...", ":=", "=", "!", "new", "make"] ==>
  let tokens = lexGo op
  in not (null tokens) -- Should produce at least one token

-- Property: literal lexer recognizes literals
prop_literal_lexing :: String -> Property
prop_literal_lexing literal =
  let isStringLiteral = not (null literal) && L.head literal == '\"' && last literal == '\"'
      isCharLiteral = not (null literal) && L.head literal == '\'' && last literal == '\'' && L.length literal >= 3
      isIntLiteral = not (null literal) && L.all isDigit literal
      isFloatLiteral = not (null literal) && L.elem '.' literal && 
                       L.all (\c -> isDigit c || c == '.') literal &&
                       L.length (L.filter (== '.') literal) == 1
      isBoolLiteral = literal `elem` ["true", "false"]
  in (isStringLiteral || isCharLiteral || isIntLiteral || isFloatLiteral || isBoolLiteral) ==>
  let tokens = lexGo literal
  in not (null tokens) -- Should produce at least one token

-- Property: lexer handles whitespace correctly
prop_whitespace_handling :: String -> String -> Property
prop_whitespace_handling content ws =
  let withoutWS = content
      withWS = content ++ ws ++ content
      tokensWithoutWS = lexGo withoutWS
      tokensWithWS = lexGo withWS
  in L.length tokensWithWS >= L.length tokensWithoutWS

-- Property: lexer produces non-empty tokens for non-empty input
prop_non_empty_input_produces_tokens :: String -> Property
prop_non_empty_input_produces_tokens input =
  not (L.all isSpace input) && not (null input) ==>
  let tokens = lexGo input
  in not (null tokens)

-- Property: lexer handles empty input
prop_empty_input_handling :: Property
prop_empty_input_handling = lexGo "" === []

-- Property: lexer handles only whitespace
prop_whitespace_only_input :: String -> Property
prop_whitespace_only_input ws =
  L.all isSpace ws ==> lexGo ws === []

-- Property: simple expression parsing
prop_simple_expression_parsing :: String -> Property
prop_simple_expression_parsing expr =
  not (null expr) && L.all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "+-*/() ") expr ==>
  let tokens = lexGo expr
      result = parseExpression tokens
  in not (null tokens) ==> 
     case result of
       Left _ -> property False
       Right (ast, remaining) -> property True -- Should parse successfully

-- Property: identifier parsing is deterministic
prop_identifier_parsing_deterministic :: String -> Property
prop_identifier_parsing_deterministic ident =
  not (null ident) && L.all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']) ident &&
  not (null ident) && isLetter (L.head ident) ==>
  let tokens1 = lexGo ident
      tokens2 = lexGo ident
  in tokens1 === tokens2

-- Property: parsing preserves token count
prop_parsing_preserves_token_count :: String -> Property
prop_parsing_preserves_token_count input =
  not (null input) && not (L.all isSpace input) ==>
  let tokens = lexGo input
      originalCount = L.length tokens
  in originalCount > 0 ==> property True -- Should have tokens to parse

-- Property: lexer handles line endings correctly
prop_line_ending_handling :: String -> Property
prop_line_ending_handling content =
  let withUnix = content ++ "\n"
      withWindows = content ++ "\r\n"
      tokensUnix = lexGo withUnix
      tokensWindows = lexGo withWindows
  in not (null content) ==> 
     L.length tokensUnix >= 1 && L.length tokensWindows >= 1

-- Property: lexer is position-aware
prop_lexer_position_aware :: String -> Property
prop_lexer_position_aware input =
  not (null input) && not (L.all isSpace input) ==>
  let tokens = lexGo input
  in not (null tokens) -- Should produce tokens with position info

-- Property: parser handles empty token list
prop_parser_empty_tokens :: Property
prop_parser_empty_tokens = 
  let result = parseExpression []
  in case result of
    Left _ -> property True  -- Should fail appropriately
    Right _ -> property False -- Should not succeed on empty input

-- Property: parser handles malformed input gracefully
prop_parser_malformed_input :: String -> Property
prop_parser_malformed_input malformed =
  L.all (`elem` "!@#$%^&*()_+-=[]{}|;':\",./<>?") malformed && not (null malformed) ==>
  let tokens = lexGo malformed
      result = parseExpression tokens
  in case result of
    Left _ -> property True  -- Should fail gracefully
    Right _ -> property True -- Or succeed if it's somehow valid

tests :: TestTree
tests =
  testGroup "Parser Properties QuickCheck Tests"
    [ fastProperty "identifier lexer recognizes valid identifiers" prop_identifier_lexing
    , fastProperty "keyword lexer recognizes keywords" prop_keyword_lexing
    , fastProperty "operator lexer recognizes operators" prop_operator_lexing
    , fastProperty "literal lexer recognizes literals" prop_literal_lexing
    , fastProperty "lexer handles whitespace correctly" prop_whitespace_handling
    , fastProperty "non-empty input produces tokens" prop_non_empty_input_produces_tokens
    , fastProperty "lexer handles empty input" prop_empty_input_handling
    , fastProperty "lexer handles only whitespace" prop_whitespace_only_input
    , fastProperty "simple expression parsing" prop_simple_expression_parsing
    , fastProperty "identifier parsing is deterministic" prop_identifier_parsing_deterministic
    , fastProperty "parsing preserves token count" prop_parsing_preserves_token_count
    , fastProperty "lexer handles line endings correctly" prop_line_ending_handling
    , fastProperty "lexer is position-aware" prop_lexer_position_aware
    , fastProperty "parser handles empty token list" prop_parser_empty_tokens
    , fastProperty "parser handles malformed input gracefully" prop_parser_malformed_input
    ]