{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.ConciseParserQuickCheckSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>))
import qualified Data.Text as T
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Data.Char (isSpace, isAlpha, isAlphaNum, toLower, toUpper, isDigit, isLetter)
import Parser (TypusFile(..), parseTypusFile, parseExpression, parseDeclaration)

-- Helper generators for Parser tests
genIdentifier :: Gen String
genIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
  return $ first : rest

genKeyword :: Gen String
genKeyword = elements ["func", "var", "let", "if", "else", "for", "while", "return", "type", "struct", "interface"]

genOperator :: Gen String
genOperator = elements ["+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">=", "&&", "||", "!", "="]

genLiteral :: Gen String
genLiteral = oneof
  [ genStringLiteral
  , genNumberLiteral
  , genBooleanLiteral
  ]

genStringLiteral :: Gen String
genStringLiteral = do
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
  return $ "\"" ++ content ++ "\""

genNumberLiteral :: Gen String
genNumberLiteral = do
  sign <- elements ["", "-"]
  intPart <- choose (0, 1000)
  fracPart <- choose (0, 100)
  return $ sign ++ show intPart ++ if fracPart > 0 then "." ++ show fracPart else ""

genBooleanLiteral :: Gen String
genBooleanLiteral = elements ["true", "false"]

genSimpleExpression :: Gen String
genSimpleExpression = oneof
  [ genIdentifier
  , genLiteral
  ]

genComplexExpression :: Gen String
genComplexExpression = do
  left <- genSimpleExpression
  op <- genOperator
  right <- genSimpleExpression
  return $ left ++ " " ++ op ++ " " ++ right

genValidTypusCode :: Gen String
genValidTypusCode = do
  numLines <- choose (1, 5)
  lines <- vectorOf numLines $ oneof
    [ genVarDeclaration
    , genFuncDeclaration
    , genExpressionStatement
    ]
  return $ unlines lines

genVarDeclaration :: Gen String
genVarDeclaration = do
  name <- genIdentifier
  value <- genSimpleExpression
  return $ "var " ++ name ++ " = " ++ value

genFuncDeclaration :: Gen String
genFuncDeclaration = do
  name <- genIdentifier
  numParams <- choose (0, 3)
  params <- vectorOf numParams genIdentifier
  body <- genSimpleExpression
  return $ "func " ++ name ++ "(" ++ unwords params ++ ") " ++ body

genExpressionStatement :: Gen String
genExpressionStatement = do
  expr <- genComplexExpression
  return $ expr ++ ";"

-- Test properties for Parser module

-- Basic parsing tests
prop_parse_identifier_no_crash :: String -> Property
prop_parse_identifier_no_crash s = 
  not (null s) && isAlpha (head s) && all (\c -> isAlphaNum c || c == '_') s ==>
  case parseExpression s of
    Left _ -> property True
    Right _ -> property True

prop_parse_literal_no_crash :: String -> Property
prop_parse_literal_no_crash s = 
  (isPrefixOf "\"" s && isSuffixOf "\"" s) || 
  all isDigit s || 
  s `elem` ["true", "false"] ==>
  case parseExpression s of
    Left _ -> property True
    Right _ -> property True

prop_parse_simple_expression_no_crash :: String -> Property
prop_parse_simple_expression_no_crash s = 
  not (null s) && all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_") s ==>
  case parseExpression s of
    Left _ -> property True
    Right _ -> property True

prop_parse_complex_expression_no_crash :: String -> String -> String -> Property
prop_parse_complex_expression_no_crash left op right = 
  not (null left) && not (null right) && op `elem` ["+", "-", "*", "/", "==", "!=", "<", ">"] ==>
  let expr = left ++ " " ++ op ++ " " ++ right
  in case parseExpression expr of
       Left _ -> property True
       Right _ -> property True

prop_parse_var_declaration_no_crash :: String -> String -> Property
prop_parse_var_declaration_no_crash name value = 
  not (null name) && isAlpha (head name) && all (\c -> isAlphaNum c || c == '_') name &&
  not (null value) ==>
  let decl = "var " ++ name ++ " = " ++ value ++ ";"
  in case parseDeclaration decl of
       Left _ -> property True
       Right _ -> property True

prop_parse_func_declaration_no_crash :: String -> [String] -> String -> Property
prop_parse_func_declaration_no_crash name params body = 
  not (null name) && isAlpha (head name) && all (\c -> isAlphaNum c || c == '_') name &&
  all (\p -> not (null p) && isAlpha (head p) && all (\c -> isAlphaNum c || c == '_') p) params &&
  not (null body) ==>
  let decl = "func " ++ name ++ "(" ++ unwords params ++ ") " ++ body
  in case parseDeclaration decl of
       Left _ -> property True
       Right _ -> property True

prop_parse_typus_file_no_crash :: String -> Property
prop_parse_typus_file_no_crash code = 
  not (null code) ==>
  case parseTypusFile code of
    Left _ -> property True
    Right _ -> property True

prop_parse_empty_file :: Property
prop_parse_empty_file = 
  case parseTypusFile "" of
    Left _ -> property True
    Right file -> property $ null (declarations file)

prop_parse_whitespace_only :: String -> Property
prop_parse_whitespace_only ws = 
  all isSpace ws ==>
  case parseTypusFile ws of
    Left _ -> property True
    Right file -> property $ null (declarations file)

tests :: TestTree
tests = testGroup "Concise Parser QuickCheck Tests"
  [ testProperties "Basic Parsing Tests"
    [ ("parse identifier no crash", prop_parse_identifier_no_crash)
    , ("parse literal no crash", prop_parse_literal_no_crash)
    , ("parse simple expression no crash", prop_parse_simple_expression_no_crash)
    , ("parse complex expression no crash", prop_parse_complex_expression_no_crash)
    ]
  , testProperties "Declaration Tests"
    [ ("parse var declaration no crash", prop_parse_var_declaration_no_crash)
    , ("parse func declaration no crash", prop_parse_func_declaration_no_crash)
    ]
  , testProperties "File Parsing Tests"
    [ ("parse typus file no crash", prop_parse_typus_file_no_crash)
    , ("parse empty file", prop_parse_empty_file)
    , ("parse whitespace only", prop_parse_whitespace_only)
    ]
  ]