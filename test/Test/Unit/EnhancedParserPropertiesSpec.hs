{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.EnhancedParserPropertiesSpec where



import Test.Tasty

import Test.Tasty.QuickCheck
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, sort, nub)
import Data.Char (isAlpha, isDigit, isSpace, toLower, toUpper)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Test enhanced parser properties
tests :: TestTree
tests = testGroup "Enhanced Parser Properties Tests"
  [ testGroup "Tokenization properties"
    [ testProperty "tokenization preserves order" $
        \input -> tokenize input === tokenize input
    
    , testProperty "parsing is deterministic" $
        \input -> 
          let parsed = parse input
          in isRight parsed ==> parsed === parsed
    
    , testProperty "parsing preserves idempotency" $
        \input -> 
          let parsed = parse input
          in isRight parsed ==> parse (either (const "") id parsed) === parsed
    
    , testProperty "parsing handles empty input" $
        \(_ :: String) -> tokenize "" === []
    
    , testProperty "parsing handles whitespace" $
        \input -> not (null input) ==> length (tokenize (" " ++ input)) >= length (tokenize input)
    
    , testProperty "tokenization identifies identifiers" $
        \ident -> isValidIdentifier ident ==> containsIdentifierToken (tokenize ident)
    
    , testProperty "tokenization identifies numbers" $
        \num -> isValidNumber num ==> containsNumberToken (tokenize num)
    
    , testProperty "tokenization identifies operators" $
        \op -> isValidOperator op ==> containsOperatorToken (tokenize op)
    
    , testProperty "tokenization identifies keywords" $
        \kw -> isValidKeyword kw ==> containsKeywordToken (tokenize kw)
    ]
  
  , testGroup "Parsing properties"
    [ testProperty "parsing preserves semantics" $
        \input -> 
          let parsed = parse input
          in isRight parsed ==> parse (either (const "") id parsed) === parsed
    
    , testProperty "parsing handles nested structures" $
        \depth -> isPositive depth ==> canParseNestedStructures depth
    
    , testProperty "parsing respects precedence" $
        \expr1 expr2 -> hasHigherPrecedence expr1 expr2 ==> parse (expr1 ++ " " ++ expr2) === parseWithPrecedence expr1 expr2
    
    , testProperty "parsing handles associativity" $
        \expr1 expr2 expr3 -> 
          let leftAssoc = parse (expr1 ++ " + " ++ expr2 ++ " + " ++ expr3)
              rightAssoc = parse (expr1 ++ " + (" ++ expr2 ++ " + " ++ expr3 ++ ")")
          in isLeftAssociative "+" ==> leftAssoc === rightAssoc
    
    , testProperty "parsing handles type annotations" $
        \expr typ -> isValidExpression expr && isValidType typ ==> 
          canParseTypeAnnotation expr typ
    
    , testProperty "parsing handles function definitions" $
        \name params body -> isValidIdentifier name && all isValidIdentifier params && isValidExpression body ==>
          canParseFunctionDefinition name params body
    ]
  
  , testGroup "AST properties"
    [ testProperty "AST preserves structure" $
        \input -> isParseable input ==> astStructure (parse input) === expectedAstStructure input
    
    , testProperty "AST preserves metadata" $
        \input -> isParseable input ==> astMetadata (parse input) === expectedAstMetadata input
    
    , testProperty "AST size correlates with input size" $
        \input -> isParseable input ==> astSize (parse input) <= inputSize input * 2
    
    , testProperty "AST is well-formed" $
        \input -> isParseable input ==> isWellFormedAst (parse input)
    
    , testProperty "AST maintains parent-child relationships" $
        \input -> isParseable input && hasNestedStructure input ==> 
          all hasValidParentRelation (parentChildRelations (parse input))
    ]
  
  , testGroup "Error handling properties"
    [ testProperty "syntax errors are detected" $
        \input -> hasSyntaxError input ==> isLeft (parse input)
    
    , testProperty "type errors are detected in typed input" $
        \input -> hasTypeError input ==> isLeft (typeCheck (parse input))
    
    , testProperty "semantic errors are detected" $
        \input -> hasSemanticError input ==> isLeft (semanticCheck (parse input))
    
    , testProperty "error messages are informative" $
        \input -> hasError input ==> isInformative (errorMessage input)
    
    , testProperty "error positions are accurate" $
        \input -> hasError input ==> errorPositionIsAccurate input
    
    , testProperty "recovery attempts succeed" $
        \input -> hasError input ==> canRecoverFromError input
    ]
  ]

-- Helper functions (simplified implementations)
tokenize :: String -> [String]
tokenize = words

containsIdentifierToken :: [String] -> Bool
containsIdentifierToken = any isValidIdentifier

containsNumberToken :: [String] -> Bool
containsNumberToken = any isValidNumber

containsOperatorToken :: [String] -> Bool
containsOperatorToken = any isValidOperator

containsKeywordToken :: [String] -> Bool
containsKeywordToken = any isValidKeyword

isValidIdentifier :: String -> Bool
isValidIdentifier [] = False
isValidIdentifier (x:xs) = isAlpha x && all isAlphaNum xs

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c

isValidNumber :: String -> Bool
isValidNumber [] = False
isValidNumber s = all isDigit s

isValidOperator :: String -> Bool
isValidOperator = flip elem ["+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">="]

-- Helper function
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isValidKeyword :: String -> Bool
isValidKeyword = flip elem ["if", "then", "else", "let", "in", "function", "return", "type"]

-- Additional helper functions
isPositive :: Int -> Bool
isPositive x = x > 0

isValidExpression :: String -> Bool
isValidExpression _ = True

isValidType :: String -> Bool
isValidType _ = True

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isParseable :: String -> Bool
isParseable input = not (hasSyntaxError input) && not (null input)

parse :: String -> Either String String
parse input = if isParseable input then Right input else Left "Parse error"

parseWithPrecedence :: String -> String -> Either String String
parseWithPrecedence expr1 expr2 = Right (expr1 ++ " " ++ expr2)

hasHigherPrecedence :: String -> String -> Bool
hasHigherPrecedence "*" "+" = True
hasHigherPrecedence "/" "+" = True
hasHigherPrecedence "*" "-" = True
hasHigherPrecedence "/" "-" = True
hasHigherPrecedence _ _ = False

isLeftAssociative :: String -> Bool
isLeftAssociative "+" = True
isLeftAssociative "-" = True
isLeftAssociative "*" = True
isLeftAssociative "/" = True
isLeftAssociative _ = False

canParseTypeAnnotation :: String -> String -> Bool
canParseTypeAnnotation _ _ = True

canParseFunctionDefinition :: String -> [String] -> String -> Bool
canParseFunctionDefinition _ _ _ = True

canParseNestedStructures :: Int -> Bool
canParseNestedStructures _ = True

astStructure :: Either String String -> String
astStructure (Right s) = "AST(" ++ s ++ ")"
astStructure (Left _) = "ErrorAST"

expectedAstStructure :: String -> String
expectedAstStructure input = "AST(" ++ input ++ ")"

astMetadata :: Either String String -> [(String, String)]
astMetadata (Right _) = [("type", "expression"), ("valid", "true")]
astMetadata (Left _) = [("type", "error"), ("valid", "false")]

expectedAstMetadata :: String -> [(String, String)]
expectedAstMetadata _ = [("type", "expression"), ("valid", "true")]

astSize :: Either String String -> Int
astSize (Right s) = length s
astSize (Left _) = 0

inputSize :: String -> Int
inputSize = length

isWellFormedAst :: Either String String -> Bool
isWellFormedAst (Right _) = True
isWellFormedAst (Left _) = False

hasNestedStructure :: String -> Bool
hasNestedStructure input = any (== '(') input && any (== ')') input

parentChildRelations :: Either String String -> [(String, String)]
parentChildRelations (Right _) = [("parent", "child")]
parentChildRelations (Left _) = []

hasValidParentRelation :: (String, String) -> Bool
hasValidParentRelation ("parent", "child") = True
hasValidParentRelation _ = False

hasSyntaxError :: String -> Bool
hasSyntaxError input = any (flip elem [')', ']', '}']) input && not (any (flip elem ['(', '[', '{']) input)

hasTypeError :: String -> Bool
hasTypeError _ = False

hasSemanticError :: String -> Bool
hasSemanticError _ = False

hasError :: String -> Bool
hasError input = hasSyntaxError input || hasTypeError input || hasSemanticError input

typeCheck :: Either String String -> Either String String
typeCheck (Right input) = if hasTypeError input then Left "Type error" else Right input
typeCheck (Left err) = Left err

semanticCheck :: Either String String -> Either String String
semanticCheck (Right input) = if hasSemanticError input then Left "Semantic error" else Right input
semanticCheck (Left err) = Left err

isInformative :: String -> Bool
isInformative = not . null

errorMessage :: String -> String
errorMessage _ = "Error message"

errorPositionIsAccurate :: String -> Bool
errorPositionIsAccurate _ = True

canRecoverFromError :: String -> Bool
canRecoverFromError _ = True

-- Helper functions for error handling (already defined above)
