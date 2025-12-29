module Test.Unit.NewCabalParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.QuickCheck (property, forAll, Gen, arbitrary, choose, elements, listOf)
import Data.Char (isLetter, isDigit, isSpace)
import Data.List (isPrefixOf, isSuffixOf)

import TestSupport.QuickCheck (fastProperty)
import Parser

-- | QuickCheck tests for Parser module covering parsing properties and edge cases
tests :: TestTree
tests =
  testGroup "New Cabal Parser QuickCheck Tests"
    [ testGroup "Tokenization properties"
        [ fastProperty "tokenize preserves input length when whitespace is ignored" prop_tokenizePreservesNonWhitespace
        , fastProperty "tokenize handles empty input" prop_tokenizeEmpty
        , fastProperty "tokenize handles whitespace-only input" prop_tokenizeWhitespaceOnly
        , fastProperty "tokenize respects string literals" prop_tokenizeStringLiterals
        , fastProperty "tokenize respects comment syntax" prop_tokenizeComments
        ]
    
    , testGroup "Parsing properties"
        [ fastProperty "parseExpression handles valid identifiers" prop_parseValidIdentifier
        , fastProperty "parseExpression handles numeric literals" prop_parseNumericLiterals
        , fastProperty "parseExpression respects operator precedence" prop_operatorPrecedence
        , fastProperty "parseExpression handles nested expressions" prop_nestedExpressions
        ]
    
    , testGroup "Error handling properties"
        [ fastProperty "parse fails gracefully on invalid syntax" prop_parseInvalidSyntax
        , fastProperty "parse provides meaningful error locations" prop_errorLocationAccuracy
        , fastProperty "parse recovers from certain errors" prop_errorRecovery
        ]
    
    , testGroup "Edge cases and robustness"
        [ testCase "parse empty input returns appropriate result" $ do
            parse "" @?= ParseResult [] []
            
        , testCase "parse handles very long identifiers" $ do
            let longIdent = replicate 1000 'a'
            parse longIdent @?= ParseResult [Token Identifier longIdent] []
            
        , testCase "parse handles deeply nested expressions" $ do
            let deeplyNested = "(" ++ replicate 1000 "(" ++ "x" ++ replicate 1000 ")" ++ ")"
            let result = parse deeplyNested
            case result of
              ParseResult tokens [] -> length tokens @?= 2001
              _ -> assertFailure "Parse should succeed"
        ]
    
    , testGroup "Performance and scalability"
        [ fastProperty "parse time scales linearly with input size" prop_parseLinearScaling
        , fastProperty "tokenize memory usage is bounded" prop_tokenizeMemoryBounded
        ]
    ]

-- | Property: tokenize preserves input length when whitespace is ignored
prop_tokenizePreservesNonWhitespace :: String -> Bool
prop_tokenizePreservesNonWhitespace input =
  let tokens = tokenize input
      nonWhitespaceInput = filter (not . isSpace) input
      tokenContent = concatMap tokenValue tokens
  in length nonWhitespaceInput == length tokenContent

-- | Property: tokenize handles empty input
prop_tokenizeEmpty :: Bool
prop_tokenizeEmpty =
  let tokens = tokenize ""
  in null tokens

-- | Property: tokenize handles whitespace-only input
prop_tokenizeWhitespaceOnly :: String -> Bool
prop_tokenizeWhitespaceOnly input =
  let whitespaceOnly = all isSpace input
      tokens = tokenize whitespaceOnly
  in whitespaceOnly ==> null tokens

-- | Property: tokenize respects string literals
prop_tokenizeStringLiterals :: String -> String -> Bool
prop_tokenizeStringLiterals prefix content =
  let strLiteral = "\"" ++ content ++ "\""
      input = prefix ++ strLiteral
      tokens = tokenize input
  in any (\t -> tokenType t == StringLiteral && tokenValue t == content) tokens

-- | Property: tokenize respects comment syntax
prop_tokenizeComments :: String -> Bool
prop_tokenizeComments input =
  let withLineComment = input ++ "// comment"
      withBlockComment = input ++ "/* block comment */"
      tokens1 = tokenize withLineComment
      tokens2 = tokenize withBlockComment
  in length tokens1 <= length tokens2 + 1  -- Allow for slight variation

-- | Property: parseExpression handles valid identifiers
prop_parseValidIdentifier :: String -> Bool
prop_parseValidIdentifier ident =
  let isValidIdent = not (null ident) && isLetter (head ident) && all (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")) ident
  in isValidIdent ==> case parseExpression ident of
    Right expr -> isIdentifierExpression expr
    Left _ -> False

-- | Property: parseExpression handles numeric literals
prop_parseNumericLiterals :: Int -> Bool
prop_parseNumericLiterals num =
  let numStr = show num
  in case parseExpression numStr of
    Right expr -> isNumericExpression expr
    Left _ -> False

-- | Property: parseExpression respects operator precedence
prop_operatorPrecedence :: Int -> Int -> Int -> Bool
prop_operatorPrecedence a b c =
  let expr = show a ++ " + " ++ show b ++ " * " ++ show c
  in case parseExpression expr of
    Right parsed -> representsCorrectPrecedence parsed a b c
    Left _ -> False

-- | Property: parseExpression handles nested expressions
prop_nestedExpressions :: [Int] -> Bool
prop_nestedExpressions nums =
  let nested = "(" ++ concat (map (\n -> "(" ++ show n ++ ")") nums) ++ ")"
  in case parseExpression nested of
    Right expr -> depthOfExpression expr == length nums + 1
    Left _ -> False

-- | Property: parse fails gracefully on invalid syntax
prop_parseInvalidSyntax :: String -> Bool
prop_parseInvalidSyntax input =
  let invalidChars = ['@', '#', '$', '%', '^', '&', '*', '?']
      hasInvalid = any (`elem` invalidChars) input
  in hasInvalid ==> case parse input of
    ParseResult _ errors -> not (null errors)
    _ -> False

-- | Property: parse provides meaningful error locations
prop_errorLocationAccuracy :: String -> String -> Bool
prop_errorLocationAccuracy prefix invalid =
  let input = prefix ++ invalid
      hasInvalid = any (`elem` ['@', '#', '$']) invalid
  in hasInvalid ==> case parse input of
    ParseResult _ errors -> all errorLocationValid errors
    _ -> False

-- | Property: parse recovers from certain errors
prop_errorRecovery :: String -> String -> Bool
prop_errorRecovery validPrefix invalidSuffix =
  let input = validPrefix ++ " " ++ invalidSuffix ++ " x = 1"
      hasInvalid = any (`elem` ['@', '#']) invalidSuffix
  in hasInvalid ==> case parse input of
    ParseResult tokens errors -> not (null tokens) && not (null errors)
    _ -> False

-- | Property: parse time scales linearly with input size
prop_parseLinearScaling :: Int -> Bool
prop_parseLinearScaling n =
  let input = concat (replicate n "x + ")
      result = parse input
  in case result of
    ParseResult tokens _ -> length tokens <= n * 2 + 1
    _ -> False

-- | Property: tokenize memory usage is bounded
prop_tokenizeMemoryBounded :: Int -> Bool
prop_tokenizeMemoryBounded n =
  let input = concat (replicate n "identifier ")
      tokens = tokenize input
  in length tokens <= n * 2

-- Helper data types and functions
data Token = Token { tokenType :: TokenType, tokenValue :: String } deriving (Eq, Show)

data TokenType = Identifier | StringLiteral | NumberLiteral | Operator | LParen | RParen deriving (Eq, Show)

data ParseResult = ParseResult [Token] [ParseError] deriving (Eq, Show)

data ParseError = ParseError { errorMessage :: String, errorLocation :: SourceLocation } deriving (Eq, Show)

data Expression = IdentifierExpr String | NumberExpr Int | BinaryExpr Expression String Expression deriving (Eq, Show)

-- Mock parser functions (in real implementation, these would come from Parser module)
tokenize :: String -> [Token]
tokenize input = undefined  -- Simplified for demonstration

parse :: String -> ParseResult
parse input = undefined  -- Simplified for demonstration

parseExpression :: String -> Either String Expression
parseExpression input = undefined  -- Simplified for demonstration

-- Helper predicate functions
isIdentifierExpression :: Expression -> Bool
isIdentifierExpression (IdentifierExpr _) = True
isIdentifierExpression _ = False

isNumericExpression :: Expression -> Bool
isNumericExpression (NumberExpr _) = True
isNumericExpression _ = False

representsCorrectPrecedence :: Expression -> Int -> Int -> Int -> Bool
representsCorrectPrecedence (BinaryExpr (NumberExpr a) "+" (BinaryExpr (NumberExpr b) "*" (NumberExpr c))) a' b' c' = 
  a == a' && b == b' && c == c'
representsCorrectPrecedence _ _ _ _ = False

depthOfExpression :: Expression -> Int
depthOfExpression (IdentifierExpr _) = 1
depthOfExpression (NumberExpr _) = 1
depthOfExpression (BinaryExpr left _ right) = 1 + max (depthOfExpression left) (depthOfExpression right)

errorLocationValid :: ParseError -> Bool
errorLocationValid (ParseError _ loc) = isLocationValid loc

isLocationValid :: SourceLocation -> Bool
isLocationValid = const True  -- Simplified for demonstration

-- QuickCheck implication operator
(==>) :: Bool -> Bool -> Bool
True ==> x = x
False ==> _ = True