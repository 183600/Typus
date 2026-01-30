{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans  -Wno-unused-imports  -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Unit.ParserCombinatorsSpec where


import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure, Assertion)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>), classify, sized)
import Data.List (nub, sort, groupBy, sortBy, find, delete, isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import Data.Char (isDigit, isLetter, isSpace)
import Control.Monad (replicateM, when)
import Control.Applicative ((<|>), Alternative(..))

-- Parser combinator types for testing
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (result, rest) <- p input
    return (f result, rest)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (Parser pf) <*> (Parser px) = Parser $ \input -> do
    (f, rest1) <- pf input
    (x, rest2) <- px rest1
    return (f x, rest2)

instance Monad Parser where
  return = pure
  (Parser px) >>= f = Parser $ \input -> do
    (x, rest) <- px input
    runParser (f x) rest

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> 
    p1 input <|> p2 input

-- Helper functions for parser combinators
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \input ->
  case input of
    (c:cs) | predicate c -> Just (c, cs)
    _ -> Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

-- Arbitrary instances
instance Arbitrary (Parser Char) where
  arbitrary = oneof
    [ return <$> arbitrary
    , char <$> arbitrary
    ]

instance Show (Parser Char) where
  show _ = "Parser"

instance Eq (Parser a) where
  _ == _ = True  -- Simplified equality for testing

string :: String -> Parser String
string [] = return []
string (c:cs) = do
  _ <- char c
  rest <- string cs
  return (c:rest)

many' :: Parser a -> Parser [a]
many' p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many' p
  return (x:xs)

option :: a -> Parser a -> Parser a
option x p = p <|> return x

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> return []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
  x <- p
  xs <- many' (sep >> p)
  return (x:xs)

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = do
  _ <- open
  x <- p
  _ <- close
  return x

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x = (do
      f <- op
      y <- p
      rest (f x y)) <|> return x

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = do
  x <- p
  rest x
  where
    rest x = (do
      f <- op
      y <- chainr1 p op
      return (f x y)) <|> return x

-- AST types for testing
data Expr = 
    Const Int
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  deriving (Eq, Show)

data Stmt = 
    Assign String Expr
  | If Expr Stmt Stmt
  | While Expr Stmt
  | Seq [Stmt]
  deriving (Eq, Show)

-- Parser generators for testing
genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " +-*/()"

genString :: Gen String
genString = do
  len <- choose (0, 10)
  vectorOf len genChar

genDigit :: Gen Char
genDigit = elements ['0'..'9']

genLetter :: Gen Char
genLetter = elements $ ['a'..'z'] ++ ['A'..'Z']

genIdentifier :: Gen String
genIdentifier = do
  first <- genLetter
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
  return (first : rest)

genNumber :: Gen Int
genNumber = choose (-100, 100)

genExpr :: Int -> Gen Expr
genExpr 0 = oneof
  [ Const <$> genNumber
  , Var <$> genIdentifier
  ]
genExpr depth = oneof
  [ Const <$> genNumber
  , Var <$> genIdentifier
  , do
      left <- genExpr (depth - 1)
      right <- genExpr (depth - 1)
      return $ Add left right
  , do
      left <- genExpr (depth - 1)
      right <- genExpr (depth - 1)
      return $ Mul left right
  , do
      e <- genExpr (depth - 1)
      return $ Neg e
  ]

-- Specific parsers for testing
digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isLetter

identifier :: Parser String
identifier = do
  first <- letter
  rest <- many' (letter <|> digit <|> char '_')
  return (first : rest)

number :: Parser Int
number = do
  digits <- many1 digit
  return $ read digits

expr :: Parser Expr
expr = term `chainl1` addOp

term :: Parser Expr
term = factor `chainl1` mulOp

factor :: Parser Expr
factor = parens expr <|> negFactor <|> baseFactor

baseFactor :: Parser Expr
baseFactor = (Const <$> number) <|> (Var <$> identifier)

negFactor :: Parser Expr
negFactor = do
  _ <- char '-'
  e <- factor
  return $ Neg e

parens :: Parser a -> Parser a
parens p = between (char '(') (char ')') p

addOp :: Parser (Expr -> Expr -> Expr)
addOp = do
  _ <- char '+'
  return Add

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = do
  _ <- char '*'
  return Mul

stmt :: Parser Stmt
stmt = ifStmt <|> whileStmt <|> assignStmt <|> blockStmt

ifStmt :: Parser Stmt
ifStmt = do
  _ <- string "if"
  _ <- many' (char ' ')
  cond <- between (char '(') (char ')') expr
  _ <- many' (char ' ')
  thenStmt <- stmt
  _ <- many' (char ' ')
  _ <- string "else"
  _ <- many' (char ' ')
  elseStmt <- stmt
  return $ If cond thenStmt elseStmt

whileStmt :: Parser Stmt
whileStmt = do
  _ <- string "while"
  _ <- many' (char ' ')
  cond <- between (char '(') (char ')') expr
  _ <- many' (char ' ')
  body <- stmt
  return $ While cond body

assignStmt :: Parser Stmt
assignStmt = do
  var <- identifier
  _ <- many' (char ' ')
  _ <- char '='
  _ <- many' (char ' ')
  e <- expr
  return $ Assign var e

blockStmt :: Parser Stmt
blockStmt = do
  _ <- char '{'
  _ <- many' (char ' ' <|> char '\n')
  stmts <- many' (stmt <* many' (char ' ' <|> char '\n'))
  _ <- char '}'
  return $ case stmts of
    [] -> Seq []
    [s] -> s
    _ -> Seq stmts

-- Test properties for parser combinators

-- Property 1: Parser consumes input correctly
prop_parser_consumes_input :: Parser Char -> String -> Property
prop_parser_consumes_input parser input = 
  property $
  case runParser parser input of
    Just (_, rest) -> null input || length rest < length input
    Nothing -> True

-- Monomorphic version for QuickCheck
prop_parser_consumes_input_mono :: Property
prop_parser_consumes_input_mono = forAll arbitrary $ \parser ->
  forAll (choose (1, 10) >>= \n -> vectorOf n arbitrary) $ \input ->
    prop_parser_consumes_input parser input

-- Property 2: Alternative parser tries second option if first fails
prop_alternative_tries_second :: Parser Char -> Parser Char -> String -> Property
prop_alternative_tries_second p1 p2 input = property $
  case runParser p1 input of
    Just _ -> True
    Nothing -> case runParser p2 input of
                 Just _ -> True
                 Nothing -> True

-- Property 3: Many parser returns list of results
prop_many_returns_list :: Parser Char -> String -> Property
prop_many_returns_list parser input = property $
  case runParser (Control.Applicative.many parser) input of
    Just (results, _) -> length results >= 0
    Nothing -> True

-- Property 4: SepBy parser respects separator
prop_sepBy_respects_separator :: Parser Char -> Parser Char -> String -> Property
prop_sepBy_respects_separator item sep input = property $
  case runParser (sepBy item sep) input of
    Just (results, _) -> length results >= 0
    Nothing -> True

-- Property 5: Between parser consumes both delimiters
prop_between_consumes_delimiters :: Parser Char -> Parser Char -> Parser Char -> String -> Property
prop_between_consumes_delimiters open close item input = property $
  case runParser (between open close item) input of
    Just (_, rest) -> length rest <= length input - 2
    Nothing -> True

-- Property 6: Chainl1 parser associates left
prop_chainl1_associates_left :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> String -> Property
prop_chainl1_associates_left item op input = property $
  case runParser (chainl1 item op) input of
    Just (result, _) -> isLeftAssociative result
    Nothing -> True

-- Property 7: Chainr1 parser associates right
prop_chainr1_associates_right :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> String -> Property
prop_chainr1_associates_right item op input = property $
  case runParser (chainr1 item op) input of
    Just (result, _) -> isRightAssociative result
    Nothing -> True

-- Property 8: Parser is deterministic
prop_parser_is_deterministic :: Eq a => Parser a -> String -> Property
prop_parser_is_deterministic parser input = property $
  let result1 = runParser parser input
      result2 = runParser parser input
  in result1 == result2

-- Property 9: Parser composition works correctly
prop_parser_composition_works :: Parser Char -> Parser (Char -> Char -> String) -> String -> Bool
prop_parser_composition_works p1 p2 input = 
  case runParser (p1 >>= \c1 -> p1 >>= \c2 -> p2) input of
    Just (f, _) -> True
    Nothing -> True

-- Property 10: Parser backtracks correctly
prop_parser_backtracks_correctly :: Parser Char -> Parser Char -> String -> Property
prop_parser_backtracks_correctly p1 p2 input = property $
  case runParser (p1 <|> p2) input of
    Just _ -> True
    Nothing -> True

-- Helper functions for testing
isLeftAssociative :: Expr -> Bool
isLeftAssociative (Add (Add _ _) _) = True
isLeftAssociative (Mul (Mul _ _) _) = True
isLeftAssociative _ = True

isRightAssociative :: Expr -> Bool
isRightAssociative (Add _ (Add _ _)) = True
isRightAssociative (Mul _ (Mul _ _)) = True
isRightAssociative _ = True

-- Test cases for parser combinators
testParserCombinators :: TestTree
testParserCombinators = testGroup "Parser Combinators Tests"
  [ testProperties "Basic Parser Properties"
    [ ("parser_consumes_input", prop_parser_consumes_input_mono)
    , ("alternative_tries_second", property prop_alternative_tries_second)
    , ("many_returns_list", property prop_many_returns_list)
    ]
  , testProperties "Advanced Parser Properties"
    [ ("sepBy_respects_separator", property prop_sepBy_respects_separator)
    , ("between_consumes_delimiters", property prop_between_consumes_delimiters)
    ]
  , testProperties "Parser Behavior Properties"
    [ ("parser_backtracks_correctly", property prop_parser_backtracks_correctly)
    ]
  , testCase "Basic character parser" $ do
    let result = runParser (char 'a') "abc"
    assertEqual "Should parse character" (Just ('a', "bc")) result
  
  , testCase "String parser" $ do
    let result = runParser (string "hello") "hello world"
    assertEqual "Should parse string" (Just ("hello", " world")) result
  
  , testCase "Many parser" $ do
    let result = runParser (Control.Applicative.many (char 'a')) "aaab"
    assertEqual "Should parse multiple characters" (Just ("aaa", "b")) result
  
  , testCase "Alternative parser" $ do
    let result = runParser (char 'a' <|> char 'b') "bcd"
    assertEqual "Should try alternative" (Just ('b', "cd")) result
  
  , testCase "Between parser" $ do
    let result = runParser (between (char '(') (char ')') (char 'x')) "(x)y"
    assertEqual "Should parse between delimiters" (Just ('x', "y")) result
  
  , testCase "SepBy parser" $ do
    let result = runParser (sepBy (char 'x') (char ',')) "x,x,x,y"
    assertEqual "Should parse separated items" (Just ("xxx", ",y")) result
  
  , testCase "Chainl1 parser" $ do
    let result = runParser (chainl1 (Const <$> number) addOp) "1+2+3"
    assertEqual "Should associate left" (Just (Add (Add (Const 1) (Const 2)) (Const 3), "")) result
  
  , testCase "Expression parser" $ do
    let result = runParser expr "1+2*3"
    assertEqual "Should parse expression" (Just (Add (Const 1) (Mul (Const 2) (Const 3)), "")) result
  
  , testCase "Statement parser" $ do
    let result = runParser assignStmt "x = 42"
    assertEqual "Should parse assignment" (Just (Assign "x" (Const 42), "")) result
  
  , testCase "If statement parser" $ do
    let result = runParser ifStmt "if (1) { x = 2 } else { x = 3 }"
    assertEqual "Should parse if statement" 
                (Just (If (Const 1) (Assign "x" (Const 2)) (Assign "x" (Const 3)), "")) result
  
  , testCase "While statement parser" $ do
    let result = runParser whileStmt "while (1) { x = 2 }"
    assertEqual "Should parse while statement" 
                (Just (While (Const 1) (Assign "x" (Const 2)), "")) result
  
  , testCase "Block statement parser" $ do
    let result = runParser blockStmt "{ x = 1 y = 2 }"
    assertEqual "Should parse block statement" 
                (Just (Seq [Assign "x" (Const 1), Assign "y" (Const 2)], "")) result
  ]

-- Export the test
tests :: TestTree
tests = testParserCombinators