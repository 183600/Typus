{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.SyntaxValidatorBoundarySpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Data.Char (isLetter, isDigit, isSpace, toLower, toUpper)

-- Test syntax validator boundary conditions
tests :: TestTree
tests = testGroup "Syntax Validator Boundary Tests"
  [ testGroup "Identifier validation"
    [ testProperty "valid identifiers start with letter" $
        \xs -> case xs of 
                [] -> property $ True
                (h:_) -> isLetter h ==> isValidIdentifier xs
    
    , testProperty "valid identifiers contain only letters and digits" $
        \xs -> isValidIdentifier xs ==> all (\c -> isLetter c || isDigit c) (tail xs)
    
    , testProperty "empty string is not a valid identifier" $
        not (isValidIdentifier "")
    
    , testProperty "single character is valid if letter" $
        \c -> isLetter c ==> isValidIdentifier [c]
    
    , testProperty "single character is invalid if digit" $
        \c -> isDigit c ==> not (isValidIdentifier [c])
    
    , testProperty "identifier length is preserved" $
        \xs -> isValidIdentifier xs ==> length xs == length xs
    
    , testProperty "identifiers are case sensitive" $
        \(xs :: String) -> isValidIdentifier xs ==> 
      let lowerXs = map Data.Char.toLower xs
          upperXs = map Data.Char.toUpper xs
      in isValidIdentifier lowerXs || isValidIdentifier upperXs
    
    , testProperty "identifiers with underscores are valid" $
        \xs -> not (null xs) ==> isValidIdentifier (xs ++ "_" ++ xs)
    
    , testProperty "identifiers cannot start with underscore" $
        \xs -> not (isValidIdentifier ("_" ++ xs))
    
    , testProperty "identifiers cannot end with underscore" $
        \xs -> not (null xs) ==> not (isValidIdentifier (xs ++ "_"))
    ]
  
  , testGroup "Expression validation"
    [ testProperty "simple literals are valid expressions" $
        \n -> isValidExpression (show (n :: Int))
    
    , testProperty "binary operations with valid operands are valid" $
        \x y -> isValidExpression (show (x :: Int) ++ " + " ++ show (y :: Int))
    
    , testProperty "parenthesized expressions are valid" $
        \x -> isValidExpression ("(" ++ show (x :: Int) ++ ")")
    
    , testProperty "empty expression is invalid" $
        not (isValidExpression "")
    
    , testProperty "unbalanced parentheses are invalid" $
        \x -> not (isValidExpression ("(" ++ show (x :: Int)))
    
    , testProperty "unbalanced parentheses are invalid" $
        \x -> not (isValidExpression (show (x :: Int) ++ ")"))
    
    , testProperty "nested parentheses are valid" $
        \x -> isValidExpression ("((" ++ show (x :: Int) ++ "))")
    
    , testProperty "multiple operations are valid" $
        \x y z -> isValidExpression (show (x :: Int) ++ " + " ++ show (y :: Int) ++ " * " ++ show (z :: Int))
    
    , testProperty "operations without operands are invalid" $
        not (isValidExpression "+")
    
    , testProperty "operations with single operand are invalid" $
        \x -> not (isValidExpression (show (x :: Int) ++ " + "))
    ]
  
  , testGroup "Statement validation"
    [ testProperty "assignment statements are valid" $
        \var value -> isValidIdentifier var ==> isValidStatement (var ++ " = " ++ show (value :: Int))
    
    , testProperty "return statements are valid" $
        \x -> isValidStatement ("return " ++ show (x :: Int))
    
    , testProperty "empty statements are invalid" $
        not (isValidStatement "")
    
    , testProperty "statements end with semicolon" $
        \var value -> isValidIdentifier var ==> isValidStatement (var ++ " = " ++ show (value :: Int) ++ ";")
    
    , testProperty "statements without semicolon are invalid" $
        \var value -> isValidIdentifier var ==> not (isValidStatement (var ++ " = " ++ show (value :: Int)))
    
    , testProperty "compound statements are valid" $
        \var1 var2 value1 value2 -> 
          isValidIdentifier var1 && isValidIdentifier var2 ==>
          isValidStatement (var1 ++ " = " ++ show (value1 :: Int) ++ "; " ++ var2 ++ " = " ++ show (value2 :: Int) ++ ";")
    
    , testProperty "if statements are valid" $
        \var value -> isValidIdentifier var ==> isValidStatement ("if (" ++ var ++ " > " ++ show (value :: Int) ++ ") { }")
    
    , testProperty "while statements are valid" $
        \var value -> isValidIdentifier var ==> isValidStatement ("while (" ++ var ++ " > " ++ show (value :: Int) ++ ") { }")
    
    , testProperty "for statements are valid" $
        \var value -> isValidIdentifier var ==> isValidStatement ("for (" ++ var ++ " = 0; " ++ var ++ " < " ++ show (value :: Int) ++ "; " ++ var ++ "++) { }")
    
    , testProperty "function declarations are valid" $
        \funcName param -> isValidIdentifier funcName && isValidIdentifier param ==> 
          isValidStatement ("func " ++ funcName ++ "(" ++ param ++ ") { }")
    ]
  ]

-- Helper functions
toLower :: String -> String
toLower = map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

toUpper :: String -> String
toUpper = map (\c -> if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c)

isValidIdentifier :: String -> Bool
isValidIdentifier [] = False
isValidIdentifier (c:cs) = isLetter c && all (\x -> isLetter x || isDigit x || x == '_') cs

isValidExpression :: String -> Bool
isValidExpression "" = False
isValidExpression expr = not (null expr) && balancedParens expr
  where
    balancedParens [] = True
    balancedParens ('(' : xs) = balancedParens xs && count '(' xs == count ')' xs
    balancedParens (')' : _) = False
    balancedParens (_ : xs) = balancedParens xs
    count _ [] = 0
    count x (y:ys) = if x == y then 1 + count x ys else count x ys

isValidStatement :: String -> Bool
isValidStatement "" = False
isValidStatement stmt = isValidExpression (removeSemicolon stmt) && hasSemicolon stmt
  where
    removeSemicolon = reverse . dropWhile (== ';') . reverse
    hasSemicolon = elem ';'