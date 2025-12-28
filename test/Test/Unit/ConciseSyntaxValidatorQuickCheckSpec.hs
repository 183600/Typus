module Test.Unit.ConciseSyntaxValidatorQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Property, (===), Arbitrary(..), Gen, oneof, choose, elements, listOf)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isAlphaNum, isAlpha, isDigit, isSpace)
import SyntaxValidator (SyntaxError(..), ErrorType(..), SyntaxValidator, validateSyntax)

-- | 简洁的QuickCheck测试，针对SyntaxValidator模块的基础功能
tests :: TestTree
tests =
  testGroup "Concise SyntaxValidator QuickCheck Tests"
    [ testGroup "Error type properties"
        [ testProperty "Error types are distinguishable" $
            \err1 err2 -> err1 === err2 || errType err1 /= errType err2
            
        , testProperty "Syntax errors preserve error type" $
            \errorType line msg -> 
            let error = SyntaxError errorType line msg ""
            in errType error === errorType
        ]
        
    , testGroup "Basic syntax validation"
        [ testProperty "Empty input produces no syntax errors" $
            \_ -> null (validateSyntax "")
            
        , testProperty "Whitespace-only input produces no syntax errors" $
            \ws -> all isSpace ws ==> null (validateSyntax ws)
            
        , testProperty "Valid identifiers pass validation" $
            \ident -> isValidIdentifier ident ==> 
                null (validateSyntax ident)
                
        , testProperty "Mismatched braces produce errors" $
            \open close -> not (isMatchingPair open close) ==> 
                not (null (validateSyntax (open ++ "content" ++ close)))
        ]
        
    , testGroup "String literal validation"
        [ testProperty "Properly quoted strings are valid" $
            \content -> let quoted = "\"" ++ content ++ "\""
            in not (hasUnbalancedQuotes quoted)
            
        , testProperty "Unbalanced quotes produce errors" $
            \content -> 
            let unbalanced = "\"" ++ content
            in hasUnbalancedQuotes unbalanced
        ]
        
    , testGroup "Bracket matching properties"
        [ testProperty "Matching brackets are balanced" $
            \content -> 
            let brackets = "(" ++ content ++ ")"
            in areBracketsBalanced brackets
            
        , testProperty "Nested matching brackets are balanced" $
            \depth -> 
            let nested = replicate (min depth 10) '(' ++ "content" ++ replicate (min depth 10) ')'
            in areBracketsBalanced nested
            
        , testProperty "Mismatched brackets are unbalanced" $
            \content -> 
            let mismatched = "(" ++ content ++ "]"
            in not (areBracketsBalanced mismatched)
        ]
        
    , testGroup "Comment validation"
        [ testProperty "Properly closed comments are valid" $
            \content -> 
            let comment = "/*" ++ content ++ "*/"
            in not (hasUnclosedComments comment)
            
        , testProperty "Unclosed comments produce errors" $
            \content -> 
            let unclosed = "/*" ++ content
            in hasUnclosedComments unclosed
        ]
        
    , testGroup "Statement validation"
        [ testProperty "Simple statements end properly" $
            \stmt -> not (null stmt) ==> 
                let withSemicolon = stmt ++ ";"
                in endsProperly withSemicolon
                
        , testProperty "Incomplete statements are detected" $
            \stmt -> not (null stmt) ==> 
                not (isCompleteStatement stmt) || endsProperly stmt
        ]
    ]

-- Helper functions for testing
isValidIdentifier :: String -> Bool
isValidIdentifier [] = False
isValidIdentifier (c:cs) = isAlpha c && all isAlphaNum cs

isMatchingPair :: Char -> Char -> Bool
isMatchingPair '(' ')' = True
isMatchingPair '[' ']' = True
isMatchingPair '{' '}' = True
isMatchingPair _ _ = False

hasUnbalancedQuotes :: String -> Bool
hasUnbalancedQuotes = odd . countQuotes
  where
    countQuotes [] = 0
    countQuotes ('"':rest) = 1 + countQuotes (dropWhile (/= '"') rest)
    countQuotes (_:rest) = countQuotes rest

areBracketsBalanced :: String -> Bool
areBracketsBalanced = checkBalance []
  where
    checkBalance [] [] = True
    checkBalance _ [] = False
    checkBalance stack (c:rest)
      | c `elem` "([{" = checkBalance (c:stack) rest
      | c `elem` ")]" = case stack of
                           [] -> False
                           (top:remaining) -> isMatchingPair top c && checkBalance remaining rest
      | otherwise = checkBalance stack rest

hasUnclosedComments :: String -> Bool
hasUnclosedComments = hasUnclosed 0
  where
    hasUnclosed _ [] = False
    hasUnclosed depth ('/':'*':rest) = hasUnclosed (depth + 1) rest
    hasUnclosed depth ('*':'/':rest) 
      | depth > 0 = hasUnclosed (depth - 1) rest
      | otherwise = hasUnclosed depth rest
    hasUnclosed depth (_:rest) = hasUnclosed depth rest

endsProperly :: String -> Bool
endsProperly [] = False
endsProperly s = last s `elem` ";{}"

isCompleteStatement :: String -> Bool
isCompleteStatement s = not (null s) && 
                       (hasKeyword s "if" || hasKeyword s "for" || hasKeyword s "while" || endsProperly s)

hasKeyword :: String -> String -> Bool
hasKeyword s keyword = keyword `isInfixOf` s

-- Generate test data
instance Arbitrary ErrorType where
  arbitrary = oneof
    [ return MissingBrace
    , return MissingParenthesis
    , return MissingBracket
    , return UnclosedString
    , return UnclosedComment
    , return InvalidIdentifier
    , return InvalidTypeDeclaration
    , return InvalidFunctionDeclaration
    , return InvalidImport
    , return InvalidStatement
    , return UnterminatedBlock
    , return InvalidOperator
    , return MissingSemicolon
    , return UnexpectedToken
    , return MissingPackageDeclaration
    , return DuplicateDeclaration
    , return InvalidBlockStructure
    , return UndeclaredVariable
    , return SyntaxWarning
    ]

instance Arbitrary SyntaxError where
  arbitrary = do
    errorType <- arbitrary
    line <- choose (1, 1000)
    message <- listOf $ elements ['a'..'z']
    context <- listOf $ elements ['a'..'z']
    return $ SyntaxError errorType line message context

instance Arbitrary String where
  arbitrary = oneof
    [ return ""
    , listOf $ elements ['a'..'z']
    , listOf $ elements ['A'..'Z']
    , listOf $ elements "0123456789"
    , listOf $ elements "(){}[];, ."
    , listOf $ elements " \t\n\r"
    ]

-- Helper property function
property :: Bool -> Property
property = id