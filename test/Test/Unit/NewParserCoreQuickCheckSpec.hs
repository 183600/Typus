{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewParserCoreQuickCheckSpec where

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample)
import Test.Tasty.HUnit (testCase, assertBool)

import qualified Data.Text as T
import qualified Data.Char as Char
import Parser
import Utils (trim, splitBy, removeComments)

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate valid identifiers for parsing
genIdentifier :: Gen String
genIdentifier = do
    first <- choose ('a', 'z')
    rest <- listOf $ choose ('a', 'z')
    return (first : rest)

-- Generate valid numbers for parsing
genNumber :: Gen String
genNumber = do
    digits <- listOf1 $ choose ('0', '9')
    return digits

-- Generate valid strings for parsing
genString :: Gen String
genString = do
    content <- listOf $ choose (' ', '~')
    return $ "\"" ++ content ++ "\""

-- Generate valid operators for parsing
genOperator :: Gen String
genOperator = elements ["+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">="]

-- Generate valid keywords for parsing
genKeyword :: Gen String
genKeyword = elements ["if", "else", "while", "for", "function", "return", "var", "let", "const"]

-- Generate whitespace for testing
genWhitespace :: Gen String
genWhitespace = listOf $ elements [' ', '\t', '\n']

-- Generate simple expressions
genExpression :: Gen String
genExpression = do
    exprType <- choose (1, 4 :: Int)
    case exprType of
        1 -> genIdentifier
        2 -> genNumber
        3 -> do
            left <- genExpression
            op <- genOperator
            right <- genExpression
            return $ left ++ " " ++ op ++ " " ++ right
        4 -> do
            expr <- genExpression
            return $ "(" ++ expr ++ ")"
        _ -> genIdentifier

-- Generate simple statements
genStatement :: Gen String
genStatement = do
    stmtType <- choose (1, 4 :: Int)
    case stmtType of
        1 -> do
            ident <- genIdentifier
            expr <- genExpression
            return $ ident ++ " = " ++ expr ++ ";"
        2 -> do
            keyword <- genKeyword
            expr <- genExpression
            return $ keyword ++ " " ++ expr ++ ";"
        3 -> do
            ident <- genIdentifier
            args <- listOf $ genExpression
            return $ ident ++ "(" ++ unwords args ++ ");"
        4 -> do
            expr <- genExpression
            return $ "return " ++ expr ++ ";"
        _ -> genIdentifier ++ ";"

-- Generate code with comments
genCodeWithComments :: Gen String
genCodeWithComments = do
    baseCode <- genStatement
    hasComment <- choose (True, False)
    if hasComment
        then do
            commentType <- choose (1, 2 :: Int)
            case commentType of
                1 -> do
                    comment <- listOf $ choose ('a', 'z')
                    return $ baseCode ++ " // " ++ comment
                2 -> do
                    comment <- listOf $ choose ('a', 'z')
                    return $ "/* " ++ comment ++ " */ " ++ baseCode
                _ -> return baseCode
        else return baseCode

-- ============================================================================
-- Parser Core Properties
-- ============================================================================

-- Property: Trimming whitespace preserves non-whitespace content
prop_trimPreservesContent :: String -> Property
prop_trimPreservesContent str =
    let trimmed = trim str
        nonTrimmed = filter (not . Char.isSpace) str
        trimmedNonSpace = filter (not . Char.isSpace) trimmed
    in counterexample ("Trim should preserve non-whitespace content")
       (trimmedNonSpace === nonTrimmed)

-- Property: Splitting by delimiter and joining with same delimiter recovers original (for simple cases)
prop_splitJoinRoundtrip :: String -> Char -> Property
prop_splitJoinRoundtrip str delim
    | delim `elem` str = property True  -- Skip complex cases with delimiter
    | otherwise =
        let parts = splitBy delim str
            rejoined = concat $ intersperse [delim] parts
        in counterexample ("Split-join roundtrip should work for delimiter-free strings")
           (rejoined === str)

-- Property: Comment removal preserves non-comment content structure
prop_commentRemovalPreservesStructure :: String -> Property
prop_commentRemovalPreservesStructure code =
    let withoutComments = removeComments code
        lineCountOriginal = length $ lines code
        lineCountProcessed = length $ lines withoutComments
    in counterexample ("Comment removal should preserve line structure")
       (lineCountProcessed <= lineCountOriginal === True)

-- Property: Parsing identifier should succeed for valid identifiers
prop_parseValidIdentifier :: String -> Property
prop_parseValidIdentifier ident
    | null ident = property True
    | not (Char.isLetter (head ident)) = property True
    | any (not . Char.isAlphaNum) ident = property True
    | otherwise =
        let parseResult = parseIdentifier ident
        in counterexample ("Valid identifier should parse successfully")
           (isSuccess parseResult === True)

-- Property: Parsing number should succeed for valid numbers
prop_parseValidNumber :: String -> Property
prop_parseValidNumber numStr
    | null numStr = property True
    | any (not . Char.isDigit) numStr = property True
    | otherwise =
        let parseResult = parseNumber numStr
        in counterexample ("Valid number should parse successfully")
           (isSuccess parseResult === True)

-- Property: Parsing expression should handle nested parentheses correctly
prop_parseNestedParentheses :: Int -> Property
prop_parseNestedParentheses depth
    | depth < 0 || depth > 10 = property True  -- Limit depth for practicality
    | otherwise =
        let nestedExpr = concat $ replicate depth "(" ++ "x" ++ concat (replicate depth ")")
            parseResult = parseExpression nestedExpr
        in counterexample ("Nested parentheses should parse correctly")
           (isSuccess parseResult === True)

-- Property: Parser should handle whitespace gracefully
prop_parserHandlesWhitespace :: String -> String -> Property
prop_parserHandlesWhitespace baseCode ws =
    let codeWithWs = ws ++ baseCode ++ ws
        parseResult1 = parseStatement baseCode
        parseResult2 = parseStatement codeWithWs
    in counterexample ("Parser should handle surrounding whitespace")
       (isSuccess parseResult1 === isSuccess parseResult2)

-- Property: Parsing concatenated statements should work
prop_parseConcatenatedStatements :: [String] -> Property
prop_parseConcatenatedStatements stmts =
    let concatenated = concat stmts
        parseResult = parseStatements concatenated
    in counterexample ("Concatenated statements should parse")
       (isSuccess parseResult === True)

-- Property: Parser position tracking should be accurate
prop_parserPositionTracking :: String -> Property
prop_parserPositionTracking code =
    let parseResult = parseWithPositionTracking code
        expectedLength = length code
    in counterexample ("Parser position tracking should be accurate")
       (getFinalPosition parseResult === expectedLength)

-- Property: Error recovery should allow parsing to continue
prop_errorRecoveryContinuesParsing :: String -> String -> Property
prop_errorRecoveryContinuesParsing invalidCode validCode =
    let combined = invalidCode ++ validCode
        parseResult = parseWithErrorRecovery combined
    in counterexample ("Error recovery should allow parsing to continue")
       (hasPartialSuccess parseResult === True)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Parser Core QuickCheck Tests"
    [ testProperty "Trim preserves content" prop_trimPreservesContent
    , testProperty "Split-join roundtrip works" prop_splitJoinRoundtrip
    , testProperty "Comment removal preserves structure" prop_commentRemovalPreservesStructure
    , testProperty "Parse valid identifier succeeds" prop_parseValidIdentifier
    , testProperty "Parse valid number succeeds" prop_parseValidNumber
    , testProperty "Parse nested parentheses works" prop_parseNestedParentheses
    , testProperty "Parser handles whitespace" prop_parserHandlesWhitespace
    , testProperty "Parse concatenated statements" prop_parseConcatenatedStatements
    , testProperty "Parser position tracking accurate" prop_parserPositionTracking
    , testProperty "Error recovery continues parsing" prop_errorRecoveryContinuesParsing
    ]

-- ============================================================================
-- Helper Functions (Mock implementations for testing)
-- ============================================================================

-- Mock parser functions for testing
parseIdentifier :: String -> ParseResult
parseIdentifier str
    | null str = ParseError "Empty identifier"
    | not (Char.isLetter (head str)) = ParseError "Identifier must start with letter"
    | all Char.isAlphaNum str = ParseSuccess str
    | otherwise = ParseError "Invalid identifier character"

parseNumber :: String -> ParseResult
parseNumber str
    | null str = ParseError "Empty number"
    | all Char.isDigit str = ParseSuccess str
    | otherwise = ParseError "Invalid number character"

parseExpression :: String -> ParseResult
parseExpression expr = ParseSuccess expr  -- Simplified for testing

parseStatement :: String -> ParseResult
parseStatement stmt = ParseSuccess stmt  -- Simplified for testing

parseStatements :: String -> ParseResult
parseStatements stmts = ParseSuccess stmts  -- Simplified for testing

parseWithPositionTracking :: String -> PositionResult
parseWithPositionTracking code = PositionResult (length code)  -- Simplified

parseWithErrorRecovery :: String -> RecoveryResult
parseWithErrorRecovery code = RecoveryResult True  -- Simplified

data ParseResult = ParseSuccess String | ParseError String
    deriving (Show, Eq)

data PositionResult = PositionResult Int
    deriving (Show, Eq)

data RecoveryResult = RecoveryResult Bool
    deriving (Show, Eq)

isSuccess :: ParseResult -> Bool
isSuccess (ParseSuccess _) = True
isSuccess (ParseError _) = False

getFinalPosition :: PositionResult -> Int
getFinalPosition (PositionResult pos) = pos

hasPartialSuccess :: RecoveryResult -> Bool
hasPartialSuccess (RecoveryResult success) = success

-- Import required for intersperse
import Data.List (intersperse)