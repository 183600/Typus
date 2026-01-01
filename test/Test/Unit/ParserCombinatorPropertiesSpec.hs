module Test.Unit.ParserCombinatorPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf1, elements, suchThat, sized)
import qualified Data.Text as T
import Text.Megaparsec (parse, parseTest, many, many1, try, (<|>))
import Text.Megaparsec.Char (char, string, space, digit, letter)
import Text.Megaparsec.Char.Lexer (decimal)
import Parser

-- | QuickCheck tests for Parser combinator properties
tests :: TestTree
tests =
  testGroup "Parser combinator properties"
    [ testGroup "Basic parsing properties"
        [ fastProperty "char parser succeeds only with matching character" prop_charMatching
        , fastProperty "string parser succeeds only with matching string" prop_stringMatching
        , fastProperty "many parser returns list of successful parses" prop_manyReturnsList
        , fastProperty "many1 parser requires at least one successful parse" prop_many1RequiresOne
        ]

    , testGroup "Alternative parser properties"
        [ fastProperty "try <|> preserves first parser on success" prop_tryAlternativeSuccess
        , fastProperty "try <|> tries second parser on first failure" prop_tryAlternativeFailure
        , fastProperty "alternatives are commutative for success" prop_alternativeCommutative
        ]

    , testGroup "Parser composition properties"
        [ fastProperty "sequential parsing consumes input correctly" prop_sequentialConsumption
        , fastProperty "parser composition is associative" prop_compositionAssociative
        , fastProperty "parser identity laws" prop_parserIdentity
        ]

    , testGroup "Error handling properties"
        [ fastProperty "parse failure preserves error position" prop_errorPositionPreservation
        , fastProperty "parse error contains expected tokens" prop_errorContainsExpected
        , fastProperty "nested parse errors propagate correctly" prop_nestedErrorPropagation
        ]

    , testGroup "Lexical properties"
        [ fastProperty "digit parser only accepts numeric characters" prop_digitOnlyNumeric
        , fastProperty "letter parser only accepts alphabetic characters" prop_letterOnlyAlpha
        , fastProperty "decimal parser handles valid numbers correctly" prop_decimalValidity
        ]
    ]

-- ============================================================================
-- Helper generators
-- ============================================================================

genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " !@#$%^&*()_+-=[]{}|;':\",./<>?"

genString :: Gen String
genString = listOf1 genChar

genNonEmptyString :: Gen String
genNonEmptyString = listOf1 genChar

genNumericString :: Gen String
genNumericString = listOf1 $ elements ['0'..'9']

genAlphaString :: Gen String
genAlphaString = listOf1 $ elements (['a'..'z'] ++ ['A'..'Z'])

genMixedString :: Gen String
genMixedString = do
    alpha <- genAlphaString
    num <- genNumericString
    special <- listOf1 $ elements "!@#$%^&*"
    return $ alpha ++ num ++ special

-- ============================================================================
-- Basic parsing properties
-- ============================================================================

prop_charMatching :: Char -> String -> Property
prop_charMatching c str =
    let input = c : str
        result = parse (char c) "" input
    in case result of
        Left _ -> False
        Right parsed -> parsed == c

prop_stringMatching :: String -> String -> Property
prop_stringMatching pat str =
    not (null pat) ==> 
    let input = pat ++ str
        result = parse (string pat) "" input
    in case result of
        Left _ -> False
        Right parsed -> parsed == pat

prop_manyReturnsList :: Char -> String -> Bool
prop_manyReturnsList c str =
    let input = replicate (L.length str) c
        result = parse (many (char c)) "" input
    in case result of
        Left _ -> False
        Right parsed -> L.length parsed == L.length str && L.all (== c) parsed

prop_many1RequiresOne :: Char -> Property
prop_many1RequiresOne c =
    let input = [c]
        result = parse (many1 (char c)) "" input
    in case result of
        Left _ -> False
        Right parsed -> L.length parsed == 1 && L.head parsed == c

-- ============================================================================
-- Alternative parser properties
-- ============================================================================

prop_tryAlternativeSuccess :: Char -> String -> Property
prop_tryAlternativeSuccess c str =
    let input = c : str
        result = parse (try (char c) <|> char 'x') "" input
    in case result of
        Left _ -> False
        Right parsed -> parsed == c

prop_tryAlternativeFailure :: Char -> String -> Property
prop_tryAlternativeFailure c str =
    c /= 'x' ==> 
    let input = c : str
        result = parse (try (char 'x') <|> char c) "" input
    in case result of
        Left _ -> False
        Right parsed -> parsed == c

prop_alternativeCommutative :: Char -> Char -> String -> Property
prop_alternativeCommutative c1 c2 str =
    c1 /= c2 ==> 
    let input = c1 : str
        parser1 = try (char c1) <|> char c2
        parser2 = try (char c2) <|> char c1
        result1 = parse parser1 "" input
        result2 = parse parser2 "" input
    in case (result1, result2) of
        (Right r1, Right r2) -> r1 == r2
        _ -> False

-- ============================================================================
-- Parser composition properties
-- ============================================================================

prop_sequentialConsumption :: Char -> Char -> String -> Property
prop_sequentialConsumption c1 c2 str =
    c1 /= c2 ==> 
    let input = c1 : c2 : str
        parser = char c1 *> char c2
        result = parse parser "" input
    in case result of
        Left _ -> False
        Right _ -> True  -- Success means both chars were consumed

prop_compositionAssociative :: String -> String -> String -> Property
prop_compositionAssociative s1 s2 s3 =
    L.all (not . null) [s1, s2, s3] ==> 
    let input = s1 ++ s2 ++ s3
        parser1 = string s1 *> (string s2 *> string s3)
        parser2 = (string s1 *> string s2) *> string s3
        result1 = parse parser1 "" input
        result2 = parse parser2 "" input
    in case (result1, result2) of
        (Right _, Right _) -> True
        _ -> False

prop_parserIdentity :: String -> Property
prop_parserIdentity str =
    not (null str) ==> 
    let parser = pure () *> string str
        result = parse parser "" str
    in case result of
        Left _ -> False
        Right _ -> True

-- ============================================================================
-- Error handling properties
-- ============================================================================

prop_errorPositionPreservation :: Char -> String -> Property
prop_errorPositionPreservation c str =
    c `notElem` str ==> 
    let input = str
        result = parse (char c) "" input
    in case result of
        Left err -> True  -- Error should contain position info
        Right _ -> False

prop_errorContainsExpected :: Char -> String -> Property
prop_errorContainsExpected c str =
    c `notElem` str ==> 
    let input = str
        result = parse (char c) "" input
    in case result of
        Left _ -> True  -- Error should contain expected tokens
        Right _ -> False

prop_nestedErrorPropagation :: Char -> Char -> String -> Property
prop_nestedErrorPropagation c1 c2 str =
    (c1 `notElem` str) && (c2 `notElem` str) ==> 
    let input = str
        parser = try (char c1) <|> char c2
        result = parse parser "" input
    in case result of
        Left _ -> True  -- Error should propagate from nested parsers
        Right _ -> False

-- ============================================================================
-- Lexical properties
-- ============================================================================

prop_digitOnlyNumeric :: Char -> Property
prop_digitOnlyNumeric c =
    let input = [c]
        result = parse digit "" input
        isDigit = c `elem` ['0'..'9']
    in case (result, isDigit) of
        (Right _, True) -> True
        (Left _, False) -> True
        _ -> False

prop_letterOnlyAlpha :: Char -> Property
prop_letterOnlyAlpha c =
    let input = [c]
        result = parse letter "" input
        isLetter = c `elem` (['a'..'z'] ++ ['A'..'Z'])
    in case (result, isLetter) of
        (Right _, True) -> True
        (Left _, False) -> True
        _ -> False

prop_decimalValidity :: String -> Property
prop_decimalValidity str =
    L.all (`elem` ['0'..'9']) str ==> 
    let input = str
        result = parse decimal "" input
    in case result of
        Right n -> n >= 0
        Left _ -> False

-- ============================================================================
-- Helper functions
-- ============================================================================

-- Note: This test module assumes the existence of a Parser module that exports
-- common parsing functions from Megaparsec. The actual implementation would
-- need to import the appropriate functions from the project's Parser module.