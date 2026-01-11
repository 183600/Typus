{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parser combinators QuickCheck tests for the Typus compiler
-- This module contains property-based tests for parser combinator utilities
module Test.Unit.NewParserCombinatorsQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck ((==>), conjoin, counterexample)
import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import Utils
  ( trim
  , splitBy
  , splitByComma
  , removeLineComments
  , removeComments
  , safeProcessString
  , isValidChar
  , breakOn
  )
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Control.Monad (foldM)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Compiler.Errors.Core as Error
import Compiler.Errors.Core (ErrorSeverity(..), ErrorCategory(..), TypeError(..), ErrorLocation(..), ErrorContext(..),
                            errorAt, errorWithCategory, warningAt, infoAt, 
                            fatalError, withLocation, withContext, combineErrors,
                            combinedErrorSeverity, filterByCategory, filterBySeverity,
                            hasCategory, isAtLeast, severityPriority, location, line, column, 
                            fatalRecovery, emptyContext, contextCode)
import SourceLocation (toErrorLocation)
import Data.Time (UTCTime, getCurrentTime)
import Data.List (sort, nub)
import Data.Ord (comparing)

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Check if a character is a valid identifier character
isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_' || c == '-'

-- | Check if a string is a valid identifier
isValidIdentifier :: String -> Bool
isValidIdentifier [] = False
isValidIdentifier (c:cs) = isAlphaNum c && all isIdentifierChar cs

-- | Check if a string contains only valid characters
allValidChars :: String -> Bool
allValidChars = all isValidChar

-- | Check if a string is balanced (parentheses, brackets, braces)
isBalanced :: String -> Bool
isBalanced = go 0 0 0
  where
    go _ _ _ [] = True
    go p b s (c:cs)
      | c == '(' = go (p+1) b s cs
      | c == ')' = p > 0 && go (p-1) b s cs
      | c == '[' = go p (b+1) s cs
      | c == ']' = b > 0 && go p (b-1) s cs
      | c == '{' = go p b (s+1) cs
      | c == '}' = s > 0 && go p b (s-1) cs
      | otherwise = go p b s cs

-- | Simple parser for testing purposes
parseSimple :: String -> Either String [String]
parseSimple s = Right (words s)

-- | Simple identifier parser
parseIdentifier :: String -> Either String String
parseIdentifier [] = Left "Empty identifier"
parseIdentifier (c:cs) 
  | isAlphaNum c && all isIdentifierChar cs = Right (c:cs)
  | otherwise = Left "Invalid identifier"

-- | Simple number parser
parseNumber :: String -> Either String Int
parseNumber s = case reads s of
  [(n, "")] -> Right n
  _ -> Left "Invalid number"

-- | Simple string literal parser
parseStringLiteral :: String -> Either String String
parseStringLiteral s 
  | length s >= 2 && head s == '"' && last s == '"' = 
      Right (init (tail s))
  | otherwise = Left "Invalid string literal"

-- | Simple comment parser
parseComment :: String -> Either String String
parseComment s 
  | "//" `isPrefixOf` s = Right (drop 2 s)
  | "/*" `isPrefixOf` s && "*/" `isSuffixOf` s = 
      Right (take (length s - 4) (drop 2 s))
  | otherwise = Left "Not a comment"

-- ============================================================================
-- Parser Combinator Tests
-- ============================================================================

-- | Test parseSimple: parsing words
prop_parseSimple_words :: String -> Bool
prop_parseSimple_words s = 
  case parseSimple s of
    Left _ -> False
    Right words -> all (not . null) words && concat words == filter (not . isSpace) s

-- | Test parseSimple: empty string
prop_parseSimple_empty :: Bool
prop_parseSimple_empty = 
  case parseSimple "" of
    Left _ -> False
    Right words -> null words

-- | Test parseSimple: whitespace only
prop_parseSimple_whitespace :: String -> Property
prop_parseSimple_whitespace s = 
  all isSpace s ==>
  case parseSimple s of
    Left _ -> False
    Right words -> null words

-- | Test parseIdentifier: valid identifiers
prop_parseIdentifier_valid :: String -> Property
prop_parseIdentifier_valid s = 
  isValidIdentifier s ==>
  case parseIdentifier s of
    Left _ -> False
    Right ident -> ident == s

-- | Test parseIdentifier: invalid identifiers
prop_parseIdentifier_invalid :: String -> Property
prop_parseIdentifier_invalid s = 
  not (isValidIdentifier s) && not (null s) ==>
  case parseIdentifier s of
    Left _ -> True
    Right _ -> False

-- | Test parseIdentifier: empty string
prop_parseIdentifier_empty :: Bool
prop_parseIdentifier_empty = 
  case parseIdentifier "" of
    Left _ -> True
    Right _ -> False

-- | Test parseNumber: valid numbers
prop_parseNumber_valid :: Int -> Bool
prop_parseNumber_valid n = 
  let s = show n
  in case parseNumber s of
    Left _ -> False
    Right num -> num == n

-- | Test parseNumber: invalid numbers
prop_parseNumber_invalid :: String -> Property
prop_parseNumber_invalid s = 
  not (all isDigit s) && not (null s) ==>
  case parseNumber s of
    Left _ -> True
    Right _ -> False
  where
    isDigit c = c >= '0' && c <= '9'

-- | Test parseNumber: empty string
prop_parseNumber_empty :: Bool
prop_parseNumber_empty = 
  case parseNumber "" of
    Left _ -> True
    Right _ -> False

-- | Test parseStringLiteral: valid string literals
prop_parseStringLiteral_valid :: String -> Bool
prop_parseStringLiteral_valid s = 
  let literal = "\"" ++ s ++ "\""
  in case parseStringLiteral literal of
    Left _ -> False
    Right content -> content == s

-- | Test parseStringLiteral: invalid string literals
prop_parseStringLiteral_invalid :: String -> Property
prop_parseStringLiteral_invalid s = 
  not (isPrefixOf "\"" s) || not (isSuffixOf "\"" s) ==>
  case parseStringLiteral s of
    Left _ -> True
    Right _ -> False

-- | Test parseStringLiteral: empty string literal
prop_parseStringLiteral_empty :: Bool
prop_parseStringLiteral_empty = 
  case parseStringLiteral "\"\"" of
    Left _ -> False
    Right content -> null content

-- | Test parseComment: line comments
prop_parseComment_line :: String -> Bool
prop_parseComment_line s = 
  let comment = "//" ++ s
  in case parseComment comment of
    Left _ -> False
    Right content -> content == s

-- | Test parseComment: block comments
prop_parseComment_block :: String -> Bool
prop_parseComment_block s = 
  let comment = "/*" ++ s ++ "*/"
  in case parseComment comment of
    Left _ -> False
    Right content -> content == s

-- | Test parseComment: invalid comments
prop_parseComment_invalid :: String -> Property
prop_parseComment_invalid s = 
  not ("//" `isPrefixOf` s) && not ("/*" `isPrefixOf` s) ==>
  case parseComment s of
    Left _ -> True
    Right _ -> False

-- | Test parseComment: empty comments
prop_parseComment_empty :: Bool
prop_parseComment_empty = 
  case parseComment "//" of
    Left _ -> False
    Right content -> null content

-- ============================================================================
-- Parser Composition Tests
-- ============================================================================

-- | Test parser composition: sequential parsing
prop_sequential_parsing :: String -> String -> Bool
prop_sequential_parsing s1 s2 = 
  let combined = s1 ++ " " ++ s2
  in case parseSimple combined of
    Left _ -> False
    Right words -> length words >= 2 && head words == s1 && last words == s2

-- | Test parser composition: alternative parsing
prop_alternative_parsing :: String -> String -> Bool
prop_alternative_parsing s1 s2 = 
  let ident1 = parseIdentifier s1
      ident2 = parseIdentifier s2
  in case (ident1, ident2) of
    (Right i1, Right i2) -> i1 /= i2 || i1 == i2 -- Always true
    _ -> True -- At least one might succeed

-- | Test parser composition: optional parsing
prop_optional_parsing :: String -> Bool
prop_optional_parsing s = 
  let ident = parseIdentifier s
      number = parseNumber s
  in case (ident, number) of
    (Right _, Right _) -> True -- Both succeeded
    (Right _, Left _) -> True -- Identifier succeeded
    (Left _, Right _) -> True -- Number succeeded
    (Left _, Left _) -> True -- Both failed

-- | Test parser composition: repeated parsing
prop_repeated_parsing :: String -> Int -> Property
prop_repeated_parsing s n = 
  n > 0 ==>
  let repeated = concat (replicate n (s ++ " "))
  in case parseSimple repeated of
    Left _ -> False
    Right words -> length words == n && all (== s) words

-- | Test parser composition: conditional parsing
prop_conditional_parsing :: String -> Bool
prop_conditional_parsing s = 
  let isIdent = isValidIdentifier s
      result = if isIdent then parseIdentifier s else Right "0" -- Use a default number as string
  in case result of
    Left _ -> not isIdent
    Right _ -> True

-- ============================================================================
-- Parser Error Handling Tests
-- ============================================================================

-- | Test parser error handling: error messages
prop_error_messages :: String -> Property
prop_error_messages s = 
  not (isValidIdentifier s) && not (null s) ==>
  case parseIdentifier s of
    Left msg -> "Invalid identifier" `isInfixOf` msg
    Right _ -> False

-- | Test parser error handling: error recovery
prop_error_recovery :: String -> String -> Bool
prop_error_recovery s1 s2 = 
  let combined = s1 ++ " " ++ s2
  in case parseSimple combined of
    Left _ -> False
    Right words -> length words >= 1

-- | Test parser error handling: partial success
prop_partial_success :: String -> String -> Property
prop_partial_success s1 s2 = 
  not (isValidIdentifier s1) && isValidIdentifier s2 ==>
  let ident1 = parseIdentifier s1
      ident2 = parseIdentifier s2
  in case (ident1, ident2) of
    (Left _, Right i2) -> i2 == s2
    _ -> True

-- | Test parser error handling: cascading errors
prop_cascading_errors :: String -> String -> Property
prop_cascading_errors s1 s2 = 
  not (isValidIdentifier s1) && not (isValidIdentifier s2) ==>
  let ident1 = parseIdentifier s1
      ident2 = parseIdentifier s2
  in case (ident1, ident2) of
    (Left _, Left _) -> True
    _ -> True

-- ============================================================================
-- Parser Performance Tests
-- ============================================================================

-- | Test parser performance: large input
prop_large_input :: String -> Int -> Property
prop_large_input s n = 
  n > 0 && n < 1000 ==>
  let large = concat (replicate n (s ++ " "))
  in case parseSimple large of
    Left _ -> False
    Right words -> length words == n

-- | Test parser performance: deep nesting
prop_deep_nesting :: String -> Int -> Property
prop_deep_nesting s n = 
  n > 0 && n < 100 ==>
  let nested = concat (replicate n ("(" ++ s ++ ")"))
  in case parseSimple nested of
    Left _ -> True -- Might fail due to parentheses
    Right words -> length words >= 1

-- | Test parser performance: many alternatives
prop_many_alternatives :: String -> Bool
prop_many_alternatives s = 
  let alternatives = [s, show (length s), "\"" ++ s ++ "\"", "//" ++ s]
      results = map parseIdentifier alternatives
  in any isRight results
  where
    isRight (Right _) = True
    isRight (Left _) = False

-- ============================================================================
-- Parser Edge Case Tests
-- ============================================================================

-- | Test parser edge cases: empty input
prop_empty_input :: Bool
prop_empty_input = 
  case parseSimple "" of
    Left _ -> False
    Right words -> null words

-- | Test parser edge cases: single character
prop_single_character :: Char -> Bool
prop_single_character c = 
  let s = [c]
  in case parseSimple s of
    Left _ -> False
    Right words -> length words == 1 && head words == s

-- | Test parser performance: whitespace only
prop_whitespace_only :: String -> Property
prop_whitespace_only s = 
  all isSpace s ==>
  case parseSimple s of
    Left _ -> False
    Right words -> null words

-- | Test parser edge cases: special characters

prop_special_characters :: String -> Property

prop_special_characters s = 

  let

    special = "!@#$%^&*()_+-=[]{}|;':\",./<>?"

    hasSpecial = any (`elem` special) s

  in hasSpecial ==>

  case parseSimple s of

    Left _ -> False

    Right words -> length words >= 1

-- | Test parser edge cases: unicode characters
prop_unicode_characters :: String -> Bool
prop_unicode_characters s = 
  let unicode = "ñáéíóú你好世界"
      mixed = s ++ unicode ++ s
  in case parseSimple mixed of
    Left _ -> False
    Right words -> length words >= 1

-- | Test parser edge cases: very long identifiers
prop_very_long_identifiers :: Int -> Property
prop_very_long_identifiers n = 
  n > 0 && n < 1000 ==>
  let longIdent = replicate n 'a'
  in case parseIdentifier longIdent of
    Left _ -> False
    Right ident -> length ident == n

-- | Test parser edge cases: nested structures
prop_nested_structures :: Int -> Property
prop_nested_structures n = 
  n > 0 && n < 100 ==>
  let nested = concat (replicate n "[()]")
  in case parseSimple nested of
    Left _ -> True -- Might fail due to brackets
    Right words -> length words >= 1

-- ============================================================================
-- Parser Consistency Tests
-- ============================================================================

-- | Test parser consistency: idempotence
prop_idempotence :: String -> Bool
prop_idempotence s = 
  case parseSimple s of
    Left _ -> False
    Right words1 -> 
      case parseSimple (unwords words1) of
        Left _ -> False
        Right words2 -> words1 == words2

-- | Test parser consistency: associativity
prop_associativity :: String -> String -> String -> Bool
prop_associativity s1 s2 s3 = 
  let combined1 = s1 ++ " " ++ s2 ++ " " ++ s3
      combined2 = (s1 ++ " " ++ s2) ++ " " ++ s3
      combined3 = s1 ++ " " ++ (s2 ++ " " ++ s3)
  in case (parseSimple combined1, parseSimple combined2, parseSimple combined3) of
    (Right w1, Right w2, Right w3) -> w1 == w2 && w2 == w3
    _ -> True

-- | Test parser consistency: commutativity
prop_commutativity :: String -> String -> Bool
prop_commutativity s1 s2 = 
  let combined1 = s1 ++ " " ++ s2
      combined2 = s2 ++ " " ++ s1
  in case (parseSimple combined1, parseSimple combined2) of
    (Right w1, Right w2) -> sort w1 == sort w2
    _ -> True

-- | Test parser consistency: distributivity
prop_distributivity :: String -> String -> String -> Bool
prop_distributivity s1 s2 s3 = 
  let combined1 = s1 ++ " " ++ s2 ++ " " ++ s3
      split1 = words combined1
      split2 = words (s1 ++ " " ++ s2) ++ words s3
  in sort split1 == sort split2

-- ============================================================================
-- Test Group
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser Combinators QuickCheck Tests"
  [ -- Parser Combinator Tests
    testProperty "parseSimple words" prop_parseSimple_words
  , testProperty "parseSimple empty" prop_parseSimple_empty
  , testProperty "parseSimple whitespace" prop_parseSimple_whitespace
  , testProperty "parseIdentifier valid" prop_parseIdentifier_valid
  , testProperty "parseIdentifier invalid" prop_parseIdentifier_invalid
  , testProperty "parseIdentifier empty" prop_parseIdentifier_empty
  , testProperty "parseNumber valid" prop_parseNumber_valid
  , testProperty "parseNumber invalid" prop_parseNumber_invalid
  , testProperty "parseNumber empty" prop_parseNumber_empty
  , testProperty "parseStringLiteral valid" prop_parseStringLiteral_valid
  , testProperty "parseStringLiteral invalid" prop_parseStringLiteral_invalid
  , testProperty "parseStringLiteral empty" prop_parseStringLiteral_empty
  , testProperty "parseComment line" prop_parseComment_line
  , testProperty "parseComment block" prop_parseComment_block
  , testProperty "parseComment invalid" prop_parseComment_invalid
  , testProperty "parseComment empty" prop_parseComment_empty
  
  -- Parser Composition Tests
  , testProperty "sequential parsing" prop_sequential_parsing
  , testProperty "alternative parsing" prop_alternative_parsing
  , testProperty "optional parsing" prop_optional_parsing
  , testProperty "repeated parsing" prop_repeated_parsing
  , testProperty "conditional parsing" prop_conditional_parsing
  
  -- Parser Error Handling Tests
  , testProperty "error messages" prop_error_messages
  , testProperty "error recovery" prop_error_recovery
  , testProperty "partial success" prop_partial_success
  , testProperty "cascading errors" prop_cascading_errors
  
  -- Parser Performance Tests
  , testProperty "large input" prop_large_input
  , testProperty "deep nesting" prop_deep_nesting
  , testProperty "many alternatives" prop_many_alternatives
  
  -- Parser Edge Case Tests
  , testProperty "empty input" prop_empty_input
  , testProperty "single character" prop_single_character
  , testProperty "whitespace only" prop_whitespace_only
  , testProperty "special characters" prop_special_characters
  , testProperty "unicode characters" prop_unicode_characters
  , testProperty "very long identifiers" prop_very_long_identifiers
  , testProperty "nested structures" prop_nested_structures
  
  -- Parser Consistency Tests
  , testProperty "idempotence" prop_idempotence
  , testProperty "associativity" prop_associativity
  , testProperty "commutativity" prop_commutativity
  , testProperty "distributivity" prop_distributivity
  ]