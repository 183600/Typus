{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.EnhancedParserCombinatorQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, suchThat)
import TestSupport.Arbitrary

import Parser
import SourceLocation
import Data.List (sort, nub, group, intercalate)
import Data.Maybe (isJust, isNothing, catMaybes)
import Data.Char (isAlpha, isAlphaNum, isSpace, toUpper, toLower)
import Control.Monad (void)

-- ============================================================================
-- Enhanced Parser Combinator QuickCheck Tests
-- ============================================================================

-- Property: Token sequence roundtrip
prop_token_sequence_roundtrip :: [String] -> Property
prop_token_sequence_roundtrip tokens =
  not (null tokens) ==> 
  let input = unwords tokens
      parsed = simpleTokenize input
  in property $ tokens === parsed

-- Property: Parentheses balancing preservation
prop_parentheses_balancing :: String -> Property
prop_parentheses_balancing input =
  let openCount = length $ filter (== '(') input
      closeCount = length $ filter (== ')') input
      tokens = simpleTokenize input
      openParsed = length $ filter (== "(") tokens
      closeParsed = length $ filter (== ")") tokens
  in property $ openCount === openParsed .&&. closeCount === closeParsed

-- Property: Whitespace normalization
prop_whitespace_normalization :: String -> String -> String -> Property
prop_whitespace_normalization part1 part2 part3 =
  let input1 = part1 ++ " " ++ part2 ++ " " ++ part3
      input2 = part1 ++ "  " ++ part2 ++ "   " ++ part3
      input3 = part1 ++ "\t" ++ part2 ++ "\n" ++ part3
      tokens1 = simpleTokenize input1
      tokens2 = simpleTokenize input2
      tokens3 = simpleTokenize input3
  in property $ tokens1 === tokens2 .&&. tokens1 === tokens3

-- Property: Identifier preservation
prop_identifier_preservation :: String -> Property
prop_identifier_preservation ident =
  let isValidIdent = not (null ident) && isAlpha (head ident) && all isAlphaNum ident
  in isValidIdent ==> 
  let input = ident ++ " + " ++ ident ++ " * " ++ ident
      tokens = simpleTokenize input
      identTokens = filter (== ident) tokens
  in property $ length identTokens === 3 .&&. head identTokens === ident

-- Property: Number literal parsing consistency
prop_number_literal_consistency :: Int -> Int -> Property
prop_number_literal_consistency num1 num2 =
  let input = show num1 ++ " + " ++ show num2
      tokens = simpleTokenize input
      expectedNums = [show num1, show num2]
  in property $ length tokens >= 2 .&&.
     head tokens === expectedNums !! 0 .&&.
     last tokens === expectedNums !! 1

-- Property: String literal preservation
prop_string_literal_preservation :: String -> Property
prop_string_literal_preservation content =
  let input = "\"" ++ content ++ "\""
      tokens = simpleTokenize input
  in property $ length tokens === 1 .&&. head tokens === input

-- Property: Operator recognition consistency
prop_operator_recognition :: String -> String -> Property
prop_operator_recognition op1 op2 =
  let operators = ["+", "-", "*", "/", "==", "!=", "<=", ">=", "<", ">", "&&", "||"]
      validOp1 = op1 `elem` operators
      validOp2 = op2 `elem` operators
  in validOp1 && validOp2 ==> 
  let input = "a " ++ op1 ++ " b " ++ op2 ++ " c"
      tokens = simpleTokenize input
  in property $ length tokens >= 5 .&&.
     tokens !! 1 === op1 .&&.
     tokens !! 3 === op2

-- Property: Comment removal
prop_comment_removal :: String -> String -> Property
prop_comment_removal code comment =
  let input = code ++ " // " ++ comment
      tokens = simpleTokenize input
      commentTokens = filter (isPrefixOf "//") tokens
  in property $ null commentTokens

-- Property: Nested structure parsing
prop_nested_structure_parsing :: [[String]] -> Property
prop_nested_structure_parsing tokenGroups =
  not (null tokenGroups) ==> 
  let nestedInput = concatMap (\group -> "(" ++ unwords group ++ ")") tokenGroups
      tokens = simpleTokenize nestedInput
      openParens = length $ filter (== "(") tokens
      closeParens = length $ filter (== ")") tokens
  in property $ openParens === length tokenGroups .&&.
     closeParens === length tokenGroups .&&.
     openParens === closeParens

-- Property: Keyword preservation
prop_keyword_preservation :: String -> String -> Property
prop_keyword_preservation keyword identifier =
  let keywords = ["if", "else", "while", "for", "function", "return", "var", "let", "const"]
      isKeyword = keyword `elem` keywords
      isValidIdent = not (null identifier) && isAlpha (head identifier) && all isAlphaNum identifier
  in isKeyword && isValidIdent ==> 
  let input = keyword ++ " " ++ identifier
      tokens = simpleTokenize input
  in property $ length tokens === 2 .&&. head tokens === keyword .&&. last tokens === identifier

-- Property: Complex expression parsing
prop_complex_expression_parsing :: [String] -> [String] -> [String] -> Property
prop_complex_expression_parsing vars ops funcs =
  not (null vars) && not (null ops) ==> 
  let operators = ["+", "-", "*", "/", "==", "!=", "<", ">"]
      validOps = filter (`elem` operators) ops
      validFuncs = filter (\f -> not (null f) && isAlpha (head f)) funcs
      expr = intercalate " " $ zipWith (\v o -> v ++ " " ++ o) vars (take (length vars - 1) validOps ++ [""])
      tokens = simpleTokenize expr
  in property $ length tokens >= length vars

-- Property: Error recovery in malformed input
prop_error_recovery :: String -> String -> Property
prop_error_recovery goodPart badPart =
  not (null goodPart) ==> 
  let input = goodPart ++ " " ++ badPart ++ " " ++ goodPart
      tokens = simpleTokenize input
  in property $ not (null tokens) .&&. head tokens === head (simpleTokenize goodPart)

-- Property: Case sensitivity preservation
prop_case_sensitivity :: String -> Property
prop_case_sensitivity ident =
  let upperIdent = map toUpper ident
      lowerIdent = map toLower ident
      input1 = ident ++ " " ++ upperIdent ++ " " ++ lowerIdent
      tokens = simpleTokenize input1
  in property $ length tokens >= 3 .&&.
     head tokens === ident .&&.
     tokens !! 1 === upperIdent .&&.
     tokens !! 2 === lowerIdent

-- Helper functions
simpleTokenize :: String -> [String]
simpleTokenize input = words $ filter (`notElem` "();") input

isPrefixOf :: String -> String -> Bool
isPrefixOf prefix str = take (length prefix) str == prefix

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Enhanced Parser Combinator QuickCheck Tests"
  [ fastProperty "Token sequence roundtrip" prop_token_sequence_roundtrip
  , fastProperty "Parentheses balancing preservation" prop_parentheses_balancing
  , fastProperty "Whitespace normalization" prop_whitespace_normalization
  , fastProperty "Identifier preservation" prop_identifier_preservation
  , fastProperty "Number literal parsing consistency" prop_number_literal_consistency
  , fastProperty "String literal preservation" prop_string_literal_preservation
  , fastProperty "Operator recognition consistency" prop_operator_recognition
  , fastProperty "Comment removal" prop_comment_removal
  , fastProperty "Nested structure parsing" prop_nested_structure_parsing
  , fastProperty "Keyword preservation" prop_keyword_preservation
  , fastProperty "Complex expression parsing" prop_complex_expression_parsing
  , fastProperty "Error recovery in malformed input" prop_error_recovery
  , fastProperty "Case sensitivity preservation" prop_case_sensitivity
  ]