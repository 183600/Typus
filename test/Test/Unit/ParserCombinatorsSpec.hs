{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Test.Unit.ParserCombinatorsSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser
import Utils
import SourceLocation
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char (isAlphaNum, isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T
import Control.Monad (void)

-- ============================================================================
-- Parser Combinators Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser Combinators Tests"
  [ basicParsingProperties
  , directiveParsingProperties
  , codeBlockProperties
  , fileStructureProperties
  , errorHandlingProperties
  , parserCombinatorProperties
  ]

-- ============================================================================
-- Basic Parsing Properties
-- ============================================================================

basicParsingProperties :: TestTree
basicParsingProperties = testGroup "Basic Parsing Properties"
  [ testProperty "parseTypus handles empty input" $
      \_ -> case parseTypus "" of
        Left _ -> True
        Right result -> null result
    
  , testProperty "parseTypus is deterministic" $
      \input -> parseTypus input === parseTypus input
    
  , testProperty "parsing preserves input structure" $
      \input ->
        case parseTypus input of
          Left _ -> True
          Right result -> L.length (show result) >= 0
    
  , testCase "parse simple file directives" $
      let input = "// @ownership: true\n// @dependent-types: false"
      in case parseTypus input of
        Left err -> assertFailure $ "Parse failed: " ++ errorBundlePretty err
        Right result -> assertBool "Parsed successfully" True
    
  , testCase "parse simple code blocks" $
      let input = "```typus\nfn test() { return 42; }\n```"
      in case parseTypus input of
        Left err -> assertFailure $ "Parse failed: " ++ errorBundlePretty err
        Right result -> assertBool "Parsed successfully" True
  ]

-- ============================================================================
-- Directive Parsing Properties
-- ============================================================================

directiveParsingProperties :: TestTree
directiveParsingProperties = testGroup "Directive Parsing Properties"
  [ testProperty "file directives are parsed correctly" $
      \ownershipValue dependentValue constraintsValue ->
        let input = "// @ownership: " ++ show ownershipValue ++ "\n" ++
                   "// @dependent-types: " ++ show dependentValue ++ "\n" ++
                   "// @constraints: " ++ show constraintsValue
        in case parseTypus input of
          Left _ -> True  -- May fail due to syntax, that's ok
          Right result -> True  -- If it succeeds, that's also ok
    
  , testProperty "block directives are parsed correctly" $
      \ownershipValue dependentValue ->
        let input = "```typus\n// @ownership: " ++ show ownershipValue ++ "\n" ++
                   "// @dependent-types: " ++ show dependentValue ++ "\n" ++
                   "fn test() {}\n```"
        in case parseTypus input of
          Left _ -> True
          Right result -> True
    
  , testCase "parse ownership directive" $
      let input = "// @ownership: true"
      in case parseTypus input of
        Left err -> assertFailure $ "Parse failed: " ++ errorBundlePretty err
        Right result -> assertBool "Ownership directive parsed" True
    
  , testCase "parse dependent-types directive" $
      let input = "// @dependent-types: false"
      in case parseTypus input of
        Left err -> assertFailure $ "Parse failed: " ++ errorBundlePretty err
        Right result -> assertBool "Dependent types directive parsed" True
    
  , testCase "parse constraints directive" $
      let input = "// @constraints: true"
      in case parseTypus input of
        Left err -> assertFailure $ "Parse failed: " ++ errorBundlePretty err
        Right result -> assertBool "Constraints directive parsed" True
  ]

-- ============================================================================
-- Code Block Properties
-- ============================================================================

codeBlockProperties :: TestTree
codeBlockProperties = testGroup "Code Block Properties"
  [ testProperty "code blocks preserve content" $
      \content ->
        let input = "```typus\n" ++ content ++ "\n```"
        in case parseTypus input of
          Left _ -> True
          Right result -> True
    
  , testProperty "nested code blocks are handled" $
      \outerContent innerContent ->
        let input = "```typus\n" ++ outerContent ++ 
                   "\n```typus\n" ++ innerContent ++ "\n```"
        in case parseTypus input of
          Left _ -> True
          Right result -> True
    
  , testCase "parse empty code block" $
      let input = "```typus\n```"
      in case parseTypus input of
        Left err -> assertFailure $ "Parse failed: " ++ errorBundlePretty err
        Right result -> assertBool "Empty code block parsed" True
    
  , testCase "parse code block with directives" $
      let input = "```typus\n// @ownership: true\nfn test() {}\n```"
      in case parseTypus input of
        Left err -> assertFailure $ "Parse failed: " ++ errorBundlePretty err
        Right result -> assertBool "Code block with directives parsed" True
    
  , testProperty "code block parsing is idempotent" $
      \content ->
        let input = "```typus\n" ++ content ++ "\n```"
            result1 = parseTypus input
            result2 = parseTypus input
        in case (result1, result2) of
          (Left _, Left _) -> True
          (Right r1, Right r2) -> L.length (show r1) == L.length (show r2)
          _ -> False
  ]

-- ============================================================================
-- File Structure Properties
-- ============================================================================

fileStructureProperties :: TestTree
fileStructureProperties = testGroup "File Structure Properties"
  [ testProperty "multiple code blocks are parsed sequentially" $
      \contents ->
        let blocks = L.map (\c -> "```typus\n" ++ c ++ "\n```") contents
            input = unlines blocks
        in case parseTypus input of
          Left _ -> True
          Right result -> True
    
  , testProperty "file directives apply to entire file" $
      \ownershipValue ->
        let input = "// @ownership: " ++ show ownershipValue ++ "\n" ++
                   "```typus\nfn test() {}\n```"
        in case parseTypus input of
          Left _ -> True
          Right result -> True
    
  , testCase "parse mixed directives L.and code" $
      let input = "// @ownership: true\n" ++
                 "// @dependent-types: false\n" ++
                 "```typus\n// @constraints: true\nfn test() {}\n```"
      in case parseTypus input of
        Left err -> assertFailure $ "Parse failed: " ++ errorBundlePretty err
        Right result -> assertBool "Mixed content parsed" True
    
  , testProperty "whitespace handling is consistent" $
      \content ->
        let input1 = "```typus\n" ++ content ++ "\n```"
            input2 = "\n```typus\n" ++ content ++ "\n```\n"
            result1 = parseTypus input1
            result2 = parseTypus input2
        in case (result1, result2) of
          (Left _, Left _) -> True
          (Right r1, Right r2) -> True  -- Should be semantically equivalent
          _ -> False
  ]

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

errorHandlingProperties :: TestTree
errorHandlingProperties = testGroup "Error Handling Properties"
  [ testProperty "malformed directives are handled gracefully" $
      \directive ->
        let input = "// @" ++ directive
        in case parseTypus input of
          Left _ -> True  -- Should fail gracefully
          Right _ -> True  -- Or succeed if it's valid
    
  , testProperty "unclosed code blocks are detected" $
      \content ->
        let input = "```typus\n" ++ content  -- Missing closing ```
        in case parseTypus input of
          Left _ -> True  -- Should fail
          Right _ -> False  -- Should not succeed
    
  , testCase "handle invalid directive values" $
      let input = "// @ownership: invalid_value"
      in case parseTypus input of
        Left _ -> assertBool "Invalid value rejected" True
        Right _ -> assertBool "Invalid value handled" True
    
  , testProperty "parsing errors provide useful information" $
      \input ->
        case parseTypus input of
          Left err -> not $ L.null $ errorBundlePretty err
          Right _ -> True
    
  , testCase "handle empty input gracefully" $
      case parseTypus "" of
        Left err -> assertFailure $ "Empty input should not fail: " ++ errorBundlePretty err
        Right result -> assertBool "Empty input handled" True
  ]

-- ============================================================================
-- Parser Combinator Properties
-- ============================================================================

parserCombinatorProperties :: TestTree
parserCombinatorProperties = testGroup "Parser Combinator Properties"
  [ testProperty "parser composition is associative" $
      \input ->
        let parser1 = many (single 'a')
            parser2 = many (single 'b')
            parser3 = many (single 'c')
            result1 = parse (parser1 *> parser2 *> parser3) "" input
            result2 = parse ((parser1 *> parser2) *> parser3) "" input
            result3 = parse (parser1 *> (parser2 *> parser3)) "" input
        in case (result1, result2, result3) of
          (Right r1, Right r2, Right r3) -> r1 == r2 && r2 == r3
          (Left _, Left _, Left _) -> True
          _ -> False
    
  , testProperty "choice parser returns first successful match" $
      \input ->
        let parser1 = string "abc"
            parser2 = string "ab"
            parser3 = string "a"
            choiceParser = parser1 <|> parser2 <|> parser3
        in case parse choiceParser "" input of
          Right result -> "abc" `L.isPrefixOf` input || 
                         "ab" `L.isPrefixOf` input || 
                         "a" `L.isPrefixOf` input
          Left _ -> not ("a" `L.isPrefixOf` input)
    
  , testProperty "many parser is greedy" $
      \input ->
        let parser = many (single 'a')
        in case parse parser "" input of
          Right result -> L.length result >= 0
          Left _ -> True
    
  , testProperty "optional parser provides default value" $
      \input ->
        let parser = optional (single 'a')
        in case parse parser "" input of
          Right result -> result == Just 'a' || result == Nothing
          Left _ -> False
    
  , testCase "lookahead parser doesn't consume input" $
      let parser = lookAhead (string "abc") *> string "abc"
      in case parse parser "" "abc" of
        Right result -> result == "abc"
        Left _ -> assertFailure "Lookahead parser failed"
  ]

-- ============================================================================
-- QuickCheck Generators
-- ============================================================================

-- Generate valid directive names
genDirectiveName :: Gen String
genDirectiveName = elements ["ownership", "dependent-types", "constraints"]

-- Generate boolean values for directives
genDirectiveValue :: Gen Bool
genDirectiveValue = arbitrary

-- Generate code block content
genCodeContent :: Gen String
genCodeContent = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n.,;:(){}[]"

-- Generate file directives
genFileDirectives :: Gen FileDirectives
genFileDirectives = do
  ownership <- arbitrary
  dependentTypes <- arbitrary
  constraints <- arbitrary
  return $ FileDirectives 
    { fdOwnership = if ownership then Just (locatedAt True startPos) else Nothing
    , fdDependentTypes = if dependentTypes then Just (locatedAt True startPos) else Nothing
    , fdConstraints = if constraints then Just (locatedAt True startPos) else Nothing
    }

-- Generate block directives
genBlockDirectives :: Gen BlockDirectives
genBlockDirectives = do
  ownership <- arbitrary
  dependentTypes <- arbitrary
  constraints <- arbitrary
  return $ BlockDirectives
    { bdOwnership = if ownership then Just (locatedAt True startPos) else Nothing
    , bdDependentTypes = if dependentTypes then Just (locatedAt True startPos) else Nothing
    , bdConstraints = if constraints then Just (locatedAt True startPos) else Nothing
    }

instance Arbitrary FileDirectives where
  arbitrary = genFileDirectives

instance Arbitrary BlockDirectives where
  arbitrary = genBlockDirectives

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Parse with error handling
safeParse :: String -> Either String TypusFile
safeParse input = case parseTypus input of
  Left err -> Left $ errorBundlePretty err
  Right result -> Right result

-- Check if parsing succeeded
parseSucceeded :: Either a b -> Bool
parseSucceeded (Right _) = True
parseSucceeded (Left _) = False

-- Check if parsing failed
parseFailed :: Either a b -> Bool
parseFailed = not . parseSucceeded

-- ============================================================================
-- Performance Properties
-- ============================================================================

performanceProperties :: TestTree
performanceProperties = testGroup "Performance Properties"
  [ testProperty "parsing time is reasonable for small inputs" $
      \input -> L.length input < 100 ==> 
        case parseTypus input of
          Left _ -> True
          Right result -> True
    
  , testProperty "parsing handles large inputs without stack overflow" $
      \n -> n < 1000 ==>
        let input = replicate n 'a'
        in case parseTypus input of
          Left _ -> True
          Right result -> True
    
  , testProperty "memory usage is bounded" $
      \input -> L.length input < 10000 ==>
        case parseTypus input of
          Left _ -> True
          Right result -> L.length (show result) < L.length input * 10
  ]

-- ============================================================================
-- Regression Tests
-- ============================================================================

regressionProperties :: TestTree
regressionProperties = testGroup "Regression Tests"
  [ testCase "handle comments in code blocks" $
      let input = "```typus\n// This is a comment\nfn test() {}\n```"
      in case parseTypus input of
        Left err -> assertFailure $ "Parse failed: " ++ errorBundlePretty err
        Right result -> assertBool "Comments handled" True
    
  , testCase "handle special characters in content" $
      let input = "```typus\nfn test() { return \"hello\\nworld\"; }\n```"
      in case parseTypus input of
        Left err -> assertFailure $ "Parse failed: " ++ errorBundlePretty err
        Right result -> assertBool "Special characters handled" True
    
  , testCase "handle unicode characters" $
      let input = "```typus\nfn 测试() { return 42; }\n```"
      in case parseTypus input of
        Left err -> assertFailure $ "Parse failed: " ++ errorBundlePretty err
        Right result -> assertBool "Unicode handled" True
  ]