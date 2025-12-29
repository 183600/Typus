{-# OPTIONS_GHC -Wno-deprecations #-}
module Test.Unit.LexerBoundaryQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Compiler.GoLexer (GoToken(..), GoTokenKind(..), tokenizeGo)
import Data.Char (isSpace, isDigit, isAlphaNum)
import Data.List (isPrefixOf)

-- ============================================================================
-- Lexer Boundary Property Tests
-- ============================================================================

-- | Test that tokenization preserves the original text length
prop_tokenizationPreservesLength :: String -> Property
prop_tokenizationPreservesLength input =
  let tokens = tokenizeGo input
      combinedText = concatMap tokenText tokens
  in length combinedText === length input

-- | Test that whitespace tokens contain only whitespace characters
prop_whitespaceTokensContainOnlyWhitespace :: String -> Property
prop_whitespaceTokensContainOnlyWhitespace input =
  let tokens = tokenizeGo input
      whitespaceTokens = filter (\t -> tokenKind t == TokWhitespace) tokens
      allWhitespace = all (\t -> all isSpace (tokenText t)) whitespaceTokens
  in counterexample ("Found non-whitespace in whitespace tokens: " ++ show whitespaceTokens) allWhitespace

-- | Test that string tokens start and end with quotes
prop_stringTokensHaveQuotes :: String -> Property
prop_stringTokensHaveQuotes input =
  let tokens = tokenizeGo input
      stringTokens = filter (\t -> tokenKind t == TokString) tokens
      validStringTokens = all (\t -> 
        let text = tokenText t
        in (head text == '"' && last text == '"') ||
           (head text == '\'' && last text == '\'') ||
           (head text == '`' && last text == '`')
        ) stringTokens
  in counterexample ("Invalid string token format: " ++ show stringTokens) validStringTokens

-- | Test that comment tokens start with // or /*
prop_commentTokensHavePrefix :: String -> Property
prop_commentTokensHavePrefix input =
  let tokens = tokenizeGo input
      commentTokens = filter (\t -> tokenKind t == TokComment) tokens
      validCommentTokens = all (\t ->
        let text = tokenText t
        in "//" `isPrefixOf` text || "/*" `isPrefixOf` text
        ) commentTokens
  in counterexample ("Invalid comment token format: " ++ show commentTokens) validCommentTokens

-- | Test that number tokens contain only digits and at most one decimal point
prop_numberTokensAreValid :: String -> Property
prop_numberTokensAreValid input =
  let tokens = tokenizeGo input
      numberTokens = filter (\t -> tokenKind t == TokNumber) tokens
      isValidNumber text = 
        let digitsOnly = filter isDigit text
            decimalPoints = length $ filter (== '.') text
        in not (null digitsOnly) && decimalPoints <= 1
      validNumberTokens = all (\t -> isValidNumber (tokenText t)) numberTokens
  in counterexample ("Invalid number token format: " ++ show numberTokens) validNumberTokens

-- | Test that tokenization is idempotent - tokenizing each token individually
-- should produce the same token structure
prop_tokenizationIsIdempotent :: String -> Property
prop_tokenizationIsIdempotent input =
  let tokens = tokenizeGo input
      individualTokens = concatMap (tokenizeGo . tokenText) tokens
      kindsMatch = length tokens == length individualTokens &&
                   all (\(a, b) -> tokenKind a == tokenKind b) (zip tokens individualTokens)
  in counterexample ("Tokenization not idempotent. Original: " ++ show tokens ++ 
                     " Re-tokenized: " ++ show individualTokens) kindsMatch

-- | Test that tokenization handles empty input gracefully
prop_emptyInputProducesNoTokens :: Property
prop_emptyInputProducesNoTokens =
  let tokens = tokenizeGo ""
  in null tokens === True

-- | Test that tokenization handles whitespace-only input
prop_whitespaceOnlyInput :: Property
prop_whitespaceOnlyInput =
  forAll arbitrary $ \ws ->
    let whitespaceOnly = filter isSpace ws
        tokens = tokenizeGo whitespaceOnly
        allWhitespace = all (\t -> tokenKind t == TokWhitespace) tokens
    in counterexample ("Non-whitespace token found in whitespace-only input: " ++ show tokens) allWhitespace

-- | Test that tokenization handles very long identifiers
prop_longIdentifiers :: Property
prop_longIdentifiers =
  forAll (vectorOf 1000 (elements ['a'..'z'])) $ \chars ->
    let longIdent = "veryLongIdentifier" ++ chars
        tokens = tokenizeGo longIdent
        identifierTokens = filter (\t -> tokenKind t == TokIdentifier) tokens
    in counterexample ("Failed to tokenize long identifier: " ++ longIdent) 
       (length identifierTokens === 1)

-- | Test that tokenization handles nested block comment scenarios
prop_nestedBlockCommentHandling :: String -> String -> Property
prop_nestedBlockCommentHandling prefix suffix =
  let input = prefix ++ "/* /* nested */ */" ++ suffix
      tokens = tokenizeGo input
      commentTokens = filter (\t -> tokenKind t == TokComment) tokens
      hasCommentBlock = any (\t -> "/*" `isPrefixOf` tokenText t) commentTokens
  in counterexample ("Failed to handle nested block comments in: " ++ input) hasCommentBlock

-- | Test that tokenization preserves line structure in comments
prop_lineStructureInComments :: Property
prop_lineStructureInComments =
  forAll (listOf1 (elements ['a'..'z'])) $ \words ->
    let commentText = "// " ++ unwords words ++ "\nsecond line"
        tokens = tokenizeGo commentText
        commentTokens = filter (\t -> tokenKind t == TokComment) tokens
        containsNewline = any (\t -> '\n' `elem` tokenText t) commentTokens
    in counterexample ("Line structure not preserved in comment: " ++ commentText) 
       (length commentTokens >= 1 &&==> containsNewline)

-- | Test that tokenization handles escape sequences in strings
prop_stringEscapeSequences :: Property
prop_stringEscapeSequences =
  let stringWithEscapes = "\"Hello \\\"World\\\" \\n \\t \\\\\""
      tokens = tokenizeGo stringWithEscapes
      stringTokens = filter (\t -> tokenKind t == TokString) tokens
  in counterexample ("Failed to handle string with escape sequences: " ++ stringWithEscapes)
     (length stringTokens === 1)

-- | Test that tokenization handles Unicode characters
prop_unicodeCharacters :: Property
prop_unicodeCharacters =
  let unicodeString = "héllo 世界 🌟 identifier_测试"
      tokens = tokenizeGo unicodeString
      identifierTokens = filter (\t -> tokenKind t == TokIdentifier) tokens
  in counterexample ("Failed to handle Unicode characters: " ++ unicodeString)
     (length identifierTokens >= 1)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Lexer Boundary QuickCheck Tests"
  [ testProperty "Tokenization preserves input length" prop_tokenizationPreservesLength
  , testProperty "Whitespace tokens contain only whitespace" prop_whitespaceTokensContainOnlyWhitespace
  , testProperty "String tokens have proper quote boundaries" prop_stringTokensHaveQuotes
  , testProperty "Comment tokens have proper prefixes" prop_commentTokensHavePrefix
  , testProperty "Number tokens are valid" prop_numberTokensAreValid
  , testProperty "Tokenization is idempotent" prop_tokenizationIsIdempotent
  , testProperty "Empty input produces no tokens" prop_emptyInputProducesNoTokens
  , testProperty "Whitespace-only input produces only whitespace tokens" prop_whitespaceOnlyInput
  , testProperty "Long identifiers are tokenized correctly" prop_longIdentifiers
  , testProperty "Nested block comment handling" prop_nestedBlockCommentHandling
  , testProperty "Line structure preserved in comments" prop_lineStructureInComments
  , testProperty "String escape sequences handled correctly" prop_stringEscapeSequences
  , testProperty "Unicode characters handled correctly" prop_unicodeCharacters
  ]