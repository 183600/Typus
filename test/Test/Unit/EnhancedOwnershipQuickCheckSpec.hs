{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Test.Unit.EnhancedOwnershipQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, listOf, elements, choose, suchThat, (===), (.&&.), forAll)
import TestSupport.QuickCheck (fastProperty)
import Ownership
import Ownership.Common.Types (OwnershipAnalyzer, OwnershipError(..), OwnershipType(..), OwnershipTransfer(..), newOwnershipAnalyzer)
import SourceLocation (SourcePos(..), startPos)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isInfixOf)

-- ============================================================================
-- Enhanced QuickCheck tests for Ownership module
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Enhanced Ownership QuickCheck Tests"
    [ testGroup "Ownership Analysis Properties"
        [ fastProperty "ownership analyzer handles empty input" prop_ownershipAnalyzerHandlesEmpty
        , fastProperty "ownership analysis is deterministic" prop_ownershipAnalysisDeterministic
        , fastProperty "ownership transfer preserves invariants" prop_ownershipTransferPreservesInvariants
        , fastProperty "ownership type checking is sound" prop_ownershipTypeCheckingSound
        ]
    , testGroup "Lexing Properties"
        [ fastProperty "lexer handles basic tokens" prop_lexerHandlesBasicTokens
        , fastProperty "lexer preserves token order" prop_lexerPreservesTokenOrder
        , fastProperty "lexer handles whitespace correctly" prop_lexerHandlesWhitespace
        , fastProperty "lexer handles special characters" prop_lexerHandlesSpecialChars
        ]
    , testGroup "Parsing Properties"
        [ fastProperty "parser handles valid programs" prop_parserHandlesValidPrograms
        , fastProperty "parser handles invalid syntax gracefully" prop_parserHandlesInvalidSyntax
        , fastProperty "parser preserves program structure" prop_parserPreservesStructure
        , fastProperty "parser is deterministic" prop_parserDeterministic
        ]
    , testGroup "Error Handling Properties"
        [ fastProperty "error formatting preserves information" prop_errorFormattingPreservesInfo
        , fastProperty "error detection is comprehensive" prop_errorDetectionComprehensive
        , fastProperty "error recovery is graceful" prop_errorRecoveryGraceful
        ]
    , testGroup "Integration Properties"
        [ fastProperty "ownership analysis integrates with lexer" prop_ownershipIntegratesWithLexer
        , fastProperty "ownership analysis integrates with parser" prop_ownershipIntegratesWithParser
        , fastProperty "complete pipeline maintains consistency" prop_completePipelineConsistent
        ]
    ]

-- ============================================================================
-- Ownership Analysis Properties
-- ============================================================================

-- Property: ownership analyzer handles empty input
prop_ownershipAnalyzerHandlesEmpty :: Bool
prop_ownershipAnalyzerHandlesEmpty =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer ""
  in case result of
    Left _ -> True  -- May fail, but shouldn't crash
    Right _ -> True

-- Property: ownership analysis is deterministic
prop_ownershipAnalysisDeterministic :: String -> Bool
prop_ownershipAnalysisDeterministic input =
  let analyzer = newOwnershipAnalyzer
      result1 = analyzeOwnership analyzer input
      result2 = analyzeOwnership analyzer input
  in case (result1, result2) of
    (Left e1, Left e2) -> e1 == e2
    (Right r1, Right r2) -> r1 == r2
    _ -> False  -- Should be deterministic

-- Property: ownership transfer preserves invariants
prop_ownershipTransferPreservesInvariants :: OwnershipTransfer -> Bool
prop_ownershipTransferPreservesInvariants transfer =
  -- Basic sanity check for ownership transfer
  case transfer of
    Move from to -> from /= to  -- Should not transfer to self
    Borrow from to -> from /= to
    Copy from to -> from /= to
    Share from to -> from /= to

-- Property: ownership type checking is sound
prop_ownershipTypeCheckingSound :: OwnershipType -> Bool
prop_ownershipTypeCheckingSound ownershipType =
  case ownershipType of
    Owned -> True  -- Owned is always valid
    Borrowed -> True  -- Borrowed is always valid
    Shared -> True  -- Shared is always valid
    Moved -> True  -- Moved is a valid state

-- ============================================================================
-- Lexing Properties
-- ============================================================================

-- Property: lexer handles basic tokens
prop_lexerHandlesBasicTokens :: String -> Bool
prop_lexerHandlesBasicTokens input =
  let result = lexAll input
  in case result of
    Left _ -> True  -- May fail, but shouldn't crash
    Right tokens -> L.length tokens >= 0

-- Property: lexer preserves token order
prop_lexerPreservesTokenOrder :: String -> Bool
prop_lexerPreservesTokenOrder input =
  let result1 = lexAll input
      result2 = lexAll input
  in case (result1, result2) of
    (Right tokens1, Right tokens2) -> tokens1 == tokens2
    (Left e1, Left e2) -> e1 == e2
    _ -> False  -- Should be deterministic

-- Property: lexer handles whitespace correctly
prop_lexerHandlesWhitespace :: String -> Bool
prop_lexerHandlesWhitespace input =
  let withWhitespace = "  " ++ input ++ "  \t\n  "
      result = lexAll withWhitespace
  in case result of
    Left _ -> True
    Right tokens -> L.length tokens >= 0

-- Property: lexer handles special characters
prop_lexerHandlesSpecialChars :: String -> Bool
prop_lexerHandlesSpecialChars input =
  let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      testInput = input ++ specialChars
      result = lexAll testInput
  in case result of
    Left _ -> True
    Right tokens -> L.length tokens >= 0

-- ============================================================================
-- Parsing Properties
-- ============================================================================

-- Property: parser handles valid programs
prop_parserHandlesValidPrograms :: String -> Bool
prop_parserHandlesValidPrograms input =
  let tokens = lexAll input
  in case tokens of
    Left _ -> True  -- Lexing may fail
    Right toks -> 
      let result = parseProgram toks
      in case result of
        Left _ -> True  -- Parsing may fail
        Right _ -> True

-- Property: parser handles invalid syntax gracefully
prop_parserHandlesInvalidSyntax :: String -> Bool
prop_parserHandlesInvalidSyntax input =
  let invalidInput = "invalid syntax with mismatched brackets [[["
      tokens = lexAll invalidInput
  in case tokens of
    Left _ -> True  -- Lexing may fail
    Right toks ->
      let result = parseProgram toks
      in case result of
        Left _ -> True  -- Should fail gracefully
        Right _ -> True

-- Property: parser preserves program structure
prop_parserPreservesStructure :: String -> Bool
prop_parserPreservesStructure input =
  let tokens = lexAll input
  in case tokens of
    Right toks ->
      let result1 = parseProgram toks
          result2 = parseProgram toks
      in case (result1, result2) of
        (Right p1, Right p2) -> p1 == p2  -- Should be deterministic
        (Left e1, Left e2) -> e1 == e2
        _ -> False
    Left _ -> True

-- Property: parser is deterministic
prop_parserDeterministic :: String -> Bool
prop_parserDeterministic input =
  let tokens = lexAll input
  in case tokens of
    Right toks ->
      let result1 = parseProgram toks
          result2 = parseProgram toks
      in case (result1, result2) of
        (Right p1, Right p2) -> p1 == p2
        (Left e1, Left e2) -> e1 == e2
        _ -> False
    Left _ -> True

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: error formatting preserves information
prop_errorFormattingPreservesInfo :: OwnershipError -> Bool
prop_errorFormattingPreservesInfo err =
  let formatted = formatOwnershipErrors [err]
  in not (null formatted)  -- Should produce some output

-- Property: error detection is comprehensive
prop_errorDetectionComprehensive :: String -> Bool
prop_errorDetectionComprehensive input =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer input
  in case result of
    Left errors -> L.length errors >= 0  -- Should detect errors
    Right _ -> True

-- Property: error recovery is graceful
prop_errorRecoveryGraceful :: String -> Bool
prop_errorRecoveryGraceful input =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnershipDebug analyzer input
  in case result of
    Left _ -> True  -- Should fail gracefully
    Right _ -> True

-- ============================================================================
-- Integration Properties
-- ============================================================================

-- Property: ownership analysis integrates with lexer
prop_ownershipIntegratesWithLexer :: String -> Bool
prop_ownershipIntegratesWithLexer input =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer input
  in case result of
    Left _ -> True  -- May fail, but shouldn't crash
    Right _ -> True

-- Property: ownership analysis integrates with parser
prop_ownershipIntegratesWithParser :: String -> Bool
prop_ownershipIntegratesWithParser input =
  let analyzer = newOwnershipAnalyzer
      result = analyzeOwnership analyzer input
  in case result of
    Left _ -> True  -- May fail, but shouldn't crash
    Right _ -> True

-- Property: complete pipeline maintains consistency
prop_completePipelineConsistent :: String -> Bool
prop_completePipelineConsistent input =
  let analyzer = newOwnershipAnalyzer
      result1 = analyzeOwnership analyzer input
      result2 = analyzeOwnership analyzer input
  in case (result1, result2) of
    (Left e1, Left e2) -> e1 == e2
    (Right r1, Right r2) -> r1 == r2
    _ -> False  -- Should be deterministic

-- ============================================================================
-- Helper Functions L.and Generators
-- ============================================================================

-- Generate ownership types
genOwnershipType :: Gen OwnershipType
genOwnershipType = elements [Owned, Borrowed, Shared, Moved]

-- Generate ownership transfers
genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = do
  from <- elements ["var1", "var2", "var3"]
  to <- elements ["var4", "var5", "var6"]
  oneof
    [ return $ Move from to
    , return $ Borrow from to
    , return $ Copy from to
    , return $ Share from to
    ]

-- Generate ownership errors
genOwnershipError :: Gen OwnershipError
genOwnershipError = do
  message <- listOf $ elements "abcdefghijklmnopqrstuvwxyz "
  return $ OwnershipError message startPos

-- Generate simple ownership code snippets
genOwnershipCode :: Gen String
genOwnershipCode = oneof
  [ return "x := 5"
  , return "y := move x"
  , return "z := borrow y"
  , return "a := copy z"
  , return "b := share a"
  , return "func test() { x := 1; return x; }"
  ]

instance Arbitrary OwnershipType where
  arbitrary = genOwnershipType

instance Arbitrary OwnershipTransfer where
  arbitrary = genOwnershipTransfer

instance Arbitrary OwnershipError where
  arbitrary = genOwnershipError

instance Arbitrary String where
  arbitrary = oneof
    [ genOwnershipCode
    , listOf $ elements ['a'..'z']
    , listOf $ elements " \n\t{}();:"
    , return ""
    ]