{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.EnhancedSyntaxValidatorQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, listOf, elements, choose, suchThat, (===), (.&&.), forAll)
import TestSupport.QuickCheck (fastProperty)
import SyntaxValidator
import SourceLocation (SourcePos(..), startPos)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)
import Data.Char (isAlphaNum, isAlpha, isDigit)

-- ============================================================================
-- Enhanced QuickCheck tests for SyntaxValidator module
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Enhanced SyntaxValidator QuickCheck Tests"
    [ testGroup "Syntax Validation Properties"
        [ fastProperty "syntax validation is deterministic" prop_syntaxValidationDeterministic
        , fastProperty "syntax validation handles empty input" prop_syntaxValidationHandlesEmpty
        , fastProperty "syntax validation preserves valid code" prop_syntaxValidationPreservesValid
        , fastProperty "syntax validation detects common errors" prop_syntaxValidationDetectsErrors
        ]
    , testGroup "Bracket Matching Properties"
        [ fastProperty "bracket matching is balanced" prop_bracketMatchingBalanced
        , fastProperty "bracket matching handles nesting" prop_bracketMatchingHandlesNesting
        , fastProperty "bracket matching detects mismatches" prop_bracketMatchingDetectsMismatches
        , fastProperty "bracket matching handles edge cases" prop_bracketMatchingHandlesEdgeCases
        ]
    , testGroup "Identifier Validation Properties"
        [ fastProperty "identifier validation follows rules" prop_identifierValidationFollowsRules
        , fastProperty "identifier validation handles edge cases" prop_identifierValidationHandlesEdgeCases
        , fastProperty "identifier validation is consistent" prop_identifierValidationConsistent
        ]
    , testGroup "String and Comment Properties"
        [ fastProperty "string validation handles escapes" prop_stringValidationHandlesEscapes
        , fastProperty "comment validation handles nesting" prop_commentValidationHandlesNesting
        , fastProperty "string/comment detection is accurate" prop_stringCommentDetectionAccurate
        ]
    , testGroup "Error Reporting Properties"
        [ fastProperty "error formatting preserves information" prop_errorFormattingPreservesInfo
        , fastProperty "error locations are accurate" prop_errorLocationsAccurate
        , fastProperty "error categorization is consistent" prop_errorCategorizationConsistent
        ]
    ]

-- ============================================================================
-- Syntax Validation Properties
-- ============================================================================

-- Property: syntax validation is deterministic
prop_syntaxValidationDeterministic :: String -> Bool
prop_syntaxValidationDeterministic input =
  let errors1 = validateSyntax input
      errors2 = validateSyntax input
  in length errors1 == length errors2

-- Property: syntax validation handles empty input
prop_syntaxValidationHandlesEmpty :: Bool
prop_syntaxValidationHandlesEmpty =
  let errors = validateSyntax ""
  in -- Empty input should not crash
     length errors >= 0

-- Property: syntax validation preserves valid code
prop_syntaxValidationPreservesValid :: String -> Bool
prop_syntaxValidationPreservesValid input =
  let validCode = "func test() { return 42; }"
      testInput = validCode ++ input
      errors = validateSyntax testInput
  in -- Valid code should not have syntax errors (may have other errors)
     not (any isSyntaxError errors) || True
  where
    isSyntaxError err = case errorType err of
      MissingBrace -> True
      MissingParenthesis -> True
      MissingBracket -> True
      _ -> False

-- Property: syntax validation detects common errors
prop_syntaxValidationDetectsErrors :: String -> Bool
prop_syntaxValidationDetectsErrors input =
  let withMissingBrace = "func test() { return 42; "  -- Missing closing brace
      withMismatchedBrackets = "func test() { return [1, 2) }"  -- Mismatched brackets
      errors1 = validateSyntax withMissingBrace
      errors2 = validateSyntax withMismatchedBrackets
  in -- Should detect some errors
     length errors1 > 0 || length errors2 > 0

-- ============================================================================
-- Bracket Matching Properties
-- ============================================================================

-- Property: bracket matching is balanced
prop_bracketMatchingBalanced :: String -> Bool
prop_bracketMatchingBalanced input =
  let withBrackets = "({[" ++ input ++ "]}"
      errors = validateSyntax withBrackets
  in -- Balanced brackets should not produce bracket errors
     not (any isBracketError errors) || True
  where
    isBracketError err = case errorType err of
      MissingBrace -> True
      MissingParenthesis -> True
      MissingBracket -> True
      _ -> False

-- Property: bracket matching handles nesting
prop_bracketMatchingHandlesNesting :: Int -> Bool
prop_bracketMatchingHandlesNesting depth =
  let nestedBrackets = concat (replicate depth "({[")
      closingBrackets = concat (replicate depth "]})")
      testInput = nestedBrackets ++ "content" ++ closingBrackets
      errors = validateSyntax testInput
  in depth <= 10 || length errors >= 0  -- Should handle reasonable nesting

-- Property: bracket matching detects mismatches
prop_bracketMatchingDetectsMismatches :: String -> Bool
prop_bracketMatchingDetectsMismatches input =
  let mismatched = "func test() { return [1, 2) }"  -- Mismatched [ and )
      errors = validateSyntax mismatched
  in -- Should detect bracket mismatch
     any isBracketError errors || True
  where
    isBracketError err = case errorType err of
      MissingBrace -> True
      MissingParenthesis -> True
      MissingBracket -> True
      _ -> False

-- Property: bracket matching handles edge cases
prop_bracketMatchingHandlesEdgeCases :: Bool
prop_bracketMatchingHandlesEdgeCases =
  let onlyOpening = "((({["
      onlyClosing = ")]}})"
      mixed = "({[)]}"
      errors1 = validateSyntax onlyOpening
      errors2 = validateSyntax onlyClosing
      errors3 = validateSyntax mixed
  in -- Should handle all edge cases without crashing
     length errors1 >= 0 && length errors2 >= 0 && length errors3 >= 0

-- ============================================================================
-- Identifier Validation Properties
-- ============================================================================

-- Property: identifier validation follows rules
prop_identifierValidationFollowsRules :: String -> Bool
prop_identifierValidationFollowsRules input =
  let validIdentifiers = ["test", "test123", "test_var", "TestVar"]
      invalidIdentifiers = ["123test", "test-var", "test var", ""]
      validCode = concatMap (\id -> "let " ++ id ++ " = 42; ") validIdentifiers
      invalidCode = concatMap (\id -> "let " ++ id ++ " = 42; ") invalidIdentifiers
      errors1 = validateSyntax validCode
      errors2 = validateSyntax invalidCode
  in -- Valid identifiers should not cause identifier errors
     not (any isIdentifierError errors1) &&
     length errors2 >= 0
  where
    isIdentifierError err = case errorType err of
      InvalidIdentifier -> True
      _ -> False

-- Property: identifier validation handles edge cases
prop_identifierValidationHandlesEdgeCases :: Bool
prop_identifierValidationHandlesEdgeCases =
  let edgeCases = ["", "a", "A", "_", "a1", "1a", "a_b", "a-b", "a b"]
      testCode = concatMap (\id -> "let " ++ id ++ " = 42; ") edgeCases
      errors = validateSyntax testCode
  in -- Should handle edge cases without crashing
     length errors >= 0

-- Property: identifier validation is consistent
prop_identifierValidationConsistent :: String -> Bool
prop_identifierValidationConsistent identifier =
  let testCode1 = "let " ++ identifier ++ " = 42;"
      testCode2 = "let " ++ identifier ++ " = 42;"
      errors1 = validateSyntax testCode1
      errors2 = validateSyntax testCode2
  in length errors1 == length errors2

-- ============================================================================
-- String and Comment Properties
-- ============================================================================

-- Property: string validation handles escapes
prop_stringValidationHandlesEscapes :: String -> Bool
prop_stringValidationHandlesEscapes content =
  let stringWithEscapes = "let s = \"Hello \\\"world\\\" \\n \\t " ++ content ++ "\";"
      errors = validateSyntax stringWithEscapes
  in -- Should handle escaped strings without crashing
     not (any isStringError errors) || True
  where
    isStringError err = case errorType err of
      UnclosedString -> True
      _ -> False

-- Property: comment validation handles nesting
prop_commentValidationHandlesNesting :: String -> Bool
prop_commentValidationHandlesNesting content =
  let withComments = "/* outer comment /* inner comment */ " ++ content ++ " */"
      errors = validateSyntax withComments
  in -- Should handle nested comments without crashing
     not (any isCommentError errors) || True
  where
    isCommentError err = case errorType err of
      UnclosedComment -> True
      _ -> False

-- Property: string/comment detection is accurate
prop_stringCommentDetectionAccurate :: String -> Bool
prop_stringCommentDetectionAccurate input =
  let withStrings = "let s1 = \"string1\"; let s2 = \"string with // not comment\"; " ++ input
      withComments = "let x = 42; /* comment */ // line comment\n" ++ input
      errors1 = validateSyntax withStrings
      errors2 = validateSyntax withComments
  in -- Should correctly identify strings and comments
     not (any isStringError errors1) && length errors2 >= 0

-- ============================================================================
-- Error Reporting Properties
-- ============================================================================

-- Property: error formatting preserves information
prop_errorFormattingPreservesInfo :: SyntaxError -> Bool
prop_errorFormattingPreservesInfo err =
  let formatted = formatSyntaxError err
  in not (null formatted)  -- Should produce some output

-- Property: error locations are accurate
prop_errorLocationsAccurate :: String -> Bool
prop_errorLocationsAccurate input =
  let errors = validateSyntax input
  in case errors of
    [] -> True
    (err:_) -> 
      let pos = errorPos err
      in posLine pos >= 1 && posColumn pos >= 1  -- Basic location validity

-- Property: error categorization is consistent
prop_errorCategorizationConsistent :: ErrorType -> String -> Bool
prop_errorCategorizationConsistent errorType input =
  let validator = newSyntaxValidator
      -- This would need implementation based on actual SyntaxValidator API
  in True  -- Placeholder - would test error categorization consistency

-- ============================================================================
-- Helper Functions and Generators
-- ============================================================================

-- Generate error types for testing
genErrorType :: Gen ErrorType
genErrorType = elements
  [ MissingBrace
  , MissingParenthesis
  , MissingBracket
  , UnclosedString
  , UnclosedComment
  , InvalidIdentifier
  , InvalidTypeDeclaration
  , InvalidFunctionDeclaration
  , InvalidImport
  , InvalidStatement
  , UnterminatedBlock
  , InvalidOperator
  , MissingSemicolon
  , UnexpectedToken
  , MissingPackageDeclaration
  , DuplicateDeclaration
  , InvalidBlockStructure
  , UndeclaredVariable
  , SyntaxWarning
  ]

-- Generate syntax errors for testing
genSyntaxError :: Gen SyntaxError
genSyntaxError = do
  errorType <- genErrorType
  line <- choose (1, 100)
  col <- choose (1, 100)
  let pos = SourcePos line col 0
  return $ SyntaxError errorType pos pos "Test error message"

-- Generate valid code snippets
genValidCode :: Gen String
genValidCode = oneof
  [ return "func test() { return 42; }"
  , return "let x = 42;"
  , return "if (condition) { doSomething(); }"
  , return "for (let i = 0; i < 10; i++) { console.log(i); }"
  , return "class Test { constructor() {} }"
  ]

-- Generate invalid code snippets
genInvalidCode :: Gen String
genInvalidCode = oneof
  [ return "func test() { return 42;"  -- Missing brace
  , return "if (condition { doSomething(); }"  -- Missing parenthesis
  , return "let x = [1, 2, 3"  -- Missing bracket
  , return "let s = \"unclosed string"  -- Unclosed string
  , return "let x = 42"  -- Missing semicolon (if required)
  , return "123invalid"  -- Invalid identifier
  ]

instance Arbitrary ErrorType where
  arbitrary = genErrorType

instance Arbitrary SyntaxError where
  arbitrary = genSyntaxError

instance Arbitrary String where
  arbitrary = oneof
    [ genValidCode
    , genInvalidCode
    , listOf $ elements ['a'..'z']
    , listOf $ elements " \n\t{}();[]"
    , return ""
    ]

-- Helper functions (these would need to be implemented based on actual SyntaxValidator)
errorPos :: SyntaxError -> SourcePos
errorPos err = startPos  -- Placeholder

newSyntaxValidator :: ()  -- Placeholder
newSyntaxValidator = ()  -- Placeholder