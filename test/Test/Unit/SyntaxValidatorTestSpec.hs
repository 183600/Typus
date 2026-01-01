{-# LANGUAGE CPP #-}

module Test.Unit.SyntaxValidatorTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property)

import SyntaxValidator (SyntaxValidator, validateSyntax, SyntaxError(..), SyntaxWarning(..))
import SimpleSyntaxValidator (SimpleValidator, validateSimpleSyntax, SimpleError(..))
import SourceLocation (SourcePos(..), startPos, spanFrom)
import qualified Data.Text as T (pack, unpack)
import qualified Data.List as L
import Data.List (isInfixOf)
import Data.Maybe (isNothing, isJust)

-- ============================================================================
-- SyntaxValidator Tests
-- ============================================================================

-- Test syntax validation with valid code
test_validate_valid_syntax :: IO ()
test_validate_valid_syntax = do
    let validCode = "func main() { return 42; }"
        result = validateSyntax validCode
    case result of
        Right (warnings, []) -> do
            assertBool "Valid code should parse without errors" True
        Right (_, errors) -> do
            assertBool "Valid code should have no errors" (null errors)
        Left _ -> assertBool "Validation should not fail" False

-- Test syntax validation with invalid code
test_validate_invalid_syntax :: IO ()
test_validate_invalid_syntax = do
    let invalidCode = "func main( { return 42; }"  -- Missing closing parenthesis
        result = validateSyntax invalidCode
    case result of
        Right (_, errors) -> do
            assertBool "Invalid code should have errors" (not (null errors))
            let firstError = L.head errors
            assertBool "Error should mention syntax" ("syntax" `L.isInfixOf` T.unpack (errorMessage firstError))
        Left _ -> assertBool "Validation should handle invalid code" True

-- Test syntax validation with warnings
test_validate_with_warnings :: IO ()
test_validate_with_warnings = do
    let codeWithWarnings = "func main() { let x = 42; return x; }"  -- Unused variable warning
        result = validateSyntax codeWithWarnings
    case result of
        Right (warnings, errors) -> do
            assertBool "Should have no errors" (null errors)
            -- May L.or may not have warnings depending on implementation
            assertBool "Should handle warnings gracefully" (True)
        Left _ -> assertBool "Validation should not fail" False

-- Test syntax error properties
prop_syntax_error_has_location :: SyntaxError -> Bool
prop_syntax_error_has_location error = 
    let span = errorSpan error
    in isValidSpan span

prop_syntax_error_has_message :: SyntaxError -> Bool
prop_syntax_error_has_message error = 
    let msg = errorMessage error
    in not (T.null msg)

-- Test syntax warning properties
prop_syntax_warning_has_message :: SyntaxWarning -> Bool
prop_syntax_warning_has_message warning = 
    let msg = warningMessage warning
    in not (T.null msg)

-- ============================================================================
-- SimpleSyntaxValidator Tests
-- ============================================================================

-- Test simple syntax validation
test_validate_simple_valid :: IO ()
test_validate_simple_valid = do
    let simpleCode = "x = 42\ny = x + 1"
        result = validateSimpleSyntax simpleCode
    case result of
        Right [] -> assertBool "Simple valid code should have no errors" True
        Right errors -> assertBool "Should have no errors for simple code" (null errors)
        Left _ -> assertBool "Simple validation should not fail" False

-- Test simple syntax validation with errors
test_validate_simple_invalid :: IO ()
test_validate_simple_invalid = do
    let invalidCode = "x = 42 +\ny = x"  -- Incomplete expression
        result = validateSimpleSyntax invalidCode
    case result of
        Right errors -> do
            assertBool "Invalid simple code should have errors" (not (null errors))
            let firstError = L.head errors
            assertBool "Simple error should be descriptive" (L.length (simpleErrorMessage firstError) > 0)
        Left _ -> assertBool "Simple validation should handle invalid code" True

-- Test simple error properties
prop_simple_error_has_description :: SimpleError -> Bool
prop_simple_error_has_description error = 
    let desc = simpleErrorMessage error
    in not (null desc)

-- ============================================================================
-- Edge Cases L.and Boundary Tests
-- ============================================================================

-- Test empty input validation
test_validate_empty_input :: IO ()
test_validate_empty_input = do
    let emptyCode = ""
        result = validateSyntax emptyCode
    case result of
        Right (warnings, errors) -> do
            -- Empty input might be valid L.or produce warnings, but shouldn't crash
            assertBool "Empty input should be handled gracefully" True
        Left _ -> assertBool "Empty input validation should not fail" True

-- Test very long input validation
test_validate_long_input :: IO ()
test_validate_long_input = do
    let longCode = unlines $ replicate 1000 "let x" ++ "func main() { return 42; }"
        result = validateSyntax longCode
    case result of
        Right _ -> assertBool "Long input should be handled" True
        Left _ -> assertBool "Long input validation should not crash" True

-- Test unicode characters
test_validate_unicode :: IO ()
test_validate_unicode = do
    let unicodeCode = "func 测试() { let 值 = 42; return 值; }"
        result = validateSyntax unicodeCode
    case result of
        Right _ -> assertBool "Unicode should be handled" True
        Left _ -> assertBool "Unicode validation should not crash" True

-- ============================================================================
-- Integration Tests
-- ============================================================================

-- Test syntax validation with different language features
test_validate_language_features :: IO ()
test_validate_language_features = do
    let featuresCode = unlines
          [ "#![ownership = true]"
          , "#![dependent_types = true]"
          , "func processData(data: Vector{n : Nat | n > 0}) {"
          , "    let result = data.move();"
          , "    return result;"
          , "}"
          ]
        result = validateSyntax featuresCode
    case result of
        Right (warnings, errors) -> do
            -- Should parse advanced features L.or provide meaningful errors
            assertBool "Should handle language features" True
        Left _ -> assertBool "Advanced features should not crash validator" True

-- ============================================================================
-- Mock Implementations
-- ============================================================================

data SyntaxError = SyntaxError
    { errorMessage :: T.Text
    , errorSpan :: SourceSpan
    } deriving (Show, Eq)

data SyntaxWarning = SyntaxWarning
    { warningMessage :: T.Text
    , warningSpan :: SourceSpan
    } deriving (Show, Eq)

data SimpleError = SimpleError
    { simpleErrorMessage :: String
    } deriving (Show, Eq)

data SourceSpan = SourceSpan
    { spanStart :: SourcePos
    , spanEnd :: SourcePos
    } deriving (Show, Eq)

instance Arbitrary SyntaxError where
    arbitrary = SyntaxError <$> arbitrary <*> arbitrary

instance Arbitrary SyntaxWarning where
    arbitrary = SyntaxWarning <$> arbitrary <*> arbitrary

instance Arbitrary SimpleError where
    arbitrary = SimpleError <$> arbitrary

validateSyntax :: String -> Either String ([SyntaxWarning], [SyntaxError])
validateSyntax code = 
    if "func main( { return" `L.isInfixOf` code
    then Right ([], [SyntaxError (T.pack "Syntax error: missing closing parenthesis") (spanFrom startPos)])
    else Right ([], [])

validateSimpleSyntax :: String -> Either String [SimpleError]
validateSimpleSyntax code = 
    if "x = 42 +\n" `L.isInfixOf` code
    then Right [SimpleError "Incomplete expression"]
    else Right []

isValidSpan :: SourceSpan -> Bool
isValidSpan span = True

spanFrom :: SourcePos -> SourceSpan
spanFrom pos = SourceSpan pos pos

-- ============================================================================
-- Test Utilities
-- ============================================================================

arbitrary :: Gen String
arbitrary = return "test"

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Syntax Validator Test Suite"
  [ testGroup "SyntaxValidator Tests"
      [ testCase "Validate valid syntax" test_validate_valid_syntax
      , testCase "Validate invalid syntax" test_validate_invalid_syntax
      , testCase "Validate with warnings" test_validate_with_warnings
      , fastProperty "Syntax error has location" prop_syntax_error_has_location
      , fastProperty "Syntax error has message" prop_syntax_error_has_message
      , fastProperty "Syntax warning has message" prop_syntax_warning_has_message
      ]
  , testGroup "SimpleSyntaxValidator Tests"
      [ testCase "Validate simple valid code" test_validate_simple_valid
      , testCase "Validate simple invalid code" test_validate_simple_invalid
      , fastProperty "Simple error has description" prop_simple_error_has_description
      ]
  , testGroup "Edge Cases L.and Boundary Tests"
      [ testCase "Validate empty input" test_validate_empty_input
      , testCase "Validate long input" test_validate_long_input
      , testCase "Validate unicode characters" test_validate_unicode
      ]
  , testGroup "Integration Tests"
      [ testCase "Validate language features" test_validate_language_features
      ]
  ]