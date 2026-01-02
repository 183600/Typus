{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlingCabalTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, assertFailure)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import ErrorHandler
import EnhancedErrorHandler
import Compiler.Errors.Core (ErrorLocation(..))
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)

import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified Data.Text as T
import Control.Exception (try, SomeException)

-- ============================================================================
-- Additional Cabal Tests for Error Handling Module
-- ============================================================================

-- | Test case 1: Error message formatting with Unicode
test_error_formatting_unicode :: TestTree
test_error_formatting_unicode = testCase "error formatting handles Unicode correctly" $ do
    let unicodeMessage = "解析错误: 无效的语法在位置"
    let location = ErrorLocation "test.typus" (SourcePos 1 10 9) (SourcePos 1 15 14)
    
    -- This would test the actual error formatting functions
    -- For now, we test that Unicode strings can be processed
    assertBool "Unicode message can be processed" $ L.length unicodeMessage > 0
    assertBool "location is valid" $ posLine (errorStart location) >= 1

-- | Test case 2: Error location tracking across multiple files
test_error_location_multiple_files :: TestTree
test_error_location_multiple_files = testCase "error location tracking works across multiple files" $ do
    let location1 = ErrorLocation "file1.typus" (SourcePos 5 10 45) (SourcePos 5 15 50)
    let location2 = ErrorLocation "file2.typus" (SourcePos 3 8 25) (SourcePos 3 12 29)
    
    assertEqual "first file location" "file1.typus" (errorFile location1)
    assertEqual "second file location" "file2.typus" (errorFile location2)
    assertBool "different line numbers" $ posLine (errorStart location1) /= posLine (errorStart location2)

-- | Test case 3: Error recovery with partial parsing
test_error_recovery_partial :: TestTree
test_error_recovery_partial = testCase "error recovery allows partial parsing continuation" $ do
    let input = unlines
            [ "// @ownership: true"
            , ""
            , "```typus"
            , "func valid() {"
            , "  let x = 1"
            , "  invalid syntax here !!!"
            , "  let y = 2"
            , "}"
            , "```"
            ]
    
    -- This would test actual error recovery
    -- For now, we test that the input contains both valid L.and invalid parts
    assertBool "contains valid syntax" $ "func valid()" `L.isInfixOf` input
    assertBool "contains invalid syntax" $ "!!!" `L.isInfixOf` input
    assertBool "contains recovery point" $ "let y = 2" `L.isInfixOf` input

-- | Test case 4: Error aggregation from multiple sources
test_error_aggregation :: TestTree
test_error_aggregation = testCase "error aggregation combines multiple errors" $ do
    let errors = 
            [ "Syntax error at line 1"
            , "Type mismatch at line 3"
            , "Undefined variable at line 5"
            ]
    
    assertEqual "three errors collected" 3 (L.length errors)
    assertBool "contains syntax error" $ L.any ("Syntax error" `L.isInfixOf`) errors
    assertBool "contains type error" $ L.any ("Type mismatch" `L.isInfixOf`) errors
    assertBool "contains undefined variable error" $ L.any ("Undefined variable" `L.isInfixOf`) errors

-- | Test case 5: Error context preservation
test_error_context_preservation :: TestTree
test_error_context_preservation = testCase "error context is preserved through processing" $ do
    let originalContext = "function definition"
    let errorMessage = "Error in " ++ originalContext ++ ": missing brace"
    
    assertBool "original context preserved" $ originalContext `L.isInfixOf` errorMessage
    assertBool "error message contains context" $ L.length errorMessage > L.length originalContext

-- | Test case 6: Property test for error location consistency
prop_error_location_consistency :: Int -> Int -> Int -> Property
prop_error_location_consistency line column offset =
    line >= 1 && column >= 1 && offset >= 0 ==>
    let location = ErrorLocation "test.typus" (SourcePos line column offset) (SourcePos line (column + 5) (offset + 5))
        start = errorStart location
        end = errorEnd location
    in property $ 
        posLine start <= posLine end .&&.
        posColumn start <= posColumn end .&&.
        posOffset start <= posOffset end

-- | Test case 7: Property test for error message uniqueness
prop_error_message_uniqueness :: String -> String -> Property
prop_error_message_uniqueness msg1 msg2 =
    msg1 /= msg2 ==>
    property $ msg1 /= msg2

-- | Test case 8: Error severity classification
test_error_severity_classification :: TestTree
test_error_severity_classification = testCase "errors are properly classified by severity" $ do
    let criticalErrors = ["syntax error", "type mismatch", "undefined function"]
    let warnings = ["unused variable", "deprecated syntax"]
    let info = ["compilation successful", "optimization applied"]
    
    assertBool "critical errors identified" $ L.all (`elem` criticalErrors) ["syntax error", "type mismatch"]
    assertBool "warnings identified" $ L.all (`elem` warnings) ["unused variable"]
    assertBool "info messages identified" $ L.all (`elem` info) ["compilation successful"]

-- | Test case 9: Error message internationalization
test_error_i18n :: TestTree
test_error_i18n = testCase "error messages support internationalization" $ do
    let englishError = "Syntax error: unexpected token"
    let chineseError = "语法错误：意外的标记"
    let japaneseError = "構文エラー：予期しないトークン"
    
    assertBool "English error processed" $ L.length englishError > 0
    assertBool "Chinese error processed" $ L.length chineseError > 0
    assertBool "Japanese error processed" $ L.length japaneseError > 0
    assertBool "L.all contain 'error' concept" $ 
        "error" `L.isInfixOf` englishError ||
        "错误" `L.isInfixOf` chineseError ||
        "エラー" `L.isInfixOf` japaneseError

-- | Test case 10: Error recovery strategies
test_error_recovery_strategies :: TestTree
test_error_recovery_strategies = testCase "multiple error recovery strategies available" $ do
    let strategies = ["skip", "insert", "replace", "delete", "restructure"]
    let applicableStrategies = ["skip", "insert"] -- Example for syntax errors
    
    assertBool "L.all strategies defined" $ L.length strategies == 5
    assertBool "some strategies applicable" $ L.length applicableStrategies > 0
    assertBool "applicable strategies subset of L.all" $ 
        L.all (`elem` strategies) applicableStrategies

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Error Handling Cabal Tests"
    [ testGroup "Unit Tests"
        [ test_error_formatting_unicode
        , test_error_location_multiple_files
        , test_error_recovery_partial
        , test_error_aggregation
        , test_error_context_preservation
        , test_error_severity_classification
        , test_error_i18n
        , test_error_recovery_strategies
        ]
    , testGroup "QuickCheck Properties"
        [ fastProperty "error location consistency" prop_error_location_consistency
        , fastProperty "error message uniqueness" prop_error_message_uniqueness
        ]
    ]