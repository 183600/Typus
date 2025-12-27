{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, choose, elements, listOf
  , oneof, suchThat, vectorOf, Positive(..), NonNegative(..)
  )

import Utils
  ( trim, splitBy, splitByCollapsed, splitByComma, removeLineComments
  , removeComments, normalizeIndentation
  )
import SourceLocation
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, posAt, emptySpan, spanFrom, mergeSpans
  , locatedAt, locatedWithSpan, locatedValue, locatedSpan
  , advancePos, isValidSpan
  )
import Compiler.Errors.Core
  ( ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..)
  , ErrorCollector, newErrorCollector, addError, addWarning
  , getErrors, getWarnings, hasErrors, hasWarnings, formatError
  , errorAt, warningAt, errorWithCategory
  )
import Parser
  ( FileDirectives(..), BlockDirectives(..)
  , defaultFileDirectives, defaultBlockDirectives, parseTypus
  )
import Data.Char (isSpace, isDigit, isLetter)
import Data.List (isPrefixOf, isSuffixOf, sort, nub)
import qualified Data.Text as T

-- ============================================================================
-- Utils Module Tests
-- ============================================================================

-- Property: trim removes leading and trailing whitespace
prop_trim_removes_whitespace :: String -> Property
prop_trim_removes_whitespace s =
  not (null (trim s)) ==> 
  let trimmed = trim s
  in counterexample "trimmed string should not start or end with whitespace" $
     (null trimmed || not (isSpace (head trimmed))) &&
     (null trimmed || not (isSpace (last trimmed)))

-- Property: splitBy consistency with splitByCollapsed for non-empty segments
prop_splitBy_consistency :: Char -> String -> Property
prop_splitBy_consistency c s =
  not (c `elem` s) ==> splitBy c s === splitByCollapsed c s

-- Property: removeLineComments preserves non-comment lines
prop_removeLineComments_preserves :: String -> Property
prop_removeLineComments_preserves s =
  not ("//" `isPrefixOf` s) ==> removeLineComments s === s

-- ============================================================================
-- SourceLocation Module Tests  
-- ============================================================================

-- Property: startPos is always valid
prop_startPos_valid :: Property
prop_startPos_valid =
  let pos = startPos
  in counterexample "startPos should have positive line and column" $
     posLine pos > 0 && posColumn pos > 0

-- Property: mergeSpans is commutative for valid spans
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_commutative span1 span2 =
  isValidSpan span1 && isValidSpan span2 ==>
  mergeSpans span1 span2 === mergeSpans span2 span1

-- Property: advancePos correctly handles newlines
prop_advancePos_newline :: Positive Int -> Positive Int -> Property
prop_advancePos_newline (Positive lines) (Positive cols) =
  let start = startPos
      result = advancePos start '\n'
  in counterexample "newline should advance line but reset column" $
     posLine result == posLine start + 1 && posColumn result == 1

-- ============================================================================
-- Error Handling Tests
-- ============================================================================

-- Test: Basic error collection and retrieval
test_error_collection :: IO ()
test_error_collection = do
  collector <- newErrorCollector
  addError collector (errorAt "Test error" startPos)
  errors <- getErrors collector
  assertBool "Should have one error" (length errors == 1)
  assertBool "Should have errors" (hasErrors collector)
  
-- Test: Warning collection separate from errors
test_warning_collection :: IO ()
test_warning_collection = do
  collector <- newErrorCollector
  addWarning collector (warningAt "Test warning" startPos)
  errors <- getErrors collector
  warnings <- getWarnings collector
  assertBool "Should have no errors" (null errors)
  assertBool "Should have one warning" (length warnings == 1)
  assertBool "Should have warnings" (hasWarnings collector)

-- ============================================================================
-- Parser Tests
-- ============================================================================

-- Test: Parse simple valid Typus code
test_parse_simple_code :: IO ()
test_parse_simple_code = do
  let source = "package main\n\nfunc main() {\n    return 0\n}"
  case parseTypus source of
    Left err -> assertFailure $ "Failed to parse simple code: " ++ err
    Right _ -> return ()  -- Success

-- Test: Parse file with directives
test_parse_directives :: IO ()
test_parse_directives = do
  let source = unlines 
        [ "//! ownership: on"
        , "//! dependent_types: off" 
        , "package main"
        , "func main() {}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "Failed to parse directives: " ++ err
    Right typusFile -> do
      let directives = tfDirectives typusFile
      case fdOwnership directives of
        Nothing -> assertFailure "Expected ownership directive"
        Just loc -> locatedValue loc @?= True
      case fdDependentTypes directives of
        Nothing -> assertFailure "Expected dependent types directive"
        Just loc -> locatedValue loc @?= False

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal Tests"
  [ testGroup "Utils Properties"
      [ fastProperty "trim removes whitespace" prop_trim_removes_whitespace
      , fastProperty "splitBy consistency" prop_splitBy_consistency  
      , fastProperty "removeLineComments preserves" prop_removeLineComments_preserves
      ]
      
  , testGroup "SourceLocation Properties"
      [ fastProperty "startPos valid" prop_startPos_valid
      , fastProperty "mergeSpans commutative" prop_mergeSpans_commutative
      , fastProperty "advancePos handles newline" prop_advancePos_newline
      ]
      
  , testGroup "Error Handling"
      [ testCase "error collection" test_error_collection
      , testCase "warning collection" test_warning_collection
      ]
      
  , testGroup "Parser"
      [ testCase "parse simple code" test_parse_simple_code
      , testCase "parse directives" test_parse_directives
      ]
  ]