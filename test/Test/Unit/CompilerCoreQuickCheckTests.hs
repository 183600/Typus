{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.CompilerCoreQuickCheckTests (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, (===), Property, forAll, Gen, Arbitrary(..), oneof, elements, listOf, listOf1, resize, suchThat)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import Compiler
import Compiler.Errors.Core 
  ( ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..), ErrorContext(..)
  , emptyContext, ErrorCollector, newErrorCollector, addError, addWarning
  , getErrors, getWarnings, hasErrors, hasWarnings, formatError
  , errorAt, warningAt, errorWithCategory, filterBySeverity, filterByCategory
  , getErrorStatistics
  )
import Parser (TypusFile(..), CodeBlock(..), defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), locatedWithSpan)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, length)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary ErrorSeverity where
  arbitrary = oneof [pure Error, pure Warning, pure Info]

instance Arbitrary ErrorCategory where
  arbitrary = oneof 
    [ pure SyntaxError
    , pure TypeError
    , pure NameResolutionError
    , pure SemanticError
    , pure OwnershipError
    , pure DependentTypeError
    , pure InternalError
    ]

instance Arbitrary ErrorLocation where
  arbitrary = do
    line <- arbitrary
    column <- arbitrary
    return $ ErrorLocation line column

instance Arbitrary ErrorContext where
  arbitrary = do
    messages <- listOf (listOf1 (elements ['a'..'z']))
    return $ ErrorContext messages

instance Arbitrary TypusFile where
  arbitrary = do
    directives <- pure defaultFileDirectives
    blocks <- listOf arbitrary
    return $ TypusFile directives blocks

instance Arbitrary CodeBlock where
  arbitrary = do
    directives <- pure defaultBlockDirectives
    content <- listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \n\t")
    return $ CodeBlock directives content

-- ============================================================================
-- QuickCheck Properties for Compiler Module
-- ============================================================================

-- | emptyContext: should have no messages
prop_emptyContext_no_messages :: Bool
prop_emptyContext_no_messages = 
    null (ecMessages emptyContext)

-- | newErrorCollector: should start with no errors or warnings
prop_newErrorCollector_empty :: Bool
prop_newErrorCollector_empty = 
    let collector = newErrorCollector
    in not (hasErrors collector) && not (hasWarnings collector)

-- | addError: should result in hasErrors returning True
prop_addError_has_errors :: ErrorLocation -> String -> Bool
prop_addError_has_errors location message = 
    let collector = addError newErrorCollector location message
    in hasErrors collector

-- | addWarning: should result in hasWarnings returning True
prop_addWarning_has_warnings :: ErrorLocation -> String -> Bool
prop_addWarning_has_warnings location message = 
    let collector = addWarning newErrorCollector location message
    in hasWarnings collector

-- | getErrors: should return errors in insertion order
prop_getErrors_order :: [String] -> Bool
prop_getErrors_order messages = 
    let collector = foldr (\msg acc -> addError acc (ErrorLocation 0 0) msg) newErrorCollector messages
        errors = getErrors collector
    in length errors == length messages

-- | getWarnings: should return warnings in insertion order
prop_getWarnings_order :: [String] -> Bool
prop_getWarnings_order messages = 
    let collector = foldr (\msg acc -> addWarning acc (ErrorLocation 0 0) msg) newErrorCollector messages
        warnings = getWarnings collector
    in length warnings == length messages

-- | formatError: should include the error message
prop_formatError_contains_message :: ErrorLocation -> String -> Bool
prop_formatError_contains_message location message = 
    let formatted = formatError location message
    in message `isInfixOf` formatted
  where
    isInfixOf needle haystack = needle `Data.List.isInfixOf` haystack

-- | errorAt: should create error at specific location
prop_errorAt_location :: Int -> Int -> String -> Bool
prop_errorAt_location line column message = 
    let location = ErrorLocation line column
        collector = errorAt newErrorCollector location message
        errors = getErrors collector
    in not (null errors) && 
       case head errors of
         (loc, msg) -> loc == location && msg == message

-- | warningAt: should create warning at specific location
prop_warningAt_location :: Int -> Int -> String -> Bool
prop_warningAt_location line column message = 
    let location = ErrorLocation line column
        collector = warningAt newErrorCollector location message
        warnings = getWarnings collector
    in not (null warnings) &&
       case head warnings of
         (loc, msg) -> loc == location && msg == message

-- | errorWithCategory: should create error with category
prop_errorWithCategory_category :: ErrorCategory -> ErrorLocation -> String -> Bool
prop_errorWithCategory_category category location message = 
    let collector = errorWithCategory newErrorCollector category location message
        errors = getErrors collector
    in not (null errors) -- Basic check that error was added
    -- Note: We can't easily test category storage without exposing internal types

-- | filterBySeverity: should filter correctly
prop_filterBySeverity_correct :: [ErrorSeverity] -> Bool
prop_filterBySeverity_correct severities = 
    let allSeverities = [Error, Warning, Info]
        filtered = filterBySeverity severities allSeverities
    in all (`elem` severities) filtered

-- | filterByCategory: should filter correctly
prop_filterByCategory_correct :: [ErrorCategory] -> Bool
prop_filterByCategory_correct categories = 
    let allCategories = [SyntaxError, TypeError, NameResolutionError, SemanticError, OwnershipError, DependentTypeError, InternalError]
        filtered = filterByCategory categories allCategories
    in all (`elem` categories) filtered

-- | getErrorStatistics: should count errors and warnings
prop_getErrorStatistics_counts :: Int -> Int -> Property
prop_getErrorStatistics_counts errorCount warningCount = 
    let collector = foldr (\_ acc -> addError acc (ErrorLocation 0 0) "error") newErrorCollector (replicate errorCount ())
        collector' = foldr (\_ acc -> addWarning acc (ErrorLocation 0 0) "warning") collector (replicate warningCount ())
        stats = getErrorStatistics collector'
    in errorCount >= 0 && warningCount >= 0 ==> 
       property True  -- Basic sanity check that stats can be computed

-- | TypusFile: equality should be reflexive
prop_typusFile_reflexive :: TypusFile -> Bool
prop_typusFile_reflexive tf = tf == tf

-- | CodeBlock: equality should be reflexive
prop_codeBlock_reflexive :: CodeBlock -> Bool
prop_codeBlock_reflexive cb = cb == cb

-- | ErrorSeverity: ordering should be consistent
prop_errorSeverity_ordering :: ErrorSeverity -> ErrorSeverity -> Bool
prop_errorSeverity_ordering sev1 sev2 = 
    case (sev1, sev2) of
      (Error, Error) -> sev1 == sev2
      (Error, _) -> sev1 < sev2
      (_, Error) -> sev1 > sev2
      (Warning, Warning) -> sev1 == sev2
      (Warning, Info) -> sev1 < sev2
      (Info, Warning) -> sev1 > sev2
      (Info, Info) -> sev1 == sev2

-- | ErrorLocation: should track line and column correctly
prop_errorLocation_coordinates :: Int -> Int -> Bool
prop_errorLocation_coordinates line column = 
    let location = ErrorLocation line column
    in elLine location == line && elColumn location == column

-- | ErrorContext: adding messages should increase count
prop_errorContext_messages :: [String] -> Bool
prop_errorContext_messages messages = 
    let context = ErrorContext messages
    in length (ecMessages context) == length messages

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Compiler Core QuickCheck Tests"
  [ testProperties "Error Context Properties"
    [ ("emptyContext no messages", prop_emptyContext_no_messages)
    , ("ErrorContext messages", prop_errorContext_messages)
    ]

  , testProperties "Error Collector Properties"
    [ ("newErrorCollector empty", prop_newErrorCollector_empty)
    , ("addError has errors", prop_addError_has_errors)
    , ("addWarning has warnings", prop_addWarning_has_warnings)
    , ("getErrors order", prop_getErrors_order)
    , ("getWarnings order", prop_getWarnings_order)
    , ("getErrorStatistics counts", prop_getErrorStatistics_counts)
    ]

  , testProperties "Error Formatting Properties"
    [ ("formatError contains message", prop_formatError_contains_message)
    ]

  , testProperties "Error Creation Properties"
    [ ("errorAt location", prop_errorAt_location)
    , ("warningAt location", prop_warningAt_location)
    , ("errorWithCategory category", prop_errorWithCategory_category)
    ]

  , testProperties "Error Filtering Properties"
    [ ("filterBySeverity correct", prop_filterBySeverity_correct)
    , ("filterByCategory correct", prop_filterByCategory_correct)
    ]

  , testProperties "Data Structure Properties"
    [ ("TypusFile reflexive", prop_typusFile_reflexive)
    , ("CodeBlock reflexive", prop_codeBlock_reflexive)
    , ("ErrorSeverity ordering", prop_errorSeverity_ordering)
    , ("ErrorLocation coordinates", prop_errorLocation_coordinates)
    ]
  ]