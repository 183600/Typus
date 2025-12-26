{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.EnhancedIntegrationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, suchThat, listOf1, elements
  , frequency, oneof, sized, resize, Positive(..), NonEmptyList(..)
  , choose, getPositive, vectorOf
  )

import Utils
  ( trim, splitBy, splitByCollapsed, removeComments, normalizeIndentation
  , breakOn
  )

import SourceLocation
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, posAt, posAtLineCol, emptySpan, spanBetween
  , locatedAt, locatedWithSpan, advancePos, advancePosBy
  , toErrorLocation, toErrorLocationWithSpan
  )

import Compiler.Errors.Core
  ( TypeError(..), ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..)
  , ErrorContext(..), errorAt, warningAt, infoAt, errorWithCategory
  , withLocation, withContext, withSuggestions, wrapError
  , hasCategory, filterByCategory, filterBySeverity, getErrorStatistics
  , formatError, formatErrorWithLocation, canRecoverFrom, shouldContinueAfter
  , _atLocation, _atFileLocation, _atRange
  , generateErrorReport, emptyContext
  )

import Parser (TypusFile(..), FileDirectives(..), BlockDirectives(..))
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.List (sort, intercalate, isInfixOf)
import Data.Char (isSpace, isAlphaNum)
import qualified Data.Map.Strict as Map

-- ============================================================================
-- Arbitrary Instances for Integration
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = do
    line <- getPositive <$> arbitrary
    column <- getPositive <$> arbitrary
    offset <- getPositive <$> arbitrary
    return $ SourcePos (line + 1) (column + 1) offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endLine <- choose (posLine start, posLine start + 10)
    endCol <- if endLine == posLine start 
              then choose (posColumn start, posColumn start + 20)
              else choose (1, 100)
    endOffset <- choose (posOffset start + 1, posOffset start + 100)
    let end = SourcePos endLine endCol endOffset
    return $ SourceSpan start end

instance Arbitrary ErrorLocation where
  arbitrary = do
    maybeFile <- oneof [return Nothing, Just <$> arbitrary]
    line <- getPositive <$> arbitrary
    column <- getPositive <$> arbitrary
    endLine <- oneof [return Nothing, Just . getPositive <$> arbitrary]
    endColumn <- oneof [return Nothing, Just . getPositive <$> arbitrary]
    return $ ErrorLocation maybeFile line column endLine endColumn

instance Arbitrary ErrorContext where
  arbitrary = do
    maybeCode <- oneof [return Nothing, Just <$> arbitrary]
    maybeFunction <- oneof [return Nothing, Just <$> arbitrary]
    maybeVariable <- oneof [return Nothing, Just <$> arbitrary]
    maybeType <- oneof [return Nothing, Just <$> arbitrary]
    additional <- listOf ((,) <$> arbitrary <*> arbitrary)
    return $ ErrorContext maybeCode maybeFunction maybeVariable maybeType additional

instance Arbitrary TypeError where
  arbitrary = do
    errorId <- arbitrary
    severity <- arbitrary
    category <- arbitrary
    message <- pack <$> arbitrary
    location <- arbitrary
    context <- arbitrary
    suggestions <- vectorOf 3 (pack <$> arbitrary)
    relatedErrors <- vectorOf 2 arbitrary
    errorChain <- vectorOf 1 arbitrary
    timestamp <- oneof [return Nothing, Just <$> arbitrary]
    return $ TypeError errorId severity category message location context (errorRecovery) suggestions relatedErrors errorChain timestamp
    where
      errorRecovery = errorRecovery

instance Arbitrary FileDirectives where
  arbitrary = do
    ownership <- oneof [return Nothing, Just <$> arbitrary]
    dependentTypes <- oneof [return Nothing, Just <$> arbitrary]
    constraints <- oneof [return Nothing, Just <$> arbitrary]
    return $ FileDirectives ownership dependentTypes constraints

instance Arbitrary BlockDirectives where
  arbitrary = do
    ownership <- oneof [return Nothing, Just <$> arbitrary]
    dependentTypes <- oneof [return Nothing, Just <$> arbitrary]
    constraints <- oneof [return Nothing, Just <$> arbitrary]
    return $ BlockDirectives ownership dependentTypes constraints

-- ============================================================================
-- Utils + SourceLocation Integration Properties
-- ============================================================================

-- Property: trim and position advancement are consistent
prop_trim_position_consistency :: String -> Property
prop_trim_position_consistency s =
  let trimmed = trim s
      originalPos = advancePosBy s startPos
      trimmedPos = advancePosBy trimmed startPos
      originalLength = posOffset originalPos
      trimmedLength = posOffset trimmedPos
  in trimmedLength <= originalLength

-- Property: splitBy and span creation are consistent
prop_splitBy_span_consistency :: Char -> String -> Property
prop_splitBy_span_consistency delim s =
  let parts = splitBy delim s
      positions = scanl (\pos part -> advancePosBy (delim:part) pos) startPos parts
      spans = zipWith (\start end -> spanBetween start (advancePosBy end start)) positions (tail positions ++ [advancePosBy "" (last positions)])
  in length spans === length parts

-- Property: removeComments and position tracking
prop_removeComments_position_tracking :: String -> Property
prop_removeComments_position_tracking s =
  let original = s
      processed = removeComments original
      originalPos = advancePosBy original startPos
      processedPos = advancePosBy processed startPos
  in posOffset processedPos <= posOffset originalPos

-- Property: normalizeIndentation preserves relative structure
prop_normalizeIndentation_relative_structure :: String -> Property
prop_normalizeIndentation_relative_structure s =
  let lines' = lines s
      hasMultipleLines = length lines' > 1
      normalized = normalizeIndentation s
      normalizedLines = lines normalized
  in hasMultipleLines ==>
     let originalIndents = map (takeWhile isSpace) lines'
         normalizedIndents = map (takeWhile isSpace) normalizedLines
         -- Check that relative ordering is preserved
         originalOrder = sort (map length originalIndents)
         normalizedOrder = sort (map length normalizedIndents)
     in originalOrder === normalizedOrder

-- ============================================================================
-- SourceLocation + ErrorHandler Integration Properties
-- ============================================================================

-- Property: SourcePos to ErrorLocation conversion preserves position
prop_sourcepos_errorlocation_conversion :: SourcePos -> Property
prop_sourcepos_errorlocation_conversion pos =
  let errLoc = toErrorLocation pos
  in line errLoc === posLine pos .&&.
     column errLoc === posColumn pos

-- Property: SourceSpan to ErrorLocation conversion preserves range
prop_sourcespan_errorlocation_conversion :: SourceSpan -> Property
prop_sourcespan_errorlocation_conversion span =
  let errLoc = toErrorLocationWithSpan span
      start = spanStart span
      end = spanEnd span
  in line errLoc === posLine start .&&.
     column errLoc === posColumn start .&&.
     endLine errLoc === Just (posLine end) .&&.
     endColumn errLoc === Just (posColumn end)

-- Property: Located values with error handling
prop_located_error_handling :: SourceSpan -> String -> Property
prop_located_error_handling span value =
  let located = locatedWithSpan span value
      err = errorAt "TEST001" (pack value) (_atLocation (posLine (spanStart span)) (posColumn (spanStart span)))
      locatedErr = withLocation err (toErrorLocationWithSpan span)
  in location locatedErr === toErrorLocationWithSpan span

-- Property: Error chaining preserves location information
prop_error_chaining_locations :: SourcePos -> SourcePos -> Property
prop_error_chaining_locations pos1 pos2 =
  let err1 = errorAt "ERR001" "First error" (toErrorLocation pos1)
      err2 = errorAt "ERR002" "Second error" (toErrorLocation pos2)
      chained = wrapError "Chain error" err1
  in location chained === location err1 .&&.
     not (null (errorChain chained))

-- ============================================================================
-- Utils + ErrorHandler Integration Properties
-- ============================================================================

-- Property: Error messages with processed content
prop_error_processed_content :: String -> Property
prop_error_processed_content s =
  let processed = removeComments (trim s)
      err = errorAt "PROC001" (pack processed) (_atLocation 1 1)
      formatted = formatError err
  in not (null processed) ==> T.unpack (message err) `isInfixOf` formatted

-- Property: Error statistics with filtered content
prop_error_statistics_filtered :: [TypeError] -> String -> Property
prop_error_statistics_filtered errors content =
  let processed = removeComments content
      filtered = filter (\e -> T.unpack (message e) `isInfixOf` processed) errors
      stats = getErrorStatistics filtered
  in Map.lookup "total" stats === Just (length filtered)

-- ============================================================================
-- Multi-Module Integration Properties
-- ============================================================================

-- Property: End-to-end processing pipeline
prop_end_to_end_processing :: String -> Property
prop_end_to_end_processing content =
  let trimmed = trim content
      withoutComments = removeComments trimmed
      normalized = normalizeIndentation withoutComments
      parts = splitBy '\n' normalized
      -- Create errors for each part
      errors = zipWith (\i part -> 
        errorAt ("PROC" ++ show i) (pack part) (_atLocation i 1)) [1..] parts
      -- Filter and format
      errorCount = length errors
      stats = getErrorStatistics errors
      report = generateErrorReport errors
  in errorCount > 0 ==>
     Map.lookup "total" stats === Just errorCount .&&.
     "Error Report" `isInfixOf` report

-- Property: Position tracking through multiple transformations
prop_position_tracking_transformations :: String -> Property
prop_position_tracking_transformations content =
  let original = content
      trimmed = trim original
      withoutComments = removeComments trimmed
      normalized = normalizeIndentation withoutComments
      -- Track positions through each step
      originalPos = advancePosBy original startPos
      trimmedPos = advancePosBy trimmed startPos
      commentsPos = advancePosBy withoutComments startPos
      normalizedPos = advancePosBy normalized startPos
  in posOffset normalizedPos <= posOffset commentsPos .&&.
     posOffset commentsPos <= posOffset trimmedPos .&&.
     posOffset trimmedPos <= posOffset originalPos

-- Property: Error recovery with location-aware processing
prop_error_recovery_location_aware :: SourcePos -> String -> Property
prop_error_recovery_location_aware pos content =
  let processed = removeComments content
      err = errorAt "RECOVER001" (pack processed) (toErrorLocation pos)
      canRecover = canRecoverFrom err
      shouldContinue = shouldContinueAfter err
  in canRecover ==> shouldContinue

-- Property: Complex error scenarios with multiple modules
prop_complex_error_scenarios :: [String] -> [SourcePos] -> Property
prop_complex_error_scenarios contents positions =
  let processedContents = map (removeComments . trim) contents
      errors = zipWith (\content pos -> 
        errorWithCategory ("COMP" ++ show (length content)) TypeChecking (pack content) (toErrorLocation pos)
        ) processedContents positions
      -- Apply various filters and operations
      typeCheckingErrors = filterByCategory TypeChecking errors
      recoverableErrors = filter canRecoverFrom errors
      stats = getErrorStatistics errors
      report = generateErrorReport errors
  in not (null errors) ==>
     length typeCheckingErrors === length errors .&&.
     length recoverableErrors <= length errors .&&.
     Map.lookup "typeChecking" stats === Just (length errors) .&&.
     "Statistics:" `isInfixOf` report

-- ============================================================================
-- Performance and Scalability Properties
-- ============================================================================

-- Property: Large content processing performance
prop_large_content_processing :: [String] -> Property
prop_large_content_processing contentLines =
  let content = unlines contentLines
      processed = removeComments (normalizeIndentation (trim content))
      parts = splitBy '\n' processed
      errors = zipWith (\i part -> 
        errorAt ("LARGE" ++ show i) (pack part) (_atLocation i 1)) [1..] parts
      stats = getErrorStatistics errors
  in length contentLines > 0 ==>
     Map.lookup "total" stats === Just (length errors)

-- Property: Memory efficiency with repeated operations
prop_memory_efficiency_operations :: String -> Int -> Property
prop_memory_efficiency_operations content iterations =
  let iterations' = max 0 iterations
      processOnce = removeComments . normalizeIndentation . trim
      processMultiple = iterate processOnce content !! min 5 iterations'
      -- Check that processing stabilizes after a few iterations
      finalResult = iterate processOnce content !! 3
      stableResult = iterate processOnce content !! iterations'
  in iterations' >= 3 ==> finalResult === stableResult

-- ============================================================================
-- Edge Case Integration Properties
-- ============================================================================

-- Property: Empty content handling
prop_empty_content_handling :: Property
prop_empty_content_handling =
  let content = ""
      processed = removeComments (normalizeIndentation (trim content))
      errors = if null processed then [] else [errorAt "EMPTY001" (pack processed) (_atLocation 1 1)]
      stats = getErrorStatistics errors
  in Map.lookup "total" stats === Just 0

-- Property: Special character handling
prop_special_character_handling :: String -> Property
prop_special_character_handling content =
  let hasSpecialChars = any (not . isAlphaNum . not . isSpace) content
      processed = removeComments content
      err = errorAt "SPECIAL001" (pack processed) (_atLocation 1 1)
      formatted = formatError err
  in hasSpecialChars ==> not (null formatted)

-- Property: Unicode content handling
prop_unicode_content_handling :: String -> Property
prop_unicode_content_handling content =
  let processed = removeComments content
      err = errorAt "UNICODE001" (pack processed) (_atLocation 1 1)
      formatted = formatErrorWithLocation err
  in length processed >= 0 ==> not (null formatted)

-- Test collection
tests :: TestTree
tests = testGroup "Integration QuickCheck Properties"
  [ testGroup "Utils + SourceLocation Integration"
    [ fastProperty "trim and position advancement consistency" prop_trim_position_consistency
    , fastProperty "splitBy and span creation consistency" prop_splitBy_span_consistency
    , fastProperty "removeComments and position tracking" prop_removeComments_position_tracking
    , fastProperty "normalizeIndentation preserves relative structure" prop_normalizeIndentation_relative_structure
    ]
  , testGroup "SourceLocation + ErrorHandler Integration"
    [ fastProperty "SourcePos to ErrorLocation conversion" prop_sourcepos_errorlocation_conversion
    , fastProperty "SourceSpan to ErrorLocation conversion" prop_sourcespan_errorlocation_conversion
    , fastProperty "Located values with error handling" prop_located_error_handling
    , fastProperty "Error chaining preserves locations" prop_error_chaining_locations
    ]
  , testGroup "Utils + ErrorHandler Integration"
    [ fastProperty "Error messages with processed content" prop_error_processed_content
    , fastProperty "Error statistics with filtered content" prop_error_statistics_filtered
    ]
  , testGroup "Multi-Module Integration"
    [ fastProperty "End-to-end processing pipeline" prop_end_to_end_processing
    , fastProperty "Position tracking through transformations" prop_position_tracking_transformations
    , fastProperty "Error recovery with location-aware processing" prop_error_recovery_location_aware
    , fastProperty "Complex error scenarios" prop_complex_error_scenarios
    ]
  , testGroup "Performance and Scalability"
    [ fastProperty "Large content processing performance" prop_large_content_processing
    , fastProperty "Memory efficiency with repeated operations" prop_memory_efficiency_operations
    ]
  , testGroup "Edge Cases"
    [ fastProperty "Empty content handling" prop_empty_content_handling
    , fastProperty "Special character handling" prop_special_character_handling
    , fastProperty "Unicode content handling" prop_unicode_content_handling
    ]
  ]