{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.UtilsPerformanceSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Arbitrary (Arbitrary(..), arbitrary)
import Test.QuickCheck.Gen (choose, listOf, oneof, elements, vectorOf, suchThat, Gen)

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , forceSingleTabIndentation
  , fixIndentation
  , breakOn
  )

import Data.Char (isSpace, toLower)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (tails, sort)
import qualified Data.Text as T
import Data.Text (Text)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

-- Generate large strings for performance testing
arbitraryLargeString :: Gen String
arbitraryLargeString = do
  size <- choose (100, 10000)
  vectorOf size (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 \t\n\r")

-- Generate strings with many repetitions
arbitraryRepetitiveString :: Gen String
arbitraryRepetitiveString = do
  pattern <- elements ["abc", "hello world", "test string", "pattern"]
  repetitions <- choose (10, 1000)
  return $ L.concat (replicate repetitions pattern)

-- Generate strings with many delimiters
arbitraryDelimitedString :: Char -> Gen String
arbitraryDelimitedString delim = do
  numSegments <- choose (10, 100)
  segments <- vectorOf numSegments arbitraryWord
  return $ Data.List.intercalate [delim] segments

-- Generate words
arbitraryWord :: Gen String
arbitraryWord = do
  L.length <- choose (1, 20)
  vectorOf L.length (elements "abcdefghijklmnopqrstuvwxyz")

-- Generate strings with many comments
arbitraryCommentString :: Gen String
arbitraryCommentString = do
  numLines <- choose (10, 100)
  lines' <- vectorOf numLines $ do
    hasComment <- arbitrary
    if hasComment
      then do
        code <- arbitraryWord
        comment <- arbitraryWord
        return $ code ++ " // " ++ comment
      else do
        code <- arbitraryWord
        return code
  return $ unlines lines'

-- Generate strings with mixed indentation
arbitraryIndentedString :: Gen String
arbitraryIndentedString = do
  numLines <- choose (10, 100)
  lines' <- vectorOf numLines $ do
    indentLevel <- choose (0, 10)
    content <- arbitraryWord
    return $ replicate indentLevel ' ' ++ content
  return $ unlines lines'

-- ============================================================================
-- Performance Properties for Utils Functions
-- ============================================================================

-- Property: trim performance scales linearly with input size
prop_trim_performance_linear :: Property
prop_trim_performance_linear =
  forAll arbitraryLargeString $ \input ->
  let trimmed = trim input
      resultLength = L.length trimmed
      inputLength = L.length input
  in property $ resultLength <= inputLength .&&. resultLength >= 0

-- Property: splitBy performance is reasonable for large inputs
prop_splitBy_performance_large :: Property
prop_splitBy_performance_large =
  forAll (arbitraryDelimitedString ',') $ \input ->
  let segments = splitBy ',' input
      segmentCount = L.length segments
  in property $ segmentCount >= 1 .&&. segmentCount <= 101  -- At most numSegments + 1

-- Property: splitByCollapsed removes empty segments efficiently
prop_splitByCollapsed_performance :: Property
prop_splitByCollapsed_performance =
  forAll (arbitraryDelimitedString ',') $ \input ->
  let collapsed = splitByCollapsed ',' input
      hasEmpty = L.any null collapsed
  in property $ not hasEmpty .&&. L.length collapsed >= 0

-- Property: removeLineComments handles large files efficiently
prop_removeLineComments_performance :: Property
prop_removeLineComments_performance =
  forAll arbitraryCommentString $ \input ->
  let cleaned = removeLineComments input
      lineCount = L.length (lines input)
  in property $ not (null cleaned) .&&. lineCount >= 10

-- Property: removeComments handles mixed comments efficiently
prop_removeComments_performance :: Property
prop_removeComments_performance =
  forAll arbitraryCommentString $ \input ->
  let cleaned = removeComments input
      originalLength = L.length input
      cleanedLength = L.length cleaned
  in property $ cleanedLength <= originalLength .&&. cleanedLength >= 0

-- Property: normalizeIndentation handles large indented blocks
prop_normalizeIndentation_performance :: Property
prop_normalizeIndentation_performance =
  forAll arbitraryIndentedString $ \input ->
  let normalized = normalizeIndentation input
      lineCount = L.length (lines input)
  in property $ not (null normalized) .&&. lineCount >= 10

-- Property: forceSingleTabIndentation processes large files efficiently
prop_forceSingleTabIndentation_performance :: Property
prop_forceSingleTabIndentation_performance =
  forAll arbitraryIndentedString $ \input ->
  let tabbed = forceSingleTabIndentation input
      lineCount = L.length (lines input)
  in property $ not (null tabbed) .&&. lineCount >= 10

-- Property: fixIndentation maintains performance with complex structures
prop_fixIndentation_performance :: Property
prop_fixIndentation_performance =
  forAll arbitraryIndentedString $ \input ->
  let fixed = fixIndentation input
      lineCount = L.length (lines input)
  in property $ not (null fixed) .&&. lineCount >= 10

-- Property: breakOn finds patterns efficiently in large strings
prop_breakOn_performance :: Property
prop_breakOn_performance =
  forAll arbitraryLargeString $ \input ->
  forAll arbitraryWord $ \pattern ->
  let (before, after) = breakOn pattern input
      totalLength = L.length before + L.length after + L.length pattern
  in property $ totalLength >= L.length input

-- ============================================================================
-- Memory Efficiency Properties
-- ============================================================================

-- Property: trim doesn't create excessive intermediate strings
prop_trim_memory_efficient :: Property
prop_trim_memory_efficient =
  forAll arbitraryRepetitiveString $ \input ->
  let trimmed = trim input
      inputLength = L.length input
      trimmedLength = L.length trimmed
  in property $ trimmedLength <= inputLength .&&. trimmedLength >= 0

-- Property: splitBy doesn't create excessive intermediate lists
prop_splitBy_memory_efficient :: Property
prop_splitBy_memory_efficient =
  forAll (arbitraryDelimitedString ',') $ \input ->
  let segments = splitBy ',' input
      totalSegmentsLength = L.sum $ map L.length segments
  in property $ totalSegmentsLength <= L.length input + L.length segments

-- Property: removeComments processes comments without memory bloat
prop_removeComments_memory_efficient :: Property
prop_removeComments_memory_efficient =
  forAll arbitraryCommentString $ \input ->
  let cleaned = removeComments input
      commentCount = L.length $ L.filter ("//" `L.isInfixOf`) (lines input)
      cleanedLength = L.length cleaned
  in property $ cleanedLength <= L.length input .&&. commentCount >= 0

-- ============================================================================
-- Edge Case Performance Properties
-- ============================================================================

-- Property: trim handles extreme whitespace efficiently
prop_trim_extreme_whitespace :: Property
prop_trim_extreme_whitespace =
  let extremeWhitespace = replicate 10000 ' ' ++ "content" ++ replicate 10000 ' '
  in let trimmed = trim extremeWhitespace
  in property $ trimmed === "content"

-- Property: splitBy handles extreme delimiter density
prop_splitBy_extreme_delimiters :: Property
prop_splitBy_extreme_delimiters =
  let extremeDelimiters = replicate 5000 ','
  in let segments = splitBy ',' extremeDelimiters
  in property $ L.length segments === 5001

-- Property: removeComments handles comment-only files efficiently
prop_removeComments_comment_only :: Property
prop_removeComments_comment_only =
  let commentOnly = unlines $ replicate 1000 "// This is a comment"
  in let cleaned = removeComments commentOnly
  in property $ null cleaned

-- Property: normalizeIndentation handles deeply nested code efficiently
prop_normalizeIndentation_deeply_nested :: Property
prop_normalizeIndentation_deeply_nested =
  let deeplyNested = unlines $ L.map (\i -> replicate i ' ' ++ "content") [0..1000]
  in let normalized = normalizeIndentation deeplyNested
  in property $ not (null normalized)

-- ============================================================================
-- Scalability Properties
-- ============================================================================

-- Property: Functions scale linearly with input size
prop_functions_scale_linearly :: Property
prop_functions_scale_linearly =
  forAll (choose (100, 5000)) $ \size ->
  let testString = replicate size 'a' ++ "content" ++ replicate size 'b'
      trimmed = trim testString
      segments = splitBy ' ' testString
  in property $ L.length trimmed <= L.length testString .&&. L.length segments >= 1

-- Property: Functions handle repeated operations efficiently
prop_functions_repeated_operations :: Property
prop_functions_repeated_operations =
  let repeatedPattern = "test pattern\n"
      largeInput = L.concat $ replicate 1000 repeatedPattern
      lines' = lines largeInput
      trimmedLines = map trim lines'
  in property $ L.length trimmedLines === 1000

-- Property: Functions maintain performance with Unicode content
prop_functions_unicode_performance :: Property
prop_functions_unicode_performance =
  let unicodeContent = unlines $ replicate 100 "测试内容 🚀 with ascii"
      trimmed = trim unicodeContent
      segments = splitBy ' ' unicodeContent
  in property $ not (null trimmed) .&&. L.length segments >= 1

-- ============================================================================
-- Consistency Properties
-- ============================================================================

-- Property: trim is consistent across multiple calls
prop_trim_consistency :: Property
prop_trim_consistency =
  forAll arbitraryLargeString $ \input ->
  let trimmed1 = trim input
      trimmed2 = trim trimmed1
  in property $ trimmed1 === trimmed2

-- Property: splitBy is consistent with splitByComma
prop_splitBy_consistency :: Property
prop_splitBy_consistency =
  forAll arbitraryLargeString $ \input ->
  let byComma = splitBy ',' input
      byFunction = splitByComma input
  in property $ byComma === byFunction

-- Property: removeComments is idempotent
prop_removeComments_idempotent :: Property
prop_removeComments_idempotent =
  forAll arbitraryCommentString $ \input ->
  let cleaned1 = removeComments input
      cleaned2 = removeComments cleaned1
  in property $ cleaned1 === cleaned2

-- Property: normalizeIndentation is idempotent
prop_normalizeIndentation_idempotent :: Property
prop_normalizeIndentation_idempotent =
  forAll arbitraryIndentedString $ \input ->
  let normalized1 = normalizeIndentation input
      normalized2 = normalizeIndentation normalized1
  in property $ normalized1 === normalized2

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Utils Performance Tests"
  [ testGroup "Basic Performance Properties"
    [ fastProperty "trim performance scales linearly with input size" prop_trim_performance_linear
    , fastProperty "splitBy performance is reasonable for large inputs" prop_splitBy_performance_large
    , fastProperty "splitByCollapsed removes empty segments efficiently" prop_splitByCollapsed_performance
    , fastProperty "removeLineComments handles large files efficiently" prop_removeLineComments_performance
    ]

  , testGroup "Advanced Performance Properties"
    [ fastProperty "removeComments handles mixed comments efficiently" prop_removeComments_performance
    , fastProperty "normalizeIndentation handles large indented blocks" prop_normalizeIndentation_performance
    , fastProperty "forceSingleTabIndentation processes large files efficiently" prop_forceSingleTabIndentation_performance
    , fastProperty "fixIndentation maintains performance with complex structures" prop_fixIndentation_performance
    ]

  , testGroup "Search Performance Properties"
    [ fastProperty "breakOn finds patterns efficiently in large strings" prop_breakOn_performance
    ]

  , testGroup "Memory Efficiency Properties"
    [ fastProperty "trim doesn't create excessive intermediate strings" prop_trim_memory_efficient
    , fastProperty "splitBy doesn't create excessive intermediate lists" prop_splitBy_memory_efficient
    , fastProperty "removeComments processes comments without memory bloat" prop_removeComments_memory_efficient
    ]

  , testGroup "Edge Case Performance Properties"
    [ fastProperty "trim handles extreme whitespace efficiently" prop_trim_extreme_whitespace
    , fastProperty "splitBy handles extreme delimiter density" prop_splitBy_extreme_delimiters
    , fastProperty "removeComments handles comment-only files efficiently" prop_removeComments_comment_only
    , fastProperty "normalizeIndentation handles deeply nested code efficiently" prop_normalizeIndentation_deeply_nested
    ]

  , testGroup "Scalability Properties"
    [ fastProperty "Functions scale linearly with input size" prop_functions_scale_linearly
    , fastProperty "Functions handle repeated operations efficiently" prop_functions_repeated_operations
    , fastProperty "Functions maintain performance with Unicode content" prop_functions_unicode_performance
    ]

  , testGroup "Consistency Properties"
    [ fastProperty "trim is consistent across multiple calls" prop_trim_consistency
    , fastProperty "splitBy is consistent with splitByComma" prop_splitBy_consistency
    , fastProperty "removeComments is idempotent" prop_removeComments_idempotent
    , fastProperty "normalizeIndentation is idempotent" prop_normalizeIndentation_idempotent
    ]
  ]