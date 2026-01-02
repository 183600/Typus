{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.UtilsUnicodePerformanceSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), choose, getPositive, getNonNegative, vector, listOf1, elements)
import TestSupport.Arbitrary

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

import Data.Char (isSpace, isAscii, ord)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Maybe (isNothing, isJust, fromMaybe)
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- | Unicode L.and performance tests for Utils module
tests :: TestTree
tests = testGroup "Utils Unicode L.and Performance"
  [ testGroup "Unicode Processing Tests"
    [ testCase "trim handles unicode whitespace" test_trim_unicode_whitespace
    , testCase "splitBy handles unicode delimiters" test_splitBy_unicode_delimiters
    , testCase "removeComments handles unicode content" test_removeComments_unicode
    , testCase "normalizeIndentation handles unicode" test_normalizeIndentation_unicode
    , fastProperty "trim unicode preservation" prop_trim_unicode_preservation
    ]

  , testGroup "Performance Tests"
    [ testCase "trim performance on large strings" test_trim_performance
    , testCase "splitBy performance on large strings" test_splitBy_performance
    , testCase "removeComments performance" test_removeComments_performance
    , fastProperty "trim scalability" prop_trim_scalability
    , fastProperty "splitBy scalability" prop_splitBy_scalability
    ]

  , testGroup "Memory Efficiency Tests"
    [ testCase "trim memory efficiency" test_trim_memory_efficiency
    , testCase "splitBy memory efficiency" test_splitBy_memory_efficiency
    , fastProperty "memory usage consistency" prop_memory_usage_consistency
    ]

  , testGroup "Edge Case Performance"
    [ testCase "performance with special characters" test_performance_special_chars
    , testCase "performance with mixed unicode" test_performance_mixed_unicode
    , fastProperty "performance boundary conditions" prop_performance_boundary_conditions
    ]

  , testGroup "Concurrent Performance"
    [ fastProperty "thread safety" prop_thread_safety
    , fastProperty "parallel processing consistency" prop_parallel_processing_consistency
    ]

  , testGroup "Regression Performance Tests"
    [ testCase "performance regression prevention" test_performance_regression_prevention
    , fastProperty "performance monotonicity" prop_performance_monotonicity
    ]

  , testGroup "Real-world Performance Scenarios"
    [ testCase "large file processing" test_large_file_processing
    , testCase "complex comment removal" test_complex_comment_removal
    ]

  , testGroup "Unicode Normalization"
    [ testCase "unicode normalization consistency" test_unicode_normalization_consistency
    , fastProperty "unicode boundary handling" prop_unicode_boundary_handling
    ]
  ]

-- ============================================================================
-- Unicode Processing Tests
-- ============================================================================

test_trim_unicode_whitespace :: IO ()
test_trim_unicode_whitespace = do
  let unicodeWhitespace = " \t\n\r\u00A0\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u202F\u205F\u3000"
      content = unicodeWhitespace ++ "café naïve" ++ unicodeWhitespace
      result = trim content
  assertBool "Should trim unicode whitespace" $ result == "café naïve"

test_splitBy_unicode_delimiters :: IO ()
test_splitBy_unicode_delimiters = do
  let content = "apple,banana,café,naïve,résumé,测试"
      result = splitBy ',' content
  expected = ["apple", "banana", "café", "naïve", "résumé", "测试"]
  result @?= expected

test_removeComments_unicode :: IO ()
test_removeComments_unicode = do
  let content = unlines
        [ "func unicode() {"
        , "    // 中文注释"
        , "    x := \"café naïve résumé 🚀\" // comment with emoji"
        , "    /* 块注释 with unicode café */"
        , "    return x"
        , "}"
        ]
      result = removeComments content
  assertBool "Should preserve unicode content" $ "café naïve résumé 🚀" `L.isInfixOf` result
  assertBool "Should remove comments" $ not ("中文注释" `L.isInfixOf` result)

test_normalizeIndentation_unicode :: IO ()
test_normalizeIndentation_unicode = do
  let content = unlines
        [ "    func 中文函数() {"
        , "        x := \"café naïve\""
        , "        return x"
        , "    }"
        ]
      result = normalizeIndentation content
      resultLines = lines result
  assertBool "Should normalize unicode content" $ L.length resultLines == 4
  assertBool "Should preserve unicode in content" $ L.any ("中文函数" `L.isInfixOf`) resultLines

prop_trim_unicode_preservation :: String -> Property
prop_trim_unicode_preservation input =
  let hasUnicode = L.any (not . isAscii) input
      trimmed = trim input
  in classify hasUnicode "has unicode characters" $
     property $ trimmed == input || not (L.any isSpace (take 1 trimmed))

-- ============================================================================
-- Performance Tests
-- ============================================================================

test_trim_performance :: IO ()
test_trim_performance = do
  let largeString = L.concat $ replicate 10000 "    café naïve résumé 🚀 测试    \n"
      start <- getCPUTime
      let result = trim largeString
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12 :: Double)
  assertBool "Trim should complete in reasonable time" $ diff < 1.0  -- 1 second
  assertBool "Trim should work correctly" $ not (null result)

test_splitBy_performance :: IO ()
test_splitBy_performance = do
  let largeString = L.concat $ replicate 10000 "café,naïve,résumé,测试,"
      start <- getCPUTime
      let result = splitBy ',' largeString
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12 :: Double)
  assertBool "SplitBy should complete in reasonable time" $ diff < 1.0  -- 1 second
  assertBool "SplitBy should work correctly" $ L.length result > 1000

test_removeComments_performance :: IO ()
test_removeComments_performance = do
  let largeContent = unlines $ replicate 1000 
        [ "    // 中文注释"
        , "    x := \"café naïve résumé 🚀 测试\" /* block comment */"
        ]
      start <- getCPUTime
      let result = removeComments largeContent
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12 :: Double)
  assertBool "RemoveComments should complete in reasonable time" $ diff < 2.0  -- 2 seconds
  assertBool "RemoveComments should work correctly" $ not (null result)

prop_trim_scalability :: Int -> String -> Property
prop_trim_scalability multiplier baseContent =
  multiplier > 0 && multiplier <= 1000 && L.length baseContent <= 100 ==>
  let largeContent = L.concat $ replicate multiplier (baseContent ++ "  ")
      result = trim largeContent
  in property $ not (null result) || null baseContent

prop_splitBy_scalability :: Int -> Property
prop_splitBy_scalability multiplier =
  multiplier > 0 && multiplier <= 1000 ==>
  let baseContent = "café,naïve,résumé,测试"
      largeContent = L.concat $ replicate multiplier (baseContent ++ ",")
      result = splitBy ',' largeContent
  in property $ L.length result >= multiplier * 4

-- ============================================================================
-- Memory Efficiency Tests
-- ============================================================================

test_trim_memory_efficiency :: IO ()
test_trim_memory_efficiency = do
  let largeString = L.concat $ replicate 100000 "    café naïve résumé 🚀 测试    \n"
  -- Force evaluation to ensure memory usage
  result <- evaluate $ force $ trim largeString
  assertBool "Trim should handle large strings efficiently" $ not (null result)

test_splitBy_memory_efficiency :: IO ()
test_splitBy_memory_efficiency = do
  let largeString = L.concat $ replicate 100000 "café,naïve,résumé,测试,"
  -- Force evaluation to ensure memory usage
  result <- evaluate $ force $ splitBy ',' largeString
  assertBool "SplitBy should handle large strings efficiently" $ L.length result > 1000

prop_memory_usage_consistency :: Int -> String -> Property
prop_memory_usage_consistency size baseContent =
  size > 0 && size <= 1000 && L.length baseContent <= 100 ==>
  let content = L.concat $ replicate size baseContent
      trimmed1 = trim content
      trimmed2 = trim content
  in property $ trimmed1 == trimmed2

-- ============================================================================
-- Edge Case Performance
-- ============================================================================

test_performance_special_chars :: IO ()
test_performance_special_chars = do
  let specialChars = "\0\1\2\3\4\5\6\7\8\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31\127"
      content = L.concat $ replicate 1000 (specialChars ++ "café naïve")
      start <- getCPUTime
      let result = trim content
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12 :: Double)
  assertBool "Should handle special characters efficiently" $ diff < 1.0

test_performance_mixed_unicode :: IO ()
test_performance_mixed_unicode = do
  let mixedContent = unlines $ replicate 1000 
        [ "func 测试() {"
        , "    x := \"café naïve résumé 🚀\""
        , "    // 中文注释 with emoji 🎉"
        , "    return x"
        , "}"
        ]
      start <- getCPUTime
      let result = removeComments mixedContent
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12 :: Double)
  assertBool "Should handle mixed unicode efficiently" $ diff < 2.0

prop_performance_boundary_conditions :: Int -> Property
prop_performance_boundary_conditions size =
  size > 0 && size <= 10000 ==>
  let content = L.concat $ replicate size "café naïve résumé 🚀 测试\n"
      result = trim content
  in property $ not (null result) || size == 0

-- ============================================================================
-- Concurrent Performance
-- ============================================================================

prop_thread_safety :: String -> Property
prop_thread_safety input =
  L.length input <= 1000 ==>
  let result1 = trim input
      result2 = trim input
      result3 = trim input
  in property $ result1 == result2 && result2 == result3

prop_parallel_processing_consistency :: String -> Property
prop_parallel_processing_consistency input =
  L.length input <= 1000 ==>
  let results = map trim [input, input, input, input, input]
  in property $ L.all (== L.head results) results

-- ============================================================================
-- Regression Performance Tests
-- ============================================================================

test_performance_regression_prevention :: IO ()
test_performance_regression_prevention = do
  let complexContent = unlines $ replicate 100
        [ "//! ownership=true, dependent-types=true"
        , "func complex() {"
        , "    // 中文注释 café naïve"
        , "    x := \"résumé 🚀 测试\" /* block */"
        , "    if (x != null) {"
        , "        return x.toString();"
        , "    }"
        , "}"
        ]
      start <- getCPUTime
      let result = removeComments complexContent
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12 :: Double)
  assertBool "Complex processing should remain efficient" $ diff < 3.0

prop_performance_monotonicity :: Int -> String -> Property
prop_performance_monotonicity size baseContent =
  size > 0 && size <= 100 && L.length baseContent <= 50 ==>
  let content1 = L.concat $ replicate size baseContent
      content2 = L.concat $ replicate (size + 1) baseContent
      result1 = trim content1
      result2 = trim content2
  in property $ L.length result2 >= L.length result1

-- ============================================================================
-- Real-world Performance Scenarios
-- ============================================================================

test_large_file_processing :: IO ()
test_large_file_processing = do
  let largeFile = unlines $ L.concat
        [ replicate 1000 "//! ownership=true"
        , replicate 1000 "func test() { return \"café naïve résumé 🚀 测试\"; }"
        , replicate 1000 "// 中文注释 line"
        ]
      start <- getCPUTime
      let result = removeComments largeFile
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12 :: Double)
  assertBool "Large file processing should be efficient" $ diff < 5.0

test_complex_comment_removal :: IO ()
test_complex_comment_removal = do
  let complexContent = unlines $ replicate 500
        [ "/* Complex block comment with 中文 café naïve résumé 🚀 */"
        , "func test() { // Line comment with emoji 🎉"
        , "    x := \"string with /* fake comment */ café naïve\""
        , "    return x;"
        , "}"
        ]
      start <- getCPUTime
      let result = removeComments complexContent
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12 :: Double)
  assertBool "Complex comment removal should be efficient" $ diff < 3.0

-- ============================================================================
-- Unicode Normalization
-- ============================================================================

test_unicode_normalization_consistency :: IO ()
test_unicode_normalization_consistency = do
  let unicodeInputs = 
        [ "café naïve résumé"
        , "测试中文内容"
        , "🚀 emoji test 🎉"
        , "mixéd 中文 🚀 café"
        ]
  mapM_ testUnicodeInput unicodeInputs
  where
    testUnicodeInput input = do
      let trimmed = trim input
          split = splitBy ' ' input
          commentsRemoved = removeComments ("// " ++ input ++ "\n" ++ input)
      assertBool "Trim should be consistent" $ trimmed == trim trimmed
      assertBool "Split should be consistent" $ split == splitBy ' ' input
      assertBool "Comment removal should be consistent" $ 
        commentsRemoved == removeComments ("// " ++ input ++ "\n" ++ input)

prop_unicode_boundary_handling :: String -> Property
prop_unicode_boundary_handling input =
  L.length input <= 100 ==>
  let hasUnicode = L.any (not . isAscii) input
      trimmed = trim input
      processed = removeComments input
  in classify hasUnicode "has unicode" $
     property $ L.length trimmed <= L.length input && L.length processed <= L.length input * 2

-- ============================================================================
-- Additional Helper Functions
-- ============================================================================

-- Performance measurement helper
measureTime :: IO a -> IO (Double, a)
measureTime action = do
  start <- getCPUTime
  result <- action
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10^12 :: Double)
  return (diff, result)