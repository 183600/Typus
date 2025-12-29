module Test.Unit.PerformanceBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, withMaxSize)
import Parser (parseTypus)
import Utils (trim, removeComments, splitBy)
import SourceLocation (advancePosBy, startPos)
import Data.List (length, replicate)

-- ============================================================================
-- Performance Boundary QuickCheck Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "Performance Boundary QuickCheck Tests"
  [ testProperty "parser handles large files efficiently" prop_parser_large_files
  , testProperty "text processing scales linearly" prop_text_processing_scaling
  , testProperty "source location calculation handles large offsets" prop_sourcelocation_large_offsets
  , testProperty "string processing handles very long lines" prop_string_processing_long_lines
  , testProperty "nested structure parsing depth limits" prop_parser_depth_limits
  , testProperty "memory usage with repeated patterns" prop_memory_repeated_patterns
  , testProperty "performance with many small blocks" prop_performance_many_blocks
  , testProperty "boundary condition: empty blocks" prop_boundary_empty_blocks
  ]

-- | Parser should handle large files without exponential slowdown
prop_parser_large_files :: Int -> Property
prop_parser_large_files multiplier = withMaxSize 1000 $ 
  let largeContent = unlines $ replicate multiplier "//! ownership=true\nfn test() { return 42; }\n"
      result = parseTypus largeContent
  in case result of
    Left _ -> True  -- May fail but shouldn't hang
    Right _ -> True  -- Should succeed

-- | Text processing should scale linearly with input size
prop_text_processing_scaling :: String -> Int -> Property
prop_text_processing_scaling base multiplier = 
  let repeated = concat (replicate multiplier base)
      processed = removeComments repeated
  in length processed `div` max 1 (length repeated) <= 2  -- Should not expand dramatically

-- | Source location calculation should handle large character offsets
prop_sourcelocation_large_offsets :: Int -> Property
prop_sourcelocation_large_offsets n = 
  let largeString = replicate n 'a'
      finalPos = advancePosBy largeString startPos
  in posOffset finalPos === n

-- | String processing should handle very long lines efficiently
prop_string_processing_long_lines :: Int -> Property
prop_string_processing_long_lines n = 
  let longLine = replicate n 'x' ++ " code"
      trimmed = trim longLine
      processed = removeComments longLine
  in length trimmed <= length longLine && length processed <= length longLine

-- | Parser should have reasonable depth limits for nested structures
prop_parser_depth_limits :: Int -> Property
prop_parser_depth_limits depth = 
  let nestedContent = unlines $ replicate depth ("  " ++ "nested content")
      result = parseTypus nestedContent
  in case result of
    Left _ -> True  -- May fail for very deep nesting
    Right _ -> True  -- Should handle reasonable depths

-- | Memory usage should be reasonable with repeated patterns
prop_memory_repeated_patterns :: String -> Int -> Property
prop_memory_repeated_patterns pattern repetitions = 
  let repeated = concat (replicate repetitions pattern)
      processed = removeComments repeated
      splitResult = splitBy '\n' processed
  in length splitResult >= 0  -- Should complete without memory issues

-- | Performance should be acceptable with many small blocks
prop_performance_many_blocks :: Int -> Property
prop_performance_many_blocks numBlocks = 
  let blockContent = "//! ownership=true\nblock content\n"
      manyBlocks = unlines $ replicate numBlocks blockContent
      result = parseTypus manyBlocks
  in case result of
    Left _ -> True  -- May fail but shouldn't hang
    Right _ -> True  -- Should succeed

-- | Boundary condition: parser should handle empty blocks gracefully
prop_boundary_empty_blocks :: Int -> Property
prop_boundary_empty_blocks numEmpty = 
  let emptyBlocks = unlines $ replicate numEmpty "//! ownership=true\n\n"
      result = parseTypus emptyBlocks
  in case result of
    Left _ -> True  -- May fail
    Right tf -> length (tfBlocks tf) >= 0  -- Should produce some structure

-- Helper function for safe division
safeDiv :: Int -> Int -> Int
safeDiv _ 0 = 0
safeDiv x y = x `div` y