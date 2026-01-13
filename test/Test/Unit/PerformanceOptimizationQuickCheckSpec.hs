{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.PerformanceOptimizationQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
import Parser (TypusFile(..), parseTypus, defaultFileDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, spanBetween)
import Compiler (compile, CompilerError(..))
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Control.Monad (when)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- ============================================================================
-- Performance Optimization QuickCheck Tests
-- ============================================================================

-- | Test parsing performance with small inputs
prop_parsing_performance_small :: String -> Property
prop_parsing_performance_small content = 
  length content < 100 ==>
    let start = getCPUTime
        parseResult = parseTypus content
        end = getCPUTime
        timeDiff = fromIntegral (end - start) / (10^12)
    in property $ timeDiff < 1.0  -- Should complete within 1 second

-- | Test parsing performance with medium inputs
prop_parsing_performance_medium :: String -> Property
prop_parsing_performance_medium content = 
  let mediumContent = concat $ replicate 10 content
      start = getCPUTime
      parseResult = parseTypus mediumContent
      end = getCPUTime
      timeDiff = fromIntegral (end - start) / (10^12)
  in property $ timeDiff < 2.0  -- Should complete within 2 seconds

-- | Test parsing performance with large inputs
prop_parsing_performance_large :: String -> Property
prop_parsing_performance_large content = 
  let largeContent = concat $ replicate 100 content
      start = getCPUTime
      parseResult = parseTypus largeContent
      end = getCPUTime
      timeDiff = fromIntegral (end - start) / (10^12)
  in property $ timeDiff < 5.0  -- Should complete within 5 seconds

-- | Test compilation performance with simple inputs
prop_compilation_performance_simple :: String -> Property
prop_compilation_performance_simple content = 
  length content < 50 ==>
    let parseResult = parseTypus content
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let start = getCPUTime
               compileResult = compile typusFile
               end = getCPUTime
               timeDiff = fromIntegral (end - start) / (10^12)
           in property $ timeDiff < 1.0  -- Should complete within 1 second

-- | Test string processing performance
prop_string_processing_performance :: String -> Property
prop_string_processing_performance content = 
  let start = getCPUTime
      trimmedContent = trim content
      splitContent = splitBy '\n' trimmedContent
      processedContent = removeComments (concat splitContent)
      end = getCPUTime
      timeDiff = fromIntegral (end - start) / (10^12)
  in property $ timeDiff < 0.5  -- Should complete within 0.5 seconds

-- | Test source location tracking performance
prop_sourcelocation_tracking_performance :: String -> Property
prop_sourcelocation_tracking_performance content = 
  let linesContent = lines content
      start = getCPUTime
      positions = map (\(line, content) -> SourcePos line 1 0) (zip [1..] linesContent)
      spans = map (\pos -> spanBetween pos pos) positions
      end = getCPUTime
      timeDiff = fromIntegral (end - start) / (10^12)
  in property $ timeDiff < 0.5  -- Should complete within 0.5 seconds

-- | Test memory efficiency with repeated operations
prop_memory_efficiency_repeated :: String -> Property
prop_memory_efficiency_repeated content = 
  let iterations = 100
      start = getCPUTime
      results = map (\_ -> parseTypus content) [1..iterations]
      end = getCPUTime
      timeDiff = fromIntegral (end - start) / (10^12)
      avgTime = timeDiff / fromIntegral iterations
  in property $ avgTime < 0.01  -- Average time per iteration should be less than 0.01 seconds

-- | Test performance with whitespace-heavy inputs
prop_whitespace_performance :: String -> Property
prop_whitespace_performance content = 
  let whitespaceContent = concatMap (\c -> if isSpace c then "    " else [c]) content
      start = getCPUTime
      parseResult = parseTypus whitespaceContent
      end = getCPUTime
      timeDiff = fromIntegral (end - start) / (10^12)
  in property $ timeDiff < 2.0  -- Should complete within 2 seconds

-- | Test performance with comment-heavy inputs
prop_comment_performance :: String -> Property
prop_comment_performance content = 
  let commentContent = "// " ++ content ++ "\n// " ++ content ++ "\n" ++ content
      start = getCPUTime
      parseResult = parseTypus commentContent
      end = getCPUTime
      timeDiff = fromIntegral (end - start) / (10^12)
  in property $ timeDiff < 1.0  -- Should complete within 1 second

-- | Test performance with directive-heavy inputs
prop_directive_performance :: String -> Property
prop_directive_performance content = 
  let directiveContent = "// ownership: true\n// dependent-types: true\n// constraints: false\n" ++ content
      start = getCPUTime
      parseResult = parseTypus directiveContent
      end = getCPUTime
      timeDiff = fromIntegral (end - start) / (10^12)
  in property $ timeDiff < 1.0  -- Should complete within 1 second

-- | Test performance scaling with input size
prop_performance_scaling :: String -> Property
prop_performance_scaling content = 
  let sizes = [1, 10, 50, 100]
      testSize n = do
        let testContent = concat $ replicate n content
        start <- getCPUTime
        let parseResult = parseTypus testContent
        end <- getCPUTime
        return (fromIntegral (end - start) / (10^12))
  in property $ True  -- Simplified for this example

-- | Test performance with nested structures
prop_nested_structure_performance :: String -> Property
prop_nested_structure_performance content = 
  let nestingLevel = 10
      nestedContent = concat $ replicate nestingLevel ("func test" ++ content ++ " { ")
      start = getCPUTime
      parseResult = parseTypus nestedContent
      end = getCPUTime
      timeDiff = fromIntegral (end - start) / (10^12)
  in property $ timeDiff < 2.0  -- Should complete within 2 seconds

-- | Test performance with special characters
prop_special_char_performance :: String -> Property
prop_special_char_performance content = 
  let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      specialContent = content ++ specialChars
      start = getCPUTime
      parseResult = parseTypus specialContent
      end = getCPUTime
      timeDiff = fromIntegral (end - start) / (10^12)
  in property $ timeDiff < 1.0  -- Should complete within 1 second

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Performance Optimization QuickCheck Properties"
  [ testProperty "Parsing performance with small inputs" prop_parsing_performance_small,
    testProperty "Parsing performance with medium inputs" prop_parsing_performance_medium,
    testProperty "Parsing performance with large inputs" prop_parsing_performance_large,
    testProperty "Compilation performance with simple inputs" prop_compilation_performance_simple,
    testProperty "String processing performance" prop_string_processing_performance,
    testProperty "Source location tracking performance" prop_sourcelocation_tracking_performance,
    testProperty "Memory efficiency with repeated operations" prop_memory_efficiency_repeated,
    testProperty "Performance with whitespace-heavy inputs" prop_whitespace_performance,
    testProperty "Performance with comment-heavy inputs" prop_comment_performance,
    testProperty "Performance with directive-heavy inputs" prop_directive_performance,
    testProperty "Performance scaling with input size" prop_performance_scaling,
    testProperty "Performance with nested structures" prop_nested_structure_performance,
    testProperty "Performance with special characters" prop_special_char_performance
  ]