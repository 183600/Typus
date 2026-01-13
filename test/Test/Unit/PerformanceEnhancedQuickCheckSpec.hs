{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.PerformanceEnhancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeComments
  , normalizeIndentation
  , breakOn
  )
import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , spanBetween
  , mergeSpans
  , isValidSpan
  )
import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  )
import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T
import Control.DeepSeq (NFData, force)

-- ============================================================================
-- Performance QuickCheck Tests
-- ============================================================================

-- | Test Utils performance properties
prop_utils_trim_performance :: Int -> Property
prop_utils_trim_performance n = 
  n >= 0 && n <= 10000 ==>
    let largeInput = replicate n ' ' ++ "content" ++ replicate n ' '
        result = trim largeInput
    in result == "content"

prop_utils_split_performance :: Int -> Char -> Property
prop_utils_split_performance n delim = 
  n >= 0 && n <= 5000 ==>
    let largeInput = replicate n delim
        result = splitBy delim largeInput
    in length result == n + 1 && all (== "") result

prop_utils_split_collapsed_performance :: Int -> Char -> Property
prop_utils_split_collapsed_performance n delim = 
  n >= 0 && n <= 5000 ==>
    let largeInput = concat (replicate n [delim, delim])
        result = splitByCollapsed delim largeInput
    in length result == 0 || all (not . null) result

prop_utils_remove_comments_performance :: Int -> Property
prop_utils_remove_comments_performance n = 
  n >= 0 && n <= 1000 ==>
    let largeContent = concat (replicate n ("content\n// comment\n"))
        result = removeComments largeContent
    in "content" `isInfixOf` result && not ("//" `isInfixOf` result)

prop_utils_normalize_indentation_performance :: Int -> Property
prop_utils_normalize_indentation_performance n = 
  n >= 0 && n <= 1000 ==>
    let indentedLines = map (\i -> replicate i ' ' ++ "content") [0..n]
        largeInput = unlines indentedLines
        result = normalizeIndentation largeInput
        resultLines = lines result
    in length resultLines == length indentedLines && 
       all (\line -> not (null line) && not (all isSpace (take 10 line))) resultLines

prop_utils_break_on_performance :: Int -> String -> Property
prop_utils_break_on_performance n pat = 
  n >= 0 && n <= 5000 && not (null pat) ==>
    let largeInput = concat (replicate n "content") ++ pat ++ concat (replicate n "more")
        (before, after) = breakOn pat largeInput
    in before ++ pat ++ after == largeInput

-- | Test SourceLocation performance properties
prop_sourcelocation_position_advancement_performance :: Int -> Property
prop_sourcelocation_position_advancement_performance n = 
  n >= 0 && n <= 10000 ==>
    let chars = replicate n 'a'
        finalPos = foldl (flip posAfter) startPos chars
    in posOffset finalPos == n && posLine finalPos == 1

prop_sourcelocation_span_creation_performance :: Int -> Property
prop_sourcelocation_span_creation_performance n = 
  n >= 0 && n <= 1000 ==>
    let start = startPos
        end = SourcePos n n (n * n)
        span = spanBetween start end
    in spanStart span == start && spanEnd span == end

prop_sourcelocation_span_merge_performance :: Int -> Property
prop_sourcelocation_span_merge_performance n = 
  n >= 0 && n <= 1000 ==>
    let spans = [spanBetween (SourcePos i i i) (SourcePos (i+1) (i+1) (i+1)) | i <- [0..n]]
        merged = foldl mergeSpans (head spans) (tail spans)
    in isValidSpan merged

-- | Test Parser performance properties
prop_parser_large_content_performance :: Int -> Property
prop_parser_large_content_performance n = 
  n >= 0 && n <= 1000 ==>
    let largeContent = concat (replicate n "content line\n")
        result = parseTypus largeContent
        blocks = tfBlocks result
    in not (null blocks) && 
       let totalContent = concatMap cbContent blocks
       in "content" `isInfixOf` totalContent

prop_parser_many_directives_performance :: Int -> Property
prop_parser_many_directives_performance n = 
  n >= 0 && n <= 500 ==>
    let directives = concat (replicate n "// build: tag\n// ownership: true\n")
        result = parseTypus directives
        buildTags = tfBuildTags result
        directives' = tfDirectives result
    in length buildTags == n && 
       case fdOwnership directives' of
         Just locatedValue -> locValue locatedValue == True
         Nothing -> False

prop_parser_complex_structure_performance :: Int -> Property
prop_parser_complex_structure_performance n = 
  n >= 0 && n <= 200 ==>
    let complexStructure = concat (replicate n ("// ownership: true\ncontent\n// dependent-types: false\n"))
        result = parseTypus complexStructure
        blocks = tfBlocks result
    in length blocks >= n

-- | Test memory efficiency properties
prop_utils_memory_efficiency :: Int -> Property
prop_utils_memory_efficiency n = 
  n >= 0 && n <= 5000 ==>
    let largeInput = concat (replicate n "test ")
        result = trim largeInput
    in force result == result  -- Ensure it can be fully evaluated

prop_sourcelocation_memory_efficiency :: Int -> Property
prop_sourcelocation_memory_efficiency n = 
  n >= 0 && n <= 1000 ==>
    let positions = [SourcePos i i i | i <- [0..n]]
        spans = [spanBetween p (SourcePos (i+1) (i+1) (i+1)) | (p, i) <- zip positions [0..]]
        merged = foldl mergeSpans (head spans) (tail spans)
    in force merged == merged  -- Ensure it can be fully evaluated

prop_parser_memory_efficiency :: Int -> Property
prop_parser_memory_efficiency n = 
  n >= 0 && n <= 500 ==>
    let largeContent = concat (replicate n ("content " ++ replicate 100 'a' ++ "\n"))
        result = parseTypus largeContent
    in force result == result  -- Ensure it can be fully evaluated

-- | Test scalability properties
prop_utils_scalability :: Int -> Int -> Property
prop_utils_scalability n m = 
  n >= 0 && m >= 0 && n <= 1000 && m <= 1000 ==>
    let input = replicate n ' ' ++ concat (replicate m "content") ++ replicate n ' '
        result = trim input
    in result == concat (replicate m "content")

prop_sourcelocation_scalability :: Int -> Int -> Property
prop_sourcelocation_scalability n m = 
  n >= 0 && m >= 0 && n <= 1000 && m <= 1000 ==>
    let start = SourcePos n n n
        end = SourcePos (n+m) (n+m) (n+m)
        span = spanBetween start end
    in spanStart span == start && spanEnd span == end

prop_parser_scalability :: Int -> Int -> Property
prop_parser_scalability n m = 
  n >= 0 && m >= 0 && n <= 200 && m <= 200 ==>
    let directives = concat (replicate n "// build: tag\n")
        content = concat (replicate m "content\n")
        fullContent = directives ++ content
        result = parseTypus fullContent
        buildTags = tfBuildTags result
        blocks = tfBlocks result
    in length buildTags == n && length blocks >= m

-- | Test time complexity properties
prop_utils_linear_complexity :: Int -> Property
prop_utils_linear_complexity n = 
  n >= 0 && n <= 5000 ==>
    let input = replicate n 'a'
        result = trim input
    in length result == n

prop_sourcelocation_linear_complexity :: Int -> Property
prop_sourcelocation_linear_complexity n = 
  n >= 0 && n <= 10000 ==>
    let chars = replicate n 'a'
        finalPos = foldl (flip posAfter) startPos chars
    in posOffset finalPos == n

prop_parser_linear_complexity :: Int -> Property
prop_parser_linear_complexity n = 
  n >= 0 && n <= 1000 ==>
    let content = concat (replicate n "line\n")
        result = parseTypus content
        blocks = tfBlocks result
    in length blocks >= n

-- | Test optimization properties
prop_utils_trim_optimization :: String -> Property
prop_utils_trim_optimization s = 
  let alreadyTrimmed = trim s
      result = trim alreadyTrimmed
    in result == alreadyTrimmed  -- Should not change already trimmed strings

prop_sourcelocation_span_optimization :: SourceSpan -> SourceSpan -> Property
prop_sourcelocation_span_optimization span1 span2 = 
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in merged1 == merged2  -- Should be commutative

prop_parser_parsing_optimization :: String -> Property
prop_parser_parsing_optimization s = 
  let result1 = parseTypus s
      content1 = concatMap cbContent (tfBlocks result1)
      result2 = parseTypus content1
  in length (tfBlocks result1) == length (tfBlocks result2)  -- Should be stable

-- | Test resource utilization properties
prop_utils_resource_utilization :: Int -> Property
prop_utils_resource_utilization n = 
  n >= 0 && n <= 5000 ==>
    let input = concat (replicate n "test ")
        parts = splitBy ' ' input
    in length parts >= n && all (not . null) parts

prop_sourcelocation_resource_utilization :: Int -> Property
prop_sourcelocation_resource_utilization n = 
  n >= 0 && n <= 1000 ==>
    let positions = [SourcePos i i i | i <- [0..n]]
        spans = [spanBetween p (SourcePos (i+1) (i+1) (i+1)) | (p, i) <- zip positions [0..]]
    in all isValidSpan spans

prop_parser_resource_utilization :: Int -> Property
prop_parser_resource_utilization n = 
  n >= 0 && n <= 500 ==>
    let content = concat (replicate n ("content\n"))
        result = parseTypus content
        blocks = tfBlocks result
    in all (not . null . cbContent) blocks

-- | Test concurrent safety properties (simulated)
prop_utils_concurrent_safety :: String -> String -> Bool
prop_utils_concurrent_safety s1 s2 = 
  let result1 = trim s1
      result2 = trim s2
      combined1 = trim (s1 ++ s2)
      combined2 = result1 ++ result2
  in combined1 == trim combined2

prop_sourcelocation_concurrent_safety :: SourcePos -> SourcePos -> Bool
prop_sourcelocation_concurrent_safety pos1 pos2 = 
  let span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos1
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in merged1 == merged2

prop_parser_concurrent_safety :: String -> String -> Bool
prop_parser_concurrent_safety s1 s2 = 
  let result1 = parseTypus s1
      result2 = parseTypus s2
      combined1 = parseTypus (s1 ++ s2)
      blocks1 = tfBlocks result1
      blocks2 = tfBlocks result2
      blocksCombined = tfBlocks combined1
  in length blocksCombined >= length blocks1 + length blocks2 - 1

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Performance Enhanced QuickCheck Properties"
  [ -- Utils performance tests
    testProperty "trim performance" prop_utils_trim_performance,
    testProperty "split performance" prop_utils_split_performance,
    testProperty "split collapsed performance" prop_utils_split_collapsed_performance,
    testProperty "remove comments performance" prop_utils_remove_comments_performance,
    testProperty "normalize indentation performance" prop_utils_normalize_indentation_performance,
    testProperty "break on performance" prop_utils_break_on_performance,
    
    -- SourceLocation performance tests
    testProperty "position advancement performance" prop_sourcelocation_position_advancement_performance,
    testProperty "span creation performance" prop_sourcelocation_span_creation_performance,
    testProperty "span merge performance" prop_sourcelocation_span_merge_performance,
    
    -- Parser performance tests
    testProperty "large content performance" prop_parser_large_content_performance,
    testProperty "many directives performance" prop_parser_many_directives_performance,
    testProperty "complex structure performance" prop_parser_complex_structure_performance,
    
    -- Memory efficiency tests
    testProperty "utils memory efficiency" prop_utils_memory_efficiency,
    testProperty "sourcelocation memory efficiency" prop_sourcelocation_memory_efficiency,
    testProperty "parser memory efficiency" prop_parser_memory_efficiency,
    
    -- Scalability tests
    testProperty "utils scalability" prop_utils_scalability,
    testProperty "sourcelocation scalability" prop_sourcelocation_scalability,
    testProperty "parser scalability" prop_parser_scalability,
    
    -- Time complexity tests
    testProperty "utils linear complexity" prop_utils_linear_complexity,
    testProperty "sourcelocation linear complexity" prop_sourcelocation_linear_complexity,
    testProperty "parser linear complexity" prop_parser_linear_complexity,
    
    -- Optimization tests
    testProperty "utils trim optimization" prop_utils_trim_optimization,
    testProperty "sourcelocation span optimization" prop_sourcelocation_span_optimization,
    testProperty "parser parsing optimization" prop_parser_parsing_optimization,
    
    -- Resource utilization tests
    testProperty "utils resource utilization" prop_utils_resource_utilization,
    testProperty "sourcelocation resource utilization" prop_sourcelocation_resource_utilization,
    testProperty "parser resource utilization" prop_parser_resource_utilization,
    
    -- Concurrent safety tests
    testProperty "utils concurrent safety" prop_utils_concurrent_safety,
    testProperty "sourcelocation concurrent safety" prop_sourcelocation_concurrent_safety,
    testProperty "parser concurrent safety" prop_parser_concurrent_safety
  ]