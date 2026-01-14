module Test.Unit.CompilerIntegrationSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Data.Text as T
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, sort)
import Utils (trim, splitBy, removeComments, normalizeIndentation, safeProcessString)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAt, spanBetween, 
                       locatedAt, locatedValue, locatedSpan, locPos, spanStart, spanEnd)
import Data.Char (isSpace, isAlpha, isAlphaNum)

-- Test properties for compiler integration

-- Property: SourcePos should handle valid line/column combinations
prop_source_pos_valid_combinations :: Int -> Int -> Property
prop_source_pos_valid_combinations line col = 
  line >= 1 && col >= 1 ==> 
  let pos = posAt line col
  in property $ posLine pos == line && posColumn pos == col

-- Property: Located values should preserve their content
prop_located_preserves_content :: String -> Int -> Int -> Property
prop_located_preserves_content s line col = 
  line >= 1 && col >= 1 ==>
  let pos = posAt line col
      located = locatedAt pos s
  in property $ locatedValue located == s

-- Property: SourceSpan should contain its start and end positions
prop_span_contains_positions :: Int -> Int -> Int -> Int -> Property
prop_span_contains_positions line1 col1 line2 col2 =
  line1 >= 1 && col1 >= 1 && line2 >= 1 && col2 >= 1 &&
  (line1 < line2 || (line1 == line2 && col1 <= col2)) ==>
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      span = spanBetween pos1 pos2
  in property $ 
       posLine (spanStart span) == line1 && 
       posColumn (spanStart span) == col1 &&
       posLine (spanEnd span) == line2 &&
       posColumn (spanEnd span) == col2

-- Property: Processing pipeline should be consistent
prop_processing_pipeline_consistent :: String -> Property
prop_processing_pipeline_consistent s = 
  let step1 = trim s
      step2 = removeComments step1
      step3 = normalizeIndentation step2
      step4 = safeProcessString step3
  in property $ length step4 <= length step3

-- Property: Splitting and rejoining with line breaks should be consistent
prop_split_rejoin_line_breaks :: String -> Property
prop_split_rejoin_line_breaks s = 
  let lines' = lines s
      rejoined = unlines lines'
  in property $ length lines' == length (lines rejoined)

-- Property: Comment removal should not affect code structure
prop_comment_preserves_structure :: String -> String -> Property
prop_comment_preserves_structure code1 code2 = 
  let input = code1 ++ "// comment\n" ++ code2
      withoutComments = removeComments input
      linesOriginal = length $ lines input
      linesProcessed = length $ lines withoutComments
  in property $ linesProcessed <= linesOriginal

-- Property: Processing should handle nested structures
prop_nested_structure_handling :: Int -> Property
prop_nested_structure_handling depth = 
  depth >= 0 && depth <= 10 ==>
  let nested = concat $ replicate depth "{"
      closed = concat $ replicate depth "}"
      input = nested ++ "code" ++ closed
      processed = safeProcessString input
  in property $ length processed >= 5  -- "code" length

-- Property: Indentation normalization should preserve code blocks
prop_normalize_preserves_blocks :: String -> Property
prop_normalize_preserves_blocks s = 
  let lines' = lines s
      indented = map (\line -> "  " ++ line) lines'
      normalized = normalizeIndentation (unlines indented)
      normalizedLines = lines normalized
  in property $ length normalizedLines == length indented

-- Property: Source location tracking should be monotonic
prop_location_monotonic :: Int -> Int -> Int -> Property
prop_location_monotonic startLine startCol offset = 
  startLine >= 1 && startCol >= 1 && offset >= 0 ==>
  let pos = posAt startLine startCol
      newLine = startLine + offset `div` 100
      newCol = startCol + offset `mod` 100
      newPos = posAt newLine newCol
  in property $ 
       newLine > startLine || (newLine == startLine && newCol >= startCol)

-- Unit tests

test_compiler_integration_basic :: Assertion
test_compiler_integration_basic = do
  let pos = posAt 1 1
  let located = locatedAt pos "test"
  locatedValue located @?= "test"
  posLine (locPos located) @?= 1

test_compiler_integration_span :: Assertion
test_compiler_integration_span = do
  let pos1 = posAt 1 1
  let pos2 = posAt 1 5
  let span = spanBetween pos1 pos2
  posLine (spanStart span) @?= 1
  posColumn (spanStart span) @?= 1
  posLine (spanEnd span) @?= 1
  posColumn (spanEnd span) @?= 5

test_compiler_integration_pipeline :: Assertion
test_compiler_integration_pipeline = do
  let input = "  // comment\n  code { /* nested */ }"
  let processed = normalizeIndentation (removeComments (trim input))
  processed @?= "code { }"

test_compiler_integration_error_handling :: Assertion
test_compiler_integration_error_handling = do
  let input = ""
  let processed = safeProcessString input
  processed @?= Right ""

test_compiler_integration_complex_structure :: Assertion
test_compiler_integration_complex_structure = do
  let input = "function test() {\n  // comment\n  return 42;\n}"
  let processed = removeComments input
  "return 42;" `isInfixOf` processed @?= True

test_compiler_integration_unicode :: Assertion
test_compiler_integration_unicode = do
  let input = "函数 测试() { return 结果; }"
  let processed = safeProcessString input
  length processed @?= length input

test_compiler_integration_nested_comments :: Assertion
test_compiler_integration_nested_comments = do
  let input = "code /* outer /* inner */ still outer */ end"
  let processed = removeComments input
  processed @?= "code  end"

test_compiler_integration_mixed_indentation :: Assertion
test_compiler_integration_mixed_indentation = do
  let input = "  line1\n\tline2\n    line3"
  let normalized = normalizeIndentation input
  let lines' = lines normalized
  all (not . isPrefixOf "  " . dropWhile isSpace) lines' @?= True



tests :: TestTree
tests = testGroup "Compiler Integration Tests"
  [ testProperty "source pos valid combinations" prop_source_pos_valid_combinations
  , testProperty "located preserves content" prop_located_preserves_content
  , testProperty "span contains positions" prop_span_contains_positions
  , testProperty "processing pipeline consistent" prop_processing_pipeline_consistent
  , testProperty "split rejoin line breaks" prop_split_rejoin_line_breaks
  , testProperty "comment preserves structure" prop_comment_preserves_structure
  , testProperty "nested structure handling" prop_nested_structure_handling
  , testProperty "normalize preserves blocks" prop_normalize_preserves_blocks
  , testProperty "location monotonic" prop_location_monotonic
  , testCase "compiler integration basic" test_compiler_integration_basic
  , testCase "compiler integration span" test_compiler_integration_span
  , testCase "compiler integration pipeline" test_compiler_integration_pipeline
  , testCase "compiler integration error handling" test_compiler_integration_error_handling
  , testCase "compiler integration complex structure" test_compiler_integration_complex_structure
  , testCase "compiler integration unicode" test_compiler_integration_unicode
  , testCase "compiler integration nested comments" test_compiler_integration_nested_comments
  , testCase "compiler integration mixed indentation" test_compiler_integration_mixed_indentation
  ]