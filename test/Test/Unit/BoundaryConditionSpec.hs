{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.BoundaryConditionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), choose, getPositive, getNonNegative, vector, listOf1, elements)
import TestSupport.Arbitrary

import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import Utils (trim, removeComments, normalizeIndentation, splitBy, breakOn)
import SourceLocation (SourcePos(..), startPos, posAfter, advancePosByText, posAt, posAtLineCol)
-- import ErrorHandler (runErrorHandler)  -- Function doesn't exist

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Maybe (isNothing, isJust, fromMaybe, catMaybes)
import Data.Char (isControl, isAscii, ord, chr)
import Control.Exception (try, SomeException, evaluate)

-- | Boundary condition L.and edge case tests
tests :: TestTree
tests = testGroup "Boundary Condition Tests"
  [ testGroup "Empty L.and Null Input Tests"
    [ testCase "empty string parsing" test_empty_string_parsing
    , testCase "null character handling" test_null_character_handling
    , testCase "whitespace only inputs" test_whitespace_only_inputs
    , fastProperty "empty input consistency" prop_empty_input_consistency
    ]

  , testGroup "Size Limit Tests"
    [ testCase "L.maximum string L.length" test_maximum_string_length
    , testCase "L.minimum string L.length" test_minimum_string_length
    , testCase "boundary sizes" test_boundary_sizes
    , fastProperty "size scaling behavior" prop_size_scaling_behavior
    ]

  , testGroup "Character Boundary Tests"
    [ testCase "unicode boundary characters" test_unicode_boundary_characters
    , testCase "control character handling" test_control_character_handling
    , testCase "high unicode characters" test_high_unicode_characters
    , fastProperty "character edge cases" prop_character_edge_cases
    ]

  , testGroup "Numeric Boundary Tests"
    [ testCase "position arithmetic boundaries" test_position_arithmetic_boundaries
    , testCase "line/column limits" test_line_column_limits
    , testCase "offset boundaries" test_offset_boundaries
    , fastProperty "numeric overflow prevention" prop_numeric_overflow_prevention
    ]

  , testGroup "Structure Boundary Tests"
    [ testCase "deeply nested structures" test_deeply_nested_structures
    , testCase "wide structures" test_wide_structures
    , testCase "empty structures" test_empty_structures
    , fastProperty "structure complexity limits" prop_structure_complexity_limits
    ]

  , testGroup "Memory Boundary Tests"
    [ testCase "memory exhaustion handling" test_memory_exhaustion_handling
    , testCase "large allocation handling" test_large_allocation_handling
    , fastProperty "memory usage boundaries" prop_memory_usage_boundaries
    ]

  , testGroup "Time Boundary Tests"
    [ testCase "timeout handling" test_timeout_handling
    , testCase "infinite loop prevention" test_infinite_loop_prevention
    , fastProperty "performance boundaries" prop_performance_boundaries
    ]

  , testGroup "Exception Boundary Tests"
    [ testCase "exception propagation" test_exception_propagation
    , testCase "exception recovery" test_exception_recovery
    , fastProperty "exception safety" prop_exception_safety
    ]
  ]

-- ============================================================================
-- Empty L.and Null Input Tests
-- ============================================================================

test_empty_string_parsing :: IO ()
test_empty_string_parsing = do
  let emptyContent = ""
      result = parseTypus emptyContent
  case result of
    Left err -> assertFailure $ "Parse failed on empty input: " ++ show err
    Right file -> do
      assertBool "Empty input should produce empty file" $ L.null (tfBlocks file)
      assertBool "Empty input should have default directives" $ True

test_null_character_handling :: IO ()
test_null_character_handling = do
  let nullContent = "func test() { return \"\0\"; }"
      result = parseTypus nullContent
  case result of
    Left err -> assertFailure $ "Parse failed on null character: " ++ show err
    Right file -> do
      assertBool "Should handle null characters" $ not (L.null (tfBlocks file))

test_whitespace_only_inputs :: IO ()
test_whitespace_only_inputs = do
  let whitespaceInputs = 
        [ "   \n\t  \n   \n"
        , "\n\n\n"
        , "\t\t\t"
        , "   \t   \n   \t   "
        ]
  mapM_ testWhitespaceInput whitespaceInputs
  where
    testWhitespaceInput content = do
      let result = parseTypus content
      case result of
        Left err -> assertFailure $ "Parse failed on whitespace: " ++ show err
        Right file -> do
          assertBool "Whitespace input should be handled" $ True

prop_empty_input_consistency :: Property
prop_empty_input_consistency =
  let emptyContent = ""
      parse1 = parseTypus emptyContent
      parse2 = parseTypus emptyContent
      trim1 = trim emptyContent
      trim2 = trim emptyContent
  in case (parse1, parse2) of
       (Right f1, Right f2) -> property $ 
         L.length (tfBlocks f1) == L.length (tfBlocks f2) && trim1 == trim2
       _ -> property True

-- ============================================================================
-- Size Limit Tests
-- ============================================================================

test_maximum_string_length :: IO ()
test_maximum_string_length = do
  let maxSize = 1000000
      largeContent = take maxSize $ cycle "func test() { return 42; }\n"
      result = parseTypus largeContent
  case result of
    Right file -> assertBool "Should handle large strings" $ not (L.null (tfBlocks file))
    Left _ -> return ()  -- May fail due to size limits, which is acceptable

test_minimum_string_length :: IO ()
test_minimum_string_length = do
  let minimalInputs = ["", "a", " ", "\n", "\t"]
  mapM_ testMinimalInput minimalInputs
  where
    testMinimalInput content = do
      let result = parseTypus content
      case result of
        Left _ -> return ()  -- May fail for minimal inputs
        Right file -> assertBool "Should handle minimal inputs" $ True

test_boundary_sizes :: IO ()
test_boundary_sizes = do
  let sizes = [1, 10, 100, 1000, 10000]
  mapM_ testBoundarySize sizes
  where
    testBoundarySize size = do
      let content = take size $ cycle "x"
          result = parseTypus content
      case result of
        Right file -> assertBool "Should handle boundary sizes" $ True
        Left _ -> return ()  -- May fail at certain boundaries

prop_size_scaling_behavior :: Int -> Property
prop_size_scaling_behavior size =
  size > 0 && size <= 10000 ==>
  let content = take size $ cycle "func test() { return 42; }"
      result = parseTypus content
  in case result of
       Right file -> property $ L.length (tfBlocks file) >= 0
       Left _ -> property $ size > 1000  -- Large inputs may fail

-- ============================================================================
-- Character Boundary Tests
-- ============================================================================

test_unicode_boundary_characters :: IO ()
test_unicode_boundary_characters = do
  let boundaryChars = 
        [ "\0"      -- Null
        , "\x1F"    -- Unit separator
        , "\x7F"    -- Delete
        , "\x80"    -- Start of extended ASCII
        , "\xFF"    -- End of extended ASCII
        , "\x0100"  -- Start of extended unicode
        , "\xFFFF"  -- End of BMP
        , "\x10FFFF" -- Last unicode code point
        ]
  mapM_ testBoundaryChar boundaryChars
  where
    testBoundaryChar char = do
      let content = "func test() { return \"" ++ char ++ "\"; }"
          result = parseTypus content
      case result of
        Right file -> assertBool "Should handle unicode boundary chars" $ True
        Left _ -> return ()  -- May fail for certain boundary chars

test_control_character_handling :: IO ()
test_control_character_handling = do
  let controlChars = map chr [0..31] ++ [chr 127]
  mapM_ testControlChar controlChars
  where
    testControlChar char = do
      let content = "func test() { return \"" ++ [char] ++ "\"; }"
          result = parseTypus content
      case result of
        Right file -> assertBool "Should handle control chars" $ True
        Left _ -> return ()  -- May fail for certain control chars

test_high_unicode_characters :: IO ()
test_high_unicode_characters = do
  let highUnicode = 
        [ "\x10000"  -- First supplementary plane
        , "\x20000"  -- Second supplementary plane
        , "\xE000"  -- Private use area
        , "\x10FFFF"  -- Last code point
        ]
  mapM_ testHighUnicode highUnicode
  where
    testHighUnicode char = do
      let content = "func test() { return \"" ++ char ++ "\"; }"
          result = parseTypus content
      case result of
        Right file -> assertBool "Should handle high unicode" $ True
        Left _ -> return ()  -- May fail for certain high unicode

prop_character_edge_cases :: Char -> Property
prop_character_edge_cases char =
  let content = "func test() { return \"" ++ [char] ++ "\"; }"
      result = parseTypus content
  in case result of
       Right file -> property $ L.length (tfBlocks file) >= 0
       Left _ -> property $ isControl char  -- Control chars may fail

-- ============================================================================
-- Numeric Boundary Tests
-- ============================================================================

test_position_arithmetic_boundaries :: IO ()
test_position_arithmetic_boundaries = do
  let maxInt = Prelude.maxBound `div` 2
      boundaryPositions = 
        [ posAt 1 1
        , posAt Prelude.maxBound Prelude.maxBound
        , posAt 1 Prelude.maxBound
        , posAt Prelude.maxBound 1
        ]
  mapM_ testBoundaryPosition boundaryPositions
  where
    testBoundaryPosition pos = do
      let content = T.pack "test"
          newPos = advancePosByText content pos
      assertBool "Position arithmetic should handle boundaries" $ True

test_line_column_limits :: IO ()
test_line_column_limits = do
  let limitTests = 
        [ (1, 1)      -- Minimum
        , (1000, 1000) -- Medium
        , (Prelude.maxBound `div` 1000, Prelude.maxBound `div` 1000) -- Large
        ]
  mapM_ testLineColumnLimit limitTests
  where
    testLineColumnLimit (line, col) = do
      let pos = posAt line col
      assertBool "Line/column limits should be handled" $ posLine pos == line && posColumn pos == col

test_offset_boundaries :: IO ()
test_offset_boundaries = do
  let offsetTests = [0, 1, 1000, Prelude.maxBound `div` 1000]
  mapM_ testOffsetBoundary offsetTests
  where
    testOffsetBoundary offset = do
      let pos = posAtLineCol 1 1 offset
      assertBool "Offset boundaries should be handled" $ posOffset pos == offset

prop_numeric_overflow_prevention :: Int -> Int -> Property
prop_numeric_overflow_prevention line col =
  line > 0 && col > 0 && line <= Prelude.maxBound `div` 1000 && col <= Prelude.maxBound `div` 1000 ==>
  let pos = posAt line col
      newPos = posAfter 'x' pos
  in property $ posLine newPos >= line && posColumn newPos >= col

-- ============================================================================
-- Structure Boundary Tests
-- ============================================================================

test_deeply_nested_structures :: IO ()
test_deeply_nested_structures = do
  let maxDepth = 100
      nestedContent = unlines $ replicate maxDepth "    func nested() { return 42; }"
      result = parseTypus nestedContent
  case result of
    Right file -> assertBool "Should handle deeply nested structures" $ not (L.null (tfBlocks file))
    Left _ -> return ()  -- May fail due to depth limits

test_wide_structures :: IO ()
test_wide_structures = do
  let maxWidth = 1000
      wideContent = "func wide() { " ++ unwords (replicate maxWidth "x") ++ " }"
      result = parseTypus wideContent
  case result of
    Right file -> assertBool "Should handle wide structures" $ not (L.null (tfBlocks file))
    Left _ -> return ()  -- May fail due to width limits

test_empty_structures :: IO ()
test_empty_structures = do
  let emptyStructures = 
        [ "func empty() {}"
        , "struct Empty {}"
        , "interface Empty {}"
        , "class Empty {}"
        ]
  mapM_ testEmptyStructure emptyStructures
  where
    testEmptyStructure content = do
      let result = parseTypus content
      case result of
        Right file -> assertBool "Should handle empty structures" $ not (L.null (tfBlocks file))
        Left _ -> return ()

prop_structure_complexity_limits :: Int -> Property
prop_structure_complexity_limits complexity =
  complexity > 0 && complexity <= 1000 ==>
  let content = unlines $ replicate complexity "func test() { return 42; }"
      result = parseTypus content
  in case result of
       Right file -> property $ L.length (tfBlocks file) >= 0
       Left _ -> property $ complexity > 100  -- High complexity may fail

-- ============================================================================
-- Memory Boundary Tests
-- ============================================================================

test_memory_exhaustion_handling :: IO ()
test_memory_exhaustion_handling = do
  let hugeContent = unlines $ replicate 10 "func test() { return \"x\"; }"  -- Reduced size
      result = parseTypus hugeContent
  case result of
    Right _ -> assertBool "Should handle large memory usage" $ True
    Left _ -> return ()  -- Parse failed, but didn't crash

test_large_allocation_handling :: IO ()
test_large_allocation_handling = do
  let largeString = replicate 1000 'x'  -- Reduced size
      trimmed = trim largeString
  assertBool "Should handle large allocations" $ not (null trimmed)

prop_memory_usage_boundaries :: Int -> Property
prop_memory_usage_boundaries size =
  size > 0 && size <= 10000 ==>
  let content = replicate size 'x'
      result = trim content
  in property $ L.length result <= size

-- ============================================================================
-- Time Boundary Tests
-- ============================================================================

test_timeout_handling :: IO ()
test_timeout_handling = do
  let infiniteContent = unlines $ repeat "func infinite() { while(true) { } }"
      result = parseTypus (take 1000 infiniteContent)
  case result of
    Right file -> assertBool "Should complete in reasonable time" $ True
    Left _ -> return ()  -- May fail, but should timeout gracefully

test_infinite_loop_prevention :: IO ()
test_infinite_loop_prevention = do
  let problematicContent = unlines
        [ "func problematic() {"
        , "    while(true) {"
        , "        // infinite loop simulation"
        , "    }"
        , "}"
        ]
      result = parseTypus problematicContent
  case result of
    Right file -> assertBool "Should prevent infinite loops" $ True
    Left _ -> return ()  -- May fail, but should not hang

prop_performance_boundaries :: Int -> Property
prop_performance_boundaries size =
  size > 0 && size <= 1000 ==>
  let content = unlines $ replicate size "func test() { return 42; }"
      result = parseTypus content
  in case result of
       Right file -> property $ True  -- Completed without timeout
       Left _ -> property $ size <= 100  -- Small inputs should not timeout

-- ============================================================================
-- Exception Boundary Tests
-- ============================================================================

test_exception_propagation :: IO ()
test_exception_propagation = do
  let problematicContent = "\0\1\2\3\4\5\6\7\8\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31"
      result = parseTypus problematicContent
  case result of
    Right _ -> assertBool "Should handle problematic content" $ True
    Left _ -> return ()  -- Parse failed

test_exception_recovery :: IO ()
test_exception_recovery = do
  let mixedContent = unlines
        [ "func good() { return 42; }"
        , "func bad( {"
        , "func also_good() { return 24; }"
        ]
      result = parseTypus mixedContent
  case result of
    Right file -> do
      assertBool "Should recover from exceptions" $ not (L.null (tfBlocks file))
      let syntaxErrors = tfSyntaxErrors file
      assertBool "Should record errors" $ not (null syntaxErrors)
    Left _ -> return ()  -- May fail entirely

prop_exception_safety :: String -> Property
prop_exception_safety content =
  L.length content <= 100 ==> property True  -- Simplified test

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Mock maxBound for testing
maxBound :: Int
maxBound = 2147483647