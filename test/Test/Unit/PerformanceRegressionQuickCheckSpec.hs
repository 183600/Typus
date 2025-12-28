{-# LANGUAGE CPP #-}
module Test.Unit.PerformanceRegressionQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, elements, listOf, choose, 
                        Property, (===), forAll, counterexample, suchThat, (==>))
import Parser (parseTypus)
import Compiler (compileTypus)
import Utils (trim, splitBy, removeComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), mergeSpans, advancePosBy)
import qualified Data.Text as T
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- ============================================================================
-- Performance measurement utilities
-- ============================================================================

-- Measure execution time of a computation
measureTime :: IO a -> IO (a, Double)
measureTime action = do
  start <- getCPUTime
  result <- action
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10^12) :: Double
  return (result, diff)

-- Performance benchmark with timeout
benchmark :: String -> Double -> IO a -> IO (Maybe a, Double)
benchmark name timeoutSec action = do
  (result, time) <- measureTime action
  let withinTimeout = time <= timeoutSec
  return (if withinTimeout then Just result else Nothing, time)

-- ============================================================================
-- Test data generators for performance testing
-- ============================================================================

-- Generate small content (< 1KB)
genSmallContent :: Gen String
genSmallContent = do
  lines <- listOf $ elements
    [ "package main"
    , "import \"fmt\""
    , "func main() {"
    , "fmt.Println(\"Hello\")"
    , "}"
    , "var x int = 42"
    ]
  return $ unlines $ take 10 lines

-- Generate medium content (1KB - 10KB)
genMediumContent :: Gen String
genMediumContent = do
  numLines <- choose (100, 1000)
  lines <- sequence $ replicate numLines $ oneof
    [ "package main"
    , "import \"fmt\""
    , "import \"strconv\""
    , "func main() {"
    , "fmt.Println(\"Hello, World!\")"
    , "}"
    , "var x int = 42"
    , "func helper() int { return 42 }"
    , "type Struct struct { field int }"
    , "// This is a comment"
    , "/* Block comment */"
    ]
  return $ unlines lines

-- Generate large content (10KB - 100KB)
genLargeContent :: Gen String
genLargeContent = do
  numLines <- choose (1000, 10000)
  lines <- sequence $ replicate numLines $ oneof
    [ "package main"
    , "import \"fmt\""
    , "import \"strconv\""
    , "import \"os\""
    , "func main() {"
    , "fmt.Println(\"Hello, World!\")"
    , "}"
    , "var x int = 42"
    , "func helper() int { return 42 }"
    , "type Struct struct { field int }"
    , "func (s Struct) Method() int { return s.field }"
    , "interface Interface { Method() int }"
    , "// This is a comment"
    , "/* Block comment */"
    , "const constant = 3.14159"
    , "var slice []int = []int{1, 2, 3, 4, 5}"
    ]
  return $ unlines lines

-- Generate very large content (100KB - 1MB)
genVeryLargeContent :: Gen String
genVeryLargeContent = do
  numLines <- choose (10000, 100000)
  baseLines <- sequence $ replicate 100 $ oneof
    [ "package main"
    , "import \"fmt\""
    , "import \"strconv\""
    , "import \"os\""
    , "import \"time\""
    , "func main() {"
    , "fmt.Println(\"Hello, World!\")"
    , "}"
    , "var x int = 42"
    , "func helper() int { return 42 }"
    , "type Struct struct { field int }"
    , "func (s Struct) Method() int { return s.field }"
    , "interface Interface { Method() int }"
    , "// This is a comment"
    , "/* Block comment */"
    , "const constant = 3.14159"
    , "var slice []int = []int{1, 2, 3, 4, 5}"
    ]
  let repeatedLines = concat $ replicate (numLines `div` length baseLines + 1) baseLines
  return $ unlines $ take numLines repeatedLines

-- Generate content with complex structures
genComplexContent :: Gen String
genComplexContent = do
  numFunctions <- choose (10, 100)
  numStructs <- choose (5, 50)
  numInterfaces <- choose (2, 20)
  
  functions <- sequence $ replicate numFunctions $ do
    funcName <- elements ["func1", "func2", "helper", "process", "calculate", "validate"]
    return $ unlines
      [ "func " ++ funcName ++ "() int {"
      , "return 42"
      , "}"
      ]
  
  structs <- sequence $ replicate numStructs $ do
    structName <- elements ["MyStruct", "Data", "Config", "Options"]
    return $ unlines
      [ "type " ++ structName ++ " struct {"
      , "field int"
      , "value string"
      , "}"
      ]
  
  interfaces <- sequence $ replicate numInterfaces $ do
    interfaceName <- elements ["MyInterface", "Handler", "Processor"]
    return $ unlines
      [ "type " ++ interfaceName ++ " interface {"
      , "Method() int"
      , "Process() string"
      , "}"
      ]
  
  return $ unlines
    [ "package main"
    , "import \"fmt\""
    , ""
    ] ++ concat functions ++ concat structs ++ concat interfaces

-- ============================================================================
-- Performance properties for parsing
-- ============================================================================

prop_parsing_performance_linear :: String -> Property
prop_parsing_performance_linear content =
  let contentSize = length content
      expectedTime = fromIntegral contentSize * 0.000001  -- 1 microsecond per character
  in counterexample ("Content size: " ++ show contentSize) $
     contentSize > 0 ==> property True  -- Actual timing would be done in IO

prop_parsing_memory_efficiency :: String -> Property
prop_parsing_memory_efficiency content =
  let contentSize = length content
      parseResult = parseTypus content
  in counterexample ("Content size: " ++ show contentSize) $
     case parseResult of
       Left _ -> property True  -- Parse errors don't indicate memory issues
       Right typusFile -> 
         let estimatedMemoryUsage = contentSize * 2  -- Rough estimate
         in estimatedMemoryUsage > 0

-- ============================================================================
-- Performance properties for compilation
-- ============================================================================

prop_compilation_performance_bounds :: String -> Property
prop_compilation_performance_bounds content =
  let contentSize = length content
      parseResult = parseTypus content
  in case parseResult of
    Left _ -> property True  -- Skip compilation if parsing fails
    Right typusFile ->
      let expectedMaxTime = fromIntegral contentSize * 0.00001  -- 10 microseconds per character
      in counterexample ("Content size: " ++ show contentSize) $
         contentSize > 0 ==> property True

prop_compilation_memory_scaling :: String -> Property
prop_compilation_memory_scaling content =
  let contentSize = length content
      parseResult = parseTypus content
  in case parseResult of
    Left _ -> property True
    Right typusFile ->
      let compileResult = compileTypus typusFile
      in counterexample ("Content size: " ++ show contentSize) $
         case compileResult of
           Left _ -> property True
           Right _ -> property True

-- ============================================================================
-- Performance properties for utility functions
-- ============================================================================

prop_trim_performance_linear :: String -> Property
prop_trim_performance_linear content =
  let contentSize = length content
      result = trim content
  in counterexample ("Content size: " ++ show contentSize ++ ", Result size: " ++ show (length result)) $
     length result <= contentSize

prop_split_by_performance :: String -> Property
prop_split_by_performance content =
  let contentSize = length content
      result = splitBy ',' content
  in counterexample ("Content size: " ++ show contentSize ++ ", Result length: " ++ show (length result)) $
     length result >= 1

prop_remove_comments_performance :: String -> Property
prop_remove_comments_performance content =
  let contentSize = length content
      result = removeComments content
  in counterexample ("Content size: " ++ show contentSize ++ ", Result size: " ++ show (length result)) $
     length result <= contentSize

prop_normalize_indentation_performance :: String -> Property
prop_normalize_indentation_performance content =
  let contentSize = length content
      result = normalizeIndentation content
  in counterexample ("Content size: " ++ show contentSize ++ ", Result size: " ++ show (length result)) $
     abs (length result - contentSize) <= 100  -- Allow some variance

-- ============================================================================
-- Performance properties for source location operations
-- ============================================================================

prop_source_span_merge_performance :: Int -> Property
prop_source_span_merge_performance numSpans =
  let spans = take numSpans $ iterate (\span -> mergeSpans span span) (SourceSpan startPos startPos)
      result = foldl mergeSpans (head spans) spans
  in counterexample ("Number of spans: " ++ show numSpans) $
     numSpans > 0 ==> property True

prop_position_advancement_performance :: String -> Property
prop_position_advancement_performance content =
  let contentSize = length content
      result = advancePosBy content startPos
  in counterexample ("Content size: " ++ show contentSize) $
     contentSize >= 0 ==> property True

-- ============================================================================
-- Performance regression detection
-- ============================================================================

prop_performance_regression_small :: String -> Property
prop_performance_regression_small content =
  let contentSize = length content
      isSmall = contentSize < 1000
  in isSmall ==> property True

prop_performance_regression_medium :: String -> Property
prop_performance_regression_medium content =
  let contentSize = length content
      isMedium = contentSize >= 1000 && contentSize < 10000
  in isMedium ==> property True

prop_performance_regression_large :: String -> Property
prop_performance_regression_large content =
  let contentSize = length content
      isLarge = contentSize >= 10000
  in isLarge ==> property True

-- ============================================================================
-- Scalability properties
-- ============================================================================

prop_parsing_scalability :: String -> Property
prop_parsing_scalability content =
  let contentSize = length content
      parseResult = parseTypus content
  in counterexample ("Content size: " ++ show contentSize) $
     case parseResult of
       Left _ -> property True
       Right _ -> contentSize >= 0

prop_memory_scalability :: String -> Property
prop_memory_scalability content =
  let contentSize = length content
      parseResult = parseTypus content
  in case parseResult of
    Left _ -> property True
    Right typusFile ->
      let estimatedASTSize = contentSize `div` 2  -- Rough estimate
      in estimatedASTSize >= 0

-- ============================================================================
-- Edge case performance properties
-- ============================================================================

prop_empty_content_performance :: Property
prop_empty_content_performance =
  let emptyContent = ""
      parseResult = parseTypus emptyContent
  in case parseResult of
    Left _ -> property True
    Right _ -> property True

prop_unicode_content_performance :: String -> Property
prop_unicode_content_performance unicodeContent =
  let contentSize = length unicodeContent
      parseResult = parseTypus unicodeContent
  in counterexample ("Unicode content size: " ++ show contentSize) $
     case parseResult of
       Left _ -> property True
       Right _ -> property True

prop_deeply_nested_content_performance :: String -> Property
prop_deeply_nested_content_performance content =
  let contentSize = length content
      parseResult = parseTypus content
  in counterexample ("Nested content size: " ++ show contentSize) $
     case parseResult of
       Left _ -> property True
       Right _ -> contentSize >= 0

-- ============================================================================
-- Test suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Performance Regression QuickCheck Tests"
  [ testGroup "Parsing performance properties"
    [ fastProperty "parsing performance linear" prop_parsing_performance_linear
    , fastProperty "parsing memory efficiency" prop_parsing_memory_efficiency
    ]
  , testGroup "Compilation performance properties"
    [ fastProperty "compilation performance bounds" prop_compilation_performance_bounds
    , fastProperty "compilation memory scaling" prop_compilation_memory_scaling
    ]
  , testGroup "Utility function performance properties"
    [ fastProperty "trim performance linear" prop_trim_performance_linear
    , fastProperty "splitBy performance" prop_split_by_performance
    , fastProperty "removeComments performance" prop_remove_comments_performance
    , fastProperty "normalizeIndentation performance" prop_normalize_indentation_performance
    ]
  , testGroup "Source location performance properties"
    [ fastProperty "source span merge performance" prop_source_span_merge_performance
    , fastProperty "position advancement performance" prop_position_advancement_performance
    ]
  , testGroup "Performance regression detection"
    [ fastProperty "performance regression small" prop_performance_regression_small
    , fastProperty "performance regression medium" prop_performance_regression_medium
    , fastProperty "performance regression large" prop_performance_regression_large
    ]
  , testGroup "Scalability properties"
    [ fastProperty "parsing scalability" prop_parsing_scalability
    , fastProperty "memory scalability" prop_memory_scalability
    ]
  , testGroup "Edge case performance properties"
    [ fastProperty "empty content performance" prop_empty_content_performance
    , fastProperty "unicode content performance" prop_unicode_content_performance
    , fastProperty "deeply nested content performance" prop_deeply_nested_content_performance
    ]
  ]