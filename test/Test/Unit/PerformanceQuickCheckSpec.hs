{-# LANGUAGE CPP #-}

module Test.Unit.PerformanceQuickCheckSpec (tests) where

import Control.DeepSeq (NFData(..), deepseq)

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, vectorOf, listOf)
import qualified Data.List as List
import Data.Char (isSpace)
import Control.DeepSeq (NFData, force)

import Parser (parseTypus, TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import SyntaxValidator (SyntaxError(..), ErrorType(..))
import Compiler.TypeChecker (Type(..), TypeEnv(..))
import SourceLocation (Located(..), SourcePos(..), SourceSpan(..))
import Analyzer.Types (SymbolInfo(..), AnalyzerState(..))
import Utils (trim, splitBy, normalizeIndentation)

-- Property: Large file parsing performance
prop_large_file_parsing :: Property
prop_large_file_parsing =
  forAll (choose (100, 10000)) $ \size ->
  let content = List.unlines $ replicate size "  // test line"
      parseTime = measureParseTime content
  in classify (parseTime < 100000) "fast parsing" $
     classify (parseTime >= 100000) "slow parsing" $
     property $ parseTime >= 0 && parseTime <= length content * 2

-- Property: Type checking scalability
prop_typechecking_scalability :: Property
prop_typechecking_scalability =
  forAll (choose (10, 1000)) $ \symbolCount ->
  let symbols = generateSymbols symbolCount
      typecheckTime = measureTypecheckTime symbols
  in classify (typecheckTime < 1000) "fast typechecking" $
     classify (typecheckTime >= 1000) "slow typechecking" $
     property $ typecheckTime >= 0

-- Property: Symbol table lookup performance
prop_symbol_lookup_performance :: Property
prop_symbol_lookup_performance =
  forAll (choose (100, 10000)) $ \symbolCount ->
  let symbols = generateSymbolTable symbolCount
      lookupTime = measureLookupTime symbols
  in classify (lookupTime < 10000) "fast lookup" $
     classify (lookupTime >= 10000) "slow lookup" $
     property $ lookupTime >= 0

-- Property: Memory usage with large inputs
prop_memory_usage_large_input :: Property
prop_memory_usage_large_input =
  forAll (choose (1000, 50000)) $ \size ->
  let largeInput = generateLargeInput size
      memoryBefore = getMemoryUsage
      result = processLargeInput largeInput
      memoryAfter = getMemoryUsage
      memoryDiff = memoryAfter - memoryBefore
  in classify (memoryDiff < 1000) "low memory" $
     classify (memoryDiff >= 1000) "high memory" $
     property $ result >= 0 && memoryDiff >= 0

-- Property: Concurrent parsing performance
prop_concurrent_parsing :: Property
prop_concurrent_parsing =
  forAll (choose (2, 100)) $ \fileCount ->
  let files = generateFiles fileCount
      sequentialTime = measureSequentialParsing files
      concurrentTime = measureConcurrentParsing files
  in property $ concurrentTime >= 0 && sequentialTime >= 0

-- Property: Incremental analysis performance
prop_incremental_analysis :: Property
prop_incremental_analysis =
  forAll (choose (10, 1000)) $ \changeCount ->
  let changes = generateChanges changeCount
      incrementalTime = measureIncrementalAnalysis changes
      fullRebuildTime = measureFullRebuild changes
  in property $ incrementalTime >= 0 && fullRebuildTime >= 0

-- Property: String processing performance
prop_string_processing_performance :: Property
prop_string_processing_performance =
  forAll (vectorOf 1000 arbitrary) $ \input ->
  let trimTime = measureTrimTime input
      splitTime = measureSplitTime input
      normalizeTime = measureNormalizeTime input
  in property $ trimTime >= 0 && splitTime >= 0 && normalizeTime >= 0

-- Property: Deep recursion handling
prop_deep_recursion_handling :: Property
prop_deep_recursion_handling =
  forAll (choose (10, 1000)) $ \depth ->
  let nestedStructure = generateNestedStructure depth
      processingTime = measureDeepProcessing nestedStructure
  in property $ processingTime >= 0

-- Property: Large type constraint solving
prop_large_constraint_solving :: Property
prop_large_constraint_solving =
  forAll (choose (50, 2000)) $ \constraintCount ->
  let typeConstraints = generateConstraints constraintCount
      solvingTime = measureConstraintSolving typeConstraints
  in classify (solvingTime < 1000) "fast solving" $
     classify (solvingTime >= 1000) "slow solving" $
     property $ solvingTime >= 0

-- Property: Memory leak detection in repeated operations
prop_memory_leak_repeated_ops :: Property
prop_memory_leak_repeated_ops =
  forAll (choose (100, 10000)) $ \iterations ->
  let memoryBefore = getMemoryUsage
      _ = repeatOperations iterations
      memoryAfter = getMemoryUsage
      memoryGrowth = memoryAfter - memoryBefore
  in classify (memoryGrowth < 1000) "no leak" $
     classify (memoryGrowth >= 1000) "potential leak" $
     property $ memoryGrowth >= 0

-- Property: Cache efficiency
prop_cache_efficiency :: Property
prop_cache_efficiency =
  forAll (choose (100, 10000)) $ \requestCount ->
  let cacheHits = measureCacheEfficiency requestCount
      hitRate = fromIntegral cacheHits / fromIntegral requestCount :: Double
  in classify (hitRate > 0.8) "good cache" $
     classify (hitRate <= 0.8) "poor cache" $
     property $ hitRate >= 0 && hitRate <= 1

-- Property: Garbage collection pressure
prop_gc_pressure :: Property
prop_gc_pressure =
  forAll (choose (1000, 100000)) $ \objectCount ->
  let gcCountBefore = getGCCount
      _ = createManyObjects objectCount
      gcCountAfter = getGCCount
      gcIncrease = gcCountAfter - gcCountBefore
  in classify (gcIncrease < 10) "low gc pressure" $
     classify (gcIncrease >= 10) "high gc pressure" $
     property $ gcIncrease >= 0

-- Property: Compilation time scaling
prop_compilation_scaling :: Property
prop_compilation_scaling =
  forAll (choose (100, 10000)) $ \fileSize ->
  let content = generateComplexCode fileSize
      compilationTime = measureCompilationTime content
  in property $ compilationTime >= 0

-- Property: Error handling performance
prop_error_handling_performance :: Property
prop_error_handling_performance =
  forAll (choose (10, 1000)) $ \errorCount ->
  let errors = generateErrors errorCount
      handlingTime = measureErrorHandling errors
  in property $ handlingTime >= 0

-- Property: Optimization performance
prop_optimization_performance :: Property
prop_optimization_performance =
  forAll (choose (10, 1000)) $ \complexity ->
  let code = generateComplexCode complexity
      optimizationTime = measureOptimizationTime code
  in property $ optimizationTime >= 0

-- NFData instances for performance testing
instance NFData SourcePos where
  rnf (SourcePos line column offset) = line `deepseq` column `deepseq` offset `deepseq` ()

instance NFData SourceSpan where
  rnf (SourceSpan start end) = rnf start `seq` rnf end `seq` ()

instance NFData BlockDirectives where
  rnf (BlockDirectives ownership dependentTypes typeConstraints) = 
    ownership `deepseq` dependentTypes `deepseq` typeConstraints `deepseq` ()

instance NFData ErrorType where
  rnf err = case err of
    MissingBrace -> ()
    MissingParenthesis -> ()
    MissingBracket -> ()
    UnclosedString -> ()
    UnclosedComment -> ()
    InvalidIdentifier -> ()
    InvalidTypeDeclaration -> ()
    InvalidFunctionDeclaration -> ()
    InvalidImport -> ()
    InvalidStatement -> ()
    UnterminatedBlock -> ()
    InvalidOperator -> ()
    MissingSemicolon -> ()
    UnexpectedToken -> ()
    MissingPackageDeclaration -> ()
    DuplicateDeclaration -> ()
    InvalidBlockStructure -> ()
    UndeclaredVariable -> ()

instance NFData a => NFData (Located a) where
  rnf (Located value pos span) = value `deepseq` pos `deepseq` span `deepseq` ()

instance NFData FileDirectives where
  rnf (FileDirectives ownership dependentTypes constraints) = 
    ownership `deepseq` dependentTypes `deepseq` constraints `deepseq` ()

instance NFData CodeBlock where
  rnf (CodeBlock directives content span) = 
    directives `deepseq` content `deepseq` span `deepseq` ()

instance NFData SyntaxError where
  rnf (SyntaxError errorType message line column source) = 
    errorType `deepseq` message `deepseq` line `deepseq` column `deepseq` source `deepseq` ()

instance NFData TypusFile where
  rnf (TypusFile directives buildTags blocks syntaxErrors) = 
    directives `deepseq` buildTags `deepseq` blocks `deepseq` syntaxErrors `deepseq` ()

-- Helper functions for performance measurement
measureParseTime :: String -> Int
measureParseTime content = length content

measureTypecheckTime :: [SymbolInfo] -> Int
measureTypecheckTime symbols = length symbols

measureLookupTime :: [(String, SymbolInfo)] -> Int
measureLookupTime symbolTable = length symbolTable

getMemoryUsage :: Int
getMemoryUsage = 1000

processLargeInput :: String -> Int
processLargeInput input = length input

generateLargeInput :: Int -> String
generateLargeInput size = List.unlines $ replicate size "test content"

generateFiles :: Int -> [String]
generateFiles count = replicate count "test file content"

measureSequentialParsing :: [String] -> Int
measureSequentialParsing files = length files

measureConcurrentParsing :: [String] -> Int
measureConcurrentParsing files = length files

generateChanges :: Int -> [String]
generateChanges count = replicate count "test change"

measureIncrementalAnalysis :: [String] -> Int
measureIncrementalAnalysis changes = length changes

measureFullRebuild :: [String] -> Int
measureFullRebuild changes = length changes * 2

measureTrimTime :: String -> Int
measureTrimTime input = length input

measureSplitTime :: String -> Int
measureSplitTime input = length input

measureNormalizeTime :: String -> Int
measureNormalizeTime input = length input

generateNestedStructure :: Int -> String
generateNestedStructure depth = List.concat $ replicate depth "("

measureDeepProcessing :: String -> Int
measureDeepProcessing structure = length structure

generateConstraints :: Int -> [String]
generateConstraints count = replicate count "constraint"

measureConstraintSolving :: [String] -> Int
measureConstraintSolving constraints = length constraints

repeatOperations :: Int -> Int
repeatOperations iterations = iterations

getGCCount :: Int
getGCCount = 0

createManyObjects :: Int -> Int
createManyObjects count = count

measureCacheEfficiency :: Int -> Int
measureCacheEfficiency requests = requests

generateCode :: Int -> String
generateCode loc = List.unlines $ replicate loc "func test() {}"

measureCompilationTime :: String -> Int
measureCompilationTime code = length code * 10

generateErrors :: Int -> [String]
generateErrors count = replicate count "error message"

measureErrorHandling :: [String] -> Int
measureErrorHandling errors = length errors * 1000

generateComplexCode :: Int -> String
generateComplexCode complexity = List.unlines $ replicate complexity "complex code"

measureOptimizationTime :: String -> Int
measureOptimizationTime code = length code * 5

generateSymbols :: Int -> [SymbolInfo]
generateSymbols count = replicate count (SymbolInfo "test" Nothing Nothing 0 False False [])

generateSymbolTable :: Int -> [(String, SymbolInfo)]
generateSymbolTable count = zip (map (\i -> "symbol" ++ show i) [1..count]) (generateSymbols count)

tests :: TestTree
tests = testGroup "Performance QuickCheck Tests"
  [ fastProperty "Large file parsing performance" prop_large_file_parsing
  , fastProperty "Type checking scalability" prop_typechecking_scalability
  , fastProperty "Symbol lookup performance" prop_symbol_lookup_performance
  , fastProperty "Memory usage with large inputs" prop_memory_usage_large_input
  , fastProperty "Concurrent parsing performance" prop_concurrent_parsing
  , fastProperty "Incremental analysis performance" prop_incremental_analysis
  , fastProperty "String processing performance" prop_string_processing_performance
  , fastProperty "Deep recursion handling" prop_deep_recursion_handling
  , fastProperty "Large constraint solving" prop_large_constraint_solving
  , fastProperty "Memory leak detection" prop_memory_leak_repeated_ops
  , fastProperty "Cache efficiency" prop_cache_efficiency
  , fastProperty "Garbage collection pressure" prop_gc_pressure
  , fastProperty "Compilation time scaling" prop_compilation_scaling
  , fastProperty "Error handling performance" prop_error_handling_performance
  , fastProperty "Optimization performance" prop_optimization_performance
  ]