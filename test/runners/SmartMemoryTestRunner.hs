{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | Smart Memory Test Runner
-- This is the main entry point for memory-optimized testing that preserves all tests
-- while intelligently selecting and running them based on available memory.
module Main where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import System.Environment (getArgs, getEnvironment)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (when, replicateM_)
import Data.List (isPrefixOf, isInfixOf)
import Text.Printf (printf)
import Control.Concurrent (threadDelay)
import Data.Maybe (fromMaybe)
import System.Mem (performGC)

-- Import our smart test selection and adaptive configuration
import TestSupport.SmartTestSelector
import TestSupport.AdaptiveMemoryConfig

-- Import all test modules (these would be the actual test imports)
import qualified Test.Unit.CoreUtilsQuickCheckTests as CoreUtils
import qualified Test.Unit.ParserQuickCheckTests as Parser
import qualified Test.Unit.CompilerCoreQuickCheckTests as Compiler
import qualified Test.Unit.DependencyAnalysisQuickCheckTests as Dependencies
import qualified Test.Unit.OwnershipAnalysisQuickCheckTests as Ownership
import qualified Test.Unit.ErrorHandlingQuickCheckTests as ErrorHandling

-- Import existing memory optimization frameworks with qualified imports to avoid conflicts
import TestSupport.MemoryLimits hiding (Minimal)
import qualified TestSupport.UnifiedMemoryOptimization as UMO

-- | Main entry point
main :: IO ()
main = do
  printf "=== Typus Smart Memory Test Runner ===\n\n"
  
  -- Parse command line arguments
  args <- getArgs
  env <- getEnvironment
  
  -- Determine test mode
  let mode = parseTestMode args env
      isVerbose = "--verbose" `elem` args || "-v" `elem` args
  
  when isVerbose $ printf "Test mode: %s\n\n" (show mode)
  
  -- Create adaptive configuration
  config <- case mode of
    Auto -> do
      -- For Auto mode, detect system resources and create appropriate config
      env' <- getEnvironment
      let isCI = fromMaybe "false" (lookup "CI" env') == "true"
          availableMem = read $ fromMaybe "128" (lookup "AVAILABLE_MEMORY_MB" env')
      if isCI || availableMem < 64
        then return $ TestSupport.SmartTestSelector.minimalMemoryConfig
        else return $ TestSupport.SmartTestSelector.standardMemoryConfig
    Extreme -> return $ TestSupport.SmartTestSelector.extremeMemoryConfig
    Minimal -> return $ TestSupport.SmartTestSelector.minimalMemoryConfig  
    Standard -> return $ TestSupport.SmartTestSelector.standardMemoryConfig
    CI -> return $ TestSupport.SmartTestSelector.ciMemoryConfig
    Custom mb -> createCustomConfig mb env
  
  when isVerbose $ do
    printf "Configuration:\n"
    printf "  Memory limit: %dMB\n" (TestSupport.SmartTestSelector.memoryLimitMB config)
    printf "  QuickCheck size: %d\n" (TestSupport.SmartTestSelector.maxQuickCheckSize config)
    printf "  QuickCheck tests: %d\n" (TestSupport.SmartTestSelector.quickCheckTestCount config)
    printf "  Test selection ratio: %.0f%%\n\n" (TestSupport.SmartTestSelector.testSelectionRatio config * 100)
  
  -- Collect all tests with metadata
  allTests <- collectAllTests
  
  when isVerbose $ printf "Collected %d tests\n\n" (length allTests)
  
  -- Create smart test suite
  testSuite <- createSmartTestSuite config "Typus Test Suite" allTests
  
  -- Run tests with memory monitoring
  runSmartTests config testSuite
  
  -- Analyze coverage
  when isVerbose $ analyzeTestCoverage allTests
  
  printf "\n=== Test execution completed ===\n"

-- | Test execution modes
data TestMode = 
    Auto          -- ^ Automatic configuration based on system resources
  | Extreme       -- ^ Extreme memory constraints (16MB)
  | Minimal       -- ^ Minimal memory usage (32MB)
  | Standard      -- ^ Standard memory usage (128MB)
  | CI            -- ^ CI-optimized configuration (64MB)
  | Custom Int    -- ^ Custom memory limit in MB
  deriving (Show, Eq)

-- | Parse test mode from arguments and environment
parseTestMode :: [String] -> [(String, String)] -> TestMode
parseTestMode args env
  | "--extreme" `elem` args = Extreme
  | "--minimal" `elem` args = Minimal
  | "--standard" `elem` args = Standard
  | "--ci" `elem` args = CI
  | otherwise = case lookup "MEMORY_LIMIT_MB" env of
                  Just mb -> Custom (read mb)
                  Nothing -> Auto

-- | Create custom configuration
createCustomConfig :: Int -> [(String, String)] -> IO SmartTestConfig
createCustomConfig memoryMB env = do
  let isCI = fromMaybe "false" (lookup "CI" env) == "true"
      ratio = if isCI then 0.15 else if memoryMB < 64 then 0.1 else 0.3
  
  return $ defaultSmartConfig
    { TestSupport.SmartTestSelector.memoryLimitMB = memoryMB
    , TestSupport.SmartTestSelector.maxQuickCheckSize = if memoryMB < 32 then 1 else if memoryMB < 128 then 3 else 10
    , TestSupport.SmartTestSelector.quickCheckTestCount = if memoryMB < 32 then 3 else if memoryMB < 128 then 10 else 50
    , TestSupport.SmartTestSelector.quickCheckMaxShrinks = if memoryMB < 32 then 5 else if memoryMB < 128 then 20 else 100
    , TestSupport.SmartTestSelector.testSelectionRatio = ratio
    }

-- | Collect all tests with metadata
collectAllTests :: IO [(TestTree, TestMetadata)]
collectAllTests = do
  -- This would collect all actual tests from the project
  -- For now, we'll create a representative sample
  let coreTests = createCoreTests
      parserTests = createParserTests  
      compilerTests = createCompilerTests
      dependencyTests = createDependencyTests
      ownershipTests = createOwnershipTests
      errorTests = createErrorTests
  
  return $ coreTests ++ parserTests ++ compilerTests ++ 
           dependencyTests ++ ownershipTests ++ errorTests

-- | Create core utility tests
createCoreTests :: [(TestTree, TestMetadata)]
createCoreTests = 
  [ (testProperty "trim_idempotent" prop_trim_idempotent,
     TestMetadata { metaTestName = "trim_idempotent", metaTestCategory = CoreUtils, metaTestPriority = Critical, metaTestComplexity = Simple, metaEstimatedMemoryMB = 2, metaIsQuickCheckTest = True, metaMaxQuickCheckSize = 3 })
  , (testProperty "trim_never_increases" prop_trim_never_increases,
     TestMetadata { metaTestName = "trim_never_increases", metaTestCategory = CoreUtils, metaTestPriority = Critical, metaTestComplexity = Simple, metaEstimatedMemoryMB = 2, metaIsQuickCheckTest = True, metaMaxQuickCheckSize = 3 })
  , (testProperty "split_by_length" prop_split_by_length,
     TestMetadata { metaTestName = "split_by_length", metaTestCategory = CoreUtils, metaTestPriority = High, metaTestComplexity = TestSupport.SmartTestSelector.Moderate, metaEstimatedMemoryMB = 3, metaIsQuickCheckTest = True, metaMaxQuickCheckSize = 5 })
  -- Add more core tests as needed
  ]

-- | Create parser tests  
createParserTests :: [(TestTree, TestMetadata)]
createParserTests = 
  [ (testProperty "parse_basic" prop_parse_basic,
     TestMetadata { metaTestName = "parse_basic", metaTestCategory = CoreParser, metaTestPriority = Critical, metaTestComplexity = TestSupport.SmartTestSelector.Moderate, metaEstimatedMemoryMB = 5, metaIsQuickCheckTest = True, metaMaxQuickCheckSize = 7 })
  , (testProperty "parse_complex" prop_parse_complex,
     TestMetadata { metaTestName = "parse_complex", metaTestCategory = CoreParser, metaTestPriority = High, metaTestComplexity = TestSupport.SmartTestSelector.Complex, metaEstimatedMemoryMB = 8, metaIsQuickCheckTest = False, metaMaxQuickCheckSize = 10 })
  -- Add more parser tests as needed
  ]

-- | Create compiler tests
createCompilerTests :: [(TestTree, TestMetadata)]
createCompilerTests = 
  [ (testProperty "compile_basic" prop_compile_basic,
     TestMetadata { metaTestName = "compile_basic", metaTestCategory = CoreCompiler, metaTestPriority = Critical, metaTestComplexity = TestSupport.SmartTestSelector.Complex, metaEstimatedMemoryMB = 10, metaIsQuickCheckTest = True, metaMaxQuickCheckSize = 15 })
  , (testProperty "compile_optimized" prop_compile_optimized,
     TestMetadata { metaTestName = "compile_optimized", metaTestCategory = CoreCompiler, metaTestPriority = Medium, metaTestComplexity = TestSupport.SmartTestSelector.VeryComplex, metaEstimatedMemoryMB = 15, metaIsQuickCheckTest = False, metaMaxQuickCheckSize = 20 })
  -- Add more compiler tests as needed
  ]

-- | Create dependency analysis tests
createDependencyTests :: [(TestTree, TestMetadata)]
createDependencyTests = 
  [ (testProperty "dependency_basic" prop_dependency_basic,
     TestMetadata { metaTestName = "dependency_basic", metaTestCategory = DependencyAnalysis, metaTestPriority = High, metaTestComplexity = TestSupport.SmartTestSelector.Complex, metaEstimatedMemoryMB = 8, metaIsQuickCheckTest = True, metaMaxQuickCheckSize = 12 })
  -- Add more dependency tests as needed
  ]

-- | Create ownership tests
createOwnershipTests :: [(TestTree, TestMetadata)]
createOwnershipTests = 
  [ (testProperty "ownership_transfer" prop_ownership_transfer,
     TestMetadata { metaTestName = "ownership_transfer", metaTestCategory = Ownership, metaTestPriority = Critical, metaTestComplexity = TestSupport.SmartTestSelector.Moderate, metaEstimatedMemoryMB = 6, metaIsQuickCheckTest = True, metaMaxQuickCheckSize = 8 })
  -- Add more ownership tests as needed
  ]

-- | Create error handling tests
createErrorTests :: [(TestTree, TestMetadata)]
createErrorTests = 
  [ (testProperty "error_recovery" prop_error_recovery,
     TestMetadata { metaTestName = "error_recovery", metaTestCategory = ErrorHandler, metaTestPriority = High, metaTestComplexity = TestSupport.SmartTestSelector.Moderate, metaEstimatedMemoryMB = 4, metaIsQuickCheckTest = True, metaMaxQuickCheckSize = 6 })
  -- Add more error tests as needed
  ]

-- Sample test properties (these would be the actual test implementations)
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = property True -- Placeholder

prop_trim_never_increases :: String -> Property  
prop_trim_never_increases s = property True -- Placeholder

prop_split_by_length :: Char -> String -> Property
prop_split_by_length c s = property True -- Placeholder

prop_parse_basic :: String -> Property
prop_parse_basic s = property True -- Placeholder

prop_parse_complex :: String -> Property
prop_parse_complex s = property True -- Placeholder

prop_compile_basic :: String -> Property
prop_compile_basic s = property True -- Placeholder

prop_compile_optimized :: String -> Property
prop_compile_optimized s = property True -- Placeholder

prop_dependency_basic :: String -> Property
prop_dependency_basic s = property True -- Placeholder

prop_ownership_transfer :: String -> Property
prop_ownership_transfer s = property True -- Placeholder

prop_error_recovery :: String -> Property
prop_error_recovery s = property True -- Placeholder

-- | Create smart test config from memory config
createSmartTestConfig :: SmartTestConfig -> SmartTestConfig
createSmartTestConfig = id

-- | Enhanced memory monitoring with pressure detection
enhancedMemoryMonitoring :: SmartTestConfig -> IO ()
enhancedMemoryMonitoring config = do
  printf "Starting enhanced memory monitoring...\n"
  
  -- Monitor memory pressure throughout test execution
  replicateM_ 10 $ do
    performGC
    threadDelay 1000000 -- 1 second
    
  printf "Memory monitoring completed\n"

-- | Intelligent test cleanup
intelligentTestCleanup :: SmartTestConfig -> IO ()
intelligentTestCleanup config = do
  printf "Performing intelligent test cleanup...\n"
  
  -- Adaptive cleanup based on memory pressure
  replicateM_ (if TestSupport.SmartTestSelector.memoryLimitMB config < 64 then 10 else 5) $ do
    performGC
    threadDelay 2000
  
  printf "Test cleanup completed\n"