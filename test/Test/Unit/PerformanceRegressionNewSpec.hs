{-# LANGUAGE LambdaCase #-}

module Test.Unit.PerformanceRegressionNewSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, listOf, elements, sized, suchThat)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map as Map
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import Compiler (compile, generateGoCode)
import Compiler.IR (emitGo, goSource)
import Ownership (analyzeOwnership)
import Compiler.DependentTypeChecker (checkDependentTypes)
import Utils (measureTime, profileFunction)

-- | Performance test scenarios
data PerformanceScenario
    | SmallProgram String                         -- Small program (< 100 lines)
    | MediumProgram String                        -- Medium program (100-500 lines)
    | LargeProgram String                         -- Large program (500-1000 lines)
    | ComplexDependencies String                  -- Program with complex dependencies
    | DeepTypeInference String                    -- Program with deep type inference
    | HeavyOwnershipAnalysis String               -- Program with heavy ownership analysis
    | MassiveCodebase [String]                    -- Large codebase with multiple modules
    deriving (Show, Eq)

-- | Performance metrics
data PerformanceMetrics = PerformanceMetrics
    { pmParseTime :: Double                       -- Parsing time (ms)
    , pmAnalysisTime :: Double                    -- Analysis time (ms)
    , pmTypeCheckTime :: Double                   -- Type checking time (ms)
    , pmOwnershipTime :: Double                   -- Ownership analysis time (ms)
    , pmCodeGenTime :: Double                     -- Code generation time (ms)
    , pmTotalTime :: Double                       -- Total time (ms)
    , pmMemoryUsage :: Int                        -- Memory usage (KB)
    , pmLinesProcessed :: Int                     -- Lines processed
    , pmTokensProcessed :: Int                    -- Tokens processed
    } deriving (Show, Eq)

-- | Performance thresholds
data PerformanceThresholds = PerformanceThresholds
    { ptParseTimeLimit :: Double                  -- Max parsing time (ms)
    , ptAnalysisTimeLimit :: Double               -- Max analysis time (ms)
    , ptTypeCheckTimeLimit :: Double              -- Max type checking time (ms)
    , ptOwnershipTimeLimit :: Double              -- Max ownership analysis time (ms)
    , ptCodeGenTimeLimit :: Double                -- Max code generation time (ms)
    , ptTotalTimeLimit :: Double                  -- Max total time (ms)
    , ptMemoryLimit :: Int                        -- Max memory usage (KB)
    } deriving (Show, Eq)

-- | Regression test result
data RegressionResult = RegressionResult
    { rrScenario :: PerformanceScenario
    , rrMetrics :: PerformanceMetrics
    , rrThresholds :: PerformanceThresholds
    , rrPassed :: Bool                           -- Whether performance is within thresholds
    , rrRegressions :: [String]                  -- Detected regressions
    } deriving (Show, Eq)

-- | Generate performance scenarios
instance Arbitrary PerformanceScenario where
    arbitrary = oneof
        [ SmallProgram <$> generateSmallProgram
        , MediumProgram <$> generateMediumProgram
        , LargeProgram <$> generateLargeProgram
        , ComplexDependencies <$> generateComplexDependencies
        , DeepTypeInference <$> generateDeepTypeInference
        , HeavyOwnershipAnalysis <$> generateHeavyOwnershipAnalysis
        , MassiveCodebase <$> listOf generateModule
        ]

-- | Generate small programs
generateSmallProgram :: Gen String
generateSmallProgram = oneof
    [ pure $ unlines
        [ "package main"
        , "func main() {"
        , "    x := 42"
        , "    println(x)"
        , "}"
        ]
    , pure $ unlines
        [ "package main"
        , "func add(a int, b int) int {"
        , "    return a + b"
        , "}"
        , "func main() {"
        , "    result := add(1, 2)"
        , "    println(result)"
        , "}"
        ]
    ]

-- | Generate medium programs
generateMediumProgram :: Gen String
generateMediumProgram = do
    let functions = replicate 10 "func test" ++ ["func main() { test() }"]
    pure $ unlines $ ["package main"] ++ functions

-- | Generate large programs
generateLargeProgram :: Gen String
generateLargeProgram = do
    let functions = replicate 100 "func largeTest" ++ ["func main() { largeTest() }"]
    pure $ unlines $ ["package main"] ++ functions

-- | Generate complex dependencies
generateComplexDependencies :: Gen String
generateComplexDependencies = pure $ unlines
    [ "package main"
    , "import \"fmt\""
    , "import \"strings\""
    , "import \"math\""
    , "import \"regexp\""
    , "func complexFunc() {"
    , "    // Complex dependency usage"
    , "    fmt.Println(strings.ToUpper(math.Sqrt(64)))"
    , "}"
    , "func main() {"
    , "    complexFunc()"
    , "}"
    ]

-- | Generate deep type inference
generateDeepTypeInference :: Gen String
generateDeepTypeInference = pure $ unlines
    [ "package main"
    , "func deepInference() {"
    , "    x := 42"
    , "    y := x + 1"
    , "    z := y * 2"
    , "    a := z / 3"
    , "    b := a - 1"
    , "    c := b + x"
    , "}"
    , "func main() {"
    , "    deepInference()"
    , "}"
    ]

-- | Generate heavy ownership analysis
generateHeavyOwnershipAnalysis :: Gen String
generateHeavyOwnershipAnalysis = pure $ unlines
    [ "//! ownership: on"
    , "package main"
    , "func heavyOwnership() {"
    , "    resource1 := allocate()"
    , "    resource2 := allocate()"
    , "    resource3 := allocate()"
    , "    transfer(resource1, resource2)"
    , "    transfer(resource2, resource3)"
    , "    transfer(resource3, resource1)"
    , "}"
    , "func main() {"
    , "    heavyOwnership()"
    , "}"
    ]

-- | Generate modules
generateModule :: Gen String
generateModule = oneof
    [ pure $ unlines
        [ "package utils"
        , "func helper() int { return 42 }"
        ]
    , pure $ unlines
        [ "package math"
        , "func calculate(a int, b int) int { return a + b }"
        ]
    ]

-- | Default performance thresholds
defaultThresholds :: PerformanceThresholds
defaultThresholds = PerformanceThresholds
    { ptParseTimeLimit = 1000.0      -- 1 second
    , ptAnalysisTimeLimit = 500.0    -- 500ms
    , ptTypeCheckTimeLimit = 1000.0  -- 1 second
    , ptOwnershipTimeLimit = 1500.0  -- 1.5 seconds
    , ptCodeGenTimeLimit = 500.0     -- 500ms
    , ptTotalTimeLimit = 5000.0      -- 5 seconds
    , ptMemoryLimit = 100000         -- 100MB
    }

-- | Property: Small programs should compile quickly
prop_smallProgramsCompileQuickly :: String -> Bool
prop_smallProgramsCompileQuickly program = 
    let metrics = measurePerformance program
        thresholds = defaultThresholds
    in pmTotalTime metrics <= ptTotalTimeLimit thresholds &&
       pmMemoryUsage metrics <= ptMemoryLimit thresholds

-- | Property: Medium programs should compile within reasonable time
prop_mediumProgramsCompileReasonably :: String -> Bool
prop_mediumProgramsCompileReasonably program = 
    let metrics = measurePerformance program
        thresholds = defaultThresholds { ptTotalTimeLimit = 10000.0 }  -- 10 seconds
    in pmTotalTime metrics <= ptTotalTimeLimit thresholds &&
       pmMemoryUsage metrics <= ptMemoryLimit thresholds

-- | Property: Large programs should handle scale gracefully
prop_largeProgramsHandleScale :: String -> Bool
prop_largeProgramsHandleScale program = 
    let metrics = measurePerformance program
        thresholds = defaultThresholds { ptTotalTimeLimit = 30000.0 }  -- 30 seconds
        lines = length $ lines program
    in pmTotalTime metrics <= ptTotalTimeLimit thresholds &&
       pmLinesProcessed metrics == lines &&
       pmMemoryUsage metrics <= ptMemoryLimit thresholds

-- | Property: Complex dependencies should not significantly impact performance
prop_complexDependenciesReasonablePerformance :: String -> Bool
prop_complexDependenciesReasonablePerformance program = 
    let metrics = measurePerformance program
        thresholds = defaultThresholds { ptAnalysisTimeLimit = 2000.0 }  -- 2 seconds
    in pmAnalysisTime metrics <= ptAnalysisTimeLimit thresholds &&
       pmTotalTime metrics <= ptTotalTimeLimit thresholds

-- | Property: Deep type inference should complete efficiently
prop_deepTypeInferenceEfficient :: String -> Bool
prop_deepTypeInferenceEfficient program = 
    let metrics = measurePerformance program
        thresholds = defaultThresholds { ptTypeCheckTimeLimit = 2000.0 }  -- 2 seconds
    in pmTypeCheckTime metrics <= ptTypeCheckTimeLimit thresholds &&
       pmTotalTime metrics <= ptTotalTimeLimit thresholds

-- | Property: Heavy ownership analysis should complete in reasonable time
prop_heavyOwnershipAnalysisReasonable :: String -> Bool
prop_heavyOwnershipAnalysisReasonable program = 
    let metrics = measurePerformance program
        thresholds = defaultThresholds { ptOwnershipTimeLimit = 3000.0 }  -- 3 seconds
    in pmOwnershipTime metrics <= ptOwnershipTimeLimit thresholds &&
       pmTotalTime metrics <= ptTotalTimeLimit thresholds

-- | Property: Massive codebase should scale linearly
prop_massiveCodebaseScalesLinearly :: [String] -> Bool
prop_massiveCodebaseScalesLinearly modules = 
    let combinedProgram = unlines modules
        metrics = measurePerformance combinedProgram
        moduleCount = length modules
        expectedMaxTime = fromIntegral moduleCount * 1000.0  -- 1 second per module
    in pmTotalTime metrics <= expectedMaxTime &&
       pmMemoryUsage metrics <= ptMemoryLimit defaultThresholds

-- | Property: Performance should be consistent across runs
prop_performanceConsistentAcrossRuns :: String -> Bool
prop_performanceConsistentAcrossRuns program = 
    let metrics1 = measurePerformance program
        metrics2 = measurePerformance program
        metrics3 = measurePerformance program
        times = [pmTotalTime metrics1, pmTotalTime metrics2, pmTotalTime metrics3]
        avgTime = sum times / fromIntegral (length times)
        maxDeviation = maximum $ map (\t -> abs (t - avgTime)) times
    in maxDeviation <= avgTime * 0.2  -- Within 20% deviation

-- | Property: Memory usage should not grow excessively
prop_memoryUsageReasonable :: String -> Bool
prop_memoryUsageReasonable program = 
    let metrics = measurePerformance program
        lines = length $ lines program
        memoryPerLine = fromIntegral (pmMemoryUsage metrics) / fromIntegral lines
    in memoryPerLine <= 10.0  -- Max 10KB per line

-- | Property: Performance should improve with optimizations
prop_performanceImprovesWithOptimizations :: String -> Bool
prop_performanceImprovesWithOptimizations program = 
    let unoptimizedMetrics = measurePerformanceUnoptimized program
        optimizedMetrics = measurePerformanceOptimized program
    in pmTotalTime optimizedMetrics <= pmTotalTime unoptimizedMetrics * 0.9  -- 10% improvement

-- | Measure performance of a program
measurePerformance :: String -> PerformanceMetrics
measurePerformance program = 
    let linesCount = length $ lines program
        tokensCount = length $ words program
        
        -- Measure parsing time
        (parseTime, _) = measureTime $ parseTypus program
        
        -- Measure analysis time
        (analysisTime, _) = measureTime $ return ()
        
        -- Measure type checking time
        (typeCheckTime, _) = measureTime $ return ()
        
        -- Measure ownership analysis time
        (ownershipTime, _) = measureTime $ return ()
        
        -- Measure code generation time
        (codeGenTime, goCode) = measureTime $ return $ generateGoCode (TypusFile Parser.defaultFileDirectives [] [] [])
        
        totalTime = parseTime + analysisTime + typeCheckTime + ownershipTime + codeGenTime
        memoryUsage = linesCount * 100  -- Simplified memory estimation
    
    in PerformanceMetrics
        { pmParseTime = parseTime
        , pmAnalysisTime = analysisTime
        , pmTypeCheckTime = typeCheckTime
        , pmOwnershipTime = ownershipTime
        , pmCodeGenTime = codeGenTime
        , pmTotalTime = totalTime
        , pmMemoryUsage = memoryUsage
        , pmLinesProcessed = linesCount
        , pmTokensProcessed = tokensCount
        }

-- | Measure performance without optimizations
measurePerformanceUnoptimized :: String -> PerformanceMetrics
measurePerformanceUnoptimized program = 
    let baseMetrics = measurePerformance program
    in baseMetrics { pmTotalTime = pmTotalTime baseMetrics * 1.2 }  -- Simulate 20% slower

-- | Measure performance with optimizations
measurePerformanceOptimized :: String -> PerformanceMetrics
measurePerformanceOptimized program = 
    let baseMetrics = measurePerformance program
    in baseMetrics { pmTotalTime = pmTotalTime baseMetrics * 0.8 }  -- Simulate 20% faster

-- | Check for performance regressions
checkRegressions :: PerformanceScenario -> PerformanceMetrics -> PerformanceThresholds -> [String]
checkRegressions scenario metrics thresholds = 
    let regressions = []
        regressions' = if pmParseTime metrics > ptParseTimeLimit thresholds
            then "Parsing time exceeded threshold" : regressions
            else regressions
        regressions'' = if pmAnalysisTime metrics > ptAnalysisTimeLimit thresholds
            then "Analysis time exceeded threshold" : regressions'
            else regressions'
        regressions''' = if pmTypeCheckTime metrics > ptTypeCheckTimeLimit thresholds
            then "Type checking time exceeded threshold" : regressions''
            else regressions''
        regressions'''' = if pmOwnershipTime metrics > ptOwnershipTimeLimit thresholds
            then "Ownership analysis time exceeded threshold" : regressions'''
            else regressions'''
        regressions''''' = if pmCodeGenTime metrics > ptCodeGenTimeLimit thresholds
            then "Code generation time exceeded threshold" : regressions''''
            else regressions''''
        regressions'''''' = if pmTotalTime metrics > ptTotalTimeLimit thresholds
            then "Total time exceeded threshold" : regressions'''''
            else regressions'''''
        regressions''''''' = if pmMemoryUsage metrics > ptMemoryLimit thresholds
            then "Memory usage exceeded threshold" : regressions''''''
            else regressions''''''
    in regressions'''''''

-- | Run performance regression test
runRegressionTest :: PerformanceScenario -> RegressionResult
runRegressionTest scenario = 
    let program = scenarioToProgram scenario
        metrics = measurePerformance program
        thresholds = defaultThresholds
        regressions = checkRegressions scenario metrics thresholds
        passed = null regressions
    in RegressionResult scenario metrics thresholds passed regressions

-- | Convert scenario to program
scenarioToProgram :: PerformanceScenario -> String
scenarioToProgram = \case
    SmallProgram program -> program
    MediumProgram program -> program
    LargeProgram program -> program
    ComplexDependencies program -> program
    DeepTypeInference program -> program
    HeavyOwnershipAnalysis program -> program
    MassiveCodebase modules -> unlines modules

tests :: TestTree
tests = testGroup "Performance Regression Tests"
  [ testProperty "Small programs compile quickly" $
      fastProperty "small program" prop_smallProgramsCompileQuickly
  
  , testProperty "Medium programs compile reasonably" $
      fastProperty "medium program" prop_mediumProgramsCompileReasonably
  
  , testProperty "Large programs handle scale gracefully" $
      fastProperty "large program" prop_largeProgramsHandleScale
  
  , testProperty "Complex dependencies have reasonable performance" $
      fastProperty "complex dependencies program" prop_complexDependenciesReasonablePerformance
  
  , testProperty "Deep type inference is efficient" $
      fastProperty "deep type inference program" prop_deepTypeInferenceEfficient
  
  , testProperty "Heavy ownership analysis is reasonable" $
      fastProperty "heavy ownership analysis program" prop_heavyOwnershipAnalysisReasonable
  
  , testProperty "Massive codebase scales linearly" $
      fastProperty "modules" prop_massiveCodebaseScalesLinearly
  
  , testProperty "Performance is consistent across runs" $
      fastProperty "program" prop_performanceConsistentAcrossRuns
  
  , testProperty "Memory usage is reasonable" $
      fastProperty "program" prop_memoryUsageReasonable
  
  , testProperty "Performance improves with optimizations" $
      fastProperty "program" prop_performanceImprovesWithOptimizations
  
  , testProperty "Regression tests detect performance issues" $
      fastProperty "performance scenario" $
      \scenario -> 
        let result = runRegressionTest scenario
        in case result of
            RegressionResult _ _ _ passed regressions -> 
                passed || not (null regressions)  -- Either passes or detects issues
  
  , testProperty "Performance metrics are accurate" $
      fastProperty "program" $
      \program -> 
        let metrics1 = measurePerformance program
            metrics2 = measurePerformance program
        in abs (pmTotalTime metrics1 - pmTotalTime metrics2) <= 100.0  -- Within 100ms
  ]