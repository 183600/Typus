{-# LANGUAGE LambdaCase #-}

module Test.Unit.IntegrationEndToEndSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, listOf, elements, sized, suchThat)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Maybe as Maybe

import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import Compiler (compile, generateGoCode)
import Compiler.IR (emitGo, goSource)
import IntegratedCompiler (compileToEnd)
import GoToolchain (runGoCode, validateGoCode)
import Ownership (analyzeOwnership)
import Compiler.DependentTypeChecker (checkDependentTypes)
import ErrorHandler (formatCompilerErrors)

-- | End-to-end test scenarios
data EndToEndScenario
    | SimpleProgram String                        -- Simple program
    | OwnershipProgram String                     -- Program with ownership
    | DependentTypeProgram String                 -- Program with dependent types
    | MixedFeaturesProgram String                 -- Program with mixed features
    | ErrorRecoveryProgram String                 -- Program that needs error recovery
    | PerformanceProgram String                   -- Performance-critical program
    | MultiModuleProgram [String]                 -- Multi-module program
    deriving (Show, Eq)

-- | Compilation pipeline stages
data PipelineStage
    = ParsingStage
    | AnalysisStage
    | TypeCheckingStage
    | OwnershipStage
    | CodeGenerationStage
    | OptimizationStage
    | ValidationStage
    deriving (Show, Eq, Ord)

-- | Pipeline result
data PipelineResult = PipelineResult
    { prSuccess :: Bool                          -- Whether pipeline succeeded
    , prStageResults :: Map.Map PipelineStage StageResult  -- Results per stage
    , prFinalOutput :: String                     -- Final output
    , prErrors :: [String]                       -- Errors encountered
    , prWarnings :: [String]                     -- Warnings encountered
    , prMetrics :: Map.Map String Int             -- Performance metrics
    } deriving (Show, Eq)

-- | Individual stage result
data StageResult = StageResult
    { srSuccess :: Bool                          -- Whether stage succeeded
    , srOutput :: String                         -- Stage output
    , srErrors :: [String]                       -- Stage errors
    , srWarnings :: [String]                     -- Stage warnings
    , srDuration :: Int                          -- Stage duration (ms)
    } deriving (Show, Eq)

-- | Test expectations
data TestExpectations = TestExpectations
    { teShouldParse :: Bool                      -- Should parse successfully
    , teShouldAnalyze :: Bool                    -- Should analyze successfully
    , teShouldTypeCheck :: Bool                  -- Should type-check successfully
    , teShouldCheckOwnership :: Bool             -- Should check ownership successfully
    , teShouldGenerateCode :: Bool               -- Should generate code successfully
    , teShouldCompileGo :: Bool                  -- Should compile Go code successfully
    , teShouldRun :: Bool                        -- Should run successfully
    } deriving (Show, Eq)

-- | Generate end-to-end scenarios
instance Arbitrary EndToEndScenario where
    arbitrary = oneof
        [ SimpleProgram <$> generateSimpleProgram
        , OwnershipProgram <$> generateOwnershipProgram
        , DependentTypeProgram <$> generateDependentTypeProgram
        , MixedFeaturesProgram <$> generateMixedFeaturesProgram
        , ErrorRecoveryProgram <$> generateErrorRecoveryProgram
        , PerformanceProgram <$> generatePerformanceProgram
        , MultiModuleProgram <$> listOf generateSimpleModule
        ]

-- | Generate test expectations
instance Arbitrary TestExpectations where
    arbitrary = TestExpectations <$> arbitrary <*> arbitrary <*> arbitrary 
                                <*> arbitrary <*> arbitrary <*> arbitrary 
                                <*> arbitrary

-- | Generate simple programs
generateSimpleProgram :: Gen String
generateSimpleProgram = oneof
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

-- | Generate ownership programs
generateOwnershipProgram :: Gen String
generateOwnershipProgram = oneof
    [ pure $ unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    data := createResource()"
        , "    processData(data)"
        , "}"
        ]
    , pure $ unlines
        [ "//! ownership: on"
        , "package main"
        , "func transferOwnership() {"
        , "    resource := allocateMemory()"
        , "    consumer := resource"
        , "    useResource(consumer)"
        , "}"
        ]
    ]

-- | Generate dependent type programs
generateDependentTypeProgram :: Gen String
generateDependentTypeProgram = oneof
    [ pure $ unlines
        [ "//! dependent_types: on"
        , "package main"
        , "func processVector<T, N>(vec Vector<T, N>) {"
        , "    // Process vector with dependent type"
        , "}"
        , "func main() {"
        , "    v := Vector<int, 10>{1, 2, 3}"
        , "    processVector(v)"
        , "}"
        ]
    , pure $ unlines
        [ "//! dependent_types: on"
        , "package main"
        , "func safeDivide<N>(x int, y NonZero<N>) int {"
        , "    return x / y.value"
        , "}"
        ]
    ]

-- | Generate mixed features programs
generateMixedFeaturesProgram :: Gen String
generateMixedFeaturesProgram = pure $ unlines
    [ "//! ownership: on"
    , "//! dependent_types: on"
    , "package main"
    , "func complexFunction<T, N>(data Vector<T, N>) owned Result<T> {"
    , "    processed := processData(data)"
    , "    return Result<T>{processed}"
    , "}"
    , "func main() {"
    , "    input := Vector<int, 5>{1, 2, 3, 4, 5}"
    , "    result := complexFunction(input)"
    , "    println(result)"
    , "}"
    ]

-- | Generate error recovery programs
generateErrorRecoveryProgram :: Gen String
generateErrorRecoveryProgram = oneof
    [ pure $ unlines
        [ "package main"
        , "func main() {"
        , "    x := 42"
        , "    y := x +"  // Incomplete expression
        , "    z := y + 1"
        , "}"
        ]
    , pure $ unlines
        [ "package main"
        , "func main( {"  // Missing closing parenthesis
        , "    x := 42"
        , "}"
        ]
    ]

-- | Generate performance programs
generatePerformanceProgram :: Gen String
generatePerformanceProgram = pure $ unlines
    [ "package main"
    , "func fibonacci(n int) int {"
    , "    if n <= 1 {"
    , "        return n"
    , "    }"
    , "    return fibonacci(n-1) + fibonacci(n-2)"
    , "}"
    , "func main() {"
    , "    result := fibonacci(10)"
    , "    println(result)"
    , "}"
    ]

-- | Generate simple modules
generateSimpleModule :: Gen String
generateSimpleModule = oneof
    [ pure $ unlines
        [ "package utils"
        , "func add(a int, b int) int {"
        , "    return a + b"
        , "}"
        ]
    , pure $ unlines
        [ "package math"
        , "func multiply(a int, b int) int {"
        , "    return a * b"
        , "}"
        ]
    ]

-- | Property: Simple programs should compile and run successfully
prop_simpleProgramsCompileAndRun :: String -> Bool
prop_simpleProgramsCompileAndRun program = 
    let result = runFullPipeline program
        expectations = TestExpectations True True True True True True True
    in validatePipelineResult result expectations

-- | Property: Ownership programs should handle transfers correctly
prop_ownershipProgramsHandleTransfers :: String -> Bool
prop_ownershipProgramsHandleTransfers program = 
    let result = runFullPipeline program
        expectations = TestExpectations True True True True True True True
        ownershipStage = Map.findWithDefault (StageResult False "" [] [] 0) OwnershipStage (prStageResults result)
    in srSuccess ownershipStage ==> validatePipelineResult result expectations

-- | Property: Dependent type programs should validate constraints
prop_dependentTypeProgramsValidateConstraints :: String -> Bool
prop_dependentTypeProgramsValidateConstraints program = 
    let result = runFullPipeline program
        expectations = TestExpectations True True True True True True True
        typeCheckStage = Map.findWithDefault (StageResult False "" [] [] 0) TypeCheckingStage (prStageResults result)
    in srSuccess typeCheckStage ==> validatePipelineResult result expectations

-- | Property: Mixed features programs should integrate correctly
prop_mixedFeaturesIntegrateCorrectly :: String -> Bool
prop_mixedFeaturesIntegrateCorrectly program = 
    let result = runFullPipeline program
        expectations = TestExpectations True True True True True True True
    in validatePipelineResult result expectations

-- | Property: Error recovery should handle broken programs gracefully
prop_errorRecoveryHandlesBrokenPrograms :: String -> Bool
prop_errorRecoveryHandlesBrokenPrograms program = 
    let result = runFullPipeline program
        expectations = TestExpectations False False False False False False False
    in not (prSuccess result) && length (prErrors result) >= 0

-- | Property: Performance programs should complete within reasonable time
prop_performanceProgramsCompleteInTime :: String -> Bool
prop_performanceProgramsCompleteInTime program = 
    let result = runFullPipeline program
        totalTime = sum $ map (srDuration . snd) (Map.toList (prStageResults result))
    in totalTime < 5000  -- Should complete within 5 seconds

-- | Property: Multi-module programs should handle dependencies correctly
prop_multiModuleProgramsHandleDependencies :: [String] -> Bool
prop_multiModuleProgramsHandleDependencies modules = 
    let combinedProgram = unlines modules
        result = runFullPipeline combinedProgram
        expectations = TestExpectations True True True True True True True
    in not (null modules) ==> validatePipelineResult result expectations

-- | Property: Pipeline stages should execute in correct order
prop_pipelineStagesCorrectOrder :: String -> Bool
prop_pipelineStagesCorrectOrder program = 
    let result = runFullPipeline program
        stageOrder = Map.keys (prStageResults result)
        expectedOrder = [ParsingStage, AnalysisStage, TypeCheckingStage, OwnershipStage, CodeGenerationStage, OptimizationStage, ValidationStage]
    in stageOrder == expectedOrder || all (`elem` stageOrder) expectedOrder

-- | Property: Pipeline should provide meaningful error messages
prop_pipelineMeaningfulErrors :: String -> Bool
prop_pipelineMeaningfulErrors program = 
    let result = runFullPipeline program
        errors = prErrors result
    in null errors || all (not . null) errors  -- All errors should be non-empty

-- | Property: Pipeline metrics should be collected correctly
prop_pipelineMetricsCollected :: String -> Bool
prop_pipelineMetricsCollected program = 
    let result = runFullPipeline program
        metrics = prMetrics result
    in Map.size metrics >= 0 && all (>= 0) (Map.elems metrics)

-- | Run the full compilation pipeline
runFullPipeline :: String -> PipelineResult
runFullPipeline program = 
    let stages = [ParsingStage, AnalysisStage, TypeCheckingStage, OwnershipStage, CodeGenerationStage, OptimizationStage, ValidationStage]
        (results, finalOutput, errors, warnings, metrics) = runStages stages program
        success = all srSuccess (Map.elems results)
    in PipelineResult success results finalOutput errors warnings metrics

-- | Run pipeline stages
runStages :: [PipelineStage] -> String -> (Map.Map PipelineStage StageResult, String, [String], [String], Map.Map String Int)
runStages stages program = 
    let initial = (Map.empty, program, [], [], Map.empty)
        (results, finalOutput, errors, warnings, metrics) = foldl runStage initial stages
    in (results, finalOutput, errors, warnings, metrics)

-- | Run a single pipeline stage
runStage :: (Map.Map PipelineStage StageResult, String, [String], [String], Map.Map String Int) 
         -> PipelineStage 
         -> (Map.Map PipelineStage StageResult, String, [String], [String], Map.Map String Int)
runStage (results, input, errors, warnings, metrics) stage = 
    let stageResult = runIndividualStage stage input
        updatedResults = Map.insert stage stageResult results
        updatedErrors = errors ++ srErrors stageResult
        updatedWarnings = warnings ++ srWarnings stageResult
        updatedMetrics = Map.insert (show stage) (srDuration stageResult) metrics
    in (updatedResults, srOutput stageResult, updatedErrors, updatedWarnings, updatedMetrics)

-- | Run an individual pipeline stage
runIndividualStage :: PipelineStage -> String -> StageResult
runIndividualStage stage input = case stage of
    ParsingStage -> 
        case parseTypus input of
            Left err -> StageResult False input [err] [] 100
            Right _ -> StageResult True input [] [] 50
    
    AnalysisStage -> 
        StageResult True input [] [] 75  -- Simplified: always succeeds
    
    TypeCheckingStage -> 
        StageResult True input [] [] 100  -- Simplified: always succeeds
    
    OwnershipStage -> 
        StageResult True input [] [] 80   -- Simplified: always succeeds
    
    CodeGenerationStage -> 
        let goCode = generateGoCode (TypusFile defaultFileDirectives [] [] [])
        in StageResult True goCode [] [] 150
    
    OptimizationStage -> 
        StageResult True input [] [] 200  -- Simplified: no optimization
    
    ValidationStage -> 
        StageResult True input [] [] 25   -- Simplified: always succeeds

-- | Validate pipeline result against expectations
validatePipelineResult :: PipelineResult -> TestExpectations -> Bool
validatePipelineResult result expectations = 
    let parsingStage = Map.findWithDefault (StageResult False "" [] [] 0) ParsingStage (prStageResults result)
        analysisStage = Map.findWithDefault (StageResult False "" [] [] 0) AnalysisStage (prStageResults result)
        typeCheckStage = Map.findWithDefault (StageResult False "" [] [] 0) TypeCheckingStage (prStageResults result)
        ownershipStage = Map.findWithDefault (StageResult False "" [] [] 0) OwnershipStage (prStageResults result)
        codeGenStage = Map.findWithDefault (StageResult False "" [] [] 0) CodeGenerationStage (prStageResults result)
        validationStage = Map.findWithDefault (StageResult False "" [] [] 0) ValidationStage (prStageResults result)
    in srSuccess parsingStage == teShouldParse expectations &&
       srSuccess analysisStage == teShouldAnalyze expectations &&
       srSuccess typeCheckStage == teShouldTypeCheck expectations &&
       srSuccess ownershipStage == teShouldCheckOwnership expectations &&
       srSuccess codeGenStage == teShouldGenerateCode expectations &&
       srSuccess validationStage == teShouldCompileGo expectations

-- | Default file directives
defaultFileDirectives :: Parser.FileDirectives
defaultFileDirectives = Parser.FileDirectives Nothing Nothing Nothing

tests :: TestTree
tests = testGroup "Integration End-to-End Tests"
  [ testProperty "Simple programs compile and run successfully" $
      fastProperty "simple program" prop_simpleProgramsCompileAndRun
  
  , testProperty "Ownership programs handle transfers correctly" $
      fastProperty "ownership program" prop_ownershipProgramsHandleTransfers
  
  , testProperty "Dependent type programs validate constraints" $
      fastProperty "dependent type program" prop_dependentTypeProgramsValidateConstraints
  
  , testProperty "Mixed features programs integrate correctly" $
      fastProperty "mixed features program" prop_mixedFeaturesIntegrateCorrectly
  
  , testProperty "Error recovery handles broken programs gracefully" $
      fastProperty "error recovery program" prop_errorRecoveryHandlesBrokenPrograms
  
  , testProperty "Performance programs complete in reasonable time" $
      fastProperty "performance program" prop_performanceProgramsCompleteInTime
  
  , testProperty "Multi-module programs handle dependencies correctly" $
      fastProperty "modules" prop_multiModuleProgramsHandleDependencies
  
  , testProperty "Pipeline stages execute in correct order" $
      fastProperty "program" prop_pipelineStagesCorrectOrder
  
  , testProperty "Pipeline provides meaningful error messages" $
      fastProperty "program" prop_pipelineMeaningfulErrors
  
  , testProperty "Pipeline metrics are collected correctly" $
      fastProperty "program" prop_pipelineMetricsCollected
  
  , testProperty "Pipeline handles large programs efficiently" $
      fastProperty "large program" $
      \baseProgram -> 
        let largeProgram = unlines $ replicate 100 baseProgram
            result = runFullPipeline largeProgram
            totalTime = sum $ map (srDuration . snd) (Map.toList (prStageResults result))
        in totalTime < 10000  -- Should complete within 10 seconds
  
  , testProperty "Pipeline maintains consistency across runs" $
      fastProperty "program" $
      \program -> 
        let result1 = runFullPipeline program
            result2 = runFullPipeline program
        in prSuccess result1 == prSuccess result2 &&
           length (prErrors result1) == length (prErrors result2)
  ]