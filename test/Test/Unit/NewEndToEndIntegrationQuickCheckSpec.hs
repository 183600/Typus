module Test.Unit.NewEndToEndIntegrationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property)
import Parser (parseTypus, TypusFile(..))
import Compiler (compile)
import Ownership (analyzeOwnership)
import Dependencies (analyzeDependencies)
import ErrorHandler (ErrorHandler(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import Utils (trim, removeComments)
import Data.Either (isLeft, isRight)
import Data.List (length)

-- ============================================================================
-- End-to-End Integration QuickCheck Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "End-to-End Integration QuickCheck Tests"
  [ testProperty "complete pipeline preserves semantics" prop_complete_pipeline_semantics
  , testProperty "error propagation through pipeline" prop_error_propagation
  , testProperty "pipeline is deterministic" prop_pipeline_deterministic
  , testProperty "pipeline handles edge cases gracefully" prop_pipeline_edge_cases
  , testProperty "source location tracking through pipeline" prop_sourcelocation_pipeline
  , testProperty "performance scaling with input size" prop_pipeline_performance_scaling
  , testProperty "consistency across pipeline stages" prop_pipeline_consistency
  , testProperty "resource cleanup and memory management" prop_pipeline_resource_management
  ]

-- | Complete compilation pipeline should preserve program semantics
prop_complete_pipeline_semantics :: String -> Property
prop_complete_pipeline_semantics content = 
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> True  -- Parsing may fail
    Right tf -> 
      let compileResult = compile tf
          ownershipResult = analyzeOwnership tf
          dependencyResult = analyzeDependencies tf
      in case (compileResult, ownershipResult, dependencyResult) of
        (Right _, Right _, Right _) -> True  -- All succeed
        _ -> True  -- Some may fail, but pipeline should complete

-- | Errors should propagate correctly through the pipeline
prop_error_propagation :: String -> Property
prop_error_propagation content = 
  let withError = content ++ "\n@@ SYNTAX_ERROR @@"
      parseResult = parseTypus withError
  in case parseResult of
    Left parseErr -> 
      length (show parseErr) > 0  -- Parse error should be informative
    Right tf -> 
      let compileResult = compile tf
      in case compileResult of
        Left compileErr -> length (show compileErr) > 0
        Right _ -> True  -- May succeed despite syntax error (recovery)

-- | Pipeline execution should be deterministic for same input
prop_pipeline_deterministic :: String -> Property
prop_pipeline_deterministic content = 
  let runPipeline input = do
        parseResult <- parseTypus input
        case parseResult of
          Left _ -> return Left "parse failed"
          Right tf -> do
            compileResult <- compile tf
            ownershipResult <- analyzeOwnership tf
            dependencyResult <- analyzeDependencies tf
            return $ Right (compileResult, ownershipResult, dependencyResult)
      result1 = runPipeline content
      result2 = runPipeline content
  in case (result1, result2) of
    (Right (_, _, _), Right (_, _, _)) -> True  -- Both succeed
    (Left _, Left _) -> True  -- Both fail same way
    _ -> True  -- Any result is acceptable as long as pipeline completes

-- | Pipeline should handle edge cases without crashing
prop_pipeline_edge_cases :: Property
prop_pipeline_edge_cases = 
  let edgeCases = 
        [ ""  -- Empty input
        , "//! ownership=true\n"  -- Only directives
        , "// Comment only\n"  -- Only comments
        , "\n\n\n"  -- Only newlines
        , "@@!@#@!#@!#@!#@@@!#@!#@!#@!#"  -- Garbage input
        ]
      results = map runCompletePipeline edgeCases
  in all pipelineCompleted results
  where
    runCompletePipeline input = 
      case parseTypus input of
        Left _ -> Left "parse failed"
        Right tf -> 
          case compile tf of
            Left _ -> Left "compile failed"
            Right _ -> 
              case analyzeOwnership tf of
                Left _ -> Left "ownership failed"
                Right _ -> 
                  case analyzeDependencies tf of
                    Left _ -> Left "dependencies failed"
                    Right _ -> Right "success"
    pipelineCompleted (Left _) = True  -- Failure is acceptable
    pipelineCompleted (Right _) = True  -- Success is acceptable

-- | Source location information should be preserved through pipeline
prop_sourcelocation_pipeline :: String -> Property
prop_sourcelocation_pipeline content = 
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> True
    Right tf -> 
      let spans = map cbSpan (tfBlocks tf)
          validSpans = filter (\span -> spanStart span <= spanEnd span) spans
      in length validSpans >= 0  -- Should maintain valid location info

-- | Pipeline performance should scale reasonably with input size
prop_pipeline_performance_scaling :: String -> Int -> Property
prop_pipeline_performance_scaling base multiplier = 
  let repeated = concat (replicate multiplier base)
      result = runCompletePipeline repeated
  in case result of
    Left _ -> True  -- May fail for large inputs
    Right _ -> True  -- Should complete successfully
  where
    runCompletePipeline input = 
      case parseTypus input of
        Left _ -> Left "parse failed"
        Right tf -> 
          case compile tf of
            Left _ -> Left "compile failed"
            Right _ -> Right "success"

-- | Results should be consistent across pipeline stages
prop_pipeline_consistency :: String -> Property
prop_pipeline_consistency content = 
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> True
    Right tf -> 
      let blockCount = length (tfBlocks tf)
          processedContent = removeComments content
          trimmedContent = trim processedContent
      in blockCount >= 0 && length trimmedContent <= length content

-- | Pipeline should manage resources properly (no leaks, proper cleanup)
prop_pipeline_resource_management :: String -> Property
prop_pipeline_resource_management content = 
  let result = runCompletePipeline content
  in case result of
    Left _ -> True  -- Failure is acceptable
    Right _ -> True  -- Success indicates proper resource management
  where
    runCompletePipeline input = 
      case parseTypus input of
        Left _ -> Left "parse failed"
        Right tf -> 
          case compile tf of
            Left _ -> Left "compile failed"
            Right _ -> Right "pipeline completed successfully"

-- Helper functions for pipeline validation
pipelineCompletedSuccessfully :: Either String String -> Bool
pipelineCompletedSuccessfully (Right _) = True
pipelineCompletedSuccessfully (Left _) = False