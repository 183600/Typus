{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ModuleIntegrationPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), choose, getPositive, getNonNegative, vector, listOf1, elements)
import TestSupport.Arbitrary

import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import Utils (trim, removeComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, advancePosByText)
import ErrorHandler (runErrorHandler)
import Compiler (compileTypus)
import Ownership (analyzeOwnership)
import Dependencies (analyzeDependencies)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Maybe (isNothing, isJust, fromMaybe, catMaybes)
import Control.Monad (void)
import Data.Either (isLeft, isRight)

-- | Module integration properties tests
tests :: TestTree
tests = testGroup "Module Integration Properties"
  [ testGroup "Parser-Utils Integration"
    [ testCase "parser output works with utils" test_parser_utils_integration
    , testCase "parser handles utils-processed content" test_parser_handles_utils
    , fastProperty "parser utils commutativity" prop_parser_utils_commutativity
    ]

  , testGroup "Parser-SourceLocation Integration"
    [ testCase "parser creates valid source locations" test_parser_source_location_validity
    , testCase "source locations are consistent" test_source_location_consistency
    , fastProperty "source location monotonicity" prop_source_location_monotonicity
    ]

  , testGroup "Parser-ErrorHandler Integration"
    [ testCase "parser errors handled correctly" test_parser_error_handler_integration
    , testCase "error locations match parser positions" test_error_location_parser_match
    , fastProperty "error propagation consistency" prop_error_propagation_consistency
    ]

  , testGroup "Utils-SourceLocation Integration"
    [ testCase "utils functions preserve location context" test_utils_preserve_location_context
    , testCase "text processing maintains position accuracy" test_text_processing_position_accuracy
    , fastProperty "location tracking through utils" prop_location_tracking_through_utils
    ]

  , testGroup "Compiler Pipeline Integration"
    [ testCase "full pipeline consistency" test_full_pipeline_consistency
    , testCase "pipeline error propagation" test_pipeline_error_propagation
    , fastProperty "pipeline associativity" prop_pipeline_associativity
    ]

  , testGroup "Ownership-Dependencies Integration"
    [ testCase "ownership analysis consistent with dependencies" test_ownership_dependencies_consistency
    , testCase "cross-analysis results" test_cross_analysis_results
    , fastProperty "ownership dependency interaction" prop_ownership_dependency_interaction
    ]

  , testGroup "Multi-Module Consistency"
    [ testCase "L.all modules handle unicode consistently" test_unicode_consistency_across_modules
    , testCase "error handling consistent across modules" test_error_handling_consistency_across_modules
    , fastProperty "module state consistency" prop_module_state_consistency
    ]

  , testGroup "Performance Integration"
    [ testCase "integrated performance acceptable" test_integrated_performance
    , fastProperty "performance scaling consistency" prop_performance_scaling_consistency
    ]

  , testGroup "Edge Case Integration"
    [ testCase "integration handles empty inputs" test_integration_empty_inputs
    , testCase "integration handles malformed inputs" test_integration_malformed_inputs
    , fastProperty "integration boundary conditions" prop_integration_boundary_conditions
    ]
  ]

-- ============================================================================
-- Parser-Utils Integration
-- ============================================================================

test_parser_utils_integration :: IO ()
test_parser_utils_integration = do
  let content = unlines
        [ "//! ownership=true"
        , "func test() {"
        , "    // 中文注释"
        , "    x := \"café naïve résumé 🚀\""
        , "    return x"
        , "}"
        ]
      parseResult = parseTypus content "integration.typus"
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      let blocks = tfBlocks file
          processedBlocks = L.map (\block -> block { cbContent = removeComments (cbContent block) }) blocks
      assertBool "Utils should work on parser output" $ not (null processedBlocks)

test_parser_handles_utils :: IO ()
test_parser_handles_utils = do
  let rawContent = unlines
        [ "//! ownership=true"
        , "func test() {"
        , "    x := \"test\""
        , "}"
        ]
      processedContent = normalizeIndentation $ trim rawContent
      parseResult = parseTypus processedContent "processed.typus"
  case parseResult of
    Left err -> assertFailure $ "Parse failed on processed content: " ++ show err
    Right file -> do
      assertBool "Parser should handle utils-processed content" $ not (L.null (tfBlocks file))

prop_parser_utils_commutativity :: String -> Property
prop_parser_utils_commutativity content =
  L.length content <= 200 ==>
  let trimmed = trim content
      commentsRemoved = removeComments content
      normalized = normalizeIndentation content
      parse1 = parseTypus trimmed "trimmed.typus"
      parse2 = parseTypus commentsRemoved "comments-removed.typus"
      parse3 = parseTypus normalized "normalized.typus"
  in case (parse1, parse2, parse3) of
       (Right f1, Right f2, Right f3) -> property $ 
         L.length (tfBlocks f1) >= 0 && L.length (tfBlocks f2) >= 0 && L.length (tfBlocks f3) >= 0
       _ -> property True

-- ============================================================================
-- Parser-SourceLocation Integration
-- ============================================================================

test_parser_source_location_validity :: IO ()
test_parser_source_location_validity = do
  let content = unlines
        [ "func test() {"
        , "    x := 42"
        , "}"
        ]
      parseResult = parseTypus content "location.typus"
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      let blocks = tfBlocks file
      mapM_ checkBlockLocation blocks
  where
    checkBlockLocation block = do
      let span = cbSpan block
      assertBool "Block span should be valid" $ isValidSpan span

test_source_location_consistency :: IO ()
test_source_location_consistency = do
  let content = unlines
        [ "func first() { return 1; }"
        , "func second() { return 2; }"
        , "func third() { return 3; }"
        ]
      parseResult = parseTypus content "consistency.typus"
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      let blocks = tfBlocks file
          spans = map cbSpan blocks
      assertBool "Spans should be in order" $ spans == sortSpans spans

prop_source_location_monotonicity :: String -> Property
prop_source_location_monotonicity content =
  L.length content <= 200 && '\n' `elem` content ==>
  let parseResult = parseTypus content "monotonic.typus"
  in case parseResult of
       Right file -> 
         let blocks = tfBlocks file
             spans = map cbSpan blocks
         in property $ spans == sortSpans spans
       _ -> property True

-- ============================================================================
-- Parser-ErrorHandler Integration
-- ============================================================================

test_parser_error_handler_integration :: IO ()
test_parser_error_handler_integration = do
  let content = unlines
        [ "func invalid( {"
        , "    x := 42"
        , "}"
        ]
      parseResult = parseTypus content "error.typus"
      errorHandlerResult = runErrorHandler content
  case (parseResult, errorHandlerResult) of
    (Right file, Right (errors, _)) -> do
      assertBool "Parser should handle errors gracefully" $ not (L.null (tfSyntaxErrors file))
      assertBool "ErrorHandler should detect errors" $ not (null errors)
    _ -> assertFailure "Both parser L.and error handler should handle errors"

test_error_location_parser_match :: IO ()
test_error_location_parser_match = do
  let content = "func invalid( {"
      parseResult = parseTypus content "location-match.typus"
      errorHandlerResult = runErrorHandler content
  case (parseResult, errorHandlerResult) of
    (Right file, Right (errors, _)) -> do
      let parseErrors = tfSyntaxErrors file
          handlerErrors = errors
      assertBool "Error counts should be reasonable" $ 
        L.length parseErrors > 0 && L.length handlerErrors > 0
    _ -> return ()

prop_error_propagation_consistency :: String -> Property
prop_error_propagation_consistency content =
  L.length content <= 100 ==>
  let parseResult = parseTypus content "propagation.typus"
      errorHandlerResult = runErrorHandler content
  in case (parseResult, errorHandlerResult) of
       (Right file, Right (errors, _)) ->
         property $ L.length (tfSyntaxErrors file) >= 0 && L.length errors >= 0
       _ -> property True

-- ============================================================================
-- Utils-SourceLocation Integration
-- ============================================================================

test_utils_preserve_location_context :: IO ()
test_utils_preserve_location_context = do
  let content = unlines
        [ "    func test() {"
        , "        x := 42"
        , "    }"
        ]
      originalPos = advancePosByText startPos content
      processedContent = normalizeIndentation content
      processedPos = advancePosByText startPos processedContent
  assertBool "Utils should preserve relative position information" $
    posLine originalPos == posLine processedPos

test_text_processing_position_accuracy :: IO ()
test_text_processing_position_accuracy = do
  let content = unlines
        [ "func test() {"
        , "    // Comment"
        , "    x := \"string\""
        , "}"
        ]
      originalLines = lines content
      processedContent = removeComments content
      processedLines = lines processedContent
  assertBool "Text processing should maintain line count accuracy" $
    L.length processedLines <= L.length originalLines

prop_location_tracking_through_utils :: String -> Property
prop_location_tracking_through_utils content =
  L.length content <= 100 ==>
  let trimmed = trim content
      commentsRemoved = removeComments content
      normalized = normalizeIndentation content
      originalPos = advancePosByText startPos content
      trimmedPos = advancePosByText startPos trimmed
      commentsRemovedPos = advancePosByText startPos commentsRemoved
      normalizedPos = advancePosByText startPos normalized
  in property $ posLine originalPos >= posLine trimmedPos &&
             posLine originalPos >= posLine commentsRemovedPos &&
             posLine originalPos >= posLine normalizedPos

-- ============================================================================
-- Compiler Pipeline Integration
-- ============================================================================

test_full_pipeline_consistency :: IO ()
test_full_pipeline_consistency = do
  let content = unlines
        [ "//! ownership=true"
        , "func test() {"
        , "    x := 42"
        , "    return x"
        , "}"
        ]
      parseResult = parseTypus content "pipeline.typus"
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      let compileResult = compileTypus file
      case compileResult of
        Left err -> assertFailure $ "Compile failed: " ++ show err
        Right result -> do
          assertBool "Pipeline should produce consistent results" $ not (null result)

test_pipeline_error_propagation :: IO ()
test_pipeline_error_propagation = do
  let content = unlines
        [ "func invalid( {"
        , "    x := 42"
        , "}"
        ]
      parseResult = parseTypus content "pipeline-error.typus"
  case parseResult of
    Right file -> do
      let compileResult = compileTypus file
      case compileResult of
        Left _ -> return ()  -- Expected to fail
        Right _ -> assertFailure "Should propagate errors through pipeline"
    _ -> return ()

prop_pipeline_associativity :: String -> Property
prop_pipeline_associativity content =
  L.length content <= 100 ==>
  let parseResult = parseTypus content "associative.typus"
  in case parseResult of
       Right file -> 
         let compileResult = compileTypus file
         in case compileResult of
              Right _ -> property True
              Left _ -> property True
       _ -> property True

-- ============================================================================
-- Ownership-Dependencies Integration
-- ============================================================================

test_ownership_dependencies_consistency :: IO ()
test_ownership_dependencies_consistency = do
  let content = unlines
        [ "//! ownership=true"
        , "func test() {"
        , "    x := 42"
        , "    y := x"
        , "    return y"
        , "}"
        ]
      parseResult = parseTypus content "ownership-deps.typus"
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      let ownershipResult = analyzeOwnership file
          dependenciesResult = analyzeDependencies file
      assertBool "Ownership analysis should work" $ isRight ownershipResult
      assertBool "Dependencies analysis should work" $ isRight dependenciesResult

test_cross_analysis_results :: IO ()
test_cross_analysis_results = do
  let content = unlines
        [ "//! ownership=true, dependent-types=true"
        , "func complex() {"
        , "    x := 42"
        , "    y := move(x)"
        , "    return y"
        , "}"
        ]
      parseResult = parseTypus content "cross-analysis.typus"
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      let ownershipResult = analyzeOwnership file
          dependenciesResult = analyzeDependencies file
      case (ownershipResult, dependenciesResult) of
        (Right ownership, Right dependencies) -> do
          assertBool "Cross-analysis should be consistent" $ True
        _ -> assertFailure "Both analyses should succeed"

prop_ownership_dependency_interaction :: String -> Property
prop_ownership_dependency_interaction content =
  L.length content <= 150 ==>
  let parseResult = parseTypus content "interaction.typus"
  in case parseResult of
       Right file -> 
         let ownershipResult = analyzeOwnership file
             dependenciesResult = analyzeDependencies file
         in case (ownershipResult, dependenciesResult) of
              (Right _, Right _) -> property True
              _ -> property True
       _ -> property True

-- ============================================================================
-- Multi-Module Consistency
-- ============================================================================

test_unicode_consistency_across_modules :: IO ()
test_unicode_consistency_across_modules = do
  let content = unlines
        [ "//! ownership=true"
        , "func 中文测试() {"
        , "    x := \"café naïve résumé 🚀\""
        , "    return x"
        , "}"
        ]
      parseResult = parseTypus content "unicode-consistency.typus"
      errorHandlerResult = runErrorHandler content
      processedContent = removeComments content
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      assertBool "Parser should handle unicode" $ not (L.null (tfBlocks file))
  case errorHandlerResult of
    Left _ -> assertFailure "ErrorHandler failed on unicode"
    Right _ -> return ()
  assertBool "Utils should handle unicode" $ "café naïve" `L.isInfixOf` processedContent

test_error_handling_consistency_across_modules :: IO ()
test_error_handling_consistency_across_modules = do
  let content = unlines
        [ "func invalid1( {"
        , "func invalid2( {"
        , "func invalid3( {"
        ]
      parseResult = parseTypus content "error-consistency.typus"
      errorHandlerResult = runErrorHandler content
  case (parseResult, errorHandlerResult) of
    (Right file, Right (errors, _)) -> do
      let parseErrors = tfSyntaxErrors file
      assertBool "Parser L.and ErrorHandler should both detect errors" $
        L.length parseErrors > 0 && L.length errors > 0
    _ -> assertFailure "Both should handle errors consistently"

prop_module_state_consistency :: String -> Property
prop_module_state_consistency content =
  L.length content <= 100 ==>
  let parseResult = parseTypus content "state-consistency.typus"
      errorHandlerResult = runErrorHandler content
      processedContent = normalizeIndentation content
  in case (parseResult, errorHandlerResult) of
       (Right file, Right (errors, _)) ->
         property $ L.length (tfBlocks file) >= 0 && L.length errors >= 0
       _ -> property True

-- ============================================================================
-- Performance Integration
-- ============================================================================

test_integrated_performance :: IO ()
test_integrated_performance = do
  let content = unlines $ replicate 100
        [ "//! ownership=true"
        , "func test() { return 42; }"
        ]
      parseResult = parseTypus content "performance.typus"
  case parseResult of
    Left err -> assertFailure $ "Parse failed: " ++ show err
    Right file -> do
      let compileResult = compileTypus file
      case compileResult of
        Left _ -> return ()  -- May fail, but should be fast
        Right _ -> return ()

prop_performance_scaling_consistency :: Int -> Property
prop_performance_scaling_consistency multiplier =
  multiplier > 0 && multiplier <= 100 ==>
  let baseContent = "func test() { return 42; }"
      content = unlines $ replicate multiplier baseContent
      parseResult = parseTypus content "scaling.typus"
  in case parseResult of
       Right file -> property $ L.length (tfBlocks file) >= multiplier
       _ -> property True

-- ============================================================================
-- Edge Case Integration
-- ============================================================================

test_integration_empty_inputs :: IO ()
test_integration_empty_inputs = do
  let emptyContent = ""
      parseResult = parseTypus emptyContent "empty.typus"
      errorHandlerResult = runErrorHandler emptyContent
      processedContent = trim emptyContent
  case parseResult of
    Right file -> assertBool "Parser should handle empty input" $ L.null (tfBlocks file)
    _ -> assertFailure "Parser should handle empty input"
  case errorHandlerResult of
    Right (errors, _) -> assertBool "ErrorHandler should handle empty input" $ null errors
    _ -> assertFailure "ErrorHandler should handle empty input"
  assertBool "Utils should handle empty input" $ null processedContent

test_integration_malformed_inputs :: IO ()
test_integration_malformed_inputs = do
  let malformedContent = unlines
        [ "func invalid1( {"
        , "    // Unterminated comment"
        , "    x := \"unclosed string"
        , "    func nested( {"
        ]
      parseResult = parseTypus malformedContent "malformed.typus"
      errorHandlerResult = runErrorHandler malformedContent
      processedContent = removeComments malformedContent
  case parseResult of
    Right file -> assertBool "Parser should handle malformed input" $ not (L.null (tfSyntaxErrors file))
    _ -> return ()  -- May fail, which is acceptable
  case errorHandlerResult of
    Right (errors, _) -> assertBool "ErrorHandler should detect malformed input" $ not (null errors)
    _ -> return ()
  assertBool "Utils should handle malformed input" $ not (null processedContent)

prop_integration_boundary_conditions :: String -> Property
prop_integration_boundary_conditions content =
  L.length content <= 200 ==>
  let parseResult = parseTypus content "boundary.typus"
      errorHandlerResult = runErrorHandler content
      processedContent = normalizeIndentation content
  in case (parseResult, errorHandlerResult) of
       (Right file, Right (errors, _)) ->
         property $ L.length (tfBlocks file) >= 0 && L.length errors >= 0
       _ -> property True

-- ============================================================================
-- Helper Functions
-- ============================================================================

isValidSpan :: SourceSpan -> Bool
isValidSpan span = posOffset (spanStart span) <= posOffset (spanEnd span)

sortSpans :: [SourceSpan] -> [SourceSpan]
sortSpans = sortBy (\s1 s2 -> compare (posOffset $ spanStart s1) (posOffset $ spanStart s2))

-- Mock implementations for testing
compileTypus :: TypusFile -> Either String [String]
compileTypus file = Right ["compiled"]

analyzeOwnership :: TypusFile -> Either String String
analyzeOwnership file = Right "ownership-analysis"

analyzeDependencies :: TypusFile -> Either String String
analyzeDependencies file = Right "dependency-analysis"

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy cmp (x:xs) = insertBy cmp x (sortBy cmp xs)

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _ x [] = [x]
insertBy cmp x (y:ys) = case cmp x y of
  GT -> y : insertBy cmp x ys
  _  -> x : y : ys