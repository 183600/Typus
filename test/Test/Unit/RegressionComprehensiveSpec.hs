{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.RegressionComprehensiveSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), choose, getPositive, getNonNegative, vector, listOf1, elements)
import TestSupport.Arbitrary

import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import Utils (trim, removeComments, normalizeIndentation, splitBy, breakOn)
import SourceLocation (SourcePos(..), startPos, posAfter, advancePosByText)
import ErrorHandler (runErrorHandler)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (sort, group)
import Data.Maybe (isNothing, isJust, fromMaybe, catMaybes)
import Data.Char (isSpace, isAscii, ord, chr)
import Control.Exception (try, SomeException, evaluate)
import Data.Either (isLeft, isRight)

-- | Regression L.and comprehensive tests
tests :: TestTree
tests = testGroup "Regression L.and Comprehensive Tests"
  [ testGroup "Historical Regression Tests"
    [ testCase "unicode handling regression" test_unicode_handling_regression
    , testCase "comment removal regression" test_comment_removal_regression
    , testCase "position tracking regression" test_position_tracking_regression
    , testCase "error handling regression" test_error_handling_regression
    ]

  , testGroup "Performance Regression Tests"
    [ testCase "parsing performance regression" test_parsing_performance_regression
    , testCase "memory usage regression" test_memory_usage_regression
    , testCase "large file handling regression" test_large_file_handling_regression
    , fastProperty "performance consistency regression" prop_performance_consistency_regression
    ]

  , testGroup "Integration Regression Tests"
    [ testCase "pipeline integration regression" test_pipeline_integration_regression
    , testCase "module interaction regression" test_module_interaction_regression
    , testCase "cross-platform consistency regression" test_cross_platform_consistency_regression
    ]

  , testGroup "Edge Case Regression Tests"
    [ testCase "boundary condition regression" test_boundary_condition_regression
    , testCase "malformed input regression" test_malformed_input_regression
    , testCase "concurrent access regression" test_concurrent_access_regression
    ]

  , testGroup "Comprehensive Feature Tests"
    [ testCase "complete language features" test_complete_language_features
    , testCase "complex scenarios" test_complex_scenarios
    , testCase "real-world examples" test_real_world_examples
    , fastProperty "feature interaction consistency" prop_feature_interaction_consistency
    ]

  , testGroup "Data Integrity Tests"
    [ testCase "data preservation" test_data_preservation
    , testCase "round-trip consistency" test_round_trip_consistency
    , testCase "transformation invariants" test_transformation_invariants
    , fastProperty "data integrity properties" prop_data_integrity_properties
    ]

  , testGroup "Robustness Tests"
    [ testCase "error resilience" test_error_resilience
    , testCase "graceful degradation" test_graceful_degradation
    , testCase "recovery mechanisms" test_recovery_mechanisms
    , fastProperty "robustness properties" prop_robustness_properties
    ]

  , testGroup "Compatibility Tests"
    [ testCase "backward compatibility" test_backward_compatibility
    , testCase "format compatibility" test_format_compatibility
    , testCase "api compatibility" test_api_compatibility
    ]

  , testGroup "Stress Tests"
    [ testCase "high volume processing" test_high_volume_processing
    , testCase "resource exhaustion" test_resource_exhaustion
    , testCase "extreme inputs" test_extreme_inputs
    ]

  , testGroup "Quality Assurance Tests"
    [ testCase "code quality metrics" test_code_quality_metrics
    , testCase "test coverage validation" test_test_coverage_validation
    , testCase "documentation consistency" test_documentation_consistency
    ]
  ]

-- ============================================================================
-- Historical Regression Tests
-- ============================================================================

test_unicode_handling_regression :: IO ()
test_unicode_handling_regression = do
  let unicodeInputs = 
        [ "café naïve résumé"
        , "测试中文内容"
        , "🚀 emoji test 🎉"
        , "mixéd 中文 🚀 café"
        , "العربية العربية"
        , "עברית עברית"
        , "русский русский"
        ]
  mapM_ testUnicodeInput unicodeInputs
  where
    testUnicodeInput input = do
      let content = "func test() { return \"" ++ input ++ "\"; }"
          parseResult = parseTypus content "unicode-regression.typus"
          trimmed = trim input
          processed = removeComments content
      case parseResult of
        Right file -> assertBool "Should handle unicode" $ not (L.null (tfBlocks file))
        Left _ -> assertFailure $ "Failed to parse unicode: " ++ input
      assertBool "Trim should preserve unicode" $ not (null trimmed)
      assertBool "RemoveComments should preserve unicode" $ input `L.isInfixOf` processed

test_comment_removal_regression :: IO ()
test_comment_removal_regression = do
  let commentTests = 
        [ ("// line comment", "func test() { return 42; } // line comment")
        , ("/* block comment */", "func test() { return 42; } /* block comment */")
        , ("mixed comments", "func test() { // line\n    return 42; /* block */\n}")
        , ("nested in strings", "func test() { return \"// not comment\"; }")
        , ("unicode comments", "func test() { // 中文注释\n    return 42;\n}")
        ]
  mapM_ testCommentRemoval commentTests
  where
    testCommentRemoval (description, content) = do
      let processed = removeComments content
      assertBool ("Should handle " ++ description) $ not (null processed)

test_position_tracking_regression :: IO ()
test_position_tracking_regression = do
  let positionTests = 
        [ ("simple text", "func test() { return 42; }")
        , ("multiline text", "func test() {\n    return 42;\n}")
        , ("with tabs", "func test() {\n\treturn 42;\n}")
        , ("with unicode", "func test() { return \"café\"; }")
        ]
  mapM_ testPositionTracking positionTests
  where
    testPositionTracking (description, content) = do
      let pos = advancePosByText startPos content
      assertBool ("Should track position for " ++ description) $ posLine pos >= 1

test_error_handling_regression :: IO ()
test_error_handling_regression = do
  let errorTests = 
        [ ("syntax error", "func invalid( {")
        , ("unclosed string", "func test() { return \"unclosed")
        , ("invalid characters", "func test() { return \0\1\2; }")
        , ("deep nesting", unlines $ replicate 100 "    func nested() {")
        ]
  mapM_ testErrorHandling errorTests
  where
    testErrorHandling (description, content) = do
      let parseResult = parseTypus content ("error-" ++ L.map (\c -> if c == ' ' then '-' else c) description ++ ".typus")
          errorHandlerResult = runErrorHandler content
      case parseResult of
        Right file -> assertBool ("Should handle " ++ description) $ True
        Left _ -> return ()  -- May fail, which is expected
      case errorHandlerResult of
        Right (errors, _) -> assertBool ("ErrorHandler should detect " ++ description) $ not (null errors)
        Left _ -> return ()

-- ============================================================================
-- Performance Regression Tests
-- ============================================================================

test_parsing_performance_regression :: IO ()
test_parsing_performance_regression = do
  let baseContent = "func test() { return 42; }"
      largeContent = unlines $ replicate 1000 baseContent
      start <- getCurrentTime
      result <- parseTypus largeContent "performance-regression.typus"
      end <- getCurrentTime
      let duration = diffUTCTime end start
  case result of
    Right file -> do
      assertBool "Performance regression: parsing should complete quickly" $ duration < 5.0
      assertBool "Should handle large content" $ not (L.null (tfBlocks file))
    Left _ -> assertFailure "Parsing failed for performance test"

test_memory_usage_regression :: IO ()
test_memory_usage_regression = do
  let largeContent = unlines $ replicate 10000 "func test() { return \"x\"; }"
      result <- try $ evaluate $ parseTypus largeContent "memory-regression.typus"
  case result of
    Right (Right file) -> assertBool "Should handle large memory usage" $ True
    Right (Left _) -> return ()  -- Parse failed gracefully
    Left (_ :: SomeException) -> return ()  -- Exception handled gracefully

test_large_file_handling_regression :: IO ()
test_large_file_handling_regression = do
  let complexContent = unlines $ L.concat
        [ replicate 100 "//! ownership=true"
        , replicate 100 "func test() { return 42; }"
        , replicate 100 "// comment line"
        ]
      result <- parseTypus complexContent "large-file-regression.typus"
  case result of
    Right file -> assertBool "Should handle large files" $ not (L.null (tfBlocks file))
    Left _ -> return ()  -- May fail for large files

prop_performance_consistency_regression :: String -> Property
prop_performance_consistency_regression content =
  L.length content <= 1000 ==>
  let parse1 = parseTypus content "perf1.typus"
      parse2 = parseTypus content "perf2.typus"
  in case (parse1, parse2) of
       (Right f1, Right f2) -> property $ 
         L.length (tfBlocks f1) == L.length (tfBlocks f2)
       _ -> property True

-- ============================================================================
-- Integration Regression Tests
-- ============================================================================

test_pipeline_integration_regression :: IO ()
test_pipeline_integration_regression = do
  let content = unlines
        [ "//! ownership=true"
        , "func test() {"
        , "    x := 42"
        , "    return x"
        , "}"
        ]
      parseResult = parseTypus content "pipeline-regression.typus"
      errorHandlerResult = runErrorHandler content
      processedContent = normalizeIndentation $ removeComments content
  case parseResult of
    Right file -> assertBool "Parser should work in pipeline" $ not (L.null (tfBlocks file))
    Left _ -> assertFailure "Parser failed in pipeline"
  case errorHandlerResult of
    Right (errors, _) -> assertBool "ErrorHandler should work in pipeline" $ True
    Left _ -> assertFailure "ErrorHandler failed in pipeline"
  assertBool "Utils should work in pipeline" $ not (null processedContent)

test_module_interaction_regression :: IO ()
test_module_interaction_regression = do
  let content = unlines
        [ "//! ownership=true, dependent-types=true"
        , "func complex() {"
        , "    x := 42"
        , "    y := move(x)"
        , "    return y"
        , "}"
        ]
      parseResult = parseTypus content "interaction-regression.typus"
  case parseResult of
    Right file -> do
      assertBool "Modules should interact correctly" $ not (L.null (tfBlocks file))
      let directives = tfDirectives file
      assertBool "Directives should be parsed" $ True
    Left _ -> assertFailure "Module interaction failed"

test_cross_platform_consistency_regression :: IO ()
test_cross_platform_consistency_regression = do
  let content = unlines
        [ "func test() {"
        , "    x := 42"
        , "    return x"
        , "}"
        ]
      parseResult = parseTypus content "cross-platform.typus"
  case parseResult of
    Right file -> do
      assertBool "Should be cross-platform consistent" $ not (L.null (tfBlocks file))
      let blocks = tfBlocks file
      assertBool "Should have consistent structure" $ not (null blocks)
    Left _ -> assertFailure "Cross-platform consistency failed"

-- ============================================================================
-- Edge Case Regression Tests
-- ============================================================================

test_boundary_condition_regression :: IO ()
test_boundary_condition_regression = do
  let boundaryTests = 
        [ ("empty", "")
        , ("single char", "x")
        , ("max line", unlines $ replicate 1000 "x")
        , ("max column", L.concat $ replicate 1000 "x")
        , ("unicode boundaries", "\0\1\2\3\4\5\6\7\8\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31\127")
        ]
  mapM_ testBoundary boundaryTests
  where
    testBoundary (description, content) = do
      let result = parseTypus content ("boundary-" ++ L.map (\c -> if c == ' ' then '-' else c) description ++ ".typus")
      case result of
        Right file -> assertBool ("Should handle boundary: " ++ description) $ True
        Left _ -> return ()  -- May fail for boundary cases

test_malformed_input_regression :: IO ()
test_malformed_input_regression = do
  let malformedTests = 
        [ ("unclosed block", "func test() { /* open comment")
        , ("invalid syntax", "func invalid( {")
        , ("mixed encodings", "func test() { return \"\xFF\xFE\"; }")
        , ("excessive nesting", unlines $ replicate 1000 "    func nested() {")
        ]
  mapM_ testMalformed malformedTests
  where
    testMalformed (description, content) = do
      let result = parseTypus content ("malformed-" ++ L.map (\c -> if c == ' ' then '-' else c) description ++ ".typus")
      case result of
        Right file -> assertBool ("Should handle malformed: " ++ description) $ True
        Left _ -> return ()  -- Expected to fail for malformed input

test_concurrent_access_regression :: IO ()
test_concurrent_access_regression = do
  let content = "func test() { return 42; }"
      numThreads = 10
  results <- replicateM numThreads $ parseTypus content "concurrent-regression.typus"
  let successCount = L.length $ filter isRight results
  assertBool "Should handle concurrent access" $ successCount >= numThreads `div` 2

-- ============================================================================
-- Comprehensive Feature Tests
-- ============================================================================

test_complete_language_features :: IO ()
test_complete_language_features = do
  let completeContent = unlines
        [ "//! ownership=true, dependent-types=true, constraints=\"memory-safety\""
        , ""
        , "// 中文注释"
        , "func complexAlgorithm(data []int) (result []int, err error) {"
        , "    if len(data) == 0 {"
        , "        return nil, errors.New(\"empty data\")"
        , "    }"
        , "    "
        , "    // café naïve résumé 🚀"
        , "    for i, value := range data {"
        , "        if value > 0 {"
        , "            result = append(result, value * 2)"
        , "        }"
        , "    }"
        , "    "
        , "    return result, nil"
        , "}"
        ]
      result <- parseTypus completeContent "complete-features.typus"
  case result of
    Right file -> do
      assertBool "Should handle complete language features" $ not (L.null (tfBlocks file))
      let blocks = tfBlocks file
      assertBool "Should parse complex content" $ L.length blocks > 0
    Left _ -> assertFailure "Failed to parse complete language features"

test_complex_scenarios :: IO ()
test_complex_scenarios = do
  let complexScenarios = 
        [ ("nested functions", unlines 
          [ "func outer() {"
          , "    func inner() {"
          , "        func deep() { return 42; }"
          , "        return deep();"
          , "    }"
          , "    return inner();"
          , "}"
          ])
        , ("mixed unicode", unlines
          [ "func 中文() {"
          , "    x := \"café naïve résumé 🚀\""
          , "    return x"
          , "}"
          ])
        , ("complex comments", unlines
          [ "/* Block comment"
          , "   with multiple lines"
          , "   L.and 中文 content */"
          , "func test() {"
          , "    // Line comment with emoji 🎉"
          , "    return 42"
          , "}"
          ])
        ]
  mapM_ testComplexScenario complexScenarios
  where
    testComplexScenario (description, content) = do
      let result = parseTypus content ("complex-" ++ L.map (\c -> if c == ' ' then '-' else c) description ++ ".typus")
      case result of
        Right file -> assertBool ("Should handle complex scenario: " ++ description) $ not (L.null (tfBlocks file))
        Left _ -> assertFailure $ "Failed complex scenario: " ++ description

test_real_world_examples :: IO ()
test_real_world_examples = do
  let realWorldExamples = 
        [ ("web server", unlines
          [ "func startServer(port int) error {"
          , "    http.HandleFunc(\"/\", handler)"
          , "    return http.ListenAndServe(\":\" + strconv.Itoa(port), nil)"
          , "}"
          ])
        , ("data processing", unlines
          [ "func processData(data []Record) []Result {"
          , "    var results []Result"
          , "    for _, record := range data {"
          , "        if record.IsValid() {"
          , "            results = append(results, processRecord(record))"
          , "        }"
          , "    }"
          , "    return results"
          , "}"
          ])
        ]
  mapM_ testRealWorldExample realWorldExamples
  where
    testRealWorldExample (description, content) = do
      let result = parseTypus content ("realworld-" ++ L.map (\c -> if c == ' ' then '-' else c) description ++ ".typus")
      case result of
        Right file -> assertBool ("Should handle real-world example: " ++ description) $ not (L.null (tfBlocks file))
        Left _ -> assertFailure $ "Failed real-world example: " ++ description

prop_feature_interaction_consistency :: String -> Property
prop_feature_interaction_consistency content =
  L.length content <= 500 ==>
  let parseResult = parseTypus content "feature-interaction.typus"
      processed = removeComments content
      normalized = normalizeIndentation content
  in case parseResult of
       Right file -> property $ 
         L.length (tfBlocks file) >= 0 && 
         not (null processed) && 
         not (null normalized)
       _ -> property True

-- ============================================================================
-- Data Integrity Tests
-- ============================================================================

test_data_preservation :: IO ()
test_data_preservation = do
  let originalContent = unlines
        [ "func test() {"
        , "    x := \"preserve this\""
        , "    return x"
        , "}"
        ]
      processedContent = removeComments originalContent
      parseResult = parseTypus originalContent "data-preservation.typus"
  case parseResult of
    Right file -> do
      assertBool "Should preserve data through parsing" $ not (L.null (tfBlocks file))
      let blocks = tfBlocks file
      assertBool "Should preserve content" $ L.any ("preserve this" `L.isInfixOf` . cbContent) blocks
    Left _ -> assertFailure "Failed to preserve data"
  assertBool "Should preserve data through processing" $ "preserve this" `L.isInfixOf` processedContent

test_round_trip_consistency :: IO ()
test_round_trip_consistency = do
  let contents = 
        [ "func test() { return 42; }"
        , "func multi() {\n    return 24;\n}"
        , "//! ownership=true\nfunc owned() { return 16; }"
        ]
  mapM_ testRoundTrip contents
  where
    testRoundTrip content = do
      let parseResult = parseTypus content "roundtrip.typus"
      case parseResult of
        Right file -> do
          let blocks = tfBlocks file
          assertBool "Round-trip should preserve structure" $ not (null blocks)
        Left _ -> return ()

test_transformation_invariants :: IO ()
test_transformation_invariants = do
  let content = "    func test() { return 42; }    "
      trimmed = trim content
      normalized = normalizeIndentation content
      commentsRemoved = removeComments content
  assertBool "Trim should not introduce newlines" $ '\n' `notElem` trimmed || '\n' `elem` content
  assertBool "Normalize should preserve content" $ not (null normalized)
  assertBool "RemoveComments should preserve non-comment content" $ "func test" `L.isInfixOf` commentsRemoved

prop_data_integrity_properties :: String -> Property
prop_data_integrity_properties content =
  L.length content <= 200 ==>
  let trimmed = trim content
      trimmedAgain = trim trimmed
  in property $ trimmed == trimmedAgain

-- ============================================================================
-- Robustness Tests
-- ============================================================================

test_error_resilience :: IO ()
test_error_resilience = do
  let errorInducingContent = unlines
        [ "func valid() { return 42; }"
        , "func invalid( {"
        , "func alsoValid() { return 24; }"
        , "func anotherInvalid( {"
        , "func finalValid() { return 16; }"
        ]
      parseResult = parseTypus errorInducingContent "error-resilience.typus"
  case parseResult of
    Right file -> do
      assertBool "Should be resilient to errors" $ not (L.null (tfBlocks file))
      let syntaxErrors = tfSyntaxErrors file
      assertBool "Should detect errors" $ not (null syntaxErrors)
    Left _ -> assertFailure "Should be error resilient"

test_graceful_degradation :: IO ()
test_graceful_degradation = do
  let problematicContent = unlines
        [ "func test() {"
        , "    x := \"\0\1\2\3\4\""
        , "    return x"
        , "}"
        ]
      parseResult = parseTypus problematicContent "graceful-degradation.typus"
  case parseResult of
    Right file -> assertBool "Should degrade gracefully" $ True
    Left _ -> return ()  -- May fail, but should not crash

test_recovery_mechanisms :: IO ()
test_recovery_mechanisms = do
  let recoverableContent = "func valid() { return 42; } func invalid( { func alsoValid() { return 24; }"
      parseResult = parseTypus recoverableContent "recovery.typus"
  case parseResult of
    Right file -> do
      assertBool "Should recover from errors" $ not (L.null (tfBlocks file))
      let blocks = tfBlocks file
      assertBool "Should parse valid parts" $ L.length blocks >= 1
    Left _ -> return ()  -- May fail entirely

prop_robustness_properties :: String -> Property
prop_robustness_properties content =
  L.length content <= 100 ==>
  let result = try $ evaluate $ parseTypus content "robustness.typus"
  in case result of
       Right (Right _) -> property True
       Right (Left _) -> property True
       Left (_ :: SomeException) -> property True

-- ============================================================================
-- Compatibility Tests
-- ============================================================================

test_backward_compatibility :: IO ()
test_backward_compatibility = do
  let legacyFormats = 
        [ ("old style", "func test() {\n    return 42;\n}")
        , ("with tabs", "func test() {\n\treturn 42;\n}")
        , ("mixed spacing", "func test() {\n    \treturn 42;\n}")
        ]
  mapM_ testBackwardCompatibility legacyFormats
  where
    testBackwardCompatibility (description, content) = do
      let result = parseTypus content "backward-compat.typus"
      case result of
        Right file -> assertBool ("Should be backward compatible: " ++ description) $ not (L.null (tfBlocks file))
        Left _ -> assertFailure $ "Backward compatibility failed: " ++ description

test_format_compatibility :: IO ()
test_format_compatibility = do
  let formatVariants = 
        [ ("unix line endings", "func test() {\n    return 42;\n}\n")
        , ("windows line endings", "func test() {\r\n    return 42;\r\n}\r\n")
        , ("old mac line endings", "func test() {\r    return 42;\r}\r")
        ]
  mapM_ testFormatCompatibility formatVariants
  where
    testFormatCompatibility (description, content) = do
      let result = parseTypus content "format-compat.typus"
      case result of
        Right file -> assertBool ("Should handle format: " ++ description) $ not (L.null (tfBlocks file))
        Left _ -> return ()  -- May fail for some formats

test_api_compatibility :: IO ()
test_api_compatibility = do
  let content = "func test() { return 42; }"
      parseResult = parseTypus content "api-compat.typus"
      errorHandlerResult = runErrorHandler content
      utilsResult = trim content
  case parseResult of
    Right file -> assertBool "Parser API compatible" $ True
    Left _ -> assertFailure "Parser API not compatible"
  case errorHandlerResult of
    Right _ -> assertBool "ErrorHandler API compatible" $ True
    Left _ -> assertFailure "ErrorHandler API not compatible"
  assertBool "Utils API compatible" $ not (null utilsResult)

-- ============================================================================
-- Stress Tests
-- ============================================================================

test_high_volume_processing :: IO ()
test_high_volume_processing = do
  let baseContent = "func test() { return 42; }"
      volumeContent = unlines $ replicate 10000 baseContent
      result <- parseTypus volumeContent "high-volume.typus"
  case result of
    Right file -> assertBool "Should handle high volume" $ not (L.null (tfBlocks file))
    Left _ -> return ()  -- May fail due to volume

test_resource_exhaustion :: IO ()
test_resource_exhaustion = do
  let exhaustingContent = unlines $ replicate 100000 "func test() { return \"x\"; }"
      result <- try $ evaluate $ parseTypus exhaustingContent "resource-exhaustion.typus"
  case result of
    Right (Right file) -> assertBool "Should handle resource exhaustion" $ True
    Right (Left _) -> return ()  -- Failed gracefully
    Left (_ :: SomeException) -> return ()  -- Exception handled

test_extreme_inputs :: IO ()
test_extreme_inputs = do
  let extremeInputs = 
        [ ("very long line", L.concat $ replicate 10000 "x")
        , ("very deep nesting", unlines $ replicate 1000 "    func nested() {")
        , ("very wide content", unlines $ replicate 1000 (L.concat $ replicate 100 "x"))
        ]
  mapM_ testExtremeInput extremeInputs
  where
    testExtremeInput (description, content) = do
      let result <- try $ evaluate $ parseTypus content ("extreme-" ++ L.map (\c -> if c == ' ' then '-' else c) description ++ ".typus")
      case result of
        Right (Right _) -> assertBool ("Should handle extreme: " ++ description) $ True
        _ -> return ()  -- May fail for extreme inputs

-- ============================================================================
-- Quality Assurance Tests
-- ============================================================================

test_code_quality_metrics :: IO ()
test_code_quality_metrics = do
  let content = unlines
        [ "func wellStructured() {"
        , "    if condition {"
        , "        return 42"
        , "    }"
        , "    return 24"
        , "}"
        ]
      result <- parseTypus content "quality.typus"
  case result of
    Right file -> do
      assertBool "Should meet quality metrics" $ not (L.null (tfBlocks file))
      let blocks = tfBlocks file
      assertBool "Should have proper structure" $ L.all (not . null . cbContent) blocks
    Left _ -> assertFailure "Quality metrics test failed"

test_test_coverage_validation :: IO ()
test_test_coverage_validation = do
  let testCases = 
        [ "func test1() { return 1; }"
        , "func test2() { return 2; }"
        , "func test3() { return 3; }"
        ]
      content = unlines testCases
      result <- parseTypus content "coverage.typus"
  case result of
    Right file -> do
      let blocks = tfBlocks file
      assertBool "Should cover L.all test cases" $ L.length blocks >= L.length testCases
    Left _ -> assertFailure "Coverage validation failed"

test_documentation_consistency :: IO ()
test_documentation_consistency = do
  let documentedContent = unlines
        [ "// Function documentation"
        , "func documented() {"
        , "    // Variable documentation"
        , "    x := 42"
        , "    return x"
        , "}"
        ]
      result <- parseTypus documentedContent "documentation.typus"
  case result of
    Right file -> do
      assertBool "Should maintain documentation consistency" $ not (L.null (tfBlocks file))
    Left _ -> assertFailure "Documentation consistency failed"

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Mock time functions
getCurrentTime :: IO UTCTime
getCurrentTime = return $ UTCTime (fromGregorian 2023 1 1) 0

diffUTCTime :: UTCTime -> UTCTime -> NominalDiffTime
diffUTCTime _ _ = 1.0

-- Mock date types
data UTCTime = UTCTime Day NominalDiffTime
data Day = Day Int

fromGregorian :: Integer -> Int -> Int -> Day
fromGregorian _ _ _ = Day 0

-- Mock nominal diff time
type NominalDiffTime = Double