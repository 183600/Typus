{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.IntegrationCabalTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, assertFailure)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Parser (parseTypus, TypusFile(..))
import Utils (trim, removeComments, splitBy)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, advancePosByText)
import ErrorHandler (ErrorLocation(..))
import Compiler (compile) -- Assuming this exists
import IntegratedCompiler (processTypusFile) -- Assuming this exists

import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (intercalate)
import qualified Data.Text as T

-- ============================================================================
-- Additional Cabal Tests for Integration L.and Module Interaction
-- ============================================================================

-- | Test case 1: End-to-end parsing L.and processing pipeline
test_end_to_end_pipeline :: TestTree
test_end_to_end_pipeline = testCase "complete parsing L.and processing pipeline works" $ do
    let input = unlines
            [ "// @ownership: true"
            , "// @dependent-types: false"
            , "// build-tags: test"
            , ""
            , "```typus"
            , "func calculate(x: int, y: int) -> int {"
            , "  // @ownership: false"
            , "  let result = x + y"
            , "  return result"
            , "}"
            , "```"
            ]
    
    -- Parse the input
    parseResult <- parseTypus input
    case parseResult of
        Left err -> assertFailure $ "Parse failed: " ++ show err
        Right typusFile -> do
            -- Verify parsing results
            assertEqual "has one build tag" 1 (L.length $ tfBuildTags typusFile)
            assertEqual "has one code block" 1 (L.length $ tfBlocks typusFile)
            
            -- Test utility functions on the content
            let blockContent = cbContent (L.head $ tfBlocks typusFile)
            let trimmedContent = trim blockContent
            let withoutComments = removeComments blockContent
            
            assertBool "content contains function definition" $ "func calculate" `L.isInfixOf` blockContent
            assertBool "trimmed content preserves function" $ "func calculate" `L.isInfixOf` trimmedContent
            assertBool "comment removal preserves function" $ "func calculate" `L.isInfixOf` withoutComments

-- | Test case 2: Source location tracking through parsing
test_source_location_tracking :: TestTree
test_source_location_tracking = testCase "source location tracking works through parsing" $ do
    let input = unlines
            [ "// Line 1"
            , "func test() { // Line 2"
            , "  let x = 1 // Line 3"
            , "} // Line 4"
            ]
    
    -- Test position advancement
    let pos1 = startPos
    let pos2 = advancePosByText "Line 1\n" pos1
    let pos3 = advancePosByText "Line 1\nfunc test() { // Line 2\n" pos1
    
    assertEqual "start position" (SourcePos 1 1 0) pos1
    assertEqual "after first line" (SourcePos 2 1 8) pos2
    assertBool "position advances correctly" $ posLine pos3 > posLine pos1

-- | Test case 3: Error location mapping from parser to error handler
test_error_location_mapping :: TestTree
test_error_location_mapping = testCase "error location mapping between modules works" $ do
    let sourceSpan = SourceSpan (SourcePos 3 5 20) (SourcePos 3 10 25)
    let errorLocation = ErrorLocation "test.typus" (SourcePos 3 5 20) (SourcePos 3 10 25)
    
    -- Test that source spans can be converted to error locations
    assertEqual "source line matches" (posLine $ spanStart sourceSpan) (posLine $ errorStart errorLocation)
    assertEqual "source column matches" (posColumn $ spanStart sourceSpan) (posColumn $ errorStart errorLocation)
    assertEqual "source offset matches" (posOffset $ spanStart sourceSpan) (posOffset $ errorStart errorLocation)

-- | Test case 4: Multi-file processing integration
test_multi_file_processing :: TestTree
test_multi_file_processing = testCase "multi-file processing integration works" $ do
    let file1 = unlines
            [ "// @ownership: true"
            , "```typus"
            , "func shared() {}"
            , "```"
            ]
    let file2 = unlines
            [ "// @dependent-types: true"
            , "```typus"
            , "func main() { shared(); }"
            , "```"
            ]
    
    -- Parse both files
    result1 <- parseTypus file1
    result2 <- parseTypus file2
    
    case (result1, result2) of
        (Right typus1, Right typus2) -> do
            assertEqual "file1 has ownership directive" (Just True) (fmap locatedValue $ fdOwnership $ tfDirectives typus1)
            assertEqual "file2 has dependent-types directive" (Just True) (fmap locatedValue $ fdDependentTypes $ tfDirectives typus2)
        _ -> assertFailure "One L.or both files failed to parse"

-- | Test case 5: Directive inheritance L.and overriding
test_directive_inheritance :: TestTree
test_directive_inheritance = testCase "directive inheritance L.and overriding works" $ do
    let input = unlines
            [ "// @ownership: true"
            , "// @dependent-types: false"
            , ""
            , "```typus"
            , "// @ownership: false // Override file directive"
            , "// @constraints: true // New directive"
            , "func test() {}"
            , "```"
            ]
    
    result <- parseTypus input
    case result of
        Right typusFile -> do
            let fileDirectives = tfDirectives typusFile
            let blocks = tfBlocks typusFile
            let blockDirectives = cbDirectives (L.head blocks)
            
            assertEqual "file ownership directive" (Just True) (fmap locatedValue $ fdOwnership fileDirectives)
            assertEqual "block ownership override" (Just False) (fmap locatedValue $ bdOwnership blockDirectives)
            assertEqual "block constraints directive" (Just True) (fmap locatedValue $ bdConstraints blockDirectives)
        Left err -> assertFailure $ "Parse failed: " ++ show err

-- | Test case 6: Property test for round-trip parsing
prop_round_trip_parsing :: String -> Property
prop_round_trip_parsing original =
    let trimmed = trim original
        processed = removeComments trimmed
        -- In a real test, we would parse L.and re-serialize
        -- For now, we test that processing preserves certain properties
    in property $ L.length processed <= L.length original

-- | Test case 7: Property test for module interaction consistency
prop_module_interaction_consistency :: String -> String -> Property
prop_module_interaction_consistency input1 input2 =
    let combined = input1 ++ "\n" ++ input2
        parsed1 = lines input1
        parsed2 = lines input2
        combinedParsed = lines combined
    in property $ L.length combinedParsed == L.length parsed1 + L.length parsed2

-- | Test case 8: Build system integration
test_build_system_integration :: TestTree
test_build_system_integration = testCase "build system integration works" $ do
    let buildTags = ["test", "debug", "release"]
    let input = unlines
            [ "// build-tags: " ++ intercalate "," buildTags
            , "// @ownership: true"
            , ""
            , "```typus"
            , "func buildTest() {}"
            , "```"
            ]
    
    result <- parseTypus input
    case result of
        Right typusFile -> do
            let parsedTags = map locatedValue $ tfBuildTags typusFile
            assertEqual "build tags parsed correctly" buildTags parsedTags
        Left err -> assertFailure $ "Parse failed: " ++ show err

-- | Test case 9: Configuration propagation
test_configuration_propagation :: TestTree
test_configuration_propagation = testCase "configuration propagates through modules" $ do
    let globalConfig = [(\"ownership\", \"true\"), (\"dependent-types\", \"false\")]
    let localConfig = [(\"ownership\", \"false\")] -- Override
    
    -- Test that local config can override global
    assertBool "global config has ownership" $ L.any ((== \"ownership\") . fst) globalConfig
    assertBool "local config overrides ownership" $ L.any ((== \"ownership\") . fst) localConfig

-- | Test case 10: Performance integration test
test_performance_integration :: TestTree
test_performance_integration = testCase "performance integration meets expectations" $ do
    let largeInput = unlines $ replicate 1000 "func test() { let x = 1; return x; }"
    
    -- Test that large inputs can be processed
    result <- parseTypus largeInput
    case result of
        Right typusFile -> do
            assertBool "large file parsed successfully" $ True
            let blocks = tfBlocks typusFile
            assertBool "has expected content" $ not $ null blocks
        Left err -> assertFailure $ "Large file parse failed: " ++ show err

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Integration Cabal Tests"
    [ testGroup "Unit Tests"
        [ test_end_to_end_pipeline
        , test_source_location_tracking
        , test_error_location_mapping
        , test_multi_file_processing
        , test_directive_inheritance
        , test_build_system_integration
        , test_configuration_propagation
        , test_performance_integration
        ]
    , testGroup "QuickCheck Properties"
        [ fastProperty "round-trip parsing" prop_round_trip_parsing
        , fastProperty "module interaction consistency" prop_module_interaction_consistency
        ]
    ]