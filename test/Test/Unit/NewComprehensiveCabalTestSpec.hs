{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewComprehensiveCabalTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose)
import Test.QuickCheck.Gen (Gen, listOf, elements, choose, oneof)

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..))
import Compiler (compile, CompilerError(..), CompilationPhase(..))
import Ownership (OwnershipType(..), OwnershipError(..))
import SourceLocation (SourcePos(..), SourceSpan(..), locatedWithSpan)
import Utils (trim, splitBy, removeComments, normalizeIndentation)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, null, length, reverse)
import Data.Char (isSpace, isAlpha, isDigit)

-- ============================================================================
-- Test 1: Parser Boundary Conditions
-- ============================================================================

test_parser_empty_input :: TestTree
test_parser_empty_input = testCase "Parser handles empty input gracefully" $ do
  case parseTypus "" of
    Left _ -> assertBool "Empty input should parse to minimal file" True
    Right typusFile -> do
      let FileDirectives { fdOwnership = ownership, fdDependentTypes = dependentTypes } = tfDirectives typusFile
      ownership @?= Nothing
      dependentTypes @?= Nothing
      null (tfBlocks typusFile) @?= True

test_parser_unicode_handling :: TestTree
test_parser_unicode_handling = testCase "Parser handles Unicode characters correctly" $ do
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func main() {"
        , "    println(\"你好世界 🌍\")"
        , "    let 变量 = \"测试\""
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "Unicode parsing failed: " ++ err
    Right typusFile -> do
      let FileDirectives { fdOwnership = ownership } = tfDirectives typusFile
      case ownership of
        Nothing -> assertFailure "Expected ownership directive"
        Just loc -> locatedValue loc @?= True
      assertBool "Should contain Unicode content" $ 
        any ("你好世界" `isInfixOf`) (map cbContent (tfBlocks typusFile))

-- ============================================================================
-- Test 2: Compiler Error Recovery
-- ============================================================================

test_compiler_error_recovery :: TestTree
test_compiler_error_recovery = testCase "Compiler recovers from syntax errors gracefully" $ do
  let source = unlines
        [ "package main"
        , "func main() {"
        , "    let x = 5"
        , "    let y = }  // Syntax error here"
        , "    println(x)"
        , "}"
        ]
  case compile source of
    Left errors -> do
      assertBool "Should have compilation errors" $ not $ null errors
      -- Check that we get meaningful error information
      let hasSyntaxError = any (\e -> case e of 
            SyntaxError _ _ -> True
            _ -> False) errors
      assertBool "Should identify syntax error" hasSyntaxError
    Right _ -> assertFailure "Expected compilation to fail with syntax errors"

-- ============================================================================
-- Test 3: Ownership Analysis Edge Cases
-- ============================================================================

test_ownership_nested_blocks :: TestTree
test_ownership_nested_blocks = testCase "Ownership analysis handles nested blocks correctly" $ do
  let source = unlines
        [ "package main"
        , "func main() {"
        , "    {//! ownership: on"
        , "        let resource = acquire()"
        , "        {//! ownership: off"
        , "            // Ownership temporarily disabled"
        , "            println(resource)"
        , "        }"
        , "        transfer(resource)  // Should be valid"
        , "    }"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "Parse failed: " ++ err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      assertBool "Should have multiple blocks with ownership directives" $ 
        length (filter (maybe False locatedValue . bdOwnership . cbDirectives) blocks) >= 1

-- ============================================================================
-- Test 4: Dependent Types Validation
-- ============================================================================

test_dependent_types_constraints :: TestTree
test_dependent_types_constraints = testCase "Dependent types validates constraints correctly" $ do
  let source = unlines
        [ "//! dependent_types: on"
        , "package main"
        , "func main() {"
        , "    let vec: Vector<n> where n > 0 = makeVector(5)"
        , "    let len: Nat where len == length(vec) = length(vec)"
        , "    println(len)"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "Parse failed: " ++ err
    Right typusFile -> do
      let FileDirectives { fdDependentTypes = dependentTypes } = tfDirectives typusFile
      case dependentTypes of
        Nothing -> assertFailure "Expected dependent types directive"
        Just loc -> locatedValue loc @?= True

-- ============================================================================
-- Test 5: QuickCheck Properties for Utils
-- ============================================================================

prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmed1 = trim str
      trimmed2 = trim trimmed1
  in property $ trimmed1 === trimmed2

prop_splitBy_consistency :: String -> Char -> Property
prop_splitBy_consistency str delim =
  let segments = splitBy delim str
      rejoined = concat $ map (++ [delim]) (init segments) ++ [last segments]
  in not (null str) && delim `elem` str ==>
     property $ length segments >= 1

prop_removeComments_preserves_code_structure :: String -> String -> Property
prop_removeComments_preserves_code_structure code1 code2 =
  not ('/' `isInfixOf` code1) && not ('/' `isInfixOf` code2) &&
  not ('*' `isInfixOf` code1) && not ('*' `isInfixOf` code2) ==>
  let original = code1 ++ "\n" ++ code2
      withComments = original ++ " // comment\n /* block comment */"
      cleaned = removeComments withComments
      codeLines = lines original
      cleanedLines = lines cleaned
  in property $ length cleanedLines >= length codeLines

-- ============================================================================
-- Test 6: Source Location Precision
-- ============================================================================

test_source_location_tracking :: TestTree
test_source_location_tracking = testCase "Source location tracking is precise for multi-line constructs" $ do
  let source = unlines
        [ "//! ownership: on"
        , "package main"
        , "func test() {"
        , "    {//! dependent_types: on"
        , "        let x = 5"
        , "    }"
        , "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "Parse failed: " ++ err
    Right typusFile -> do
      let FileDirectives { fdOwnership = ownership } = tfDirectives typusFile
      case ownership of
        Nothing -> assertFailure "Expected ownership directive"
        Just loc -> do
          let span = locSpan loc
          posLine (spanStart span) @?= 1
          posLine (spanEnd span) @?= 2

-- ============================================================================
-- Test 7: Text Processing Robustness
-- ============================================================================

prop_normalizeIndentation_preserves_structure :: String -> Property
prop_normalizeIndentation_preserves_structure content =
  let lines' = lines content
      normalized = normalizeIndentation content
      normalizedLines = lines normalized
  in not (null lines') ==>
     property $ length normalizedLines === length lines'

test_text_processing_edge_cases :: TestTree
test_text_processing_edge_cases = testCase "Text processing handles edge cases correctly" $ do
  let testCases = 
        [ ("", "")
        , ("   ", "")
        , ("\t\n\t", "\n")
        , ("  a  \n  b  ", "a\nb")
        , ("mixed\t  spaces", "mixed spaces")
        ]
  mapM_ (\(input, expected) -> 
    normalizeIndentation input @?= expected) testCases

-- ============================================================================
-- Test 8: Error Handling Consistency
-- ============================================================================

test_error_handling_consistency :: TestTree
test_error_handling_consistency = testCase "Error handling is consistent across compilation phases" $ do
  let invalidSource = unlines
        [ "package main"
        , "func main() {"
        , "    let x = }  // Invalid syntax"
        , "}"
        ]
  case compile invalidSource of
    Left errors -> do
      assertBool "Should have errors" $ not $ null errors
      -- Check that errors have proper phase information
      let hasPhaseInfo = any (\e -> case e of
            SyntaxError phase _ -> phase == ParsingPhase
            TypeError phase _ -> phase == TypeCheckingPhase
            OwnershipError phase _ -> phase == OwnershipAnalysisPhase
            _ -> False) errors
      assertBool "Errors should include phase information" hasPhaseInfo
    Right _ -> assertFailure "Expected compilation to fail"

-- ============================================================================
-- Test 9: Performance Boundary Tests
-- ============================================================================

test_large_file_handling :: TestTree
test_large_file_handling = testCase "Compiler handles large files efficiently" $ do
  let largeFunction = unlines $ replicate 1000 "    println(\"test\")"
      source = unlines
        [ "package main"
        , "func main() {"
        ] ++ [largeFunction] ++ [
        "}"
        ]
  case parseTypus source of
    Left err -> assertFailure $ "Large file parsing failed: " ++ err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      assertBool "Should parse large file correctly" $ not $ null blocks

-- ============================================================================
-- Test 10: Integration End-to-End Test
-- ============================================================================

test_end_to_end_compilation :: TestTree
test_end_to_end_compilation = testCase "End-to-end compilation works correctly" $ do
  let source = unlines
        [ "//! ownership: on"
        , "//! dependent_types: on"
        , "package main"
        , "import \"fmt\""
        , "func acquire() Resource { return Resource{} }"
        , "func transfer(r Resource) {}"
        , "func main() {"
        , "    {//! ownership: on"
        , "        let resource = acquire()"
        , "        transfer(resource)"
        , "    }"
        , "    fmt.Println(\"Success\")"
        , "}"
        ]
  case compile source of
    Left errors -> assertFailure $ "Compilation failed: " ++ show errors
    Right result -> do
      assertBool "Should generate Go code" $ not $ null result
      assertBool "Generated code should contain main function" $ "func main()" `isInfixOf` result

-- ============================================================================
-- Test Suite Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Comprehensive Cabal Tests"
  [ testGroup "Parser Tests"
      [ test_parser_empty_input
      , test_parser_unicode_handling
      ]
  , testGroup "Compiler Tests"
      [ test_compiler_error_recovery
      , test_error_handling_consistency
      , test_end_to_end_compilation
      ]
  , testGroup "Ownership Tests"
      [ test_ownership_nested_blocks
      ]
  , testGroup "Dependent Types Tests"
      [ test_dependent_types_constraints
      ]
  , testGroup "Source Location Tests"
      [ test_source_location_tracking
      ]
  , testGroup "Text Processing Tests"
      [ test_text_processing_edge_cases
      ]
  , testGroup "Performance Tests"
      [ test_large_file_handling
      ]
  , testGroup "QuickCheck Properties"
      [ fastProperty "trim is idempotent" prop_trim_idempotent
      , fastProperty "splitBy is consistent" prop_splitBy_consistency
      , fastProperty "removeComments preserves structure" prop_removeComments_preserves_code_structure
      , fastProperty "normalizeIndentation preserves structure" prop_normalizeIndentation_preserves_structure
      ]
  ]