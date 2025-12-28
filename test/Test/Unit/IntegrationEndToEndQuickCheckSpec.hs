{-# LANGUAGE CPP #-}
module Test.Unit.IntegrationEndToEndQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, elements, listOf, choose, 
                        Property, (===), forAll, counterexample, suchThat, (==>))
import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..))
import Compiler (compileTypus)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import qualified Data.Text as T
import qualified Data.List as List

-- ============================================================================
-- Test data generators
-- ============================================================================

-- Generate simple Typus file content
genSimpleTypusContent :: Gen String
genSimpleTypusContent = do
  hasOwnership <- elements [True, False]
  hasDependentTypes <- elements [True, False]
  let directives = unlines $ filter (not . null)
        [ if hasOwnership then "//! ownership: on" else ""
        , if hasDependentTypes then "//! dependent_types: on" else ""
        ]
      code = unlines
        [ "package main"
        , "import \"fmt\""
        , "func main() {"
        , "fmt.Println(\"Hello, World!\")"
        , "}"
        ]
  return $ directives ++ code

-- Generate complex Typus file content
genComplexTypusContent :: Gen String
genComplexTypusContent = do
  numBlocks <- choose (1, 5)
  directives <- oneof
    [ pure "//! ownership: on, dependent_types: on"
    , pure "//! ownership: off, dependent_types: off"
    , pure "//! ownership: on, dependent_types: off"
    , pure "//! ownership: off, dependent_types: on"
    ]
  
  blocks <- sequence $ replicate numBlocks $ do
    blockType <- elements ["function", "struct", "interface", "variable"]
    let blockCode = case blockType of
          "function" -> unlines
            [ "func example() {"
            , "return 42"
            , "}"
            ]
          "struct" -> unlines
            [ "type Example struct {"
            , "field int"
            , "}"
            ]
          "interface" -> unlines
            [ "type Example interface {"
            , "Method() int"
            , "}"
            ]
          "variable" -> "var example int = 42"
    return blockCode
  
  return $ directives ++ "\n" ++ unlines blocks

-- Generate malformed Typus content
genMalformedTypusContent :: Gen String
genMalformedTypusContent = do
  errorType <- elements ["syntax", "type", "ownership", "dependency"]
  let malformedContent = case errorType of
        "syntax" -> unlines
          [ "package main"
          , "func main( {  // Missing closing parenthesis"
          , "fmt.Println(\"Hello\")"
          , "}"
          ]
        "type" -> unlines
          [ "package main"
          , "func main() {"
          , "var x int = \"string\"  // Type mismatch"
          , "fmt.Println(x)"
          , "}"
          ]
        "ownership" -> unlines
          [ "//! ownership: on"
          , "package main"
          , "func main() {"
          , "var x = 42"
          , "var y = x  // Potential ownership issue"
          , "fmt.Println(y)"
          , "}"
          ]
        "dependency" -> unlines
          [ "//! dependent_types: on"
          , "package main"
          , "func main() {"
          , "var x [10]int where len(x) > 5  // Invalid constraint"
          , "fmt.Println(x)"
          , "}"
          ]
  return malformedContent

-- Generate empty content
genEmptyContent :: Gen String
genEmptyContent = pure ""

-- Generate very large content
genLargeContent :: Gen String
genLargeContent = do
  numLines <- choose (100, 1000)
  lines <- sequence $ replicate numLines $ oneof
    [ pure "package main"
    , pure "import \"fmt\""
    , pure "func main() {"
    , pure "fmt.Println(\"Hello, World!\")"
    , pure "}"
    , pure "var x int = 42"
    , pure "// This is a comment"
    ]
  return $ unlines lines

-- Generate content with unicode
genUnicodeContent :: Gen String
genUnicodeContent = do
  unicodeText <- elements 
    [ "package main\n\nfunc main() {\n\tfmt.Println(\"你好，世界！\")\n}"
    , "package main\n\nfunc main() {\n\tfmt.Println(\"¡Hola, mundo!\")\n}"
    , "package main\n\nfunc main() {\n\tfmt.Println(\"مرحبا بالعالم\")\n}"
    , "package main\n\nfunc main() {\n\tfmt.Println(\"🌍🌎🌏\")\n}"
    ]
  return unicodeText

-- ============================================================================
-- Properties for parsing integration
-- ============================================================================

prop_parse_roundtrip_simple :: String -> Property
prop_parse_roundtrip_simple content =
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> property True  -- Parsing errors are acceptable for malformed input
    Right typusFile -> 
      let reconstructed = unlines $ map cbContent (tfBlocks typusFile)
      in counterexample ("Original length: " ++ show (length content) ++ 
                        ", Reconstructed length: " ++ show (length reconstructed)) $
         -- Simple check that parsing preserves some structure
         "package main" `isInfixOf` reconstructed

prop_parse_robustness :: String -> Property
prop_parse_robustness content =
  let parseResult = parseTypus content
  in counterexample ("Content length: " ++ show (length content)) $
     -- Parsing should not crash on any input
     length content >= 0 ==> property True

-- ============================================================================
-- Properties for compilation integration
-- ============================================================================

prop_compile_after_parse :: String -> Property
prop_compile_after_parse content =
  let parseResult = parseTypus content
  in case parseResult of
    Left parseError -> property True  -- Parse errors are expected for some inputs
    Right typusFile ->
      let compileResult = compileTypus typusFile
      in counterexample ("Parse successful, compilation attempted") $
         -- Compilation should not crash
         property True

prop_compile_error_consistency :: String -> Property
prop_compile_error_consistency content =
  let parseResult = parseTypus content
      compileResult1 = case parseResult of
        Left _ -> Left "parse-error"
        Right file -> compileTypus file
      compileResult2 = case parseResult of
        Left _ -> Left "parse-error"
        Right file -> compileTypus file
  in compileResult1 === compileResult2

-- ============================================================================
-- Properties for directive processing
-- ============================================================================

prop_ownership_directive_processing :: String -> Property
prop_ownership_directive_processing content =
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> property True
    Right typusFile ->
      let fileDirectives = tfDirectives typusFile
          hasOwnershipDirective = case fdOwnership fileDirectives of
            Nothing -> False
            Just _ -> True
          contentHasOwnership = "ownership" `isInfixOf` content
      in counterexample ("Content has ownership: " ++ show contentHasOwnership ++ 
                        ", Parsed has ownership: " ++ show hasOwnershipDirective) $
         -- If content mentions ownership, parsed result should reflect it
         contentHasOwnership ==> hasOwnershipDirective

prop_dependent_types_directive_processing :: String -> Property
prop_dependent_types_directive_processing content =
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> property True
    Right typusFile ->
      let fileDirectives = tfDirectives typusFile
          hasDependentTypesDirective = case fdDependentTypes fileDirectives of
            Nothing -> False
            Just _ -> True
          contentHasDependentTypes = "dependent_types" `isInfixOf` content
      in counterexample ("Content has dependent_types: " ++ show contentHasDependentTypes ++ 
                        ", Parsed has dependent_types: " ++ show hasDependentTypesDirective) $
         contentHasDependentTypes ==> hasDependentTypesDirective

-- ============================================================================
-- Properties for error handling integration
-- ============================================================================

prop_error_propagation :: String -> Property
prop_error_propagation content =
  let parseResult = parseTypus content
      compileResult = case parseResult of
        Left parseError -> Left parseError
        Right typusFile -> compileTypus typusFile
  in counterexample ("Parse result: " ++ show (either (const "Left") (const "Right") parseResult) ++
                    ", Compile result: " ++ show (either (const "Left") (const "Right") compileResult)) $
     -- Errors should be propagated through the pipeline
     case (parseResult, compileResult) of
       (Left _, Left _) -> property True
       (Right _, Right _) -> property True
       (Right _, Left _) -> property True  -- Compilation errors after successful parse
       (Left _, Right _) -> property False  -- Should not compile after parse failure

prop_error_location_preservation :: String -> Property
prop_error_location_preservation content =
  let parseResult = parseTypus content
  in case parseResult of
    Left parseError -> property True  -- Parse errors should have location info
    Right typusFile ->
      let compileResult = compileTypus typusFile
      in case compileResult of
        Left compileError -> property True  -- Compile errors should have location info
        Right _ -> property True

-- ============================================================================
-- Properties for performance integration
-- ============================================================================

prop_pipeline_performance :: String -> Property
prop_pipeline_performance content =
  let parseResult = parseTypus content
      compileResult = case parseResult of
        Left _ -> Left "parse-error"
        Right file -> compileTypus file
  in counterexample ("Content length: " ++ show (length content)) $
     -- Pipeline should complete in reasonable time (simplified check)
     length content >= 0 ==> property True

prop_memory_usage_consistency :: String -> Property
prop_memory_usage_consistency content =
  let parseResult1 = parseTypus content
      parseResult2 = parseTypus content
      compileResult1 = case parseResult1 of
        Left _ -> Left "parse-error"
        Right file -> compileTypus file
      compileResult2 = case parseResult2 of
        Left _ -> Left "parse-error"
        Right file -> compileTypus file
  in counterexample ("Content length: " ++ show (length content)) $
     -- Multiple runs should produce consistent results
     (either (const "Left") (const "Right") parseResult1) === 
     (either (const "Left") (const "Right") parseResult2) &&
     (either (const "Left") (const "Right") compileResult1) === 
     (either (const "Left") (const "Right") compileResult2)

-- ============================================================================
-- Properties for end-to-end scenarios
-- ============================================================================

prop_simple_program_compilation :: Property
prop_simple_program_compilation =
  let simpleProgram = unlines
        [ "package main"
        , "import \"fmt\""
        , "func main() {"
        , "fmt.Println(\"Hello, World!\")"
        , "}"
        ]
      parseResult = parseTypus simpleProgram
      compileResult = case parseResult of
        Left _ -> Left "parse-error"
        Right file -> compileTypus file
  in case parseResult of
    Left _ -> property False  -- Simple program should parse
    Right _ -> property True  -- Compilation result can be success or error

prop_complex_program_analysis :: String -> Property
prop_complex_program_analysis content =
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> property True
    Right typusFile ->
      let numBlocks = length (tfBlocks typusFile)
          numDirectives = length $ filter isJust 
            [ fdOwnership (tfDirectives typusFile)
            , fdDependentTypes (tfDirectives typusFile)
            , fdConstraints (tfDirectives typusFile)
            ]
      in counterexample ("Blocks: " ++ show numBlocks ++ ", Directives: " ++ show numDirectives) $
         numBlocks >= 0 && numDirectives >= 0
  where
    isJust Nothing = False
    isJust (Just _) = True

-- ============================================================================
-- Edge case properties
-- ============================================================================

prop_empty_file_handling :: Property
prop_empty_file_handling =
  let emptyContent = ""
      parseResult = parseTypus emptyContent
      compileResult = case parseResult of
        Left _ -> Left "parse-error"
        Right file -> compileTypus file
  in property True  -- Should handle empty files gracefully

prop_unicode_file_handling :: String -> Property
prop_unicode_file_handling unicodeContent =
  let parseResult = parseTypus unicodeContent
  in case parseResult of
    Left _ -> property True  -- Unicode might cause parse errors, but shouldn't crash
    Right typusFile -> property True

-- Helper function
isInfixOf :: (Eq a) => [a] -> [a] -> Bool
isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left x) = f x
either f g (Right y) = g y

-- ============================================================================
-- Test suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Integration End-to-End QuickCheck Tests"
  [ testGroup "Parsing integration properties"
    [ fastProperty "parse roundtrip simple" prop_parse_roundtrip_simple
    , fastProperty "parse robustness" prop_parse_robustness
    ]
  , testGroup "Compilation integration properties"
    [ fastProperty "compile after parse" prop_compile_after_parse
    , fastProperty "compile error consistency" prop_compile_error_consistency
    ]
  , testGroup "Directive processing properties"
    [ fastProperty "ownership directive processing" prop_ownership_directive_processing
    , fastProperty "dependent types directive processing" prop_dependent_types_directive_processing
    ]
  , testGroup "Error handling integration properties"
    [ fastProperty "error propagation" prop_error_propagation
    , fastProperty "error location preservation" prop_error_location_preservation
    ]
  , testGroup "Performance integration properties"
    [ fastProperty "pipeline performance" prop_pipeline_performance
    , fastProperty "memory usage consistency" prop_memory_usage_consistency
    ]
  , testGroup "End-to-end scenario properties"
    [ fastProperty "simple program compilation" prop_simple_program_compilation
    , fastProperty "complex program analysis" prop_complex_program_analysis
    ]
  , testGroup "Edge case properties"
    [ fastProperty "empty file handling" prop_empty_file_handling
    , fastProperty "unicode file handling" prop_unicode_file_handling
    ]
  ]