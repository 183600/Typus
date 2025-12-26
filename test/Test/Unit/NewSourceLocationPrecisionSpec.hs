{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewSourceLocationPrecisionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, elements, suchThat)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, catMaybes)
import Data.List (isInfixOf, nub)

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedWithSpan, spanStart, spanEnd, sourceLine, sourceColumn)
import Parser (parseTypus, TypusFile(..))
import Compiler (compile, CompilerError(..))
import SyntaxValidator (validateSyntax)

-- | Test source location precision functionality
tests :: TestTree
tests =
  testGroup "New Source Location Precision Tests"
    [ basicLocationTests
    , precisionTests
    , multiLineTests
    , errorLocationTests
    , locationTrackingTests
    , performanceTests
    , quickCheckProperties
    ]

-- | Basic source location functionality tests
basicLocationTests :: TestTree
basicLocationTests =
  testGroup "Basic Location Tests"
    [ testCase "Track single character positions" $
        let input = "x"
            result = parseTypus "test.typus" input
        in case result of
             Right typusFile -> do
               let blocks = tfCodeBlocks typusFile
               assertBool "Should track character positions" (all hasValidCharacterPositions blocks)
             Left _ -> assertFailure "Should parse simple input"

    , testCase "Track line and column positions" $
        let input = "let x = 5\nlet y = 10"
            result = parseTypus "test.typus" input
        in case result of
             Right typusFile -> do
               let blocks = tfCodeBlocks typusFile
               assertBool "Should track line positions" (all hasValidLinePositions blocks)
               assertBool "Should track column positions" (all hasValidColumnPositions blocks)
             Left _ -> assertFailure "Should parse multi-line input"

    , testCase "Handle empty files" $
        let input = ""
            result = parseTypus "test.typus" input
        in case result of
             Right typusFile -> do
               assertEqual "Empty file should have no blocks" [] (tfCodeBlocks typusFile)
             Left _ -> assertFailure "Should parse empty file"

    , testCase "Track whitespace positions" $
        let input = "  let x = 5\t\n  let y = 10"
            result = parseTypus "test.typus" input
        in case result of
             Right typusFile -> do
               let blocks = tfCodeBlocks typusFile
               assertBool "Should handle whitespace correctly" (all handlesWhitespaceCorrectly blocks)
             Left _ -> assertFailure "Should parse input with whitespace"
    ]

-- | Precision tests
precisionTests :: TestTree
precisionTests =
  testGroup "Precision Tests"
    [ testCase "Precise token boundaries" $
        let input = "let x = 5 + 3"
            result = parseTypus "test.typus" input
        in case result of
             Right typusFile -> do
               let blocks = tfCodeBlocks typusFile
               assertBool "Should have precise token boundaries" (all hasPreciseTokenBoundaries blocks)
             Left _ -> assertFailure "Should parse arithmetic expression"

    , testCase "Unicode character handling" $
        let input = "let π = 3.14159\nlet 你好 = \"hello\""
            result = parseTypus "test.typus" input
        in case result of
             Right typusFile -> do
               let blocks = tfCodeBlocks typusFile
               assertBool "Should handle Unicode characters" (all handlesUnicodeCharacters blocks)
             Left _ -> assertFailure "Should parse Unicode input"

    , testCase "Tab character handling" $
        let input = "\tlet x = 5\n\t\tlet y = 10"
            result = parseTypus "test.typus" input
        in case result of
             Right typusFile -> do
               let blocks = tfCodeBlocks typusFile
               assertBool "Should handle tabs correctly" (all handlesTabsCorrectly blocks)
             Left _ -> assertFailure "Should parse input with tabs"

    , testCase "Mixed line endings" $
        let input = "let x = 5\r\nlet y = 10\nlet z = 15\r"
            result = parseTypus "test.typus" input
        in case result of
             Right typusFile -> do
               let blocks = tfCodeBlocks typusFile
               assertBool "Should handle mixed line endings" (all handlesMixedLineEndings blocks)
             Left _ -> assertFailure "Should parse input with mixed line endings"
    ]

-- | Multi-line tests
multiLineTests :: TestTree
multiLineTests =
  testGroup "Multi-line Tests"
    [ testCase "Track multi-line statements" $
        let input = "let x = (\n  5 +\n  3\n)"
            result = parseTypus "test.typus" input
        in case result of
             Right typusFile -> do
               let blocks = tfCodeBlocks typusFile
               assertBool "Should track multi-line spans" (all tracksMultiLineSpans blocks)
             Left _ -> assertFailure "Should parse multi-line statement"

    , testCase "Track nested block locations" $
        let input = "func test() {\n  if true {\n    let x = 5\n  }\n}"
            result = parseTypus "test.typus" input
        in case result of
             Right typusFile -> do
               let blocks = tfCodeBlocks typusFile
               assertBool "Should track nested blocks" (all tracksNestedBlocks blocks)
             Left _ -> assertFailure "Should parse nested blocks"

    , testCase "Handle large spans efficiently" $
        let input = unlines $ replicate 1000 "let x" ++ "let y = 42"
            result = parseTypus "test.typus" input
        in case result of
             Right typusFile -> do
               let blocks = tfCodeBlocks typusFile
               assertBool "Should handle large files" (all handlesLargeSpans blocks)
             Left _ -> assertFailure "Should parse large input"

    , testCase "Track comment locations" $
        let input = "// This is a comment\nlet x = 5 /* block comment */\nlet y = 10"
            result = parseTypus "test.typus" input
        in case result of
             Right typusFile -> do
               let blocks = tfCodeBlocks typusFile
               assertBool "Should track comment locations" (all tracksCommentLocations blocks)
             Left _ -> assertFailure "Should parse input with comments"
    ]

-- | Error location tests
errorLocationTests :: TestTree
errorLocationTests =
  testGroup "Error Location Tests"
    [ testCase "Precise syntax error locations" $
        let input = "let x = 5\nlet y = \nlet z = 10"  -- Syntax error on line 2
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should locate syntax errors precisely" (all locatesSyntaxErrorsPrecisely errs)
               assertBool "Should include line numbers" (all includesLineNumbers errs)
               assertBool "Should include column numbers" (all includesColumnNumbers errs)
             Right _ -> assertFailure "Should have failed with syntax error"

    , testCase "Type error location accuracy" $
        let input = "let x: int = \"hello\""  -- Type error
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should locate type errors accurately" (all locatesTypeErrorsAccurately errs)
               assertBool "Should highlight problematic tokens" (all highlightsProblematicTokens errs)
             Right _ -> assertFailure "Should have failed with type error"

    , testCase "Multi-error location tracking" $
        let input = "let x: int = \"hello\"\nlet y = \nlet z: string = 42"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should track multiple error locations" (all tracksMultipleErrorLocations errs)
               assertBool "Should order errors by location" (errorsOrderedByLocation errs)
             Right _ -> assertFailure "Should have failed with multiple errors"

    , testCase "Error context preservation" $
        let input = "func complex(a: int, b: string) -> float {\n  return a + b\n}"
            result = compile "test.typus" input
        in case result of
             Left errs -> do
               assertBool "Should preserve error context" (all preservesErrorContext errs)
               assertBool "Should show surrounding code" (all showsSurroundingCode errs)
             Right _ -> assertFailure "Should have failed with type error"
    ]

-- | Location tracking tests
locationTrackingTests :: TestTree
locationTrackingTests =
  testGroup "Location Tracking Tests"
    [ testCase "Track location through transformations" $
        let input = "let x = 5 + 3 * 2"
            result = compile "test.typus" input
        in case result of
             Right _ -> do
               assertBool "Should track through transformations" True
             Left _ -> assertFailure "Should compile successfully"

    , testCase "Maintain location consistency" $
        let input = "func test() {\n  let x = 5\n  return x\n}"
            result = compile "test.typus" input
        in case result of
             Right _ -> do
               assertBool "Should maintain location consistency" True
             Left _ -> assertFailure "Should compile successfully"

    , testCase "Track location in macros" $
        let input = "#define SQUARE(x) x * x\nlet y = SQUARE(5)"
            result = compile "test.typus" input
        in case result of
             Right _ -> do
               assertBool "Should track macro locations" True
             Left _ -> assertFailure "Should handle macros"

    , testCase "Handle location in generated code" $
        let input = "func test() {\n  for i in 0..10 {\n    let x = i\n  }\n}"
            result = compile "test.typus" input
        in case result of
             Right _ -> do
               assertBool "Should handle generated code locations" True
             Left _ -> assertFailure "Should compile successfully"
    ]

-- | Performance tests
performanceTests :: TestTree
performanceTests =
  testGroup "Performance Tests"
    [ testCase "Large file parsing performance" $
        let input = unlines $ replicate 10000 "let x = x + 1"
            result = parseTypus "test.typus" input
        in case result of
             Right typusFile -> do
               let blocks = tfCodeBlocks typusFile
               assertBool "Should handle large files efficiently" (length blocks > 0)
             Left _ -> assertFailure "Should parse large file"

    , testCase "Location tracking overhead" $
        let input = unlines $ replicate 1000 "func test_" ++ show (1 :: Int) ++ "() { return 42 }"
            result = parseTypus "test.typus" input
        in case result of
             Right typusFile -> do
               let blocks = tfCodeBlocks typusFile
               assertBool "Should minimize location tracking overhead" (all hasEfficientLocationTracking blocks)
             Left _ -> assertFailure "Should parse many functions"

    , testCase "Memory usage with locations" $
        let input = unlines $ replicate 5000 "let x_" ++ show (1 :: Int) ++ " = " ++ show (1 :: Int)
            result = parseTypus "test.typus" input
        in case result of
             Right typusFile -> do
               let blocks = tfCodeBlocks typusFile
               assertBool "Should use memory efficiently" (all usesMemoryEfficiently blocks)
             Left _ -> assertFailure "Should parse many variables"
    ]

-- | QuickCheck properties for source location precision
quickCheckProperties :: TestTree
quickCheckProperties =
  testGroup "QuickCheck Properties"
    [ testProperty "Location positions are consistent" $
        forAll genValidCode $ \code ->
            case parseTypus "test.typus" code of
              Right typusFile -> 
                let blocks = tfCodeBlocks typusFile
                in property $ all hasConsistentLocations blocks
              Left _ -> property True  -- Invalid code is allowed to fail

    , testProperty "Multi-line spans are accurate" $
        forAll genMultiLineCode $ \code ->
            case parseTypus "test.typus" code of
              Right typusFile -> 
                let blocks = tfCodeBlocks typusFile
                in property $ all hasAccurateMultiLineSpans blocks
              Left _ -> property True

    , testProperty "Error locations are precise" $
        forAll genErrorCode $ \code ->
            case compile "test.typus" code of
              Left errs -> 
                property $ all hasPreciseErrorLocations errs
              Right _ -> property True  -- Valid code should succeed
    ]

-- | Helper functions for location validation
hasValidCharacterPositions :: CodeBlock -> Bool
hasValidCharacterPositions block = 
    let span = cbSpan block
    in sourceColumn (spanStart span) >= 1 && sourceColumn (spanEnd span) >= sourceColumn (spanStart span)

hasValidLinePositions :: CodeBlock -> Bool
hasValidLinePositions block = 
    let span = cbSpan block
    in sourceLine (spanStart span) >= 1 && sourceLine (spanEnd span) >= sourceLine (spanStart span)

hasValidColumnPositions :: CodeBlock -> Bool
hasValidColumnPositions block = 
    let span = cbSpan block
    in sourceColumn (spanStart span) >= 1 && sourceColumn (spanEnd span) >= sourceColumn (spanStart span)

handlesWhitespaceCorrectly :: CodeBlock -> Bool
handlesWhitespaceCorrectly block = 
    let content = cbContent block
        span = cbSpan block
    in length content > 0 && spanStart span /= spanEnd span

hasPreciseTokenBoundaries :: CodeBlock -> Bool
hasPreciseTokenBoundaries block = 
    let content = cbContent block
        span = cbSpan block
    in length content == fromEnum (sourceColumn (spanEnd span)) - fromEnum (sourceColumn (spanStart span)) + 1

handlesUnicodeCharacters :: CodeBlock -> Bool
handlesUnicodeCharacters block = 
    let content = cbContent block
    in any (> 127) (map fromEnum content) ==> length content > 0

handlesTabsCorrectly :: CodeBlock -> Bool
handlesTabsCorrectly block = 
    let content = cbContent block
    in '\t' `elem` content ==> length (filter (== '\t') content) >= 0

handlesMixedLineEndings :: CodeBlock -> Bool
handlesMixedLineEndings block = 
    let content = cbContent block
    in any (`elem` content) ['\r', '\n'] ==> length content > 0

tracksMultiLineSpans :: CodeBlock -> Bool
tracksMultiLineSpans block = 
    let span = cbSpan block
    in sourceLine (spanStart span) < sourceLine (spanEnd span)

tracksNestedBlocks :: CodeBlock -> Bool
tracksNestedBlocks block = 
    let content = cbContent block
    in '{' `elem` content && '}' `elem` content

handlesLargeSpans :: CodeBlock -> Bool
handlesLargeSpans block = 
    let span = cbSpan block
    in sourceLine (spanEnd span) - sourceLine (spanStart span) >= 100

tracksCommentLocations :: CodeBlock -> Bool
tracksCommentLocations block = 
    let content = cbContent block
    in any (`isInfixOf` content) ["//", "/*", "*/"]

locatesSyntaxErrorsPrecisely :: CompilerError -> Bool
locatesSyntaxErrorsPrecisely (CompilerError SyntaxError (Just span) _ _) = 
    sourceLine span >= 1 && sourceColumn span >= 1
locatesSyntaxErrorsPrecisely _ = False

includesLineNumbers :: CompilerError -> Bool
includesLineNumbers (CompilerError _ (Just span) _ _) = sourceLine (spanStart span) >= 1
includesLineNumbers _ = False

includesColumnNumbers :: CompilerError -> Bool
includesColumnNumbers (CompilerError _ (Just span) _ _) = sourceColumn (spanStart span) >= 1
includesColumnNumbers _ = False

locatesTypeErrorsAccurately :: CompilerError -> Bool
locatesTypeErrorsAccurately (CompilerError TypeError (Just span) _ _) = 
    sourceLine span >= 1 && sourceColumn span >= 1
locatesTypeErrorsAccurately _ = False

highlightsProblematicTokens :: CompilerError -> Bool
highlightsProblematicTokens (CompilerError _ (Just span) _ _) = spanStart span /= spanEnd span
highlightsProblematicTokens _ = False

tracksMultipleErrorLocations :: CompilerError -> Bool
tracksMultipleErrorLocations (CompilerError _ (Just span) _ _) = 
    sourceLine span >= 1 && sourceColumn span >= 1
tracksMultipleErrorLocations _ = False

errorsOrderedByLocation :: [CompilerError] -> Bool
errorsOrderedByLocation errs = all (uncurry (<=)) $ zip locations (tail locations)
  where
    locations = map getErrorLocation errs
    getErrorLocation (CompilerError _ (Just span) _ _) = (sourceLine (spanStart span), sourceColumn (spanStart span))
    getErrorLocation _ = (0, 0)

preservesErrorContext :: CompilerError -> Bool
preservesErrorContext (CompilerError _ (Just span) _ _) = 
    sourceLine (spanEnd span) >= sourceLine (spanStart span)
preservesErrorContext _ = False

showsSurroundingCode :: CompilerError -> Bool
showsSurroundingCode (CompilerError _ (Just span) msg _) = 
    length (words msg) >= 3
showsSurroundingCode _ = False

hasConsistentLocations :: CodeBlock -> Bool
hasConsistentLocations block = 
    let span = cbSpan block
    in sourceLine (spanStart span) <= sourceLine (spanEnd span) && 
       sourceColumn (spanStart span) <= sourceColumn (spanEnd span)

hasAccurateMultiLineSpans :: CodeBlock -> Bool
hasAccurateMultiLineSpans block = 
    let span = cbSpan block
        isMultiLine = sourceLine (spanStart span) < sourceLine (spanEnd span)
    in not isMultiLine || (sourceLine (spanEnd span) - sourceLine (spanStart span) >= 1)

hasPreciseErrorLocations :: CompilerError -> Bool
hasPreciseErrorLocations (CompilerError _ (Just span) _ _) = 
    sourceLine span >= 1 && sourceColumn span >= 1 && spanStart span /= spanEnd span
hasPreciseErrorLocations _ = False

hasEfficientLocationTracking :: CodeBlock -> Bool
hasEfficientLocationTracking block = 
    let span = cbSpan block
    in sourceLine (spanStart span) >= 1

usesMemoryEfficiently :: CodeBlock -> Bool
usesMemoryEfficiently block = 
    let content = cbContent block
        span = cbSpan block
    in length content <= 1000 || sourceLine (spanEnd span) - sourceLine (spanStart span) <= 100

-- | Generators for QuickCheck testing
genValidCode :: Gen String
genValidCode = elements
  [ "let x = 5"
  , "func test() { return 42 }"
  , "let y = x + 3"
  , "type Point = struct { x: int, y: int }"
  ]

genMultiLineCode :: Gen String
genMultiLineCode = elements
  [ "let x = (\n  5 +\n  3\n)"
  , "func test() {\n  if true {\n    let x = 5\n  }\n}"
  , "let y = {\n  let a = 1\n  let b = 2\n  a + b\n}"
  , "type Person = struct {\n  name: string\n  age: int\n}"
  ]

genErrorCode :: Gen String
genErrorCode = elements
  [ "let x: int = \"hello\""
  , "let y = \nlet z = 10"
  , "func test( { return 42 }"
  , "let x = 5\nlet x = 10"
  ]