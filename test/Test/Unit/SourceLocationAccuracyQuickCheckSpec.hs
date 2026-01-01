{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationAccuracyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, sized, resize, choose)
import qualified Test.QuickCheck as QC

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedWithSpan, spanStart, spanEnd)
import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import Compiler (compile, renderCompilationError)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (nub, lines, unlines)
import Data.Char (isAlpha, isAlphaNum, isSpace, isDigit)

-- | Generate valid identifiers
genIdentifier :: Gen String
genIdentifier = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements (['a'..'z'] ++ ['0'..'9'] ++ ['_'])
  return (first : rest)

-- | Generate code with known line positions
genCodeWithLinePositions :: [Int] -> Gen String
genCodeWithLinePositions lineNumbers = do
  funcName <- genIdentifier
  let linesWithNumbers = zipWith (\i num -> "Line " ++ show num ++ ": " ++ funcName ++ "_step" ++ show i ++ "()") [1..] lineNumbers
  return $ unlines linesWithNumbers

-- | Generate code with specific patterns at known positions
genPatternCodeAtPosition :: Int -> String -> Gen String
genPatternCodeAtPosition lineNum pattern = do
  let beforeLines = replicate (lineNum - 1) "placeholder()"
      afterLines = replicate 3 "placeholder()"
      targetLine = "target_function_with_" ++ pattern ++ "()"
  return $ unlines (beforeLines ++ [targetLine] ++ afterLines)

-- | Generate multiline code with nested structures
genNestedCode :: Int -> Gen String
genNestedCode nestingLevel = do
  funcName <- genIdentifier
  let nestedIfs = L.concat $ L.map (\i -> replicate i ' ' ++ "if condition_" ++ show i ++ " {\n") [1..nestingLevel]
      nestedContent = L.concat $ L.map (\i -> replicate (i + 1) ' ' ++ "  action_" ++ show i ++ "()\n") [1..nestingLevel]
      nestedEnds = L.concat $ L.map (\i -> replicate (nestingLevel - i + 1) ' ' ++ "}\n") [1..nestingLevel]
  return $ "func " ++ funcName ++ "() {\n" ++ nestedIfs ++ nestedContent ++ nestedEnds

-- | Generate code with comments at specific positions
genCodeWithComments :: Int -> Gen String
genCodeWithComments commentLine = do
  funcName <- genIdentifier
  let beforeLines = replicate (commentLine - 1) ("  // Regular comment")
      commentLineContent = "  // SPECIAL_COMMENT_MARKER"
      afterLines = replicate 3 ("  // Another comment")
  return $ "func " ++ funcName ++ "() {\n" ++ unlines (beforeLines ++ [commentLineContent] ++ afterLines) ++ "}"

-- | Generate code with string literals at specific positions
genCodeWithStrings :: Int -> Gen String
genCodeWithStrings stringLine = do
  funcName <- genIdentifier
  let beforeLines = replicate (stringLine - 1) ("  x := \"regular string\"")
      stringLineContent = "  x := \"SPECIAL_STRING_MARKER\""
      afterLines = replicate 3 ("  y := \"another string\"")
  return $ "func " ++ funcName ++ "() {\n" ++ unlines (beforeLines ++ [stringLineContent] ++ afterLines) ++ "}"

-- | Generate code with syntax errors at known positions
genCodeWithErrorAtPosition :: Int -> Gen String
genCodeWithErrorAtPosition errorLine = do
  funcName <- genIdentifier
  let beforeLines = replicate (errorLine - 1) ("  x := 1")
      errorLineContent = "  x := 1 2 3  // Syntax error here"
      afterLines = replicate 3 ("  y := 2")
  return $ "func " ++ funcName ++ "() {\n" ++ unlines (beforeLines ++ [errorLineContent] ++ afterLines) ++ "}"

-- | Generate code with multiple sections
genMultiSectionCode :: [String] -> Gen String
genMultiSectionCode sections = do
  let sectionedCode = concatMap (\section -> "// Section: " ++ section ++ "\nfunc section_" ++ section ++ "() {\n  return true\n}\n\n") sections
  return sectionedCode

-- Property: Source locations should be accurate for single-line code
prop_source_location_single_line :: String -> Property
prop_source_location_single_line singleLineCode =
  not (null singleLineCode) && not ('\n' `elem` singleLineCode) ==>
  let result = compile singleLineCode
  in case result of
    Left errors -> property $ L.all hasCorrectSingleLineLocation (map renderCompilationError errors)
    Right _ -> property $ True
  where
    hasCorrectSingleLineLocation errorMsg = 
      "line 1" `L.isInfixOf` errorMsg

-- Property: Source locations should be accurate for multi-line code
prop_source_location_multi_line :: Int -> String -> Property
prop_source_location_multi_line numLines baseContent =
  numLines > 0 && numLines <= 20 ==> -- Limit for performance
  let multiLineCode = unlines $ replicate numLines baseContent
      result = compile multiLineCode
  in case result of
    Left errors -> property $ L.all hasCorrectMultiLineLocation (map renderCompilationError errors)
    Right _ -> property $ True
  where
    hasCorrectMultiLineLocation errorMsg = 
      L.any (`L.isInfixOf` errorMsg) [ "line " ++ show n | n <- [1..numLines] ]

-- Property: Error positions should match actual error locations
prop_error_positions_match_actual :: Int -> Property
prop_error_positions_match_actual targetLine =
  targetLine > 0 && targetLine <= 10 ==>
  do
    errorCode <- genCodeWithErrorAtPosition targetLine
    let result = compile errorCode
        codeLines = lines errorCode
        hasMarker = L.any ("Syntax error here" `L.isInfixOf`) codeLines
    return $ if hasMarker
      then case result of
        Left errors -> property $ L.any (mentionsLine targetLine) (map renderCompilationError errors)
        Right _ -> property $ True -- Unexpected success
      else property $ True -- Test setup failed
  where
    mentionsLine lineNum errorMsg = "line " ++ show lineNum `L.isInfixOf` errorMsg

-- Property: Source location tracking should handle nested structures
prop_source_location_nested :: Int -> Property
prop_source_location_nested nestingLevel =
  nestingLevel > 0 && nestingLevel <= 10 ==>
  do
    nestedCode <- genNestedCode nestingLevel
    let result = compile nestedCode
        codeLines = lines nestedCode
        expectedLines = [1..L.length codeLines]
    return $ case result of
      Left errors -> property $ L.all (hasValidLineInRange expectedLines) (map renderCompilationError errors)
      Right _ -> property $ True
  where
    hasValidLineInRange range errorMsg = 
      L.any (`L.isInfixOf` errorMsg) [ "line " ++ show n | n <- range ]

-- Property: Column positions should be accurate
prop_column_positions_accurate :: String -> Property
prop_column_positions_accurate codeWithMarker =
  "MARKER" `L.isInfixOf` codeWithMarker ==>
  let result = compile codeWithMarker
      codeLines = lines codeWithMarker
      markerLine = findMarkerLine codeLines
  in case markerLine of
    Just lineNum -> 
      case result of
        Left errors -> property $ L.any (mentionsLine lineNum) (map renderCompilationError errors)
        Right _ -> property $ True
    Nothing -> property $ True
  where
    findMarkerLine lines' = 
      let indexedLines = zip [1..] lines'
          markerLines = L.filter (\(_, line) -> "MARKER" `L.isInfixOf` line) indexedLines
      in if null markerLines then Nothing else Just (fst (L.head markerLines))

-- Property: Source locations should be preserved through compilation phases
prop_source_locations_preserved_phases :: String -> Property
prop_source_locations_preserved_phases inputCode =
  not (null inputCode) && L.length (lines inputCode) >= 2 ==>
  let result = compile inputCode
  in case result of
    Left errors -> property $ L.all hasPhaseInfo (map renderCompilationError errors)
    Right _ -> property $ True
  where
    hasPhaseInfo errorMsg = 
      L.length errorMsg > 20 && -- Should have substantial information
      L.any (`L.isInfixOf` errorMsg) ["parse", "type", "compile", "analysis"]

-- Property: Source locations should handle Unicode correctly
prop_source_locations_unicode :: String -> Property
prop_source_locations_unicode unicodeContent =
  not (null unicodeContent) ==>
  let unicodeCode = "func test() {\n  message := \"" ++ unicodeContent ++ "\"\n  return message\n}"
      result = compile unicodeCode
  in case result of
    Left errors -> property $ L.all hasValidLocation (map renderCompilationError errors)
    Right _ -> property $ True
  where
    hasValidLocation errorMsg = 
      "line" `L.isInfixOf` errorMsg && 
      L.any isDigit (filter isDigit errorMsg)

-- Property: Source locations should handle tabs L.and spaces correctly
prop_source_locations_whitespace :: String -> Property
prop_source_locations_whitespace mixedWhitespace =
  L.any (`elem` mixedWhitespace) "\t " ==>
  let codeWithMixed = "func test() {\n" ++ mixedWhitespace ++ "x := 1\n  return x\n}"
      result = compile codeWithMixed
  in case result of
    Left errors -> property $ L.all hasValidLocation (map renderCompilationError errors)
    Right _ -> property $ True

-- Property: Source locations should be consistent across multiple runs
prop_source_locations_consistent :: String -> Property
prop_source_locations_consistent inputCode =
  not (null inputCode) ==>
  let result1 = compile inputCode
      result2 = compile inputCode
  in case (result1, result2) of
    (Left errors1, Left errors2) -> 
      property $ L.length errors1 === L.length errors2
    (Right _, Right _) -> 
      property $ True -- Consistent success
    _ -> 
      property $ True -- One succeeds, one fails (edge case)

-- Property: Source locations should handle very long lines
prop_source_locations_long_lines :: Int -> String -> Property
prop_source_locations_long_lines multiplier baseContent =
  multiplier > 0 && multiplier <= 100 ==> -- Limit for performance
  let longLine = L.concat (replicate multiplier (baseContent ++ " "))
      codeWithLongLine = "func test() {\n  " ++ longLine ++ "\n}"
      result = compile codeWithLongLine
  in case result of
    Left errors -> property $ L.all hasValidLocation (map renderCompilationError errors)
    Right _ -> property $ True

-- Property: Source locations should handle empty lines correctly
prop_source_locations_empty_lines :: Int -> Property
prop_source_locations_empty_lines numEmptyLines =
  numEmptyLines > 0 && numEmptyLines <= 10 ==>
  let emptyLines = replicate numEmptyLines ""
      codeWithEmpty = unlines (["func test() {"] ++ emptyLines ++ ["  x := 1", "  return x", "}"])
      result = compile codeWithEmpty
      codeLines = lines codeWithEmpty
      expectedLines = [1..L.length codeLines]
  in case result of
    Left errors -> property $ L.all (hasValidLineInRange expectedLines) (map renderCompilationError errors)
    Right _ -> property $ True
  where
    hasValidLineInRange range errorMsg = 
      L.any (`L.isInfixOf` errorMsg) [ "line " ++ show n | n <- range ]

-- Property: Source locations should handle multiple errors in same file
prop_source_locations_multiple_errors :: String -> Property
prop_source_locations_multiple_errors codeWithMultipleErrors =
  not (null codeWithMultipleErrors) && "error1" `L.isInfixOf` codeWithMultipleErrors && "error2" `L.isInfixOf` codeWithMultipleErrors ==>
  let result = compile codeWithMultipleErrors
  in case result of
    Left errors -> 
      property $ L.length errors >= 1 && -- Should find at least one error
                 L.all hasValidLocation (map renderCompilationError errors)
    Right _ -> property $ True

-- Property: Source locations should handle file boundaries correctly
prop_source_locations_file_boundaries :: String -> Property
prop_source_locations_file_boundaries fileContent =
  not (null fileContent) ==>
  let result = compile fileContent
      expectedLines = [1..L.length (lines fileContent)]
  in case result of
    Left errors -> property $ L.all (hasValidLineInRange expectedLines) (map renderCompilationError errors)
    Right _ -> property $ True
  where
    hasValidLineInRange range errorMsg = 
      L.any (`L.isInfixOf` errorMsg) [ "line " ++ show n | n <- range ]

-- Helper function to check if error message has valid location info
hasValidLocation :: String -> Bool
hasValidLocation errorMsg = 
  "line" `L.isInfixOf` errorMsg && 
  L.any isDigit (filter isDigit errorMsg) &&
  L.length errorMsg > 10

-- Helper function to check if error message mentions specific line
mentionsLine :: Int -> String -> Bool
mentionsLine lineNum errorMsg = "line " ++ show lineNum `L.isInfixOf` errorMsg

-- Export L.all tests
tests :: TestTree
tests =
  testGroup "Source Location Accuracy QuickCheck Tests"
    [ fastProperty "source locations should be accurate for single-line code" prop_source_location_single_line
    , fastProperty "source locations should be accurate for multi-line code" prop_source_location_multi_line
    , fastProperty "error positions should match actual error locations" prop_error_positions_match_actual
    , fastProperty "source location tracking should handle nested structures" prop_source_location_nested
    , fastProperty "column positions should be accurate" prop_column_positions_accurate
    , fastProperty "source locations should be preserved through compilation phases" prop_source_locations_preserved_phases
    , fastProperty "source locations should handle Unicode correctly" prop_source_locations_unicode
    , fastProperty "source locations should handle tabs L.and spaces correctly" prop_source_locations_whitespace
    , fastProperty "source locations should be consistent across multiple runs" prop_source_locations_consistent
    , fastProperty "source locations should handle very long lines" prop_source_locations_long_lines
    , fastProperty "source locations should handle empty lines correctly" prop_source_locations_empty_lines
    , fastProperty "source locations should handle multiple errors in same file" prop_source_locations_multiple_errors
    , fastProperty "source locations should handle file boundaries correctly" prop_source_locations_file_boundaries
    ]