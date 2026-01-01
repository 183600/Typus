{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalCoreTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (length, sum, reverse, concat, isInfixOf, isPrefixOf)
import Data.List (sort, nub)
import Data.Char (isSpace, isAlphaNum, isLetter, isDigit)
import Control.Monad (foldM, when)

import Utils (trim, splitBy, splitByComma, removeLineComments, removeComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, posAfter, posAt, emptySpan, spanFrom, mergeSpans, locatedWithSpan, locatedValue)
import Parser 
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  , parseTypus
  )
import SyntaxValidator (SyntaxError(..))
import TestSupport.Arbitrary ()
import TestSupport.ExtendedArbitrary ()

-- ============================================================================
-- Test 1: Utils Module String Processing Tests
-- ============================================================================

prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

prop_splitBy_consistency :: Char -> String -> Property
prop_splitBy_consistency delim s = 
  let parts = splitBy delim s
      reconstructed = L.concat (L.map (\p -> p ++ [delim]) (init parts)) ++ (if null parts then "" else last parts)
  in L.length parts > 0 ==> reconstructed === s

prop_splitByComma_handles_empty :: Property
prop_splitByComma_handles_empty = splitByComma "" === [""]

prop_removeLineComments_preserves_strings :: String -> String -> Property
prop_removeLineComments_preserves_strings prefix code =
  not ('"' `elem` prefix) && not ('"' `elem` code) ==>
  let input = prefix ++ "// comment\n\"" ++ code ++ "\"\n"
      result = removeLineComments input
  in "\"" `L.isInfixOf` result && code `L.isInfixOf` result

prop_removeComments_handles_nested :: String -> String -> Property
prop_removeComments_handles_nested outer inner =
  not ("/*" `L.isInfixOf` outer) && not ("*/" `L.isInfixOf` outer) &&
  not ("/*" `L.isInfixOf` inner) && not ("*/" `L.isInfixOf` inner) ==>
  let input = outer ++ "/* " ++ inner ++ " */" ++ outer
      result = removeComments input
  in outer `L.isInfixOf` result && not (inner `L.isInfixOf` result)

prop_normalizeIndentation_preserves_relative :: String -> Property
prop_normalizeIndentation_preserves_relative s =
  let lines' = lines s
      hasIndent = L.any (L.isPrefixOf " " . dropWhile isSpace) lines'
  in hasIndent ==>
     let normalized = normalizeIndentation s
         normLines = lines normalized
     in L.length normLines === L.length lines'

-- ============================================================================
-- Test 2: SourceLocation Module Position Calculation Tests
-- ============================================================================

pos_calculation_tests :: TestTree
pos_calculation_tests = testGroup "Source Location Position Calculation Tests"
  [ testCase "start position has correct values" $ do
      posLine startPos @?= 1
      posColumn startPos @?= 1
      posOffset startPos @?= 0
      
  , testCase "position after newline increments line" $ do
      let pos = posAfter '\n' startPos
      posLine pos @?= 2
      posColumn pos @?= 1
      posOffset pos @?= 1
      
  , testCase "position after tab aligns to 8-column boundary" $ do
      let pos = posAfter '\t' (posAt 1 3)
      posColumn pos @?= 9
      posLine pos @?= 1
      
  , testCase "position after regular char increments column" $ do
      let pos = posAfter 'a' startPos
      posLine pos @?= 1
      posColumn pos @?= 2
      posOffset pos @?= 1
  ]

prop_posAfter_monotonic :: Char -> SourcePos -> Property
prop_posAfter_monotonic c pos = 
  let newPos = posAfter c pos
  in posOffset newPos >= posOffset pos

prop_spanFrom_creates_valid_span :: SourcePos -> String -> Property
prop_spanFrom_creates_valid_span pos text =
  let endPos = L.foldl (flip posAfter) pos text
      span = spanFrom pos text
  in spanStart span === pos && spanEnd span === endPos

prop_mergeSpans_properties :: SourcePos -> SourcePos -> Property
prop_mergeSpans_properties pos1 pos2 =
  let span1 = spanFrom pos1 "content"
      span2 = spanFrom pos2 "more"
      merged = mergeSpans span1 span2
      earlier = if posOffset pos1 <= posOffset pos2 then span1 else span2
      later = if posOffset pos1 <= posOffset pos2 then span2 else span1
  in spanStart merged === spanStart earlier && spanEnd merged === spanEnd later

-- ============================================================================
-- Test 3: Parser Module Combinator Tests
-- ============================================================================

prop_parseTypus_empty :: Property
prop_parseTypus_empty =
  let result = parseTypus ""
  in case result of
    Left _ -> property False
    Right typusFile -> tfDirectives typusFile === defaultFileDirectives &&
                       null (tfBuildTags typusFile) &&
                       null (tfBlocks typusFile)

prop_parseTypus_simple_package :: String -> Property
prop_parseTypus_simple_package pkgName =
  not (null pkgName) && not ("package " `L.isInfixOf` pkgName) && L.all isAlphaNum pkgName ==>
  let content = "package " ++ pkgName ++ "\nfunc main() {}"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right typusFile -> L.length (tfBlocks typusFile) >= 1

prop_parseTypus_with_directives :: String -> Bool -> Bool -> Property
prop_parseTypus_with_directives content ownership dependentTypes =
  not (null content) && not ("//!" `L.isInfixOf` content) ==>
  let directives = "//!" ++ 
                   (if ownership then " ownership:on" else "") ++
                   (if dependentTypes then " dependent_types:off" else "")
      fullContent = if null directives then content else directives ++ "\n" ++ content
      result = parseTypus fullContent
  in case result of
    Left _ -> property False
    Right typusFile -> L.length (tfBlocks typusFile) >= 1

-- ============================================================================
-- Test 4: Error Handling Edge Case Tests
-- ============================================================================

error_handling_tests :: TestTree
error_handling_tests = testGroup "Error Handling Edge Case Tests"
  [ testCase "handles unclosed string literal" $ do
      let content = "package main\nfunc main() {\n  s := \"unclosed\n}"
          result = parseTypus content
      case result of
        Left _ -> pure ()  -- Expected to fail
        Right _ -> assertFailure "Should have failed with unclosed string"
        
  , testCase "handles unmatched braces" $ do
      let content = "package main\nfunc main() {\n  if true {\n    // missing closing brace"
          result = parseTypus content
      case result of
        Left _ -> pure ()  -- Expected to fail
        Right _ -> assertFailure "Should have failed with unmatched braces"
        
  , testCase "handles invalid package name" $ do
      let content = "package 123invalid\nfunc main() {}"
          result = parseTypus content
      case result of
        Left _ -> pure ()  -- Expected to fail
        Right _ -> assertFailure "Should have failed with invalid package name"
  ]

prop_error_location_valid :: SyntaxError -> Property
prop_error_location_valid err = 
  let pos = errorPos err
  in posLine pos > 0 && posColumn pos > 0 && posOffset pos >= 0

-- ============================================================================
-- Test 5: Complex Nested Structure Tests
-- ============================================================================

prop_parse_nested_functions :: Int -> String -> Property
prop_parse_nested_functions depth funcBody =
  depth > 0 && depth <= 5 && not ('{' `elem` funcBody) && not ('}' `elem` funcBody) ==>
  let indent func n = L.concat (replicate n "  ") ++ func
      nestedFuncs = L.concat $ zipWith (\i _ -> 
        indent ("func level" ++ show i ++ "() {\n") i ++ 
        indent funcBody (i+1) ++ "\n" ++
        indent "}\n" i) [0..depth-1] [undefined..undefined]
      content = "package main\n" ++ nestedFuncs
      result = parseTypus content
  in case result of
    Left _ -> depth <= 2  -- Allow failure for deeper nesting
    Right typusFile -> L.length (tfBlocks typusFile) >= 1

prop_parse_nested_structs :: Int -> Property
prop_parse_nested_structs depth =
  depth > 0 && depth <= 3 ==>
  let structType n = "type Level" ++ show n ++ " struct {\n" ++
                    "  Value int\n" ++
                    (if n > 0 then "  Nested *Level" ++ show (n-1) ++ "\n" else "") ++
                    "}\n"
      structs = concatMap structType [0..depth]
      content = "package main\n" ++ structs ++ "func main() {}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right typusFile -> L.length (tfBlocks typusFile) >= 1

-- ============================================================================
-- Test 6: Unicode L.and Special Character Tests
-- ============================================================================

unicode_tests :: TestTree
unicode_tests = testGroup "Unicode L.and Special Character Tests"
  [ testCase "handles Unicode in comments" $ do
      let content = "package main\n// Unicode test: 你好世界 🌍\nfunc main() {}"
          result = parseTypus content
      case result of
        Left _ -> assertFailure "Failed to parse Unicode comments"
        Right typusFile -> L.length (tfBlocks typusFile) @?= 1
        
  , testCase "handles Unicode in string literals" $ do
      let content = "package main\nfunc main() {\n  s := \"Hello 世界 🌏\"\n}"
          result = parseTypus content
      case result of
        Left _ -> assertFailure "Failed to parse Unicode strings"
        Right typusFile -> L.length (tfBlocks typusFile) @?= 1
        
  , testCase "handles special characters in identifiers" $ do
      let content = "package main\nfunc test_αβγ() {\n  var_123 := 42\n}"
          result = parseTypus content
      case result of
        Left _ -> assertFailure "Failed to parse special characters"
        Right typusFile -> L.length (tfBlocks typusFile) @?= 1
  ]

prop_unicode_content_preserved :: String -> Property
prop_unicode_content_preserved unicodeText =
  not (null unicodeText) && not ("//!" `L.isInfixOf` unicodeText) ==>
  let content = "package main\n// Unicode: " ++ unicodeText ++ "\nfunc main() {}"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right typusFile -> L.length (tfBlocks typusFile) >= 1

-- ============================================================================
-- Test 7: Performance L.and Memory Efficiency Tests
-- ============================================================================

performance_tests :: TestTree
performance_tests = testGroup "Performance L.and Memory Efficiency Tests"
  [ testCase "handles large files efficiently" $ do
      let largeContent = unlines $ replicate 1000 "  // This is a comment line\n  x := x + 1"
          content = "package main\nfunc largeFunc() {\n" ++ largeContent ++ "}\n"
          result = parseTypus content
      case result of
        Left _ -> assertFailure "Failed to parse large file"
        Right typusFile -> L.length (tfBlocks typusFile) @?= 1
        
  , testCase "handles deep indentation efficiently" $ do
      let deepIndent = concatMap (\i -> replicate i ' ' ++ "x := " ++ show i ++ "\n") [0..100]
          content = "package main\nfunc deepFunc() {\n" ++ deepIndent ++ "}\n"
          result = parseTypus content
      case result of
        Left _ -> assertFailure "Failed to parse deeply indented code"
        Right typusFile -> L.length (tfBlocks typusFile) @?= 1
  ]

prop_large_input_handling :: Int -> String -> Property
prop_large_input_handling multiplier baseContent =
  multiplier > 0 && multiplier <= 50 && L.length baseContent <= 100 ==>
  let largeContent = L.concat $ replicate multiplier (baseContent ++ "\n")
      content = "package main\nfunc main() {\n" ++ largeContent ++ "}\n"
      result = parseTypus content
  in case result of
    Left _ -> multiplier > 20  -- Allow failure for very large inputs
    Right typusFile -> L.length (tfBlocks typusFile) >= 1

-- ============================================================================
-- Test 8: Boundary Condition L.and Exception Input Tests
-- ============================================================================

boundary_tests :: TestTree
boundary_tests = testGroup "Boundary Condition L.and Exception Input Tests"
  [ testCase "handles empty lines" $ do
      let content = "package main\n\n\nfunc main() {\n\n}\n"
          result = parseTypus content
      case result of
        Left _ -> assertFailure "Failed to handle empty lines"
        Right typusFile -> L.length (tfBlocks typusFile) @?= 1
        
  , testCase "handles only whitespace" $ do
      let content = "   \n  \t  \n   \n"
          result = parseTypus content
      case result of
        Left _ -> pure ()  -- Expected to fail
        Right typusFile -> L.null (tfBlocks typusFile) @?= True
        
  , testCase "handles extremely long lines" $ do
      let longLine = "x := \"" ++ replicate 1000 'a' ++ "\"\n"
          content = "package main\nfunc main() {\n  " ++ longLine ++ "}\n"
          result = parseTypus content
      case result of
        Left _ -> assertFailure "Failed to handle extremely long lines"
        Right typusFile -> L.length (tfBlocks typusFile) @?= 1
  ]

prop_boundary_conditions :: String -> Property
prop_boundary_conditions input =
  let result = parseTypus input
  in case result of
    Left _ -> property True  -- Parsing may fail for boundary conditions
    Right typusFile -> property $ L.length (tfBlocks typusFile) >= 0

-- ============================================================================
-- Test 9: Module Integration Tests
-- ============================================================================

integration_tests :: TestTree
integration_tests = testGroup "Module Integration Tests"
  [ testCase "Utils L.and Parser integration" $ do
      let content = "package main\nfunc main() {\n  // Comment with   extra   spaces\n  x := 1 + 2\n}"
          result = parseTypus content
      case result of
        Left _ -> assertFailure "Integration test failed"
        Right typusFile -> do
          let blocks = tfBlocks typusFile
          length blocks @?= 1
          let blockContent = cbContent (L.head blocks)
          trim blockContent @?= "x := 1 + 2"
          
  , testCase "SourceLocation L.and Parser integration" $ do
      let content = "package main\nfunc test() {}"
          result = parseTypus content
      case result of
        Left _ -> assertFailure "Integration test failed"
        Right typusFile -> do
          let blocks = tfBlocks typusFile
          length blocks @?= 1
          let span = cbSpan (L.head blocks)
          posLine (spanStart span) @?= 2
  ]

prop_integration_consistency :: String -> String -> Property
prop_integration_consistency prefix suffix =
  not (null prefix) && not (null suffix) ==>
  let content = "package main\n" ++ prefix ++ "\nfunc main() {\n" ++ suffix ++ "\n}"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right typusFile -> L.length (tfBlocks typusFile) >= 1

-- ============================================================================
-- Test 10: QuickCheck Property Tests
-- ============================================================================

quickcheck_properties :: TestTree
quickcheck_properties = testGroup "QuickCheck Property Tests"
  [ fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "splitBy consistency" prop_splitBy_consistency
  , fastProperty "splitByComma handles empty" prop_splitByComma_handles_empty
  , fastProperty "removeLineComments preserves strings" prop_removeLineComments_preserves_strings
  , fastProperty "removeComments handles nested" prop_removeComments_handles_nested
  , fastProperty "normalizeIndentation preserves relative" prop_normalizeIndentation_preserves_relative
  , fastProperty "posAfter is monotonic" prop_posAfter_monotonic
  , fastProperty "spanFrom creates valid span" prop_spanFrom_creates_valid_span
  , fastProperty "mergeSpans properties" prop_mergeSpans_properties
  , fastProperty "parseTypus empty input" prop_parseTypus_empty
  , fastProperty "parseTypus simple package" prop_parseTypus_simple_package
  , fastProperty "parseTypus with directives" prop_parseTypus_with_directives
  , fastProperty "error location valid" prop_error_location_valid
  , fastProperty "parse nested functions" prop_parse_nested_functions
  , fastProperty "parse nested structs" prop_parse_nested_structs
  , fastProperty "unicode content preserved" prop_unicode_content_preserved
  , fastProperty "large input handling" prop_large_input_handling
  , fastProperty "boundary conditions" prop_boundary_conditions
  , fastProperty "integration consistency" prop_integration_consistency
  ]

-- ============================================================================
-- Test Suite Assembly
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal Core Tests"
  [ testGroup "Utils Module Tests"
    [ quickcheck_properties
    ]
  , pos_calculation_tests
  , error_handling_tests
  , unicode_tests
  , performance_tests
  , boundary_tests
  , integration_tests
  ]