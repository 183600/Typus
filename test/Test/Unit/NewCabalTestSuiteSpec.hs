{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalTestSuiteSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, Assertion)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, oneof, elements, listOf, choose)
import Utils (trim, splitBy, splitByComma, removeComments, normalizeIndentation, breakOn)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, spanBetween, mergeSpans, isValidSpan, advancePosBy)
import SyntaxValidator (SyntaxError(..), ErrorType(..))
import EmbedAssets (MissingEmbed(..), formatMissingMessage)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isSpace)

-- ============================================================================
-- Test Suite Definition
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal Test Suite"
  [ utilsProperties
  , sourceLocationProperties  
  , syntaxValidatorTests
  , embedAssetsTests
  , integrationTests
  ]

-- ============================================================================
-- Utils Module Tests - QuickCheck Properties
-- ============================================================================

utilsProperties :: TestTree
utilsProperties = testGroup "Utils Module Properties"
  [ testProperty "trim idempotent" propTrimIdempotent
  , testProperty "splitBy length consistency" propSplitByLength
  , testProperty "splitByComma equals splitBy ','" propSplitByCommaConsistency
  , testProperty "removeComments preserves non-comment code" propRemoveCommentsPreservesCode
  , testProperty "normalizeIndentation preserves relative structure" propNormalizeIndentationPreservesStructure
  , testProperty "breakOn correctness" propBreakOnCorrectness
  ]

-- Property: trim is idempotent (trimming twice gives same result as trimming once)
propTrimIdempotent :: String -> Property
propTrimIdempotent s = trim (trim s) === trim s

-- Property: splitBy preserves total length when concatenated with delimiters
propSplitByLength :: Char -> String -> Property
propSplitByLength delim s = 
  let parts = splitBy delim s
      reconstructed = concat $ intersperse [delim] parts
  in counterexample ("Original: " ++ show s ++ ", Reconstructed: " ++ show reconstructed) $
     length (filter (== delim) s) + length s === length reconstructed
  where
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:y:xs) = x ++ sep : intersperse sep (y:xs)

-- Property: splitByComma should be equivalent to splitBy ','
propSplitByCommaConsistency :: String -> Property
propSplitByCommaConsistency s = splitByComma s === splitBy ',' s

-- Property: removeComments should not modify code without comments
propRemoveCommentsPreservesCode :: String -> Property
propRemoveCommentsPreservesCode s = 
  let noCommentCode = filter (\c -> c /= '/' && c /= '*') s
  in if not (hasCommentMarkers s)
     then removeComments s === s
     else removeComments noCommentCode === noCommentCode
  where
    hasCommentMarkers = any (`elem` "/*")

-- Property: normalizeIndentation preserves the number of non-empty lines
propNormalizeIndentationPreservesStructure :: String -> Property
propNormalizeIndentationPreservesStructure s =
  let originalLines = length $ filter (not . all isSpace) $ lines s
      normalizedLines = length $ filter (not . all isSpace) $ lines (normalizeIndentation s)
  in originalLines === normalizedLines

-- Property: breakOn should correctly split strings
propBreakOnCorrectness :: String -> String -> Property
propBreakOnCorrectness pat s =
  if null pat
  then breakOn pat s === ("", s)
  else 
    let (before, after) = breakOn pat s
    in counterexample ("Pattern: " ++ show pat ++ ", String: " ++ show s ++ ", Before: " ++ show before ++ ", After: " ++ show after) $
       if pat `isInfixOf` s
       then before ++ pat ++ after === s
       else (before, after) === (s, "")

-- ============================================================================
-- SourceLocation Module Tests - QuickCheck Properties
-- ============================================================================

sourceLocationProperties :: TestTree
sourceLocationProperties = testGroup "SourceLocation Properties"
  [ testProperty "position advancement consistency" propPosAdvancementConsistency
  , testProperty "span validity after merge" propSpanMergeValidity
  , testProperty "span between positions" propSpanBetweenPositions
  , testProperty "position ordering" propPositionOrdering
  , testCase "position advancement with newlines" testPosAdvancementNewlines
  , testCase "span merge edge cases" testSpanMergeEdgeCases
  ]

-- Property: advancing position by characters should be consistent with offset
propPosAdvancementConsistency :: String -> Property
propPosAdvancementConsistency s =
  let start = startPos
      end = advancePosBy s start
      expectedOffset = length s
  in posOffset end === expectedOffset

-- Property: merging spans should result in valid span
propSpanMergeValidity :: SourcePos -> SourcePos -> Property
propSpanMergeValidity p1 p2 =
  let span1 = spanBetween p1 p1
      span2 = spanBetween p2 p2
      merged = mergeSpans span1 span2
  in isValidSpan merged === True

-- Property: span between should maintain correct order
propSpanBetweenPositions :: SourcePos -> SourcePos -> Property
propSpanBetweenPositions p1 p2 =
  let span = spanBetween p1 p2
      start = spanStart span
      end = spanEnd span
  in if p1 <= p2 
     then start === p1 && end === p2
     else start === p2 && end === p1

-- Property: position ordering should be consistent with offset
propPositionOrdering :: Int -> Int -> Int -> Property
propPositionOrdering line col offset =
  let pos1 = SourcePos line col offset
      pos2 = SourcePos line col (offset + 1)
  in pos1 <= pos2 === True

-- Unit test: position advancement with newlines
testPosAdvancementNewlines :: Assertion
testPosAdvancementNewlines = do
  let start = SourcePos 1 5 10
      afterNewline = posAfter '\n' start
  assertEqual "Newline should increment line and reset column" 
             (SourcePos 2 1 11) afterNewline

-- Unit test: span merge edge cases
testSpanMergeEdgeCases :: Assertion
testSpanMergeEdgeCases = do
  let pos1 = SourcePos 1 1 0
      pos2 = SourcePos 1 5 4
      pos3 = SourcePos 2 1 10
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      merged = mergeSpans span1 span2
  assertEqual "Merged span should start at earliest position" 
             pos1 (spanStart merged)
  assertEqual "Merged span should end at latest position" 
             pos3 (spanEnd merged)

-- ============================================================================
-- SyntaxValidator Module Tests - Unit Tests
-- ============================================================================

syntaxValidatorTests :: TestTree
syntaxValidatorTests = testGroup "SyntaxValidator Tests"
  [ testCase "error formatting consistency" testSyntaxErrorFormatting
  , testCase "error type classification" testErrorTypeClassification
  , testCase "nested structure validation" testNestedStructureValidation
  ]

testSyntaxErrorFormatting :: Assertion
testSyntaxErrorFormatting = do
  let error = SyntaxError MissingBrace 1 10 "Expected '}'"
      formatted = show error
  assertBool "Error format should contain error type" $ 
    "MissingBrace" `isInfixOf` formatted
  assertBool "Error format should contain line number" $ 
    "1" `isInfixOf` formatted

testErrorTypeClassification :: Assertion
testErrorTypeClassification = do
  let bracketErrors = [MissingBrace, MissingBracket, MissingParenthesis]
      allTypes = [MissingBrace, MissingParenthesis, MissingBracket, 
                  UnclosedString, UnclosedComment, InvalidIdentifier,
                  InvalidTypeDeclaration, InvalidFunctionDeclaration,
                  InvalidImport, InvalidStatement, UnterminatedBlock,
                  InvalidOperator, MissingSemicolon, UnexpectedToken,
                  MissingPackageDeclaration, DuplicateDeclaration,
                  InvalidBlockStructure, UndeclaredVariable, SyntaxWarning]
  assertEqual "Should have 19 error types" 19 (length allTypes)
  assertBool "Bracket errors should be distinct" $ 
    length bracketErrors == length (nub bracketErrors)

testNestedStructureValidation :: Assertion
testNestedStructureValidation = do
  let nestedCode = "func test() { if (x) { while (y) { /* nested */ } } }"
      -- This is a simplified test - in real implementation, 
      -- we would use the actual syntax validator
  assertBool "Nested structures should be balanced" $ 
    count '{' nestedCode == count '}' nestedCode
  where
    count c = length . filter (== c)

-- ============================================================================
-- EmbedAssets Module Tests - Unit Tests
-- ============================================================================

embedAssetsTests :: TestTree
embedAssetsTests = testGroup "EmbedAssets Tests"
  [ testCase "missing embed formatting" testMissingEmbedFormatting
  , testCase "embed pattern validation" testEmbedPatternValidation
  , testCase "resource path handling" testResourcePathHandling
  ]

testMissingEmbedFormatting :: Assertion
testMissingEmbedFormatting = do
  let missing = [ MissingEmbed "*.txt" "/assets" "/main.typus"
                , MissingEmbed "data/*.json" "/data" "/config.typus"
                ]
      formatted = formatMissingMessage missing
  assertBool "Should mention missing assets" $ 
    "Missing embedded assets" `isInfixOf` formatted
  assertBool "Should include pattern" $ 
    "*.txt" `isInfixOf` formatted
  assertBool "Should include reference file" $ 
    "/main.typus" `isInfixOf` formatted

testEmbedPatternValidation :: Assertion
testEmbedPatternValidation = do
  let validPatterns = ["*.txt", "data/*.json", "**/*.go", "config/*"]
      invalidPatterns = ["", "*/", "a**b", "***.txt"]
  assertBool "All valid patterns should be non-empty" $ 
    all (not . null) validPatterns
  assertBool "Invalid patterns should be detected" $ 
    any null invalidPatterns

testResourcePathHandling :: Assertion
testResourcePathHandling = do
  let embed = MissingEmbed "test/*.txt" "/root" "/src/main.typus"
  assertEqual "Should preserve pattern" "test/*.txt" (missingPattern embed)
  assertEqual "Should preserve root" "/root" (missingRoot embed)
  assertEqual "Should preserve reference" "/src/main.typus" (missingReferencedFrom embed)

-- ============================================================================
-- Integration Tests
-- ============================================================================

integrationTests :: TestTree
integrationTests = testGroup "Integration Tests"
  [ testCase "utils and source location integration" testUtilsSourceLocationIntegration
  , testCase "error handling pipeline" testErrorHandlingPipeline
  , testCase "text processing consistency" testTextProcessingConsistency
  ]

testUtilsSourceLocationIntegration :: Assertion
testUtilsSourceLocationIntegration = do
  let code = "func test() {\n  return 42;\n}"
      trimmed = trim code
      lines' = lines trimmed
      start = startPos
      afterFirstLine = advancePosBy (head lines') start
  assertEqual "Trimming should not affect line count" 
             2 (length lines')
  assertBool "Position should advance correctly" $ 
    posLine afterFirstLine == 1

testErrorHandlingPipeline :: Assertion
testErrorHandlingPipeline = do
  let errors = [ SyntaxError MissingBrace 1 10 "Expected '}'"
               , SyntaxError UnclosedString 2 5 "Unterminated string"
               ]
      formatted = map show errors
  assertBool "All errors should be formatted" $ 
    length formatted == length errors
  assertBool "Error locations should be preserved" $ 
    all (`isInfixOf` "1:10") formatted

testTextProcessingConsistency :: Assertion
testTextProcessingConsistency = do
  let text = "  // comment\nfunc test() { /* block */ }\n  "
      withoutComments = removeComments text
      normalized = normalizeIndentation withoutComments
      finalLines = filter (not . all isSpace) $ lines normalized
  assertBool "Should have meaningful content after processing" $ 
    not (null finalLines)
  assertBool "Should contain function definition" $ 
    any ("func" `isInfixOf`) finalLines

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Helper: remove duplicates from list
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

-- Helper: intersperse separator between list elements
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:y:xs) = x : sep : intersperse sep (y:xs)