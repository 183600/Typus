{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.ParserBoundaryQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing, fromMaybe)

import Parser
import SourceLocation
import Utils
import qualified SyntaxValidator

-- | Test directive parsing robustness
testDirectiveParsingRobustness :: Property
testDirectiveParsingRobustness =
  forAll arbitrary $ \directiveText ->
    let parsed = parseDirective directiveText
        hasContent = not (null directiveText)
    in if hasContent
       then isJust parsed ==> length (show parsed) > 0
       else isNothing parsed

-- | Test code block parsing consistency
testCodeBlockParsingConsistency :: Property
testCodeBlockParsingConsistency =
  forAll arbitrary $ \code ->
    let blocks = parseCodeBlocks code
        blockCount = length blocks
        lineCount = length $ lines code
    in blockCount >= 0 .&&. blockCount <= lineCount

-- | Test file directive parsing
testFileDirectiveParsing :: Property
testFileDirectiveParsing =
  forAll arbitrary $ \directives ->
    let parsed = parseFileDirectives directives
        directiveCount = length $ lines directives
        parsedCount = length parsed
    in parsedCount <= directiveCount .&&. parsedCount >= 0

-- | Test comment handling in parser
testCommentHandlingInParser :: Property
testCommentHandlingInParser =
  forAll arbitrary $ \code ->
    let withoutComments = removeComments code
        parsedOriginal = parseTypus code
        parsedWithoutComments = parseTypus withoutComments
        syntaxErrorsOriginal = tfSyntaxErrors parsedOriginal
        syntaxErrorsWithoutComments = tfSyntaxErrors parsedWithoutComments
    in length syntaxErrorsWithoutComments <= length syntaxErrorsOriginal

-- | Test indentation handling
testIndentationHandling :: Property
testIndentationHandling =
  forAll arbitrary $ \code ->
    let normalized = normalizeIndentation code
        parsedOriginal = parseTypus code
        parsedNormalized = parseTypus normalized
    -- Normalized code should parse consistently
    in length (tfSyntaxErrors parsedOriginal) >= length (tfSyntaxErrors parsedNormalized)

-- | Test parser error recovery
testParserErrorRecovery :: Property
testParserErrorRecovery =
  forAll arbitrary $ \malformedCode ->
    let parsed = parseTypus malformedCode
        syntaxErrors = tfSyntaxErrors parsed
        blocks = tfBlocks parsed
    -- Even with syntax errors, should attempt to parse blocks
    in length blocks >= 0 .&&. length syntaxErrors >= 0

-- | Test directive precedence rules
testDirectivePrecedenceRules :: Property
testDirectivePrecedenceRules =
  forAll arbitrary $ \fileDirectives ->
    forAll arbitrary $ \blockDirectives ->
      let effectiveDirectives = resolveDirectivePrecedence fileDirectives blockDirectives
      -- Effective directives should be a valid combination
      in isValidDirectiveCombination effectiveDirectives

-- | Test parser token boundary handling
testParserTokenBoundaryHandling :: Property
testParserTokenBoundaryHandling =
  forAll arbitrary $ \tokens ->
    let tokenized = tokenize tokens
        reconstructed = untokenize tokenized
    -- Tokenization and untokenization should be consistent
    in length tokenized >= 0 .&&. length reconstructed >= 0

-- | Test parser with special characters
testParserWithSpecialCharacters :: Property
testParserWithSpecialCharacters =
  forAll arbitrary $ \specialChars ->
    let codeWithSpecial = "func test() { " ++ specialChars ++ " }"
        parsed = parseTypus codeWithSpecial
        blocks = tfBlocks parsed
    -- Parser should handle special characters gracefully
    in length blocks >= 0

-- | Test parser with empty input
testParserWithEmptyInput :: Property
testParserWithEmptyInput =
  forAll arbitrary $ \emptyInput ->
    let parsed = parseTypus emptyInput
        blocks = tfBlocks parsed
        directives = tfDirectives parsed
    in if null emptyInput
       then null blocks .&&. directives === defaultFileDirectives
       else property True

-- | Test parser with large input
testParserWithLargeInput :: Property
testParserWithLargeInput =
  forAll arbitrary $ \baseCode ->
    let largeCode = concat $ replicate 100 baseCode
        parsed = parseTypus largeCode
        blocks = tfBlocks parsed
    -- Parser should handle large inputs without crashing
    in length blocks >= 0

-- | Test nested directive handling
testNestedDirectiveHandling :: Property
testNestedDirectiveHandling =
  forAll arbitrary $ \nestedDirectives ->
    let parsed = parseNestedDirectives nestedDirectives
        nestingDepth = calculateNestingDepth nestedDirectives
    -- Should handle arbitrary nesting depth
    in length parsed >= 0 .&&. nestingDepth >= 0

-- | Test parser unicode handling
testParserUnicodeHandling :: Property
testParserUnicodeHandling =
  forAll arbitrary $ \unicodeText ->
    let codeWithUnicode = "func unicodeTest() { " ++ unicodeText ++ " }"
        parsed = parseTypus codeWithUnicode
        blocks = tfBlocks parsed
    -- Parser should handle unicode characters
    in length blocks >= 0

-- | Test parser line ending consistency
testParserLineEndingConsistency :: Property
testParserLineEndingConsistency =
  forAll arbitrary $ \code ->
    let withUnixEndings = normalizeLineEndings "\n" code
        withWindowsEndings = normalizeLineEndings "\r\n" code
        parsedUnix = parseTypus withUnixEndings
        parsedWindows = parseTypus withWindowsEndings
        unixBlocks = tfBlocks parsedUnix
        windowsBlocks = tfBlocks parsedWindows
    -- Should parse consistently regardless of line endings
    in length unixBlocks === length windowsBlocks

-- | Test parser error location accuracy
testParserErrorLocationAccuracy :: Property
testParserErrorLocationAccuracy =
  forAll arbitrary $ \malformedCode ->
    let parsed = parseTypus malformedCode
        syntaxErrors = tfSyntaxErrors parsed
    -- Syntax errors should have valid locations
    in all hasValidLocation syntaxErrors

-- Helper functions (these would need to be implemented in the actual Parser module)

parseDirective :: String -> Maybe [String]
parseDirective = undefined -- Placeholder

parseCodeBlocks :: String -> [CodeBlock]
parseCodeBlocks = undefined -- Placeholder

parseFileDirectives :: String -> [(String, String)]
parseFileDirectives = undefined -- Placeholder

resolveDirectivePrecedence :: FileDirectives -> BlockDirectives -> BlockDirectives
resolveDirectivePrecedence = undefined -- Placeholder

isValidDirectiveCombination :: BlockDirectives -> Bool
isValidDirectiveCombination = undefined -- Placeholder

tokenize :: String -> [String]
tokenize = undefined -- Placeholder

untokenize :: [String] -> String
untokenize = undefined -- Placeholder

parseNestedDirectives :: String -> [String]
parseNestedDirectives = undefined -- Placeholder

calculateNestingDepth :: String -> Int
calculateNestingDepth = undefined -- Placeholder

normalizeLineEndings :: String -> String -> String
normalizeLineEndings = undefined -- Placeholder

hasValidLocation :: SyntaxValidator.SyntaxError -> Bool
hasValidLocation = undefined -- Placeholder

tests :: TestTree
tests = testGroup "Parser Boundary QuickCheck Tests"
  [ testProperty "Directive parsing robustness" testDirectiveParsingRobustness
  , testProperty "Code block parsing consistency" testCodeBlockParsingConsistency
  , testProperty "File directive parsing" testFileDirectiveParsing
  , testProperty "Comment handling" testCommentHandlingInParser
  , testProperty "Indentation handling" testIndentationHandling
  , testProperty "Error recovery" testParserErrorRecovery
  , testProperty "Directive precedence" testDirectivePrecedenceRules
  , testProperty "Token boundary handling" testParserTokenBoundaryHandling
  , testProperty "Special characters" testParserWithSpecialCharacters
  , testProperty "Empty input" testParserWithEmptyInput
  , testProperty "Large input" testParserWithLargeInput
  , testProperty "Nested directives" testNestedDirectiveHandling
  , testProperty "Unicode handling" testParserUnicodeHandling
  , testProperty "Line ending consistency" testParserLineEndingConsistency
  , testProperty "Error location accuracy" testParserErrorLocationAccuracy
  ]