{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | New Cabal QuickCheck Test Suite
-- Comprehensive property-based tests for core Typus functionality
module Test.Unit.NewCabalQuickCheckTestSuite (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, ioProperty, counterexample, (==>))
import Test.Tasty.HUnit (testCase, (@?=))

import Utils (trim, splitBy, splitByCollapsed, removeLineComments, breakOn, normalizeIndentation)
import SourceLocation (SourcePos(..), startPos, posAfter, advancePos, advancePosByText)
import Compiler.Errors.Core (ErrorSeverity(..), ErrorCategory(..), formatError, errorAt)
import qualified Data.Text as T
import Data.Char (isSpace)
import Data.List (isPrefixOf)

-- ============================================================================
-- QuickCheck Generators
-- ============================================================================

-- Generate strings with various whitespace patterns
genWhitespaceString :: Gen String
genWhitespaceString = listOf $ elements [' ', '\t', '\n', '\r']

-- Generate strings with alphanumeric characters
genAlphaNumString :: Gen String
genAlphaNumString = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- Generate strings that may contain comment markers
genCommentString :: Gen String
genCommentString = do
    base <- genAlphaNumString
    hasLineComment <- arbitrary
    hasBlockComment <- arbitrary
    let withLine = if hasLineComment then base ++ "// comment" else base
    let withBlock = if hasBlockComment then withLine ++ "/* block */" else withLine
    pure withBlock

-- Generate source positions with reasonable values
genSourcePos :: Gen SourcePos
genSourcePos = do
    line <- choose (1, 1000)
    column <- choose (1, 200)
    offset <- choose (0, 10000)
    pure $ SourcePos line column offset

instance Arbitrary SourcePos where
    arbitrary = genSourcePos

-- ============================================================================
-- Utils Module Tests
-- ============================================================================

-- Property: trim is idempotent
prop_trimIdempotent :: String -> Bool
prop_trimIdempotent input =
    let once = trim input
        twice = trim once
    in once == twice

-- Property: trim removes only leading/trailing whitespace
prop_trimOnlyWhitespace :: String -> String -> Bool
prop_trimOnlyWhitespace prefix suffix =
    let input = prefix ++ "hello world" ++ suffix
        trimmed = trim input
    in not (all isSpace prefix) || not (all isSpace suffix) || 
       trimmed == "hello world"

-- Property: splitBy preserves empty segments
prop_splitByPreservesEmpty :: Char -> String -> Bool
prop_splitByPreservesEmpty delim input =
    let result = splitBy delim input
        expectedCount = length (filter (== delim) input) + 1
    in length result == expectedCount

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsedRemovesEmpty :: Char -> String -> Bool
prop_splitByCollapsedRemovesEmpty delim input =
    let result = splitByCollapsed delim input
    in all (not . null) result

-- Property: breakOn returns correct prefix and suffix
prop_breakOnCorrect :: String -> String -> Property
prop_breakOnCorrect pattern text =
    not (null pattern) ==>
    let (prefix, suffix) = breakOn pattern text
        found = pattern `isPrefixOf` text
    in if found
       then prefix ++ pattern ++ suffix == text
       else prefix == text && null suffix

-- Property: removeLineComments preserves non-comment content
prop_removeLineCommentsPreservesContent :: String -> Property
prop_removeLineCommentsPreservesContent input =
    let linesWithoutComments = filter (not . ("//" `isPrefixOf`)) (lines input)
        hasStringLiterals = any ('"' `elem`) linesWithoutComments
    in not hasStringLiterals ==> -- Simple case: no string literals
       let result = removeLineComments input
           resultLines = lines result
       in length resultLines == length linesWithoutComments

-- ============================================================================
-- SourceLocation Module Tests
-- ============================================================================

-- Property: posAfter correctly advances line number for newlines
prop_posAfterNewline :: SourcePos -> Bool
prop_posAfterNewline pos =
    let newPos = posAfter '\n' pos
    in posLine newPos == posLine pos + 1 && posColumn newPos == 1

-- Property: posAfter correctly advances column for regular characters
prop_posAfterRegularChar :: SourcePos -> Char -> Property
prop_posAfterRegularChar pos char =
    char /= '\n' && char /= '\t' ==>
    let newPos = posAfter char pos
    in posLine newPos == posLine pos && 
       posColumn newPos == posColumn pos + 1 &&
       posOffset newPos == posOffset pos + 1

-- Property: advancePosByText processes text correctly
prop_advancePosByText :: SourcePos -> String -> Property
prop_advancePosByText pos text =
    not (null text) ==>
    let finalPos = advancePosByText pos text
        startPos' = pos
    in posOffset finalPos >= posOffset startPos'

-- Property: advancePos is consistent with posAfter
prop_advancePosConsistency :: SourcePos -> Char -> Bool
prop_advancePosConsistency pos char =
    let directResult = posAfter char pos
        advanceResult = advancePos pos char
    in directResult == advanceResult

-- ============================================================================
-- ErrorHandler Module Tests
-- ============================================================================

-- Property: error formatting includes location information
prop_errorFormatIncludesLocation :: SourcePos -> String -> Property
prop_errorFormatIncludesLocation pos message =
    not (null message) ==>
    let error = errorAt pos message
        formatted = formatError error
        posStr = show (posLine pos) ++ ":" ++ show (posColumn pos)
    in posStr `isInfixOf` formatted

-- Property: error messages are preserved in formatting
prop_errorFormatPreservesMessage :: SourcePos -> String -> Property
prop_errorFormatPreservesMessage pos message =
    not (null message) ==>
    let error = errorAt pos message
        formatted = formatError error
    in message `isInfixOf` formatted

-- ============================================================================
-- Integration Tests
-- ============================================================================

-- Property: normalizeIndentation preserves relative structure
prop_normalizeIndentationPreservesStructure :: String -> Property
prop_normalizeIndentationPreservesStructure input =
    let lines' = lines input
        hasMultipleLines = length lines' > 1
        hasContent = any (not . null) lines'
    in hasMultipleLines && hasContent ==>
       let normalized = normalizeIndentation input
           normLines = lines normalized
       in length normLines == length lines'

-- Property: comment removal and indentation normalization commute
prop_commentsAndIndentationCommute :: String -> Property
prop_commentsAndIndentationCommute input =
    let withoutComments = removeLineComments input
        normalizedFirst = normalizeIndentation input
        normalizedThenComments = removeLineComments normalizedFirst
        commentsThenNormalized = normalizeIndentation withoutComments
    in commentsThenNormalized == normalizedThenComments

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal QuickCheck Test Suite"
    [ testGroup "Utils Module Properties"
        [ testProperty "trim is idempotent" prop_trimIdempotent
        , testProperty "trim removes only whitespace" prop_trimOnlyWhitespace
        , testProperty "splitBy preserves empty segments" prop_splitByPreservesEmpty
        , testProperty "splitByCollapsed removes empty segments" prop_splitByCollapsedRemovesEmpty
        , testProperty "breakOn returns correct parts" prop_breakOnCorrect
        , testProperty "removeLineComments preserves content" prop_removeLineCommentsPreservesContent
        ]
    
    , testGroup "SourceLocation Module Properties"
        [ testProperty "posAfter handles newlines correctly" prop_posAfterNewline
        , testProperty "posAfter handles regular characters" prop_posAfterRegularChar
        , testProperty "advancePosByText processes text correctly" prop_advancePosByText
        , testProperty "advancePos consistency with posAfter" prop_advancePosConsistency
        ]
    
    , testGroup "ErrorHandler Module Properties"
        [ testProperty "error format includes location" prop_errorFormatIncludesLocation
        , testProperty "error format preserves message" prop_errorFormatPreservesMessage
        ]
    
    , testGroup "Integration Properties"
        [ testProperty "normalizeIndentation preserves structure" prop_normalizeIndentationPreservesStructure
        , testProperty "comment removal and indentation commute" prop_commentsAndIndentationCommute
        ]
    
    , testGroup "Unit Tests (Sanity Checks)"
        [ testCase "trim basic functionality" $ do
            trim "  hello  " @?= "hello"
            
        , testCase "splitBy basic functionality" $ do
            splitBy ',' "a,b,c" @?= ["a", "b", "c"]
            
        , testCase "breakOn basic functionality" $ do
            breakOn "world" "hello world" @?= ("hello ", "world")
            
        , testCase "SourcePos basic arithmetic" $ do
            let pos = startPos
            let afterA = posAfter 'a' pos
            posColumn afterA @?= 2
        ]
    ]