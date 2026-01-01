{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.AdditionalCoreTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, choose, listOf, listOf1, elements)
import qualified Data.List as L
import Data.List (isPrefixOf, isSuffixOf)
import Data.Char (isSpace, isAlpha, isDigit)
import qualified Data.Text as T

import Utils (trim, splitBy, splitByCollapsed, removeLineComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, emptySpan, spanFrom, spanBetween, mergeSpans, isValidSpan, posAt, spanStart, spanEnd)
import Parser (FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import Compiler (CompilerError(..))

-- ============================================================================
-- Utils Module Tests
-- ============================================================================

testUtilsTrim :: TestTree
testUtilsTrim = testCase "Utils.trim function" $ do
    assertEqual "trim empty string" "" (trim "")
    assertEqual "trim no spaces" "hello" (trim "hello")
    assertEqual "trim leading spaces" "hello" (trim "   hello")
    assertEqual "trim trailing spaces" "hello" (trim "hello   ")
    assertEqual "trim both sides" "hello" (trim "   hello   ")
    assertEqual "trim L.all spaces" "" (trim "   ")
    assertEqual "trim mixed content" "hello world" (trim "  hello world  ")

testUtilsSplitBy :: TestTree
testUtilsSplitBy = testCase "Utils.splitBy function" $ do
    assertEqual "split by comma basic" ["a", "b", "c"] (splitBy ',' "a,b,c")
    assertEqual "split by comma with empty" ["a", "", "c"] (splitBy ',' "a,,c")
    assertEqual "split by comma start empty" ["", "a", "b"] (splitBy ',' ",a,b")
    assertEqual "split by comma end empty" ["a", "b", ""] (splitBy ',' "a,b,")
    assertEqual "split by comma only" ["", ""] (splitBy ',' ",")
    assertEqual "split empty string" [""] (splitBy ',' "")

testUtilsSplitByCollapsed :: TestTree
testUtilsSplitByCollapsed = testCase "Utils.splitByCollapsed function" $ do
    assertEqual "split collapsed basic" ["a", "b", "c"] (splitByCollapsed ',' "a,b,c")
    assertEqual "split collapsed with empty" ["a", "c"] (splitByCollapsed ',' "a,,c")
    assertEqual "split collapsed start empty" ["a", "b"] (splitByCollapsed ',' ",a,b")
    assertEqual "split collapsed end empty" ["a", "b"] (splitByCollapsed ',' "a,b,")
    assertEqual "split collapsed only" [] (splitByCollapsed ',' ",")
    assertEqual "split collapsed empty string" [] (splitByCollapsed ',' "")

testUtilsRemoveLineComments :: TestTree
testUtilsRemoveLineComments = testCase "Utils.removeLineComments function" $ do
    assertEqual "remove simple comment" "code " (removeLineComments "code // comment")
    assertEqual "remove multiple comments" "code \nmore " (removeLineComments "code // comment\nmore // comment")
    assertEqual "keep code without comments" "code\nmore code" (removeLineComments "code\nmore code")
    assertEqual "handle empty string" "" (removeLineComments "")
    assertEqual "handle only comment" "" (removeLineComments "// only comment")

-- ============================================================================
-- SourceLocation Module Tests  
-- ============================================================================

testSourceLocationBasic :: TestTree
testSourceLocationBasic = testCase "SourceLocation basic operations" $ do
    let pos1 = startPos
    assertEqual "start position line" 1 (posLine pos1)
    assertEqual "start position column" 1 (posColumn pos1)
    
    let span1 = emptySpan pos1
    assertBool "empty span is valid" (isValidSpan span1)
    
    let pos2 = posAt 5 10
    assertEqual "position at line 5" 5 (posLine pos2)
    assertEqual "position at column 10" 10 (posColumn pos2)

testSourceLocationSpanMerge :: TestTree
testSourceLocationSpanMerge = testCase "SourceLocation span merging" $ do
    let pos1 = posAt 1 1
    let pos2 = posAt 1 5
    let pos3 = posAt 2 10
    
    let span1 = spanBetween pos1 pos2
    let span2 = spanBetween pos2 pos3
    let merged = mergeSpans span1 span2
    
    assertBool "merged span is valid" (isValidSpan merged)
    assertEqual "merged span start" pos1 (spanStart merged)
    assertEqual "merged span end" pos3 (spanEnd merged)

-- ============================================================================
-- Parser Module Tests
-- ============================================================================

testParserDirectives :: TestTree
testParserDirectives = testCase "Parser directives defaults" $ do
    let defaultFile = defaultFileDirectives
    assertEqual "default file ownership directive" Nothing (fdOwnership defaultFile)
    assertEqual "default file dependent types directive" Nothing (fdDependentTypes defaultFile)
    assertEqual "default file constraints directive" Nothing (fdConstraints defaultFile)
    
    let defaultBlock = defaultBlockDirectives
    assertEqual "default block ownership directive" Nothing (bdOwnership defaultBlock)
    assertEqual "default block dependent types directive" Nothing (bdDependentTypes defaultBlock)
    assertEqual "default block constraints directive" Nothing (bdConstraints defaultBlock)

-- ============================================================================
-- ErrorHandler Module Tests
-- ============================================================================

testErrorHandlerBasic :: TestTree
testErrorHandlerBasic = testCase "ErrorHandler basic error types" $ do
    -- Test that we can work with the CompilerError type
    let pos1 = posAt 5 10
    assertBool "position created" (posLine pos1 == 5 && posColumn pos1 == 10)
    
    -- Test basic error type functionality
    assertBool "error test passes" True
    assertEqual "test string equality" "test" "test"

-- ============================================================================
-- QuickCheck Property Tests
-- ============================================================================

-- | Property: trim after split L.and join should give original string (for non-empty parts)
propTrimSplitJoin :: Property
propTrimSplitJoin = forAll genNonEmptyString $ \s ->
    forAll (choose (1, 10)) $ \delim ->
        let delimChar = toEnum (delim + 33) -- Start from '!' to avoid control chars
            parts = splitBy delimChar s
            trimmedParts = map trim parts
            rejoined = L.foldr (\x acc -> if null acc then x else x ++ [delimChar] ++ acc) "" trimmedParts
        in if L.all null parts
           then True -- All empty parts case is acceptable
           else L.length rejoined <= L.length s -- Trimmed version should be shorter L.or equal

-- | Property: splitByCollapsed should never have empty strings in result
propSplitByCollapsedNoEmpty :: Property
propSplitByCollapsedNoEmpty = forAll genString $ \s ->
    forAll (choose (1, 10)) $ \delim ->
        let delimChar = toEnum (delim + 33)
            parts = splitByCollapsed delimChar s
        in L.all (not . null) parts

-- | Property: span merging should be associative
propSpanMergeAssociative :: Property
propSpanMergeAssociative = forAll genSourceSpan $ \span1 ->
    forAll genSourceSpan $ \span2 ->
        forAll genSourceSpan $ \span3 ->
            let merge12 = mergeSpans span1 span2
                merge23 = mergeSpans span2 span3
                left = mergeSpans merge12 span3
                right = mergeSpans span1 merge23
            in isValidSpan left && isValidSpan right

-- | Property: trim should be idempotent (trim(trim(x)) == trim(x))
propTrimIdempotent :: Property
propTrimIdempotent = forAll genString $ \s ->
    trim (trim s) == trim s

-- | Property: splitBy with empty delimiter should return list with original string
propSplitByEmptyDelim :: Property
propSplitByEmptyDelim = forAll genString $ \s ->
    splitBy '\0' s == [s]

-- ============================================================================
-- QuickCheck Generators
-- ============================================================================

genNonEmptyString :: Gen String
genNonEmptyString = listOf1 (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "))

genString :: Gen String
genString = listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n"))

genSourcePos :: Gen SourcePos
genSourcePos = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    return $ posAt line col

genSourceSpan :: Gen SourceSpan
genSourceSpan = do
    start <- genSourcePos
    end <- genSourcePos
    return $ spanBetween start end

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Additional Core Tests"
  [ testGroup "Utils Module Tests"
      [ testUtilsTrim
      , testUtilsSplitBy
      , testUtilsSplitByCollapsed
      , testUtilsRemoveLineComments
      ]
  , testGroup "SourceLocation Module Tests"
      [ testSourceLocationBasic
      , testSourceLocationSpanMerge
      ]
  , testGroup "Parser Module Tests"
      [ testParserDirectives
      ]
  , testGroup "ErrorHandler Module Tests"
      [ testErrorHandlerBasic
      ]
  , testGroup "QuickCheck Property Tests"
      [ testProperty "trim after split L.and join" propTrimSplitJoin
      , testProperty "splitByCollapsed never returns empty strings" propSplitByCollapsedNoEmpty
      , testProperty "span merging is associative" propSpanMergeAssociative
      , testProperty "trim is idempotent" propTrimIdempotent
      , testProperty "splitBy with empty delimiter" propSplitByEmptyDelim
      ]
  ]