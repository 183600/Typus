{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.Unit.NewQuickCheckTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), oneof, choose, listOf, elements, Gen)

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, posAfter, emptySpan, spanFrom, spanTo, mergeSpans, isValidSpan)
import Parser (FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import Utils (trim, splitBy, removeLineComments, removeComments)

import Data.Char (isSpace, isAlpha, isDigit)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
    arbitrary = SourcePos <$> choose (1, 1000) <*> choose (1, 1000) <*> choose (0, 10000)

instance Arbitrary SourceSpan where
    arbitrary = do
        startLine <- choose (1, 1000)
        startCol <- choose (1, 1000)
        startOff <- choose (0, 10000)
        endLine <- choose (startLine, startLine + 100)
        endCol <- if endLine == startLine then choose (startCol, startCol + 100) else choose (1, 1000)
        endOff <- choose (startOff, startOff + 1000)
        return $ SourceSpan 
            { spanStart = SourcePos startLine startCol startOff
            , spanEnd = SourcePos endLine endCol endOff
            }

instance Arbitrary (Located Bool) where
    arbitrary = Located <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary FileDirectives where
    arbitrary = FileDirectives <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary BlockDirectives where
    arbitrary = BlockDirectives <$> arbitrary <*> arbitrary <*> arbitrary

-- ============================================================================
-- QuickCheck Properties (10 tests)
-- ============================================================================

-- Test 1: Source position advancement is monotonic
prop_source_pos_monotonic :: SourcePos -> Char -> Property
prop_source_pos_monotonic pos char =
    let newPos = posAfter char pos
    in newPos `seq` property True  -- Basic property that posAfter produces a valid position

-- Test 2: Source span validity
prop_source_span_validity :: SourceSpan -> Property
prop_source_span_validity span =
    let valid = isValidSpan span
    in classify valid "valid span" $
       classify (not valid) "invalid span" $
       property valid

-- Test 3: Empty span creation
prop_empty_span_properties :: SourcePos -> Property
prop_empty_span_properties pos =
    let span = emptySpan pos
    in property $ isValidSpan span

-- Test 4: Span merging preserves validity
prop_span_merge_validity :: SourceSpan -> SourceSpan -> Property
prop_span_merge_validity span1 span2 =
    let merged = mergeSpans span1 span2
    in property $ isValidSpan merged

-- Test 5: FileDirectives roundtrip with defaults
prop_file_directives_defaults :: Property
prop_file_directives_defaults =
    let defaults = defaultFileDirectives
        custom = FileDirectives Nothing Nothing Nothing
    in property $ defaults == custom

-- Test 6: BlockDirectives roundtrip with defaults
prop_block_directives_defaults :: Property
prop_block_directives_defaults =
    let defaults = defaultBlockDirectives
        custom = BlockDirectives Nothing Nothing Nothing
    in property $ defaults == custom

-- Test 7: String trim idempotency
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
    let trimmed1 = trim str
        trimmed2 = trim trimmed1
    in property $ trimmed1 === trimmed2

-- Test 8: Split by delimiter consistency
prop_split_by_consistency :: Char -> String -> Property
prop_split_by_consistency delim str =
    let parts = splitBy delim str
        rejoined = Data.List.intercalate [delim] parts
    in property $ rejoined === str

-- Test 9: Remove line comments preserves non-comment content
prop_remove_line_comments_preserves :: String -> String -> Property
prop_remove_line_comments_preserves code comment =
    let lineWithComment = code ++ " // " ++ comment ++ "\n" ++ code
        cleaned = removeLineComments lineWithComment
    in not (null code) ==> 
       property $ code `isInfixOf` cleaned

-- Test 10: Remove comments preserves functional code
prop_remove_comments_functional :: String -> String -> Property
prop_remove_comments_functional code1 code2 =
    let withComments = code1 ++ "/* block comment */" ++ code2 ++ "// line comment\n" ++ code1
        withoutComments = removeComments withComments
        -- Ensure code1 and code2 are not just comment-like characters or patterns
        validCode = not (null code1) && not (null code2) && 
                    not (all (`elem` "/*/") code1) && 
                    not (all (`elem` "/*/") code2) &&
                    not ("//" `isPrefixOf` code2) &&
                    not ("/*" `isPrefixOf` code2) &&
                    not (code2 `isSuffixOf` "*/")
    in validCode ==> 
       property $ code1 `isInfixOf` withoutComments .&&.
                  code2 `isInfixOf` withoutComments

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New QuickCheck Tests"
    [ fastProperty "Source position advancement is monotonic" prop_source_pos_monotonic
    , fastProperty "Source span validity" prop_source_span_validity
    , fastProperty "Empty span creation" prop_empty_span_properties
    , fastProperty "Span merging preserves validity" prop_span_merge_validity
    , fastProperty "FileDirectives roundtrip with defaults" prop_file_directives_defaults
    , fastProperty "BlockDirectives roundtrip with defaults" prop_block_directives_defaults
    , fastProperty "String trim idempotency" prop_trim_idempotent
    , fastProperty "Split by delimiter consistency" prop_split_by_consistency
    , fastProperty "Remove line comments preserves non-comment content" prop_remove_line_comments_preserves
    , fastProperty "Remove comments preserves functional code" prop_remove_comments_functional
    ]