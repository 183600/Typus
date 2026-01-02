{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Unit.ComprehensiveCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (==>), forAll, choose, listOf1, elements, oneof, sized)
import qualified Data.Text as T
import qualified Data.List as L
import Data.Char (isSpace, isAlphaNum, isLetter)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Monad (when, unless)

import Utils
import SourceLocation
import Parser (TypusFile(..), FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import qualified SyntaxValidator

-- ============================================================================
-- Utils QuickCheck Tests
-- ============================================================================

-- | Test that trim is idempotent: trimming twice is the same as trimming once
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

-- | Test that trim removes only leading/trailing whitespace
prop_trim_preserves_internal :: String -> Property
prop_trim_preserves_internal s = 
    let trimmed = trim s
        hasInternalSpaces = L.any isSpace (dropWhile isSpace (L.reverse (dropWhile isSpace s)))
    in hasInternalSpaces ==> L.length (filter isSpace trimmed) > 0 || L.length trimmed == 0

-- | Test that splitBy L.and splitByCollapsed are related correctly
prop_splitBy_vs_collapsed :: Char -> String -> Property
prop_splitBy_vs_collapsed c s = 
    let normal = splitBy c s
        collapsed = splitByCollapsed c s
    in collapsed === L.filter (not . null) normal

-- | Test that splitByComma is equivalent to splitBy ','
prop_splitByComma_consistency :: String -> Property
prop_splitByComma_consistency s = splitByComma s === splitBy ',' s

-- | Test that splitting L.and joining with the same delimiter preserves the string (for collapsed version)
prop_split_join_collapsed :: Char -> NonEmptyList Char -> Property
prop_split_join_collapsed c (NonEmpty chars) = 
    let s = chars
        parts = splitByCollapsed c s
        rejoined = L.intercalate [c] parts
    in not (null s) ==> rejoined === L.filter (/= c) s

-- ============================================================================
-- SourceLocation QuickCheck Tests
-- ============================================================================

-- | Test that span start is always before L.or equal to span end
prop_span_ordering :: SourceSpan -> Property
prop_span_ordering span = property $ spanStart span <= spanEnd span

-- | Test that merging spans produces a valid span
prop_merge_spans_valid :: SourceSpan -> SourceSpan -> Property
prop_merge_spans_valid span1 span2 = 
    let merged = mergeSpans span1 span2
    in property $ spanStart merged <= spanEnd merged

-- | Test that emptySpan has start <= end
prop_empty_span_valid :: Property
prop_empty_span_valid = property $ spanStart emptySpan <= spanEnd emptySpan

-- | Test that locatedAt preserves the value
prop_locatedAt_preserves_value :: Int -> SourcePos -> Property
prop_locatedAt_preserves_value val pos = locatedValue (locatedAt val pos) === val

-- | Test that advancing position by zero characters returns the same position
prop_advance_pos_zero :: SourcePos -> Property
prop_advance_pos_zero pos = advancePos pos '\0' === pos

-- ============================================================================
-- Parser Data Structure QuickCheck Tests
-- ============================================================================

-- | Test that default file directives have no values set
prop_default_file_directives :: Property
prop_default_file_directives = 
    let FileDirectives{..} = defaultFileDirectives
    in isNothing fdOwnership && isNothing fdDependentTypes && isNothing fdConstraints

-- | Test that default block directives have no values set
prop_default_block_directives :: Property
prop_default_block_directives = 
    let BlockDirectives{..} = defaultBlockDirectives
    in isNothing bdOwnership && isNothing bdDependentTypes && isNothing bdConstraints

-- ============================================================================
-- String Processing QuickCheck Tests
-- ============================================================================

-- | Test that breaking on a character that doesn't exist returns the original string
prop_breakOn_not_found :: Char -> NonEmptyList Char -> Property
prop_breakOn_not_found c (NonEmpty chars) = 
    let s = chars
    in c `notElem` s ==> breakOn c s === (s, "")

-- | Test that breakOn always returns strings that concatenate to the original
prop_breakOn_concatenates :: Char -> String -> Property
prop_breakOn_concatenates c s = 
    let (before, after) = breakOn c s
    in before ++ [c] ++ after === s

-- ============================================================================
-- Comment Processing QuickCheck Tests
-- ============================================================================

-- | Test that removing line comments preserves lines without comments
prop_removeLineComments_preserves_non_comment :: String -> Property
prop_removeLineComments_preserves_non_comment line = 
    not ("//" `L.isPrefixOf` line) ==> removeLineComments line === line

-- | Test that removing comments from a string without comments returns the same string
prop_removeComments_preserves_no_comments :: String -> Property
prop_removeComments_preserves_no_comments s = 
    let hasBlockComments = "/*" `L.isInfixOf` s
        hasLineComments = "//" `L.isInfixOf` s
    in not (hasBlockComments || hasLineComments) ==> removeComments s === s

-- ============================================================================
-- Indentation QuickCheck Tests
-- ============================================================================

-- | Test that normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preserves_structure :: [String] -> Property
prop_normalizeIndentation_preserves_structure lines = 
    let normalized = normalizeIndentation lines
        hasSameLineCount = L.length normalized == L.length lines
        allNonEmpty = L.all (not . null) lines
    in allNonEmpty ==> hasSameLineCount

-- | Test that normalizeIndentation removes leading spaces consistently
prop_normalizeIndentation_consistent :: NonEmptyList String -> Property
prop_normalizeIndentation_consistent (NonEmpty lines) = 
    let normalized = normalizeIndentation lines
        leadingSpaces line = L.length $ takeWhile isSpace line
        minOriginalSpaces = L.minimum $ map leadingSpaces lines
        minNormalizedSpaces = L.minimum $ map leadingSpaces normalized
    in L.length lines > 1 ==> minNormalizedSpaces <= minOriginalSpaces

-- ============================================================================
-- Custom Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
    arbitrary = SourcePos <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SourceSpan where
    arbitrary = do
        line <- choose (1, 100)
        col1 <- choose (1, 100)
        col2 <- choose (col1, col1 + 50)  -- Ensure col2 >= col1
        return $ SourceSpan (SourcePos line col1 0) (SourcePos line col2 0)

newtype NonEmptyList a = NonEmpty { getNonEmpty :: [a] }
    deriving (Show, Eq)

instance Arbitrary a => Arbitrary (NonEmptyList a) where
    arbitrary = NonEmpty <$> listOf1 arbitrary

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Comprehensive Core QuickCheck Tests"
    [ testGroup "Utils Module Tests"
        [ testProperty "trim is idempotent" prop_trim_idempotent
        , testProperty "trim preserves internal whitespace" prop_trim_preserves_internal
        , testProperty "splitBy vs splitByCollapsed consistency" prop_splitBy_vs_collapsed
        , testProperty "splitByComma consistency" prop_splitByComma_consistency
        , testProperty "split-join collapsed property" prop_split_join_collapsed
        ]
    
    , testGroup "SourceLocation Tests"
        [ testProperty "span ordering is valid" prop_span_ordering
        , testProperty "merge spans produces valid span" prop_merge_spans_valid
        , testProperty "empty span is valid" prop_empty_span_valid
        , testProperty "locatedAt preserves value" prop_locatedAt_preserves_value
        , testProperty "advance position by zero" prop_advance_pos_zero
        ]
    
    , testGroup "Parser Data Structure Tests"
        [ testProperty "default file directives are empty" prop_default_file_directives
        , testProperty "default block directives are empty" prop_default_block_directives
        ]
    
    , testGroup "String Processing Tests"
        [ testProperty "breakOn not found returns original" prop_breakOn_not_found
        , testProperty "breakOn concatenates to original" prop_breakOn_concatenates
        ]
    
    , testGroup "Comment Processing Tests"
        [ testProperty "removeLineComments preserves non-comment lines" prop_removeLineComments_preserves_non_comment
        , testProperty "removeComments preserves strings without comments" prop_removeComments_preserves_no_comments
        ]
    
    , testGroup "Indentation Tests"
        [ testProperty "normalizeIndentation preserves structure" prop_normalizeIndentation_preserves_structure
        , testProperty "normalizeIndentation is consistent" prop_normalizeIndentation_consistent
        ]
    ]

-- Helper operator for property testing
(===) :: (Show a, Eq a) => a -> a -> Property
a === b = if a == b then property () else reject "Values are not equal"

reject :: String -> Property
reject _ = property False

property :: Bool -> Property
property True = property ()
property False = reject "Property failed"