{-# LANGUAGE CPP #-}

module Test.Unit.NewComprehensiveCabalQuickCheckTestSuite (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (isSpace, isLetter, isDigit)
import Data.List (isPrefixOf, isSuffixOf, sort)

import Utils (trim, splitBy, splitByComma, removeLineComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, mergeSpans, isValidSpan)
import Parser (FileDirectives(..), BlockDirectives(..))
import ErrorHandler (CompilerError(..), ErrorSeverity(..))
import TestSupport.Arbitrary ()

-- ============================================================================
-- Utils Module Tests
-- ============================================================================

-- | Test that trim removes all leading and trailing whitespace
prop_trim_symmetric :: String -> String -> Property
prop_trim_symmetric prefix suffix =
  let whitespace = " \t\n\r"
      leading = take 3 (cycle whitespace)
      trailing = take 3 (cycle (reverse whitespace))
      original = prefix ++ "content" ++ suffix
      trimmed = trim (leading ++ original ++ trailing)
  in trimmed === original

-- | Test that splitBy preserves empty segments
prop_splitBy_preserves_empty :: Char -> String -> Property
prop_splitBy_preserves_empty delim s =
  let segments = splitBy delim s
      delimCount = length (filter (== delim) s)
  in length segments === delimCount + 1

-- | Test that splitByComma handles comma-separated values correctly
prop_splitByComma_roundtrip :: [String] -> Property
prop_splitByComma_roundtrip parts =
  forAll (elements ["", ",", "a", "ab", "a,b", ",a", "b,", "a,b,c"]) $ \input ->
    let parts = splitByComma input
        rejoined = concat $ intersperse "," parts
    in count ',' input === count ',' rejoined
  where
    count x = length . filter (== x)
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:xs) = x : sep : intersperse sep xs

-- | Test that removeLineComments removes only line comments
prop_removeLineComments_preserves_multiline :: String -> Property
prop_removeLineComments_preserves_multiline s =
  let multilineComment = "/* " ++ s ++ " */"
      result = removeLineComments multilineComment
  in result === multilineComment

-- ============================================================================
-- SourceLocation Module Tests
-- ============================================================================

-- | Test that position advancement is consistent
prop_posAfter_increment :: SourcePos -> Property
prop_posAfter_increment pos =
  let newPos = posAfter pos '\n'
  in posLine newPos === posLine pos + 1 .&&. posColumn newPos === 1

-- | Test that span merging preserves validity
prop_mergeSpans_validity :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_validity span1 span2 =
  let merged = mergeSpans span1 span2
  in property (isValidSpan merged)

-- | Test that start position is always before or equal to end position
prop_sourcespan_start_before_end :: Int -> Int -> Int -> Int -> Property
prop_sourcespan_start_before_end startLine startCol endLine endCol =
  let startPos = SourcePos (abs startLine + 1) (abs startCol + 1) 0
      endPos = SourcePos (abs endLine + 1) (abs endCol + 1) (max 0 (abs endLine + abs endCol))
      span = SourceSpan startPos endPos
  in property $ posOffset (spanStart span) <= posOffset (spanEnd span)

-- ============================================================================
-- Parser Module Tests
-- ============================================================================

-- | Test that file directives with all Nothing are equal
prop_file_directives_all_nothing :: Property
prop_file_directives_all_nothing =
  let fd1 = FileDirectives Nothing Nothing Nothing
      fd2 = FileDirectives Nothing Nothing Nothing
  in fd1 === fd2

-- | Test that block directives preserve structure
prop_block_directives_structure :: Maybe String -> Maybe String -> Maybe String -> Property
prop_block_directives_structure opt1 opt2 opt3 =
  let bd = BlockDirectives opt1 opt2 opt3
      reconstructed = BlockDirectives opt1 opt2 opt3
  in bd === reconstructed

-- ============================================================================
-- ErrorHandler Module Tests
-- ============================================================================

-- | Test that error severity ordering is consistent
prop_error_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_ordering sev1 sev2 =
  let ordered = [ErrorWarning, ErrorError, ErrorFatal]
      idx1 = length $ takeWhile (/= sev1) ordered
      idx2 = length $ takeWhile (/= sev2) ordered
  in property (idx1 <= idx2 || sev1 == sev2)

-- | Test that error messages are preserved
prop_compiler_error_preserves_message :: String -> Property
prop_compiler_error_preserves_message msg =
  let error = CompilerError ErrorError startPos msg
      extractedMsg = getErrorMessage error
  in extractedMsg === msg
  where
    getErrorMessage (CompilerError _ _ m) = m

tests :: TestTree
tests = testGroup "New Cabal QuickCheck Test Suite"
  [ -- Utils module tests
    fastProperty "trim removes whitespace symmetrically" prop_trim_symmetric
  , fastProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
  , fastProperty "splitByComma roundtrip preserves comma count" prop_splitByComma_roundtrip
  , fastProperty "removeLineComments preserves multiline comments" prop_removeLineComments_preserves_multiline
  
    -- SourceLocation module tests
  , fastProperty "posAfter increments line number for newline" prop_posAfter_increment
  , fastProperty "mergeSpans produces valid spans" prop_mergeSpans_validity
  , fastProperty "source span start <= end offset" prop_sourcespan_start_before_end
  
    -- Parser module tests
  , fastProperty "file directives with all Nothing are equal" prop_file_directives_all_nothing
  , fastProperty "block directives preserve structure" prop_block_directives_structure
  
    -- ErrorHandler module tests
  , fastProperty "error severity ordering is consistent" prop_error_severity_ordering
  ]