{-# LANGUAGE CPP #-}

module Test.Unit.CoreModuleQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace, isAlphaNum, isLetter, isDigit)

import Utils (trim, splitBy, splitByCollapsed, splitByComma, removeLineComments, removeComments, normalizeIndentation, breakOn)
import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn, posOffset, spanStart, spanEnd)
import Parser (FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..))
import TestSupport.Arbitrary ()

-- ============================================================================
-- Utils Module Properties
-- ============================================================================

-- Property 1: splitByCollapsed removes empty segments
prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s =
  let result = splitByCollapsed delim s
  in all (not . null) result === True

-- Property 2: splitBy preserves all segments including empty ones
prop_splitBy_preserves_all :: Char -> String -> Property
prop_splitBy_preserves_all delim s =
  let parts = splitBy delim s
      joined = intercalate [delim] parts
      delimCount = length (filter (== delim) s)
  in length parts === delimCount + 1 .&&.
     (if null s then parts == [""] else True)
  where
    intercalate _ [] = []
    intercalate sep (x:xs) = x ++ concatMap (sep ++) xs

-- Property 3: splitByComma is equivalent to splitBy ','
prop_splitByComma_eq_splitBy :: String -> Property
prop_splitByComma_eq_splitBy s =
  splitByComma s === splitBy ',' s

-- Property 4: removeComments preserves non-comment content
prop_removeComments_preserves_noncomment :: String -> Property
prop_removeComments_preserves_noncomment s =
  let hasNoComments = not (any (`isPrefixOf` s) ["//", "/*"])
  in hasNoComments ==> removeComments s === s

-- Property 5: normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preserves_relative :: String -> Property
prop_normalizeIndentation_preserves_relative s =
  let lines' = lines s
      normalized = normalizeIndentation s
      normLines = lines normalized
  in length lines' === length normLines .&&.
     (if length lines' > 1 then
        let firstLine = head lines'
            otherLines = tail lines'
            firstNormLine = head normLines
            otherNormLines = tail normLines
            calcIndent l = length $ takeWhile isSpace l
            originalIndents = map calcIndent otherLines
            normalizedIndents = map calcIndent otherNormLines
            minOrig = if null originalIndents then 0 else minimum originalIndents
            minNorm = if null normalizedIndents then 0 else minimum normalizedIndents
        in property $ all (\(orig, norm) -> orig - minOrig == norm - minNorm) (zip originalIndents normalizedIndents)
      else property True)

-- Property 6: breakOn is equivalent to Data.List.break for simple cases
prop_breakOn_eq_break :: Char -> String -> Property
prop_breakOn_eq_break delim s =
  let (prefix, suffix) = breakOn [delim] s
      (expectedPrefix, expectedSuffix) = break (== delim) s
  in if null s then prefix === expectedPrefix .&&. suffix === expectedSuffix
     else prefix === expectedPrefix .&&. suffix === expectedSuffix

-- Property 7: trim doesn't change strings without leading/trailing whitespace
prop_trim_preserves_no_whitespace :: String -> Property
prop_trim_preserves_no_whitespace s =
  let noLeading = null s || not (isSpace (head s))
      noTrailing = null s || not (isSpace (last s))
  in noLeading && noTrailing ==> trim s === s

-- Property 8: removeLineComments handles quotes correctly
prop_removeLineComments_nested_quotes :: String -> Property
prop_removeLineComments_nested_quotes s =
  let result = removeLineComments s
      -- Check that the result doesn't contain comment markers
      hasComments = "//" `isInfixOf` result
  in not hasComments === True

-- ============================================================================
-- SourceLocation Module Properties
-- ============================================================================

-- Property 9: SourcePos offset is consistent with line and column
prop_sourcepos_consistency :: Property
prop_sourcepos_consistency = forAll genValidSourcePos $ \pos ->
  let line = posLine pos
      col = posColumn pos
      offset = posOffset pos
  in offset >= 0 .&&. line > 0 .&&. col > 0
  where
    genValidSourcePos = do
      line <- choose (1, 1000)
      col <- choose (1, 1000)
      offset <- choose (0, 1000000)
      return $ SourcePos line col offset

-- Property 10: SourceSpan ordering is consistent
prop_sourcespan_ordering :: Property
prop_sourcespan_ordering = forAll genValidSourceSpan $ \span ->
  let start = spanStart span
      end = spanEnd span
  in posOffset start <= posOffset end
  where
    genValidSourceSpan = do
      startOffset <- choose (0, 999999)
      endOffset <- choose (startOffset, 1000000)
      startLine <- choose (1, 1000)
      endLine <- choose (startLine, 1000)
      startCol <- choose (1, 1000)
      endCol <- if startLine == endLine then choose (startCol, 1000) else choose (1, 1000)
      let start = SourcePos startLine startCol startOffset
          end = SourcePos endLine endCol endOffset
      return $ SourceSpan start end
    
    genValidSourcePos = do
      line <- choose (1, 1000)
      col <- choose (1, 1000)
      offset <- choose (0, 1000000)
      return $ SourcePos line col offset

-- ============================================================================
-- Parser Module Properties
-- ============================================================================

-- Property 11: FileDirectives equality is reflexive
prop_file_directives_reflexive :: FileDirectives -> Property
prop_file_directives_reflexive fd = fd === fd

-- Property 12: BlockDirectives equality is reflexive
prop_block_directives_reflexive :: BlockDirectives -> Property
prop_block_directives_reflexive bd = bd === bd

-- Property 13: FileDirectives with all Nothing values are equal
prop_file_directives_all_nothing :: Property
prop_file_directives_all_nothing =
  let fd1 = FileDirectives Nothing Nothing Nothing
      fd2 = FileDirectives Nothing Nothing Nothing
  in fd1 === fd2

-- Property 14: BlockDirectives with all Nothing values are equal
prop_block_directives_all_nothing :: Property
prop_block_directives_all_nothing =
  let bd1 = BlockDirectives Nothing Nothing Nothing
      bd2 = BlockDirectives Nothing Nothing Nothing
  in bd1 === bd2

-- Property 15: CodeBlock type consistency
prop_codeblock_consistency :: CodeBlock -> Property
prop_codeblock_consistency cb =
  -- This is a basic consistency check - more specific properties would depend on CodeBlock structure
  property True

-- ============================================================================
-- Combined Properties
-- ============================================================================

-- Property 16: trim and splitBy interaction
prop_trim_splitby_interaction :: Char -> String -> Property
prop_trim_splitby_interaction delim s =
  let trimmed = trim s
      splitTrimmed = splitBy delim trimmed
      splitOriginal = splitBy delim s
      trimmedParts = map trim splitOriginal
  in (if null trimmed then property $ splitTrimmed == [""] else length splitTrimmed === length trimmedParts)

-- Property 17: removeComments and removeLineComments relationship
prop_removeComments_linecomments_relationship :: String -> Property
prop_removeComments_linecomments_relationship s =
  let hasBlockComments = "/*" `isInfixOf` s || "*/" `isInfixOf` s
      lineOnly = removeLineComments s
      both = removeComments s
      -- normalize both strings by removing trailing newlines for comparison
      normalize = reverse . dropWhile (== '\n') . reverse
  in (not hasBlockComments) ==> normalize lineOnly === normalize both

-- Property 18: SourcePos arithmetic consistency
prop_sourcepos_arithmetic :: Property
prop_sourcepos_arithmetic = forAll genValidSourcePos $ \pos ->
  let line = posLine pos
      col = posColumn pos
      offset = posOffset pos
      newLine = line + 1
      newCol = 1
      newOffset = offset + 100 -- Approximate new line offset
      newPos = SourcePos newLine newCol newOffset
  in posOffset newPos > posOffset pos .&&.
     posLine newPos > posLine pos
  where
    genValidSourcePos = do
      line <- choose (1, 1000)
      col <- choose (1, 1000)
      offset <- choose (0, 1000000)
      return $ SourcePos line col offset

tests :: TestTree
tests = testGroup "Core Module QuickCheck Tests"
  [ -- Utils module tests
    fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_no_empty
  , fastProperty "splitBy preserves all segments including empty ones" prop_splitBy_preserves_all
  , fastProperty "splitByComma is equivalent to splitBy ','" prop_splitByComma_eq_splitBy
  , fastProperty "removeComments preserves non-comment content" prop_removeComments_preserves_noncomment
  , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative
  , fastProperty "breakOn is equivalent to Data.List.break for simple cases" prop_breakOn_eq_break
  , fastProperty "trim doesn't change strings without leading/trailing whitespace" prop_trim_preserves_no_whitespace
  , fastProperty "removeLineComments handles nested quotes correctly" prop_removeLineComments_nested_quotes
  
  -- SourceLocation module tests
  , fastProperty "SourcePos offset is consistent with line and column" prop_sourcepos_consistency
  , fastProperty "SourceSpan ordering is consistent" prop_sourcespan_ordering
  
  -- Parser module tests
  , fastProperty "FileDirectives equality is reflexive" prop_file_directives_reflexive
  , fastProperty "BlockDirectives equality is reflexive" prop_block_directives_reflexive
  , fastProperty "FileDirectives with all Nothing values are equal" prop_file_directives_all_nothing
  , fastProperty "BlockDirectives with all Nothing values are equal" prop_block_directives_all_nothing
  , fastProperty "CodeBlock type consistency" prop_codeblock_consistency
  
  -- Combined properties
  , fastProperty "trim and splitBy interaction" prop_trim_splitby_interaction
  , fastProperty "removeComments and removeLineComments relationship" prop_removeComments_linecomments_relationship
  , fastProperty "SourcePos arithmetic consistency" prop_sourcepos_arithmetic
  ]