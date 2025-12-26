{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.TextProcessingBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, resize, Positive(..))
import Data.Char (isSpace, isAlphaNum, isPunctuation, isControl, chr)
import Data.List (isPrefixOf, isInfixOf, sort, nub)
import qualified Data.Text as T

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , forceSingleTabIndentation
  , fixIndentation
  , breakOn
  )

-- Property: trim is idempotent - applying it twice gives same result
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmedOnce = trim s
      trimmedTwice = trim (trim s)
  in counterexample "trim should be idempotent" $
     trimmedOnce === trimmedTwice

-- Property: trim never increases string length
prop_trim_never_increases_length :: String -> Property
prop_trim_never_increases_length s =
  let originalLength = length s
      trimmedLength = length (trim s)
  in counterexample "trim should never increase string length" $
     trimmedLength <= originalLength

-- Property: splitBy preserves total character count (excluding delimiters)
prop_splitBy_preserves_content :: Char -> String -> Property
prop_splitBy_preserves_content delim s =
  let parts = splitBy delim s
      rejoined = concat parts
      originalWithoutDelims = filter (/= delim) s
  in counterexample "splitBy should preserve all non-delimiter characters" $
     rejoined === originalWithoutDelims

-- Property: splitByCollapsed result contains no empty strings
prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s =
  let parts = splitByCollapsed delim
  in counterexample "splitByCollapsed should never produce empty strings" $
     all (not . null) parts

-- Property: splitByComma is equivalent to splitBy ','
prop_splitByComma_equals_splitBy_comma :: String -> Property
prop_splitByComma_equals_splitBy_comma s =
  let byCommaFunc = splitByComma s
      byGeneric = splitBy ',' s
  in counterexample "splitByComma should equal splitBy ','" $
     byCommaFunc === byGeneric

-- Property: removeLineComments preserves non-comment lines
prop_removeLineComments_preserves_non_comment :: String -> Property
prop_removeLineComments_preserves_non_comment s =
  let withoutLineComments = removeLineComments s
      linesWithoutComments = lines withoutLineComments
      linesWithComments = lines s
      nonCommentLines = filter (not . isPrefixOf "//") linesWithComments
  in counterexample "removeLineComments should preserve non-comment content" $
     length (filter (not . null) linesWithoutComments) >= length nonCommentLines

-- Property: normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preserves_structure :: String -> Property
prop_normalizeIndentation_preserves_structure s =
  let normalized = normalizeIndentation s
      originalLines = lines s
      normalizedLines = lines normalized
  in counterexample "normalizeIndentation should preserve line structure" $
     length normalizedLines === length originalLines

-- Property: breakOn always returns a pair where concatenation equals original
prop_breakOn_concatenation :: String -> String -> Property
prop_breakOn_concatenation needle haystack =
  let (before, after) = breakOn needle haystack
      reconstructed = before ++ needle ++ after
  in case needle of
    [] -> property True -- Empty needle case is implementation defined
    _ -> counterexample "breakOn result should reconstruct original" $
         reconstructed === haystack

-- Property: trim preserves non-space characters
prop_trim_preserves_non_space :: String -> Property
prop_trim_preserves_non_space s =
  let trimmed = trim s
      originalNonSpaces = filter (not . isSpace) s
      trimmedNonSpaces = filter (not . isSpace) trimmed
  in counterexample "trim should preserve all non-space characters" $
     originalNonSpaces === trimmedNonSpaces

-- Property: splitBy with delimiter not in string returns single-element list
prop_splitBy_no_delimiter :: Char -> String -> Property
prop_splitBy_no_delimiter delim s =
  delim `notElem` s ==>
  let parts = splitBy delim s
  in counterexample "splitBy with non-existent delimiter should return single-element list" $
     parts === [s]

-- Property: removeComments preserves string structure (roughly)
prop_removeComments_preserves_structure :: String -> Property
prop_removeComments_preserves_structure s =
  let withoutComments = removeComments s
      -- Rough check: preserve line count (approximately)
      originalLines = length $ lines s
      withoutCommentsLines = length $ lines withoutComments
  in counterexample "removeComments should roughly preserve structure" $
     withoutCommentsLines <= originalLines + 1 -- Allow for some variation

-- Generate strings with various Unicode characters for boundary testing
genUnicodeString :: Gen String
genUnicodeString = listOf $ oneof
  [ elements ['a'..'z']
  , elements ['A'..'Z']
  , elements ['0'..'9']
  , elements " !@#$%^&*()_+-=[]{}|;':\",./<>?"
  , elements "\t\n\r\f\v"
  , choose (0x80, 0xFFFF) >>= \c -> return [chr c]
  ]

-- Property: trim works correctly with Unicode whitespace
prop_trim_unicode_whitespace :: Property
prop_trim_unicode_whitespace =
  forAll genUnicodeString $ \s ->
  let trimmed = trim s
      hasLeadingUnicodeSpace = not (null s) && isSpace (head s) && head s > ' '
      hasTrailingUnicodeSpace = not (null s) && isSpace (last s) && last s > ' '
      noLeadingSpace = null trimmed || not (isSpace (head trimmed))
      noTrailingSpace = null trimmed || not (isSpace (last trimmed))
  in classify hasLeadingUnicodeSpace "has leading Unicode whitespace" $
     classify hasTrailingUnicodeSpace "has trailing Unicode whitespace" $
     property $ noLeadingSpace .&&. noTrailingSpace

tests :: TestTree
tests = testGroup "Text Processing Boundary QuickCheck Tests"
  [ fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "trim never increases length" prop_trim_never_increases_length
  , fastProperty "splitBy preserves content" prop_splitBy_preserves_content
  , fastProperty "splitByCollapsed has no empty strings" prop_splitByCollapsed_no_empty
  , fastProperty "splitByComma equals splitBy ','" prop_splitByComma_equals_splitBy_comma
  , fastProperty "removeLineComments preserves non-comment lines" prop_removeLineComments_preserves_non_comment
  , fastProperty "normalizeIndentation preserves structure" prop_normalizeIndentation_preserves_structure
  , fastProperty "breakOn concatenation property" prop_breakOn_concatenation
  , fastProperty "trim preserves non-space characters" prop_trim_preserves_non_space
  , fastProperty "splitBy with no delimiter" prop_splitBy_no_delimiter
  , fastProperty "removeComments preserves structure" prop_removeComments_preserves_structure
  , fastProperty "trim works with Unicode whitespace" prop_trim_unicode_whitespace
  ]