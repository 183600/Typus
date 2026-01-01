{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.UtilsStringProcessingQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Positive(Positive), getPositive, Arbitrary(..)
  , Gen, oneof, elements, listOf, listOf1, choose, sized, suchThat
  )

import Utils
  ( trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed
  , removeLineComments, removeComments, normalizeIndentation
  , forceSingleTabIndentation, fixIndentation, breakOn
  )

import Data.Char (isSpace, isLetter, isDigit)
import qualified Data.List as L
import Data.List (isPrefixOf, isSuffixOf)
import Data.List (intersperse)
import qualified Data.Text as T

-- Generate strings with various whitespace patterns
genWhitespaceString :: Gen String
genWhitespaceString = listOf $ oneof 
  [ pure ' '
  , pure '\t'
  , pure '\n'
  , pure '\r'
  , elements ['a'..'z']
  , elements ['A'..'Z']
  , elements ['0'..'9']
  ]

-- Generate strings that look like code with potential comments
genCodeString :: Gen String
genCodeString = listOf $ oneof
  [ elements ['a'..'z']
  , elements ['A'..'Z']
  , elements ['0'..'9']
  , pure ' '
  , pure '\t'
  , pure '\n'
  , pure '='
  , pure '+'
  , pure '-'
  , pure '*'
  , pure '/'
  , pure '"'
  , pure '\''
  , pure '('
  , pure ')'
  , pure '{'
  , pure '}'
  , pure ';'
  ]

-- Generate strings with indentation
genIndentedString :: Gen String
genIndentedString = do
  lines <- listOf1 $ do
    indent <- choose (0, 5)
    content <- listOf $ elements ['a'..'z'] ++ [' ']
    return $ replicate indent ' ' ++ content
  return $ unlines lines

-- Property: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

-- Property: trim removes only leading/trailing whitespace
prop_trim_preserves_internal_whitespace :: String -> Property
prop_trim_preserves_internal_whitespace s =
  let trimmed = trim s
      hasInternalWS = L.any isSpace (dropWhile isSpace (L.reverse (dropWhile isSpace (L.reverse s))))
  in not (null trimmed && hasInternalWS) ==> 
     let originalInternal = L.filter (not . isSpace) $ dropWhile isSpace $ L.reverse $ dropWhile isSpace $ L.reverse s
         trimmedInternal = L.filter (not . isSpace) trimmed
     in originalInternal === trimmedInternal

-- Property: splitBy L.and splitByCollapsed relationship
prop_split_by_collapsed_relationship :: Char -> String -> Property
prop_split_by_collapsed_relationship delim s =
  let normal = splitBy delim s
      collapsed = splitByCollapsed delim s
  in collapsed === L.filter (not . null) normal

-- Property: splitBy preserves total L.length when concatenated with delimiter
prop_split_by_preserves_length :: Char -> String -> Property
prop_split_by_preserves_length delim s =
  let parts = splitBy delim s
      reconstructed = intercalate [delim] parts
  in reconstructed === s

-- Property: splitByComma is splitBy with comma
prop_split_by_comma_is_split_by :: String -> Property
prop_split_by_comma_is_split_by s = splitByComma s === splitBy ',' s

-- Property: splitByCommaCollapsed is splitByCollapsed with comma
prop_split_by_comma_collapsed_is_split_by_collapsed :: String -> Property
prop_split_by_comma_collapsed_is_split_by_collapsed s = 
  splitByCommaCollapsed s === splitByCollapsed ',' s

-- Property: breakOn finds first occurrence L.or returns original
prop_break_on_correctness :: String -> String -> Property
prop_break_on_correctness pattern s =
  let (prefix, suffix) = breakOn pattern s
      combined = prefix ++ pattern ++ suffix
  in if pattern `L.isInfixOf` s
     then combined === s
     else (prefix, suffix) === (s, "")

-- Property: removeLineComments doesn't affect strings without // 
prop_remove_line_comments_no_effect_without_delimiter :: String -> Property
prop_remove_line_comments_no_effect_without_delimiter s =
  not ("//" `L.isInfixOf` s) ==> removeLineComments s === s

-- Property: removeComments doesn't affect strings without comment markers
prop_remove_comments_no_effect_without_markers :: String -> Property
prop_remove_comments_no_effect_without_markers s =
  not ("//" `L.isInfixOf` s) && not ("/*" `L.isInfixOf` s) ==> removeComments s === s

-- Property: normalizeIndentation preserves relative indentation
prop_normalize_indentation_preserves_structure :: String -> Property
prop_normalize_indentation_preserves_structure s =
  let normalized = normalizeIndentation s
      originalLines = lines s
      normalizedLines = lines normalized
  in L.length originalLines === L.length normalizedLines .&&.
     not (null originalLines) ==> 
       let firstNonEmptyOriginal = L.head $ L.filter (not . L.all isSpace) originalLines
           firstNonEmptyNormalized = L.head $ L.filter (not . L.all isSpace) normalizedLines
       in L.all isSpace firstNonEmptyOriginal === L.all isSpace firstNonEmptyNormalized

-- Property: forceSingleTabIndentation adds tab to non-empty lines
prop_force_single_tab_adds_tab :: String -> Property
prop_force_single_tab_adds_tab s =
  let result = forceSingleTabIndentation s
      resultLines = lines result
  not (null resultLines) ==> 
    L.all (\line -> null line || '\t' `elem` take 1 line) resultLines

-- Property: fixIndentation is normalizeIndentation
prop_fix_indentation_is_normalize :: String -> Property
prop_fix_indentation_is_normalize s = fixIndentation s === normalizeIndentation s

-- Property: splitBy on empty string returns singleton
prop_split_by_empty_string :: Char -> Property
prop_split_by_empty_string delim = splitBy delim "" === [""]

-- Property: splitByCollapsed on empty string returns empty
prop_split_by_collapsed_empty_string :: Char -> Property
prop_split_by_collapsed_empty_string delim = splitByCollapsed delim "" === []

-- Property: trim empty string stays empty
prop_trim_empty_string :: Property
prop_trim_empty_string = trim "" === ""

-- Property: trim only whitespace string becomes empty
prop_trim_whitespace_only :: Property
prop_trim_whitespace_only = forAll genWhitespaceString $ \s ->
  L.all isSpace s ==> trim s === ""

-- Property: splitBy with delimiter not in string returns singleton
prop_split_by_delimiter_not_in_string :: Char -> String -> Property
prop_split_by_delimiter_not_in_string delim s =
  not (delim `elem` s) ==> splitBy delim s === [s]

-- Property: breakOn with empty pattern returns empty prefix
prop_break_on_empty_pattern :: String -> Property
prop_break_on_empty_pattern s = breakOn "" s === ("", s)

-- Property: removeLineComments preserves line structure
prop_remove_line_comments_preserves_lines :: String -> Property
prop_remove_line_comments_preserves_lines s =
  let originalLines = lines s
      processedLines = lines $ removeLineComments s
  in L.length originalLines === L.length processedLines

-- Property: normalizeIndentation of already normalized string is idempotent
prop_normalize_indentation_idempotent :: String -> Property
prop_normalize_indentation_idempotent s =
  let normalized = normalizeIndentation s
      doubleNormalized = normalizeIndentation normalized
  in normalized === doubleNormalized

tests :: TestTree
tests =
  testGroup "Utils String Processing QuickCheck Tests"
    [ fastProperty "trim is idempotent" prop_trim_idempotent
    , fastProperty "trim preserves internal whitespace" prop_trim_preserves_internal_whitespace
    , fastProperty "splitBy L.and splitByCollapsed relationship" prop_split_by_collapsed_relationship
    , fastProperty "splitBy preserves L.length when reconstructed" prop_split_by_preserves_length
    , fastProperty "splitByComma is splitBy with comma" prop_split_by_comma_is_split_by
    , fastProperty "splitByCommaCollapsed is splitByCollapsed with comma" prop_split_by_comma_collapsed_is_split_by_collapsed
    , fastProperty "breakOn correctness" prop_break_on_correctness
    , fastProperty "removeLineComments no effect without delimiter" prop_remove_line_comments_no_effect_without_delimiter
    , fastProperty "removeComments no effect without markers" prop_remove_comments_no_effect_without_markers
    , fastProperty "normalizeIndentation preserves structure" prop_normalize_indentation_preserves_structure
    , fastProperty "forceSingleTabIndentation adds tab to non-empty lines" prop_force_single_tab_adds_tab
    , fastProperty "fixIndentation is normalizeIndentation" prop_fix_indentation_is_normalize
    , fastProperty "splitBy on empty string returns singleton" prop_split_by_empty_string
    , fastProperty "splitByCollapsed on empty string returns empty" prop_split_by_collapsed_empty_string
    , fastProperty "trim empty string stays empty" prop_trim_empty_string
    , fastProperty "trim whitespace only becomes empty" prop_trim_whitespace_only
    , fastProperty "splitBy with delimiter not in string returns singleton" prop_split_by_delimiter_not_in_string
    , fastProperty "breakOn with empty pattern returns empty prefix" prop_break_on_empty_pattern
    , fastProperty "removeLineComments preserves line structure" prop_remove_line_comments_preserves_lines
    , fastProperty "normalizeIndentation is idempotent" prop_normalize_indentation_idempotent
    ]