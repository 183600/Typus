{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.UtilsCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(..), NonNegative(..), Arbitrary(..), oneof, elements, Gen, suchThat)

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

import Data.Char (isSpace, toLower, isAlphaNum)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (sort, nub)
import Data.String (IsString)

-- Property: trim removes leading L.and trailing whitespace
prop_trim_removes_leading_trailing :: String -> String -> Property
prop_trim_removes_leading_trailing prefix suffix =
  let content = prefix ++ "content" ++ suffix
      trimmed = trim content
      hasLeading = L.any isSpace prefix
      hasTrailing = L.any isSpace suffix
      noLeadingSpace = null trimmed || not (isSpace (L.head trimmed))
      noTrailingSpace = null trimmed || not (isSpace (last trimmed))
  in classify hasLeading "has leading whitespace" $
     classify hasTrailing "has trailing whitespace" $
     property $ noLeadingSpace .&&. noTrailingSpace

-- Property: trim preserves internal whitespace
prop_trim_preserves_internal :: String -> String -> String -> Property
prop_trim_preserves_internal before middle after =
  let content = before ++ middle ++ after
      trimmed = trim content
      middleTrimmed = trim middle
  in not (null middle) ==> L.any (not . isSpace) middle ==>
  property $ middleTrimmed `L.isInfixOf` trimmed

-- Property: trim idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
      doubleTrimmed = trim trimmed
  in property $ trimmed === doubleTrimmed

-- Property: splitBy preserves empty segments
prop_splitBy_preserves_empty :: String -> Char -> Property
prop_splitBy_preserves_empty s sep =
  let result = splitBy sep s
      expectedCount = L.length (L.filter (== sep) s) + 1
  in property $ L.length result === expectedCount

-- Property: splitByCollapsed removes consecutive separators
prop_splitByCollapsed_removes_consecutive :: String -> Char -> Property
prop_splitByCollapsed_removes_consecutive s sep =
  let result = splitByCollapsed sep s
      normalResult = splitBy sep s
      hasConsecutive = sep : sep `L.isInfixOf` s
  in classify hasConsecutive "has consecutive separators" $
     property $ not hasConsecutive ==> L.length result === L.length normalResult

-- Property: splitByComma handles commas correctly
prop_splitByComma_handles_commas :: String -> Property
prop_splitByComma_handles_commas s =
  let result = splitByComma s
      manualResult = splitBy ',' s
  in property $ result === manualResult

-- Property: splitByCommaCollapsed handles consecutive commas
prop_splitByCommaCollapsed_consecutive :: String -> Property
prop_splitByCommaCollapsed_consecutive s =
  let result = splitByCommaCollapsed s
      manualResult = splitByCollapsed ',' s
  in property $ result === manualResult

-- Property: removeLineComments removes // comments
prop_removeLineComments_removes_slash_comments :: String -> Property
prop_removeLineComments_removes_slash_comments code =
  let withComment = code ++ "// this is a comment\n"
      withoutComment = removeLineComments withComment
  in not (null code) ==> 
  property $ not ("//" `L.isInfixOf` withoutComment) .&&. code `L.isPrefixOf` withoutComment

-- Property: removeLineComments preserves code before comments
prop_removeLineComments_preserves_before :: String -> String -> Property
prop_removeLineComments_preserves_before code comment =
  not (null code) ==> not (null comment) ==> 
  let withComment = code ++ "// " ++ comment
      withoutComment = removeLineComments withComment
  in property $ code `L.isPrefixOf` withoutComment

-- Property: removeComments handles both // L.and /* */ comments
prop_removeComments_handles_both :: String -> String -> Property
prop_removeComments_handles_both code blockComment =
  not (null code) ==> not (null blockComment) ==> 
  let withComments = code ++ "/* " ++ blockComment ++ "*/" ++ " // line comment\n"
      withoutComments = removeComments withComments
  in property $ not ("/*" `L.isInfixOf` withoutComments) .&&. not ("*/" `L.isInfixOf` withoutComments) .&&. not ("//" `L.isInfixOf` withoutComments)

-- Property: normalizeIndentation removes common prefix
prop_normalizeIndentation_removes_prefix :: String -> String -> Property
prop_normalizeIndentation_removes_prefix line1 line2 =
  not (null line1) ==> not (null line2) ==> 
  let indented1 = "  " ++ line1
      indented2 = "  " ++ line2
      source = indented1 ++ "\n" ++ indented2 ++ "\n"
      normalized = normalizeIndentation source
  in property $ line1 `L.isInfixOf` normalized .&&. line2 `L.isInfixOf` normalized

-- Property: normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preserves_relative :: String -> String -> String -> Property
prop_normalizeIndentation_preserves_relative line1 line2 line3 =
  not (null line1) ==> not (null line2) ==> not (null line3) ==> 
  let source = "    " ++ line1 ++ "\n" ++ "  " ++ line2 ++ "\n" ++ "      " ++ line3 ++ "\n"
      normalized = normalizeIndentation source
      lines' = lines normalized
  in L.length lines' >= 3 ==> 
  property $ L.length (takeWhile isSpace (lines' !! 1)) < L.length (takeWhile isSpace (lines' !! 0)) .&&.
             L.length (takeWhile isSpace (lines' !! 1)) < L.length (takeWhile isSpace (lines' !! 2))

-- Property: forceSingleTabIndentation converts to tabs
prop_forceSingleTabIndentation_to_tabs :: String -> Property
prop_forceSingleTabIndentation_to_tabs s =
  let withSpaces = "    " ++ s
      withTabs = forceSingleTabIndentation withSpaces
  in property $ "\t" `L.isPrefixOf` withTabs

-- Property: fixIndentation is alias for normalizeIndentation
prop_fixIndentation_alias_normalize :: String -> Property
prop_fixIndentation_alias_normalize s =
  let normalized = normalizeIndentation s
      fixed = fixIndentation s
  in property $ normalized === fixed

-- Property: breakOn finds first occurrence
prop_breakOn_finds_first :: String -> String -> String -> Property
prop_breakOn_finds_first prefix needle suffix =
  not (null needle) ==> 
  let haystack = prefix ++ needle ++ suffix ++ needle ++ "end"
      (before, after) = breakOn needle haystack
  in property $ before === prefix .&&. after === suffix ++ needle ++ "end"

-- Property: breakOn handles needle not found
prop_breakOn_not_found :: String -> String -> Property
prop_breakOn_not_found haystack needle =
  not (needle `L.isInfixOf` haystack) ==> not (null needle) ==>
  let (before, after) = breakOn needle haystack
  in property $ before === haystack .&&. null after

-- Property: breakOn handles empty needle
prop_breakOn_empty_needle :: String -> Property
prop_breakOn_empty_needle haystack =
  let (before, after) = breakOn "" haystack
  in property $ null before .&&. after === haystack

-- Property: trim handles empty string
prop_trim_empty :: Property
prop_trim_empty =
  let result = trim ""
  in property $ result === ""

-- Property: splitBy handles empty string
prop_splitBy_empty :: Char -> Property
prop_splitBy_empty sep =
  let result = splitBy sep ""
  in property $ result === [""]

-- Property: removeLineComments handles string without comments
prop_removeLine_comments_no_comments :: String -> Property
prop_removeLine_comments_no_comments code =
  not ("//" `L.isInfixOf` code) ==> 
  let result = removeLineComments code
  in property $ result === code

-- Property: removeComments handles string without comments
prop_remove_comments_no_comments :: String -> Property
prop_remove_comments_no_comments code =
  not ("//" `L.isInfixOf` code) ==> not ("/*" `L.isInfixOf` code) ==> 
  let result = removeComments code
  in property $ result === code

-- Property: normalizeIndentation handles single line
prop_normalizeIndentation_single_line :: String -> Property
prop_normalizeIndentation_single_line line =
  let result = normalizeIndentation line
  in property $ result === line

-- Property: String splitting L.and joining consistency
prop_split_join_consistency :: String -> Char -> Property
prop_split_join_consistency s sep =
  let parts = splitBy sep s
      rejoined = Data.List.intercalate [sep] parts
  in property $ rejoined === s

-- Property: String trimming L.and whitespace consistency
prop_trim_whitespace_consistency :: String -> Property
prop_trim_whitespace_consistency s =
  let trimmed = trim s
      hasOnlyWhitespace = L.all isSpace s
  in classify hasOnlyWhitespace "only whitespace" $
     classify (not hasOnlyWhitespace) "has content" $
     property $ 
       if hasOnlyWhitespace 
       then null trimmed
       else L.any (not . isSpace) trimmed

tests :: TestTree
tests =
  testGroup "Utils Core QuickCheck Tests"
    [ fastProperty "trim removes leading L.and trailing whitespace" prop_trim_removes_leading_trailing
    , fastProperty "trim preserves internal whitespace" prop_trim_preserves_internal
    , fastProperty "trim is idempotent" prop_trim_idempotent
    , fastProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
    , fastProperty "splitByCollapsed removes consecutive separators" prop_splitByCollapsed_removes_consecutive
    , fastProperty "splitByComma handles commas correctly" prop_splitByComma_handles_commas
    , fastProperty "splitByCommaCollapsed handles consecutive commas" prop_splitByCommaCollapsed_consecutive
    , fastProperty "removeLineComments removes // comments" prop_removeLineComments_removes_slash_comments
    , fastProperty "removeLineComments preserves code before comments" prop_removeLineComments_preserves_before
    , fastProperty "removeComments handles both // L.and /* */ comments" prop_removeComments_handles_both
    , fastProperty "normalizeIndentation removes common prefix" prop_normalizeIndentation_removes_prefix
    , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative
    , fastProperty "forceSingleTabIndentation converts to tabs" prop_forceSingleTabIndentation_to_tabs
    , fastProperty "fixIndentation is alias for normalizeIndentation" prop_fixIndentation_alias_normalize
    , fastProperty "breakOn finds first occurrence" prop_breakOn_finds_first
    , fastProperty "breakOn handles needle not found" prop_breakOn_not_found
    , fastProperty "breakOn handles empty needle" prop_breakOn_empty_needle
    , fastProperty "trim handles empty string" prop_trim_empty
    , fastProperty "splitBy handles empty string" prop_splitBy_empty
    , fastProperty "removeLineComments handles string without comments" prop_removeLine_comments_no_comments
    , fastProperty "removeComments handles string without comments" prop_remove_comments_no_comments
    , fastProperty "normalizeIndentation handles single line" prop_normalizeIndentation_single_line
    , fastProperty "string splitting L.and joining consistency" prop_split_join_consistency
    , fastProperty "string trimming L.and whitespace consistency" prop_trim_whitespace_consistency
    ]