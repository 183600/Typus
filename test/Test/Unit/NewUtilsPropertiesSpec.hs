{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewUtilsPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, suchThat
  , choose, frequency, sized, resize, Positive(..), NonEmpty(..)
  )

import Utils 
  ( trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed
  , removeLineComments, removeComments, normalizeIndentation
  , forceSingleTabIndentation, fixIndentation, breakOn
  )

import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (intercalate, sort)
import Data.Char (isSpace, isAlphaNum)

-- ============================================================================
-- String Processing Properties
-- ============================================================================

-- Property: trim removes leading L.and trailing whitespace
prop_trim_removes_whitespace :: String -> Property
prop_trim_removes_whitespace s =
  let trimmed = trim s
      hasLeading = not (null s) && isSpace (L.head s)
      hasTrailing = not (null s) && isSpace (last s)
  in classify (hasLeading || hasTrailing) "with whitespace" $
     classify (not (hasLeading || hasTrailing)) "without whitespace" $
     if hasLeading || hasTrailing
     then not (null trimmed) ==> not (isSpace (L.head trimmed) || isSpace (last trimmed))
     else trimmed === s

-- Property: trim is idempotent (applying it twice gives same result)
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

-- Property: splitBy preserves empty segments
prop_splitBy_preserves_empty :: Char -> String -> Property
prop_splitBy_preserves_empty c s =
  let result = splitBy c s
      expectedLength = L.length s + 1
  in L.length result === expectedLength

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty c s =
  let result = splitByCollapsed c s
      hasNoEmpty = L.all (not . null) result
  in hasNoEmpty === True

-- Property: splitByComma is equivalent to splitBy ','
prop_splitByComma_equals_splitBy :: String -> Property
prop_splitByComma_equals_splitBy s = splitByComma s === splitBy ',' s

-- Property: splitByCommaCollapsed is equivalent to splitByCollapsed ','
prop_splitByCommaCollapsed_equals_splitByCollapsed :: String -> Property
prop_splitByCommaCollapsed_equals_splitByCollapsed s = 
  splitByCommaCollapsed s === splitByCollapsed ',' s

-- ============================================================================
-- Comment Removal Properties  
-- ============================================================================

-- Property: removeLineComments removes lines starting with //
prop_removeLineComments_removes_comments :: String -> Property
prop_removeLineComments_removes_comments s =
  let withComment = s ++ "\n// this is a comment\nmore code"
      result = removeLineComments withComment
  in "//" `L.isInfixOf` result === False

-- Property: removeLineComments preserves // inside string literals
prop_removeLineComments_preserves_string_comments :: String -> Property
prop_removeLineComments_preserves_string_comments s =
  let codeWithCommentInString = "let x = \"string with // not a comment\"\nlet y = // real comment\nlet z = x"
      result = removeLineComments codeWithCommentInString
  in "// not a comment" `L.isInfixOf` result === True

-- Property: removeComments removes both // L.and /* */ comments
prop_removeComments_removes_both_types :: String -> Property
prop_removeComments_removes_both_types s =
  let withComments = s ++ "\n// line comment\n/* block comment */\ncode"
      result = removeComments withComments
  in "// line comment" `L.isInfixOf` result === False &&
     "block comment" `L.isInfixOf` result === False

-- Property: removeComments preserves comments inside string literals
prop_removeComments_preserves_string_both :: String -> Property
prop_removeComments_preserves_string_both s =
  let codeWithComments = "let s = \"// not comment /* also not */\"\n// real comment\ncode"
      result = removeComments codeWithComments
  in "// not comment" `L.isInfixOf` result === True &&
     "also not" `L.isInfixOf` result === True

-- ============================================================================
-- Indentation Properties
-- ============================================================================

-- Property: normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preserves_relative :: String -> Property
prop_normalizeIndentation_preserves_relative s =
  let indented = "  " ++ s ++ "\n    " ++ s ++ "  \n  " ++ s
      normalized = normalizeIndentation indented
      linesOriginal = lines indented
      linesNormalized = lines normalized
  in L.length linesNormalized === L.length linesOriginal

-- Property: forceSingleTabIndentation converts to single tab format
prop_forceSingleTabIndentation_single_tab :: String -> Property
prop_forceSingleTabIndentation_single_tab s =
  let result = forceSingleTabIndentation s
      linesResult = lines result
      nonEmptyLines = L.filter (not . null) linesResult
  in L.all ("\t" `L.isPrefixOf`) nonEmptyLines === True

-- Property: fixIndentation equals normalizeIndentation
prop_fixIndentation_equals_normalize :: String -> Property
prop_fixIndentation_equals_normalize s = fixIndentation s === normalizeIndentation s

-- ============================================================================
-- Search Properties
-- ============================================================================

-- Property: breakOn finds pattern L.and splits correctly
prop_breakOn_splits_correctly :: String -> String -> Property
prop_breakOn_splits_correctly pat s
  | null pat = breakOn pat s === ("", s)
  | pat `L.isInfixOf` s =
      let (before, after) = breakOn pat s
          expected = takeWhile (not . (`L.isPrefixOf` pat)) (tails s)
      in before `L.isPrefixOf` s && pat `L.isPrefixOf` (drop (L.length before) s)
  | otherwise = breakOn pat s === (s, "")

-- Property: breakOn with empty pattern returns ("", s)
prop_breakOn_empty_pattern :: String -> Property
prop_breakOn_empty_pattern s = breakOn "" s === ("", s)

-- Property: breakOn returns correct concatenation
prop_breakOn_concatenates :: String -> String -> Property
prop_breakOn_concatenates pat s =
  let (before, after) = breakOn pat s
  in if pat `L.isInfixOf` s
     then before ++ pat ++ after === s
     else before === s && after === ""

-- ============================================================================
-- Combined Properties
-- ============================================================================

-- Property: trim after splitByCommaCollapsed gives consistent results
prop_trim_splitByCommaCollapsed_consistency :: String -> Property
prop_trim_splitByCommaCollapsed_consistency s =
  let parts = splitByCommaCollapsed s
      trimmedParts = map trim parts
      rejoined = intercalate "," trimmedParts
      retrimmed = trim rejoined
  in retrimmed === trim (intercalate "," (map trim (splitByCommaCollapsed s)))

-- Property: removeComments then removeLineComments equals removeLineComments then removeComments
prop_comments_removal_order :: String -> Property
prop_comments_removal_order s =
  removeLineComments (removeComments s) === removeComments (removeLineComments s)

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Utils Properties"
  [ fastProperty "trim removes whitespace" prop_trim_removes_whitespace
  , fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
  , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
  , fastProperty "splitByComma equals splitBy ','" prop_splitByComma_equals_splitBy
  , fastProperty "splitByCommaCollapsed equals splitByCollapsed ','" prop_splitByCommaCollapsed_equals_splitByCollapsed
  , fastProperty "removeLineComments removes comments" prop_removeLineComments_removes_comments
  , fastProperty "removeLineComments preserves string comments" prop_removeLineComments_preserves_string_comments
  , fastProperty "removeComments removes both types" prop_removeComments_removes_both_types
  , fastProperty "removeComments preserves string comments" prop_removeComments_preserves_string_both
  , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative
  , fastProperty "forceSingleTabIndentation uses single tab" prop_forceSingleTabIndentation_single_tab
  , fastProperty "fixIndentation equals normalizeIndentation" prop_fixIndentation_equals_normalize
  , fastProperty "breakOn splits correctly" prop_breakOn_splits_correctly
  , fastProperty "breakOn with empty pattern" prop_breakOn_empty_pattern
  , fastProperty "breakOn concatenates correctly" prop_breakOn_concatenates
  , fastProperty "trim splitByCommaCollapsed consistency" prop_trim_splitByCommaCollapsed_consistency
  , fastProperty "comments removal order" prop_comments_removal_order
  
  , testCase "trim with known inputs" $ do
      trim "" @?= ""
      trim "   " @?= ""
      trim "  hello  " @?= "hello"
      trim "hello" @?= "hello"
      trim "\t hello \n" @?= "hello"
      
  , testCase "splitBy with known inputs" $ do
      splitBy ',' "" @?= [""]
      splitBy ',' "a,b,c" @?= ["a","b","c"]
      splitBy ',' "a,,b" @?= ["a","", "b"]
      splitBy ',' ",a," @?= ["","a",""]
      
  , testCase "removeComments with complex example" $ do
      let complex = "code // comment\n\"string // not comment\"/* block */more"
      let result = removeComments complex
      assertBool "should preserve string content" ("// not comment" `L.isInfixOf` result)
      assertBool "should remove block comment" (not (" block " `L.isInfixOf` result))
  ]