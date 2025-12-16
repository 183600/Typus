{-# LANGUAGE CPP #-}

module Test.Unit.UtilsPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import Data.List (intersperse, isInfixOf, isPrefixOf)

import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed,
             removeLineComments, removeComments, normalizeIndentation, 
             forceSingleTabIndentation, fixIndentation, breakOn)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Utils Properties QuickCheck"
  [ stringManipulationTests
  , splittingTests
  , commentRemovalTests
  , indentationTests
  , searchTests
  ]

stringManipulationTests :: TestTree
stringManipulationTests = testGroup "String Manipulation Properties"
  [ fastProperty "trim removes leading and trailing whitespace" prop_trim_removes_whitespace
  , fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "trim of empty string is empty" prop_trim_empty
  , fastProperty "trim preserves non-whitespace content" prop_trim_preserves_content
  , fastProperty "trim of all whitespace is empty" prop_trim_all_whitespace
  ]

splittingTests :: TestTree
splittingTests = testGroup "Splitting Properties"
  [ fastProperty "splitBy preserves total length" prop_splitBy_preserves_length
  , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_no_empty
  , fastProperty "splitBy and join are inverse" prop_splitBy_join_inverse
  , fastProperty "splitByComma equals splitBy ','" prop_splitByComma_equals_splitBy
  , fastProperty "splitByCommaCollapsed equals splitByCollapsed ','" prop_splitByCommaCollapsed_equals_splitByCollapsed
  , fastProperty "splitBy on empty string returns singleton" prop_splitBy_empty_singleton
  , fastProperty "splitByCollapsed on empty string returns empty" prop_splitByCollapsed_empty_empty
  ]

commentRemovalTests :: TestTree
commentRemovalTests = testGroup "Comment Removal Properties"
  [ fastProperty "removeLineComments removes all // comments" prop_removeLineComments_removes
  , fastProperty "removeLineComments preserves non-comment lines" prop_removeLineComments_preserves
  , fastProperty "removeComments removes // and /* */" prop_removeComments_removes_both
  , fastProperty "removeComments preserves non-comment content" prop_removeComments_preserves
  , fastProperty "removeComments handles nested block comments" prop_removeComments_nested
  ]

indentationTests :: TestTree
indentationTests = testGroup "Indentation Properties"
  [ fastProperty "normalizeIndentation preserves line count" prop_normalizeIndentation_preserves_lines
  , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_relative
  , fastProperty "forceSingleTabIndentation uses only tabs" prop_forceSingleTabIndentation_tabs_only
  , fastProperty "fixIndentation equals normalizeIndentation" prop_fixIndentation_equals_normalize
  , fastProperty "normalizeIndentation preserves non-empty lines" prop_normalizeIndentation_preserves_nonempty
  ]

searchTests :: TestTree
searchTests = testGroup "Search Properties"
  [ fastProperty "breakOn finds substring" prop_breakOn_finds
  , fastProperty "breakOn returns correct position" prop_breakOn_position
  , fastProperty "breakOn on empty string returns empty" prop_breakOn_empty
  , fastProperty "breakOn on non-existent substring returns original" prop_breakOn_not_found
  ]

-- String Manipulation Properties
prop_trim_removes_whitespace :: String -> Property
prop_trim_removes_whitespace s =
  let trimmed = trim s
  in not (null trimmed) ==> 
     property (not (head trimmed `elem` " \t\n\r") && not (last trimmed `elem` " \t\n\r"))

prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
  in trim trimmed === trimmed

prop_trim_empty :: Property
prop_trim_empty =
  trim "" === ""

prop_trim_preserves_content :: String -> Property
prop_trim_preserves_content s =
  let trimmed = trim s
      content = filter (not . isSpace) s
      trimmedContent = filter (not . isSpace) trimmed
  in content === trimmedContent

prop_trim_all_whitespace :: Property
prop_trim_all_whitespace =
  let allWhitespace = " \t\n\r \t \n\r "
  in trim allWhitespace === ""

-- Splitting Properties
prop_splitBy_preserves_length :: Char -> String -> Property
prop_splitBy_preserves_length delim s =
  let parts = splitBy delim s
      rejoined = concat $ intersperse [delim] parts
  in rejoined === s

prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s =
  let parts = splitByCollapsed delim s
  in property $ all (not . null) parts

prop_splitBy_join_inverse :: Char -> NonEmptyList String -> Property
prop_splitBy_join_inverse delim (NonEmpty parts) =
  let s = concat $ intersperse [delim] parts
  in splitBy delim s === parts

prop_splitByComma_equals_splitBy :: String -> Property
prop_splitByComma_equals_splitBy s =
  splitByComma s === splitBy ',' s

prop_splitByCommaCollapsed_equals_splitByCollapsed :: String -> Property
prop_splitByCommaCollapsed_equals_splitByCollapsed s =
  splitByCommaCollapsed s === splitByCollapsed ',' s

prop_splitBy_empty_singleton :: Char -> Property
prop_splitBy_empty_singleton delim =
  splitBy delim "" === [""]

prop_splitByCollapsed_empty_empty :: Char -> Property
prop_splitByCollapsed_empty_empty delim =
  splitByCollapsed delim "" === []

-- Comment Removal Properties
prop_removeLineComments_removes :: String -> Property
prop_removeLineComments_removes s =
  let withComments = s ++ "\n// This is a comment\n// Another comment"
      withoutComments = removeLineComments withComments
  in property $ "//" `notElem` words withoutComments

prop_removeLineComments_preserves :: String -> Property
prop_removeLineComments_preserves s =
  "//" `notElem` words s ==>
  property $ not $ "//" `elem` words (removeLineComments s)

prop_removeComments_removes_both :: String -> Property
prop_removeComments_removes_both s =
  let withComments = s ++ "\n// Line comment\n/* Block comment */"
      withoutComments = removeComments withComments
  in property $ "//" `notElem` words withoutComments && "/*" `notElem` words withoutComments

prop_removeComments_preserves :: String -> Property
prop_removeComments_preserves s =
  "//" `notElem` words s && "/*" `notElem` words s ==>
  property $ removeComments s === s

prop_removeComments_nested :: String -> Property
prop_removeComments_nested s =
  let withNested = s ++ "\n/* Outer /* inner */ comment */"
      withoutNested = removeComments withNested
  in property $ "/*" `notElem` words withoutNested

-- Indentation Properties
prop_normalizeIndentation_preserves_lines :: String -> Property
prop_normalizeIndentation_preserves_lines s =
  let normalized = normalizeIndentation s
      originalLines = lines s
      normalizedLines = lines normalized
  in length originalLines === length normalizedLines

prop_normalizeIndentation_relative :: String -> Property
prop_normalizeIndentation_relative s =
  let normalized = normalizeIndentation s
      originalLines = lines s
      normalizedLines = lines normalized
  in length originalLines === length normalizedLines

prop_forceSingleTabIndentation_tabs_only :: String -> Property
prop_forceSingleTabIndentation_tabs_only s =
  let tabIndented = forceSingleTabIndentation s
      lines' = lines tabIndented
      leadingSpaces = map (takeWhile (== ' ')) lines'
  in property $ all null leadingSpaces

prop_fixIndentation_equals_normalize :: String -> Property
prop_fixIndentation_equals_normalize s =
  fixIndentation s === normalizeIndentation s

prop_normalizeIndentation_preserves_nonempty :: String -> Property
prop_normalizeIndentation_preserves_nonempty s =
  let normalized = normalizeIndentation s
      originalNonEmpty = filter (not . null) (lines s)
      normalizedNonEmpty = filter (not . null) (lines normalized)
  in length originalNonEmpty === length normalizedNonEmpty

-- Search Properties
prop_breakOn_finds :: String -> String -> Property
prop_breakOn_finds needle haystack =
  not (null needle) && needle `isInfixOf` haystack ==>
  let (before, after) = breakOn needle haystack
  in property $ needle `isInfixOf` after

prop_breakOn_position :: String -> String -> Property
prop_breakOn_position needle haystack =
  not (null needle) && needle `isInfixOf` haystack ==>
  let (before, after) = breakOn needle haystack
      original = before ++ needle ++ after
  in original === haystack

prop_breakOn_empty :: String -> Property
prop_breakOn_empty haystack =
  breakOn "" haystack === ("", haystack)

prop_breakOn_not_found :: String -> String -> Property
prop_breakOn_not_found needle haystack =
  not (null needle) && needle `notElem` words haystack ==>
  breakOn needle haystack === (haystack, "")