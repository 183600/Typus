{-# LANGUAGE CPP #-}

module Test.Unit.NewUtilsStringProcessingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Text as T

import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed, 
             removeLineComments, removeComments, normalizeIndentation, 
             forceSingleTabIndentation, fixIndentation, breakOn)

tests :: TestTree
tests = testGroup "New Utils String Processing QuickCheck Tests"
  [ trimProperties
  , splitByProperties
  , commentRemovalProperties
  , indentationProperties
  , searchProperties
  ]

trimProperties :: TestTree
trimProperties = testGroup "Trim Properties"
  [ fastProperty "trim removes leading whitespace" prop_trim_removes_leading
  , fastProperty "trim removes trailing whitespace" prop_trim_removes_trailing
  , fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "trim preserves internal whitespace" prop_trim_preserves_internal
  , fastProperty "trim of empty string is empty" prop_trim_empty
  ]

splitByProperties :: TestTree
splitByProperties = testGroup "Split By Properties"
  [ fastProperty "splitBy preserves total length when concatenated with delimiter" prop_splitby_preserves_length
  , fastProperty "splitByCollapsed removes empty segments" prop_splitbycollapsed_no_empty
  , fastProperty "splitBy on empty string returns singleton" prop_splitby_empty_string
  , fastProperty "splitByCollapsed on empty string returns empty" prop_splitbycollapsed_empty_string
  , fastProperty "splitByComma is splitBy with comma" prop_splitbycomma_is_splitby
  , fastProperty "splitByCommaCollapsed is splitByCollapsed with comma" prop_splitbycomcollapsed_is_splitbycollapsed
  ]

commentRemovalProperties :: TestTree
commentRemovalProperties = testGroup "Comment Removal Properties"
  [ fastProperty "removeLineComments preserves lines without // marker" prop_removelinecomments_preserves_noncomment
  , fastProperty "removeLineComments reduces line count when removing comments" prop_removelinecomments_line_count
  , fastProperty "removeComments never increases string length" prop_removecomments_never_increases
  , fastProperty "removeComments preserves non-comment content" prop_removecomments_preserves_content
  ]

indentationProperties :: TestTree
indentationProperties = testGroup "Indentation Properties"
  [ fastProperty "normalizeIndentation preserves line count" prop_normalizeindentation_preserves_lines
  , fastProperty "normalizeIndentation is idempotent" prop_normalizeindentation_idempotent
  , fastProperty "forceSingleTabIndentation adds tab to non-empty lines" prop_forcesingletab_adds_tab
  , fastProperty "fixIndentation equals normalizeIndentation" prop_fixindentation_equals_normalize
  ]

searchProperties :: TestTree
searchProperties = testGroup "Search Properties"
  [ fastProperty "breakOn finds pattern when present" prop_breakon_finds_pattern
  , fastProperty "breakOn returns original string when pattern absent" prop_breakon_absent_pattern
  , fastProperty "breakOn with empty pattern returns empty prefix" prop_breakon_empty_pattern
  ]

-- Trim properties
prop_trim_removes_leading :: String -> Property
prop_trim_removes_leading s =
  let trimmed = trim s
      originalLines = lines s
      trimmedLines = lines trimmed
  in not (null originalLines) && not (null (head originalLines)) && isSpace (head (head originalLines)) ==>
     property $ case trimmedLines of
       [] -> True
       (t:_) -> null t || not (isSpace (head t))

prop_trim_removes_trailing :: String -> Property
prop_trim_removes_trailing s =
  let trimmed = trim s
      originalLines = lines s
  in not (null originalLines) && not (null (last originalLines)) && isSpace (last (last originalLines)) ==>
     property $ case lines trimmed of
       [] -> True
       ts -> null (last ts) || not (isSpace (last (last ts)))

prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
  in property $ trim trimmed == trimmed

prop_trim_preserves_internal :: String -> Property
prop_trim_preserves_internal s =
  let trimmed = trim s
      wordsOriginal = words s
      wordsTrimmed = words trimmed
  in property $ length wordsOriginal == length wordsTrimmed

prop_trim_empty :: Property
prop_trim_empty = property $ trim "" == ""

-- Split by properties
prop_splitby_preserves_length :: Char -> String -> Property
prop_splitby_preserves_length delim s =
  let parts = splitBy delim s
      reconstructed = concat $ intersperse [delim] parts
  in property $ s == reconstructed

prop_splitbycollapsed_no_empty :: Char -> String -> Property
prop_splitbycollapsed_no_empty delim s =
  let parts = splitByCollapsed delim s
  in property $ all (not . null) parts

prop_splitby_empty_string :: Char -> Property
prop_splitby_empty_string delim =
  property $ splitBy delim "" == [""]

prop_splitbycollapsed_empty_string :: Char -> Property
prop_splitbycollapsed_empty_string delim =
  property $ splitByCollapsed delim "" == []

prop_splitbycomma_is_splitby :: String -> Property
prop_splitbycomma_is_splitby s =
  property $ splitByComma s == splitBy ',' s

prop_splitbycomcollapsed_is_splitbycollapsed :: String -> Property
prop_splitbycomcollapsed_is_splitbycollapsed s =
  property $ splitByCommaCollapsed s == splitByCollapsed ',' s

-- Comment removal properties
prop_removelinecomments_preserves_noncomment :: String -> Property
prop_removelinecomments_preserves_noncomment s =
  "//" `notElem` words s ==>
  property $ removeLineComments s == s

prop_removelinecomments_line_count :: String -> Property
prop_removelinecomments_line_count s =
  let originalLines = lines s
      processedLines = lines (removeLineComments s)
  in property $ length processedLines <= length originalLines

prop_removecomments_never_increases :: String -> Property
prop_removecomments_never_increases s =
  property $ length (removeComments s) <= length s

prop_removecomments_preserves_content :: String -> Property
prop_removecomments_preserves_content s =
  let withoutComments = removeComments s
      -- Check that alphanumeric words are preserved
      originalWords = filter (all isAlphaNum) $ words s
      processedWords = filter (all isAlphaNum) $ words withoutComments
  in property $ all (`elem` processedWords) originalWords

-- Indentation properties
prop_normalizeindentation_preserves_lines :: String -> Property
prop_normalizeindentation_preserves_lines s =
  let originalLines = lines s
      normalizedLines = lines (normalizeIndentation s)
  in property $ length originalLines == length normalizedLines

prop_normalizeindentation_idempotent :: String -> Property
prop_normalizeindentation_idempotent s =
  let normalized = normalizeIndentation s
  in property $ normalizeIndentation normalized == normalized

prop_forcesingletab_adds_tab :: String -> Property
prop_forcesingletab_adds_tab s =
  let processed = forceSingleTabIndentation s
      processedLines = lines processed
      nonEmptyLines = filter (not . null) processedLines
  in property $ all ("\t" `isPrefixOf`) nonEmptyLines

prop_fixindentation_equals_normalize :: String -> Property
prop_fixindentation_equals_normalize s =
  property $ fixIndentation s == normalizeIndentation s

-- Search properties
prop_breakon_finds_pattern :: String -> String -> Property
prop_breakon_finds_pattern pattern s =
  not (null pattern) && pattern `isInfixOf` s ==>
  let (prefix, suffix) = breakOn pattern s
  in property $ pattern `isSuffixOf` prefix && pattern `isPrefixOf` suffix

prop_breakon_absent_pattern :: String -> String -> Property
prop_breakon_absent_pattern pattern s =
  not (null pattern) && pattern `notElem` s ==>
  let (prefix, suffix) = breakOn pattern s
  in property $ prefix == s && suffix == ""

prop_breakon_empty_pattern :: String -> Property
prop_breakon_empty_pattern s =
  let (prefix, suffix) = breakOn "" s
  in property $ prefix == "" && suffix == s

-- Helper function
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)
  where
    tails [] = [[]]
    tails xs@(_:ys) = xs : tails ys