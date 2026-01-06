{-# LANGUAGE CPP #-}

module Test.Unit.NewUtilsStringProcessingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.Char (isSpace, isAlphaNum, isDigit, isLetter)
import qualified Data.List as L
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
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
  , textProcessingProperties
  ]

trimProperties :: TestTree
trimProperties = testGroup "Trim Properties"
  [ fastProperty "trim removes leading whitespace" prop_trim_removes_leading
  , fastProperty "trim removes trailing whitespace" prop_trim_removes_trailing
  , fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "trim preserves internal whitespace" prop_trim_preserves_internal
  , fastProperty "trim of empty string is empty" prop_trim_empty
  , fastProperty "trim preserves alphanumeric content" prop_trim_preserves_alphanumeric
  ]

splitByProperties :: TestTree
splitByProperties = testGroup "Split By Properties"
  [ fastProperty "splitBy preserves total L.length when concatenated with delimiter" prop_splitby_preserves_length
  , fastProperty "splitByCollapsed removes empty segments" prop_splitbycollapsed_no_empty
  , fastProperty "splitBy on empty string returns singleton" prop_splitby_empty_string
  , fastProperty "splitByCollapsed on empty string returns empty" prop_splitbycollapsed_empty_string
  , fastProperty "splitByComma is splitBy with comma" prop_splitbycomma_is_splitby
  , fastProperty "splitByCommaCollapsed is splitByCollapsed with comma" prop_splitbycomcollapsed_is_splitbycollapsed
  , fastProperty "splitBy handles Unicode correctly" prop_splitby_unicode
  ]

commentRemovalProperties :: TestTree
commentRemovalProperties = testGroup "Comment Removal Properties"
  [ fastProperty "removeLineComments preserves lines without // marker" prop_removelinecomments_preserves_noncomment
  , fastProperty "removeLineComments reduces line count when removing comments" prop_removelinecomments_line_count
  , fastProperty "removeComments never increases string L.length" prop_removecomments_never_increases
  , fastProperty "removeComments preserves non-comment content" prop_removecomments_preserves_content
  , fastProperty "removeComments handles nested comments" prop_removecomments_nested
  ]

indentationProperties :: TestTree
indentationProperties = testGroup "Indentation Properties"
  [ fastProperty "normalizeIndentation preserves line count" prop_normalizeindentation_preserves_lines
  , fastProperty "normalizeIndentation is idempotent" prop_normalizeindentation_idempotent
  , fastProperty "forceSingleTabIndentation adds tab to non-empty lines" prop_forcesingletab_adds_tab
  , fastProperty "fixIndentation equals normalizeIndentation" prop_fixindentation_equals_normalize
  , fastProperty "normalizeIndentation handles mixed whitespace" prop_normalizeindentation_mixed_whitespace
  ]

searchProperties :: TestTree
searchProperties = testGroup "Search Properties"
  [ fastProperty "breakOn finds pattern when present" prop_breakon_finds_pattern
  , fastProperty "breakOn returns original string when pattern absent" prop_breakon_absent_pattern
  , fastProperty "breakOn with empty pattern returns empty prefix" prop_breakon_empty_pattern
  , fastProperty "breakOn handles multiple occurrences" prop_breakon_multiple_occurrences
  ]

textProcessingProperties :: TestTree
textProcessingProperties = testGroup "Text Processing Properties"
  [ fastProperty "string processing preserves alphanumeric sequences" prop_preserves_alphanumeric_sequences
  , fastProperty "whitespace normalization is consistent" prop_whitespace_normalization_consistent
  , fastProperty "case conversion preserves L.length" prop_case_conversion_preserves_length
  ]

-- Trim properties
prop_trim_removes_leading :: String -> Property
prop_trim_removes_leading s =
  let trimmed = trim s
      originalLines = lines s
      trimmedLines = lines trimmed
  in not (null originalLines) && not (L.null (L.head originalLines)) && isSpace (L.head (L.head originalLines)) ==>
     property $ case trimmedLines of
       [] -> True
       (t:_) -> null t || not (isSpace (L.head t))

prop_trim_removes_trailing :: String -> Property
prop_trim_removes_trailing s =
  let trimmed = trim s
      originalLines = lines s
  in not (null originalLines) && not (L.null (last originalLines)) && isSpace (last (last originalLines)) ==>
     property $ case lines trimmed of
       [] -> True
       ts -> L.null (last ts) || not (isSpace (last (last ts)))

prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
  in property $ trim trimmed == trimmed

prop_trim_preserves_internal :: String -> Property
prop_trim_preserves_internal s =
  let trimmed = trim s
      wordsOriginal = words s
      wordsTrimmed = words trimmed
  in property $ L.length wordsOriginal == L.length wordsTrimmed

prop_trim_empty :: Property
prop_trim_empty = property $ trim "" == ""

prop_trim_preserves_alphanumeric :: String -> Property
prop_trim_preserves_alphanumeric s =
  let trimmed = trim s
      originalAlpha = filter isAlphaNum s
      trimmedAlpha = filter isAlphaNum trimmed
  in property $ originalAlpha == trimmedAlpha

-- Split by properties
prop_splitby_preserves_length :: Char -> String -> Property
prop_splitby_preserves_length delim s =
  let parts = splitBy delim s
      reconstructed = L.concat $ intersperse [delim] parts
  in property $ s == reconstructed

prop_splitbycollapsed_no_empty :: Char -> String -> Property
prop_splitbycollapsed_no_empty delim s =
  let parts = splitByCollapsed delim s
  in property $ L.all (not . null) parts

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

prop_splitby_unicode :: String -> Property
prop_splitby_unicode s =
  let delim = '∑'  -- Unicode character
      parts = splitBy delim s
  in property $ L.concat (intersperse [delim] parts) == s

-- Comment removal properties
prop_removelinecomments_preserves_noncomment :: String -> Property
prop_removelinecomments_preserves_noncomment s =
  "//" `notElem` words s ==>
  property $ removeLineComments s == s

prop_removelinecomments_line_count :: String -> Property
prop_removelinecomments_line_count s =
  let originalLines = lines s
      processedLines = lines (removeLineComments s)
  in property $ L.length processedLines <= L.length originalLines

prop_removecomments_never_increases :: String -> Property
prop_removecomments_never_increases s =
  property $ L.length (removeComments s) <= L.length s

prop_removecomments_preserves_content :: String -> Property
prop_removecomments_preserves_content s =
  let withoutComments = removeComments s
      -- Check that alphanumeric words are preserved
      originalWords = L.filter (L.all isAlphaNum) $ words s
      processedWords = L.filter (L.all isAlphaNum) $ words withoutComments
  in property $ L.all (`elem` processedWords) originalWords

prop_removecomments_nested :: String -> Property
prop_removecomments_nested s =
  let withNested = s ++ " /* outer /* inner */ still outer */ end"
      processed = removeComments withNested
  in property $ "/*" `notElem` processed && "*/" `notElem` processed

-- Indentation properties
prop_normalizeindentation_preserves_lines :: String -> Property
prop_normalizeindentation_preserves_lines s =
  let originalLines = lines s
      normalizedLines = lines (normalizeIndentation s)
  in property $ L.length originalLines == L.length normalizedLines

prop_normalizeindentation_idempotent :: String -> Property
prop_normalizeindentation_idempotent s =
  let normalized = normalizeIndentation s
  in property $ normalizeIndentation normalized == normalized

prop_forcesingletab_adds_tab :: String -> Property
prop_forcesingletab_adds_tab s =
  let processed = forceSingleTabIndentation s
      processedLines = lines processed
      nonEmptyLines = L.filter (not . null) processedLines
  in property $ L.all ("\t" `L.isPrefixOf`) nonEmptyLines

prop_fixindentation_equals_normalize :: String -> Property
prop_fixindentation_equals_normalize s =
  property $ fixIndentation s == normalizeIndentation s

prop_normalizeindentation_mixed_whitespace :: String -> Property
prop_normalizeindentation_mixed_whitespace s =
  let mixed = "  \t   \t  " ++ s ++ "\t\t  "
      normalized = normalizeIndentation mixed
  in property $ not (L.any (isPrefixOf "  \t") (lines normalized))

-- Search properties
prop_breakon_finds_pattern :: String -> String -> Property
prop_breakon_finds_pattern pattern s =
  not (null pattern) && pattern `L.isInfixOf` s ==>
  let (prefix, suffix) = breakOn pattern s
  in property $ pattern `L.isSuffixOf` prefix && pattern `L.isPrefixOf` suffix

prop_breakon_absent_pattern :: String -> String -> Property
prop_breakon_absent_pattern pattern s =
  not (null pattern) && pattern `notElem` s ==>
  let (prefix, suffix) = breakOn pattern s
  in property $ prefix == s && suffix == ""

prop_breakon_empty_pattern :: String -> Property
prop_breakon_empty_pattern s =
  let (prefix, suffix) = breakOn "" s
  in property $ prefix == "" && suffix == s

prop_breakon_multiple_occurrences :: String -> String -> Property
prop_breakon_multiple_occurrences pattern s =
  not (null pattern) && pattern `L.isInfixOf` s ==>
  let (prefix, suffix) = breakOn pattern s
      prefixLength = L.length prefix
  in property $ prefixLength < L.length s

-- Text processing properties
prop_preserves_alphanumeric_sequences :: String -> Property
prop_preserves_alphanumeric_sequences s =
  let alphaSequences = L.filter (L.all isAlphaNum) $ words s
      processed = removeComments (trim s)
      processedSequences = L.filter (L.all isAlphaNum) $ words processed
  in property $ L.all (`elem` processedSequences) alphaSequences

prop_whitespace_normalization_consistent :: String -> Property
prop_whitespace_normalization_consistent s =
  let normalized1 = normalizeIndentation s
      normalized2 = normalizeIndentation normalized1
  in property $ normalized1 == normalized2

prop_case_conversion_preserves_length :: String -> Property
prop_case_conversion_preserves_length s =
  let upper = map toUpper s
      lower = map toLower s
  in property $ L.length s == L.length upper && L.length s == L.length lower
  where
    toUpper c
      | isLower c = toEnum (fromEnum c - 32)
      | otherwise = c
    toLower c
      | isUpper c = toEnum (fromEnum c + 32)
      | otherwise = c

-- Helper functions
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs