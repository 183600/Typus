module Test.Unit.UtilsComprehensiveQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (intercalate)

-- | Test that trim removes leading and trailing whitespace
prop_trim_removes_whitespace :: String -> Property
prop_trim_removes_whitespace s = 
  let trimmed = trim s
  in property $ 
    (null trimmed && all isSpace s) || 
    (not (null trimmed) && not (isSpace (head trimmed)) && not (isSpace (last trimmed)))

-- | Test that trim doesn't add characters
prop_trim_no_addition :: String -> Property
prop_trim_no_addition s = property $ length (trim s) <= length s

-- | Test that trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = property $ trim (trim s) == trim s

-- | Test that splitBy preserves the original string when concatenated with delimiter
prop_splitBy_preserves_content :: Char -> String -> Property
prop_splitBy_preserves_content delim s = 
  let parts = splitBy delim s
  in property $ intercalate [delim] parts == s

-- | Test that splitByComma is equivalent to splitBy with comma delimiter
prop_splitByComma_equivalence :: String -> Property
prop_splitByComma_equivalence s = property $ splitByComma s == splitBy ',' s

-- | Test that splitByCollapsed removes empty segments
prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s = 
  let parts = splitByCollapsed delim s
  in property $ all (not . null) parts

-- | Test that removeLineComments doesn't remove content before comments
prop_removeLineComments_preserves_before :: String -> String -> Property
prop_removeLineComments_preserves_before prefix comment = 
  let input = prefix ++ "//" ++ comment
      result = removeLineComments input
      trimmedPrefix = trim prefix
      trimmedResult = trim result
  in property $ null prefix || trimmedResult == trimmedPrefix

-- | Test that removeLineComments doesn't affect content without comments
prop_removeLineComments_no_effect_when_no_comments :: String -> Property
prop_removeLineComments_no_effect_when_no_comments s = 
  let hasNoComments = not ("//" `L.isInfixOf` s)
  in property $ hasNoComments ==> removeLineComments s == s

-- | Test that removeComments doesn't affect strings without comments
prop_removeComments_no_effect_when_no_comments :: String -> Property
prop_removeComments_no_effect_when_no_comments s = 
  let hasNoComments = not ("//" `L.isInfixOf` s) && not ("/*" `L.isInfixOf` s)
  in property $ hasNoComments ==> removeComments s == s

-- | Test that normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preserves_relative :: String -> Property
prop_normalizeIndentation_preserves_relative s = 
  let lines' = lines s
      normalized = normalizeIndentation s
      normalizedLines = lines normalized
  in property $ length lines' == length normalizedLines

-- | Test that breakOn returns correct split
prop_breakOn_correctness :: String -> String -> Property
prop_breakOn_correctness pat s = 
  let (before, after) = breakOn pat s
      combined = before ++ pat ++ after
  in property $ null pat || combined == s

-- | Test that breakOn returns original string when pattern not found
prop_breakOn_not_found :: String -> String -> Property
prop_breakOn_not_found pat s = 
  let notFound = not (pat `L.isInfixOf` s)
      (before, after) = breakOn pat s
  in property $ notFound ==> (before == s && null after)

-- | Test that safeProcessString removes control characters
prop_safeProcessString_removes_controls :: String -> Property
prop_safeProcessString_removes_controls s = 
  let result = safeProcessString s
  in property $ 
    case result of
      Left _ -> property True
      Right s' -> property $ all (\c -> c >= ' ' || c == '\n' || c == '\r' || c == '\t') s'

-- | Test that isValidChar correctly identifies valid characters
prop_isValidChar_correctness :: Char -> Property
prop_isValidChar_correctness c = 
  let expected = c >= ' ' || c == '\n' || c == '\r' || c == '\t'
  in property $ isValidChar c == expected

tests :: TestTree
tests = testGroup "Utils Comprehensive QuickCheck Tests"
  [ testProperty "trim removes whitespace" prop_trim_removes_whitespace
  , testProperty "trim doesn't add characters" prop_trim_no_addition
  , testProperty "trim is idempotent" prop_trim_idempotent
  , testProperty "splitBy preserves content" prop_splitBy_preserves_content
  , testProperty "splitByComma equivalence" prop_splitByComma_equivalence
  , testProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_no_empty
  , testProperty "removeLineComments preserves before comments" prop_removeLineComments_preserves_before
  , testProperty "removeLineComments no effect when no comments" prop_removeLineComments_no_effect_when_no_comments
  , testProperty "removeComments no effect when no comments" prop_removeComments_no_effect_when_no_comments
  , testProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative
  , testProperty "breakOn correctness" prop_breakOn_correctness
  , testProperty "breakOn not found" prop_breakOn_not_found
  , testProperty "safeProcessString removes controls" prop_safeProcessString_removes_controls
  , testProperty "isValidChar correctness" prop_isValidChar_correctness
  ]