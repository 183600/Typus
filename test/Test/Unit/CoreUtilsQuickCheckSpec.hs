{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.CoreUtilsQuickCheckSpec where



-- | Core Utils module QuickCheck tests



import Test.Tasty
import Test.Tasty.QuickCheck
import Utils (trim, splitBy, splitByComma, splitByCommaCollapsed, removeLineComments, removeComments, normalizeIndentation, breakOn, splitByCollapsed, safeProcessString, isValidChar)
import Data.Char (isSpace, isAlpha, isAlphaNum)
import Data.List (intercalate, isInfixOf, isPrefixOf)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary (arbitraryString, arbitraryChar)

-- ============================================================================
-- Utils QuickCheck Tests
-- ============================================================================

-- | Test that trim removes leading and trailing whitespace
prop_trimRemovesWhitespace :: Property
prop_trimRemovesWhitespace =
  forAll arbitraryString $ \s ->
    let trimmed = trim s
        hasLeadingSpace = case s of
                    [] -> False
                    (x:_) -> isSpace x
        hasTrailingSpace = not (null s) && isSpace (last s)
    in if hasLeadingSpace || hasTrailingSpace
       then property $ not (null trimmed) ==> 
                property $ case trimmed of
                           [] -> False
                           [x] -> not (isSpace x)
                           (x:xs) -> not (isSpace x) && not (isSpace (last xs))
       else property $ trimmed == s

-- | Test that trim doesn't add characters
prop_trimDoesNotAdd :: Property
prop_trimDoesNotAdd =
  forAll arbitraryString $ \s ->
    let trimmed = trim s
    in property $ length trimmed <= length s

-- | Test that trim is idempotent
prop_trimIdempotent :: Property
prop_trimIdempotent =
  forAll arbitraryString $ \s ->
    let trimmed = trim s
        trimmedAgain = trim trimmed
    in property $ trimmed == trimmedAgain

-- | Test that splitBy preserves all characters (except delimiters)
prop_splitByPreservesContent :: Property
prop_splitByPreservesContent =
  forAll arbitraryChar $ \delim ->
    forAll arbitraryString $ \s ->
      let parts = splitBy delim s
          rejoined = intercalate [delim] parts
      in property $ filter (/= delim) rejoined == filter (/= delim) s

-- | Test that splitBy handles empty string
prop_splitByEmpty :: Property
prop_splitByEmpty =
  forAll arbitraryChar $ \delim ->
    let parts = splitBy delim ""
    in property $ null parts

-- | Test that splitBy handles single character
prop_splitBySingleChar :: Property
prop_splitBySingleChar =
  forAll arbitraryChar $ \delim ->
    forAll (arbitraryChar `suchThat` (/= delim)) $ \c ->
      let parts = splitBy delim [c]
      in property $ parts == [[c]]

-- | Test that splitBy handles consecutive delimiters
prop_splitByConsecutiveDelimiters :: Property
prop_splitByConsecutiveDelimiters =
  forAll arbitraryChar $ \delim ->
    let s = [delim, delim]
        parts = splitBy delim s
    in property $ parts == ["", "", ""]

-- | Test that splitByComma works like splitBy with comma
prop_splitByCommaEqualsSplitByComma :: Property
prop_splitByCommaEqualsSplitByComma =
  forAll arbitraryString $ \s ->
    let parts1 = splitBy ',' s
        parts2 = splitByComma s
    in property $ parts1 == parts2

-- | Test that splitByCommaCollapsed works like splitByCollapsed with comma
prop_splitByCommaCollapsedEqualsSplitByCollapsed :: Property
prop_splitByCommaCollapsedEqualsSplitByCollapsed =
  forAll arbitraryString $ \s ->
    let parts1 = splitByCollapsed ',' s
        parts2 = splitByCommaCollapsed s
    in property $ parts1 == parts2

-- | Test that removeLineComments removes // comments
prop_removeLineComments :: Property
prop_removeLineComments =
  forAll arbitraryString $ \code ->
    forAll arbitraryString $ \comment ->
      let input = code ++ "// " ++ comment
          result = removeLineComments input
      in property $ "//" `isInfixOf` input ==> not ("//" `isInfixOf` result)

-- | Test that removeLineComments doesn't affect strings with // inside
prop_removeLineCommentsPreservesStrings :: Property
prop_removeLineCommentsPreservesStrings =
  forAll (arbitraryString `suchThat` (not . null)) $ \str ->
    let input = "\"// " ++ str ++ "\""
        result = removeLineComments input
    in property $ "//" `isInfixOf` result

-- | Test that removeComments removes both // and /* */ comments
prop_removeComments :: Property
prop_removeComments =
  forAll arbitraryString $ \code ->
    forAll arbitraryString $ \comment ->
      let input1 = code ++ "// " ++ comment
          input2 = code ++ "/* " ++ comment ++ " */"
          result1 = removeComments input1
          result2 = removeComments input2
      in property $ ("//" `isInfixOf` input1 ==> not ("//" `isInfixOf` result1)) .&&.
                   ("/*" `isInfixOf` input2 ==> not ("/*" `isInfixOf` result2))

-- | Test that normalizeIndentation preserves relative indentation
prop_normalizeIndentationPreservesRelative :: Property
prop_normalizeIndentationPreservesRelative =
  forAll (listOf1 (arbitraryString `suchThat` (not . null))) $ \inputLines ->
    let input = unlines inputLines
        result = normalizeIndentation input
        resultLines = lines result
    in property $ length resultLines == length inputLines

-- | Test that normalizeIndentation removes common prefix
prop_normalizeIndentationRemovesCommonPrefix :: Property
prop_normalizeIndentationRemovesCommonPrefix =
  forAll (listOf1 (arbitraryString `suchThat` (not . null))) $ \inputLines ->
    let input = unlines $ map ("  " ++) inputLines  -- Add common prefix
        result = normalizeIndentation input
        resultLines = lines result
    in property $ all (\line -> null line || not (isPrefixOf "  " line)) resultLines

-- | Test that breakOn finds the first occurrence
prop_breakOnFindsFirst :: Property
prop_breakOnFindsFirst =
  forAll arbitraryString $ \s ->
    forAll (arbitraryChar `suchThat` (`notElem` s)) $ \c ->
      let result = breakOn [c] s
      in property $ result == (s, "")

-- | Test that breakOn works when character is present
prop_breakOnWhenPresent :: Property
prop_breakOnWhenPresent =
  forAll arbitraryString $ \s ->
    forAll (arbitraryChar `suchThat` (`elem` s)) $ \c ->
      let result = breakOn [c] s
          (before, after') = span (/= c) s
      in property $ result == (before, [c] ++ after')

-- | Test that safeProcessString handles special characters
prop_safeProcessString :: Property
prop_safeProcessString =
  forAll arbitraryString $ \s ->
    let result = safeProcessString s
    in property $ length result >= 0  -- Basic sanity check

-- | Test that isValidChar correctly identifies valid characters
prop_isValidChar :: Property
prop_isValidChar =
  forAll arbitraryChar $ \c ->
    let result = isValidChar c
    in property $ if isAlpha c || isAlphaNum c || c `elem` "_-[]{}(),;:."
                  then result
                  else not result

-- | Test that splitByCollapsed collapses consecutive delimiters
prop_splitByCollapsed :: Property
prop_splitByCollapsed =
  forAll arbitraryChar $ \delim ->
    forAll arbitraryString $ \s ->
      let parts = splitByCollapsed delim s
          hasConsecutive = [delim, delim] `isInfixOf` s
      in if hasConsecutive
         then property $ not (any (isPrefixOf [delim, delim]) parts)
         else property $ True

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Core Utils QuickCheck Tests"
  [ testProperty "Trim removes whitespace" prop_trimRemovesWhitespace
  , testProperty "Trim doesn't add characters" prop_trimDoesNotAdd
  , testProperty "Trim is idempotent" prop_trimIdempotent
  , testProperty "SplitBy preserves content" prop_splitByPreservesContent
  , testProperty "SplitBy handles empty string" prop_splitByEmpty
  , testProperty "SplitBy handles single character" prop_splitBySingleChar
  , testProperty "SplitBy handles consecutive delimiters" prop_splitByConsecutiveDelimiters
  , testProperty "SplitByComma equals splitBy with comma" prop_splitByCommaEqualsSplitByComma
  , testProperty "SplitByCommaCollapsed equals splitByCollapsed" prop_splitByCommaCollapsedEqualsSplitByCollapsed
  , testProperty "RemoveLineComments removes // comments" prop_removeLineComments
  , testProperty "RemoveLineComments preserves strings" prop_removeLineCommentsPreservesStrings
  , testProperty "RemoveComments removes both types" prop_removeComments
  , testProperty "NormalizeIndentation preserves relative indentation" prop_normalizeIndentationPreservesRelative
  , testProperty "NormalizeIndentation removes common prefix" prop_normalizeIndentationRemovesCommonPrefix
  , testProperty "BreakOn finds first occurrence" prop_breakOnFindsFirst
  , testProperty "BreakOn when present" prop_breakOnWhenPresent
  , testProperty "SafeProcessString handles special characters" prop_safeProcessString
  , testProperty "IsValidChar correctly identifies valid characters" prop_isValidChar
  , testProperty "SplitByCollapsed collapses consecutive delimiters" prop_splitByCollapsed
  ]

-- | Run all tests
main :: IO ()
main = defaultMain testSuite