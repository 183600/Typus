{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewUtilsPropertiesQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils (trim, splitBy, splitByCollapsed, removeComments, normalizeIndentation, breakOn)
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)

-- | Test trim function properties
prop_trim_idempotent :: String -> Bool
prop_trim_idempotent s = trim (trim s) == trim s

prop_trim_no_leading_trailing_spaces :: String -> Bool
prop_trim_no_leading_trailing_spaces s = 
    let trimmed = trim s
    in null trimmed || 
       (not (isSpace (L.head trimmed)) && not (isSpace (last trimmed)))

prop_trim_removes_only_whitespace :: String -> Bool
prop_trim_removes_only_whitespace s =
    let trimmed = trim s
        originalNonSpaces = L.filter (not . isSpace) s
        trimmedNonSpaces = L.filter (not . isSpace) trimmed
    in originalNonSpaces == trimmedNonSpaces

-- | Test splitBy function properties
prop_splitby_empty_string :: Char -> Bool
prop_splitby_empty_string c = splitBy c "" == [""]

prop_splitby_concatenates_to_original :: Char -> String -> Property
prop_splitby_concatenates_to_original c s = 
    let parts = splitBy c s
        reconstructed = L.concat (L.map (++ [c]) (init parts) ++ [last parts])
    in L.length parts > 0 ==> reconstructed == s

prop_splitby_preserves_empty_segments :: Char -> String -> Bool
prop_splitby_preserves_empty_segments c s =
    let parts = splitBy c s
        doubleC = [c, c]
        hasDoubleC = doubleC `L.isInfixOf` s
    in if hasDoubleC
       then L.any null parts
       else True

-- | Test splitByCollapsed vs splitBy relationship
prop_splitby_collapsed_removes_empties :: Char -> String -> Bool
prop_splitby_collapsed_removes_empties c s =
    let normal = splitBy c s
        collapsed = splitByCollapsed c s
    in L.all (not . null) collapsed

prop_splitby_collapsed_subset_of_normal :: Char -> String -> Bool
prop_splitby_collapsed_subset_of_normal c s =
    let normal = splitBy c s
        collapsed = splitByCollapsed c s
        nonEmptyNormal = L.filter (not . null) normal
    in collapsed == nonEmptyNormal

-- | Test removeComments function properties
prop_remove_comments_preserves_non_comment_code :: String -> Bool
prop_remove_comments_preserves_non_comment_code s =
    let withoutComments = removeComments s
        -- Count non-comment, non-whitespace characters before L.and after
        originalCodeChars = L.length $ L.filter (not . (`elem` " \t\n\r/")) s
        codeCharsAfter = L.length $ L.filter (not . (`elem` " \t\n\r/")) withoutComments
    in codeCharsAfter <= originalCodeChars

prop_remove_comments_removes_line_comments :: String -> Property
prop_remove_comments_removes_line_comments s =
    let withComment = s ++ "// this is a comment\nmore code"
        withoutComments = removeComments withComment
    in "// this is a comment" `L.isInfixOf` withComment ==> 
       not ("// this is a comment" `L.isInfixOf` withoutComments)

prop_remove_comments_preserves_string_literals :: String -> Property
prop_remove_comments_preserves_string_literals s =
    let stringWithLiteral = s ++ "\"code with // not a comment\""
        withoutComments = removeComments stringWithLiteral
    in "\"code with // not a comment\"" `L.isInfixOf` withoutComments

-- | Test normalizeIndentation function properties
prop_normalize_indentation_preserves_relative_structure :: String -> Bool
prop_normalize_indentation_preserves_relative_structure s =
    let normalized = normalizeIndentation s
        originalLines = lines s
        normalizedLines = lines normalized
        
        -- Check that non-empty lines preserve their relative indentation differences
        indentDifferences orig = 
            case L.filter (not . L.all isSpace) orig of
                [] -> []
                lines' -> zipWith (-) (L.map (L.length . takeWhile isSpace) (L.tail lines')) 
                                    (L.map (L.length . takeWhile isSpace) lines')
        
        origDiffs = indentDifferences originalLines
        normDiffs = indentDifferences normalizedLines
    in L.length origDiffs <= 1 || L.length normDiffs <= 1 || 
       take (min (L.length origDiffs) 5) origDiffs == take (min (L.length normDiffs) 5) normDiffs

prop_normalize_indentation_no_leading_empty_lines :: String -> Bool
prop_normalize_indentation_no_leading_empty_lines s =
    let normalized = normalizeIndentation s
        lines' = lines normalized
        firstNonEmpty = dropWhile L.all isSpace lines'
    in null firstNonEmpty || 
       (L.head firstNonEmpty /= "" && not (isSpace (L.head (L.head firstNonEmpty))))

-- | Test breakOn function properties
prop_breakon_finds_pattern :: String -> String -> Property
prop_breakon_finds_pattern s pat =
    not (null pat) && pat `L.isInfixOf` s ==>
    let (before, after) = breakOn pat s
    in pat `L.isInfixOf` s && (before ++ pat ++ after) == s

prop_breakon_empty_pattern :: String -> Bool
prop_breakon_empty_pattern s = breakOn "" s == ("", s)

prop_breakon_pattern_not_found :: String -> String -> Property
prop_breakon_pattern_not_found s pat =
    not (null pat) && not (pat `L.isInfixOf` s) ==>
    let (before, after) = breakOn pat s
    in before == s && after == ""

-- | Test cross-function properties
prop_trim_then_normalize_consistency :: String -> Bool
prop_trim_then_normalize_consistency s =
    let trimThenNormalize = normalizeIndentation (trim s)
        normalizeThenTrim = trim (normalizeIndentation s)
    in lines trimThenNormalize == lines normalizeThenTrim

prop_split_roundtrip_with_join :: Char -> String -> Property
prop_split_roundtrip_with_join c s = 
    let parts = splitBy c s
    in L.length parts > 0 ==> L.concat (intersperse [c] parts) == s
  where
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:y:xs) = x ++ sep ++ intersperse sep (y:xs)

-- | Test edge cases
prop_trim_unicode_handling :: String -> Bool
prop_trim_unicode_handling s =
    let withUnicode = s ++ "  中文测试  "
        trimmed = trim withUnicode
    in not (null trimmed) ==> 
       last trimmed `notElem` " \t\n\r" && L.head trimmed `notElem` " \t\n\r"

prop_splitby_special_characters :: String -> Bool
prop_splitby_special_characters s =
    let specialChars = ",.;\n\t:"
        testChar c = L.length (splitBy c s) >= 1
    in L.all testChar specialChars

tests :: TestTree
tests = testGroup "Utils Properties QuickCheck Tests"
  [ testProperty "trim is idempotent" prop_trim_idempotent
  , testProperty "trim removes leading/trailing spaces" prop_trim_no_leading_trailing_spaces
  , testProperty "trim only removes whitespace" prop_trim_removes_only_whitespace
  , testProperty "splitBy on empty string" prop_splitby_empty_string
  , testProperty "splitBy concatenates to original" prop_splitby_concatenates_to_original
  , testProperty "splitBy preserves empty segments" prop_splitby_preserves_empty_segments
  , testProperty "splitByCollapsed removes empties" prop_splitby_collapsed_removes_empties
  , testProperty "splitByCollapsed subset of normal" prop_splitby_collapsed_subset_of_normal
  , testProperty "removeComments preserves non-comment code" prop_remove_comments_preserves_non_comment_code
  , testProperty "removeComments removes line comments" prop_remove_comments_removes_line_comments
  , testProperty "removeComments preserves string literals" prop_remove_comments_preserves_string_literals
  , testProperty "normalizeIndentation preserves relative structure" prop_normalize_indentation_preserves_relative_structure
  , testProperty "normalizeIndentation no leading empty lines" prop_normalize_indentation_no_leading_empty_lines
  , testProperty "breakOn finds pattern" prop_breakon_finds_pattern
  , testProperty "breakOn empty pattern" prop_breakon_empty_pattern
  , testProperty "breakOn pattern not found" prop_breakon_pattern_not_found
  , testProperty "trim then normalize consistency" prop_trim_then_normalize_consistency
  , testProperty "split roundtrip with join" prop_split_roundtrip_with_join
  , testProperty "trim unicode handling" prop_trim_unicode_handling
  , testProperty "splitBy special characters" prop_splitby_special_characters
  ]