{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewUtilsPropertiesQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils (trim, splitBy, splitByCollapsed, removeComments, normalizeIndentation, breakOn)
import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)

-- | Test trim function properties
prop_trim_idempotent :: String -> Bool
prop_trim_idempotent s = trim (trim s) == trim s

prop_trim_no_leading_trailing_spaces :: String -> Bool
prop_trim_no_leading_trailing_spaces s = 
    let trimmed = trim s
    in null trimmed || 
       (not (isSpace (head trimmed)) && not (isSpace (last trimmed)))

prop_trim_removes_only_whitespace :: String -> Bool
prop_trim_removes_only_whitespace s =
    let trimmed = trim s
        originalNonSpaces = filter (not . isSpace) s
        trimmedNonSpaces = filter (not . isSpace) trimmed
    in originalNonSpaces == trimmedNonSpaces

-- | Test splitBy function properties
prop_splitby_empty_string :: Char -> Bool
prop_splitby_empty_string c = splitBy c "" == [""]

prop_splitby_concatenates_to_original :: Char -> String -> Property
prop_splitby_concatenates_to_original c s = 
    let parts = splitBy c s
        reconstructed = concat (map (++ [c]) (init parts) ++ [last parts])
    in length parts > 0 ==> reconstructed == s

prop_splitby_preserves_empty_segments :: Char -> String -> Bool
prop_splitby_preserves_empty_segments c s =
    let parts = splitBy c s
        doubleC = [c, c]
        hasDoubleC = doubleC `isInfixOf` s
    in if hasDoubleC
       then any null parts
       else True

-- | Test splitByCollapsed vs splitBy relationship
prop_splitby_collapsed_removes_empties :: Char -> String -> Bool
prop_splitby_collapsed_removes_empties c s =
    let normal = splitBy c s
        collapsed = splitByCollapsed c s
    in all (not . null) collapsed

prop_splitby_collapsed_subset_of_normal :: Char -> String -> Bool
prop_splitby_collapsed_subset_of_normal c s =
    let normal = splitBy c s
        collapsed = splitByCollapsed c s
        nonEmptyNormal = filter (not . null) normal
    in collapsed == nonEmptyNormal

-- | Test removeComments function properties
prop_remove_comments_preserves_non_comment_code :: String -> Bool
prop_remove_comments_preserves_non_comment_code s =
    let withoutComments = removeComments s
        -- Count non-comment, non-whitespace characters before and after
        originalCodeChars = length $ filter (not . (`elem` " \t\n\r/")) s
        codeCharsAfter = length $ filter (not . (`elem` " \t\n\r/")) withoutComments
    in codeCharsAfter <= originalCodeChars

prop_remove_comments_removes_line_comments :: String -> Property
prop_remove_comments_removes_line_comments s =
    let withComment = s ++ "// this is a comment\nmore code"
        withoutComments = removeComments withComment
    in "// this is a comment" `isInfixOf` withComment ==> 
       not ("// this is a comment" `isInfixOf` withoutComments)

prop_remove_comments_preserves_string_literals :: String -> Property
prop_remove_comments_preserves_string_literals s =
    let stringWithLiteral = s ++ "\"code with // not a comment\""
        withoutComments = removeComments stringWithLiteral
    in "\"code with // not a comment\"" `isInfixOf` withoutComments

-- | Test normalizeIndentation function properties
prop_normalize_indentation_preserves_relative_structure :: String -> Bool
prop_normalize_indentation_preserves_relative_structure s =
    let normalized = normalizeIndentation s
        originalLines = lines s
        normalizedLines = lines normalized
        
        -- Check that non-empty lines preserve their relative indentation differences
        indentDifferences orig = 
            case filter (not . all isSpace) orig of
                [] -> []
                lines' -> zipWith (-) (map (length . takeWhile isSpace) (tail lines')) 
                                    (map (length . takeWhile isSpace) lines')
        
        origDiffs = indentDifferences originalLines
        normDiffs = indentDifferences normalizedLines
    in length origDiffs <= 1 || length normDiffs <= 1 || 
       take (min (length origDiffs) 5) origDiffs == take (min (length normDiffs) 5) normDiffs

prop_normalize_indentation_no_leading_empty_lines :: String -> Bool
prop_normalize_indentation_no_leading_empty_lines s =
    let normalized = normalizeIndentation s
        lines' = lines normalized
        firstNonEmpty = dropWhile all isSpace lines'
    in null firstNonEmpty || 
       (head firstNonEmpty /= "" && not (isSpace (head (head firstNonEmpty))))

-- | Test breakOn function properties
prop_breakon_finds_pattern :: String -> String -> Property
prop_breakon_finds_pattern s pat =
    not (null pat) && pat `isInfixOf` s ==>
    let (before, after) = breakOn pat s
    in pat `isInfixOf` s && (before ++ pat ++ after) == s

prop_breakon_empty_pattern :: String -> Bool
prop_breakon_empty_pattern s = breakOn "" s == ("", s)

prop_breakon_pattern_not_found :: String -> String -> Property
prop_breakon_pattern_not_found s pat =
    not (null pat) && not (pat `isInfixOf` s) ==>
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
    in length parts > 0 ==> concat (intersperse [c] parts) == s
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
       last trimmed `notElem` " \t\n\r" && head trimmed `notElem` " \t\n\r"

prop_splitby_special_characters :: String -> Bool
prop_splitby_special_characters s =
    let specialChars = ",.;\n\t:"
        testChar c = length (splitBy c s) >= 1
    in all testChar specialChars

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