module Test.Unit.EnhancedUtilsSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed, 
             removeLineComments, removeComments, normalizeIndentation, 
             breakOn, safeProcessString, isValidChar)

-- | Test trim function
prop_trim_roundtrip :: String -> Property
prop_trim_roundtrip s = trim (trim s) === trim s

prop_trim_no_leading_trailing_spaces :: String -> Property
prop_trim_no_leading_trailing_spaces s = 
  let trimmed = trim s
  in property $ 
    if null trimmed 
    then True 
    else not (isSpace (head trimmed)) && not (isSpace (last trimmed))

-- | Test splitBy function
prop_split_by_empty :: Char -> Property
prop_split_by_empty c = splitBy c "" === [""]

prop_split_by_single_char :: Char -> Char -> Property
prop_split_by_single_char delim c = 
  delim /= c ==> splitBy delim [c] === [[c]]

prop_split_by_preserves_empty :: Char -> String -> Property
prop_split_by_preserves_empty delim s = 
  let result = splitBy delim s
      expectedCount = length (filter (== delim) s) + 1
  in property $ length result === expectedCount

-- | Test splitByCollapsed function
prop_split_by_collapsed_no_empty :: Char -> String -> Property
prop_split_by_collapsed_no_empty delim s = 
  property $ all (not . null) (splitByCollapsed delim s)

prop_split_by_collapsed_consistency :: Char -> String -> Property
prop_split_by_collapsed_consistency delim s = 
  splitByCollapsed delim s === filter (not . null) (splitBy delim s)

-- | Test splitByComma and splitByCommaCollapsed
prop_split_by_comma_consistency :: String -> Property
prop_split_by_comma_consistency s = 
  splitByComma s === splitBy ',' s

prop_split_by_comma_collapsed_consistency :: String -> Property
prop_split_by_comma_collapsed_consistency s = 
  splitByCommaCollapsed s === splitByCollapsed ',' s

-- | Test removeLineComments function
prop_remove_line_comments_no_comments :: String -> Property
prop_remove_line_comments_no_comments s = 
  not ("//" `isInfixOf` s) ==> removeLineComments s === s

prop_remove_line_comments_preserves_non_comment :: String -> String -> Property
prop_remove_line_comments_preserves_non_comment s1 s2 = 
  let line = s1 ++ " // " ++ s2
      result = removeLineComments line
  in property $ take (length s1) result === s1

-- | Test removeComments function
prop_remove_comments_no_comments :: String -> Property
prop_remove_comments_no_comments s = 
  not ("//" `isInfixOf` s) && not ("/*" `isInfixOf` s) ==> 
  removeComments s === s

prop_remove_comments_line_and_block :: String -> String -> Property
prop_remove_comments_line_and_block s1 s2 = 
  let input = s1 ++ " // line comment\n" ++ s2 ++ " /* block comment */"
      result = removeComments input
  in property $ "//" `notElem` result && "/*" `notElem` result

-- | Test breakOn function
prop_break_on_found :: String -> String -> Property
prop_break_on_found needle haystack = 
  needle `isInfixOf` haystack ==> 
  let (before, after) = breakOn needle haystack
  in property $ needle `isInfixOf` after

prop_break_on_not_found :: String -> String -> Property
prop_break_on_not_found needle haystack = 
  not (needle `isInfixOf` haystack) ==> 
  breakOn needle haystack === (haystack, "")

-- | Test safeProcessString function
prop_safe_process_string_roundtrip :: String -> Property
prop_safe_process_string_roundtrip s = 
  let processed = safeProcessString s
  in property $ length processed <= length s

-- | Test isValidChar function
prop_is_valid_char_ascii :: Char -> Property
prop_is_valid_char_ascii c = 
  property $ isValidChar c === (c >= ' ' && c <= '~')

-- | Test string processing properties
prop_string_processing_associative :: String -> String -> String -> Property
prop_string_processing_associative s1 s2 s3 = 
  trim (s1 ++ " " ++ s2 ++ " " ++ s3) === 
  trim (trim s1 ++ " " ++ trim s2 ++ " " ++ trim s3)

-- | Test comment removal properties
prop_comment_removal_idempotent :: String -> Property
prop_comment_removal_idempotent s = 
  let once = removeComments s
      twice = removeComments once
  in property $ once === twice

prop_line_comment_removal_idempotent :: String -> Property
prop_line_comment_removal_idempotent s = 
  let once = removeLineComments s
      twice = removeLineComments once
  in property $ once === twice

-- | Test split properties
prop_split_join_roundtrip :: Char -> String -> Property
prop_split_join_roundtrip delim s = 
  let parts = splitBy delim s
      rejoined = concatMap (\p -> p ++ [delim]) (init parts) ++ last parts
  in property $ length s > 0 ==> rejoined === s

tests :: TestTree
tests = testGroup "Enhanced Utils Tests"
  [ testGroup "trim tests"
    [ testProperty "trim roundtrip" prop_trim_roundtrip
    , testProperty "trim removes leading/trailing spaces" prop_trim_no_leading_trailing_spaces
    ]
  , testGroup "splitBy tests"
    [ testProperty "splitBy empty string" prop_split_by_empty
    , testProperty "splitBy single character" prop_split_by_single_char
    , testProperty "splitBy preserves empty segments" prop_split_by_preserves_empty
    ]
  , testGroup "splitByCollapsed tests"
    [ testProperty "splitByCollapsed removes empty segments" prop_split_by_collapsed_no_empty
    , testProperty "splitByCollapsed consistency" prop_split_by_collapsed_consistency
    ]
  , testGroup "Comma split tests"
    [ testProperty "splitByComma consistency" prop_split_by_comma_consistency
    , testProperty "splitByCommaCollapsed consistency" prop_split_by_comma_collapsed_consistency
    ]
  , testGroup "Comment removal tests"
    [ testProperty "removeLineComments no comments" prop_remove_line_comments_no_comments
    , testProperty "removeLineComments preserves non-comment" prop_remove_line_comments_preserves_non_comment
    , testProperty "removeComments no comments" prop_remove_comments_no_comments
    , testProperty "removeComments line and block" prop_remove_comments_line_and_block
    , testProperty "comment removal idempotent" prop_comment_removal_idempotent
    , testProperty "line comment removal idempotent" prop_line_comment_removal_idempotent
    ]
  , testGroup "breakOn tests"
    [ testProperty "breakOn found" prop_break_on_found
    , testProperty "breakOn not found" prop_break_on_not_found
    ]
  , testGroup "safeProcessString tests"
    [ testProperty "safeProcessString roundtrip" prop_safe_process_string_roundtrip
    ]
  , testGroup "isValidChar tests"
    [ testProperty "isValidChar ASCII" prop_is_valid_char_ascii
    ]
  , testGroup "String processing properties"
    [ testProperty "string processing associative" prop_string_processing_associative
    ]
  , testGroup "Split properties"
    [ testProperty "split join roundtrip" prop_split_join_roundtrip
    ]
  ]