module Test.Unit.NewQuickCheckPropertySpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.List (sort, nub, reverse, length, null)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, toUpper, toLower)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- ============================================================================
-- QuickCheck Properties for Basic Data Types and Functions
-- ============================================================================

-- Property 1: List length is non-negative
prop_list_length_non_negative :: [Int] -> Property
prop_list_length_non_negative xs = property $ length xs >= 0

-- Property 2: Reverse of reverse is original
prop_reverse_reverse :: [Int] -> Property
prop_reverse_reverse xs = property $ reverse (reverse xs) == xs

-- Property 3: Sort preserves length
prop_sort_preserves_length :: [Int] -> Property
prop_sort_preserves_length xs = property $ length (sort xs) == length xs

-- Property 4: Nub removes duplicates (length doesn't increase)
prop_nub_not_longer :: [Int] -> Property
prop_nub_not_longer xs = property $ length (nub xs) <= length xs

-- Property 5: Null check is consistent with length
prop_null_consistent_with_length :: [Int] -> Property
prop_null_consistent_with_length xs = property $ null xs == (length xs == 0)

-- Property 6: String length is non-negative
prop_string_length_non_negative :: String -> Property
prop_string_length_non_negative s = property $ length s >= 0

-- Property 7: ToUpper and toLower preserve length
prop_case_preserve_length :: String -> Property
prop_case_preserve_length s = property $ 
  length (map toUpper s) == length s && 
  length (map toLower s) == length s

-- Property 8: Alpha check is boolean
prop_is_alpha_boolean :: Char -> Property
prop_is_alpha_boolean c = property $ isAlpha c == True || isAlpha c == False

-- Property 9: AlphaNum check is boolean
prop_is_alnum_boolean :: Char -> Property
prop_is_alnum_boolean c = property $ isAlphaNum c == True || isAlphaNum c == False

-- Property 10: Digit check is boolean
prop_is_digit_boolean :: Char -> Property
prop_is_digit_boolean c = property $ isDigit c == True || isDigit c == False

-- Property 11: Space check is boolean
prop_is_space_boolean :: Char -> Property
prop_is_space_boolean c = property $ isSpace c == True || isSpace c == False

-- Property 12: Maybe isJust or isNothing
prop_maybe_just_or_nothing :: Maybe Int -> Property
prop_maybe_just_or_nothing m = property $ isJust m == True || isNothing m == True

-- Property 13: FromMaybe with default returns default for Nothing
prop_from_maybe_nothing :: Int -> Property
prop_from_maybe_nothing def = property $ fromMaybe def Nothing == def

-- Property 14: FromMaybe with Just returns Just value
prop_from_maybe_just :: Int -> Int -> Property
prop_from_maybe_just def val = property $ fromMaybe def (Just val) == val

-- Property 15: List concatenation is associative
prop_concat_associative :: [Int] -> [Int] -> [Int] -> Property
prop_concat_associative xs ys zs = property $ (xs ++ ys) ++ zs == xs ++ (ys ++ zs)

-- Property 16: List concatenation with empty list
prop_concat_empty :: [Int] -> Property
prop_concat_empty xs = property $ xs ++ [] == xs && [] ++ xs == xs

-- Property 17: String concatenation length
prop_string_concat_length :: String -> String -> Property
prop_string_concat_length s1 s2 = property $ 
  length (s1 ++ s2) == length s1 + length s2

-- Property 18: Reverse of empty list is empty
prop_reverse_empty :: Property
prop_reverse_empty = property $ reverse ([] :: [Int]) == []

-- Property 19: Sort of empty list is empty
prop_sort_empty :: Property
prop_sort_empty = property $ sort ([] :: [Int]) == []

-- Property 20: Nub of empty list is empty
prop_nub_empty :: Property
prop_nub_empty = property $ nub ([] :: [Int]) == []

-- Property 21: Head of non-empty list exists
prop_head_non_empty :: [Int] -> Property
prop_head_non_empty xs = not (null xs) ==> property $ head xs `elem` xs

-- Property 22: Last of non-empty list exists
prop_last_non_empty :: [Int] -> Property
prop_last_non_empty xs = not (null xs) ==> property $ last xs `elem` xs

-- Property 23: Take length doesn't exceed original
prop_take_length :: [Int] -> Int -> Property
prop_take_length xs n = 
  let n' = abs n
      taken = take n' xs
  in property $ length taken <= length xs

-- Property 24: Drop length doesn't exceed original
prop_drop_length :: [Int] -> Int -> Property
prop_drop_length xs n = 
  let n' = abs n
      dropped = drop n' xs
  in property $ length dropped <= length xs

-- Property 25: Take and drop are complementary
prop_take_drop_complementary :: [Int] -> Int -> Property
prop_take_drop_complementary xs n = 
  let n' = abs n `mod` (length xs + 1)
      taken = take n' xs
      dropped = drop n' xs
  in property $ taken ++ dropped == xs

-- Property 26: Map preserves length
prop_map_preserves_length :: [Int] -> Property
prop_map_preserves_length xs = property $ length (map (*2) xs) == length xs

-- Property 27: Filter doesn't increase length
prop_filter_not_longer :: [Int] -> Property
prop_filter_not_longer xs = property $ length (filter even xs) <= length xs

-- Property 28: All elements of filtered list satisfy predicate
prop_filter_satisfies_predicate :: [Int] -> Property
prop_filter_satisfies_predicate xs = 
  let evens = filter even xs
  in property $ all even evens

-- Property 29: Any element that satisfies predicate is in filtered list
prop_filter_contains_matching :: [Int] -> Property
prop_filter_contains_matching xs = 
  let evens = filter even xs
      hasEven = any even xs
  in property $ if hasEven then not (null evens) else True

-- Property 30: Fold with + equals sum
prop_fold_sum :: [Int] -> Property
prop_fold_sum xs = property $ foldl (+) 0 xs == sum xs

-- ============================================================================
-- Unit Tests for Basic Verification
-- ============================================================================

test_list_length :: Assertion
test_list_length = 
  let xs = [1,2,3,4,5]
  in assertEqual "List length should be positive" 5 (length xs)

test_empty_list_length :: Assertion
test_empty_list_length = 
  let xs = [] :: [Int]
  in assertEqual "Empty list length should be 0" 0 (length xs)

test_reverse_property :: Assertion
test_reverse_property = 
  let xs = [1,2,3]
  in assertEqual "Reverse of reverse should be original" xs (reverse (reverse xs))

test_sort_property :: Assertion
test_sort_property = 
  let xs = [3,1,4,1,5]
      sorted = sort xs
  in assertEqual "Sort should preserve length" (length xs) (length sorted)

test_nub_property :: Assertion
test_nub_property = 
  let xs = [1,2,2,3,3,3]
      unique = nub xs
  in assertBool "Nub should not increase length" $ length unique <= length xs

test_null_consistency :: Assertion
test_null_consistency = 
  do
    let xs = [] :: [Int]
    assertEqual "Empty list should be null" True (null xs)
    assertEqual "Empty list should have length 0" 0 (length xs)
    
    let ys = [1]
    assertEqual "Non-empty list should not be null" False (null ys)
    assertBool "Non-empty list should have non-zero length" $ length ys > 0

test_string_operations :: Assertion
test_string_operations = 
  let s = "Hello World"
      upper = map toUpper s
      lower = map toLower s
  in do
    assertEqual "ToUpper should preserve length" (length s) (length upper)
    assertEqual "ToLower should preserve length" (length s) (length lower)

test_character_checks :: Assertion
test_character_checks = 
  do
    assertBool "Alpha check should be boolean" $ isAlpha 'a' == True || isAlpha 'a' == False
    assertBool "Digit check should be boolean" $ isDigit '5' == True || isDigit '5' == False
    assertBool "Space check should be boolean" $ isSpace ' ' == True || isSpace ' ' == False

test_maybe_operations :: Assertion
test_maybe_operations = 
  do
    let m = Just 5
    assertBool "Maybe should be Just or Nothing" $ isJust m || isNothing m
    assertEqual "FromMaybe with Just should return value" 5 (fromMaybe 0 m)
    
    let n = Nothing :: Maybe Int
    assertEqual "FromMaybe with Nothing should return default" 0 (fromMaybe 0 n)

test_concatenation :: Assertion
test_concatenation = 
  let xs = [1,2]
      ys = [3,4]
      zs = [5,6]
  in do
    assertEqual "Concatenation with empty" xs (xs ++ [])
    assertEqual "Empty concatenation" xs ([] ++ xs)
    assertEqual "Associative property" ((xs ++ ys) ++ zs) (xs ++ (ys ++ zs))

test_take_drop :: Assertion
test_take_drop = 
  let xs = [1,2,3,4,5]
      n = 2
      taken = take n xs
      dropped = drop n xs
  in do
    assertBool "Take should not exceed original length" $ length taken <= length xs
    assertBool "Drop should not exceed original length" $ length dropped <= length xs
    assertEqual "Take and drop should be complementary" xs (taken ++ dropped)

test_map_filter :: Assertion
test_map_filter = 
  let xs = [1,2,3,4,5]
      doubled = map (*2) xs
      evens = filter even xs
  in do
    assertEqual "Map should preserve length" (length xs) (length doubled)
    assertBool "Filter should not increase length" $ length evens <= length xs
    assertBool "All filtered elements should satisfy predicate" $ all even evens

tests :: TestTree
tests = testGroup "Test.Unit.NewQuickCheckPropertySpec Tests"
  [ testGroup "QuickCheck Properties"
    [ testProperty "list length non-negative" prop_list_length_non_negative
    , testProperty "reverse reverse" prop_reverse_reverse
    , testProperty "sort preserves length" prop_sort_preserves_length
    , testProperty "nub not longer" prop_nub_not_longer
    , testProperty "null consistent with length" prop_null_consistent_with_length
    , testProperty "string length non-negative" prop_string_length_non_negative
    , testProperty "case preserve length" prop_case_preserve_length
    , testProperty "is alpha boolean" prop_is_alpha_boolean
    , testProperty "is alnum boolean" prop_is_alnum_boolean
    , testProperty "is digit boolean" prop_is_digit_boolean
    , testProperty "is space boolean" prop_is_space_boolean
    , testProperty "maybe just or nothing" prop_maybe_just_or_nothing
    , testProperty "from maybe nothing" prop_from_maybe_nothing
    , testProperty "from maybe just" prop_from_maybe_just
    , testProperty "concat associative" prop_concat_associative
    , testProperty "concat empty" prop_concat_empty
    , testProperty "string concat length" prop_string_concat_length
    , testProperty "reverse empty" prop_reverse_empty
    , testProperty "sort empty" prop_sort_empty
    , testProperty "nub empty" prop_nub_empty
    , testProperty "head non empty" prop_head_non_empty
    , testProperty "last non empty" prop_last_non_empty
    , testProperty "take length" prop_take_length
    , testProperty "drop length" prop_drop_length
    , testProperty "take drop complementary" prop_take_drop_complementary
    , testProperty "map preserves length" prop_map_preserves_length
    , testProperty "filter not longer" prop_filter_not_longer
    , testProperty "filter satisfies predicate" prop_filter_satisfies_predicate
    , testProperty "filter contains matching" prop_filter_contains_matching
    , testProperty "fold sum" prop_fold_sum
    ]
  , testGroup "Unit Tests"
    [ testCase "list length" test_list_length
    , testCase "empty list length" test_empty_list_length
    , testCase "reverse property" test_reverse_property
    , testCase "sort property" test_sort_property
    , testCase "nub property" test_nub_property
    , testCase "null consistency" test_null_consistency
    , testCase "string operations" test_string_operations
    , testCase "character checks" test_character_checks
    , testCase "maybe operations" test_maybe_operations
    , testCase "concatenation" test_concatenation
    , testCase "take drop" test_take_drop
    , testCase "map filter" test_map_filter
    ]
  ]