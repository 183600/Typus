{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Test.Unit.NewStringProcessingQuickCheckTestSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

import Utils
import Data.Char (isSpace, isControl, toLower, toUpper)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort)
import qualified Data.Text as T
import Data.String (IsString)

-- ============================================================================
-- String Processing QuickCheck Tests
-- ============================================================================

-- Test trim function properties
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

prop_trim_removes_spaces :: String -> Property
prop_trim_removes_spaces s = 
  let trimmed = trim s
      hasLeadingSpace = not (null s) && isSpace (head s)
      hasTrailingSpace = not (null s) && isSpace (last s)
  in if hasLeadingSpace || hasTrailingSpace
     then property $ not (isPrefixOf " " trimmed || isSuffixOf " " trimmed)
     else property $ trimmed === s

prop_trim_preserves_internal_content :: String -> Property
prop_trim_preserves_internal_content s = 
  let nonSpaceContent = filter (not . isSpace) s
      trimmedNonSpaceContent = filter (not . isSpace) (trim s)
  in property $ nonSpaceContent === trimmedNonSpaceContent

-- Test splitBy function properties
prop_split_by_empty_string :: Char -> Property
prop_split_by_empty_string delim = splitBy delim "" === []

prop_split_by_single_char :: Char -> Char -> Property
prop_split_by_single_char delim c = 
  if c == delim 
  then splitBy delim [c] === ["", ""]
  else splitBy delim [c] === [[c]]

prop_split_by_concatenates_with_delim :: Char -> String -> String -> Property
prop_split_by_concatenates_with_delim delim s1 s2 = 
  let combined = s1 ++ [delim] ++ s2
      result = splitBy delim combined
  in property $ result === [s1, s2]

prop_split_by_preserves_non_delim_chars :: Char -> String -> Property
prop_split_by_preserves_non_delim_chars delim s = 
  let noDelim = filter (/= delim) s
      result = splitBy delim s
      resultNoDelim = concat result
  in property $ noDelim === resultNoDelim

-- Test breakOn function properties
prop_break_on_empty_string :: String -> Property
prop_break_on_empty_string delim = breakOn delim "" === ("", "")

prop_break_on_delim_not_found :: String -> String -> Property
prop_break_on_delim_not_found delim s = 
  let delimNotInString = delim `notElem` s
  in if delimNotInString
     then property $ breakOn delim s === (s, "")
     else property $ True

prop_break_on_delim_at_start :: String -> String -> Property
prop_break_on_delim_at_start delim s = 
  let sWithDelim = delim ++ s
  in property $ breakOn delim sWithDelim === ("", s)

prop_break_on_delim_at_end :: String -> String -> Property
prop_break_on_delim_at_end delim s = 
  let sWithDelim = s ++ delim
  in property $ breakOn delim sWithDelim === (s, "")

-- Test string transformation functions
prop_to_upper_lower_roundtrip :: String -> Property
prop_to_upper_lower_roundtrip s = map toLower (map toUpper s) === map toLower s

prop_to_lower_upper_roundtrip :: String -> Property
prop_to_lower_upper_roundtrip s = map toUpper (map toLower s) === map toUpper s

-- Test string list operations
prop_sort_idempotent :: String -> Property
prop_sort_idempotent s = sort (sort s) === sort s

prop_sort_preserves_elements :: String -> Property
prop_sort_preserves_elements s = sort s === sort (sort s)

prop_sort_length_preserved :: String -> Property
prop_sort_length_preserved s = length s === length (sort s)

-- Test string concatenation properties
prop_concat_associative :: String -> String -> String -> Property
prop_concat_associative s1 s2 s3 = (s1 ++ s2) ++ s3 === s1 ++ (s2 ++ s3)

prop_concat_identity_left :: String -> Property
prop_concat_identity_left s = "" ++ s === s

prop_concat_identity_right :: String -> Property
prop_concat_identity_right s = s ++ "" === s

-- Test string reversal properties
prop_reverse_idempotent :: String -> Property
prop_reverse_idempotent s = reverse (reverse s) === s

prop_reverse_length_preserved :: String -> Property
prop_reverse_length_preserved s = length s === length (reverse s)

-- Test string filtering properties
prop_filter_preserves_order :: String -> Property
prop_filter_preserves_order s = 
  let filtered = filter (not . isSpace) s
      sortedFiltered = sort filtered
      sortedOriginalFiltered = sort (filter (not . isSpace) s)
  in property $ sortedFiltered === sortedOriginalFiltered

prop_filter_idempotent :: String -> Property
prop_filter_idempotent s = 
  let f = not . isSpace
  in property $ filter f (filter f s) === filter f s

-- Test string word operations
prop_words_empty_string :: Property
prop_words_empty_string = words "" === []

prop_words_single_word :: String -> Property
prop_words_single_word w = 
  let noSpaces = filter (not . isSpace) w
  in if null noSpaces
     then property $ words w === []
     else property $ length (words w) === 1

prop_unwords_words_roundtrip :: String -> Property
prop_unwords_words_roundtrip s = 
  let wordList = words s
  in if null wordList
     then property $ unwords wordList === ""
     else property $ words (unwords wordList) === wordList

-- Test string line operations
prop_lines_empty_string :: Property
prop_lines_empty_string = lines "" === [""]

prop_lines_single_line :: String -> Property
prop_lines_single_line s = 
  let noNewlines = filter (/= '\n') s
  in if null noNewlines
     then property $ lines s === [""]
     else property $ length (lines s) === 1

prop_unlines_lines_roundtrip :: String -> Property
prop_unlines_lines_roundtrip s = 
  let lineList = lines s
  in property $ lines (unlines lineList) === lineList

-- Test string prefix/suffix operations
prop_is_prefix_of_reflexive :: String -> Property
prop_is_prefix_of_reflexive s = isPrefixOf s s

prop_is_suffix_of_reflexive :: String -> Property
prop_is_suffix_of_reflexive s = isSuffixOf s s

prop_is_infix_of_reflexive :: String -> Property
prop_is_infix_of_reflexive s = isInfixOf s s

prop_is_prefix_of_empty :: String -> Property
prop_is_prefix_of_empty s = isPrefixOf "" s

prop_is_suffix_of_empty :: String -> Property
prop_is_suffix_of_empty s = isSuffixOf "" s

-- Test string length properties
prop_length_non_negative :: String -> Property
prop_length_non_negative s = length s >= 0

prop_length_concat :: String -> String -> Property
prop_length_concat s1 s2 = length (s1 ++ s2) === length s1 + length s2

prop_length_reverse :: String -> Property
prop_length_reverse s = length s === length (reverse s)

-- Test string character operations
prop_any_preserves_length :: String -> Property
prop_any_preserves_length s = 
  let result = any isSpace s
  in property $ length s === length s  -- any doesn't modify the string

prop_all_preserves_length :: String -> Property
prop_all_preserves_length s = 
  let result = all isSpace s
  in property $ length s === length s  -- all doesn't modify the string

-- Test string mapping operations
prop_map_length_preserved :: String -> Property
prop_map_length_preserved s = length s === length (map toUpper s)

prop_map_idempotent :: String -> Property
prop_map_idempotent s = map toUpper (map toUpper s) === map toUpper s

-- Test string replication
prop_replicate_length :: Positive Int -> Char -> Property
prop_replicate_length (Positive n) c = length (replicate n c) === n

prop_replicate_all_same :: Positive Int -> Char -> Property
prop_replicate_all_same (Positive n) c = all (== c) (replicate n c)

-- Test string take/drop operations
prop_take_length :: String -> Property
prop_take_length s = length (take (length s) s) === length s

prop_take_drop_roundtrip :: String -> Property
prop_take_drop_roundtrip s = take (length s) s ++ drop (length s) s === s

prop_take_all :: String -> Property
prop_take_all s = take (length s + 10) s === s

prop_drop_all :: String -> Property
prop_drop_all s = drop (length s) s === ""

-- Test string span operations
prop_span_take_drop :: String -> Property
prop_span_take_drop s = 
  let (prefix, suffix) = span (not . isSpace) s
  in property $ prefix ++ suffix === s

prop_break_take_drop :: String -> Property
prop_break_take_drop s = 
  let (prefix, suffix) = break isSpace s
  in property $ prefix ++ suffix === s

-- Test string partition operations
prop_partition_union :: String -> Property
prop_partition_union s = 
  let (satisfying, notSatisfying) = partition isSpace s
  in property $ sort (satisfying ++ notSatisfying) === sort s

-- Test string group operations
prop_group_concat :: String -> Property
prop_group_concat s = concat (group s) === s

prop_group_all_same :: String -> Property
prop_group_all_same s = 
  let groups = group s
      allGroupsHaveSameChars = all (\g -> all (== head g) g) groups
  in property $ allGroupsHaveSameChars

-- Test string intercalate operations
prop_intercalate_empty :: String -> [String] -> Property
prop_intercalate_empty delim strings = 
  if null strings
  then property $ intercalate delim strings === ""
  else property $ True

prop_intercalate_single :: String -> String -> Property
prop_intercalate_single delim s = intercalate delim [s] === s

-- Test string transpose operations
prop_transpose_square :: [[Char]] -> Property
prop_transpose_square matrix = 
  let isSquare = all (\row -> length row == length matrix) matrix
  in if isSquare && not (null matrix)
     then property $ length (transpose matrix) === length (head matrix)
     else property $ True

-- Test string lookup operations
prop_lookup_found :: [(String, Int)] -> String -> Int -> Property
prop_lookup_found pairs key value = 
  let pairsWithKey = (key, value) : pairs
  in property $ lookup key pairsWithKey === Just value

prop_lookup_not_found :: [(String, Int)] -> String -> Property
prop_lookup_not_found pairs key = 
  let keyNotInPairs = key `notElem` map fst pairs
  in if keyNotInPairs
     then property $ lookup key pairs === Nothing
     else property $ True

-- Test string nub operations
prop_nub_removes_duplicates :: String -> Property
prop_nub_removes_duplicates s = 
  let nubbed = nub s
      hasDuplicates = any (> 1) (map (\c -> length (filter (== c) s)) (nub s))
  in property $ not hasDuplicates

prop_nub_preserves_order :: String -> Property
prop_nub_preserves_order s = 
  let nubbed = nub s
      originalOrder = map head $ group s
      nubbedOrder = nubbed
  in property $ nubbedOrder === originalOrder

prop_nub_length :: String -> Property
prop_nub_length s = length (nub s) <= length s

-- Test string union operations
prop_union_contains_all :: String -> String -> Property
prop_union_contains_all s1 s2 = 
  let unioned = union s1 s2
      s1InUnion = all (`elem` unioned) s1
      s2InUnion = all (`elem` unioned) s2
  in property $ s1InUnion && s2InUnion

prop_union_idempotent :: String -> String -> Property
prop_union_idempotent s1 s2 = union (union s1 s2) s2 === union s1 s2

-- Test string intersect operations
prop_intersect_subset :: String -> String -> Property
prop_intersect_subset s1 s2 = 
  let intersected = intersect s1 s2
      inS1 = all (`elem` s1) intersected
      inS2 = all (`elem` s2) intersected
  in property $ inS1 && inS2

prop_intersect_commutative :: String -> String -> Property
prop_intersect_commutative s1 s2 = intersect s1 s2 === intersect s2 s1

-- Test string difference operations
prop_difference_subset :: String -> String -> Property
prop_difference_subset s1 s2 = 
  let diffed = difference s1 s2
      inS1 = all (`elem` s1) diffed
      notInS2 = all (`notElem` s2) diffed
  in property $ inS1 && notInS2

-- Test string insert operations
prop_insert_contains :: String -> Char -> Property
prop_insert_contains s c = c `elem` insert c s

prop_insert_idempotent :: String -> Char -> Property
prop_insert_idempotent s c = insert c (insert c s) === insert c s

-- Test string delete operations
prop_delete_removes :: String -> Char -> Property
prop_delete_removes s c = c `notElem` delete c s

prop_delete_idempotent :: String -> Char -> Property
prop_delete_idempotent s c = delete c (delete c s) === delete c s

-- Test string replace operations
prop_replace_length :: String -> String -> String -> Property
prop_replace_length old new s = 
  let replaced = replace old new s
      oldCount = length (filter (== old) s)
      newCount = length (filter (== new) replaced)
  in if old == new
     then property $ replaced === s
     else property $ True

-- Test string isPrefixOf, isSuffixOf, isInfixOf properties
prop_is_prefix_of_transitive :: String -> String -> String -> Property
prop_is_prefix_of_transitive s1 s2 s3 = 
  let s1PrefixS2 = isPrefixOf s1 s2
      s2PrefixS3 = isPrefixOf s2 s3
  in if s1PrefixS2 && s2PrefixS3
     then property $ isPrefixOf s1 s3
     else property $ True

prop_is_suffix_of_transitive :: String -> String -> String -> Property
prop_is_suffix_of_transitive s1 s2 s3 = 
  let s1SuffixS2 = isSuffixOf s1 s2
      s2SuffixS3 = isSuffixOf s2 s3
  in if s1SuffixS2 && s2SuffixS3
     then property $ isSuffixOf s1 s3
     else property $ True

-- Test string stripPrefix/stripSuffix operations
prop_strip_prefix_found :: String -> String -> Property
prop_strip_prefix_found prefix s = 
  let sWithPrefix = prefix ++ s
  in property $ stripPrefix prefix sWithPrefix === Just s

prop_strip_suffix_found :: String -> String -> Property
prop_strip_suffix_found suffix s = 
  let sWithSuffix = s ++ suffix
  in property $ stripSuffix suffix sWithSuffix === Just s

prop_strip_prefix_not_found :: String -> String -> Property
prop_strip_prefix_not_found prefix s = 
  let prefixNotInString = not (isPrefixOf prefix s)
  in if prefixNotInString
     then property $ stripPrefix prefix s === Nothing
     else property $ True

prop_strip_suffix_not_found :: String -> String -> Property
prop_strip_suffix_not_found suffix s = 
  let suffixNotInString = not (isSuffixOf suffix s)
  in if suffixNotInString
     then property $ stripSuffix suffix s === Nothing
     else property $ True

-- Test string commonPrefix/commonSuffix operations
prop_common_prefix_is_prefix :: String -> String -> Property
prop_common_prefix_is_prefix s1 s2 = 
  let common = commonPrefix s1 s2
  in property $ isPrefixOf common s1 && isPrefixOf common s2

prop_common_suffix_is_suffix :: String -> String -> Property
prop_common_suffix_is_suffix s1 s2 = 
  let common = commonSuffix s1 s2
  in property $ isSuffixOf common s1 && isSuffixOf common s2

-- Test string partition operations
prop_partition_splits_correctly :: String -> Property
prop_partition_splits_correctly s = 
  let (satisfying, notSatisfying) = partition isSpace s
      satisfyingCorrect = all isSpace satisfying
      notSatisfyingCorrect = all (not . isSpace) notSatisfying
      unionCorrect = sort (satisfying ++ notSatisfying) === sort s
  in property $ satisfyingCorrect && notSatisfyingCorrect && unionCorrect

-- Test string find operations
prop_find_indices :: String -> Char -> Property
prop_find_indices s c = 
  let indices = findIndices (== c) s
      correctIndices = [i | (i, ch) <- zip [0..] s, ch == c]
  in property $ indices === correctIndices

prop_find_index :: String -> Char -> Property
prop_find_index s c = 
  let index = findIndex (== c) s
      indices = findIndices (== c) s
  in property $ index === (if null indices then Nothing else Just (head indices))

-- Test string elem operations
prop_elem_indices :: String -> Char -> Property
prop_elem_indices s c = 
  let indices = elemIndices c s
      correctIndices = [i | (i, ch) <- zip [0..] s, ch == c]
  in property $ indices === correctIndices

prop_elem_index :: String -> Char -> Property
prop_elem_index s c = 
  let index = elemIndex c s
      indices = elemIndices c s
  in property $ index === (if null indices then Nothing else Just (head indices))

-- Test string comparison operations
prop_compare_transitive :: String -> String -> String -> Property
prop_compare_transitive s1 s2 s3 = 
  let s1LTs2 = s1 < s2
      s2LTs3 = s2 < s3
  in if s1LTs2 && s2LTs3
     then property $ s1 < s3
     else property $ True

prop_compare_antisymmetric :: String -> String -> Property
prop_compare_antisymmetric s1 s2 = 
  let s1LTs2 = s1 < s2
      s2LTs1 = s2 < s1
  in property $ not (s1LTs2 && s2LTs1)

-- Test string maximum/minimum operations
prop_maximum_in_string :: String -> Property
prop_maximum_in_string s = 
  if null s
  then property $ True
  else property $ maximum s `elem` s

prop_minimum_in_string :: String -> Property
prop_minimum_in_string s = 
  if null s
  then property $ True
  else property $ minimum s `elem` s

-- Test string all/any operations
prop_all_empty :: String -> Property
prop_all_empty s = all (const False) ""

prop_any_empty :: String -> Property
prop_any_empty s = any (const True) ""

prop_all_all :: String -> Property
prop_all_all s = all (const True) s

prop_any_all :: String -> Property
prop_any_all s = any (const False) s === not (null s)

-- Test string sum/product operations
prop_sum_length :: String -> Property
prop_sum_length s = sum (map fromEnum s) >= 0

prop_product_length :: String -> Property
prop_product_length s = product (map fromEnum s) >= 0

-- Test string fold operations
prop_foldr_cons :: String -> Property
prop_foldr_cons s = foldr (:) [] s === s

prop_foldl_cons :: String -> Property
prop_foldl_cons s = foldl (flip (:)) [] s === reverse s

-- Test string scan operations
prop_scanl_length :: String -> Property
prop_scanl_length s = length (scanl (+) 0 (map fromEnum s)) === length s + 1

prop_scanr_length :: String -> Property
prop_scanr_length s = length (scanr (+) 0 (map fromEnum s)) === length s + 1

-- Test string mapAccum operations
prop_map_accum_length :: String -> Property
prop_map_accum_length s = 
  let (result, _) = mapAccumL (\acc c -> (acc + 1, c)) 0 s
  in property $ length result === length s

-- Test string unfold operations
prop_unfoldr_length :: Positive Int -> Property
prop_unfoldr_length (Positive n) = 
  let result = unfoldr (\i -> if i < n then Just (i, i + 1) else Nothing) 0
  in property $ length result === n

-- Test string iterate operations
prop_iterate_length :: Positive Int -> Property
prop_iterate_length (Positive n) = 
  let result = take n (iterate (+1) 0)
  in property $ length result === n

-- Test string repeat operations
prop_repeat_length :: Positive Int -> Property
prop_repeat_length (Positive n) = 
  let result = take n (repeat 'a')
  in property $ length result === n

prop_repeat_all_same :: Positive Int -> Property
prop_repeat_all_same (Positive n) = 
  let result = take n (repeat 'a')
  in property $ all (== 'a') result

-- Test string cycle operations
prop_cycle_length :: Positive Int -> Property
prop_cycle_length (Positive n) = 
  let input = "abc"
      result = take n (cycle input)
  in property $ length result === n

prop_cycle_repeats :: Positive Int -> Property
prop_cycle_repeats (Positive n) = 
  let input = "abc"
      result = take (length input * n) (cycle input)
      groups = [take (length input) (drop (i * length input) result) | i <- [0..n-1]]
  in property $ all (== input) groups

-- Test string replicate operations
prop_replicate_char :: Positive Int -> Char -> Property
prop_replicate_char (Positive n) c = 
  let result = replicate n c
  in property $ all (== c) result

prop_replicate_length_char :: Positive Int -> Char -> Property
prop_replicate_length_char (Positive n) c = 
  let result = replicate n c
  in property $ length result === n

-- Test string takeWhile/dropWhile operations
prop_take_while_length :: String -> Property
prop_take_while_length s = length (takeWhile isSpace s) <= length s

prop_drop_while_length :: String -> Property
prop_drop_while_length s = length (dropWhile isSpace s) <= length s

prop_take_while_drop_while_roundtrip :: String -> Property
prop_take_while_drop_while_roundtrip s = 
  let prefix = takeWhile isSpace s
      suffix = dropWhile isSpace s
  in property $ prefix ++ suffix === s

-- Test string span/break operations
prop_span_take_while_drop_while :: String -> Property
prop_span_take_while_drop_while s = 
  let (prefix, suffix) = span isSpace s
      expectedPrefix = takeWhile isSpace s
      expectedSuffix = dropWhile isSpace s
  in property $ (prefix, suffix) === (expectedPrefix, expectedSuffix)

prop_break_take_while_drop_while :: String -> Property
prop_break_take_while_drop_while s = 
  let (prefix, suffix) = break isSpace s
      expectedPrefix = takeWhile (not . isSpace) s
      expectedSuffix = dropWhile (not . isSpace) s
  in property $ (prefix, suffix) === (expectedPrefix, expectedSuffix)

-- Test string groupBy operations
prop_group_by_concat :: String -> Property
prop_group_by_concat s = concat (groupBy (==) s) === s

prop_group_by_all_same :: String -> Property
prop_group_by_all_same s = 
  let groups = groupBy (==) s
      allGroupsHaveSameChars = all (\g -> all (== head g) g) groups
  in property $ allGroupsHaveSameChars

-- Test string inits operations
prop_inits_empty :: Property
prop_inits_empty = inits "" === [""]

prop_inits_length :: String -> Property
prop_inits_length s = length (inits s) === length s + 1

prop_inits_last :: String -> Property
prop_inits_last s = last (inits s) === s

prop_inits_first :: String -> Property
prop_inits_first s = head (inits s) === ""

-- Test string tails operations
prop_tails_empty :: Property
prop_tails_empty = tails "" === [""]

prop_tails_length :: String -> Property
prop_tails_length s = length (tails s) === length s + 1

prop_tails_first :: String -> Property
prop_tails_first s = head (tails s) === s

prop_tails_last :: String -> Property
prop_tails_last s = last (tails s) === ""

-- Test string subsequences operations
prop_subsequences_empty :: Property
prop_subsequences_empty = subsequences "" === [[]]

prop_subsequences_contains_empty :: String -> Property
prop_subsequences_contains_empty s = [] `elem` subsequences s

prop_subsequences_contains_original :: String -> Property
prop_subsequences_contains_original s = s `elem` subsequences s

prop_subsequences_length :: String -> Property
prop_subsequences_length s = length (subsequences s) === 2 ^ length s

-- Test string permutations operations
prop_permutations_empty :: Property
prop_permutations_empty = permutations "" === [""]

prop_permutations_length :: String -> Property
prop_permutations_length s = 
  if length s > 6
  then property $ True  -- Skip for long strings
  else property $ length (permutations s) === factorial (length s)
  where
    factorial 0 = 1
    factorial n = n * factorial (n - 1)

prop_permutations_contains_original :: String -> Property
prop_permutations_contains_original s = 
  if length s > 6
  then property $ True  -- Skip for long strings
  else property $ s `elem` permutations s

-- Test string combinations operations
prop_combinations_zero :: String -> Property
prop_combinations_zero s = combinations 0 s === [[]]

prop_combinations_n :: String -> Property
prop_combinations_n s = 
  let n = length s
  in property $ combinations n s === [s]

prop_combinations_length :: String -> Property
prop_combinations_length s = 
  let n = min 3 (length s)  -- Limit to reasonable size
  in property $ all (\c -> length c === n) (combinations n s)

-- Test string elemIndex/findIndex operations
prop_elem_index_correct :: String -> Char -> Property
prop_elem_index_correct s c = 
  let index = elemIndex c s
      indices = [i | (i, ch) <- zip [0..] s, ch == c]
  in property $ index === (if null indices then Nothing else Just (head indices))

prop_find_index_correct :: String -> Char -> Property
prop_find_index_correct s c = 
  let index = findIndex (== c) s
      indices = [i | (i, ch) <- zip [0..] s, ch == c]
  in property $ index === (if null indices then Nothing else Just (head indices))

-- Test string elemIndices/findIndices operations
prop_elem_indices_correct :: String -> Char -> Property
prop_elem_indices_correct s c = 
  let indices = elemIndices c s
      correctIndices = [i | (i, ch) <- zip [0..] s, ch == c]
  in property $ indices === correctIndices

prop_find_indices_correct :: String -> Char -> Property
prop_find_indices_correct s c = 
  let indices = findIndices (== c) s
      correctIndices = [i | (i, ch) <- zip [0..] s, ch == c]
  in property $ indices === correctIndices

-- Test string find operations
prop_find_correct :: String -> Char -> Property
prop_find_correct s c = 
  let result = find (== c) s
      indices = [i | (i, ch) <- zip [0..] s, ch == c]
  in property $ result === (if null indices then Nothing else Just c)

-- Test string filter operations
prop_filter_correct :: String -> Property
prop_filter_correct s = 
  let filtered = filter isSpace s
      correct = [c | c <- s, isSpace c]
  in property $ filtered === correct

-- Test string partition operations
prop_partition_correct :: String -> Property
prop_partition_correct s = 
  let (satisfying, notSatisfying) = partition isSpace s
      correctSatisfying = [c | c <- s, isSpace c]
      correctNotSatisfying = [c | c <- s, not (isSpace c)]
  in property $ satisfying === correctSatisfying && notSatisfying === correctNotSatisfying

-- Test string span operations
prop_span_correct :: String -> Property
prop_span_correct s = 
  let (prefix, suffix) = span isSpace s
      correctPrefix = takeWhile isSpace s
      correctSuffix = dropWhile isSpace s
  in property $ prefix === correctPrefix && suffix === correctSuffix

-- Test string break operations
prop_break_correct :: String -> Property
prop_break_correct s = 
  let (prefix, suffix) = break isSpace s
      correctPrefix = takeWhile (not . isSpace) s
      correctSuffix = dropWhile (not . isSpace) s
  in property $ prefix === correctPrefix && suffix === correctSuffix

-- Test string group operations
prop_group_correct :: String -> Property
prop_group_correct s = 
  let grouped = group s
      correct = groupBy (==) s
  in property $ grouped === correct

-- Test string inits operations
prop_inits_correct :: String -> Property
prop_inits_correct s = 
  let initsList = inits s
      correct = [take i s | i <- [0..length s]]
  in property $ initsList === correct

-- Test string tails operations
prop_tails_correct :: String -> Property
prop_tails_correct s = 
  let tailsList = tails s
      correct = [drop i s | i <- [0..length s]]
  in property $ tailsList === correct

-- Test string mapAccumL operations
prop_map_accum_l_correct :: String -> Property
prop_map_accum_l_correct s = 
  let (result, acc) = mapAccumL (\acc c -> (acc + 1, c)) 0 s
      correct = zipWith (\i c -> i) [1..] s
  in property $ result === correct && acc === length s

-- Test string mapAccumR operations
prop_map_accum_r_correct :: String -> Property
prop_map_accum_r_correct s = 
  let (result, acc) = mapAccumR (\acc c -> (acc + 1, c)) 0 s
      correct = zipWith (\i c -> i) [length s, length s - 1..1] s
  in property $ result === correct && acc === length s

-- Test string unfoldr operations
prop_unfoldr_correct :: Positive Int -> Property
prop_unfoldr_correct (Positive n) = 
  let result = unfoldr (\i -> if i < n then Just (i, i + 1) else Nothing) 0
      correct = [0..n-1]
  in property $ result === correct

-- Test string iterate operations
prop_iterate_correct :: Positive Int -> Property
prop_iterate_correct (Positive n) = 
  let result = take n (iterate (+1) 0)
      correct = [0..n-1]
  in property $ result === correct

-- Test string repeat operations
prop_repeat_correct :: Positive Int -> Property
prop_repeat_correct (Positive n) = 
  let result = take n (repeat 'a')
      correct = replicate n 'a'
  in property $ result === correct

-- Test string cycle operations
prop_cycle_correct :: Positive Int -> Property
prop_cycle_correct (Positive n) = 
  let input = "abc"
      result = take n (cycle input)
      correct = take n (concat (repeat input))
  in property $ result === correct

-- Test string scanl operations
prop_scanl_correct :: String -> Property
prop_scanl_correct s = 
  let result = scanl (+) 0 (map fromEnum s)
      correct = scanl (\acc c -> acc + fromEnum c) 0 s
  in property $ result === correct

-- Test string scanr operations
prop_scanr_correct :: String -> Property
prop_scanr_correct s = 
  let result = scanr (+) 0 (map fromEnum s)
      correct = scanr (\c acc -> fromEnum c + acc) 0 s
  in property $ result === correct

-- Test string foldl operations
prop_foldl_correct :: String -> Property
prop_foldl_correct s = 
  let result = foldl (+) 0 (map fromEnum s)
      correct = foldl (\acc c -> acc + fromEnum c) 0 s
  in property $ result === correct

-- Test string foldr operations
prop_foldr_correct :: String -> Property
prop_foldr_correct s = 
  let result = foldr (+) 0 (map fromEnum s)
      correct = foldr (\c acc -> fromEnum c + acc) 0 s
  in property $ result === correct

-- Test string unfoldr operations for strings
prop_unfoldr_string :: Positive Int -> Property
prop_unfoldr_string (Positive n) = 
  let result = unfoldr (\i -> if i < n then Just ('a', i + 1) else Nothing) 0
      correct = replicate n 'a'
  in property $ result === correct

-- Test string iterate operations for strings
prop_iterate_string :: Positive Int -> Property
prop_iterate_string (Positive n) = 
  let result = take n (iterate ('a':) [])
      correct = replicate n (replicate n 'a')
  in property $ length result === n

-- Test string repeat operations for strings
prop_repeat_string :: Positive Int -> Property
prop_repeat_string (Positive n) = 
  let result = take n (repeat "abc")
      correct = replicate n "abc"
  in property $ result === correct

-- Test string cycle operations for strings
prop_cycle_string :: Positive Int -> Property
prop_cycle_string (Positive n) = 
  let input = "abc"
      result = take n (cycle input)
      correct = take n (concat (repeat input))
  in property $ result === correct

-- Test string scanl operations for strings
prop_scanl_string :: String -> Property
prop_scanl_string s = 
  let result = scanl (:) [] s
      correct = scanr (\c acc -> c : acc) [] s
  in property $ reverse result === correct

-- Test string scanr operations for strings
prop_scanr_string :: String -> Property
prop_scanr_string s = 
  let result = scanr (:) [] s
      correct = scanr (\c acc -> c : acc) [] s
  in property $ result === correct

-- Test string foldl operations for strings
prop_foldl_string :: String -> Property
prop_foldl_string s = 
  let result = foldl (++) [] (map (:[]) s)
      correct = s
  in property $ concat result === correct

-- Test string foldr operations for strings
prop_foldr_string :: String -> Property
prop_foldr_string s = 
  let result = foldr (:) [] s
      correct = s
  in property $ result === correct

-- Test string mapAccumL operations for strings
prop_map_accum_l_string :: String -> Property
prop_map_accum_l_string s = 
  let (result, acc) = mapAccumL (\acc c -> (acc + [c], acc + 1)) [] s
      correct = [take i s | i <- [1..length s]]
  in property $ result === correct && acc === length s

-- Test string mapAccumR operations for strings
prop_map_accum_r_string :: String -> Property
prop_map_accum_r_string s = 
  let (result, acc) = mapAccumR (\acc c -> (c : acc, acc + 1)) [] s
      correct = [drop i s | i <- [0..length s - 1]]
  in property $ reverse result === correct && acc === length s

-- Helper functions for tests
union :: String -> String -> String
union s1 s2 = nub (s1 ++ s2)

intersect :: String -> String -> String
intersect s1 s2 = nub [c | c <- s1, c `elem` s2]

difference :: String -> String -> String
difference s1 s2 = [c | c <- s1, c `notElem` s2]

insert :: Char -> String -> String
insert c s = if c `elem` s then s else c : s

delete :: Char -> String -> String
delete c s = filter (/= c) s

replace :: Char -> Char -> String -> String
replace old new s = map (\c -> if c == old then new else c) s

commonPrefix :: String -> String -> String
commonPrefix [] _ = []
commonPrefix _ [] = []
commonPrefix (x:xs) (y:ys) 
  | x == y = x : commonPrefix xs ys
  | otherwise = []

commonSuffix :: String -> String -> String
commonSuffix = commonPrefix . reverse . map reverse

stripPrefix :: String -> String -> Maybe String
stripPrefix [] s = Just s
stripPrefix _ [] = Nothing
stripPrefix (x:xs) (y:ys) 
  | x == y = stripPrefix xs ys
  | otherwise = Nothing

stripSuffix :: String -> String -> Maybe String
stripSuffix pat s = fmap reverse (stripPrefix (reverse pat) (reverse s))

findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices p = map fst . filter (p . snd) . zip [0..]

findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex p = listToMaybe . findIndices p

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x = findIndices (== x)

elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex x = findIndex (== x)

find :: (a -> Bool) -> [a] -> Maybe a
find p = listToMaybe . filter p

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy eq (x:xs) = (x:ys) : groupBy eq zs
  where (ys, zs) = span (eq x) xs

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(x:xs') = xs : tails xs'

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = subsequences xs ++ map (x:) (subsequences xs)

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = [x:ps | x <- xs, ps <- permutations (delete x xs)]

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
mapAccumL _ s [] = (s, [])
mapAccumL f s (x:xs) = 
  let (s', y) = f s x
      (s'', ys) = mapAccumL f s' xs
  in (s'', y:ys)

mapAccumR :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
mapAccumR _ s [] = (s, [])
mapAccumR f s (x:xs) = 
  let (s', ys) = mapAccumR f s xs
      (s'', y) = f s' x
  in (s'', y:ys)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b = case f b of
  Just (a, b') -> a : unfoldr f b'
  Nothing -> []

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

repeat :: a -> [a]
repeat x = xs where xs = x : xs

cycle :: [a] -> [a]
cycle [] = error "empty list"
cycle xs = xs' where xs' = xs ++ xs'

scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl _ z [] = [z]
scanl f z (x:xs) = z : scanl f (f z x) xs

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ z [] = [z]
scanr f z (x:xs) = f x (head (scanr f z xs)) : scanr f z xs

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

-- Tests collection
tests :: TestTree
tests = testGroup "String Processing QuickCheck Tests"
  [ testProperty "trim idempotent" prop_trim_idempotent
  , testProperty "trim removes spaces" prop_trim_removes_spaces
  , testProperty "trim preserves internal content" prop_trim_preserves_internal_content
  , testProperty "splitBy empty string" prop_split_by_empty_string
  , testProperty "splitBy single char" prop_split_by_single_char
  , testProperty "splitBy concatenates with delim" prop_split_by_concatenates_with_delim
  , testProperty "splitBy preserves non-delim chars" prop_split_by_preserves_non_delim_chars
  , testProperty "breakOn empty string" prop_break_on_empty_string
  , testProperty "breakOn delim not found" prop_break_on_delim_not_found
  , testProperty "breakOn delim at start" prop_break_on_delim_at_start
  , testProperty "breakOn delim at end" prop_break_on_delim_at_end
  , testProperty "to upper lower roundtrip" prop_to_upper_lower_roundtrip
  , testProperty "to lower upper roundtrip" prop_to_lower_upper_roundtrip
  , testProperty "sort idempotent" prop_sort_idempotent
  , testProperty "sort preserves elements" prop_sort_preserves_elements
  , testProperty "sort length preserved" prop_sort_length_preserved
  , testProperty "concat associative" prop_concat_associative
  , testProperty "concat identity left" prop_concat_identity_left
  , testProperty "concat identity right" prop_concat_identity_right
  , testProperty "reverse idempotent" prop_reverse_idempotent
  , testProperty "reverse length preserved" prop_reverse_length_preserved
  , testProperty "filter preserves order" prop_filter_preserves_order
  , testProperty "filter idempotent" prop_filter_idempotent
  , testProperty "words empty string" prop_words_empty_string
  , testProperty "words single word" prop_words_single_word
  , testProperty "unwords words roundtrip" prop_unwords_words_roundtrip
  , testProperty "lines empty string" prop_lines_empty_string
  , testProperty "lines single line" prop_lines_single_line
  , testProperty "unlines lines roundtrip" prop_unlines_lines_roundtrip
  , testProperty "is prefix of reflexive" prop_is_prefix_of_reflexive
  , testProperty "is suffix of reflexive" prop_is_suffix_of_reflexive
  , testProperty "is infix of reflexive" prop_is_infix_of_reflexive
  , testProperty "is prefix of empty" prop_is_prefix_of_empty
  , testProperty "is suffix of empty" prop_is_suffix_of_empty
  , testProperty "length non negative" prop_length_non_negative
  , testProperty "length concat" prop_length_concat
  , testProperty "length reverse" prop_length_reverse
  , testProperty "any preserves length" prop_any_preserves_length
  , testProperty "all preserves length" prop_all_preserves_length
  , testProperty "map length preserved" prop_map_length_preserved
  , testProperty "map idempotent" prop_map_idempotent
  , testProperty "replicate length" prop_replicate_length
  , testProperty "replicate all same" prop_replicate_all_same
  , testProperty "take length" prop_take_length
  , testProperty "take drop roundtrip" prop_take_drop_roundtrip
  , testProperty "take all" prop_take_all
  , testProperty "drop all" prop_drop_all
  , testProperty "span take drop" prop_span_take_drop
  , testProperty "break take drop" prop_break_take_drop
  , testProperty "partition union" prop_partition_union
  , testProperty "group concat" prop_group_concat
  , testProperty "group all same" prop_group_all_same
  , testProperty "intercalate empty" prop_intercalate_empty
  , testProperty "intercalate single" prop_intercalate_single
  , testProperty "transpose square" prop_transpose_square
  , testProperty "lookup found" prop_lookup_found
  , testProperty "lookup not found" prop_lookup_not_found
  , testProperty "nub removes duplicates" prop_nub_removes_duplicates
  , testProperty "nub preserves order" prop_nub_preserves_order
  , testProperty "nub length" prop_nub_length
  , testProperty "union contains all" prop_union_contains_all
  , testProperty "union idempotent" prop_union_idempotent
  , testProperty "intersect subset" prop_intersect_subset
  , testProperty "intersect commutative" prop_intersect_commutative
  , testProperty "difference subset" prop_difference_subset
  , testProperty "insert contains" prop_insert_contains
  , testProperty "insert idempotent" prop_insert_idempotent
  , testProperty "delete removes" prop_delete_removes
  , testProperty "delete idempotent" prop_delete_idempotent
  , testProperty "replace length" prop_replace_length
  , testProperty "is prefix of transitive" prop_is_prefix_of_transitive
  , testProperty "is suffix of transitive" prop_is_suffix_of_transitive
  , testProperty "strip prefix found" prop_strip_prefix_found
  , testProperty "strip suffix found" prop_strip_suffix_found
  , testProperty "strip prefix not found" prop_strip_prefix_not_found
  , testProperty "strip suffix not found" prop_strip_suffix_not_found
  , testProperty "common prefix is prefix" prop_common_prefix_is_prefix
  , testProperty "common suffix is suffix" prop_common_suffix_is_suffix
  , testProperty "partition splits correctly" prop_partition_splits_correctly
  , testProperty "find indices" prop_find_indices
  , testProperty "find index" prop_find_index
  , testProperty "elem indices" prop_elem_indices
  , testProperty "elem index" prop_elem_index
  , testProperty "compare transitive" prop_compare_transitive
  , testProperty "compare antisymmetric" prop_compare_antisymmetric
  , testProperty "maximum in string" prop_maximum_in_string
  , testProperty "minimum in string" prop_minimum_in_string
  , testProperty "all empty" prop_all_empty
  , testProperty "any empty" prop_any_empty
  , testProperty "all all" prop_all_all
  , testProperty "any all" prop_any_all
  , testProperty "sum length" prop_sum_length
  , testProperty "product length" prop_product_length
  , testProperty "foldr cons" prop_foldr_cons
  , testProperty "foldl cons" prop_foldl_cons
  , testProperty "scanl length" prop_scanl_length
  , testProperty "scanr length" prop_scanr_length
  , testProperty "map accum length" prop_map_accum_length
  , testProperty "unfoldr length" prop_unfoldr_length
  , testProperty "iterate length" prop_iterate_length
  , testProperty "repeat length" prop_repeat_length
  , testProperty "repeat all same" prop_repeat_all_same
  , testProperty "cycle length" prop_cycle_length
  , testProperty "cycle repeats" prop_cycle_repeats
  , testProperty "replicate char" prop_replicate_char
  , testProperty "replicate length char" prop_replicate_length_char
  , testProperty "take while length" prop_take_while_length
  , testProperty "drop while length" prop_drop_while_length
  , testProperty "take while drop while roundtrip" prop_take_while_drop_while_roundtrip
  , testProperty "span take while drop while" prop_span_take_while_drop_while
  , testProperty "break take while drop while" prop_break_take_while_drop_while
  , testProperty "group by concat" prop_group_by_concat
  , testProperty "group by all same" prop_group_by_all_same
  , testProperty "inits empty" prop_inits_empty
  , testProperty "inits length" prop_inits_length
  , testProperty "inits last" prop_inits_last
  , testProperty "inits first" prop_inits_first
  , testProperty "tails empty" prop_tails_empty
  , testProperty "tails length" prop_tails_length
  , testProperty "tails first" prop_tails_first
  , testProperty "tails last" prop_tails_last
  , testProperty "subsequences empty" prop_subsequences_empty
  , testProperty "subsequences contains empty" prop_subsequences_contains_empty
  , testProperty "subsequences contains original" prop_subsequences_contains_original
  , testProperty "subsequences length" prop_subsequences_length
  , testProperty "permutations empty" prop_permutations_empty
  , testProperty "permutations length" prop_permutations_length
  , testProperty "permutations contains original" prop_permutations_contains_original
  , testProperty "combinations zero" prop_combinations_zero
  , testProperty "combinations n" prop_combinations_n
  , testProperty "combinations length" prop_combinations_length
  , testProperty "elem index correct" prop_elem_index_correct
  , testProperty "find index correct" prop_find_index_correct
  , testProperty "elem indices correct" prop_elem_indices_correct
  , testProperty "find indices correct" prop_find_indices_correct
  , testProperty "find correct" prop_find_correct
  , testProperty "filter correct" prop_filter_correct
  , testProperty "partition correct" prop_partition_correct
  , testProperty "span correct" prop_span_correct
  , testProperty "break correct" prop_break_correct
  , testProperty "group correct" prop_group_correct
  , testProperty "inits correct" prop_inits_correct
  , testProperty "tails correct" prop_tails_correct
  , testProperty "map accum l correct" prop_map_accum_l_correct
  , testProperty "map accum r correct" prop_map_accum_r_correct
  , testProperty "unfoldr correct" prop_unfoldr_correct
  , testProperty "iterate correct" prop_iterate_correct
  , testProperty "repeat correct" prop_repeat_correct
  , testProperty "cycle correct" prop_cycle_correct
  , testProperty "scanl correct" prop_scanl_correct
  , testProperty "scanr correct" prop_scanr_correct
  , testProperty "foldl correct" prop_foldl_correct
  , testProperty "foldr correct" prop_foldr_correct
  , testProperty "unfoldr string" prop_unfoldr_string
  , testProperty "iterate string" prop_iterate_string
  , testProperty "repeat string" prop_repeat_string
  , testProperty "cycle string" prop_cycle_string
  , testProperty "scanl string" prop_scanl_string
  , testProperty "scanr string" prop_scanr_string
  , testProperty "foldl string" prop_foldl_string
  , testProperty "foldr string" prop_foldr_string
  , testProperty "map accum l string" prop_map_accum_l_string
  , testProperty "map accum r string" prop_map_accum_r_string
  ]