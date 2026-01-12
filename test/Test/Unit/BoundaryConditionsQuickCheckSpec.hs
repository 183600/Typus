module Test.Unit.BoundaryConditionsQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import TestSupport.QuickCheck (fastProperty)
import Data.Char (isDigit, isLetter, isSpace)
import Data.List (null, head, tail, init, last)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Properties for empty collections
prop_empty_list_length :: Bool
prop_empty_list_length = length ([] :: [Int]) == 0

prop_empty_list_null :: Bool
prop_empty_list_null = null ([] :: [Int])

prop_empty_string_length :: Bool
prop_empty_string_length = length "" == 0

prop_empty_map_size :: Bool
prop_empty_map_size = Map.size (Map.empty :: Map.Map Int String) == 0

prop_empty_set_size :: Bool
prop_empty_set_size = Set.size (Set.empty :: Set.Set Int) == 0

-- Properties for single-element collections
prop_singleton_list_length :: Int -> Bool
prop_singleton_list_length x = length [x] == 1

prop_singleton_list_head :: Int -> Bool
prop_singleton_list_head x = head [x] == x

prop_singleton_list_last :: Int -> Bool
prop_singleton_list_last x = last [x] == x

prop_singleton_map_size :: Int -> String -> Bool
prop_singleton_map_size k v = Map.size (Map.singleton k v) == 1

prop_singleton_set_size :: Int -> Bool
prop_singleton_set_size x = Set.size (Set.singleton x) == 1

-- Properties for boundary values in arithmetic
prop_add_zero :: Int -> Bool
prop_add_zero x = x + 0 == x

prop_mult_zero :: Int -> Bool
prop_mult_zero x = x * 0 == 0

prop_mult_one :: Int -> Bool
prop_mult_one x = x * 1 == x

prop_div_one :: Property
prop_div_one x = x /= 0 ==> div x 1 == x

prop_mod_one :: Property
prop_mod_one x = x /= 0 ==> mod x 1 == 0

-- Properties for string boundaries
prop_empty_string_reverse :: Bool
prop_empty_string_reverse = reverse "" == ""

prop_empty_string_concat :: String -> Bool
prop_empty_string_concat s = "" ++ s == s && s ++ "" == s

prop_single_char_string_length :: Char -> Bool
prop_single_char_string_length c = length [c] == 1

prop_single_char_string_reverse :: Char -> Bool
prop_single_char_string_reverse c = reverse [c] == [c]

-- Properties for list boundaries
prop_empty_list_reverse :: Bool
prop_empty_list_reverse = reverse ([] :: [Int]) == []

prop_empty_list_concat :: [Int] -> Bool
prop_empty_list_concat xs = [] ++ xs == xs && xs ++ [] == xs

prop_single_element_list_reverse :: Int -> Bool
prop_single_element_list_reverse x = reverse [x] == [x]

prop_list_head_tail :: [Int] -> Property
prop_list_head_tail xs = not (null xs) ==> head xs : tail xs == xs

prop_list_init_last :: [Int] -> Property
prop_list_init_last xs = not (null xs) ==> init xs ++ [last xs] == xs

-- Properties for character boundaries
prop_isdigit_digit :: Property
prop_isdigit_digit d = isDigit d ==> d `elem` ['0'..'9']

prop_isletter_letter :: Property
prop_isletter_letter l = isLetter l ==> 
  l `elem` ['a'..'z'] || l `elem` ['A'..'Z']

prop_isspace_space :: Property
prop_isspace_space s = isSpace s ==> s `elem` " \t\n\r\f\v"

-- Properties for map boundaries
prop_empty_map_lookup :: Int -> Bool
prop_empty_map_lookup k = Map.lookup k (Map.empty :: Map.Map Int String) == Nothing

prop_singleton_map_lookup :: Int -> String -> Int -> Bool
prop_singleton_map_lookup k v k' = Map.lookup k' (Map.singleton k v) == 
  if k' == k then Just v else Nothing

prop_empty_map_member :: Int -> Bool
prop_empty_map_member k = not (Map.member k (Map.empty :: Map.Map Int String))

prop_singleton_map_member :: Int -> String -> Int -> Bool
prop_singleton_map_member k v k' = Map.member k' (Map.singleton k v) == (k' == k)

-- Properties for set boundaries
prop_empty_set_member :: Int -> Bool
prop_empty_set_member x = not (Set.member x (Set.empty :: Set.Set Int))

prop_singleton_set_member :: Int -> Int -> Bool
prop_singleton_set_member x y = Set.member y (Set.singleton x) == (y == x)

prop_empty_set_insert :: Int -> Bool
prop_empty_set_insert x = Set.insert x (Set.empty :: Set.Set Int) == Set.singleton x

prop_singleton_set_delete :: Int -> Int -> Bool
prop_singleton_set_delete x y = Set.delete y (Set.singleton x) == 
  if x == y then Set.empty else Set.singleton x

-- Properties for boundary conditions in functions
prop_maximum_singleton :: Int -> Bool
prop_maximum_singleton x = maximum [x] == x

prop_minimum_singleton :: Int -> Bool
prop_minimum_singleton x = minimum [x] == x

prop_sum_empty :: Bool
prop_sum_empty = sum ([] :: [Int]) == 0

prop_sum_singleton :: Int -> Bool
prop_sum_singleton x = sum [x] == x

prop_product_empty :: Bool
prop_product_empty = product ([] :: [Int]) == 1

prop_product_singleton :: Int -> Bool
prop_product_singleton x = product [x] == x

-- Properties for boundary conditions in Boolean logic
prop_bool_and_false :: Bool -> Bool
prop_bool_and_false b = b && False == False

prop_bool_or_true :: Bool -> Bool
prop_bool_or_true b = b || True == True

prop_bool_and_true :: Bool -> Bool
prop_bool_and_true b = b && True == b

prop_bool_or_false :: Bool -> Bool
prop_bool_or_false b = b || False == b

-- Properties for boundary conditions in Maybe
prop_nothing_isNothing :: Bool
prop_nothing_isNothing = isNothing (Nothing :: Maybe Int)

prop_just_isJust :: Int -> Bool
prop_just_isJust x = isJust (Just x)

prop_nothing_fromMaybe :: Int -> Bool
prop_nothing_fromMaybe x = fromMaybe x Nothing == x

prop_just_fromMaybe :: Int -> Int -> Bool
prop_just_fromMaybe def x = fromMaybe def (Just x) == x

-- Properties for boundary conditions in Either
prop_left_fromLeft :: Int -> String -> Bool
prop_left_fromLeft x y = fromLeft x (Left x) == x

prop_right_fromRight :: Int -> String -> Bool
prop_right_fromRight x y = fromRight y (Right y) == y

tests :: TestTree
tests = testGroup "Test.Unit.BoundaryConditionsQuickCheckSpec Tests"
  [ fastProperty "empty list length" (const prop_empty_list_length)
  , fastProperty "empty list null" (const prop_empty_list_null)
  , fastProperty "empty string length" (const prop_empty_string_length)
  , fastProperty "empty map size" (const prop_empty_map_size)
  , fastProperty "empty set size" (const prop_empty_set_size)
  , fastProperty "singleton list length" prop_singleton_list_length
  , fastProperty "singleton list head" prop_singleton_list_head
  , fastProperty "singleton list last" prop_singleton_list_last
  , fastProperty "singleton map size" prop_singleton_map_size
  , fastProperty "singleton set size" prop_singleton_set_size
  , fastProperty "add zero" prop_add_zero
  , fastProperty "mult zero" prop_mult_zero
  , fastProperty "mult one" prop_mult_one
  , fastProperty "div one" prop_div_one
  , fastProperty "mod one" prop_mod_one
  , fastProperty "empty string reverse" (const prop_empty_string_reverse)
  , fastProperty "empty string concat" prop_empty_string_concat
  , fastProperty "single char string length" prop_single_char_string_length
  , fastProperty "single char string reverse" prop_single_char_string_reverse
  , fastProperty "empty list reverse" (const prop_empty_list_reverse)
  , fastProperty "empty list concat" prop_empty_list_concat
  , fastProperty "single element list reverse" prop_single_element_list_reverse
  , fastProperty "list head tail" prop_list_head_tail
  , fastProperty "list init last" prop_list_init_last
  , fastProperty "isdigit digit" prop_isdigit_digit
  , fastProperty "isletter letter" prop_isletter_letter
  , fastProperty "isspace space" prop_isspace_space
  , fastProperty "empty map lookup" prop_empty_map_lookup
  , fastProperty "singleton map lookup" prop_singleton_map_lookup
  , fastProperty "empty map member" prop_empty_map_member
  , fastProperty "singleton map member" prop_singleton_map_member
  , fastProperty "empty set member" prop_empty_set_member
  , fastProperty "singleton set member" prop_singleton_set_member
  , fastProperty "empty set insert" prop_empty_set_insert
  , fastProperty "singleton set delete" prop_singleton_set_delete
  , fastProperty "maximum singleton" prop_maximum_singleton
  , fastProperty "minimum singleton" prop_minimum_singleton
  , fastProperty "sum empty" (const prop_sum_empty)
  , fastProperty "sum singleton" prop_sum_singleton
  , fastProperty "product empty" (const prop_product_empty)
  , fastProperty "product singleton" prop_product_singleton
  , fastProperty "bool and false" prop_bool_and_false
  , fastProperty "bool or true" prop_bool_or_true
  , fastProperty "bool and true" prop_bool_and_true
  , fastProperty "bool or false" prop_bool_or_false
  , fastProperty "nothing isNothing" (const prop_nothing_isNothing)
  , fastProperty "just isJust" prop_just_isJust
  , fastProperty "nothing fromMaybe" prop_nothing_fromMaybe
  , fastProperty "just fromMaybe" prop_just_fromMaybe
  , fastProperty "left fromLeft" prop_left_fromLeft
  , fastProperty "right fromRight" prop_right_fromRight
  ]