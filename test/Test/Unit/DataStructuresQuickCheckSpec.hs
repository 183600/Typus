module Test.Unit.DataStructuresQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import TestSupport.QuickCheck (fastProperty)
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight, fromLeft, fromRight)

-- Properties for lists
prop_list_reverse_reverse :: [Int] -> Bool
prop_list_reverse_reverse xs = reverse (reverse xs) == xs

prop_list_length_append :: [Int] -> [Int] -> Bool
prop_list_length_append xs ys = length (xs ++ ys) == length xs + length ys

prop_list_append_associative :: [Int] -> [Int] -> [Int] -> Bool
prop_list_append_associative xs ys zs = (xs ++ ys) ++ zs == xs ++ (ys ++ zs)

prop_list_append_identity_left :: [Int] -> Bool
prop_list_append_identity_left xs = [] ++ xs == xs

prop_list_append_identity_right :: [Int] -> Bool
prop_list_append_identity_right xs = xs ++ [] == xs

prop_list_head_length :: [Int] -> Property
prop_list_head_length xs = not (null xs) ==> length (head xs : tail xs) == length xs

prop_list_tail_length :: [Int] -> Property
prop_list_tail_length xs = not (null xs) ==> length xs == 1 + length (tail xs)

prop_list_sort_preserves_elements :: [Int] -> Bool
prop_list_sort_preserves_elements xs = L.sort xs `isPermutationOf` xs
  where
    isPermutationOf ys zs = L.sort ys == L.sort zs

-- Properties for Maybe
prop_maybe_nothing_is_nothing :: Maybe Int -> Bool
prop_maybe_nothing_is_nothing m = isNothing m == (m == Nothing)

prop_maybe_just_is_just :: Maybe Int -> Bool
prop_maybe_just_is_just m = isJust m == (m /= Nothing)

prop_maybe_fromMaybe :: Int -> Maybe Int -> Bool
prop_maybe_fromMaybe def m = fromMaybe def m == if isNothing m then def else let (Just x) = m in x

-- Properties for Either
prop_either_left_is_left :: Either Int String -> Bool
prop_either_left_is_left e = isLeft e == case e of Left _ -> True; Right _ -> False

prop_either_right_is_right :: Either Int String -> Bool
prop_either_right_is_right e = isRight e == case e of Left _ -> False; Right _ -> True

prop_either_fromLeft :: Int -> Either Int String -> Bool
prop_either_fromLeft def e = fromLeft def e == case e of Left x -> x; Right _ -> def

prop_either_fromRight :: String -> Either Int String -> Bool
prop_either_fromRight def e = fromRight def e == case e of Left _ -> def; Right x -> x

-- Properties for Map
prop_map_size_insert :: Map.Map Int String -> Int -> String -> Bool
prop_map_size_insert m k v = Map.size (Map.insert k v m) == 
  if Map.member k m then Map.size m else Map.size m + 1

prop_map_member_insert :: Map.Map Int String -> Int -> String -> Bool
prop_map_member_insert m k v = Map.member k (Map.insert k v m)

prop_map_lookup_insert :: Map.Map Int String -> Int -> String -> Bool
prop_map_lookup_insert m k v = Map.lookup k (Map.insert k v m) == Just v

prop_map_delete_not_member :: Map.Map Int String -> Int -> Property
prop_map_delete_not_member m k = not (Map.member k m) ==> not (Map.member k (Map.delete k m))

prop_map_union_size :: Map.Map Int String -> Map.Map Int String -> Bool
prop_map_union_size m1 m2 = Map.size (Map.union m1 m2) <= Map.size m1 + Map.size m2

-- Properties for Set
prop_set_size_insert :: Set.Set Int -> Int -> Bool
prop_set_size_insert s x = Set.size (Set.insert x s) == 
  if Set.member x s then Set.size s else Set.size s + 1

prop_set_member_insert :: Set.Set Int -> Int -> Bool
prop_set_member_insert s x = Set.member x (Set.insert x s)

prop_set_delete_not_member :: Set.Set Int -> Int -> Property
prop_set_delete_not_member s x = not (Set.member x s) ==> not (Set.member x (Set.delete x s))

prop_set_union_size :: Set.Set Int -> Set.Set Int -> Bool
prop_set_union_size s1 s2 = Set.size (Set.union s1 s2) <= Set.size s1 + Set.size s2

prop_set_intersection_size :: Set.Set Int -> Set.Set Int -> Bool
prop_set_intersection_size s1 s2 = Set.size (Set.intersection s1 s2) <= min (Set.size s1) (Set.size s2)

prop_set_difference_size :: Set.Set Int -> Set.Set Int -> Bool
prop_set_difference_size s1 s2 = Set.size (Set.difference s1 s2) <= Set.size s1

-- Properties for tuples
prop_tuple_fst :: (Int, String) -> Bool
prop_tuple_fst t = fst t == case t of (x, _) -> x

prop_tuple_snd :: (Int, String) -> Bool
prop_tuple_snd t = snd t == case t of (_, y) -> y

prop_tuple_swap :: (Int, String) -> Bool
prop_tuple_swap t = let (x, y) = t in (y, x) == swap t
  where
    swap (a, b) = (b, a)

-- Properties for functions
prop_function_composition_associative :: Int -> Bool
prop_function_composition_associative x = ((f . g) . h) x == (f . (g . h)) x
  where
    f = (* 2)
    g = (+ 1)
    h = (^ 2)

prop_function_identity :: Int -> Bool
prop_function_identity x = (id . id) x == id x

-- Properties for Boolean operations
prop_bool_not :: Bool -> Bool
prop_bool_not b = not (not b) == b

prop_bool_and_identity :: Bool -> Bool
prop_bool_and_identity b = b && True == b

prop_bool_or_identity :: Bool -> Bool
prop_bool_or_identity b = b || False == b

prop_bool_and_commutative :: Bool -> Bool -> Bool
prop_bool_and_commutative b1 b2 = (b1 && b2) == (b2 && b1)

prop_bool_or_commutative :: Bool -> Bool -> Bool
prop_bool_or_commutative b1 b2 = (b1 || b2) == (b2 || b1)

prop_bool_and_associative :: Bool -> Bool -> Bool -> Bool
prop_bool_and_associative b1 b2 b3 = (b1 && b2 && b3) == (b1 && (b2 && b3))

prop_bool_or_associative :: Bool -> Bool -> Bool -> Bool
prop_bool_or_associative b1 b2 b3 = (b1 || b2 || b3) == (b1 || (b2 || b3))

prop_bool_distributive :: Bool -> Bool -> Bool -> Bool
prop_bool_distributive b1 b2 b3 = (b1 && (b2 || b3)) == ((b1 && b2) || (b1 && b3))

tests :: TestTree
tests = testGroup "Test.Unit.DataStructuresQuickCheckSpec Tests"
  [ fastProperty "list reverse reverse" prop_list_reverse_reverse
  , fastProperty "list length append" prop_list_length_append
  , fastProperty "list append associative" prop_list_append_associative
  , fastProperty "list append identity left" prop_list_append_identity_left
  , fastProperty "list append identity right" prop_list_append_identity_right
  , fastProperty "list head length" prop_list_head_length
  , fastProperty "list tail length" prop_list_tail_length
  , fastProperty "list sort preserves elements" prop_list_sort_preserves_elements
  , fastProperty "maybe nothing is nothing" prop_maybe_nothing_is_nothing
  , fastProperty "maybe just is just" prop_maybe_just_is_just
  , fastProperty "maybe fromMaybe" prop_maybe_fromMaybe
  , fastProperty "either left is left" prop_either_left_is_left
  , fastProperty "either right is right" prop_either_right_is_right
  , fastProperty "either fromLeft" prop_either_fromLeft
  , fastProperty "either fromRight" prop_either_fromRight
  , fastProperty "map size insert" prop_map_size_insert
  , fastProperty "map member insert" prop_map_member_insert
  , fastProperty "map lookup insert" prop_map_lookup_insert
  , fastProperty "map delete not member" prop_map_delete_not_member
  , fastProperty "map union size" prop_map_union_size
  , fastProperty "set size insert" prop_set_size_insert
  , fastProperty "set member insert" prop_set_member_insert
  , fastProperty "set delete not member" prop_set_delete_not_member
  , fastProperty "set union size" prop_set_union_size
  , fastProperty "set intersection size" prop_set_intersection_size
  , fastProperty "set difference size" prop_set_difference_size
  , fastProperty "tuple fst" prop_tuple_fst
  , fastProperty "tuple snd" prop_tuple_snd
  , fastProperty "tuple swap" prop_tuple_swap
  , fastProperty "function composition associative" prop_function_composition_associative
  , fastProperty "function identity" prop_function_identity
  , fastProperty "bool not" prop_bool_not
  , fastProperty "bool and identity" prop_bool_and_identity
  , fastProperty "bool or identity" prop_bool_or_identity
  , fastProperty "bool and commutative" prop_bool_and_commutative
  , fastProperty "bool or commutative" prop_bool_or_commutative
  , fastProperty "bool and associative" prop_bool_and_associative
  , fastProperty "bool or associative" prop_bool_or_associative
  , fastProperty "bool distributive" prop_bool_distributive
  ]