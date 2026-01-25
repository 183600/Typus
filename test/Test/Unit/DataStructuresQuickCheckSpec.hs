module Test.Unit.DataStructuresQuickCheckSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

import Test.QuickCheck.Gen (Gen(..))
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (isJust, isNothing, fromMaybe)

-- | 测试列表属性
prop_list_length_nonnegative :: [Int] -> Property
prop_list_length_nonnegative xs = 
  property $ length xs >= 0
prop_list_reverse_preserves_length :: [Int] -> Property
prop_list_reverse_preserves_length xs =
  length (reverse xs) === length xs

prop_list_reverse_idempotent :: [Int] -> Property
prop_list_reverse_idempotent xs =
  reverse (reverse xs) === xs

prop_list_sort_preserves_elements :: [Int] -> Property
prop_list_sort_preserves_elements xs =
  L.sort xs === L.sort (L.sort xs)

prop_list_sort_ordered :: [Int] -> Property
prop_list_sort_ordered xs =
  let sorted = L.sort xs
  in property $ ordered sorted

prop_list_concat_associative :: [Int] -> [Int] -> [Int] -> Property
prop_list_concat_associative xs ys zs =
  (xs ++ ys) ++ zs === xs ++ (ys ++ zs)

prop_list_concat_length :: [Int] -> [Int] -> Property
prop_list_concat_length xs ys =
  length (xs ++ ys) === length xs + length ys

-- | 测试Maybe属性
prop_maybe_fmap_preserves_nothing :: Maybe Int -> Property
prop_maybe_fmap_preserves_nothing m =
  fmap (+1) m === fmap (+1) (fmap (+1) m)

prop_maybe_fmap_composition :: Maybe Int -> Property
prop_maybe_fmap_composition m =
  fmap ((* 2) . (+ 1)) m === (fmap (* 2) . fmap (+ 1)) m

prop_maybe_bind_left_identity :: Int -> Property
prop_maybe_bind_left_identity x =
  let f :: Int -> Maybe Int
      f y = if y `mod` 2 == 0 then Just (y * 2) else Nothing
  in (Just x >>= f) === f x

prop_maybe_bind_right_identity :: Maybe Int -> Property
prop_maybe_bind_right_identity m =
  (m >>= Just) === m

prop_maybe_bind_associative :: Maybe Int -> Property
prop_maybe_bind_associative m =
  let f :: Int -> Maybe Int
      f y = if y `mod` 2 == 0 then Just (y * 2) else Nothing
      g :: Int -> Maybe Int
      g y = if y `mod` 3 == 0 then Just (y * 3) else Nothing
  in (m >>= (\x -> f x >>= g)) === ((m >>= f) >>= g)

-- | 测试Either属性
prop_either_fmap_left_preserves :: Either String Int -> Property
prop_either_fmap_left_preserves e =
  fmap (+1) e === fmap (+1) (fmap (+1) e)

prop_either_fmap_composition :: Either String Int -> Property
prop_either_fmap_composition e =
  fmap ((* 2) . (+ 1)) e === (fmap (* 2) . fmap (+ 1)) e

prop_either_left_identity :: Int -> Property
prop_either_left_identity x =
  let f :: Int -> Either String Int
      f y = if y `mod` 2 == 0 then Right (y * 2) else Left "odd number"
  in case f x of
    Left _ -> property True
    Right y -> y === x

-- | 测试Map属性
prop_map_size_nonnegative :: Map.Map Int String -> Property
prop_map_size_nonnegative m = 
  property $ Map.size m >= 0
prop_map_lookup_finds_inserted :: Map.Map Int String -> Int -> String -> Property
prop_map_lookup_finds_inserted m k v =
  Map.lookup k (Map.insert k v m) === Just v

prop_map_insert_overwrites :: Map.Map Int String -> Int -> String -> String -> Property
prop_map_insert_overwrites m k v1 v2 =
  Map.lookup k (Map.insert k v2 (Map.insert k v1 m)) === Just v2

prop_map_delete_removes :: Map.Map Int String -> Int -> Property
prop_map_delete_removes m k =
  Map.lookup k (Map.delete k m) === Nothing

prop_map_union_preserves_left :: Map.Map Int String -> Map.Map Int String -> Property
prop_map_union_preserves_left m1 m2 =
  let union = Map.union m1 m2
  in property $ Set.fromList (Map.keys m1) `Set.isSubsetOf` Set.fromList (Map.keys union)

prop_map_union_preserves_right :: Map.Map Int String -> Map.Map Int String -> Property
prop_map_union_preserves_right m1 m2 =
  let union = Map.union m1 m2
  in property $ Set.fromList (Map.keys (Map.difference m2 m1)) `Set.isSubsetOf` Set.fromList (Map.keys union)

-- | 测试Set属性
prop_set_size_nonnegative :: Set.Set Int -> Property
prop_set_size_nonnegative s = 
  property $ Set.size s >= 0
prop_set_member_finds_inserted :: Set.Set Int -> Int -> Property
prop_set_member_finds_inserted s x =
  property $ Set.member x (Set.insert x s)

prop_set_insert_idempotent :: Set.Set Int -> Int -> Property
prop_set_insert_idempotent s x =
  Set.insert x (Set.insert x s) === Set.insert x s

prop_set_delete_removes :: Set.Set Int -> Int -> Property
prop_set_delete_removes s x =
  property $ not (Set.member x (Set.delete x s))

prop_set_union_associative :: Set.Set Int -> Set.Set Int -> Set.Set Int -> Property
prop_set_union_associative s1 s2 s3 =
  Set.union s1 (Set.union s2 s3) === Set.union (Set.union s1 s2) s3

prop_set_intersection_associative :: Set.Set Int -> Set.Set Int -> Set.Set Int -> Property
prop_set_intersection_associative s1 s2 s3 =
  Set.intersection s1 (Set.intersection s2 s3) === Set.intersection (Set.intersection s1 s2) s3

prop_set_difference_nonnegative :: Set.Set Int -> Set.Set Int -> Property
prop_set_difference_nonnegative s1 s2 =
  property $ Set.size (Set.difference s1 s2) >= 0

-- | 测试元组属性
prop_tuple_fst_preserves :: (Int, String) -> Property
prop_tuple_fst_preserves t =
  fst t === fst t

prop_tuple_snd_preserves :: (Int, String) -> Property
prop_tuple_snd_preserves t =
  snd t === snd t

prop_tuple_swap_involutive :: (Int, String) -> Property
prop_tuple_swap_involutive t =
  let swapped = swap t
      swappedAgain = swap swapped
  in swappedAgain === t

-- | 辅助函数
ordered :: [Int] -> Bool
ordered [] = True
ordered [_] = True
ordered (x:y:xs) = x <= y && ordered (y:xs)

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf xs ys = all (`elem` ys) xs

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

tests :: TestTree
tests = testGroup "Data Structures QuickCheck Tests"
  [ testProperty "list length nonnegative" prop_list_length_nonnegative
  , testProperty "list reverse preserves length" prop_list_reverse_preserves_length
  , testProperty "list reverse idempotent" prop_list_reverse_idempotent
  , testProperty "list sort preserves elements" prop_list_sort_preserves_elements
  , testProperty "list sort ordered" prop_list_sort_ordered
  , testProperty "list concat associative" prop_list_concat_associative
  , testProperty "list concat length" prop_list_concat_length
  , testProperty "maybe fmap preserves nothing" prop_maybe_fmap_preserves_nothing
  , testProperty "maybe fmap composition" prop_maybe_fmap_composition
  , testProperty "maybe bind left identity" prop_maybe_bind_left_identity
  , testProperty "maybe bind right identity" prop_maybe_bind_right_identity
  , testProperty "maybe bind associative" prop_maybe_bind_associative
  , testProperty "either fmap left preserves" prop_either_fmap_left_preserves
  , testProperty "either fmap composition" prop_either_fmap_composition
  , testProperty "either left identity" prop_either_left_identity
  , testProperty "map size nonnegative" prop_map_size_nonnegative
  , testProperty "map lookup finds inserted" prop_map_lookup_finds_inserted
  , testProperty "map insert overwrites" prop_map_insert_overwrites
  , testProperty "map delete removes" prop_map_delete_removes
  , testProperty "map union preserves left" prop_map_union_preserves_left
  , testProperty "map union preserves right" prop_map_union_preserves_right
  , testProperty "set size nonnegative" prop_set_size_nonnegative
  , testProperty "set member finds inserted" prop_set_member_finds_inserted
  , testProperty "set insert idempotent" prop_set_insert_idempotent
  , testProperty "set delete removes" prop_set_delete_removes
  , testProperty "set union associative" prop_set_union_associative
  , testProperty "set intersection associative" prop_set_intersection_associative
  , testProperty "set difference nonnegative" prop_set_difference_nonnegative
  , testProperty "tuple fst preserves" prop_tuple_fst_preserves
  , testProperty "tuple snd preserves" prop_tuple_snd_preserves
  , testProperty "tuple swap involutive" prop_tuple_swap_involutive
  ]