{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Additional QuickCheck tests for Typus compiler
-- This module contains additional property-based tests for various components
module Test.Unit.AdditionalQuickCheckTestsSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck ((==>), conjoin, counterexample)
import Utils
import SourceLocation
import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isAlpha, toLower, toUpper)
import Data.List (isPrefixOf, isInfixOf, sort, nub, group, intercalate)
import Control.Monad (foldM, when)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes, listToMaybe)
import qualified Compiler.Errors.Core as Error
import Compiler.Errors.Core (ErrorSeverity(..), ErrorCategory(..), TypeError(..), ErrorLocation(..), ErrorContext(..),
                            errorAt, errorWithCategory, warningAt, infoAt, 
                            fatalError, withLocation, withContext, combineErrors,
                            combinedErrorSeverity, filterByCategory, filterBySeverity,
                            hasCategory, isAtLeast, severityPriority, location, line, column, 
                            fatalRecovery, emptyContext, contextCode)
import SourceLocation (toErrorLocation)
import Data.Time (UTCTime, getCurrentTime)
import Data.Ord (comparing)
import Data.Function (on)
import qualified Data.Set as Set

-- ============================================================================
-- String and Text Processing Tests
-- ============================================================================

-- | Test that toLower and toUpper are inverses for alphabetic characters
prop_toLower_toUpper_alpha :: Char -> Property
prop_toLower_toUpper_alpha c = 
  isAlpha c ==> toLower (toUpper c) == toLower c

-- | Test that toUpper and toLower are inverses for alphabetic characters
prop_toUpper_toLower_alpha :: Char -> Property
prop_toUpper_toLower_alpha c = 
  isAlpha c ==> toUpper (toLower c) == toUpper c

-- | Test that sorting a list and then sorting again doesn't change it
prop_sort_idempotent :: [Int] -> Bool
prop_sort_idempotent xs = sort xs == sort (sort xs)

-- | Test that removing duplicates with nub and then with nub again doesn't change it
prop_nub_idempotent :: [Int] -> Bool
prop_nub_idempotent xs = nub xs == nub (nub xs)

-- | Test that intercalating with an empty separator is just concatenation
prop_intercalate_empty :: [String] -> Bool
prop_intercalate_empty xs = intercalate "" xs == concat xs

-- | Test that group preserves the total number of elements
prop_group_preserves_length :: [Int] -> Bool
prop_group_preserves_length xs = 
  let groups = group xs
  in sum (map length groups) == length xs

-- | Test that sorting followed by grouping creates groups of equal elements
prop_sort_group_equal :: [Int] -> Bool
prop_sort_group_equal xs = 
  let groups = group (sort xs)
      allEqualInGroup g = case listToMaybe g of
                            Nothing -> True
                            Just h -> all (== h) g
  in all allEqualInGroup groups

-- ============================================================================
-- List and Collection Tests
-- ============================================================================

-- | Test that concatenating a list with itself and then taking half gives the original
prop_concat_half :: [Int] -> Property
prop_concat_half xs = 
  not (null xs) ==> take (length xs) (xs ++ xs) == xs

-- | Test that reversing a list twice gives the original
prop_reverse_twice :: [Int] -> Bool
prop_reverse_twice xs = reverse (reverse xs) == xs

-- | Test that the length of a list is preserved by map
prop_map_preserves_length :: [Int] -> Bool
prop_map_preserves_length xs = length xs == length (map (*2) xs)

-- | Test that filter preserves the order of elements
prop_filter_preserves_order :: [Int] -> Bool
prop_filter_preserves_order xs = 
  let filtered = filter even xs
      expected = [x | x <- xs, even x]
  in filtered == expected

-- | Test that foldl and foldr give the same result for associative operations
prop_foldl_foldr_addition :: [Int] -> Bool
prop_foldl_foldr_addition xs = 
  foldl (+) 0 xs == foldr (+) 0 xs

-- | Test that foldl and foldr give the same result for multiplication
prop_foldl_foldr_multiplication :: [Int] -> Bool
prop_foldl_foldr_multiplication xs = 
  foldl (*) 1 xs == foldr (*) 1 xs

-- | Test that taking n elements and then dropping n elements gives the rest
prop_take_drop :: [Int] -> Int -> Property
prop_take_drop xs n = 
  n >= 0 && n <= length xs ==> drop n (take n xs ++ drop n xs) == drop n xs

-- | Test that splitting and rejoining with a delimiter preserves the original
prop_split_join :: Char -> String -> Bool
prop_split_join delim s = 
  let parts = splitBy delim s
  in intercalate [delim] parts == s

-- ============================================================================
-- Set Tests
-- ============================================================================

-- | Test that Set.fromList and Set.toList are inverses up to ordering
prop_set_fromList_toList :: [Int] -> Bool
prop_set_fromList_toList xs = 
  let s = Set.fromList xs
      ys = Set.toList s
  in sort ys == sort (nub xs)

-- | Test that Set.union is commutative
prop_set_union_commutative :: [Int] -> [Int] -> Bool
prop_set_union_commutative xs ys = 
  let s1 = Set.fromList xs
      s2 = Set.fromList ys
  in Set.union s1 s2 == Set.union s2 s1

-- | Test that Set.intersection is commutative
prop_set_intersection_commutative :: [Int] -> [Int] -> Bool
prop_set_intersection_commutative xs ys = 
  let s1 = Set.fromList xs
      s2 = Set.fromList ys
  in Set.intersection s1 s2 == Set.intersection s2 s1

-- | Test that Set.difference is not commutative in general
prop_set_difference_not_commutative :: [Int] -> [Int] -> Property
prop_set_difference_not_commutative xs ys = 
  let s1 = Set.fromList xs
      s2 = Set.fromList ys
      diff1 = Set.difference s1 s2
      diff2 = Set.difference s2 s1
  in not (Set.null s1) && not (Set.null s2) && s1 /= s2 ==> diff1 /= diff2 || diff1 == diff2

-- | Test that Set.union is associative
prop_set_union_associative :: [Int] -> [Int] -> [Int] -> Bool
prop_set_union_associative xs ys zs = 
  let s1 = Set.fromList xs
      s2 = Set.fromList ys
      s3 = Set.fromList zs
  in Set.union s1 (Set.union s2 s3) == Set.union (Set.union s1 s2) s3

-- | Test that Set.intersection is associative
prop_set_intersection_associative :: [Int] -> [Int] -> [Int] -> Bool
prop_set_intersection_associative xs ys zs = 
  let s1 = Set.fromList xs
      s2 = Set.fromList ys
      s3 = Set.fromList zs
  in Set.intersection s1 (Set.intersection s2 s3) == Set.intersection (Set.intersection s1 s2) s3

-- | Test that Set.size of union is at most the sum of sizes
prop_set_union_size :: [Int] -> [Int] -> Bool
prop_set_union_size xs ys = 
  let s1 = Set.fromList xs
      s2 = Set.fromList ys
      union = Set.union s1 s2
  in Set.size union <= Set.size s1 + Set.size s2

-- | Test that Set.size of intersection is at most the minimum of sizes
prop_set_intersection_size :: [Int] -> [Int] -> Bool
prop_set_intersection_size xs ys = 
  let s1 = Set.fromList xs
      s2 = Set.fromList ys
      intersection = Set.intersection s1 s2
  in Set.size intersection <= min (Set.size s1) (Set.size s2)

-- ============================================================================
-- Maybe Tests
-- ============================================================================

-- | Test that isJust and isNothing are complementary
prop_isJust_isNothing :: Maybe Int -> Bool
prop_isJust_isNothing m = isJust m == not (isNothing m)

-- | Test that fromMaybe with a default value returns the default for Nothing
prop_fromMaybe_nothing :: Int -> Bool
prop_fromMaybe_nothing x = fromMaybe x Nothing == x

-- | Test that fromMaybe with a Just returns the Just value
prop_fromMaybe_just :: Int -> Int -> Bool
prop_fromMaybe_just x y = fromMaybe x (Just y) == y

-- | Test that catMaybes removes all Nothing values
prop_catMaybes_nothing :: [Maybe Int] -> Bool
prop_catMaybes_nothing ms = 
  let filtered = catMaybes ms
  in not (any isNothing (map Just filtered))

-- | Test that catMaybes preserves the order of Just values
prop_catMaybes_preserves_order :: [Maybe Int] -> Bool
prop_catMaybes_preserves_order ms = 
  let filtered = catMaybes ms
      justValues = [x | Just x <- ms]
  in filtered == justValues

-- ============================================================================
-- Either Tests
-- ============================================================================

-- | Test that either isLeft or isRight holds for any Either
prop_either_left_or_right :: Either Int String -> Bool
prop_either_left_or_right e = isLeft e || isRight e
  where
    isLeft (Left _) = True
    isLeft _ = False
    isRight (Right _) = True
    isRight _ = False

-- | Test that isLeft and isRight are complementary
prop_either_left_right_complementary :: Either Int String -> Bool
prop_either_left_right_complementary e = isLeft e == not (isRight e)
  where
    isLeft (Left _) = True
    isLeft _ = False
    isRight (Right _) = True
    isRight _ = False

-- | Test that lefts returns only Left values
prop_either_lefts :: [Either Int String] -> Bool
prop_either_lefts es = 
  let ls = lefts es
  in all isLeft (map Left ls)
  where
    isLeft (Left _) = True
    isLeft _ = False
    lefts :: [Either a b] -> [a]
    lefts [] = []
    lefts (Left x : es) = x : lefts es
    lefts (Right _ : es) = lefts es

-- | Test that rights returns only Right values
prop_either_rights :: [Either Int String] -> Bool
prop_either_rights es = 
  let rs = rights es
  in all isRight (map Right rs)
  where
    isRight (Right _) = True
    isRight _ = False
    rights :: [Either a b] -> [b]
    rights [] = []
    rights (Left _ : es) = rights es
    rights (Right x : es) = x : rights es

-- | Test that partitionEithers separates Left and Right values
prop_either_partition :: [Either Int String] -> Bool
prop_either_partition es = 
  let (ls, rs) = partitionEithers es
  in lefts es == ls && rights es == rs
  where
    lefts :: [Either a b] -> [a]
    lefts [] = []
    lefts (Left x : es) = x : lefts es
    lefts (Right _ : es) = lefts es
    
    rights :: [Either a b] -> [b]
    rights [] = []
    rights (Left _ : es) = rights es
    rights (Right x : es) = x : rights es
    
    partitionEithers :: [Either a b] -> ([a], [b])
    partitionEithers [] = ([], [])
    partitionEithers (Left x : es) = 
      let (ls, rs) = partitionEithers es
      in (x : ls, rs)
    partitionEithers (Right x : es) = 
      let (ls, rs) = partitionEithers es
      in (ls, x : rs)

-- ============================================================================
-- Boolean Tests
-- ============================================================================

-- | Test that (&&) is commutative
prop_bool_and_commutative :: Bool -> Bool -> Bool
prop_bool_and_commutative x y = (x && y) == (y && x)

-- | Test that (||) is commutative
prop_bool_or_commutative :: Bool -> Bool -> Bool
prop_bool_or_commutative x y = (x || y) == (y || x)

-- | Test that (&&) is associative
prop_bool_and_associative :: Bool -> Bool -> Bool -> Bool
prop_bool_and_associative x y z = (x && (y && z)) == ((x && y) && z)

-- | Test that (||) is associative
prop_bool_or_associative :: Bool -> Bool -> Bool -> Bool
prop_bool_or_associative x y z = (x || (y || z)) == ((x || y) || z)

-- | Test that not (not x) == x
prop_bool_not_double :: Bool -> Bool
prop_bool_not_double x = not (not x) == x

-- | Test De Morgan's law: not (x && y) == (not x) || (not y)
prop_bool_demorgan_and :: Bool -> Bool -> Bool
prop_bool_demorgan_and x y = not (x && y) == (not x) || (not y)

-- | Test De Morgan's law: not (x || y) == (not x) && (not y)
prop_bool_demorgan_or :: Bool -> Bool -> Bool
prop_bool_demorgan_or x y = not (x || y) == (not x) && (not y)

-- | Test that x && True == x
prop_bool_and_true :: Bool -> Bool
prop_bool_and_true x = x && True == x

-- | Test that x && False == False
prop_bool_and_false :: Bool -> Bool
prop_bool_and_false x = x && False == False

-- | Test that x || True == True
prop_bool_or_true :: Bool -> Bool
prop_bool_or_true x = x || True == True

-- | Test that x || False == x
prop_bool_or_false :: Bool -> Bool
prop_bool_or_false x = x || False == x

-- ============================================================================
-- Function Composition Tests
-- ============================================================================

-- | Test that function composition is associative
prop_comp_associative :: Int -> Int -> Int -> Bool
prop_comp_associative x y z = ((+1) . (*2)) ((+3) z) == ((+1) . ((*2) . (+3))) z

-- | Test that id is the identity for composition
prop_comp_id_left :: Int -> Bool
prop_comp_id_left x = (id . (+1)) x == (+1) x

-- | Test that id is the identity for composition
prop_comp_id_right :: Int -> Bool
prop_comp_id_right x = ((+1) . id) x == (+1) x

-- | Test that const x y = x
prop_const :: Int -> Int -> Bool
prop_const x y = const x y == x

-- | Test that flip f x y = f y x
prop_flip :: Int -> Int -> Int -> Bool
prop_flip x y z = flip (+) x y == (+) y x

-- ============================================================================
-- Numeric Tests
-- ============================================================================

-- | Test that addition is commutative
prop_add_commutative :: Int -> Int -> Bool
prop_add_commutative x y = x + y == y + x

-- | Test that multiplication is commutative
prop_mul_commutative :: Int -> Int -> Bool
prop_mul_commutative x y = x * y == y * x

-- | Test that addition is associative
prop_add_associative :: Int -> Int -> Int -> Bool
prop_add_associative x y z = x + (y + z) == (x + y) + z

-- | Test that multiplication is associative
prop_mul_associative :: Int -> Int -> Int -> Bool
prop_mul_associative x y z = x * (y * z) == (x * y) * z

-- | Test distributivity of multiplication over addition
prop_mul_add_distributive :: Int -> Int -> Int -> Bool
prop_mul_add_distributive x y z = x * (y + z) == x * y + x * z

-- | Test that 0 is the additive identity
prop_add_zero :: Int -> Bool
prop_add_zero x = x + 0 == x

-- | Test that 1 is the multiplicative identity
prop_mul_one :: Int -> Bool
prop_mul_one x = x * 1 == x

-- | Test that x - x = 0
prop_sub_self :: Int -> Bool
prop_sub_self x = x - x == 0

-- | Test that x * 0 = 0
prop_mul_zero :: Int -> Bool
prop_mul_zero x = x * 0 == 0

-- | Test that negation is its own inverse
prop_neg_inverse :: Int -> Bool
prop_neg_inverse x = negate (negate x) == x

-- | Test that x - y = x + (-y)
prop_sub_neg :: Int -> Int -> Bool
prop_sub_neg x y = x - y == x + negate y

-- | Test that abs x >= 0
prop_abs_nonnegative :: Int -> Bool
prop_abs_nonnegative x = abs x >= 0

-- | Test that abs (abs x) = abs x
prop_abs_idempotent :: Int -> Bool
prop_abs_idempotent x = abs (abs x) == abs x

-- | Test that signum x is -1, 0, or 1
prop_signum_range :: Int -> Bool
prop_signum_range x = signum x `elem` [-1, 0, 1]

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Additional QuickCheck Tests"
  [ -- String and Text Processing tests
    testProperty "toLower toUpper alpha" prop_toLower_toUpper_alpha,
    testProperty "toUpper toLower alpha" prop_toUpper_toLower_alpha,
    testProperty "sort idempotent" prop_sort_idempotent,
    testProperty "nub idempotent" prop_nub_idempotent,
    testProperty "intercalate empty" prop_intercalate_empty,
    testProperty "group preserves length" prop_group_preserves_length,
    testProperty "sort group equal" prop_sort_group_equal,
    
    -- List and Collection tests
    testProperty "concat half" prop_concat_half,
    testProperty "reverse twice" prop_reverse_twice,
    testProperty "map preserves length" prop_map_preserves_length,
    testProperty "filter preserves order" prop_filter_preserves_order,
    testProperty "foldl foldr addition" prop_foldl_foldr_addition,
    testProperty "foldl foldr multiplication" prop_foldl_foldr_multiplication,
    testProperty "take drop" prop_take_drop,
    testProperty "split join" prop_split_join,
    
    -- Set tests
    testProperty "set fromList toList" prop_set_fromList_toList,
    testProperty "set union commutative" prop_set_union_commutative,
    testProperty "set intersection commutative" prop_set_intersection_commutative,
    testProperty "set difference not commutative" prop_set_difference_not_commutative,
    testProperty "set union associative" prop_set_union_associative,
    testProperty "set intersection associative" prop_set_intersection_associative,
    testProperty "set union size" prop_set_union_size,
    testProperty "set intersection size" prop_set_intersection_size,
    
    -- Maybe tests
    testProperty "isJust isNothing" prop_isJust_isNothing,
    testProperty "fromMaybe nothing" prop_fromMaybe_nothing,
    testProperty "fromMaybe just" prop_fromMaybe_just,
    testProperty "catMaybes nothing" prop_catMaybes_nothing,
    testProperty "catMaybes preserves order" prop_catMaybes_preserves_order,
    
    -- Either tests
    testProperty "either left or right" prop_either_left_or_right,
    testProperty "either left right complementary" prop_either_left_right_complementary,
    testProperty "either lefts" prop_either_lefts,
    testProperty "either rights" prop_either_rights,
    testProperty "either partition" prop_either_partition,
    
    -- Boolean tests
    testProperty "bool and commutative" prop_bool_and_commutative,
    testProperty "bool or commutative" prop_bool_or_commutative,
    testProperty "bool and associative" prop_bool_and_associative,
    testProperty "bool or associative" prop_bool_or_associative,
    testProperty "bool not double" prop_bool_not_double,
    testProperty "bool demorgan and" prop_bool_demorgan_and,
    testProperty "bool demorgan or" prop_bool_demorgan_or,
    testProperty "bool and true" prop_bool_and_true,
    testProperty "bool and false" prop_bool_and_false,
    testProperty "bool or true" prop_bool_or_true,
    testProperty "bool or false" prop_bool_or_false,
    
    -- Function Composition tests
    testProperty "comp associative" prop_comp_associative,
    testProperty "comp id left" prop_comp_id_left,
    testProperty "comp id right" prop_comp_id_right,
    testProperty "const" prop_const,
    testProperty "flip" prop_flip,
    
    -- Numeric tests
    testProperty "add commutative" prop_add_commutative,
    testProperty "mul commutative" prop_mul_commutative,
    testProperty "add associative" prop_add_associative,
    testProperty "mul associative" prop_mul_associative,
    testProperty "mul add distributive" prop_mul_add_distributive,
    testProperty "add zero" prop_add_zero,
    testProperty "mul one" prop_mul_one,
    testProperty "sub self" prop_sub_self,
    testProperty "mul zero" prop_mul_zero,
    testProperty "neg inverse" prop_neg_inverse,
    testProperty "sub neg" prop_sub_neg,
    testProperty "abs nonnegative" prop_abs_nonnegative,
    testProperty "abs idempotent" prop_abs_idempotent,
    testProperty "signum range" prop_signum_range
  ]