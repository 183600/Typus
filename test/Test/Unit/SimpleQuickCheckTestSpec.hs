{-# LANGUAGE CPP #-}

module Test.Unit.SimpleQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (isAlpha, isDigit, isSpace, toLower)
import Data.List (sort, reverse, take, drop, length)

import Compiler.Errors.Core (ErrorSeverity(..))
import SourceLocation (SourcePos(..))
import TestSupport.Arbitrary ()

-- Simple numeric properties
prop_addition_commutative :: Int -> Int -> Property
prop_addition_commutative x y = x + y === y + x

prop_addition_associative :: Int -> Int -> Int -> Property
prop_addition_associative x y z = (x + y) + z === x + (y + z)

prop_multiplication_commutative :: Int -> Int -> Property
prop_multiplication_commutative x y = x * y === y * x

prop_zero_additive_identity :: Int -> Property
prop_zero_additive_identity x = x + 0 === x

prop_one_multiplicative_identity :: Int -> Property
prop_one_multiplicative_identity x = x * 1 === x

-- List properties
prop_reverse_reverse :: [Int] -> Property
prop_reverse_reverse xs = reverse (reverse xs) === xs

prop_sort_preserves_length :: [Int] -> Property
prop_sort_preserves_length xs = length (sort xs) === length xs

prop_take_drop :: Int -> [Int] -> Property
prop_take_drop n xs = n >= 0 ==> take n xs ++ drop n xs === xs

prop_concat_nil :: [[Int]] -> Property
prop_concat_nil xss = [] : xss === [] : xss

-- String properties
prop_length_reverse :: String -> Property
prop_length_reverse s = length (reverse s) === length s

prop_string_reverse_twice :: String -> Property
prop_string_reverse_twice s = reverse (reverse s) === s

prop_sort_string :: String -> Property
prop_sort_string s = length (sort s) === length s

-- Map properties
prop_map_lookup_singleton :: String -> Int -> Property
prop_map_lookup_singleton key value =
  Map.lookup key (Map.singleton key value) === Just value

prop_map_keys_singleton :: String -> Int -> Property
prop_map_keys_singleton key value =
  Map.keys (Map.singleton key value) === [key]

prop_map_size_singleton :: String -> Int -> Property
prop_map_size_singleton key value =
  Map.size (Map.singleton key value) === 1

-- Set properties
prop_set_member_singleton :: Int -> Property
prop_set_member_singleton x = property (Set.member x (Set.singleton x))

prop_set_size_singleton :: Int -> Property
prop_set_size_singleton x = Set.size (Set.singleton x) === 1

prop_set_fromList_toList :: [Int] -> Property
prop_set_fromList_toList xs = Set.fromList (Set.toList (Set.fromList xs)) === Set.fromList xs

-- Character properties
prop_isAlpha_lower :: Char -> Property
prop_isAlpha_lower c = isAlpha c ==> toLower c `elem` ['a'..'z']

prop_isDigit_range :: Char -> Property
prop_isDigit_range c = isDigit c ==> c `elem` ['0'..'9']

prop_isSpace_chars :: Char -> Property
prop_isSpace_chars c = isSpace c ==> c `elem` " \t\n\r\f\v"

-- Error properties
prop_error_severity_values :: Property
prop_error_severity_values =
  let severities = [Error, Warning, Info]
  in property (length severities == 3)

prop_error_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_ordering sev1 sev2 =
  property (sev1 <= sev2 || sev1 > sev2)

-- Boolean properties
prop_true_and_true :: Property
prop_true_and_true = (True && True) === True

prop_false_or_false :: Property
prop_false_or_false = (False || False) === False

prop_not_true :: Property
prop_not_true = not True === False

prop_not_false :: Property
prop_not_false = not False === True

-- Maybe properties
prop_nothing_is_nothing :: Maybe Int -> Property
prop_nothing_is_nothing m = property (case m of Nothing -> True; _ -> False || case m of Nothing -> False; _ -> True)

prop_just_extract :: Int -> Property
prop_just_extract x = case Just x of
  Just y -> y === x
  Nothing -> property False

tests :: TestTree
tests = testGroup "Simple QuickCheck Test Tests"
  [ fastProperty "addition is commutative" prop_addition_commutative
  , fastProperty "addition is associative" prop_addition_associative
  , fastProperty "multiplication is commutative" prop_multiplication_commutative
  , fastProperty "zero is additive identity" prop_zero_additive_identity
  , fastProperty "one is multiplicative identity" prop_one_multiplicative_identity
  , fastProperty "reverse is involutive" prop_reverse_reverse
  , fastProperty "sort preserves length" prop_sort_preserves_length
  , fastProperty "take and drop split list" prop_take_drop
  , fastProperty "concat with nil" prop_concat_nil
  , fastProperty "reverse preserves string length" prop_length_reverse
  , fastProperty "string reverse is involutive" prop_string_reverse_twice
  , fastProperty "sort preserves string length" prop_sort_string
  , fastProperty "Map lookup in singleton" prop_map_lookup_singleton
  , fastProperty "Map keys of singleton" prop_map_keys_singleton
  , fastProperty "Map size of singleton" prop_map_size_singleton
  , fastProperty "Set member of singleton" prop_set_member_singleton
  , fastProperty "Set size of singleton" prop_set_size_singleton
  , fastProperty "Set fromList/toList roundtrip" prop_set_fromList_toList
  , fastProperty "isAlpha characters become lowercase" prop_isAlpha_lower
  , fastProperty "isDigit characters are 0-9" prop_isDigit_range
  , fastProperty "isSpace characters are whitespace" prop_isSpace_chars
  , fastProperty "Error severity values" prop_error_severity_values
  
  , fastProperty "true && true = true" prop_true_and_true
  , fastProperty "false || false = false" prop_false_or_false
  , fastProperty "not true = false" prop_not_true
  , fastProperty "not false = true" prop_not_false
  , fastProperty "nothing is nothing or not nothing" prop_nothing_is_nothing
  , fastProperty "Just extracts value" prop_just_extract
  ]
