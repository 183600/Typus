{-# OPTIONS_GHC -Wno-type-defaults #-}
module Test.Unit.MathOperationsQuickCheckSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

import TestSupport.QuickCheck (fastProperty)
import Data.List (sort)

-- Properties for basic arithmetic operations
prop_addition_commutative :: Int -> Int -> Bool
prop_addition_commutative x y = x + y == y + x

prop_addition_associative :: Int -> Int -> Int -> Bool
prop_addition_associative x y z = (x + y) + z == x + (y + z)

prop_addition_identity :: Int -> Bool
prop_addition_identity x = x + 0 == x

prop_multiplication_commutative :: Int -> Int -> Bool
prop_multiplication_commutative x y = x * y == y * x

prop_multiplication_associative :: Int -> Int -> Int -> Bool
prop_multiplication_associative x y z = (x * y) * z == x * (y * z)

prop_multiplication_identity :: Int -> Bool
prop_multiplication_identity x = x * 1 == x

prop_multiplication_zero :: Int -> Bool
prop_multiplication_zero x = x * 0 == 0

-- Properties for distributive law
prop_distributive_law :: Int -> Int -> Int -> Bool
prop_distributive_law x y z = x * (y + z) == x * y + x * z

-- Properties for subtraction
prop_subtraction_identity :: Int -> Bool
prop_subtraction_identity x = x - 0 == x

prop_subtraction_self :: Int -> Bool
prop_subtraction_self x = x - x == 0

-- Properties for division (avoiding division by zero)
prop_division_identity :: Int -> Property
prop_division_identity x = x /= 0 ==> div x 1 == x

prop_division_by_self :: Int -> Property
prop_division_by_self x = x /= 0 ==> div x x == 1

-- Properties for modulo operation
prop_modulo_non_negative :: Int -> Property
prop_modulo_non_negative x = x /= 0 ==> mod x 1 >= 0

prop_modulo_less_than_divisor :: Int -> Property
prop_modulo_less_than_divisor x = x /= 0 ==> mod x 2 < 2

-- Properties for exponentiation (with small numbers to avoid overflow)
prop_exponentiation_zero :: Int -> Bool
prop_exponentiation_zero x = x ^ 0 == 1

prop_exponentiation_one :: Int -> Bool
prop_exponentiation_one x = x ^ 1 == x

-- Properties for absolute value
prop_abs_non_negative :: Int -> Bool
prop_abs_non_negative x = abs x >= 0

prop_abs_idempotent :: Int -> Bool
prop_abs_idempotent x = abs (abs x) == abs x

-- Properties for signum
prop_signum_abs :: Int -> Bool
prop_signum_abs x = signum x * abs x == x

-- Properties for minimum and maximum
prop_min_commutes :: Int -> Int -> Bool
prop_min_commutes x y = min x y == min y x

prop_max_commutes :: Int -> Int -> Bool
prop_max_commutes x y = max x y == max y x

prop_min_leq :: Int -> Int -> Bool
prop_min_leq x y = min x y <= x && min x y <= y

prop_max_geq :: Int -> Int -> Bool
prop_max_geq x y = max x y >= x && max x y >= y

-- Properties for even and odd
prop_even_add_even :: Int -> Int -> Property
prop_even_add_even x y = even x && even y ==> even (x + y)

prop_odd_add_odd :: Int -> Int -> Property
prop_odd_add_odd x y = odd x && odd y ==> even (x + y)

prop_even_add_odd :: Int -> Int -> Property
prop_even_add_odd x y = even x && odd y ==> odd (x + y)

-- Properties for gcd
prop_gcd_non_negative :: Int -> Int -> Bool
prop_gcd_non_negative x y = gcd x y >= 0

prop_gcd_commutative :: Int -> Int -> Bool
prop_gcd_commutative x y = gcd x y == gcd y x

prop_gcd_divides_both :: Int -> Int -> Bool
prop_gcd_divides_both x y = x `mod` gcd x y == 0 && y `mod` gcd x y == 0

-- Properties for lcm
prop_lcm_non_negative :: Int -> Int -> Bool
prop_lcm_non_negative x y = lcm x y >= 0

prop_lcm_commutative :: Int -> Int -> Bool
prop_lcm_commutative x y = lcm x y == lcm y x

-- Properties for sorting
prop_sort_length :: [Int] -> Bool
prop_sort_length xs = length (sort xs) == length xs

prop_sort_sorted :: [Int] -> Bool
prop_sort_sorted xs = isSorted (sort xs)
  where
    isSorted [] = True
    isSorted [_] = True
    isSorted (x:y:xs') = x <= y && isSorted (y:xs')

prop_sort_permutation :: [Int] -> Bool
prop_sort_permutation xs = sort xs `isPermutationOf` xs
  where
    isPermutationOf ys zs = sort ys == sort zs

tests :: TestTree
tests = testGroup "Test.Unit.MathOperationsQuickCheckSpec Tests"
  [ fastProperty "addition commutative" prop_addition_commutative
  , fastProperty "addition associative" prop_addition_associative
  , fastProperty "addition identity" prop_addition_identity
  , fastProperty "multiplication commutative" prop_multiplication_commutative
  , fastProperty "multiplication associative" prop_multiplication_associative
  , fastProperty "multiplication identity" prop_multiplication_identity
  , fastProperty "multiplication zero" prop_multiplication_zero
  , fastProperty "distributive law" prop_distributive_law
  , fastProperty "subtraction identity" prop_subtraction_identity
  , fastProperty "subtraction self" prop_subtraction_self
  , fastProperty "division identity" prop_division_identity
  , fastProperty "division by self" prop_division_by_self
  , fastProperty "modulo non negative" prop_modulo_non_negative
  , fastProperty "modulo less than divisor" prop_modulo_less_than_divisor
  , fastProperty "exponentiation zero" prop_exponentiation_zero
  , fastProperty "exponentiation one" prop_exponentiation_one
  , fastProperty "abs non negative" prop_abs_non_negative
  , fastProperty "abs idempotent" prop_abs_idempotent
  , fastProperty "signum abs" prop_signum_abs
  , fastProperty "min commutes" prop_min_commutes
  , fastProperty "max commutes" prop_max_commutes
  , fastProperty "min leq" prop_min_leq
  , fastProperty "max geq" prop_max_geq
  , fastProperty "even add even" prop_even_add_even
  , fastProperty "odd add odd" prop_odd_add_odd
  , fastProperty "even add odd" prop_even_add_odd
  , fastProperty "gcd non negative" prop_gcd_non_negative
  , fastProperty "gcd commutative" prop_gcd_commutative
  , fastProperty "gcd divides both" prop_gcd_divides_both
  , fastProperty "lcm non negative" prop_lcm_non_negative
  , fastProperty "lcm commutative" prop_lcm_commutative
  , fastProperty "sort length" prop_sort_length
  , fastProperty "sort sorted" prop_sort_sorted
  , fastProperty "sort permutation" prop_sort_permutation
  ]