module Test.Unit.MathematicalPropertiesQuickCheckSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

import Data.List (sort)

-- | 测试整数算术属性
prop_addition_commutative :: Int -> Int -> Property
prop_addition_commutative x y =
  x + y === y + x

prop_addition_associative :: Int -> Int -> Int -> Property
prop_addition_associative x y z =
  (x + y) + z === x + (y + z)

prop_addition_identity :: Int -> Property
prop_addition_identity x =
  x + 0 === x

prop_multiplication_commutative :: Int -> Int -> Property
prop_multiplication_commutative x y =
  x * y === y * x

prop_multiplication_associative :: Int -> Int -> Int -> Property
prop_multiplication_associative x y z =
  (x * y) * z === x * (y * z)

prop_multiplication_identity :: Int -> Property
prop_multiplication_identity x =
  x * 1 === x

prop_distributive_law :: Int -> Int -> Int -> Property
prop_distributive_law x y z =
  x * (y + z) === (x * y) + (x * z)

-- | 测试整数除法属性
prop_division_monotonic :: Int -> Int -> Int -> Property
prop_division_monotonic x y z =
  x > 0 && y > 0 && z > 0 ==> 
  x `div` (y + z) <= (x `div` y) + (x `div` z)

prop_modulo_properties :: Int -> Int -> Property
prop_modulo_properties x y =
  y /= 0 ==> 
  let r = x `mod` y
  in r >= 0 && r < abs y

prop_div_mod_relation :: Int -> Int -> Property
prop_div_mod_relation x y =
  y /= 0 ==> 
  let (q, r) = x `divMod` y
  in (x === q * y + r) .&&. (r >= 0) .&&. (r < abs y)

-- | 测试幂运算属性
prop_power_identity :: Int -> Property
prop_power_identity x =
  x >= 0 ==> x ^ 1 === x

prop_power_zero :: Int -> Property
prop_power_zero x =
  x /= 0 ==> x ^ 0 === 1

prop_power_associative :: Int -> Int -> Int -> Property
prop_power_associative x y z =
  x >= 0 && y >= 0 && z >= 0 ==> 
  (x ^ y) ^ z === x ^ (y * z)

-- | 测试最大公约数和最小公倍数
prop_gcd_properties :: Int -> Int -> Property
prop_gcd_properties x y =
  let g = gcd x y
      divisors = filter (\d -> d `divides` x && d `divides` y) [1..min (abs x) (abs y)]
  in property (g `divides` x && g `divides` y) .&&.
     property (forallPred divisors (\d -> d <= g))

prop_lcm_properties :: Int -> Int -> Property
prop_lcm_properties x y =
  let l = lcm x y
      multiples = filter (\m -> x `divides` m && y `divides` m) [max (abs x) (abs y)..abs x * abs y]
  in property (x `divides` l && y `divides` l) .&&.
     property (forallPred multiples (\m -> l <= m))

prop_gcd_lcm_relation :: Int -> Int -> Property
prop_gcd_lcm_relation x y =
  x /= 0 && y /= 0 ==> 
  abs (x * y) === gcd x y * lcm x y

-- | 测试奇偶性属性
prop_even_odd_properties :: Int -> Property
prop_even_odd_properties x =
  property ((even x && not (odd x)) || (odd x && not (even x)))

prop_even_addition :: Int -> Int -> Property
prop_even_addition x y =
  even x && even y ==> even (x + y)

prop_odd_addition :: Int -> Int -> Property
prop_odd_addition x y =
  odd x && odd y ==> even (x + y)

prop_even_odd_addition :: Int -> Int -> Property
prop_even_odd_addition x y =
  even x && odd y ==> odd (x + y)

prop_even_multiplication :: Int -> Int -> Property
prop_even_multiplication x y =
  even x || even y ==> even (x * y)

prop_odd_multiplication :: Int -> Int -> Property
prop_odd_multiplication x y =
  odd x && odd y ==> odd (x * y)

-- | 测试绝对值属性
prop_abs_nonnegative :: Int -> Property
prop_abs_nonnegative x =
  property (abs x >= 0)

prop_abs_idempotent :: Int -> Property
prop_abs_idempotent x =
  abs (abs x) === abs x

prop_abs_multiplicative :: Int -> Int -> Property
prop_abs_multiplicative x y =
  abs (x * y) === abs x * abs y

-- | 测试符号函数属性
prop_signum_properties :: Int -> Property
prop_signum_properties x =
  let s = signum x
  in ((x > 0) ==> (s === 1)) .&&.
     ((x == 0) ==> (s === 0)) .&&.
     ((x < 0) ==> (s === -1))

prop_signum_multiplicative :: Int -> Int -> Property
prop_signum_multiplicative x y =
  signum (x * y) === signum x * signum y

-- | 辅助函数
divides :: Int -> Int -> Bool
x `divides` y = y `mod` x == 0

forallPred :: [a] -> (a -> Bool) -> Bool
forallPred xs p = all p xs

tests :: TestTree
tests = testGroup "Mathematical Properties QuickCheck Tests"
  [ testProperty "addition commutative" prop_addition_commutative
  , testProperty "addition associative" prop_addition_associative
  , testProperty "addition identity" prop_addition_identity
  , testProperty "multiplication commutative" prop_multiplication_commutative
  , testProperty "multiplication associative" prop_multiplication_associative
  , testProperty "multiplication identity" prop_multiplication_identity
  , testProperty "distributive law" prop_distributive_law
  , testProperty "division monotonic" prop_division_monotonic
  , testProperty "modulo properties" prop_modulo_properties
  , testProperty "div mod relation" prop_div_mod_relation
  , testProperty "power identity" prop_power_identity
  , testProperty "power zero" prop_power_zero
  , testProperty "power associative" prop_power_associative
  , testProperty "gcd properties" prop_gcd_properties
  , testProperty "lcm properties" prop_lcm_properties
  , testProperty "gcd lcm relation" prop_gcd_lcm_relation
  , testProperty "even odd properties" prop_even_odd_properties
  , testProperty "even addition" prop_even_addition
  , testProperty "odd addition" prop_odd_addition
  , testProperty "even odd addition" prop_even_odd_addition
  , testProperty "even multiplication" prop_even_multiplication
  , testProperty "odd multiplication" prop_odd_multiplication
  , testProperty "abs nonnegative" prop_abs_nonnegative
  , testProperty "abs idempotent" prop_abs_idempotent
  , testProperty "abs multiplicative" prop_abs_multiplicative
  , testProperty "signum properties" prop_signum_properties
  , testProperty "signum multiplicative" prop_signum_multiplicative
  ]