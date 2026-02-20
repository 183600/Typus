{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.NewMathematicalPropertiesSpec where



import Test.Tasty.HUnit
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>))
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub, group, isInfixOf, isPrefixOf, isSuffixOf, delete, union, intersect, (\\))
import Data.Char (isSpace, isAlpha, isAlphaNum, toLower, toUpper, isDigit, isLetter, ord, chr)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Monad (replicateM)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
import Test.Tasty
import Test.Tasty.QuickCheck

import SourceLocation
import Utils

-- Helper generators for mathematical tests
genSmallInt :: Gen Int
genSmallInt = choose (-100, 100)

genPositiveInt :: Gen Int
genPositiveInt = choose (1, 100)

genNonNegativeInt :: Gen Int
genNonNegativeInt = choose (0, 100)

genFractional :: Gen Double
genFractional = choose (-1000.0, 1000.0)

genPositiveFractional :: Gen Double
genPositiveFractional = choose (0.1, 1000.0)

genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "!@#$%^&*()"

genString :: Gen String
genString = do
  len <- choose (0, 20)
  vectorOf len genChar

genList :: Gen a -> Gen [a]
genList gen = do
  len <- choose (0, 10)
  vectorOf len gen

-- Test properties for mathematical operations

-- Property 1: Addition is commutative
prop_additionCommutative :: Int -> Int -> Bool
prop_additionCommutative x y = x + y == y + x

-- Property 2: Addition is associative
prop_additionAssociative :: Int -> Int -> Int -> Bool
prop_additionAssociative x y z = (x + y) + z == x + (y + z)

-- Property 3: Multiplication is commutative
prop_multiplicationCommutative :: Int -> Int -> Bool
prop_multiplicationCommutative x y = x * y == y * x

-- Property 4: Multiplication is associative
prop_multiplicationAssociative :: Int -> Int -> Int -> Bool
prop_multiplicationAssociative x y z = (x * y) * z == x * (y * z)

-- Property 5: Distributive law
prop_distributiveLaw :: Int -> Int -> Int -> Bool
prop_distributiveLaw x y z = x * (y + z) == x * y + x * z

-- Property 6: Identity elements
prop_identityElements :: Int -> Bool
prop_identityElements x = x + 0 == x && x * 1 == x

-- Property 7: Additive inverse
prop_additiveInverse :: Int -> Bool
prop_additiveInverse x = x + (-x) == 0

-- Property 8: Multiplication by zero
prop_multiplicationByZero :: Int -> Bool
prop_multiplicationByZero x = x * 0 == 0

-- Property 9: Exponentiation laws
prop_exponentiationLaws :: Int -> Int -> Int -> Property
prop_exponentiationLaws base exp1 exp2 =
  base >= 0 && exp1 >= 0 && exp2 >= 0 && exp1 <= 10 && exp2 <= 10 ==> 
    base ^ (exp1 + exp2) == (base ^ exp1) * (base ^ exp2)

-- Property 10: Modulus properties
prop_modulusProperties :: Int -> Int -> Property
prop_modulusProperties x y =
  y /= 0 ==> 
    let mod1 = x `mod` y
        mod2 = (-x) `mod` y
    in mod1 >= 0 && mod1 < abs y && (x - mod1) `mod` y == 0

-- Property 11: Division properties
prop_divisionProperties :: Int -> Int -> Property
prop_divisionProperties x y =
  y /= 0 ==> 
    let (quot, rem) = x `quotRem` y
    in x == quot * y + rem && abs rem < abs y

-- Property 12: GCD properties
prop_gcdProperties :: Int -> Int -> Property
prop_gcdProperties x y =
  not (x == 0 && y == 0) ==> 
    let g = gcd x y
    in g `gcd` x == g && g `gcd` y == g && g >= 0

-- Property 13: LCM properties
prop_lcmProperties :: Int -> Int -> Property
prop_lcmProperties x y =
  x /= 0 && y /= 0 ==> 
    let l = lcm x y
    in l `mod` x == 0 && l `mod` y == 0 && l >= 0

-- Property 14: List concatenation is associative
prop_listConcatenationAssociative :: [Int] -> [Int] -> [Int] -> Bool
prop_listConcatenationAssociative xs ys zs = (xs ++ ys) ++ zs == xs ++ (ys ++ zs)

-- Property 15: List concatenation identity
prop_listConcatenationIdentity :: [Int] -> Bool
prop_listConcatenationIdentity xs = [] ++ xs == xs && xs ++ [] == xs

-- Property 16: List length properties
prop_listLengthProperties :: [Int] -> [Int] -> Bool
prop_listLengthProperties xs ys = length (xs ++ ys) == length xs + length ys

-- Property 17: List reverse properties
prop_listReverseProperties :: [Int] -> Bool
prop_listReverseProperties xs = reverse (reverse xs) == xs

-- Property 18: Set union properties
prop_setUnionProperties :: [Int] -> [Int] -> Bool
prop_setUnionProperties xs ys = 
  let setXs = Set.fromList xs
      setYs = Set.fromList ys
      unionSet = Set.union setXs setYs
  in Set.size unionSet <= Set.size setXs + Set.size setYs

-- Property 19: Set intersection properties
prop_setIntersectionProperties :: [Int] -> [Int] -> Bool
prop_setIntersectionProperties xs ys = 
  let setXs = Set.fromList xs
      setYs = Set.fromList ys
      intersectionSet = Set.intersection setXs setYs
  in Set.size intersectionSet <= min (Set.size setXs) (Set.size setYs)

-- Property 20: Map union properties
prop_mapUnionProperties :: [(String, Int)] -> [(String, Int)] -> Bool
prop_mapUnionProperties xs ys = 
  let mapXs = Map.fromList xs
      mapYs = Map.fromList ys
      unionMap = Map.union mapXs mapYs
  in Map.size unionMap >= max (Map.size mapXs) (Map.size mapYs)

-- Property 21: String concatenation is associative
prop_stringConcatenationAssociative :: String -> String -> String -> Bool
prop_stringConcatenationAssociative xs ys zs = (xs ++ ys) ++ zs == xs ++ (ys ++ zs)

-- Property 22: String length properties
prop_stringLengthProperties :: String -> String -> Bool
prop_stringLengthProperties xs ys = length (xs ++ ys) == length xs + length ys

-- Property 23: Character code properties
prop_characterCodeProperties :: Char -> Bool
prop_characterCodeProperties c = 
  let code = ord c
  in code >= 0 && code <= 0x10FFFF

-- Property 24: Fractional arithmetic properties
prop_fractionalArithmeticProperties :: Double -> Double -> Property
prop_fractionalArithmeticProperties x y =
  not (isNaN x || isNaN y || isInfinite x || isInfinite y) ==> 
    abs (x + y - (y + x)) < 1e-10 && abs (x * y - (y * x)) < 1e-10

-- Property 25: Square root properties
prop_squareRootProperties :: Double -> Property
prop_squareRootProperties x =
  x >= 0 && not (isNaN x || isInfinite x) ==> 
    let sqrtX = sqrt x
    in sqrtX * sqrtX - x < 1e-10

-- Property 26: Trigonometric properties
prop_trigonometricProperties :: Double -> Property
prop_trigonometricProperties x =
  not (isNaN x || isInfinite x) ==> 
    let sinX = sin x
        cosX = cos x
        tanX = tan x
    in abs (sinX * sinX + cosX * cosX - 1) < 1e-10 && 
       (abs cosX > 1e-10 ==> abs (tanX - sinX / cosX) < 1e-10)

-- Property 27: Logarithm properties
prop_logarithmProperties :: Double -> Property
prop_logarithmProperties x =
  x > 0 && not (isNaN x || isInfinite x) ==> 
    let logX = log x
        log10X = logBase 10 x
        log2X = logBase 2 x
    in exp logX - x < 1e-10 && 
       10 ** log10X - x < 1e-10 && 
       2 ** log2X - x < 1e-10

-- Property 28: Rounding properties
prop_roundingProperties :: Double -> Bool
prop_roundingProperties x =
  let rounded = round x
      floored = floor x
      ceiled = ceiling x
  in rounded - x <= 0.5 && x - rounded < 0.5 &&
     floored <= x && x < floored + 1 &&
     ceiled - 1 < x && x <= ceiled

-- Property 29: Absolute value properties
prop_absoluteValueProperties :: Int -> Bool
prop_absoluteValueProperties x = 
  let absX = abs x
  in absX >= 0 && (absX == x || absX == -x)

-- Property 30: Signum properties
prop_signumProperties :: Int -> Bool
prop_signumProperties x = 
  let signumX = signum x
  in (x > 0 && signumX == 1) ||
     (x == 0 && signumX == 0) ||
     (x < 0 && signumX == -1)

-- Helper functions
isNaN :: Double -> Bool
isNaN x = x /= x

isInfinite :: Double -> Bool
isInfinite x = abs x > 1/0

newMathematicalPropertiesTests :: TestTree
newMathematicalPropertiesTests = testGroup "New Mathematical Properties Tests"
  [ testProperties "Basic Arithmetic Properties"
    [ ("Addition is commutative", property prop_additionCommutative)
    , ("Addition is associative", property prop_additionAssociative)
    , ("Multiplication is commutative", property prop_multiplicationCommutative)
    , ("Multiplication is associative", property prop_multiplicationAssociative)
    , ("Distributive law", property prop_distributiveLaw)
    , ("Identity elements", property prop_identityElements)
    , ("Additive inverse", property prop_additiveInverse)
    , ("Multiplication by zero", property prop_multiplicationByZero)
    ]
  , testProperties "Number Theory Properties"
    [ ("Exponentiation laws", property prop_exponentiationLaws)
    , ("Modulus properties", property prop_modulusProperties)
    , ("Division properties", property prop_divisionProperties)
    , ("GCD properties", property prop_gcdProperties)
    , ("LCM properties", property prop_lcmProperties)
    ]
  , testProperties "Data Structure Properties"
    [ ("List concatenation is associative", property prop_listConcatenationAssociative)
    , ("List concatenation identity", property prop_listConcatenationIdentity)
    , ("List length properties", property prop_listLengthProperties)
    , ("List reverse properties", property prop_listReverseProperties)
    , ("Set union properties", property prop_setUnionProperties)
    , ("Set intersection properties", property prop_setIntersectionProperties)
    , ("Map union properties", property prop_mapUnionProperties)
    ]
  , testProperties "String and Character Properties"
    [ ("String concatenation is associative", property prop_stringConcatenationAssociative)
    , ("String length properties", property prop_stringLengthProperties)
    , ("Character code properties", property prop_characterCodeProperties)
    ]
  , testProperties "Floating Point Properties"
    [ ("Fractional arithmetic properties", property prop_fractionalArithmeticProperties)
    , ("Square root properties", property prop_squareRootProperties)
    , ("Trigonometric properties", property prop_trigonometricProperties)
    , ("Logarithm properties", property prop_logarithmProperties)
    , ("Rounding properties", property prop_roundingProperties)
    ]
  , testProperties "Integer Properties"
    [ ("Absolute value properties", property prop_absoluteValueProperties)
    , ("Signum properties", property prop_signumProperties)
    ]
  ]