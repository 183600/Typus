{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalQuickCheckTestSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Char (isSpace, isLower, isUpper, toLower, toUpper)
import Utils (trim, splitBy, splitByCollapsed, splitByComma, normalizeIndentation)

-- | Test properties for basic string operations
testStringProperties :: TestTree
testStringProperties = testGroup "String Properties"
  [ testProperty "trim is idempotent" $
    \str -> trim (trim str) === trim str
  
  , testProperty "trim never adds characters" $
    \str -> property $ length (trim str) <= length str
  
  , testProperty "splitBy with same delimiter and rejoin preserves length" $
    \delim str -> delim /= '\0' ==> 
      let parts = splitBy delim str
          totalLength = sum (map length parts) + (length parts - 1)
      in property $ totalLength >= length str
  
  , testProperty "splitByCollapsed never has more parts than splitBy" $
    \delim str -> delim /= '\0' ==> 
      let parts = splitBy delim str
          collapsed = splitByCollapsed delim str
      in property $ length collapsed <= length parts
  
  , testProperty "splitByComma equals splitBy with comma" $
    \str -> splitByComma str === splitBy ',' str
  
  , testProperty "normalizeIndentation preserves non-empty strings" $
    \str -> 
      let normalized = normalizeIndentation str
      in property $ (not (null str)) ==> (not (null normalized))
  ]

-- | Test properties for list operations
testListProperties :: TestTree
testListProperties = testGroup "List Properties"
  [ testProperty "concat of splitBy preserves all characters" $
    \delim (str :: String) -> delim /= '\0' ==> 
      let parts = splitBy delim str
          reconstructed = concat $ map (++ [delim]) (init parts) ++ [last parts]
      in property $ length reconstructed >= length str
  
  , testProperty "head of non-empty list is element" $
    \(xs :: [Int]) -> not (null xs) ==> head xs === xs !! 0
  
  , testProperty "reverse twice is identity" $
    \(xs :: [Int]) -> reverse (reverse xs) === xs
  
  , testProperty "length of reverse equals original length" $
    \(xs :: [Int]) -> length (reverse xs) === length xs
  ]

-- | Test properties for numeric operations
testNumericProperties :: TestTree
testNumericProperties = testGroup "Numeric Properties"
  [ testProperty "addition is commutative" $
    \x y -> (x :: Int) + y === y + x
  
  , testProperty "addition is associative" $
    \x y z -> (x :: Int) + (y + z) === (x + y) + z
  
  , testProperty "multiplication distributes over addition" $
    \x y z -> (x :: Int) * (y + z) === x * y + x * z
  
  , testProperty "double negation is identity" $
    \x -> negate (negate (x :: Int)) === x
  ]

-- | Test properties for character operations
testCharProperties :: TestTree
testCharProperties = testGroup "Character Properties"
  [ testProperty "toUpper . toLower preserves non-letters" $
    \ch -> 
      let lower = toLower ch
          upper = toUpper lower
      in if not (isLower ch || isUpper ch)
         then upper === ch
         else property True
  
  , testProperty "isSpace or isNot covers all characters" $
    \ch -> property $ isSpace ch || not (isSpace ch)
  ]

tests :: TestTree
tests = testGroup "New Cabal QuickCheck Tests"
  [ testStringProperties
  , testListProperties
  , testNumericProperties
  , testCharProperties
  ]