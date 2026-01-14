{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CoreUtilsPropertiesSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.List (sort, nub, group, intercalate)
import Data.Char (toLower, toUpper, isSpace)
import qualified Data.Set as Set

-- Test basic string manipulation properties
tests :: TestTree
tests = testGroup "Core Utils Properties Tests"
  [ testGroup "String manipulation properties"
    [ testProperty "sort is idempotent" $
        \xs -> sort (sort xs) === sort (xs :: String)
    
    , testProperty "sort preserves length" $
        \xs -> length (sort xs) === length (xs :: String)
    
    , testProperty "nub removes duplicates" $
        \xs -> length (nub xs) <= length (xs :: String)
    
    , testProperty "group preserves total length" $
        \xs -> sum (map length (group xs)) === length (xs :: String)
    
    , testProperty "intercalate with empty string concatenates" $
        \xs -> intercalate "" xs === concat xs
    
    , testProperty "toLower . toUpper preserves length" $
        \xs -> length (map toLower (map toUpper xs)) === length (xs :: String)
    
    , testProperty "toUpper . toLower preserves length" $
        \xs -> length (map toUpper (map toLower xs)) === length (xs :: String)
    
    , testProperty "filter isSpace removes all whitespace" $
        \xs -> all (not . isSpace) (filter (not . isSpace) xs)
    
    , testProperty "words . unwords preserves non-space words" $
        \xs -> words (unwords xs) === xs
    
    , testProperty "lines . unlines preserves non-empty lines" $
        \xs -> lines (unlines xs) === xs
    ]
  
  , testGroup "Set properties"
    [ testProperty "Set.fromList . Set.toList is idempotent" $
        \(xs :: [Int]) -> Set.fromList (Set.toList (Set.fromList xs)) === Set.fromList xs
    
    , testProperty "Set.size is never negative" $
        \(xs :: [Int]) -> Set.size (Set.fromList xs) >= 0
    
    , testProperty "Set.member after insertion" $
        \(x :: Int) (xs :: [Int]) -> Set.member x (Set.insert x (Set.fromList xs))
    
    , testProperty "Set.delete removes element" $
        \(x :: Int) (xs :: [Int]) -> not (Set.member x (Set.delete x (Set.fromList xs)))
    
    , testProperty "Set.union contains both sets" $
        \(xs :: [Int]) (ys :: [Int]) -> all (`Set.member` Set.union (Set.fromList xs) (Set.fromList ys)) xs
    ]
  
  , testGroup "List properties"
    [ testProperty "reverse . reverse is identity" $
        \xs -> reverse (reverse xs) === (xs :: [Int])
    
    , testProperty "concat . singleton is identity" $
        \x -> concat [x] === (x :: String)
    
    , testProperty "map preserves length" $
        \xs -> length (map (+1) xs) === length (xs :: [Int])
    
    , testProperty "filter reduces length" $
        \xs -> length (filter even xs) <= length (xs :: [Int])
    
    , testProperty "foldr with cons reconstructs list" $
        \xs -> foldr (:) [] xs === (xs :: [Int])
    ]
  ]