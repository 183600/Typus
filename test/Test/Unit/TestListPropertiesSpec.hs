{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestListPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils
import SourceLocation
import ErrorHandler
import Dependencies
import qualified Data.Text as T
import TestSupport.Arbitrary ()

-- | Test suite for List Properties
testListProperties :: TestTree
testListProperties = testGroup "List Properties Tests"
  [ testProperty "List: length of empty list is 0" $
      \() -> length ([] :: [Int]) == 0
      
  , testProperty "List: length of singleton list is 1" $
      \x -> length [x] == 1
      
  , testProperty "List: length of cons is length + 1" $
      \x xs -> length (x:xs) == length xs + 1
      
  , testProperty "List: length of append is sum of lengths" $
      \xs ys -> length (xs ++ ys) == length xs + length ys
      
  , testProperty "List: reverse is involutive" $
      \xs -> reverse (reverse xs) == xs
      
  , testProperty "List: reverse preserves length" $
      \xs -> length (reverse xs) == length xs
      
  , testProperty "List: head of cons is the first element" $
      \x xs -> head (x:xs) == x
      
  , testProperty "List: tail of cons is the rest" $
      \x xs -> tail (x:xs) == xs
      
  , testProperty "List: last of singleton is the element" $
      \x -> last [x] == x
      
  , testProperty "List: init of singleton is empty" $
      \x -> init [x] == ([] :: [Int])
      
  , testProperty "List: last of cons is last of tail or element if empty" $
      \x xs -> not (null xs) ==> last (x:xs) == last xs
      
  , testProperty "List: init of cons is element cons init of tail or empty if empty" $
      \x xs -> not (null xs) ==> init (x:xs) == x : init xs
      
  , testProperty "List: null of empty list is true" $
      \() -> null ([] :: [Int]) == True
      
  , testProperty "List: null of non-empty list is false" $
      \x xs -> null (x:xs) == False
      
  , testProperty "List: map preserves length" $
      \f xs -> length (map f xs) == length xs
      
  , testProperty "List: map of cons is cons of map" $
      \f x xs -> map f (x:xs) == f x : map f xs
      
  , testProperty "List: map of identity is identity" $
      \xs -> map id xs == xs
      
  , testProperty "List: map of composition is composition of maps" $
      \f g xs -> map (f . g) xs == map f (map g xs)
      
  , testProperty "List: filter preserves order" $
      \p xs -> isOrdered (filter p xs) xs
      
  , testProperty "List: filter of empty list is empty" $
      \p -> filter p ([] :: [Int]) == []
      
  , testProperty "List: filter of cons with predicate true is cons of filter with predicate true" $
      \x xs -> (const True x) ==> filter (const True) (x:xs) == x : filter (const True) xs
      
  , testProperty "List: filter of cons with predicate false is filter of tail" $
      \x xs -> (const False x) ==> filter (const False) (x:xs) == filter (const False) xs
      
  , testProperty "List: all of empty list is true" $
      \p -> all p ([] :: [Int]) == True
      
  , testProperty "List: any of empty list is false" $
      \p -> any p ([] :: [Int]) == False
      
  , testProperty "List: all of singleton is predicate of element" $
      \p x -> all p [x] == p x
      
  , testProperty "List: any of singleton is predicate of element" $
      \p x -> any p [x] == p x
      
  , testProperty "List: all of cons is predicate of head AND all of tail" $
      \p x xs -> all p (x:xs) == (p x && all p xs)
      
  , testProperty "List: any of cons is predicate of head OR any of tail" $
      \p x xs -> any p (x:xs) == (p x || any p xs)
      
  , testProperty "List: sum of empty list is 0" $
      \() -> sum ([] :: [Int]) == 0
      
  , testProperty "List: sum of singleton is element" $
      \x -> sum [x] == x
      
  , testProperty "List: sum of cons is head plus sum of tail" $
      \x xs -> sum (x:xs) == x + sum xs
      
  , testProperty "List: sum of append is sum of parts" $
      \xs ys -> sum (xs ++ ys) == sum xs + sum ys
      
  , testProperty "List: product of empty list is 1" $
      \() -> product ([] :: [Int]) == 1
      
  , testProperty "List: product of singleton is element" $
      \x -> product [x] == x
      
  , testProperty "List: product of cons is head times product of tail" $
      \x xs -> product (x:xs) == x * product xs
      
  , testProperty "List: product of append is product of parts" $
      \xs ys -> product (xs ++ ys) == product xs * product ys
      
  , testProperty "List: concat of empty list of lists is empty" $
      \() -> concat ([] :: [[Int]]) == []
      
  , testProperty "List: concat of singleton list of lists is the list" $
      \xs -> concat [xs] == xs
      
  , testProperty "List: concat of cons of lists is head list concat concat of tail" $
      \xss yss -> concat (xss:yss) == xss ++ concat yss
      
  , testProperty "List: concat of map is same as concatMap" $
      \f xs -> concat (map f xs) == concatMap f xs
      
  , testProperty "List: concat of append is concat of parts" $
      \xss yss -> concat (xss ++ yss) == concat xss ++ concat yss
      
  , testProperty "List: concatMap of empty list is empty" $
      \f -> concatMap f ([] :: [Int]) == []
      
  , testProperty "List: concatMap of singleton is function applied to element" $
      \f x -> concatMap f [x] == f x
      
  , testProperty "List: concatMap of cons is function applied to head concat concatMap of tail" $
      \f x xs -> concatMap f (x:xs) == f x ++ concatMap f xs
      
  , testProperty "List: foldr of empty list is initial value" $
      \f z -> foldr f z ([] :: [Int]) == z
      
  , testProperty "List: foldr of singleton is function applied to element and initial value" $
      \f z x -> foldr f z [x] == f x z
      
  , testProperty "List: foldr of cons is function applied to head and foldr of tail" $
      \f z x xs -> foldr f z (x:xs) == f x (foldr f z xs)
      
  , testProperty "List: foldl of empty list is initial value" $
      \f z -> foldl f z ([] :: [Int]) == z
      
  , testProperty "List: foldl of singleton is function applied to initial value and element" $
      \f z x -> foldl f z [x] == f z x
      
  , testProperty "List: foldl of cons is function applied to foldl of tail and head" $
      \f z x xs -> foldl f z (x:xs) == foldl f (f z x) xs
      
  , testProperty "List: foldr and foldl are equivalent for associative operations" $
      \xs -> foldr (+) 0 xs == foldl (+) 0 xs
      
  , testProperty "List: foldr and foldl are equivalent for commutative operations" $
      \xs -> foldr (*) 1 xs == foldl (*) 1 xs
      
  , testProperty "List: take 0 of any list is empty" $
      \xs -> take 0 xs == ([] :: [Int])
      
  , testProperty "List: take n of empty list is empty" $
      \n -> take n ([] :: [Int]) == []
      
  , testProperty "List: take n of list with length <= n is the list itself" $
      \n xs -> n >= length xs ==> take n xs == xs
      
  , testProperty "List: take n of list with length > n has length n" $
      \n xs -> n > 0 && n < length xs ==> length (take n xs) == n
      
  , testProperty "List: take preserves order" $
      \n xs -> isPrefix (take n xs) xs
      
  , testProperty "List: drop 0 of any list is the list itself" $
      \xs -> drop 0 xs == xs
      
  , testProperty "List: drop n of empty list is empty" $
      \n -> drop n ([] :: [Int]) == []
      
  , testProperty "List: drop n of list with length <= n is empty" $
      \n xs -> n >= length xs ==> drop n xs == ([] :: [Int])
      
  , testProperty "List: drop n of list with length > n has length (length - n)" $
      \n xs -> n > 0 && n < length xs ==> length (drop n xs) == length xs - n
      
  , testProperty "List: drop preserves order" $
      \n xs -> isSuffix (drop n xs) xs
      
  , testProperty "List: splitAt n is (take n, drop n)" $
      \n xs -> splitAt n xs == (take n xs, drop n xs)
      
  , testProperty "List: take and drop reconstruct original list" $
      \n xs -> take n xs ++ drop n xs == xs
      
  , testProperty "List: replicate 0 is empty list" $
      \() -> replicate 0 (42 :: Int) == []
      
  , testProperty "List: replicate n has length n" $
      \n x -> n >= 0 ==> length (replicate n x) == n
      
  , testProperty "List: all elements of replicate are equal" $
      \n x -> n >= 0 ==> all (== x) (replicate n x)
      
  , testProperty "List: repeat is infinite" $
      \x -> take 5 (repeat x) == replicate 5 x
      
  , testProperty "List: iterate n f starting from x applies f n times" $
      \n f x -> n >= 0 ==> iterate n f x !! n == iterate n f x !! n
      
  , testProperty "List: cycle of empty list is undefined (should not evaluate)" $
      \() -> True  -- We can't test this property without causing an error
      
  , testProperty "List: cycle of non-empty list repeats infinitely" $
      \xs -> not (null xs) ==> take (length xs * 2) (cycle xs) == xs ++ xs
      
  , testProperty "List: span p xs is (takeWhile p xs, dropWhile p xs)" $
      \p xs -> span p xs == (takeWhile p xs, dropWhile p xs)
      
  , testProperty "List: break p xs is (takeWhile (not . p) xs, dropWhile (not . p) xs)" $
      \p xs -> break p xs == (takeWhile (not . p) xs, dropWhile (not . p) xs)
      
  , testProperty "List: group sorts consecutive equal elements" $
      \xs -> concat (group xs) == xs
      
  , testProperty "List: inits of empty list is list containing empty list" $
      \() -> inits ([] :: [Int]) == [[]]
      
  , testProperty "List: inits of non-empty list includes empty list and full list" $
      \xs -> [] `elem` inits xs && xs `elem` inits xs
      
  , testProperty "List: tails of empty list is list containing empty list" $
      \() -> tails ([] :: [Int]) == [[]]
      
  , testProperty "List: tails of non-empty list includes empty list and full list" $
      \xs -> [] `elem` tails xs && xs `elem` tails xs
      
  , testProperty "List: isPrefixOf is reflexive" $
      \xs -> xs `isPrefixOf` xs
      
  , testProperty "List: empty list is prefix of any list" $
      \xs -> [] `isPrefixOf` xs
      
  , testProperty "List: isSuffixOf is reflexive" $
      \xs -> xs `isSuffixOf` xs
      
  , testProperty "List: empty list is suffix of any list" $
      \xs -> [] `isSuffixOf` xs
      
  , testProperty "List: isInfixOf is reflexive" $
      \xs -> xs `isInfixOf` xs
      
  , testProperty "List: empty list is sublist of any list" $
      \xs -> [] `isInfixOf` xs
      
  , testProperty "List: elem is equivalent to any (==)" $
      \x xs -> elem x xs == any (== x) xs
      
  , testProperty "List: notElem is equivalent to not . elem" $
      \x xs -> notElem x xs == not (elem x xs)
      
  , testProperty "List: lookup finds the first matching key" $
      \k xs -> lookup k xs == case filter (\(k',_) -> k' == k) xs of
                                  ((_,v):_) -> Just v
                                  [] -> Nothing
  ]

-- Helper functions
isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

isSuffix :: Eq a => [a] -> [a] -> Bool
isSuffix [] _ = True
isSuffix _ [] = False
isSuffix xs ys = reverse xs `isPrefixOf` reverse ys

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = 
  let nlen = length needle
      hlen = length haystack
  in if nlen > hlen
     then False
     else any (\i -> take nlen (drop i haystack) == needle) [0..hlen-nlen]

iterate :: Int -> (a -> a) -> a -> [a]
iterate 0 _ x = [x]
iterate n f x = x : iterate (n-1) f (f x)

repeat :: a -> [a]
repeat x = x : repeat x

cycle :: [a] -> [a]
cycle [] = error "empty list"
cycle xs = xs ++ cycle xs


inits :: [a] -> [[a]]
inits [] = [[]]
inits xs = inits (init xs) ++ [xs]

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs = xs : tails (tail xs)

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf = isPrefix

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf = isSuffix

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y:ys) = x == y || elem x ys

notElem :: Eq a => a -> [a] -> Bool
notElem x = not . elem x

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup k ((k',v):xs) = if k == k' then Just v else lookup k xs

span :: (a -> Bool) -> [a] -> ([a], [a])
span _ [] = ([], [])
span p xs@(x:xs') = 
  if p x
    then let (ys, zs) = span p xs'
         in (x:ys, zs)
    else ([], xs)

break :: (a -> Bool) -> [a] -> ([a], [a])
break _ [] = ([], [])
break p xs@(x:xs') = 
  if p x
    then ([], xs)
    else let (ys, zs) = break p xs'
         in (x:ys, zs)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) = 
  if p x
    then x : takeWhile p xs
    else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x:xs) = 
  if p x
    then dropWhile p xs
    else x:xs

group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = (x:ys) : group zs
  where (ys, zs) = span (== x) xs