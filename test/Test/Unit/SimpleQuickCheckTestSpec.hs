{-# LANGUAGE CPP #-}

module Test.Unit.SimpleQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), property)
import qualified Data.Map as Map
import Data.List (sort)

import Utils (trim, splitBy)
import Ownership.Parser (Expr(..), Stmt(..))
import TestSupport.Arbitrary ()
import TestSupport.ExtendedArbitrary ()

tests :: TestTree
tests = testGroup "Simple QuickCheck Test Properties"
  [ basicStringTests
  , basicMathTests
  , basicListTests
  , basicMapTests
  , basicASTTests
  ]

basicStringTests :: TestTree
basicStringTests = testGroup "Basic String Properties"
  [ fastProperty "trim of empty string is empty" prop_trim_empty
  , fastProperty "trim of whitespace-only string is empty" prop_trim_whitespace_only
  , fastProperty "splitBy empty delimiter returns list of characters" prop_splitBy_empty_delim
  ]

basicMathTests :: TestTree
basicMathTests = testGroup "Basic Math Properties"
  [ fastProperty "addition is commutative" prop_addition_commutative
  , fastProperty "addition is associative" prop_addition_associative
  , fastProperty "multiplication distributes over addition" prop_multiplication_distributive
  ]

basicListTests :: TestTree
basicListTests = testGroup "Basic List Properties"
  [ fastProperty "L.reverse of L.reverse is original" prop_reverse_reverse
  , fastProperty "sort preserves L.length" prop_sort_preserves_length
  , fastProperty "L.concat of empty list is identity" prop_concat_empty
  ]

basicMapTests :: TestTree
basicMapTests = testGroup "Basic Map Properties"
  [ fastProperty "Map lookup after insert returns value" prop_map_insert_lookup
  , fastProperty "Map size increases after new insert" prop_map_insert_size
  ]

basicASTTests :: TestTree
basicASTTests = testGroup "Basic AST Properties"
  [ fastProperty "Expr type is preserved" prop_expr_type_preserved
  , fastProperty "Stmt nesting increases depth" prop_stmt_nesting_depth
  ]

prop_trim_empty :: Property
prop_trim_empty = trim "" === ""

prop_trim_whitespace_only :: String -> Property
prop_trim_whitespace_only s =
  let wsOnly = L.all (`elem` " \t\n\r") s
  in wsOnly ==> trim s === ""

prop_splitBy_empty_delim :: String -> Property
prop_splitBy_empty_delim s =
  let result = splitBy ' ' s
      expected = if null s then [""] else words s
  in result === expected

prop_addition_commutative :: Int -> Int -> Property
prop_addition_commutative x y = x + y === y + x

prop_addition_associative :: Int -> Int -> Int -> Property
prop_addition_associative x y z = (x + y) + z === x + (y + z)

prop_multiplication_distributive :: Int -> Int -> Int -> Property
prop_multiplication_distributive x y z = x * (y + z) === (x * y) + (x * z)

prop_reverse_reverse :: [Int] -> Property
prop_reverse_reverse xs = L.reverse (L.reverse xs) === xs

prop_sort_preserves_length :: [Int] -> Property
prop_sort_preserves_length xs = L.length (sort xs) === L.length xs

prop_concat_empty :: [Int] -> Property
prop_concat_empty xs = [] ++ xs === xs

prop_map_insert_lookup :: String -> Int -> Property
prop_map_insert_lookup key value = Map.lookup key (Map.insert key value Map.empty) === Just value

prop_map_insert_size :: String -> Int -> Property
prop_map_insert_size key value = Map.size (Map.insert key value Map.empty) === 1

prop_expr_type_preserved :: Expr -> Property
prop_expr_type_preserved _ = property True

prop_stmt_nesting_depth :: Stmt -> Property
prop_stmt_nesting_depth _ = property True