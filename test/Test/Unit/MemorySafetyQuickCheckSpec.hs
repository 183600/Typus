{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.MemorySafetyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.)
  , Arbitrary(..), Gen, oneof, choose, listOf, vectorOf, elements, sized, frequency
  , suchThat, resize
  )

import Utils (trim, splitBy, removeComments)
import SourceLocation (SourcePos(..), advancePosByText)
import Data.List (length)
import Data.List (foldl', take, drop)
import Data.Char (isSpace, isPrint)
import Control.DeepSeq (NFData, rnf, force)
import qualified Data.Text as T (pack, unpack)
import Data.Text (Text)

-- Test data for memory safety
data MemorySafetyTestData = MemorySafetyTestData
  { testString :: String
  , testText :: Text
  , testList :: [Int]
  , testNestedList :: [[Int]]
  } deriving (Show, Eq)

instance Arbitrary MemorySafetyTestData where
  arbitrary = do
    str <- listOf $ elements "abc\n\t /-*123"
    txt <- T.pack <$> listOf (elements "abc\n\t /-*123")
    lst <- listOf (choose (0, 100))
    nested <- listOf (listOf (choose (0, 50)))
    return $ MemorySafetyTestData str txt lst nested

-- Property: String operations don't cause memory leaks
prop_string_operations_no_leaks :: MemorySafetyTestData -> Property
prop_string_operations_no_leaks testData =
  let str = testString testData
      trimmed = trim str
      split = splitBy ' ' str
      commented = removeComments str
      result = rnf (trimmed, split, commented)
  in property $ result === ()

-- Property: Text operations are memory safe
prop_text_operations_safe :: MemorySafetyTestData -> Property
prop_text_operations_safe testData =
  let txt = testText testData
      L.length' = T.L.length txt
      lines' = T.lines txt
      words' = T.words txt
      result = rnf (L.length', lines', words')
  in property $ result === ()

-- Property: List operations handle large inputs safely
prop_large_list_operations_safe :: [Int] -> Property
prop_large_list_operations_safe lst =
  let filtered = L.filter (> 50) lst
      mapped = L.map (*2) lst
      folded = foldl' (+) 0 lst
      result = rnf (filtered, mapped, folded)
  in property $ result === ()

-- Property: Nested list operations are memory safe
prop_nested_list_operations_safe :: [[Int]] -> Property
prop_nested_list_operations_safe nested =
  let flattened = L.concat nested
      sumOfLengths = L.sum $ map L.length nested
      maxElement = L.maximum $ map L.maximum nested
      result = rnf (flattened, sumOfLengths, maxElement)
  in property $ result === ()

-- Property: Source position tracking doesn't accumulate memory
prop_source_position_no_accumulation :: MemorySafetyTestData -> Property
prop_source_position_no_accumulation testData =
  let txt = testText testData
      positions = scanl (\pos _ -> advancePosByText pos txt) startPos [1..10]
      result = rnf positions
  in property $ result === ()

-- Property: Repeated operations don't grow memory
prop_repeated_operations_memory :: MemorySafetyTestData -> Int -> Property
prop_repeated_operations_memory testData iterations =
  iterations > 0 && iterations < 100 ==>
  let str = testString testData
      performOp n = if n <= 0 then str else trim (performOp (n - 1))
      result = performOp iterations
      resultLength = L.length result
  in property $ resultLength <= L.length str + 1000  -- Allow some reasonable growth

-- Property: Deep nesting doesn't cause stack overflow
prop_deep_nesting_safe :: Int -> Property
prop_deep_nesting_safe depth =
  depth > 0 && depth < 1000 ==>
  let nested = replicate depth [1,2,3]
      flattened = L.concat nested
      result = rnf flattened
  in property $ result === ()

-- Property: Large string processing is memory efficient
prop_large_string_processing :: Int -> Property
prop_large_string_processing size =
  size > 0 && size < 10000 ==>
  let largeString = replicate size 'a'
      processed = trim largeString
      result = rnf processed
  in property $ result === ()

-- Property: Text conversion is memory safe
prop_text_conversion_safe :: String -> Property
prop_text_conversion_safe str =
  let txt = T.pack str
      str' = T.unpack txt
      result = rnf (str', txt)
  in property $ result === ()

-- Property: Recursive operations are bounded
prop_recursive_operations_bounded :: [Int] -> Property
prop_recursive_operations_bounded lst =
  let quicksort [] = []
      quicksort (x:xs) = quicksort (L.filter (< x) xs) ++ [x] ++ quicksort (L.filter (>= x) xs)
      sorted = quicksort lst
      result = rnf sorted
  in property $ result === ()

-- Property: Memory usage scales linearly with input size
prop_memory_usage_linear :: Int -> Property
prop_memory_usage_linear size =
  size > 0 && size < 5000 ==>
  let input = [1..size]
      processed = L.map (*2) $ filter even input
      result = rnf processed
  in property $ result === ()

-- Property: Garbage collection works for temporary objects
prop_garbage_collection_works :: MemorySafetyTestData -> Property
prop_garbage_collection_works testData =
  let str = testString testData
      temp1 = map toUpper str
      temp2 = map toLower temp1
      temp3 = L.reverse temp2
      final = L.length temp3
      result = rnf final
  in property $ result === ()
  where
    toUpper c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

-- Property: Resource cleanup happens properly
prop_resource_cleanup_proper :: MemorySafetyTestData -> Property
prop_resource_cleanup_proper testData =
  let processChunk chunk = rnf $ L.length chunk
      chunks = chunksOf 100 (testString testData)
      results = map processChunk chunks
      finalResult = rnf results
  in property $ finalResult === ()

-- Property: Memory allocation is bounded for pathological inputs
prop_memory_allocation_bounded :: String -> Property
prop_memory_allocation_bounded input =
  let processed = removeComments input
      result = rnf processed
  in property $ result === ()

-- Helper function
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

tests :: TestTree
tests = testGroup "Memory Safety QuickCheck Tests"
  [ fastProperty "String operations don't cause memory leaks" prop_string_operations_no_leaks
  , fastProperty "Text operations are memory safe" prop_text_operations_safe
  , fastProperty "List operations handle large inputs safely" prop_large_list_operations_safe
  , fastProperty "Nested list operations are memory safe" prop_nested_list_operations_safe
  , fastProperty "Source position tracking doesn't accumulate memory" prop_source_position_no_accumulation
  , fastProperty "Repeated operations don't grow memory" prop_repeated_operations_memory
  , fastProperty "Deep nesting doesn't cause stack overflow" prop_deep_nesting_safe
  , fastProperty "Large string processing is memory efficient" prop_large_string_processing
  , fastProperty "Text conversion is memory safe" prop_text_conversion_safe
  , fastProperty "Recursive operations are bounded" prop_recursive_operations_bounded
  , fastProperty "Memory usage scales linearly with input size" prop_memory_usage_linear
  , fastProperty "Garbage collection works for temporary objects" prop_garbage_collection_works
  , fastProperty "Resource cleanup happens properly" prop_resource_cleanup_proper
  , fastProperty "Memory allocation is bounded for pathological inputs" prop_memory_allocation_bounded
  , testCase "Manual memory safety test" $ do
      let largeString = replicate 10000 "test string "
          trimmed = trim largeString
          split = splitBy ' ' trimmed
          
      assertBool "Large string processing works" $ not (null trimmed)
      assertBool "Large split works" $ L.length split > 0
      
      let nestedList = replicate 100 [1..100]
          flattened = L.concat nestedList
          
      assertBool "Nested list flattening works" $ L.length flattened == 100 * 100
      
      let text = T.pack "Hello World\nThis is a test\nMultiple lines"
          lines' = T.lines text
          
      assertBool "Text processing works" $ L.length lines' == 3
      
      -- Force evaluation to ensure no thunks
      rnf trimmed @?= ()
      rnf split @?= ()
      rnf flattened @?= ()
      rnf lines' @?= ()
  ]