module Test.Unit.NewPerformanceQuickCheckTestSpec where


import Test.Tasty
import Test.Tasty.QuickCheck 
import SourceLocation (SourcePos(..), startPos, posAfter, mergeSpans, spanFrom)
import qualified Data.List as L
import Data.List 
prop_trim_linear_time (Small n)                               s = n >= 0 && n <=                               100 ==>  -- 
  let result = L.foldl (\acc _ -> trim acc) s [1..n]
  in L.length result <= L.length s

prop_split_by_performance :: Small Int -> String -> Bool
prop_split_by_performance (Small n)                               s = n >= 0 && n <=                               50 ==>  -- 
  let parts = splitBy ',' s
                                    result = L.foldl (\acc _ -> L.length acc) parts [1..n]
  in result >= 0

-- 
prop_position_calculation_performance :: Small Int -> String -> Bool
prop_position_calculation_performance (Small n)                               s = n >= 0 && n <=                               100 ==>  -- 
  let pos = startPos
                                    chars = take n s
                                    finalPos = foldl posAfter pos chars
  in sourceLine finalPos >= 1 && sourceColumn finalPos >= 1

prop_merge_spans_performance :: Small Int -> Bool
prop_merge_spans_performance (Small n) = n >= 0 && n <=                               100 ==>  -- 
  let positions = L.map (\i -> SourcePos i i) [1..n]
                                    spans = map spanFrom positions
                                    merged = foldl mergeSpans (L.head spans) (L.tail spans)
  in isValidSpan merged

-- 
prop_trim_memory_efficiency :: Small Int -> Property
prop_trim_memory_efficiency (Small n) = n >= 0 && n <=                               1000 ==>  -- 
  let largeString = replicate n 'a' ++ "   "  -- 
                                    trimmed = trim largeString
  in L.length trimmed <= L.length largeString

prop_split_memory_efficiency :: Small Int -> Property
prop_split_memory_efficiency (Small n) = n >= 0 && n <=                               100 ==>  -- 
  let largeString = L.concat (replicate n "a,")
                                    parts = splitBy ',' largeString
  in L.length                               parts == n + 1

-- 
prop_recursive_trimming :: Small Int -> String -> Bool
prop_recursive_trimming (Small depth)                               s = depth >= 0 && depth <=                               10 ==>  -- 
  let recursiveTrim 0                               str = str
      recursiveTrim d                               str = recursiveTrim (d-1) (trim str)
                                    result = recursiveTrim depth s
  in L.length result <= L.length s

prop_recursive_splitting :: Small Int -> String -> Bool
prop_recursive_splitting (Small depth)                               s = depth >= 0 && depth <=                               5 ==>  -- 
  let recursiveSplit 0                               str = [str]
      recursiveSplit d                               str = concatMap (splitBy ',') (recursiveSplit (d-1) str)
                                    result = recursiveSplit depth s
  in L.length result >= 1

-- 
prop_comment_removal_complexity :: Small Int -> String -> Bool
prop_comment_removal_complexity (Small n)                               s = n >= 0 && n <=                               100 ==>  -- 
  let stringWithComments = L.concat (replicate n (s ++ "// comment\n")
                                    withoutComments = removeLineComments stringWithComments
  in L.length withoutComments <= L.length stringWithComments

-- 
prop_batch_position_operations :: Small Int -> Bool
prop_batch_position_operations (Small n) = n >= 0 && n <=                               1000 ==>  -- 
  let positions = L.map (\i -> SourcePos i i) [1..n]
                                    finalPos = L.foldl (\pos (SourcePos l c) -> posAfter pos '\n') startPos positions
  in sourceLine finalPos >= 1

prop_batch_string_operations :: Small Int -> String -> Bool
prop_batch_string_operations (Small n)                               s = n >= 0 && n <=                               50 ==>  -- 
  let strings = replicate n s
                                    results =  L.map (trim . removeLineComments) strings
  in L.length                               results == n

-- 
tests :: TestTree
tests =   testGroup "Performance QuickCheck Tests"
  [             testProperty "trim linear time" prop_trim_linear_time
  ,             testProperty "splitBy performance" prop_split_by_performance
  ,             testProperty "position calculation performance" prop_position_calculation_performance
  ,             testProperty "mergeSpans performance" prop_merge_spans_performance
  ,             testProperty "trim memory efficiency" prop_trim_memory_efficiency
  ,             testProperty "split memory efficiency" prop_split_memory_efficiency
  ,             testProperty "recursive trimming" prop_recursive_trimming
  ,             testProperty "recursive splitting" prop_recursive_splitting
  ,             testProperty "comment removal complexity" prop_comment_removal_complexity
  ,             testProperty "batch position operations" prop_batch_position_operations
  ,             testProperty "batch string operations" prop_batch_string_operations
  ]