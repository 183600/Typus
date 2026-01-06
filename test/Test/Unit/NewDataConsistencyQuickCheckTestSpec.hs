{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewDataConsistencyQuickCheckTestSpec where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck (property)
import Utils (trim, splitBy, splitByCollapsed, removeLineComments)
import SourceLocation (SourcePos(..), startPos, posAfter, SourceSpan(..), spanFrom, mergeSpans, isValidSpan)
import Parser (FileDirectives(..), BlockDirectives(..))
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- 测试字符串操作的一致性
prop_trim_associative :: String -> Bool
prop_trim_associative s = trim (trim s) == trim s

prop_split_by_idempotent :: Char -> String -> Bool
prop_split_by_idempotent delim s = 
  let parts1 = splitBy delim s
      parts2 = splitBy delim (L.concat parts1)
  in L.length parts1 == L.length parts2

prop_split_by_collapsed_consistency :: Char -> String -> Bool
prop_split_by_collapsed_consistency delim s =
  let normal = splitBy delim s
      collapsed = splitByCollapsed delim s
  in L.all (not . null) collapsed && L.length collapsed <= L.length normal

-- 测试位置操作的一致性
prop_position_monotonic :: String -> Bool
prop_position_monotonic s = 
  let chars = take 10 s  -- 限制长度
      positions = scanl posAfter startPos chars
      isMonotonic ps = L.all (uncurry (<=)) (zip ps (L.tail ps))
  in isMonotonic positions

prop_span_merge_associative :: SourcePos -> SourcePos -> SourcePos -> Bool
prop_span_merge_associative pos1 pos2 pos3 =
  let span1 = spanFrom pos1
      span2 = spanFrom pos2  
      span3 = spanFrom pos3
      merge12 = mergeSpans span1 span2
      merge23 = mergeSpans span2 span3
      result1 = mergeSpans merge12 span3
      result2 = mergeSpans span1 merge23
  in spanStart result1 == spanStart result2 && spanEnd result1 == spanEnd result2

-- 测试解析器数据结构的一致性
prop_file_directives_roundtrip :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Bool
prop_file_directives_roundtrip own deps cons =
  let directives = FileDirectives own deps cons
      extracted = (fdOwnership directives, fdDependentTypes directives, fdConstraints directives)
  in extracted == (own, deps, cons)

prop_block_directives_roundtrip :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Bool
prop_block_directives_roundtrip own deps cons =
  let directives = BlockDirectives own deps cons
      extracted = (bdOwnership directives, bdDependentTypes directives, bdConstraints directives)
  in extracted == (own, deps, cons)

-- 测试数据不变量
prop_trim_length_invariant :: String -> Bool
prop_trim_length_invariant s = L.length (trim s) <= L.length s

prop_split_by_length_invariant :: Char -> String -> Bool
prop_split_by_length_invariant delim s =
  let parts = splitBy delim s
      totalLength = L.sum (map L.length parts) + L.length (L.filter (== delim) s)
  in totalLength == L.length s

prop_position_ordering_invariant :: SourcePos -> SourcePos -> Bool
prop_position_ordering_invariant pos1 pos2 =
  let span1 = spanFrom pos1
      span2 = spanFrom pos2
      merged = mergeSpans span1 span2
  in spanStart merged <= pos1 && spanEnd merged >= pos2

-- 测试数据转换的一致性
prop_comment_removal_idempotent :: String -> Bool
prop_comment_removal_idempotent s = 
  let once = removeLineComments s
      twice = removeLineComments once
  in once == twice

prop_string_processing_pipeline :: String -> Bool
prop_string_processing_pipeline s =
  let step1 = trim s
      step2 = removeLineComments step1
      step3 = splitBy ' ' step2
      step4 = L.concat step3
      step5 = trim step4
  in L.length step5 <= L.length s

-- 测试数据完整性
prop_span_integrity :: SourcePos -> SourcePos -> Bool
prop_span_integrity start end =
  let span = SourceSpan start end
  in spanStart span == start && spanEnd span == end

prop_directives_integrity :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Bool
prop_directives_integrity own deps cons =
  let fileDirs = FileDirectives own deps cons
      blockDirs = BlockDirectives own deps cons
  -- 不同类型的指令应该保持相同的值
  in (isNothing (fdOwnership fileDirs) == isNothing (bdOwnership blockDirs)) &&
     (isNothing (fdDependentTypes fileDirs) == isNothing (bdDependentTypes blockDirs)) &&
     (isNothing (fdConstraints fileDirs) == isNothing (bdConstraints blockDirs))

-- 测试可逆性
prop_split_join_reversibility :: Char -> String -> Bool
prop_split_join_reversibility delim s = 
  let parts = splitBy delim s
      rejoined = L.concat (intersperse [delim] parts)
  in rejoined == s
  where
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:xs) = x ++ sep ++ intersperse sep xs

-- 生成测试套件
tests :: TestTree
tests = testGroup "Data Consistency QuickCheck Tests"
  [ testProperty "trim associative" prop_trim_associative
  , testProperty "splitBy idempotent" prop_split_by_idempotent
  , testProperty "splitByCollapsed consistency" prop_split_by_collapsed_consistency
  , testProperty "position monotonic" prop_position_monotonic
  , testProperty "span merge associative" prop_span_merge_associative
  , testProperty "file directives roundtrip" prop_file_directives_roundtrip
  , testProperty "block directives roundtrip" prop_block_directives_roundtrip
  , testProperty "trim L.length invariant" prop_trim_length_invariant
  , testProperty "splitBy L.length invariant" prop_split_by_length_invariant
  , testProperty "position ordering invariant" prop_position_ordering_invariant
  , testProperty "comment removal idempotent" prop_comment_removal_idempotent
  , testProperty "string processing pipeline" prop_string_processing_pipeline
  , testProperty "span integrity" prop_span_integrity
  , testProperty "directives integrity" prop_directives_integrity
  , testProperty "split join reversibility" prop_split_join_reversibility
  ]