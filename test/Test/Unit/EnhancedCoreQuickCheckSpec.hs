{-# LANGUAGE CPP #-}

module Test.Unit.EnhancedCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, emptySpan)
import Utils (trim, splitBy, splitByCollapsed)
import TestSupport.Arbitrary ()

-- | 测试字符串分割的幂等性：对空字符串进行多次分割应该得到相同结果
prop_splitBy_empty_idempotent :: Char -> Property
prop_splitBy_empty_idempotent delim =
  splitBy delim "" === splitBy delim (splitBy delim "" !! 0)

-- | 测试分割后的连接：分割再连接应该恢复原始字符串
prop_splitBy_join_roundtrip :: Char -> String -> Property
prop_splitBy_join_roundtrip delim s =
  not (elem delim s) ==>
  concat (splitBy delim s) === s

-- | 测试折叠分割的等价性：折叠分割应该等于普通分割后过滤空字符串
prop_splitByCollapsed_equivalence :: Char -> String -> Property
prop_splitByCollapsed_equivalence delim s =
  splitByCollapsed delim s === filter (not . null) (splitBy delim s)

-- | 测试trim的幂等性：多次trim应该与一次trim结果相同
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  trim (trim s) === trim s

-- | 测试trim后分割的等价性：先trim再分割应该等于分割后trim每个部分
prop_trim_split_equivalence :: String -> Property
prop_trim_split_equivalence s =
  splitBy ',' (trim s) === map trim (splitBy ',' s)

-- | 测试SourcePos的递进：posAfter应该增加列数
prop_posAfter_increments_column :: Int -> Int -> Int -> Property
prop_posAfter_increments_column line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = SourcePos line col offset
      nextPos = posAfter 'a' pos
  in posLine nextPos === line .&&. posColumn nextPos === col + 1

-- | 测试SourceSpan的空跨度：空跨度的开始和结束位置应该相同
prop_empty_span_same_start_end :: Property
prop_empty_span_same_start_end =
  let pos = startPos
      srcSpan = emptySpan pos
  in spanStart srcSpan === spanEnd srcSpan

-- | 测试Map的插入和查找：插入后查找应该返回Just值
prop_map_insert_lookup :: String -> Int -> Property
prop_map_insert_lookup key value =
  Map.lookup key (Map.insert key value Map.empty) === Just value

-- | 测试Set的插入和成员检查：插入后应该成为成员
prop_set_insert_member :: Int -> Property
prop_set_insert_member value =
  property (Set.member value (Set.insert value Set.empty))

tests :: TestTree
tests = testGroup "Enhanced Core QuickCheck Tests"
  [ fastProperty "splitBy on empty string is idempotent" prop_splitBy_empty_idempotent
  , fastProperty "splitBy and concat roundtrip preserves string" prop_splitBy_join_roundtrip
  , fastProperty "splitByCollapsed equals splitBy filtered" prop_splitByCollapsed_equivalence
  , fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "trim then split equals split then trim parts" prop_trim_split_equivalence
  , fastProperty "posAfter increments column for single char" prop_posAfter_increments_column
  , fastProperty "empty span has same start and end" prop_empty_span_same_start_end
  , fastProperty "Map insert then lookup returns Just value" prop_map_insert_lookup
  , fastProperty "Set insert then member returns True" prop_set_insert_member
  ]