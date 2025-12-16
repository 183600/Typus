{-# LANGUAGE CPP #-}

module Test.Unit.NewCabalQuickCheckTests (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Char (isSpace, toLower, toUpper)
import Data.List (sort, nub)

import Utils (trim, splitBy, splitByCollapsed, splitByComma, removeLineComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn, posOffset, spanStart, spanEnd)
import Parser (FileDirectives(..), BlockDirectives(..))
import TestSupport.Arbitrary ()

-- 测试trim函数的幂等性
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  trim (trim s) === trim s

-- 测试splitBy和splitByCollapsed的关系
prop_splitBy_collapsed_relationship :: Char -> String -> Property
prop_splitBy_collapsed_relationship delim s =
  splitByCollapsed delim s === filter (not . null) (splitBy delim s)

-- 测试splitByComma与splitBy的一致性
prop_splitByComma_consistency :: String -> Property
prop_splitByComma_consistency s =
  splitByComma s === splitBy ',' s

-- 测试removeLineComments不会影响没有注释的字符串
prop_removeLineComments_no_comment :: Property
prop_removeLineComments_no_comment =
  forAll (listOf (elements $ filter (/= '/') ['a'..'z'])) $ \s ->
    removeLineComments s === s

-- 测试SourcePos的创建和访问器
prop_sourcepos_creation_access :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcepos_creation_access (Positive line) (Positive col) (Positive offset) =
  let pos = SourcePos line col offset
  in posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === offset

-- 测试SourceSpan的基本属性
prop_sourcespan_basic :: SourceSpan -> Property
prop_sourcespan_basic span =
  let start = spanStart span
      end = spanEnd span
  in property $ posOffset start <= posOffset end

-- 测试Map的insert+lookup属性
prop_map_insert_lookup :: String -> Int -> Property
prop_map_insert_lookup key value =
  Map.lookup key (Map.insert key value Map.empty) === Just value

-- 测试Set的insert+member属性
prop_set_insert_member :: Int -> Property
prop_set_insert_member value =
  property (Set.member value (Set.insert value Set.empty))

-- 测试字符串大小写转换的往返性
prop_string_case_roundtrip :: String -> Property
prop_string_case_roundtrip s =
  map toLower (map toUpper s) === map toUpper (map toLower s)

-- 测试列表排序后去重的性质
prop_sort_nub_property :: [Int] -> Property
prop_sort_nub_property xs =
  sort (nub xs) === nub (sort xs)

tests :: TestTree
tests = testGroup "New Cabal QuickCheck Tests"
  [ fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "splitByCollapsed relationship with splitBy" prop_splitBy_collapsed_relationship
  , fastProperty "splitByComma consistency with splitBy" prop_splitByComma_consistency
  , fastProperty "removeLineComments preserves strings without comments" prop_removeLineComments_no_comment
  , fastProperty "SourcePos creation and accessors" prop_sourcepos_creation_access
  , fastProperty "SourceSpan basic property" prop_sourcespan_basic
  , fastProperty "Map insert+lookup property" prop_map_insert_lookup
  , fastProperty "Set insert+member property" prop_set_insert_member
  , fastProperty "String case conversion roundtrip" prop_string_case_roundtrip
  , fastProperty "Sort and nub property" prop_sort_nub_property
  ]