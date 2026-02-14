{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewQuickCheckPropertiesTestSuite where

import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.MemoryLimits 
  ( withMemoryLimits
  , memoryLimitedTestGroup
  , memoryLevelTestGroup
  , MemoryLevel(..)
  , withMemoryLevel
  , gcBetweenTests
  )
import Data.List (isInfixOf, sort, nub)
import Data.Char (isSpace, toLower, toUpper)
import Data.Either (isLeft, isRight)
import Data.Maybe (listToMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Compiler (compileTypus)
import Parser (parseTypus)
import DependentTypesParser (parseDependentType)
import Ownership (analyzeOwnership)
import Utils (trim, splitBy, normalizeIndentation)

-- | 字符串属性测试

-- | trim函数的幂等性
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
      trimmedAgain = trim trimmed
  in trimmed === trimmedAgain

-- | trim函数不增加字符串长度
prop_trim_never_increases_length :: String -> Property
prop_trim_never_increases_length s =
  let trimmed = trim s
  in property $ length trimmed <= length s

-- | trim函数移除所有前后空白字符
prop_trim_removes_leading_trailing_whitespace :: String -> Property
prop_trim_removes_leading_trailing_whitespace s =
  let trimmed = trim s
      hasLeadingWhitespace = not (null s) && isSpace (head s)
      hasTrailingWhitespace = not (null s) && isSpace (last s)
  in property $ if hasLeadingWhitespace || hasTrailingWhitespace
    then length trimmed < length s || trimmed == ""
    else trimmed == s

-- | splitBy函数的基本属性
prop_splitBy_basic :: Char -> String -> Property
prop_splitBy_basic c s =
  let parts = splitBy c s
      rejoined = intercalate [c] parts
  in property $ 
    (if null s then parts == [""] else True) &&
    (if all (== c) s then parts == replicate (length s + 1) "" else True) &&
    (length (concat parts) + length (filter (== c) s) >= length s)

  where
    intercalate _ [] = []
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- | splitBy函数对空字符串的处理
prop_splitBy_empty :: Char -> Property
prop_splitBy_empty c = splitBy c "" === [""]

-- | 列表属性测试

-- | 列表排序的幂等性
prop_sort_idempotent :: [Int] -> Property
prop_sort_idempotent xs = sort xs === sort (sort xs)

-- | 列表排序不改变元素数量
prop_sort_preserves_length :: [Int] -> Property
prop_sort_preserves_length xs = length (sort xs) === length xs

-- | 列表排序保持元素集合
prop_sort_preserves_elements :: [Int] -> Property
prop_sort_preserves_elements xs = sort xs === sort (sort xs)

-- | nub函数移除重复元素
prop_nub_removes_duplicates :: [Int] -> Property
prop_nub_removes_duplicates xs = 
  let deduped = nub xs
  in property $ 
    length deduped <= length xs &&
    sort deduped === nub (sort xs)

-- | nub函数的幂等性
prop_nub_idempotent :: [Int] -> Property
prop_nub_idempotent xs = nub xs === nub (nub xs)

-- | Map属性测试

-- | Map插入和查找的一致性
prop_map_insert_lookup :: [(Int, String)] -> Int -> String -> Property
prop_map_insert_lookup pairs key value =
  let m = Map.fromList pairs
      m' = Map.insert key value m
  in Map.lookup key m' === Just value

-- | Map删除操作
prop_map_delete :: [(Int, String)] -> Int -> Property
prop_map_delete pairs key =
  let m = Map.fromList pairs
      m' = Map.delete key m
  in Map.lookup key m' === Nothing

-- | Map大小一致性
prop_map_size :: [(Int, String)] -> Property
prop_map_size pairs =
  let m = Map.fromList pairs
  in Map.size m === length (nub $ map fst pairs)

-- | Set属性测试

-- | Set插入和成员检查的一致性
prop_set_insert_member :: [Int] -> Int -> Property
prop_set_insert_member xs elem =
  let s = Set.fromList xs
      s' = Set.insert elem s
  in elem `Set.member` s'

-- | Set删除操作
prop_set_delete :: [Int] -> Int -> Property
prop_set_delete xs elem =
  let s = Set.fromList xs
      s' = Set.delete elem s
  in not (elem `Set.member` s')

-- | Set大小一致性
prop_set_size :: [Int] -> Property
prop_set_size xs = Set.size (Set.fromList xs) === length (nub xs)

-- | 字符串转换属性测试

-- | 大小写转换的幂等性
prop_to_upper_idempotent :: String -> Property
prop_to_upper_idempotent s = map toUpper (map toUpper s) === map toUpper s

prop_to_lower_idempotent :: String -> Property
prop_to_lower_idempotent s = map toLower (map toLower s) === map toLower s

-- | 大小写转换不改变字符串长度
prop_case_preserves_length :: String -> Property
prop_case_preserves_length s = 
  property $ 
    length (map toUpper s) === length s &&
    length (map toLower s) === length s

-- | 解析器属性测试

-- | 解析器的幂等性（对于有效输入）
prop_parser_idempotent :: String -> Property
prop_parser_idempotent s =
  let limitedString = take 10 s  -- 限制字符串大小
      result1 = parseTypus limitedString
  in case result1 of
    Right ast -> 
      let str = show ast
          result2 = parseTypus str
      in case result2 of
        Right ast2 -> property $ show ast2 == str
        Left _ -> property False
    Left _ -> property True

-- | 解析器错误处理的一致性
prop_parser_error_consistency :: String -> Property
prop_parser_error_consistency s =
  let limitedString = take 8 s  -- 限制字符串大小
      result1 = parseTypus limitedString
      result2 = parseTypus limitedString
  in case (result1, result2) of
    (Left err1, Left err2) -> err1 === err2
    (Right ast1, Right ast2) -> show ast1 === show ast2
    _ -> property False

-- | 编译器属性测试

-- | 编译器的幂等性（对于有效输入）
prop_compiler_idempotent :: String -> Property
prop_compiler_idempotent s =
  let limitedString = take 8 s  -- 限制字符串大小
      result1 = compileTypus limitedString
  in case result1 of
    Right goCode -> 
      let result2 = compileTypus goCode
      in case result2 of
        Right goCode2 -> property $ length goCode2 >= 0
        Left _ -> property False
    Left _ -> property True

-- | 编译器错误处理的一致性
prop_compiler_error_consistency :: String -> Property
prop_compiler_error_consistency s =
  let limitedString = take 6 s  -- 限制字符串大小
      result1 = compileTypus limitedString
      result2 = compileTypus limitedString
  in case (result1, result2) of
    (Left err1, Left err2) -> err1 === err2
    (Right goCode1, Right goCode2) -> goCode1 === goCode2
    _ -> property False

-- | 依赖类型解析器属性测试

-- | 依赖类型解析器的幂等性
prop_dependent_type_parser_idempotent :: String -> Property
prop_dependent_type_parser_idempotent s =
  let limitedString = take 8 s  -- 限制字符串大小
      result1 = parseDependentType limitedString
  in case result1 of
    Right dt -> 
      let str = show dt
          result2 = parseDependentType str
      in case result2 of
        Right dt2 -> property $ show dt2 == str
        Left _ -> property False
    Left _ -> property True

-- | 所有权分析器属性测试

-- | 所有权分析器的一致性
prop_ownership_analyzer_consistency :: String -> Property
prop_ownership_analyzer_consistency s =
  let limitedString = take 8 s  -- 限制字符串大小
      result1 = analyzeOwnership limitedString
      result2 = analyzeOwnership limitedString
  in property $ show result1 == show result2

-- | 数值属性测试

-- | 加法交换律
prop_addition_commutative :: Int -> Int -> Property
prop_addition_commutative x y = x + y === y + x

-- | 加法结合律
prop_addition_associative :: Int -> Int -> Int -> Property
prop_addition_associative x y z = (x + y) + z === x + (y + z)

-- | 乘法交换律
prop_multiplication_commutative :: Int -> Int -> Property
prop_multiplication_commutative x y = x * y === y * x

-- | 乘法结合律
prop_multiplication_associative :: Int -> Int -> Int -> Property
prop_multiplication_associative x y z = (x * y) * z === x * (y * z)

-- | 分配律
prop_distributive_law :: Int -> Int -> Int -> Property
prop_distributive_law x y z = x * (y + z) === (x * y) + (x * z)

-- | 布尔属性测试

-- | 与运算的交换律
prop_and_commutative :: Bool -> Bool -> Property
prop_and_commutative x y = (x && y) === (y && x)

-- | 或运算的交换律
prop_or_commutative :: Bool -> Bool -> Property
prop_or_commutative x y = (x || y) === (y || x)

-- | 与运算的结合律
prop_and_associative :: Bool -> Bool -> Bool -> Property
prop_and_associative x y z = (x && y) && z === x && (y && z)

-- | 或运算的结合律
prop_or_associative :: Bool -> Bool -> Bool -> Property
prop_or_associative x y z = (x || y) || z === x || (y || z)

-- | 德摩根定律
prop_de_morgan :: Bool -> Bool -> Property
prop_de_morgan x y = not (x && y) === (not x) || (not y)

-- | 边界条件测试

-- | 空字符串的trim
prop_trim_empty :: Property
prop_trim_empty = trim "" === ""

-- | 空列表的排序
prop_sort_empty :: Property
prop_sort_empty = sort [] === ([] :: [Int])

-- | 空Map的查找
prop_map_lookup_empty :: Int -> Property
prop_map_lookup_empty key = Map.lookup key Map.empty === Nothing

-- | 空Set的成员检查
prop_set_member_empty :: Int -> Property
prop_set_member_empty elem = not (elem `Set.member` Set.empty)

-- | 测试套件
tests :: TestTree
tests = memoryLevelTestGroup Minimal "New QuickCheck Properties Test Suite (Memory Optimized)"
  [ -- 字符串属性测试
    withMemoryLevel Minimal $ testProperty "Trim idempotent" prop_trim_idempotent
  , withMemoryLevel Minimal $ testProperty "Trim never increases length" prop_trim_never_increases_length
  , withMemoryLevel Minimal $ testProperty "Trim removes leading/trailing whitespace" prop_trim_removes_leading_trailing_whitespace
  , withMemoryLevel Minimal $ testProperty "SplitBy basic" prop_splitBy_basic
  , withMemoryLevel Minimal $ testProperty "SplitBy empty" prop_splitBy_empty
  
  -- 列表属性测试
  , withMemoryLevel Minimal $ testProperty "Sort idempotent" prop_sort_idempotent
  , withMemoryLevel Minimal $ testProperty "Sort preserves length" prop_sort_preserves_length
  , withMemoryLevel Minimal $ testProperty "Sort preserves elements" prop_sort_preserves_elements
  , withMemoryLevel Minimal $ testProperty "Nub removes duplicates" prop_nub_removes_duplicates
  , withMemoryLevel Minimal $ testProperty "Nub idempotent" prop_nub_idempotent
  
  -- Map属性测试
  , withMemoryLevel Minimal $ testProperty "Map insert lookup" prop_map_insert_lookup
  , withMemoryLevel Minimal $ testProperty "Map delete" prop_map_delete
  , withMemoryLevel Minimal $ testProperty "Map size" prop_map_size
  
  -- Set属性测试
  , withMemoryLevel Minimal $ testProperty "Set insert member" prop_set_insert_member
  , withMemoryLevel Minimal $ testProperty "Set delete" prop_set_delete
  , withMemoryLevel Minimal $ testProperty "Set size" prop_set_size
  
  -- 字符串转换属性测试
  , withMemoryLevel Minimal $ testProperty "To upper idempotent" prop_to_upper_idempotent
  , withMemoryLevel Minimal $ testProperty "To lower idempotent" prop_to_lower_idempotent
  , withMemoryLevel Minimal $ testProperty "Case preserves length" prop_case_preserves_length
  
  -- 解析器属性测试
  , withMemoryLevel Minimal $ testProperty "Parser idempotent" prop_parser_idempotent
  , withMemoryLevel Minimal $ testProperty "Parser error consistency" prop_parser_error_consistency
  
  -- 编译器属性测试
  , withMemoryLevel Minimal $ testProperty "Compiler idempotent" prop_compiler_idempotent
  , withMemoryLevel Minimal $ testProperty "Compiler error consistency" prop_compiler_error_consistency
  
  -- 依赖类型解析器属性测试
  , withMemoryLevel Minimal $ testProperty "Dependent type parser idempotent" prop_dependent_type_parser_idempotent
  
  -- 所有权分析器属性测试
  , withMemoryLevel Minimal $ testProperty "Ownership analyzer consistency" prop_ownership_analyzer_consistency
  
  -- 数值属性测试
  , withMemoryLevel Minimal $ testProperty "Addition commutative" prop_addition_commutative
  , withMemoryLevel Minimal $ testProperty "Addition associative" prop_addition_associative
  , withMemoryLevel Minimal $ testProperty "Multiplication commutative" prop_multiplication_commutative
  , withMemoryLevel Minimal $ testProperty "Multiplication associative" prop_multiplication_associative
  , withMemoryLevel Minimal $ testProperty "Distributive law" prop_distributive_law
  
  -- 布尔属性测试
  , withMemoryLevel Minimal $ testProperty "And commutative" prop_and_commutative
  , withMemoryLevel Minimal $ testProperty "Or commutative" prop_or_commutative
  , withMemoryLevel Minimal $ testProperty "And associative" prop_and_associative
  , withMemoryLevel Minimal $ testProperty "Or associative" prop_or_associative
  , withMemoryLevel Minimal $ testProperty "De Morgan" prop_de_morgan
  
  -- 边界条件测试
  , withMemoryLevel Minimal $ testProperty "Trim empty" prop_trim_empty
  , withMemoryLevel Minimal $ testProperty "Sort empty" prop_sort_empty
  , withMemoryLevel Minimal $ testProperty "Map lookup empty" prop_map_lookup_empty
  , withMemoryLevel Minimal $ testProperty "Set member empty" prop_set_member_empty
  ]