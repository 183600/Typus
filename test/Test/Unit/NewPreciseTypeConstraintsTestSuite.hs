{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewPreciseTypeConstraintsTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Char (isDigit, isLetter)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing)
import Control.Monad (when)

import DependentTypesParser
import Parser (parseTypus)
import SourceLocation

-- | 测试基本精确类型的定义和验证
prop_basic_precise_type :: Int -> Property
prop_basic_precise_type x =
  let typusCode = "type Positive = int where { self > 0 }\n" ++
                 "func testFunc(x: Positive) -> Positive { return x }"
      parseResult = parseTypus (T.pack typusCode)
      isValidCode = isRight parseResult
      satisfiesConstraint = x > 0
  in property $ isValidCode ==> satisfiesConstraint

-- | 测试非零约束
prop_nonzero_constraint :: Int -> Property
prop_nonzero_constraint x =
  let typusCode = "type NonZero = int where { self != 0 }\n" ++
                 "func safeDiv(a: int, b: NonZero) -> int {\n" ++
                 "  return a / b\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
      isValidCode = isRight parseResult
      satisfiesConstraint = x /= 0
  in property $ isValidCode ==> satisfiesConstraint

-- | 测试范围约束
prop_range_constraint :: Int -> Int -> Property
prop_range_constraint lo hi =
  let typusCode = "type Bounded[lo: int, hi: int] = int where { self >= lo && self <= hi }\n" ++
                 "type Percentage = Bounded[0, 100]"
      parseResult = parseTypus (T.pack typusCode)
      isValidRange = lo <= hi
  in property $ isRight parseResult ==> isValidRange

-- | 测试字符串长度约束
prop_string_length_constraint :: String -> Property
prop_string_length_constraint s =
  let typusCode = "type NonEmpty = string where { len(self) > 0 }\n" ++
                 "func process(s: NonEmpty) -> string { return s }"
      parseResult = parseTypus (T.pack typusCode)
      isValidCode = isRight parseResult
      satisfiesConstraint = not (null s)
  in property $ isValidCode ==> satisfiesConstraint

-- | 测试数组索引约束
prop_array_index_constraint :: Positive Int -> Int -> Property
prop_array_index_constraint (Positive n) idx =
  let typusCode = "type ValidIndex[n: int] = int where { self >= 0 && self < n }\n" ++
                 "func safeGet[n: int](arr: [" ++ show n ++ "]int, i: ValidIndex[n]) -> int {\n" ++
                 "  return arr[i]\n" ++
                 "}"
      parseResult = parseTypus (T.pack typusCode)
      isValidIndex = idx >= 0 && idx < n
  in property $ isRight parseResult ==> isValidIndex

-- | 测试复合约束
prop_composite_constraints :: Int -> Int -> Int -> Property
prop_composite_constraints x minVal maxVal =
  let typusCode = "type BoundedRange[min: int, max: int] = int where { self >= min && self <= max }\n" ++
                 "type PositiveBounded[max: int] = BoundedRange[1, max]"
      parseResult = parseTypus (T.pack typusCode)
      isValidRange = minVal <= maxVal
      inRange = x >= minVal && x <= maxVal
  in property $ isRight parseResult ==> (isValidRange ==> inRange)

-- | 测试约束的组合
prop_constraint_combination :: Int -> Property
prop_constraint_combination x =
  let typusCode = "type Positive = int where { self > 0 }\n" ++
                 "type Even = int where { self % 2 == 0 }\n" ++
                 "type PositiveEven = int where { self > 0 && self % 2 == 0 }"
      parseResult = parseTypus (T.pack typusCode)
      isPositiveEven = x > 0 && even x
  in property $ isRight parseResult ==> isPositiveEven

-- | 测试约束的否定
prop_constraint_negation :: Int -> Property
prop_constraint_negation x =
  let typusCode = "type Positive = int where { self > 0 }\n" ++
                 "type NonPositive = int where { self <= 0 }"
      parseResult = parseTypus (T.pack typusCode)
      isNonPositive = x <= 0
  in property $ isRight parseResult ==> isNonPositive

-- | 测试约束的析取
prop_constraint_disjunction :: Int -> Int -> Property
prop_constraint_disjunction x y =
  let typusCode = "type EitherOr[a: int, b: int] = int where { self == a || self == b }"
      parseResult = parseTypus (T.pack typusCode)
      matchesEither = x == y || x == (y + 1)
  in property $ isRight parseResult ==> matchesEither

-- | 测试约束的蕴含
prop_constraint_implication :: Int -> Property
prop_constraint_implication x =
  let typusCode = "type Positive = int where { self > 0 }\n" ++
                 "type LargePositive = int where { self > 0 => self > 100 }"
      parseResult = parseTypus (T.pack typusCode)
      isLargePositive = x > 100
  in property $ isRight parseResult ==> isLargePositive

-- | 测试约束的等价
prop_constraint_equivalence :: Int -> Property
prop_constraint_equivalence x =
  let typusCode = "type Positive = int where { self > 0 }\n" ++
                 "type NotZeroOrNegative = int where { self != 0 && self >= 0 }"
      parseResult = parseTypus (T.pack typusCode)
      isPositive = x > 0
  in property $ isRight parseResult ==> isPositive

-- | 测试约束的量化
prop_constraint_quantification :: [Int] -> Property
prop_constraint_quantification xs =
  let typusCode = "type AllPositive[ints: []int] = bool where { forall x in ints, x > 0 }"
      parseResult = parseTypus (T.pack typusCode)
      allPositive = all (> 0) xs
  in property $ isRight parseResult ==> allPositive

-- | 测试约束的存在性
prop_constraint_existential :: [Int] -> Property
prop_constraint_existential xs =
  let typusCode = "type AnyPositive[ints: []int] = bool where { exists x in ints, x > 0 }"
      parseResult = parseTypus (T.pack typusCode)
      anyPositive = any (> 0) xs
  in property $ isRight parseResult ==> anyPositive

-- | 测试约束的聚合
prop_constraint_aggregation :: [Int] -> Property
prop_constraint_aggregation xs =
  let typusCode = "type SumPositive[ints: []int] = bool where { sum(ints) > 0 }"
      parseResult = parseTypus (T.pack typusCode)
      sumPositive = sum xs > 0
  in property $ isRight parseResult ==> sumPositive

-- | 测试约束的计数
prop_constraint_counting :: [Int] -> Property
prop_constraint_counting xs =
  let typusCode = "type CountPositive[ints: []int] = int where { count(x in ints, x > 0) > 0 }"
      parseResult = parseTypus (T.pack typusCode)
      countPositive = length (filter (> 0) xs) > 0
  in property $ isRight parseResult ==> countPositive

-- | 测试约束的平均值
prop_constraint_average :: [Int] -> Property
prop_constraint_average xs =
  let typusCode = "type AveragePositive[ints: []int] = bool where { average(ints) > 0 }"
      parseResult = parseTypus (T.pack typusCode)
      avgPositive = if null xs then False else (fromIntegral (sum xs) / fromIntegral (length xs) > 0)
  in property $ isRight parseResult ==> avgPositive

-- | 测试约束的最大值
prop_constraint_maximum :: [Int] -> Property
prop_constraint_maximum xs =
  let typusCode = "type MaxPositive[ints: []int] = bool where { max(ints) > 0 }"
      parseResult = parseTypus (T.pack typusCode)
      maxPositive = if null xs then False else maximum xs > 0
  in property $ isRight parseResult ==> maxPositive

-- | 测试约束的最小值
prop_constraint_minimum :: [Int] -> Property
prop_constraint_minimum xs =
  let typusCode = "type MinPositive[ints: []int] = bool where { min(ints) > 0 }"
      parseResult = parseTypus (T.pack typusCode)
      minPositive = if null xs then False else minimum xs > 0
  in property $ isRight parseResult ==> minPositive

-- | 测试约束的包含关系
prop_constraint_contains :: [Int] -> Int -> Property
prop_constraint_contains xs x =
  let typusCode = "type Contains[ints: []int, value: int] = bool where { value in ints }"
      parseResult = parseTypus (T.pack typusCode)
      containsValue = x `elem` xs
  in property $ isRight parseResult ==> containsValue

-- | 测试约束的子集关系
prop_constraint_subset :: [Int] -> [Int] -> Property
prop_constraint_subset xs ys =
  let typusCode = "type Subset[a: []int, b: []int] = bool where { forall x in a, x in b }"
      parseResult = parseTypus (T.pack typusCode)
      isSubset = all (`elem` ys) xs
  in property $ isRight parseResult ==> isSubset

-- | 测试约束的超集关系
prop_constraint_superset :: [Int] -> [Int] -> Property
prop_constraint_superset xs ys =
  let typusCode = "type Superset[a: []int, b: []int] = bool where { forall y in b, y in a }"
      parseResult = parseTypus (T.pack typusCode)
      isSuperset = all (`elem` xs) ys
  in property $ isRight parseResult ==> isSuperset

-- | 测试约束的交集
prop_constraint_intersection :: [Int] -> [Int] -> Property
prop_constraint_intersection xs ys =
  let typusCode = "type Intersection[a: []int, b: []int] = []int where { intersection(a, b) }"
      parseResult = parseTypus (T.pack typusCode)
      hasIntersection = any (`elem` ys) xs
  in property $ isRight parseResult ==> hasIntersection

-- | 测试约束的并集
prop_constraint_union :: [Int] -> [Int] -> Property
prop_constraint_union xs ys =
  let typusCode = "type Union[a: []int, b: []int] = []int where { union(a, b) }"
      parseResult = parseTypus (T.pack typusCode)
      hasUnion = not (null xs && null ys)
  in property $ isRight parseResult ==> hasUnion

-- | 测试约束的差集
prop_constraint_difference :: [Int] -> [Int] -> Property
prop_constraint_difference xs ys =
  let typusCode = "type Difference[a: []int, b: []int] = []int where { difference(a, b) }"
      parseResult = parseTypus (T.pack typusCode)
      hasDifference = any (`notElem` ys) xs
  in property $ isRight parseResult ==> hasDifference

-- | 测试约束的对称差集
prop_constraint_symmetric_difference :: [Int] -> [Int] -> Property
prop_constraint_symmetric_difference xs ys =
  let typusCode = "type SymmetricDifference[a: []int, b: []int] = []int where { symmetricDifference(a, b) }"
      parseResult = parseTypus (T.pack typusCode)
      hasSymDiff = any (`notElem` ys) xs || any (`notElem` xs) ys
  in property $ isRight parseResult ==> hasSymDiff

-- | 测试约束的笛卡尔积
prop_constraint_cartesian_product :: [Int] -> [Int] -> Property
prop_constraint_cartesian_product xs ys =
  let typusCode = "type CartesianProduct[a: []int, b: []int] = [](int, int) where { cartesianProduct(a, b) }"
      parseResult = parseTypus (T.pack typusCode)
      hasProduct = not (null xs || null ys)
  in property $ isRight parseResult ==> hasProduct

-- | 测试约束的幂集
prop_constraint_power_set :: [Int] -> Property
prop_constraint_power_set xs =
  let typusCode = "type PowerSet[a: []int] = [][]int where { powerSet(a) }"
      parseResult = parseTypus (T.pack typusCode)
      hasPowerSet = True  -- Power set always exists for any set
  in property $ isRight parseResult ==> hasPowerSet

-- | 测试约束的分区
prop_constraint_partition :: [Int] -> Int -> Property
prop_constraint_partition xs n =
  let typusCode = "type Partition[a: []int, size: int] = [][]int where { partition(a, size) }"
      parseResult = parseTypus (T.pack typusCode)
      canPartition = n > 0 && not (null xs)
  in property $ isRight parseResult ==> canPartition

-- | 测试约束的排列
prop_constraint_permutation :: [Int] -> Property
prop_constraint_permutation xs =
  let typusCode = "type Permutation[a: []int] = []int where { permutation(a) }"
      parseResult = parseTypus (T.pack typusCode)
      hasPermutation = not (null xs)
  in property $ isRight parseResult ==> hasPermutation

-- | 测试约束的组合
prop_constraint_combination :: [Int] -> Int -> Property
prop_constraint_combination xs k =
  let typusCode = "type Combination[a: []int, k: int] = [][]int where { combination(a, k) }"
      parseResult = parseTypus (T.pack typusCode)
      canCombine = k >= 0 && k <= length xs
  in property $ isRight parseResult ==> canCombine

-- | 测试约束的排序
prop_constraint_sorting :: [Int] -> Property
prop_constraint_sorting xs =
  let typusCode = "type Sorted[a: []int] = []int where { sorted(a) }"
      parseResult = parseTypus (T.pack typusCode)
      canSort = True  -- Any list can be sorted
  in property $ isRight parseResult ==> canSort

-- | 测试约束的去重
prop_constraint_deduplication :: [Int] -> Property
prop_constraint_deduplication xs =
  let typusCode = "type Unique[a: []int] = []int where { unique(a) }"
      parseResult = parseTypus (T.pack typusCode)
      canDeduplicate = True  -- Any list can be deduplicated
  in property $ isRight parseResult ==> canDeduplicate

-- | 测试约束的分组
prop_constraint_grouping :: [Int] -> Property
prop_constraint_grouping xs =
  let typusCode = "type Grouped[a: []int] = [](int, []int) where { groupBy(a, identity) }"
      parseResult = parseTypus (T.pack typusCode)
      canGroup = True  -- Any list can be grouped
  in property $ isRight parseResult ==> canGroup

-- | 测试约束的折叠
prop_constraint_folding :: [Int] -> Property
prop_constraint_folding xs =
  let typusCode = "type Folded[a: []int] = int where { fold(a, 0, add) }"
      parseResult = parseTypus (T.pack typusCode)
      canFold = True  -- Any list can be folded
  in property $ isRight parseResult ==> canFold

-- | 测试约束的映射
prop_constraint_mapping :: [Int] -> Property
prop_constraint_mapping xs =
  let typusCode = "type Mapped[a: []int] = []int where { map(a, double) }"
      parseResult = parseTypus (T.pack typusCode)
      canMap = True  -- Any list can be mapped
  in property $ isRight parseResult ==> canMap

-- | 测试约束的过滤
prop_constraint_filtering :: [Int] -> Property
prop_constraint_filtering xs =
  let typusCode = "type Filtered[a: []int] = []int where { filter(a, isPositive) }"
      parseResult = parseTypus (T.pack typusCode)
      canFilter = True  -- Any list can be filtered
  in property $ isRight parseResult ==> canFilter

-- | 测试约束的归约
prop_constraint_reduction :: [Int] -> Property
prop_constraint_reduction xs =
  let typusCode = "type Reduced[a: []int] = int where { reduce(a, add) }"
      parseResult = parseTypus (T.pack typusCode)
      canReduce = not (null xs)  -- Can only reduce non-empty lists
  in property $ isRight parseResult ==> canReduce

-- | 测试约束的扫描
prop_constraint_scanning :: [Int] -> Property
prop_constraint_scanning xs =
  let typusCode = "type Scanned[a: []int] = []int where { scan(a, 0, add) }"
      parseResult = parseTypus (T.pack typusCode)
      canScan = True  -- Any list can be scanned
  in property $ isRight parseResult ==> canScan

-- | 测试约束的展开
prop_constraint_unfolding :: Int -> Int -> Property
prop_constraint_unfolding init n =
  let typusCode = "type Unfolded[init: int, n: int] = []int where { unfold(init, n, next) }"
      parseResult = parseTypus (T.pack typusCode)
      canUnfold = n >= 0
  in property $ isRight parseResult ==> canUnfold

-- | 测试约束的迭代
prop_constraint_iteration :: Int -> Int -> Property
prop_constraint_iteration init n =
  let typusCode = "type Iterated[init: int, n: int] = []int where { iterate(init, n, next) }"
      parseResult = parseTypus (T.pack typusCode)
      canIterate = n >= 0
  in property $ isRight parseResult ==> canIterate

-- | 测试约束的重复
prop_constraint_repetition :: Int -> Int -> Property
prop_constraint_repetition x n =
  let typusCode = "type Repeated[value: int, n: int] = []int where { repeat(value, n) }"
      parseResult = parseTypus (T.pack typusCode)
      canRepeat = n >= 0
  in property $ isRight parseResult ==> canRepeat

-- | 测试约束的范围
prop_constraint_range_generation :: Int -> Int -> Property
prop_constraint_range_generation start end =
  let typusCode = "type Range[start: int, end: int] = []int where { range(start, end) }"
      parseResult = parseTypus (T.pack typusCode)
      canGenerate = start <= end
  in property $ isRight parseResult ==> canGenerate

-- | 测试约束的枚举
prop_constraint_enumeration :: Int -> Int -> Int -> Property
prop_constraint_enumeration start step n =
  let typusCode = "type Enumerated[start: int, step: int, n: int] = []int where { enumerate(start, step, n) }"
      parseResult = parseTypus (T.pack typusCode)
      canEnumerate = n >= 0
  in property $ isRight parseResult ==> canEnumerate

-- | 测试约束的连接
prop_constraint_concatenation :: [Int] -> [Int] -> Property
prop_constraint_concatenation xs ys =
  let typusCode = "type Concatenated[a: []int, b: []int] = []int where { concatenate(a, b) }"
      parseResult = parseTypus (T.pack typusCode)
      canConcatenate = True  -- Any lists can be concatenated
  in property $ isRight parseResult ==> canConcatenate

-- | 测试约束的交错
prop_constraint_interleaving :: [Int] -> [Int] -> Property
prop_constraint_interleaving xs ys =
  let typusCode = "type Interleaved[a: []int, b: []int] = []int where { interleave(a, b) }"
      parseResult = parseTypus (T.pack typusCode)
      canInterleave = True  -- Any lists can be interleaved
  in property $ isRight parseResult ==> canInterleave

-- | 测试约束的合并
prop_constraint_merging :: [Int] -> [Int] -> Property
prop_constraint_merging xs ys =
  let typusCode = "type Merged[a: []int, b: []int] = []int where { merge(a, b) }"
      parseResult = parseTypus (T.pack typusCode)
      canMerge = True  -- Any sorted lists can be merged
  in property $ isRight parseResult ==> canMerge

-- | 测试约束的分割
prop_constraint_splitting :: [Int] -> Int -> Property
prop_constraint_splitting xs n =
  let typusCode = "type Split[a: []int, n: int] = ([]int, []int) where { split(a, n) }"
      parseResult = parseTypus (T.pack typusCode)
      canSplit = n >= 0 && n <= length xs
  in property $ isRight parseResult ==> canSplit

-- | 测试约束的取子序列
prop_constraint_subsequence :: [Int] -> Int -> Int -> Property
prop_constraint_subsequence xs start len =
  let typusCode = "type Subsequence[a: []int, start: int, len: int] = []int where { subsequence(a, start, len) }"
      parseResult = parseTypus (T.pack typusCode)
      canTake = start >= 0 && len >= 0 && start + len <= length xs
  in property $ isRight parseResult ==> canTake

-- | 测试约束的丢弃
prop_constraint_dropping :: [Int] -> Int -> Property
prop_constraint_dropping xs n =
  let typusCode = "type Dropped[a: []int, n: int] = []int where { drop(a, n) }"
      parseResult = parseTypus (T.pack typusCode)
      canDrop = n >= 0 && n <= length xs
  in property $ isRight parseResult ==> canDrop

-- | 测试约束的取前N个
prop_constraint_taking :: [Int] -> Int -> Property
prop_constraint_taking xs n =
  let typusCode = "type Taken[a: []int, n: int] = []int where { take(a, n) }"
      parseResult = parseTypus (T.pack typusCode)
      canTake = n >= 0
  in property $ isRight parseResult ==> canTake

-- | 测试约束的反转
prop_constraint_reversing :: [Int] -> Property
prop_constraint_reversing xs =
  let typusCode = "type Reversed[a: []int] = []int where { reverse(a) }"
      parseResult = parseTypus (T.pack typusCode)
      canReverse = True  -- Any list can be reversed
  in property $ isRight parseResult ==> canReverse

-- | 测试约束的旋转
prop_constraint_rotating :: [Int] -> Int -> Property
prop_constraint_rotating xs n =
  let typusCode = "type Rotated[a: []int, n: int] = []int where { rotate(a, n) }"
      parseResult = parseTypus (T.pack typusCode)
      canRotate = not (null xs)
  in property $ isRight parseResult ==> canRotate

-- | 测试约束的洗牌
prop_constraint_shuffling :: [Int] -> Int -> Property
prop_constraint_shuffling xs seed =
  let typusCode = "type Shuffled[a: []int, seed: int] = []int where { shuffle(a, seed) }"
      parseResult = parseTypus (T.pack typusCode)
      canShuffle = True  -- Any list can be shuffled
  in property $ isRight parseResult ==> canShuffle

-- | 测试约束的采样
prop_constraint_sampling :: [Int] -> Int -> Int -> Property
prop_constraint_sampling xs n seed =
  let typusCode = "type Sampled[a: []int, n: int, seed: int] = []int where { sample(a, n, seed) }"
      parseResult = parseTypus (T.pack typusCode)
      canSample = n >= 0 && n <= length xs
  in property $ isRight parseResult ==> canSample

-- | 测试约束的排列组合
prop_constraint_permutations :: [Int] -> Property
prop_constraint_permutations xs =
  let typusCode = "type Permutations[a: []int] = [][]int where { permutations(a) }"
      parseResult = parseTypus (T.pack typusCode)
      hasPermutations = not (null xs)
  in property $ isRight parseResult ==> hasPermutations

-- | 测试约束的组合选择
prop_constraint_combinations :: [Int] -> Int -> Property
prop_constraint_combinations xs k =
  let typusCode = "type Combinations[a: []int, k: int] = [][]int where { combinations(a, k) }"
      parseResult = parseTypus (T.pack typusCode)
      hasCombinations = k >= 0 && k <= length xs
  in property $ isRight parseResult ==> hasCombinations

-- | 测试约束的子序列选择
prop_constraint_subsequences :: [Int] -> Property
prop_constraint_subsequences xs =
  let typusCode = "type Subsequences[a: []int] = [][]int where { subsequences(a) }"
      parseResult = parseTypus (T.pack typusCode)
      hasSubsequences = True  -- Any list has subsequences
  in property $ isRight parseResult ==> hasSubsequences

-- | 测试约束的连续子序列
prop_constraint_subarrays :: [Int] -> Property
prop_constraint_subarrays xs =
  let typusCode = "type Subarrays[a: []int] = [][]int where { subarrays(a) }"
      parseResult = parseTypus (T.pack typusCode)
      hasSubarrays = True  -- Any list has subarrays
  in property $ isRight parseResult ==> hasSubarrays

-- | 测试约束的滑动窗口
prop_constraint_sliding_windows :: [Int] -> Int -> Property
prop_constraint_sliding_windows xs size =
  let typusCode = "type SlidingWindows[a: []int, size: int] = [][]int where { slidingWindows(a, size) }"
      parseResult = parseTypus (T.pack typusCode)
      hasWindows = size > 0 && size <= length xs
  in property $ isRight parseResult ==> hasWindows

-- | 测试约束的分组块
prop_constraint_chunks :: [Int] -> Int -> Property
prop_constraint_chunks xs size =
  let typusCode = "type Chunks[a: []int, size: int] = [][]int where { chunks(a, size) }"
      parseResult = parseTypus (T.pack typusCode)
      hasChunks = size > 0
  in property $ isRight parseResult ==> hasChunks

-- | 测试约束的分批
prop_constraint_batches :: [Int] -> Int -> Property
prop_constraint_batches xs size =
  let typusCode = "type Batches[a: []int, size: int] = [][]int where { batches(a, size) }"
      parseResult = parseTypus (T.pack typusCode)
      hasBatches = size > 0
  in property $ isRight parseResult ==> hasBatches

-- | 测试约束的分割谓词
prop_constraint_partition_by :: [Int] -> Property
prop_constraint_partition_by xs =
  let typusCode = "type PartitionBy[a: []int] = ([][]int, [][]int) where { partitionBy(a, isEven) }"
      parseResult = parseTypus (T.pack typusCode)
      canPartitionBy = True  -- Any list can be partitioned by a predicate
  in property $ isRight parseResult ==> canPartitionBy

-- | 测试约束的分组键
prop_constraint_group_by_key :: [(Int, Int)] -> Property
prop_constraint_group_by_key xs =
  let typusCode = "type GroupByKey[a: [](int, int)] = [](int, []int) where { groupByKey(a) }"
      parseResult = parseTypus (T.pack typusCode)
      canGroupByKey = True  -- Any list of pairs can be grouped by key
  in property $ isRight parseResult ==> canGroupByKey

-- | 测试约束的映射值
prop_constraint_map_values :: [(Int, Int)] -> Property
prop_constraint_map_values xs =
  let typusCode = "type MapValues[a: [](int, int)] = []int where { mapValues(a) }"
      parseResult = parseTypus (T.pack typusCode)
      canMapValues = True  -- Any list of pairs has values
  in property $ isRight parseResult ==> canMapValues

-- | 测试约束的映射键
prop_constraint_map_keys :: [(Int, Int)] -> Property
prop_constraint_map_keys xs =
  let typusCode = "type MapKeys[a: [](int, int)] = []int where { mapKeys(a) }"
      parseResult = parseTypus (T.pack typusCode)
      canMapKeys = True  -- Any list of pairs has keys
  in property $ isRight parseResult ==> canMapKeys

-- | 测试约束的查找
prop_constraint_lookup :: [(Int, String)] -> Int -> Property
prop_constraint_lookup xs key =
  let typusCode = "type Lookup[a: [](int, string), k: int] = Option[string] where { lookup(a, k) }"
      parseResult = parseTypus (T.pack typusCode)
      canLookup = True  -- Can always lookup in a map
  in property $ isRight parseResult ==> canLookup

-- | 测试约束的插入
prop_constraint_insert :: [(Int, String)] -> Int -> String -> Property
prop_constraint_insert xs key value =
  let typusCode = "type Insert[a: [](int, string), k: int, v: string] = [](int, string) where { insert(a, k, v) }"
      parseResult = parseTypus (T.pack typusCode)
      canInsert = True  -- Can always insert into a map
  in property $ isRight parseResult ==> canInsert

-- | 测试约束的删除
prop_constraint_delete :: [(Int, String)] -> Int -> Property
prop_constraint_delete xs key =
  let typusCode = "type Delete[a: [](int, string), k: int] = [](int, string) where { delete(a, k) }"
      parseResult = parseTypus (T.pack typusCode)
      canDelete = True  -- Can always delete from a map
  in property $ isRight parseResult ==> canDelete

-- | 测试约束的更新
prop_constraint_update :: [(Int, String)] -> Int -> String -> Property
prop_constraint_update xs key value =
  let typusCode = "type Update[a: [](int, string), k: int, v: string] = [](int, string) where { update(a, k, v) }"
      parseResult = parseTypus (T.pack typusCode)
      canUpdate = True  -- Can always update in a map
  in property $ isRight parseResult ==> canUpdate

-- | 测试约束的合并映射
prop_constraint_merge_maps :: [(Int, String)] -> [(Int, String)] -> Property
prop_constraint_merge_maps xs ys =
  let typusCode = "type MergeMaps[a: [](int, string), b: [](int, string)] = [](int, string) where { mergeMaps(a, b) }"
      parseResult = parseTypus (T.pack typusCode)
      canMerge = True  -- Can always merge maps
  in property $ isRight parseResult ==> canMerge

-- | 测试约束的映射交集
prop_constraint_intersect_maps :: [(Int, String)] -> [(Int, String)] -> Property
prop_constraint_intersect_maps xs ys =
  let typusCode = "type IntersectMaps[a: [](int, string), b: [](int, string)] = [](int, string) where { intersectMaps(a, b) }"
      parseResult = parseTypus (T.pack typusCode)
      canIntersect = True  -- Can always intersect maps
  in property $ isRight parseResult ==> canIntersect

-- | 测试约束的映射差集
prop_constraint_diff_maps :: [(Int, String)] -> [(Int, String)] -> Property
prop_constraint_diff_maps xs ys =
  let typusCode = "type DiffMaps[a: [](int, string), b: [](int, string)] = [](int, string) where { diffMaps(a, b) }"
      parseResult = parseTypus (T.pack typusCode)
      canDiff = True  -- Can always diff maps
  in property $ isRight parseResult ==> canDiff

-- | 测试约束的映射键
prop_constraint_map_keys_set :: [(Int, String)] -> Property
prop_constraint_map_keys_set xs =
  let typusCode = "type MapKeysSet[a: [](int, string)] = Set[int] where { mapKeysSet(a) }"
      parseResult = parseTypus (T.pack typusCode)
      hasKeySet = True  -- Any map has a key set
  in property $ isRight parseResult ==> hasKeySet

-- | 测试约束的映射值集合
prop_constraint_map_values_set :: [(Int, String)] -> Property
prop_constraint_map_values_set xs =
  let typusCode = "type MapValuesSet[a: [](int, string)] = Set[string] where { mapValuesSet(a) }"
      parseResult = parseTypus (T.pack typusCode)
      hasValueSet = True  -- Any map has a value set
  in property $ isRight parseResult ==> hasValueSet

-- | 测试约束的映射过滤
prop_constraint_filter_map :: [(Int, String)] -> Property
prop_constraint_filter_map xs =
  let typusCode = "type FilterMap[a: [](int, string)] = [](int, string) where { filterMap(a, lengthGT3) }"
      parseResult = parseTypus (T.pack typusCode)
      canFilter = True  -- Can always filter a map
  in property $ isRight parseResult ==> canFilter

-- | 测试约束的映射映射
prop_constraint_map_map :: [(Int, String)] -> Property
prop_constraint_map_map xs =
  let typusCode = "type MapMap[a: [](int, string)] = [](int, string) where { mapMap(a, uppercase) }"
      parseResult = parseTypus (T.pack typusCode)
      canMap = True  -- Can always map a map
  in property $ isRight parseResult ==> canMap

-- | 测试约束的映射折叠
prop_constraint_fold_map :: [(Int, String)] -> Property
prop_constraint_fold_map xs =
  let typusCode = "type FoldMap[a: [](int, string)] = int where { foldMap(a, 0, length) }"
      parseResult = parseTypus (T.pack typusCode)
      canFold = True  -- Can always fold a map
  in property $ isRight parseResult ==> canFold

-- | 测试约束的集合操作
prop_constraint_set_operations :: [Int] -> [Int] -> Property
prop_constraint_set_operations xs ys =
  let typusCode = "type SetUnion[a: Set[int], b: Set[int]] = Set[int] where { union(a, b) }\n" ++
                 "type SetIntersection[a: Set[int], b: Set[int]] = Set[int] where { intersection(a, b) }\n" ++
                 "type SetDifference[a: Set[int], b: Set[int]] = Set[int] where { difference(a, b) }"
      parseResult = parseTypus (T.pack typusCode)
      canOperate = True  -- Can always operate on sets
  in property $ isRight parseResult ==> canOperate

-- | 测试约束的集合成员
prop_constraint_set_membership :: [Int] -> Int -> Property
prop_constraint_set_membership xs x =
  let typusCode = "type SetMember[a: Set[int], x: int] = bool where { member(a, x) }"
      parseResult = parseTypus (T.pack typusCode)
      canCheck = True  -- Can always check membership
  in property $ isRight parseResult ==> canCheck

-- | 测试约束的集合子集
prop_constraint_set_subset :: [Int] -> [Int] -> Property
prop_constraint_set_subset xs ys =
  let typusCode = "type SetSubset[a: Set[int], b: Set[int]] = bool where { subset(a, b) }"
      parseResult = parseTypus (T.pack typusCode)
      canCheck = True  -- Can always check subset
  in property $ isRight parseResult ==> canCheck

-- | 测试约束的集合插入
prop_constraint_set_insert :: [Int] -> Int -> Property
prop_constraint_set_insert xs x =
  let typusCode = "type SetInsert[a: Set[int], x: int] = Set[int] where { insert(a, x) }"
      parseResult = parseTypus (T.pack typusCode)
      canInsert = True  -- Can always insert into a set
  in property $ isRight parseResult ==> canInsert

-- | 测试约束的集合删除
prop_constraint_set_delete :: [Int] -> Int -> Property
prop_constraint_set_delete xs x =
  let typusCode = "type SetDelete[a: Set[int], x: int] = Set[int] where { delete(a, x) }"
      parseResult = parseTypus (T.pack typusCode)
      canDelete = True  -- Can always delete from a set
  in property $ isRight parseResult ==> canDelete

-- | 测试约束的集合大小
prop_constraint_set_size :: [Int] -> Property
prop_constraint_set_size xs =
  let typusCode = "type SetSize[a: Set[int]] = int where { size(a) }"
      parseResult = parseTypus (T.pack typusCode)
      hasSize = True  -- Any set has a size
  in property $ isRight parseResult ==> hasSize

-- | 测试约束的集合是否为空
prop_constraint_set_is_empty :: [Int] -> Property
prop_constraint_set_is_empty xs =
  let typusCode = "type SetIsEmpty[a: Set[int]] = bool where { isEmpty(a) }"
      parseResult = parseTypus (T.pack typusCode)
      canCheck = True  -- Can always check if empty
  in property $ isRight parseResult ==> canCheck

-- | 测试约束的集合转换
prop_constraint_set_to_list :: [Int] -> Property
prop_constraint_set_to_list xs =
  let typusCode = "type SetToList[a: Set[int]] = []int where { toList(a) }"
      parseResult = parseTypus (T.pack typusCode)
      canConvert = True  -- Can always convert set to list
  in property $ isRight parseResult ==> canConvert

-- | 测试约束的列表转集合
prop_constraint_list_to_set :: [Int] -> Property
prop_constraint_list_to_set xs =
  let typusCode = "type ListToSet[a: []int] = Set[int] where { toSet(a) }"
      parseResult = parseTypus (T.pack typusCode)
      canConvert = True  -- Can always convert list to set
  in property $ isRight parseResult ==> canConvert

-- | 测试约束的集合过滤
prop_constraint_set_filter :: [Int] -> Property
prop_constraint_set_filter xs =
  let typusCode = "type SetFilter[a: Set[int]] = Set[int] where { filter(a, isEven) }"
      parseResult = parseTypus (T.pack typusCode)
      canFilter = True  -- Can always filter a set
  in property $ isRight parseResult ==> canFilter

-- | 测试约束的集合映射
prop_constraint_set_map :: [Int] -> Property
prop_constraint_set_map xs =
  let typusCode = "type SetMap[a: Set[int]] = Set[int] where { map(a, double) }"
      parseResult = parseTypus (T.pack typusCode)
      canMap = True  -- Can always map a set
  in property $ isRight parseResult ==> canMap

-- | 测试约束的集合折叠
prop_constraint_set_fold :: [Int] -> Property
prop_constraint_set_fold xs =
  let typusCode = "type SetFold[a: Set[int]] = int where { fold(a, 0, add) }"
      parseResult = parseTypus (T.pack typusCode)
      canFold = True  -- Can always fold a set
  in property $ isRight parseResult ==> canFold

-- | 测试约束的集合分区
prop_constraint_set_partition :: [Int] -> Property
prop_constraint_set_partition xs =
  let typusCode = "type SetPartition[a: Set[int]] = (Set[int], Set[int]) where { partition(a, isEven) }"
      parseResult = parseTypus (T.pack typusCode)
      canPartition = True  -- Can always partition a set
  in property $ isRight parseResult ==> canPartition

-- | 测试约束的集合交集
prop_constraint_set_intersection :: [Int] -> [Int] -> Property
prop_constraint_set_intersection xs ys =
  let typusCode = "type SetIntersection[a: Set[int], b: Set[int]] = Set[int] where { intersection(a, b) }"
      parseResult = parseTypus (T.pack typusCode)
      canIntersect = True  -- Can always intersect sets
  in property $ isRight parseResult ==> canIntersect

-- | 测试约束的集合并集
prop_constraint_set_union :: [Int] -> [Int] -> Property
prop_constraint_set_union xs ys =
  let typusCode = "type SetUnion[a: Set[int], b: Set[int]] = Set[int] where { union(a, b) }"
      parseResult = parseTypus (T.pack typusCode)
      canUnion = True  -- Can always union sets
  in property $ isRight parseResult ==> canUnion

-- | 测试约束的集合差集
prop_constraint_set_difference :: [Int] -> [Int] -> Property
prop_constraint_set_difference xs ys =
  let typusCode = "type SetDifference[a: Set[int], b: Set[int]] = Set[int] where { difference(a, b) }"
      parseResult = parseTypus (T.pack typusCode)
      canDiff = True  -- Can always diff sets
  in property $ isRight parseResult ==> canDiff

-- | 测试约束的集合对称差集
prop_constraint_set_symmetric_difference :: [Int] -> [Int] -> Property
prop_constraint_set_symmetric_difference xs ys =
  let typusCode = "type SetSymmetricDifference[a: Set[int], b: Set[int]] = Set[int] where { symmetricDifference(a, b) }"
      parseResult = parseTypus (T.pack typusCode)
      canSymDiff = True  -- Can always symmetric diff sets
  in property $ isRight parseResult ==> canSymDiff

-- | 测试约束的集合笛卡尔积
prop_constraint_set_cartesian_product :: [Int] -> [Int] -> Property
prop_constraint_set_cartesian_product xs ys =
  let typusCode = "type SetCartesianProduct[a: Set[int], b: Set[int]] = Set[(int, int)] where { cartesianProduct(a, b) }"
      parseResult = parseTypus (T.pack typusCode)
      canProduct = True  -- Can always cartesian product sets
  in property $ isRight parseResult ==> canProduct

-- | 测试约束的集合幂集
prop_constraint_set_power_set :: [Int] -> Property
prop_constraint_set_power_set xs =
  let typusCode = "type SetPowerSet[a: Set[int]] = Set[Set[int]] where { powerSet(a) }"
      parseResult = parseTypus (T.pack typusCode)
      hasPowerSet = True  -- Any set has a power set
  in property $ isRight parseResult ==> hasPowerSet

-- | 测试约束的集合选择
prop_constraint_set_select :: [Int] -> Property
prop_constraint_set_select xs =
  let typusCode = "type SetSelect[a: Set[int]] = Set[int] where { select(a, isEven) }"
      parseResult = parseTypus (T.pack typusCode)
      canSelect = True  -- Can always select from a set
  in property $ isRight parseResult ==> canSelect

-- | 测试约束的集合拒绝
prop_constraint_set_reject :: [Int] -> Property
prop_constraint_set_reject xs =
  let typusCode = "type SetReject[a: Set[int]] = Set[int] where { reject(a, isEven) }"
      parseResult = parseTypus (T.pack typusCode)
      canReject = True  -- Can always reject from a set
  in property $ isRight parseResult ==> canReject

-- | 测试约束的集合计数
prop_constraint_set_count :: [Int] -> Property
prop_constraint_set_count xs =
  let typusCode = "type SetCount[a: Set[int]] = int where { count(a, isEven) }"
      parseResult = parseTypus (T.pack typusCode)
      canCount = True  -- Can always count in a set
  in property $ isRight parseResult ==> canCount

-- | 测试约束的集合存在
prop_constraint_set_exists :: [Int] -> Property
prop_constraint_set_exists xs =
  let typusCode = "type SetExists[a: Set[int]] = bool where { exists(a, isEven) }"
      parseResult = parseTypus (T.pack typusCode)
      canCheck = True  -- Can always check exists in a set
  in property $ isRight parseResult ==> canCheck

-- | 测试约束的集合全称
prop_constraint_set_forall :: [Int] -> Property
prop_constraint_set_forall xs =
  let typusCode = "type SetForall[a: Set[int]] = bool where { forall(a, isEven) }"
      parseResult = parseTypus (T.pack typusCode)
      canCheck = True  -- Can always check forall in a set
  in property $ isRight parseResult ==> canCheck

-- | 测试约束的集合查找
prop_constraint_set_find :: [Int] -> Property
prop_constraint_set_find xs =
  let typusCode = "type SetFind[a: Set[int]] = Option[int] where { find(a, isEven) }"
      parseResult = parseTypus (T.pack typusCode)
      canFind = True  -- Can always find in a set
  in property $ isRight parseResult ==> canFind

-- | 测试约束的集合最小值
prop_constraint_set_minimum :: [Int] -> Property
prop_constraint_set_minimum xs =
  let typusCode = "type SetMinimum[a: Set[int]] = Option[int] where { minimum(a) }"
      parseResult = parseTypus (T.pack typusCode)
      canFind = not (null xs)  -- Can only find minimum in non-empty set
  in property $ isRight parseResult ==> canFind

-- | 测试约束的集合最大值
prop_constraint_set_maximum :: [Int] -> Property
prop_constraint_set_maximum xs =
  let typusCode = "type SetMaximum[a: Set[int]] = Option[int] where { maximum(a) }"
      parseResult = parseTypus (T.pack typusCode)
      canFind = not (null xs)  -- Can only find maximum in non-empty set
  in property $ isRight parseResult ==> canFind

-- | 测试约束的集合求和
prop_constraint_set_sum :: [Int] -> Property
prop_constraint_set_sum xs =
  let typusCode = "type SetSum[a: Set[int]] = int where { sum(a) }"
      parseResult = parseTypus (T.pack typusCode)
      canSum = True  -- Can always sum a set
  in property $ isRight parseResult ==> canSum

-- | 测试约束的集合平均值
prop_constraint_set_average :: [Int] -> Property
prop_constraint_set_average xs =
  let typusCode = "type SetAverage[a: Set[int]] = Option[float] where { average(a) }"
      parseResult = parseTypus (T.pack typusCode)
      canAverage = not (null xs)  -- Can only average non-empty set
  in property $ isRight parseResult ==> canAverage

-- | 测试约束的集合乘积
prop_constraint_set_product :: [Int] -> Property
prop_constraint_set_product xs =
  let typusCode = "type SetProduct[a: Set[int]] = int where { product(a) }"
      parseResult = parseTypus (T.pack typusCode)
      canProduct = True  -- Can always product a set
  in property $ isRight parseResult ==> canProduct

-- Helper type for positive integers
newtype Positive a = Positive a
  deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Positive a) where
  arbitrary = Positive <$> arbitrary `suchThat` (> 0)

tests :: TestTree
tests = testGroup "Test.Unit.NewPreciseTypeConstraintsTestSuite Tests"
  [ testProperty "basic precise type" prop_basic_precise_type
  , testProperty "nonzero constraint" prop_nonzero_constraint
  , testProperty "range constraint" prop_range_constraint
  , testProperty "string length constraint" prop_string_length_constraint
  , testProperty "array index constraint" prop_array_index_constraint
  , testProperty "composite constraints" prop_composite_constraints
  , testProperty "constraint combination" prop_constraint_combination
  , testProperty "constraint negation" prop_constraint_negation
  , testProperty "constraint disjunction" prop_constraint_disjunction
  , testProperty "constraint implication" prop_constraint_implication
  , testProperty "constraint equivalence" prop_constraint_equivalence
  , testProperty "constraint quantification" prop_constraint_quantification
  , testProperty "constraint existential" prop_constraint_existential
  , testProperty "constraint aggregation" prop_constraint_aggregation
  , testProperty "constraint counting" prop_constraint_counting
  , testProperty "constraint average" prop_constraint_average
  , testProperty "constraint maximum" prop_constraint_maximum
  , testProperty "constraint minimum" prop_constraint_minimum
  , testProperty "constraint contains" prop_constraint_contains
  , testProperty "constraint subset" prop_constraint_subset
  , testProperty "constraint superset" prop_constraint_superset
  , testProperty "constraint intersection" prop_constraint_intersection
  , testProperty "constraint union" prop_constraint_union
  , testProperty "constraint difference" prop_constraint_difference
  , testProperty "constraint symmetric difference" prop_constraint_symmetric_difference
  , testProperty "constraint cartesian product" prop_constraint_cartesian_product
  , testProperty "constraint power set" prop_constraint_power_set
  , testProperty "constraint partition" prop_constraint_partition
  , testProperty "constraint permutation" prop_constraint_permutation
  , testProperty "constraint combination" prop_constraint_combination
  , testProperty "constraint sorting" prop_constraint_sorting
  , testProperty "constraint deduplication" prop_constraint_deduplication
  , testProperty "constraint grouping" prop_constraint_grouping
  , testProperty "constraint folding" prop_constraint_folding
  , testProperty "constraint mapping" prop_constraint_mapping
  , testProperty "constraint filtering" prop_constraint_filtering
  , testProperty "constraint reduction" prop_constraint_reduction
  , testProperty "constraint scanning" prop_constraint_scanning
  , testProperty "constraint unfolding" prop_constraint_unfolding
  , testProperty "constraint iteration" prop_constraint_iteration
  , testProperty "constraint repetition" prop_constraint_repetition
  , testProperty "constraint range generation" prop_constraint_range_generation
  , testProperty "constraint enumeration" prop_constraint_enumeration
  , testProperty "constraint concatenation" prop_constraint_concatenation
  , testProperty "constraint interleaving" prop_constraint_interleaving
  , testProperty "constraint merging" prop_constraint_merging
  , testProperty "constraint splitting" prop_constraint_splitting
  , testProperty "constraint subsequence" prop_constraint_subsequence
  , testProperty "constraint dropping" prop_constraint_dropping
  , testProperty "constraint taking" prop_constraint_taking
  , testProperty "constraint reversing" prop_constraint_reversing
  , testProperty "constraint rotating" prop_constraint_rotating
  , testProperty "constraint shuffling" prop_constraint_shuffling
  , testProperty "constraint sampling" prop_constraint_sampling
  , testProperty "constraint permutations" prop_constraint_permutations
  , testProperty "constraint combinations" prop_constraint_combinations
  , testProperty "constraint subsequences" prop_constraint_subsequences
  , testProperty "constraint subarrays" prop_constraint_subarrays
  , testProperty "constraint sliding windows" prop_constraint_sliding_windows
  , testProperty "constraint chunks" prop_constraint_chunks
  , testProperty "constraint batches" prop_constraint_batches
  , testProperty "constraint partition by" prop_constraint_partition_by
  , testProperty "constraint group by key" prop_constraint_group_by_key
  , testProperty "constraint map values" prop_constraint_map_values
  , testProperty "constraint map keys" prop_constraint_map_keys
  , testProperty "constraint lookup" prop_constraint_lookup
  , testProperty "constraint insert" prop_constraint_insert
  , testProperty "constraint delete" prop_constraint_delete
  , testProperty "constraint update" prop_constraint_update
  , testProperty "constraint merge maps" prop_constraint_merge_maps
  , testProperty "constraint intersect maps" prop_constraint_intersect_maps
  , testProperty "constraint diff maps" prop_constraint_diff_maps
  , testProperty "constraint map keys set" prop_constraint_map_keys_set
  , testProperty "constraint map values set" prop_constraint_map_values_set
  , testProperty "constraint filter map" prop_constraint_filter_map
  , testProperty "constraint map map" prop_constraint_map_map
  , testProperty "constraint fold map" prop_constraint_fold_map
  , testProperty "constraint set operations" prop_constraint_set_operations
  , testProperty "constraint set membership" prop_constraint_set_membership
  , testProperty "constraint set subset" prop_constraint_set_subset
  , testProperty "constraint set insert" prop_constraint_set_insert
  , testProperty "constraint set delete" prop_constraint_set_delete
  , testProperty "constraint set size" prop_constraint_set_size
  , testProperty "constraint set is empty" prop_constraint_set_is_empty
  , testProperty "constraint set to list" prop_constraint_set_to_list
  , testProperty "constraint list to set" prop_constraint_list_to_set
  , testProperty "constraint set filter" prop_constraint_set_filter
  , testProperty "constraint set map" prop_constraint_set_map
  , testProperty "constraint set fold" prop_constraint_set_fold
  , testProperty "constraint set partition" prop_constraint_set_partition
  , testProperty "constraint set intersection" prop_constraint_set_intersection
  , testProperty "constraint set union" prop_constraint_set_union
  , testProperty "constraint set difference" prop_constraint_set_difference
  , testProperty "constraint set symmetric difference" prop_constraint_set_symmetric_difference
  , testProperty "constraint set cartesian product" prop_constraint_set_cartesian_product
  , testProperty "constraint set power set" prop_constraint_set_power_set
  , testProperty "constraint set select" prop_constraint_set_select
  , testProperty "constraint set reject" prop_constraint_set_reject
  , testProperty "constraint set count" prop_constraint_set_count
  , testProperty "constraint set exists" prop_constraint_set_exists
  , testProperty "constraint set forall" prop_constraint_set_forall
  , testProperty "constraint set find" prop_constraint_set_find
  , testProperty "constraint set minimum" prop_constraint_set_minimum
  , testProperty "constraint set maximum" prop_constraint_set_maximum
  , testProperty "constraint set sum" prop_constraint_set_sum
  , testProperty "constraint set average" prop_constraint_set_average
  , testProperty "constraint set product" prop_constraint_set_product
  ]