{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 智能测试选择器模块
-- 这个模块提供了基于内存约束的智能测试选择策略
module TestSupport.SmartTestSelector 
  ( -- 测试选择器
    TestSelector(..)
  , SmartTestConfig(..)
  , defaultSmartConfig
    
    -- 测试选择策略
  , selectTestsByMemory
  , selectTestsByPriority
  , selectTestsByCategory
  , selectTestsByComplexity
    
    -- 测试分类
  , TestCategory(..)
  , TestPriority(..)
  , TestComplexity(..)
  , TestInfo(..)
  , classifyTest
    
    -- 动态选择
  , dynamicTestSelection
  , adaptiveMemorySelection
  , createBalancedTestSuite
    
    -- 监控和统计
  , TestSelectionStats(..)
  , calculateSelectionStats
  , printSelectionReport
    
    -- 预定义配置
  , minimalMemoryConfig
  , standardMemoryConfig
  , extremeMemoryConfig
  , ciMemoryConfig
    
    -- 测试元数据
  , TestMetadata(..)
    
    -- 智能测试套件
  , createSmartTestSuite
  , runSmartTests
  , analyzeTestCoverage
  ) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit (testCase)
import Data.List (sort, groupBy, partition, sortBy)
import Data.Function (on)
import Data.Ord (comparing)
import Text.Printf (printf)
import Data.Maybe (isJust, isNothing)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable (traverse_)

-- | 测试类别
data TestCategory = 
    CoreParser          -- ^ 核心解析器测试
  | CoreCompiler        -- ^ 核心编译器测试
  | CoreUtils           -- ^ 核心工具测试
  | ErrorHandler        -- ^ 错误处理测试
  | DependencyAnalysis  -- ^ 依赖分析测试
  | DependentTypes      -- ^ 依赖类型测试
  | Ownership           -- ^ 所有权系统测试
  | SourceLocation      -- ^ 源码位置测试
  | Integration         -- ^ 集成测试
  | Performance         -- ^ 性能测试
  | EdgeCase            -- ^ 边界情况测试
  | Regression          -- ^ 回归测试
  | Other               -- ^ 其他测试
  deriving (Show, Eq, Ord)

-- | 测试优先级
data TestPriority = 
    Critical    -- ^ 关键测试（必须运行）
  | High        -- ^ 高优先级
  | Medium      -- ^ 中等优先级
  | Low         -- ^ 低优先级
  | Optional    -- ^ 可选测试
  deriving (Show, Eq, Ord)

-- | 测试复杂度
data TestComplexity = 
    Simple      -- ^ 简单测试（低内存）
  | Moderate    -- ^ 中等复杂度
  | Complex     -- ^ 复杂测试（高内存）
  | VeryComplex -- ^ 非常复杂（极高内存）
  deriving (Show, Eq, Ord)

-- | 测试信息
data TestInfo = TestInfo
  { testName :: String
  , testCategory :: TestCategory
  , testPriority :: TestPriority
  , testComplexity :: TestComplexity
  , estimatedMemoryMB :: Int
  , testDescription :: String
  } deriving (Show, Eq, Ord)

-- | 智能测试配置
data SmartTestConfig = SmartTestConfig
  { memoryLimitMB :: Int              -- ^ 内存限制
  , maxTestCount :: Int               -- ^ 最大测试数量
  , priorityWeights :: Map.Map TestPriority Double  -- ^ 优先级权重
  , categoryWeights :: Map.Map TestCategory Double  -- ^ 类别权重
  , complexityPenalty :: Map.Map TestComplexity Double -- ^ 复杂度惩罚
  , enableAdaptiveSelection :: Bool   -- ^ 启用自适应选择
  , preserveCriticalTests :: Bool     -- ^ 保留关键测试
  , balanceCategories :: Bool         -- ^ 平衡类别分布
  , maxQuickCheckSize :: Int          -- ^ QuickCheck最大大小
  , quickCheckTestCount :: Int        -- ^ QuickCheck测试数量
  , quickCheckMaxShrinks :: Int       -- ^ QuickCheck最大收缩次数
  , testSelectionRatio :: Double      -- ^ 测试选择比例
  } deriving (Show, Eq)

-- | 默认智能配置
defaultSmartConfig :: SmartTestConfig
defaultSmartConfig = SmartTestConfig
  { memoryLimitMB = 64
  , maxTestCount = 50
  , priorityWeights = Map.fromList
      [ (Critical, 1.0)
      , (High, 0.8)
      , (Medium, 0.6)
      , (Low, 0.4)
      , (Optional, 0.2)
      ]
  , categoryWeights = Map.fromList
      [ (CoreParser, 1.0)
      , (CoreCompiler, 1.0)
      , (CoreUtils, 0.9)
      , (ErrorHandler, 0.8)
      , (DependencyAnalysis, 0.7)
      , (DependentTypes, 0.6)
      , (Ownership, 0.6)
      , (SourceLocation, 0.5)
      , (Integration, 0.4)
      , (Performance, 0.3)
      , (EdgeCase, 0.3)
      , (Regression, 0.5)
      , (Other, 0.2)
      ]
  , complexityPenalty = Map.fromList
      [ (Simple, 1.0)
      , (Moderate, 0.8)
      , (Complex, 0.5)
      , (VeryComplex, 0.2)
      ]
  , enableAdaptiveSelection = True
  , preserveCriticalTests = True
  , balanceCategories = True
  , maxQuickCheckSize = 5
  , quickCheckTestCount = 25
  , quickCheckMaxShrinks = 20
  , testSelectionRatio = 0.5
  }

-- | 最小内存配置
minimalMemoryConfig :: SmartTestConfig
minimalMemoryConfig = defaultSmartConfig
  { memoryLimitMB = 16
  , maxTestCount = 10
  }

-- | 标准内存配置
standardMemoryConfig :: SmartTestConfig
standardMemoryConfig = defaultSmartConfig
  { memoryLimitMB = 64
  , maxTestCount = 50
  }

-- | 极限内存配置
extremeMemoryConfig :: SmartTestConfig
extremeMemoryConfig = defaultSmartConfig
  { memoryLimitMB = 256
  , maxTestCount = 200
  }

-- | CI内存配置
ciMemoryConfig :: SmartTestConfig
ciMemoryConfig = defaultSmartConfig
  { memoryLimitMB = 32
  , maxTestCount = 25
  }

-- | 测试元数据
data TestMetadata = TestMetadata
  { metaTestName :: String
  , metaTestCategory :: TestCategory
  , metaTestPriority :: TestPriority
  , metaTestComplexity :: TestComplexity
  , metaEstimatedMemoryMB :: Int
  , metaIsQuickCheckTest :: Bool
  , metaMaxQuickCheckSize :: Int
  } deriving (Show, Eq, Ord)

-- | 测试选择器
data TestSelector = TestSelector
  { config :: SmartTestConfig
  , testDatabase :: [TestInfo]
  } deriving (Show, Eq)



-- | 根据测试名称分类测试
classifyTest :: String -> TestInfo
classifyTest testName = 
  let category = determineCategory testName
      priority = determinePriority testName
      complexity = determineComplexity testName
      memory = estimateMemory testName
  in TestInfo testName category priority complexity memory ""

-- | 确定测试类别
determineCategory :: String -> TestCategory
determineCategory name
  | "parser" `isInfixOf` name = CoreParser
  | "compiler" `isInfixOf` name = CoreCompiler
  | "utils" `isInfixOf` name = CoreUtils
  | "error" `isInfixOf` name = ErrorHandler
  | "dependency" `isInfixOf` name = DependencyAnalysis
  | "dependent" `isInfixOf` name = DependentTypes
  | "ownership" `isInfixOf` name = Ownership
  | "location" `isInfixOf` name = SourceLocation
  | "integration" `isInfixOf` name = Integration
  | "performance" `isInfixOf` name = Performance
  | "edge" `isInfixOf` name || "boundary" `isInfixOf` name = EdgeCase
  | "regression" `isInfixOf` name = Regression
  | otherwise = Other
  where
    isInfixOf = flip $ \x y -> x `elem` (words (map (\c -> if c == '_' then ' ' else c) y))

-- | 确定测试优先级
determinePriority :: String -> TestPriority
determinePriority name
  | "critical" `isInfixOf` name || "essential" `isInfixOf` name = Critical
  | "core" `isInfixOf` name || "basic" `isInfixOf` name = High
  | "enhanced" `isInfixOf` name || "advanced" `isInfixOf` name = Medium
  | "optional" `isInfixOf` name || "extra" `isInfixOf` name = Low
  | otherwise = Medium
  where
    isInfixOf = flip $ \x y -> x `elem` (words (map (\c -> if c == '_' then ' ' else c) y))

-- | 确定测试复杂度
determineComplexity :: String -> TestComplexity
determineComplexity name
  | "simple" `isInfixOf` name || "basic" `isInfixOf` name = Simple
  | "comprehensive" `isInfixOf` name || "advanced" `isInfixOf` name = VeryComplex
  | "complex" `isInfixOf` name || "integration" `isInfixOf` name = Complex
  | otherwise = Moderate
  where
    isInfixOf = flip $ \x y -> x `elem` (words (map (\c -> if c == '_' then ' ' else c) y))

-- | 估算内存使用
estimateMemory :: String -> Int
estimateMemory name
  | "simple" `isInfixOf` name = 1
  | "basic" `isInfixOf` name = 2
  | "core" `isInfixOf` name = 3
  | "enhanced" `isInfixOf` name = 4
  | "comprehensive" `isInfixOf` name = 8
  | "advanced" `isInfixOf` name = 6
  | "integration" `isInfixOf` name = 10
  | "performance" `isInfixOf` name = 12
  | otherwise = 5
  where
    isInfixOf = flip $ \x y -> x `elem` (words (map (\c -> if c == '_' then ' ' else c) y))

-- | 根据内存选择测试
selectTestsByMemory :: SmartTestConfig -> [TestInfo] -> [TestInfo]
selectTestsByMemory config tests = 
  let memoryLimit = memoryLimitMB config
      suitableTests = filter (\t -> estimatedMemoryMB t <= memoryLimit) tests
  in if null suitableTests
     then take (maxTestCount config) $ sortBy (comparing estimatedMemoryMB) tests
     else take (maxTestCount config) $ sortBy (comparing estimatedMemoryMB) suitableTests

-- | 根据优先级选择测试
selectTestsByPriority :: SmartTestConfig -> [TestInfo] -> [TestInfo]
selectTestsByPriority config tests = 
  let sortedTests = sortBy (comparing testPriority) tests
      criticalTests = filter (\t -> testPriority t == Critical) sortedTests
      otherTests = filter (\t -> testPriority t /= Critical) sortedTests
      maxOther = maxTestCount config - length criticalTests
  in if preserveCriticalTests config
     then criticalTests ++ take maxOther otherTests
     else take (maxTestCount config) sortedTests

-- | 根据类别选择测试
selectTestsByCategory :: SmartTestConfig -> [TestInfo] -> [TestInfo]
selectTestsByCategory config tests = 
  let groupedTests = groupBy ((==) `on` testCategory) $ sortBy (comparing testCategory) tests
      selectedFromEach = concatMap (take 1) groupedTests  -- 每个类别选择1个
      remaining = maxTestCount config - length selectedFromEach
      otherTests = concatMap (drop 1) groupedTests
  in selectedFromEach ++ take remaining otherTests

-- | 根据复杂度选择测试
selectTestsByComplexity :: SmartTestConfig -> [TestInfo] -> [TestInfo]
selectTestsByComplexity config tests = 
  let simpleTests = filter (\t -> testComplexity t == Simple) tests
      moderateTests = filter (\t -> testComplexity t == Moderate) tests
      complexTests = filter (\t -> testComplexity t `elem` [Complex, VeryComplex]) tests
      
      simpleCount = maxTestCount config `div` 2
      moderateCount = maxTestCount config `div` 3
      complexCount = maxTestCount config - simpleCount - moderateCount
  in take simpleCount simpleTests ++ 
     take moderateCount moderateTests ++ 
     take complexCount complexTests

-- | 动态测试选择
dynamicTestSelection :: SmartTestConfig -> [TestInfo] -> [TestInfo]
dynamicTestSelection config tests = 
  let memoryTests = selectTestsByMemory config tests
      priorityTests = selectTestsByPriority config tests
      categoryTests = selectTestsByCategory config tests
      complexityTests = selectTestsByComplexity config tests
      
      -- 合并选择结果，去重
      allSelected = Set.fromList $ concat [memoryTests, priorityTests, categoryTests, complexityTests]
      selectedList = Set.toList allSelected
      
      -- 如果选择的测试太多，按优先级排序后截取
      finalSelected = if length selectedList > maxTestCount config
                     then take (maxTestCount config) $ sortBy (comparing testPriority) selectedList
                     else selectedList
  in finalSelected

-- | 自适应内存选择
adaptiveMemorySelection :: Int -> [TestInfo] -> [TestInfo]
adaptiveMemorySelection availableMemory tests = 
  let config = case availableMemory of
        _ | availableMemory <= 16 -> defaultSmartConfig { memoryLimitMB = 16, maxTestCount = 10 }
        _ | availableMemory <= 32 -> defaultSmartConfig { memoryLimitMB = 32, maxTestCount = 20 }
        _ | availableMemory <= 64 -> defaultSmartConfig { memoryLimitMB = 64, maxTestCount = 50 }
        _ | availableMemory <= 128 -> defaultSmartConfig { memoryLimitMB = 128, maxTestCount = 100 }
        _ -> defaultSmartConfig { memoryLimitMB = 256, maxTestCount = 200 }
  in dynamicTestSelection config tests

-- | 创建平衡的测试套件
createBalancedTestSuite :: SmartTestConfig -> [TestTree] -> TestTree
createBalancedTestSuite config testTrees = 
  let testInfos = map (\t -> classifyTest "unknown") testTrees  -- 简化实现
      selectedInfos = dynamicTestSelection config testInfos
      selectedTests = take (length selectedInfos) testTrees
  in testGroup ("Smart Selected Tests (" ++ show (length selectedTests) ++ "/" ++ show (length testTrees) ++ ")") selectedTests

-- | 测试选择统计
data TestSelectionStats = TestSelectionStats
  { totalTests :: Int
  , selectedTests :: Int
  , memoryUsageMB :: Int
  , categoryDistribution :: Map.Map TestCategory Int
  , priorityDistribution :: Map.Map TestPriority Int
  , complexityDistribution :: Map.Map TestComplexity Int
  } deriving (Show, Eq)

-- | 计算选择统计
calculateSelectionStats :: [TestInfo] -> [TestInfo] -> TestSelectionStats
calculateSelectionStats allTests selectedTests = 
  let total = length allTests
      selected = length selectedTests
      memory = sum $ map estimatedMemoryMB selectedTests
      
      categoryDist = Map.fromListWith (+) 
        [ (testCategory t, 1) | t <- selectedTests ]
      priorityDist = Map.fromListWith (+) 
        [ (testPriority t, 1) | t <- selectedTests ]
      complexityDist = Map.fromListWith (+) 
        [ (testComplexity t, 1) | t <- selectedTests ]
  in TestSelectionStats total selected memory categoryDist priorityDist complexityDist

-- | 打印选择报告
printSelectionReport :: TestSelectionStats -> IO ()
printSelectionReport stats = do
  putStrLn "=== 智能测试选择报告 ==="
  putStrLn $ "总测试数: " ++ show (totalTests stats)
  putStrLn $ "选择测试数: " ++ show (selectedTests stats)
  putStrLn $ "预计内存使用: " ++ show (memoryUsageMB stats) ++ "MB"
  putStrLn $ "选择比例: " ++ show ((fromIntegral (selectedTests stats) / fromIntegral (totalTests stats) * 100) :: Double) ++ "%"
  putStrLn ""
  putStrLn "类别分布:"
  traverse_ (\count -> putStrLn $ "  " ++ show count ++ " tests") (categoryDistribution stats)
  putStrLn ""
  putStrLn "优先级分布:"
  traverse_ (\count -> putStrLn $ "  " ++ show count ++ " tests") (priorityDistribution stats)
  putStrLn ""
  putStrLn "复杂度分布:"
  traverse_ (\count -> putStrLn $ "  " ++ show count ++ " tests") (complexityDistribution stats)

-- | 创建智能测试套件
createSmartTestSuite :: SmartTestConfig -> String -> [(TestTree, TestMetadata)] -> IO TestTree
createSmartTestSuite config name tests = do
  let testInfos = map (\(tree, meta) -> classifyTest (metaTestName meta)) tests
      selectedInfos = dynamicTestSelection config testInfos
      selectedTests = take (length selectedInfos) (map fst tests)
  return $ testGroup (name ++ " (" ++ show (length selectedTests) ++ "/" ++ show (length tests) ++ ")") selectedTests

-- | 运行智能测试
runSmartTests :: SmartTestConfig -> TestTree -> IO ()
runSmartTests config testSuite = do
  putStrLn "=== 运行智能测试套件 ==="
  putStrLn $ "内存限制: " ++ show (memoryLimitMB config) ++ "MB"
  putStrLn $ "测试数量: " ++ show (maxTestCount config)
  defaultMain testSuite

-- | 分析测试覆盖率
analyzeTestCoverage :: [(TestTree, TestMetadata)] -> IO ()
analyzeTestCoverage tests = do
  putStrLn "=== 测试覆盖率分析 ==="
  putStrLn $ "总测试数: " ++ show (length tests)
  
  let categories = map (metaTestCategory . snd) tests
      categoryCounts = Map.fromListWith (+) [(c, 1) | c <- categories]
  
  putStrLn "\n类别分布:"
  traverse_ (\count -> putStrLn $ "  " ++ show count ++ " tests") categoryCounts
  
  let priorities = map (metaTestPriority . snd) tests
      priorityCounts = Map.fromListWith (+) [(p, 1) | p <- priorities]
  
  putStrLn "\n优先级分布:"
  traverse_ (\count -> putStrLn $ "  " ++ show count ++ " tests") priorityCounts
  
  let complexities = map (metaTestComplexity . snd) tests
      complexityCounts = Map.fromListWith (+) [(c, 1) | c <- complexities]
  
  putStrLn "\n复杂度分布:"
  traverse_ (\count -> putStrLn $ "  " ++ show count ++ " tests") complexityCounts
  
  let quickCheckTests = filter (metaIsQuickCheckTest . snd) tests
      totalMemory = sum $ map (metaEstimatedMemoryMB . snd) tests
  
  putStrLn $ "\nQuickCheck测试: " ++ show (length quickCheckTests)
  putStrLn $ "预计总内存使用: " ++ show totalMemory ++ "MB"