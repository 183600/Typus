# Typus 极度内存优化测试指南

本指南提供了确保测试用例不会消耗大量内存的完整解决方案，专注于在不删除测试用例的情况下优化内存使用。

## 概述

Typus项目现在包含了一套完整的内存优化基础设施，包括：

1. **极度内存优化的QuickCheck配置**
2. **自适应内存管理系统**
3. **综合内存清理机制**
4. **多层次内存监控**
5. **动态内存调整策略**

## 快速开始

### 1. 使用极度增强的内存测试脚本

```bash
# 自动检测并选择合适的内存级别
./scripts/run_extreme_enhanced_memory_tests.sh --auto

# 使用特定的内存级别
./scripts/run_extreme_enhanced_memory_tests.sh minimal

# 启用自适应内存管理
./scripts/run_extreme_enhanced_memory_tests.sh --adaptive

# 启用内存监控
./scripts/run_extreme_enhanced_memory_tests.sh --monitor
```

### 2. 内存级别说明

| 级别 | 内存限制 | QuickCheck测试次数 | 适用场景 |
|------|----------|-------------------|----------|
| critical | 8MB | 1 | CI/CD环境，极端内存限制 |
| minimal | 16MB | 2 | 低配置开发环境 |
| ultra | 24MB | 3 | 内存受限环境 |
| enhanced | 32MB | 5 | 标准开发环境 |
| optimized | 48MB | 10 | 性能测试环境 |
| standard | 64MB | 25 | 完整测试环境 |

## 核心组件

### 1. 极度QuickCheck内存优化 (`TestSupport.ExtremeQuickCheckMemoryOptimization`)

提供极度内存优化的QuickCheck配置：

```haskell
import TestSupport.ExtremeQuickCheckMemoryOptimization

-- 使用关键内存配置
prop_my_test :: String -> Property
prop_my_test s = criticalMemoryProperty "my test" myProperty genSmallString

-- 自定义内存配置
prop_custom_test :: [Int] -> Property
prop_custom_test xs = 
  let config = extremeMemoryConfig { maxTestCount = 2, maxTestSize = 1 }
  in extremeMemoryProperty config "custom test" myProperty (genExtremeSmallList config genSmallInt)
```

### 2. 统一自适应内存优化 (`TestSupport.UnifiedAdaptiveMemoryOptimization`)

基于系统资源的动态内存管理：

```haskell
import TestSupport.UnifiedAdaptiveMemoryOptimization

-- 自动检测系统资源并创建配置
main = do
  config <- getRecommendedMemoryConfig
  runWithAdaptiveMemory myTestSuite

-- 创建自适应测试套件
adaptiveSuite <- adaptiveTestSuite "My Tests" [test1, test2, test3]
```

### 3. 综合内存清理 (`TestSupport.ComprehensiveMemoryCleanup`)

多层次的内存清理策略：

```haskell
import TestSupport.ComprehensiveMemoryCleanup

-- 测试间自动清理
withCleanupBetweenTests defaultCleanupStrategy $ do
  runMyTest

-- 自适应清理
withAdaptiveCleanup aggressiveCleanupStrategy $ do
  runLongRunningTest

-- 紧急清理
emergencyCleanup
```

### 4. 统一极度内存测试运行器 (`TestSupport.UnifiedExtremeMemoryTestRunner`)

集成的内存优化测试执行：

```haskell
import TestSupport.UnifiedExtremeMemoryTestRunner

-- 运行极度内存优化测试
main = runExtremeMemoryTests myTestSuite

-- 运行自适应内存测试
main = runAdaptiveMemoryTests myTestSuite

-- 使用特定内存级别
main = runConfigurableMemoryTests Minimal myTestSuite
```

## 环境变量配置

### 内存管理环境变量

```bash
# 设置内存级别
export TYPUS_MEMORY_LEVEL=minimal

# 启用内存监控
export TYPUS_MEMORY_MONITOR=true

# 启用自适应内存管理
export TYPUS_ADAPTIVE_MEMORY=true

# 启用紧急模式
export TYPUS_EMERGENCY_MODE=true

# 跳过Go构建以节省内存
export TYPUS_SKIP_GO_BUILD=1

# 设置清理策略
export TYPUS_CLEANUP_STRATEGY=aggressive

# 强制垃圾回收
export TYPUS_FORCE_GC=true
```

### GHC RTS选项

系统会根据内存级别自动设置RTS选项：

- **Critical**: `-M8m -A1m -n128k -H1m -qg -G1`
- **Minimal**: `-M16m -A2m -n256k -H2m -qg -G1`
- **Ultra**: `-M24m -A3m -n512k -H3m -qg -G1`
- **Enhanced**: `-M32m -A4m -n1m -H4m -qg -G1`
- **Optimized**: `-M48m -A8m -n2m -H6m -qg -G1`
- **Standard**: `-M64m -A16m -n4m -H8m -qg -G1`

## 集成到现有测试

### 1. 更新现有测试模块

在现有的QuickCheck测试中添加内存优化：

```haskell
-- 原始测试
prop_trim_basic :: String -> Property
prop_trim_basic s = trim (trim s) === trim s

-- 内存优化版本
prop_trim_basic :: String -> Property
prop_trim_basic s = 
  let limitedString = take 1 s  -- 限制输入大小
  in criticalMemoryProperty "trim basic" 
        (\s' -> trim (trim s') === trim s') 
        (return limitedString)
```

### 2. 使用内存优化的生成器

```haskell
import TestSupport.ExtremeQuickCheckMemoryOptimization

-- 使用内存优化的生成器
prop_my_list_test :: [Int] -> Property
prop_my_list_test xs = 
  let config = minimalMemoryConfig
  in extremeMemoryProperty config "list test" 
        myProperty 
        (genExtremeSmallList config genExtremeSmallInt)
```

### 3. 添加清理机制

```haskell
import TestSupport.ComprehensiveMemoryCleanup

-- 在测试套件中添加清理
myTestSuite :: TestTree
myTestSuite = testGroup "My Tests" 
  [ withCleanupBetweenTests defaultCleanupStrategy $ 
      testProperty "test1" prop_test1
  , withCleanupBetweenTests defaultCleanupStrategy $ 
      testProperty "test2" prop_test2
  ]
```

## 最佳实践

### 1. 测试设计原则

- **限制输入大小**: 使用`take`限制字符串和列表长度
- **简化数据结构**: 使用最小的数据表示
- **避免递归**: 优先使用迭代而非递归
- **及时清理**: 在测试间执行内存清理

### 2. 内存优化技巧

```haskell
-- ✅ 好的做法：限制输入大小
prop_good_test :: String -> Property
prop_good_test s = 
  let limitedS = take 3 s
  in property $ length limitedS <= 3

-- ❌ 避免的做法：无限制输入
prop_bad_test :: String -> Property
prop_bad_test s = property $ length s >= 0  -- 可能消耗大量内存
```

### 3. 批量测试优化

```haskell
-- 使用批量执行控制
executeBatchWithMemoryControl config tests
  where config = createExtremeTestConfig Minimal
```

## 监控和调试

### 1. 内存监控

```bash
# 启用详细内存监控
./scripts/run_extreme_enhanced_memory_tests.sh --monitor --verbose

# 检查内存使用情况
export TYPUS_MEMORY_MONITOR=true
cabal test
```

### 2. 调试内存问题

```haskell
-- 使用调试模式
import TestSupport.EnhancedMemoryOptimization

debugMemoryUsage :: IO ()
debugMemoryUsage = do
  monitorMemoryUsage
  runMyTest
  monitorMemoryUsage
```

## 故障排除

### 1. 内存不足错误

如果遇到内存不足错误：

1. 降低内存级别：`critical` → `minimal`
2. 启用自适应模式：`--adaptive`
3. 增加清理频率：设置`TYPUS_FORCE_GC=true`
4. 使用紧急模式：`export TYPUS_EMERGENCY_MODE=true`

### 2. 测试失败

如果测试因内存限制失败：

1. 检查测试是否使用了过大的输入
2. 减少QuickCheck测试次数
3. 增加内存清理频率
4. 使用更激进的清理策略

### 3. 性能问题

如果测试运行过慢：

1. 调整GC频率
2. 减少监控开销
3. 优化清理策略
4. 使用适当的内存级别

## 高级配置

### 1. 自定义内存配置

```haskell
-- 创建自定义内存配置
customConfig :: ExtremeMemoryConfig
customConfig = ExtremeMemoryConfig
  { maxTestSize = 2
  , maxTestCount = 3
  , maxShrinks = 0
  , stringMaxLength = 2
  , listMaxLength = 1
  , intMaxValue = 5
  , gcBetweenTests = True
  , monitorMemory = False
  , adaptiveMode = False
  }
```

### 2. 自定义清理策略

```haskell
-- 创建自定义清理策略
customCleanupStrategy :: CleanupStrategy
customCleanupStrategy = CleanupStrategy
  { gcRounds = 7
  , gcDelay = 25
  , systemCacheCleanup = True
  , haskellHeapCleanup = True
  , forceFinalization = True
  , memoryCompaction = True
  , monitoringEnabled = True
  }
```

## 总结

通过使用这套完整的内存优化基础设施，Typus项目可以：

1. **在不删除测试用例的情况下显著减少内存使用**
2. **根据系统资源自动调整内存配置**
3. **提供多层次的内存清理机制**
4. **支持实时内存监控和调整**
5. **确保在各种内存限制环境下都能正常运行测试**

这些优化措施使得Typus项目能够在内存受限的环境中（如CI/CD管道、低配置开发机等）稳定运行，同时保持完整的测试覆盖率。