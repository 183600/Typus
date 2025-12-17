# 新增 QuickCheck 测试套件

## 概述

为 Typus 项目新增了一个包含 10 个 QuickCheck 属性测试的测试套件。

## 测试文件

- **文件路径**: `test/Test/Unit/NewCabalQuickCheckTestSuite2Spec.hs`
- **模块名称**: `Test.Unit.NewCabalQuickCheckTestSuite2Spec`

## 测试内容

### 1. 核心数据结构属性 (2个测试)

1. **Map.fromList then Map.toList preserves unique keys**
   - 验证 Map 的 fromList 和 toList 操作保持唯一键

2. **Set.union is commutative**
   - 验证 Set 的 union 操作满足交换律

### 2. 高级字符串处理属性 (2个测试)

3. **trim preserves non-whitespace content**
   - 验证 trim 函数保留非空白字符内容

4. **splitBy length equals number of delimiters plus one**
   - 验证 splitBy 函数的分割结果长度等于分隔符数量加一

### 3. 源位置高级属性 (2个测试)

5. **mergeSpans is commutative**
   - 验证 mergeSpans 操作满足交换律

6. **mergeSpans contains both original spans**
   - 验证 mergeSpans 结果包含两个原始 span 的范围

### 4. Set 操作属性 (2个测试)

7. **Set.intersection is commutative**
   - 验证 Set 的 intersection 操作满足交换律

8. **Set.difference is not commutative**
   - 验证 Set 的 difference 操作不满足交换律

### 5. 函数组合属性 (2个测试)

9. **map composition distributes**
   - 验证 map 函数的组合分配律

10. **filter composition is conjunction**
    - 验证 filter 函数的组合等价于逻辑与

## 集成方式

1. 在 `typus.cabal` 文件的 `test-suite typus-test` 部分的 `other-modules` 列表中添加了该模块
2. 在 `test/Test/Unit/Tests.hs` 中导入并注册了该测试模块
3. 测试使用 `fastProperty` 包装器，支持快速测试模式

## 运行测试

```bash
# 运行所有测试
cabal test typus-test

# 仅运行新增的测试套件
cabal test typus-test --test-option="--pattern=New Cabal QuickCheck Test Suite 2"

# 快速测试模式
cabal test typus-test --flags="fast"
```

## 特性

- 使用 QuickCheck 进行属性测试
- 支持快速测试模式（通过 FAST_TESTS CPP 标志）
- 测试覆盖核心数据结构、字符串处理、源位置管理等关键功能
- 所有测试都是纯函数式的，不依赖外部资源
