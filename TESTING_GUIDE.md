# Typus 测试指南

## 快速开始

### 运行所有测试（生产门禁）

```bash
# 最简单的方式 - 运行所有测试
stack test

# 隐藏成功的测试，只显示失败的
stack test --test-arguments='--hide-successes'

# 启用覆盖率报告
stack test --coverage
```

### 如何确认项目可以用于生产？

**只需一个命令：**

```bash
stack test
```

✅ 如果这个命令通过，项目就可以安全地用于生产环境。
❌ 如果这个命令失败，**不要**部署到生产环境。

---

## 测试套件概览

### 测试类型

| 测试类型 | 数量 | 用途 |
|---------|------|------|
| 单元测试 | 55+ | 测试单个函数和模块 |
| 集成测试 | 8 | 测试模块间协作 |
| 端到端测试 | 50+ | 测试完整工作流 |
| CLI测试 | 7 | 测试命令行接口 |
| 性能测试 | 5 | 验证性能阈值 |
| 属性测试 | 300+ | QuickCheck自动生成 |
| **总计** | **425+** | |

### 覆盖的核心模块

- ✅ **Parser** - 21个单元测试（基础功能、指令、边界情况、错误处理、复杂场景）
- ✅ **Compiler** - 19个单元测试（基础编译、指令、边界情况、错误处理、复杂编译、输出验证）
- ✅ **Ownership** - 15个单元测试（基础功能、移动语义、借用检查、边界情况、错误检测）
- ✅ **DependentTypes** - 完整功能测试
- ✅ **CLI Commands** - 所有命令均已测试
- ✅ **Integration** - 端到端流程测试

---

## 运行特定测试

### 1. 仅运行单元测试

```bash
# 运行Parser测试
stack test --test-arguments='-p "Parser Tests"'

# 运行Compiler测试
stack test --test-arguments='-p "Compiler Tests"'

# 运行Ownership测试
stack test --test-arguments='-p "Ownership Tests"'
```

### 2. 仅运行集成测试

```bash
stack test --test-arguments='-p "Integration Tests"'
```

### 3. 仅运行快速测试（跳过慢测试）

```bash
stack test --fast
```

### 4. 运行特定测试用例

```bash
# 运行名称包含"Basic"的测试
stack test --test-arguments='-p "Basic"'

# 运行名称包含"Edge Case"的测试
stack test --test-arguments='-p "Edge Case"'
```

---

## 查看覆盖率报告

### 生成覆盖率

```bash
# 运行测试并生成覆盖率数据
stack test --coverage

# 查看文本覆盖率报告
hpc report typus-test.tix

# 生成HTML覆盖率报告
hpc markup typus-test.tix --destdir=coverage-report

# 打开HTML报告
xdg-open coverage-report/hpc_index.html  # Linux
open coverage-report/hpc_index.html      # macOS
```

### 覆盖率阈值

默认覆盖率阈值是 **70%**。可以通过环境变量自定义：

```bash
# 设置80%的覆盖率阈值
export TYPUS_COVERAGE_THRESHOLD=80
stack test --coverage
```

如果覆盖率低于阈值，测试将失败并阻止部署。

---

## 测试开发指南

### 添加新的单元测试

1. **选择合适的测试文件**：
   - Parser测试 → `test/TestParser.hs`
   - Compiler测试 → `test/TestCompiler.hs`
   - Ownership测试 → `test/TestOwnership.hs`

2. **添加测试用例**：

```haskell
TH.testCase "测试描述" $ do
    let input = ...
    let expected = ...
    let actual = functionUnderTest input
    TH.assertEqual "失败时的消息" expected actual
```

3. **运行测试验证**：

```bash
stack test
```

### 添加新的集成测试

在 `test/IntegrationTests.hs` 或 `test/EndToEndTests.hs` 中添加：

```haskell
TH.testCase "集成测试描述" $ do
    -- 设置测试环境
    -- 执行多个模块的协作
    -- 验证结果
    TH.assertBool "验证消息" condition
```

### 测试最佳实践

1. **独立性**：每个测试应独立运行，不依赖其他测试
2. **可重复性**：同样的输入应产生同样的输出
3. **清晰的失败消息**：当测试失败时，应该清楚地说明原因
4. **测试边界**：不仅测试正常情况，还要测试边界条件和错误情况
5. **简洁性**：每个测试应该只测试一件事情

---

## 性能测试

### 性能阈值

当前性能阈值：

- **解析时间**：< 5秒
- **编译时间**：< 10秒
- **所有权分析**：< 8秒
- **内存使用**：< 512MB

### 运行性能测试

```bash
# 性能测试包含在主测试套件中
stack test --test-arguments='-p "Performance Tests"'
```

### 调整性能阈值

在 `test/PerformanceTests.hs` 中修改阈值常量。

---

## 调试失败的测试

### 1. 查看详细输出

```bash
# 显示所有测试输出
stack test --test-arguments='--verbose'
```

### 2. 运行单个失败的测试

```bash
# 使用测试名称过滤
stack test --test-arguments='-p "失败的测试名称"'
```

### 3. 使用GHCi进行交互式调试

```bash
# 启动测试套件的GHCi
stack ghci typus:test:typus-test

# 设置断点
:break ModuleName 行号

# 运行测试
:main
```

### 4. 检查测试日志

测试输出会显示在终端。对于E2E测试，可能会生成临时文件：

```bash
# 查看临时测试文件
ls test_temp/
ls typus_test_*/
```

---

## 持续集成 (CI/CD)

### GitHub Actions 配置示例

创建 `.github/workflows/test.yml`：

```yaml
name: Typus Tests

on:
  push:
    branches: [ master, develop ]
  pull_request:
    branches: [ master ]

jobs:
  test:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Setup Haskell
      uses: haskell/actions/setup@v2
      with:
        ghc-version: '9.8.4'
        enable-stack: true
        stack-version: 'latest'
    
    - name: Setup Go
      uses: actions/setup-go@v3
      with:
        go-version: '1.25'
    
    - name: Cache Stack dependencies
      uses: actions/cache@v3
      with:
        path: ~/.stack
        key: ${{ runner.os }}-stack-${{ hashFiles('stack.yaml.lock') }}
    
    - name: Build
      run: stack build --fast
    
    - name: Run Tests (Production Gate)
      run: stack test --coverage
    
    - name: Generate Coverage Report
      run: |
        hpc report typus-test.tix
        hpc markup typus-test.tix --destdir=coverage-report
    
    - name: Upload Coverage Report
      uses: actions/upload-artifact@v3
      with:
        name: coverage-report
        path: coverage-report/
```

### GitLab CI 配置示例

创建 `.gitlab-ci.yml`：

```yaml
image: haskell:9.8.4

cache:
  paths:
    - .stack-work/
    - ~/.stack/

stages:
  - test

before_script:
  - apt-get update -qq && apt-get install -y -qq golang-go
  - stack --version

test:
  stage: test
  script:
    - stack build --fast
    - stack test --coverage
    - hpc report typus-test.tix
  artifacts:
    reports:
      coverage_report:
        coverage_format: cobertura
        path: coverage.xml
  coverage: '/\d+\.\d+% expressions used/'
```

---

## 常见问题 (FAQ)

### Q: 测试运行很慢怎么办？

**A**: 使用快速模式：

```bash
stack test --fast
```

或者并行运行测试：

```bash
stack test --test-arguments='+RTS -N4 -RTS'
```

### Q: 如何只运行新添加的测试？

**A**: 使用模式匹配：

```bash
stack test --test-arguments='-p "你的新测试名称"'
```

### Q: 测试通过了，但我怀疑有问题？

**A**: 检查覆盖率和添加更多边界测试：

```bash
stack test --coverage
hpc report typus-test.tix
```

然后查看哪些代码路径没有被测试覆盖。

### Q: 如何测试性能回归？

**A**: 性能测试已内置阈值检查。如果性能下降超过阈值，测试会失败。

### Q: 测试依赖外部工具（如Go编译器）失败怎么办？

**A**: 确保Go编译器已安装且在PATH中：

```bash
go version  # 应显示 1.21 或更高版本
```

### Q: 覆盖率如何提高？

**A**: 
1. 找出未覆盖的代码：`hpc report typus-test.tix`
2. 为未覆盖的代码路径添加测试
3. 重新运行测试验证

---

## 测试维护清单

### 每次提交前

- [ ] 运行 `stack test` 确保所有测试通过
- [ ] 检查新代码是否有对应的测试
- [ ] 确保覆盖率不低于阈值

### 添加新功能时

- [ ] 为新功能添加单元测试
- [ ] 如果涉及多个模块，添加集成测试
- [ ] 如果是用户可见功能，添加E2E测试
- [ ] 更新相关文档

### 修复Bug时

- [ ] 添加回归测试（复现bug的测试）
- [ ] 确保修复后测试通过
- [ ] 验证没有破坏其他功能

### 性能优化时

- [ ] 验证性能测试仍然通过
- [ ] 考虑调整性能阈值（如果有显著改进）
- [ ] 确保所有功能测试仍然通过

---

## 相关文档

- [PRODUCTION_READINESS.md](PRODUCTION_READINESS.md) - 详细的生产就绪性报告
- [README.md](README.md) - 项目总体介绍
- [ChangeLog.md](ChangeLog.md) - 变更历史

---

## 获取帮助

如果遇到测试问题：

1. **查看测试输出**：详细的错误消息通常能说明问题
2. **检查文档**：本文档和 PRODUCTION_READINESS.md
3. **查看测试源码**：`test/` 目录下的测试文件有详细注释
4. **联系团队**：通过项目issue或QQ交流群

---

**记住：只要 `stack test` 通过，项目就可以安全地用于生产环境！**
