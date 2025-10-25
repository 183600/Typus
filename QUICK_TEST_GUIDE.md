# Typus 测试快速指南

## 🚀 快速开始

### 运行所有测试
```bash
# 方式1: 使用 cabal
cabal test

# 方式2: 使用 stack
stack test

# 查看详细输出
cabal test --test-show-details=streaming
```

### 运行测试并生成覆盖率报告
```bash
# 使用 cabal
cabal test --flag typus:coverage

# 使用 stack (推荐)
stack test --coverage

# 生成HTML覆盖率报告
stack test --coverage
stack hpc report --all --destdir=coverage-report
# 然后在浏览器中打开 coverage-report/index.html
```

## 📊 测试模式

### 快速测试 (开发时使用)
```bash
cabal test --flag typus:fast
```
仅运行快速单元测试，适合开发时快速验证。

### 完整测试 (提交前使用)
```bash
cabal test --flag typus:full
```
运行所有测试包括慢速集成测试和性能测试。

### 生产测试 (发布前使用)
```bash
cabal test --flag typus:production
```
启用严格的生产级测试，将警告视为错误。

## 🎯 特定测试套件

### 运行主测试套件
```bash
cabal test typus-test
```

### 运行增强所有权测试
```bash
cabal test enhanced-ownership-tests
```

### 运行基准测试
```bash
cabal bench typus-bench
```

## 🔍 调试测试失败

### 查看详细测试日志
```bash
cabal test --test-show-details=direct

# 或者使用 streaming 模式实时查看
cabal test --test-show-details=streaming
```

### 查看测试日志文件
```bash
# 日志文件位置
cat dist-newstyle/build/x86_64-linux/ghc-*/typus-*/t/typus-test/test/typus-*-typus-test.log
```

### 运行单个测试模块
```bash
# 编译并运行特定测试
cabal run parser-test
cabal run compiler-test
cabal run minimal-compiler-test
cabal run typus-compilation-test
```

## 📝 添加新测试

### 1. 创建测试文件
在 `test/` 目录下创建新的测试模块，例如 `test/MyNewTest.hs`:

```haskell
module MyNewTest (myNewTestSuite) where

import Test.Tasty
import Test.Tasty.HUnit

myNewTestSuite :: TestTree
myNewTestSuite = testGroup "My New Tests" [
    testCase "Test case 1" $ do
        -- 测试代码
        assertEqual "description" expected actual
    ]
```

### 2. 在 typus.cabal 中注册
在 `typus.cabal` 的 `test-suite typus-test` 部分添加:
```cabal
other-modules:
    ...
    MyNewTest
```

### 3. 在 TestSuite.hs 中导入
在 `test/TestSuite.hs` 中导入并运行:
```haskell
import MyNewTest (myNewTestSuite)

-- 在 main 函数中添加测试
```

### 4. 添加测试数据
将测试所需的 `.typus` 文件放在 `test/data/` 目录下。

## 🧪 测试类型示例

### 单元测试
```haskell
testCase "Parser handles simple code" $ do
    let code = "func main() { println(\"hello\") }"
    case Parser.parseTypus code of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right ast -> assertBool "Should parse successfully" True
```

### 属性测试
```haskell
testProperty "Parser roundtrip" $ \code ->
    case Parser.parseTypus code of
        Left _ -> True  -- Invalid input is acceptable
        Right ast -> length (show ast) > 0
```

### 性能测试
```haskell
testCase "Parse performance" $ do
    start <- getCurrentTime
    result <- evaluate $ Parser.parseTypus largeCode
    end <- getCurrentTime
    let duration = diffUTCTime end start
    assertBool "Should complete within 5 seconds" (duration < 5)
```

### 集成测试
```haskell
testCase "End-to-end compilation" $ do
    code <- readFile "test/data/simple_code.typus"
    case Parser.parseTypus code of
        Right ast -> case Compiler.compile ast of
            Right goCode -> assertBool "Should generate Go code" 
                ("package main" `isInfixOf` goCode)
            Left err -> assertFailure $ "Compile failed: " ++ err
        Left err -> assertFailure $ "Parse failed: " ++ err
```

## 📈 检查覆盖率

### 查看覆盖率报告
```bash
# 1. 运行测试并生成覆盖率
stack test --coverage

# 2. 生成HTML报告
stack hpc report --all --destdir=coverage-report

# 3. 在浏览器中打开
firefox coverage-report/index.html  # Linux
open coverage-report/index.html     # macOS
start coverage-report/index.html    # Windows
```

### 设置覆盖率阈值
```bash
# 临时设置阈值为 80%
TYPUS_COVERAGE_THRESHOLD=80 cabal test

# 或在 shell 配置文件中设置
export TYPUS_COVERAGE_THRESHOLD=80
```

## 🐛 常见问题

### 测试失败但不知道原因
```bash
# 使用 streaming 模式查看实时输出
cabal test --test-show-details=streaming

# 查看完整日志
find dist-newstyle -name "*test.log" -exec cat {} \;
```

### 测试数据文件缺失
```bash
# 确保所有必需的测试数据文件存在
ls test/data/

# 必需文件列表在 test/TestSuite.hs 的 requiredFiles 中
```

### 内存不足错误
```bash
# 增加RTS内存限制
cabal test --test-options="+RTS -M8G -RTS"
```

### 测试运行太慢
```bash
# 使用并行测试 (如果支持)
cabal test --test-options="-j4"

# 或只运行快速测试
cabal test --flag typus:fast
```

### HPC覆盖率文件冲突
```bash
# 清理旧的覆盖率文件
rm -f typus-test.tix
rm -rf .hpc/

# 重新运行测试
cabal test --flag typus:coverage
```

## 🔄 持续集成

### 在 CI 中运行测试
```yaml
# GitHub Actions 示例
- name: Run tests
  run: |
    cabal update
    cabal build all
    cabal test --test-show-details=streaming
    
- name: Check coverage
  run: |
    cabal test --flag typus:coverage
    TYPUS_COVERAGE_THRESHOLD=70 cabal test
```

### 在提交前自动运行测试
创建 `.git/hooks/pre-commit`:
```bash
#!/bin/bash
echo "Running tests before commit..."
cabal test --flag typus:fast
if [ $? -ne 0 ]; then
    echo "Tests failed! Commit aborted."
    exit 1
fi
```

## 📚 更多资源

- **完整测试覆盖报告**: 查看 `TEST_COVERAGE_REPORT.md`
- **测试指南**: 查看 `TESTING_GUIDE.md`
- **生产就绪清单**: 查看 `PRODUCTION_READINESS.md`
- **Tasty文档**: https://github.com/UnkindPartition/tasty
- **HUnit文档**: https://hackage.haskell.org/package/HUnit
- **QuickCheck文档**: https://hackage.haskell.org/package/QuickCheck

## ⚡ 性能测试基准

当前性能目标:
- 解析时间: < 5秒 (大文件)
- 编译时间: < 10秒
- 所有权分析: < 8秒
- 内存使用: < 512 MB
- 单元测试: < 1分钟
- 完整测试: < 5分钟

## 💡 最佳实践

1. **开发时**: 使用 `cabal test --flag typus:fast` 快速验证
2. **提交前**: 运行 `cabal test` 确保所有测试通过
3. **发布前**: 运行 `cabal test --flag typus:full --flag typus:production`
4. **定期**: 生成覆盖率报告，确保覆盖率不下降
5. **新功能**: 先写测试，然后实现功能 (TDD)
6. **Bug修复**: 先写重现bug的测试，然后修复

---

**提示**: 保持测试快速、可靠、有意义。好的测试是项目质量的基石！
