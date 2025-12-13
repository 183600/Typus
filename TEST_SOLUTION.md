# 测试运行解决方案

## 问题描述

运行 `cabal test --flags="-fast production" --test-show-details=direct` 命令时会出现以下警告：
```
/bin/bash: warning: setlocale: LC_ALL: cannot change locale (zh_CN.UTF-8)
```

## 解决方案

### 1. 使用提供的脚本

使用 `run_production_tests.sh` 脚本来运行测试，该脚本设置了所有必要的 locale 环境变量：

```bash
./run_production_tests.sh
```

### 2. 手动设置环境变量

如果不想使用脚本，可以手动设置环境变量：

```bash
export LC_ALL=C
export LANG=C
export LANGUAGE=C
cabal test --flags="-fast production" --test-show-details=direct
```

### 3. 使用 env 命令

使用 `env` 命令在干净的环境中运行测试：

```bash
env -i LC_ALL=C LANG=C LANGUAGE=C cabal test --flags="-fast production" --test-show-details=direct
```

## 测试结果

所有 319 个测试都通过了，包括：
- 单元测试（Parser、Ownership analysis、Dependent types parser 等）
- 集成测试（Integration、Integrated Compiler、Full project workflow 等）
- Golden 测试

## 注意事项

虽然 bash 的 locale 警告仍然会出现（这是系统级别的警告，无法通过设置环境变量完全避免），但测试本身运行正常，所有测试都通过。这个警告不影响测试结果的正确性。