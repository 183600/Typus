# Typus 调试功能使用指南

## 概述

Typus 编译器现在提供了强大的调试功能，包括：

1. **调试日志记录** - 不同级别的日志输出
2. **命令行断点调试** - 在关键位置设置断点
3. **交互式调试模式** - 实时控制程序执行
4. **性能监控** - 执行时间和统计信息

## 启动调试模式

### 交互式调试模式

```bash
# 进入交互式调试模式
./dist-newstyle/build/x86_64-linux/ghc-9.6.3/typus-0.12.0/x/typus/build/typus/typus debug
```

### 命令行调试参数

```bash
# 显示调试帮助
./dist-newstyle/build/x86_64-linux/ghc-9.6.3/typus-0.12.0/x/typus/build/typus/typus debug help

# 设置断点
./dist-newstyle/build/x86_64-linux/ghc-9.6.3/typus-0.12.0/x/typus/build/typus/typus debug breakpoint set Parser.parseTypus

# 列出断点
./dist-newstyle/build/x86_64-linux/ghc-9.6.3/typus-0.12.0/x/typus/build/typus/typus debug breakpoint list

# 设置日志级别
./dist-newstyle/build/x86_64-linux/ghc-9.6.3/typus-0.12.0/x/typus/build/typus/typus debug log level debug
```

## 调试命令参考

### 断点命令

- `breakpoint set <location>` - 在指定位置设置断点
- `bp set <location>` - 设置断点（简写）
- `breakpoint list` - 列出所有断点
- `bp list` - 列出断点（简写）
- `breakpoint clear` - 清除所有断点
- `bp clear` - 清除断点（简写）

### 日志命令

- `log level <level>` - 设置日志级别（debug/info/warning/error）
- `log debug` - 启用调试跟踪
- `log info` - 设置信息级别（禁用调试跟踪）
- `trace on` - 启用跟踪
- `trace off` - 禁用跟踪

### 其他命令

- `stats` - 显示调试统计信息
- `run <filename>` - 运行文件并启用调试
- `help` - 显示帮助信息
- `h` - 显示帮助信息（简写）
- `exit` / `quit` / `q` - 退出调试模式

## 常用调试位置

以下是一些常用的断点位置：

- `Parser.parseTypus` - 解析器入口点
- `Compiler.compile` - 编译器入口点
- `Ownership.analyze` - 所有权分析入口点
- `DependentTypesParser.parseDependentType` - 依赖类型解析
- `TypeSystem.checkType` - 类型检查

## 调试示例

### 示例 1：基本调试

```bash
# 启动调试模式
./dist-newstyle/build/x86_64-linux/ghc-9.6.3/typus-0.12.0/x/typus/build/typus/typus debug

# 在调试模式中
debug> breakpoint set Parser.parseTypus
debug> breakpoint set Compiler.compile
debug> log level debug
debug> run debug_example.typus
```

### 示例 2：性能分析

```bash
# 启动调试模式
./dist-newstyle/build/x86_64-linux/ghc-9.6.3/typus-0.12.0/x/typus/build/typus/typus debug

# 在调试模式中
debug> log level debug
debug> run large_file.typus
debug> stats
```

## 调试输出说明

### 断点命中

当断点被命中时，会显示：

```
=== REGULAR BREAKPOINT ===
Location: Parser.parseTypus
Function stack:
  Compiler.compile
  main
Execution count: 1

Breakpoint commands:
  c, continue - Continue execution
  s, stack - Show function stack
  i, info - Show debug info
  t, trace - Enable/disable tracing
  h, help - Show help
  q, quit - Quit program
```

### 调试统计

```
=== Debug Statistics ===

Execution Counts:
  Parser.parseTypus: 1
  Compiler.compile: 1

Timings:
  Parser.parseTypus: 0.123s
  Compiler.compile: 0.456s

Log Counts:
  Debug: 15
  Info: 8
  Warning: 2
  Error: 0
```

## 集成到开发工作流

### 1. 日常开发

```bash
# 开发时启用调试日志
./typus debug log level debug
./typus check your_file.typus
```

### 2. 问题诊断

```bash
# 遇到问题时设置断点
./typus debug breakpoint set Parser.parseTypus
./typus debug run problematic_file.typus
```

### 3. 性能优化

```bash
# 分析性能瓶颈
./typus debug log level debug
./typus debug run large_file.typus
./typus debug stats
```

## 注意事项

1. **性能影响**：启用调试功能会影响性能，仅在开发时使用
2. **日志级别**：合理设置日志级别，避免过多输出
3. **断点位置**：确保断点位置正确，避免在无效位置设置断点
4. **交互模式**：交互式调试模式适合深度调试，命令行参数适合快速检查

## 故障排除

### 问题：调试模式无法启动

**解决方案**：
1. 确保项目正确构建：`cabal build`
2. 检查可执行文件路径
3. 确保有足够的权限

### 问题：断点不生效

**解决方案**：
1. 检查断点位置是否正确
2. 确保代码路径经过断点位置
3. 使用 `breakpoint list` 检查断点是否正确设置

### 问题：日志输出过多

**解决方案**：
1. 调整日志级别：`log level info`
2. 使用 `log info` 禁用调试跟踪
3. 仅在必要时启用调试日志

## 扩展调试功能

### 添加自定义断点

在代码中添加自定义断点：

```haskell
import EnhancedDebug

-- 在函数中添加断点
myFunction args = do
    debugConfig <- defaultEnhancedDebugConfig
    checkAndHandleBreakpoint debugConfig "myFunction"
    -- 函数逻辑
```

### 添加自定义日志

```haskell
import EnhancedDebug

-- 添加调试日志
myFunction args = do
    debugConfig <- defaultEnhancedDebugConfig
    logDebug debugConfig "Entering myFunction"
    -- 函数逻辑
    logDebug debugConfig "Exiting myFunction"
```

## 总结

Typus 的调试功能提供了强大的工具来帮助开发者理解程序行为、诊断问题和优化性能。通过合理使用断点、日志和统计信息，可以大大提高开发效率。