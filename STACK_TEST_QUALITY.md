# Stack Test 测试用例质量分析

> 分析日期：2025-11-22（基于 `analysis-stack-test-cases-quality` 分支最新提交）

## 结论概览

- **整体评估：中等偏上** — `stack test` 已经覆盖了解析器、所有权分析、依赖类型桥接以及一部分端到端流程，核心路径能够被回归；但仍存在明显短板（黄金用例稀少、部分模块只有 1~2 个断言、覆盖率数据缺失）。
- **规模现状**：按照 `rg -o "testCase \"" test | wc -l` 统计，共有 **181 个显式 HUnit `testCase`**，`Test/Golden/CompilerSpec.hs` 仅包含 **1 个 golden 对比**。与文档中宣称的 “425+ 个测试” 差异较大。
- **执行范围**：`stack.yaml` 将 `fast:false`、`production:true`、`coverage:true` 写死，使 `stack test` 默认运行单元 + 集成 + golden，并套用 `-Werror`、HPC。入口 `test/Main.hs` 挂载了 `Test.Unit`、`Test.Integration`、`Test.Golden` 三大套件。
- **质量亮点**：解析器、依赖类型、所有权分析和集成分析器测试（例如 `Test/Unit/DependentTypesSpec.hs`、`Test/Integration/FullProjectSpec.hs`、`Test/Integration/AnalyzerSpec.hs`）对指令传播、跨分析器错误以及批处理 CLI 路径都有真实断言。
- **主要风险**：
  1. 覆盖率产物 `coverage-report/summary.json` 显示 **0%**，说明 HPC 流程实际未生效。
  2. QuickCheck 仅注册了 `Test.Dependencies.Arbitrary` 实例，但没有任何 `Test.Tasty.QuickCheck` / `testProperty` 使用，属性测试缺位。
  3. 黄金用例和 CLI 端到端用例数量远低于文档承诺，难以捕获回归。

## stack test 运行范围

- **入口结构**：`test/Main.hs` 中 `testGroup "Typus" [...]` 汇总 `Test.Unit.tests`、`Test.Integration.tests`、`Test.Golden.tests`。`FAST_TESTS` 宏关闭后（即默认 `stack test`），三大套件都会执行。
- **编译标志**：`stack.yaml` 强制 `production:true` -> `typus.cabal` 第 251 行附加 `-DPRODUCTION_TESTS`、`-Werror`；同时 `coverage:true` 会加 `-fhpc`。这确保了 `stack test` 既是功能门禁又带覆盖率钩子。
- **覆盖率数据缺失**：尽管启用了 `-fhpc`，`coverage-report/summary.json` 却保留着 `"total_coverage": "0%"`、`module-report.txt` 也都是 `0/0`。当前仓库没有可用的最新 `.tix`，需要重新跑并上传，才能让“70% 阈值”有依据。

## 测试用例盘点

> 统计口径：`rg -o "testCase \"" <file>`，仅包含真正的 `testCase` 断言；不含 import 行。

| 模块 (Spec) | 类型 | `testCase` 数 | 主要覆盖场景 |
| --- | --- | --- | --- |
| `Test/Unit/DependentTypesSpec.hs` | 单元 | 29 | 依赖类型语法、约束抽取、AST 校验、跨分析器桥接 |
| `Test/Unit/OwnershipSpec.hs` | 单元 | 22 | 移动语义、借用冲突、指令开关、循环作用域 |
| `Test/Unit/UtilsSpec.hs` | 单元 | 26 | 渲染/诊断工具函数、文件系统辅助逻辑 |
| `Test/Unit/ErrorHandlingSpec.hs` | 单元 | 14 | 诊断格式化、错误分级、上下文拼接 |
| `Test/Unit/ParserSpec.hs` | 单元 | 8 | 文件/块级指令、build tag、错误提示 |
| `Test/Unit/TypeSystemSpec.hs` | 单元 | 11 | 类型推断、依赖类型校验、约束合并 |
| `Test/Unit/EmbedAssetsSpec.hs` | 单元 | 6 | 资源嵌入、路径解析、strict-embed 标志 |
| `Test/Unit/VerbositySpec.hs` | 单元 | 4 | CLI verbosity 级别与 logger 行为 |
| `Test/Unit/ValueAnalysisSpec.hs` | 单元 | 1 | Go AST 值类型 vs 引用类型分类 |
| `Test/Unit/SymbolTableSpec.hs` | 单元 | 3 | 符号注册、作用域重影 |
| `Test/Unit/OwnershipBridgeSpec.hs` | 单元 | 1 | 所有权桥接到依赖类型分析的出口 |
| `Test/Unit/CompilerSpec.hs` | 单元 | 5 | 代码生成 happy-path、依赖类型错误冒泡、诊断串联 |
| `Test/Unit/CommandLineDebugSpec.hs` | 单元 | 2 | `--debug` / `--emit-ast` 解析 |
| `Test/Unit/CLISpec.hs` | 单元 | 11 | convert/check/build/run 子命令及 `--strict-embed` 传递 |
| `Test/Integration/AnalyzerSpec.hs` | 集成 | 20 | 依赖类型与所有权联动、CrossAnalyzer 错误、编译器集成结果 |
| `Test/Integration/FullProjectSpec.hs` | 集成 | 6 | `batchConvert`/`batchCheck`、Go 执行器调用、错误冒泡 |
| `Test/Integration/IntegratedCompilerSpec.hs` | 集成 | 4 | 综合编译器输出、错误过滤、配置项 |
| `Test/Integration/OwnershipSpec.hs` | 集成 | 5 | 所有权分析器与 Go AST 的端到端路径 |
| `Test/Integration/PipelineSpec.hs` | 集成 | 3 | Parser→Compiler→Go build 基线、复杂所有权 fixture、指令传播 |
| `Test/Golden/CompilerSpec.hs` | Golden | 1 (goldenVsStringDiff) | `simple_go_code.typus` → `simple_go_code.go` 逐字节对拍 |

> 其余模块（例如 `TestSupport/*`、`Test/Golen.hs` 等）仅承担装配职责，不含断言。

## 质量观察

### 优势

1. **关键语义覆盖深度高**：
   - `Test/Unit/DependentTypesSpec.hs`（约 400 行）不仅验证 parser，还验证 `Analyzer.DependentTypeBridge`、`Dependencies.TypeSystem`，确保复杂 where 子句、type class 约束、范围约束均能被抽取并传入约束求解。
   - `Test/Unit/OwnershipSpec.hs` 构造了 20+ 个 Go 代码片段，覆盖 use-after-move、mutable/immutable borrow 冲突、指令级开关、loop 作用域与 built-in 函数（`len`、`fmt.Println`）的例外情况。
2. **跨子系统端到端验证**：`Test/Integration/FullProjectSpec.hs` 使用真实的 `test/fixtures/full_project`，对 `batchConvert`、`batchCheck`、Go 执行器跳板（记录命令/跳过/失败三种 executor）都做了断言，能捕捉 CLI 工作流的回归。
3. **综合分析器保障**：`Test/Integration/AnalyzerSpec.hs` 20 个测试覆盖了 `newIntegratedAnalyzer` 在 ownership + dependent types 联动、CrossAnalyzer 错误聚合、severity 传播、compiler config 过滤 warning 等多种路径，是真正的“分析栈”质量底座。
4. **CLI 参数健壮性**：`Test/Unit/CLISpec.hs` 覆盖 convert/check/build/run 正常/forwarding/严格模式组合，保证 `stack test` 下 CLI 参数解析不会回归。

### 缺口与风险

1. **覆盖率数据缺失**：HPC 报告 (`coverage-report/summary.json`, `module-report.txt`) 为 0/0，意味着 CI 并未上传任何真实 `.tix`，无法验证 70% 目标是否达成。
2. **黄金用例过少**：`Test/Golden/CompilerSpec.hs` 只覆盖 `simple_go_code` 一种 happy-path。没有针对指令、依赖类型或错误输出的 golden，对生成代码的回归保护力有限。
3. **属性测试宣称与现实不符**：虽然 `typus.cabal` 在 production/full 模式下拉入 `QuickCheck`，`Test.Dependencies.Arbitrary` 也提供 `Arbitrary` 实例，但整个 `test/` 目录没有任何 `testProperty` 或 `Test.Tasty.QuickCheck` 引用。文档中“300+ QuickCheck 测试” 的说法无法兑现。
4. **模块覆盖极不均衡**：
   - `ValueAnalysisSpec`、`OwnershipBridgeSpec`、`CommandLineDebugSpec` 分别只有 1/1/2 个断言，很难覆盖复杂分支。
   - 集成层的 `PipelineSpec` 只有 3 个用例，且 `assertGoBuilds` 在缺少 Go 工具链时直接 `assertBool True`，会悄然跳过真正的验证。
5. **文档与现实错位**：`TEST_ENHANCEMENT_SUMMARY.md` 等文件宣称 425+ 测试、90+% 覆盖等，但通过代码统计只能看到 ~182 个 HUnit 用例和 1 个 golden。依赖这些文档做质量背书会产生误导。
6. **缺少真实 CLI/E2E 命令执行**：现有集成测试直接调库（例如 `Compiler.batchConvert`），没有 `typus` 可执行文件的子进程级测试，也没有对 `typus run` / `typus build` 的系统调用验证。

## 改进建议（按优先级）

1. **恢复覆盖率产物**：在 CI 中重新运行 `stack test --coverage`，确保 `.tix` 上传并更新 `coverage-report/`；将真实覆盖率写回 `summary.json`，并设置失败阈值。
2. **扩充黄金 & fixture**：为所有权、依赖类型、错误信息分别生成至少 3 个 golden，用 `tasty-golden` 验证输出稳定性；同时把现有 `test/data/*.typus`、`fixtures/full_project` 的关键场景转换成 golden。
3. **真正引入 QuickCheck**：利用 `Test.Dependencies.Arbitrary`，在 `Test/Unit/DependentTypesSpec.hs` 和 `Test/Unit/TypeSystemSpec.hs` 中添加 `testProperty`，覆盖更多组合；必要时把属性测试放到 `PRODUCTION_TESTS` guard 下。
4. **补齐薄弱模块**：为 `ValueAnalysis`, `OwnershipBridge`, `CommandLineDebug` 添加失败路径和边界测试（例如无效 flag、缺少 debug 文件、复杂 AST graph）。
5. **增加 CLI 端到端测试**：通过 `System.Process` 调用 `stack run typus -- ...` 或直接运行 `typus` 可执行体，对 `convert/check/build/run` 做黑盒验证。可使用 `withSystemTempDirectory` 隔离输出。
6. **强化 `PipelineSpec`**：即使在无 Go 工具链的 CI，也可以通过注入 fake go 命令或使用 `-fake` 环境，确保“成功构建/失败信息” 都真正断言。

## 数据来源与可复现性

- 断言统计：`rg -o "testCase \"" test/Test/**/*.hs`。
- 入口与标志：`test/Main.hs`，`stack.yaml` (flags)、`typus.cabal` (test-suite typus-test 部分)。
- 覆盖率产物：`coverage-report/summary.json`、`coverage-report/module-report.txt`。
- 关键示例：
  - `Test/Unit/ParserSpec.hs` 26-178 行 — 指令解析 & 错误提示。
  - `Test/Integration/FullProjectSpec.hs` 35-176 行 — `batchConvert/batchCheck` 全流程。
  - `Test/Integration/AnalyzerSpec.hs` 36-400+ 行 — Integrated Analyzer 错误传播。

> 只要以上缺口得到补齐，并保持 `stack test` 为唯一门禁，这套测试体系即可提供可验证的生产级保障。