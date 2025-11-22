# Stack Test 测试用例质量分析

> 分析日期：2025-11-22（基于 `analysis-stack-test-quality` 分支最新提交）

## 结论概览

- **整体评估：中等偏上** — `stack test` 覆盖了解析器、所有权分析、依赖类型桥接、管线编译器以及 CLI 端到端烟囱路径，主干逻辑具备回归保障；但覆盖率产物缺失、黄金用例和部分模块的断言仍偏少，导致信心区间受限。
- **规模现状**：`rg -o "testCase \"" test | wc -l` 统计得到 **183 个显式 HUnit `testCase`**；`Test/Golden/CompilerSpec` 中有 **3 个 tasty-golden 对拍**；在 `PRODUCTION_TESTS`/`FULL_TESTS` 变体下还会运行 **3 个 QuickCheck `testProperty`**（分别位于 `Test.Unit.DependentTypesSpec` 与 `Test.Unit.TypeSystemSpec`）。
- **执行范围**：`stack.yaml` 将 `fast:false`、`production:true`、`coverage:true`、`werror:true` 写死，因此 `stack test` 默认挂载单元 + 集成 + golden，并在 `typus.cabal` 中套用 `-DPRODUCTION_TESTS`、`-fhpc`、`-Werror`。入口 `test/Main.hs` 汇总了 `Test.Unit`、`Test.Integration`、`Test.Golden` 三大套件。
- **质量亮点**：解析/依赖类型/所有权的深度断言、`Test.Integration.AnalyzerSpec` 的跨分析器校验、`Test.Integration.FullProjectSpec` 的全项目批处理路径、`Test.Integration.CLISpec` 对真实 `stack exec typus` 的冒烟验证，都能捕捉常见回归。
- **主要风险**：覆盖率产物仍为空、黄金/CLI 用例集中在 happy-path、`ValueAnalysis` 等模块只有 1~2 个断言，且仓库文档宣称的 “400+ 测试 & 90%+ 覆盖” 与现状不符。

## stack test 运行范围

1. **入口与结构**：`test/Main.hs` 通过 `testGroup "Typus" [...]` 汇总 `Test.Unit.tests`、`Test.Integration.tests`、`Test.Golden.tests`；只要 `FAST_TESTS` 未定义（默认情况），所有单元、集成、golden、CLI、QuickCheck 测试都会执行。
2. **编译与标志**：`stack.yaml` 的 flags `production:true` + `werror:true` 触发 `typus.cabal` 第 255 行的 `-DPRODUCTION_TESTS -Werror`，同时 `coverage:true` 追加 `-fhpc`。由此 `stack test` 既是功能门禁，又会开启覆盖率采集。
3. **覆盖率数据缺失**：`coverage-report/summary.json` 与 `module-report.txt` 仍显示 “未生成”。仓库没有任何 `.tix` 与模块级覆盖率，意味着目前无法验证“70% 覆盖率”类指标，且 CI 中的 `coverage-report` 目标实际上是空洞的。

## 测试用例盘点（HUnit testCase 统计）

| 模块 (Spec) | 类型 | `testCase` 数 | 主要覆盖场景 |
| --- | --- | --- | --- |
| `Test/Unit/DependentTypesSpec.hs` | 单元 | 29 | 依赖类型语法、AST 校验、跨分析器桥接；在 PRODUCTION/FULL 下额外运行 1 个 QuickCheck 属性测试确保生成程序无误 |
| `Test/Unit/OwnershipSpec.hs` | 单元 | 22 | 移动语义、借用冲突、loop 作用域、指令开关、内置函数例外 |
| `Test/Unit/UtilsSpec.hs` | 单元 | 26 | 诊断渲染、文件系统工具、路径解析、日志/格式化辅助函数 |
| `Test/Unit/ErrorHandlingSpec.hs` | 单元 | 14 | 诊断分级、错误上下文拼接、Formatter 行为 |
| `Test/Unit/ParserSpec.hs` | 单元 | 8 | 文件/块级指令、build tag、错误提示、directives 传播 |
| `Test/Unit/TypeSystemSpec.hs` | 单元 | 11 | 约束抽取、类型推断、函数返回 refine；PRODUCTION/FULL 再运行 2 个 QuickCheck 属性测试覆盖约束求解 |
| `Test/Unit/EmbedAssetsSpec.hs` | 单元 | 6 | 资源嵌入、路径规范化、`--strict-embed` 行为 |
| `Test/Unit/VerbositySpec.hs` | 单元 | 4 | CLI verbosity 与 logger 行为 | 
| `Test/Unit/ValueAnalysisSpec.hs` | 单元 | 1 | Go AST 值/引用分类（单一、宏大的用例） |
| `Test/Unit/SymbolTableSpec.hs` | 单元 | 3 | 符号注册、作用域重影、可变/不可变标记 |
| `Test/Unit/OwnershipBridgeSpec.hs` | 单元 | 1 | 所有权错误到符号表的同步/过滤 |
| `Test/Unit/CompilerSpec.hs` | 单元 | 5 | 编译器 happy-path、诊断串联、错误冒泡 |
| `Test/Unit/CommandLineDebugSpec.hs` | 单元 | 2 | `--debug` / `--emit-ast` 标志解析 |
| `Test/Unit/CLISpec.hs` | 单元 | 11 | `convert`/`check`/`build`/`run` 子命令与 `--strict-embed` 透传、`--version` |
| `Test/Integration/AnalyzerSpec.hs` | 集成 | 20 | 所有权 & 依赖类型联动、CrossAnalyzer 错误聚合、severity 传播、CompilerConfig 过滤 |
| `Test/Integration/FullProjectSpec.hs` | 集成 | 6 | `batchConvert`/`batchCheck`、Go 执行器注入、syntax/go build 失败场景 |
| `Test/Integration/IntegratedCompilerSpec.hs` | 集成 | 4 | Integrated compiler 输出、过滤后的 diagnostics、配置项 |
| `Test/Integration/OwnershipSpec.hs` | 集成 | 5 | 所有权分析器在真实 Typus 片段上的端到端行为 |
| `Test/Integration/PipelineSpec.hs` | 集成 | 3 | Parser→Compiler→Go build（`assertGoBuilds` 如缺 Go 会直接 fail）、复杂 directive 传播 |
| `Test/Integration/CLISpec.hs` | 集成 | 2 | `stack exec typus --version` 及 `convert` 的真实 CLI 冒烟（使用临时目录和 `TYPUS_SKIP_GO_BUILD`） |
| `Test/Golden/CompilerSpec.hs` | Golden | 3 | `simple_go_code`、`statements_without_package`、`generic_type` 三个 fixture 的源码对拍 |

> QuickCheck 属性测试目前仅在 `Test/Unit/DependentTypesSpec.hs`（1 个）和 `Test/Unit/TypeSystemSpec.hs`（2 个）出现，并受 `PRODUCTION_TESTS`/`FULL_TESTS` 宏控制 —— 默认的 `stack test` 会运行它们，但 `fast` 配置会全部跳过。

## 质量观察

### 优势
1. **深度语义验证**：`Test/Unit/DependentTypesSpec.hs` 与 `Test/Unit/OwnershipSpec.hs` 均以真实 Typus 代码片段驱动，覆盖 type range、subtype、move/borrow 错误以及跨分析器桥接。属性测试进一步保证依赖类型分析与约束求解不会对生成 AST 误报。
2. **跨分析器与编译器联动**：`Test/Integration/AnalyzerSpec.hs`（20 个测试）对 `newIntegratedAnalyzer`、`analysisToCombined`、`compileWithIntegratedAnalyzers` 的行为进行了全面断言，包括 severity 传播、CrossAnalyzerError 聚合、CompilerConfig 对 warning 的过滤。
3. **端到端批处理与 CLI 实测**：`Test/Integration/FullProjectSpec.hs` 通过 `fixtures/full_project` 验证 `batchConvert`/`batchCheck`、Go executor 三态（recording/skipping/failing）；`Test/Integration/CLISpec.hs` 则使用 `stack exec typus` 驱动真实 CLI，并在临时目录中检查输出文件。
4. **Golden & Go toolchain 绑定**：`Test/Golden/CompilerSpec.hs` 现有三份 fixture，`Test/Integration/PipelineSpec.hs` 的 `assertGoBuilds` 会在缺失 Go 工具链时直接 fail（除非显式提供 `TYPUS_FAKE_GO` 或 `scripts/fake-go.sh`），确保“生成的 Go 能构建”不是空断言。

### 缺口与风险
1. **覆盖率报告仍为空**：虽然 `-fhpc` 已启用，但 `coverage-report/summary.json` 只写着 “Coverage data has not been generated yet.”，CI 无法据此评估覆盖率阈值，HPC 产物也无法支持变更评审。
2. **模块覆盖不均**：`ValueAnalysisSpec`、`OwnershipBridgeSpec`、`CommandLineDebugSpec` 只有 1~2 个断言；类似 `Analyzer.State`、`GoToolchain` 等关键组件没有独立单元测试。任何边界分支（多线程、错误路径）都缺少保护。
3. **Golden 仍是 happy-path**：三个 golden fixture 全是成功编译路径，缺少对错误信息、指令注释、复杂泛型/所有权语义的稳定性校验。代码生成器的负面/边界回归难以及早发现。
4. **CLI/E2E 覆盖有限且依赖外部工具链**：`Test/Integration/CLISpec.hs` 仅验证 `--version` 与 `convert`，没有覆盖 `check/build/run`、错误处理、退出码。并且 CLI/Pipeline 测试要求主机具备 `stack` 与 `go`（或提供假可执行文件），在沙箱或缓存失效时会导致测试易碎。
5. **文档与现实脱节**：`TEST_ENHANCEMENT_SUMMARY.md` 等仍声称存在 “425+ 测试 / 90% 覆盖 / 大量 QuickCheck”，但代码库目前只有 183 个 `testCase` + 3 个 golden + 3 个属性测试，缺乏透明的指标可能误导使用方。

### 指标偏差（文档 vs 实际数据）

| 指标 | 文档/来源 | 实际数据 | 备注 |
| --- | --- | --- | --- |
| 总测试数量 | 425+（`TEST_ENHANCEMENT_SUMMARY.md` 第 90–105 行） | 183 个 HUnit `testCase` + 3 个 tasty-golden 对拍 + 3 个 QuickCheck 属性 ≈ 189 | `rg -o "testCase \"" test \| wc -l`（2025-11-22）与 `Test/Golden/CompilerSpec.hs`、`Test/Unit` 统计 |
| QuickCheck 属性测试 | 300+（同上） | 3 个（`Test.Unit.DependentTypesSpec` 1 个，`Test.Unit.TypeSystemSpec` 2 个） | 仅在 `PRODUCTION_TESTS`/`FULL_TESTS` 下构建 |
| 端到端 / CLI 测试 | 50+ E2E & 7 CLI（同上） | `Test.Integration.FullProjectSpec` 6 个批处理用例 + `PipelineSpec` 3 个 + `Integration.CLISpec` 2 个真实 CLI 冒烟；`Test.Unit.CLISpec` 11 个仅覆盖参数解析 | 实际命中真实二进制的只有 2 个 CLI 冒烟用例 |
| 覆盖率 | ≥70%（`TEST_ENHANCEMENT_SUMMARY.md` 第 117–125 行） | `coverage-report/summary.json` 标记为 “unavailable”，未生成任何 `.tix` 数据 | `stack test --coverage` 尚未产出报告 |

### 执行稳定性与外部依赖

- `test/Test/Integration/CLISpec.hs` 通过 `requireStackBinary` 查找真实 `stack` 并执行 `stack exec typus -- …`；一旦 PATH 中缺少 stack，该测试会直接 `assertFailure`，在受限 CI 代理上属于典型的环境耦合性用例。
- `test/Test/Integration/PipelineSpec.hs` 的 `assertGoBuilds` 需要找到 `go`、`$TYPUS_FAKE_GO` 或仓库自带的 `scripts/fake-go.sh`；若都不存在就报 “Go toolchain is missing”，因此缓存被清或 fake 脚本漏配时整个 `stack test` 会失败，稳定性受外部工具链牵制。
- `stack.yaml` 将 `production:true`、`coverage:true`、`werror:true` 固定开启，使得 `typus.cabal` 永远使用 `-DPRODUCTION_TESTS -Werror -fhpc -with-rtsopts=-M8G`。这虽可保证严格性，但也意味着任何覆盖率收集脚本缺失、或 runner 可用内存 <8G 时都会导致 `stack test` 本身不稳定。
- `typus.cabal` 中 `if flag(fast)` 直接剔除 Golden/Integration/CLI 模块（第 234–252 行）；开发者若为了提速执行 `stack test --flag typus:fast`，就只剩核心单元测试，极易形成“本地绿 / CI 红”的错觉。

## 改进建议（按优先级）

1. **恢复并自动化覆盖率产物**：在 CI 中重新运行 `stack test --coverage` 并执行 `scripts/coverage-report.sh`，将真实覆盖率写回 `coverage-report/summary.json` 与 `module-report.txt`，并据此设置阈值。
2. **扩充 Golden 与 CLI 场景**：为所有权、依赖类型错误以及多文件工程生成新的 golden，对 CLI 的 `check`/`build`/`run` 增加端到端测试（包含失败路径、`--strict-embed`、Go executor 交互）。
3. **补齐薄弱模块**：新增 `ValueAnalysis`、`OwnershipBridge`、`CommandLineDebug`、`GoToolchain` 等模块的失败路径和边界断言，避免单用例覆盖过多逻辑。
4. **扩大属性测试/随机化范围**：将 QuickCheck 属性测试扩充到 `Ownership`、`Analyzer` 等模块，并尽量让 `fast` 配置保留轻量属性测试，减少 “切换 flag 即失去生成式保障” 的风险。
5. **降低对真实工具链的隐形依赖**：为 `Test/Integration/PipelineSpec.hs`、CLI 测试提供官方 fake Go 脚本 & stack stub，同时在 CI 文档中写明如何设置 `TYPUS_FAKE_GO`，减少基础设施波动带来的误报。
6. **同步质量指标到文档**：将真实的 `testCase` 数、QuickCheck 用例数、覆盖率数据写回 `README`/`TEST_ENHANCEMENT_SUMMARY.md` 等，让外部团队基于事实评估风险。

## 数据来源与可复现性

- 断言统计：`rg -o "testCase \"" test | wc -l`、`rg -c "testCase \"" test/Test/{Unit,Integration}/*.hs`。
- 属性测试定位：`rg -n "testProperty" test/Test/Unit`，确认 `DependentTypesSpec`、`TypeSystemSpec` 中的 QuickCheck 覆盖。
- Golden & CLI 行为：`test/Test/Golden/CompilerSpec.hs`、`test/Test/Integration/CLISpec.hs`、`test/Test/Integration/PipelineSpec.hs`。
- 标志与运行条件：`stack.yaml`（flags）、`typus.cabal`（`test-suite typus-test` 段）。
- 覆盖率现状：`coverage-report/summary.json`、`coverage-report/module-report.txt`。

> 只要补齐上述缺口，并保持 `stack test` 为强制门禁，这套测试栈可以继续为 Typus 编译/分析器提供可靠的生产级回归信心。
