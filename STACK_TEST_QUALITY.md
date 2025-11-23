# Stack Test 测试用例质量分析

> 分析日期：2025-11-23（基于 `audit-stack-test-quality` 分支最新提交）

## 结论概览

- **整体评估：中等偏上** —— `stack test` 继续覆盖解析器、所有权/依赖类型分析、Go 代码生成以及 CLI 端到端路径。相比年初，多了 Go 工具链与 CLI 错误路径的断言，但覆盖率产物依旧缺失，Golden/属性测试仍然偏向 happy-path。
- **规模现状**：`rg -o "testCase \"" test | wc -l` 得到 **212 个显式 HUnit `testCase`**；`Test.Golden.CompilerSpec` 仍包含 **5 个 tasty-golden 对拍**；QuickCheck/fastProperty 现有 **12 个属性测试**（分布在 DependentTypes、TypeSystem、Ownership、AnalyzerState、Utils、ValueAnalysis，默认配置都会运行）。
- **执行范围**：`stack.yaml` 固定 `production:true`、`coverage:true`、`werror:true`，因此默认 `stack test` 会以 `-DPRODUCTION_TESTS -fhpc -Werror -with-rtsopts=-M8G` 构建，并运行 `Test.Unit` + `Test.Integration` + `Test.Golden` 入口。若显式传入 `--flag typus:fast`，Integration/Golden/CLI 模块与绝大多数属性测试会被剔除。
- **质量亮点**：`Test.Integration.CLISpec` 已覆盖 `convert/check/build/run` 以及 `--strict-embed` 与缺失入口的失败路径；`Test.Unit.GoToolchainSpec` 验证 `TYPUS_SKIP_GO_BUILD`/`TYPUS_FAKE_GO` 的分支；`Test.Unit.ValueAnalysisSpec` 与 `Test.Unit.UtilsSpec` 的 fastProperty 用例为 AST/工具函数提供了生成式回归保障。
- **主要风险**：覆盖率报告依然是空模板、Golden 测试仍只检查成功路径、`Analyzer.State`/`GoToolchain` 失败分支等模块缺乏更细粒度断言，且文档对测试数量/覆盖率的宣传与现实存在巨大偏差。

## stack test 运行范围

1. **入口与结构**：`test/Main.hs` 聚合 `Test.Unit.tests`、`Test.Integration.tests`、`Test.Golden.tests`。只要未定义 `FAST_TESTS`（默认），所有单元、集成、golden、CLI 以及 fastProperty/QuickCheck 测试都会执行。
2. **编译标志**：`stack.yaml` 固定 `production:true` + `coverage:true` + `werror:true`，对应 `typus.cabal` 的 `-DPRODUCTION_TESTS -fhpc -Werror`。`fastProperty` 会在 `FAST_TESTS` 模式下降低 `QuickCheckTests`/`QuickCheckMaxSize`，但不会跳过属性测试；`Test.Dependencies.Arbitrary` 已在需要的模块内显式导入，因此即便未定义 `PRODUCTION_TESTS` 也能编译所需的 `Arbitrary` 实例。
3. **覆盖率数据缺失**：`coverage-report/summary.json` 与 `module-report.txt` 仍显示 "Coverage data has not been generated yet."，仓库没有任何 `.tix` 产物，CI 无法验证宣称的 70% 覆盖率。
4. **快速模式的缺口**：`typus:fast` flag 会直接从 `test-suite` 中移除 Golden、Integration、CLI、`Test.Dependencies.Arbitrary` 等模块；开发者若只运行 `stack test --flag typus:fast`，本地覆盖面会骤降。

## 测试用例盘点（HUnit testCase 统计）

| 模块 (Spec) | 类型 | `testCase` 数 | 主要覆盖场景 |
| --- | --- | --- | --- |
| `Test/Unit/DependentTypesSpec.hs` | 单元 | 29 | 依赖类型语法、AST 校验、跨分析器桥接；PRODUCTION/FULL 下附带 1 个 QuickCheck 属性测试 |
| `Test/Unit/AnalyzerStateSpec.hs` | 单元 | 7 | 诊断过滤、severity 分类、聚合；含 2 个 `fastProperty` 校验 filter 行为 |
| `Test/Unit/OwnershipSpec.hs` | 单元 | 22 | 移动语义、借用冲突、loop 作用域、内置例外 |
| `Test/Unit/UtilsSpec.hs` | 单元 | 26 | 诊断渲染、路径与 FS 工具；含 2 个 `fastProperty` 属性测试验证字符串/分割辅助函数 |
| `Test/Unit/ErrorHandlingSpec.hs` | 单元 | 14 | 诊断分级、错误上下文拼接、Formatter 行为 |
| `Test/Unit/ParserSpec.hs` | 单元 | 8 | 文件/块级指令、build tag、错误提示、directives 传播 |
| `Test/Unit/TypeSystemSpec.hs` | 单元 | 11 | 约束抽取、类型推断、函数 refine；PRODUCTION/FULL 下有 2 个 QuickCheck 属性测试 |
| `Test/Unit/EmbedAssetsSpec.hs` | 单元 | 6 | 资源嵌入、路径规范化、`--strict-embed` 行为 |
| `Test/Unit/VerbositySpec.hs` | 单元 | 4 | CLI verbosity 与 logger 行为 |
| `Test/Unit/ValueAnalysisSpec.hs` | 单元 | 5 | Go AST 值/引用分类；附带 3 个 `fastProperty` 断言保证 ampersand/pointer/builtin 类型识别 |
| `Test/Unit/SymbolTableSpec.hs` | 单元 | 3 | 符号注册、作用域重影、可变/不可变标记 |
| `Test/Unit/OwnershipBridgeSpec.hs` | 单元 | 3 | 所有权错误到符号表的同步/过滤、缺失错误的合成 |
| `Test/Unit/CompilerSpec.hs` | 单元 | 5 | 编译器 happy-path、诊断串联、错误冒泡 |
| `Test/Unit/CommandLineDebugSpec.hs` | 单元 | 2 | `--debug` / `--emit-ast` 标志解析 |
| `Test/Unit/CLISpec.hs` | 单元 | 11 | `convert`/`check`/`build`/`run` 子命令参数解析、`--strict-embed` 透传、`--version` |
| `Test/Unit/CliRunnerSpec.hs` | 单元 | 3 | `Cli.Runner.runWithArgs` 的回退路径、`TYPUS_FAKE_STACK`/`TYPUS_SKIP_GO_BUILD` 支持 |
| `Test/Unit/GoToolchainSpec.hs` | 单元 | 7 | `TYPUS_SKIP_GO_BUILD`/`TYPUS_FAKE_GO`、临时 Go 工程生成、`runGoCommand` 日志 |
| `Test/Integration/AnalyzerSpec.hs` | 集成 | 20 | 依赖类型 + 所有权联动、CrossAnalyzer 错误聚合、severity 传播 |
| `Test/Integration/FullProjectSpec.hs` | 集成 | 6 | `batchConvert`/`batchCheck`、Go executor 三态（recording/skipping/failing）|
| `Test/Integration/IntegratedCompilerSpec.hs` | 集成 | 4 | Integrated compiler 输出、过滤后的 diagnostics、配置项 |
| `Test/Integration/OwnershipSpec.hs` | 集成 | 5 | 所有权分析器在真实 Typus 片段上的端到端行为 |
| `Test/Integration/PipelineSpec.hs` | 集成 | 3 | Parser→Compiler→Go build（需要 `go` 或 `TYPUS_FAKE_GO`/`scripts/fake-go.sh`）|
| `Test/Integration/CLISpec.hs` | 集成 | 8 | 真实 `stack exec typus` 或 `Cli.Runner.runWithArgs` 的 `--version`、`convert/check/build/run`、`--strict-embed`、缺失入口/文件失败路径 |
| `Test/Golden/CompilerSpec.hs` | Golden | 5 | `simple_go_code`、`statements_without_package`、`generic_type`、`advanced_ownership`、`type_system_valid` 五个 fixture 的源码对拍 |

> QuickCheck/fastProperty 属性测试共 12 个：`DependentTypesSpec` 1 个、`TypeSystemSpec` 2 个、`OwnershipSpec` 2 个、`AnalyzerStateSpec` 2 个、`UtilsSpec` 2 个、`ValueAnalysisSpec` 3 个。`FAST_TESTS` 模式仅降低采样数量，不会跳过这些断言。

## 质量观察

### 优势
1. **语义深度验证增强**：`DependentTypesSpec`、`TypeSystemSpec` 的属性测试配合 `ValueAnalysisSpec`/`UtilsSpec` 的 fastProperty 断言，让解析/约束/Go 值语义在生成式输入上保持回归保障。
2. **跨分析器与批处理链路仍有保障**：`AnalyzerSpec` 继续验证 `newIntegratedAnalyzer`、severity 传播、CrossAnalyzer 聚合；`FullProjectSpec`/`PipelineSpec` 将 Parser→Compiler→Go build 和批量 CLI 组合串联。
3. **CLI 覆盖度显著提升**：`Integration.CLISpec` 现已验证 `convert`/`check`/`build`/`run` 以及 `--strict-embed`、缺失入口、`go` stub 日志；`Unit.CLISpec` 则覆盖所有子命令参数解析。
4. **Go 工具链依赖被 stub 化**：`GoToolchainSpec`、`Integration.PipelineSpec` 一起确保 `TYPUS_FAKE_GO`/`TYPUS_SKIP_GO_BUILD` 生效，并在缺失真实 `go` 时给出明确失败消息。

### 缺口与风险
1. **覆盖率报告仍为空**：开启了 `-fhpc` 却没有 `.tix` 与 `coverage-report` 产物，无法验证任何覆盖率指标。
2. **Golden 仍是 happy-path**：五个 fixture 全部是成功编译场景，缺少错误信息、指令注释、复杂泛型/所有权负面路径的稳定性校验。
3. **模块覆盖不均**：`GoToolchain`/`OwnershipBridge` 虽新增断言，但 `Analyzer.State`、`Cli.Runner` 错误分支、`ValueAnalysis` 的跨文件输入仍无独立测试；`Test.Integration.FullProjectSpec` 也只包含单一 fixture。
4. **外部依赖依旧影响稳定性**：`Integration.PipelineSpec` 必须找到真实或 fake 的 `go`，`stack exec` CLI 测试依赖 PATH 中的 Stack 或 `TYPUS_FAKE_STACK`。一旦 runner 缺少 stub，就会出现不稳定失败。
5. **文档指标与现实脱节**：`TEST_ENHANCEMENT_SUMMARY.md`/`README` 依旧宣称 425+ 测试、300+ QuickCheck、70% 覆盖率，与当前数据差异巨大，容易误导使用方。

### 指标偏差（文档 vs 实际数据）

| 指标 | 文档/来源 | 实际数据 | 备注 |
| --- | --- | --- | --- |
| 总测试数量 | 425+（`TEST_ENHANCEMENT_SUMMARY.md` 第 90–105 行） | 212 个 HUnit `testCase` + 5 个 tasty-golden + 12 个 QuickCheck/fastProperty ≈ **229** | `rg -o "testCase \"" test | wc -l`（2025-11-23） + `Test.Golden.CompilerSpec` + `fastProperty/testProperty` 统计 |
| QuickCheck 属性测试 | 300+（同上） | 12 个（分布在 DependentTypes、TypeSystem、Ownership、AnalyzerState、Utils、ValueAnalysis，默认构建均会运行） | `rg -n "testProperty" test`、`rg -n "fastProperty" test` |
| 端到端 / CLI 测试 | 50+ E2E & 7 CLI（同上） | `FullProjectSpec` 6 + `PipelineSpec` 3 + `Integration.CLISpec` 8（合计 17 个 E2E/CLI 测试，其中命中真实二进制的仍是 8 个 CLI 冒烟用例） | 真实命中二进制的仍是 8 个 CLI 冒烟用例 |
| 覆盖率 | ≥70%（`TEST_ENHANCEMENT_SUMMARY.md` 第 117–125 行） | `coverage-report/summary.json` 标记为 "unavailable"，无 `.tix` | 需要重新运行 `stack test --coverage` 并生成报告 |

### 执行稳定性与外部依赖

- `Integration.CLISpec` 会优先使用真实 `stack exec typus`，若缺少 `stack` 则回退到 `Cli.Runner.runWithArgs` 或尊重 `TYPUS_FAKE_STACK`。
- `Integration.PipelineSpec` 的 `assertGoBuilds` 需要 `go`、`$TYPUS_FAKE_GO` 或 `scripts/fake-go.sh`，缺一不可。
- `typus.cabal` 在 `production` flag 下固定 `-with-rtsopts=-M8G`。内存不足或 HPC 工具链缺失会直接导致 `stack test` 不稳定。
- `fastProperty` 确保 `FAST_TESTS` 仍执行轻量属性测试，但 `fast` flag 仍会整体移除 Integration/Golden/CLI 模块，应在贡献指南中强调差异。

## 风险热力图

| 风险 | 影响 | 概率 | 说明 |
| --- | --- | --- | --- |
| 覆盖率产物缺失 | 高 | 高 | `stack.yaml` 始终启用 `-fhpc`，但 `coverage-report/summary.json` 与 `module-report.txt` 仍为空模板，CI 无法获得真实覆盖率基线。 |
| Golden 仅覆盖成功路径 | 中高 | 中 | tasty-golden 仅验证 5 个 happy-path fixture，没有断言编译错误或资产缺失，无法防止回归到失败分支。 |
| 模块覆盖不均 | 中 | 中高 | `Analyzer.State`、`Cli.Runner`、`GoToolchain` 等复杂模块在单元层级的断言稀疏，许多失败分支未被 HUnit/属性测试锁定。 |
| 外部依赖脆弱 | 中 | 中 | CLI/Go 工具链测试依赖真实 `stack` 与 `go`，若缺少 `TYPUS_FAKE_STACK`/`TYPUS_FAKE_GO`，`stack test` 会随机失败。 |
| 文档指标与现实脱节 | 中 | 高 | 多份文档仍宣称 425+ 测试、70% 覆盖率，与当前 229 条用例/coverage 缺失的事实冲突，影响对质量门槛的预期。 |

## 改进建议（按优先级）

1. **恢复并自动化覆盖率产物**：在 CI 中重新启用 `stack test --coverage` + `scripts/coverage-report.sh`，把真实覆盖率写入 `coverage-report/summary.json` / `module-report.txt`，并据此设定阈值。
   - 产出 `.tix` 后将 `coverage-report` 目录上传为 artifact，便于追踪趋势。
   - 结合 `cabal test --enable-coverage` 或 HPC 工具，验证 `-fhpc` 与优化标志无冲突。
2. **扩充 Golden 与 CLI 负面场景**：为所有权、依赖类型错误、多文件工程等生成新的 golden，对 CLI 的 `build/run` 增加 Go 真实失败、缺失资产、退出码断言。
   - 在 `test/golden` 下新增失败样例，并验证诊断文本/退出码。
   - 在 `Test.Integration.CLISpec` 中模拟 `go build` 失败、缺少 `--entry` 等场景，确保 `Cli.Runner.runWithArgs` 能返回具体诊断。
3. **补齐薄弱模块**：针对 `Analyzer.State`、`Cli.Runner`、`GoToolchain` 错误分支、新增 `ValueAnalysis` 复合项目 fixture，避免单用例覆盖过多逻辑。
   - 为 `Analyzer.State` 写入最小化 fixture，断言多 severity mix 下的过滤行为。
   - 覆盖 `Cli.Runner` 对 `TYPUS_SKIP_GO_BUILD`、`TYPUS_FAKE_STACK` 组合路径的重试与日志。
4. **增加属性测试覆盖面**：把 `fastProperty` 扩散到 Ownership/Analyzer 等模块；同时考虑让 `DependentTypes`/`TypeSystem` 的属性测试在非 PRODUCTION 模式下也能运行（降低样本量即可）。
   - 为 `OwnershipSpec` 的 borrow checker 引入随机指令序列生成器，捕捉跨块借用冲突。
   - 通过配置 `QuickCheckTests`/`MaxSize`，让 CI 在 FULL 模式运行高样本，开发者在 FAST 模式保留轻量抽样。
5. **同步真实指标到文档**：更新 `README`、`TEST_ENHANCEMENT_SUMMARY.md` 等，列出真实测试数量/覆盖率，避免误导。
   - 在测试指南中明确 `typus:fast` flag 的取舍，提示需要定期执行完整 `stack test`。
   - 给出统计命令（如 `rg -o "testCase \"" test | wc -l`）与生成 coverage 报告的脚本，方便贡献者复现。

## 数据来源与可复现性

- 断言统计：`rg -o "testCase \"" test | wc -l`、`rg -c "testCase \"" test/Test/{Unit,Integration}/*.hs`。
- 属性测试定位：`rg -n "testProperty" test/Test/Unit`、`rg -n "fastProperty" test`。
- Golden & CLI 行为：`test/Test/Golden/CompilerSpec.hs`、`test/Test/Integration/CLISpec.hs`、`test/Test/Integration/PipelineSpec.hs`。
- 标志与运行条件：`stack.yaml`（flags）、`typus.cabal`（`test-suite typus-test` 段）。
- 覆盖率现状：`coverage-report/summary.json`、`coverage-report/module-report.txt`。

> 只要补齐覆盖率与负面场景、保持 `stack test` 作为强制门禁，这套测试栈可以继续为 Typus 编译/分析管线提供可靠的生产级回归信心。
