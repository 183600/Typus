# Typus 项目改进审视报告

## 摘要

Typus 目前已经拥有较为完善的模块拆分（分析、编译、工具链）以及丰富的文档，但在分析结果的严重度保留、跨分析错误的输出、符号收集回退逻辑以及测试覆盖上仍存在明显改进空间。本报告汇总了调研过程中发现的主要问题，并给出优先级排序的改进建议，便于后续迭代时快速定位切入点。

## 亮点回顾

- **分析与编译解耦**：`AnalyzerIntegration`、`Analyzer.State`、`Compiler.Errors.*` 等模块清晰地区分了所有权分析、依赖类型分析与编译阶段，便于单独测试与扩展。
- **工具链抽象**：`GoToolchain` 和 `CompilerUtils` 提供了可注入的执行器、日志接口，能够在 CI 或测试场景下替换系统 `go` 命令，提升可测试性。
- **文档体系完整**：根目录与 `docs/` 下提供了针对调试、测试、错误处理等主题的详尽说明，能有效降低上手成本。

## 改进机会

### 1. 跨分析错误在最终结果中被丢弃（高优先级）

- **问题描述**：`AnalyzerIntegration.combineAllResults` 只把跨分析阶段得到的 `CombinedError` 用来生成 `analysisWarnings` 与 `analysisInfo` 文本（`src/AnalyzerIntegration.hs` 第 63-72 行），并没有把这些 `CombinedError` 返回给调用方。随后 `analysisToCombined` 仅针对所有权和依赖类型错误构造 `CombinedError`，完全忽略跨分析阶段返回的 `integrationResults`。
- **影响**：即便 `Analyzer.CrossAnalysis` 检测到严重的冲突（例如 `CrossAnalyzerError ... Error`），`IntegratedCompiler.compileWithIntegratedAnalyzers` 也无法在 `filteredErrors` 中看到这些错误，`blocking` 判定自然不会触发，导致编译流程可能在存在跨分析失配的情况下仍然继续。
- **建议方案**：
  1. 在 `AnalysisResult` 中增加 `combinedErrors :: [CombinedError]` 字段，把 `runCrossAnalysis` 产生的结果完整返回。
  2. `analysisToCombined` 直接基于该字段输出结果，避免手工重建。
  3. 为跨分析错误添加集成测试（见改进项 4）。

### 2. 错误严重度在分析结果中丢失，导致筛选无效（高优先级）

- **问题描述**：`Analyzer.State.addOwnershipError` / `addDependentTypeError` 在累加错误时会保存 `CombinedError`（含严重度），但 `AnalysisResult` 只保留了纯粹的 `OwnershipError` / `DependentTypeError` 列表（`src/Analyzer/Types.hs` 第 20-26 行）。随后 `analysisToCombined` 只能把它们全部包装成 `Error` 级别（`src/AnalyzerIntegration.hs` 第 210-214 行）。
- **影响**：
  - `CompilerConfig.errorReportingLevel` 的过滤条件形同虚设——无论原本被标记为 `Warning`/`Info` 的错误，最终都会以 `Error` 严重度出现。
  - 所有权分析若希望以 `Warning` 上报某些模式（`Analyzer.State.addOwnershipError Warning …`），这些意图会在结果中丢失。
- **建议方案**：
  - 将 `AnalysisResult` 中的错误列表改为 `[(ErrorSeverity, OwnershipError)]` 等结构，或只保留完整的 `CombinedError` 列表。
  - 修改 `analysisToCombined` 以直接返回这些带有严重度的信息。

### 3. 跨分析的未使用变量检测过于启发式，误报风险高（中优先级）

- **证据**：`Analyzer.CrossAnalysis.checkUnusedVariables` 通过简单的 `words`/`tokenizeIdentifiers` 来统计变量使用次数（`src/Analyzer/CrossAnalysis.hs` 第 63-165 行），未考虑：
  - 字面量、结构体字段或方法调用中的符号。
  - 多行声明、作用域嵌套及 `_` 占位符等 Go 语法细节。
- **影响**：在真实项目中容易把字段名、接口方法或仅在 `if`/`switch` 分支使用的变量误判为未使用，从而产生干扰性警告。
- **建议**：
  - 优先复用 `Analyzer.SymbolTable` 已经收集到的符号信息（包括作用域、是否移动/借用），按作用域统计使用次数。
  - 或在 `collectSymbolsAndTypes` 失败时早退出，而不是退化到基于文本的启发式统计。

### 4. 符号收集的文本回退可能掩盖语法问题（中优先级）

- **问题描述**：`collectSymbolsAndTypes` 在 `parseGoModule` 失败后，会 fallback 到逐行字符串分析（`src/Analyzer/SymbolTable.hs` 第 24-66 行）。这一分支使用 `words`/`isPrefixOf` 等方式提取变量、类型和函数，无法正确处理：
  - 多行 `var (...)` / `type (...)` 块。
  - 多返回值、泛型或 `func (r Receiver)` 方法声明。
  - 复合字面量与内嵌结构体声明。
- **影响**：当 Go AST 解析失败时，分析阶段仍然会继续，但得到的是不完整甚至错误的符号表，可能进一步触发误报或漏报。
- **建议**：
  - 在 AST 解析失败时直接返回错误，让调用方明确编译无法继续。
  - 如果确需回退，至少限制在只读 `.go` 文件的简单场景，并在日志中提示精度降低。

### 5. 集成编译入口缺少覆盖测试（中优先级）

- **情况**：`test/` 目录下未见 `IntegratedCompiler` 或 `AnalyzerIntegration` 的测试（`test` 搜索结果为空）。现有单元测试覆盖 parser/ownership/依赖类型/CLI，但缺少：
  - 验证跨分析错误能阻断编译。
  - 测试 `errorReportingLevel` 对警告/错误的筛选行为。
  - 验证 `TYPUS_SKIP_GO_BUILD` 对 `batchCheck` 的影响。
- **建议**：
  - 在 `test/Test/Integration/` 下新增集成用例，构造同时触发所有权、依赖以及跨分析错误的 Typus 代码，确保 `filteredErrors`、`compilationWarnings` 与 `success` 字段符合预期。
  - 覆盖 `analysisWarnings` / `analysisInfo` 的输出，防止未来回归。

### 6. 分析上下文缺少真实文件路径（低优先级）

- **证据**：`AnalyzerIntegration.analyzeCodeWithBothAnalyzers` 在进入分析前把 `analysisContext.currentFile` 固定为 `"<input>"`（`src/AnalyzerIntegration.hs` 第 48 行）。
- **影响**：所有错误信息都会带上同一个占位路径，不利于 CLI 或 IDE 在多文件场景下定位问题。
- **建议**：
  - 扩展 `runIntegratedAnalysis` 的输入，允许调用方传入源文件路径，并在 `analysisContext` 中保存。
  - 若为单文件字符串分析，也可允许调用方设置自定义标签。

## 建议的优先级排序

| 优先级 | 改进项 | 预期收益 |
| ------ | ------ | -------- |
| **P0** | 保留跨分析错误 + 正确传递严重度（改进项 1 & 2） | 修复会导致错误输出缺失的核心缺陷，恢复 `errorReportingLevel` 的意义。 |
| **P1** | 优化跨分析/符号表回退逻辑（改进项 3 & 4） | 降低误报、漏报，提升分析可信度。 |
| **P1** | 增补集成测试（改进项 5） | 防止上述缺陷再次出现，覆盖核心成功/失败路径。 |
| **P2** | 传递真实文件路径（改进项 6） | 改善诊断体验，便于 IDE/CLI 集成。 |

## 后续行动建议

1. 由分析模块维护者负责调整 `AnalysisResult` 与集成编译管线的数据结构，确保 `CombinedError` 在整个流程中不丢失。
2. 在相同 PR 中补充集成测试，验证跨分析错误/警告的完整性。
3. 评估 `Analyzer.CrossAnalysis` 与 `Analyzer.SymbolTable` 的启发式逻辑是否应替换为 AST 驱动实现，必要时分阶段上线。
4. 更新 `docs/IMPROVEMENTS_SUMMARY.md` 或在新章节中引用本报告，保持文档一致性。

---

> 本报告生成于 `project-improvement-audit` 分支，依据当前仓库快照进行分析。