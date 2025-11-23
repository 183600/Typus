# 测试套件增强总结

## 执行日期
2025-11-23（基于 `test/sync-real-metrics-to-readme-and-summary` 分支最新统计）

## 任务目标
- 保持 `stack test` 作为唯一且可信的质量门禁；
- 让 README 及配套测试文档反映真实的测试规模与覆盖现状，而非“425+ 测试 / 70% 覆盖率”这类过期数字；
- 记录覆盖率空白并提供可复现的统计方法，方便后续持续改进。

## 真实指标概览
- `stack test` 当前会执行 **229** 个 Tasty 节点：`rg -o "testCase \"" test | wc -l` 得到 **212 个 HUnit `testCase`**，再加上 5 个 `tasty-golden` 对拍与 12 个 `fastProperty`/QuickCheck 属性测试。
- 166 个单元测试覆盖 parser、ownership、type system、CLI 参数解析、Go 工具链 stub 等模块；46 个集成测试涵盖 Analyzer、Pipeline、FullProject 与 Ownership，另有 8 个直接命中 `stack exec typus` 的 CLI 冒烟用例。
- `Test.Golden.CompilerSpec` 对五个典型 Typus 输入进行输出对拍，确保 Parser→Compiler→Go 渲染逻辑保持稳定。
- `coverage-report/summary.json` 的 `total_coverage` 字段目前为 `"unavailable"`，仓库内缺少可信的 `.tix` 产物，暂时无法宣称 70% 覆盖率。

| 分类 | 数量 | 说明 |
| --- | --- | --- |
| 单元测试 | 166 | `Test/Unit/*` 中的 HUnit `testCase`，默认 `stack test` 全量执行 |
| 集成测试 | 46 | `Test/Integration/*` 的 HUnit `testCase`，覆盖 Analyzer、Pipeline、Ownership、CLI |
| Golden 测试 | 5 | `Test.Golden.CompilerSpec` 中的 `goldenVsStringDiff` 对拍 |
| QuickCheck / fastProperty | 12 | 分布在 DependentTypes、TypeSystem、Ownership、AnalyzerState、Utils、ValueAnalysis |
| CLI 冒烟 | 8 | `Test/Integration/CLISpec.hs`，真实触发 `typus` 子命令与失败路径 |
| 总计 | 229 | 166（Unit）+ 46（Integration）+ 5（Golden）+ 12（属性测试） |

### 统计命令
```bash
rg -o "testCase \"" test | wc -l             # 212（全部 HUnit testCase）
rg -o "testCase \"" test/Test/Unit | wc -l    # 166（单元测试）
rg -o "testCase \"" test/Test/Integration | wc -l  # 46（集成/CLI 测试）
rg -n "fastProperty" test/Test/Unit/*.hs       # 12 个属性测试定义
```

## 单元测试覆盖面
| 模块 | `testCase` 数 | 重点场景 |
| --- | --- | --- |
| `Test/Unit/AnalyzerStateSpec.hs` | 7 | Analyzer.State 的诊断过滤、聚合、severity 排序，另含 2 个 `fastProperty` 验证 filter* 行为 |
| `Test/Unit/CLISpec.hs` | 11 | `convert`/`check`/`build`/`run` 子命令参数解析、`--strict-embed` 透传、版本信息 |
| `Test/Unit/CliRunnerSpec.hs` | 3 | `Cli.Runner.runWithArgs` 对缺失 `stack` 的回退、`TYPUS_FAKE_STACK` 支持 |
| `Test/Unit/CommandLineDebugSpec.hs` | 2 | `--debug`、`--emit-ast` 标志解析 |
| `Test/Unit/CompilerSpec.hs` | 5 | 编译器的 happy-path 与错误冒泡 |
| `Test/Unit/DependentTypesSpec.hs` | 29 | 类型/函数声明解析、错误路径，含 1 个 `fastProperty` 验证随机 AST |
| `Test/Unit/EmbedAssetsSpec.hs` | 6 | 资源嵌入、`--strict-embed` 行为 |
| `Test/Unit/ErrorHandlingSpec.hs` | 14 | 诊断分级、错误上下文拼接、Formatter 行为 |
| `Test/Unit/GoToolchainSpec.hs` | 7 | `TYPUS_SKIP_GO_BUILD`、`TYPUS_FAKE_GO`、临时 Go 工程生成 |
| `Test/Unit/OwnershipSpec.hs` | 22 | 移动语义、借用冲突、块级作用域，含 2 个 `fastProperty` 断言 |
| `Test/Unit/OwnershipBridgeSpec.hs` | 3 | 所有权错误同步到符号表的行为 |
| `Test/Unit/ParserSpec.hs` | 8 | 文件/块级指令、Build tag、错误提示 |
| `Test/Unit/SymbolTableSpec.hs` | 3 | 作用域遮蔽、可变/不可变标记 |
| `Test/Unit/TypeSystemSpec.hs` | 11 | 类型检查、约束求解，含 2 个 `fastProperty` |
| `Test/Unit/UtilsSpec.hs` | 26 | 字符串、路径与诊断辅助函数，含 2 个 `fastProperty` |
| `Test/Unit/ValueAnalysisSpec.hs` | 5 | Go AST 值/引用分类、控制流，含 3 个 `fastProperty` |
| `Test/Unit/VerbositySpec.hs` | 4 | CLI 日志等级与 `--verbose` 处理 |

## 集成 / Golden / 端到端用例
| 模块 | `testCase` 数 | 覆盖内容 |
| --- | --- | --- |
| `Test/Integration/AnalyzerSpec.hs` | 20 | 依赖类型 + 所有权联动、CrossAnalyzer 聚合、severity 传播 |
| `Test/Integration/FullProjectSpec.hs` | 6 | `batchConvert`/`batchCheck`、Go executor 的记录/跳过/失败三态 |
| `Test/Integration/IntegratedCompilerSpec.hs` | 4 | 集成编译输出、诊断过滤、配置覆盖 |
| `Test/Integration/OwnershipSpec.hs` | 5 | 真实 Typus 片段的端到端所有权验证 |
| `Test/Integration/PipelineSpec.hs` | 3 | Parser→Compiler→Go build 流程，依赖真实或 fake `go` 可执行文件 |
| `Test/Integration/CLISpec.hs` | 8 | `stack exec typus` 的 `--version`、`convert/check/build/run`、`--strict-embed`、缺失入口等路径 |
| `Test/Golden/CompilerSpec.hs` | 5 | `simple_go_code`、`statements_without_package`、`generic_type`、`advanced_ownership`、`type_system_valid` 对拍 |

## 属性测试分布
- **DependentTypes**：1 个属性测试（随机生成 AST，确保 `analyzeAST` 不产生错误）。
- **TypeSystem**：2 个属性测试（`solveConstraints` 处理自反约束与队列清空）。
- **Ownership**：2 个属性测试（指令 token 化不会产生伪造换行，内置函数名不含空白）。
- **AnalyzerState**：2 个属性测试（`filterWarnings` / `filterInfo` 等价于人工遍历）。
- **Utils**：2 个属性测试（`trim` 幂等、`splitByCollapsed` 不产出空 chunk）。
- **ValueAnalysis**：3 个属性测试（`isReferenceInit`、`isValueType`、指针识别）。

以上 12 个 `fastProperty` 均在默认 `stack test` 中执行，与是否启用 `PRODUCTION_TESTS` 无关。

## 覆盖率现状
- `coverage-report/summary.json` 与 `coverage-report/module-report.txt` 均显示 “Coverage data has not been generated yet.”，仓库缺少可信的 `.tix`。
- 需要运行 `stack test --coverage`，然后执行 `scripts/coverage-report.sh` 才能生成最新的 HTML/JSON/TXT 报告；生成结果应随代码一起提交，避免再次出现“unavailable”状态。

```bash
stack test --coverage              # 生成 typus.tix
scripts/coverage-report.sh         # 汇总到 coverage-report/*.json|txt
```

## 质量保证机制
1. `stack.yaml` 默认启用 `production:true`、`coverage:true`、`werror:true`，因此 `stack test` 构建时自动带上 `-DPRODUCTION_TESTS -fhpc -Werror -with-rtsopts=-M8G`。
2. `FAST_TESTS` flag 会直接剔除 Integration 与 Golden 模块，仅适合本地快速迭代；CI/发布必须运行默认配置。
3. `Integration.CLISpec` 优先使用真实 `stack exec typus`，若 CI 缺少 `stack` 则回退到 `Cli.Runner.runWithArgs`，同时允许设置 `TYPUS_FAKE_STACK`、`TYPUS_FAKE_GO`。
4. `fastProperty` 封装 QuickCheck 的采样上限，在默认模式下即可提供生成式回归保障，无需额外 flag。

## 验证方法
- `stack test`：运行全部单元、集成、Golden、属性测试。
- `stack test --coverage && scripts/coverage-report.sh`：生成并刷新 coverage-report。
- `rg -o "testCase \"" test/Test/{Unit,Integration} | wc -l`：快速确认单元/集成用例数量。
- `rg -n "fastProperty" test/Test/Unit/*.hs`：定位属性测试定义及所属模块。
- `stack test --flag typus:fast`：仅在需要快速反馈时使用，用于强调“fast 模式会跳过 CLI/Golen/Integration”这一差异。

## 成果展示
1. 仓库文档首次列出了 **真实** 的测试数量与分布，避免继续宣传 425+ 测试或 300+ QuickCheck。
2. 明确记录了当前覆盖率状态为 “unavailable”，并提供了可复现的刷新步骤。
3. README、`STACK_TEST_QUALITY.md`、本文件之间的统计口径保持一致，后续只需根据上述命令刷新数字即可。

## 后续建议
1. 将 `stack test --coverage` + `scripts/coverage-report.sh` 纳入 CI，自动生成可追踪的覆盖率数据。
2. 为 CLI/Golang 流程补充失败路径（缺少 main、go build 失败等）的 Golden/Integration 测试。
3. 把 `fastProperty` 扩展到 Analyzer.State 以外的薄弱模块（如 GoToolchain、Cli.Runner 的错误分支）。
4. 为 `Test/Integration/FullProjectSpec` 增加更多 fixture，覆盖多入口/多模块工程。
5. 建立定期的指标刷新流程（例如 release 前运行一次统计脚本并更新文档），防止数字再次漂移。

## 相关文件
- [README.md](README.md)：对外展示的测试命令与指标速览。
- [STACK_TEST_QUALITY.md](STACK_TEST_QUALITY.md)：深入的测试质量分析与改进建议。
- [TEST_COVERAGE_REPORT.md](TEST_COVERAGE_REPORT.md)：覆盖率报告与生成流程（需结合本文件的“覆盖率现状”章节使用）。
- [PRODUCTION_READINESS.md](PRODUCTION_READINESS.md)：生产就绪性说明，可按本文件的统计数据更新测试部分。
