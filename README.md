# Typus
- Typus 是一个编程语言，在 Go 基础上增加所有权机制和其他一些特性
- Typus 引入了强大的类型系统，融合了依赖类型 (Dependent Types) 的核心思想和精确类型 (Refinement Types) 的实用语法，允许类型由值来约束和参数化。这使得在编译时就能保证更深层次的程序正确性。
- 编译到 Go
- 所有权机制可以开关
- 即使使用了所有权机制，也还是会有 GC
- 支持根据代码本身是否是按照所有权机制写的来判断是否按照所有权机制的代码处理
- [交流群](https://qun.qq.com/universal-share/share?ac=1&authKey=gzdz%2FVOju6Rm8Cbnmu71zCvdIpSbN8sltlf1ahjpk3jhXU0RZdQcov%2Fa0i3nhv9q&busi_data=eyJncm91cENvZGUiOiI4MTA0NzY2NzgiLCJ0b2tlbiI6IlNzR1dHaFN1NXpycWN3cVVGV09JWmVMSnZ5OC9FU2lpQW13ZEVEOXg2S1NEcC9VQWRkaEtla2xhZnhlYnh6T28iLCJ1aW4iOiIzMDI1Mzg1NDcyIn0%3D&data=DopbLeWghbu22Vwnj8AcfxUdz4A4VT5KpD--38y74CIsde08Q3YfC29Fms1jpaeOs_OyPAyt1Lpt_Jk2o8gZwA&svctype=4&tempid=h5_group_info)


## 语言规范与特性

### 核心语言特性

Typus 是基于 Go 的扩展语言，提供以下核心特性：

1. **所有权系统 (Ownership System)**
   - 可选启用，通过文件级或块级指令控制
   - 支持所有权转移和借用检查
   - 即使启用所有权机制，仍然保留垃圾回收器
   - 兼容标准 Go 代码的渐进式采用

2. **依赖类型与精确类型 (Dependent & Refinement Types)**
   - 类型可以由值参数化（如 `Vector(3)` vs `Vector(4)`）
   - 支持值的精确约束，在编译时验证更深层次的程序正确性
   - 运行时检查确保类型约束满足
   - 扩展 Go 的类型推导能力

3. **Go 兼容性**
   - 完全兼容标准 Go 语法
   - 编译到标准 Go 代码
   - 保持 Go 的简洁性和性能特征

### 语言限制

- **所有权系统限制**：
  - 当前不支持生命周期注解
  - 所有权分析主要在编译时进行，运行时检查有限
  - 与 Go 的垃圾回收器共存，可能有性能影响

- **类型系统限制**：
  - 依赖类型检查主要通过运行时断言实现
  - 类型推导在某些复杂场景下可能受限
  - 不支持完全的依赖类型证明

- **兼容性限制**：
  - 某些 Go 标准库特性可能与所有权系统冲突
  - 反射和 unsafe 包的使用可能导致未定义行为

## 兼容性矩阵

### 编译器支持

| 组件 | 最低版本 | 推荐版本 | 测试版本 |
|------|----------|----------|----------|
| GHC | 9.6.4+ | 9.8.4+ | 9.8.4+ |
| Stack | 3.0.0+ | 3.3.1+ | 3.3.1+ |
| Cabal | 3.6.0+ | 3.10.0+ | 3.10.0+ |

### 运行时支持

| 组件 | 最低版本 | 推荐版本 | 测试版本 |
|------|----------|----------|----------|
| Go | 1.21+ | 1.25+ | 1.25.1+ |
| Go Modules | 1.16+ | 1.21+ | 1.21+ |

### 操作系统支持

| 操作系统 | 架构 | 状态 | 备注 |
|----------|------|------|------|
| Linux | x86_64 | ✅ 完全支持 | 主要开发和测试平台 |
| Linux | ARM64 | ✅ 支持 | 基础测试通过 |
| macOS | x86_64 | ✅ 支持 | 定期测试 |
| macOS | ARM64 (Apple Silicon) | ✅ 支持 | 基础测试通过 |
| Windows | x86_64 | ⚠️ 部分支持 | CLI 工具可用，部分功能受限 |
| Windows | ARM64 | ❌ 未测试 | 不推荐用于生产 |

### 依赖库兼容性

| 库名 | 版本范围 | 用途 |
|------|----------|------|
| base | >= 4.7 && < 5 | Haskell 基础库 |
| parsec | >= 3.1.0 && < 3.2 | 解析器组合器 |
| megaparsec | >= 9.0.0 && < 10 | 现代解析器 |
| containers | >= 0.6 && < 1 | 数据结构 |
| mtl | >= 2.2 && < 3 | Monad 转换器 |
| text | >= 1.2.0 && < 3 | 文本处理 |
| optparse-applicative | >= 0.16.0 && < 1 | CLI 参数解析 |

### 构建配置

项目使用 LTS (Long Term Support) Stackage 快照：
- **当前快照**: lts-24.4
- **GHC 版本**: 9.8.4
- **构建标志**:
  - `production=true` (生产模式)
  - `coverage=false` (默认关闭覆盖率)
  - `werror=true` (警告视为错误)

### 测试覆盖率

- **单元测试**: 完全覆盖核心编译器功能
- **集成测试**: CLI 工具和端到端工作流
- **属性测试**: 使用 QuickCheck 进行模糊测试
- **性能测试**: 使用 Criterion 进行基准测试
- **最低覆盖率要求**: 70% (生产门禁)

## 前置依赖
- GHC 和 Cabal（用于构建 Haskell 项目）
- Go 1.21+（用于 typus build/run 时调用 go 工具链）


## 使用方法

```bash
# 构建项目
cabal build
# 或者使用 Makefile
make build

# 安装
cabal install
# 或者使用 Makefile
make install
```

## 使用示例

1. **转换单个文件**：
   ```bash
   typus convert input.typus -o output.go
   ```

2. **转换目录**：
   ```bash
   typus convert src/ -o out/
   ```

3. **检查语法**：
   ```bash
   typus check input.typus
   ```

4. **构建项目**（调用go build）：
   ```bash
   typus build
   ```

5. **运行项目**（调用go run）：
   ```bash
   typus run main.typus
   ```

6. **查看版本**：
   ```bash
   typus --version
   # 或
   typus -v
   ```
```

## 文件级指令

```go
// 为此文件启用所有权
//! ownership: on

// 为此文件启用依赖类型
//! constraints: on
// 上面这行代码constraints可以写成dependent_types

package main

// 代码的其余部分...
```

## 块级指令

```go
func main() {
    // 常规 Go 代码
    
    // 启用所有权的块
    {//! ownership: on
        // 具有所有权语义的代码
    }
    
    // 启用依赖类型的块
    // 下面这行代码constraints可以写成dependent_types
    {//! constraints: on
        // 具有依赖类型的代码
    }
}
```
支持一个块同时启用所有权和依赖类型

## 依赖与精确类型 (Dependent and Refinement Types)

Typus 引入了强大的类型系统，融合了**依赖类型 (Dependent Types)** 的核心思想和**精确类型 (Refinement Types)** 的实用语法，允许类型由值来约束和参数化。这使得在编译时就能保证更深层次的程序正确性。

### 1. 值参数化的类型 (依赖类型特性)

类型可以由值（如数组长度）来参数化，创建出更精确的类型家族。例如，一个 `Vector(3)` 和 `Vector(4)` 是完全不同的类型。

```go
// Vector 类型由其长度参数化
type Vector struct {
    length int
    data   []float64
}

func NewVector(length int, data []float64) *Vector {
    if len(data) != length {
        panic("Vector data length doesn't match dimension")
    }
    return &Vector{length: length, data: data}
}
```

### 2. 值的精确约束 (精确类型特性)

Typus 使用运行时检查来**精确化 (refine)** 一个类型，确保其值满足特定条件。这对于防止运行时错误（如除以零、数组越界）非常有效。

```go
// 类型 `Vector` 约束其索引访问必须在有效范围内
func (v *Vector) Get(index int) float64 {
    if index < 0 || index >= v.length {
        panic("Vector index out of bounds")
    }
    return v.data[index]
}

// 函数 `SafeDivide` 要求其第二个参数 `b` 不能为零
func SafeDivide(a, b int) int {
    if b == 0 {
        panic("SafeDivide: 除数不能为零")
    }
    return a / b
}
```
## 类型推导

Typus 保留 Golang 的类型推导能力，并在依赖类型上下文中扩展它：

```
func createVector(n int, value float64) Vector(n) {
    elements := make([]float64, n)
    for i := 0; i < n; i++ {
        elements[i] = value
    }
    return Vector{elements} // 类型自动推导为 Vector(n)
}
```

## 示例 Typus 文件

```go
//! ownership: on
//! dependent_types: on

package main

import "fmt"

type MyString struct {
    data string
}

func NewMyString(s string) MyString {
    return MyString{data: s}
}

type Vector struct {
    length int
    data   []float64
}

func NewVector(length int, data []float64) *Vector {
    if len(data) != length {
        panic("Vector data length doesn't match dimension")
    }
    return &Vector{length: length, data: data}
}

func (v *Vector) Get(index int) float64 {
    if index < 0 || index >= v.length {
        panic("Vector index out of bounds")
    }
    return v.data[index]
}

func SafeDivide(a, b int) int {
    if b == 0 {
        panic("SafeDivide: 除数不能为零")
    }
    return a / b
}

func main() {
    // Regular Go code
    fmt.Println("Hello, Typus!")
    
    {//! ownership: on

## 调试指南（日志与断点）

项目提供了统一的调试工具（src/Debug.hs），可在命令行环境中输出结构化日志，并支持在代码中设置断点。断点默认不暂停，设置环境变量后可在命令行等待回车继续。

- 日志
  - 在任意模块中引入 Debug 并调用：
    - Debug.debugInfo "Module" "信息"
    - Debug.debugWarn "Module" "警告"
    - Debug.debugError "Module" "错误"
    - Debug.debugTrace "Module" "更详细的调试跟踪"
  - 默认配置：dcEnabled=True，dcLogLevel=3（info 级别及以上会输出），带时间戳与位置信息。

- 断点
  - 在代码中插入：Debug.debugBreakpoint "Module" "到此为止打个断点"
  - 默认不暂停。若需在命令行暂停等待回车：
    - Linux/macOS: 运行命令前设置环境变量
      - 仅测试：TYPUS_DEBUG_WAIT=1 stack test
      - 单次运行：TYPUS_DEBUG_WAIT=1 stack exec -- typus convert examples/... -o out

- GHCI 源码级断点
  - 进入测试 ghci：
    - stack ghci typus:test:typus-test
  - 开启断点选项：
    - :set -fbreak-on-exception -fbreak-on-error
  - 下断点并运行：
    - :break ModuleName 行号
    - :main
  - 命中断点后可用 :step / :continue / :trace 等命令调试。

- 常见场景
  - 只看日志不暂停：stack test
  - 日志并在断点暂停：TYPUS_DEBUG_WAIT=1 stack test
  - 运行可执行并在断点暂停：TYPUS_DEBUG_WAIT=1 stack exec -- typus run main.typus

## 生产就绪与测试门禁

- 我们使用 stack test 作为唯一的生产门禁，只要 stack test 通过，即代表：
  - 单元测试、集成测试、端到端（E2E）测试均已通过。
  - 属性测试、性能测试、CLI 行为测试、指令解析测试、依赖类型与所有权迁移等也都覆盖。
  - 代码在生产模式下构建，关键警告以 -Werror 方式视为失败。
  - 已开启 HPC 覆盖率收集（需要 coverage flag），可通过 typus.tix 查看覆盖信息。

- 如何本地运行：
  - 默认开启 production 与 coverage，并将警告视为错误：
    stack test --test-arguments='--hide-successes'
  - 若需查看覆盖率：运行后查看根目录 typus.tix 或使用 hpc 工具生成报告。

- CI/CD 建议：
  - 直接以 stack test 作为唯一通过门槛；失败即阻止发布。



注意：CLI 测试中 typus build 目前可能返回 go build 的错误（例如 missing import path），测试用例会将其视为“尚未完全实现”的预期行为并继续通过。若需要，我们可以在后续迭代中完善该命令的 go.mod 或导入路径逻辑。

        // Ownership-enabled block
        s := NewMyString("hello")
        t := s // Ownership transferred
        fmt.Println(t.data)
    }
    
    {//! dependent_types: on
        // Dependent types block
        v_data := []float64{1.0, 2.0, 3.0}
        v := NewVector(3, v_data)
        fmt.Printf("Vector length: %d\n", v.length)
        fmt.Printf("Vector[0]: %.1f\n", v.Get(0))
        fmt.Printf("Vector[1]: %.1f\n", v.Get(1))
        fmt.Printf("Vector[2]: %.1f\n", v.Get(2))
    }
    
    // Test safe division
    result := SafeDivide(10, 2)
    fmt.Printf("10 / 2 = %d\n", result)
}
```
