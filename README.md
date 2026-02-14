# Typus

Typus 是一门构建在 Go 之上的编程语言，引入**依赖类型**、**精确类型**和可选的**所有权机制**，编译到标准 Go 代码。

有些特性目前可能没有实现

**核心理念**：在保持 Go 生态兼容性的前提下，将尽可能多的错误从运行时移到编译时。

[交流群](https://qun.qq.com/universal-share/share?ac=1&authKey=gzdz%2FVOju6Rm8Cbnmu71zCvdIpSbN8sltlf1ahjpk3jhXU0RZdQcov%2Fa0i3nhv9q&busi_data=eyJncm91cENvZGUiOiI4MTA0NzY2NzgiLCJ0b2tlbiI6IlNzR1dHaFN1NXpycWN3cVVGV09JWmVMSnZ5OC9FU2lpQW13ZEVEOXg2S1NEcC9VQWRkaEtla2xhZnhlYnh6T28iLCJ1aW4iOiIzMDI1Mzg1NDcyIn0%3D&data=DopbLeWghbu22Vwnj8AcfxUdz4A4VT5KpD--38y74CIsde08Q3YfC29Fms1jpaeOs_OyPAyt1Lpt_Jk2o8gZwA&svctype=4&tempid=h5_group_info)

---

## 目录

- [快速开始](#快速开始)
- [设计原则](#设计原则)
- [指令系统](#指令系统)
- [依赖类型与精确类型](#依赖类型与精确类型)
- [所有权机制](#所有权机制)
- [编译模型](#编译模型)
- [与 Go 互操作](#与-go-互操作)
- [完整示例](#完整示例)
- [语法速查表](#语法速查表)
- [约束求解器](#约束求解器)
- [环境变量](#环境变量)
- [测试](#测试)

---

## 快速开始

### 安装

```bash
cabal install
# 或
make install
```

### 基本用法

```bash
# 转换文件
typus convert input.typus -o output.go

# 转换目录
typus convert src/ -o out/

# 类型检查（不生成代码）
typus check input.typus

# 构建项目（转换后调用 go build）
typus build

# 运行项目（转换后调用 go run）
typus run main.typus

# 查看版本
typus --version
```

### 30 秒体验

```go
//! dependent_types: on

package main

type Positive = int where { self > 0 }
type NonZero  = int where { self != 0 }

func safeDiv(a: int, b: NonZero) -> int {
    return a / b
}

func main() {
    r := safeDiv(10, 2)    // ✓ 编译期验证 2 ≠ 0
    // safeDiv(10, 0)       // ✗ 编译期错误：0 不满足 NonZero
}
```

---

## 设计原则

1. **渐进式采用**：通过指令按文件或代码块粒度启用特性，可在现有 Go 项目中逐步引入。
2. **编译期优先，运行时兜底**：约束能静态证明就静态证明；不能时显式插入运行时检查，绝不静默忽略。
3. **可预测的约束求解**：明确公开求解器的能力边界（见 [约束求解器](#约束求解器)），避免"编译器有时能证明有时不能"的困惑。
4. **Go 生态兼容**：编译产物是合法且可读的 Go 代码，可直接与 Go 模块互操作。

---

## 指令系统

### 文件级指令

在 `package` 声明之前，用 `//!` 注释启用特性：

```go
//! ownership: on
//! dependent_types: on

package main
```

`dependent_types` 也可写作 `constraints`。

### 块级指令

在函数内部按代码块粒度启用：

```go
func main() {
    // 普通 Go 代码

    {//! ownership: on
        // 此块启用所有权语义
    }

    {//! dependent_types: on
        // 此块启用依赖类型
    }
}
```

一个块可以同时启用多个特性。未标注的代码按普通 Go 语义处理。

---

## 依赖类型与精确类型

### 值参数化类型

用 `[name: type]` 声明**值参数**，与 Go 的类型参数 `[T any]` 区分：

```go
type Vector[n: int] struct {
    data [n]float64
}

type Matrix[rows: int, cols: int] struct {
    data [rows][cols]float64
}

// 混合类型参数与值参数
type BoundedSlice[T any, cap: int] struct {
    data []T
}
```

`Vector[3]` 和 `Vector[4]` 是不同的类型。

### 精确类型

用 `where` 给类型附加谓词，`self` 指代被约束的值：

```go
type NonZero  = int where { self != 0 }
type Positive = int where { self > 0 }

type Bounded[lo: int, hi: int] = int where { self >= lo && self <= hi }
type Percentage = Bounded[0, 100]

type NonEmpty    = string where { len(self) > 0 }
type ValidIndex[n: int] = int where { self >= 0 && self < n }
```

### 依赖函数签名

返回类型和参数类型可以引用其他参数的值：

```go
// 返回类型依赖参数
func zeros(n: Positive) -> Vector[n] {
    return Vector[n]{data: make([]float64, n)}
}

// 参数之间依赖
func get[n: int](v: Vector[n], i: ValidIndex[n]) -> float64 {
    return v.data[i]
}

// 类型级算术
func concat[m: int, n: int](a: Vector[m], b: Vector[n]) -> Vector[m + n] {
    result := make([]float64, m+n)
    copy(result, a.data)
    copy(result[m:], b.data)
    return Vector[m+n]{data: result}
}

// 矩阵乘法：类型保证维度对齐
func matMul[m: int, n: int, p: int](
    a: Matrix[m, n],
    b: Matrix[n, p],
) -> Matrix[m, p]
    where { m > 0, n > 0, p > 0 }
{
    // ...
}
```

### 函数前置条件

用 `where` 子句在签名后声明前置条件：

```go
func average[n: int](v: Vector[n]) -> float64
    where { n > 0 }
{
    sum := 0.0
    for _, x := range v.data {
        sum += x
    }
    return sum / float64(n)
}
```

### 断言与条件窄化

`assert` 将运行时值的类型精确化。编译器能静态证明时不生成代码，否则插入运行时检查：

```go
func processInput(n: int) {
    assert n > 0           // 此后 n 被视为 Positive

    v := zeros(n)          // ✓
    avg := average(v)      // ✓ 已知 n > 0
}
```

`if` 分支中的条件自动窄化类型：

```go
d := readInt()
// safeDiv(10, d)          // ✗ 无法证明 d ≠ 0

if d != 0 {
    r := safeDiv(10, d)    // ✓ 分支内 d 满足 NonZero
}
```

> **`assert` vs `static_assert`**：`assert expr` 允许在无法静态证明时退化为运行时检查。如果你需要**强制编译期证明**，使用 `static_assert expr`——无法静态证明时直接编译错误，不会插入任何运行时代码。

### 编译期常量传播

当值在编译期已知时，编译器直接验证，不插入检查：

```go
v := zeros(3)            // v: Vector[3]
x := get(v, 0)           // ✓ 编译期验证 0 < 3
y := get(v, 2)           // ✓ 编译期验证 2 < 3
// z := get(v, 5)        // ✗ 编译期错误：5 ≥ 3

r := safeDiv(10, 2)      // ✓ 编译期验证 2 ≠ 0
```

当值在运行时确定时，根据 `assert` / `static_assert` 策略处理：

```go
n := readInt()
assert n > 0              // 运行时检查
vn := zeros(n)            // ✓ 已 assert

idx := readInt()
w := get(vn, idx)         // 自动插入运行时边界检查
```

### 存在类型

当值参数在运行时才确定时，用 `some` 标记：

```go
func readVector(input: []float64) -> Vector[some n: int]
    where { n == len(input) }
{
    return Vector[len(input)]{data: input}
}
```

使用 `match` 解包存在量化的值参数：

```go
func processVector() {
    data := []float64{1.0, 2.0, 3.0}
    v := readVector(data)       // v: Vector[some n]

    match v.(n) {               // 绑定 n，此块内 v: Vector[n]
        fmt.Println(get(v, 0))

        if n > 1 {
            fmt.Println(get(v, 1))   // ✓ 已知 n > 1
        }
    }
}
```

### 类型推导

Typus 在 Go 类型推导基础上扩展到依赖类型上下文：

```go
func createVector(n: Positive, value: float64) -> Vector[n] {
    elements := make([]float64, n)
    for i := 0; i < n; i++ {
        elements[i] = value
    }
    return Vector{elements}    // 自动推导为 Vector[n]
}
```

---

## 所有权机制

> **状态**：所有权机制目前是实验性特性。当前版本提供基本的移动语义检查，更完整的借用和生命周期系统在规划中。

### 基本语义

启用 `ownership: on` 后，赋值默认为**移动**（move）。移动后的变量不可再使用：

```go
{//! ownership: on
    s := NewMyString("hello")
    t := s                    // 移动：s 的所有权转移给 t
    fmt.Println(t.data)       // ✓
    // fmt.Println(s.data)    // ✗ 编译错误：s 已被移动
}
```

### 借用

用 `&` 创建不可变借用，用 `&mut` 创建可变借用：

```go
{//! ownership: on
    s := NewMyString("hello")

    r := &s                   // 不可变借用
    fmt.Println(r.data)       // ✓ 通过借用读取
    fmt.Println(s.data)       // ✓ 原值仍可读

    m := &mut s               // 可变借用（同一时刻只能有一个）
    m.data = "world"          // ✓ 通过可变借用修改
}
```

**借用规则**：
- 同一时刻可以有多个 `&` 借用，**或**一个 `&mut` 借用，不可同时存在。
- 借用的生命周期不能超过原值的作用域。
- 违反规则是**编译错误**。

### 与 GC 的关系

所有权机制是**编译期的静态分析层**，不替代 Go 的垃圾回收：

- 所有权检查发生在编译期，**零运行时开销**。
- 通过所有权检查的代码保证不存在 use-after-move 和数据竞争（单线程内）。
- 内存回收仍由 Go GC 负责。所有权机制提供的是**逻辑正确性**保证，而非内存管理。

### 当前限制

- 跨 goroutine 的所有权转移需要通过 channel 显式传递，编译器会检查发送后原值不再使用。
- 生命周期标注尚不支持，当前依赖作用域推断。
- 与 Go 接口的交互：实现接口方法时，接收者的所有权语义遵循方法签名声明。

---

## 编译模型

Typus 编译到标准 Go 代码。核心转换规则：

| Typus 概念 | Go 编译产物 |
|------------|-------------|
| 值参数 `[n: int]` | 结构体运行时字段 `_n int` |
| 精确类型约束 | 运行时检查函数 |
| `assert` | `if !cond { panic(...) }` 或空（编译期可证明时） |
| `static_assert` | 空（编译期必须证明，否则编译失败） |
| 所有权 / 借用 | 擦除（纯编译期检查，不影响生成代码） |

### 示例

**Typus 源码**：

```go
//! dependent_types: on

package main

type NonZero = int where { self != 0 }
type Vector[n: int] struct {
    data [n]float64
}

func zeros(n: Positive) -> Vector[n] {
    return Vector[n]{data: make([]float64, n)}
}

func get[n: int](v: Vector[n], i: ValidIndex[n]) -> float64 {
    return v.data[i]
}

func safeDiv(a: int, b: NonZero) -> int {
    return a / b
}
```

**编译产物**：

```go
package main

import "fmt"

type Vector struct {
    _n   int
    data []float64
}

func zeros(n int) Vector {
    if !(n > 0) {
        panic("typus: constraint Positive violated: expected n > 0")
    }
    return Vector{_n: n, data: make([]float64, n)}
}

func get(v Vector, i int) float64 {
    if !(i >= 0 && i < v._n) {
        panic("typus: constraint ValidIndex violated: expected 0 <= i < n")
    }
    return v.data[i]
}

func safeDiv(a int, b int) int {
    if !(b != 0) {
        panic("typus: constraint NonZero violated: expected b != 0")
    }
    return a / b
}
```

### 错误处理策略

默认情况下约束违反产生 `panic`。可通过指令切换为 `error` 模式：

```go
//! constraint_mode: error
```

此模式下，带约束参数的函数会编译为返回 `error` 的 Go 函数：

```go
// constraint_mode: error 下的编译产物
func safeDiv(a int, b int) (int, error) {
    if !(b != 0) {
        return 0, fmt.Errorf("typus: constraint NonZero violated: expected b != 0")
    }
    return a / b, nil
}
```

---

## 与 Go 互操作

### 调用 Go 包

Typus 代码可以直接 import 和调用任何 Go 包。Go 函数在 Typus 中被视为无约束签名：

```go
//! dependent_types: on

package main

import "sort"

func sortedFirst[n: int](v: Vector[n]) -> float64
    where { n > 0 }
{
    sort.Float64s(v.data)   // 直接调用 Go 标准库
    return v.data[0]
}
```

### 导出给 Go 代码

编译后的 Go 代码是普通 Go 包，可被其他 Go 代码正常导入。导出函数名遵循以下规则：

- 函数名保持不变（`zeros` → `zeros`）。
- 值参数和约束被擦除为普通参数和运行时检查。
- 导出类型会额外包含以 `_` 开头的运行时字段（如 `_n int`），Go 调用方需要正确初始化这些字段。

```go
// Go 调用方
v := typuspkg.Zeros(3)          // 正常调用
x := typuspkg.Get(v, 0)        // 运行时检查 0 < 3
```

### 边界标注

对于从 Go 侧传入的值，可在 Typus 边界处用 `assert` 建立约束：

```go
// 接收 Go 传入的未约束值
func ProcessGoData(data []float64) {
    assert len(data) > 0
    v := readVector(data)
    // 此后可安全使用依赖类型操作
}
```

---

## 完整示例

```go
//! ownership: on
//! dependent_types: on

package main

import "fmt"

// ---- 精确类型 ----
type NonZero    = int where { self != 0 }
type Positive   = int where { self > 0 }
type ValidIndex[n: int] = int where { self >= 0 && self < n }

// ---- 值参数化类型 ----
type Vector[n: int] struct {
    data [n]float64
}

// ---- 构造 ----
func zeros(n: Positive) -> Vector[n] {
    return Vector[n]{data: make([]float64, n)}
}

func ones(n: Positive) -> Vector[n] {
    data := make([]float64, n)
    for i := 0; i < n; i++ {
        data[i] = 1.0
    }
    return Vector[n]{data: data}
}

// ---- 安全访问 ----
func get[n: int](v: Vector[n], i: ValidIndex[n]) -> float64 {
    return v.data[i]
}

func set[n: int](v: *Vector[n], i: ValidIndex[n], val: float64) {
    v.data[i] = val
}

// ---- 向量运算 ----
func add[n: int](a: Vector[n], b: Vector[n]) -> Vector[n] {
    result := zeros(n)
    for i := 0; i < n; i++ {
        result.data[i] = a.data[i] + b.data[i]
    }
    return result
}

func dot[n: int](a: Vector[n], b: Vector[n]) -> float64
    where { n > 0 }
{
    sum := 0.0
    for i := 0; i < n; i++ {
        sum += a.data[i] * b.data[i]
    }
    return sum
}

func concat[m: int, n: int](a: Vector[m], b: Vector[n]) -> Vector[m + n] {
    result := make([]float64, m+n)
    copy(result, a.data)
    copy(result[m:], b.data)
    return Vector[m+n]{data: result}
}

// ---- 安全除法 ----
func safeDiv(a: int, b: NonZero) -> int {
    return a / b
}

// ---- 矩阵 ----
type Matrix[rows: int, cols: int] struct {
    data [rows][cols]float64
}

func matMul[m: int, n: int, p: int](
    a: Matrix[m, n],
    b: Matrix[n, p],
) -> Matrix[m, p]
    where { m > 0, n > 0, p > 0 }
{
    result := Matrix[m, p]{}
    for i := 0; i < m; i++ {
        for j := 0; j < p; j++ {
            for k := 0; k < n; k++ {
                result.data[i][j] += a.data[i][k] * b.data[k][j]
            }
        }
    }
    return result
}

// ---- 入口 ----
func main() {
    fmt.Println("Hello, Typus!")

    // 所有权
    {//! ownership: on
        s := NewMyString("hello")
        t := s                       // 移动
        fmt.Println(t.data)
        // fmt.Println(s.data)       // ✗ 编译错误：s 已移动
    }

    // 依赖类型——编译期安全
    v1 := zeros(3)                   // Vector[3]
    v2 := ones(3)                    // Vector[3]
    v3 := add(v1, v2)               // ✓ 维度匹配
    x  := get(v3, 0)                // ✓ 编译期验证 0 < 3
    y  := get(v3, 2)                // ✓ 编译期验证 2 < 3
    // get(v3, 5)                   // ✗ 编译期错误

    v4 := zeros(4)                   // Vector[4]
    // add(v1, v4)                  // ✗ Vector[3] ≠ Vector[4]

    v7 := concat(v3, v4)            // Vector[7] ✓

    fmt.Printf("v3[0]=%.1f v3[2]=%.1f v7.len=%d\n", x, y, 7)

    // 安全除法
    r := safeDiv(10, 2)              // ✓ 编译期验证
    fmt.Printf("10 / 2 = %d\n", r)

    // 运行时值——条件窄化
    d := readInt()
    if d != 0 {
        r2 := safeDiv(10, d)         // ✓ 分支内 d 满足 NonZero
        fmt.Printf("10 / d = %d\n", r2)
    }

    // 动态维度——assert 窄化
    n := readInt()
    assert n > 0
    vn := zeros(n)
    fmt.Printf("dynamic vector, length %d\n", n)
}
```

---

## 语法速查表

| 特性 | 语法 | 说明 |
|------|------|------|
| 值参数化类型 | `type T[n: int]` | 类型由值参数化 |
| 精确类型 | `type T = int where { self > 0 }` | 给基础类型附加谓词 |
| 参数化精确类型 | `type T[lo: int, hi: int] = int where { ... }` | 参数化约束 |
| 依赖返回类型 | `func f(n: int) -> Vector[n]` | 返回类型依赖参数 |
| 依赖参数类型 | `func f[n: int](v: Vector[n], i: ValidIndex[n])` | 参数间依赖 |
| 函数前置条件 | `where { n > 0 }` | 函数签名后的约束 |
| 类型级算术 | `Vector[m + n]` | 类型中的值表达式 |
| 混合参数 | `type T[E any, n: int]` | 类型参数 + 值参数 |
| 存在类型 | `Vector[some n: int]` | 值参数运行时确定 |
| 存在解包 | `match v.(n) { ... }` | 绑定存在量化的值 |
| 断言窄化 | `assert expr` | 运行时检查（可能省略） |
| 静态断言 | `static_assert expr` | 必须编译期证明 |
| 条件窄化 | `if d != 0 { safeDiv(10, d) }` | 分支内自动窄化 |
| 不可变借用 | `r := &x` | 所有权模式下的借用 |
| 可变借用 | `m := &mut x` | 所有权模式下的可变借用 |
| 错误模式 | `//! constraint_mode: error` | 约束违反返回 error |

---

## 约束求解器

Typus 的约束求解器支持以下能力，超出范围的约束需要用户通过 `assert` 或 `static_assert` 显式标注。

### 支持的推理

| 类别 | 示例 | 说明 |
|------|------|------|
| 常量求值 | `get(v, 2)` 当 `v: Vector[3]` → 验证 `2 < 3` | 字面量直接计算 |
| 线性整数算术 | `Vector[m + n]`、`n - 1 >= 0` | 加减、比较、等式 |
| 条件窄化 | `if x > 0 { ... }` → 分支内 `x: Positive` | `if`/`else` 分支 |
| 等式传播 | `a == b` → `Vector[a]` 可赋给 `Vector[b]` | 已知等式替换 |
| 简单不等式链 | `a > b, b > 0` → `a > 0` | 传递性推理 |

### 不支持的推理

| 类别 | 示例 | 需要的操作 |
|------|------|-----------|
| 非线性算术 | `n * n - (n-1) * (n+1) == 1` | 用 `static_assert` 或将结果绑定到中间变量 |
| 用户定义函数展开 | `Vector[f(n)]` 其中 `f` 是自定义函数 | 在调用处 `assert` 结果的约束 |
| 归纳证明 | 递归函数的终止性 | 当前不检查终止性 |
| 浮点约束 | `self > 0.0` | 仅支持整数约束 |

### 诊断工具

```bash
# 查看约束求解过程
typus check --show-constraints input.typus

# 查看哪些约束被静态证明、哪些插入了运行时检查
typus check --show-evidence input.typus
```

---

## 环境变量

| 变量 | 说明 |
|------|------|
| `TYPUS_SKIP_GO_BUILD` | 设为 `1`/`true`/`yes`/`on` 时跳过 Go 工具链调用，仅执行 Typus → Go 转换 |

---

## 测试

```bash
# 单元测试（默认运行typus-test套件）
cabal test

# 完整测试套件（含集成 / Golden 测试）
cabal test --flags="-fast full"

# 运行特定测试套件
cabal test typus-test              # 主要测试套件
cabal test typus-test-enhanced     # 增强内存优化测试套件
cabal test typus-test-optimized    # 内存优化测试套件

# 运行特定测试模式（使用Tasty框架选项）
cabal test --test-options="--pattern \"Parser\""  # 运行名称包含"Parser"的测试
cabal test --test-options="--test-name \"具体测试名\"" # 运行特定名称的测试

# 生产环境测试（包含警告检查）
./scripts/run_production_tests.sh

# 内存优化测试
./scripts/run_memory_optimized_tests.sh

# 极度内存优化测试（适用于CI/CD或内存受限环境）
./scripts/run_ultra_memory_optimized_tests.sh

# 增强内存优化测试
./scripts/enhanced_memory_test.sh

# 极度增强内存优化测试
./scripts/run_extreme_enhanced_memory_tests.sh
```

### 测试套件说明

- **typus-test**：主要测试套件，包含所有核心功能测试
- **typus-test-enhanced**：增强内存优化测试套件，提供高级内存管理功能
- **typus-test-optimized**：内存优化测试套件，适用于资源受限环境

### 测试标志说明

- **fast**：快速测试模式（默认启用），跳过耗时的测试
- **full**：包含慢速/集成/性能测试的完整测试套件
- **production**：生产级测试，包含严格的警告检查
- **werror**：将警告视为错误（需要手动启用）

### 测试类型说明

- **单元测试**：基础功能测试，使用默认内存限制，适用于日常开发验证
- **完整测试套件**：包含集成测试和Golden测试，较慢但全面，适用于发布前全面验证
- **内存优化测试**：使用512MB内存限制，适用于资源受限环境，如容器化部署或低配置开发机
- **极度内存优化测试**：使用16MB内存限制，只运行10个最关键的测试，适用于CI/CD或极度受限环境，如GitHub Actions免费版或其他内存受限的CI环境

更多文档请参考项目源码和测试用例

---

## 许可证

[LICENSE](LICENSE)