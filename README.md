

# Typus

Typus 是一个编程语言，在 Go 基础上增加所有权机制、依赖类型系统和其他一些特性，编译到 Go。

- 所有权机制可以开关，即使启用也还是会有 GC
- 支持根据代码本身是否按照所有权机制写的来判断是否按照所有权机制处理
- 融合依赖类型 (Dependent Types) 和精确类型 (Refinement Types)，允许类型由值来约束和参数化，在编译时保证更深层次的程序正确性

[交流群](https://qun.qq.com/universal-share/share?ac=1&authKey=gzdz%2FVOju6Rm8Cbnmu71zCvdIpSbN8sltlf1ahjpk3jhXU0RZdQcov%2Fa0i3nhv9q&busi_data=eyJncm91cENvZGUiOiI4MTA0NzY2NzgiLCJ0b2tlbiI6IlNzR1dHaFN1NXpycWN3cVVGV09JWmVMSnZ5OC9FU2lpQW13ZEVEOXg2S1NEcC9VQWRkaEtla2xhZnhlYnh6T28iLCJ1aW4iOiIzMDI1Mzg1NDcyIn0%3D&data=DopbLeWghbu22Vwnj8AcfxUdz4A4VT5KpD--38y74CIsde08Q3YfC29Fms1jpaeOs_OyPAyt1Lpt_Jk2o8gZwA&svctype=4&tempid=h5_group_info)

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

```bash
# 转换单个文件
typus convert input.typus -o output.go

# 转换目录
typus convert src/ -o out/

# 检查语法
typus check input.typus

# 构建项目（调用 go build）
typus build

# 运行项目（调用 go run）
typus run main.typus

# 查看版本
typus --version
typus -v
```

## 环境变量

- `TYPUS_SKIP_GO_BUILD`：设置为 `1`、`true`、`yes` 或 `on` 时跳过所有 Go 工具链调用，仅执行 Typus → Go 的转换。

---

## 指令系统

### 文件级指令

```go
//! ownership: on
//! dependent_types: on

package main
```

`dependent_types` 也可以写成 `constraints`。

### 块级指令

```go
func main() {
    // 常规 Go 代码

    {//! ownership: on
        // 具有所有权语义的代码
    }

    {//! dependent_types: on
        // 具有依赖类型的代码
    }
}
```

一个块可以同时启用所有权和依赖类型。

---

## 依赖类型与精确类型

Typus 的类型系统允许**类型由值来参数化**，并允许对值施加**编译期可验证的约束**。当编译器无法静态证明约束成立时，会自动插入运行时检查。

### 1. 值参数化类型

用 `[name: type]` 声明类型级值参数，区别于 Go 的类型参数 `[T any]`：

```go
// 长度参数化的向量
type Vector[n: int] struct {
    data [n]float64
}

// 行列参数化的矩阵
type Matrix[rows: int, cols: int] struct {
    data [rows][cols]float64
}

// 混合类型参数与值参数
type BoundedSlice[T any, cap: int] struct {
    data []T
}
```

`Vector[3]` 和 `Vector[4]` 是完全不同的类型，编译器禁止混用。

### 2. 精确类型

用 `type ... where` 给基础类型附加谓词，`self` 指代被约束的值本身：

```go
type NonZero = int where { self != 0 }

type Positive = int where { self > 0 }

type Bounded[lo: int, hi: int] = int where { self >= lo && self <= hi }

type Percentage = Bounded[0, 100]

type NonEmpty = string where { len(self) > 0 }

type ValidIndex[n: int] = int where { self >= 0 && self < n }
```

### 3. 依赖函数签名

返回类型和参数类型可以引用其他参数的值：

```go
// 返回类型依赖参数
func zeros(n: Positive) -> Vector[n] {
    return Vector[n]{data: make([]float64, n)}
}

// 参数类型依赖另一个参数
func get[n: int](v: Vector[n], i: ValidIndex[n]) -> float64 {
    return v.data[i]
}

// 安全除法
func safeDiv(a: int, b: NonZero) -> int {
    return a / b
}

// 返回类型依赖多个参数
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

### 4. 函数级 `where` 子句

在函数签名之后附加前置条件：

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

func split[n: int](v: Vector[n]) -> (Vector[n/2], Vector[n - n/2])
    where { n > 0 }
{
    mid := n / 2
    left := Vector[n/2]{data: v.data[:mid]}
    right := Vector[n-n/2]{data: v.data[mid:]}
    return left, right
}
```

### 5. 存在类型

当值参数在运行时才确定时，用 `some` 标记：

```go
func readVector(input: []float64) -> Vector[some n: int] {
    return Vector[len(input)]{data: input}
}

func processVector() {
    data := []float64{1.0, 2.0, 3.0}
    v := readVector(data)

    let n = v.length in {
        // 此块中 v 的类型为 Vector[n]
        fmt.Println(get(v, 0))
    }
}
```

### 6. 断言提升与条件窄化

```go
func processInput(n: int) {
    // assert 窄化类型：若编译器无法静态验证，则插入运行时检查
    assert n > 0

    // 此后 n 被精确化为 Positive
    v := zeros(n)
    avg := average(v)    // ✓ 已知 n > 0
}
```

编译器也能识别 `if` 分支中的条件：

```go
d := readInt()
// safeDiv(10, d)          // ✗ 无法证明 d != 0

if d != 0 {
    r := safeDiv(10, d)    // ✓ 分支中 d != 0
}
```

### 7. 编译期常量传播

当值在编译期已知时，编译器直接静态验证，不插入运行时检查：

```go
v := zeros(3)            // v: Vector[3]
x := get(v, 0)           // ✓ 编译期验证 0 < 3
y := get(v, 2)           // ✓ 编译期验证 2 < 3
// z := get(v, 5)        // ✗ 编译期错误：5 ≥ 3

r := safeDiv(10, 2)      // ✓ 编译期验证 2 ≠ 0
```

当值在运行时确定时，自动插入运行时检查：

```go
n := readInt()
assert n > 0
vn := zeros(n)           // 插入运行时检查 n > 0

idx := readInt()
w := get(vn, idx)        // 插入运行时检查 idx >= 0 && idx < n
```

### 8. 类型推导

Typus 保留 Go 的类型推导能力，并扩展到依赖类型上下文中：

```go
func createVector(n: Positive, value: float64) -> Vector[n] {
    elements := make([]float64, n)
    for i := 0; i < n; i++ {
        elements[i] = value
    }
    return Vector{elements}  // 类型自动推导为 Vector[n]
}
```

---

## 编译到 Go

Typus 的依赖类型特性编译为普通 Go 代码。类型级值参数退化为运行时字段，约束退化为运行时检查。

**Typus 源码**：

```go
//! dependent_types: on

package main

type NonZero = int where { self != 0 }
type Positive = int where { self > 0 }
type ValidIndex[n: int] = int where { self >= 0 && self < n }

type Vector[n: int] struct {
    data [n]float64
}

func zeros(n: Positive) -> Vector[n] {
    return Vector[n]{data: make([]float64, n)}
}

func get[n: int](v: Vector[n], i: ValidIndex[n]) -> float64 {
    return v.data[i]
}

func add[n: int](a: Vector[n], b: Vector[n]) -> Vector[n] {
    result := zeros(n)
    for i := 0; i < n; i++ {
        result.data[i] = a.data[i] + b.data[i]
    }
    return result
}

func safeDiv(a: int, b: NonZero) -> int {
    return a / b
}
```

**编译后的 Go 代码**：

```go
package main

type Vector struct {
    _n   int
    data []float64
}

func zeros(n int) Vector {
    if !(n > 0) {
        panic("typus: Positive constraint violated: n must be > 0")
    }
    return Vector{_n: n, data: make([]float64, n)}
}

func vectorGet(v Vector, i int) float64 {
    if !(i >= 0 && i < v._n) {
        panic("typus: ValidIndex constraint violated")
    }
    return v.data[i]
}

func vectorAdd(a Vector, b Vector) Vector {
    if a._n != b._n {
        panic("typus: dimension mismatch: Vector[a.n] != Vector[b.n]")
    }
    n := a._n
    result := zeros(n)
    for i := 0; i < n; i++ {
        result.data[i] = a.data[i] + b.data[i]
    }
    return result
}

func safeDiv(a int, b int) int {
    if !(b != 0) {
        panic("typus: NonZero constraint violated: b must be != 0")
    }
    return a / b
}
```

---

## 完整示例

```go
//! ownership: on
//! dependent_types: on

package main

import "fmt"

// --- 精确类型 ---
type NonZero = int where { self != 0 }
type Positive = int where { self > 0 }
type ValidIndex[n: int] = int where { self >= 0 && self < n }

// --- 值参数化类型 ---
type Vector[n: int] struct {
    data [n]float64
}

// --- 构造 ---
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

// --- 安全访问 ---
func get[n: int](v: Vector[n], i: ValidIndex[n]) -> float64 {
    return v.data[i]
}

func set[n: int](v: *Vector[n], i: ValidIndex[n], val: float64) {
    v.data[i] = val
}

// --- 向量运算 ---
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

// --- 安全除法 ---
func safeDiv(a: int, b: NonZero) -> int {
    return a / b
}

// --- 矩阵 ---
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

// --- 所有权 ---
type MyString struct {
    data string
}

func NewMyString(s string) MyString {
    return MyString{data: s}
}

// --- 入口 ---
func main() {
    fmt.Println("Hello, Typus!")

    // 所有权块
    {//! ownership: on
        s := NewMyString("hello")
        t := s  // 所有权转移
        fmt.Println(t.data)
    }

    // 依赖类型块
    {//! dependent_types: on
        // 编译期安全
        v1 := zeros(3)            // Vector[3]
        v2 := ones(3)             // Vector[3]
        v3 := add(v1, v2)         // Vector[3] ✓ 维度匹配
        x := get(v3, 0)           // ✓ 编译期验证 0 < 3
        y := get(v3, 2)           // ✓ 编译期验证 2 < 3
        // get(v3, 5)             // ✗ 编译期错误：5 ≥ 3

        // 维度不匹配 —— 编译错误
        v4 := zeros(4)            // Vector[4]
        // add(v1, v4)            // ✗ Vector[3] 与 Vector[4] 不匹配

        // 向量拼接
        v7 := concat(v3, v4)     // Vector[7] ✓

        fmt.Printf("v3[0] = %.1f\n", x)
        fmt.Printf("v3[2] = %.1f\n", y)
        fmt.Printf("v7 length = %d\n", 7)
    }

    // 安全除法
    r := safeDiv(10, 2)           // ✓ 编译期验证 2 ≠ 0
    fmt.Printf("10 / 2 = %d\n", r)

    // 运行时值需要手动证明
    d := readInt()
    if d != 0 {
        r2 := safeDiv(10, d)      // ✓ 分支中 d ≠ 0
        fmt.Printf("10 / d = %d\n", r2)
    }

    // 动态维度需要 assert
    n := readInt()
    assert n > 0
    vn := zeros(n)                // Vector[n]
    fmt.Printf("dynamic vector created with length %d\n", n)
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
| 存在类型 | `Vector[some n: int]` | 长度运行时确定 |
| 存在类型解包 | `let n = v.length in { ... }` | 绑定存在量化的值 |
| 断言窄化 | `assert n > 0` | 窄化值的精确类型 |
| 条件窄化 | `if d != 0 { safeDiv(10, d) }` | 分支中自动精确化 |

---

## 测试

```bash
# 默认运行快速单元测试
cabal test

# 运行完整测试套件（包含集成 / Golden 测试）
cabal test --flags="-fast full"

# 运行生产级测试
cabal test --flags="-fast production"

# 生成覆盖率报告
cabal test --flags="-fast coverage"

# 运行特定测试模块
cabal test --test-options="--pattern \"Parser\""

# 内存优化测试
./scripts/run_memory_optimized_tests.sh        # 256MB
./scripts/run_ultra_memory_optimized_tests.sh   # 128MB
```

更多测试文档：
- [QUICK_TEST_GUIDE.md](QUICK_TEST_GUIDE.md)
- [COVERAGE_MATRIX.md](COVERAGE_MATRIX.md)
- [TEST_CONSOLIDATION.md](TEST_CONSOLIDATION.md)
- [MEMORY_OPTIMIZATION.md](docs/MEMORY_OPTIMIZATION.md)
- [TEST_DOCUMENTATION_INDEX.md](docs/TEST_DOCUMENTATION_INDEX.md)

## Fixture 与示例

- `fixtures/reference/`：历史调试脚本和手动测试文件
- `examples/`：文档和演示使用的示例程序