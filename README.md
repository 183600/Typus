# Typus
- Typus 是一个编程语言，在 Go 基础上增加所有权机制和其他一些特性
- Typus 引入了强大的类型系统，融合了依赖类型 (Dependent Types) 的核心思想和精确类型 (Refinement Types) 的实用语法，允许类型由值来约束和参数化。这使得在编译时就能保证更深层次的程序正确性。
- 编译到 Go
- 所有权机制可以开关
- 即使使用了所有权机制，也还是会有 GC
- 支持根据代码本身是否是按照所有权机制写的来判断是否按照所有权机制的代码处理
- [交流群](https://qun.qq.com/universal-share/share?ac=1&authKey=gzdz%2FVOju6Rm8Cbnmu71zCvdIpSbN8sltlf1ahjpk3jhXU0RZdQcov%2Fa0i3nhv9q&busi_data=eyJncm91cENvZGUiOiI4MTA0NzY2NzgiLCJ0b2tlbiI6IlNzR1dHaFN1NXpycWN3cVVGV09JWmVMSnZ5OC9FU2lpQW13ZEVEOXg2S1NEcC9VQWRkaEtla2xhZnhlYnh6T28iLCJ1aW4iOiIzMDI1Mzg1NDcyIn0%3D&data=DopbLeWghbu22Vwnj8AcfxUdz4A4VT5KpD--38y74CIsde08Q3YfC29Fms1jpaeOs_OyPAyt1Lpt_Jk2o8gZwA&svctype=4&tempid=h5_group_info)

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

## 环境变量

- `TYPUS_SKIP_GO_BUILD`: 设置为 `1`、`true`、`yes` 或 `on` 时会跳过所有 Go 工具链调用，仅执行 Typus -> Go 的转换，适合无法使用系统 Go 编译器或需要纯编译模式的场景。

## 测试文档导航

项目中的测试文档较多，可以通过 [docs/TEST_DOCUMENTATION_INDEX.md](docs/TEST_DOCUMENTATION_INDEX.md)
查看各文档的定位与用途。

### 运行测试

```bash
# 默认运行快速单元测试
cabal test

# 运行完整测试套件（包含集成 / Golden 测试）
cabal test --flags="-fast full"

# 运行生产级测试（启用严格校验）
cabal test --flags="-fast production"

# 生成覆盖率报告（推荐关闭 fast 以获取完整数据）
cabal test --flags="-fast coverage"

# 运行特定测试模块
cabal test --test-options="--pattern \"Parser\""
```

更多测试相关信息请参考:
- [QUICK_TEST_GUIDE.md](QUICK_TEST_GUIDE.md) - 快速测试指南
- [COVERAGE_MATRIX.md](COVERAGE_MATRIX.md) - 测试覆盖率矩阵
- [TEST_CONSOLIDATION.md](TEST_CONSOLIDATION.md) - 测试整合说明

## Fixture 与示例资源

- `fixtures/reference/` 收纳了历史调试脚本、手动测试文件等 `.typus` 资源，适合快速验证编译与所有权分析逻辑。
- `examples/` 目录保留了文档和演示使用的示例程序，可配合脚本与指南一起使用。

原先散落在仓库根目录的 `.typus` 文件已经统一迁移到上述目录，便于查找并保持根目录整洁。

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

