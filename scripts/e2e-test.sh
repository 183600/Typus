#!/bin/bash

# 端到端测试脚本
# 测试完整的编译器工作流程

set -e

echo "运行端到端测试..."
echo "=================="

# 查找编译器二进制文件
COMPILER_PATH=$(find dist-newstyle/build/x86_64-linux/ghc-*/typus-*/x/typus/build/typus/typus -type f -executable 2>/dev/null | head -1)
if [ -z "$COMPILER_PATH" ]; then
    echo "错误: 编译器二进制文件未找到"
    echo "请先运行 'npm run build' 构建项目"
    exit 1
fi

echo "使用编译器: $COMPILER_PATH"

# 创建临时目录
TEST_DIR="/tmp/typus_e2e_$$"
mkdir -p "$TEST_DIR"

# 清理函数
cleanup() {
    echo "清理临时文件..."
    rm -rf "$TEST_DIR"
}
trap cleanup EXIT

# 测试用例1: 基本语法
test_basic_syntax() {
    echo "测试1: 基本语法编译"
    cat > "$TEST_DIR/basic.typus" << 'EOF'
package main

func main() {
    println("Hello, World!")
}
EOF

    $COMPILER_PATH convert "$TEST_DIR/basic.typus" -o "$TEST_DIR/basic.go"

    if [ -f "$TEST_DIR/basic.go" ]; then
        echo "✓ 基本语法编译成功"
        go run "$TEST_DIR/basic.go" > "$TEST_DIR/basic_output.txt" 2>&1
        if [ $? -eq 0 ]; then
            echo "✓ 生成的Go代码运行成功"
        else
            echo "错误: 生成的Go代码运行失败"
            cat "$TEST_DIR/basic_output.txt"
            return 1
        fi
    else
        echo "错误: 基本语法编译失败"
        return 1
    fi
}

# 测试用例2: 复杂类型
test_complex_types() {
    echo "测试2: 复杂类型处理"
    cat > "$TEST_DIR/complex.typus" << 'EOF'
package main

type User struct {
    name: string
    age: int
    active: bool
}

type Result[T] struct {
    value: T
    error: string
}

func createUser(name: string, age: int) User {
    return User{name: name, age: age, active: true}
}

func main() {
    user := createUser("Alice", 30)
    result := Result[User]{value: user, error: ""}
    println("User: " + result.value.name)
}
EOF

    $COMPILER_PATH convert "$TEST_DIR/complex.typus" -o "$TEST_DIR/complex.go"

    if [ -f "$TEST_DIR/complex.go" ]; then
        echo "✓ 复杂类型编译成功"
    else
        echo "错误: 复杂类型编译失败"
        return 1
    fi
}

# 测试用例3: 错误处理
test_error_handling() {
    echo "测试3: 错误处理"
    cat > "$TEST_DIR/error.typus" << 'EOF'
package main

func divide(a: int, b: int) int {
    if b == 0 {
        return -1  // 错误码
    }
    return a / b
}

func main() {
    result := divide(10, 2)
    if result >= 0 {
        println("结果: " + toString(result))
    }
}
EOF

    $COMPILER_PATH convert "$TEST_DIR/error.typus" -o "$TEST_DIR/error.go"

    if [ -f "$TEST_DIR/error.go" ]; then
        echo "✓ 错误处理代码编译成功"
    else
        echo "错误: 错误处理代码编译失败"
        return 1
    fi
}

# 测试用例4: 性能基准
test_performance_benchmark() {
    echo "测试4: 性能基准测试"
    cat > "$TEST_DIR/benchmark.typus" << 'EOF'
package main

func fibonacci(n: int) int {
    if n <= 1 {
        return n
    }
    return fibonacci(n - 1) + fibonacci(n - 2)
}

func main() {
    // 计算第20个斐波那契数
    result := fibonacci(20)
    println("Fibonacci(20) = " + toString(result))
}
EOF

    start_time=$(date +%s.%N)
    $COMPILER_PATH convert "$TEST_DIR/benchmark.typus" -o "$TEST_DIR/benchmark.go"
    end_time=$(date +%s.%N)
    compile_time=$(echo "$end_time - $start_time" | bc)

    if [ -f "$TEST_DIR/benchmark.go" ]; then
        echo "✓ 性能基准代码编译成功 (耗时: ${compile_time}s)"
    else
        echo "错误: 性能基准代码编译失败"
        return 1
    fi
}

# 测试用例5: 大文件处理
test_large_file() {
    echo "测试5: 大文件处理"

    # 创建一个较大的测试文件
    cat > "$TEST_DIR/large.typus" << 'EOF'
package main

// 生成大量的函数定义
EOF

    # 添加100个函数定义
    for i in $(seq 1 100); do
        cat >> "$TEST_DIR/large.typus" << EOF
func func$i(x: int) int {
    return x * $i
}

EOF
    done

    cat >> "$TEST_DIR/large.typus" << 'EOF'

func main() {
    result := 0
    result += func1(1)
    result += func2(2)
    result += func3(3)
    println("Result: " + toString(result))
}
EOF

    $COMPILER_PATH convert "$TEST_DIR/large.typus" -o "$TEST_DIR/large.go"

    if [ -f "$TEST_DIR/large.go" ]; then
        echo "✓ 大文件处理成功"
    else
        echo "错误: 大文件处理失败"
        return 1
    fi
}

# 运行所有测试
run_all_tests() {
    echo "开始运行端到端测试套件..."
    echo "=============================="

    local failed=0

    # 运行每个测试用例
    test_basic_syntax || failed=$((failed + 1))
    test_complex_types || failed=$((failed + 1))
    test_error_handling || failed=$((failed + 1))
    test_performance_benchmark || failed=$((failed + 1))
    test_large_file || failed=$((failed + 1))

    echo "=============================="
    if [ $failed -eq 0 ]; then
        echo "✓ 所有端到端测试通过！"
        return 0
    else
        echo "错误: $failed 个端到端测试失败"
        return 1
    fi
}

# 执行测试
run_all_tests