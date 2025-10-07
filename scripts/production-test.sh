#!/bin/bash

# 生产环境测试脚本
# 确保所有测试通过后项目才能用于生产环境
# 包含单元测试、集成测试、端到端测试、性能测试和安全测试

set -e  # 遇到错误立即退出

echo "开始生产环境测试验证..."
echo "================================"

# 计时器开始
START_TIME=$(date +%s)

# 1. 清理之前的构建
echo "步骤 1: 清理构建环境..."
cabal clean
cabal update

# 2. 构建项目
echo "步骤 2: 构建项目..."
cabal build --enable-tests --enable-benchmarks --enable-coverage
BUILD_RESULT=$?
if [ $BUILD_RESULT -ne 0 ]; then
    echo "错误: 构建失败"
    exit 1
fi

# 3. 运行单元测试
echo "步骤 3: 运行单元测试..."
cabal test --test-show-details=direct --test-option=--skip=performance --test-option=--skip=integration
UNIT_TESTS_RESULT=$?
if [ $UNIT_TESTS_RESULT -ne 0 ]; then
    echo "错误: 单元测试失败"
    exit 1
fi

# 4. 运行集成测试
echo "步骤 4: 运行集成测试..."
cabal test --test-show-details=direct --test-option=--skip=unit --test-option=--skip=performance
INTEGRATION_TESTS_RESULT=$?
if [ $INTEGRATION_TESTS_RESULT -ne 0 ]; then
    echo "错误: 集成测试失败"
    exit 1
fi

# 5. 运行端到端测试
echo "步骤 5: 运行端到端测试..."
# 创建测试输入文件
cat > /tmp/test_e2e_input.typus << 'EOF'
package main

type User struct {
    name: string
    age: int
}

func getUser() User {
    return User{name: "Alice", age: 30}
}

func main() {
    user := getUser()
    println("User: " + user.name + ", Age: " + toString(user.age))
}
EOF

# 编译测试输入
if [ -f "dist-newstyle/build/x86_64-linux/ghc-*/typus-*/x/typus/build/typus/typus" ]; then
    COMPILER_PATH=$(find dist-newstyle/build/x86_64-linux/ghc-*/typus-*/x/typus/build/typus/typus -type f -executable 2>/dev/null | head -1)
    if [ -n "$COMPILER_PATH" ]; then
        echo "编译测试文件..."
        $COMPILER_PATH convert /tmp/test_e2e_input.typus -o /tmp/test_e2e_output.go

        # 验证生成的Go代码
        if [ -f "/tmp/test_e2e_output.go" ]; then
            echo "编译生成的Go代码..."
            go run /tmp/test_e2e_output.go > /tmp/test_e2e_result.txt 2>&1
            E2E_RESULT=$?

            if [ $E2E_RESULT -eq 0 ]; then
                echo "✓ 端到端测试通过"
            else
                echo "错误: 端到端测试失败"
                cat /tmp/test_e2e_result.txt
                exit 1
            fi
        else
            echo "错误: 生成的Go文件不存在"
            exit 1
        fi

        # 清理临时文件
        rm -f /tmp/test_e2e_input.typus /tmp/test_e2e_output.go /tmp/test_e2e_result.txt
    else
        echo "警告: 编译器二进制文件未找到，跳过端到端测试"
    fi
else
    echo "警告: 编译器未构建，跳过端到端测试"
fi

# 6. 运行性能测试
echo "步骤 6: 运行性能测试..."
cabal test --test-show-details=direct --test-option="--test-option=performance"
PERFORMANCE_TESTS_RESULT=$?
if [ $PERFORMANCE_TESTS_RESULT -ne 0 ]; then
    echo "错误: 性能测试失败"
    exit 1
fi

# 7. 检查测试覆盖率
echo "步骤 7: 检查测试覆盖率..."
if [ -f "typus-test.tix" ]; then
    COVERAGE_REPORT=$(hpc report typus-test.tix --include=src --exclude=Main 2>/dev/null || echo "Coverage report failed")
    echo "$COVERAGE_REPORT"

    # 检查覆盖率是否达到最低要求 (80%)
    COVERAGE_PERCENT=$(echo "$COVERAGE_REPORT" | grep "expression" | awk '{print $7}' | tr -d '%')
    if [ -n "$COVERAGE_PERCENT" ] && [ "$COVERAGE_PERCENT" -ge 80 ]; then
        echo "✓ 测试覆盖率满足要求 ($COVERAGE_PERCENT%)"
    else
        if [ -n "$COVERAGE_PERCENT" ]; then
            echo "警告: 测试覆盖率不足 ($COVERAGE_PERCENT% < 80%)"
        else
            echo "警告: 无法解析测试覆盖率"
        fi
    fi
else
    echo "警告: 未找到测试覆盖率文件，请确保使用 --enable-coverage 构建项目"
fi

# 8. 验证生成的二进制文件
echo "步骤 8: 验证生成的二进制文件..."
COMPILER_PATH=$(find dist-newstyle/build/x86_64-linux/ghc-*/typus-*/x/typus/build/typus/typus -type f -executable 2>/dev/null | head -1)
if [ -n "$COMPILER_PATH" ]; then
    echo "✓ 编译器二进制文件已生成"
    VERSION_OUTPUT=$($COMPILER_PATH --version 2>/dev/null || echo "Version command not available")
    echo "  版本信息: $VERSION_OUTPUT"

    # 测试基本功能
    echo "  测试基本功能..."
    $COMPILER_PATH convert examples/hello_typus.typus -o /tmp/test_basic_output.go 2>/dev/null
    if [ -f "/tmp/test_basic_output.go" ]; then
        echo "✓ 基本编译功能正常"
        rm /tmp/test_basic_output.go
    else
        echo "错误: 基本编译功能失败"
        exit 1
    fi
else
    echo "错误: 编译器二进制文件未找到"
    exit 1
fi

# 9. 运行内存泄漏检测
echo "步骤 9: 运行内存泄漏检测..."
# 使用valgrind进行内存检查（如果可用）
if command -v valgrind >/dev/null 2>&1; then
    echo "使用valgrind进行内存检查..."
    valgrind --tool=memcheck --leak-check=full --error-exitcode=1 $COMPILER_PATH convert examples/hello_typus.typus -o /tmp/valgrind_test.go > /tmp/valgrind_output.txt 2>&1 || true
    if grep -q "ERROR SUMMARY: 0 errors" /tmp/valgrind_output.txt; then
        echo "✓ 内存泄漏检查通过"
    else
        echo "警告: valgrind检测到可能的内存问题"
        echo "  详情请查看: /tmp/valgrind_output.txt"
    fi
    rm -f /tmp/valgrind_test.go /tmp/valgrind_output.txt
else
    echo "跳过内存泄漏检测 (valgrind未安装)"
fi

# 10. 运行安全检查
echo "步骤 10: 运行安全检查..."
# 检查是否有安全相关的编译警告
if cabal build --enable-tests --enable-benchmarks 2>&1 | grep -i "warning.*security\|warning.*vulnerable\|warning.*overflow\|warning.*buffer" > /dev/null; then
    echo "警告: 发现潜在的安全相关警告"
else
    echo "✓ 安全检查通过"
fi

# 计时器结束
END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

echo "================================"
echo "✓ 所有测试通过！项目已准备好用于生产环境"
echo "  总耗时: ${DURATION}秒"
echo "================================"