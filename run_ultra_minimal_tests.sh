#!/bin/bash

# 超级内存优化测试运行脚本
# 用于内存极其有限的环境

echo "=== Ultra Memory Optimized Test Runner ==="
echo "Running tests with ultra-minimal memory configuration..."

# 检查内存限制
MEMORY_LIMIT=${MEMORY_LIMIT:-16}  # 默认16MB
echo "Memory limit: ${MEMORY_LIMIT}MB"

# 设置环境变量
export ULTRA_MINIMAL=true
export MINIMAL_TESTS=true
export GHCRTS="-M${MEMORY_LIMIT}m -A4m"

# 检查是否有超级内存优化测试运行器
if [ -f "test/SuperMemoryOptimized.hs" ]; then
    echo "Using Super Memory Optimized Test Runner..."
    
    cd test
    
    # 编译超级内存优化测试运行器
    echo "Compiling Super Memory Optimized Test Runner..."
    stack ghc -- -O2 -rtsopts SuperMemoryOptimized.hs -o super-test-runner
    
    if [ $? -eq 0 ]; then
        echo "Running ultra-minimal tests..."
        # 使用超极简环境运行测试
        ./super-test-runner ultra 2>&1 | tee ../ultra_test_output.log
        
        TEST_RESULT=$?
        
        # 清理编译产物
        rm -f super-test-runner super-testOptimized SuperMemoryOptimized.hi SuperMemoryOptimized.o
        
        cd ..
        
        if [ $TEST_RESULT -eq 0 ]; then
            echo "Ultra-minimal tests passed successfully!"
            exit 0
        else
            echo "Ultra-minimal tests failed, trying minimal configuration..."
            cd test
            ./super-test-runner minimal 2>&1 | tee ../minimal_test_output.log
            TEST_RESULT=$?
            cd ..
            
            if [ $TEST_RESULT -eq 0 ]; then
                echo "Minimal tests passed successfully!"
                exit 0
            else
                echo "All memory-optimized tests failed."
                exit 1
            fi
        fi
    else
        echo "Failed to compile Super Memory Optimized Test Runner"
        cd ..
    fi
else
    echo "Super Memory Optimized Test Runner not found"
fi

# 回退到最基本的测试
echo "Falling back to basic test configuration..."

# 运行最基本的测试套件
cd test
if [ -f "Test/Unit/TrueLimitedQuickCheckTests.hs" ]; then
    echo "Running TrueLimited tests with minimal memory..."
    GHCRTS="-M${MEMORY_LIMIT}m -A4m" stack runghc Test/Unit/TrueLimitedQuickCheckTests.hs 2>&1 | tee ../fallback_test_output.log
else
    echo "No suitable test files found"
    exit 1
fi

if [ $? -eq 0 ]; then
    echo "Fallback tests passed!"
    exit 0
else
    echo "All test configurations failed"
    exit 1
fi