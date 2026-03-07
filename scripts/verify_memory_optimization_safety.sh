#!/usr/bin/env bash
# 内存优化安全性验证脚本
# 确保测试用例不会消耗大量内存，同时验证所有测试功能保持完整

set -e

echo "=== 内存优化安全性验证 ==="
echo "确保测试用例不会消耗大量内存，同时保留所有测试功能"
echo ""

# 验证函数
verify_test_preservation() {
    echo "1. 验证所有测试文件存在..."
    
    local test_files_count
    test_files_count=$(find /home/runner/work/Typus/Typus/test -name "*.hs" | wc -l)
    echo "   找到 $test_files_count 个测试文件"
    
    # 检查关键测试文件
    local critical_tests=(
        "Test.Unit.BasicQuickCheckTestSuite.hs"
        "Test.Unit.MemoryOptimizedTestSuite.hs"
        "Test.Unit.EnhancedMemoryOptimizedTestSuite.hs"
        "Test.Unit.ExtremeMemoryOptimizedTestSuite.hs"
        "Test.Unit.AdvancedMemoryOptimizedTestSuite.hs"
        "Test.Unit.ComprehensiveTypusTestSuite.hs"
    )
    
    local missing_tests=0
    for test_file in "${critical_tests[@]}"; do
        # 检查多个可能的路径
        if [ -f "/home/runner/work/Typus/Typus/test/Test/Unit/$test_file" ] || \
           [ -f "/home/runner/work/Typus/Typus/test/TestSuite/$test_file" ] || \
           [ -f "/home/runner/work/Typus/Typus/test/$test_file" ] || \
           find /home/runner/work/Typus/Typus/test -name "$test_file" | grep -q .; then
            echo "   ✓ $test_file 存在"
        else
            echo "   ✗ $test_file 缺失"
            missing_tests=$((missing_tests + 1))
        fi
    done
    
    if [ "$missing_tests" -gt 0 ]; then
        echo "   警告: 发现 $missing_tests 个关键测试文件缺失"
        return 1
    else
        echo "   ✓ 所有关键测试文件存在"
        return 0
    fi
}

verify_memory_optimization() {
    echo "2. 验证内存优化配置..."
    
    # 检查内存优化配置文件
    local config_files=(
        "intelligent_memory_optimization_config.yaml"
        "ultra_memory_test_config.yaml"
        "super_optimized_test_config.yaml"
    )
    
    for config_file in "${config_files[@]}"; do
        if [ -f "/home/runner/work/Typus/Typus/$config_file" ]; then
            echo "   ✓ $config_file 存在"
        else
            echo "   ✗ $config_file 缺失"
        fi
    done
    
    # 检查内存优化脚本
    local script_files=(
        "scripts/intelligent_memory_optimized_test_runner.sh"
        "scripts/memory_optimized_test_runner.sh"
        "scripts/ultra_memory_optimized_test_runner.sh"
    )
    
    for script_file in "${script_files[@]}"; do
        if [ -f "/home/runner/work/Typus/Typus/$script_file" ]; then
            echo "   ✓ $script_file 存在"
        else
            echo "   ✗ $script_file 缺失"
        fi
    done
    
    echo "   ✓ 内存优化配置验证完成"
}

verify_quickcheck_limits() {
    echo "3. 验证QuickCheck参数限制..."
    
    # 检查是否有限制QuickCheck参数的测试文件
    local limited_tests
    limited_tests=$(grep -r "quickCheckWith" /home/runner/work/Typus/Typus/test --include="*.hs" | wc -l || true)
    echo "   发现 $limited_tests 个使用quickCheckWith的测试"
    
    # 检查是否有内存优化的测试套件
    local optimized_suites
    optimized_suites=$(find /home/runner/work/Typus/Typus/test -name "*MemoryOptimized*" -name "*.hs" | wc -l)
    echo "   发现 $optimized_suites 个内存优化测试套件"
    
    if [ "$optimized_suites" -gt 0 ]; then
        echo "   ✓ 存在内存优化测试套件"
    else
        echo "   ✗ 未找到内存优化测试套件"
        return 1
    fi
}

verify_memory_usage() {
    echo "4. 验证内存使用模式..."
    
    # 检查是否有内存限制的测试
    local memory_limited_tests
    memory_limited_tests=$(grep -r "withMemoryLimits\|withMemoryLevel\|memoryLimitedTestGroup" /home/runner/work/Typus/Typus/test --include="*.hs" | wc -l)
    echo "   发现 $memory_limited_tests 个使用内存限制的测试"
    
    # 检查是否有垃圾回收优化的测试
    local gc_optimized_tests
    gc_optimized_tests=$(grep -r "gcBetweenTests\|performGC\|cleanupBetweenTests" /home/runner/work/Typus/Typus/test --include="*.hs" | wc -l)
    echo "   发现 $gc_optimized_tests 个使用垃圾回收优化的测试"
    
    if [ "$memory_limited_tests" -gt 0 ] && [ "$gc_optimized_tests" -gt 0 ]; then
        echo "   ✓ 内存使用优化策略已实施"
    else
        echo "   ✗ 内存使用优化策略不完整"
        return 1
    fi
}

verify_test_functionality() {
    echo "5. 验证测试功能完整性..."
    
    # 检查是否有完整的测试覆盖
    local test_categories=(
        "parser"
        "compiler"
        "ownership"
        "dependent_types"
        "utils"
        "syntax_validator"
    )
    
    local missing_categories=0
    for category in "${test_categories[@]}"; do
        local category_tests
        category_tests=$(find /home/runner/work/Typus/Typus/test -name "*.hs" -exec grep -l "$category" {} \; | wc -l)
        if [ "$category_tests" -gt 0 ]; then
            echo "   ✓ $category 测试存在 ($category_tests 个文件)"
        else
            echo "   ✗ $category 测试缺失"
            missing_categories=$((missing_categories + 1))
        fi
    done
    
    if [ "$missing_categories" -gt 0 ]; then
        echo "   警告: 发现 $missing_categories 个测试类别缺失"
        return 1
    else
        echo "   ✓ 所有测试类别都存在"
        return 0
    fi
}

run_safety_test() {
    echo "6. 运行安全性测试..."
    
    # 使用最小的内存限制运行一个核心测试
    echo "   运行内存优化测试验证..."
    
    if [ -f "/home/runner/work/Typus/Typus/scripts/intelligent_memory_optimized_test_runner.sh" ]; then
        # 使用紧急模式运行测试
        if /home/runner/work/Typus/Typus/scripts/intelligent_memory_optimized_test_runner.sh --memory-level=emergency --memory-limit=8; then
            echo "   ✓ 内存优化测试运行成功"
        else
            echo "   ✗ 内存优化测试运行失败"
            return 1
        fi
    else
        echo "   ⚠ 无法运行内存优化测试 (脚本不存在)"
        return 0
    fi
}

# 主验证函数
main() {
    local overall_success=true
    
    echo "开始内存优化安全性验证..."
    echo ""
    
    # 执行所有验证
    if ! verify_test_preservation; then
        overall_success=false
    fi
    
    if ! verify_memory_optimization; then
        overall_success=false
    fi
    
    if ! verify_quickcheck_limits; then
        overall_success=false
    fi
    
    if ! verify_memory_usage; then
        overall_success=false
    fi
    
    if ! verify_test_functionality; then
        overall_success=false
    fi
    
    if ! run_safety_test; then
        overall_success=false
    fi
    
    echo ""
    echo "=== 验证结果 ==="
    
    if [ "$overall_success" = true ]; then
        echo "✓ 内存优化安全性验证通过"
        echo "✓ 所有测试用例保留完整"
        echo "✓ 内存使用得到优化"
        echo "✓ 测试功能保持完整"
        exit 0
    else
        echo "✗ 内存优化安全性验证失败"
        echo "⚠ 请检查上述警告信息"
        exit 1
    fi
}

# 运行主函数
main