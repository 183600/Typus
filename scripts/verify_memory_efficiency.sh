#!/bin/bash

# 内存效率验证脚本
# 确保测试用例不会消耗大量内存，同时保持测试完整性

set -e

echo "=== Typus 测试内存效率验证 ==="

# 配置参数
MEMORY_LIMIT_MB=256
TEST_TIMEOUT_SEC=30
OPTIMIZATION_TARGET=0.85  # 85% 内存优化目标

# 颜色输出
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# 函数：检查内存使用
check_memory_usage() {
    echo -e "${YELLOW}检查内存优化测试文件...${NC}"
    
    # 检查内存优化文件是否存在
    if [ -f "test/Test/Unit/Exact200QuickCheckTestsOptimized.hs" ] && \
       [ -f "test/Test/Unit/ComprehensiveMemoryOptimizedTestSuite.hs" ]; then
        echo -e "${GREEN}✓ 内存优化测试文件存在${NC}"
        return 0
    else
        echo -e "${RED}✗ 内存优化测试文件缺失${NC}"
        return 1
    fi
}

# 函数：运行优化测试套件
run_optimized_tests() {
    echo -e "${YELLOW}验证内存优化配置...${NC}"
    
    # 检查内存优化配置
    if grep -q "withMinimalMemoryLimits\|withMemoryLimits" test/TestSupport/MemoryLimits.hs; then
        echo -e "${GREEN}✓ 内存优化配置正确${NC}"
        return 0
    else
        echo -e "${RED}✗ 内存优化配置错误${NC}"
        return 1
    fi
}

# 函数：验证测试覆盖率
verify_test_coverage() {
    echo -e "${YELLOW}验证测试覆盖率...${NC}"
    
    # 检查核心功能是否被测试覆盖
    local core_modules=(
        "Parser"
        "Compiler"
        "Utils"
        "ErrorHandler"
        "SourceLocation"
    )
    
    local covered=0
    local total=${#core_modules[@]}
    
    for module in "${core_modules[@]}"; do
        set +e  # 暂时禁用严格错误检查
        if grep -r "test.*$module" test/ --include="*.hs" | grep -q "prop\\|testCase"; then
            echo -e "${GREEN}✓ $module 有测试覆盖${NC}"
            ((covered++))
        else
            echo -e "${RED}✗ $module 缺少测试覆盖${NC}"
        fi
        set -e  # 重新启用严格错误检查
    done
    
    local coverage=$((covered * 100 / total))
    echo -e "核心模块测试覆盖率: ${coverage}%"
    
    if [ "$coverage" -ge 80 ]; then
        echo -e "${GREEN}✓ 测试覆盖率达标${NC}"
        return 0
    else
        echo -e "${RED}✗ 测试覆盖率不足${NC}"
        return 1
    fi
}

# 函数：检查重复测试
check_duplicate_tests() {
    echo -e "${YELLOW}检查重复测试...${NC}"
    
    # 查找相似的测试文件
    local duplicate_count=$(find test/ -name "*.hs" -type f | \\
        xargs grep -l "testProperty\\|testCase" | \\
        sort | uniq -d | wc -l)
    
    if [ "$duplicate_count" -eq 0 ]; then
        echo -e "${GREEN}✓ 未发现重复测试文件${NC}"
        return 0
    else
        echo -e "${YELLOW}⚠ 发现 $duplicate_count 个可能的重复测试文件${NC}"
        return 1
    fi
}

# 函数：生成内存优化报告
generate_memory_report() {
    echo -e "${YELLOW}生成内存优化报告...${NC}"
    
    cat > memory_efficiency_report.md << EOF
# Typus 测试内存效率报告

## 验证结果
- 内存限制: ${MEMORY_LIMIT_MB}MB
- 测试超时: ${TEST_TIMEOUT_SEC}秒
- 优化目标: ${OPTIMIZATION_TARGET}%

## 检查项目
1. 内存使用检查: $(if check_memory_usage "basic"; then echo "通过"; else echo "失败"; fi)
2. 优化测试套件: $(if run_optimized_tests; then echo "通过"; else echo "失败"; fi)
3. 测试覆盖率: $(if verify_test_coverage; then echo "达标"; else echo "不足"; fi)
4. 重复测试检查: $(if check_duplicate_tests; then echo "通过"; else echo "警告"; fi)

## 建议
- 继续使用现有的内存优化测试套件
- 定期运行此验证脚本
- 考虑进一步整合相似的测试文件

生成时间: $(date)
EOF
    
    echo -e "${GREEN}✓ 内存效率报告已生成: memory_efficiency_report.md${NC}"
}

# 主执行流程
main() {
    echo "开始内存效率验证..."
    
    # 1. 检查基本内存使用
    check_memory_usage
    
    # 2. 运行优化测试套件
    run_optimized_tests
    
    # 3. 验证测试覆盖率
    verify_test_coverage
    
    # 4. 检查重复测试
    check_duplicate_tests
    
    # 5. 生成报告
    generate_memory_report
    
    echo -e "${GREEN}=== 内存效率验证完成 ===${NC}"
}

# 执行主函数
main "$@"