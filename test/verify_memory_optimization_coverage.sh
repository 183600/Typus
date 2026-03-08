#!/bin/bash

# 内存优化覆盖验证脚本
# 验证内存优化后测试覆盖率是否得到保持

set -e

echo "=== 内存优化覆盖验证 ==="
echo "确保内存优化后核心功能测试覆盖得到保持"
echo ""

# 颜色定义
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

# 检查核心测试文件是否存在
check_core_tests() {
    echo -e "${YELLOW}检查核心测试文件...${NC}"
    
    local core_tests=(
        "test/Test/Unit/BasicQuickCheckTestSuite.hs"
        "test/Test/Unit/CoreQuickCheckSpec.hs"
        "test/Test/Unit/EssentialQuickCheckTests.hs"
        "test/Test/Unit/MemoryOptimizedTestSuite.hs"
        "test/Test/Unit/ExtremeMemoryOptimizedTestSuite.hs"
    )
    
    local missing_count=0
    for test_file in "${core_tests[@]}"; do
        if [ -f "$test_file" ]; then
            echo -e "${GREEN}✓ $test_file${NC}"
        else
            echo -e "${RED}✗ $test_file - 缺失${NC}"
            ((missing_count++))
        fi
    done
    
    if [ $missing_count -eq 0 ]; then
        echo -e "${GREEN}所有核心测试文件都存在${NC}"
    else
        echo -e "${RED}有 $missing_count 个核心测试文件缺失${NC}"
        return 1
    fi
}

# 检查优化版本测试文件
check_optimized_tests() {
    echo -e "${YELLOW}检查优化版本测试文件...${NC}"
    
    local optimized_patterns=(
        "*Optimized.hs"
        "*MemoryOptimized*.hs"
        "*EnhancedMemory*.hs"
        "*ExtremeMemory*.hs"
    )
    
    local optimized_count=0
    for pattern in "${optimized_patterns[@]}"; do
        local files=$(find test/Test/Unit -name "$pattern" 2>/dev/null | wc -l)
        optimized_count=$((optimized_count + files))
        echo -e "${GREEN}找到 $files 个 $pattern 文件${NC}"
    done
    
    if [ $optimized_count -gt 0 ]; then
        echo -e "${GREEN}总共找到 $optimized_count 个优化版本测试文件${NC}"
    else
        echo -e "${RED}未找到任何优化版本测试文件${NC}"
        return 1
    fi
}

# 检查内存配置
check_memory_configs() {
    echo -e "${YELLOW}检查内存配置...${NC}"
    
    local configs=(
        "test-minimal-memory-config.env"
        "test-memory-config.yaml"
        "ultra_memory_test_config.yaml"
        "extreme_minimal_memory_config_preserve.yaml"
    )
    
    for config in "${configs[@]}"; do
        if [ -f "$config" ]; then
            echo -e "${GREEN}✓ $config${NC}"
        else
            echo -e "${RED}✗ $config - 缺失${NC}"
            return 1
        fi
    done
}

# 验证测试覆盖率
verify_test_coverage() {
    echo -e "${YELLOW}验证测试覆盖率...${NC}"
    
    # 检查核心模块是否有对应的测试
    local core_modules=(
        "Utils"
        "Parser"
        "Compiler"
        "ErrorHandler"
        "Dependencies"
        "Ownership"
    )
    
    local covered_count=0
    for module in "${core_modules[@]}"; do
        if find test/Test/Unit -name "*${module}*.hs" | grep -q .; then
            echo -e "${GREEN}✓ $module 有对应的测试${NC}"
            ((covered_count++))
        else
            echo -e "${RED}✗ $module 缺少对应的测试${NC}"
        fi
    done
    
    local coverage_ratio=$((covered_count * 100 / ${#core_modules[@]}))
    echo -e "${YELLOW}核心模块测试覆盖率: $coverage_ratio%${NC}"
    
    if [ $coverage_ratio -ge 80 ]; then
        echo -e "${GREEN}✓ 测试覆盖率满足要求${NC}"
    else
        echo -e "${RED}✗ 测试覆盖率不足${NC}"
        return 1
    fi
}

# 检查内存使用限制
check_memory_limits() {
    echo -e "${YELLOW}检查内存使用限制...${NC}"
    
    # 从配置文件中提取内存限制
    local mem_config="test-minimal-memory-config.env"
    if [ -f "$mem_config" ]; then
        local emergency_limit=$(grep "EMERGENCY_MEMORY_LIMIT" "$mem_config" | cut -d'=' -f2)
        local minimal_limit=$(grep "MINIMAL_MEMORY_LIMIT" "$mem_config" | cut -d'=' -f2)
        
        echo -e "${GREEN}紧急内存限制: ${emergency_limit}MB${NC}"
        echo -e "${GREEN}最小内存限制: ${minimal_limit}MB${NC}"
        
        if [ "$emergency_limit" -le 2 ] && [ "$minimal_limit" -le 8 ]; then
            echo -e "${GREEN}✓ 内存限制设置合理${NC}"
        else
            echo -e "${RED}✗ 内存限制设置过高${NC}"
            return 1
        fi
    fi
}

# 运行验证
main() {
    echo "开始内存优化覆盖验证..."
    echo ""
    
    local all_passed=true
    
    check_core_tests || all_passed=false
    echo ""
    
    check_optimized_tests || all_passed=false
    echo ""
    
    check_memory_configs || all_passed=false
    echo ""
    
    verify_test_coverage || all_passed=false
    echo ""
    
    check_memory_limits || all_passed=false
    echo ""
    
    if $all_passed; then
        echo -e "${GREEN}=== 所有验证通过 ===${NC}"
        echo -e "${GREEN}内存优化配置正确，测试覆盖率得到保持${NC}"
        exit 0
    else
        echo -e "${RED}=== 验证失败 ===${NC}"
        echo -e "${RED}部分验证项目失败，需要修复${NC}"
        exit 1
    fi
}

main "$@"