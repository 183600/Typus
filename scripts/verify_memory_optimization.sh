#!/bin/bash
# 验证内存优化效果的脚本

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

echo -e "${PURPLE}=== Typus 内存优化验证脚本 ===${NC}"
echo ""

# 检查优化的测试文件是否存在
echo -e "${CYAN}检查优化的测试文件...${NC}"

optimized_files=(
    "test/Test/Unit/CoreUtilsQuickCheckTests.hs"
    "test/Test/Unit/ParserQuickCheckTests.hs"
)

for file in "${optimized_files[@]}"; do
    if [ -f "$file" ]; then
        echo -e "${GREEN}✓ $file 存在${NC}"
        
        # 检查是否包含内存优化导入
        if grep -q "import.*TestSupport.*Memory" "$file"; then
            echo -e "${GREEN}  ✓ 包含内存优化导入${NC}"
        else
            echo -e "${RED}  ✗ 缺少内存优化导入${NC}"
        fi
        
        # 检查是否包含内存限制
        if grep -q "take.*[0-9]" "$file"; then
            echo -e "${GREEN}  ✓ 包含输入大小限制${NC}"
        else
            echo -e "${YELLOW}  ⚠ 可能缺少输入大小限制${NC}"
        fi
        
        # 检查是否使用内存优化的测试组
        if grep -q "minimalMemoryLimitedTestGroup\|withMinimalMemoryLimits" "$file"; then
            echo -e "${GREEN}  ✓ 使用内存优化的测试组${NC}"
        else
            echo -e "${YELLOW}  ⚠ 可能没有使用内存优化的测试组${NC}"
        fi
    else
        echo -e "${RED}✗ $file 不存在${NC}"
    fi
    echo ""
done

# 检查内存优化脚本
echo -e "${CYAN}检查内存优化脚本...${NC}"

scripts=(
    "scripts/run_extreme_enhanced_memory_tests.sh"
    "scripts/run_optimized_memory_preserving_tests.sh"
)

for script in "${scripts[@]}"; do
    if [ -f "$script" ]; then
        echo -e "${GREEN}✓ $script 存在${NC}"
        
        # 检查是否可执行
        if [ -x "$script" ]; then
            echo -e "${GREEN}  ✓ 可执行${NC}"
        else
            echo -e "${YELLOW}  ⚠ 不可执行，运行: chmod +x $script${NC}"
        fi
    else
        echo -e "${RED}✗ $script 不存在${NC}"
    fi
    echo ""
done

# 检查内存优化支持模块
echo -e "${CYAN}检查内存优化支持模块...${NC}"

support_modules=(
    "test/TestSupport/ExtremeQuickCheckMemoryOptimization.hs"
    "test/TestSupport/UnifiedAdaptiveMemoryOptimization.hs"
    "test/TestSupport/ComprehensiveMemoryCleanup.hs"
)

for module in "${support_modules[@]}"; do
    if [ -f "$module" ]; then
        echo -e "${GREEN}✓ $module 存在${NC}"
    else
        echo -e "${RED}✗ $module 不存在${NC}"
    fi
done

echo ""
echo -e "${PURPLE}=== 内存优化建议 ===${NC}"
echo -e "${BLUE}1. 使用以下脚本运行内存优化测试：${NC}"
echo -e "   ${CYAN}./scripts/run_optimized_memory_preserving_tests.sh --auto${NC}"
echo -e "${BLUE}2. 或使用极度内存优化：${NC}"
echo -e "   ${CYAN}./scripts/run_extreme_enhanced_memory_tests.sh critical${NC}"
echo -e "${BLUE}3. 监控内存使用：${NC}"
echo -e "   ${CYAN}./scripts/run_optimized_memory_preserving_tests.sh --monitor${NC}"
echo -e "${BLUE}4. 自适应内存管理：${NC}"
echo -e "   ${CYAN}./scripts/run_optimized_memory_preserving_tests.sh --adaptive${NC}"
echo ""
echo -e "${GREEN}=== 优化完成 ===${NC}"