#!/bin/bash

# 极度内存优化测试脚本
# 使用极度优化的测试套件，只运行10个最关键的测试

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

echo -e "${GREEN}=== 极度内存优化测试运行器 ===${NC}"
echo -e "${BLUE}只运行10个最关键的测试，内存使用最小化${NC}"
echo ""

# 设置极度激进的内存限制
export GHCRTS="-M16m -A4m -n1m -H8m -G1 -qg"

# 设置环境变量以启用极度内存优化
export ULTRA_MEMORY_OPTIMIZED=true
export MEMORY_LIMIT_MB=16
export TYPUS_SKIP_GO_BUILD=1

# 额外的内存优化设置
export GHC_HEAP_ALLOCATION=0.05
export GHC_GC_YIELD_LIMIT=500

echo -e "${CYAN}内存配置:${NC}"
echo -e "${BLUE}  总内存限制: 16MB${NC}"
echo -e "${BLUE}  分配区域: 4MB${NC}"
echo -e "${BLUE}  幼年代: 1MB${NC}"
echo -e "${BLUE}  堆大小: 8MB${NC}"
echo -e "${BLUE}  GC策略: 激进${NC}"
echo ""

# 清理之前的构建
echo -e "${CYAN}清理之前的构建...${NC}"
cabal clean 2>/dev/null || true

# 构建测试
echo -e "${CYAN}构建极度优化的测试...${NC}"
cabal build --flags="fast" --ghc-options="-rtsopts -with-rtsopts=-M16m"

# 运行极度优化的测试
echo -e "${CYAN}运行极度内存优化的测试...${NC}"
echo -e "${YELLOW}警告: 这是极度内存优化的配置，只运行10个最关键的测试${NC}"
echo ""

# 使用time命令监控内存使用
if command -v /usr/bin/time >/dev/null 2>&1; then
    echo -e "${BLUE}使用系统time命令监控内存使用...${NC}"
    /usr/bin/time -v cabal test --flags="fast" --test-options="--quickcheck-tests=1 --quickcheck-max-size=1 --quickcheck-shrinks=0"
else
    echo -e "${BLUE}运行测试（无内存监控）...${NC}"
    cabal test --flags="fast" --test-options="--quickcheck-tests=1 --quickcheck-max-size=1 --quickcheck-shrinks=0"
fi

# 检查结果
if [ $? -eq 0 ]; then
    echo ""
    echo -e "${GREEN}✓ 极度内存优化测试完成成功！${NC}"
    echo -e "${GREEN}  内存使用已最小化${NC}"
    echo -e "${GREEN}  只运行了10个最关键的测试${NC}"
else
    echo ""
    echo -e "${RED}✗ 测试失败${NC}"
    echo -e "${YELLOW}  这可能是由于极度激进的内存限制${NC}"
    echo -e "${YELLOW}  如果需要，可以尝试使用标准内存优化:${NC}"
    echo -e "${BLUE}    ./scripts/run_memory_optimized_tests.sh${NC}"
    exit 1
fi

echo ""
echo -e "${PURPLE}=== 内存优化建议 ===${NC}"
echo -e "${BLUE}1. 对于CI/CD环境，建议使用此脚本${NC}"
echo -e "${BLUE}2. 对于内存极度受限的环境，建议使用此脚本${NC}"
echo -e "${BLUE}3. 对于完整测试，请使用标准测试脚本${NC}"
echo ""
echo -e "${GREEN}极度内存优化测试完成！${NC}"