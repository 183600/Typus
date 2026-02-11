#!/bin/bash

# 极限内存优化测试脚本
# 用于在内存极度受限的环境中运行测试

set -e

# 内存限制设置（以MB为单位）
MEMORY_LIMITS=(32 48 64 96 128)

# 颜色输出
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Starting extreme memory optimized test run...${NC}"

# 检查是否在CI/CD环境中
if [ "$CI" = "true" ] || [ "$CONTINUOUS_INTEGRATION" = "true" ]; then
    echo -e "${YELLOW}Detected CI/CD environment, using minimal memory configuration${NC}"
    MEMORY_LIMITS=(32 48)
fi

# 设置环境变量以减少内存使用
export LC_ALL=C
export LANG=C
export LC_CTYPE=C
export LC_MESSAGES=C
export LC_COLLATE=C

# GHC运行时选项以减少内存使用
export GHCRTS="-M${MEMORY_LIMITS[0]}m -A8m -n8m -qg"

for limit in "${MEMORY_LIMITS[@]}"; do
    echo -e "${YELLOW}Running tests with ${limit}MB memory limit...${NC}"
    
    # 更新GHC运行时选项
    export GHCRTS="-M${limit}m -A8m -n8m -qg"
    
    # 运行内存优化的测试套件
    if cabal test typus-test-optimized --test-options="--timeout=30" \
        --ghc-options="-rtsopts -with-rtsopts=-M${limit}m"; then
        echo -e "${GREEN}✓ Tests passed with ${limit}MB limit${NC}"
    else
        echo -e "${RED}✗ Tests failed with ${limit}MB limit${NC}"
        exit 1
    fi
    
    # 强制垃圾回收
    echo -e "${YELLOW}Forcing garbage collection...${NC}"
    sync
    echo 3 > /proc/sys/vm/drop_caches 2>/dev/null || true
done

echo -e "${GREEN}All memory-optimized tests passed successfully!${NC}"
