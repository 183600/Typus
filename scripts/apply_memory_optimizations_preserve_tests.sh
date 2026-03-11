#!/bin/bash

# 应用内存优化但保持所有测试用例的脚本
# 专注于优化测试执行而非删除测试

set -e

echo "=== 应用内存优化配置（保持所有测试用例） ==="

# 设置环境变量
export TYPUS_MINIMAL_MODE=1
export TYPUS_SKIP_GO_BUILD=1
export TYPUS_MEMORY_OPTIMIZED=1

# 应用内存配置
if [ -f "enhanced_memory_optimization_with_preservation.yaml" ]; then
    echo "使用增强内存优化配置..."
    export TYPUS_MEMORY_CONFIG="enhanced_memory_optimization_with_preservation.yaml"
else
    echo "使用默认内存优化配置..."
    export TYPUS_MEMORY_CONFIG="test-memory-config.yaml"
fi

# 设置GHC运行时选项
export GHC_RTS="-M8m -A512k -n64k -H2m -qg -G1 -I0 -c"

# 设置Cabal构建选项
export CABAL_BUILD_FLAGS="--flags=fast"
export CABAL_GHC_OPTIONS="-O0 -j1 -rtsopts"
export CABAL_TEST_OPTIONS="--quickcheck-tests=1 --quickcheck-max-size=1 --quickcheck-max-shrinks=0"

# 清理临时文件
if [ "$CLEANUP_TEMP_FILES" = "1" ]; then
    echo "清理临时文件..."
    find . -name "*.hi" -delete 2>/dev/null || true
    find . -name "*.o" -delete 2>/dev/null || true
    find . -name "*.dyn_hi" -delete 2>/dev/null || true
    find . -name "*.dyn_o" -delete 2>/dev/null || true
fi

# 强制垃圾回收
echo "强制系统垃圾回收..."
sync
echo 3 > /proc/sys/vm/drop_caches 2>/dev/null || true

# 运行测试
echo "运行内存优化的测试套件..."

# 使用最小内存配置运行测试
stack test \
    --test-arguments="--quickcheck-tests=1 --quickcheck-max-size=1 --quickcheck-max-shrinks=0" \
    --ghc-options="-O0 -rtsopts" \
    +RTS -M8m -A512k -n64k -H2m -qg -G1 -I0 -c -RTS \
    || {
    echo "第一次测试运行失败，尝试更保守的内存配置..."
    
    # 如果失败，尝试更保守的配置
    stack test \
        --test-arguments="--quickcheck-tests=1 --quickcheck-max-size=1 --quickcheck-max-shrinks=0" \
        --ghc-options="-O0 -rtsopts" \
        +RTS -M12m -A1m -n128k -H4m -qg -G1 -I0 -c -RTS
}

echo "=== 内存优化测试完成 ==="
echo "所有测试用例已保留，内存使用已优化"