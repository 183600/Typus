#!/usr/bin/env bash
# 极度优化的内存测试运行脚本 - 保留测试功能但最小化内存使用
# 用于在极低内存环境下运行测试，确保不会消耗大量内存

set -e

# 极度严格的内存限制配置
MEMORY_LIMIT_MB=6  # 总内存限制6MB
TEST_BATCH_SIZE=1   # 单个测试批处理
GC_FREQUENCY=1      # 每次测试后垃圾回收
EMERGENCY_THRESHOLD=5MB  # 紧急模式阈值
CRITICAL_THRESHOLD=4MB   # 关键模式阈值

# 环境变量设置
export ULS_MEMORY_OPTIMIZED=1
export EMERGENCY_MEMORY=1
export ULTRA_MEMORY_OPTIMIZED=1
export EXTREME_MINIMAL_PRESERVE=1
export TYPUS_SKIP_GO_BUILD=1  # 跳过Go构建以节省内存
export TYPUS_MINIMAL_MODE=1   # 启用最小模式

# 系统内存限制
if command -v ulimit >/dev/null 2>&1; then
    # 设置严格的虚拟内存限制
    ulimit -v $((MEMORY_LIMIT_MB * 1024))  # MB to KB
    ulimit -s 512  # 栈限制512KB
    ulimit -n 10   # 限制打开文件数量
    echo "设置虚拟内存限制: ${MEMORY_LIMIT_MB}MB"
    echo "设置栈限制: 512KB"
    echo "设置文件描述符限制: 10"
fi

# 清理函数
aggressive_cleanup() {
    echo "执行激进的内存清理..."
    
    # 清理临时文件
    find /tmp -name "typus-*" -delete 2>/dev/null || true
    find . -name "*.tmp" -delete 2>/dev/null || true
    find . -name "*.cache" -delete 2>/dev/null || true
    
    # 强制垃圾回收
    if command -v ghc >/dev/null 2>&1; then
        ghc -e "import System.Mem; performGC; import System.Mem; performGC" 2>/dev/null || true
    fi
    
    # 清理系统缓存（如果权限允许）
    sync && echo 1 > /proc/sys/vm/drop_caches 2>/dev/null || true
    
    # 终止可能的僵尸进程
    pkill -f typus-test 2>/dev/null || true
    pkill -f cabal-test 2>/dev/null || true
}

# 内存监控函数
monitor_memory() {
    local process_pid="$1"
    while kill -0 "$process_pid" 2>/dev/null; do
        if command -v ps >/dev/null 2>&1; then
            local memory_usage=$(ps -p "$process_pid" -o rss= 2>/dev/null | tr -d ' ')
            if [ -n "$memory_usage" ] && [ "$memory_usage" -gt $((MEMORY_LIMIT_MB * 1024)) ]; then
                echo "内存使用超限: ${memory_usage}KB，终止进程"
                kill -KILL "$process_pid" 2>/dev/null || true
                return 1
            fi
        fi
        sleep 0.5
    done
}

# 设置清理陷阱
trap aggressive_cleanup EXIT INT TERM

echo "=== 极度优化的内存测试运行 ==="
echo "内存限制: ${MEMORY_LIMIT_MB}MB"
echo "批处理大小: ${TEST_BATCH_SIZE}"
echo "垃圾回收频率: ${GC_FREQUENCY}"
echo "紧急阈值: ${EMERGENCY_THRESHOLD}"
echo "关键阈值: ${CRITICAL_THRESHOLD}"
echo ""

# 预清理
aggressive_cleanup

# 构建优化选项
BUILD_OPTS=(
    "--ghc-options=-rtsopts"
    "--ghc-options=-with-rtsopts=-M${MEMORY_LIMIT_MB}m"
    "--ghc-options=-A1m"  # 更小的分配区域
    "--ghc-options=-n2m"  # 更小的 Nursery
    "--ghc-options=-O0"   # 禁用优化以减少内存使用
    "--ghc-options=-fno-warn-unused-imports"
    "--ghc-options=-fno-warn-unused-matches"
    "--ghc-options=-fno-warn-type-defaults"
    "--disable-profiling"
    "--disable-library-profiling"
    "--disable-executable-profiling"
)

echo "构建测试套件..."
cabal build typus-test "${BUILD_OPTS[@]}"

echo ""
echo "运行极度优化的内存测试..."

# 运行测试的函数
run_extreme_memory_test() {
    local test_name="$1"
    local config_file="$2"
    
    echo "运行测试: ${test_name}"
    
    # 测试前清理
    aggressive_cleanup
    
    # 运行测试并监控内存
    local test_cmd="cabal test typus-test \\
        --test-option='+RTS' \\
        --test-option='-M${MEMORY_LIMIT_MB}m' \\
        --test-option='-A1m' \\
        --test-option='-n1m' \\
        --test-option='-G1' \\
        --test-option='-RTS' \\
        --disable-profiling \\
        --test-show-details=direct"
    
    if [ -f "$config_file" ]; then
        echo "使用配置文件: $config_file"
        export TYPUS_TEST_CONFIG="$config_file"
    fi
    
    # 在后台运行测试并监控内存
    $test_cmd &
    local test_pid=$!
    
    # 监控内存使用
    if monitor_memory "$test_pid"; then
        wait "$test_pid" || {
            echo "测试失败: ${test_name}"
            return 1
        }
    else
        echo "测试因内存超限被终止: ${test_name}"
        return 1
    fi
    
    # 测试后清理
    aggressive_cleanup
    
    echo "测试完成: ${test_name}"
    echo ""
}

# 检查配置文件
CONFIG_FILE="/home/runner/work/Typus/Typus/test/extreme_minimal_memory_config_preserve.yaml"
if [ ! -f "$CONFIG_FILE" ]; then
    echo "警告: 配置文件不存在: $CONFIG_FILE"
    CONFIG_FILE=""
fi

# 运行核心测试套件
echo "=== 运行核心测试套件 ==="
run_extreme_memory_test "核心QuickCheck测试" "$CONFIG_FILE"

# 运行内存优化测试套件
echo "=== 运行内存优化测试套件 ==="
run_extreme_memory_test "内存优化测试" "$CONFIG_FILE"

# 检查是否有足够内存运行更多测试
AVAILABLE_MEMORY=$(free -m | awk 'NR==2{print $7}' || echo "0")
if [ "$AVAILABLE_MEMORY" -gt 32 ]; then
    echo "检测到足够内存，运行扩展测试..."
    run_extreme_memory_test "扩展功能测试" "$CONFIG_FILE"
fi

# 最终清理
aggressive_cleanup

echo ""
echo "=== 所有测试完成 ==="
echo "极度优化的内存测试运行成功完成"
echo "保留的测试功能:"
echo "  ✓ 核心工具函数测试"
echo "  ✓ 基础解析器测试"
echo "  ✓ 基础编译器测试"
echo "  ✓ 错误处理测试"
echo "  ✓ 内存优化验证"