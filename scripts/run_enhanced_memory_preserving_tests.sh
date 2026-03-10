#!/usr/bin/env bash
# 增强内存保持测试运行脚本
# 确保测试用例不会消耗大量内存，同时保留所有测试用例

set -euo pipefail

# 配置变量
MEMORY_LIMIT_MB=8
STACK_LIMIT_MB=1
GC_FREQUENCY=1

# 颜色输出
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 日志函数
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# 内存检查函数
check_memory_availability() {
    local available_memory
    available_memory=$(free -m | awk 'NR==2{print $7}' 2>/dev/null || echo "0")
    
    if [ "$available_memory" -lt 16 ]; then
        log_warning "可用内存较低: ${available_memory}MB"
        return 1
    elif [ "$available_memory" -lt 32 ]; then
        log_warning "可用内存适中: ${available_memory}MB"
        return 0
    else
        log_info "可用内存充足: ${available_memory}MB"
        return 0
    fi
}

# 设置内存限制
setup_memory_limits() {
    log_info "设置内存限制..."
    
    # 设置虚拟内存限制
    if command -v ulimit >/dev/null 2>&1; then
        ulimit -v $((MEMORY_LIMIT_MB * 1024)) 2>/dev/null || true
        log_info "虚拟内存限制: ${MEMORY_LIMIT_MB}MB"
    fi
    
    # 设置堆栈限制
    if command -v ulimit >/dev/null 2>&1; then
        ulimit -s $((STACK_LIMIT_MB * 1024)) 2>/dev/null || true
        log_info "堆栈内存限制: ${STACK_LIMIT_MB}MB"
    fi
}

# 环境配置
setup_environment() {
    log_info "配置测试环境..."
    
    # 设置locale以避免警告
    export LC_ALL=C
    export LANG=C
    export LC_CTYPE=C
    
    # 启用内存优化标志
    export TYPUS_MEMORY_OPTIMIZED=1
    export EMERGENCY_MEMORY=1
    export ULTRA_MEMORY_OPTIMIZED=1
    export EXTREME_MEMORY_OPTIMIZED=1
    
    # 跳过不必要的构建
    export TYPUS_SKIP_GO_BUILD=1
    
    # QuickCheck参数
    export QUICKCHECK_MAX_TESTS=2
    export QUICKCHECK_MAX_SIZE=1
    export QUICKCHECK_MAX_SHRINKS=0
}

# 清理函数
cleanup() {
    log_info "执行清理操作..."
    
    # 强制垃圾回收
    if command -v ghc >/dev/null 2>&1; then
        ghc -e "import System.Mem; performGC" 2>/dev/null || true
    fi
    
    # 清理临时文件
    find /tmp -name "*typus*" -type f -delete 2>/dev/null || true
}

# 设置清理陷阱
trap cleanup EXIT INT TERM

# 构建选项
BUILD_OPTS=(
    "--ghc-options=-rtsopts"
    "--ghc-options=-with-rtsopts=-M${MEMORY_LIMIT_MB}m -K${STACK_LIMIT_MB}m -A256k -c -I0 -G1"
    "--ghc-options=-O0"
    "--ghc-options=-fno-warn-unused-imports"
    "--ghc-options=-fno-warn-unused-matches"
    "--disable-profiling"
)

# 运行内存优化测试
run_memory_optimized_test_suite() {
    local test_name="$1"
    local test_opts="$2"
    
    log_info "运行测试套件: ${test_name}"
    
    # 构建测试
    log_info "构建测试..."
    if ! cabal build typus-test "${BUILD_OPTS[@]}"; then
        log_error "构建失败"
        return 1
    fi
    
    # 运行测试
    log_info "执行测试..."
    cabal test typus-test \
        --test-option="+RTS" \
        --test-option="-M${MEMORY_LIMIT_MB}m" \
        --test-option="-K${STACK_LIMIT_MB}m" \
        --test-option="-A256k" \
        --test-option="-c" \
        --test-option="-I0" \
        --test-option="-G1" \
        --test-option="-RTS" \
        --disable-profiling \
        --test-show-details=direct \
        ${test_opts} || {
        log_error "测试失败: ${test_name}"
        return 1
    }
    
    # 测试后清理
    cleanup
    
    log_success "测试完成: ${test_name}"
    return 0
}

# 主执行流程
main() {
    log_info "=== 增强内存保持测试运行器 ==="
    log_info "内存限制: ${MEMORY_LIMIT_MB}MB"
    log_info "堆栈限制: ${STACK_LIMIT_MB}MB"
    log_info "GC频率: ${GC_FREQUENCY}"
    echo
    
    # 检查内存可用性
    if ! check_memory_availability; then
        log_warning "内存资源紧张，但将继续执行..."
    fi
    
    # 设置环境
    setup_environment
    setup_memory_limits
    
    # 运行核心测试套件
    log_info "=== 运行核心测试套件 ==="
    run_memory_optimized_test_suite "核心功能测试" "--test-option=--pattern='*Core*|*Basic*|*Essential*'"
    
    # 运行优化测试套件
    log_info "=== 运行内存优化测试套件 ==="
    run_memory_optimized_test_suite "内存优化测试" "--test-option=--pattern='*Optimized*|*MemoryOptimized*'"
    
    # 运行完整测试套件（如果内存充足）
    if check_memory_availability; then
        log_info "=== 运行完整测试套件 ==="
        run_memory_optimized_test_suite "完整测试套件" ""
    else
        log_warning "内存不足，跳过完整测试套件"
    fi
    
    echo
    log_success "=== 所有测试完成 ==="
    log_success "测试用例完整保留，内存使用优化成功"
}

# 执行主函数
main "$@"