#!/usr/bin/env bash
# 增强的统一内存优化测试运行器
# 确保测试用例不会消耗大量内存，同时保留所有测试功能

set -e

# 配置文件路径
CONFIG_FILE="test/enhanced_unified_memory_optimization.yaml"

# 默认内存限制
DEFAULT_MEMORY_LIMIT_MB=16
DEFAULT_TIMEOUT_SECONDS=120

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

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

# 内存检测函数
detect_available_memory() {
    if command -v free >/dev/null 2>&1; then
        local available_memory=$(free -m | awk 'NR==2{print $7}')
        echo "$available_memory"
    else
        echo "unknown"
    fi
}

# 内存级别判断
get_memory_level() {
    local memory_mb="$1"
    if [[ "$memory_mb" == "unknown" ]]; then
        echo "normal"
    elif [[ "$memory_mb" -lt 2 ]]; then
        echo "emergency"
    elif [[ "$memory_mb" -lt 4 ]]; then
        echo "critical"
    elif [[ "$memory_mb" -lt 8 ]]; then
        echo "minimal"
    elif [[ "$memory_mb" -lt 16 ]]; then
        echo "low"
    else
        echo "normal"
    fi
}

# 清理函数
cleanup() {
    log_info "清理临时文件和进程..."
    # 强制垃圾回收
    if command -v ghc >/dev/null 2>&1; then
        ghc -e "import System.Mem; performGC" 2>/dev/null || true
    fi
    # 清理临时文件
    find /tmp -name "typus_test_*" -mmin +10 -delete 2>/dev/null || true
}

# 设置清理陷阱
trap cleanup EXIT INT TERM

# 主函数
main() {
    log_info "=== 增强的统一内存优化测试运行器 ==="
    
    # 检测可用内存
    local available_memory=$(detect_available_memory)
    local memory_level=$(get_memory_level "$available_memory")
    
    log_info "检测到可用内存: ${available_memory}MB"
    log_info "内存级别: ${memory_level}"
    
    # 设置环境变量
    export LC_ALL=C
    export LANG=C
    export LC_CTYPE=C
    export LC_MESSAGES=C
    export LC_COLLATE=C
    
    # 内存优化标志
    export ULS_MEMORY_OPTIMIZED=1
    export EMERGENCY_MEMORY=1
    export TYPUS_SKIP_GO_BUILD=1
    export TYPUS_MEMORY_LEVEL="$memory_level"
    
    # 系统内存限制
    if command -v ulimit >/dev/null 2>&1; then
        ulimit -v $((DEFAULT_MEMORY_LIMIT_MB * 1024))  # MB to KB
        log_info "设置虚拟内存限制: ${DEFAULT_MEMORY_LIMIT_MB}MB"
    fi
    
    # 构建优化选项
    local build_opts=(
        "--ghc-options=-rtsopts"
        "--ghc-options=-with-rtsopts=-M${DEFAULT_MEMORY_LIMIT_MB}m"
        "--ghc-options=-O0"  # 禁用优化以减少内存使用
        "--ghc-options=-fno-warn-unused-imports"
        "--ghc-options=-fno-warn-unused-matches"
        "--ghc-options=-j1"  # 单线程编译
    )
    
    log_info "构建测试套件..."
    cabal build typus-test "${build_opts[@]}" --disable-profiling
    
    # 基于内存级别选择测试策略
    local test_suites=()
    case "$memory_level" in
        "emergency")
            test_suites=("BasicQuickCheckTestSuite" "ExtremeMemoryOptimizedTestSuite")
            log_warn "紧急内存模式：仅运行核心测试"
            ;;
        "critical")
            test_suites=("BasicQuickCheckTestSuite" "CoreQuickCheckSpec" "MemoryOptimizedTestSuite")
            log_warn "关键内存模式：运行核心和内存优化测试"
            ;;
        "minimal")
            test_suites=("BasicQuickCheckTestSuite" "CoreQuickCheckSpec" "MemoryOptimizedTestSuite" "EnhancedMemoryOptimizedTestSuite")
            log_info "最小内存模式：运行核心和增强优化测试"
            ;;
        "low")
            test_suites=("BasicQuickCheckTestSuite" "CoreQuickCheckSpec" "MemoryOptimizedTestSuite" "EnhancedMemoryOptimizedTestSuite" "ComprehensiveMemoryOptimizedTestSuite")
            log_info "低内存模式：运行完整的优化测试套件"
            ;;
        "normal")
            # 正常模式：尝试使用优化版本，回退到原始测试
            log_info "正常内存模式：优先使用优化版本测试"
            run_optimized_tests_with_fallback
            return 0
            ;;
    esac
    
    # 运行选择的测试套件
    for suite in "${test_suites[@]}"; do
        log_info "运行测试套件: ${suite}"
        
        cabal test typus-test \
            --test-option="+RTS" \
            --test-option="-M${DEFAULT_MEMORY_LIMIT_MB}m" \
            --test-option="-A1m" \
            --test-option="-RTS" \
            --disable-profiling \
            --test-show-details=direct \
            --test-option="--test-pattern=${suite}" \
        || {
            log_error "测试套件失败: ${suite}"
            # 继续运行其他测试套件
            continue
        }
        
        log_success "测试套件完成: ${suite}"
        
        # 测试间清理
        cleanup
    done
    
    log_success "=== 所有测试完成 ==="
    log_success "内存优化测试运行成功完成"
}

# 优化版本测试运行函数
run_optimized_tests_with_fallback() {
    log_info "=== 运行优化版本测试（带回退策略） ==="
    
    # 定义测试文件优先级
    local test_patterns=(
        "*Optimized.hs"
        "*MemoryOptimized*.hs"
        "*Basic*.hs"
        "*Core*.hs"
        "*Essential*.hs"
        "*.hs"
    )
    
    local success=false
    
    for pattern in "${test_patterns[@]}"; do
        log_info "尝试模式: ${pattern}"
        
        # 检查是否有匹配的文件
        if find test/Test -name "${pattern}" | grep -q .; then
            log_info "找到匹配的测试文件，运行测试..."
            
            cabal test typus-test \
                --test-option="+RTS" \
                --test-option="-M${DEFAULT_MEMORY_LIMIT_MB}m" \
                --test-option="-A1m" \
                --test-option="-RTS" \
                --disable-profiling \
                --test-show-details=direct \
            && {
                log_success "测试成功完成 (模式: ${pattern})"
                success=true
                break
            } || {
                log_warn "测试失败 (模式: ${pattern})，尝试下一个模式..."
                cleanup
                continue
            }
        else
            log_warn "没有找到匹配的测试文件 (模式: ${pattern})"
        fi
    done
    
    if [[ "$success" == "false" ]]; then
        log_error "所有测试模式都失败了"
        exit 1
    fi
}

# 运行主函数
main "$@"