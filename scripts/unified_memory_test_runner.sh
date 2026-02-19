#!/bin/bash
# 统一内存优化测试运行脚本
# 确保所有测试用例都在内存限制下运行

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

# 内存配置
EMERGENCY_MEMORY_MB=2
CRITICAL_MEMORY_MB=4
MINIMAL_MEMORY_MB=8
LOW_MEMORY_MB=16
MODERATE_MEMORY_MB=32

# 默认内存级别
DEFAULT_MEMORY_LEVEL="minimal"

# 打印函数
print_header() {
    echo -e "${PURPLE}===================================${NC}"
    echo -e "${PURPLE}统一内存优化测试运行器${NC}"
    echo -e "${PURPLE}===================================${NC}"
    echo ""
}

print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# 设置内存环境
setup_memory_environment() {
    local level=$1
    local memory_mb=$2
    
    print_status "设置内存环境: $level (${memory_mb}MB)"
    
    # 设置GHC运行时选项
    export GHCRTS="-M${memory_mb}m -A256k -n32k -H1m -qg -G1"
    
    # 设置QuickCheck参数
    export QUICKCHECK_MAX_TESTS=1
    export QUICKCHECK_MAX_SIZE=1
    export QUICKCHECK_MAX_SHRINKS=0
    
    # 设置Typus内存限制
    export TYPUS_MAX_STRING_LENGTH=50
    export TYPUS_MAX_LIST_LENGTH=10
    export TYPUS_MAX_INT_RANGE=100
    export TYPUS_MAX_NESTING_DEPTH=3
    
    # 设置其他优化选项
    export TYPUS_SKIP_GO_BUILD=1
    export TYPUS_MINIMAL_MODE=1
    
    print_success "内存环境设置完成"
}

# 内存清理
cleanup_memory() {
    print_status "执行内存清理..."
    
    # 强制垃圾回收
    if command -v ghc >/dev/null 2>&1; then
        ghc -e "+RTS -S -RTS" >/dev/null 2>&1 || true
    fi
    
    # 清理临时文件
    find /tmp -name "typus-*" -type f -delete 2>/dev/null || true
    find /tmp -name "cabal-*" -type f -delete 2>/dev/null || true
    find /tmp -name "ghc-*" -type f -delete 2>/dev/null || true
    
    # 清理cabal临时文件
    rm -rf dist-newstyle/tmp 2>/dev/null || true
    rm -rf .stack-work/tmp 2>/dev/null || true
    
    print_success "内存清理完成"
}

# 运行内存优化测试
run_memory_optimized_tests() {
    local level=$1
    local memory_mb=$2
    local extra_args=$3
    
    print_status "运行内存优化测试 ($level 模式)..."
    
    # 设置内存环境
    setup_memory_environment "$level" "$memory_mb"
    
    # 清理内存
    cleanup_memory
    
    # 构建测试
    print_status "构建测试..."
    cabal clean >/dev/null 2>&1 || true
    cabal configure --enable-tests --enable-benchmarks --flags=fast >/dev/null 2>&1 || true
    
    # 使用内存优化构建
    cabal build --ghc-options="-O0 -j1 -rtsopts" test:Typus >/dev/null 2>&1 || {
        print_error "构建失败"
        return 1
    }
    
    # 运行测试
    print_status "执行测试..."
    local test_cmd="cabal test --test-show-details=direct Typus"
    
    if [ -n "$extra_args" ]; then
        test_cmd="$test_cmd $extra_args"
    fi
    
    # 添加内存限制
    test_cmd="$test_cmd --test-option=+RTS --test-option=-M${memory_mb}m"
    
    if eval "$test_cmd"; then
        print_success "所有测试通过 ($level 模式)"
        return 0
    else
        print_error "测试失败 ($level 模式)"
        return 1
    fi
}

# 显示帮助信息
show_help() {
    echo "用法: $0 [选项] [内存级别]"
    echo ""
    echo "内存级别:"
    echo "  emergency  (2MB)  - 极度激进的内存优化"
    echo "  critical   (4MB)  - 非常激进的内存优化"
    echo "  minimal    (8MB)  - 激进的内存优化 (默认)"
    echo "  low        (16MB) - 适度的内存优化"
    echo "  moderate   (32MB) - 平衡的内存优化"
    echo ""
    echo "选项:"
    echo "  -h, --help     显示此帮助信息"
    echo "  -v, --verbose  详细输出"
    echo "  -c, --cleanup  仅执行内存清理"
    echo "  --dry-run      仅显示将要执行的命令"
    echo ""
    echo "示例:"
    echo "  $0                    # 使用默认的minimal级别"
    echo "  $0 emergency          # 使用emergency级别"
    echo "  $0 --verbose moderate # 使用moderate级别并显示详细输出"
}

# 主函数
main() {
    local memory_level="$DEFAULT_MEMORY_LEVEL"
    local verbose=false
    local cleanup_only=false
    local dry_run=false
    
    # 解析命令行参数
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_help
                exit 0
                ;;
            -v|--verbose)
                verbose=true
                shift
                ;;
            -c|--cleanup)
                cleanup_only=true
                shift
                ;;
            --dry-run)
                dry_run=true
                shift
                ;;
            emergency|critical|minimal|low|moderate)
                memory_level="$1"
                shift
                ;;
            *)
                print_error "未知参数: $1"
                show_help
                exit 1
                ;;
        esac
    done
    
    # 显示标题
    print_header
    
    # 仅执行清理
    if [ "$cleanup_only" = true ]; then
        cleanup_memory
        exit 0
    fi
    
    # 确定内存限制
    local memory_mb
    case "$memory_level" in
        emergency)
            memory_mb=$EMERGENCY_MEMORY_MB
            ;;
        critical)
            memory_mb=$CRITICAL_MEMORY_MB
            ;;
        minimal)
            memory_mb=$MINIMAL_MEMORY_MB
            ;;
        low)
            memory_mb=$LOW_MEMORY_MB
            ;;
        moderate)
            memory_mb=$MODERATE_MEMORY_MB
            ;;
        *)
            print_error "未知内存级别: $memory_level"
            exit 1
            ;;
    esac
    
    # 显示配置信息
    print_status "内存级别: $memory_level"
    print_status "内存限制: ${memory_mb}MB"
    print_status "详细输出: $verbose"
    echo ""
    
    # 干运行模式
    if [ "$dry_run" = true ]; then
        print_status "将要执行的命令:"
        echo "  GHCRTS=\"-M${memory_mb}m -A256k -n32k -H1m -qg -G1\""
        echo "  QUICKCHECK_MAX_TESTS=1"
        echo "  QUICKCHECK_MAX_SIZE=1"
        echo "  QUICKCHECK_MAX_SHRINKS=0"
        echo "  cabal test --test-show-details=direct Typus --test-option=+RTS --test-option=-M${memory_mb}m"
        exit 0
    fi
    
    # 运行测试
    local extra_args=""
    if [ "$verbose" = true ]; then
        extra_args="--test-option=--verbose"
    fi
    
    if run_memory_optimized_tests "$memory_level" "$memory_mb" "$extra_args"; then
        print_success "内存优化测试完成"
        exit 0
    else
        print_error "内存优化测试失败"
        exit 1
    fi
}

# 运行主函数
main "$@"