#!/bin/bash

# 超级优化测试运行脚本
# 专为极端内存受限环境设计

set -e

# 默认配置
MEMORY_LEVEL=${TYPUS_MEMORY_LEVEL:-"super_optimized"}
DEBUG=${TYPUS_DEBUG:-"false"}
MEMORY_LIMIT=${TYPUS_MEMORY_LIMIT:-"8"}  # 默认8MB内存限制

# 颜色输出
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 打印带颜色的消息
print_info() {
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

# 检查依赖
check_dependencies() {
    print_info "Checking dependencies..."
    
    if ! command -v stack &> /dev/null; then
        print_error "Stack not found. Please install Stack."
        exit 1
    fi
    
    if ! command -v ghc &> /dev/null; then
        print_error "GHC not found. Please install GHC."
        exit 1
    fi
    
    print_success "Dependencies check passed"
}

# 设置内存限制
setup_memory_limits() {
    print_info "Setting up memory limits..."
    print_info "Memory level: $MEMORY_LEVEL"
    print_info "Memory limit: ${MEMORY_LIMIT}MB"
    
    # 设置GHC运行时内存限制
    export GHCRTS="-M${MEMORY_LIMIT}m -A1m -n2m"
    
    # 设置环境变量
    export TYPUS_MEMORY_LEVEL="$MEMORY_LEVEL"
    export TYPUS_DEBUG="$DEBUG"
    export TYPUS_MEMORY_LIMIT="$MEMORY_LIMIT"
    
    print_success "Memory limits configured"
}

# 清理函数
cleanup() {
    print_info "Cleaning up..."
    
    # 清理临时文件
    rm -f /tmp/typus-test-*.log
    
    # 清理stack工作目录
    stack clean 2>/dev/null || true
    
    print_success "Cleanup completed"
}

# 构建超级优化测试运行器
build_test_runner() {
    print_info "Building super optimized test runner..."
    
    # 使用最小化的构建选项
    stack build \
        --ghc-options="-O0 -fno-warn-unused-imports -fno-warn-missing-signatures" \
        --fast
    
    print_success "Test runner built successfully"
}

# 运行超级优化测试
run_super_optimized_tests() {
    print_info "Running super optimized tests..."
    
    # 创建日志文件
    LOG_FILE="/tmp/typus-test-$(date +%s).log"
    
    # 运行测试
    if stack exec super-optimized-test-runner \
        -- \
        --quickcheck-tests=1 \
        --quickcheck-max-size=1 \
        --quickcheck-max-shrinks=0 \
        2>&1 | tee "$LOG_FILE"; then
        
        print_success "All tests passed!"
        
        # 分析内存使用
        if [[ "$DEBUG" == "true" ]]; then
            print_info "Memory usage analysis:"
            grep -i "memory" "$LOG_FILE" || print_warning "No memory usage data found"
        fi
        
    else
        print_error "Tests failed!"
        print_error "Check log file: $LOG_FILE"
        
        # 显示错误摘要
        if grep -q "FAIL\|ERROR" "$LOG_FILE"; then
            print_error "Error summary:"
            grep -E "FAIL|ERROR" "$LOG_FILE" | head -5
        fi
        
        exit 1
    fi
}

# 运行内存验证
verify_memory_usage() {
    print_info "Verifying memory usage..."
    
    # 获取当前进程的内存使用
    if command -v ps &> /dev/null; then
        CURRENT_PID=$$
        MEMORY_USAGE=$(ps -o rss= -p "$CURRENT_PID" | awk '{print $1}')
        
        if [[ -n "$MEMORY_USAGE" ]]; then
            MEMORY_MB=$((MEMORY_USAGE / 1024))
            print_info "Current memory usage: ${MEMORY_MB}MB"
            
            if [[ $MEMORY_MB -gt $MEMORY_LIMIT ]]; then
                print_warning "Memory usage exceeds limit!"
            else
                print_success "Memory usage within limits"
            fi
        fi
    fi
}

# 主函数
main() {
    print_info "Starting Super Optimized Test Runner"
    print_info "====================================="
    
    # 设置陷阱，确保清理
    trap cleanup EXIT
    
    # 执行步骤
    check_dependencies
    setup_memory_limits
    cleanup
    build_test_runner
    run_super_optimized_tests
    verify_memory_usage
    
    print_success "Super optimized test run completed successfully!"
}

# 处理命令行参数
while [[ $# -gt 0 ]]; do
    case "$1" in
        --help|-h)
            echo "Super Optimized Test Runner Script"
            echo ""
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --help, -h              Show this help message"
            echo "  --memory-level LEVEL    Set memory level (super_optimized, extreme, minimal, standard, ci)"
            echo "  --memory-limit MB       Set memory limit in MB (default: 8)"
            echo "  --debug                 Enable debug output"
            echo ""
            echo "Environment Variables:"
            echo "  TYPUS_MEMORY_LEVEL      Memory optimization level"
            echo "  TYPUS_DEBUG             Enable debug output (true/false)"
            echo "  TYPUS_MEMORY_LIMIT      Memory limit in MB"
            echo ""
            echo "Examples:"
            echo "  $0"
            echo "  $0 --memory-level extreme --memory-limit 16"
            echo "  TYPUS_DEBUG=true $0"
            exit 0
            ;;
        --memory-level)
            MEMORY_LEVEL="$2"
            shift 2
            ;;
        --memory-limit)
            MEMORY_LIMIT="$2"
            shift 2
            ;;
        --debug)
            DEBUG="true"
            export TYPUS_DEBUG="true"
            shift
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

# 运行主函数
main "$@"