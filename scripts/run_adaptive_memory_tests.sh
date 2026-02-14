#!/bin/bash
# 自适应内存测试运行器
# 根据可用内存自动选择最适合的测试级别

set -e

# 颜色代码
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

print_header() {
    echo -e "${PURPLE}========================================${NC}"
    echo -e "${PURPLE}自适应内存测试运行器${NC}"
    echo -e "${PURPLE}========================================${NC}"
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

print_config() {
    echo -e "${CYAN}[CONFIG]${NC} $1"
}

# 帮助函数
show_help() {
    echo "自适应内存测试运行器 for Typus"
    echo ""
    echo "用法: $0 [选项]"
    echo ""
    echo "选项:"
    echo "  --verbose, -v     启用详细输出"
    echo "  --help, -h        显示此帮助信息"
    echo "  --force-level     强制指定内存级别 (nano|micro|tiny|lightweight|standard)"
    echo "  --check-memory    检查可用内存并建议配置"
    echo ""
    echo "环境变量:"
    echo "  TYPUS_FORCE_MEMORY_LEVEL  强制指定内存级别"
    echo "  TYPUS_VERBOSE             启用详细输出"
    echo ""
    echo "内存级别说明:"
    echo "  nano        - 极度受限环境 (<64MB)"
    echo "  micro       - 微型环境 (64-128MB)"
    echo "  tiny        - 超轻量环境 (128-256MB)"
    echo "  lightweight - 轻量环境 (256-512MB)"
    echo "  standard    - 标准环境 (>512MB)"
}

# 检查可用内存
check_memory() {
    print_status "检查系统内存..."
    
    if command -v free >/dev/null 2>&1; then
        # Linux with free command
        local available=$(free -m | awk 'NR==2{printf "%.0f", $7}')
        print_config "可用内存: ${available}MB"
        
        # 建议内存级别
        if [ "$available" -le 64 ]; then
            suggested="nano"
        elif [ "$available" -le 128 ]; then
            suggested="micro"
        elif [ "$available" -le 256 ]; then
            suggested="tiny"
        elif [ "$available" -le 512 ]; then
            suggested="lightweight"
        else
            suggested="standard"
        fi
        
        print_config "建议内存级别: $suggested"
        return 0
    elif command -v vm_stat >/dev/null 2>&1; then
        # macOS with vm_stat
        local page_size=$(vm_stat | head -1 | sed 's/.*page size of \([0-9]*\).*/\1/')
        local free_pages=$(vm_stat | awk '/free/ {gsub(/\./, ""); print $3}')
        local available=$((free_pages * page_size / 1024 / 1024))
        print_config "可用内存: ${available}MB"
        return 0
    else
        print_warning "无法确定可用内存，使用默认配置"
        return 1
    fi
}

# 构建适应性测试运行器
build_adaptive_runner() {
    print_status "构建适应性测试运行器..."
    
    if cabal build adaptive-memory-test-runner; then
        print_success "构建成功"
    else
        print_error "构建失败"
        return 1
    fi
}

# 运行适应性测试
run_adaptive_tests() {
    local force_level="$1"
    local verbose="$2"
    
    print_status "运行自适应内存测试..."
    
    # 设置环境变量
    if [ -n "$force_level" ]; then
        export TYPUS_FORCE_MEMORY_LEVEL="$force_level"
        print_config "强制内存级别: $force_level"
    fi
    
    if [ "$verbose" = "true" ]; then
        export TYPUS_VERBOSE="true"
        print_config "详细模式: 已启用"
    fi
    
    # 根据可用内存设置RTS选项
    local available=$(free -m | awk 'NR==2{printf "%.0f", $7}' 2>/dev/null || echo "1024")
    
    if [ "$available" -le 64 ]; then
        export GHCRTS="-M8m -A256k -n32k -H1m -qg"
        print_config "RTS选项: 极度内存受限"
    elif [ "$available" -le 128 ]; then
        export GHCRTS="-M16m -A512k -n64k -H2m -qg"
        print_config "RTS选项: 微型内存"
    elif [ "$available" -le 256 ]; then
        export GHCRTS="-M32m -A1m -n128k -H4m -qg"
        print_config "RTS选项: 超轻量内存"
    elif [ "$available" -le 512 ]; then
        export GHCRTS="-M64m -A2m -n256k -H8m -qg"
        print_config "RTS选项: 轻量内存"
    else
        export GHCRTS="-M128m -A4m -n512k -H16m -qg"
        print_config "RTS选项: 标准内存"
    fi
    
    print_config "RTS选项: $GHCRTS"
    
    # 运行测试
    local test_args=""
    if [ "$verbose" = "true" ]; then
        test_args="--verbose"
    fi
    
    if cabal run adaptive-memory-test-runner -- $test_args; then
        print_success "自适应内存测试完成!"
    else
        print_error "测试失败"
        return 1
    fi
    
    # 清理环境
    unset GHCRTS
}

# 主执行逻辑
main() {
    local force_level=""
    local verbose="false"
    local show_help="false"
    local check_memory_only="false"
    
    # 解析命令行参数
    while [[ $# -gt 0 ]]; do
        case $1 in
            --help|-h)
                show_help="true"
                shift
                ;;
            --verbose|-v)
                verbose="true"
                shift
                ;;
            --force-level)
                force_level="$2"
                shift 2
                ;;
            --check-memory)
                check_memory_only="true"
                shift
                ;;
            *)
                print_error "未知选项: $1"
                echo "使用 --help 查看使用信息"
                exit 1
                ;;
        esac
    done
    
    # 显示帮助信息
    if [ "$show_help" = "true" ]; then
        show_help
        exit 0
    fi
    
    # 仅检查内存
    if [ "$check_memory_only" = "true" ]; then
        check_memory
        exit 0
    fi
    
    # 打印标题
    print_header
    
    # 检查内存
    check_memory
    
    # 检查是否强制指定了内存级别
    if [ -n "$TYPUS_FORCE_MEMORY_LEVEL" ]; then
        force_level="$TYPUS_FORCE_MEMORY_LEVEL"
        print_config "使用环境变量强制内存级别: $force_level"
    fi
    
    # 验证内存级别
    if [ -n "$force_level" ]; then
        case "$force_level" in
            nano|micro|tiny|lightweight|standard)
                print_config "验证通过: 内存级别 $force_level"
                ;;
            *)
                print_error "无效的内存级别: $force_level"
                echo "有效级别: nano, micro, tiny, lightweight, standard"
                exit 1
                ;;
        esac
    fi
    
    # 检查详细标志
    if [ "$TYPUS_VERBOSE" = "true" ]; then
        verbose="true"
    fi
    
    # 构建测试运行器
    if build_adaptive_runner; then
        # 运行测试
        if run_adaptive_tests "$force_level" "$verbose"; then
            echo ""
            print_success "自适应内存测试成功完成!"
        else
            echo ""
            print_error "测试执行失败"
            exit 1
        fi
    else
        print_error "无法构建测试运行器"
        exit 1
    fi
}

# 运行主函数
main "$@"