#!/bin/bash
# 增强内存测试配置脚本
# 提供高级内存配置和环境设置

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

# 增强内存配置
declare -A ENHANCED_CONFIGS
ENHANCED_CONFIGS[standard]="16:balanced:smart:detailed"
ENHANCED_CONFIGS[performance]="32:lazy:full:realtime"
ENHANCED_CONFIGS[minimal]="2:immediate:essential:none"
ENHANCED_CONFIGS[development]="64:predictive:full:realtime"

# 打印函数
print_header() {
    echo -e "${PURPLE}======================================${NC}"
    echo -e "${PURPLE}增强内存测试配置${NC}"
    echo -e "${PURPLE}======================================${NC}"
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

# 显示增强配置
show_enhanced_config() {
    local environment=$1
    local config=${ENHANCED_CONFIGS[$environment]}
    
    if [ -z "$config" ]; then
        print_error "未知的增强环境: $environment"
        return 1
    fi
    
    IFS=':' read -r memory_limit gc_strategy test_selection monitoring <<< "$config"
    
    echo -e "${CYAN}[增强配置]${NC}"
    echo -e "  环境: ${environment}"
    echo -e "  内存限制: ${memory_limit}MB"
    echo -e "  GC策略: ${gc_strategy}"
    echo -e "  测试选择: ${test_selection}"
    echo -e "  监控级别: ${monitoring}"
    echo ""
}

# 应用增强配置
apply_enhanced_config() {
    local environment=$1
    local config=${ENHANCED_CONFIGS[$environment]}
    
    if [ -z "$config" ]; then
        print_error "未知的增强环境: $environment"
        return 1
    fi
    
    IFS=':' read -r memory_limit gc_strategy test_selection monitoring <<< "$config"
    
    print_status "应用增强配置: $environment"
    show_enhanced_config "$environment"
    
    # 设置环境变量
    export TYPUS_ENHANCED_MODE=1
    export TYPUS_ENVIRONMENT="$environment"
    export TYPUS_ENHANCED_MEMORY_LIMIT="$memory_limit"
    export TYPUS_ENHANCED_GC_STRATEGY="$gc_strategy"
    export TYPUS_ENHANCED_TEST_SELECTION="$test_selection"
    export TYPUS_ENHANCED_MONITORING="$monitoring"
    
    # 根据环境设置特定变量
    case $environment in
        standard)
            export QUICKCHECK_TESTS=3
            export QUICKCHECK_MAX_SIZE=2
            export QUICKCHECK_MAX_SHRINKS=1
            export GHCRTS="-M16m -A1m -n128k -H4m -qg -G1"
            ;;
        performance)
            export QUICKCHECK_TESTS=5
            export QUICKCHECK_MAX_SIZE=3
            export QUICKCHECK_MAX_SHRINKS=2
            export GHCRTS="-M32m -A2m -n256k -H8m -qg -G1"
            ;;
        minimal)
            export QUICKCHECK_TESTS=1
            export QUICKCHECK_MAX_SIZE=1
            export QUICKCHECK_MAX_SHRINKS=0
            export GHCRTS="-M2m -A128k -n16k -H512k -qg -G1"
            ;;
        development)
            export QUICKCHECK_TESTS=10
            export QUICKCHECK_MAX_SIZE=5
            export QUICKCHECK_MAX_SHRinks=3
            export GHCRTS="-M64m -A4m -n512k -H16m -qg -G1"
            ;;
    esac
    
    print_success "增强配置已应用: $environment"
}

# 列出所有可用环境
list_environments() {
    echo "可用的增强环境:"
    for env in "${!ENHANCED_CONFIGS[@]}"; do
        echo "  $env"
    done
    echo ""
}

# 显示帮助
show_help() {
    echo "增强内存测试配置脚本"
    echo ""
    echo "用法: $0 [环境] [选项]"
    echo ""
    echo "环境:"
    echo "  standard     标准环境 (16MB) - 平衡模式"
    echo "  performance  性能环境 (32MB) - 完整测试"
    echo "  minimal      最小环境 (2MB) - 仅关键测试"
    echo "  development  开发环境 (64MB) - 完整功能"
    echo ""
    echo "选项:"
    echo "  --help, -h      显示此帮助信息"
    echo "  --list          列出所有可用环境"
    echo "  --show [env]    显示指定环境的配置"
    echo "  --environment   显示当前环境配置"
    echo ""
    echo "示例:"
    echo "  $0 standard      # 应用标准环境配置"
    echo "  $0 minimal       # 应用最小环境配置"
    echo "  $0 --show perf   # 显示性能环境配置"
    echo ""
}

# 显示当前环境
show_current_environment() {
    if [ -n "$TYPUS_ENVIRONMENT" ]; then
        print_status "当前增强环境: $TYPUS_ENVIRONMENT"
        show_enhanced_config "$TYPUS_ENVIRONMENT"
    else
        print_warning "未设置增强环境"
    fi
}

# 主函数
main() {
    local environment=""
    local show_only=false
    local list_only=false
    local show_current=false
    
    # 解析命令行参数
    while [[ $# -gt 0 ]]; do
        case $1 in
            --help|-h)
                show_help
                exit 0
                ;;
            --list)
                list_only=true
                shift
                ;;
            --show)
                show_only=true
                shift
                if [[ $# -gt 0 && ! "$1" =~ ^-- ]]; then
                    environment="$1"
                    shift
                fi
                ;;
            --environment)
                show_current=true
                shift
                ;;
            standard|performance|minimal|development)
                environment="$1"
                shift
                ;;
            *)
                print_error "未知选项: $1"
                show_help
                exit 1
                ;;
        esac
    done
    
    # 打印头部
    print_header
    
    # 列出环境
    if [ "$list_only" = true ]; then
        list_environments
        exit 0
    fi
    
    # 显示当前环境
    if [ "$show_current" = true ]; then
        show_current_environment
        exit 0
    fi
    
    # 显示环境配置
    if [ "$show_only" = true ]; then
        if [ -n "$environment" ]; then
            show_enhanced_config "$environment"
        else
            print_warning "请指定要显示的环境"
            list_environments
        fi
        exit 0
    fi
    
    # 应用环境配置
    if [ -n "$environment" ]; then
        apply_enhanced_config "$environment"
        
        print_status "环境变量已设置:"
        print_status "  TYPUS_ENHANCED_MODE=$TYPUS_ENHANCED_MODE"
        print_status "  TYPUS_ENVIRONMENT=$TYPUS_ENVIRONMENT"
        print_status "  QUICKCHECK_TESTS=$QUICKCHECK_TESTS"
        print_status "  QUICKCHECK_MAX_SIZE=$QUICKCHECK_MAX_SIZE"
        print_status "  QUICKCHECK_MAX_SHRINKS=$QUICKCHECK_MAX_SHRINKS"
        print_status "  GHCRTS=$GHCRTS"
        
        print_success "增强内存配置完成！"
    else
        print_warning "请指定要应用的环境"
        list_environments
        exit 1
    fi
}

# 运行主函数
main "$@"