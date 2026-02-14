#!/bin/bash
# 极度增强的内存优化测试脚本
# 专注于减少内存消耗而不删除测试用例

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

# 内存级别配置
declare -A MEMORY_LEVELS=(
    ["critical"]="8MB - 关键测试仅"
    ["minimal"]="16MB - 最小内存使用"
    ["ultra"]="24MB - 超低内存使用"
    ["enhanced"]="32MB - 增强内存优化"
    ["optimized"]="48MB - 优化内存使用"
    ["standard"]="64MB - 标准内存限制"
)

# 每个级别的QuickCheck参数
declare -A QUICKCHECK_CONFIGS=(
    ["critical"]="--quickcheck-tests=1 --quickcheck-max-size=1 --quickcheck-shrinks=0"
    ["minimal"]="--quickcheck-tests=2 --quickcheck-max-size=1 --quickcheck-shrinks=0"
    ["ultra"]="--quickcheck-tests=3 --quickcheck-max-size=2 --quickcheck-shrinks=1"
    ["enhanced"]="--quickcheck-tests=5 --quickcheck-max-size=3 --quickcheck-shrinks=2"
    ["optimized"]="--quickcheck-tests=10 --quickcheck-max-size=5 --quickcheck-shrinks=5"
    ["standard"]="--quickcheck-tests=25 --quickcheck-max-size=10 --quickcheck-shrinks=10"
)

# RTS内存限制配置
declare -A RTS_CONFIGS=(
    ["critical"]="-M8m -A1m -n128k -H1m -qg -G1"
    ["minimal"]="-M16m -A2m -n256k -H2m -qg -G1"
    ["ultra"]="-M24m -A3m -n512k -H3m -qg -G1"
    ["enhanced"]="-M32m -A4m -n1m -H4m -qg -G1"
    ["optimized"]="-M48m -A8m -n2m -H6m -qg -G1"
    ["standard"]="-M64m -A16m -n4m -H8m -qg -G1"
)

# 显示帮助信息
show_help() {
    echo -e "${PURPLE}极度增强的内存优化测试运行器${NC}"
    echo ""
    echo "用法: $0 [内存级别] [选项]"
    echo ""
    echo "内存级别:"
    for level in "${!MEMORY_LEVELS[@]}"; do
        printf "  %-12s - %s\n" "$level" "${MEMORY_LEVELS[$level]}"
    done
    echo ""
    echo "选项:"
    echo "  --auto                自动检测并选择合适的内存级别"
    echo "  --monitor             启用内存监控"
    echo "  --verbose, -v         详细输出"
    echo "  --help, -h            显示帮助信息"
    echo "  --skip-go-build       跳过Go构建步骤"
    echo "  --force-cleanup       强制内存清理"
    echo "  --adaptive            启用自适应内存管理"
    echo ""
    echo "环境变量:"
    echo "  TYPUS_MEMORY_LEVEL     内存级别"
    echo "  TYPUS_FORCE_GC         强制垃圾回收"
    echo "  TYPUS_MEMORY_MONITOR   启用内存监控"
}

# 检测可用内存
detect_available_memory() {
    local available_mb=0
    
    if command -v free >/dev/null 2>&1; then
        available_mb=$(free -m | awk 'NR==2{printf "%.0f", $7}')
    elif command -v vm_stat >/dev/null 2>&1; then
        local page_size=$(vm_stat | head -1 | sed 's/.*page size of \([0-9]*\).*/\1/')
        local free_pages=$(vm_stat | awk '/free/ {gsub(/\./, ""); print $3}')
        available_mb=$((free_pages * page_size / 1024 / 1024))
    fi
    
    echo $available_mb
}

# 自动选择内存级别
auto_select_memory_level() {
    local available_mb=$(detect_available_memory)
    echo -e "${CYAN}检测到可用内存: ${available_mb}MB${NC}"
    
    if [ "$available_mb" -le 16 ]; then
        echo "critical"
    elif [ "$available_mb" -le 32 ]; then
        echo "minimal"
    elif [ "$available_mb" -le 64 ]; then
        echo "ultra"
    elif [ "$available_mb" -le 128 ]; then
        echo "enhanced"
    elif [ "$available_mb" -le 256 ]; then
        echo "optimized"
    else
        echo "standard"
    fi
}

# 应用内存优化设置
apply_memory_optimizations() {
    local level="$1"
    
    echo -e "${CYAN}应用内存优化 (级别: $level)${NC}"
    echo -e "${BLUE}  内存限制: ${MEMORY_LEVELS[$level]}${NC}"
    echo -e "${BLUE}  QuickCheck配置: ${QUICKCHECK_CONFIGS[$level]}${NC}"
    echo -e "${BLUE}  RTS配置: ${RTS_CONFIGS[$level]}${NC}"
    
    # 设置RTS选项
    export GHCRTS="${RTS_CONFIGS[$level]}"
    
    # 设置环境变量
    export TYPUS_MEMORY_LEVEL="$level"
    export TYPUS_FORCE_GC="true"
    export TYPUS_SKIP_GO_BUILD="${SKIP_GO_BUILD:-1}"
    
    # 额外的内存优化设置
    export GHC_HEAP_ALLOCATION=0.05
    export GHC_GC_YIELD_LIMIT=500
    export TYPUS_MINIMAL_MEMORY="true"
}

# 执行增强的内存清理
enhanced_memory_cleanup() {
    echo -e "${CYAN}执行增强的内存清理...${NC}"
    
    # 多轮垃圾回收
    for i in {1..5}; do
        echo -ne "\r  清理轮次 $i/5"
        sync
        echo 3 > /proc/sys/vm/drop_caches 2>/dev/null || true
        sleep 0.1
    done
    echo ""
    
    # 强制Haskell GC
    if command -v ghc >/dev/null 2>&1; then
        ghc -e 'import System.Mem; performGC; replicateM_ 5 performGC' 2>/dev/null || true
    fi
}

# 构建项目
build_project() {
    echo -e "${CYAN}构建项目...${NC}"
    
    # 清理之前的构建
    cabal clean 2>/dev/null || true
    
    # 使用内存优化构建
    cabal build --flags="fast" --ghc-options="-rtsopts -with-rtsopts=${GHCRTS}"
    
    if [ $? -ne 0 ]; then
        echo -e "${RED}构建失败${NC}"
        exit 1
    fi
    
    echo -e "${GREEN}✓ 构建成功${NC}"
}

# 运行测试
run_tests() {
    local level="$1"
    local qc_config="${QUICKCHECK_CONFIGS[$level]}"
    
    echo -e "${CYAN}运行测试 (内存级别: $level)...${NC}"
    
    # 预清理
    enhanced_memory_cleanup
    
    # 运行测试
    if command -v /usr/bin/time >/dev/null 2>&1 && [ "$MONITOR" = "true" ]; then
        echo -e "${BLUE}启用内存监控...${NC}"
        /usr/bin/time -v cabal test --flags="fast" --test-options="$qc_config"
    else
        cabal test --flags="fast" --test-options="$qc_config"
    fi
    
    local test_result=$?
    
    # 后清理
    enhanced_memory_cleanup
    
    if [ $test_result -eq 0 ]; then
        echo -e "${GREEN}✓ 测试完成成功！${NC}"
        echo -e "${GREEN}  内存使用已优化至: ${MEMORY_LEVELS[$level]}${NC}"
    else
        echo -e "${RED}✗ 测试失败${NC}"
        echo -e "${YELLOW}  建议尝试更高的内存级别${NC}"
        return 1
    fi
}

# 自适应内存管理
adaptive_memory_management() {
    echo -e "${CYAN}启用自适应内存管理...${NC}"
    
    local current_level="$1"
    local test_result="$2"
    
    # 如果测试失败，尝试更高的内存级别
    if [ $test_result -ne 0 ]; then
        case "$current_level" in
            "critical")
                echo -e "${YELLOW}测试失败，尝试minimal级别...${NC}"
                return 2
                ;;
            "minimal")
                echo -e "${YELLOW}测试失败，尝试ultra级别...${NC}"
                return 3
                ;;
            "ultra")
                echo -e "${YELLOW}测试失败，尝试enhanced级别...${NC}"
                return 4
                ;;
            "enhanced")
                echo -e "${YELLOW}测试失败，尝试optimized级别...${NC}"
                return 5
                ;;
            "optimized")
                echo -e "${YELLOW}测试失败，尝试standard级别...${NC}"
                return 6
                ;;
            *)
                echo -e "${RED}所有内存级别都失败${NC}"
                return 7
                ;;
        esac
    fi
    
    return 0
}

# 主函数
main() {
    local memory_level=""
    local auto_detect="false"
    local monitor="false"
    local verbose="false"
    local skip_go_build="false"
    local force_cleanup="false"
    local adaptive="false"
    
    # 解析命令行参数
    while [[ $# -gt 0 ]]; do
        case $1 in
            --help|-h)
                show_help
                exit 0
                ;;
            --auto)
                auto_detect="true"
                shift
                ;;
            --monitor)
                monitor="true"
                export TYPUS_MEMORY_MONITOR="true"
                shift
                ;;
            --verbose|-v)
                verbose="true"
                export TYPUS_VERBOSE="true"
                shift
                ;;
            --skip-go-build)
                skip_go_build="true"
                export SKIP_GO_BUILD="1"
                shift
                ;;
            --force-cleanup)
                force_cleanup="true"
                shift
                ;;
            --adaptive)
                adaptive="true"
                shift
                ;;
            critical|minimal|ultra|enhanced|optimized|standard)
                memory_level="$1"
                shift
                ;;
            *)
                echo -e "${RED}未知选项: $1${NC}"
                echo "使用 --help 查看帮助信息"
                exit 1
                ;;
        esac
    done
    
    # 自动检测内存级别
    if [ "$auto_detect" = "true" ] || [ -z "$memory_level" ]; then
        if [ -n "$TYPUS_MEMORY_LEVEL" ]; then
            memory_level="$TYPUS_MEMORY_LEVEL"
            echo -e "${CYAN}使用环境变量内存级别: $memory_level${NC}"
        else
            memory_level=$(auto_select_memory_level)
            echo -e "${CYAN}自动选择内存级别: $memory_level${NC}"
        fi
    fi
    
    # 验证内存级别
    if [ -z "${MEMORY_LEVELS[$memory_level]}" ]; then
        echo -e "${RED}无效的内存级别: $memory_level${NC}"
        echo ""
        echo "可用的内存级别:"
        for level in "${!MEMORY_LEVELS[@]}"; do
            printf "  %-12s - %s\n" "$level" "${MEMORY_LEVELS[$level]}"
        done
        exit 1
    fi
    
    # 显示配置
    echo -e "${PURPLE}=== 极度增强的内存优化测试 ===${NC}"
    echo -e "${BLUE}内存级别: $memory_level (${MEMORY_LEVELS[$memory_level]})${NC}"
    echo -e "${BLUE}自适应管理: $([ "$adaptive" = "true" ] && echo "启用" || echo "禁用")${NC}"
    echo -e "${BLUE}内存监控: $([ "$monitor" = "true" ] && echo "启用" || echo "禁用")${NC}"
    echo ""
    
    # 强制清理
    if [ "$force_cleanup" = "true" ]; then
        enhanced_memory_cleanup
    fi
    
    # 应用内存优化
    apply_memory_optimizations "$memory_level"
    
    # 构建项目
    build_project
    
    # 运行测试
    local test_result=0
    if [ "$adaptive" = "true" ]; then
        # 自适应内存管理
        local current_level="$memory_level"
        local max_attempts=3
        local attempt=1
        
        while [ $attempt -le $max_attempts ]; do
            echo -e "${CYAN}尝试 $attempt/$max_attempts (级别: $current_level)${NC}"
            
            apply_memory_optimizations "$current_level"
            run_tests "$current_level"
            test_result=$?
            
            if [ $test_result -eq 0 ]; then
                break
            fi
            
            # 尝试下一个级别
            case "$current_level" in
                "critical") current_level="minimal" ;;
                "minimal") current_level="ultra" ;;
                "ultra") current_level="enhanced" ;;
                "enhanced") current_level="optimized" ;;
                "optimized") current_level="standard" ;;
                *) break ;;
            esac
            
            attempt=$((attempt + 1))
        done
    else
        # 标准运行
        run_tests "$memory_level"
        test_result=$?
    fi
    
    # 显示结果
    echo ""
    if [ $test_result -eq 0 ]; then
        echo -e "${GREEN}=== 测试成功完成 ===${NC}"
        echo -e "${GREEN}内存优化级别: $memory_level${NC}"
        echo -e "${GREEN}内存使用: ${MEMORY_LEVELS[$memory_level]}${NC}"
    else
        echo -e "${RED}=== 测试失败 ===${NC}"
        echo -e "${YELLOW}建议:${NC}"
        echo -e "${BLUE}1. 使用 --adaptive 启用自适应内存管理${NC}"
        echo -e "${BLUE}2. 尝试更高的内存级别${NC}"
        echo -e "${BLUE}3. 使用 --auto 自动选择合适的级别${NC}"
        exit 1
    fi
}

# 运行主函数
main "$@"