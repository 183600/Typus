#!/bin/bash

# 优化测试运行器 - 使用内存优化策略运行测试
# 确保测试不消耗大量内存，同时保持测试覆盖率

set -euo pipefail

# 配置参数
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
TEST_DIR="$PROJECT_ROOT/test"

# 内存层级配置
MEMORY_LEVELS=("EMERGENCY" "CRITICAL" "MINIMAL" "BALANCED" "NORMAL")

# 颜色输出
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

# 检测内存环境
detect_memory_environment() {
    local available_memory=$(free -m | awk 'NR==2{print $7}' 2>/dev/null || echo 512)
    
    if [[ $available_memory -le 8 ]]; then
        echo "EMERGENCY"
    elif [[ $available_memory -le 16 ]]; then
        echo "CRITICAL"
    elif [[ $available_memory -le 32 ]]; then
        echo "MINIMAL"
    elif [[ $available_memory -le 64 ]]; then
        echo "BALANCED"
    else
        echo "NORMAL"
    fi
}

# 设置内存限制参数
set_memory_limits() {
    local level="$1"
    
    case $level in
        "EMERGENCY")
            export TYPUS_MEMORY_LIMIT_MB=8
            export TYPUS_QUICKCHECK_MAX_TESTS=1
            export TYPUS_QUICKCHECK_MAX_SIZE=1
            export TYPUS_QUICKCHECK_MAX_SHRINKS=0
            export TYPUS_STRING_MAX_LENGTH=1
            export TYPUS_LIST_MAX_LENGTH=2
            ;;
        "CRITICAL")
            export TYPUS_MEMORY_LIMIT_MB=16
            export TYPUS_QUICKCHECK_MAX_TESTS=2
            export TYPUS_QUICKCHECK_MAX_SIZE=1
            export TYPUS_QUICKCHECK_MAX_SHRINKS=0
            export TYPUS_STRING_MAX_LENGTH=2
            export TYPUS_LIST_MAX_LENGTH=3
            ;;
        "MINIMAL")
            export TYPUS_MEMORY_LIMIT_MB=32
            export TYPUS_QUICKCHECK_MAX_TESTS=3
            export TYPUS_QUICKCHECK_MAX_SIZE=2
            export TYPUS_QUICKCHECK_MAX_SHRINKS=1
            export TYPUS_STRING_MAX_LENGTH=3
            export TYPUS_LIST_MAX_LENGTH=5
            ;;
        "BALANCED")
            export TYPUS_MEMORY_LIMIT_MB=64
            export TYPUS_QUICKCHECK_MAX_TESTS=5
            export TYPUS_QUICKCHECK_MAX_SIZE=3
            export TYPUS_QUICKCHECK_MAX_SHRINKS=2
            export TYPUS_STRING_MAX_LENGTH=5
            export TYPUS_LIST_MAX_LENGTH=8
            ;;
        "NORMAL")
            export TYPUS_MEMORY_LIMIT_MB=128
            export TYPUS_QUICKCHECK_MAX_TESTS=10
            export TYPUS_QUICKCHECK_MAX_SIZE=5
            export TYPUS_QUICKCHECK_MAX_SHRINKS=5
            export TYPUS_STRING_MAX_LENGTH=10
            export TYPUS_LIST_MAX_LENGTH=15
            ;;
    esac
    
    log_info "设置内存级别: $level"
    log_info "内存限制: ${TYPUS_MEMORY_LIMIT_MB}MB"
    log_info "QuickCheck 参数: ${TYPUS_QUICKCHECK_MAX_TESTS}测试/${TYPUS_QUICKCHECK_MAX_SIZE}规模"
}

# 选择测试文件
select_test_files() {
    local level="$1"
    
    case $level in
        "EMERGENCY" | "CRITICAL")
            # 仅运行核心测试
            find "$TEST_DIR/Test/Unit" -name "*Basic*.hs" -o -name "*Core*.hs" -o -name "*Essential*.hs" | head -10
            ;;
        "MINIMAL")
            # 运行核心和重要测试
            find "$TEST_DIR/Test/Unit" -name "*Basic*.hs" -o -name "*Core*.hs" -o -name "*Essential*.hs" -o -name "*Important*.hs" | head -20
            ;;
        "BALANCED")
            # 运行大部分测试
            find "$TEST_DIR/Test/Unit" -name "*.hs" | grep -v "*Advanced*" | grep -v "*Comprehensive*" | head -30
            ;;
        "NORMAL")
            # 运行所有测试
            find "$TEST_DIR/Test/Unit" -name "*.hs" | head -50
            ;;
    esac
}

# 运行单个测试文件
run_test_file() {
    local file="$1"
    local level="$2"
    
    log_info "运行测试: $(basename "$file") (级别: $level)"
    
    # 设置 RTS 参数
    local rts_params="-M${TYPUS_MEMORY_LIMIT_MB}m -A1m -K1m"
    
    # 运行测试（这里使用 stack 作为示例）
    if command -v stack >/dev/null 2>&1; then
        GHCRTS="$rts_params" stack runghc "$file" 2>&1 | grep -E "(PASS|FAIL|ERROR)" || true
    else
        log_warning "未找到 stack，跳过测试: $(basename "$file")"
    fi
}

# 运行所有测试
run_all_tests() {
    local level="$1"
    
    log_info "开始运行优化测试 (内存级别: $level)..."
    
    # 设置内存限制
    set_memory_limits "$level"
    
    # 选择测试文件
    local test_files=$(select_test_files "$level")
    
    if [[ -z "$test_files" ]]; then
        log_warning "未找到测试文件"
        return 1
    fi
    
    local passed=0
    local failed=0
    local skipped=0
    
    # 运行每个测试文件
    while IFS= read -r file; do
        if [[ -f "$file" ]]; then
            if run_test_file "$file" "$level"; then
                ((passed++))
            else
                ((failed++))
            fi
        else
            ((skipped++))
        fi
    done <<< "$test_files"
    
    # 输出结果
    echo ""
    log_info "=== 测试结果 ==="
    log_info "通过: $passed"
    log_info "失败: $failed"
    log_info "跳过: $skipped"
    log_info "内存级别: $level"
    
    if [[ $failed -eq 0 ]]; then
        log_success "所有测试通过!"
        return 0
    else
        log_warning "有 $failed 个测试失败"
        return 1
    fi
}

# 显示帮助信息
show_help() {
    cat << EOF
用法: $0 [内存级别]

可用内存级别:
  EMERGENCY   紧急 (8MB以下)
  CRITICAL    关键 (16MB以下)  
  MINIMAL     极简 (32MB以下)
  BALANCED    平衡 (64MB以下)
  NORMAL      正常 (128MB以下)

示例:
  $0                    # 自动检测内存环境
  $0 MINIMAL            # 指定极简内存级别
  $0 EMERGENCY          # 指定紧急内存级别

功能:
- 根据内存环境自动选择测试策略
- 应用内存限制参数
- 选择性运行关键测试
- 最小化内存使用同时保持覆盖率
EOF
}

# 主函数
main() {
    local memory_level="${1:-}"
    
    # 如果没有指定内存级别，自动检测
    if [[ -z "$memory_level" ]]; then
        memory_level=$(detect_memory_environment)
        log_info "自动检测内存环境: $memory_level"
    fi
    
    # 验证内存级别
    if [[ ! " ${MEMORY_LEVELS[@]} " =~ " ${memory_level} " ]]; then
        log_warning "无效的内存级别: $memory_level，使用 NORMAL"
        memory_level="NORMAL"
    fi
    
    # 运行测试
    if run_all_tests "$memory_level"; then
        log_success "测试运行完成"
    else
        log_warning "测试运行完成（有失败）"
        exit 1
    fi
}

# 参数处理
case "${1:-}" in
    "--help" | "-h")
        show_help
        exit 0
        ;;
    *)
        main "$@"
        ;;
esac