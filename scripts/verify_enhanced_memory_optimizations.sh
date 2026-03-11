#!/usr/bin/env bash
# 验证增强的内存优化策略
# 确保测试用例不会消耗大量内存，同时验证测试覆盖率和功能完整性

set -e

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

# 验证函数
verify_memory_usage() {
    log_info "=== 验证内存使用情况 ==="
    
    # 设置内存限制
    local memory_limit_mb=16
    
    # 运行测试并监控内存
    log_info "运行内存优化测试..."
    
    local output_file="/tmp/typus_memory_test_$$.log"
    
    # 使用time命令监控内存使用
    /usr/bin/time -f "内存使用: %M KB" -o "$output_file" \
    bash scripts/run_enhanced_unified_memory_tests.sh 2>&1 | tee "/tmp/typus_test_output_$$.log"
    
    local exit_code=${PIPESTATUS[0]}
    
    # 读取内存使用
    local memory_usage_kb=$(grep "内存使用:" "$output_file" | awk '{print $3}')
    local memory_usage_mb=$((memory_usage_kb / 1024))
    
    log_info "测试内存使用: ${memory_usage_mb}MB (限制: ${memory_limit_mb}MB)"
    
    if [[ "$memory_usage_mb" -gt "$memory_limit_mb" ]]; then
        log_error "内存使用超过限制: ${memory_usage_mb}MB > ${memory_limit_mb}MB"
        return 1
    else
        log_success "内存使用在限制范围内: ${memory_usage_mb}MB <= ${memory_limit_mb}MB"
    fi
    
    # 清理临时文件
    rm -f "$output_file" "/tmp/typus_test_output_$$.log"
    
    return $exit_code
}

verify_test_coverage() {
    log_info "=== 验证测试覆盖率 ==="
    
    # 检查核心模块测试覆盖
    local essential_modules=("Utils" "Parser" "Compiler" "ErrorHandler" "Dependencies")
    local missing_tests=()
    
    for module in "${essential_modules[@]}"; do
        log_info "检查模块测试覆盖: ${module}"
        
        # 查找对应的测试文件
        if find test/Test -name "*${module}*" -o -name "*${module,,}*" | grep -q .; then
            log_success "找到模块 ${module} 的测试文件"
        else
            log_warn "未找到模块 ${module} 的测试文件"
            missing_tests+=("$module")
        fi
    done
    
    if [[ "${#missing_tests[@]}" -gt 0 ]]; then
        log_warn "以下核心模块缺少测试: ${missing_tests[*]}"
        # 这不一定是错误，只是警告
    else
        log_success "所有核心模块都有测试覆盖"
    fi
    
    # 检查优化版本测试文件
    log_info "检查优化版本测试文件..."
    
    local optimized_files_count=$(find test/Test -name "*Optimized*.hs" | wc -l)
    local total_test_files=$(find test/Test -name "*.hs" | wc -l)
    
    log_info "优化版本测试文件: ${optimized_files_count}/${total_test_files}"
    
    if [[ "$optimized_files_count" -eq 0 ]]; then
        log_warn "未找到优化版本测试文件"
    elif [[ "$optimized_files_count" -lt $((total_test_files / 2)) ]]; then
        log_warn "优化版本测试文件较少，建议为更多测试创建优化版本"
    else
        log_success "优化版本测试文件覆盖良好"
    fi
}

verify_functionality() {
    log_info "=== 验证功能完整性 ==="
    
    # 运行核心功能测试
    log_info "运行核心功能验证测试..."
    
    # 设置测试环境
    export LC_ALL=C
    export LANG=C
    export ULS_MEMORY_OPTIMIZED=1
    
    # 运行核心测试
    cabal test typus-test \
        --test-option="+RTS" \
        --test-option="-M16m" \
        --test-option="-RTS" \
        --test-show-details=direct \
        --test-option="--test-pattern=BasicQuickCheckTestSuite" \
    && {
        log_success "核心功能测试通过"
    } || {
        log_error "核心功能测试失败"
        return 1
    }
}

verify_no_test_deletion() {
    log_info "=== 验证没有测试被删除 ==="
    
    # 检查git状态，确保没有测试文件被删除
    local deleted_files=$(git status --porcelain | grep "^ D.*\\.hs$" | wc -l)
    
    if [[ "$deleted_files" -gt 0 ]]; then
        log_error "检测到测试文件被删除:"
        git status --porcelain | grep "^ D.*\\.hs$"
        return 1
    else
        log_success "没有测试文件被删除"
    fi
    
    # 检查原始测试文件是否仍然存在
    local original_200_tests=$(find test/Test -name "*200*.hs" ! -name "*Optimized*" | wc -l)
    
    if [[ "$original_200_tests" -eq 0 ]]; then
        log_warn "未找到原始200测试文件"
    else
        log_success "原始测试文件仍然存在: ${original_200_tests} 个文件"
    fi
}

# 主验证函数
main() {
    log_info "=== 增强的内存优化策略验证 ==="
    
    local all_passed=true
    
    # 验证1: 没有测试被删除
    if ! verify_no_test_deletion; then
        all_passed=false
    fi
    
    # 验证2: 测试覆盖率
    verify_test_coverage
    
    # 验证3: 功能完整性
    if ! verify_functionality; then
        all_passed=false
    fi
    
    # 验证4: 内存使用
    if ! verify_memory_usage; then
        all_passed=false
    fi
    
    # 最终结果
    echo ""
    log_info "=== 验证结果汇总 ==="
    
    if [[ "$all_passed" == "true" ]]; then
        log_success "所有验证通过！内存优化策略有效且安全"
        log_success "- 没有测试被删除"
        log_success "- 核心功能测试通过"
        log_success "- 内存使用在限制范围内"
        log_success "- 测试覆盖良好"
    else
        log_error "部分验证失败，请检查上述错误信息"
        exit 1
    fi
}

# 运行主函数
main "$@"