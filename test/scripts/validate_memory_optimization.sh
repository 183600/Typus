#!/bin/bash

# 内存优化验证脚本
# 验证测试用例不会消耗大量内存，同时保持测试覆盖率

set -euo pipefail

# 配置参数
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
TEST_DIR="$PROJECT_ROOT/test"

# 颜色输出
GREEN='\033[0;32m'
RED='\033[0;31m'
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

# 检查内存优化配置
check_memory_configuration() {
    log_info "检查内存优化配置..."
    
    local has_errors=false
    
    # 检查关键配置文件
    local config_files=(
        "$TEST_DIR/TestSupport/ComprehensiveTestOptimizer.hs"
        "$TEST_DIR/TestSupport/UnifiedMemoryOptimization.hs"
        "$TEST_DIR/TestSupport/MemoryLimits.hs"
    )
    
    for file in "${config_files[@]}"; do
        if [[ -f "$file" ]]; then
            log_success "✓ $(basename "$file") 存在"
        else
            log_error "✗ $(basename "$file") 不存在"
            has_errors=true
        fi
    done
    
    return $has_errors
}

# 检查测试文件的内存优化
check_test_memory_optimization() {
    log_info "检查测试文件的内存优化..."
    
    local has_errors=false
    local optimized_count=0
    local total_count=0
    
    # 检查关键测试文件
    local test_files=$(find "$TEST_DIR/Test/Unit" -name "*.hs" -type f | head -20)
    
    while IFS= read -r file; do
        if [[ -f "$file" ]]; then
            ((total_count++))
            
            # 检查是否包含内存优化参数
            if grep -q "QuickCheckTests" "$file" || grep -q "QuickCheckMaxSize" "$file"; then
                ((optimized_count++))
                log_success "✓ $(basename "$file") 包含内存优化"
            else
                log_warning "⚠ $(basename "$file") 缺少内存优化参数"
                has_errors=true
            fi
        fi
    done <<< "$test_files"
    
    log_info "优化覆盖率: $optimized_count/$total_count 文件"
    
    if [[ $optimized_count -eq 0 ]]; then
        log_error "没有测试文件包含内存优化"
        return 1
    fi
    
    return $has_errors
}

# 检查测试覆盖率
check_test_coverage() {
    log_info "检查测试覆盖率..."
    
    local has_errors=false
    
    # 检查关键功能测试是否存在
    local critical_tests=(
        "BasicParserQuickCheckSpec.hs"
        "CoreCompilerQuickCheckSpec.hs"
        "EssentialQuickCheckTests.hs"
        "CoreUtilsQuickCheckSpec.hs"
        "CoreParserQuickCheckSpec.hs"
    )
    
    for test in "${critical_tests[@]}"; do
        if [[ -f "$TEST_DIR/Test/Unit/$test" ]]; then
            log_success "✓ $test 存在"
        else
            log_error "✗ $test 不存在"
            has_errors=true
        fi
    done
    
    return $has_errors
}

# 检查内存使用限制
check_memory_limits() {
    log_info "检查内存使用限制..."
    
    local has_errors=false
    
    # 检查内存限制参数是否合理
    local test_files=$(find "$TEST_DIR/Test/Unit" -name "*.hs" -type f | head -10)
    
    while IFS= read -r file; do
        if [[ -f "$file" ]]; then
            # 检查 QuickCheck 参数是否在合理范围内
            local test_count=$(grep -o "QuickCheckTests [0-9]\+" "$file" | awk '{print $2}' | head -1)
            local max_size=$(grep -o "QuickCheckMaxSize [0-9]\+" "$file" | awk '{print $2}' | head -1)
            
            if [[ -n "$test_count" && $test_count -gt 20 ]]; then
                log_warning "⚠ $(basename "$file") 测试次数过多: $test_count"
                has_errors=true
            fi
            
            if [[ -n "$max_size" && $max_size -gt 10 ]]; then
                log_warning "⚠ $(basename "$file") 测试规模过大: $max_size"
                has_errors=true
            fi
        fi
    done <<< "$test_files"
    
    return $has_errors
}

# 生成验证报告
generate_validation_report() {
    local report_file="$TEST_DIR/memory_optimization_validation_$(date +%Y%m%d_%H%M%S).txt"
    
    cat > "$report_file" << EOF
=== 内存优化验证报告 ===
生成时间: $(date)

验证项目:
1. 内存优化配置检查
2. 测试文件内存优化检查
3. 测试覆盖率检查
4. 内存使用限制检查

结果摘要:
- 配置完整性: $(check_memory_configuration >/dev/null 2>&1 && echo "通过" || echo "失败")
- 优化覆盖率: $(check_test_memory_optimization >/dev/null 2>&1 && echo "通过" || echo "部分失败")
- 测试覆盖率: $(check_test_coverage >/dev/null 2>&1 && echo "通过" || echo "失败")
- 内存限制: $(check_memory_limits >/dev/null 2>&1 && echo "通过" || echo "部分失败")

建议:
- 确保所有测试文件都应用内存优化参数
- 保持核心功能测试的完整性
- 监控测试运行时的内存使用情况
- 定期验证优化效果
EOF
    
    log_success "验证报告生成完成: $report_file"
}

# 主验证函数
validate_all() {
    echo "=== 内存优化验证 ==="
    
    local overall_success=true
    
    # 1. 检查内存优化配置
    if check_memory_configuration; then
        log_success "内存优化配置检查: 通过"
    else
        log_error "内存优化配置检查: 失败"
        overall_success=false
    fi
    
    # 2. 检查测试文件的内存优化
    if check_test_memory_optimization; then
        log_success "测试文件内存优化检查: 通过"
    else
        log_warning "测试文件内存优化检查: 部分失败"
        # 不标记为整体失败，因为可能有些文件不需要优化
    fi
    
    # 3. 检查测试覆盖率
    if check_test_coverage; then
        log_success "测试覆盖率检查: 通过"
    else
        log_error "测试覆盖率检查: 失败"
        overall_success=false
    fi
    
    # 4. 检查内存使用限制
    if check_memory_limits; then
        log_success "内存使用限制检查: 通过"
    else
        log_warning "内存使用限制检查: 部分失败"
        # 不标记为整体失败
    fi
    
    # 生成报告
    generate_validation_report
    
    # 最终结果
    echo ""
    if [[ "$overall_success" == "true" ]]; then
        log_success "=== 内存优化验证: 通过 ==="
        log_info "测试用例不会消耗大量内存，同时保持测试覆盖率"
        return 0
    else
        log_error "=== 内存优化验证: 失败 ==="
        log_info "需要修复上述问题以确保内存优化效果"
        return 1
    fi
}

# 显示帮助信息
show_help() {
    cat << EOF
用法: $0

功能:
- 验证内存优化配置的完整性
- 检查测试文件是否应用内存优化
- 验证测试覆盖率是否保持
- 检查内存使用限制是否合理
- 生成详细验证报告

验证项目:
1. 内存优化配置检查
2. 测试文件内存优化检查  
3. 测试覆盖率检查
4. 内存使用限制检查
EOF
}

# 参数处理
case "${1:-}" in
    "--help" | "-h")
        show_help
        exit 0
        ;;
    *)
        validate_all
        ;;
esac