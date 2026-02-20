#!/usr/bin/env bash
# Enhanced Memory Optimization Verification and Enhancement Script
# 确保测试用例不会消耗大量内存，同时尽量不删除测试用例

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

# 统计变量
TOTAL_TEST_FILES=0
OPTIMIZED_TEST_FILES=0
QUICKCHECK_FILES=0
MEMORY_OPTIMIZED_FILES=0
HIGH_RISK_FILES=0

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

log_header() {
    echo -e "${PURPLE}=== $1 ===${NC}"
}

# 检查文件是否使用了内存优化
is_memory_optimized() {
    local file="$1"
    if grep -q -E "(SuperMemoryOptimization|ExtremeMemoryOptimization|AdvancedMemoryOptimization|MemoryOptimizedQuickCheck|withSuperMemoryLimits|withExtremeMemoryLimits|withAdvancedMemoryLimits|QuickCheckMaxSize.*1|QuickCheckTests.*1|quickcheck-tests=1|quickcheck-max-size=1)" "$file" 2>/dev/null; then
        return 0
    else
        return 1
    fi
}

# 检查文件是否有高内存使用风险
has_high_memory_risk() {
    local file="$1"
    local risk_count=0
    
    # 检查大字符串生成
    if grep -q -E "(vectorOf|listOf|elements|arbitrary|sized).*[0-9]+" "$file" 2>/dev/null; then
        risk_count=$((risk_count + 1))
    fi
    
    # 检查大数生成
    if grep -q -E "(choose|oneof|frequency).*[0-9]{2,}" "$file" 2>/dev/null; then
        risk_count=$((risk_count + 1))
    fi
    
    # 检查递归数据结构
    if grep -q -E "(Tree|Graph|Recursive).*arbitrary" "$file" 2>/dev/null; then
        risk_count=$((risk_count + 1))
    fi
    
    # 检查没有大小限制的生成器
    if grep -q -E "arbitrary.*::.*Gen" "$file" 2>/dev/null && ! grep -q "resize\|scale\|limit" "$file" 2>/dev/null; then
        risk_count=$((risk_count + 1))
    fi
    
    # 如果有2个或更多风险因素，则认为是高风险
    if [ "$risk_count" -ge 2 ]; then
        return 0
    else
        return 1
    fi
}

# 分析测试文件
analyze_test_files() {
    log_header "分析测试文件"
    
    # 查找所有Haskell测试文件
    local test_files=$(find test -name "*.hs" -type f 2>/dev/null)
    
    for file in $test_files; do
        TOTAL_TEST_FILES=$((TOTAL_TEST_FILES + 1))
        
        # 检查是否是QuickCheck文件
        if echo "$file" | grep -q -i "quickcheck\|property\|prop_"; then
            QUICKCHECK_FILES=$((QUICKCHECK_FILES + 1))
        fi
        
        # 检查是否使用了内存优化
        if is_memory_optimized "$file"; then
            OPTIMIZED_TEST_FILES=$((OPTIMIZED_TEST_FILES + 1))
            if echo "$file" | grep -q -i "quickcheck\|property\|prop_"; then
                MEMORY_OPTIMIZED_FILES=$((MEMORY_OPTIMIZED_FILES + 1))
            fi
        fi
        
        # 检查是否有高内存使用风险
        if has_high_memory_risk "$file"; then
            HIGH_RISK_FILES=$((HIGH_RISK_FILES + 1))
            log_warning "高风险文件: $file"
        fi
    done
    
    log_info "总测试文件数: $TOTAL_TEST_FILES"
    log_info "QuickCheck文件数: $QUICKCHECK_FILES"
    log_info "已优化文件数: $OPTIMIZED_TEST_FILES"
    log_info "已优化的QuickCheck文件数: $MEMORY_OPTIMIZED_FILES"
    log_info "高风险文件数: $HIGH_RISK_FILES"
}

# 验证内存优化配置
verify_memory_configurations() {
    log_header "验证内存优化配置"
    
    local config_files=(
        "test/super-memory-config.env"
        "test/ultimate-memory-config.env"
        "test/test-memory-config.yaml"
        "test/test-minimal-memory-config.env"
        "super_optimized_test_config.yaml"
    )
    
    for config in "${config_files[@]}"; do
        if [ -f "$config" ]; then
            log_success "配置文件存在: $config"
            
            # 检查关键配置项
            if grep -q -E "(MEMORY_LIMIT.*=.*[0-9]+|QUICKCHECK_TESTS.*=.*[0-9]+|GHCRTS.*=-M)" "$config" 2>/dev/null; then
                log_success "配置文件包含内存优化设置: $config"
            else
                log_warning "配置文件缺少内存优化设置: $config"
            fi
        else
            log_warning "配置文件不存在: $config"
        fi
    done
}

# 验证内存优化脚本
verify_memory_scripts() {
    log_header "验证内存优化脚本"
    
    local script_files=(
        "scripts/super_memory_optimized_test_runner.sh"
        "scripts/unified_memory_optimized_test_runner.sh"
        "scripts/verify_memory_optimization.sh"
        "scripts/verify_memory_optimizations.sh"
    )
    
    for script in "${script_files[@]}"; do
        if [ -f "$script" ]; then
            log_success "脚本文件存在: $script"
            
            # 检查脚本是否可执行
            if [ -x "$script" ]; then
                log_success "脚本可执行: $script"
            else
                log_warning "脚本不可执行: $script"
            fi
        else
            log_warning "脚本文件不存在: $script"
        fi
    done
}

# 检查内存优化支持模块
verify_memory_support_modules() {
    log_header "检查内存优化支持模块"
    
    local support_modules=(
        "test/TestSupport/SuperMemoryOptimization.hs"
        "test/TestSupport/ExtremeMemoryOptimization.hs"
        "test/TestSupport/AdvancedMemoryOptimization.hs"
        "test/TestSupport/MemoryOptimizedQuickCheck.hs"
        "test/TestSupport/MemoryLimits.hs"
    )
    
    for module in "${support_modules[@]}"; do
        if [ -f "$module" ]; then
            log_success "支持模块存在: $module"
        else
            log_warning "支持模块不存在: $module"
        fi
    done
}

# 生成内存优化报告
generate_memory_report() {
    log_header "生成内存优化报告"
    
    local report_file="enhanced_memory_optimization_report_$(date +%Y%m%d_%H%M%S).txt"
    
    {
        echo "Typus项目增强内存优化验证报告"
        echo "生成时间: $(date)"
        echo "=========================================="
        echo ""
        
        echo "文件统计:"
        echo "  总测试文件数: $TOTAL_TEST_FILES"
        echo "  QuickCheck文件数: $QUICKCHECK_FILES"
        echo "  已优化文件数: $OPTIMIZED_TEST_FILES"
        echo "  已优化的QuickCheck文件数: $MEMORY_OPTIMIZED_FILES"
        echo "  高风险文件数: $HIGH_RISK_FILES"
        echo ""
        
        echo "优化覆盖率:"
        if [ "$TOTAL_TEST_FILES" -gt 0 ]; then
            local overall_coverage=$((OPTIMIZED_TEST_FILES * 100 / TOTAL_TEST_FILES))
            local quickcheck_coverage=0
            if [ "$QUICKCHECK_FILES" -gt 0 ]; then
                quickcheck_coverage=$((MEMORY_OPTIMIZED_FILES * 100 / QUICKCHECK_FILES))
            fi
            
            echo "  总体优化覆盖率: $overall_coverage%"
            echo "  QuickCheck优化覆盖率: $quickcheck_coverage%"
        fi
        echo ""
        
        echo "内存优化状态:"
        if [ "$HIGH_RISK_FILES" -eq 0 ]; then
            echo "  ✓ 无高风险文件"
        else
            echo "  ⚠ 发现 $HIGH_RISK_FILES 个高风险文件"
        fi
        
        if [ "$MEMORY_OPTIMIZED_FILES" -gt $((QUICKCHECK_FILES / 2)) ]; then
            echo "  ✓ QuickCheck文件优化覆盖率良好"
        else
            echo "  ⚠ QuickCheck文件优化覆盖率需要改进"
        fi
        echo ""
        
        echo "建议:"
        if [ "$HIGH_RISK_FILES" -gt 0 ]; then
            echo "  1. 优化 $HIGH_RISK_FILES 个高风险文件"
            echo "  2. 使用更小的测试数据生成器"
            echo "  3. 添加内存限制配置"
        fi
        
        if [ "$MEMORY_OPTIMIZED_FILES" -lt $((QUICKCHECK_FILES * 80 / 100)) ]; then
            echo "  4. 为更多QuickCheck文件添加内存优化"
            echo "  5. 使用SuperMemoryOptimization或ExtremeMemoryOptimization模块"
        fi
        echo ""
        
        echo "使用方法:"
        echo "  - 使用超级内存优化脚本:"
        echo "    ./scripts/super_memory_optimized_test_runner.sh super-emergency"
        echo "  - 使用统一内存优化脚本:"
        echo "    ./scripts/unified_memory_optimized_test_runner.sh emergency"
        echo "  - 验证内存优化:"
        echo "    ./scripts/verify_memory_optimization.sh"
        echo ""
        
    } > "$report_file"
    
    log_success "内存优化报告已生成: $report_file"
}

# 提供优化建议
provide_optimization_suggestions() {
    log_header "内存优化建议"
    
    echo "基于当前分析，提供以下优化建议："
    echo ""
    
    if [ "$HIGH_RISK_FILES" -gt 0 ]; then
        log_warning "高风险文件优化建议:"
        echo "  1. 使用SuperMemoryOptimization模块包装高风险测试"
        echo "  2. 限制测试数据大小，例如："
        echo "     - 字符串长度限制为1-5个字符"
        echo "     - 列表长度限制为1-3个元素"
        echo "     - 整数范围限制为1-10"
        echo "  3. 使用QuickCheck配置："
        echo "     - QuickCheckMaxSize = 1"
        echo "     - QuickCheckTests = 1"
        echo "     - QuickCheckMaxShrinks = 0"
        echo ""
    fi
    
    if [ "$MEMORY_OPTIMIZED_FILES" -lt $((QUICKCHECK_FILES * 80 / 100)) ]; then
        log_warning "QuickCheck文件优化建议:"
        echo "  1. 为未优化的QuickCheck文件添加内存优化"
        echo "  2. 导入内存优化模块："
        echo "     import TestSupport.SuperMemoryOptimization"
        echo "     import TestSupport.ExtremeMemoryOptimization"
        echo "  3. 使用内存优化包装器："
        echo "     withSuperEmergencyMemoryLimits \$ testProperty \"test\" prop_test"
        echo ""
    fi
    
    log_success "最佳实践建议:"
    echo "  1. 在CI/CD中使用super-emergency或super-critical模式"
    echo "  2. 在开发中使用super-minimal模式"
    echo "  3. 定期运行内存优化验证脚本"
    echo "  4. 监控内存使用情况"
    echo "  5. 保持测试用例功能完整，只优化内存使用"
    echo ""
}

# 主函数
main() {
    log_header "Typus项目增强内存优化验证"
    log_info "确保测试用例不会消耗大量内存，同时尽量不删除测试用例"
    echo ""
    
    # 分析测试文件
    analyze_test_files
    echo ""
    
    # 验证内存优化配置
    verify_memory_configurations
    echo ""
    
    # 验证内存优化脚本
    verify_memory_scripts
    echo ""
    
    # 检查内存优化支持模块
    verify_memory_support_modules
    echo ""
    
    # 生成内存优化报告
    generate_memory_report
    echo ""
    
    # 提供优化建议
    provide_optimization_suggestions
    echo ""
    
    log_success "增强内存优化验证完成！"
    
    # 返回状态码
    if [ "$HIGH_RISK_FILES" -eq 0 ] && [ "$MEMORY_OPTIMIZED_FILES" -gt $((QUICKCHECK_FILES / 2)) ]; then
        log_success "内存优化状态良好"
        exit 0
    else
        log_warning "内存优化需要进一步改进"
        exit 1
    fi
}

# 运行主函数
main "$@"